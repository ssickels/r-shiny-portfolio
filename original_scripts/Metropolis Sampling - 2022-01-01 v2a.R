# v2: Switch to an *additive* interest model
# v2a: Fixes an incorrect approach in v2. (I hope!)

library(mvtnorm) # for mvnorm()
# library(rethinking) # for col.alpha() -- need to write these out!
library(ggplot2)
library(patchwork) # aligning plots
library(truncnorm) # for truncnorm distribution (for starting prices)
library(reshape2) # for melt() and dcast()
library(zoo) # for rollapply()
library(scales) # to convert ggplot scale to percentages

########## Parameters ############

# Number of timesteps
n = 1000 # 10000 # 1500

# Cost parameters
s.mean = 10
s.sd = 6 
b.mean = 10 
b.sd = 3 # .5 
s.b.corr = .7 # 0 # -0.5

# Starting values (costs) per share
start.costs = c(10, 10) # c(10, 8)

# Initial investment
init.inv = 1000 # $ invested
# perc.stocks = 0.8 # Comment this out when running Meta Script

rebal.interval = 100

s.int = 0.04
b.int = 0.02 # 0.01
int.period = 300 # 100 # Interest "period"

# Proposed step divisor. Larger numbers slow down how 
# quickly the Markov Chain explores the distribution.
propStepDivisor = 20 # 20

nSims = 100 # 1000
# For share costs and holdings.shares only (not holdings 
# value, for which I record all sims)
nSimsToRecord = 5
simSel = 2

# Width for rollapply calcs:
widthVal = 600

plotType = 1
# 1: Scatterplot, Gain (rebal & non-rebal) vs. sd
# 2: Scatterplot, Stocks, not rebalanced ## DOESN'T WORK
# 3: Scatterplot, Rebal gains vs. nonrebal gains
# 4: ## THROWS an error
# 5: Stock costs, and rolling average
#    (based on widthVal) 
# 6: Plot a bunch of sims' total trajectories; 
#    both stocks, and bonds
# 7: Plot a bunch of costs' profiles
# 8: Plot final densities of both stocks and bonds
# 9: Densities of deltas (rebal vs. not) across individual sims
# 10: Rebalanced Values (& Non-rebalanced Totals)

runCalcs = TRUE # set to FALSE to look at different plots 

set.seed(100)

######### Functions #############

annualizeFn = function(P, G, n) {
  AP = ((P+G)/P)^(1/(n/int.period)) - 1
  return(AP)
}

########## Program Body ##########

# avoid a rebalance on the last timestep (if and when n is a multiple 
# of rebal.interval)
n = n-1 

# Can skip the calcs if you're just selecting different plots 
if (runCalcs == TRUE ) {
  
  # Compose interest covariance matrix (argument to mvnorm())
  sdVec = c( s.sd, b.sd )/propStepDivisor
  sdM = diag( sdVec )
  rho = matrix( c(1, rep(s.b.corr, 2), 1), ncol=2 )
  s.b.cov = sdM %*% rho %*% sdM
  
  # Create increasing mean interest vectors
  s.int.vec = sapply(1:n, function(x) (1 + s.int)^((x-1)/int.period) ) - 1
  b.int.vec = sapply(1:n, function(x) (1 + b.int)^((x-1)/int.period) ) - 1
  
  # Create a stock price (including interest) data frame, for plotting
  s.mean.vec = s.mean * (1 + s.int.vec)
  b.mean.vec = b.mean * (1 + b.int.vec)
  
  # s.mean.vec = s.mean + s.int.vec
  # b.mean.vec = b.mean + b.int.vec
  
  mean.vecDf = data.frame( time=rep(1:n, 2), 
                           class=rep(c("stocks", "bonds"), each=n),
                           val=c(stocks=s.mean.vec, bonds=b.mean.vec) )
  
  # Initialize the matrices to hold the holdings shares, and values
  
  shares.rebal = array( dim=c(n, 2, nSimsToRecord) ) 
  shares.nonrebal = array( dim=c(n, 2, nSimsToRecord) )
  
  value.rebal = array( dim=c(n, 2, nSimsToRecord) )
  value.nonrebal = array( dim=c(n, 2, nSimsToRecord) )
  
  # Create "temp" version of the above for inside the sim loop,
  # which get overwritten each sim.
  
  shares.rebal.tmp = array( dim=c(n, 2) )
  shares.nonrebal.tmp = array( dim=c(n, 2) )

  value.rebal.tmp = array( dim=c(n, 2) )
  value.nonrebal.tmp = array( dim=c(n, 2) )
  
  # Initialize matrices to hold the *final* holdings shares, and values
  
  shares.rebal.final = array( dim=c(2, nSims) )
  shares.nonrebal.final = array( dim=c(2, nSims) )
  
  value.rebal.final = array( dim=c(2, nSims) )
  value.nonrebal.final = array( dim=c(2, nSims) )
  
  # Initialize matrices to hold the total (combined) value, and costs
  
  value.rebal.tot = array( dim=c(n, nSims) )
  value.nonrebal.tot = array( dim=c(n, nSims) )
  
  costs = array( dim=c(n, 2, nSims) )
  
  for (j in 1:nSims) {
    
    # Use *random* starting costs:
    # (a is the minimum value parameter)
    costs[1,1,j] = rtruncnorm(1, a=0.5, mean=s.mean, sd=s.sd )
    costs[1,2,j] = rtruncnorm(1, a=0.5, mean=b.mean, sd=b.sd )
    
    ### For each sim, compute the time sequence of per-share costs (prices)
    
    for (i in 2:n) {
      # probabilities from previous step
      p.s.prev = dnorm( x=costs[i-1,1,j], mean=s.mean, sd=s.sd )
      p.b.prev = dnorm( x=costs[i-1,2,j], mean=b.mean, sd=b.sd )
      
      # p.s.prev = dnorm( x=costs[i-1,1,j], mean=s.mean.vec[i], sd=s.sd )
      # p.b.prev = dnorm( x=costs[i-1,2,j], mean=b.mean.vec[i], sd=b.sd )
      
      # proposed share costs for the current step
      new = rmvnorm( n=1, mean=costs[i-1,,j], sigma=s.b.cov )
      
      # probabilities for current step
      p.s.new = dnorm( x=new[1], mean=s.mean, sd=s.sd )
      p.b.new = dnorm( x=new[2], mean=b.mean, sd=b.sd )
      
      # p.s.new = dnorm( x=new[1], mean=s.mean.vec[i], sd=s.sd )
      # p.b.new = dnorm( x=new[2], mean=b.mean.vec[i], sd=b.sd )
      
      # determine share costs for the current step (*not* accounting
      # for appreciation)
      eps = 1e-4 
      costs[i,1,j] = ifelse( runif(1) < p.s.new/p.s.prev & new[1] > eps, 
                             new[1], costs[i-1,1,j] )
      costs[i,2,j] = ifelse( runif(1) < p.b.new/p.b.prev & new[2] > eps,
                             new[2], costs[i-1,2,j] )
      
    }
    
    # Multiply costs by interest vector
    # costs[,1,j] = costs[,1,j] * (1 + s.int.vec)
    # costs[,2,j] = costs[,2,j] * (1 + b.int.vec)
    
    # costs[,1,j] = costs[,1,j] + s.int.vec
    # costs[,2,j] = costs[,2,j] + b.int.vec
    
    ##### NOTE: in previous versions, I had this as:
    costs[,1,j] = costs[,1,j] + (s.mean.vec - s.mean)
    costs[,2,j] = costs[,2,j] + (b.mean.vec - b.mean)
    
    # Which yielded annualized gains that were slightly too high.
    # Perhaps because my s.mean.vec and b.mean.vec calcs are
    # incorrect?? 
    ##### 
    
    ### For each sim, compute holdings, from prices
    
    perc.alloc.target = c( perc.stocks, 1 - perc.stocks )
    
    # Initialize holdings shares
    # shares = matrix(NA, nrow=n, ncol=2)
    shares.rebal.tmp[1,1] = ( init.inv*perc.stocks )/costs[1,1,j]
    shares.rebal.tmp[1,2] = ( init.inv*(1-perc.stocks) )/costs[1,2,j]
    
    # Initialize holdings values for each asset type
    # value = matrix( NA, nrow=n, ncol=2 )
    value.rebal.tmp[1,] = shares.rebal.tmp[1,] * costs[1,,j]
    # And the total value
    value.rebal.tot[1,j] = sum( value.rebal.tmp[1,] )
    
    # Fill out the holdings shares and holding value matrices
    for (i in 2:n) {
      
      shares.rebal.tmp[i,] = shares.rebal.tmp[i-1,]
      value.rebal.tmp[i,] = shares.rebal.tmp[i,] * costs[i,,j]
      value.rebal.tot[i,j] = sum( value.rebal.tmp[i,] )
      
      if (i %% rebal.interval == 0) { 
        
        # Identify the shares to sell (from the asset that's over
        # the allocation target)
        holdings.perc = value.rebal.tmp[i,] / sum(value.rebal.tmp[i,])
        del.perc = holdings.perc - perc.alloc.target
        # A TRUE / FALSE index vector; TRUE for the asset that's 
        # *above* the target
        del.max = del.perc > 0 
        
        # And calculate the gain over allocation target
        gain = del.perc[del.max] * sum(value.rebal.tmp[i,])
        
        # Sell what's over target:
        shares.rebal.tmp[i,del.max] = perc.alloc.target[del.max] * 
          value.rebal.tot[i,j] / costs[i,del.max,j]
        
        # Use the gain to purchase what's under
        shares.rebal.tmp[i,!del.max] = shares.rebal.tmp[i,!del.max] +
          gain / costs[i,!del.max,j]
        
        # Overwrite the values from outside the if statement
        value.rebal.tmp[i,] = shares.rebal.tmp[i,]*costs[i,,j]
        value.rebal.tot[i,j] = sum(value.rebal.tmp[i,])
        
      } 
    }
    
    # Initialize the shares and value matrices for the 
    # non-rebalanced case
    shares.nonrebal.tmp[1,] = shares.rebal.tmp[1,]
    
    value.nonrebal.tmp[1,] = value.rebal.tmp[1,]
    value.nonrebal.tot[1,j] = value.rebal.tot[1,j]
    
    # And fill out the non-rebalanced matrices
    for (i in 2:n) {
      shares.nonrebal.tmp[i,] = shares.nonrebal.tmp[i-1,]
      value.nonrebal.tmp[i,] = shares.nonrebal.tmp[i,] * costs[i,,j]
      value.nonrebal.tot[i,j] = sum( value.nonrebal.tmp[i,] )
    }
    
    # For the number of sims specified by nSimsToRecord, write
    # this loop's results into the multi-sim array variables. 
    if (j <= nSimsToRecord) {
      
      shares.rebal[,,j] = shares.rebal.tmp
      shares.nonrebal[,,j] = shares.nonrebal.tmp
      
      value.rebal[,,j] = value.rebal.tmp
      value.nonrebal[,,j] = value.nonrebal.tmp
      
    }
    
    # Record the value and shares held for the final timestep
    if (i == n) {
      
      shares.rebal.final[,j] = shares.rebal.tmp[n,]
      shares.nonrebal.final[,j] = shares.nonrebal.tmp[n,]
      
      value.rebal.final[,j] = value.rebal.tmp[n,]
      value.nonrebal.final[,j] = value.nonrebal.tmp[n,]
      
    }
    
  }
  
  ### Value, totals for the two strategies
  value.rebal.totDf = data.frame( time=rep(1:n, nSims), 
                                  # class=rep("total", n*nSims),
                                  sim=rep(1:nSims, each=n),
                                  type=rep("total rebal", n*nSims), 
                                  val=as.vector(value.rebal.tot) )
  
  value.nonrebal.totDf = data.frame( time=rep(1:n, nSims), 
                                     # class=rep("total", n*nSims),
                                     sim=rep((1:nSims), each=n),
                                     type=rep("total non-rebal", n*nSims), 
                                     val=as.vector(value.nonrebal.tot) )
  
  value.totDf = rbind( value.rebal.totDf, value.nonrebal.totDf )
  
  # Value, for the the two asset classes, for the two strategies
  value.rebalDf = data.frame( time=rep(1:n, nSims*2),
                              class=rep( rep(c("stocks", "bonds"), each=n),
                                         nSims),
                              sim=rep(1:nSims, each=n*2),
                              type=rep("rebal", n*nSims*2),
                              val=as.vector(value.rebal)
  )
  
  value.nonrebalDf = data.frame( time=rep(1:n, nSims*2),
                                 class=rep( rep(c("stocks", "bonds"), each=n),
                                            nSims),
                                 sim=rep(1:nSims, each=n*2),
                                 type=rep("nonrebal", n*nSims*2),
                                 val=as.vector(value.nonrebal)
  )
  
  valueDf = rbind( value.rebalDf, value.nonrebalDf )
  
  ### Shares 
  
  shares.rebalDf = data.frame( time=rep(1:n, nSims*2),
                               class=rep( rep(c("stocks", "bonds"), each=n),
                                          nSims),
                               sim=rep(1:nSims, each=n*2),
                               type=rep("nonrebal", n*nSims*2),
                               shares=as.vector(shares.rebal)
  )
  
  shares.nonrebalDf = data.frame( time=rep(1:n, nSims*2),
                                  class=rep( rep(c("stocks", "bonds"), each=n),
                                             nSims),
                                  sim=rep(1:nSims, each=n*2),
                                  type=rep("nonrebal", n*nSims*2),
                                  shares=as.vector(shares.nonrebal)
  )
  
  sharesDf = rbind( shares.rebalDf, shares.nonrebalDf )
  
  ### Costs 
  
  costsDf = data.frame( time=rep(1:n, nSims*2), 
                        class=rep( rep(c("stocks", "bonds"), each=n),
                                   nSims),
                        sim=rep(1:nSims, each=n*2),
                        cost=as.vector(costs)
  )
  
  ### Delta at Final Time
  
  deltaDf = data.frame(rebal=value.rebal.totDf[value.rebal.totDf$time == n, ]$val,
                       nonrebal=value.nonrebal.totDf[value.nonrebal.totDf$time == n, ]$val)
  deltaDf = cbind(deltaDf, delta=deltaDf[,1]-deltaDf[,2])
  
  ### Standard Deviations
  
  # Calculate the sd's for value.rebal.tot and value.non.rebal.tot
  
  ## Rebalanced total
  mean.rebal = matrix( nrow=n-widthVal+1, ncol=nSims )
  error.rebal = matrix( nrow=n-widthVal+1, ncol=nSims )
  errorSD.rebal = vector( length=nSims )
  for (j in 1:nSims) {
    mean.rebal[,j] = rollapply( value.rebal.tot[,j], width=widthVal,
                                align="center", FUN=mean )
    error.rebal[,j] = value.rebal.tot[(widthVal/2):(n-widthVal/2),j] - 
      mean.rebal[,j]
    errorSD.rebal[j] = sd(error.rebal[,j])
    # Convert SDs to percentages. (Dividing by the mean mean is an 
    # approximation! Particularly if there's a big swing in the mean)
    ### errorSD.rebal[j] = errorSD.rebal[j] / mean( mean.rebal[,j] )
  }
  
  ############# 1/1/2022
  
  # costs [1:n, 1:2, 1:nSims]
  # shares.rebal.tmp [n, 1:2]
  # shares.nonrebal.tmp [n, 1:2]

  shares.perc.rebal = shares.rebal.tmp/rowSums(shares.rebal.tmp)
  weightedCPS.rebal = rowSums( costs[,,j]*shares.perc.rebal ) 
  
  # Need to make sure that my rolling average covers through the final 
  # value of the time sequence. 
  
  
  
  ##############
  
  ## Non-rebalanced total
  mean.nonrebal = matrix( nrow=n-widthVal+1, ncol=nSims )
  error.nonrebal = matrix( nrow=n-widthVal+1, ncol=nSims )
  errorSD.nonrebal = vector( length=nSims )
  for (j in 1:nSims) {
    mean.nonrebal[,j] = rollapply( value.nonrebal.tot[,j], width=widthVal,
                                   align="center", FUN=mean )
    error.nonrebal[,j] = value.nonrebal.tot[(widthVal/2):(n-widthVal/2),j] - 
      mean.nonrebal[,j]
    errorSD.nonrebal[j] = sd(error.nonrebal[,j])
    # Convert SDs to percentages. (Dividing by the mean mean is an 
    # approsimation! Particularly if there's a big swing in the mean)
    ### errorSD.nonrebal[j] = errorSD.nonrebal[j] / mean( mean.nonrebal[,j] )
  }
  
  # Calculate the gain at the n'th (the final) time step
  gain.rebal = value.rebal.tot[n,] - init.inv
  gain.rebal = annualizeFn( init.inv, gain.rebal, n )
  
  gain.nonrebal = value.nonrebal.tot[n,] - init.inv 
  gain.nonrebal = annualizeFn( init.inv, gain.nonrebal, n)
  
  gainVsSD.rebalDf = data.frame( gain=gain.rebal, sd=errorSD.rebal, 
                                 sim=1:nSims,
                                 type=rep("rebal", nSims))
  ## The following line was fixed in version d (from sd=errorSD.rebal): 
  gainVsSD.nonrebalDf = data.frame( gain=gain.nonrebal, sd=errorSD.nonrebal, 
                                    sim=1:nSims,
                                    type=rep("nonrebal", nSims))
  gainVsSDDf = rbind( gainVsSD.rebalDf, gainVsSD.nonrebalDf )
  
  gainsVsSD.point.estDf = data.frame( sd=c( mean(errorSD.rebal),
                                            mean(errorSD.nonrebal)),
                                      gain=c( mean(gain.rebal), 
                                              mean(gain.nonrebal)), 
                                      type=c("rebal", "nonrebal") )
  
}

########## Plots ############

if (plotType == 1) { # gain (rebal & non-rebal) vs. sd
  
  plt = ggplot( data=gainVsSDDf, aes(x=sd, y=gain, color=type) ) +
    geom_point( alpha=0.08 ) +
    geom_point( data = gainsVsSD.point.estDf, aes(x=sd, y=gain,
                                                  fill=type),
                size=3, shape=21 , color="black", alpha=0.8 ) +
    geom_hline(yintercept=c(s.int, b.int), linetype="dashed", 
               color="darkblue", alpha=0.3) + 
    ylab("gain (annualized %)") +
    xlab("standard deviation") +
    ggtitle(paste0("Stocks / Bonds = ", perc.stocks*100, "% / ", 
                   (1-perc.stocks)*100, "%")) +
    
    # scale_y_continuous(labels = scales::percent, limits=c(-0.05, 0.2)) +
    # xlim(0, 1000) +
    
    # # Zoomed in parameters
    # scale_y_continuous(labels = scales::percent, limits=c(0, 0.08)) +
    # xlim(0, 300) +
    
    # # Medium view
    # scale_y_continuous(labels = scales::percent, limits=c(0, 0.05)) +
    # xlim(0, 250) +
  
    # # Medium view
    # scale_y_continuous(labels = scales::percent, limits=c(-0.02, 0.15)) +
    # xlim(0, 1000) +
  
    # Unconstrained
    scale_y_continuous(labels = scales::percent) +
    
    theme_light()
  plot(plt)
  
} else if (plotType == 2) { # Scatterplot, for stocks, not rebalanced
  
  ### Needs to be fixed 
  
  gain.nonrebal.s = value.nonrebal[n,1,] - init.inv*perc.stocks
  gain.nonrebal.s = annualizeFn( init.inv*(1-perc.stocks), gain.nonrebal.s, n )
  
  gainVsSD.nonrebal.sDf = data.frame( gain=gain.nonrebal.s, 
                                      sd=errorSD.nonrebal, 
                                      sim=1:nSims,
                                      type=rep("rebal", nSims))
  
  plt = ggplot( data=gainVsSD.nonrebal.sDf, aes(x=sd, y=gain) ) +
    geom_point( alpha=0.1, color="steelblue" ) +
    geom_point( x=mean(gainVsSD.nonrebal.sDf$sd),
                y=mean(gainVsSD.nonrebal.sDf$gain),
                size=3, color="steelblue" ) +
    xlab("sd (%)") +
    ylab("gain (annualized %)") +
    theme_light()
  plot(plt)
  
} else if (plotType == 3) { # Scatterplot of rebal gains vs. nonrebal gains
  
  # I don't think this is working correctly
  
  gainVsSDCastDf = dcast( gainVsSDDf, sim ~ type, value.var="gain" )
  plt = ggplot( data=gainVsSDCastDf, aes( x=nonrebal, y=rebal,
                                          group=sim ) ) +
    geom_point( alpha=0.2, color="steelblue" ) +
    geom_abline( intercept=0, slope=1, color="darkred", linetype="dashed",
                 alpha=0.5) +
    theme_light()
  plot(plt)
  
} else if (plotType == 4) {
  
  # Throws an error
  # (Non-ggplot)
  
  # Try the above approach on the asset values
  mean.s.rebal = matrix( nrow=n-widthVal+1, ncol=nSims )
  error.s.rebal = matrix( nrow=n-widthVal+1, ncol=nSims )
  errorSD.s.rebal = vector( length=nSims )
  for (j in 1:nSims) {
    mean.s.rebal[,j] = rollapply( value.rebal[,1,j], width=widthVal,
                                  align="center", FUN=mean )
    error.s.rebal[,j] = value.rebal[(widthVal/2):(n-widthVal/2),1,j] -
      mean.s.rebal[,j]
    errorSD.s.rebal[j] = sd(error.s.rebal[,j])
  }
  simSel = 1
  plot(value.rebal[(widthVal/2):(n-widthVal/2),1,simSel], type="l",
       col="darkgreen")
  lines(mean.s.rebal[1:(n-widthVal+1),simSel], type="l", col="darkred")
  
} else if (plotType == 5) { # Stock costs, and rolling average
                            # (based on widthVal)
  
  # Try the above approach on the stock costs
  mean.s.costs = matrix( nrow=n-widthVal+1, ncol=nSims )
  error.s.costs = matrix( nrow=n-widthVal+1, ncol=nSims )
  errorSD.s.costs = vector( length=nSims )
  for (j in 1:nSims) {
    mean.s.costs[,j] = rollapply( costs[,1,j], width=widthVal,
                                  align="center", FUN=mean )
    error.s.costs[,j] = costs[(widthVal/2):(n-widthVal/2),1,j] -
      mean.s.costs[,j]
    errorSD.s.costs[j] = sd(error.s.costs[,j])
  }
  
  plot(costs[(widthVal/2):(n-widthVal/2),1,simSel], type="l",
       col="darkgreen")
  lines(mean.s.costs[1:(n-widthVal+1),simSel], type="l", col="darkred")
  
  # plot(errorSD.s.costs, col=col.alpha("blue", 0.1))
  # dens(errorSD.s.costs)
  # mean(errorSD.s.costs)
  
} else if (plotType == 6) { # Plot a bunch of sims' total trajectories; 
                            # both stocks, and bonds
  
  plt = ggplot(data=value.totDf[with(value.totDf, sim <= 100), ],
               aes(x=time, y=val, group=interaction(type, sim), color=type)) +
    geom_line( alpha=0.1 ) +
    # ylim(0, 4000) +
    theme_light()
  plot(plt)
  
} else if (plotType == 7) { # Plot a bunch of costs' profiles
  
  plt = ggplot(data=costsDf[with(costsDf, sim <= 10), ],
               aes(x=time, y=cost, group=interaction(class, sim), color=class)) +
    geom_line( alpha=0.5 ) +
    theme_light()
  plot(plt)
  
} else if (plotType == 8) {   # Plot final densities of both stocks and bonds
  
  dfFinal = value.totDf[ value.totDf$time == n , ]
  plt = ggplot( data=dfFinal, aes( x=val, group=type, color=type,
                                   fill=type) ) +
    geom_density( alpha=0.2) +
    xlim(0, 1e4) +
    theme_light()
  plot(plt)
  
} else if (plotType == 9) { # Densities of deltas across individual sims
                            # (rebal, and not)
  
  medianDel = median(deltaDf$delta)
  plt = ggplot(data=deltaDf, aes(x=delta)) +
    geom_density( color="blue", alpha=0.2) +
    geom_vline( xintercept=medianDel, color="blue", linetype="dashed" ) +
    # xlim(-1000, 2000) +
    ggtitle( paste("Density of deltas between rebal and non-rebal; median =",
                   round(medianDel, 2)) ) +
    theme_light() +
    theme( plot.title = element_text( size=10 ) )
  plot(plt)
  
} else if (plotType == 10) {
  
  ### Rebalanced Values (& Non-rebalanced Totals)
  
  value.sim.selDf = valueDf[valueDf$sim == simSel & valueDf$type == "rebal", ]
  value.tot.sim.selDf = value.totDf[value.totDf$sim == simSel, ]
  plt.vals = ggplot() +
    geom_line( data=value.sim.selDf, aes(x=time, y=val, color=class) ) +
    geom_line( data=value.tot.sim.selDf, aes(x=time, y=val, color=type) ) +
    ylab("value") + 
    ggtitle( paste("Rebalanced Asset Values (& Non-rebalanced Totals); Sim #", simSel) ) +
    theme_light() +
    theme( plot.title = element_text( size=10 ), axis.title.x=element_blank() )
  # plot(plt.vals)
  
  ### Shares (Rebalanced)
  shares.sim.selDf = sharesDf[costsDf$sim == simSel & valueDf$type == "rebal", ]
  
  plt.shares = ggplot( data=shares.sim.selDf, aes(x=time, y=shares, color=class) ) +
    geom_line() +
    ggtitle( paste("Shares Held, Rebalanced; Sim #", simSel) ) +
    theme_light() +
    theme( plot.title = element_text( size=10 ), axis.title.x=element_blank() )
  # plot(plt.shares)
  
  ### Costs
  costs.sim.selDf = costsDf[costsDf$sim == simSel, ]
  plt.costs = ggplot( data=costs.sim.selDf, aes(x=time, y=cost, color=class) ) +
    geom_line() +
    geom_line( data=mean.vecDf, aes(x=time, y=val, color=class), alpha = 0.5,
               linetype="dashed") +
    ggtitle( paste("Costs per Share; Sim #", simSel) ) +
    theme_light() +
    theme( plot.title = element_text( size=10 ) )
  # plot(plt.costs)
  
  plt = plt.vals + plt.shares + plt.costs +
    plot_layout( ncol=1, heights=c(3, 1, 1))
  plot(plt)
  
}

cat("mean rebal    :", mean(value.rebal.tot), "\n")
cat("mean non-rebal:", mean(value.nonrebal.tot), "\n")
