
# Note: Make sure runCalcs in the called script is set to TRUE
perc.stocks.vec = c(0.000001, seq(from=0.1, to=.9, by=0.1), 0.999999)
# perc.stocks.vec = c(0.000001, .25, .5, .75, 0.999999)

# And must comment out set.seed(100), which uses the same cost trajectory
# for all sim runs. (This is fixed in Claude's refactoring.)

perc.stocks = perc.stocks.vec[1]
# source("~/Documents/Bayes/Financial Modeling/Metropolis Sampling - 2021-12-18 v3d.R")
source("~/Documents/R/Financial Modeling/New Files (2026)/Metropolis Sampling - 2022-01-01 v2a.R")
gainsVsSdDf = gainsVsSD.point.estDf

for (k in 2:length(perc.stocks.vec)) {
  perc.stocks = perc.stocks.vec[k]
  source("~/Documents/R/Financial Modeling/New Files (2026)/Metropolis Sampling - 2022-01-01 v2a.R")
  gainsVsSdDf = rbind( gainsVsSdDf, gainsVsSD.point.estDf )
}

gainsVsSdDf = cbind(gainsVsSdDf, perc=rep(as.factor(perc.stocks.vec), each=2))
print(gainsVsSdDf)

plt = ggplot( data=gainsVsSdDf, aes(x=sd, y=gain, color=type) ) +
  # geom_point() +
  # geom_jitter(width=5) +
  geom_point() + 
  geom_hline(yintercept=c(s.int, b.int), linetype="dashed", 
             color="darkblue", alpha=0.3) + 
  # scale_y_continuous(labels = scales::percent, limits=c(0, 0.1)) + 
  scale_y_continuous(labels = scales::percent) + 
  # xlim(0, 500) + 
  ylab("gain (annualized %)") +
  # xlim(0, 1000) +
  theme_light()
plot(plt)
