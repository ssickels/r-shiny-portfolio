(function () {
  const NAV_HTML = `
<nav id="site-nav">
  <button id="nav-hamburger" title="Menu" aria-label="Open site menu">
    <span></span><span></span><span></span>
  </button>
  <div id="nav-tabs">
    <a href="/monte-carlo.html" class="nav-tab" data-path="monte-carlo.html">Simulator</a>
    <a href="/monte-carlo-about.html" class="nav-tab" data-path="monte-carlo-about">About</a>
    <a href="/monte-carlo-methodology.html" class="nav-tab" data-path="monte-carlo-methodology">How It Works</a>
  </div>
  <div id="nav-panel" aria-hidden="true">
    <a href="https://stevessite.com" class="nav-panel-home">&#8962; Home</a>
    <div class="nav-panel-section">
      <div class="nav-panel-heading">Investment Simulator</div>
      <a href="https://investment-dashboard-aapf.onrender.com">Comparing Strategies</a>
      <a href="https://investment-dashboard-aapf.onrender.com/guide">Strategies User Guide</a>
      <a href="https://investment-dashboard-aapf.onrender.com/rebalancing">Rebalancing Impact</a>
      <a href="https://investment-dashboard-aapf.onrender.com/rebalancing-guide">Rebalancing User Guide</a>
      <a href="https://investment-dashboard-aapf.onrender.com/reading-charts">What the Charts Show</a>
      <a href="https://investment-dashboard-aapf.onrender.com/methodology">Methodology</a>
      <a href="https://github.com/ssickels/investment-dashboard" target="_blank" class="nav-panel-github">GitHub ↗</a>
    </div>
    <div class="nav-panel-section">
      <div class="nav-panel-heading">Monte Carlo Simulator</div>
      <a href="https://stevessite.com/monte-carlo.html">Simulator</a>
      <a href="https://stevessite.com/monte-carlo-about.html">About</a>
      <a href="https://stevessite.com/monte-carlo-methodology.html">How It Works</a>
      <a href="https://github.com/ssickels/r-shiny-portfolio" target="_blank" class="nav-panel-github">GitHub ↗</a>
    </div>
    <div class="nav-panel-section">
      <div class="nav-panel-heading">US Economic Dashboard</div>
      <a href="https://stevessite.com/dashboard-two/">Dashboard</a>
    </div>
    <div class="nav-panel-section">
      <div class="nav-panel-heading">Boids / Fish</div>
      <a href="https://stevessite.com/playground.html">Playground</a>
      <a href="https://stevessite.com/boids-explain.html">How It Works</a>
      <a href="https://stevessite.com/boids-about.html">About</a>
      <a href="https://stevessite.com/boids-impl.html">Dev Notes</a>
      <a href="https://github.com/ssickels/boids-playgrounds" target="_blank" class="nav-panel-github">GitHub ↗</a>
    </div>
    <div class="nav-panel-section">
      <div class="nav-panel-heading">Boids / Murmurations</div>
      <a href="https://stevessite.com/murmuration.html">Playground</a>
      <a href="https://stevessite.com/murmuration-about.html">How It Works</a>
      <a href="https://stevessite.com/murmuration-hawk.html">Hawk Attack</a>
      <a href="https://stevessite.com/murmuration-waves.html">Density Waves</a>
      <a href="https://stevessite.com/murmuration-intro.html">About</a>
      <a href="https://github.com/ssickels/boids-playgrounds" target="_blank" class="nav-panel-github">GitHub ↗</a>
    </div>
    <div class="nav-panel-section">
      <a href="https://stevessite.com/about.html" class="nav-panel-top-link">About This Site</a>
    </div>
    <a href="https://stevessite.com/contact.html" class="nav-panel-contact">Contact</a>
  </div>
</nav>`;

  const NAV_CSS = `
#site-nav {
  position: fixed;
  top: 0; left: 0; right: 0;
  height: 34px;
  background: rgba(26, 20, 16, 0.92);
  backdrop-filter: blur(6px);
  display: flex;
  align-items: center;
  padding: 0 12px;
  z-index: 9999;
  border-bottom: 1px solid rgba(200, 185, 160, 0.12);
  box-sizing: border-box;
}
#nav-tabs {
  display: flex;
  align-items: stretch;
  height: 34px;
  margin-left: 8px;
}
.nav-tab {
  color: rgba(200, 185, 160, 0.55);
  text-decoration: none;
  font-size: 12px;
  font-family: system-ui, -apple-system, sans-serif;
  display: flex;
  align-items: center;
  padding: 0 12px;
  border-bottom: 2px solid transparent;
  transition: color 0.15s, border-color 0.15s;
}
.nav-tab:hover { color: #e8ddd0; }
.nav-tab.nav-tab-active {
  color: #d4b896;
  border-bottom: 2px solid #d4b896;
}
#nav-hamburger {
  background: none;
  border: none;
  cursor: pointer;
  padding: 4px 6px;
  display: flex;
  flex-direction: column;
  justify-content: center;
  gap: 4px;
  height: 34px;
}
#nav-hamburger span {
  display: block;
  width: 18px;
  height: 1.5px;
  background: #d4b896;
  transition: background 0.15s;
}
#nav-hamburger:hover span { background: #e8ddd0; }
#nav-panel {
  display: none;
  flex-direction: column;
  position: absolute;
  top: 34px;
  left: 0;
  min-width: 220px;
  background: rgba(20, 14, 10, 0.97);
  border: 1px solid rgba(200, 185, 160, 0.14);
  border-top: none;
  z-index: 10000;
  padding: 6px 0;
  font-family: system-ui, -apple-system, sans-serif;
  font-size: 13px;
}
#nav-panel.is-open { display: flex; }
.nav-panel-home {
  color: #e8ddd0;
  text-decoration: none;
  padding: 7px 18px;
  display: block;
  border-bottom: 1px solid rgba(200, 185, 160, 0.12);
  margin-bottom: 4px;
  transition: background 0.12s;
}
.nav-panel-home:hover { background: rgba(200, 185, 160, 0.08); }
.nav-panel-section {
  display: flex;
  flex-direction: column;
  padding: 4px 0;
  border-bottom: 1px solid rgba(200, 185, 160, 0.10);
}
.nav-panel-section:last-child { border-bottom: none; }
.nav-panel-heading {
  color: rgba(200, 185, 160, 0.55);
  font-size: 10.5px;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  padding: 5px 18px 3px;
}
.nav-panel-section a {
  color: #c4b5a0;
  text-decoration: none;
  padding: 5px 18px 5px 28px;
  transition: background 0.12s, color 0.12s;
}
.nav-panel-section a:hover {
  background: rgba(200, 185, 160, 0.10);
  color: #e8ddd0;
}
.nav-panel-top-link {
  padding-left: 18px !important;
}
.nav-panel-github {
  color: rgba(200, 185, 160, 0.4) !important;
  font-size: 11.5px;
}
.nav-panel-github:hover {
  color: rgba(200, 185, 160, 0.75) !important;
}
.nav-panel-contact {
  color: #c4b5a0;
  text-decoration: none;
  padding: 7px 18px;
  display: block;
  border-top: 1px solid rgba(200, 185, 160, 0.12);
  margin-top: 4px;
  transition: background 0.12s;
}
.nav-panel-contact:hover { background: rgba(200, 185, 160, 0.08); }
`;

  const style = document.createElement('style');
  style.textContent = NAV_CSS;
  document.head.appendChild(style);

  document.body.insertAdjacentHTML('afterbegin', NAV_HTML);

  const btn   = document.getElementById('nav-hamburger');
  const panel = document.getElementById('nav-panel');

  btn.addEventListener('click', function (e) {
    e.stopPropagation();
    const opening = !panel.classList.contains('is-open');
    panel.classList.toggle('is-open', opening);
    panel.setAttribute('aria-hidden', String(!opening));
  });

  document.addEventListener('click', function () {
    panel.classList.remove('is-open');
    panel.setAttribute('aria-hidden', 'true');
  });

  // Highlight the active tab
  const path = window.location.pathname;
  document.querySelectorAll('.nav-tab').forEach(tab => {
    if (path.includes(tab.dataset.path)) {
      tab.classList.add('nav-tab-active');
    }
  });
})();
