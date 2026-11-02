// Stub. The site menu now lives in site-nav.js (steves-site repo, public/site-nav.js).
// To add, rename or hide pages, edit SITE_NAV_CONFIG there, not this file.
(function () {
  var me = document.currentScript;
  var s = document.createElement('script');
  s.src = '/site-nav.js';
  s.dataset.section = 'montecarlo';
  // Local previews of other repos have no /site-nav.js: use the live one.
  if (!/(^|\.)stevessite\.com$/.test(location.hostname)) {
    s.onerror = function () {
      var live = document.createElement('script');
      live.src = 'https://stevessite.com/site-nav.js';
      live.dataset.section = 'montecarlo';
      s.parentNode.insertBefore(live, s.nextSibling);
    };
  }
  me.parentNode.insertBefore(s, me.nextSibling);
})();
