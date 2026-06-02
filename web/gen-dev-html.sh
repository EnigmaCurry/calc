#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/public"
GIT_SHA=$(git -C "$(dirname "$0")" rev-parse --short HEAD 2>/dev/null || echo "dev")
sed "s/main\.__HASH__\.js/main.js/g; s/__HASH__/dev/g; s/__GIT_SHA__/${GIT_SHA}/g" index.html.template > index.html
sed 's/main\.__HASH__\.js/main.js/g; s/__HASH__/dev/g' sw.js.template > sw.js

# In dev mode: unregister service workers and auto-reload when shadow-cljs restarts.
# Removes the SW registration from the template and adds a polling reload script.
sed -i '/<\/body>/i <script>\
(function() {\
  /* Unregister service workers in dev so they cannot serve stale assets */\
  if (navigator.serviceWorker) {\
    navigator.serviceWorker.getRegistrations().then(function(regs) {\
      regs.forEach(function(r) { r.unregister(); });\
    });\
  }\
  /* Poll shadow-cljs dev server; reload when it comes back after going down */\
  var wasDown = false;\
  setInterval(function() {\
    var x = new XMLHttpRequest();\
    x.open("HEAD", "/js/main.js?_=" + Date.now(), true);\
    x.timeout = 1500;\
    x.onload = function() { if (wasDown) location.reload(true); };\
    x.onerror = x.ontimeout = function() { wasDown = true; };\
    x.send();\
  }, 2000);\
})();\
</script>' index.html

# Remove the service worker registration line from dev build
sed -i '/navigator\.serviceWorker\.register/d' index.html
