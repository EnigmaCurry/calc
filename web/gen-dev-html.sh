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
  if (navigator.serviceWorker) {\
    navigator.serviceWorker.getRegistrations().then(function(regs) {\
      regs.forEach(function(r) { r.unregister(); });\
    });\
  }\
  var wasDown = false;\
  var lastLen = null;\
  setInterval(function() {\
    var x = new XMLHttpRequest();\
    x.open("GET", "/js/main.js?_=" + Date.now(), true);\
    x.timeout = 3000;\
    x.onload = function() {\
      var len = x.responseText.length;\
      if (!wasDown) { lastLen = len; return; }\
      /* Server is back; only reload once JS content has changed (build done) */\
      if (lastLen !== null && len !== lastLen) location.reload(true);\
      if (lastLen === null) lastLen = len;\
    };\
    x.onerror = x.ontimeout = function() { wasDown = true; };\
    x.send();\
  }, 2000);\
})();\
</script>' index.html

# Remove the service worker registration line from dev build
sed -i '/navigator\.serviceWorker\.register/d' index.html
