#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/public"
GIT_SHA=$(git -C "$(dirname "$0")" rev-parse --short HEAD 2>/dev/null || echo "dev")
sed "s/main\.__HASH__\.js/main.js/g; s/__HASH__/dev/g; s/__GIT_SHA__/${GIT_SHA}/g" index.html.template > index.html
sed 's/main\.__HASH__\.js/main.js/g; s/__HASH__/dev/g' sw.js.template > sw.js

# Inject dev-only auto-reload: polls shadow-cljs, reloads page when server comes back
sed -i '/<\/body>/i <script>\
(function() {\
  var wasDown = false;\
  setInterval(function() {\
    fetch("/js/main.js", {method: "HEAD", cache: "no-store"})\
      .then(function() { if (wasDown) location.reload(); })\
      .catch(function() { wasDown = true; });\
  }, 2000);\
})();\
</script>' index.html
