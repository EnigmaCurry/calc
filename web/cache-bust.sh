#!/usr/bin/env bash
set -euo pipefail

# Post-build cache-busting: hash main.js and stamp index.html + sw.js
cd "$(dirname "$0")/public"

HASH=$(sha256sum js/main.js | cut -c1-10)

# Rename the JS bundle (remove previous hashed bundles first)
for f in js/main.*.js; do
  [ -f "$f" ] && rm -f "$f"
done
mv js/main.js "js/main.${HASH}.js"

# Generate index.html and sw.js from templates with the hash stamped in
sed "s/__HASH__/${HASH}/g" index.html.template > index.html
sed "s/__HASH__/${HASH}/g" sw.js.template > sw.js

echo "Cache-busted with hash: ${HASH}"
