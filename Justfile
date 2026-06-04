set shell := ["bash", "-eu", "-o", "pipefail", "-c"]

# Run a command via nix develop if available, otherwise directly
_nix *CMD:
    @if command -v nix &>/dev/null; then nix develop --command bash -c "{{CMD}}"; else bash -c "{{CMD}}"; fi

# Show available commands
help:
    just --list

# Enter `nix develop` sub-shell
dev:
    nix develop

# Fast tests under Babashka
_test-bb:
    bb test

# JVM Clojure tests
_test-clj:
    clojure -M:test

# Run tests in Babashka, JVM Clojure, ClojureScript (Node), and Basilisp (Python)
test:
    @rm -rf .test-results
    @echo "Running tests for Babashka, JVM, ClojureScript, and Basilisp..."
    @just _nix "bb test; clojure -M:test; cd web && npm ci --silent && npx shadow-cljs compile test 2>&1 | grep -v -E '^\[:|^shadow-cljs|^='; cd .."
    @just _test-lpy-report
    @just _nix "bb test/report.clj"

# Build the static ClojureScript web app (output: web/public/)
web-build:
    just _nix "cd web && npm ci && npx shadow-cljs release app && bash cache-bust.sh"

# Run the web app dev server with hot reload (http://localhost:8080)
web-dev:
    just _nix "cd web && npm ci && bash gen-dev-html.sh && npx shadow-cljs watch app"

# Run ClojureScript tests via Node
web-test:
    just _nix "cd web && npm ci --silent && npx shadow-cljs compile test"

# Watch for pushes and auto-restart web-dev
# https://github.com/EnigmaCurry/sway-home/blob/master/bin/git-drone
drone-dev:
    git drone --cmd "just web-dev" --pull "git fetch origin && git reset --hard origin/$(git rev-parse --abbrev-ref HEAD)" --repo EnigmaCurry/calc

# Ensure Basilisp venv exists
_ensure-venv:
    #!/usr/bin/env bash
    set -euo pipefail
    if [ ! -d .venv ]; then
        uv venv .venv
        source .venv/bin/activate
        uv pip install "basilisp[pytest]"
    fi

# Run tests under Basilisp (Python)
test-lpy: _ensure-venv
    #!/usr/bin/env bash
    set -euo pipefail
    source .venv/bin/activate
    find . -name "__pycache__" -type d -exec rm -rf {} + 2>/dev/null || true
    rm -rf .pytest_cache
    PYTHONPATH=src:test BASILISP_TEST_PATH=test basilisp test

# Run Basilisp tests and write EDN report to .test-results/
_test-lpy-report: _ensure-venv
    #!/usr/bin/env bash
    set -euo pipefail
    source .venv/bin/activate
    find . -name "__pycache__" -type d -exec rm -rf {} + 2>/dev/null || true
    rm -rf .pytest_cache
    mkdir -p .test-results
    PYTHONPATH=src:test BASILISP_TEST_PATH=test basilisp test -- \
        --junit-xml=.test-results/lpy-junit.xml --tb=no -q --no-header || true
    python3 test/lpy_report.py

# Remove build artifacts
clean:
    rm -rf web/node_modules web/.shadow-cljs web/out web/public/js web/public/index.html web/public/sw.js web/public/calc.html

# Run a conversion (e.g., just calc 5 miles to km)
calc *ARGS:
    #!/usr/bin/env bash
    set -euo pipefail
    ARGS='{{ARGS}}'
    if command -v nix &>/dev/null; then
        nix develop --command bb calc $ARGS
    else
        bb calc $ARGS
    fi
