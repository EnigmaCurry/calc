set shell := ["bash", "-eu", "-o", "pipefail", "-c"]

# Show available commands
help:
    just --list

# Fast tests under Babashka
test-bb:
    bb test

# JVM Clojure tests
test-clj:
    clojure -M:test

# Run both runtimes
test: test-bb test-clj

