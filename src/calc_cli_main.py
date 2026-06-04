"""Thin Python entry point that bootstraps Basilisp and runs calc."""
import sys


def main():
    phrase = " ".join(sys.argv[1:])
    if not phrase:
        print("Usage: calc <expression>")
        print("  e.g. calc 12 feet in meters")
        print("       calc 100 celsius in fahrenheit")
        print("       calc sqrt 144")
        print("       calc sin 30")
        print("       calc 20 percent tip on 50")
        print("       calc 255 in hex")
        sys.exit(1)

    safe = phrase.replace("\\", "\\\\").replace('"', '\\"')
    code = (
        '(require \'[calc.parser :as p])'
        '(require \'[calc.eval :as ev])'
        '(require \'[calc.format :as fmt])'
        '(let [req (p/parse-request "' + safe + '")'
        '      result (ev/convert-request req)]'
        '  (if (:ok? result)'
        '    (let [fmt-opts (:format req)'
        '          op-str (fmt/format-op-result req result fmt-opts)]'
        '      (println (or op-str (fmt/format-number (:value result) fmt-opts))))'
        '    (binding [*out* *err*]'
        '      (println (fmt/format-error result)))))'
    )
    sys.argv = ["basilisp", "run", "-c", code]

    from basilisp.cli import invoke_cli

    try:
        invoke_cli()
    except SystemExit as e:
        sys.exit(e.code or 0)
