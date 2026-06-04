"""Convert Basilisp JUnit XML test results to EDN for the test matrix report."""
import sys
import xml.etree.ElementTree as ET

tree = ET.parse(".test-results/lpy-junit.xml")
tests = failures = errors = 0
names = []
for s in tree.findall(".//testsuite"):
    tests += int(s.get("tests", 0))
    failures += int(s.get("failures", 0))
    errors += int(s.get("errors", 0))
    for c in s.findall("testcase"):
        ns = (
            c.get("classname", "")
            .replace("test.calc.", "calc.")
            .replace("_", "-")
            .replace(".cljc", "")
        )
        names.append(ns + "/" + c.get("name", ""))
passes = tests - failures - errors
names_edn = " ".join('"' + n + '"' for n in sorted(names))
with open(".test-results/lpy.edn", "w") as f:
    f.write(
        '{:platform "Basilisp" :test %d :pass %d :fail %d :error %d :test-names [%s]}'
        % (tests, passes, failures, errors, names_edn)
    )
if failures + errors > 0:
    sys.exit(1)
