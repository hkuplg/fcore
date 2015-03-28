# Testsuite

The layout of the testsuite is this:

| Path                   | Description                    |
|------------------------|--------------------------------|
| `tests`                | The actual test cases to run   |
| ` └ shouldCompile`     | Tests that should compile (against `f2j` and then `javac`) |
| ` └ shouldRun`         | Tests that should compile, run, and generate a particular output |
| ` └ shouldntTypecheck` | Tests that shouldn't typecheck |
