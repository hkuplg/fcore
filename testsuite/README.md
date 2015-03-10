# Testsuite

The layout of the testsuite is this:

| Path                    | Description                    |
|-------------------------|--------------------------------|
| `tests`                 | The actual test cases to run   |
| ` └ should_compile`     | Tests that should compile only |
| ` └ should_run`         | Tests that should compile, run, and generate a particular output |
| ` └ shouldnt_typecheck` | Tests that shouldn't typecheck |
