# emacs.d
My emacs init files. These include lots of bits and bobs that I've picked up over several years.

## Tests and coverage

Install the development dependencies in the project Eask environment:

```shell
eask install-deps --dev
```

Run all tests, or only specs whose full description matches a pattern:

```shell
./run-tests.sh
./run-tests.sh eds-utils
```

Run the same tests with Undercover instrumentation and print a per-module
coverage summary:

```shell
./run-coverage.sh
./run-coverage.sh eds-utils
```

The machine-readable SimpleCov report is written to
`coverage/.resultset.json`. The coverage command deletes any previous report
first and fails if Undercover records no instrumented files or executed lines.
Both scripts use the project Eask environment; using `eask -g` would expose the
global Buttercup install without the project's Undercover dependency.

For details of conventions in this repo, see the [Agents configuration](./AGENTS.md).
