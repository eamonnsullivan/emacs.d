# emacs.d

My Emacs initialisation. This is unlikely to be of use to anyone else, except perhaps as sample code. I have made exactly zero effort to make this configuration usable by others.

The config is broken into separate, domain-focused modules (e.g., `init-org.el` or `init-global-behaviour.el`) and then required in the main `init.el`. I use [straight.el](https://radian-software.github.io/straight.el/) and the `use-package` macro. I also tend to use the main development branch of Emacs. Those two choices mean I run on the bleeding edge and sometimes run into issues, but using straight.el means I have locally checked-out versions of the packages and can diagnose bugs, if need be.

My reusable Elisp functions are generally located in modules with a similar, domain-focused naming convention: `eds-*.el`. For example, `eds-org.el` contains functions for customising the way `org-mode` works for me.

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

Run the same tests with Undercover instrumentation and print a per-module coverage summary:

```shell
./run-coverage.sh
./run-coverage.sh eds-utils
```

The machine-readable SimpleCov report is written to `coverage/.resultset.json`. The coverage command deletes any previous report first and fails if Undercover records no instrumented files or executed lines. Both scripts use the project Eask environment; using `eask -g` would expose the global Buttercup install without the project's Undercover dependency.

For details of conventions in this repo, see the [Agents configuration](./AGENTS.md).
