# emacs.d

My Emacs initialisation. This is unlikely to be of use to anyone else, except perhaps as sample code. I have made exactly zero effort to make this configuration usable by others.

The config is broken into separate, domain-focused modules (e.g., `init-org.el` or `init-global-behaviour.el`) and then required in the main `init.el`. I use [straight.el](https://radian-software.github.io/straight.el/) and the `use-package` macro. I also tend to use the main development branch of Emacs. Those two choices mean I run on the bleeding edge and sometimes run into issues, but using straight.el means I have locally checked-out versions of the packages and can diagnose bugs, if need be.

My reusable Elisp functions are generally located in modules with a similar, domain-focused naming convention: `eds-*.el`. For example, `eds-org.el` contains functions for customising the way `org-mode` works for me.

## External tools

The configuration assumes `git` is installed and on `PATH`. It is needed by straight.el to fetch packages and by the Git integrations.

Other commands are needed only when their corresponding features are used:

| Command | Used for |
| --- | --- |
| `cfn-lint` | CloudFormation Flycheck checks |
| `clj-kondo` | Clojure Flycheck checks; the integration is disabled when the command is absent |
| `copilot-language-server` | ECA with the GitHub Copilot provider |
| `dot` | Org Babel Graphviz blocks and Org-roam graphs |
| `gh` | GitHub Actions and pull-request commands; it must also be authenticated |
| `grep`, `find`, `locate`, `man`, `rg` | Built-in and Consult search commands |
| `metals` | Scala language-server support through Eglot |
| `mplayer` | Pomidor timer sounds |
| `msmtp` | Sending mail from mu4e |
| `mu` | Searching and indexing mail for mu4e |
| `nautilus` | Opening the current directory on Linux (`open` is used on macOS) |
| `pandoc` | Converting Markdown buffers to Org |
| `sbt` | Scala build commands through sbt-mode |
| `trash` | Moving files to the macOS Trash |

Org Babel also has shell, Scheme, JavaScript, TypeScript, Clojure, Haskell, Python, and Go support enabled. Running those source blocks requires the relevant language runtime, but this repository does not configure a specific executable for each one.

Some related assumptions are fixed paths rather than `PATH` lookups:

- macOS GNU `find` is expected at `/opt/homebrew/opt/findutils/libexec/gnubin/find` (Linux uses `/usr/bin/find`).
- PlantUML is expected at `/opt/homebrew/bin/plantuml`.
- mu4e is expected under `/opt/homebrew/share/emacs/site-lisp/mu/mu4e`.
- Mail synchronisation uses `~/bin/sync-mailboxes.sh`.

## Tests and coverage

Development commands require `eask` and `emacs` on `PATH`. The GitHub issue workflow described under `docs/agents/` also requires authenticated `gh` access.

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
