# AGENTS.md

## Build, Lint, and Test
- **Lint/Check:**
  - Emacs (elisp): Use Flycheck (enabled globally; see `init-flycheck.el`).
  - JS/TS: ESLint via Flycheck (`eslint` setup, runs on save).
  - Scala: scalastyle via Flycheck.
  - Clojure: clj-kondo via Flycheck.
- **Format:**
  - EditorConfig and `dtrt-indent` enforce whitespace. Prettier for JS/TS (`prettier-mode` in Emacs). Yapf for Python.
- **Test:**
  - Emacs Lisp: Use ERT.
    - Run all: `M-x ert RET t RET`
    - Single test: `M-x ert RET <test-name> RET`
    - (e.g., `eds/test-...`)
  - External (JS/Scala/etc): Use respective project tools (e.g. `npm test`, `sbt test`).

## Code Style Guidelines
- **Imports/Requires:**
  - Group at file top, use `use-package` for setup when possible.
- **Formatting:**
  - Spaces only (no tabs), EditorConfig standard.
- **Types / Annotations:**
  - Use types and annotations where the language supports them (esp. Scala, TS, etc.).
- **Naming:**
  - Emacs Lisp: kebab-case for files and public functions, prefix user functions with `eds/`.
  - JS/TS: camelCase for functions, PascalCase for components/classes.
  - Python: snake_case.
  - Test functions: `eds/test-*` for elisp.
- **Error Handling:**
  - Use explicit error messages (`user-error` or `error` in elisp). Use `console.log`, `fprintf`, or equivalents as per abbreviations in comments.
- **General:**
  - Keep cross-platform. Use keybindings via `use-package`. Dependency management via straight.el.

_This guide is for coding agents to standardize operations in this repository._
