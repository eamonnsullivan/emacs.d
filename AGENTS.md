name: emacs-config

Short guide for automated agents working on this repo.

Build / test / lint:
- Run full test suite: ./run-tests.sh
- Run a subset of tests by name pattern: ./run-tests.sh <pattern>  # e.g. ./run-tests.sh eds-utils

Style rules (quick):
- Files: header with "-*- lexical-binding: t; -*-" and trailing provide + footer.
- Use `use-package` + `:straight t` for package config; do not use `package.el`.
- Use `setopt` for defcustoms; use `setq` for ordinary vars.
- Naming: config modules `init-*.el`, libraries `eds-*.el`, tests `tests/test-eds-*.el`.
- Functions/vars: hyphen-separated names, prefix public helpers with `eds-`.
- Error handling: prefer `condition-case` for recoverable errors; signal programmer errors.
- Tests: Buttercup tests, keep each file focused on one eds-*.el library.

Quick dos and don'ts:
- Do keep `early-init.el` minimal (no package setup).
- Do include docstrings and `(provide 'feature)` at EOF.
- Don't edit auto-generated files (`abbrev_defs`, `diary`) or `straight/`.
- Don't use `custom-set-variables` or the Customize UI.

Agents: follow these commands and conventions; ask if uncertain about running tests.
