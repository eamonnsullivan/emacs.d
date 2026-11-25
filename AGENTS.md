name: emacs-config

Short guide for automated agents working on this repo.

Build / test / lint:
- Before running tests, load `run-emacs-tests` ([skill](.agents/skills/run-emacs-tests/SKILL.md)) for SQLite-enabled Emacs selection, Eask dev dependencies, and focused or full runs.
- If a required tool is missing, inaccessible, or blocked by the sandbox, stop and ask the user before continuing. Name the tool and what it blocks.

Style rules (quick):
- Files: header with "-*- lexical-binding: t; -*-" and trailing provide + footer.
- Use `use-package` + `:straight t` for package config; do not use `package.el`.
- Use `setopt` for defcustoms; use `setq` for ordinary vars.
- Naming: config modules `init-*.el`, libraries `eds-*.el`, tests `tests/test-eds-*.el`.
- Functions/vars: hyphen-separated names, prefix public helpers with `eds-`.
- Error handling: prefer `condition-case` for recoverable errors; signal programmer errors.
- Tests: Buttercup tests, keep each file focused on one eds-*.el library.

Git:
- Use one-line commit messages (no body).

Quick dos and don'ts:
- Do keep `early-init.el` minimal (no package setup).
- Do include docstrings and `(provide 'feature)` at EOF.
- Don't edit auto-generated files (`abbrev_defs`, `diary`) or `straight/`.
- Don't use `custom-set-variables` or the Customize UI.

Agents: follow these commands and conventions; ask if uncertain about running tests.

## Agent skills

### Issue tracker

Issues live in this repo's GitHub Issues. See `docs/agents/issue-tracker.md`.

### Triage labels

Use the five canonical triage labels unchanged. See `docs/agents/triage-labels.md`.

### Domain docs

Use a single-context domain-doc layout. See `docs/agents/domain.md`.
