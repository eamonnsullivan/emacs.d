# GitHub Copilot Instructions for emacs.d

This is Eamonn Sullivan's personal Emacs 31+ configuration repository, maintained since 2017.

## Core Principles

- **Package management**: Use `straight.el` exclusively (never `package.el`)
- **Package configuration**: Use `use-package` with `straight-use-package-by-default t`
- **Testing**: Use Buttercup for testing (NOT ERT)
- **Lexical binding**: Always include `lexical-binding: t` in file headers
- **Platforms**: Support both GNU/Linux and macOS (Apple Silicon via Homebrew)

## Repository Structure

### Configuration Files
- `init.el` — Main entry point, bootstraps straight.el, requires all modules
- `early-init.el` — Pre-GUI startup only (keep minimal)
- `minimal-init.el` — Minimal init for debugging

### Module Organization
- `lisp/init-*.el` — Configuration modules (one per feature/package group)
- `lisp/eds-*.el` — Custom utility libraries with business logic
- `tests/test-eds-*.el` — Buttercup test files (one per `eds-*.el` library)
- `snippets/` — YASnippet definitions

### Auto-Generated Files (DO NOT EDIT)
- `abbrev_defs`
- `diary`

## File Header Format

Every `.el` file must start with:

```emacs-lisp
;;; filename.el --- One-line description -*- lexical-binding: t; -*-

;; Copyright (C) <year> Eamonn Sullivan <me@eamonnsullivan.co.uk>

;; Author: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Maintainer: Eamonn Sullivan <me@eamonnsullivan.co.uk>
;; Version: 0.1
;; Package-Requires: ((emacs "31.0"))
;; Keywords: <relevant keywords>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; <Brief description of what this file does>

;;; Code:
```

Every `.el` file must end with:

```emacs-lisp
(provide 'feature-name)
;;; filename.el ends here
```

## Coding Conventions

### Variables and Options
- Use `setopt` for user-customizable options (defined with `defcustom`)
- Use `setq` for regular variables and non-`defcustom` settings

### Package Configuration Pattern

```emacs-lisp
(use-package some-package
  :straight t
  :after other-package
  :hook (some-mode . some-package-mode)
  :custom
  (some-package-option "value")
  :config
  (some-package-setup))
```

### Platform-Specific Code

```emacs-lisp
(cond
 ((eq system-type 'gnu/linux)
  ;; Linux-specific setup
  )
 ((eq system-type 'darwin)
  ;; macOS / Homebrew setup at /opt/homebrew/
  ))
```

## Adding New Code

### New Configuration Module (`init-*.el`)
1. Create `lisp/init-foo.el` with proper headers/footers
2. Add `(require 'init-foo)` to `init.el`
3. Use `use-package` for all package setup
4. End with `(provide 'init-foo)`

### New Utility Library (`eds-*.el`)
1. Create `lisp/eds-foo.el` with proper header and docstrings
2. Create `tests/test-eds-foo.el` with Buttercup tests
3. Require the library from an appropriate `init-*.el` module
4. Run tests with `./run-tests.sh`

### Test Format (Buttercup)

```emacs-lisp
;;; test-eds-foo.el --- Tests for eds-foo  -*- lexical-binding: t; -*-

(load-file "tests/setup.el")

(describe "eds-foo/some-function"
  (it "does something when given input"
    (with-temp-buffer
      (insert "test content")
      (goto-char (point-min))
      (eds-foo/some-function)
      (expect (buffer-string) :to-equal "expected result")))

  (it "handles edge cases"
    (expect (eds-foo/some-function nil) :to-be nil)))

(provide 'test-eds-foo)
;;; test-eds-foo.el ends here
```

### Buttercup Testing Patterns

- Use `describe` to group related tests
- Use `it` for individual test cases
- Use `expect` with matchers like `:to-equal`, `:to-be`, `:to-match`, etc.
- Use `spy-on` for mocking functions
- Use `:var` in `describe` blocks to declare spies
- Load test setup with `(load-file "tests/setup.el")`

## What NOT to Do

- ❌ Never edit `abbrev_defs` or `diary` (auto-generated)
- ❌ Never edit anything under `straight/` (managed by straight.el)
- ❌ Never use `package.el`, `package-install`, or MELPA `package-initialize`
- ❌ Never add package configuration to `early-init.el`
- ❌ Never use `custom-set-variables` or the Customize interface
- ❌ Never omit `lexical-binding: t` from file headers
- ❌ Never use ERT for tests (use Buttercup instead)

## Key Modules

- `init-git.el` — Magit and git tooling
- `init-eglot.el` — LSP via Eglot
- `init-org.el` — Org-mode configuration
- `init-clojure.el` — Clojure / CIDER
- `init-copilot.el` — GitHub Copilot integration
- `init-appearance.el` — Themes, fonts, modeline
- `init-global-behaviour.el` — Core editor settings
- `init-global-bindings.el` — Global key bindings
- `init-platform.el` — Platform-specific settings

## Testing

Run the test suite with:
```bash
./run-tests.sh
```

Tests use Buttercup and are organized by library under `tests/`.
