---
name: run-emacs-tests
description: Run this repo's Buttercup tests with Eask. Use for focused or full test runs, missing Buttercup development dependencies, or Org-roam and Vulpea failures caused by an Emacs build without SQLite.
---

# Run Emacs tests

1. Work from the repository root, next to `Eask` and `run-tests.sh`. Check `command -v eask`.
2. Pick a SQLite-enabled Emacs before installing dependencies or running tests. Probe the selected executable:

   ```sh
   "${EMACS:-emacs}" --batch -Q --eval '(princ (and (fboundp (quote sqlite-available-p)) (sqlite-available-p)))'
   ```

   Expect `t`. In the Linux ECA sandbox, `/usr/bin/emacs-nox` passed this probe even when `emacs` on `PATH` returned `nil`. Use the probed executable for both dependency installation and test runs via `EMACS`. The commands below use the verified sandbox path; substitute the executable that passed on your machine.
3. On a fresh workspace, after changing Emacs versions, or if Buttercup or another declared test dependency cannot load, install the development dependencies:

   ```sh
   EMACS=/usr/bin/emacs-nox eask install-deps --dev
   ```

4. Run a focused pattern against Buttercup spec names, or omit the pattern for the full suite:

   ```sh
   EMACS=/usr/bin/emacs-nox ./run-tests.sh eds-org-capture
   EMACS=/usr/bin/emacs-nox ./run-tests.sh
   ```

   Check the exit status and Buttercup summary. A focused run must execute at least one spec; discovery alone is not a pass.

## Stop and ask

Follow the blocker rules in `AGENTS.md`. If Eask, a SQLite-enabled Emacs, or another required tool is unavailable, name what is missing and which command it blocks. If missing npm registry authentication blocks private packages, ask the user to restore access without sharing credentials or tokens. Distinguish setup blockers from failing specs; report the test counts that actually ran.
