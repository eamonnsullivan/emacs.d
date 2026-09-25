# Emacs Configuration

Personal Emacs workflows and conventions encoded by this configuration.

## Org agenda

**Agenda eligibility**:
Whether an Org file belongs in the agenda. A file is eligible when it contains an unfinished TODO or is an explicitly included file.
_Avoid_: Agenda membership

**Active work**:
Any heading whose TODO keyword is not in `org-done-keywords`; scheduling, nesting, and file location do not affect this status.
_Avoid_: Open task, pending item

**Agenda marker**:
The top-level `agenda` file tag derived from agenda eligibility. It speeds discovery but is not the source of truth.
_Avoid_: Agenda tag

**Explicit agenda file**:
An Org file included in the agenda regardless of active work. `calendar.org` is the only explicit agenda file.
_Avoid_: Permanent agenda file
