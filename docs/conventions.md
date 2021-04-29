# Coding and Documentation Conventions

## Git Commit Message Prefixes

Each commit message should ideally begin with one of the following prefixes:

- `boxer-sunrise-{num}` For a commit addressing a ticket number in the github issues for the repo, use this with the
  number of the ticket. ex: `boxer-sunrise-42 Implemented meaning of the universe`
- `boxer-bugs-{num}` As part of our efforts of dogfooding Boxer, some bugs and issues are logged in a Boxer based
  database. These should be indexed by their number in that database.
  ie. `boxer-bugs-35 Fixing key/mouse help menu`
- `format` For commits that are re-indenting code or comments with no impact on behavior can simply be prefixed with
  `format`
- `crash-fix` Commits that fix a minor issue that caused a system crash, but was not part of another ticket, such as
  a bad argument type, division by zero, etc, can be prefixed with `crash-fix`
- `re-org` Commits that are simple moving around files or directory structure can begin with `re-org`
- `the-attic` Commits which are retiring old unused code to the attic can be prefixed with `the-attic`. This involves
  code that was commented out, or not included via `#+` reader macros for some platform that no longer exists. If the
  code is interesting, it's usually pasted in the `the-attic.lisp` so it can be searched easily in the future.
- `doco` - Any commits adding or updating documentation... such as this file.
