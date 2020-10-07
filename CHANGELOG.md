# Change Log

## 3.4.0 2020-10-06

- Boxer-Bugs-10 Fixed circular port rendering

- Boxer-Bugs-7 Fixed up `boxer-version` command, updated so it automatically pulls the
  version from the asdf component.

- Removed old expiration code. 

- Fixed boxer-function-arglist issues. Fixed issue causing crash (rather than error), 
  when not supplying the correct number of arguments to a boxer procedure.

- Continuing cleanup: Convertings mixed space/tab indenting to spaces, reformatting with
  paredit, removing conditionally included code from lisp implementations that are no 
  longer in use.