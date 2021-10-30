# Development Debugging and Inspecting

## Inspecting the box data structures

When running boxer, the following global variables allow access to viewing the current running state, and these can
be accessed from a REPL.

- boxer::*initial-box* This is the top level root box for the entire world.
- boxer::*outermost-screen-box* This is whichever box is current full screened in the world.
- boxer::*point* This contains a structure which includes the current row and screen-box of the cursor/point which can
  be inspected.

## Restarting the boxer canvas after investigating an error

After debugging/inspecting an issue stopped execution while running Boxer in Lispworks, the world canvas can
be restarted at the REPL with:

```
(bw::boxer-process-top-level-fn bw::*boxer-pane*)
```

## Using `trace`

 (trace (boxer::reset-region :break (equal boxer::*debug-reset-region* t)))

## Dumping the contents of a Box file

Dumping the version and opcodes of a `.box` file can be done at the REPL with:

```

```

## Changing the keybindings at runtime

The `make-input-devices` defun from `keydef-high.lisp` is what sets up all the keybindings and
can optionally rebind all the keys (if being run again after the initial boxer load and setup).
This is also what is called when we have the machine platform option in the user preferences
accessible from the top menu.

```lisp
;; Using in initial setup:
(make-input-devices *initial-platform* nil)

;; Rebinding later in the application:
(make-input-devices canonicalized-name) ; such as :LWM
```
