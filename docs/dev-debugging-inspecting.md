# Development Debugging and Inspecting

## Inspecting the box data structures

When running boxer, the following global variables allow access to viewing the current running state, and these can
be accessed from a REPL.

- boxer::*initial-box* This is the top level root box for the entire world.
- boxer::*outermost-screen-box* This is whichever box is current full screened in the world.
- boxer::*point* This contains a structure which includes the current row and screen-box of the cursor/point which can
  be inspected.

