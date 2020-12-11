# Change Log

## 3.4.2 2020-12-11

- First major round of work for supporting modern truetype and vector fonts backed by freetype2.
  Temporary transition primitive `toggle-fonts` provided to switch between old and new implementation
  in boxer.

  Remaining issues for font work:
    - Cursor location for box names is off. You can type and edit them fine, but the cursor is
      painted a bit off.
    - Advance spacing is still a bit off. Some letters get slightly chopped off, especially when
      italized.
    - Still need to find an open source verdana replacement. As of now it is also rendering with
      Liberation Sans (which is currently being used for Arial).

- Including the Liberation set of truetype fonts: San, Serif, and Monospace.

- Fixed numerous crash fixes.
    - Checking for null input in 'name-string-or-null' before attempting to get slot-value
    - Moving mouse-event defstruct above usage necessary during compilation.
    - Adding nil check in `search-upward-for-visible-row`
    - Generating status-line error rather than system error when trying to insert boxes in a name-row
    - Add nil checks for several chas and screen-rows.

- Updates toward a common-lisp agnostic boxer core than runs in sbcl along with lispworks and others
    - Several common lisp style updates flagged by sbcl (eval-when keywords, defconstant * -> +, etc)
    - Significant cleanup in package.lisp for boxer packages and exports
    - Created boxer-core asdf component for core boxer evaluator engine that runs in straight
      common lisp without dependencies on lispworks opengl, capi, and mp packages.

- Explicitly namespacing calls to lispworks libraries to prepare for general common lisp
  refactoring.

- Lots more continued source cleanup, moving duplicate code to the attic, converting header
  comments to semicolons, tabs to spaces, paredit indentation, cleaning up reader feature macros
  for old lisp platforms.

- Changing evaluator-helpful-message to use boxer-editor-message to avoid constant beeping.

- Minor change for Catalina and LW 7.1.2 to floor integer coordinates before passing them to opengl.

- Build and delivery updates, incuding a shell script and work arounds to ensure we don't
  try and load cl-freetype2 until application startup. The binary MacOS application includes a custom
  build freetype2 2.10.4 dylib with no dependencies on libpng.  Also includes a pre-compiled version
  of cl-freetype2 to avoid invoking compile-file on application startup (removed by lispworks delivery),
  as the cffi groveler compiles these on the fly. These are manual steps and need to be automated
  for the next release.

## 3.4.1 2020-10-29

- Increased scrollbar width a bit for visibility.

- Refactoring various bits to prepare for future work:
  - Namespaced all opengl calls (rather than having them as :use in the :boxer-window namespace)
    Cleaning up the separation between the evaluator and editor to streamline
    future opengl work as well as headless/server boxer and other potential
    display engines.
  - Continue cleanup of file formatting and read macro includes for
    lisp systems/architectures no longer in use.

- Fixed opml export, refactoring export so I can make some improvements and write
  my blog in boxer and export it.

- Boxer-bugs-14 Unifying cut and paste operations while retaining yank.

- Boxer-bugs-22 Added an extra check to ensure the error params are correctly
  interpreted.

- Created the ATTIC to clean up and archive commented out, but interesting
  code.

- Initial fixup of loading boxes over HTTPS. Rewrote previous custom
  code using the well tested `drakma` cl http library.

- Boxer-bugs-13 Fixed default bindings for most (if not all) magic mouse
  commands so they will no longer crash if you don't specify a custom method.
  Fixed return items so they no longer return "Wierd" items.

- Adding additional accelerator keys to the menus
    - Create new Port  cmd-shift-p
    - Flip closet      cmd-shift-os
    - Flip graphics    cmd-}
    - Flip Data/Doit   cmd-]
    - Flip Export      cmd-e
    - Zoom in          cmd-=
    - Zoom out         cmd--

- Boxer-Bugs-17 Adding back in support for changing the mouse cursor
    - In old versions of boxer, there were various options for changing the mouse
      cursors, but it seems to just be stubbed out until now.
    - Adding in support initially again to change the mouse to a crosshair for
      selecting the target of a new port. Added stubs for other keywords found
      around the code base.

- Fixed some of the keyboard macros to enable cross platform keyboard usage again.
  ie. Fixed the keyboard binding on Windows.

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
