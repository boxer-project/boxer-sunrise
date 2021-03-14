# Change Log

## 3.4.4 2020-03-12

- boxer-sunrise-16 Minor improvements to scrollbar rendering. Removing tiny up/down buttons.

- boxer-sunrise-6 Fix annoying default file name issue with Windows 10 File Chooser.
  - Updating default pathname to be a directory without default foo.bar file.

- boxer-sunrise-15 Removing currently un-needed preferences for Email setup and keyboard device configuration (lwm)

- boxer-bugs-39 Adjusting sizes to fix zooming issues.
  - Includes a crash fix when zooming down to the lowest size.
  - Fixes issue where fonts at a size 28 would render at size 8.

- boxer-bugs-3 Adding support for scroll gestures from trackpad gestures or mouse scroll wheels.

- boxer-sunrise-9
  - Updates to hover over renderings. Including some minor color contrast similar to macos max/min
    buttons and circles with solid borders for contrast
  - Cleaning up box corner clicks, refactoring pop-up menus, implementing new menu item designs.
  - Standardizing keyboard bindings across MacOS, Windows, Linux. The only minor differences going forward should stem
    from differences in the OS key (Windows, Command, etc) and Option rather than Alt, and sutble cultural differences
    as to when things should belong to Control vs Command (ie. cut n paste, etc)
  - Fixing up top right/left, bottom left corner menus based on revised designs from Andy

  - Removing mouse delays for toggling graphics and box type.
    - Toggling graphics already had a defvar *slow-graphics-toggle*, switched
      this to nil
    - Introduced a new defvar *slow-box-type-toggle* to control behavior for
      toggling box types.

- boxer-sunrise-13 Cleaning up keyboards between OS's to simplify setup.

- boxer-sunrise-12 Linux Support
  - Minor changes to allow starting up under 64-bit Lispworks for Linux/GTK.

- boxer-sunrise-11 Windows 10 Support
  - Updating start script to take in to account windows drive letters for paths, as well as the slightly
    difference directory sturcture, and load 64ofasl files for windows and 64xfasl files for MacOS
  - Minor refactoring for included library paths.

- boxer-sunrise-7 First set of work on updated HTML5 export.

- boxer-sunrise-3 Minor refactoring of JSON export to share with HTML and other export types.

- crash-fixes
  - Fixed an issue with colors not being correctly initialized before used in the opengl context
  - Added a special check for some old microworlds storing fonts with relative sizes and starting
    with font size zero.
  - Adding extra check to avoid division by zero with max-scroll-wid math

- Continued formatting fixes and improvements across source.

- the-attic
  - Moving lots of pre-opengl drawing routines from `new-borders.lisp`, `disply.lisp`, `popup.lisp` to the attic.
  - Lots of other minor removals from deprecated platforms.

## 3.4.3 2020-01-27

- boxer-sunrise-3 First set of work on JSON export format

- boxer-sunrise-4 Menu reorganization and open recent
    - Adding boxapp-data.lisp, which will store session data for boxer in
      the OS appdata dir (on MacOS will be ~/Library/Application Support/Boxer),
      such as recently opened files and such.
    - Re-organizing new, open, mark box, and save menus based on UI suggestions
      from Andy.

- boxer-sunrise-5 Major refactoring and simplification of fonts, removing old bitmap font support as well as
  abstraction layers from 2 other old font implementations.

- boxer-bugs-26 Fixes some crashes with higher unicode codepoints.
  - Several strings were being created with element type base-char.
  - Changes a make-string call in chunker.lisp to :element-type 'character
  - Changes an array for with-output-to-string in comsa.lisp to :element-type 'character

- boxer-bugs-27 Removed an erronesous 1 extra padding of pixel when calculating each
  glyph width that was causing the cursor to move forward faster than the text, as
  box names are actually rendered as a single pixmap.

- boxer-bugs-28 Fixed the CHA-HEI functions, recentering the box names.

- MacOS Big Sur
  - Applied a patch from lispworks that fixed the issue that was causing Boxer to not start up on Big Sur.

- MacOS High Sierra
  - Recompiled the libfreetype.6.dylib library on High Sierra that was causing Boxer to crash on startup.
    The same Boxer app should be working now across all Intel versions of High Sierra, Mojave, Catalina, and Big Sur.

- Windows 10 support. Fixed opengl rendering issue with pixmaps on Windows as well as several code loading issues. Close
  to fully supported now.

- crash-fixes
  - Fixed a crash that occured when clicking on the top level WORLD name row. (You can't rename the top box name.)
  - Fixed a crash that occured when trying to calculate the width of a port actual-obj that didn't exist.

- the-attic Lots of continuing cleanup in `mouse.lisp`, `comsb.lisp`, `grobjs.lisp`, `sysprims.lisp`,
  `graphics-clear.lisp`, `comdef.lisp`, `makcpy.lisp`, `mousedoc.lisp`, `boxwin-opengl.lisp`, `realprinter.lisp`,
  `comp.lisp`, `bind.lisp`,

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
