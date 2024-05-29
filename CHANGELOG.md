# Change Log

## 3.4.18 2024-05-26

This is the first release of our reworked scrolling and rendering layout, replacing our old single screen
style scrolling, with a more modern canvas that expands to the size of your contect, and uses OS scrollbars
on the top level window to move around. Additionally, the scrolling is now smooth (rather than one line at
a time), Boxes maintain their horizontal widths when scrolling, plus many other modernizations to bring Boxer
in line with how you'd expect a modern desktop authoring application to pan, scroll, and move.

Part of this includes a total overhaul of the first pass rendering layout algorithm, along with lots of
other refactoring and cleanup of old code and technical debt. This rework will continue to make it easier
and faster to implement newer interactions and graphics on the system.  It's very exciting for future work!

Do expect some glitches and occasionall rendering bugs as this is wrapped up in a series of smaller bug
fix releases over the next few months.

### Full Change Log

sunrise-11 Smooth Scrolling and New Pass-1 Repain Layout Algorithm
  - Adding outer level OS scrollbars to the boxer-pane
  - cleanup Removing old av-info checks from repaint-2 algorithm.
  - Changing status bar from Font Zoom to real Zoom
  - Viewport document offset methods for boxer canvas.
  - Removing usage of slot 'old-graphics-sheet
  - Adding scroll bar translations to blinker drawing.
  - Adding set-transform defmethod to boxgl-device
  - Adding *document-mouse-x* and y to track translated mouse location
  - Using adjust-transform instead of window-system-dependent-set-origin
  - Removing unneeded %draw-rectangle clip and repaint-1 with-drawing-inside-region
  - Cleaning up com-open-link-file from old explicit-redisplay and commented out with-drawing-port reference
  - Routines for our usage of openGL Stencil Buffers.
  - Factoring openGL enables/disables into a single defun
  - Adjusting lispworks openGL config for alpha/depth/stencil configs
  - Totally retiring the source for the old layout algorithm.
  - Optimization for stenciling transforms
    Currently our stencil calculations always draw from
    the origin 0, 0 origin. Adding a shader just to draw
    without the global uniform transform so we don't need
    keep updating the buffer for each stencil when wrapping
    code sections in with-clipping-inside
  - Small optimization to stop looking up the font-face unless we really need to create a new glyph.
  - Removing unused member max-scroll-wid from defclass screen-box
  - Performance improvements for rendering large screens/boxes
    For both content that is scrolled globablly and also locally inside
    a manually sized box, we check that the row is vertically inside the
    box/window before rendering it.
  - Repinning global scrolling/transform to top-left when resizing.
  - Fixing up math for scrollbar limits when dragging box scrollbars
  - Breaking out scroll limits into their own defuns and applying to trackpad scrolling.
  - Fixing defmethods scroll-dn-one-screen-box, scroll-up-one-screen-box
  - Moving screen-box page up/down scrolling to oglscroll.lisp
  - Fixing up defboxer-command COM-SCROLL-DN-ONE-SCREEN-BOX and UP
  - Fixing cursor repainting for new scroll rendering.
  - Fixing up cursor and region rendering so they occur in the actual clipped box.
  - Fixing scroll drag maths for box scrollbars.
  - Fixing rendering interpretation of scroll-to-actual-row and some zoom maths
  - Fixing up slug size and position on global scroll bars.
  - Fixing up scrollbar math on outermost window OS scrolling.
  - Fixing zoomed wid/hei %clip-lef in drawing-on-window-bootstrap-clipping-and-scaling
  - Fixing recursive stencil buffering by adding the wanted stencil-func before calling glClear midway.
  - wip updating defmethod ensure-row-is-displayed
  - Fixing two finger scrolling.
  - Refactoring lispworks version of boxer-canvas

minor-fix
  - Removing usage of a fixnum operator that sometimes got floats.

refactor
  - Removing duplicated code that is just defun check-for-window-resize
  - Moving capi pane scrolling code to it's own file.

debug
  - Adding an outline tree view pane of screen-objs for development.

doco
  - Documentation strings for scroll-y-offset and scroll-x-offset.

the-attic
  - Removing unneeded with-scrolling-origin
  - Removing unused %local-clip-lef/top/rig/bot, %clip-total-height
  - Refactoring defun window-system-dependent-set-origin (h v) to
    defmethod adjust-transform ((self boxgl-device) h v)
  - Removing redundant defmacros with-scrolling-origin, with-turtle-clipping
  - Removing empty defmacro prepare-sheet
  - Retiring defvar %drawing-array
  - merging with-window-system-dependent-clipping into with-clipping-inside
  - Retiring scale-x and scale-y
  - Archiving defmacro with-mouse-tracking-inside, and defun warp-pointer
  - Archiving unused defun mouse-position-screen-values
  - Removing old commented bits that were erasing parts of screen...
  - Archiving unused invalidate-absolute-position-caches
  - Archiving defmacro with-graphics-screen-parameters, drawing-on-turtle-slate
  - Simplifying drawing-on-window, archiving drawing-on-window-without-prepare-sheet
  - Archiving with-drawing-inside-region and tutti frutti random color debugging.
  - Archiving defun update-absolute-pos-cache
  - Removing old unused ab-pos-cache
    - defvar *absolute-position-caches-filled*,
    - defstruct ab-pos-cache
    - defmacro with-absolute-position-cache-recording
    - member cached-absolute-pos from screen-box
  - Archiving defun my-clip-rect
  - Archiving av-stubs and av-info.
  - Removing defun com-hotspot-unfix-box-size
  - Archiving binary style application extensions.
  - Cleaning up unused and retired bits (erasing, etc) from disply.lisp
  - Archiving code for printing/exporting to postscript
  - Archivign unused defmethod VISIBLE? screen-obj
  - Removing now unused screen-box-is-scrollable?

## 3.4.17 2024-03-02

Very minor release with a single bugfix that was causing problems saving certain older files.

crash-fix Saving :cached-boxtops with old graphics commands

    Working on an issue from the most recent build, where there are old graphics lists that are stored
    in box plists under :cached-boxtop that aren't getting translated, so it's still trying to save the old
    lower ( >32 ) commands, but are blowing up.

    To fix this I put in a check for lower opcodes, and then fetch the higher level graphics-command to
    use with dump-gc.  This is ok, after reviewing all the defmethod dump-gc implementations, none of
    actually modify the original opcode when saving, and the special ones mostly deal with things like
    bitmaps and whatnot. (fonts, colors, bitmaps, alu).

## 3.4.16 2024-02-26

This is a small bug fix and maintenance release while larger work on zooming and smooth scrolling is occuring in
another branch. We've added the current filebox name and file status back to the window title bar. A problem looking
up glyphs widths has been fixed, which was very noticable in Courier New and monospace fonts. Hollow rectangle stamps
have been fixed and added back to turtle graphics. Infrastructurally, we finally updated our notarization workflow to
notarytool.  Outside of that, the remainder of the old school defboxer-graphics-command macros were moved over to CLOS
classes with a few tweaks.

bugs-150 Putting file status back on title bar and adding save file toolbar icon.

bugs-194 Fixing font-face name capitalization hash lookups for generating the glyph texture atlas.

sunrise-71 Adding missing graphics command for hollow rectangles.

sunrise-69
  - Moving some remaining extents and size comments over to the newer boxer graphics commands.
  - Retiring defgraphics-state-change, expand-mutators-and-body. Fixed issue with pen-size not taking effect.

infrastructure
  - Updating notarization to use notarytool from altool.

clean-up
  - Removing older version of lispworks opengl bindings

minor-fix
  - Updating delivery script with proper macosx read macro include.
  - Typo in name 'record-boxer-graphics-command-centered-bitmap' (missing centered)

the-attic
  - Fully removing the remaining bits of defstandard-graphics-handlers and defgraphics-command
  - Archiving unused *add-new-graphics-sheet-bit-array?*

## 3.4.15 2023-10-28

This release is largely cleanup from the last build, fixing a few annoying bugs introduced that were causing
crashes, such as fewing shrunk boxes that had cached boxtops still using old graphics list commands.
Other minor fixes include color transparency for circles, ellipses, and arcs in turtle boxes, and framebuffer
improvements for Intel.

Of notable mention for this build is a good deal of cleanup that allows us to now load the entire code base
into both ECL and SBCL, and passing some of the lisp unit tests. This doens't include any UI windows yet,
but is a major first step toward portability and moving towards an integrated future (such as running in a
web browser). Additionally, this month we fixed the Windows build and have generated a delivered
executable for it again. Do note this is extremely experimental, many obvious things are broken (such as
numerous places with image support), but it's likely it should be up to grade by year end.

Lots of other under the covers refactoring includes continued cleanup of the older graphics commands
macros for turtle primitives to streamline future graphics works and effects.

### Full Change Log

sunrise-69 Cleaning up extents calculations and graphics-command macros
  - Initial removal of :extents-form in preference of :boxer-extents-form
  - Removing turtle translators.
  - Removing process-graphics-command-marker and translate-and-scale-graphics-command machinery.
  - Cleaning up graphics commands by adding defclass graphics-command and defmacro defdraw-graphics-command
  - Removing *graphics-command-translation-table* and *graphics-command-boxer->window-translation-ta
ble* along with their related macros and functions.
  - Moving gc list copying over the graphics-command class
  - Moving extents functions to new graphics-command defclass
  - Removing *graphics-command-dispatch-table* and related machinery, as well as remaining copy-post-processin
g references.
  - Moving load-form code over to graphics-command class from *graphics-command-loader-dispatch-table*
  - Moving dump-form code over to graphics-command class from *graphics-command-dumper-dispatch-table*
  - Moving deallocate-form code over to graphics-command class from *graphics-command-deallocation-table*
  - Removing unused graphics-command-slot-offset, graphics-command-values
  - Removing *graphics-command-binding-values-table*, graphics-command-slot-offset, graphics-command-values
  - Removing *graphics-command-binding-values-table*, graphics-command-slot-offset, graphics-command-values
  - Removing *graphics-command-descriptor-table* and related functions.
  - Moving *graphics-command-sprite-command-translation-table* functionality to defclass graphics-command.
  - Removing *graphics-command-name-opcode-alist*
  - Moving recording defuns to boxer-graphics-command.lisp

sunrise-11 Fixing windows support
  - For some reason the opengl-pane members need to be redefined on the subclass boxer-lw-opengl-canvas
  - #-win32 Some opengl operations that still need fix ups
  - Disambiguating u_resolution uniform which was breaking on win32
  - A few windows fixes in the delivery paths and keyboard handling

sunrise-20 Common Lisp platform cleanup
  - Removing constant usage from lispworks opengl to cl-opengl symbols.
  - Temporarily adding #+lispworks to several things to get loading and some tests to run on ECL, SBCL

bugs-188 Fixing transparency for shader shapes: circle, ellpse, arcs

bugs-191 Removing the Repaint item from the Help menu for now

cleanup
  - Removing commented out stuff from resize-graphics-sheet

refactor Moving repaint-pass-2 methods to their own file

minor-fix
  - Fixing up top-x-intercept call arguments.
  - Adding check in boxer-playback-graphics-list for old <32 commands that are lurking in cached boxtops.
  - Framebuffer fixes for macOS/intel
    - Defaults for ARM didn't cause issue. Adding stencil-buffer to glClear calls. Wrapping
      resize framebuffer updates in the graphics-canvas enable/disable
  - Always updating mouse-status x and y coordinates to fix rendering hotspot corners.
  - Always updating mouse-status x and y coordinates to fix rendering hotspot corners.

the-attic
  - Removing defmethod synchronize-graphics-state-for-erase and usage in set-pen with erase-color.
  - Obsoleting prims pe and penerase
  - Removing :command-body from graphics commands.
  - Removing unused ensure-legal-window-coordinate

## 3.4.14 2023-09-12

This alpha release fixes and cleans up numerous items in the turtle graphics and sprites layer, along with
some other minor fixes.  Most importantly it includes a large refactoring which puts drawn turtle graphics
and sprites in a graphics box back in to the same coordinate system.  Due to historical platform reasons,
lines and shapes drawn with turtles were immediately converted into an integer topleft coordinate space while
sprites were kept in a 0,0 float space, which would cause them to loose precision and float apart when the
graphics box was resized. This results in a major simplification of the turtle infrastructure making it
easier to perform modern graphics "tricks".

Additionally, ellipses and similar shapes are now available for use again! (these have been missing for a while).
Line width on hollow ellipses, arcs, and circles have been fixed.  Sprites made with pixmaps and other shapes
can now also rotate and scale based on the sprites properties, which is a major improvement.  A caveat is that
ellipses and few shapes do not rotate yet, but their shaders should be fixed to do this in the next release.

Part of this work included adding a new graphics list command, the first new one in several decades.  Graphics
list commands underpin the record keeping for any drawn turtle graphics. The new command is `BOXER-TRANSFORM-MATRIX`
allowing an arbitrary 4x4 transformation matrix to be added to the graphics list to rotate, scale, and translate
any following commands:

```
37   BOXER-TRANSFORM-MATRIX     (MATRIX) 4x4 matrix packed in 1x16 single-float vector
```

This is quite exciting, because it gets us very close to having all the graphics operations existing in a modern
3d compatible GL environment, such that we could have 3d turtle environments and perform other interesting visual
effects.  Because of this added graphics command, the box file format version has been increased to v14 for this
build.

Other fixes include primitives for fetching screen colors like `color-at`, numerous crash fixes, dissapearing row
text render fixes, and improvements in syncing between a Sprites actual rendering and it's preview render in it's
shape box (another major refactor that has simplified the system).

The update of the toolbar when starting a long running evaluation has been fixed, the run/stop icon properly changes
now.  The internal color representations have been simplified as well.

### Full Change Log

sunrise-25
  - Putting box borders on the GL Model Meshes
  - Putting box names on the gl model.

sunrise-67 Fixing up freeze primitive so that it clears any attached graphic canvas framebuffers.

sunrise-68 Removing ogl-colors and replacing with RGBA vectors
  - centralizing color functions in color.lisp
  - Removing ogl-color conversion routines in favor of using rgb vectors.
  - Fixing toolbar color comparisons.
  - Fixing pixel->color
  - Changing precision to check color portions that max at 255.
  - Removing remaining usage of ogl color->pixel
  - Cleaning up change-graphics-color

bugs-54 First adjustments to boxer coords for stamp-dot and stamp-rectangle
  - Adjusting bitmap recording, fixing dot and bitmap drawing.
  - Fixing up coords for type recording, filled and hollow circles, hollow and filled ellipses, arcs and wedges,
    move-to, loading of colors and translating window->boxer turtle coords on load, stamp primitives, freeze,
    model and inverse matrix on dub-graphics-list, cached pen color/width/font in graphics display list framebuffer
    canvas's.
  - Converting window-coords graphics commands to boxer-coords on first render.
  - Reworking draw-line-wrap for native boxer turtle coordinates.
  - Fixing positioning for type, ltype, rtype primitives.
  - Adding translate? option to boxer-playback-graphics-list to fix boxtops.
  - Always in :boxer recording mode now, switching *graphics-command-recording-mode* to ':boxer from ':window
  - Completely removing allocate-boxer->window-command
  - Massively cleaning up resize-graphics-sheet
  - Archiving the venerable playback-graphics-list-internal
  - Changing :window-boxer template to single-float. Checking for :rgb vector in reallocate-pixel-color
  - Updating graphics-canvas resize to use global *update-bimap?* to avoid framebuffer lag on mouse resizing.
  - Moving repaint inside *update-bimap?* nil context to avoid framebuffer resizes during repaint when mouse resizing.
  - Adding new graphics command 37   BOXER-TRANSFORM-MATRIX
    - 37   BOXER-TRANSFORM-MATRIX     (MATRIX) 4x4 matrix packed in 1x16 single-float vector
      In order to fix/support stamp-self with the same rotations/scaling/etc with bitmaps and all
      primitives we need to move beyond the simple hand transforms. This graphics display list
      command allows putting an arbitrary 4x4 transformation matrix in the graphics list to
      affect upcoming commands.
  - Cleaning up sprite extents and draw-update removal
    - Simplifying and reworking enclosing-rectangle and other extents functions
    - Removing draw-update which no longer is used with sprite shapes
  - Improving enclosing-rectangle with proper scale/rotation/translating.
  - Improving wid/hei calculations for the SHAPE graphics-sheet dimensions.
  - When drawing a sprite, override the global %draw-half-width and height
    - This is so wrap-line respects the dimensions of the sprites shape, rather than using
      the graphics-sheet of the containing graphics box. This can make a big difference if
      the sprite is scaled/rotated/etc.

bugs-169 Fixing run/stop toolbar icon update during evaluation
    During evaluation we may not be in the same process/thread as the
    capi widgets. Changing the toolbar update to apply in the boxer pane
    process, fixing it changing to the 'stop' icon once we're in evaluation
    from the boxer canvas.

bugs-171 Fixing issues with freeze and gl textures getting updated
    There are a number of operations which change the pixel data in an
    ogl-pixmap. This ticket adds a flag to the class indicating if the texture
    needs updating because the data has changed, along with the necessary
    checks for this in various pixmap operations and in gl-add-pixmap.

bugs-172 Creating accessors for screen-obj-wid and hei to ensure the value is always a fixnum.

bugs-176 Fragments shaders for hollow/filled circles/arcs/ellipses

bugs-182 Removing window-shape usage
  - defstruct turtle-window-shape
  - defuns turtle-window-shape?, make-turtle-window-shape, flush-window-shape-cache,
    update-window-shape
  - defmethods update-window-shape-allocation, invalidate-window-shape-and-extent-caches
  - window-shape slot from button
  - Fixing up math and translations for enclosing-rectangle, turtle-window-extents, sprite-at-window-point
  - updating extents and drawing for turtle coordinates.

bugs-193 Fixing up color-under, color-at, and bg-color variations
  - Coalescing background-color-from-turtle and background-pixel-from-turtle
  - Converting everything to :rgb float vectors from int32 pixels
  - Adding pixmap-pixel-color defun to pixmap.lisp
  - Checking for old files where Shape interfaces may not have an attached box.
  - Fixing set-color-at by fixing up %draw-point

color-tools-demo Fixed an issue causing the framebuffer to flicker.
    Split up redisplay-graphics-sheet into redisplay-graphics-sheet-graphics-list
    and redisplay-graphics-sheet-sprites, so that in the repaint we can paint
    the graphics list first, then blit the texture for the framebuffer, and then the
    sprites that need to go on top of that.

wip First spike of work for rotated/scaled sprites.

minor-fix
  - Adding repaint back in to redisplay primitive.
  - Removing extra window chrome around boxer canvas
  - wrapping supershrunk and port struts in proper pen width.

cleanup
  - Adding float-vector to begin cleaning up filling VBOs
  - Cleaning up quicklisp dependencies.
  - Removing old Sun invocation of bitblt-pixrect-to-screen
  - Removing commented out reference to playback-graphics-list-internal
  - Removing bound but unused %drawing-array from drawing-on-bitmap and drawing-on-window-without-prepare-sheet

refactor
  - Pulling out shader program inits and adding variable to reload them during runtime for shader work.
  - Moving defgraphics-state-change and translator entries to graphics-commands.lisp
  - Renaming new-offscreen-copy -> copy-pixmap and moving to pixmap.lisp
  - Moving repaint pass-1 methods in to their own file.

doco
  - Adding 'save-under' to glossery
  - Adding boxer coord commands to table: hollow rect, wedge, arc
  - Documenting members of graphics-sheet, removing wip member transform that was never used

the-attic
  - Removing unreferenced macros from macros.lisp
    - SPLICE-LIST-INTO-LIST, SPLICE-ITEM-INTO-LIST, SPLICE-LIST-INTO-LIST-1,
      SPLICE-ITEM-OUT-OF-LIST, SPLICE-ITEM-AND-TAIL-OUT-OF-LIST, SPLICE-ITEM-AND-TAIL-OUT-OF-LIST-1,
      SPLICE-BETWEEN-ITEMS-OUT-OF-LIST, SPLICE-LIST-INTO-LIST-AT, SPLICE-ITEM-INTO-LIST-AT,
      SPLICE-ITEM-OUT-OF-LIST-AT, SPLICE-ITEM-AND-TAIL-OUT-OF-LIST-FROM, SPLICE-ITEMS-FROM-TO-OUT-OF-LIST,
      ITEMS-SPLICED-FROM-TO-FROM-LIST, char-case
  - Removing now unused with-blending macro and usage
  - Fixing up usage of draw-poly and archiving unneeded boxer-points->window-system-points
  - Removing the remaining save-under code.
  - Removing defmethods fast-erase and erase for graphics-object/button
  - Archiving flash-name, calc-name-position-x, calc-name-position-y
  - Removing unused versions of draw-arc / %draw-arc
  - Archiving unused draw-filled-arc, %draw-filled-arc
  - Removing unused defvar %drawing-window
  - Removing unreferened colormap and prepared-flag members from defstruct graphics-sheet
  - Archiving unused boxer-color defstruct from gdispl.lisp
  - Removing empty with-real-time defmacro

## 3.4.13 2023-05-08

This is an incremental alpha release on top of the openGL graphics update from the previous release.
This fixes some initial issues with images, boxtops, the snip primitive, and text rendering speed.  There
are still some incremental speed improvements to come.

The major feature in this release is the addition of an in-progress feature to use framebuffers for rendering
turtle graphics drawing, massively speeding up the rendering of turtle simulations containing large numbers
of primitives. This is still experimental and can be enabled in the preferences under the Graphics tab with
"Use Opengl Framebuffers". Some known issues of this include slightly dithered fonts, occasional background
color change when the framebuffer is recycled during a screen-graphics-box clear, and thin horizontal lines
not appearing when directly on the x-axis.  This should tightened up in the next cycle aimed at bug fixing
and remaining GL cleanup.

### Full Change Log

sunrise-14
  - Removing these checks since some older files were saved with all nils and we want to
    resize everything for now.

sunrise-25 sunrise-63 draw-cha uses a texture map when possible
    - Using our texture atlas of default sizes and ascii range chars,
      when possible. If the char is not in there load and draw it from
      the font. (TODO adding more to the atlas on the fly in the future)
    - Updates some tooling to add gl-model meshes to screen-obj and screen-row
    - Still needs some work on cleaning up openGL buffers
    - Resetting gl-model meshes for screen-row re-use.

sunrise-64
  - Fixing snip and image squishing in new pixmap vertices and texture coordinates.
  - Fixing wrong texture bind in gl-add-pixmap, fixing mulitple image/boxtop rendering.

sunrise-66 Font scaling and zooming for glyph atlas characters

sunrise-67 openGL Framebuffers to back turtle drawing

    This is the first major set of work for openGL framebuffers.
    There may be other minor cleanups, especially in regards to
    resizing graphics and the backing framebuffer textures.

    This ticket also removes the old gdispl performance lists,
    and adds a new user preference to turn the openGL framebuffers
    on and off. Additionally the *features* entry :moderngl has
    been removed, and now using openGL 3.2+ is the default.

doco
  Various doco for lisp forms and developer notes.

refactor
  - Removing old commented out version of BP-OVER-VALUES
  - Cleaning up com-mouse-bl-corner-toggle-box-view
    - Archiving defvar *slow-graphics-toggle*
  - Cleaning up com-mouse-boxsize-closet-properties-pop-up
  - Cleaning up com-mouse-box-types-pop-up
  - Cleaning up com-mouse-br-resize-box
    - adding com-mouse-br-reset-box-size so we don't have to use the old
      mouse click timer wait-with-timeout stuff anymore. Binding this new
      command to mouse-click, whereas com-mouse-br-resize-box is still
      bound to mouse-down
  - Updating package on some remaining pixel= calls from opengl to boxer
  - Removing redundant image-pixel and standardizing on pixmap-pixel for access.
  - First iteration of pixmap reorganization
    - Moving pixmap.lisp from opengl to boxer package.
    - Converted ogl-pixmap defstruct to a defclass
    - Removed most of the layer of offscreen bitmap idioms that just called
      through to the pixmaps. Historically on old window systems and graphics
      systems this probably did a lot more.
  - Fixing up support for loading boxer init files.
  - Moving outermost screen-box tracking from global alist to box canvas.
    - Removing *redisplayable-window-outermost-box-alist*
    - Adding outermost-screen-box member to boxer-canvas defclass
  - Moving boxtop drawing routines from grfdefs to their own place in redisplay.
  - Separating the rather voluminous graphics commands handlers in to their own file.
  - Reorganizing the blinkers and boxer canvas
    Removing lots of unneeded cruft for the blinker (cursor), and moving it back to the
    generic :boxer package. Creating a new canvas class to mix in to the boxer opengl pane
    to coalesce global variables that are specific to an individual canvas, starting to
    allow for the future possibility of having multiple windows, tabs, divs.
  - Removing completely unused `window` member from `blinker` defstruct.
  - Removing unneccesary constructors for blinker and region-row-blinker

minor-fixes
  - Changing a bfs-server reference to a warn since it may happen with older docs.
  - Minor fixes for tests
  - Un-enabling shader programs after drawing routines.

the-attic
  - Archiving unused defun ACTUAL-OBJ-OF-SCREEN-OBJ (usually these are methods)
  - Removing more unused old openGL code
    - devars *opengl-type-checking-included?*, *opengl-type-checking-action*, *ogl-current-color-vector*,
      *ogl-color-pool*, *initial-ogl-color-pool-size*,
    - defuns auxiliary-buffer-count, auxiliary-buffer-exists?, ogl-init, gl-enabled?,
      initialize-ogl-color-pool, allocate-ogl-color, ogl-current-color, deallocate-ogl-color
    - defmacros get-opengl-state, ogl-type
  - Retiring old versions of fixed function openGL API usage
  - Archiving mouse-still-down-after-pause?
  - Archiving unused defvar *br-popup* and defun update-br-menu
  - Archiving the unused defboxer-command com-mouse-br-pop-up
  - Removing unused constructor/defun %make-graphics-sheet-with-bitmap, make-graphics-sheet-with-bitmap
  - Removing used *default-graphics-object-height*, *default-graphics-object-width*
  - Removing now unused *repaint-during-eval?*
  - Removing unused struct screen-row-rdp1-info and utilities
    - defstruct screen-row-rdp1-info
    - defuns allocate-sr-rdp1-info, free-sr-rdp1-info, set-rdp1-info-to-erase-to-eol,
      rdp1-info-is-eol?, still-inside-rdp1-info
  - Removing unreferenced defstruct ogl-graphics-state
  - Removing unreferenced defun intern-in-boxer-user-package
  - Removing now unreferenced load-8-bit-run-length-encoded-pixmap-by-strips from loader.lisp
  - Removing this old block-compile-class metaclass.
  - Removing unreferenced defmacro cha-width
  - Removing unused defuns mouse-on-region-being-defined-p and coords-on-blinker-row.
  - Removing used cursor sheets and blinker visibility
    This *boxer-window-blinker-alist* defvar is not used, the current information is stored
    in the global *region-list*.
  - Removed
      - defvars *boxer-window-blinker-alist*, *HIGHLIGHT-YANKED-REGION*
      - defuns sheet-blinker-list, %set-sheet-blinker-list, set-cursor-visibility,
        draw-region-row-blinker, turn-on-interval, turn-off-interval, region-row-blinker-visibility,
        make-interval-visible, make-interval-invisible
      - visibility from defstructs blinker
      - defsetf region-row-blinker-visibility

## 3.4.12 2023-03-20

This is a very alpha release marking a halfway point to the modernization of our OpenGL graphics layer.
Until this release our graphics painting was using the deprecated fixed function pipeline OpenGL API's.
This release features entirely rewritten drawing operations with GLSL shaders. All operations have been
updated, as enabling a core GL context completely disables the older fixed functions. However, there is
some remaining work to be done managing our VAO/VBO buffers, as a result, screen rendering is currently
slower than before for some complex microworlds. Buffer management will be a focus over the next several
builds, as we speed up the existing operations/tests and then add new features for visual effects and
support for turtles with *large* numbers of primitives.

Some known visual issues:

- Slower in some of the performance heavy microworlds and with lots of text.
- Fonts seem to be a bit too anti-aliased/multisampled in some places, so that they look a bit too blurry.
- Name only shrunken boxes left and right borders stick down a pixel or two.
- Unfilled circle line widths are too thin.

Outside of this OpenGL rewrite, we've added a new Error dialog that is opened on errors giving the user the
option continue execution or quit. This is a vast improvement from the previous behavior of launching an
external terminal emulator with a lisp debugger. However, this is a user preference to launch the debugger
rather than a dialog for developers to debug causes of error.

The toolbar has received a little bit of love, with some minor aesthetics in addition to hover over tooltip
text for all entries. Some minor keybinding updates. The Boxer icon has been updated to have rounded corners
and take up less space to fit the style of other application icons on the dock and task switcher on macOS.
(You may need to restart Boxer once or twice for the old icon to no longer be cached.)

This build has been tested on macOS Catalina, Big Sur, Monterey, and Ventura on both x86/ARM.

### Full Change Log

bugs-157 Using border color for shrunk box types

bugs-158
  - Adding tooltip hover overs to toolbar items.
  - Some initial clumping of toolbar-icons so that they spread out in their groups.
  - Painting the start/stop icons green/red. Resizing the boxer icns and rounding the corners so it fits in.

sunrise-63 Error Dialog for users (rather than always opening a debugger in a terminal)

sunrise-25
  - New copy of lispworks opengl from LW 8.0.1
    Adding a new copy of the lispworks opengl under version control
    to apply the fixes for using openGL 3.30 shader contexts with.
    Ability to pass in :modern for an openGL 3.2core context.
  - Complete update of all drawing methods from openGL fixed functions to openGL 3.2 Shaders.
    See extensive commit log from the past several months for details.

keybindings Cleaning up copy/paste, rebinding find and cha movement

    - Removing extraneous bindings for cut/copy/paste
    - Rebinding find to Command-F
    - Binding control-f and control-b to forward and backward cha

refactor
  - Cleaning up defuns repaint and repaint-internal

the-attic
  - Removing #-opengl usages
    - Removed old-value and top-guy slots and usage
    - Removed numerous calls to with-graphics-screen-parameters
    - Removed call to drawing-on-bitmap
    - Removed pre openGL version of redisplay primitive
  - Removing commented out cached-border-info slot and usage
  - Moving unused defmacro redisplaying-box to the attic.
  - Removing needs-repaint-pass-1? and 2
    - These always return t now since we are painting every frame in
      openGL
  - Removing no longer used slot inf-shift from defclass screen-box
  - Removing *check-bit-array-color*
  - Removing defuns erase-chas-to-eol, erase-screen-cha, erase-screen-chas
  - Removing commented out slots:
      screen-obj: new-wid, new-hei, new-x-got-clipped?, new-y-got-clipped?
      screen-row: out-of-synch-mark
  - Removing old #+MCL param to with-mouse-tracking-inside
  - Moving unused defun quote-wild-char from file-prims to the attic.
  - Cleaning up unused bits of boxwin-opengl
    - defparameters *boxer-window-left-margin*, *boxer-window-right-margin*
    - defvars *expose-window-handler-function*
    - defuns boxer-abort-handler, boxer-expose-window-handler, bootstrap-expose-window-function,
      expose-window-function
  - Removing unused abort-event?
  - Removing unused save-keys defun
    - defvars *saved-keys*, *save-key-length*
    - defun save-key
  - Removing extra global vars which are immediately updated by *starting-window-width* and height on init
  - Removing near exact copy of boxer-system-error-restart-loop was only used in the dribbler
  - Removed unused defvar *literal-input?*

## 3.4.11 2022-10-31

### Overview

Welcome to Boxer release 3.4.11 2022-10-31! This release includes a large amount of under the hood
refactoring in the areas of the filesystem, redisplay and editor interaction, and other areas. Major
features include the first iteration of the new Boxer Document format, a zipped archive similar to
docx and other modern office document formats.  Also included are several additions to the toolbar as
well as data structure updates to allow setting background and border colors on boxes. Future versions
will allow arbitrary colors as soon as the drop down widget is expanded to support that.

This release fixes and reintroduces the historically important Boxer Stepper in an initial
very alpha form. Other minor item include fixes for crashing on horizontal scrolling and other
iteractions, HTTPS support for opening boxes, lots of cleanup and refactoring, and documentation.

Thank you to Beni Cherniavsky-Paskin ( @cben ) for contributing written documentation this release to
the markdown files with descriptions of several Boxer concepts and internal details.

### Full Change Log

bugs-120 Ignoring horizontal scroll errors

design-5 Adding closet, toplevel, run/stop, box border/background items to toolbar

sunrise-44 Adding a centralized way to get/set styles and support for box background and border color.

sunrise-62 First major iteration of work for the new Boxer Document File Format
  - Moving special-file-readers to it's own formats.lisp for future work

stepper
  - Fixing up the historic Boxer Movie Stepper. Can now be bound to `com-step`
  - Binding com-step to control-option-return

HTTPS Support

doco
  - Adding documentation for the boxer save format
  - Adding docstring for defmacro with-hilited-box
  - Contributed glossary content from Beni Cherniavsky-Paskin @cben

refactor
  - Removing mcl specific checks for world box to com-open-box-file. Also, always repainting.
  - Reorganizing url datasource code
    - There is a class hierarchy in boxer for loading boxes from
      various sources such as http or the filesystem. Cleaning
      these up and reorganizing them in to clearer files.
    - This commit is simply cut and pasting sections to different
      files. Changes and cleanup will be done separately.
  - Removing old comm and OpenTransport requires.
  - Moving with-hilited-box from fildfs to new-borders.
  - Switching to log:debug from BFS debugging-message.
  - Removing last major sections of deprecated boxer server client code

    Removes the last vestiges of the client properties that were kept on the plist
    of boxes, most with the prefix of cross-file-*. This includes hooks for it in the
    virtual copy, editor, loading/dumping, and other minor bits of architecture. This
    removes a lot of older complex code that hasn't been used since probably the Sun
    days.

    - Completely commenting out client.lisp an clientmacros.lisp from our asdf modules.
      Will completely archive the files after more testing and usage.
    - Moved still necessary items to surf.lisp:
        - defuns no-inferiors-for-file?, storage-chunk-plist-half-length
        - defmethods dump-storage-chunk-plist-items, filename-for-file
  - Replacing home grown decode-url-string with quri:url-decode
  - Swapping out home grown path-suffix for CLHS pathname-type.
  - Removing un-needed net-read-line, fixing defun save-net-data.
  - Moving file dialogs to new file-dialogs.lisp
  - Marginal improvement in the debug overlay generation.

the-attic
  - Archiving dump-top-level-box-to-stream from dumper.lisp
  - Archiving things related to *redisplay-clues*
    - defvar *REDISPLAY-CLUES*
    - defun REDISPLAY-CLUE (and setf ':redisplay-clue)
  - Moving unused defun make-file-box from comse.lisp to the attic.
  - Removing old mcl version of com-help text.
  - Moving unused #+mcl edit-mac-file-ref to the attic from xfile.lisp
  - Removing last traces of boxer-file-info
    - Removing write-boxer-file-info and boxer-file-info
    - These were only used on old versions of macOS with data in
      file resource forks
  - Removing fill-box-from-bfs-server, record-copy-file-info
    - These are some last bits of the BFS that appear to use box props
      :boxer-server-id and :copy-box-id, neither of which show up when
      egrepping through all our old .box files.
  - Removing old file compression utils
    - Removing defvars *automatic-file-compression-on*, *file-compress-minimum-length*
    - Removing defuns compress-file, uncompress-file, maybe-uncompress-file
    - Obsoleting primitive compress-file
    - Apparently we used to have file compression that farmed out it's work to a unix
      command line invocation
  - Removing old unix command line utils, alus, and file stuff from Suns
    - Removing defuns make-temporary-filename, read-box-from-text-stream, boxer-run-unix-program
      fix-file-alus, convert-file-alu
    - Removing defvar *file-conversion-alu-alist*
    - Obsoleting primitives fix-sun-file-graphics, fix-mac-file-graphics
  - Archiving mailto-url, file-url, make-message-box. Fixing starting url label in box properties dialog.
  - Archiving unused mcl *possible-boxer-file-mac-types* defvar
  - Archiving box-file?, replacing with duplicate defun boxer-file-contents?
  - Archiving unused source code utilities and things from mcl-utils

     Some of these were used for source code management before version control,
     and before the asdf module system, parsing header comments on files.
     Others are just utilities no longer of use.

     - defuns cf, lcf, compops, pa, pp, whereis, test-loop, keys-code, mod-history-start-line?,
       mod-history-end-line?, mod-history-date-line?, get-mod-history-lines, get-mod-history-lines-after,
       mcl-file->pc-file, pc-file->mcl-file, no-convert,
     - defvars *modarray*, *special-source-files*
     - mcl editor support add-def-types
  - Completely archiving client.lisp and clientmacros.lisp
  - Archiving lisp machine box file type defs
  - Archiving unused *max-filename-length* and defun truncated-filename from file-prims
  - Archiving unused SEMI-INIT and RETURN-INIT-PLIST-FOR-FILING from editor.lisp
  - Archiving used defun massage-pathname from lw-menu

## 3.4.10 2022-07-11

### Overview

This release begins the refactoring to improve OpenGL graphics performance by modernizing
the drawing the approach of graphics display lists to buffers and away from old school GL
fixed functions. This currently improves performance of turtles with *large* numbers of line
segments of a single color, setting the stage for future bits of geometry to be sped up in
subsequent releases.

UI updates include the preferences dialog now persists and apply changes automatically (you
no longer need to hit apply or save). This now runs as a separate dialog that allows you go
go back and forth between the preferences and boxer canvas as you update the preferences.

Previously, when flipping sprite boxes, they just disappeared. For now they are no longer
flippable, and won't disappear.

Some bits of the the graphic primitives for turtles, as well as bits of the virtual copy
mechanism, and name rows where updated to allow using characters outside of the ASCII range,
allowing any unicode character. The name rows fix for this includes a small change to the
file save format (see below).

A number of fixes were made to double clicking .box files on macOS to open Boxer if it's not
open already, or simple insert the double clicked box file at the cursor if Boxer is already
open.  Also, a number of fixes were made to laggy toolbar and rendering updates when starting
up boxer.

We are trying out some new labels on the status bar to indicate whether or not mouse and keyboard
redefinitions are active.

This build updates to using Lispworks 8.0.1.

Fixes a bug where the alpha layer of pixmaps wasn't saved, making impossible to save files with
sprites that included alpha layers for animations and such. This allow with the name row
fix mentioned above, bump the box file format from version 12 to 13. Box files with version 13
will only open on this release and future releases. You can continue to open any previous versions,
but on save they will be upgraded to version 13.

As usual, a large amount of refactoring and removal of technical debt continues under the hood.
Another portion of the codebase now loads under other lisps like SBCL and ECL, which continues
to be a work in progress.

### Full Change Log

sunrise-15 Preferences dialog now autosaves
  - Removed Cancel, Set, Set/Save buttons
  - Moved the save code to the callback for each input widget
  - Created a top level *preferences-dialog* to hold the dialog

sunrise-25 First iteration of OpenGL graphics performance improvements
  - Speeds up rendering of consecutive line segments of the same color in
    graphics command lists.
  - When inside a graphics list, keeps track of whether we are rendering line
    segments without a color change and buffers vertices to a C array, then
    uses glDrawArrays to draw the lines.
  - Size of the array is configurable with a variable
  - Added user preference to optionally turn off the in-progress performance
    increases.  Default has it turned on.
  - This is just the first of several openGL vertice speed improvements.

sunrise-29 Disabling UI flipping of sprites so they aren't invisible.

sunrise-34 Fixing virtcopy functions used in rtype, ltype to use type character
    - box-text-string was still using standard-char as it's array type rather than
      character, which failed to support higher level unicode points.

sunrise-36
  - Previously we cut out the alpha layer from RGBA pixels
    and used it for a count value when saving, as a sort of
    poor mans compression algorithm if 2 adjacent pixels were
    the same color. Using both 16-bit words now to store the
    32-bit color value. Adding one more to store the run-length
    encoding.
  - Adding function for getting the format version number of a .box file for
    testing format increments.

sunrise-52 Changing name rows to dump as strings
  - Name rows were dumping as cha op codes in the dumper. However, because 4-bits
    of the 16-bit words were taken up by the op code, any unicode code point larger
    than 12-bits would get chopped off.
  - Changed name rows to dump as strings.  Bumped file version to 13, and added a check
    in the loader for older versions to still load the name rows as cha's.

bugs-66 Removing old delayed mouse clicks
  - After several releases of having both, we are now removing
    the old delayed mouse clicks, and the preference that went
    along with turning them on still.
  - All of our working microworlds have been updated to the new
    click magic names for doit boxes.

bugs-130 bugs-137 sunrise-60 More fixes to mime Boxer launching and toolbar updates
  - Added update-toolbar-font-buttons invocations to menu functions that adjust items on the
    tool/statusbars
  - Added an **boxer-init-queue* to put double clicked .box files on, so they can be queued when
    the finished-launching message is sent. Previously these were getting flushed from the eval
    loop because the eval loop starts before the application is finished launching.
  - Also added toolbar updated to finished-launching to adjust the window geometry depending on user
    prefs as to which toolbars are on or off.

Upgraded from Lispworks 8.0 to 8.0.1

refactor
  - Cleaning up commented out parts in boxer-sunrise.core asdf
  - Coalescing lispworks specific opengl code in to draw-low-opengl-lispworks module
  - Minor cleanups in package.lisp

the-attic
  - Removing commented out and #+'d out portions of dumper.lisp
    - In addition to obsolete comments and partially commented out regions, removing:
    - Symolics/mcl items *the-lisp-package*, canonicalize-package-name,
      array-bits-per-element, file-stream-position, canonicalize-display-style,
    - We always use 32 bit depth pixmaps now so the following are never called:
      dump-8-bit-pixmap, fast-mac-dump-8-bit-pixmap, get-picture-byte,
      dump-1-bit-pixmap, dump-picture
  - Test removing bfslocal and it's one usage in the loader.
  - Removing old version of html-export
    - Now that the newer html5 of the html export does much more,
      we are removing the previous version of html export.
  - Archiving unused portions of surf/tcp stack
    - defvars *correct-password-retries*, *return-tcp-stream*, *tcp-error-handlers*,
      *ftp-data-listen-timeout*
    - defuns open-tcp-stream, %get-ip-address, start-char-server-stream,
      start-binary-server-stream, make-server-char-stream, make-server-binary-stream,
      poll-for-close-stream, make-ftp-data-stream, check-for-net-eof,
      net-write-line-to-binary-stream, tcp-stream-local-port, signal-tcp-error,
      handle-tcp-response, throw-to-retry-fill-tag,
    - defmacros net-write-control-line, net-write-line, tcp-handler-bind
  - Moving bfsforeign.lisp and bfslocal.lisp to the attic
  - Removing old comments on name-tab fonts in disdef and editor.
  - Removing unused code from gdispl.lisp
  - Removing #-opengl code in loader.lisp
    - Removing #-opengl specific code from load-8-bit-run-length-encoded-pixmap and
      load-true-color-run-length-encoded-pixmap that used `drawing-on-bitmap` and
      `set-offscreen-bitmap-image`
  - Removing #+mcl specific code in loader.lisp
    - Removing #+mcl specific code including defvar *use-mac-fast-bitmap-loaders* and
      defuns fast-mac-load-8-bit-run-length-encoded-pixmap and
      fast-mac-load-true-color-run-length-encoded-pixmap

## 3.4.9 2022-05-05

### Overview

This release fixes a large handful of small to medium sized bugs, includes some extensive under the hoods refactoring
for future graphics performance improvements, and some desktop environment niceties. These include reviving the
modified files dialog to allow saving/discarding modified file boxes before close, and the ability to launch Boxer by
double clicking on a .box file in the Finder, or by dragging a box file on to the applications icon in the Dock. The
Preferences dialog has gotten a first iteration overhaul as well, with a nicer looking dialog. We are planning for preferences to autosave in the next iteration.

This release has been tested on macOS Catalina, Big Sur, and Monterey on Intel and M1.  There is a known bug with the
open file dialog on Mojave, which will hopefully be addressed in a future build.  We do intend to start making Windows
builds again soon, there has been some effort involved with the Intel/M1 work and other transitions, but these should
return in a very cross platform way in the near future.

It is a known issue that with certain small .box files that are double clicked to open Boxer may not be loaded on startup.
Double clicking them in the finder will open them.  This is a timing issue that will be fixed in the build.

This release features our first external contribution from @chaals cleaning up some typos in the README. Thanks @chaals!

### Full Change Log

bugs-110 Looking at the actual obj of the screen mouse box to determine whether clicked box is shrunk.

bugs-115 Removing penxor and other alus from sprite-commands-for-new-position

bugs-116 Fixing virtual copied sprites as a top level in append-row
  - The comparison when checking if a virtual-copy struct was of type sprite
    was outdated, and updated this to look at the graphics-info member, similar
    to the current check if something is a graphics-object? of a box's graphics-info
    to determine spriteness.

bugs-118 Fixing window dimensions preferences
  - Re-arranged order of startup so the maximize-window, width, and height get
    read in from the prefs before displaying the *boxer-frame*

bugs-121 This 'check-for-unsaved-boxes function is nowwhere in the codebase

bugs-123 Fixing up confirm modified boxes on quit dialogs

bugs-124 Any size fonts can be used with set-type-font now.

bugs-126 Adding a check for graphics boxes that got always-zoom set

bugs-129 Adding check in case the current font name isn't in the option-pane collection

sunrise-15
  - Updated description of show status bar preference.
  - Archiving prefs step-wait-for-key-press, step-time, update-display-during-eval
    - The stepper prefs are being commented out until the stepper works again
    - Ideally the display update pref won't be needed again in the future based
      on current speed.
  - Cleaning up leftover prims from font refactoring
  - Removing separate site config from preferences
    All preferences are still saved in the default user preferences file Boxer.prf
    - Removing unused site config that used to be put in /usr/local by default
      and never quite finished
    - prims: configure-info, reconfigure-system
    - *default-site-directory*, *default-configuration-file-name*, *site-initialization-handlers*,
      def-site-var, handle-site-initializations, handle-site-initialization,
    - Plus actual site inits
  - Archiving unix postscript, serial, and mouse pop-up prefs.
  - Temporarily commenting out smooth-scrolling penerase-color-from-bit-array prefs
  - Collapsing pref docs to single lines in s-expressions.
  - Removing buttons and seperate capi panel to show the prefs descriptions.
  - Converting prefs dialogue pinboard layout to regular column layout.
  - Fixing up prefs initialisation on startup
    - Changing the prefs read function to also return the fixed up coerced value.
    - Changing the initialization handler code to also call the -q-function for the
      preference in case it has any side effects it needs to call on update.

sunrise-25 Merging draw-high-hardware-clip in to draw-high-common

sunrise-56 Updating instructions to LW 8.0 with more notes on quicklisp init

sunrise-60 Fixing :message-callback to insert double clicked box files on to the event queue.

refactor
  - Fixing up usage of draw functions to not call internal % versions
  - Adding public version of draw-circle
  - Cleaning up draw-high.common.lisp
    - Moved a few old things in to the attic.
    - Moved many comments to be docstrings
    - Organized functions into 3 sections, draw-on-window macros, scaling origin macros,
      and draw function.
  - Creating draw-high swap-graphics-buffers to replace internal %flush-port-buffer.
    - Also removing old force-graphics-output
  - CLeaning up draw-low-opengl.lisp
    - Moving sheet-inside-top and sheet-inside-left to the attic, replacing
      only usage with constant 0
    - Removing old comments
    - Rearranging %draw- methods to sort order
    - Moving some comments inside of docstrings
    - Removing unused %open-color=, normalize-color-component, sign-of-no
  - refactor First stage of removing save-under code
  - Moving to the attic but keeping stub functions for loader and
    dumper of:
      - update-date-under
      - scale-save-under
      - save-under-turtle
      - restore-under-turtle
      - extract-sprites (commented out previously)
      - cache-active-sprites (commented out previously)
  - Force-repaint is just repaint now.

the-attic
  - Moving pre-opengl *currently-moving-sprite* and usage to the-attic
  - Removing #-opengl usage of *currently-moving-sprite*
  - Removing #-opengl code from defboxer-primitive bu::set-background
  - Moving lispworks graphics-ports version of arc drawing to the attic.
  - Archiving defrecursive-funcall-primitive holding-position
  - with-turtle-slate-origins, *allowed-alus*, vlist-alu?
  - commented out defsprite-update-function
  - Archiving primitive with-sprites-hidden
  - Removing stub/usage of box-border-zoom-in, box-border-zoom-out, box-borders-zoom
  - Archiving old commented out version of change-alu
  - Archiving unused *bitmap-backing-store*, allocate-backing-store, deallocate-backing-store
  - Removing unused brand-new? defun
  - Archiving unused make-boxer-primitive-internal, compile-lambda-if-possible, *old-compilation-speed*
  - Test commenting out bfsforeign.lisp

## 3.4.8 2022-02-16

### Overview

This is largely under the hood tuning release, although it does fix some annoying things from the
last build, such as scroll bars not working with the mouse and the hanging progress dialog on startup.
It's also the first build on Lispworks 8.0 which is a major Lispworks release.  This build for macOS
is still Intel only, running under Rosetta on Apple Silicon, but a number of changes have been made
under the hood to bring it up to Silicon. There are still a few bugs preventing this native build though.
However, the LW 8.0 Intel build does run faster under Rosetta than the LW 7.1.2 one did.

Other minor visible fixes include:
- Fixed the caps lock key.
- Fixed the Key/name help menu item.
- Fixed more issues with the modern mouse updates
- Fixed some issues with the canvas not repainting frequently enough.
- Fixed some issues loading older v5 .box format files from the SunOS version of Boxer.

This cut of code includes a large amount of refactoring and clean-up of legacy repaint/drawing code as
well, making way for a round of performance and "prettiness" work on the renderer.

### Full Change Log

boxer-bugs-35 Fixing Key/name help menu plus refactoring copied bits
  - Fixes:
    com-document-key
    com-insert-key-name
    primitive show-key-name
    Menu item Help -> Key/Mouse
  - Added “lookup-input-name” to replace duplicated sections that checked
    if the input was a keyboard event or mouse event to get the magic name,
    plus unit tests

boxer-bugs-37 Removing status line region select copy/paste docs
  - Removing status line updates from entering-region-mode
  - Removing now unused current-mouse-click-name to the attic

boxer-bugs-66 Updating mouse click -> down for com-mouse-scroll-box

boxer-bugs-102
  - Adding checks for various levels of portness to com-expand-box
  - Moving shrunken? check to mouse-screen-box for defining regions.

boxer-bugs-104 Fixing the caps lock key

boxer-bugs-107 temporarily removing SHIFT from magic bindings until we work out the logic correctly

boxer-bugs-108 Adding extra repaint to ensure the canvas is updated after clicking.

boxer-sunrise-25 Add mouse-doc entries to dev-overlay

boxer-sunrise-42
  Fixing editor-abort-char? and test cases
  Adding bits to editor-abort-char? invocation

boxer-sunrise-44 Adding test default-light-theme and solarized-light-theme

boxer-sunrise-53
  - Changes to eval-when for vc-rows-entry, predicate, and unit tests.
  - Reworking init and opengl calls for LW8.0 Apple Silicon
  - Refactoring boxwin-opengl to be cleaner and make sure all the opengl:rendering-on happens in some callback of the *boxer-pane*
  - Adding freetype load paths for LW8.0 Intel

boxer-sunrise-54
  - Fixes and test case for opening old v5 box files from Sun machines.
    Adding box test file case from Henri.
    Extra nil check for rendering the result of these sun files to html5

boxer-sunrise-55 Initial logging support and startup log messages.

minor-fix
  - Fixing format string when a marked place no longer exists
    (when trying to move to the register mark)

documentation
  - Docstring for *mouse-doc-status*
  - Adding some initial documentation for display-style defstruct

refactor
  - Removing some absolute paths in deliver-script

crash-fix
  - Fixing a regression that locks up when resizing the window during evaluation.

copyright
  - Updating copyright year to 2022

the-attic
  - Archiving Sun based Boxer Sending Server/Client
    - boxnet.lisp and net-prims.lisp have some really cool features
      for sending boxes between machines using a binary streaming
      mechanism
    - But it's based on a pretty old Sun/Lucid socket implementation
    - Someday this will be resurrected either using a modern sockets
      library, or some sort of proper server implementation using
      maybe json, or still binary streaming over websockets or something.
  - Removing applefile.lisp, moving to attic
    - This has been commented out for several releases from asdf
    - Everything should just use standard cross platform file
      operations now.
  - Removing base64.lisp, adding to attic
    - This has been commented out of asdf for a while now.
    - We are using the qbase64 library from quicklisp now.
  - Moving defboxer-command com-receive-boxer-send to the attic
  - Moving MCL Specific show-file-info, set-boxer-file-info, and commented out really-save to attic
  - Moving record-file-box-place, record-url-box-place, mail to the-attic
    - record-file-box-place, record-url-box-place were stubs
    - this particular version of prim bu::mail was pretty old and relied on
      unix mailer daemons
  - Removing valid-boxer-license?, fast-iwmc-class-p, *MARK*, *CURSOR-BLINKER-WID*,
      *CURSOR-BLINKER-MIN-HEI*, *MINIMUM-CURSOR-HEIGHT*, *MULTIPLICATION*,
      *BOXER-READTABLE*, *BOXER-FUNCTIONS*
  - Removing unused params 'alu' and 'window' from %draw-string, %draw-cha, %draw-line,
    %draw-rectangle, %draw-poly, %bitblt-to-screen, %bitblt-from-screen, draw-line, draw-rectangle,
    bitblt-to-screen, bitblt-from-screen, draw-string, draw-cha, draw-point, draw-poly,
  - Moving #-opengl usages of drawing-on-bitmap and draw-rectangle to attic
  - Removing box-border-outside-space, box-border-inside-space, box-border-border-width,
      port-border-port-box-gap, *last-scrolled-box*, *last-scrolled-dims*, fill-button-memory,
      button-memory-match?, dont-show-resize-hotspots?
  - No longer used:
    - Call to fill-bootstrapped-font-caches
    - *typeahead-during-eval*
    - *double-click-wait-interval*
    - *literal-input*
    - handle-event-internal
    - *suppress-event-queueing?*
  - Removing *old-world* and making an archive copy of window-system-specific-start-boxer
  - Moving unused constant *number-of-mouse-buttons* to the attic
  - Moving progress dialog to the attic since boxer starts up pretty quickly these days.
  - Removing unused gray shade defvar's and initialization
    - Removing *gray0* through *gray5*, *filegray*, *graphicsgray*
    - Removing repaint code that used these before lwwin and opengl
  - Removing unneeded *cocoa-boxer-interface* and commented out opengl lib loads
  - Removing comment out call to deleted follow-mouse-internal
  - Removing menu item check for underline style, which is not currently supported.
  - Archiving mcl specific com-link-to-mac-file
  - Removing empty bodied at-user-level macro.
  - Removing mcl specific com-edit-mac-link

## 3.4.7 2021-11-30

### Release Notes

#### Overview and major update work for macOS Monteray

This release features some relatively major changes and refactorings. There has been a larger
gap of time than usual between this build and the previous one, and some of that can be
attributed to fixing  showstoppers that kept Boxer from running on macOS Monteray and
caused more frequent crashed on Big Sur. Additionally, this release binary is the first to
be signed and notarized by Apple, making ease of download and usage less jarring for those
users.

#### Toolbarrs, Rendering, UI changes.

This release for the first time adds a toolbar at the top that includes menus and icons
for adjusting the font family and styling, as well as icons for starting and stopping
evaluation. The icons will change to reflect if a microworld is currently evaluating.
Additionally, we have also added a status bar on the bottom displaying the current font zoom
and interaction mode.

These toolbars and status bar and be enabled/disabled from the View Menu. Additionally, the View menu has a new option for hiding the name rows if they empty. (Otherwise they are displayed as
tiny stubs). These are also saved as preferences under the Editor section. This is the first iteration of them, and then will likely continue to change and go through
refinement.

The box labels, if turned on, now mark graphics and sprite boxes as "Graphics" and "Sprites"
respectively, rather than just a "Data" or "Doit" box. Additionaly, the default exported box generated the first time opening a closet is now named "Drawer" rather than "Closet".

Less of a visible change, but a fair portion of renderer cleanup has started as part of this,
including many pieces that were causing frequent lockups on macOS Monteray.  Added some more
standard keyboard accelerators, such as Command-m to minimize the window on macOS.

#### Modifier keys

This release brings back the ability to use the Option/Alt, Command/Win/Meta, and Shift keys
in addition to Control, and fixes most of the issues related to overriding these. You can now
use magic names like "control-option-mouse-click" or "option-command-u-key".

Basic substitutions of modifier keys are done between platforms at the moment, such as Control
<-> Ctrl and Option <-> Alt, though more sophistication will likely be added to this in the
future.

There may be another iteration of work to stabilize the use of the shift key modifier in situations outside of the basic latin character set, which we are looking in to still.

#### Mouse Clicks

This release features a major iteration on mouse clicks, bringing them up to modern speed.
Previously Boxer handled double clicks by including a timeout mechanism to only fire a single
or double click. However, this created a lag time with every mouse click operation, the
result being that Boxers UI felt perpetually "Slow".  Clicks by default are now handled as in
most modern applications. There is no delay, and a double click emits a mouse down, up, click,
and double click. As a minor simplification we have left out emitting the mouse down/up from the
second click of a double click.  There is an option in the preferences to switch back to the
old behavior if needed for the support of older microworlds, but in general, the newer behavior
is much more responsive and should be used.

This change introduces the new magic name methods "mouse-up" and "mouse-down" in addition
to "mouse-click" and "mouse-double-click". On a double click a single click is fired ahead of
time, so you'll want to consider modern UI design when assigning handlers. ie. A single click
method should be something somewhat harmless that when added with a double click doesn't
interfere with the action. Consider double clicking on a line in boxer. The first click event
moves the cursor to the mouse position, and the double click executes the line. As an
external operation, consider double clicking on an entry in any modern file manager such as
the Windows Explorer, macOS Finder, or Linux Nautilus. The first click emitted highlights the
file, and the double click event opens the file. The single click doesn't contain any side effects
that would disrupt the double click.  Obviously this is only a concern if you plan on overriding
both the single and double  click mechanism for a box.

In updating older microworlds to these new mouse clicks the only major changes have been
changing mouse-click operations to mouse-down, when the function of the UI was to initiate
some type of mouse dragging operation, such as a color picker drag and drop, or drawing on a
turtle graphics box.

#### Other Changes

Reverted an old commit that had drastically slowed down the performance of the Diffusion
Microworld. (Was testing adding hundred more points in circle generation, which slipped in
and wasn't reverted as meant to be earlier.)

Some visual improvements to the HTML5 export. Fixed an issue that crashed when exporting to
replace an already existing filename.

Follow-mouse has been removed as the default operation on mouse-down-on-sprite to avoid making it
too easy to muck up turtle programs.

Lots more archiving of code no longer called/used on modern platforms, as well as continuing to
wall off and refactoring pieces of code specific to a given platform or lispworks.

#### Known Issues

These are some known issues introduced in this version that should be fixed in the next build.

- There is currently some lag in the display of various windows and decorations, primarily on
  macOS Monteray. The startup progress dialogue, as well as percentage progress when loading
  large file may be laggy.
- The window bar is temporarily static, due to a lockup issue when displaying the current box file
  in the bar. This will be fixed in the next release.
- Full screening on macOS causes a hang.

### Full Change Log

boxer-bugs-66
  - Reducing the raw number of events that are fired on a double click.
  - Removing the up,down, and click that are generated on the second click before
    the double click is fire.
  - Moved the repetitive click handlers for the capi :input-handlers into
    click-handlers.lisp
  - Added the new mouse2021 down/up/click versions for middle and right
    mouse buttons.
  - Adding unit tests for comdef abort chars crash and mouse pixel wander
  - Fixing interactions with selecting regions on mouse downs.
  - Adding more keydef high bindings for new interactions.
  - Updating click handlers for middle and right clicks and modifier bits.
  - Added handlers for Command key (:hyper in capi)
  - Rearranged macOS define-input-devices to match the bit order
    used everywhere else for the modifiers.
  - Reducing number of redundtant click functions.
  - Fixing up some issues when dragging across graphics, lines, to more closely
    match things like Word if you begin the click on a graphic it's not a drag
    selection. (also part of boxer-sunrise-13)
  - crashfix Adding integer check for char-code use

boxer-bugs-68 First version of a toolbar and status bar
  - Adding Icons for Run/Stop from Scratch
  - Making a single bitmap to load images from for performance
  - Prototyping some bits to update the eval versions of the icons.
  - Adding the icons bitmap
  - Adding preferences for showing/hide toolbars and statusbars.
  - Changing the icons image to a png so it loads on both Windows and macOS
  - Changing hide empty name rows pref to Show Empty Name Tabs
  - Adding everything to the View menu

boxer-bugs-72 Improved HTML5 Export
  - Adding size, font name, and styling to output
  - Changed spaces to non-breaking spaces
  - Encoded HTML entities
  - Added background color for closet rows.

boxer-bugs-73 Allowing ability to export over an existing file.
  - Add a check for nil filenames, which usually means the user hit cancel
    on the choose file dialog.
  - Add :supercede option to with-open-file to allow exporting over a file,
    by this point the choose file dialog has likely already warned the user
    about this.

boxer-bugs-74 Renaming transparent closet box from 'closet' to 'drawer'

boxer-bugs-87 Removing follow-mouse as default action for mouse-down-on-sprite

boxer-sunrise-20
  - Moving pixmap library calls from fli to cffi for portability
  - Adjusting components for both lispworks and sbcl
  - Adding more opengl constants used in the source.
  - Changing some defconstants to defvars for sbcl
  - Adding required arguments on calls to error with formatting strings
  - Adding #+lispworks reader macros for specific library usage

boxer-sunrise-23
  - Adding Window menu and minimize accelerator for MacOS
  - Adding window-menu to :menu-bar

boxer-sunrise-31 Temporarily disabling scroll gestures during evaluation to prevent locking up.

boxer-sunrise-37 Adding Graphics and Sprite as specific border type labels.

boxer-sunrise-38
  - Docs for how to sign and notarize binaries with Apple.
  - Shell script for build and notarization process.
  - In-progress entitlements and adding code signing leftovers to .gitignore
  - Docs on generating certificates and minor script improvements

boxer-sunrise-44 Testing boxer styles by applying solarized dark palette

boxer-sunrise-46 Updating Alternate Platform Names code
  - refactoring out of ev-int to keydefs module area
  - starting to add tests
  - Updated defboxer-key invocation to use updated bits.

boxer-sunrise-47
  - Removing needs-redisplay-pass-2? and force-redisplay-infs?
  - Completely removing *COMPLETE-REDISPLAY-IN-PROGRESS?*
    - Removed *COMPLETE-REDISPLAY-IN-PROGRESS?* and all instances where
      we use it.
  - Removing *currently-repainting* variable
  - Updates to toolbar displays for macOS Monterey
    A few updates to where the toolbars are refreshed during render and
    apply in pane processes. Removing toolbar updates for now from the
    repaint-in-eval for speed reasons.

boxer-sunrise-50 Initial commit of historic landscapes extension code.

boxer-sunrise-51 Converting cocoa special keys to standard gesture symbols.

documentation
  - Rebinding keys at runtime during development
  - Minor comment improvement and ql library needed for the tests to run
  - Small docs additions and reference diagnostic outputs
  - Updating build instructions

performance
  - Reverting original num-slices size which was causing dramatic slow downs in the diffusion simulation.

key-bindings Removing broken printing key, and adding emacs style prev/next line

crash-fixes
  - boxer-sunrise-40 Minor crash fix for nil boxes

tests
  - Fixing up tests for lispworks/sbcl
  - Commenting out the chunker tests until they are completed.

refactor
  - Moving lw capi source for boxwin in to the lw-capi specific folder

the-attic
  - Moving obsolete XOR primitives to obsolete prims area
  - Archiving currently broken ftp support
  - Removing binhex methods that are no longer used.
  - Archiving clearly unworking version of touching?
  - Removing commented out save-state-and-reset and restore-state for graphics-object
  - Removing usage of empty with-font-map-bound macro
  - Moving unused x-out-of-bounds? and y-out-of-bounds? macros to the attic.
  - Archiving old version of defstandard-graphics-handlers (line-segment 3)
  - Test removing base64 and applefile lisp files, plus including the obsolete primitives file.
  - Moving unused elevator-row-string out of coms-oglmouse.lisp
  - Moving ogl-init from boxwin-opengl to opengl-utils
  - Archiving pre-opengl version of set-assoc-graphics-box
  - Removing unused with-open-blinker and altering-region
  - Removing unused drawing macros with-drawing, with-clip-bindings, update-window-system-state
  - Removing used vars, defs, and #-opengl lines
    - char-bits-limit
    - *CONTROL-CHARACTER-PREFIX-TABLE*
    - CONTROL-CHARACTER-DISPLAY-PREFIX
    - *unshifted-mac-editor-abort-chars*
  - Removing :control-character-display-prefix from package
  - Removing remap-char?, remap-char, convert-gesture-spec-modifier

## 3.4.6 2021-07-16

### Release Notes

This release fixes the following Boxer primitives: `follow-mouse`, `handle-input`,
and `status-line-y-or-n`, as well as other primitives that depended on status line
input.

We're continuing to work on simplifying the File Menu, and have collaposed the Box Marking
portion to just option depending on whether the box is already saved to a file.  Additionally,
the Save option will perform a Save As on the World if nothing is saved yet, and prompt
for a file name (rather than be greyed out). As of now the only time the `Save` menu item
will be greyed out is if you are in a read-only file box.

This release includes in progress work of updating the way that the mouse clicks are triggered
and handled in boxer. This is currently experimental and subject to change.  On start up, the
old behavior is used, but the new 2021 Mouse Clicks can be enabled in the User Preferences
under the Editor section.  Previously all combinations of mouse clicks in Boxer had a single
`MOUSE-CLICK` magic name that could be overwritten, along with a `MOUSE-DOUBLE-CLICK`. This
used a delay on clicking to see if it would be a double click or not. Additionally, `MOUSE-CLICK`
behaved as a mouse press since you didn't have to let up on the mouse for it to register.

The new version of mouse clicks add `MOUSE-DOWN`, `MOUSE-UP` and trigger them accordingly along
with click on the way to a double click. There is no lag when clicking now. There is some work to
do on this behavior still, as it stands if you were using `MOUSE-CLICK` for dragging, you would
now use `MOUSE-DOWN`. In the end there may be some way to preserve this by changing names, or
legacy worlds may just need to be updated, or switched in to old click mode.  This work is in progress.

As part of the mouse clicks and general event loop handling improvement, this release includes a
refactoring that begins decoupling the Boxer Event Queue from the Lispworks CAPI Opengl handling. This is a first
small step towards an independent Boxer Evaluation engine that could be used with other rendering
implementations (Metal, Audio representation for hearing impaired, server side engine for use with
WebGL or the DOM, or another independent OpenGL implementation, etc)

The background color of the top left box corner was changed to a more subtle shade of yellow. As
part of this we created a new `boxer-styles.lisp` to beging centralizing all the colors, thicknesses,
and font style variables which are scattered around the source. Long term this should evovle into
a boxer style sheets type of utility to create themes, dark mode version of the boxer canvas etc.

This release includes some initial changes to allow codesigning and using Apples Notarization
process.  While it is possible to notarize now successfully using the service, some small
changes still need to be made with our entitlements to allow it successfully start up on Big Sur.
Fingers crossed, the next binary release with be signed and notarized. For now, you still need
to try opening it twice using right click Open after initial download.

More contributors from Boxer history were added to the top level readme.

As usual lots more code for old lisp platforms and machines was moved to the attic.  Another small
handful of crash fixes were put in.

### Full Change Log

- boxer-sunrise-38 Initial changes to allow codesigning and Apple Notarization
  - Moved binary assets (dylibs, xfasls) out of Contents/Resources and in to
    Contents/Frameworks and Contents/PlugIns

- boxer-bugs-57 Updating background corner on top left shrink corner
    - Updated background color to RGB 1.0 0.9 0.0. A sort of dark yellow.
    - Created new file boxer-styles to start collecting styles in.
    - Updating some of the ogl color hooks to take an RGB vector, so we
      don't have to rely on in-memory opengl vectors in order to set
      preferences.

- boxer-bugs-46 Updating behavior of File Save menus
    - Rename "Mark Box as File, Save..." to "Save Box as File”
    - Collapse the "Mark box" section on the menu to just
        "Unmark This Box As File" or"Save Box as File"
            (Currently: "Mark Box as File, Save…")
    - Change "Save" to not be disabled, but if a box is not a file box,
      then have it invoke either "Save As..."      or "Save Box as File”

- boxer-bugs-51
  - Changing maximum-mouse-button-encoding to use length of defined mouse actions.
  - Removing old ~A-~A-MOUSE-~A and ~A-MOUSE-~A-ON-~A click names for platforms that
    no longer exist.
  - Updated the :lwm and :ibm-pc platforms to have mouse-up and down
  - Added a sysprims preference for switching between new and old mouse behavior
  - Added hooks in capi click methods to click depending on preference
  - Added method in keys-new to swap the set of mouse key bindings for the two

- boxer-sunrise-25 Adding mouse x, y, and down? status to dev overlay.

- boxer-sunrise-31 boxer-sunrise-19 Fixed primitives handle-input and status-line-y-or-n
    - Adding checks for gesture-specs in several places as these are what are
      generating the key events now. We should look and see if the key-event?
      conditions are even necessary anymore at some point.

- boxer-sunrise-33 Fixed follow-mouse primitive
  Fixing follow-mouse by repainting-internal since we are already inside the pane
  process which is following the mouse drag.

- boxer-sunrise-32 Added binary releases page to top-level readme.

- crash-fix
  - Null checking screen-rows array
  - Check for null  before de-allocating them

- refactor
  - Removing :boxer-input mouse parameterization.
  - Moving boxer-eval-queue and related command loops to separate file to
    allow usage outside of CAPI/opengl
  - See boxer-bugs-57 for the beginning of boxer styles centralization

- doco
  - Added more contributors to the README


- the-attic
  - Removing lispworks6 version of quit
  - Removed more old 3600, TI, and MCL code
  - Removing unused methods start-box-copyright-warning, with-open-blinkers,
    redisplayable-window?, kill-redisplayable-window from boxwin-opengl
  - Moved out old versions of commands in comsa
  - Removing old -opengl redisplay cues, and minor Symbolics and MCL calls.
  - Moving Symbolics lisp machine specific functions in ev-int to the attic.
  - Removed binhex from asdf components
  - Properly deprecating mail primitives from mail.lisp and removing to the attic.
  - Properly deprecating mailfile primitives from mailfile.lisp and removing to the attic.
  - Properly decprecating gopher support and the 2 primitives gopher.lisp provided.
    (telnet, gopher-search)
  - Move commented out force-repaint-window to the attic.
  -

## 3.4.5 2021-05-25

- boxer-bugs-38 Updating default font sizes to be closer to Boxer pre 2014.

- boxer-sunrise-49 Fixing up type-font behavior
  - Moving back to MCL era of having type-font box in sprites contain the actual human readable
    fontspec rather than an internal integer.
  - Transitioning type-font from an invalid-value boxer-interface static variable to a special-value
    boxer-interface slot with a type-font-box-updater update function
  - Fixed update-type-font trigger to behave the same as if you were calling set-type-font
  - Adding an extra check for loading in font information from saved box files.

- boxer-sunrise-22 In-progress comments and work arounds.

- boxer-sunrise-26 Improved gesture scrolling
  - Looking possibly all the way to the *outermost-screen-box* now for a box to scroll
  - Going outward if current box does not have scrollbars
  - Going outward if mouse is over a :shrunk :supershrunk or :boxtop rendered box.

- boxer-sunrise-25 Preparatory HUD information for perf and rendering fixups.

- boxer-bugs-55
  - Fixing namespace on gl-vectors usage

- boxer-bugs-52 Initial fix-up of launching xref links with default Finder actions.

- Binding accel-a, accel-b, accel-i to Select Box (All), Toggle Bold, Toggle Italics

- boxer-sunrise-21 Major set of work reworking how font-ids are stored and work.
  - Removes the bit-field implementation and replaces that with a simple list of
    opengl-fonts, and some simple operators over that list.
  - Removes any limitations on having different font sizes, faces, etc.
  - Cleanup of outdated font comments
  - Renaming boxer-font params to font-no, removing unused font-stylep function.
  - Fixing font lookup on boxer::menu-item
    - Currenty the popup menu items are being created before the *default-font*
      is bound at startup.
    - Added an accessor for slot-value 'font to lookup the *default-font* in case
      it was nil during creation.
  - Fixing up font-style, font-size, and font-name functions
  - Fixing up font zooming to use a percentage as the multiplier, rather than a sliding list of
    predefined font sizes.
  - Updating the font bigger/smaller menus to incremenet/decrement in .25 chunks with a minimum
    of 0.5 zoom and a maximum of 4x zoom.
  - Updating freetype and opengl rendering methods to take an optional font-zoom parameter.
  - Removing %font-size-idx as font-size does the same thing
  - Removing relative saving var from dumper as all fonts are actual sizes now.
  - Removing never used drawing-font-map, sheet-font-map
  - Among other leftover debt bits from fonts.lisp
  - Adding back size translations for historic relative font size saves.

- boxer-sunrise-20
  - Moving graphics Macros definitions above their first usage.
  - Adding define-constant macro to work around oddities in CL defconstant behavior
  - Internal namespace accessors on opengl items and removing use of lispworks color module.
  - More seperation of platform specific libraries

- boxer-sunrise-18
  - Swapping out lw color module for regular vectors
  - Moving clipboard functionality (including last usage of color: ) to a lw-capi specific folder.
  - Removing last used graphics-port references from draw-low-opengl and bitmap functions

- crash-fixes
  - Disabling printing right now since it's broken, and hangs the system
  - Some undefined functions were getting called causing crashes.
  - Adding an extra null check
  - Sometimes we get here and there is no screen-box on the screen-row

- tests In-progress modernizing existing chunker tests to run as real unit tests and add more
  for pipes and other characters.

- re-org
  - Moving all the primitives into the unified primitives directory.
  - Moving (re)display definitions into the common definitions folder.
  - Starting to co-locate parts of the evaluator.
  - Merging evaldefs folder with evaluator
  - Merging evalvars folder with evaluator folder
  - Moving process.lisp to evaluator folder

- documentation Starting to fill out various README's and information on bits of the system.

- Continued formatting fixes and improvements across source.

- the-attic
  - Removed old `pkg.lisp`, `update-blinker-function`, more `#-opengl` code, `window-depth` function,
    old `trig` definitions, `#+3600` code, old `*uc-copyright-free*` decprecated features, `com-fat`,
    `com-nutri-system`, old SGI versions of drawing methods, old `define-eval-var` forms, unused variables
    from `boxdef` and `vrtdef`, unused `clip-x` and `clip-y` functions,
    `interval-update-repaint-current-rows`

## 3.4.4 2021-03-12

- Installer updates and expansions
  - This release we are including an experimental windows binary which requires installing the
    "Microsoft Visual C++ Redistributable for Visual Studio 2015, 2017 and 2019". Future releases
    will properly include these in the installer itself.
    https://support.microsoft.com/en-us/topic/the-latest-supported-visual-c-downloads-2647da03-1eea-4433-9aff-95f26a218cc0
  - We are including a DMG archive for the MacOS version now, with a traditional "drag and drop the binary
    in to the applications folder" type installer.  The DMG folder dimensions and artwork are a work in progress.
  - Moving forward we'll be slowly cleaning up and including more demos and microworlds with releases.
    This release we are including the "Annotated Cube" and "Button Factory" demos.

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

## 3.4.3 2021-01-27

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
