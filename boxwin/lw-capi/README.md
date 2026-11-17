# Boxer Sunrise Lispworks

This document contains instructions for building, testing, and running Boxer Desktop on Lispworks 8.1.2.

Boxer can be run and developed against using the freely available version of Lispworks. However, in order to
build deliverable binaries a professional license is required. It's a known issue that the linux version of Lispworks runs out of heap when loading Boxer.

## Setting up your Development environment

### macOS / Lispworks CAPI

- Install Lispworks 8.0, including any patches available from Lispworks.
- Install Quicklisp

  Be sure to include Quicklisp as part of your init file. In the [Quicklisp install
  instructions](https://www.quicklisp.org/beta/#installation) this happens during
  `(ql:add-to-init-file)`.
  Do note that the free version of Lispworks does not support init files, and you will
  need to manually load `quicklisp/setup.lisp` from the location of your QL installation.
  ```lisp
  ; example on windows
  (load "Z:/quicklisp/setup.lisp")
  ```
- Install our patched cl-freetype2, cl-opengl, and zip projects.  These are small workarounds, and ideally will
  be removed at some point in the future.
  ```
  # substitute for your quicklisp install location
  cd ~/quicklisp/local-projects
  git clone --branch=lispworks-fixup https://github.com/sgithens/cl-freetype2.git
  git clone --branch=lispworks-fixup https://github.com/sgithens/cl-opengl.git
  git clone --branch=lispworks-fixup https://github.com/sgithens/zip.git
  ```
- Install freetype2 libraries, headers, etc
  ```
  brew install freetype2
  ```
- `git clone git@github.com:boxer-project/boxer-sunrise.git`


### Windows 10 / Lispworks CAPI

These are the same as the above with the exception of the freetype2 libraries. Here is one way to install them using
MSYS2. You may choose to use a different build system for Windows.

- Install MSYS2 64-bit from https://www.msys2.org/

- From msys2 install freetype2

  ```
  pacman -S mingw-w64-x86_64-toolchain
  pacman -S mingw-w64-x86_64-freetype
  ```

- Add `C:\msys64\mingw64\bin` to the windows `PATH` environment variable.

You also need the patched cl-freetype2 bindings, as above (clone it under `%HOMEPATH%/quicklisp/local-projects/`).

### Building the Boxer executable on MacOS / Lispworks CAPI

*in-progress These instructions are being updated still for several binary libraries that need to be included
in the build*

MacOS application bundles can be created with the following delivery script. This will assume you have lispworks
installed and have the executable in the path. This has been tested with Lispworks 8.0. While you can run Boxer
from the personal edition of Lispworks, you will need on of the paid Profession versions that include the framework
for creating application binaries. (In Lispworks this is called the `delivery` framework).

```bash
git clone git@github.com:sgithens/boxer-sunrise.git
cd boxer-sunrise
lispworks -build src/delivery-script.lisp
```

You will now find a double-clickable MacOS application in `boxer-sunrise/data/boxersunrise.app`.

### Running Boxer

With the above dependencies installed and a lispworks Listener open, the following will compile and startup Boxer (adjusting the
path accordingly to your system.)

```lisp
;; This needs to be a full path to the bootstrap file
(load #P"~/code/boxer-sunrise/core/bootstrap.lisp")
```

To run the tests use:

```lisp
(load #P"~/path/to/boxer-sunrise/run-tests.lisp")
```
