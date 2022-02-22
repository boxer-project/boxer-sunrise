# Boxer Sunrise

This is the current interation of the historic Boxer project.

## Release Downloads

Packaged binary releases can be found on the Github Releases page:

https://github.com/boxer-project/boxer-sunrise/releases

## Supported Platforms

Currently Boxer is tested on the following platforms:

- Apple Intel: macOS 10.14 Mojave through macOS 12 Monterey
- Apple M1: macOS 11 Big Sur and macOS 12 Monterey
- x86 Windows 10

These all use Lispworks 7.1.2.

Boxer can be run and developed against using the freely available version of Lispworks. However, in order to
build deliverable binaries a professional license is required.

Work is in progress to run Boxer against fully open source versions of common lisp, focused primarily on SBCL.
Additionally the ability to run the core boxer evaluator separately to integrate with new web ecosystems (such
as node and a possible server side Boxer).

## Setting up your Development environment

### macOS

- Install Lispworks 7.1.2, including the most up to date patches for macOS Big Sur and Monterey
- Install Quicklisp
- Install our patched cl-freetype project

  ```
  cd ~/quicklisp/local-projects
  git clone git@github.com:sgithens/cl-freetype2.git
  cd cl-freetype2
  git checkout -b lispworks-fixup origin/lispworks-fixup
  ```
- Install freetype2 libraries, headers, etc
  ```
  brew install freetype2
  ```
- `git clone git@github.com:boxer-project/boxer-sunrise.git`


### Windows 10

These are the same as the above with the exception of the freetype2 libraries. Here is one way to install them using
MSYS2. You may choose to use a different build system for Windows.

- Install MSYS2 64-bit from https://www.msys2.org/

- From msys2 install freetype2

  ```
  pacman -S mingw-w64-x86_64-toolchain
  pacman -S mingw-w64-x86_64-freetype
  ```

- Add `C:\msys64\mingw64\bin` to the windows `PATH` environment variable.

### Notes

The personal version of lispworks may require you to load quicklisp manually on each startup, as it does not support
init files.
```lisp
; windows
(load "Z:/quicklisp/setup.lisp")
```

### Running Boxer

With the above dependencies installed and a lispworks Listener open, the following will compile and startup Boxer (adjusting the
path accordingly to your system.)

```lisp
;; This needs to be a full path to the bootstrap file
(load #P"~/code/boxer-sunrise/src/bootstrap.lisp")
```

### Building the Boxer executable on MacOS

*in-progress These instructions are being updated still for several binary libraries that need to be included
in the build*

MacOS application bundles can be created with the following delivery script. This will assume you have lispworks
installed and have the executable in the path. This has been tested with Lispworks 7.1. While you can run Boxer
from the personal edition of Lispworks, you will need on of the paid Profession versions that include the framework
for creating application binaries. (In Lispworks this is called the `delivery` framework).

```bash
git clone git@github.com:sgithens/boxer-sunrise.git
cd boxer-sunrise
lispworks -build src/delivery-script.lisp
```

You will now find a double-clickable MacOS application in `boxer-sunrise/data/boxersunrise.app`.

## Boxer Core

Parts of Boxer that should run on most common lisp implementation are being slowly factored in to the
`boxer-sunrise-core` asdf system.

Examples of running the unit tests on ECL, SBCL, and Lispworks are below:

```sh
# I like to wrap these in rlwrap in case I need to use the REPL, but it's optional

#ECL
rlwrap ecl --load ./run-core-tests.lisp

#SBCL
rlwrap sbcl --load ~/code/boxer-sunrise/run-core-tests.lisp

#Lispworks
# From the editor run:
(load #P"~/path/to/boxer-sunrise/run-core-tests.lisp")
```

## Authors through the years

* Andrea A. diSessa
* Hal Abelson
* David Neves
* Eric Tenenbaum
* Gregor Kiczales
* Edward H. Lay
* Leigh Klotz
* Jeremy Roschelle
* Maurice Anchor
* Steven W. Githens

## Source Acknowledgements

The start and stop icons are adapted from the Scratch project and subject to the BSD/Creative Commons License per
the rest of Scratch.
