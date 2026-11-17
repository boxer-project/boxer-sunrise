# Boxer Sunrise

This is the current iteration of the historic Boxer project.

## Release Downloads

The current stable version of Boxer is a desktop application built using Lispworks and verified on recent versions
ARM/Intel macOS. (Sequioa, Tahoe, Golden Gate).

Packaged binary releases can be found on the Github Releases page:

https://github.com/boxer-project/boxer-sunrise/releases

## Supported Platforms

Current Boxer development is focused around 3 deliverables:

- A desktop application built with Lispworks using openGL. This is currently supported on macOS.  Other platforms (Linux, Windows)
  may also work but are not actively verified. [Build instructions](boxwin/lw-capi/README.md)
- A C shared/static library built with ECL that contains the core Boxer evaluator, primitives, and other functionality.
- An in-progress Godot frontend built on top of the C library, positioned to allow future multimedia and 3D support, as
well as supporting more platforms including WASM.

## Setting up your Development environment

## Boxer Core

Parts of Boxer that should run on most common lisp implementation are being slowly factored in to the
`boxer-sunrise-core` asdf system.

Examples of running the unit tests on ECL, SBCL, and Lispworks are below:

```sh
# I like to wrap these in rlwrap in case I need to use the REPL, but it's optional

#ECL
rlwrap ecl --load ./run-tests.lisp

#SBCL
rlwrap sbcl --load ./run-tests.lisp

#Lispworks
# From the editor run:

```

## Boxer Shared Library / ECL

Using ECL, the core Boxer engine and evaluator can be compiled down to a dynamic library, capable of being linked
against in a C/C++ application.  This does not include the repaint or redisplay modules.  You'll need to have ECL
and scons installed.  At the moment we're using scons just because one of the initial embedding environments we are
working towards is Godot.

To build and run the small test example:

```sh
# You need to have uiop from asdf somewhere in either your quicklisp local-projects, or in your boxer-sunrise tree
# somewhere.  uiop is a top level asd module in the asdf repo
# git clone https://gitlab.common-lisp.net/asdf/asdf.git

build-boxer-shared-lib.sh # you may need to run this twice...
scons
./embedded-boxer
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
