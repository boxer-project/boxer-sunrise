# Embedded Boxer

## Building and Running Demo

Using ECL, the core Boxer engine and evaluator can be compiled down to a dynamic library, capable of being linked
against in a C/C++ application.  This does not include the repaint or redisplay modules.  You'll need to have ECL
and scons installed.

To build and run the small test example, from the top level boxer-sunrise folder:

```sh
# You need to have uiop from asdf somewhere in either your quicklisp local-projects, or in your boxer-sunrise tree
# somewhere.  uiop is a top level asd module in the asdf repo
# git clone https://gitlab.common-lisp.net/asdf/asdf.git

./scripts/build-boxer-shared-lib.sh # you may need to run this twice...
pushd embedded
scons
./embedded-boxer
popd
```
