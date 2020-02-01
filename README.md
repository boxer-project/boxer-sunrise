# Boxer-Sunrise

This is an in-progress reconstruction of the Boxer project based on it's original common
lisp source code.  In addition to bringing the Boxer application and medium back to working
order, it is also an effort to modernize the lisp source.

## Usage

### Running the unit tests

Currently these basic tests and bootstrapping code should work on Lispworks, SBCL, or another
modern lisp implementation with `asdf` and `quicklisp` installed.  Currently they should be run
from inside this top level project directory.

```bash
# in SBCL
cd boxer-sunrise/
sbcl --load ./run-tests.lisp
```

## Authors

* Andrea A. diSessa
* Edward H. Lay
* Steven W. Githens
