# Boxer-Sunrise

This is an in-progress reconstruction of the Boxer project based on it's original common
lisp source code.  In addition to bringing the Boxer application and medium back to working
order, it is also an effort to modernize the lisp source.

## Usage

Currently, running the entire Boxer system depends on Lispworks as it makes use of it's capi, opengl, and
delivery frameworks. There are parts of the system that should run on any common lisp, and these are being
expanded over time.

### Building the Boxer executable on MacOS

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

### Running the unit tests and UI in development

The following bootstrap script can be run from a Lispworks listener to run the common lisp tests and then
start the Boxer canvas and UI.

```lisp
;; This needs to be a full path to the bootstrap file
(load #P"~/code/boxer-sunrise/src/bootstrap.lisp")
```

## Authors through the years

* Andrea A. diSessa
* Hal Abelson
* David Neves
* Eric Tenenbaum
* Gregor Kiczales
* Edward H. Lay
* Leigh Klotz
* Steven W. Githens
