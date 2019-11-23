# Boxer-Sunrise2

A reworking of the boxer source to run on straight common lisp without
dependencies on specific lispwork or other features. Should run on lispworks
or SBCL.

## Usage

Currently, this is being tested/run with the portacle emacs and common lisp
distribution. Substituting your code path below should run the project in
a fresh copy of portacle.

```lisp
 (setf asdf:*central-registry*
               (list* '*default-pathname-defaults*
                      #P"/Users/sgithens/code/boxer-sunrise2/"
                asdf:*central-registry*))

(asdf:test-system :boxer-sunrise2)
```

## R&D Notes

### Running cl-opengl examples

In portacle the following can be run.  You do need to have freeglut or some
other package that includes libglut3 installed.

On Ubuntu this can be installed with `apt-get install freeglut3`.

```lisp
(ql:quickload 'cl-opengl)
(asdf:load-system 'cl-glut-examples)
(cl-glut-examples:run-examples)
```

## Author

* Steven Githens (steve@githens.org)

## Copyright

TODO get proper license headers for boxer
Copyright (c) 2019 Steven Githens (steve@githens.org)
