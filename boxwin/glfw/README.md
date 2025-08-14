# Boxer GLFW

This module contains a very much in progress version of Boxer that uses the
[GLFW](https://www.glfw.org/) library as it's openGL rendering and input device
environment. It can be run with both SBCL and ECL, although start up on ECL can
currently be very slow. From the top level project directory run:

```sh
sbcl --load boxwin/glfw/bootstrap-glfw.lisp
```
