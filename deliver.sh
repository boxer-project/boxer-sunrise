#!/bin/bash

# Because of the issue with not loading cl-freetype2 until application start, we
# are copying this source part to the area in the application bundle where it can
# still be available after delivery
cp src/draw/freetype-fonts.lisp data/boxersunrise.app/Contents/Resources/cl-deps

/Applications/LispWorks\ 7.1\ \(64-bit\)/LispWorks\ \(64-bit\).app/Contents/MacOS/lispworks-7-1-0-amd64-darwin -build src/delivery-script.lisp