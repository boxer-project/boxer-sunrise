# Boxer File Format

This documents the Boxer file format, used for saving Boxer microworlds with the extension `.box`.

## Overview

Currently Boxer saves it's data using the file extension `.box` in a packed binary format that evokes the nostalgia of
older binary formats from MS Office such `.doc`, `.xls`, etc.  All assets are saved in this single binary streamed
document. Work is underway to save using separated assets using a zipped format similar to the way ODF, docx, and pages
files are stored.

### Binary Binary Format

The Boxer binary format is a binary format, consisting of units packed using 16-bit words.  A number of constants
specify operations, and the data that follows conforms to that operation. Sometimes this is just regular data, and
sometimes it more closely follows the layout of a lisp data structure or list to make it easier to reconstruct.  The
first operation always designates the version of the box format.

Most the constants and procedures for reading and writing this format can be found in `fildfs.lisp`, `dumper.lisp`, and
`loader.lisp`. All of these are in the `filesystem` module.

## Versions

Historically, the only "old" versions of box files we have are files saved using version 5, which was a stable release
used in schools running on Sun OS workstations.  From 2014 thru Boxer 3.4.9 version 12 was in use. Boxer 3.4.10 made
2 small changes in the format to bring the version to 13.

TODO: Check and see what version the old windows build was using. Probably 12.

### Version 1 thru 4

As of now little is known about these early versions and we don't have any box files saved in these versions.

### Version 5

We have a collection of old files using version 5. These come from a Sun OS version of Boxer that was used in public
schools and other educational institutions.

### Versions 6 thru 11

As of now little is known about these early versions and we don't have any box files saved in these versions.

### Version 12

Version 12 was used for a long period, from the last pre-sunrise build in 2014 thru Boxer v3.4.9 2022-05-05.

### Version 13

Version 13 makes some minor changes from 12, and was introduced in Boxer v3.4.10 2022-07-11.

Changes:
  - sunrise-36 Added alpha layer to pixmaps. Previously was only saving RGB values.

    https://github.com/boxer-project/boxer-sunrise/blob/38a2cc7f4d224318d8aaaeef3ea55ddb4cd1fc6a/src/filesystem/dumper.lisp#L1139

    https://github.com/boxer-project/boxer-sunrise/blob/38a2cc7f4d224318d8aaaeef3ea55ddb4cd1fc6a/src/filesystem/loader.lisp#L1228

  - sunrise-52 Changed name rows to dump as string. Allows higher unicode characters in box names.

    https://github.com/boxer-project/boxer-sunrise/commit/d843d7374557bbd1619747de82956bb707e66edd

### Version 14

Version 14 is a marker version to note that a new graphics list command has been added for
graphics-sheets-graphics-list, such that any Boxer documents saved with this graphics command will not
open in earlier versions of Boxer.

  - Added in Boxer version 3.4.14
  - New graphics command is 37, allowing rotation/translation/scaling of turtle drawing operations by supplying
    a 4x4 transformation matrix.
    ```
    37   BOXER-TRANSFORM-MATRIX     (MATRIX) 4x4 matrix packed in 1x16 single-float vector
    In order to fix/support stamp-self with the same rotations/scaling/etc with bitmaps and all
    primitives we need to move beyond the simple hand transforms. This graphics display list
    command allows putting an arbitrary 4x4 transformation matrix in the graphics list to
    affect upcoming commands.
    ```
