# Boxer Walkthrough

This walkthrough is a narrative of the boxer data structures and source code.

## Data Structures

The core data structures for boxer are in `boxdef.lisp` which contain the primary classes and
structs for boxes, rows, and other items. The top level container is the `box` class with subclasses
`doit-box`, `data-box`, `port-box` and others for the various box types. `box` has clos accessors
for a name (which appears in it's top left tab in the display).

A box has a `superior-row` if it is contained somewhere in the document. A top level box for a world
has only `nil` for it's `superior-row`. The set of rows within a box are accessed by `first-inferior-row`
which is of type `row`.

The `boxer::row` class describes all rows inside a box, with `superior-box` pointing to it's containing
box. `previous-row` and `next-row` point to the previous and next rows. The `chas-array` is the substance
of the row, containing a vector with the row data. The first entry in the vector is a vector of
characters and boxes. The second entry is a number indicating the number of items in the row.
`TODO` The third and fourth entries are nil, it is to be determined still if these serve a purpose or
are just padding.

An example from a simple running boxer session:

```lisp
#(#(#\1 #\Space #\+ #\Space #\5 #\Space #\Space #\Space #\| #<DATA-BOX 6 > NIL NIL NIL NIL NIL NIL) 10 NIL NIL)
```

This row contains the expression `1 + 5` that has been executed by boxer, with the pipe delimiter
and a Data Box containing the result (the print preview here shows that it is 6). Then there is some
`nil` padding. The 10 indiates the number of characters and boxes, and excludes any `nil` padding.
