# glossary

## alu

### alu-seta

### alu-xor

## bfd - Boxer Font Definition

## bp - Buffer Pointer

`boxdef.lisp:610` Structure

## cbos - Compiled build object stack

Appears in `compiler/comp.lisp`

## naive realism

From a user's perspective, what they see and edit on screen is not "source" that will affect some abstract hidden state, it _is_ the very world that Boxer computations run in.  Examples:

- Variables are not abstract, they are visible named boxes.
- Scope is not abstract, it is contained/containing boxes.
    - Well, with slight complication for transparent boxes, but note how "transparent" metaphor and visualization as weaker dashed border focus on what you see.
- After editing a variable or procedure (Doit box), you don't need to "reload the definition" — you've directly modified the variable/procedure.
- The stepper is the pinnacle of naive realism, letting you see the execution unfold, and values flow.
- The current locus of execution when stepping is represented as a cursor.
- The "Copy and execute" model made it safe to mutate the currently executing box!
    - The copied function body _is_ the stack frame — local parameters/variables exist here as named boxes.
    - `inputs` primitive is not just a declaration!  When executed, the names are replaced by actual named boxes, becoming local variables.
    - Thus, the call stack materializes as nested copied boxes, and a recursive function expands into a fractal, each level with its own locals.
    - Return values get shown inline in the calling box.

Naive realism was achieved by:

1. Co-designing the language and environment from the start with this as a goal!
2. Unusually tight integration between the evaluator and the environment.
    Most languages have cleanly separated editor->files->parser->AST->evaluator; in Boxer you'll see direct calls editor<->evaluator in both directions.
3. Sleight of hand!  A naive evaluator was too slow (at least decades ago), so behind the scenes a lot is optimized & cached.  When not stepping, most of the execution _does_ happen in hidden state, but care was taken to make this equivalent.

## explicit control evaluator

The `boxer-eval` state machine maintains its state in explicit global variables (one can think of them as registers) and stacks.
Procedure calls in Boxer grow Boxer's stack, not the underlying Lisp's stack.

This exposes the state for stepper / debugging / error handling.  It also allows some forms of context switching between "processes".

### pdl [from archaic "Push Down List"]

This is the main control stack.

It consists of frames of various types, each type saving/restoring a different subset of evaluator state.
Frame types are created by `define-stack-frame` macro, which defines macros to push/pop specifically those global variables.

Some general frame types (e.g. `ufun-frame`) serve the evaluator, and some serve specific primitives.
For example `repeat` primitive keeps the remaining number of iterations in the global variable `*repeat-count*`, and that's one of the things the `repeat-special-frame` PDL frame saves/restores.

### vpdl [from "Value Push Down List"]

A stack of values which will become arguments to functions.

The naive "copy and execute" model actually mutates the calling box, replacing calls with their return values, variables with their values etc.
As an optimization when not stepping, this can mostly be skipped — and VPDL stack holds the intermediate values.

It's also the interface to primitives.  `defboxer-primitive` looks similar to `defun` but constructs a lisp functions that pops arguments from VPDL, and pushes return value to VPDL.

### recursive evaluation

The evaluator calls primitives.  Primitives like `repeat` or `tell` that need to run Boxer code can't just call the evaluator (that would grow the Lisp stack and would be opaque to the stepper).

`defrecursive-eval-primitive` / `defrecursive-funcall-primitive` macros split each such primitive into a before, after, and cleanup parts, plus a custom PDL frame type.
Between the parts, control returns to the main eval loop, with state (e.g. `repeat`'s loop counter) kept in global variables, whose previous values are retained in the PDL frame.

### process

### sfun

### ufun

## vc - virtual copy

A persistent data structure representing boxes in a way optimized for the evaluator.  It simulates deep copying & subsequent mutations while still sharing data.

### multiple pointer

The central idea in VC data structure is it gets forked _at the point of mutation_. A "single pointer" gets promoted to a "multiple pointer" with slots retaining both original and new value(s), each slot tagged by "who" (usually an ancestor of the box) and "when" made the change.

`get-pointer-value` disambiguates the relevant values for a particalur copy & timestamp.

### mps

multiple pointer slot.

## Save Unders

If you look through the attic or other logs you may occasionally see references to
functions or data structures called `save-under`, `update-save-under`, `turtle-save-under`,
and similar things. Before switching to openGL, Boxer used several different retained mode
drawing api's, (ie. The scene wasn't repainted on each frame.) This means a lot of
complex state was manually kept track of such as the area that would be under a sprite.
(the `save-under`). Such that when the sprite was moved, that little bitmap of graphics
could be painted back under where the turtle sprite was. These little cached bits of
canvas that were under the sprites were even part of the save format so that when the
box file was loaded back in, the turtle could still move from where it was, preserving
the graphics that were underneath it.

## Variables in Boxer

All this applies equally to variable and procedure lookup by name, the rules are the same.
(Lookup finds a named box; only then you'll see if it's a data or doit box, and what to do with it.
In this sense, Boxer falls into ["Lisp-1" camp](http://www.nhplace.com/kent/Papers/Technical-Issues.html).)

To avoid confusion here, it's helpful to distinguish (1) the model users learn—and can observe by stepping (2) the semantics that arise from the model (3) the optimized implementation.

### model: Lookup follows box structure

To resolve a name, Boxer looks for a named box with same name (case insensitive).
It first checks inside current box (including closet), and recursing into transparent boxes it contains.
If not found, it checks in the box containing the current box, and so on.

### model: "Copy and execute"

A called doit box is copied into the calling box (which itself is usually a copy) before execution.

### resulting semantics: Dynamic scoping

The above rules mean that free variables in a called box will be looked up from the place it was called, not the place it was defined.

### implementation: "dynamic" then "static" lookup

Slow path: When stepping, we follow "Copy and execute" literally. A (virtual) copy is made, then mutated during execution.

Fast path: When not stepping, the called box may not get copied at all (not even a virtual copy)!  The state it would track gets dispersed in multiple places, notably:

* pdl stack tracks the chain of calls;
* vpdl stack holds args / return values;
* a separate "dynamic variables" stack of (name, value) pairs is maintained for name bindings in the non-existent temporary boxes.  Local bindings are pushed when entering a box, popped when returning.

In this mode, there is some outer structure of actual boxes that changes little — so was dubbed "static", and the inner short-lived boxes that were not actually created got dubbed "dynamic".

1. Name lookup first checks the dynamic variables stack.
2. If not found, "static" lookup is done following the box structure.
   Thanks to the structure changing little during execution, an additional cache was implemnted for this part.

[These "dynamic"/"static" terms are unfortunately confusing. Both have nothing to with `static` lifetime of C/Java family, and *both* are parts of implementing the same dynamic-scoping semantics!]

The slow path is sometimes implemented by running fast path, then adjusting. E.g. `step-replace-input-line-with-values` gets called after argument bindings were already pushed onto dynamic variables stack and has to pop them after creating actual named boxes.
