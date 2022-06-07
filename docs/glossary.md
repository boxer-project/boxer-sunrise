# glossery

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
- The "Copy and execute" model makes it safe to mutate the currently executing box!
    - Return values can be displayed inline.
    - `input` primitive, is not just a declaration!  When executed, the names are replaced by actual named boxes, becoming local variables.

This was achieved by:

1. Co-designing the language and environment from the start with this as a goal!
2. Unusually tight integration between the evaluator and the environment.
    Most languages have cleanly separated editor->files->parser->AST->evaluator; in Boxer you'll see direct calls editor<->evaluator in both directions.
3. Sleight of hand!  A naive evaluator was too slow (at least decades ago), so behind the scenes a lot is optimized & cached.  When not stepping, most of the execution _does_ happen in hidden state, but care was taken to make this equivalent.

## explicit control evaluator

The `boxer-eval` state machine maintains its state in explicit global variables (one can think of them as registers) and stacks.
Procedure calls in boxer grow boxer's stack, not the underlying Lisp's stack.

This exposes the state for stepper / debugging / error handling.  It also allows some forms of context switching between "processes".

### pdl (from archaic "Push Down List")

This is the main control stack.

It consists of typed frames, each saving/restoring a different subset of state.

### vpdl (from "Value Push Down List")

A stack of values which will become arguments to functions.

The naive "copy and execute" model actually mutates the calling box, replacing calls with their return values, variables with their values etc.
As an optimization when not stepping, this can mostly be skipped — and VPDL stack holds the intermediate values.

It's also the interface to primitives.  `defboxer-primitive` looks similar to `defun` but constructs a lisp functions that pops arguments from VPDL, and pushes return value to VPDL.

### recursive evaluation

The evaluator calls primitives.  Primitives like `repeat` or `tell` that need to run boxer code can't just call the evaluator (that would grow the lisp stack and would be opaque to the stepper).

`defrecursive-eval-primitive` / `defrecursive-funcall-primitive` macros split each such primitive into a before, after, and cleanup parts, plus a custom PDL frame type.  Between the parts, control returns to the main eval loop, with state (e.g. `repeat`'s loop counter) retained in the PDL frame.

### process

### sfun

### ufun

## vc - virtual copy

## Variables in Boxer

All this applies equally to variable and procedure lookup by name, the rules are the same.
(Lookup finds a named box; only then you'll see if it's a data or doit box, and what to do with it.
In this sense, Boxer falls into ["Lisp-1" camp](http://www.nhplace.com/kent/Papers/Technical-Issues.html).)

### Static

Under the "copy and execute" model, a called box is copied into the calling box.
Names are looked up directly in the hierarchy of boxes containing the current box.
This naturally resulting semantic is dynamic binding — free variables are can be bound by the caller.

That's what "static" lookup refers to — search up the hierarchy of materialized boxes.

(The term has nothing to do with C/Java meaning of "static" lifetime.)

### Dynamic

As an optimization, when not stepping, the called box may not be copied into the call site.
But it has to resolve variables _as if_ it was there.  So a separate dynamic bindings stack is maintained, and "dynamic" lookup refers to checking that stack.

### Transparent Boxes

Boxes marked transparent "leak" all names defined there into the surrounding scope.
This means static lookup can't just search up the ancestor chain, at each level it'd also need to recurse into any transparent siblings.

TODO: is this the motivation for variable cache mechanism?
