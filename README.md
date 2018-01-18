Hi, Emacs! -*- mode: gfm -*-

# `GENSYM` for LFE

## Table of Contents
   * [GENSYM for LFE](#gensym-for-lfe)
      * [Introduction](#introduction)
      * [Simple Tutorial](#simple-tutorial)
      * [Internals](#internals)
         * [Atom table limit](#atom-table-limit)
         * [Symbol naming](#symbol-naming)
      * [Table of Contents](#table-of-contents)
      * [Dictionary](#dictionary)
         * [GENSYM [function]](#gensym-function)
         * [GENSYM_COUNTER [function]](#gensym_counter-function)
         * [GENSYM_LIMIT [function]](#gensym_limit-function)
         * [ONCE-ONLY [macro]](#once-only-macro)
         * [SYSTEM_INFO [function]](#system_info-function)
         * [WITH-GENSYMS [macro]](#with-gensyms-macro)

## Introduction
This project is an attempt at adding `gensym` functionality to LFE, giving
hackers a mechanic to help avoid the accidental capture of identifiers when
writing macros.

## Simple Tutorial
If LFE is your first exposure to Lisp, you might not have come across `gensym`
before, so I will attempt to describe it here and show an example of its usage.

First, let us consider the following trivial macro:
``` lisp
(defmacro square (value)
  `(* ,value ,value))
```
At first glance, the macro is simple enough; it generates code that multiplies
`value` by itself.  This is a highly trivialised example that you will probably
never see in the real world.

Now, let us imagine that `square` is invoked with a function as its
argument.  Imagine that the function is:
``` lisp
(defun random (limit)
  (erlang:trunc (* (rand:uniform) limit)))
```
What happens when we invoke `(square (random 10))`?  Place bets now.

Let's walk through an exansion:
``` lisp
  (square (random 10))
⇒ (* (random 10) (random 10))
⇒ (* 3 (random 10))
⇒ (* 3 8)
⇒ 24
```
Unless you already knew about this issue, this result is probably not what you
were expecting.

This is where `gensym` comes into play.  The `gensym` function itself returns a
fresh symbol that can be used inside a macro body in order to provide a means of
temporarily storing a value.

We could re-write `square` in this way:
``` lisp
(defmacro square (value)
  (let ((temp (gensym)))
    `(let ((,temp ,value))
       (* ,temp ,temp))))
```
Now when we invoke `(square (random 10))`, the following expansion occurs:
``` lisp
  (square (random 10))
⇒ (let ((sym_1 (random 10)))
    (* sym_1 sym_1))
⇒ (let ((sym_1 8))
    (* sym_1 sym_1)
⇒ (let ((sym_1 8))
    (* 8 sym_1)
⇒ (let ((sym_1 8))
    (* 8 8)
⇒ 64
```
As you can see, this technique allows us to ensure that evaluation only occurs
once.

This might look like a lot of typing, but imagine we could do something like:
``` lisp
(defun square (x)
  (with-gensyms (temp)
    `(let ((,temp ,x))
       (* ,temp ,temp))))
```
Turns out that we can.  This package provides an `with-gensyms` macro in
`include/lfe_gensym.lfe`.

What if we want to write the `square` macro like this:
``` lisp
(defun square (x)
  (once-only (x)
    `(* ,x ,x)))
```
Turns out that we can write the macro like this, too.  This package also
provides a `once-only` macro in `include/lfe_gensym.lfe`.

## Internals
### Atom table limit
In an attempt to avoid causing Erlang to hard crash due to exhaustion of the
atom table, this gensym implementation defaults to allowing only 10% of the atom
table to be used for gensym's purposes.  Once this limit is reached, `gensym`
will return a symbol that already exists.  This should not cause any issues,
however.

With that said, this has not been tested with distributed code, so caveat emptor.

### Symbol naming
Unlike other Lisps where `gensym` takes an argument and/or variables like
`*gensym-counter*` can be modified to influence the name and/or numeric
component of the next symbol generated, this implementation takes zero
arguments.  This is to help prevent atom table exhaustion, and should not have
any detrimental affect on code, but it is something to watch out for when
porting code from other Lisp implementations.

## Dictionary

### `GENSYM` [function]
#### Syntax:
`gensym` ⇒ *new-symbol*

#### Arguments and Values:
* *new-symbol* — a fresh symbol.

#### Description:
Creates and returns a new symbol.  The symbol's prefix is of the form
*sym_<number>*; e.g. `sym_1`.  The *number* is incremented each time.

The value of *number* is that of `gensym_counter`.

#### Examples:
``` lisp
(gensym) ⇒ sym_24
```

#### Side Effects:
The resulting symbol is really an Erlang atom, and is interned in Erlang's atom
table.

Will increment `gensym_counter`.

Should the gensym counter reach the limit (as returned by `gensym_limit`), the
counter will wrap around to 1.

#### Affected By:
* `gensym_counter`

#### See Also:
* [`gensym_counter`](#gensym_counter-function)
* [`gensym_limit`](#gensym_limit-function)

**Notes**:
1. Unlike Common Lisp or Maclisp, it is not possible to modify the name of the 
   generated symbol.  This is to try to avoid allowing one to shoot oneself in
   the foot by creating too many Erlang atoms.

### `GENSYM_COUNTER` [function]
#### Syntax:
`gensym_counter` ⇒ *number*

#### Arguments and Values:
* *number* — a non-negative integer.

#### Description:
Returns a numeric value that will be used in constructing the name of the next
symbol generated by `gensym`.

#### Examples:
``` lisp
(gensym_counter) ⇒ 18
(gensym) ⇒ sym_18
(gensym_counter) ⇒ 19
```

#### Affected By:
* `gensym`

#### See Also:
* [`gensym`](#gensym-function)

#### Notes:
1. Unlike Common Lisp, this is a function and not a variable.

### `GENSYM_LIMIT` [function]
#### Syntax:
`gensym_limit` ⇒ *number*

#### Arguments and Values:
* *number* — a non-negative integer.

#### Description:
Returns the maximum amount of symbols that may be created by `gensym`.

#### Example:
``` lisp
(gensym_limit) ⇒ 104857
```

#### See Also:
* [`system_info`](#system_info-function)

#### Notes:
1. The gensym limit shall be considered to be a value that is approximate to 10%
   of the Erlang atom table size.  This can be changed with the `-t` CLI
   argument.

### `ONCE-ONLY` [macro]
#### Syntax:
`once-only` `(`*symbol_1 symbol_n ...* `)` *form ...* `)` ⇒ *result*

#### Arguments and Values:
* *symbol* — a symbol.
* *form* — a form.
* *result* — a form.

#### Description:
Execute *form* in a context where the values bound to the symbols in
*symbol_1* through *n* are bound to fresh gensyms, and the symbols in
*symbol_1* through *n* are bound to the corresponding gensym.

This macro ensures that symbols passed as variables are only executed once
within the context of the body form.

Taken from Peter Norvig's *Paradigms of Artificial Intelligence Programming*.

#### Example:
``` lisp
  (defun random (limit)
    (erlang:rem
     (erlang:trunc (* (rand:uniform) limit))
     limit))
⇒ random
  (defmacro square (x)
    (once-only (x)
      `(* ,x ,x)))
⇒ square
  (macroexpand-all '(square (random 10)) $ENV)
⇒ (let ((sym_1 (random 10)))
    (call 'erlang '* sym_1 sym_1))
  (square (random 10))
⇒ 64
```

#### See Also:
* [`with-gensyms`](#with-gensyms-macro)

### `SYSTEM_INFO` [function]
#### Syntax:
`system_info` ⇒ `(tuple` *limit count* `)`

#### Arguments and Values:**
* *limit* — a non-negative integer.
* *count* — a non-negative integer.

#### Description:
Returns information about the gensym system.

*Limit* is the maximum number of symbols that can be generated with `gensym`.
*Count* is the total number of symbols that have been created with `gensym`.

#### Example:
``` lisp
(lfe_gensym:system_info) ⇒ #(104857 0)
(lfe_gensym:gensym) ⇒ sym_1
(lfe_gensym:system_info) ⇒ #(104857 1)
```

#### Affected By:
* `gensym`

#### See Also:
* [`gensym_limit`](#gensym_limit-function)

### `WITH-GENSYMS` [macro]
#### Syntax:
`with-gensym` `(`*symbol_1 symbol_n ...* `)` *form ...* `)` ⇒ *result*

#### Arguments and Values:
* *variable* — a symbol.
* *form* — a form.
* *result* — a form.

#### Description:
Execute *form* in a context where the values bound to the symbols in
*symbol_1* through *n* are bound to fresh gensyms

Taken from Paul Graham's *On Lisp*.

#### Example:
``` lisp
  (defmacro nif (val pos zero neg)
    (with-gensyms (thing)
      `(let ((,thing ,val))
        (cond ((> ,thing 0)  ,pos)
              ((== ,thing 0) ,zero)
              (else          ,neg)))))
⇒ nif
  (macroexpand-all '(nif 42 'positive 'zero 'negative) $ENV)
⇒ (let ((sym_14 42))
    (if (call 'erlang '> sym_14 0)
      (progn 'positive)
      (if (call 'erlang '== sym_14 0)
          (progn 'zero)
          (progn 'negative))))
  (nif 42
    (io:format "Positive~n" '())
    (io:format "Zero~n" '())
    (io:format "Negative~n" '()))
⇒ Positive
```

#### See Also:
* [`once-only`](#once-only-macro)
