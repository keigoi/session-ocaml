#Session-ocaml

Session-ocaml is an implementation of session types in OCaml.

## How to try it

Prepare your favourite OCaml installation and install ```findlib```.
We recommend to use ```opam```.

And type at the top directory of this distribution:

	ocaml setup.ml -configure
	ocaml setup.ml -build

Then you can play with ```session-ocaml```:

	ocaml -short-paths

Argument ```-short-paths``` is optional (it makes ```ocaml``` show the shortest path for each type).
Note that [.ocamlinit](.ocamlinit) file automatically pre-loads all required packages here and sets -rectypes option.
It also does ```open Session```.

## Example

* [A single session 1](examples/ex_single1.ml).
* [A single session 2](examples/ex_single2.ml).
* [Multiple sessions 1](examples/ex_multi1.ml).

To compile examples, you need to install ```session-ocaml``` by:

	ocaml setup.ml -install

Then, you can build them by typing ```make``` inside ```examples``` directory.

To uninstall ```session-ocaml```, type:

	ocaml setup.ml -uninstall

Or,

	ocamlfind remove session-ocaml


# Macro for branching / selection

For branching on arbitrary labels, we provide a macro ```match%branch0``` and ```match%branch```.

Single-channel case (```open Session0```):

```ocaml
  match%branch0 () with
  | `apple  -> send 100
  | `banana -> recv ()
  | `orange -> send "Hello!"
```

Its protocol type will be:

```
  [`branch of resp *
    [ `apple of [`msg of req * int * 'a]
    | `banana of [`msg of resp * 'v * 'a]
    | `orange of [`msg of req * string * 'a]]
```

Multi-channel case (```open SessionN```):

```ocaml
  match%branch _2 with
  | `batman  -> [%select _2 `goodbye]
  | `ironman -> let%s x = recv _2 in send _2 x
  | `hulk    -> send _2 "foobar"
```

Protocol type:

```
  [ `branch of resp *
    [ `batman  of [ `branch of req * _[> `goodbye of '_e ] ]
    | `hulk    of [ `msg of req * string * '_e ]
    | `ironman of [ `msg of resp * '_f * [ `msg of req * '_f * '_e ] ] ] ]
```

  Similarly, we have a macro for selection, like 

```ocaml
  [%select0 `label]
```

or

```
  [%select _n `bark]
```

## TODO

* Better error reporting inside %branch0 and %branch

----
author: Keigo IMAI (@keigoi on Twitter / keigoi __AT__ gifu-u.ac.jp)
