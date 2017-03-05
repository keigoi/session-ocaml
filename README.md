# Session-ocaml

Session-ocaml is an implementation of session types in OCaml.

(__NEW__: the [project page](http://www.ct.info.gifu-u.ac.jp/~keigoi/session-ocaml/) is now open at: [http://www.ct.info.gifu-u.ac.jp/~keigoi/session-ocaml/](http://www.ct.info.gifu-u.ac.jp/~keigoi/session-ocaml/))

## How to try it

Prepare OCaml 4.02.1 or later and install ```findlib```, ```ocamlbuild```, ```ppx_tools```.
We recommend to use ```opam``` and OCaml 4.03.0.

Install the compiler and prerequisite libraries.

    opam switch 4.03.0
    eval `opam config env`
    opam install ocamlfind ocamlbuild ppx_tools

Then clone the repository and type following at the top directory:

    git clone https://github.com/keigoi/session-ocaml.git
    cd session-ocaml
    make

Then you can play with ```session-ocaml```:

    cd examples
    make                # build examples
    ocaml -short-paths  # play with OCaml toplevel (utop will also do) 

Argument ```-short-paths``` is optional (it makes ```ocaml``` show the shortest path for each type).
Note that [.ocamlinit](examples/.ocamlinit) file automatically pre-loads all required packages into OCaml toplevel and sets -rectypes option.
It also does ```open Session```.

If things seem broken, try ```git clean -fdx``` then ```make``` (WARNING: this command erases all files except the original distribution):

## Example

* [A single session 1](examples/ex_single1.ml).
* [A single session 2](examples/ex_single2.ml).
* [Multiple sessions 1](examples/ex_multi1.ml).
* [SMTP protocol](examples/smtp.ml).

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
