# Session-ocaml

Session-ocaml is an implementation of session types in OCaml.

(__NEW__: the [project page](http://www.ct.info.gifu-u.ac.jp/~keigoi/session-ocaml/) is now open at: [http://www.ct.info.gifu-u.ac.jp/~keigoi/session-ocaml/](http://www.ct.info.gifu-u.ac.jp/~keigoi/session-ocaml/))

## How to try it

Prepare OCaml __4.05__ and install ```findlib```, ```ocamlbuild```, ```ppx_tools```.
We recommend to use ```opam```

Install the compiler and prerequisite libraries. (__NOTE__: the version number has changed:  4.03 ==> __4.05__)

    opam switch 4.05.0
    eval `opam config env`
    opam install ocamlfind ocamlbuild ppx_tools

Then clone the repository and type following at the top directory:

    git clone https://github.com/keigoi/session-ocaml.git
    cd session-ocaml
    ./configure --prefix=$(dirname `which ocaml`)/..
    make
    make install

Then you can play with ```session-ocaml```:

    cd examples
    make                       # build examples
    rlwrap ocaml -short-paths  # play with OCaml toplevel (utop will also do).
                               # rlwrap is a readline wrapper (recommended)

Argument ```-short-paths``` is optional (it makes ```ocaml``` show the shortest path for each type).
Note that [.ocamlinit](examples/.ocamlinit) file automatically pre-loads all required packages into OCaml toplevel and sets -rectypes option.
It also does ```open Session```.

If things seem broken, try ```git clean -fdx```then ```make``` (WARNING: this command erases all files except the original distribution).
Also, you can uninstall manually by ```ocamlfind remove session-ocaml```.

## Example

* [A single session 1](examples/ex_single1.ml).
* [A single session 2](examples/ex_single2.ml).
* [Multiple sessions 1](examples/ex_multi1.ml).
* [SMTP protocol](examples/smtp.ml).

# Macro for branching / selection

For branching on arbitrary labels, we provide a macro ```match%branch```.

```ocaml
  open Session
  match%branch s with
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

  Similarly, we have a macro for selection, like

```ocaml
  [%select s `label]
```

----
author: Keigo IMAI (@keigoi on Twitter / keigoi __AT__ gifu-u.ac.jp)
