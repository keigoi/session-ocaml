#Session-ocaml

Session-ocaml is an implementation of session types in OCaml.


## How to try it

	ocaml setup.ml -configure
	ocaml setup.ml -build
	ocaml -rectypes -I +threads -I _build/lib unix.cma threads.cma session-ocaml.cma


## Example

* [A single session 1](examples/ex_single1.ml).
* [A single session 2](examples/ex_single2.ml).
* [Multiple sessions 1](examples/ex_multi1.ml).

## Wishlist

### Macro for branching / selection

For branching on arbitrary labels, we will provide a macro ```match%branch0```:

```ocaml
  match%branch0 () with
  | `neg -> neg_server ()
  | `bin -> binop_server ()
  | `fin -> close ()
```

which will expand to:

```ocaml
  _branch_start (function
     | `neg(p),r -> _branch (p,r) (neg_server ())
     | `bin(p),r -> _branch (p,r) (binop_server ())
     | `fin(p),r -> _branch (p,r) (close ())
     : [`neg of 'p1 | `bin of 'p2 | `fin of 'p3] * 'a -> 'b)
```     

  Similarly, we are developing a macro for selection, like 

```ocaml
  [%select0 `label]
```

  which expands to

```
  _select (fun x -> `label(x))
```  

----
author: Keigo IMAI (@keigoi on Twitter / keigoi __AT__ gifu-u.ac.jp)
