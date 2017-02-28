(* Single-session example with branching and selection on arbitrary labels. *)

(* This will be rewritten with macros ```match%branch0``` and ```[%select]```. See README.md *)

open Session
open Session0
       
type binop = Add | Sub | Mul | Div
       
let eval_binop = function
  | Add -> (+)   | Sub -> (-)
  | Mul -> ( * )  | Div -> (/)

let rec arith_server () =
  (*
  match%branch0 () with
  | `neg -> neg_server ()
  | `bin -> binop_server ()
  | `fin -> close ()
  *)
  _branch_start (function
     | `neg(p),r -> _branch (p,r) (Ex_single1.neg_server ())
     | `bin(p),r -> _branch (p,r) (binop_server ())
     | `fin(p),r -> _branch (p,r) (close ()): [`neg of 'p1 | `bin of 'p2 | `fin of 'p3] * 'a -> 'b)

and binop_server () =
  let%s op = recv () in
  let%s (x,y) = recv () in
  send (eval_binop op x y) >>
  arith_server ()

let arith_client () =
  [%select0 `bin] >>
  send Add >>
  send (150, 250) >>
  let%s ans = recv () in
  Printf.printf "Answer: %d\n" ans;
  [%select0 `fin] >>
  close () >>
  return ()

let arith_ch = new_channel ()
                           
let _ =
  ignore @@ Thread.create
    (accept_ arith_ch arith_server) ();
  connect_ arith_ch arith_client ()
