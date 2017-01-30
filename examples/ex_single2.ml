(* Single-session example with branching and selection on arbitrary labels. *)

(* This will be rewritten with macros ```match%branch0``` and ```[%select]```. See README.md *)

open Session
open Session0
       
type binop = Add | Sub | Mul | Div
       
let eval_binop = function
  | Add -> (+)   | Sub -> (-)
  | Mul -> ( * )  | Div -> (/)

let rec arith_server () =
  _branch_start (function
     | `neg(p),r -> _branch (p,r) (Ex_single1.neg_server ())
     | `bin(p),r -> _branch (p,r) (binop_server ())
     | `fin(p),r -> _branch (p,r) (close ()): [`neg of 'p1 | `bin of 'p2 | `fin of 'p3] * 'a -> 'b)

and binop_server () =
  recv () >>= fun op ->
  recv () >>= fun (x,y) ->
  send (eval_binop op x y) >>
  arith_server ()

let arith_client () =
  _select (fun x -> `bin(x)) >>
  send Add >> send (150, 250) >>
  recv () >>= fun ans ->
  Printf.printf "Answer: %d\n" ans;
  _select (fun x -> `fin(x)) >>
  close () >>
  return ()

let arith_ch = new_channel ()
                           
let _ =
  ignore @@ Thread.create
    (accept_ arith_ch arith_server) ();
  connect_ arith_ch arith_client ()
