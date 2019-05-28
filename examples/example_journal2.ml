open Session_ocaml.Session
let xor : bool -> bool -> bool = (<>)
let print_bool = Printf.printf "%B"
type binop = And | Or | Xor | Imp
let log_ch = new_channel ()
let eval_op = function
  | And -> (&&)
  | Or -> (||)
  | Xor -> xor
  | Imp -> (fun a b -> not a || b)
let rec logic_server () =
  branch ~left:(s, fun () ->
      recv s >>= fun op ->
      recv s >>= fun (x,y) ->
      send s (eval_op op x y) >>= fun () ->
      logic_server ())
    ~right:(s, fun () -> close s);;
Thread.create
  (accept_ log_ch logic_server) ();;
connect_ log_ch (fun () ->
  select_left s >>
  send s And >>
  send s (true, false) >>
  recv s >>= fun ans ->
  (print_bool ans;
  select_right s >>
  close s)) ()
