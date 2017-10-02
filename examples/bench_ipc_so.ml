let start = ref @@ Core.Int63.of_int 0
let now () = (Core.Time_stamp_counter.now () :> Core.Int63.t)

open Linocaml.Direct
open Linocaml.Direct.Syntax
open Session_ocaml.Direct.Net

type prot = [`branch of req * branch]
and  branch = Branch of
                [`True of ([`msg of req * int * [`msg of req * int * [`msg of resp * int * prot]]], serv, stream) sess
                |`False of ([`close], serv, stream) sess]

module Receivers = struct
  let _branch
      : _ -> branch
    = fun c -> if input_value c.in_ then Branch(`True(_mksess c)) else Branch(`False(_mksess c))
  let _int c : int = input_value c.in_
  let _unit c : unit = input_value c.in_
end
module Senders = struct
  let _branch = fun c ->
    function
    | Branch(`True(_)) -> output_value c.out true; flush c.out
    | Branch(`False(_)) -> output_value c.out false; flush c.out
  let _int c (i:int) =   output_value c.out i; flush c.out
  let _unit c (i:unit) =   output_value c.out i; flush c.out
end
module Closers = Tcp.Closers
   
(* type ('a,'b,'c) ctx = <s:'a;t:'b;u:'c> [@@runner][@@deriving lens] *)
let s = {Linocaml.Lens.get=(fun x -> x); put=(fun _ x -> x)}
let run_ctx f x = Linocaml.Direct.Syntax.Internal.__run (f x) Linocaml.Base.Empty
  
let cOUNT = 8000

let rec server () =
  let rec aux () =
  branch s >>= (function%lin
  | Branch(`True(#s)) ->
      receive s >>= fun%lin n ->
      receive s >>= fun%lin m ->
      send s (n + m) >>= fun%lin () ->
      aux ()
  | Branch(`False(#s)) ->
     close s >>= fun%lin () ->
     return () : (branch lin, _, _, _) bindfun)
  in
  send s () >>
  aux ()
  

let client cnt () =
  let rec aux acc n =
    if n = 0 then begin
      select s (fun x -> Branch(`False(x))) >>
      close s >>
      return acc
    end else begin
      select s (fun x -> Branch(`True(x))) >>
      send s acc >>
      send s n >>
      receive s >>= fun%lin res ->
      aux res (n - 1)
    end
  in
  receive s >>= fun%lin (_:unit data) ->
  start := now ();
  aux 0 cnt
		      
let run () =
  ignore @@ Tcp.fork (server ()) (client cOUNT ());
  let countpersec =
    let c = (cOUNT * 4 + 1) * 1000000000 in
    Core.Int63.(of_int c / (now () - !start))
  in
  print_endline (Core.Int63.to_string countpersec)

let _ =
  run ()
