open Linocaml.Direct
open Session_ocaml.Direct.Net

type 'a t = <s:'a>
[@@runner][@@deriving lens]

type tcp = in_channel * out_channel

(* type class instances *)
module Senders = struct
  (* send a string *)
  let _f : tcp -> string -> unit =
    fun _ _ -> failwith "TODO"

  (* send "GET" label *)
  let _g : tcp -> [`GET of _] -> unit =
    fun _ (`GET(_)) -> failwith "TODO"
end

module Receivers = struct
  (* receive 200/404 label*)
  let _f : unit -> [`_200 of ('p, 'x, unit) sess | `_404 of ('q,'y,unit) sess] = fun _ -> failwith "TODO"
end

                 
let connect_tcp : string -> int -> ('p, tcp) connector =
  fun host port () ->
  (* TODO *)
  IO.return {conn=failwith "TODO";close=(fun () -> IO.return ())}

let cli () =
  let google = connect_tcp "www.google.com" 80
  in
  let%lin #s = connect google in
  select s (fun x->`GET(x)) >>
  send s "/index.html" >>
    branch s >>=
    function%lin
          | (`_200(#s)) ->
             let%lin html = receive s in
             close s
          | (`_404(#s)) ->
             let%lin html = receive s in
             close s

