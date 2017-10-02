module Chan = Channel.Make
                (Linocaml.Direct.IO)
                (struct
                  type +'a io = 'a
                  include Mutex
                end)
                (struct
                  type +'a io = 'a
                  type m = Mutex.t
                  include Condition
                end)

include Base.Make(Linocaml.Direct)(Chan)

open Linocaml.Base

module Net = struct
  include Net.Make(Linocaml.Direct)(Chan)(Unsafe.Make_raw_dchan(Dchannel.Make(Chan)))

  type stream = {in_:in_channel; out:out_channel}

  module Tcp : sig
    val connector : host:string -> port:int -> ('p, stream) connector
    val new_domain_channel : unit -> ('p, stream) connector * ('p, stream) acceptor
    val fork
      : (('p, serv, stream) sess, empty, unit lin) monad
        -> (('p, cli, stream) sess, empty, 'v lin) monad
        -> 'v io

    module Closers : sig
      val _f : stream -> unit
    end
  end = struct      
    let make (fi, fo) = {in_=Unix.in_channel_of_descr fi; out=Unix.out_channel_of_descr fo}

    let connector ~host ~port : ('p, stream) connector = create_connector @@ fun () ->
      match Unix.getaddrinfo host (string_of_int port) [] with
      | [] -> failwith ("Host not found " ^ host)
      | h::_ ->
         let ic,oc = Unix.open_connection h.Unix.ai_addr in
         {in_=ic; out=oc}

    let new_domain_channel () =
      let path = Filename.temp_file "sock" "sock" in
      Unix.unlink path;
      let sock_listen = Unix.(socket PF_UNIX SOCK_STREAM 0) in
      Unix.(bind sock_listen (ADDR_UNIX path));
      Unix.listen sock_listen 0;
      create_connector (fun () ->
        let sock_cli = Unix.(socket PF_UNIX SOCK_STREAM 0) in
        Unix.(connect sock_cli (ADDR_UNIX path));
        make (sock_cli, sock_cli)),
      create_acceptor (fun () ->
        let sock_serv, _ = Unix.(accept sock_listen) in
        make (sock_serv, sock_serv))

    let fork : 'p 'v. (('p, serv, stream) sess, empty, unit lin) monad
               -> (('p, cli, stream) sess, empty, 'v lin) monad
               -> 'v io = fun ms mc ->
      let c_in, s_out = Unix.pipe () in
      let s_in, c_out = Unix.pipe () in
      if Unix.fork () = 0 then begin
          let ch = _mksess @@ make (c_in, c_out) in
          Linocaml.Direct.Syntax.Internal.__run mc ch
        end
      else begin
          let ch = _mksess @@ make (s_in, s_out) in
          ignore (Linocaml.Direct.Syntax.Internal.__run ms ch);
          exit 0
        end
      
    
    module Closers = struct
      let _f {in_;out} = close_in in_
    end
  end


end


