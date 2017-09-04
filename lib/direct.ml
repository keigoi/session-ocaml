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

module Net = struct
  include Net.Make(Linocaml.Direct)(Chan)(Unsafe.Make_raw_dchan(Dchannel.Make(Chan)))

  type tcp = {in_:in_channel; out:out_channel}

  module Tcp : sig
    val connector : host:string -> port:int -> ('p, tcp) connector

    module Closers : sig
      val _f : tcp -> unit
    end
  end = struct
    let connector ~host ~port : ('p, tcp) connector = create_connector @@ fun () ->
      match Unix.getaddrinfo host (string_of_int port) [] with
      | [] -> failwith ("Host not found " ^ host)
      | h::_ ->
         let ic,oc = Unix.open_connection h.Unix.ai_addr in
         {in_=ic; out=oc}

    module Closers = struct
      let _f {in_} = Unix.shutdown_connection in_
    end
  end


end


