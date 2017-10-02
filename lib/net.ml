(* empty slots *)
type ('a,'b,'p,'q) slot = ('a,'b,'p,'q) Linocaml.Lens.t

open Linocaml.Base
open Linocaml.Lens

module type SESSION = sig
  type +'a io
  type ('p,'q,'a) monad

  (* polarized session types *)
  type req and resp

  type cli = req * resp
  type serv = resp * req

  type ('p, 'q, 'c) sess_
  type ('p, 'q, 'c) sess = ('p,'q, 'c) sess_ lin

  val _mksess : 'c -> ('p,'q,'c) sess

  (* connectors *)
  type ('p,'c) connector
  type ('p,'c) acceptor

  val create_connector : (unit -> 'c io) -> ('p,'c) connector
  val create_acceptor : (unit -> 'c io) -> ('p,'c) acceptor

  module Sender : sig
    type ('c,'v) t = ('c -> 'v -> unit io, [%imp Senders]) Ppx_implicits.t
  end
  module Receiver : sig
    type ('c,'v) t = ('c -> 'v io, [%imp Receivers]) Ppx_implicits.t
  end
  module Closer : sig
    type 'c t = ('c -> unit io, [%imp Closers]) Ppx_implicits.t
  end

  (* connections on shared memory *)
  type shmem
  val new_shmem_channel : unit -> ('p,shmem) connector * ('p,shmem) acceptor
  module Senders : sig
    val _f : shmem -> 'v -> unit io
  end
  module Receivers : sig
    val _f : shmem -> 'v io
  end
  module Closers : sig
    val _f : shmem -> unit io
  end

  (* connection primitives *)
  val accept : ('p,'c) acceptor -> ('pre, 'pre, ('p, serv, 'c) sess) monad
  val connect : ('p,'c) connector -> ('pre, 'pre, ('p, cli, 'c) sess) monad

  (* session primitives *)
  val close :
    ?_closer:'c Closer.t ->
    (([`close], 'r1*'r2, 'c) sess, empty, 'pre, 'post) slot -> ('pre, 'post, unit lin) monad
  val send :
    ?_sender:('c, 'v) Sender.t ->
    (([`msg of 'r1 * 'v * 'p], 'r1*'r2, 'c) sess, ('p, 'r1*'r2, 'c) sess, 'pre, 'post) slot -> 'v -> ('pre, 'post, unit lin) monad
  val receive :
    ?_receiver:('c, 'v) Receiver.t ->
    (([`msg of 'r2 * 'v * 'p], 'r1*'r2, 'c) sess, ('p, 'r1*'r2, 'c) sess, 'pre, 'post) slot -> ('pre, 'post, 'v data lin) monad

  val select
      :  ?_sender:('c,'br) Sender.t
         -> (([`branch of 'r1 * 'br],'r1*'r2,'c) sess, ('p,'r1*'r2,'c) sess, 'pre, 'post) slot
         -> (('p,'r2*'r1,'c) sess -> 'br)
         -> ('pre, 'post, unit lin) monad

  val branch
      :  ?_receiver:('c,'br) Receiver.t
         -> (([`branch of 'r2 * 'br], 'r1*'r2, 'c) sess, empty, 'pre, 'post) slot
         -> ('pre, 'post, 'br lin) monad

  (* val deleg_send *)
  (*     : (([`deleg of 'r1 * ('pp, 'qq, 'cc) sess * 'p], 'r1*'r2, 'c) sess, ('p, 'r1*'r2, 'c) sess, 'pre, 'post) slot *)
  (*       -> (('pp, 'qq, 'cc) sess, empty, 'post, 'uu) slot *)
  (*       -> ('pre, 'uu, unit) monad *)

  (* val deleg_recv *)
  (*     : (([`deleg of 'r2 * ('pp, 'qq, 'cc) sess * 'p], 'r1*'r2, 'c) sess, empty, 'pre, 'post) slot *)
  (*       -> ('pre, 'post, ('pp,'qq,'cc) sess * ('p,'r1*'r2, 'c) sess) monad *)
end

module Make(LinIO:Linocaml.Base.LIN_IO)
           (Chan:Channel.S with type 'a io = 'a LinIO.IO.io)
           (RawChan:Unsafe.RAW_DCHAN with type 'a io = 'a LinIO.IO.io)
  : SESSION
  with type 'a io = 'a LinIO.IO.io
   and type ('p,'q,'a) monad = ('p,'q,'a) LinIO.monad
  = struct
  
  type +'a io = 'a LinIO.IO.io
  type ('p,'q,'a) monad = ('p,'q,'a) LinIO.monad

  type shmem = RawChan.t

  module Sender = struct
    type ('c,'v) t = ('c -> 'v -> unit io, [%imp Senders]) Ppx_implicits.t
    let unpack : ('c,'v) t -> 'c -> 'v -> unit io = fun d -> Ppx_implicits.imp ~d
  end
  module Receiver = struct
    type ('c,'v) t = ('c -> 'v io, [%imp Receivers]) Ppx_implicits.t
    let unpack : ('c,'v) t -> 'c -> 'v io = fun d -> Ppx_implicits.imp ~d
  end
  module Closer = struct
    type 'c t = ('c -> unit io, [%imp Closers]) Ppx_implicits.t
    let unpack : 'c t -> 'c -> unit io = fun d -> Ppx_implicits.imp ~d
  end
  module Senders = struct
    let _f = RawChan.send
  end
  module Receivers = struct
    let _f = RawChan.receive
  end
  module Closers = struct
    let _f _ = LinIO.IO.return ()
  end

  type ('p,'c) connector = unit -> 'c io
  type ('p,'c) acceptor = unit -> 'c io

  let create_connector f = f
  let create_acceptor f = f

  type (_, _, 'c) sess_ = 'c
   and ('p, 'q, 'c) sess = ('p,'q,'c) sess_ lin

  let _mksess s = Lin_Internal__ s

  let new_shmem_channel () =
    let ch = Chan.create () in
    (fun () -> let raw = RawChan.create () in
               let open LinIO.IO in
               Chan.send ch (RawChan.reverse raw) >>= fun _ ->
               return raw),
    (fun () -> let open LinIO.IO in
               Chan.receive ch >>= fun raw ->
               return raw)
    

  (* polarized session types *)
  type req = Req and resp = Resp

  type cli = req * resp
  type serv = resp * req

  let cli = Req,Resp
  let serv = Resp,Req
  let swap (x,y) = y,x

  
  let untrans = function
    | Some f -> f
    | None -> failwith "no instance -- ppx_implicits not configured?"

  let unsess (Lin_Internal__ s) = s
  let mksess s = Lin_Internal__ s

  let (>>=) = LinIO.IO.(>>=)

  let accept : type c. ('p,c) acceptor -> ('pre, 'pre, ('p, serv, c) sess) monad = fun acc ->
       LinIO.Internal.__monad begin
           fun pre ->
           acc () >>= fun raw ->
           LinIO.IO.return (pre, mksess raw)
         end

  let connect : type c. ('p,c) connector -> ('pre, 'pre, ('p, cli, c) sess) monad = fun con ->
       LinIO.Internal.__monad begin
           fun pre ->
           con ()  >>= fun raw ->
           LinIO.IO.return (pre, mksess raw)
         end

  let close : ?_closer:'c Closer.t -> (([`close], 'r1*'r2, 'c) sess, empty, 'pre, 'post) slot -> ('pre, 'post, unit lin) monad =
    fun ?_closer {get;put} ->
    LinIO.Internal.__monad begin
        fun pre ->
        let open LinIO.IO in
        let closer = Closer.unpack @@ untrans _closer in
        closer (unsess (get pre)) >>= fun _ ->
        LinIO.IO.return (put pre Empty, Lin_Internal__ ())
      end
    
  let send : ?_sender:('c,'v) Sender.t -> (([`msg of 'r1 * 'v * 'p], 'r1*'r2, 'c) sess, ('p, 'r1*'r2, 'c) sess, 'pre, 'post) slot -> 'v -> ('pre, 'post, unit lin) monad =
    fun ?_sender {get;put} v ->
    LinIO.Internal.__monad begin
        fun pre ->
        let conn = unsess (get pre) in
        let sender = Sender.unpack @@ untrans _sender in
        let open LinIO.IO in
        sender conn v >>= fun () ->
        LinIO.IO.return (put pre (mksess conn), Lin_Internal__ ())
      end
      
  let receive : ?_receiver:('c,'v) Receiver.t -> (([`msg of 'r2 * 'v * 'p], 'r1*'r2, 'c) sess, ('p, 'r1*'r2, 'c) sess, 'pre, 'post) slot -> ('pre, 'post, 'v data lin) monad =
    fun ?_receiver {get;put} ->
    LinIO.Internal.__monad begin
        fun pre ->
        let conn = unsess (get pre) in
        let receiver = Receiver.unpack @@ untrans _receiver in
        receiver conn >>= fun x ->
        LinIO.IO.return (put pre (mksess conn), Lin_Internal__ (Data_Internal__ x))
      end

  let select
      :  ?_sender:('c,'br) Sender.t
         -> (([`branch of 'r1 * 'br],'r1*'r2, 'c) sess, ('p,'r1*'r2, 'c) sess, 'pre, 'post) slot
         -> (('p,'r2*'r1, 'c) sess -> 'br)
         -> ('pre, 'post, unit lin) monad =
    fun ?_sender {get;put} f ->
    LinIO.Internal.__monad begin
        fun pre ->
        let conn = unsess (get pre) in
        let sender = Sender.unpack @@ untrans _sender in
        sender conn (f (Obj.magic ())) >>= fun _ ->
        LinIO.IO.return (put pre (mksess conn), Lin_Internal__ ())
      end
        
  let branch
      :   ?_receiver:('c,'br) Receiver.t
          -> (([`branch of 'r2 * 'br], 'r1*'r2, 'c) sess, empty, 'pre, 'post) slot
          -> ('pre, 'post, 'br lin) monad =
    fun ?_receiver {get;put} ->
    LinIO.Internal.__monad begin
        fun pre ->
        let conn = unsess (get pre) in
        let receiver = Receiver.unpack @@ untrans _receiver in
        receiver conn >>= fun br -> (* FIXME *)
        LinIO.IO.return (put pre Empty, Lin_Internal__ br)
      end
        
  (* let deleg_send *)
  (*     : (([`deleg of 'r1 * ('pp, 'rr, 'cc) sess * 'p], 'r1*'r2, 'c) sess, ('p, 'r1*'r2, 'c) sess, 'pre, 'mid) slot *)
  (*       -> (('pp, 'rr, 'cc) sess, empty, 'mid, 'post) slot *)
  (*       -> ('pre, 'post, unit) monad = *)
  (*   fun s1_ s2_ -> *)
  (*   LinIO.Internal.__monad begin *)
  (*       fun pre -> *)
  (*       let s1,q = unsess (s1_.get pre) in *)
  (*       let s1' = Dchan.create () in *)
  (*       let mid = s1_.put pre (mksess (s1',q)) in *)
  (*       let s2 = s2_.get mid in *)
  (*       let post = s2_.put mid Empty in *)
  (*       Dchan.send s1 (Chan(s2,s1')) >>= fun x -> *)
  (*       LinIO.IO.return (post, ()) *)
  (*     end *)

  (* let deleg_recv *)
  (*     : (([`deleg of 'r2 * ('pp, 'rr, 'cc) sess * 'p], 'r1*'r2, 'c) sess, empty, 'pre, 'post) slot *)
  (*       -> ('pre, 'post, ('pp,'rr, 'c) sess * ('p,'r1*'r2,'c) sess) monad = *)
  (*   fun {get;put} -> *)
  (*   LinIO.Internal.__monad begin *)
  (*       fun pre -> *)
  (*       let s1,q = unsess (get pre) in *)
  (*       Dchan.receive s1 >>= fun (Chan(s2,s1')) -> *)
  (*       LinIO.IO.return (put pre Empty, (s2,mksess (s1',q))) *)
  (*     end *)
end
