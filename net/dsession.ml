type 'a lin = L of 'a
type 'a data = W of 'a
type ('pre, 'post, 'a) lmonad = 'pre -> 'post * 'a
type 'f lbind = 'f

let unlin (L v) = v

type ('a,'b,'ss,'tt) slot = ('ss -> 'a) * ('ss -> 'b -> 'tt)

let _0 = (fun (a,_) -> a), (fun (_,ss) b -> (b,ss))
let _1 = (fun (_,(a,_)) -> a), (fun (s0,(_,ss)) b -> (s0,(b,ss)))
let _2 = (fun (_,(_,(a,_))) -> a), (fun (s0,(s1,(_,ss))) b -> (s0,(s1,(b,ss))))
let _3 = (fun (_,(_,(_,(a,_)))) -> a), (fun (s0,(s1,(s2,(_,ss)))) b -> (s0,(s1,(s2,(b,ss)))))

type empty = Empty
type empty_three = empty * (empty * (empty * empty))
type empty_four = empty * empty_three

let return a pre = pre, a
let (>>=) f g pre = let mid, la = f pre in g la mid
let (>>) f g pre = let mid, _ = f pre in g mid

let run f = snd @@ f () (Empty, (Empty, (Empty, (Empty, Empty))))

type req and resp
type cli = req * resp and serv = resp * req

module type S = sig
  type ('p, 'q, 'c) sess
  type ('p, 'q, 'c) dsess = ('p, 'q, 'c) sess lin
  type shmem

  val _mksess : 'c -> ('p, 'q, 'c) dsess

  (* connectors *)
  type ('p,'c) connector
  type ('p,'c) acceptor

  val create_connector : (unit -> 'c) -> ('p,'c) connector
  val create_acceptor : (unit -> 'c) -> ('p,'c) acceptor

  module Sender : sig
    type ('c,'v) t = ('c -> 'v -> unit, [%imp Senders]) Ppx_implicits.t
  end
  module Receiver : sig
    type ('c,'v) t = ('c -> 'v, [%imp Receivers]) Ppx_implicits.t
  end
  module Closer : sig
    type 'c t = ('c -> unit, [%imp Closers]) Ppx_implicits.t
  end

  (* connections on shared memory *)
  val new_shmem_channel : unit -> ('p,shmem) connector * ('p,shmem) acceptor
  module Senders : sig
    val _f : shmem -> 'v -> unit
  end
  module Receivers : sig
    val _f : shmem -> 'v
  end
  module Closers : sig
    val _f : shmem -> unit
  end

  val accept : ('p,'c) acceptor -> ('pre, 'pre, ('p, serv, 'c) dsess) lmonad
    
  val connect : ('p,'c) connector -> ('pre, 'pre, ('p, cli, 'c) dsess) lmonad

  val close :
    ?_closer:'c Closer.t ->
    (([`close], 'r1*'r2, 'c) dsess, empty, 'pre, 'post) slot
    -> ('pre, 'post, unit) lmonad

  val send :
    ?_sender:('c, 'v) Sender.t
    -> 'v -> (([`msg of 'r1 * 'v * 'p], 'r1*'r2, 'c) dsess, empty, 'pre, 'post) slot
    -> ('pre, 'post, ('p, 'r1*'r2, 'c) dsess) lmonad

  val receive :
    ?_receiver:('c, 'v) Receiver.t
    -> (([`msg of 'r2 * 'v * 'p], 'r1*'r2, 'c) dsess, empty, 'pre, 'post) slot
    -> ('pre, 'post, 'v data * ('p, 'r1*'r2, 'c) dsess) lmonad

  val select :
    ?_sender:('c, [>] as 'br) Sender.t
    -> (('p,'r2*'r1, 'c) dsess -> 'br)
    -> (([`branch of 'r1 * 'br],'r1*'r2, 'c) dsess, empty, 'pre, 'post) slot
    -> ('pre, 'post, ('p,'r1*'r2, 'c) dsess) lmonad

  val branch :
    ?_receiver:('c, [>] as 'br) Receiver.t
    -> (([`branch of 'r2 * 'br], 'r1*'r2, 'c) dsess, empty, 'pre, 'post) slot
    -> ('pre, 'post, 'br) lmonad

  val deleg_send :
    ?_sender:('c , ('pp,'qq,'cc) sess) Sender.t
    -> (('pp, 'qq, 'cc) dsess, empty, 'mid, 'post) slot
    -> (([`deleg of 'r1 * ('pp, 'qq, 'cc) dsess * 'p], 'r1*'r2, 'c) dsess, empty, 'pre, 'mid) slot
    -> ('pre, 'post, ('p, 'r1*'r2, 'c) dsess) lmonad

  val deleg_recv :
    ?_receiver:('c, ('pp, 'qq, 'cc) sess) Receiver.t
    -> (([`deleg of 'r2 * ('pp, 'qq, 'cc) dsess * 'p], 'r1*'r2, 'c) dsess, empty, 'pre, 'post) slot
    -> ('pre, 'post, ('pp,'qq,'cc) dsess * ('p,'r1*'r2,'c) dsess) lmonad

end

module Schannel = struct
  type 'a t      = 'a Event.channel
  let create     = Event.new_channel
  let send ch x  = Event.sync (Event.send ch x)
  let receive ch = Event.sync (Event.receive ch)
end

module Make(U : sig
  type t
  val create     : unit -> t
  val send       : t -> 'a -> unit
  val receive    : t -> 'a
  val reverse    : t -> t
end) : S = struct
  type ('p, 'q, 'c) sess = 'c
  type 'p channel = U.t Schannel.t
  type ('p, 'q, 'c) dsess = ('p, 'q, 'c) sess lin
  type shmem = U.t

  let _mksess c = L c

  let new_shmem_channel () =
    let ch = Schannel.create () in
    (fun () -> let raw = U.create () in
               Schannel.send ch (U.reverse raw);
               raw),
    (fun () -> Schannel.receive ch)

  type ('p,'c) connector = unit -> 'c
  type ('p,'c) acceptor = unit -> 'c

  let create_connector f = f
  let create_acceptor f = f

  module Sender = struct
    type ('c,'v) t = ('c -> 'v -> unit, [%imp Senders]) Ppx_implicits.t
    let unpack : ('c,'v) t -> 'c -> 'v -> unit = fun d -> Ppx_implicits.imp ~d
  end
  module Receiver = struct
    type ('c,'v) t = ('c -> 'v, [%imp Receivers]) Ppx_implicits.t
    let unpack : ('c,'v) t -> 'c -> 'v = fun d -> Ppx_implicits.imp ~d
  end
  module Closer = struct
    type 'c t = ('c -> unit, [%imp Closers]) Ppx_implicits.t
    let unpack : 'c t -> 'c -> unit = fun d -> Ppx_implicits.imp ~d
  end
  module Senders = struct
    let _f = U.send
  end
  module Receivers = struct
    let _f = U.receive
  end
  module Closers = struct
    let _f _ = ()
  end

  let new_channel = Schannel.create

  let accept : 'p 'c 'pre. ('p,'c) acceptor -> ('pre, 'pre, ('p, serv, 'c) dsess) lmonad =
    fun acc pre ->
    let s = acc () in
    pre, L s

  let connect : 'p 'c 'pre. ('p,'c) connector -> ('pre, 'pre, ('p, cli, 'c) dsess) lmonad =
    fun conn pre ->
    let s = conn () in
    pre, L s

  let instance = function
      Some i -> i
    | None -> failwith "impossible: no instance"
    
  let close : 'pre 'r1 'r2 'c 'post.
              ?_closer:'c Closer.t
              -> (([`close], 'r1*'r2, 'c) dsess, empty, 'pre, 'post) slot
              -> ('pre, 'post, unit) lmonad =
    fun ?_closer (get,set) pre ->
    Closer.unpack (instance _closer) (unlin @@ get pre);
    set pre Empty, ()

  let send : 'v 'r1 'p 'r2 'c 'pre 'post.
             ?_sender:('c,'v) Sender.t
             -> 'v -> (([`msg of 'r1 * 'v * 'p], 'r1*'r2, 'c) dsess, empty, 'pre, 'post) slot
             -> ('pre, 'post, ('p, 'r1*'r2, 'c) dsess) lmonad =
    fun ?_sender v (get,set) pre ->
    let s = unlin @@ get pre in
    Sender.unpack (instance _sender) s v;
    set pre Empty, L s

  let receive : 'r2 'v 'p 'r1 'c 'pre 'post.
                ?_receiver:('c,'v) Receiver.t
                -> (([`msg of 'r2 * 'v * 'p], 'r1*'r2, 'c) dsess, empty, 'pre, 'post) slot
                -> ('pre, 'post, 'v data * ('p, 'r1*'r2, 'c) dsess) lmonad =
    fun ?_receiver (get,set) pre ->
    let s = unlin @@ get pre in
    set pre Empty, (W (Receiver.unpack (instance _receiver) s), L s)

  let select : 'p 'r2 'r1 'c 'pre 'post.
               ?_sender:('c, [>] as 'br) Sender.t
               -> (('p,'r2*'r1,'c) dsess -> 'br)
               -> (([`branch of 'r1 * 'br],'r1*'r2,'c) dsess, empty, 'pre, 'post) slot
               -> ('pre, 'post, ('p,'r1*'r2,'c) dsess) lmonad =
    fun ?_sender f (get,set) pre ->
    let s = unlin @@ get pre in
    Sender.unpack (instance _sender) s (f (L (Obj.magic ())));
    set pre Empty, L s

  let branch : 'r2 'r1 'c 'pre 'post.
               ?_receiver:('c,[>] as 'br) Receiver.t
               -> (([`branch of 'r2 * 'br], 'r1*'r2, 'c) dsess, empty, 'pre, 'post) slot
               -> ('pre, 'post, 'br) lmonad =
    fun ?_receiver (get,set) pre ->
    let s = unlin @@ get pre in
    set pre Empty, (Receiver.unpack (instance _receiver) s)

  let deleg_send : 'pp 'qq 'cc 'mid 'post 'r1 'r2 'c 'pre.
                   ?_sender:('c, ('pp, 'qq, 'cc) sess) Sender.t
                   -> (('pp, 'qq, 'cc) dsess, empty, 'mid, 'post) slot
                   -> (([`deleg of 'r1 * ('pp, 'qq, 'cc) dsess * 'p], 'r1*'r2, 'c) dsess, empty, 'pre, 'mid) slot
                   -> ('pre, 'post, ('p, 'r1*'r2, 'c) dsess) lmonad =
    fun ?_sender (get1,set1) (get2,set2) pre ->
    let s = unlin @@ get2 pre in
    let mid = set2 pre Empty in
    let t = unlin @@ get1 mid in
    Sender.unpack (instance _sender) s t;
    set1 mid Empty, L s

  let deleg_recv : 'r2 'p 'r1 'c 'pre 'post 'pp 'qq 'cc.
                   ?_receiver:('c, ('pp, 'qq, 'cc) sess) Receiver.t
                   -> (([`deleg of 'r2 * ('pp, 'qq, 'cc) dsess * 'p], 'r1*'r2, 'c) dsess, empty, 'pre, 'post) slot
                   -> ('pre, 'post, ('pp,'qq,'cc) dsess * ('p,'r1*'r2,'c) dsess) lmonad =
    fun ?_receiver (get,set) pre ->
    let s = unlin @@ get pre in
    let t = Receiver.unpack (instance _receiver) s in
    set pre Empty, (L t, L s)    

end
                               
include Make(struct
  type t      = unit Event.channel
  let create     = Event.new_channel
  let send ch x  = Event.sync (Event.send ch (Obj.magic x))
  let receive ch = Obj.magic (Event.sync (Event.receive ch))
  let reverse ch = ch
end)

module Syntax = struct
  let bind = (>>=)

  module Internal = struct
    let __return_raw v pre = pre, v
    let __bind_raw = fun m f pre -> match m pre with (mid,x) -> f x mid

    let __putval_raw = fun (_,set) v pre ->
      set pre (L v), ()

    let __takeval_raw (get,set) pre =
      set pre Empty, get pre

    let __mkbindfun f = f
    let __run m pre = (>>=) (m pre) (fun (_,L a) -> a)
    let __dispose_env m pre =
      (>>=) (m pre) (fun (_,a) -> return (Empty, a))
  end
end
      

module Tcp = struct
  type stream = {in_:in_channel; out:out_channel}

  let make (fi, fo) = {in_=Unix.in_channel_of_descr fi; out=Unix.out_channel_of_descr fo}

  let connector ~host ~port : ('p, stream) connector =
    create_connector @@
      fun () ->
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

  let fork : 'p 'v. (('p, serv, stream) dsess * (empty * (empty * (empty * empty))), empty_four, unit lin) lmonad
             -> (('p, cli, stream) dsess * (empty * (empty * (empty * empty))), empty, 'v lin) lmonad
             -> 'v = fun ms mc ->
    let c_in, s_out = Unix.pipe () in
    let s_in, c_out = Unix.pipe () in
    if Unix.fork () = 0 then begin
        let ch = _mksess @@ make (c_in, c_out) in
        unlin @@ snd @@ mc (ch,(Empty,(Empty,(Empty,Empty))))
      end
    else begin
        let ch = _mksess @@ make (s_in, s_out) in
        ignore (ms (ch,(Empty,(Empty,(Empty,Empty)))));
        exit 0
      end
    
  module Closers = struct
    let _f {in_;out} = close_in in_
  end
end
