(* empty slots *)
type ('a,'b,'p,'q) slot = ('a,'b,'p,'q) Linocaml.Lens.t

open Linocaml.Base
open Linocaml.Lens
   
module type SESSION = sig
  type +'a io
  type ('p,'q,'a) monad

  (* channels *)
  type 'p channel

  val new_shmem_channel : unit -> 'p channel

  (* polarized session types *)
  type req and resp

  type cli = req * resp
  type serv = resp * req

  type ('p, 'q) sess_
  type ('p, 'q) sess = ('p,'q) sess_ lin

  val accept : 'p channel -> ('pre, 'pre, ('p, serv) sess) monad
  val connect : 'p channel -> ('pre, 'pre, ('p, cli) sess) monad

  val close : (([`close], 'r1*'r2) sess, empty, 'pre, 'post) slot -> ('pre, 'post, unit lin) monad
  val send : 'v -> (([`msg of 'r1 * 'v * 'p], 'r1*'r2) sess, empty, 'pre, 'post) slot -> ('pre, 'post, ('p, 'r1*'r2) sess) monad
  val receive : (([`msg of 'r2 * 'v * 'p], 'r1*'r2) sess, empty, 'pre, 'post) slot -> ('pre, 'post, ('v data * ('p, 'r1*'r2) sess) lin) monad

  val select :
         (('p,'r2*'r1) sess -> 'br)
         -> (([`branch of 'r1 * 'br],'r1*'r2) sess, empty, 'pre, 'post) slot
         -> ('pre, 'post, ('p,'r1*'r2) sess) monad

  val branch :
         (([`branch of 'r2 * 'br], 'r1*'r2) sess, empty, 'pre, 'post) slot
         -> ('pre, 'post, 'br lin) monad

  val deleg_send
      : (([`deleg of 'r1 * ('pp, 'rr) sess * 'p], 'r1*'r2) sess, empty, 'pre, 'mid) slot
        -> (('pp, 'rr) sess, empty, 'mid, 'post) slot
        -> ('pre, 'post, ('p, 'r1*'r2) sess) monad

  val deleg_recv
      : (([`deleg of 'r2 * ('pp, 'rr) sess * 'p], 'r1*'r2) sess, empty, 'pre, 'post) slot
        -> ('pre, 'post, (('pp,'rr) sess * ('p,'r1*'r2) sess) lin) monad
    
  val create : unit -> ('pre, 'pre, (('p, cli) sess * ('p, serv) sess) lin) monad
end

module Make(LinIO:Linocaml.Base.LIN_IO)(Chan:Channel.S with type 'a io = 'a LinIO.IO.io)
  : SESSION
  with type 'a io = 'a LinIO.IO.io
   and type ('p,'q,'a) monad = ('p,'q,'a) LinIO.monad
  = struct
  module Uchan = Unsafe.Make_raw_dchan(Dchannel.Make(Chan))
  
  type +'a io = 'a LinIO.IO.io
  type ('p,'q,'a) monad = ('p,'q,'a) LinIO.monad

  (* channels *)
  type ('p, 'q) sess_ = Uchan.t
   and ('p, 'q) sess = ('p,'q) sess_ lin
                     
  type 'p channel = Uchan.t Chan.t

  let new_shmem_channel () = Chan.create () 

  (* polarized session types *)
  type req = Req and resp = Resp

  type cli = req * resp
  type serv = resp * req

  let cli = Req,Resp
  let serv = Resp,Req
  let swap (x,y) = y,x


  let unsess (Lin_Internal__ s) = s
  let mksess s = Lin_Internal__ s

  let (>>=) = LinIO.IO.(>>=)

  let create () =
    LinIO.Internal.__monad begin
      fun pre ->
      let raw = Uchan.create () in
      LinIO.IO.return (pre, Lin_Internal__ (mksess raw, mksess (Uchan.reverse raw)))
      end

  let accept : 'p channel -> ('pre, 'pre, ('p, serv) sess) monad = fun ch ->
    LinIO.Internal.__monad begin
      fun pre ->
      Chan.receive ch >>= fun raw ->
      LinIO.IO.return (pre, mksess raw)
      end

  let connect : 'p channel -> ('pre, 'pre, ('p, cli) sess) monad = fun ch ->
    LinIO.Internal.__monad begin
      fun pre ->
      let raw = Uchan.create () in
      Chan.send ch (Uchan.reverse raw)  >>= fun _ ->
      LinIO.IO.return (pre, mksess raw)
      end

  let close : (([`close], 'r1*'r2) sess, empty, 'pre, 'post) slot -> ('pre, 'post, unit lin) monad =
    fun {get;put} ->
    LinIO.Internal.__monad begin
        fun pre ->
        LinIO.IO.return (put pre Empty, Lin_Internal__ ())
      end
    
  let send : 'v -> (([`msg of 'r1 * 'v * 'p], 'r1*'r2) sess, empty, 'pre, 'post) slot -> ('pre, 'post, ('p, 'r1*'r2) sess) monad =
    fun v {get;put} ->
    LinIO.Internal.__monad begin
        fun pre ->
        let s = unsess (get pre) in
        Uchan.send s v >>= fun _ ->
        LinIO.IO.return (put pre Empty, mksess s)
      end

  let receive : (([`msg of 'r2 * 'v * 'p], 'r1*'r2) sess, empty, 'pre, 'post) slot -> ('pre, 'post, ('v data * ('p, 'r1*'r2) sess) lin) monad =
    fun {get;put} ->
    LinIO.Internal.__monad begin
        fun pre ->
        let s = unsess (get pre) in
        Uchan.receive s >>= fun x ->
        LinIO.IO.return (put pre Empty, Lin_Internal__ (Data_Internal__ x, mksess s))
      end

  let select
      :  (('p,'r2*'r1) sess -> 'br)
         -> (([`branch of 'r1 * 'br],'r1*'r2) sess, empty, 'pre, 'post) slot
         -> ('pre, 'post, ('p,'r1*'r2) sess) monad =
    fun f {get;put} ->
    LinIO.Internal.__monad begin
        fun pre ->
        let s = unsess (get pre) in
        Uchan.send s (f (mksess (Uchan.reverse s))) >>= fun _ ->
        LinIO.IO.return (put pre Empty, mksess s)
      end

  let branch
      :  (([`branch of 'r2 * 'br], 'r1*'r2) sess, empty, 'pre, 'post) slot
         -> ('pre, 'post, 'br lin) monad =
    fun {get;put} ->
    LinIO.Internal.__monad begin
        fun pre ->
        let s = unsess (get pre) in
        Uchan.receive s >>= fun br ->
        LinIO.IO.return (put pre Empty, Lin_Internal__ br)
      end
        
  let deleg_send
      : (([`deleg of 'r1 * ('pp, 'rr) sess * 'p], 'r1*'r2) sess, empty, 'pre, 'mid) slot
        -> (('pp, 'rr) sess, empty, 'mid, 'post) slot
        -> ('pre, 'post, ('p, 'r1*'r2) sess) monad =
    fun s1_ s2_ ->
    LinIO.Internal.__monad begin
        fun pre ->
        let s1 = unsess (s1_.get pre) in
        let mid = s1_.put pre Empty in
        let s2 = s2_.get mid in
        let post = s2_.put mid Empty in
        Uchan.send s1 s2 >>= fun x ->
        LinIO.IO.return (post, mksess s1)
      end

  let deleg_recv
      : (([`deleg of 'r2 * ('pp, 'rr) sess * 'p], 'r1*'r2) sess, empty, 'pre, 'post) slot
        -> ('pre, 'post, (('pp,'rr) sess * ('p,'r1*'r2) sess) lin) monad =
    fun {get;put} ->
    LinIO.Internal.__monad begin
        fun pre ->
        let s1 = unsess (get pre) in
        Uchan.receive s1 >>= fun s2 ->
        LinIO.IO.return (put pre Empty, Lin_Internal__ (s2, mksess s1))
      end
end
