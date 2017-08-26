(* empty slots *)
type ('a,'b,'p,'q) slot = ('a,'b,'p,'q) Linocaml.Lens.t

open Linocaml.Base
open Linocaml.Lens
   
module type SESSION = sig
  type +'a io
  type ('p,'q,'a) monad

  (* channels *)
  type 'p channel

  val new_channel : unit -> 'p channel

  (* polarized session types *)
  type req and resp

  type cli = req * resp
  type serv = resp * req

  type ('p, 'q) sess_
  type ('p, 'q) sess = ('p,'q) sess_ lin

  val accept : 'p channel -> ('pre, 'pre, ('p, serv) sess) monad
  val connect : 'p channel -> ('pre, 'pre, ('p, cli) sess) monad

  val close : (([`close], 'r1*'r2) sess, empty, 'pre, 'post) slot -> ('pre, 'post, unit) monad
  val send : (([`msg of 'r1 * 'v * 'p], 'r1*'r2) sess, ('p, 'r1*'r2) sess, 'pre, 'post) slot -> 'v -> ('pre, 'post, unit) monad
  val receive : (([`msg of 'r2 * 'v * 'p], 'r1*'r2) sess, ('p, 'r1*'r2) sess, 'pre, 'post) slot -> ('pre, 'post, 'v data lin) monad

  val select
      :  (([`branch of 'r1 * 'br],'r1*'r2) sess, ('p,'r1*'r2) sess, 'pre, 'post) slot
         -> (('p,'r2*'r1) sess -> 'br)
         -> ('pre, 'post, unit) monad

  val branch
      :  (([`branch of 'r2 * 'br], 'r1*'r2) sess, empty, 'pre, 'post) slot
         -> ('pre, 'post, 'br lin) monad

  val deleg_send
      : (([`deleg of 'r1 * ('pp, 'rr) sess * 'p], 'r1*'r2) sess, ('p, 'r1*'r2) sess, 'pre, 'post) slot
        -> (('pp, 'rr) sess, empty, 'post, 'uu) slot
        -> ('pre, 'uu, unit) monad

  val deleg_recv
      : (([`deleg of 'r2 * ('pp, 'rr) sess * 'p], 'r1*'r2) sess, empty, 'pre, 'post) slot
        -> ('pre, 'post, ('pp,'rr) sess * ('p,'r1*'r2) sess) monad
end

module Make(LinIO:Linocaml.Base.LIN_IO)(Chan:Channel.S with type 'a io = 'a LinIO.IO.io)
  : SESSION
  with type 'a io = 'a LinIO.IO.io
   and type ('p,'q,'a) monad = ('p,'q,'a) LinIO.monad
  = struct
  module Dchan = Dchannel.Make(Chan)
  
  type +'a io = 'a LinIO.IO.io
  type ('p,'q,'a) monad = ('p,'q,'a) LinIO.monad

  (* channels *)
  type 'p cont =
    Msg : ('v * 'p cont Dchan.t) -> [`msg of 'r * 'v * 'p] cont
  | Branch : 'br  -> [`branch of 'r * 'br] cont
  | Chan : (('pp, 'rr) sess * 'p cont Dchan.t) -> [`deleg of 'r * ('pp, 'rr) sess * 'p] cont
   and ('p, 'q) sess_ = 'p cont Dchan.t * 'q
   and ('p, 'q) sess = ('p,'q) sess_ lin
                     
  type 'p channel = 'p cont Dchan.t Chan.t

  let new_channel () = Chan.create () 

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

  let accept : 'p channel -> ('pre, 'pre, ('p, serv) sess) monad = fun ch ->
    LinIO.Internal.__monad begin
      fun pre ->
      Chan.receive ch >>= fun raw ->
      LinIO.IO.return (pre, mksess (raw,serv))
      end

  let connect : 'p channel -> ('pre, 'pre, ('p, cli) sess) monad = fun ch ->
    LinIO.Internal.__monad begin
      fun pre ->
      let raw = Dchan.create () in
      Chan.send ch (Dchan.reverse raw)  >>= fun _ ->
      LinIO.IO.return (pre, mksess (raw,cli))
      end

  let close : (([`close], 'r1*'r2) sess, empty, 'pre, 'post) slot -> ('pre, 'post, unit) monad =
    fun {get;put} ->
    LinIO.Internal.__monad begin
        fun pre ->
        (* Dchan.close (unsess (get pre)) >>= fun _ -> *)
        LinIO.IO.return (put pre Empty, ())
      end
    
  let send : (([`msg of 'r1 * 'v * 'p], 'r1*'r2) sess, ('p, 'r1*'r2) sess, 'pre, 'post) slot -> 'v -> ('pre, 'post, unit) monad =
    fun {get;put} v ->
    LinIO.Internal.__monad begin
        fun pre ->
        let s,q = unsess (get pre) in
        let s' = Dchan.create () in
        Dchan.send s (Msg(v, Dchan.reverse s')) >>= fun _ ->
        LinIO.IO.return (put pre (mksess (s',q)), ())
      end
      
  let receive : (([`msg of 'r2 * 'v * 'p], 'r1*'r2) sess, ('p, 'r1*'r2) sess, 'pre, 'post) slot -> ('pre, 'post, 'v data lin) monad =
    fun {get;put} ->
    LinIO.Internal.__monad begin
        fun pre ->
        let s,q = unsess (get pre) in
        Dchan.receive s >>= fun (Msg(x,s')) ->
        LinIO.IO.return (put pre (mksess (s',q)), Lin_Internal__ (Data_Internal__ x))
      end

  let select
      :  (([`branch of 'r1 * 'br],'r1*'r2) sess, ('p,'r1*'r2) sess, 'pre, 'post) slot
         -> (('p,'r2*'r1) sess -> 'br)
         -> ('pre, 'post, unit) monad =
    fun {get;put} f ->
    LinIO.Internal.__monad begin
        fun pre ->
        let s,q = unsess (get pre) in
        let s' = Dchan.create () in
        Dchan.send s (Branch (f (mksess (Dchan.reverse s',swap q)))) >>= fun _ ->
        LinIO.IO.return (put pre (mksess (s',q)), ())
      end
        
  let branch
      :  (([`branch of 'r2 * 'br], 'r1*'r2) sess, empty, 'pre, 'post) slot
         -> ('pre, 'post, 'br lin) monad =
    fun {get;put} ->
    LinIO.Internal.__monad begin
        fun pre ->
        let s,q = unsess (get pre) in
        Dchan.receive s >>= fun (Branch(br)) ->
        LinIO.IO.return (put pre Empty, Lin_Internal__ br)
      end
        
  let deleg_send
      : (([`deleg of 'r1 * ('pp, 'rr) sess * 'p], 'r1*'r2) sess, ('p, 'r1*'r2) sess, 'pre, 'mid) slot
        -> (('pp, 'rr) sess, empty, 'mid, 'post) slot
        -> ('pre, 'post, unit) monad =
    fun s1_ s2_ ->
    LinIO.Internal.__monad begin
        fun pre ->
        let s1,q = unsess (s1_.get pre) in
        let s1' = Dchan.create () in
        let mid = s1_.put pre (mksess (s1',q)) in
        let s2 = s2_.get mid in
        let post = s2_.put mid Empty in
        Dchan.send s1 (Chan(s2,s1')) >>= fun x ->
        LinIO.IO.return (post, ())
      end

  let deleg_recv
      : (([`deleg of 'r2 * ('pp, 'rr) sess * 'p], 'r1*'r2) sess, empty, 'pre, 'post) slot
        -> ('pre, 'post, ('pp,'rr) sess * ('p,'r1*'r2) sess) monad =
    fun {get;put} ->
    LinIO.Internal.__monad begin
        fun pre ->
        let s1,q = unsess (get pre) in
        Dchan.receive s1 >>= fun (Chan(s2,s1')) ->
        LinIO.IO.return (put pre Empty, (s2,mksess (s1',q)))
      end
end

     
