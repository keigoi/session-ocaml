(* slots and lenses *)
type empty = Empty
type all_empty = empty * 'a as 'a

type ('a,'b,'ss,'tt) slot = ('ss -> 'a) * ('ss -> 'b -> 'tt)

let _0 = (fun (a,_) -> a), (fun (_,ss) b -> (b,ss))
let _1 = (fun (_,(a,_)) -> a), (fun (s0,(_,ss)) b -> (s0,(b,ss)))
let _2 = (fun (_,(_,(a,_))) -> a), (fun (s0,(s1,(_,ss))) b -> (s0,(s1,(b,ss))))
let _3 = (fun (_,(_,(_,(a,_)))) -> a), (fun (s0,(s1,(s2,(_,ss)))) b -> (s0,(s1,(s2,(b,ss)))))

let rec all_empty = Empty, all_empty
let run f x = Lwt.bind (f x all_empty) (fun (_, x) -> Lwt.return x)
let run_ m = Lwt.bind (m all_empty) (fun (_, x) -> Lwt.return x) 

(* monads *)
type ('ss,'tt,'v) monad = 'ss -> ('tt * 'v) Lwt.t

let return x p = Lwt.return (p, x)
let (>>=) m f p = Lwt.bind (m p) (fun (q, v) -> f v q)
let (>>) m n p = Lwt.bind (m p) (fun (q, _) -> n q)

(* polarized session types *)
type req = Req
type resp = Resp

type cli = req * resp
type serv = resp * req

module UnsafeMVar : sig
  type t
  val create_empty : unit -> t
  val put : t -> 'a -> unit Lwt.t
  val take : t -> 'a Lwt.t
end = struct
  type t = unit Lwt_mvar.t
  let create_empty () = Obj.magic (Lwt_mvar.create_empty ())
  let put m v = Lwt_mvar.put m (Obj.magic v)
  let take m = Lwt_mvar.take (Obj.magic m)
end

type mvars = {up:UnsafeMVar.t; down:UnsafeMVar.t}

type ('p, 'r) sess = mvars

(* service channels *)
type 'p channel = mvars Lwt_mvar.t
let new_channel = Lwt_mvar.create_empty


module SessionN = struct
  let new_channel = Channel.create

  let close (get,set) ss = Lwt.return (set ss Empty, ())

  let send (get,_) v ss =
    let {up} = get ss in
    Lwt.bind
      (UnsafeMVar.put up v)
      (fun () -> Lwt.return (Obj.magic ss, ()))

  let recv (get,_) ss =
    let {down}= get ss in
    Lwt.bind
      (UnsafeMVar.take down)
      (fun v -> Lwt.return (Obj.magic ss, v))

  let deleg_send (get0,_) ~release:(get1,set1) ss =
    let {up} = get0 ss in
    let ch = get1 (Obj.magic ss) in
    Lwt.bind
      (UnsafeMVar.put up ch)
      (fun () -> Lwt.return (set1 (Obj.magic ss) Empty, ()))

  let deleg_recv (get0,_) ~bindto:(_,set1) ss =
    let {down} = get0 ss in
    Lwt.bind
      (UnsafeMVar.take down)
      (fun ch -> Lwt.return (set1 (Obj.magic ss) ch, ()))

  let accept mvar ~bindto:(_,set) ss =
    Lwt.bind
      (Lwt_mvar.take mvar)
      (fun ch -> Lwt.return (set ss ch, ()))
    
  let connect mvar ~bindto:(_,set) ss =
    let up,down = UnsafeMVar.create_empty (), UnsafeMVar.create_empty () in
    Lwt.bind
      (Lwt_mvar.put mvar {down=up;up=down})
      (fun () -> Lwt.return (set ss {up;down}, ()))

  let out : 'p 'r. ('p,'r) sess -> 'p = Obj.magic

  let _branch_start : type br.
                           (([`branch of 'r2 * br], 'r1*'r2) sess, 'x, 'ss, 'xx) slot
                           -> (br * ('r1*'r2) -> ('ss, 'uu,'v) monad)
                           -> ('ss, 'uu, 'v) monad
    = fun (get0,_) f ss ->
    let {down} = get0 ss in
    Lwt.bind
      (UnsafeMVar.take down)
      (fun labl -> f (labl, Obj.magic 42) (Obj.magic ss))
    
  let _branch :
        (([`branch of 'r2 * 'br], 'r1*'r2) sess, ('p,'r1*'r2) sess, 'ss, 'tt1) slot
        -> 'p * ('r1*'r2)
        -> ('tt1,'uu,'v) monad
        -> ('ss, 'uu, 'v) monad
    = fun _ _ m ss -> m (Obj.magic ss)


  let _select : type br p.
                     (([`branch of 'r1 * br],'r1*'r2) sess, (p,'r1*'r2) sess, 'ss, 'tt) slot
                     -> (p -> br)
                     -> ('ss, 'tt, 'v) monad = fun (get0,set0) f ss ->
    let {up} = get0 ss in
    Lwt.bind
      (UnsafeMVar.put up (Obj.magic (f (Obj.magic 42))))
      (fun () -> Lwt.return (Obj.magic ss, ()))

  let branch2 (s1,f1) (s2,f2) =
    _branch_start s1 (function
                      | `left(p1),q -> _branch s1 (p1,q) (f1 ())
                      | `right(p2),q -> _branch s2 (p2,q) (f2 ())
                     : [`left of 'p1 | `right of 'p2] * 'x -> 'y)

  let select_left s = _select s (fun x -> `left(x))

  let select_right s = _select s (fun x -> `right(x))
end

(* module Session0 = struct *)
(*   let accept_ ch f v = run (fun v -> SessionN.accept ch ~bindto:_0 >> f v) v *)
(*   let connect_ ch f v = run (fun v -> SessionN.connect ch ~bindto:_0 >> f v) v *)

(*   let close () = SessionN.close _0 *)
(*   let recv () = SessionN.recv _0 *)
(*   let send v = SessionN.send _0 v *)

(*   let branch2 = fun f g -> SessionN.branch2 (_0,f) (_0,g) *)
(*   let select_left () = SessionN.select_left _0 *)
(*   let select_right () = SessionN.select_right _0 *)

(*   let _select f = SessionN._select _0 f *)
(*   let _branch_start f = SessionN._branch_start _0 f *)
(*   let _branch wit m = SessionN._branch _0 wit m *)

(* end *)

(* type 'a parse_result = *)
(*   [`Partial of (string option -> 'a parse_result) *)
(*   |`Done    of 'a * string *)
(*   |`Fail of string] *)

(* module type Adapter = sig *)
(*   type raw_chan *)
(*   type 'p net = raw_chan -> (('p, serv) sess * all_empty, all_empty, unit) monad *)
(*   val req : ('v -> string) -> 'p net -> [`msg of req * 'v * 'p] net *)
(*   val resp : (string -> 'v parse_result) -> 'p net -> [`msg of resp * 'v * 'p] net *)
(*   val sel : left:'p1 net -> right:'p2 net -> *)
(*           [`branch of req * [`left of 'p1|`right of 'p2]] net *)
(*   val bra : left:((string -> 'v1 parse_result) * 'p1 net) -> right:'p2 net -> *)
(*           [`branch of resp * [`left of [`msg of resp * 'v1 * 'p1] |`right of 'p2]] net *)
(*   val cls : [`close] net *)
(* end *)
