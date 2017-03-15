(* slots and lenses *)
type empty = Empty
type all_empty = [`slot of empty * 'a] as 'a

type ('a,'b,'ss,'tt) slot = ('ss -> 'a) * ('ss -> 'b -> 'tt)

let _0 = (fun (`slot(a,_)) -> a), (fun (`slot(_,ss)) b -> `slot(b,ss))
let _1 = (fun (`slot(_,`slot(a,_))) -> a), (fun (`slot(s0,`slot(_,ss))) b -> `slot(s0,`slot(b,ss)))
let _2 = (fun (`slot(_,`slot(_,`slot(a,_)))) -> a), (fun (`slot(s0,`slot(s1,`slot(_,ss)))) b -> `slot(s0,`slot(s1,`slot(b,ss))))
let _3 = (fun (`slot(_,`slot(_,`slot(_,`slot(a,_))))) -> a), (fun (`slot(s0,`slot(s1,`slot(s2,`slot(_,ss))))) b -> `slot(s0,`slot(s1,`slot(s2,`slot(b,ss)))))

let rec all_empty = `slot(Empty, all_empty)
let run f x = Lwt.bind (f x all_empty) (fun (_, x) -> Lwt.return x)
let run_ m = Lwt.bind (m all_empty) (fun (_, x) -> Lwt.return x) 

(* monads *)
type ('ss,'tt,'v) monad = 'ss -> ('tt * 'v) Lwt.t

let return x p = Lwt.return (p, x)
let (>>=) m f p = Lwt.bind (m p) (fun (q, v) -> f v q)
let (>>) m n p = Lwt.bind (m p) (fun (q, _) -> n q)
let lift m s = Lwt.bind m (fun x -> Lwt.return (s, x))

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
  let new_channel = new_channel
  let run0 m ~at:(get,set) ss =
    Lwt.bind
      (m (get ss))
      (fun (s, v) -> Lwt.return (set ss s, v))

  let close (get,set) ss = Lwt.return (set ss Empty, ())

  let send (get,_) v ss =
    let {up} = get ss in
    Lwt.bind
      (UnsafeMVar.put up v)
      (fun () -> Lwt.return (Obj.magic ss, ()))

  let recv (get,_) ss =
    let {down} = get ss in
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
      (UnsafeMVar.put up (f (Obj.magic 42)))
      (fun () -> Lwt.return (Obj.magic ss, ()))

  let branch2 (s1,f1) (s2,f2) =
    _branch_start s1 (function
                      | `left(p1),q -> _branch s1 (p1,q) (f1 ())
                      | `right(p2),q -> _branch s2 (p2,q) (f2 ())
                     : [`left of 'p1 | `right of 'p2] * 'x -> 'y)

  let select_left s = _select s (fun x -> `left(x))

  let select_right s = _select s (fun x -> `right(x))
end

module Session0 = struct
    
  let accept_ mvar f v =
    Lwt.bind
      (Lwt_mvar.take mvar)
      (fun s -> Lwt.map snd (f v s))
    
  let connect_ mvar f v = 
    let up,down = UnsafeMVar.create_empty (), UnsafeMVar.create_empty () in
    Lwt.bind
      (Lwt_mvar.put mvar {down=up;up=down})
      (fun () -> Lwt.map snd (f v ({up;down})))

  let close () _ = Lwt.return (Empty, ())
              
  let recv () s =
    Lwt.bind
      (UnsafeMVar.take s.down)
      (fun v -> Lwt.return (Obj.magic s, v))
    
  let send v s =
    Lwt.bind
      (UnsafeMVar.put s.up v)
      (fun () -> Lwt.return (Obj.magic s, ()))

  let _select f s =
    Lwt.bind
      (UnsafeMVar.put s.up (f (Obj.magic 42)))
      (fun () -> Lwt.return (Obj.magic s, ()))

  let _branch_start : type br.
                           (br * ('r1*'r2) -> (([`branch of 'r2 * br], 'r1*'r2) sess, 'u,'v) monad)
                           -> (([`branch of 'r2 * br], 'r1*'r2) sess, 'u, 'v) monad
    = fun f s ->
    Lwt.bind
      (UnsafeMVar.take s.down)
      (fun labl -> f (labl, Obj.magic 42) (Obj.magic s))
    
  let _branch : type br.
     'p * ('r1*'r2)
    -> (('p,'r1*'r2) sess, 'u, 'v) monad
    -> (([`branch of 'r2 * br], 'r1*'r2) sess, 'u, 'v) monad
    = fun (_,_) m s ->
      m (Obj.magic s)

  let select_left () =
    _select (fun x -> `left(x))

  let select_right () =
    _select (fun x -> `right(x))

  let branch2 f g =
    _branch_start
      (function
       | `left(p),q -> _branch (p,q) (f ())
       | `right(p),q -> _branch (p,q) (f ()))
end

type 'a parse_result =
  [`Partial of (string option -> 'a parse_result)
  |`Done    of 'a * string
  |`Fail of string]

module type Adapter = sig
  type raw_chan
  type 'p net = raw_chan -> (('p, serv) sess, empty, unit) monad
  val req : ('v -> string) -> 'p net -> [`msg of req * 'v * 'p] net
  val resp : (string -> 'v parse_result) -> 'p net -> [`msg of resp * 'v * 'p] net
  val sel : left:'p1 net -> right:'p2 net ->
          [`branch of req * [`left of 'p1|`right of 'p2]] net
  val bra : left:((string -> 'v1 parse_result) * 'p1 net) -> right:'p2 net ->
          [`branch of resp * [`left of [`msg of resp * 'v1 * 'p1] |`right of 'p2]] net
  val cls : [`close] net
end

module Syntax = struct
  let (>>=) = (>>=)
  module Session0 = struct
    let _select = Session0._select
    let _branch_start = Session0._branch_start
    let _branch = Session0._branch
  end
  module SessionN = struct
    let _select = SessionN._select
    let _branch_start = SessionN._branch_start
    let _branch = SessionN._branch
  end
end

