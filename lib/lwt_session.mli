
(* empty slots *)
type empty
type all_empty = empty * 'a as 'a

(* lenses on slots *)
type ('a,'b,'ss,'tt) slot

val _0 : ('a, 'b, ('a * 'ss), ('b * 'ss)) slot
val _1 : ('a, 'b, ('s0 * ('a * 'ss)), ('s0 * ('b * 'ss))) slot
val _2 : ('a, 'b, ('s0 * ('s1 * ('a * 'ss))), ('s0 * ('s1 * ('b * 'ss)))) slot
val _3 : ('a, 'b, ('s0 * ('s1 * ('s2 * ('a * 'ss)))), ('s0 * ('s1 * ('s2 * ('b * 'ss))))) slot

(* parameterized monads *)
type ('x,'y,'a) monad

val return : 'v -> ('x,'x,'v) monad
val (>>=) : ('x,'y,'a) monad -> ('a -> ('y, 'z, 'b) monad) -> ('x,'z,'b) monad
val (>>) : ('x,'y,'a) monad -> ('y,'z,'b) monad -> ('x,'z,'b) monad

val run : ('a -> (all_empty, all_empty, 'b) monad) -> 'a -> 'b Lwt.t
val run_ : ((all_empty, all_empty, 'b) monad) -> 'b Lwt.t

(* channels *)
type 'p channel

val new_channel : unit -> 'p channel

(* polarized session types *)
type req and resp

type cli = req * resp
type serv = resp * req

type ('p, 'q) sess

(* module Session0 : sig *)

(*   val accept_ : 'p channel -> ('v -> (('p,serv) sess * all_empty,  all_empty, 'w) monad) -> 'v  -> 'w Lwt.t *)
(*   val connect_ : 'p channel -> ('v -> (('p,cli) sess * all_empty,  all_empty, 'w) monad) -> 'v  -> 'w Lwt.t *)

(*   val close : unit -> (([`close], 'r1*'r2) sess * 'ss, empty * 'ss, unit) monad *)
(*   val send : 'v -> (([`msg of 'r1 * 'v * 'p], 'r1*'r2) sess * 'ss, ('p, 'r1*'r2) sess * 'ss, unit) monad *)
(*   val recv : unit -> (([`msg of 'r2 * 'v * 'p], 'r1*'r2) sess * 'ss, ('p, 'r1*'r2) sess * 'ss, 'v) monad *)

(*   val branch2 *)
(*     :  (unit -> (('p1, 'r1*'r2) sess * 'ss, 'uu, 'a) monad) *)
(*     -> (unit -> (('p2, 'r1*'r2) sess * 'ss, 'uu, 'a) monad) *)
(*     -> (([`branch of 'r2 * [`left of 'p1 | `right of 'p2]], 'r1*'r2) sess * 'ss, 'uu, 'a) monad *)

(*   val select_left *)
(*     : unit -> (([`branch of 'r1 * [>`left of 's1]], 'r1*'r2) sess * 'ss, ('s1, 'r1*'r2) sess * 'ss, unit) monad *)

(*   val select_right *)
(*     : unit -> (([`branch of 'r1 * [>`right of 's2]], 'r1*'r2) sess * 'ss, ('s2, 'r1*'r2) sess * 'ss, unit) monad *)

(*   val _select *)
(*     :  ('p -> 'br) *)
(*     -> (([`branch of 'r1 * 'br],'r1*'r2) sess * 'ss, ('p,'r1*'r2) sess * 'ss, unit) monad *)

(*   val _branch_start *)
(*     :  ('br * ('r1*'r2) -> (([`branch of 'r2 * 'br], 'r1*'r2) sess * 'ss, 'uu,'v) monad) *)
(*     -> (([`branch of 'r2 * 'br], 'r1*'r2) sess * 'ss, 'uu, 'v) monad *)

(*   val _branch *)
(*     :  'p * ('r1*'r2) *)
(*     -> (('p,'r1*'r2) sess * 'ss, 'uu, 'v) monad *)
(*     -> (([`branch of 'r2 * 'br], 'r1*'r2) sess * 'ss, 'uu, 'v) monad *)
(* end *)


module SessionN : sig

  val accept : 'p channel -> bindto:(empty, ('p,serv) sess, 'ss, 'tt) slot -> ('ss, 'tt, unit) monad
  val connect : 'p channel -> bindto:(empty, ('p,cli) sess, 'ss, 'tt) slot -> ('ss, 'tt, unit) monad

  val close : (([`close], 'r1*'r2) sess, empty, 'ss, 'tt) slot -> ('ss, 'tt, unit) monad
  val send : (([`msg of 'r1 * 'v * 'p], 'r1*'r2) sess, ('p, 'r1*'r2) sess, 'ss, 'tt) slot -> 'v -> ('ss, 'tt, unit) monad
  val recv : (([`msg of 'r2 * 'v * 'p], 'r1*'r2) sess, ('p, 'r1*'r2) sess, 'ss, 'tt) slot -> ('ss, 'tt, 'v) monad

  val branch2
    :  (([`branch of 'r2 * [`left of 'p1 | `right of 'p2]], 'r1*'r2) sess, ('p1, 'r1*'r2) sess, 'ss, 'tt1) slot * (unit -> ('tt1, 'uu, 'a) monad)
    -> (([`branch of 'r2 * [`left of 'p1 | `right of 'p2]], 'r1*'r2) sess, ('p2, 'r1*'r2) sess, 'ss, 'tt2) slot * (unit -> ('tt2, 'uu, 'a) monad)
    -> ('ss, 'uu, 'a) monad

  val select_left
      : (([`branch of 'r1 * [>`left of 's1]], 'r1*'r2) sess,
         ('s1, 'r1*'r2) sess, 'ss, 'tt) slot
      -> ('ss, 'tt, unit) monad

  val select_right
      : (([`branch of 'r1 * [>`right of 's2]], 'r1*'r2) sess,
         ('s2, 'r1*'r2) sess, 'ss, 'tt) slot
      -> ('ss, 'tt, unit) monad

  val _select
    :  (([`branch of 'r1 * 'br],'r1*'r2) sess, ('p,'r1*'r2) sess, 'ss, 'tt) slot
    -> ('p -> 'br)
    -> ('ss, 'tt, unit) monad

  val _branch_start
      :  (([`branch of 'r2 * 'br], 'r1*'r2) sess, 'x, 'ss, 'xx) slot
         -> ('br * ('r1*'r2) -> ('ss, 'uu,'v) monad)
         -> ('ss, 'uu, 'v) monad

  val _branch
      :  (([`branch of 'r2 * 'br], 'r1*'r2) sess, ('p,'r1*'r2) sess, 'ss, 'tt1) slot
         -> 'p * ('r1*'r2)
         -> ('tt1, 'uu, 'v) monad
         -> ('ss, 'uu, 'v) monad

  val deleg_send
      : (([`deleg of 'r1 * ('pp, 'rr) sess * 'p], 'r1*'r2) sess, ('p, 'r1*'r2) sess, 'ss, 'tt) slot
        -> release:(('pp, 'rr) sess, empty, 'tt, 'uu) slot
        -> ('ss, 'uu, unit) monad

  val deleg_recv
      : (([`deleg of 'r2 * ('pp, 'rr) sess * 'p], 'r1*'r2) sess, ('p, 'r1*'r2) sess, 'ss, 'tt) slot
        -> bindto:(empty, ('pp, 'rr) sess, 'tt, 'uu) slot
        -> ('ss, 'uu, unit) monad

end

(* type 'a parse_result = *)
(*   [`Partial of (string option -> 'a parse_result) (\* `Partial f. invariant: (f None) may not return Partial  *\) *)
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
