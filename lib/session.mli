(* empty slots *)
type empty
type all_empty = empty * 'a as 'a

(* lenses on slots *)
type ('a,'b,'pre,'post) slot = {get:('pre -> 'a); put:('pre -> 'b -> 'post)}

val s : ('a, 'b, ('a * 'pre), ('b * 'pre)) slot
val _0 : ('a, 'b, ('a * 'pre), ('b * 'pre)) slot
val _1 : ('a, 'b, ('s0 * ('a * 'pre)), ('s0 * ('b * 'pre))) slot
val _2 : ('a, 'b, ('s0 * ('s1 * ('a * 'pre))), ('s0 * ('s1 * ('b * 'pre)))) slot
val _3 : ('a, 'b, ('s0 * ('s1 * ('s2 * ('a * 'pre)))), ('s0 * ('s1 * ('s2 * ('b * 'pre))))) slot

(* parameterized sessions *)
type ('x,'y,'a) session

val return : 'a -> ('pre,'pre,'a) session
val (>>=) : ('pre,'mid,'a) session -> ('a -> ('mid,'post,'b) session) -> ('pre,'post,'b) session
val (>>) : ('pre,'mid,'a) session -> ('mid,'post,'b) session -> ('pre,'post,'b) session

val run_ : (all_empty,all_empty,unit) session -> unit
val run : ('a -> (all_empty,all_empty,'b) session) -> 'a -> 'b
val _run_internal : 'a -> ('b -> ('a, 'a, 'c) session) -> 'b -> 'c

(* channels *)
type 'p channel

val new_channel : unit -> 'p channel

(* polarized session types *)
type req and resp

type cli = req * resp
type serv = resp * req

type ('p, 'q) sess


val accept_ : 'p channel -> ('a -> (('p,serv) sess * all_empty, all_empty, 'b) session) -> 'a -> 'b
val connect_ : 'p channel -> ('a -> (('p,cli) sess * all_empty, all_empty, 'b) session) -> 'a -> 'b

val accept : 'p channel -> bindto:(empty, ('p,serv) sess, 'pre, 'post) slot -> ('pre, 'post, unit) session
val connect : 'p channel -> bindto:(empty, ('p,cli) sess, 'pre, 'post) slot -> ('pre, 'post, unit) session

val close : (([`close], 'r1*'r2) sess, empty, 'pre, 'post) slot -> ('pre, 'post, unit) session
val send : (([`msg of 'r1 * 'v * 'p], 'r1*'r2) sess, ('p, 'r1*'r2) sess, 'pre, 'post) slot -> 'v -> ('pre, 'post, unit) session
val recv : (([`msg of 'r2 * 'v * 'p], 'r1*'r2) sess, ('p, 'r1*'r2) sess, 'pre, 'post) slot -> ('pre, 'post, 'v) session

val branch
  :  left:(([`branch of 'r2 * [`left of 'p1 | `right of 'p2]], 'r1*'r2) sess, ('p1, 'r1*'r2) sess, 'pre, 'mid1) slot * (unit -> ('mid1, 'post, 'a) session)
  -> right:(([`branch of 'r2 * [`left of 'p1 | `right of 'p2]], 'r1*'r2) sess, ('p2, 'r1*'r2) sess, 'pre, 'mid2) slot * (unit -> ('mid2, 'post, 'a) session)
  -> ('pre, 'post, 'a) session

val select_left
    : (([`branch of 'r1 * [>`left of 'p1]], 'r1*'r2) sess,
       ('p1, 'r1*'r2) sess, 'pre, 'post) slot
    -> ('pre, 'post, unit) session

val select_right
    : (([`branch of 'r1 * [>`right of 'p2]], 'r1*'r2) sess,
       ('p2, 'r1*'r2) sess, 'pre, 'post) slot
    -> ('pre, 'post, unit) session

val _select
  :  (([`branch of 'r1 * 'br],'r1*'r2) sess, ('p,'r1*'r2) sess, 'pre, 'post) slot
  -> ('p -> ([>] as 'br))
  -> ('pre, 'post, unit) session

val _branch
    :  (([`branch of 'r2 * 'br], 'r1*'r2) sess, empty, 'pre, 'mid) slot
       -> ('br * ('r1*'r2) -> ('mid, 'post,'v) session)
       -> ('pre, 'post, 'v) session

val _set_sess
    :  (empty, ('p,'r1*'r2) sess, 'pre, 'mid) slot
       -> 'p * ('r1*'r2)
       -> ('mid, 'post, 'v) session
       -> ('pre, 'post, 'v) session

val deleg_send
    : (([`deleg of 'r1 * ('pp, 'qq) sess * 'p], 'r1*'r2) sess, ('p, 'r1*'r2) sess, 'pre, 'mid) slot
      -> release:(('pp, 'qq) sess, empty, 'mid, 'post) slot
      -> ('pre, 'post, unit) session

val deleg_recv
    : (([`deleg of 'r2 * ('pp, 'qq) sess * 'p], 'r1*'r2) sess, ('p, 'r1*'r2) sess, 'pre, 'mid) slot
      -> bindto:(empty, ('pp, 'qq) sess, 'mid, 'post) slot
      -> ('pre, 'post, unit) session

type 'a parse_result =
  [`Partial of (string option -> 'a parse_result) (* `Partial f. invariant: (f None) may not return Partial  *)
  |`Done    of 'a * string
  |`Fail of string]

module type Adapter = sig
  type raw_chan
  type 'p net = raw_chan -> (('p, serv) sess * all_empty, all_empty, unit) session
  val req : ('v -> string) -> 'p net -> [`msg of req * 'v * 'p] net
  val resp : (string -> 'v parse_result) -> 'p net -> [`msg of resp * 'v * 'p] net
  val sel : left:'p1 net -> right:'p2 net ->
          [`branch of req * [`left of 'p1|`right of 'p2]] net
  val bra : left:((string -> 'v1 parse_result) * 'p1 net) -> right:'p2 net ->
          [`branch of resp * [`left of [`msg of resp * 'v1 * 'p1] |`right of 'p2]] net
  val cls : [`close] net
end

module Syntax : sig
  val bind : ('x,'y,'a) session -> ('a -> ('y, 'z, 'b) session) -> ('x,'z,'b) session
  val _select
    :  (([`branch of 'r1 * 'br],'r1*'r2) sess, ('p,'r1*'r2) sess, 'pre, 'post) slot
    -> ('p -> ([>] as 'br))
    -> ('pre, 'post, unit) session

  val _branch
      :  (([`branch of 'r2 * 'br], 'r1*'r2) sess, empty, 'pre, 'mid) slot
         -> ('br * ('r1*'r2) -> ('mid, 'post,'v) session)
         -> ('pre, 'post, 'v) session

  val _set_sess
      :  (empty, ('p,'r1*'r2) sess, 'pre, 'mid) slot
         -> 'p * ('r1*'r2)
         -> ('mid, 'post, 'v) session
         -> ('pre, 'post, 'v) session
end
