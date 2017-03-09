
(* parameterized monads *)
type (-'x,+'y,+'a) monad

val return : 'v -> ('x,'x,'v) monad
val (>>=) : ('x,'y,'a) monad -> ('a -> ('y, 'z, 'b) monad) -> ('x,'z,'b) monad
val (>>) : ('x,'y,'a) monad -> ('y,'z,'b) monad -> ('x,'z,'b) monad

(* channels *)
type 'p channel

val new_channel : unit -> 'p channel

type empty
type (+'from,+'to_) dir
   
     
module Local(X:sig type me type them end): sig
type +'p sess
val connect : 'p channel -> (empty, 'p sess, unit) monad
val close : unit -> ([`close] sess, empty, unit) monad
val send : 'them -> 'v -> ([`msg of (X.me,'them)dir * 'v * 'p] sess, 'p sess, unit) monad
val recv : 'them -> ([`msg of ('them,X.me)dir * 'v * 'p] sess, 'p sess, 'v) monad
val _select : 'them -> ('p -> 'br) -> ([`branch of (X.me,'them)dir * 'br] sess, 'p sess, unit) monad
val _branch_start :
  'them
  -> ('br*'them -> ([`branch of ('them,X.me)dir * 'br] sess, 'p, unit) monad)
  -> ([`branch of ('them,X.me)dir * 'br] sess, 'p, unit) monad
val _branch :
  'p*'them
  -> ('p sess, 'q, unit) monad
  -> ([`branch of ('them,X.me) dir * 'br] sess, 'q, unit) monad

type ('alice,'bob,'ep,'v,'p) msg = ([`msg of ('alice,'bob)dir * 'v * 'p] sess, 'p sess, unit) monad
type ('alice,'bob,'ep,'br,'p) branch = ([`branch of ('alice,'bob)dir * 'br] sess, 'p sess, unit) monad
val ignore_msg : ([`msg of (X.them,X.them) dir * 'v * 'p] sess, 'p sess, unit) monad
val ignore_branch : ('p -> 'br) -> ([`branch of (X.them,X.them) dir * 'br] sess, 'p sess, unit) monad

module Syntax : sig
  val (>>=) : ('x,'y,'a) monad -> ('a -> ('y, 'z, 'b) monad) -> ('x,'z,'b) monad
  module SessionN : sig
    val _select : 'them -> ('p -> 'br) -> ([`branch of (X.me,'them)dir * 'br] sess, 'p sess, unit) monad
    val _branch_start :
      'them
      -> ('br*'them -> ([`branch of ('them,X.me)dir * 'br] sess, 'p, unit) monad)
      -> ([`branch of ('them,X.me)dir * 'br] sess, 'p, unit) monad
    val _branch :
      'them ->
      'p*'dummy
      -> ('p sess, 'q, unit) monad
      -> ([`branch of ('them,X.me) dir * 'br] sess, 'q, unit) monad
  end
end
end
