(* slots and lenses *)
type empty = Empty

(* monads *)
type ('ss,'tt,'v) monad = 'ss -> 'tt * 'v

let return x p = p, x
let (>>=) m f p = let q,v = m p in f v q
let (>>) m n p = let q,_ = m p in n q


(* service channels *)
type 'p channel = unit
let new_channel () = Obj.magic ()

type ('ep,+'p) sess
type (+'from,+'to_) dir

              
module Local(X:sig type me type them end) = struct
type +'p sess
type ('alice,'bob,'ep,'v,'p) msg = ([`msg of ('alice,'bob)dir * 'v * 'p] sess, 'p sess, unit) monad
type ('alice,'bob,'ep,'br,'p) branch = ([`branch of ('alice,'bob)dir * 'br] sess, 'p sess, unit) monad
let connect ch = Obj.magic ()
let close = Obj.magic ()
let send _ = Obj.magic ()
let recv = Obj.magic ()
let _select _ = Obj.magic ()
let _branch_start _ = Obj.magic ()
let _branch _ = Obj.magic ()
let ignore_msg = Obj.magic ()
let ignore_branch _ = Obj.magic ()

module Syntax = struct
  let (>>=) = (>>=)
  module SessionN = struct
    let _select = _select
    let _branch_start = _branch_start
    let _branch = _branch
  end
end
end
