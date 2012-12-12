
module Session : sig
  type ('one, 'all) t
  type ('hd, 'tl) cons
  type nil
  type empty
  val c0 : ('a0 -> 'b0, ('a0,'a) cons -> ('b0,'a) cons) t

  type ('p,'q,'a) monad
  val ret : 'a -> ('p, 'p, 'a) monad
  val (>>=) : ('p,'q,'a) monad -> ('a -> ('q,'r,'b) monad) -> ('p,'r,'b) monad

  type ('a,'k) send
  type ('a,'k) recv
  val send : (('a,'k)send -> 'k, 'p -> 'q) t -> 'a -> ('p,'q,unit) monad
  val recv : (('a,'k)recv -> 'k, 'p -> 'q) t -> ('p,'q,'a) monad
  val fork : ('a -> empty, 'p -> 'q) t -> (('a,nil)cons, (empty,nil)cons, unit) monad -> ('p,'q,unit) monad

  type ('a,'b) chan
  val a2b : ('ka,'kb) chan -> (('v,'ka) send, ('v,'kb) recv) chan
  val b2a : ('ka,'kb) chan -> (('v,'ka) recv, ('v,'kb) send) chan
  val finish : (empty,empty) chan

  val new_chan : ('a,'b) chan -> ('p,('a,('b,'p)cons)cons, unit) monad

end 
= struct
  type ('f, 'g) t = 'f -> 'g
  type ('hd, 'tl) cons = 'hd * 'tl
  type nil = unit
  type empty = unit
  let c0 f (a0,a) = (f a0,a)

  type ('p,'q,'a) monad = 'p -> 'q * 'a
  let ret a p = p, a
  let (>>=) m f p = let (q,a) = m p in f a q

  type ('a, 'k) send = 'a -> 'k
  type ('a, 'k) recv = 'a * 'k
  let send c v p = c (fun f -> f v) p, ()
  let recv c p = (*FIXME*)
    let cell = ref (Obj.magic 0) in 
    let q = c (fun (a, k) -> cell := a; k) p in
    q, !cell
  let fork c m p = 
    c (fun a -> (*fork*) (m (a,())); ()) p, ()

  type ('a,'b) chan = 'a * 'b
  let a2b _ = raise Not_found (* not implemented *)
  let b2a _ = raise Not_found (* not implemented *)
  let finish = () , ()
  let new_chan (a,b) p = (a,(b,p)), ()
end;;

