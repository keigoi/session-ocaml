type ('a,'b,'t) tag = 
  | Tag_left : ('a,'b,'a) tag
  | Tag_right : ('a,'b,'b) tag
type ('a,'b) either = 
  | Left : 'a -> ('a,'b) either 
  | Right : 'b -> ('a,'b) either

module Session : sig
  type ('an,'bn,'a,'b) t
  type (+'hd, +'tl) cons
  type nil
  type empty
  type all_empty = (empty, all_empty) cons

  val c0 : ('a0, 'b0, ('a0,'a) cons, ('b0,'a) cons) t
  val c1 : ('a1, 'b1, ('a0,('a1,'a) cons) cons, ('a0,('b1,'a) cons) cons) t
  val succ : ('an, 'bn, 'a, 'b) t -> ('an, 'bn, ('a0,'a) cons, ('a0,'b) cons) t

  type ('p,'q,'a) monad
  val ret : 'a -> ('p, 'p, 'a) monad
  val (>>=) : ('p,'q,'a) monad -> ('a -> ('q,'r,'b) monad) -> ('p,'r,'b) monad
  val (>>) : ('p,'q,unit) monad -> ('q,'r,'b) monad -> ('p,'r,'b) monad
  val wrap : ('a -> 'b) -> 'a -> ('p,'p,'b) monad
  val shift : ('p,'q,'a) monad -> (('p0,'p)cons,('p0,'q)cons,'a) monad

  val run : (all_empty,all_empty,'a) monad -> 'a

  type ('a,'k) send
  type ('a,'k) recv
  type ('s,'k) send_chan
  type ('s,'k) recv_chan
  type ('k1,'k2) select
  type ('k1,'k2) branch

  val send : (('a,'k)send, 'k, 'p, 'q) t -> 'a -> ('p,'q,unit) monad
  val recv : (('a,'k)recv, 'k, 'p, 'q) t -> ('p,'q,'a) monad
  val send_chan : 
    (('s,'k) send_chan, 'k, 'q, 'r) t -> 
    ('s, empty, 'p, 'q) t ->
    ('p,'r,unit) monad
  val recv_chan :
    (('s,'k) recv_chan, 'k, 'p, 'q) t ->
    (empty, 's, 'q, 'r) t ->
    ('p,'r,unit) monad
  val send_left :
    (('k1,'k2) select, 'k1, 'p, 'q) t ->
    ('p,'q,unit) monad
  val send_right :
    (('k1,'k2) select, 'k2, 'p, 'q) t ->
    ('p,'q,unit) monad
  val branch : 
    (('k1,'k2) branch,empty, 'p,'q) t -> 
    (empty,'k1, 'q,'q1) t * ('q1,'r,'a) monad ->
    (empty,'k2, 'q,'q2) t * ('q2,'r,'a) monad ->
    ('p,'r,'a) monad
  val fork : 
    ('a, empty, 'p, 'q) t -> 
    (('a, all_empty) cons, all_empty, unit) monad -> 
    ('p,'q,unit) monad

  type ('a,'b) chan
  val a2b : ('ka,'kb) chan -> (('v,'ka) send, ('v,'kb) recv) chan
  val b2a : ('ka,'kb) chan -> (('v,'ka) recv, ('v,'kb) send) chan
  val a2b_chan : ('ka,'kb) chan -> (('s,'ka) send_chan, ('s,'kb) recv_chan) chan
  val b2a_chan : ('ka,'kb) chan -> (('s,'ka) recv_chan, ('s,'kb) send_chan) chan
  val a2b_branch : ('ka1,'kb1) chan -> ('ka2,'kb2) chan -> (('ka1,'ka2) select, ('kb1,'kb2) branch) chan
  val b2a_branch : ('ka1,'kb1) chan -> ('ka2,'kb2) chan -> (('ka1,'ka2) branch, ('kb1,'kb2) select) chan
  val (!!) : ('ka,'kb) chan Lazy.t -> ('ka,'kb) chan
  val finish : (empty,empty) chan

  val new_chan : ('a,'b) chan -> ('p,('a,('b,'p)cons)cons, unit) monad

end 
= struct
  type ('a,'b,'p,'q) t = ('p -> 'a) * ('p -> 'b -> 'q)
  type ('hd, 'tl) cons = 'hd * 'tl
  type nil = unit
  type empty = unit
  type all_empty = unit * all_empty

  let c0 = (fun (a0,_) -> a0), (fun (_,a) b0 -> (b0,a))
  let c1 = (fun (_,(a1,_)) -> a1), (fun (a0,(_,a)) b1 -> (a0,(b1,a)))
  let succ (get,set) = (fun (_,a) -> get a), (fun (a0,a) bn -> (a0,set a bn))

  type ('p,'q,'a) monad = 'p -> 'q * 'a
  let ret a p = p, a
  let (>>=) m f p = let (q,a) = m p in f a q
  let (>>) m n p =  let (q,_) = m p in n q
  let wrap f a p = p, f a
  let shift m (p0,p) = let (q,a) = m p in (p0,q),a

  let rec all_empty = (), all_empty
  let run m = snd (m all_empty)

  type ('a, 'k) send = 'a -> 'k
  type ('a, 'k) recv = unit -> 'a * 'k
  type ('s, 'k) send_chan = 's -> 'k
  type ('s, 'k) recv_chan = unit -> 's * 'k
  type ('k1, 'k2) select = {select:'k. ('k1,'k2,'k) tag -> 'k}
  type ('k1, 'k2) branch = unit -> ('k1,'k2) either
  
  let some = function Some v -> v | None -> failwith "impossible"

  let send (get,set) v p = set p (get p v), ()
  let recv (get,set) p =
    let a, k = get p () in
    set p k, a
  let send_chan (get0,set0) (get1,set1) p = 
    let s, q = get1 p, set1 p () in
    let r = set0 q (get0 q s) in
    r, ()
  let recv_chan (get0,set0) (get1,set1) p =
    let s, k = get0 p () in
    let q = set0 p k in
    let r = set1 q s in
    r, ()
  let send_left (get0,set0) p =
    set0 p ((get0 p).select Tag_left), ()
  let send_right (get0,set0) p =
    set0 p ((get0 p).select Tag_right), ()
(*
  type ('k1,'k2,'k,'p,'r) alt2 = 
      Alt :
          ((('k1,'k2) branch, 'k, 'p, 'q) t 
           * (('k1,'k2,'k) tag -> ('q,'r,unit) monad)) -> ('k1,'k2,'k,'p,'r) alt2
  and ('k1,'k2,'p,'r) alt = 
      {alt: 'k. ('k1,'k2,'k,'p,'r) alt2}
  let branch_alt alt p = 
    let Alt((get,set),f) = alt.alt in
    let Either(tag,k) = get p () in
    f tag (set p k)
*)
  let branch (get0,set0) ((get1,set1),m1) ((get2,set2),m2) p =
    let k,q = get0 p (), set0 p () in
    match k with
      | Left k1 -> m1 (set1 q k1)
      | Right k2 -> m2 (set2 q k2)
  
  let fork (get,set) m p = 
    let a, q = get p, set p () in
    ignore (Thread.create (fun _ -> ignore (m (a,all_empty))) ());
    q, ()

  type ('a,'b) chan =  ('a * 'b) Lazy.t

  let fst_l (lazy (a,b)) = a
  let snd_l (lazy (a,b)) = b

  let a2b chan = lazy begin
    let c = Channel.create () in
    (fun v -> Channel.send c v; fst_l chan), (fun () -> Channel.receive c, snd_l chan)
  end
  let b2a chan = lazy begin
    let c = Channel.create () in
    (fun () -> Channel.receive c, fst_l chan), (fun v -> Channel.send c v; snd_l chan)
  end
  let a2b_chan chan = lazy begin
    let c = Channel.create () in
    (fun v -> Channel.send c v; fst_l chan), (fun () -> Channel.receive c, snd_l chan)
  end
  let b2a_chan chan = lazy begin
    let c = Channel.create () in
    (fun () -> Channel.receive c, fst_l chan), (fun v -> Channel.send c v; snd_l chan)
  end
  let a2b_branch : type ka1 ka2. 
      (ka1*'kb1) Lazy.t -> 
      (ka2*'kb2) Lazy.t -> 
      ((ka1,ka2) select * ('kb1,'kb2) branch) Lazy.t = fun chan1 chan2 -> lazy begin
    let c = Channel.create () in
    let select : type t. (ka1,ka2,t) tag -> t = fun tag ->
      match tag with
        | Tag_left -> Channel.send c (Left (snd_l chan1)); (fst_l chan1)
        | Tag_right -> Channel.send c (Right (snd_l chan2)); (fst_l chan2)
    in
    {select}, (fun () -> Channel.receive c)
  end
  let b2a_branch : type kb1 kb2. 
      ('ka1*kb1) Lazy.t -> 
      ('ka2*kb2) Lazy.t -> 
      (('ka1,'ka2) branch * (kb1,kb2) select) Lazy.t = fun chan1 chan2 -> lazy begin
    let c = Channel.create () in
    let select : type t. (kb1,kb2,t) tag -> t = fun tag ->
      match tag with
        | Tag_left -> Channel.send c (Left (fst_l chan1)); (snd_l chan1)
        | Tag_right -> Channel.send c (Right (fst_l chan2)); (snd_l chan2)
    in
    (fun () -> Channel.receive c), {select}
  end

  let (!!) chan = lazy (Lazy.force (Lazy.force chan))

  let finish = lazy (() , ())
  let new_chan (lazy (a,b)) p = (a,(b,p)), ()
end;;

include Session;;

let p () = 
  send c0 1234 >>
  recv c0 >>= 
  wrap print_endline >>
  ret ()
(*
val p :
  unit ->
  (((int, (string, 'a) recv) send, 'b) cons, ('a, 'b) cons, unit) monad
*)

let q () =
  recv c0 >>= fun i ->
  send c0 (string_of_int (i*2)) >>
  ret ()
(*
val q :
  unit ->
  (((int, (string, 'a) send) recv, 'b) cons, ('a, 'b) cons, unit) monad
*)

let r () = 
  new_chan (a2b (b2a finish)) >>
  new_chan finish >>
  fork (succ (succ (succ c0))) (q ()) >>
  shift (shift (p ()))
(*
val r :
  unit ->
  ('a, (empty, (empty, (empty, (empty, 'a) cons) cons) cons) cons, unit)
  monad
*)

let _ = run (r())

let p2 () = 
  send c0 7777 >>
  recv_chan c0 c1 >>
  recv c1 >>= wrap print_endline
(*
val p2 :
  unit ->
  (((int, ((string, 'a) recv, 'b) recv_chan) send, (empty, 'c) cons) cons,
   ('b, ('a, 'c) cons) cons, unit)
  monad
*)

let q2 () =
  recv c0 >>= fun v ->
  new_chan (a2b finish) >>
  send_chan (succ c1) c1 >>
  send c0 (string_of_int (v - 1111))
(*
val q2 :
  unit ->
  (((int, ((string, empty) recv, 'a) send_chan) recv, 'b) cons,
   (empty, (empty, ('a, 'b) cons) cons) cons, unit)
  monad
*)

let r2 () =
  new_chan (a2b (b2a_chan finish)) >>
  fork c1 (q2 ()) >>
  p2 ()
(*
val r2 : unit -> ('a, (empty, (empty, 'a) cons) cons, unit) monad
*)

let _ = run (r2())

let rec p3 n =
    if n>10 then 
      send_right c0 >> ret () 
    else
      send_left c0 >>
        send c0 n >>
        recv c0 >>= wrap print_endline >>
        p3 (n+1)

let rec q3 () = 
  branch c0
    (c0,recv c0 >>= fun x ->
        send c0 (string_of_int x) >>
        q3 ())
    (c0,ret ())

let r3 () =
  new_chan (let rec r = lazy (a2b_branch (a2b (b2a (!! r))) finish) in !! r) >>= fun _ ->
  fork c1 (q3 ()) >>
  p3 1

let _ = run (r3 ())

(* let v () = branch {alt=Alt(c0,fun _ -> failwith "")} *)
