type ('a,'b) either = 
  | Left : 'a -> ('a,'b) either 
  | Right : 'b -> ('a,'b) either

module Session : sig
  type ('an,'bn,'a,'b) idx
  type (+'hd, +'tl) cons
  type empty
  type all_empty = (empty, 'a) cons as 'a

  val _0 : ('a0, 'b0, ('a0,'a) cons, ('b0,'a) cons) idx
  val _1 : ('a1, 'b1, ('a0,('a1,'a) cons) cons, ('a0,('b1,'a) cons) cons) idx
  val _2 : ('a2, 'b2, ('a0,('a1,('a2,'a) cons) cons) cons, ('a0,('a1,('b2,'a) cons) cons) cons) idx
  val _3 : ('a3, 'b3, ('a0,('a1,('a2,('a3,'a) cons) cons) cons) cons, ('a0,('a1,('a2,('b3,'a) cons) cons) cons) cons) idx
  val succ : ('an, 'bn, 'a, 'b) idx -> ('an, 'bn, ('a0,'a) cons, ('a0,'b) cons) idx

  type ('p,'q,'a) monad
  val ret : 'a -> ('p, 'p, 'a) monad
  val (>>=) : ('p,'q,'a) monad -> ('a -> ('q,'r,'b) monad) -> ('p,'r,'b) monad
  val (>>) : ('p,'q,'a) monad -> ('q,'r,'b) monad -> ('p,'r,'b) monad
  val fmap : ('a -> 'b) -> 'a -> ('p,'p,'b) monad
  val slide : ('p,'q,'a) monad -> (('p0,'p)cons,('p0,'q)cons,'a) monad

  val run : (all_empty,all_empty,'a) monad -> 'a

  type ('s, 'v, 'k) shot
  type ('s, 'c, 'k) pass
  type ('s, 'k1, 'k2) branch
  type finish
         
  type pos and neg

  type ('s, 't, 'k) channel
                    
  val send : (('s,'t,('s,'a,'k)shot)channel , ('s,'t,'k) channel, 'p, 'q) idx -> 'a -> ('p,'q,unit) monad
  val recv : (('s,'t,('t,'a,'k)shot)channel, ('s,'t,'k) channel, 'p, 'q) idx -> ('p,'q,'a) monad
  val close : (('s,'t,finish)channel, empty, 'p, 'q) idx -> ('p, 'q, unit) monad

  val send_slot : 
    (('s,'t,('s,'c,'k) pass) channel, ('s,'t,'k) channel, 'q, 'r) idx -> 
    ('c, empty, 'p, 'q) idx ->
    ('p,'r,unit) monad

  val recv_slot :
    (('s,'t,('t,'c,'k) pass) channel, ('s,'t,'k) channel, 'p, 'q) idx -> 
    (empty, 'c, 'q, 'r) idx ->
    ('p,'r,unit) monad

  val send_chan : 
    (('s,'t,('s,('ss,'tt,'kk)channel,'k) pass) channel, ('s,'t,'k) channel, 'q, 'r) idx -> 
    (('ss,'tt,'kk)channel, empty, 'p, 'q) idx ->
    ('p,'r,unit) monad

  val recv_chan :
    (('s,'t,('t,('ss,'tt,'kk)channel,'k) pass) channel, ('s,'t,'k) channel, 'p, 'q) idx -> 
    (empty, ('ss,'tt,'kk)channel, 'q, 'r) idx ->
    ('p,'r,unit) monad

  val send_new_chan : 
    (('s,'t,('s,(neg,pos,'kk)channel,'k) pass) channel, ('s,'t,'k) channel, 'p, 'q) idx -> 
    (empty,(pos,neg,'kk)channel, 'q, 'r) idx ->
    ('p,'r,unit) monad
                 
  val select_left :
    (('s,'t,('s,'k1,'k2)branch)channel, ('s,'t,'k1)channel, 'p, 'q) idx ->
    ('p,'q,unit) monad
  val select_right :
    (('s,'t,('s,'k1,'k2)branch)channel, ('s,'t,'k2)channel, 'p, 'q) idx ->
    ('p,'q,unit) monad
  val branch : 
    (('s,'t,('t,'k1,'k2)branch)channel, empty, 'p, 'q) idx ->
    (empty,('s,'t,'k1)channel, 'q,'q1) idx * ('q1,'r,'a) monad ->
    (empty,('s,'t,'k2)channel, 'q,'q2) idx * ('q2,'r,'a) monad ->
    ('p,'r,'a) monad

  val fork : 
    (empty, (pos,neg,'k)channel, 'p, 'q) idx -> 
    (((neg,pos,'k)channel, all_empty) cons, all_empty, unit) monad -> 
    ('p,'q,unit) monad

end 
= struct
  type ('a,'b,'p,'q) idx = ('p -> 'a) * ('p -> 'b -> 'q)
  type ('hd, 'tl) cons = 'hd * 'tl
  type empty = Empty
  type all_empty = empty * all_empty

  let _0 = (fun (a0,_) -> a0), (fun (_,a) b0 -> (b0,a))
  let _1 = (fun (_,(a1,_)) -> a1), (fun (a0,(_,a)) b1 -> (a0,(b1,a)))
  let _2 = (fun (_,(_,(a2,_))) -> a2), (fun (a0,(a1,(_,a))) b2 -> (a0,(a1,(b2,a))))
  let _3 = (fun (_,(_,(_,(a3,_)))) -> a3), (fun (a0,(a1,(a2,(_,a)))) b3 -> (a0,(a1,(a2,(b3,a)))))
  let succ (get,set) = (fun (_,a) -> get a), (fun (a0,a) bn -> (a0,set a bn))

  type ('p,'q,'a) monad = 'p -> 'q * 'a
  let ret a p = p, a
  let (>>=) m f p = let (q,a) = m p in f a q
  let (>>) m n p =  let (q,_) = m p in n q
  let fmap f a p = p, f a
  let slide m (p0,p) = let (q,a) = m p in (p0,q),a

  let rec all_empty = Empty, all_empty
  let run m = snd (m all_empty)

  type pos = Pos and neg = Neg

  type ('s,'v,'k) shot = Shot of 's * 'v * 'k Channel.t
  type ('s,'c,'k) pass = Pass of 's * 'c * 'k Channel.t
  type ('s,'k1,'k2) branch = BranchLeft of 's * 'k1 Channel.t | BranchRight of 's * 'k2 Channel.t
  type finish = unit

  type ('s,'t,'k) channel = Chan of 's * 't * 'k Channel.t

  let send (get,set) v p =
    let Chan(s,t,c) = get p in
    let c' = Channel.create () in
    Channel.send c (Shot(s,v,c'));
    set p (Chan(s,t,c')), ()
                                     
  let recv (get,set) p =
    let Chan(s,t,c) = get p in
    let Shot(_, v, c') = Channel.receive c in
    set p (Chan(s,t,c')), v

  let close (get,set) p =
    set p Empty, ()

  let send_slot (get0,set0) (get1,set1) p =
    let e, q = get1 p, set1 p Empty in
    let Chan(s,t,c) = get0 q in
    let c' = Channel.create () in
    Channel.send c (Pass(s,e,c'));
    set0 q (Chan(s,t,c')), ()

  let send_chan = send_slot
                   
  let recv_slot (get0,set0) (get1,set1) p =
    let Chan(s,t,c) = get0 p in
    let Pass(_,e,c') = Channel.receive c in
    set1 (set0 p (Chan(s,t,c'))) e, ()

  let recv_chan = recv_slot

  let send_new_chan : 
    (('s,'t,('s,(neg,pos,'kk)channel,'k) pass) channel, ('s,'t,'k) channel, 'p, 'q) idx -> 
    (empty,(pos,neg,'kk)channel, 'q, 'r) idx ->
    ('p,'r,unit) monad = fun (get0,set0) (get1,set1) p ->
    let Chan(s,t,c) = get0 p in
    let c' = Channel.create () in
    let cc = Channel.create () in
    let e = Pass(s,Chan(Neg,Pos,cc),c') in
    Channel.send c e;
    set1 (set0 p (Chan(s,t,c'))) (Chan(Pos,Neg,cc)), ()

  let select_left (get,set) p =
    let Chan(s,t,c) = get p in
    let c' = Channel.create () in
    Channel.send c (BranchLeft(s,c'));
    set p (Chan(s,t,c')), ()
                         
  let select_right (get,set) p =
    let Chan(s,t,c) = get p in
    let c' = Channel.create () in
    Channel.send c (BranchRight(s,c'));
    set p (Chan(s,t,c')), ()

  let branch :
    (('s,'t,('t,'k1,'k2)branch)channel, empty, 'p, 'q) idx ->
    (empty,('s,'t,'k1)channel, 'q,'q1) idx * ('q1,'r,'a) monad ->
    (empty,('s,'t,'k2)channel, 'q,'q2) idx * ('q2,'r,'a) monad ->
    ('p,'r,'a) monad = fun (get0,set0) ((get1,set1),m1) ((get2,set2),m2) p ->
    let Chan(s,t,c), q = get0 p, set0 p Empty in
    match Channel.receive c with
    | BranchLeft(_,c') -> m1 (set1 q (Chan(s,t,c')))
    | BranchRight(_,c') -> m2 (set2 q (Chan(s,t,c')))


  let fork (get,set) m p = 
    let chan = Channel.create () in
    ignore (Thread.create (fun _ -> ignore (m (Chan(Neg,Pos,chan),all_empty))) ());
    set p (Chan(Pos,Neg,chan)), ()

end;;

include Session;;

let p () = 
  send _0 1234 >>
  recv _0 >>= 
  fmap print_endline >>
  close _0 >>
  ret ()
      
      (*
val p :
    unit ->
      ((('a, 'b, ('a, int, ('b, string, finish) shot) shot) chan, 'c) cons,
          (empty, 'c) cons, unit)
          monad = <fun>
       *)
(*
val p :
  unit ->
  (((int, (string, 'a) recv) send, 'b) cons, ('a, 'b) cons, unit) monad
*)

let q () =
  recv _0 >>= fun i ->
  send _0 (string_of_int (i*2)) >>
  close _0 >>
  ret ()
(*
val q :
    unit ->
      ((('a, 'b, ('b, int, ('a, string, finish) shot) shot) chan, 'c) cons,
          (empty, 'c) cons, unit)
          monad
*)

let r () = 
  fork _0 (q ()) >>
  p ()
(*
val r :
    unit ->
      ((empty, (empty, 'a) cons) cons, (empty, (empty, 'a) cons) cons, unit)
          monad
*)

let _ = run (r())


let p2 () = 
  send _0 7777 >>
  recv_slot _0 _1 >>
  recv _1 >>= fmap print_endline >>
  close _0 >>
  close _1

let q2 () =
  recv _0 >>= fun v ->
  send_new_chan _0 _1 >>
  send _1 (string_of_int (v - 1111)) >>
  close _0 >>
  close _1

let r2 () =
  fork _0 (q2 ()) >>
  p2 ()

let _ = run (r2())

let rec p3 n =
    if n>10 then 
      select_right _0 >> close _0
    else
      select_left _0 >>
        send _0 n >>
        recv _0 >>= fmap print_endline >>
        p3 (n+1)

let rec q3 () = 
  branch _0
    (_0,recv _0 >>= fun x ->
        send _0 (string_of_int x) >>
        q3 ())
    (_0,close _0)

let r3 () =
  fork _0 (q3 ()) >>
  p3 1

let _ = run (r3 ())

(* let v () = branch {alt=Alt(_0,fun _ -> failwith "")} *)
