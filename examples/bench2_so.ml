let start = ref @@ Core.Int63.of_int 0
let now () = (Core.Time_stamp_counter.now () :> Core.Int63.t)
      
module Make(LinIO:Linocaml.Base.LIN_IO)(S : Session_ocaml.Base.SESSION with type 'a io = 'a LinIO.IO.io and type ('p,'q,'a) monad = ('p,'q,'a) LinIO.monad ) = struct
  open LinIO
  open LinIO.Syntax
  open S
     
  let s = {Linocaml.Lens.get=(fun (x,_) -> x); put=(fun (_,y) x -> (x,y))}
  let t = {Linocaml.Lens.get=(fun (_,(x,_)) -> x); put=(fun (x0,(_,y)) x -> (x0,(x,y)))}
  let u = {Linocaml.Lens.get=(fun (_,(_,(x,_))) -> x); put=(fun (x0,(x1,(_,y))) x -> (x0,(x1,(x,y))))}
  let run_ctx f x = LinIO.Syntax.Internal.__run (f x) (Linocaml.Base.Empty,(Linocaml.Base.Empty,(Linocaml.Base.Empty,Linocaml.Base.Empty)))
  (* type ('a,'b,'c) ctx = <s:'a; t:'b; u:'c> [@@runner][@@deriving lens] *)
    
  let cOUNT = 8000
  
  let rec server ch () =
    let rec aux () =
    branch s >>= function%lin
    | `True(#s) ->
        receive s >>= fun%lin n ->
        deleg_recv s >>= fun%lin #t ->
        receive t >>= fun%lin m ->
        send t (n + m) >>= fun%lin () ->
        close t >>
        aux ()
    | `False(#s) ->
       close s >>= fun%lin () ->
       return ()
    in
    accept ch >>= fun%lin #s ->
    aux ()
    
  
  let client ch cnt () =
    start := now ();
    let rec aux acc n =
      if n = 0 then begin
        select s (fun x -> `False(x)) >>
        close s >>
        return acc
      end else begin
        select s (fun x -> `True(x)) >>
        send s acc >>
        let%lin #t,#u = create () in  
        deleg_send s u >>  
        send t n >>
        receive t >>= fun%lin res ->
        close t >>
        aux res (n - 1)
      end
    in
    connect ch >>= fun%lin #s ->
    aux 0 cnt
  		      
  let run () =
    let ch = new_shmem_channel () in
    ignore @@ Thread.create (run_ctx (server ch)) ();
    ignore @@ run_ctx (client ch cOUNT) ();
    let countpersec =
      let c = (cOUNT * 5 + 1) * 1000000000 in
      Core.Int63.(of_int c / (now () - !start))
    in
    print_endline (Core.Int63.to_string countpersec)
end

module S = Make(Linocaml.Direct)(Session_ocaml.Synchronous)
module A = Make(Linocaml.Direct)(Session_ocaml.Direct)

let _ =
  match Sys.argv.(1) with
  | "S" -> S.run ()
  | "A" -> A.run ()
  | _ -> ()
