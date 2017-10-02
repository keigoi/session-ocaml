let start = ref @@ Core.Int63.of_int 0
let now () = (Core.Time_stamp_counter.now () :> Core.Int63.t)

module Make(LinIO:Linocaml.Base.LIN_IO)(S : Session_ocaml.Base.SESSION with type 'a io = 'a LinIO.IO.io and type ('p,'q,'a) monad = ('p,'q,'a) LinIO.monad ) = struct
  open LinIO
  open LinIO.Syntax
  open S
     
  (* type ('a,'b,'c) ctx = <s:'a;t:'b;u:'c> [@@runner][@@deriving lens] *)
  let s = {Linocaml.Lens.get=(fun x -> x); put=(fun _ x -> x)}
  let run_ctx f x = LinIO.Syntax.Internal.__run (f x) Linocaml.Base.Empty
    
  let cOUNT = 8000
  
  let rec server ch () =
    let rec aux () =
    branch s >>= function%lin
    | `True(#s) ->
        receive s >>= fun%lin n ->
        receive s >>= fun%lin m ->
        send s (n + m) >>= fun%lin () ->
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
        send s n >>
        receive s >>= fun%lin res ->
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
      let c = (cOUNT * 4 + 1) * 1000000000 in
      Core.Int63.(of_int c / (now () - !start))
    in
    print_endline (Core.Int63.to_string countpersec)
end

module M = Make(Linocaml.Direct)(Session_ocaml.Synchronous)

let _ =
  M.run ()
