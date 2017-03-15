open Session

let bufsize = 4096

type raw_chan = {in_ch:in_channel; in_buf:string; out_ch:out_channel}
type 'p net = raw_chan -> (('p, serv) sess, empty, unit) monad

let consume (read:string->'v parse_result) {in_ch;in_buf} =
  let consume_buf : string -> 'v parse_result = function
    | "" -> `Partial(function Some(x) -> read x | None -> assert false (*FIXME: EOF*))
    | buf -> read buf
  in
  let buf = Bytes.create bufsize in
  let rec read_loop : 'v parse_result -> 'v option * string = function
    | `Done(v,rest) -> Some(v),rest
    | `Fail(rest) -> None,rest
    | `Partial read ->
       let len_read = input in_ch buf 0 bufsize in
       if len_read = 0 then
         read_loop (read None)
       else
         read_loop (read @@ Some(Bytes.sub_string buf 0 len_read))
  in
  read_loop (consume_buf in_buf)

let req : 'v 'p . ('v -> string) -> 'p net -> [`msg of req * 'v * 'p] net
  = fun print cont ({out_ch} as ch) ->
  Session0.recv () >>= fun v ->
  (output_string out_ch (print v); flush out_ch; cont ch)

let resp
  = fun read cont ({in_buf} as ch) ->
  let v, in_buf = consume read ch in
  let ch = {ch with in_buf} in
  match v with
  | None -> assert false (*FIXME:EOF*)
  | Some(v) ->
     Session0.send v >>
     cont ch

let sel : 'p1 'p2. left:'p1 net -> right:'p2 net ->
          [`branch of req * [`left of 'p1|`right of 'p2]] net
  = fun ~left ~right ch ->
  Session0.branch2 (fun () -> left ch)
                   (fun () -> right ch)

let bra
  = fun ~left:(read,left) ~right ({in_ch;in_buf} as ch) ->
  let v, in_buf = consume read ch in
  let ch = {ch with in_buf} in
  match v with
  | Some v -> Session0.select_left () >> Session0.send v >> left ch
  | None -> Session0.select_right () >> right ch

let cls : [`close] net
  = fun {in_ch;out_ch;in_buf} ->
  close_in_noerr in_ch;
  close_out_noerr out_ch;
  (* assert in_buf="" *)
  Session0.close ()

module TcpSession = struct
  let new_channel adapter hostport =
    let connect () =
      let host,port =
        match Str.split (Str.regexp ":") hostport with
        | [host] -> host, "25"
        | host::port::_ -> host, port

        | [] -> assert false
      in
      match Unix.getaddrinfo host port [] with
        | [] -> failwith ("Host not found " ^ host)
        | h::t -> Unix.open_connection h.Unix.ai_addr
    in
    let in_ch,out_ch = connect () in
    let ch = Session.new_channel () in
    ignore @@ Thread.create (Session0.accept_ ch adapter) {in_ch;out_ch;in_buf=""};
    ch
end
