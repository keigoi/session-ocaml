open Session

type raw_chan = in_channel * out_channel
type 'p net = raw_chan -> (('p, serv) sess * all_empty, all_empty, unit) monad

let req : 'v 'p . ('v -> string) -> 'p net -> [`msg of req * 'v * 'p] net
  = fun print cont ((_,o) as ch) ->
  Session0.recv () >>= fun v ->
  (output_string o (print v); cont ch)

let resp : 'v 'p. (string -> 'v) -> 'p net -> [`msg of resp * 'v * 'p] net
  = fun read cont ((i,_) as ch) ->
  let str = input_line i in
  Session0.send (read str) >>
  cont ch

let sel : 'p1 'p2. left:'p1 net -> right:'p2 net ->
          [`branch of req * [`left of 'p1|`right of 'p2]] net
  = fun ~left ~right ch ->
  Session0.branch2 (fun () -> left ch)
                   (fun () -> right ch)

let bra : 'v1 'p1 'p2. left:((string -> 'v1 option) * 'p1 net) -> right:'p2 net ->
          [`branch of resp * [`left of [`msg of resp * 'v1 * 'p1] |`right of 'p2]] net
  = fun ~left:(read,left) ~right ((i,_)as ch) ->
  let str = input_line i in
  match read str with
  | Some v -> Session0.select_left () >> Session0.send v >> left ch
  | None -> Session0.select_right () >> right ch

let cls : [`close] net
  = fun (i,o) ->
  close_in_noerr i;
  close_out_noerr o;
  Session0.close ()
