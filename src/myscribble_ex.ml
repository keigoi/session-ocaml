open Myscribble

let error str =
  prerr_string str;
  exit 1

let speclist = Arg.align []

let usage_str = "./myscribble file"

let main () =
  let file = ref None in
  Arg.parse
    speclist
    (fun s ->
      if !file=None then
        file := Some s
      else
        error "too many arguments")
    usage_str;
  let file = match !file with Some s -> s | None -> error "no file given" in
  let ast = Scribble.parse_file file in
  let ast =
    match ast with
    | FileAS(_,[Localast(_,_,_,_,prot)]) -> prot
    | _ -> error "must give one local protocol"
  in
  let typ = local_of_scribblelocal ast in
  let typ = typ_of_local typ in
  let typ = [%type: [%t typ] channel ] in 
  Pprintast.core_type Format.std_formatter typ;
  Format.print_flush ();
  print_newline ();
  ()
;;

let _ =
  main ()
