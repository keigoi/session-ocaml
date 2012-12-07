#!/bin/sh
OCAMLC='ocamlfind ocamlopt -thread -package unix,threads'
$OCAMLC -c monitor.mli mVar.mli channel.mli monitor.ml mVar.ml channel.ml test.ml
$OCAMLC -linkpkg -o test monitor.cmx mVar.cmx channel.cmx test.cmx
