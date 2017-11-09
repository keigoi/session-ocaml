OCAMLC=ocamlfind ocamlc -g -thread -package session-ocaml,session-ocaml.ppx -rectypes -short-paths
OCAMLOPT=ocamlfind ocamlc -thread -package session-ocaml,session-ocaml.ppx -rectypes -short-paths
OCAMLMKTOP=ocamlfind ocamlmktop -thread -package session-ocaml -rectypes
OCAMLDEP=ocamlfind ocamldep -package session-ocaml

EXAMPLES=ex_single1 ex_single2 ex_multi1 ex_multi2 travel_agency smtp smtp2

EXAMPLES_BYTE = $(EXAMPLES:%=%.byte)


all: $(EXAMPLES_BYTE)

smtp2.byte: smtp2.cmo
	$(OCAMLC) -package session-ocaml.net -linkpkg -o $@ $^

smtp2: smtp2.cmx
	$(OCAMLOPT) -package session-ocaml.net -linkpkg -o $@ $^

ex_single2.byte: ex_single1.cmo ex_single2.cmo
	$(OCAMLC) -linkpkg -o $@ $^

ex_single2: ex_single1.cmx ex_single2.cmx
	$(OCAMLOPT) -linkpkg -o $@ $^

ex_multi1.byte: ex_single1.cmo ex_single2.cmo ex_multi1.cmo
	$(OCAMLC) -linkpkg -o $@ $^

ex_multi1: ex_single1.cmx ex_single2.cmx ex_multi1.cmx
	$(OCAMLOPT) -linkpkg -o $@ $^

%.byte: %.cmo
	$(OCAMLC) -linkpkg -o $@ $^


# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

smtp2.cmo: smtp2.ml
	$(OCAMLC) -package session-ocaml.net -c $<

smtp2.cmx: smtp2.ml
	$(OCAMLOPT) -package session-ocaml.net -c $<

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<

# Clean up
clean:
	rm -f *.top *.native *.byte
	rm -f *.cm[ioaxt] *.cmax *.cmti *.o *.annot

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
