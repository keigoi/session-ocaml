OCAMLC=ocamlfind ocamlc -thread -package session-ocaml -rectypes
OCAMLOPT=ocamlfind ocamlc -thread -package session-ocaml -rectypes
OCAMLMKTOP=ocamlfind ocamlmktop -thread -package session-ocaml -rectypes
OCAMLDEP=ocamlfind ocamldep -package session-ocaml

EXAMPLES=ex_single1 ex_single2 ex_multi1 travel_agency

EXAMPLES_BYTE = $(EXAMPLES:%=%.byte)


all: $(EXAMPLES_BYTE)

ex_single2.byte: ex_single1.cmo ex_single2.cmo
	$(OCAMLC) -linkpkg -o $@ $<

ex_multi1.byte: ex_single1.cmo ex_single2.cmo ex_multi1.cmo
	$(OCAMLC) -linkpkg -o $@ $<

%.byte: %.cmo
	$(OCAMLC) -linkpkg -o $@ $<


# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

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

