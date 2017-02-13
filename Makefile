OCAMLC=ocamlfind ocamlc -rectypes -I examples/
OCAMLOPT=ocamlfind ocamlopt -rectypes -I examples/
OCAMLMKTOP=ocamlfind ocamlmktop
OCAMLDEP=ocamlfind ocamldep
OCAMLPKGFLAGS=-thread -package unix,threads

BYTE_OBJS=monitor.cmo channel.cmo session.cmo
CMI=$(BYTE_OBJS:%.cmo=%.cmi)
NATIVE_OBJS=$(BYTE_OBJS:%.cmo=%.cmx)

all: example.byte

session.cma: $(BYTE_OBJS) $(CMI)
	$(OCAMLC) -linkpkg -a -o $@ $(BYTE_OBJS) 

session.cmxa: $(NATIVE_OBJS) $(CMI)
	$(OCAMLOPT) -linkpkg -a -o $@ $(NATIVE_OBJS) 

session.top: $(BYTE_OBJS) $(CMI)
	$(OCAMLMKTOP) -linkpkg -o $@ $(OCAMLPKGFLAGS) $(BYTE_OBJS)

example.byte: session.cma examples/ex_single1.cmo examples/ex_single2.cmo examples/ex_multi1.cmo
	$(OCAMLC) -linkpkg -o $@ $(OCAMLPKGFLAGS) session.cma examples/ex_single1.cmo examples/ex_single2.cmo examples/ex_multi1.cmo

travel_agency.byte: session.cma examples/travel_agency.cmo
	$(OCAMLC) -linkpkg -o $@ $(OCAMLPKGFLAGS) session.cma examples/travel_agency.cmo

test.byte: session.cma tests/test.cmo $(CMI)
	$(OCAMLC) -linkpkg -o $@ $(OCAMLPKGFLAGS) $(BYTE_OBJS) tests/test.cmo

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLPKGFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLPKGFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLPKGFLAGS) -c $<

# Clean up
clean:
	rm -f *.top *.native *.byte
	rm -f *.cm[ioaxt] *.cmax *.cmti *.o *.annot
	rm -f examples/*.cm[ioaxt] examples/*.o examples/*.annot

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend
	$(OCAMLDEP) $(INCLUDES) examples/*.ml >> .depend

include .depend

