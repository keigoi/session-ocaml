OCAMLC=ocamlfind ocamlc -rectypes -thread -package unix,threads -I examples/
OCAMLOPT=ocamlfind ocamlopt -rectypes -thread -package unix,threads -I examples/
OCAMLMKTOP=ocamlfind ocamlmktop -rectypes -thread -package unix,threads
OCAMLDEP=ocamlfind ocamldep
INCLUDES=                 # all relevant -I options here
OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

BYTE_OBJS=monitor.cmo channel.cmo session.cmo
CMI=$(BYTE_OBJS:%.cmo=%.cmi)
NATIVE_OBJS=$(BYTE_OBJS:%.cmo=%.cmx)

all: example.byte

session.cma: $(BYTE_OBJS) $(CMI)
	$(OCAMLC) -linkpkg -a -o $@ $(OCAMLFLAGS) $(BYTE_OBJS) 

session.cmxa: $(BYTE_OBJS) $(CMI)
	$(OCAMLC) -linkpkg -a -o $@ $(OCAMLFLAGS) $(NATIVE_OBJS) 

session.top: $(BYTE_OBJS) $(CMI)
	$(OCAMLMKTOP) -linkpkg -o $@ $(OCAMLFLAGS) $(BYTE_OBJS)

example.byte: $(BYTE_OBJS) $(CMI) examples/ex_single1.cmo examples/ex_single2.cmo examples/ex_multi1.cmo
	$(OCAMLC) -linkpkg -o $@ $(OCAMLFLAGS) $(BYTE_OBJS) examples/ex_single1.cmo examples/ex_single2.cmo examples/ex_multi1.cmo

test.byte: $(BYTE_OBJS) test.cmo $(CMI)
	$(OCAMLC) -linkpkg -o $@ $(OCAMLFLAGS) $(BYTE_OBJS) test.cmo

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

# Clean up
clean:
	rm -f *.top *.native *.byte
	rm -f *.cm[ioaxt] *.cmax *.cmti *.o *.annot
	rm -f examples/*.cm[ioaxt] examples/*.o examples/*.annot

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend

