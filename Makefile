OCAMLC=ocamlfind ocamlc -rectypes -thread -package unix,threads
OCAMLOPT=ocamlfind ocamlopt -rectypes -thread -package unix,threads
OCAMLMKTOP=ocamlfind ocamlmktop -rectypes -thread -package unix,threads
OCAMLDEP=ocamlfind ocamldep
INCLUDES=                 # all relevant -I options here
OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

BYTE_OBJS=monitor.cmo channel.cmo session.cmo
CMI=$(BYTE_OBJS:%.cmo=%.cmi)
NATIVE_OBJS=$(BYTE_OBJS:%.cmo=%.cmx)

all: example.byte

session.top: $(BYTE_OBJS) $(CMI)
	$(OCAMLMKTOP) -linkpkg -o $@ $(OCAMLFLAGS) $(BYTE_OBJS)

example.native: $(NATIVE_OBJS) example.cmx $(CMI)
	$(OCAMLOPT) -linkpkg -o $@ $(OCAMLFLAGS) $(NATIVE_OBJS) example.cmx

example.byte: $(BYTE_OBJS) example.cmo $(CMI)
	$(OCAMLC) -linkpkg -o $@ $(OCAMLFLAGS) $(BYTE_OBJS) example.cmo

test.native: $(NATIVE_OBJS) test.cmx $(CMI)
	$(OCAMLOPT) -linkpkg -o $@ $(OCAMLFLAGS) $(NATIVE_OBJS) test.cmx

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
	rm -f test.byte test.native
	rm -f *.cm[ioaxt] *.cmax *.cmti *.o *.annot

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend

