OCAMLC=ocamlfind ocamlc -rectypes -thread -package unix,threads
OCAMLOPT=ocamlfind ocamlopt -rectypes -thread -package unix,threads
OCAMLDEP=ocamlfind ocamldep
INCLUDES=                 # all relevant -I options here
OCAMLFLAGS=$(INCLUDES)    # add other options for ocamlc here
OCAMLOPTFLAGS=$(INCLUDES) # add other options for ocamlopt here

CMI=monitor.cmi mVar.cmi channel.cmi
BYTE_OBJS=monitor.cmo mVar.cmo channel.cmo session.cmo
NATIVE_OBJS=$(BYTE_OBJS:%.cmo=%.cmx)

test.native: $(NATIVE_OBJS) $(CMI)
	$(OCAMLOPT) -linkpkg -o test.native $(OCAMLFLAGS) $(NATIVE_OBJS)

test.byte: $(BYTE_OBJS) $(CMI)
	$(OCAMLC) -linkpkg -o test.byte $(OCAMLFLAGS) $(BYTE_OBJS)

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
	rm -f *.cm[iox] *.o

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend

