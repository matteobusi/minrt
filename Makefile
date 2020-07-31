TARGET = minrt

BASEDIR = $(PWD)
MCDIR = ../min-caml
IMCDIR = ../incremental-mincaml

BINDIR = $(BASEDIR)/bin
TMPDIR = $(BASEDIR)/tmp

imc:
	cd $(IMCDIR); make clean; make default; cd $(BASEDIR);
	cp $(IMCDIR)/main.native $(BINDIR)/imc

imc-rt: imc
	$(BINDIR)/imc $(TARGET).ml $(TARGET).ml

imc-rt-last: imc
	# Recover the previous version of the file
	git show HEAD~2:minrt.ml > $(TMPDIR)/$(TARGET).old.ml

	# Now compare the two versions
	$(BINDIR)/imc $(TARGET).ml $(TMPDIR)/$(TARGET).old.ml

clean:
	rm -f $(TMPDIR)/* $(BINDIR)/*

# The following four are only for testing purposes and/or porting a project from MinCaml to its explicitly typed version:
#
#     - mc, compiles the modified version of MinCaml compiler
#     - typed-rt, produces an explicitly typed version of the given MinCaml program
#     - format-rt, produces a formatted version of the given MinCaml program
#     - ocaml-rt, compiles the raytracer with the default ocaml compiler
#     - ocaml-rt-display, renders the shuttle.sld model and shows it (requires imagemagic)
mc:
	cd $(MCDIR); make clean; make min-caml; cd $(BASEDIR);
	cp $(MCDIR)/min-caml $(BINDIR)/mc

typed-rt: mc
	cp $(TARGET).ml $(TMPDIR)/$(TARGET).ml;
	$(BINDIR)/mc -tprint $(TMPDIR)/$(TARGET);

##### THIS WON'T COMPILE OOTB:
##### OCamlFormat removes necessary paratheses around product types in function declarations!
format-rt:
	ocamlformat $(TARGET).ml > $(TMPDIR)/$(TARGET).f.ml

ocaml-rt:
	# Add the runtime needed by the ocaml compiler and compile it
	cat miniMLRuntime.ml $(TARGET).ml > $(TMPDIR)/$(TARGET).o.ml;
	ocamlc -o $(BINDIR)/$(TARGET) $(TMPDIR)/$(TARGET).o.ml

ocaml-rt-display: ocaml-rt
	$(BINDIR)/$(TARGET) < models/shuttle.sld > $(TMPDIR)/res.ppm;
	display $(TMPDIR)/res.ppm
