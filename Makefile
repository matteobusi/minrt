TARGET = minrt

BASEDIR = $(PWD)
MCDIR = ../min-caml
IMCDIR = ../incremental-mincaml

BINDIR = $(BASEDIR)/bin
TMPDIR = $(BASEDIR)/tmp

mc:
	cd $(MCDIR); make clean; make min-caml; cd $(BASEDIR);
	cp $(MCDIR)/min-caml $(BINDIR)/mc

imc:
	cd $(IMCDIR); make clean; make default; cd $(BASEDIR);
	cp $(IMCDIR)/main.native $(BINDIR)/imc

typed-rt: mc
	cp $(TARGET).ml $(TMPDIR)/$(TARGET).ml;
	$(BINDIR)/mc -tprint $(TMPDIR)/$(TARGET);

ocaml-rt: typed-rt
	# Add the runtime needed by the ocaml compiler and compile it
	cat miniMLRuntime.ml $(TMPDIR)/$(TARGET).ml > $(TMPDIR)/$(TARGET).o.ml;
	ocamlc -o $(BINDIR)/$(TARGET) $(TMPDIR)/$(TARGET).o.ml

ocaml-rt-test: ocaml-rt
	$(BINDIR)/$(TARGET) < models/shuttle.sld > $(TMPDIR)/res.ppm;
	display $(TMPDIR)/res.ppm

imc-rt: imc typed-rt
	$(BINDIR)/imc $(TMPDIR)/$(TARGET).t.ml $(TMPDIR)/$(TARGET).t.ml

clean:
	rm $(TMPDIR)/* $(BINDIR)/*
