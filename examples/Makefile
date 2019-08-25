HOST:=$(shell $(CC) -dumpmachine)
TARGET=$(HOST)

ifeq ($(TARGET),$(HOST))
ifneq ($(findstring mingw,$(HOST))$(findstring msys,$(HOST)),)
BINLN=
else
BINLN=bin
endif
ifneq ($(shell rlwrap --version),)
RLWRAP=rlwrap
endif
else
BINLN=
endif

ifeq ($(OCAMLC),gcamlc)
OCAMLOPT=
else
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
endif

empty=

BUILDDIR=$(TARGET).build

.PHONY: test interacitve clean

test: $(BUILDDIR)/unicode.cma $(BINLN)
	$(OCAMLC) -c -o $(BUILDDIR)/test.cmo -I $(BUILDDIR) $(OCAMLCFLAGS) test.ml
	$(OCAMLC) -o $(BUILDDIR)/test.byte.exe -I $(BUILDDIR) bigarray.cma unicode.cma $(BUILDDIR)/test.cmo
	$(BUILDDIR)/test.byte.exe
ifneq ($(OCAMLOPT),)
	$(OCAMLOPT) -c -o $(BUILDDIR)/test.cmx -I $(BUILDDIR) $(OCAMLCFLAGS) test.ml
	$(OCAMLOPT) -o $(BUILDDIR)/test.opt.exe -I $(BUILDDIR) bigarray.cmxa unicode.cmxa $(BUILDDIR)/test.cmx
	$(BUILDDIR)/test.opt.exe
endif

interactive: $(BUILDDIR)/unicode.cma
	$(RLWRAP) ocaml -I $(BUILDDIR) bigarray.cma unicode.cma

$(BUILDDIR)/unicode.cma: $(wildcard ../source/*.ml*)
	$(MAKE) -C ../source install INSTALLDIR=$(abspath $(BUILDDIR))

$(BINLN):
	-ln -s $(BUILDDIR) $@

clean:
	-$(MAKE) -C ../source clean uninstall INSTALLDIR=$(abspath $(BUILDDIR))
	-rm $(BUILDDIR)/test.*
	-rm $(BINLN)
	-rmdir $(BUILDDIR)
