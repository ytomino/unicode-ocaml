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

.PHONY: all interacitve clean

all: $(BUILDDIR)/test_conv.byte.exe $(BINLN)

$(BUILDDIR)/%.byte.exe: %.ml $(BUILDDIR)/unicode.cma
	$(OCAMLC) -c -o $(BUILDDIR)/$*.cmo -I $(BUILDDIR) $(OCAMLCFLAGS) $<
	$(OCAMLC) -o $(BUILDDIR)/$*.byte.exe -I $(BUILDDIR) bigarray.cma unicode.cma $(BUILDDIR)/$*.cmo
	$(BUILDDIR)/$*.byte.exe
ifneq ($(OCAMLOPT),)
	$(OCAMLOPT) -c -o $(BUILDDIR)/$*.cmx -I $(BUILDDIR) $(OCAMLCFLAGS) $*.ml
	$(OCAMLOPT) -o $(BUILDDIR)/$*.opt.exe -I $(BUILDDIR) bigarray.cmxa unicode.cmxa $(BUILDDIR)/$*.cmx
	$(BUILDDIR)/$*.opt.exe
endif

interactive: $(BUILDDIR)/unicode.cma
	$(RLWRAP) ocaml -I $(BUILDDIR) bigarray.cma unicode.cma

$(BUILDDIR)/unicode.cma: $(wildcard ../source/*.ml*)
	$(MAKE) -C ../source install INSTALLDIR=$(abspath $(BUILDDIR))

$(BINLN):
	-ln -s $(BUILDDIR) $@

clean:
	-$(MAKE) -C ../source clean uninstall INSTALLDIR=$(abspath $(BUILDDIR))
	-rm $(BUILDDIR)/*.exe $(BUILDDIR)/*.cm? $(BUILDDIR)/*.o
	-rm $(BINLN)
	-rmdir $(BUILDDIR)
