ifeq ($(OCAMLC),gcamlc)
OCAMLOPT=
else
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
endif

empty=

ifneq ($(ComSpec),)
RM=del
DIRSEP=\$(empty)
else
RM=rm
DIRSEP=/
endif

BUILDDIR=build

export LD_LIBRARY_PATH=.
export CAML_LD_LIBRARY_PATH=$(BUILDDIR)/stublibs

.PHONY: test clean

test: $(BUILDDIR)/unicode.cma
	$(OCAMLC) -c -o $(BUILDDIR)/test.cmo -I $(BUILDDIR) test.ml
	$(OCAMLC) -o $(BUILDDIR)/test.byte.exe -I $(BUILDDIR) bigarray.cma unicode.cma $(BUILDDIR)/test.cmo
	$(BUILDDIR)$(DIRSEP)test.byte.exe
ifneq ($(OCAMLOPT),)
	$(OCAMLOPT) -c -o $(BUILDDIR)/test.cmx -I $(BUILDDIR) test.ml
	$(OCAMLOPT) -o $(BUILDDIR)/test.opt.exe -I $(BUILDDIR) bigarray.cmxa unicode.cmxa $(BUILDDIR)/test.cmx
	$(BUILDDIR)$(DIRSEP)test.opt.exe
endif

$(BUILDDIR)/unicode.cma: $(wildcard ../source/*.ml*)
	make -C ..$(DIRSEP)source install DESTDIR=$(abspath $(BUILDDIR))

clean:
	make -C ..$(DIRSEP)source clean uninstall DESTDIR=$(abspath $(BUILDDIR))
	-rm $(BUILDDIR)$(DIRSEP)test.*
	-rmdir $(BUILDDIR)
