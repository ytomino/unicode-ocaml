HOST:=$(shell gcc -dumpmachine)
TARGET=$(HOST)

ifeq ($(TARGET),$(HOST))
BINLN=bin
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

ifneq ($(ComSpec),)
RM=del
LINK=linkd $2 $1
UNLINK=linkd $1 /d
DIRSEP=\$(empty)
else
RM=rm
LINK=ln -s $1 $2
UNLINK=rm $1
DIRSEP=/
endif

BUILDDIR=$(TARGET).build

export LD_LIBRARY_PATH=.
export CAML_LD_LIBRARY_PATH=$(BUILDDIR)/stublibs

.PHONY: test interacitve clean

test: $(BUILDDIR)/unicode.cma $(BINLN)
	$(OCAMLC) -c -o $(BUILDDIR)/test.cmo -I $(BUILDDIR) $(OCAMLCFLAGS) test.ml
	$(OCAMLC) -o $(BUILDDIR)/test.byte.exe -I $(BUILDDIR) bigarray.cma unicode.cma $(BUILDDIR)/test.cmo
	$(BUILDDIR)$(DIRSEP)test.byte.exe
ifneq ($(OCAMLOPT),)
	$(OCAMLOPT) -c -o $(BUILDDIR)/test.cmx -I $(BUILDDIR) $(OCAMLCFLAGS) test.ml
	$(OCAMLOPT) -o $(BUILDDIR)/test.opt.exe -I $(BUILDDIR) bigarray.cmxa unicode.cmxa $(BUILDDIR)/test.cmx
	$(BUILDDIR)$(DIRSEP)test.opt.exe
endif

interactive: $(BUILDDIR)/unicode.cma
	$(RLWRAP) ocaml -I $(BUILDDIR) bigarray.cma unicode.cma

$(BUILDDIR)/unicode.cma: $(wildcard ../source/*.ml*)
	$(MAKE) -C ..$(DIRSEP)source install DESTDIR=$(abspath $(BUILDDIR))

$(BINLN):
	-$(call LINK,$(BUILDDIR),$(BINLN))

clean:
	$(MAKE) -C ..$(DIRSEP)source clean uninstall DESTDIR=$(abspath $(BUILDDIR))
	-$(RM) $(BUILDDIR)$(DIRSEP)test.*
	-$(call UNLINK,$(BINLN))
	-rmdir $(BUILDDIR)
