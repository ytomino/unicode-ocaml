HOST:=$(shell $(CC) -dumpmachine)
TARGET=$(HOST)

ifneq ($(TARGET),$(HOST))
PROGRAMPREFIX=$(TARGET)-
BINLN=
else
PROGRAMPREFIX=
ifneq ($(findstring mingw,$(HOST))$(findstring msys,$(HOST)),)
BINLN=
else
BINLN=bin
endif
ifneq ($(shell rlwrap --version),)
RLWRAP=rlwrap
endif
endif

OCAMLC=$(PROGRAMPREFIX)ocamlc
OCAMLOPT=$(OCAMLC:c=opt)

BUILDSUFFIX=.noindex
BUILDDIR=$(TARGET)$(BUILDSUFFIX)
UNICODE_BUILDDIR=$(BUILDDIR)/unicode$(BUILDSUFFIX)

EXAMPLES=$(basename $(wildcard *.ml))
TESTS=$(filter test_%,$(EXAMPLES))

.PHONY: all check interacitve clean $(TESTS)

all: \
	$(patsubst %,$(BUILDDIR)/%.byte.exe,$(EXAMPLES)) \
	$(if $(OCAMLOPT),$(patsubst %,$(BUILDDIR)/%.opt.exe,$(EXAMPLES))) \
	$(BINLN)

$(BUILDDIR)/%.byte.exe: %.ml $(BUILDDIR)/unicode.cma
	$(OCAMLC) -c -o $(BUILDDIR)/$*.cmo -I $(BUILDDIR) $(OCAMLCFLAGS) $<
	$(OCAMLC) -o $@ -I $(BUILDDIR) bigarray.cma unicode.cma $(BUILDDIR)/$*.cmo

ifneq ($(OCAMLOPT),)
$(BUILDDIR)/%.opt.exe: %.ml $(BUILDDIR)/unicode.cmxa
	$(OCAMLOPT) -c -o $(BUILDDIR)/$*.cmx -I $(BUILDDIR) $(OCAMLCFLAGS) $<
	$(OCAMLOPT) -o $@ -I $(BUILDDIR) bigarray.cmxa unicode.cmxa $(BUILDDIR)/$*.cmx
endif

$(BUILDDIR)/test_sig.%.exe: OCAMLCFLAGS+=-w -3

check: all $(TESTS)

$(TESTS): %: $(BUILDDIR)/%.byte.exe $(if $(OCAMLOPT),$(BUILDDIR)/%.opt.exe)
	$<
ifneq ($(OCAMLOPT),)
	$(patsubst %.byte.exe,%.opt.exe,$<)
endif

interactive: $(BUILDDIR)/unicode.cma
	$(RLWRAP) ocaml -I $(BUILDDIR) $(OCAMLCFLAGS) bigarray.cma unicode.cma

$(BUILDDIR)/unicode.cma $(if $(OCAMLOPT),$(BUILDDIR)/unicode.cmxa): $(wildcard ../source/unicode*)
	$(MAKE) -C ../source install \
		BUILDDIR=$(abspath $(UNICODE_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR))

$(BINLN):
	ln -s $(BUILDDIR) $@

clean:
	-$(MAKE) -C ../source clean uninstall \
		BUILDDIR=$(abspath $(UNICODE_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR))
	-rm $(BUILDDIR)/*.exe $(BUILDDIR)/*.cm? $(BUILDDIR)/*.o
	-[ -h "$(BINLN)" ] && rm "$(BINLN)"
	-rmdir $(BUILDDIR)
