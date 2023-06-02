BUILD:=$(shell $(CC) -dumpmachine)
HOST=$(BUILD)
TARGET=$(HOST)

ifneq ($(TARGET),$(BUILD))
 PROGRAMPREFIX=$(TARGET)-
 BINLN=
else
 PROGRAMPREFIX=
 ifneq ($(findstring mingw,$(BUILD))$(findstring msys,$(BUILD)),)
  BINLN=
 else
  BINLN=bin
 endif
endif

OCAMLC=$(PROGRAMPREFIX)ocamlc
OCAMLOPT=$(if $(OCAMLC),$(OCAMLC:c=opt),$(PROGRAMPREFIX)ocamlopt)
RLWRAP?=

OCAMLCFLAGS=
OCAMLOPTFLAGS=$(OCAMLCFLAGS)

BUILDSUFFIX=.noindex
BUILDDIR=$(TARGET)$(BUILDSUFFIX)
UNICODE_BUILDDIR=$(BUILDDIR)/unicode$(BUILDSUFFIX)

EXAMPLES=$(basename $(wildcard *.ml))
TESTS=$(filter test_%,$(EXAMPLES))

.PHONY: all check interacitve clean $(TESTS)

all: \
	$(if $(OCAMLC),$(patsubst %,$(BUILDDIR)/%.byte.exe,$(EXAMPLES))) \
	$(if $(OCAMLOPT),$(patsubst %,$(BUILDDIR)/%.opt.exe,$(EXAMPLES))) \
	$(BINLN)

$(BUILDDIR)/%.byte.exe: %.ml $(BUILDDIR)/unicode.cma
	$(OCAMLC) -c -o $(BUILDDIR)/$*.cmo -I $(BUILDDIR) $(OCAMLCFLAGS) $<
	$(OCAMLC) -o $@ -I $(BUILDDIR) bigarray.cma unicode.cma $(BUILDDIR)/$*.cmo

$(BUILDDIR)/%.opt.exe: %.ml $(BUILDDIR)/unicode.cmxa
	$(OCAMLOPT) -c -o $(BUILDDIR)/$*.cmx -I $(BUILDDIR) $(OCAMLOPTFLAGS) $<
	$(OCAMLOPT) -o $@ -I $(BUILDDIR) bigarray.cmxa unicode.cmxa $(BUILDDIR)/$*.cmx

$(BUILDDIR)/test_sig.%.exe: private override OCAMLCFLAGS+=-w -3

check: all $(TESTS)

$(TESTS): %: \
		$(if $(OCAMLC),$(BUILDDIR)/%.byte.exe) $(if $(OCAMLOPT),$(BUILDDIR)/%.opt.exe)
ifneq ($(OCAMLC),)
	$<
endif
ifneq ($(OCAMLOPT),)
	$(BUILDDIR)/$*.opt.exe
endif

interactive: $(BUILDDIR)/unicode.cma
	$(RLWRAP) ocaml -I $(BUILDDIR) $(OCAMLCFLAGS) bigarray.cma unicode.cma

$(if $(OCAMLC),$(BUILDDIR)/unicode.cma) \
$(if $(OCAMLOPT),$(BUILDDIR)/unicode.cmxa)&: \
		../source/unicode*
	$(MAKE) -C ../source install \
		BUILDDIR=$(abspath $(UNICODE_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR))

$(BINLN):
	ln -s $(BUILDDIR) $@

clean:
	-$(if $(BINLN),[ -h "$(BINLN)" ] && rm "$(BINLN)")
	-$(MAKE) -C ../source clean uninstall \
		BUILDDIR=$(abspath $(UNICODE_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR))
	-rm $(BUILDDIR)/*.exe $(BUILDDIR)/*.cm? $(BUILDDIR)/*.o
	-rmdir $(BUILDDIR)
