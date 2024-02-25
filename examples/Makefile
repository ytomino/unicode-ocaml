BUILD:=$(shell $(CC) -dumpmachine)
HOST=$(BUILD)
TARGET=$(HOST)

PROGRAMPREFIX=$(addsuffix -,$(filter-out $(BUILD),$(TARGET)))

OCAMLC=$(PROGRAMPREFIX)ocamlc
OCAMLOPT=$(or $(filter-out $(OCAMLC),$(OCAMLC:c=opt)),$(PROGRAMPREFIX)ocamlopt)
RLWRAP?=

DEBUG=

OCAMLCFLAGS=$(and $(filter-out 0,$(DEBUG)),-g)
OCAMLOPTFLAGS=$(OCAMLCFLAGS)
OCAMLCFLAGS_EXE=$(and $(filter-out 0,$(DEBUG)),-g)
OCAMLOPTFLAGS_EXE=$(OCAMLCFLAGS_EXE) $(addprefix -ccopt ,$(LDFLAGS))
LDFLAGS?=

BUILDSUFFIX=.noindex
BUILDDIR=$(TARGET)$(BUILDSUFFIX)
UNICODE_BUILDDIR=$(BUILDDIR)/unicode$(BUILDSUFFIX)

BINLN=$(and $(filter $(BUILD),$(TARGET)), \
        $(if $(findstring mingw,$(BUILD))$(findstring msys,$(BUILD)),,bin))

EXAMPLES=$(basename $(wildcard *.ml))
TESTS=$(filter test_%,$(EXAMPLES))

.PHONY: all check interacitve clean $(TESTS)

all: \
	$(and $(OCAMLC),$(patsubst %,$(BUILDDIR)/%.byte.exe,$(EXAMPLES))) \
	$(and $(OCAMLOPT),$(patsubst %,$(BUILDDIR)/%.opt.exe,$(EXAMPLES))) \
	$(BINLN)

$(BUILDDIR)/%.byte.exe: %.ml $(BUILDDIR)/unicode.cma
	$(OCAMLC) -c $(OCAMLCFLAGS) -o $(BUILDDIR)/$*.cmo -I $(BUILDDIR) $<
	$(OCAMLC) $(strip \
		$(OCAMLCFLAGS_EXE) \
		-o $@ -I $(BUILDDIR) unicode.cma $(BUILDDIR)/$*.cmo)

$(BUILDDIR)/%.opt.exe: %.ml $(BUILDDIR)/unicode.cmxa
	$(OCAMLOPT) -c $(OCAMLOPTFLAGS) -o $(BUILDDIR)/$*.cmx -I $(BUILDDIR) $<
	$(OCAMLOPT) $(strip \
		$(OCAMLOPTFLAGS_EXE) \
		-o $@ -I $(BUILDDIR) unicode.cmxa $(BUILDDIR)/$*.cmx)

$(BUILDDIR)/test_sig.%.exe: private override OCAMLCFLAGS+=-w -3

check: all $(TESTS)

$(TESTS): %: \
		$(and $(OCAMLC),$(BUILDDIR)/%.byte.exe) \
		$(and $(OCAMLOPT),$(BUILDDIR)/%.opt.exe)
	$(and $(OCAMLC),$<)
	$(and $(OCAMLOPT),$(BUILDDIR)/$*.opt.exe)

interactive: $(BUILDDIR)/unicode.cma
	$(RLWRAP) ocaml $(OCAMLCFLAGS) -I $(BUILDDIR) unicode.cma

$(and $(OCAMLC),$(BUILDDIR)/unicode.cma) \
$(and $(OCAMLOPT),$(BUILDDIR)/unicode.cmxa)&: \
		../source/unicode*
	$(MAKE) -C $(strip \
		../source install \
		BUILDDIR=$(abspath $(UNICODE_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR)))

$(BINLN):
	ln -s $(BUILDDIR) $@

clean:
	-$(and $(BINLN),[ -h "$(BINLN)" ] && rm "$(BINLN)")
	-$(MAKE) $(strip \
		-C ../source clean uninstall \
		BUILDDIR=$(abspath $(UNICODE_BUILDDIR)) \
		INSTALLDIR=$(abspath $(BUILDDIR)))
	-rm $(BUILDDIR)/*.exe $(BUILDDIR)/*.cm? $(BUILDDIR)/*.o
	-rmdir $(BUILDDIR)
