HOST:=$(shell $(CC) -dumpmachine)
TARGET=$(HOST)

ifneq ($(TARGET),$(HOST))
PROGRAMPREFIX=$(TARGET)-
else
PROGRAMPREFIX=
endif

OCAMLC=$(PROGRAMPREFIX)ocamlc
OCAMLOPT=$(OCAMLC:c=opt)

OCAMLLIBDIR:=$(shell $(OCAMLC) -where)
include $(OCAMLLIBDIR)/Makefile.config

PREFIX=$(abspath $(OCAMLLIBDIR)/../..)
DESTDIR=
INSTALLDIR=$(addsuffix /,$(DESTDIR))$(PREFIX)/lib/ocaml

BUILDSUFFIX=.noindex
BUILDDIR=$(TARGET)$(BUILDSUFFIX)

all:

########
MLI=unicode.mli
MLSRC=unicode.ml
MLINIT=
CSRC=

LIBNAME=unicode
STUBLIBNAME=

OCAMLCFLAGS=-w +a
CFLAGS=$(OCAMLC_CFLAGS)

CCLIB=
LDFLAGS=

$(BUILDDIR)/unicode.cmo: $(BUILDDIR)/unicode.cmi
$(BUILDDIR)/unicode.cmx: $(BUILDDIR)/unicode.cmi
########

DLLPREFIX=$(patsubst $(addsuffix /,$(DESTDIR))%,%,$(INSTALLDIR))/stublibs
ifeq ($(DLLPREFIX),$(OCAMLLIBDIR)/stublibs)
DLLIB=$(addprefix -dllib -l,$(STUBLIBNAME))
else
DLLIB=$(addprefix -dllib $(DLLPREFIX)/dll,$(addsuffix $(EXT_DLL),$(STUBLIBNAME)))
endif
SLLIB=$(addprefix -cclib -l,$(STUBLIBNAME))

MLCMO=$(addprefix $(BUILDDIR)/,$(addsuffix .cmo,$(basename $(MLSRC) $(MLINIT))))
MLCMX=$(addprefix $(BUILDDIR)/,$(addsuffix .cmx,$(basename $(MLSRC))))
MLOBJ=$(addprefix $(BUILDDIR)/,$(addsuffix .o,$(basename $(MLSRC))))
MLCMI=$(addprefix $(BUILDDIR)/,$(addsuffix .cmi,$(sort $(basename $(MLI) $(MLSRC) $(MLINIT)))))

COBJ=$(addprefix $(BUILDDIR)/,$(addsuffix .o,$(basename $(CSRC))))
CDLL=$(addprefix $(BUILDDIR)/dll,$(addsuffix $(EXT_DLL),$(STUBLIBNAME)))
CSLL=$(addprefix $(BUILDDIR)/lib,$(addsuffix .a,$(STUBLIBNAME)))

ifneq ($(filter $(SHAREDLIB_CFLAGS),$(CFLAGS)),$(SHAREDLIB_CFLAGS))
CFLAGS+=$(SHAREDLIB_CFLAGS)
endif

.PHONY: all clean install uninstall

ifneq ($(OCAMLOPT),)
all: $(BUILDDIR) $(BUILDDIR)/$(LIBNAME).cma $(BUILDDIR)/$(LIBNAME).cmxa
else
all: $(BUILDDIR) $(BUILDDIR)/$(LIBNAME).cma
endif

$(BUILDDIR)/%.cmi: %.mli
	$(OCAMLC) -c -I $(BUILDDIR) $(OCAMLCFLAGS) -o $@ $<

$(BUILDDIR)/%.cmo: %.ml
	$(OCAMLC) -c -I $(BUILDDIR) $(OCAMLCFLAGS) -o $@ $<

$(BUILDDIR)/%.cmx: %.ml
	$(OCAMLOPT) -c -I $(BUILDDIR) $(OCAMLCFLAGS) -o $@ $<

$(BUILDDIR)/%.o: %.c
	$(CC) -c -isystem "$(OCAMLLIBDIR)" $(CFLAGS) -o $@ $<

$(BUILDDIR)/$(LIBNAME).cma: $(MLCMI) $(MLCMO) $(CDLL)
	$(OCAMLC) -a -o $@ $(MLCMO) $(DLLIB) $(CCLIB)

$(BUILDDIR)/$(LIBNAME).cmxa: $(MLCMI) $(MLCMX) $(CSLL)
	$(OCAMLOPT) -a -o $@ $(MLCMX) $(SLLIB) $(CCLIB)

ifneq ($(STUBLIBNAME),)
$(BUILDDIR)/dll$(STUBLIBNAME)$(EXT_DLL): $(COBJ)
	$(MKDLL) -o $@ $< $(LDFLAGS)

ifneq ($(OCAMLOPT),)
$(BUILDDIR)/lib$(STUBLIBNAME).a: $(COBJ)
	$(ARCMD) rcs $@ $<
endif
endif

$(BUILDDIR):
	mkdir -p $@

clean:
	-rm $(BUILDDIR)/$(LIBNAME).cma
	-rm $(BUILDDIR)/$(LIBNAME).a
	-rm $(BUILDDIR)/$(LIBNAME).cmxa
	-rm $(MLCMI) $(MLCMO) $(MLCMX) $(MLOBJ)
ifneq ($(STUBLIBNAME),)
	-rm $(BUILDDIR)/dll$(STUBLIBNAME)$(EXT_DLL)
	-rm $(BUILDDIR)/lib$(STUBLIBNAME).a
	-rm $(COBJ)
endif
	-rmdir $(BUILDDIR)

install: all
	-mkdir -p $(INSTALLDIR)
	cp $(MLCMI) $(INSTALLDIR)
	cp $(BUILDDIR)/$(LIBNAME).cma $(INSTALLDIR)
ifneq ($(STUBLIBNAME),)
	-mkdir $(INSTALLDIR)/stublibs
	cp $(BUILDDIR)/dll$(STUBLIBNAME)$(EXT_DLL) $(INSTALLDIR)/stublibs
endif
ifneq ($(OCAMLOPT),)
	cp $(MLCMX) $(INSTALLDIR)
	cp $(BUILDDIR)/$(LIBNAME).a $(INSTALLDIR)
	cp $(BUILDDIR)/$(LIBNAME).cmxa $(INSTALLDIR)
ifneq ($(STUBLIBNAME),)
	cp $(BUILDDIR)/lib$(STUBLIBNAME).a $(INSTALLDIR)
endif
endif

uninstall:
	-rm $(addprefix $(INSTALLDIR)/,$(notdir $(MLCMI)))
	-rm $(INSTALLDIR)/$(LIBNAME).cma
ifneq ($(STUBLIBNAME),)
	-rm $(INSTALLDIR)/stublibs/dll$(STUBLIBNAME)$(EXT_DLL)
endif
ifneq ($(OCAMLOPT),)
	-rm $(addprefix $(INSTALLDIR)/,$(notdir $(MLCMX)))
	-rm $(INSTALLDIR)/$(LIBNAME).a
	-rm $(INSTALLDIR)/$(LIBNAME).cmxa
ifneq ($(STUBLIBNAME),)
	-rm $(INSTALLDIR)/lib$(STUBLIBNAME).a
endif
endif
