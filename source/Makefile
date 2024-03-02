include Makefile.variables

MLI=unicode.mli
MLSRC=unicode.ml
MLINIT=
CSRC=

LIBNAME=unicode
STUBLIBNAME=

DEBUG=
ASSERT=

OCAMLCFLAGS=$(and $(filter-out 0,$(DEBUG)),-g) $(if $(ASSERT),,-noassert) -w +a
OCAMLOPTFLAGS=$(OCAMLCFLAGS)
CFLAGS=

CCLIB=
LDFLAGS=
LDLIB=

include Makefile.rules
