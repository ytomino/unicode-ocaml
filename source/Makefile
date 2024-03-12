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
OCAML_INCLUDE_FLAGS=
CFLAGS=
C_INCLUDE_FLAGS=

CCLIB=
LDLIB=

include Makefile.rules
