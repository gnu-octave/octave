EXTRA_DIST += \
  parse-tree/module.mk \
  parse-tree/octave.gperf

PARSER_INC = \
  parse-tree/lex.h \
  parse-tree/parse.h \
  parse-tree/parse-private.h

PARSER_SRC = \
  parse-tree/lex.ll \
  parse-tree/oct-parse.yy

## FIXME: Automake does not support per-object rules.
##        These rules could be emulated by creating a new convenience
##        library and using per-library rules.  Or we can just live
##        with the extra warnings about old-sytle-casts. (09/18/2012)
#lex.lo lex.o oct-parse.lo oct-parse.o: \
#  AM_CXXFLAGS := $(filter-out -Wold-style-cast, $(AM_CXXFLAGS))

PARSE_TREE_INC = \
  parse-tree/pt-all.h \
  parse-tree/pt-arg-list.h \
  parse-tree/pt-assign.h \
  parse-tree/pt-binop.h \
  parse-tree/pt-bp.h \
  parse-tree/pt-cbinop.h \
  parse-tree/pt-cell.h \
  parse-tree/pt-check.h \
  parse-tree/pt-classdef.h \
  parse-tree/pt-cmd.h \
  parse-tree/pt-colon.h \
  parse-tree/pt-const.h \
  parse-tree/pt-decl.h \
  parse-tree/pt-eval.h \
  parse-tree/pt-except.h \
  parse-tree/pt-exp.h \
  parse-tree/pt-fcn-handle.h \
  parse-tree/pt-funcall.h \
  parse-tree/pt-id.h \
  parse-tree/pt-idx.h \
  parse-tree/pt-jump.h \
  parse-tree/pt-loop.h \
  parse-tree/pt-mat.h \
  parse-tree/pt-misc.h \
  parse-tree/pt-pr-code.h \
  parse-tree/pt-select.h \
  parse-tree/pt-stmt.h \
  parse-tree/pt-unop.h \
  parse-tree/pt-walk.h \
  parse-tree/pt.h \
  parse-tree/token.h \
  $(PARSER_INC)

PARSE_TREE_SRC = \
  parse-tree/pt-arg-list.cc \
  parse-tree/pt-assign.cc \
  parse-tree/pt-binop.cc \
  parse-tree/pt-bp.cc \
  parse-tree/pt-cbinop.cc \
  parse-tree/pt-cell.cc \
  parse-tree/pt-check.cc \
  parse-tree/pt-classdef.cc \
  parse-tree/pt-cmd.cc \
  parse-tree/pt-colon.cc \
  parse-tree/pt-const.cc \
  parse-tree/pt-decl.cc \
  parse-tree/pt-eval.cc \
  parse-tree/pt-except.cc \
  parse-tree/pt-exp.cc \
  parse-tree/pt-fcn-handle.cc \
  parse-tree/pt-funcall.cc \
  parse-tree/pt-id.cc \
  parse-tree/pt-idx.cc \
  parse-tree/pt-jump.cc \
  parse-tree/pt-loop.cc \
  parse-tree/pt-mat.cc \
  parse-tree/pt-misc.cc \
  parse-tree/pt-pr-code.cc \
  parse-tree/pt-select.cc \
  parse-tree/pt-stmt.cc \
  parse-tree/pt-unop.cc \
  parse-tree/pt.cc \
  parse-tree/token.cc \
  $(PARSER_SRC)

## Special rules for sources which must be built before rest of compilation.

## Don't use a pipeline to process gperf output since if gperf
## is missing but sed is not, the exit status of the pipeline
## will still be success and we will end up creating an empty
## oct-gperf.h file.
parse-tree/oct-gperf.h: parse-tree/octave.gperf
	$(GPERF) -t -C -D -G -L C++ -Z octave_kw_hash $< > $@-t1
	$(SED) 's,lookup\[,gperf_lookup[,' < $@-t1 > $@-t
	mv $@-t $@
	rm -f $@-t1

noinst_LTLIBRARIES += parse-tree/libparse-tree.la

parse_tree_libparse_tree_la_SOURCES = $(PARSE_TREE_SRC)
parse_tree_libparse_tree_la_CPPFLAGS = $(liboctinterp_la_CPPFLAGS)
