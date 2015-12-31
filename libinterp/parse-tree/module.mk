PARSE_TREE_INC = \
  libinterp/parse-tree/lex.h \
  libinterp/parse-tree/parse.h \
  libinterp/parse-tree/pt-all.h \
  libinterp/parse-tree/pt-arg-list.h \
  libinterp/parse-tree/pt-array-list.h \
  libinterp/parse-tree/pt-assign.h \
  libinterp/parse-tree/pt-binop.h \
  libinterp/parse-tree/pt-bp.h \
  libinterp/parse-tree/pt-cbinop.h \
  libinterp/parse-tree/pt-cell.h \
  libinterp/parse-tree/pt-check.h \
  libinterp/parse-tree/pt-classdef.h \
  libinterp/parse-tree/pt-cmd.h \
  libinterp/parse-tree/pt-colon.h \
  libinterp/parse-tree/pt-const.h \
  libinterp/parse-tree/pt-decl.h \
  libinterp/parse-tree/pt-eval.h \
  libinterp/parse-tree/pt-except.h \
  libinterp/parse-tree/pt-exp.h \
  libinterp/parse-tree/pt-fcn-handle.h \
  libinterp/parse-tree/pt-funcall.h \
  libinterp/parse-tree/pt-id.h \
  libinterp/parse-tree/pt-idx.h \
  libinterp/parse-tree/pt-jump.h \
  libinterp/parse-tree/pt-loop.h \
  libinterp/parse-tree/pt-mat.h \
  libinterp/parse-tree/pt-misc.h \
  libinterp/parse-tree/pt-pr-code.h \
  libinterp/parse-tree/pt-select.h \
  libinterp/parse-tree/pt-stmt.h \
  libinterp/parse-tree/pt-unop.h \
  libinterp/parse-tree/pt-walk.h \
  libinterp/parse-tree/pt.h \
  libinterp/parse-tree/token.h

PARSE_TREE_SRC = \
  libinterp/parse-tree/lex.ll \
  libinterp/parse-tree/oct-parse.yy \
  libinterp/parse-tree/pt-arg-list.cc \
  libinterp/parse-tree/pt-array-list.cc \
  libinterp/parse-tree/pt-assign.cc \
  libinterp/parse-tree/pt-binop.cc \
  libinterp/parse-tree/pt-bp.cc \
  libinterp/parse-tree/pt-cbinop.cc \
  libinterp/parse-tree/pt-cell.cc \
  libinterp/parse-tree/pt-check.cc \
  libinterp/parse-tree/pt-classdef.cc \
  libinterp/parse-tree/pt-cmd.cc \
  libinterp/parse-tree/pt-colon.cc \
  libinterp/parse-tree/pt-const.cc \
  libinterp/parse-tree/pt-decl.cc \
  libinterp/parse-tree/pt-eval.cc \
  libinterp/parse-tree/pt-except.cc \
  libinterp/parse-tree/pt-exp.cc \
  libinterp/parse-tree/pt-fcn-handle.cc \
  libinterp/parse-tree/pt-funcall.cc \
  libinterp/parse-tree/pt-id.cc \
  libinterp/parse-tree/pt-idx.cc \
  libinterp/parse-tree/pt-jump.cc \
  libinterp/parse-tree/pt-loop.cc \
  libinterp/parse-tree/pt-mat.cc \
  libinterp/parse-tree/pt-misc.cc \
  libinterp/parse-tree/pt-pr-code.cc \
  libinterp/parse-tree/pt-select.cc \
  libinterp/parse-tree/pt-stmt.cc \
  libinterp/parse-tree/pt-unop.cc \
  libinterp/parse-tree/pt.cc \
  libinterp/parse-tree/token.cc

## Special rules for sources which must be built before rest of compilation.

## Don't use a pipeline to process gperf output since if gperf
## is missing but sed is not, the exit status of the pipeline
## will still be success and we will end up creating an empty
## oct-gperf.h file.
libinterp/parse-tree/oct-gperf.h: libinterp/parse-tree/octave.gperf
	$(AM_V_GEN)rm -f $@-t $@t1 $@ && \
	$(GPERF) -t -C -D -G -L C++ -Z octave_kw_hash $< > $@-t1 && \
	$(SED) 's,lookup\[,gperf_lookup[,' < $@-t1 > $@-t && \
	mv $@-t $@ && \
	rm -f $@-t1

libinterp/parse-tree/oct-parse.h: libinterp/parse-tree/oct-parse.cc

libinterp/parse-tree/oct-parse.yy: libinterp/parse-tree/oct-parse.in.yy
	$(AM_V_GEN)$(call subst-bison-api-decls,octave_)

noinst_LTLIBRARIES += \
  libinterp/parse-tree/libparse-tree.la

libinterp_parse_tree_libparse_tree_la_SOURCES = $(PARSE_TREE_SRC)

libinterp_parse_tree_libparse_tree_la_CPPFLAGS = $(libinterp_liboctinterp_la_CPPFLAGS)

libinterp_parse_tree_libparse_tree_la_CFLAGS = $(AM_CFLAGS) $(WARN_CFLAGS)

libinterp_parse_tree_libparse_tree_la_CXXFLAGS = $(AM_CXXFLAGS) $(WARN_CXXFLAGS)

libinterp_EXTRA_DIST += \
  libinterp/parse-tree/oct-parse.in.yy \
  libinterp/parse-tree/octave.gperf

