PARSE_TREE_INC = \
  %reldir%/bp-table.h \
  %reldir%/jit-ir.h \
  %reldir%/jit-typeinfo.h \
  %reldir%/jit-util.h \
  %reldir%/lex.h \
  %reldir%/parse.h \
  %reldir%/pt-all.h \
  %reldir%/pt-arg-list.h \
  %reldir%/pt-array-list.h \
  %reldir%/pt-assign.h \
  %reldir%/pt-binop.h \
  %reldir%/pt-bp.h \
  %reldir%/pt-cbinop.h \
  %reldir%/pt-cell.h \
  %reldir%/pt-check.h \
  %reldir%/pt-classdef.h \
  %reldir%/pt-cmd.h \
  %reldir%/pt-colon.h \
  %reldir%/pt-const.h \
  %reldir%/pt-decl.h \
  %reldir%/pt-eval.h \
  %reldir%/pt-except.h \
  %reldir%/pt-exp.h \
  %reldir%/pt-fcn-handle.h \
  %reldir%/pt-funcall.h \
  %reldir%/pt-id.h \
  %reldir%/pt-idx.h \
  %reldir%/pt-jit.h \
  %reldir%/pt-jump.h \
  %reldir%/pt-loop.h \
  %reldir%/pt-mat.h \
  %reldir%/pt-misc.h \
  %reldir%/pt-pr-code.h \
  %reldir%/pt-select.h \
  %reldir%/pt-stmt.h \
  %reldir%/pt-tm-const.h \
  %reldir%/pt-unop.h \
  %reldir%/pt-walk.h \
  %reldir%/pt.h \
  %reldir%/token.h


## oct-gperf.h and oct-parse.h are in the SRC list so that they will
## be distributed but not installed.

PARSE_TREE_SRC = \
  %reldir%/bp-table.cc \
  %reldir%/jit-ir.cc \
  %reldir%/jit-typeinfo.cc \
  %reldir%/jit-util.cc \
  %reldir%/lex.ll \
  %reldir%/oct-gperf.h \
  %reldir%/oct-parse.h \
  %reldir%/oct-parse.yy \
  %reldir%/pt-arg-list.cc \
  %reldir%/pt-array-list.cc \
  %reldir%/pt-assign.cc \
  %reldir%/pt-binop.cc \
  %reldir%/pt-bp.cc \
  %reldir%/pt-cbinop.cc \
  %reldir%/pt-cell.cc \
  %reldir%/pt-check.cc \
  %reldir%/pt-classdef.cc \
  %reldir%/pt-cmd.cc \
  %reldir%/pt-colon.cc \
  %reldir%/pt-const.cc \
  %reldir%/pt-decl.cc \
  %reldir%/pt-eval.cc \
  %reldir%/pt-except.cc \
  %reldir%/pt-exp.cc \
  %reldir%/pt-fcn-handle.cc \
  %reldir%/pt-funcall.cc \
  %reldir%/pt-id.cc \
  %reldir%/pt-idx.cc \
  %reldir%/pt-jit.cc \
  %reldir%/pt-jump.cc \
  %reldir%/pt-loop.cc \
  %reldir%/pt-mat.cc \
  %reldir%/pt-misc.cc \
  %reldir%/pt-pr-code.cc \
  %reldir%/pt-select.cc \
  %reldir%/pt-stmt.cc \
  %reldir%/pt-tm-const.cc \
  %reldir%/pt-unop.cc \
  %reldir%/pt-walk.cc \
  %reldir%/pt.cc \
  %reldir%/token.cc

## Special rules for sources which must be built before rest of compilation.

## Don't use a pipeline to process gperf output since if gperf
## is missing but sed is not, the exit status of the pipeline
## will still be success and we will end up creating an empty
## oct-gperf.h file.
%reldir%/oct-gperf.h: %reldir%/octave.gperf
	$(AM_V_GEN)rm -f $@-t $@t1 $@ && \
	$(GPERF) -t -C -D -G -L C++ -Z octave_kw_hash $< > $@-t1 && \
	$(SED) -e 's,lookup\[,gperf_lookup[,' -e 's,register ,,g' < $@-t1 > $@-t && \
	mv $@-t $@ && \
	rm -f $@-t1

%reldir%/oct-parse.yy: %reldir%/oct-parse.in.yy
	$(AM_V_GEN)$(call subst-bison-api-decls,octave_)

noinst_LTLIBRARIES += \
  %reldir%/libparse-tree.la

%canon_reldir%_libparse_tree_la_SOURCES = $(PARSE_TREE_SRC)

%canon_reldir%_libparse_tree_la_CPPFLAGS = $(libinterp_liboctinterp_la_CPPFLAGS)

%canon_reldir%_libparse_tree_la_CFLAGS = $(AM_CFLAGS) $(WARN_CFLAGS)

%canon_reldir%_libparse_tree_la_CXXFLAGS = $(AM_CXXFLAGS) $(WARN_CXXFLAGS)

libinterp_EXTRA_DIST += \
  %reldir%/oct-parse.in.yy \
  %reldir%/octave.gperf
