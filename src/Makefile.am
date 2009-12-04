# Makefile for octave's src directory
#
# Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
#               2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009 John W. Eaton
#
# This file is part of Octave.
# 
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
# 
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, see
# <http://www.gnu.org/licenses/>.

TOPDIR = ..

include ../common.mk

octlib_LTLIBRARIES = \
  liboctinterp.la

AM_CPPFLAGS = \
  @CPPFLAGS@ -I$(top_srcdir)/libcruft/misc -I../liboctave -I$(top_srcdir)/liboctave -I. -I$(srcdir) 

AUTOMAKE_OPTIONS = subdir-objects

## Order matters here.  Leave builtins.cc last, because it depends on 
## $(DEF_FILES), and building those requires all the sources
## (except builtins.cc) to be available.  List only one of graphics.h
## and graphics-props.cc because if both are listed, building in
## parallel will fail.
BUILT_SOURCES = \
  defaults.h \
  graphics.h \
  lex.cc \
  mxarray.h \
  oct-conf.h \
  oct-errno.cc \
  oct-gperf.h \
  oct-parse.cc \
  ops.cc \
  version.h \
  builtins.cc

BUILT_DISTFILES = \
  oct-gperf.h \
  oct-parse.h

## FIXME -- These files don't need to be distributed.  Some of them
## do need to be installed.  So we need to add them to a list somewhere
## so that happens correctly.
BUILT_NODISTFILES = \
  defaults.h \
  graphics.h \
  oct-conf.h \
  oct-errno.cc \
  ops.cc \
  mxarray.h \
  version.h \
  $(OPT_HANDLERS) \
  $(OPT_INC) \
  $(DEF_FILES) \
  builtins.cc

EXTRA_DIST = \
  ChangeLog \
  Makefile.in \
  defaults.h.in \
  find-defun-files.sh \
  genprops.awk \
  graphics.h.in \
  mk-errno-list \
  mk-pkg-add \
  mkbuiltins \
  mkdefs \
  mkgendoc \
  mkops \
  mxarray.h.in \
  oct-conf.h.in \
  oct-errno.cc.in \
  octave.gperf \
  version.h.in \
  $(BUILT_DISTFILES)

DLL_CDEFS = @OCTINTERP_DLL_DEFS@
DLL_CXXDEFS = @OCTINTERP_DLL_DEFS@

.cc.df:	
	(echo "// DO NOT EDIT!  Generated automatically by mkdefs." ; \
	  case "$<" in \
	    $(srcdir)/*) xsrcdir="`echo $< | sed 's,^$(srcdir),src,'`" ;; \
	    *) xsrcdir="src/$<" ;; \
	  esac ; \
	  echo " XDEFUN_FILE_NAME (\"$$xsrcdir\")" ; \
	  egrep '^(///*|/\*) *PKG_ADD:' $< ; \
	  $(CXXCPP) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
	    $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) \
	    -DMAKE_BUILTINS $< | $(srcdir)/mkdefs) > $@-t
	mv $@-t $@

OPT_HANDLERS = \
  DASPK-opts.cc \
  DASRT-opts.cc \
  DASSL-opts.cc \
  LSODE-opts.cc \
  Quad-opts.cc

OPT_IN = \
  ../liboctave/DASPK-opts.in \
  ../liboctave/DASRT-opts.in \
  ../liboctave/DASSL-opts.in \
  ../liboctave/LSODE-opts.in \
  ../liboctave/Quad-opts.in

OPT_INC = \
  ../liboctave/DASPK-opts.h \
  ../liboctave/DASRT-opts.h \
  ../liboctave/DASSL-opts.h \
  ../liboctave/LSODE-opts.h \
  ../liboctave/Quad-opts.h

OV_INTTYPE_INC = \
  ov-base-int.h \
  ov-base-int.cc \
  ov-int-traits.h \
  ov-int16.h \
  ov-int32.h \
  ov-int64.h \
  ov-int8.h \
  ov-intx.h \
  ov-uint16.h \
  ov-uint32.h \
  ov-uint64.h \
  ov-uint8.h

OV_INCLUDES = \
  ov-base-diag.h \
  ov-base-diag.cc \
  ov-base-mat.h \
  ov-base-mat.cc \
  ov-base-scalar.h \
  ov-base-scalar.cc \
  ov-base.h \
  ov-bool-mat.h \
  ov-bool-mat.cc \
  ov-bool.h \
  ov-builtin.h \
  ov-cell.h \
  ov-ch-mat.h \
  ov-class.h \
  ov-colon.h \
  ov-complex.h \
  ov-cs-list.h \
  ov-cx-diag.h \
  ov-cx-mat.h \
  ov-dld-fcn.h \
  ov-fcn-handle.h \
  ov-fcn-inline.h \
  ov-fcn.h \
  ov-float.h \
  ov-flt-complex.h \
  ov-flt-cx-diag.h \
  ov-flt-cx-mat.h \
  ov-flt-re-diag.h \
  ov-flt-re-mat.h \
  ov-list.h \
  ov-mex-fcn.h \
  ov-null-mat.h \
  ov-perm.h \
  ov-range.h \
  ov-re-diag.h \
  ov-re-mat.h \
  ov-scalar.h \
  ov-str-mat.h \
  ov-struct.h \
  ov-type-conv.h \
  ov-typeinfo.h \
  ov-usr-fcn.h \
  ov.h \
  $(OV_INTTYPE_INC)

OV_SPARSE_INCLUDES = \
  ov-base-sparse.h \
  ov-bool-sparse.h \
  ov-cx-sparse.h \
  ov-re-sparse.h

PT_INCLUDES = \
  pt-all.h \
  pt-arg-list.h \
  pt-assign.h \
  pt-binop.h \
  pt-bp.h \
  pt-cbinop.h \
  pt-cell.h \
  pt-check.h \
  pt-cmd.h \
  pt-colon.h \
  pt-const.h \
  pt-decl.h \
  pt-eval.h \
  pt-except.h \
  pt-exp.h \
  pt-fcn-handle.h \
  pt-id.h \
  pt-idx.h \
  pt-jump.h \
  pt-loop.h \
  pt-mat.h \
  pt-misc.h \
  pt-pr-code.h \
  pt-select.h \
  pt-stmt.h \
  pt-unop.h \
  pt-walk.h \
  pt.h

octinclude_HEADERS = \
  Cell.h \
  base-list.h \
  builtins.h \
  c-file-ptr-stream.h \
  comment-list.h \
  debug.h \
  defaults.h \
  defun-dld.h \
  defun-int.h \
  defun.h \
  dirfns.h \
  display.h \
  dynamic-ld.h \
  error.h \
  file-io.h \
  gl-render.h \
  gl2ps.h \
  gl2ps-renderer.h \
  graphics.h \
  graphics-props.cc \
  gripes.h \
  help.h \
  input.h \
  lex.h \
  load-path.h \
  load-save.h \
  ls-ascii-helper.h \
  ls-hdf5.h \
  ls-mat-ascii.h \
  ls-mat4.h \
  ls-mat5.h \
  ls-oct-ascii.h \
  ls-oct-binary.h \
  ls-utils.h \
  mex.h \
  mexproto.h \
  mxarray.h \
  oct-conf.h \
  oct-errno.h \
  oct-fstrm.h \
  oct-gperf.h \
  oct-hdf5.h \
  oct-hist.h \
  oct-iostrm.h \
  oct-lvalue.h \
  oct-map.h \
  oct-obj.h \
  oct-prcstrm.h \
  oct-procbuf.h \
  oct-stdstrm.h \
  oct-stream.h \
  oct-strstrm.h \
  oct.h \
  octave.h \
  ops.h \
  pager.h \
  parse.h \
  pr-output.h \
  procstream.h \
  sighandlers.h \
  siglist.h \
  sparse-xdiv.h \
  sparse-xpow.h \
  symtab.h \
  sysdep.h \
  token.h \
  toplev.h \
  txt-eng-ft.h \
  txt-eng.h \
  unwind-prot.h \
  utils.h \
  variables.h \
  version.h \
  xdiv.h \
  xnorm.h \
  xpow.h \
  zfstream.h \
  $(OV_INCLUDES) \
  $(OV_SPARSE_INCLUDES) \
  $(PT_INCLUDES)

OV_INTTYPE_SRC = \
  ov-int16.cc \
  ov-int32.cc \
  ov-int64.cc \
  ov-int8.cc \
  ov-uint16.cc \
  ov-uint32.cc \
  ov-uint64.cc \
  ov-uint8.cc

OV_SPARSE_SRC = \
  ov-base-sparse.cc \
  ov-bool-sparse.cc \
  ov-cx-sparse.cc \
  ov-re-sparse.cc

OV_SRC = \
  ov-base.cc \
  ov-bool-mat.cc \
  ov-bool.cc \
  ov-builtin.cc \
  ov-cell.cc \
  ov-ch-mat.cc \
  ov-class.cc \
  ov-colon.cc \
  ov-complex.cc \
  ov-cs-list.cc \
  ov-cx-diag.cc \
  ov-cx-mat.cc \
  ov-dld-fcn.cc \
  ov-fcn-handle.cc \
  ov-fcn-inline.cc \
  ov-fcn.cc \
  ov-float.cc \
  ov-flt-complex.cc \
  ov-flt-cx-diag.cc \
  ov-flt-cx-mat.cc \
  ov-flt-re-diag.cc \
  ov-flt-re-mat.cc \
  ov-list.cc \
  ov-mex-fcn.cc \
  ov-null-mat.cc \
  ov-perm.cc \
  ov-range.cc \
  ov-re-diag.cc \
  ov-re-mat.cc \
  ov-scalar.cc \
  ov-str-mat.cc \
  ov-struct.cc \
  ov-typeinfo.cc \
  ov-usr-fcn.cc \
  ov.cc \
  $(OV_INTTYPE_SRC) \
  $(OV_SPARSE_SRC)

PT_SRC = \
  pt-arg-list.cc \
  pt-assign.cc \
  pt-binop.cc \
  pt-bp.cc \
  pt-cbinop.cc \
  pt-cell.cc \
  pt-check.cc \
  pt-cmd.cc \
  pt-colon.cc \
  pt-const.cc \
  pt-decl.cc \
  pt-eval.cc \
  pt-except.cc \
  pt-exp.cc \
  pt-fcn-handle.cc \
  pt-id.cc \
  pt-idx.cc \
  pt-jump.cc \
  pt-loop.cc \
  pt-mat.cc \
  pt-misc.cc \
  pt-pr-code.cc \
  pt-select.cc \
  pt-stmt.cc \
  pt-unop.cc \
  pt.cc

DIST_SRC = \
  Cell.cc \
  bitfcns.cc \
  c-file-ptr-stream.cc \
  comment-list.cc \
  cutils.c \
  data.cc \
  debug.cc \
  defaults.cc \
  defun.cc \
  dirfns.cc \
  display.cc \
  dynamic-ld.cc \
  error.cc \
  file-io.cc \
  gl-render.cc \
  gl2ps.c \
  gl2ps-renderer.cc \
  graphics.cc \
  gripes.cc \
  help.cc \
  input.cc \
  lex.ll \
  load-path.cc \
  load-save.cc \
  ls-ascii-helper.cc \
  ls-hdf5.cc \
  ls-mat-ascii.cc \
  ls-mat4.cc \
  ls-mat5.cc \
  ls-oct-ascii.cc \
  ls-oct-binary.cc \
  ls-utils.cc \
  mappers.cc \
  matherr.c \
  mex.cc \
  oct-fstrm.cc \
  oct-hist.cc \
  oct-iostrm.cc \
  oct-lvalue.cc \
  oct-map.cc \
  oct-obj.cc \
  oct-parse.yy \
  oct-prcstrm.cc \
  oct-procbuf.cc \
  oct-stream.cc \
  oct-strstrm.cc \
  octave.cc \
  pager.cc \
  pr-output.cc \
  procstream.cc \
  sighandlers.cc \
  siglist.c \
  sparse-xdiv.cc \
  sparse-xpow.cc \
  strfns.cc \
  symtab.cc \
  syscalls.cc \
  sysdep.cc \
  token.cc \
  toplev.cc \
  txt-eng-ft.cc \
  unwind-prot.cc \
  utils.cc \
  variables.cc \
  xdiv.cc \
  xnorm.cc \
  xpow.cc \
  zfstream.cc \
  $(OV_SRC) \
  $(PT_SRC)

include DLD-FUNCTIONS/module.mk
include OPERATORS/module.mk
include TEMPLATE-INST/module.mk

if AMCOND_ENABLE_DYNAMIC_LINKING
  DLD_DYNAMIC_SRC = $(DLD_FUNCTIONS_SRC)
  DLD_STATIC_SRC =
  OCT_FILES = $(DLD_FUNCTIONS_SRC:.cc=.oct)
else
  DLD_DYNAMIC_SRC =
  DLD_STATIC_SRC = $(DLD_FUNCTIONS_SRC)
  OCT_FILES =
endif

liboctinterp_la_SOURCES = \
  $(DIST_SRC) \
  $(DLD_STATIC_SRC) \
  $(OPERATORS_SRC) \
  $(TEMPLATE_INST_SRC)

DLD_DYNAMIC_DEF_FILES = $(DLD_DYNAMIC_SRC:.cc=.df)
DLD_STATIC_DEF_FILES = $(DLD_STATIC_SRC:.cc=.df)

SRC_DEF_FILES := $(shell $(srcdir)/find-defun-files.sh "$(srcdir)" $(DIST_SRC))

DEF_FILES = $(SRC_DEF_FILES) $(DLD_DYNAMIC_DEF_FILES) $(DLD_STATIC_DEF_FILES)

$(DEF_FILES): mkdefs Makefile

if AMCOND_ENABLE_DYNAMIC_LINKING
  OCTAVE_LIBS = \
    ./liboctinterp.la \
    ../liboctave/liboctave.la \
    ../libcruft/libcruft.la \
    $(FFTW_XLDFLAGS) $(FFTW_XLIBS) \
    $(SPARSE_XLDFLAGS) $(SPARSE_XLIBS) \
    $(GRAPHICS_LDFLAGS) $(GRAPHICS_LIBS) \
    $(FT2_LDFLAGS) $(FT2_LIBS) \
    $(HDF5_LDFLAGS) $(HDF5_LIBS) $(Z_LDFLAGS) $(Z_LIBS) \
    $(OPENGL_LIBS) $(X11_LIBS) $(CARBON_LIBS) \
    $(READLINE_LIBS) $(TERM_LIBS) \
    $(LIBGLOB) \
    $(LAPACK_LIBS) $(BLAS_LIBS) \
    $(LIBS) \
    $(FLIBS)
else
  ## FIXME -- this list is probably not complete now.  It may not even
  ## be possible to build a statically linked copy of Octave that is
  ## fully functional.
  OCTAVE_LIBS = \
    ./liboctinterp.la \
    ../liboctave/liboctave.la \
    ../libcruft/libcruft.la \
    $(FFTW_XLDFLAGS) $(FFTW_XLIBS)
    $(QHULL_LDFLAGS) $(QHULL_LIBS) \
    $(ARPACK_LDFLAGS) $(ARPACK_LIBS) \
    $(QRUPDATE_LDFLAGS) $(QRUPDATE_LIBS) \
    $(SPARSE_XLDFLAGS) $(SPARSE_XLIBS) \
    $(REGEX_LDFLAGS) $(REGEX_LIBS) \
    $(CURL_LDFLAGS) $(CURL_LIBS) \
    $(GLPK_LDFLAGS) $(GLPK_LIBS) \
    $(MAGICK_LDFLAGS) $(MAGICK_LIBS) \
    $(GRAPHICS_LDFLAGS) $(GRAPHICS_LIBS) \
    $(FT2_LDFLAGS) $(FT2_LIBS) \
    $(HDF5_LDFLAGS) $(HDF5_LIBS) $(Z_LDFLAGS) $(Z_LIBS) \
    $(OPENGL_LIBS) $(X11_LIBS) $(CARBON_LIBS) \
    $(READLINE_LIBS) $(TERM_LIBS) \
    $(LIBGLOB) \
    $(LAPACK_LIBS) $(BLAS_LIBS) \
    $(LIBS) \
    $(FLIBS)
endif

OCTINTERP_LINK_DEPS = \
  $(RLD_FLAG) \
  ../liboctave/liboctave.la \
  ../libcruft/libcruft.la \
  $(FFTW_XLDFLAGS) $(FFTW_XLIBS) \
  $(HDF5_LDFLAGS) $(HDF5_LIBS) $(Z_LDFLAGS) $(Z_LIBS) \
  $(OPENGL_LIBS) $(X11_LIBS) $(CARBON_LIBS) \
  $(READLINE_LIBS) $(TERM_LIBS) \
  $(LIBGLOB) \
  $(LAPACK_LIBS) $(BLAS_LIBS) \
  $(LIBS) \
  $(FLIBS)

liboctinterp_la_LIBADD = $(OCTINTERP_LINK_DEPS)

OCT_LINK_DEPS = \
  $(RLD_FLAG) $(LDFLAGS) \
  ./liboctinterp.la \
  ../liboctave/liboctave.la \
  ../libcruft/libcruft.la

bin_PROGRAMS = octave

octave_SOURCES = main.c

nodist_liboctinterp_la_SOURCES = \
  builtins.cc \
  defaults.h \
  graphics.h \
  mxarray.h \
  oct-conf.h \
  oct-errno.cc \
  ops.cc \
  version.h \
  $(OPT_HANDLERS) \
  $(OPT_INC)

liboctinterp_la_CPPFLAGS = @OCTINTERP_DLL_DEFS@ $(AM_CPPFLAGS)

liboctinterp_la_LDFLAGS = -release $(version) $(NO_UNDEFINED_LDFLAG)

CLEANFILES = \
  DLD-FUNCTIONS/PKG_ADD \
  doc-files \
  gendoc.cc \
  gendoc$(BUILD_EXEEXT) \
  graphics-props.cc \
  oct-parse.output \
  $(BUILT_NODISTFILES)

MAINTAINERCLEANFILES = \
  $(BUILT_DISTFILES)

octave_LDADD = $(OCTAVE_LIBS)

all-local: $(OCT_FILES) DLD-FUNCTIONS/PKG_ADD DOCSTRINGS

lex.lo lex.o oct-parse.lo oct-parse.o: \
  AM_CXXFLAGS := $(filter-out -Wold-style-cast, $(AM_CXXFLAGS))

fltk_backend.lo fltk_backend.o: \
  AM_CXXFLAGS := $(filter-out $(DLL_CXXDEFS), $(AM_CXXFLAGS) $(GRAPHICS_CFLAGS))

# XERBLA = ../libcruft/blas-xtra/xerbla.o

builtins.cc: $(DEF_FILES) mkbuiltins
	$(srcdir)/mkbuiltins $(DEF_FILES) > $@-t
	mv $@-t $@

mxarray.h: mxarray.h.in Makefile
	$(SED) < $< \
	  -e "s|%OCTAVE_IDX_TYPE%|${OCTAVE_IDX_TYPE}|" > $@-t
	mv $@-t $@

version.h: version.h.in Makefile
	$(SED) < $< \
	  -e "s|%OCTAVE_API_VERSION%|\"${OCTAVE_API_VERSION}\"|" \
	  -e "s|%OCTAVE_COPYRIGHT%|\"${OCTAVE_COPYRIGHT}\"|" \
	  -e "s|%OCTAVE_RELEASE_DATE%|\"${OCTAVE_RELEASE_DATE}\"|" \
	  -e "s|%OCTAVE_VERSION%|\"${OCTAVE_VERSION}\"|" > $@-t
	mv $@-t $@

graphics.h: graphics.h.in genprops.awk Makefile
	$(AWK) -f $(srcdir)/genprops.awk $< > $@-t
	mv $@-t $@

graphics-props.cc: graphics.h.in genprops.awk Makefile
	$(AWK) -v emit_graphics_props=1 -f $(srcdir)/genprops.awk $< > $@-t
	mv $@-t $@

DLD-FUNCTIONS/PKG_ADD: $(DLD_DYNAMIC_DEF_FILES) mk-pkg-add
	$(srcdir)/mk-pkg-add $(DLD_DYNAMIC_DEF_FILES) > $@-t
	mv $@-t $@

DOCSTRINGS: gendoc$(BUILD_EXEEXT)
	./gendoc > $@-t
	mv $@-t $@

doc-files: $(DEF_FILES)
	echo $(DEF_FILES) > $@-t
	mv $@-t $@

gendoc.cc: doc-files mkgendoc
	$(srcdir)/mkgendoc $(srcdidr) doc-files > $@-t
	mv $@-t $@

gendoc$(BUILD_EXEEXT): gendoc.cc
	$(BUILD_CXX) $(BUILD_CXXFLAGS) -o $@ $^ $(BUILD_LDFLAGS)

ops.cc: $(OPERATORS_SRC) mkops
	$(srcdir)/mkops $(OPERATORS_SRC) > $@-t
	mv $@-t $@

$(DEF_FILES): $(OPT_INC) mkdefs 

install-exec-hook: make-version-links

install-data-hook: install-oct

uninstall-local: remove-version-links uninstall-oct

make-version-links:
	cd $(DESTDIR)$(bindir) && \
	mv octave$(EXEEXT) octave-$(version)$(EXEEXT) && \
	$(LN_S) octave-$(version)$(EXEEXT) octave$(EXEEXT)
.PHONY: make-version-links

remove-version-links:
	rm -f $(DESTDIR)$(bindir)/octave-$(version)$(EXEEXT)
.PHONY: remove-version-links

if AMCOND_ENABLE_DYNAMIC_LINKING
install-oct:
	$(top_srcdir)/mkinstalldirs $(DESTDIR)$(octfiledir)
	if [ -n "`cat DLD-FUNCTIONS/PKG_ADD`" ]; then \
	  $(INSTALL_DATA) DLD-FUNCTIONS/PKG_ADD $(DESTDIR)$(octfiledir)/PKG_ADD; \
	fi
	cd $(DESTDIR)/$(octlibdir) && \
	for ltlib in $(DLD_FUNCTIONS_LIBS); do \
	  f=`echo $$ltlib | $(SED) 's,.*/,,'`; \
	  dl=`$(SED) -n -e "s/dlname='\([^']*\)'/\1/p" < $$f`; \
	  if [ -n "$$dl" ]; then \
	    $(INSTALL_PROGRAM) $$dl $(DESTDIR)$(octfiledir)/`echo $$f | $(SED) 's,^lib,,; s,\.la$$,.oct,'`; \
	  else \
	    echo "error: dlname is empty in $$ltlib!"; \
	    exit 1; \
	  fi; \
	  lnames=`$(SED) -n -e "s/library_names='\([^']*\)'/\1/p" < $$f`; \
	  if [ -n "$$lnames" ]; then \
	    rm -f $$f $$lnames; \
	  fi \
	done

uninstall-oct:
	for f in $(OCT_FILES); do \
	  rm -f $(DESTDIR)$(octfiledir)/`echo $$f | $(SED) 's,.*/,,'`; \
	done
	rm -f $(DESTDIR)$(octfiledir)/PKG_ADD
endif
.PHONY: install-oct uninstall-oct

# Special rules -- these files need special things to be defined.

$(OPT_HANDLERS) : %.cc : $(top_srcdir)/liboctave/%.in $(top_srcdir)/mk-opts.pl
	$(PERL) $(top_srcdir)/mk-opts.pl --opt-handler-fcns $< > $@-t
	mv $@-t $@

## We require Bison.
#parse.cc : parse.y
#	@echo "expect 14 shift/reduce conflicts"
#	$(YACC) $(YFLAGS) --output=$@ --defines=y.tab.h $<

#lex.cc : lex.l
#	$(LEX) $(LFLAGS) $< > $(@F)-t
#	@mv $(@F)-t $@


$(OPT_INC) : %.h : %.in
	$(MAKE) -C $(@D) $@

## We want to force an update of defaults.h and oct-conf.h every
## time make is run because some values may come from the command
## line or the environment.  The substitution rules use move-if-change,
## so this should not cause trouble if the file already exists and the
## newly generated file is not different.

defaults.h: defaults.h.in Makefile
	$(do_subst_default_vals)

oct-conf.h: oct-conf.h.in Makefile
	$(do_subst_config_vals)

oct-errno.cc: oct-errno.cc.in Makefile
	if test -n "$(PERL)"; then \
	  $(srcdir)/mk-errno-list --perl "$(PERL)" < $< > $@-t; \
	elif test -n "$(PYTHON)"; then \
	  $(srcdir)/mk-errno-list --python "$(PYTHON)" < $< > $@-t; \
	else \
	  $(SED) '/@SYSDEP_ERRNO_LIST@/D' $< > $@-t; \
	fi
	mv $@-t $@

## Don't use a pipeline to process gperf output since if gperf
## is missing but sed is not, the exit status of the pipeline
## will still be success and we will end up creating an empty
## oct-gperf.h file.
oct-gperf.h: octave.gperf
	$(GPERF) -t -C -D -G -L C++ -Z octave_kw_hash $< > $@-t1
	$(SED) 's,lookup\[,gperf_lookup[,' < $@-t1 > $@-t
	mv $@-t $@
	rm -f $@-t1

display.df display.lo: CPPFLAGS += $(X11_FLAGS)

DLD-FUNCTIONS/__magick_read__.df: CPPFLAGS += $(MAGICK_CPPFLAGS)
DLD_FUNCTIONS___magick_read___la_CPPFLAGS = $(AM_CPPFLAGS) $(MAGICK_CPPFLAGS)
DLD_FUNCTIONS___magick_read___la_LIBADD += $(MAGICK_LDFLAGS) $(MAGICK_LIBS)

DLD-FUNCTIONS/convhulln.df: CPPFLAGS += $(QHULL_CPPFLAGS)
DLD_FUNCTIONS_convhulln_la_CPPFLAGS = $(AM_CPPFLAGS) $(QHULL_CPPFLAGS)
DLD_FUNCTIONS_convhulln_la_LIBADD += $(QHULL_LDFLAGS) $(QHULL_LIBS)

DLD-FUNCTIONS/__delaunayn__.df: CPPFLAGS += $(QHULL_CPPFLAGS)
DLD_FUNCTIONS___delaunayn___la_CPPFLAGS = $(AM_CPPFLAGS) $(QHULL_CPPFLAGS)
DLD_FUNCTIONS___delaunayn___la_LIBADD += $(QHULL_LDFLAGS) $(QHULL_LIBS)

DLD-FUNCTIONS/__voronoi__.df: CPPFLAGS += $(QHULL_CPPFLAGS)
DLD_FUNCTIONS___voronoi___la_CPPFLAGS = $(AM_CPPFLAGS) $(QHULL_CPPFLAGS)
DLD_FUNCTIONS___voronoi___la_LIBADD += $(QHULL_LDFLAGS) $(QHULL_LIBS)

DLD-FUNCTIONS/eigs.df: CPPFLAGS += $(ARPACK_CPPFLAGS) $(SPARSE_XCPPFLAGS)
DLD_FUNCTIONS_eigs_la_CPPFLAGS = $(AM_CPPFLAGS) $(ARPACK_CPPFLAGS) $(SPARSE_XCPPFLAGS)
DLD_FUNCTIONS_eigs_la_LIBADD += $(ARPACK_LDFLAGS) $(ARPACK_LIBS) $(SPARSE_XLDFLAGS) $(SPARSE_XLIBS) $(LAPACK_LIBS) $(BLAS_LIBS)

#DLD-FUNCTIONS/qz.df DLD-FUNCTIONS/qz.lo:
DLD_FUNCTIONS_qz_la_LIBADD += $(LAPACK_LIBS) $(BLAS_LIBS)

DLD-FUNCTIONS/qr.df: CPPFLAGS += $(QRUPDATE_CPPFLAGS) $(SPARSE_XCPPFLAGS)
DLD_FUNCTIONS_qr_la_CPPFLAGS = $(AM_CPPFLAGS) $(QRUPDATE_CPPFLAGS) $(SPARSE_XCPPFLAGS)
DLD_FUNCTIONS_qr_la_LIBADD += $(QRUPDATE_LDFLAGS) $(QRUPDATE_LIBS) $(SPARSE_XLDFLAGS) $(SPARSE_XLIBS)

DLD-FUNCTIONS/chol.df: CPPFLAGS += $(QRUPDATE_CPPFLAGS) $(SPARSE_XCPPFLAGS)
DLD_FUNCTIONS_chol_la_CPPFLAGS = $(AM_CPPFLAGS) $(QRUPDATE_CPPFLAGS) $(SPARSE_XCPPFLAGS)
DLD_FUNCTIONS_chol_la_LIBADD += $(QRUPDATE_LDFLAGS) $(QRUPDATE_LIBS) $(SPARSE_XLDFLAGS) $(SPARSE_XLIBS)

DLD-FUNCTIONS/regexp.df: CPPFLAGS += $(REGEX_CPPFLAGS)
DLD_FUNCTIONS_regexp_la_CPPFLAGS = $(AM_CPPFLAGS) $(REGEX_CPPFLAGS)
DLD_FUNCTIONS_regexp_la_LIBADD += $(REGEX_LDFLAGS) $(REGEX_LIBS)

DLD-FUNCTIONS/urlwrite.df: CPPFLAGS += $(CURL_CPPFLAGS)
DLD_FUNCTIONS_urlwrite_la_CPPFLAGS = $(AM_CPPFLAGS) $(CURL_CPPFLAGS)
DLD_FUNCTIONS_urlwrite_la_LIBADD += $(CURL_LDFLAGS) $(CURL_LIBS)

DLD-FUNCTIONS/__glpk__.df: CPPFLAGS += $(GLPK_CPPFLAGS)
DLD_FUNCTIONS___glpk___la_CPPFLAGS = $(AM_CPPFLAGS) $(GLPK_CPPFLAGS)
DLD_FUNCTIONS___glpk___la_LIBADD += $(GLPK_LDFLAGS) $(GLPK_LIBS)

DLD-FUNCTIONS/fltk_backend.df: CPPFLAGS += $(GRAPHICS_CPPFLAGS) $(FT2_CPPFLAGS)
DLD_FUNCTIONS_fltk_backend_la_CPPFLAGS = $(AM_CPPFLAGS) $(GRAPHICS_CPPFLAGS) $(FT2_CPPFLAGS)
DLD_FUNCTIONS_fltk_backend_la_LIBADD += $(GRAPHICS_LDFLAGS) $(GRAPHICS_LIBS) $(FT2_LDFLAGS) $(FT2_LIBS)

DLD-FUNCTIONS/amd.df: CPPFLAGS += $(SPARSE_XCPPFLAGS)
DLD_FUNCTIONS_amd_la_CPPFLAGS = $(AM_CPPFLAGS) $(SPARSE_XCPPFLAGS)
DLD_FUNCTIONS_amd_la_LIBADD += $(SPARSE_XLDFLAGS) $(SPARSE_XLIBS)

DLD-FUNCTIONS/colamd.df: CPPFLAGS += $(SPARSE_XCPPFLAGS)
DLD_FUNCTIONS_colamd_la_CPPFLAGS = $(AM_CPPFLAGS) $(SPARSE_XCPPFLAGS)
DLD_FUNCTIONS_colamd_la_LIBADD += $(SPARSE_XLDFLAGS) $(SPARSE_XLIBS)

DLD-FUNCTIONS/ccolamd.df: CPPFLAGS += $(SPARSE_XCPPFLAGS)
DLD_FUNCTIONS_ccolamd_la_CPPFLAGS = $(AM_CPPFLAGS) $(SPARSE_XCPPFLAGS)
DLD_FUNCTIONS_ccolamd_la_LIBADD += $(SPARSE_XLDFLAGS) $(SPARSE_XLIBS)

DLD-FUNCTIONS/symbfact.df: CPPFLAGS += $(SPARSE_XCPPFLAGS)
DLD_FUNCTIONS_symbfact_la_CPPFLAGS = $(AM_CPPFLAGS) $(SPARSE_XCPPFLAGS)
DLD_FUNCTIONS_symbfact_la_LIBADD += $(SPARSE_XLDFLAGS) $(SPARSE_XLIBS)

DLD-FUNCTIONS/dmperm.df: CPPFLAGS += $(SPARSE_XCPPFLAGS)
DLD_FUNCTIONS_dmperm_la_CPPFLAGS = $(AM_CPPFLAGS) $(SPARSE_XCPPFLAGS)
DLD_FUNCTIONS_dmperm_la_LIBADD += $(SPARSE_XLDFLAGS) $(SPARSE_XLIBS)

DLD-FUNCTIONS/symrcm.df: CPPFLAGS += $(SPARSE_XCPPFLAGS)
DLD_FUNCTIONS_symrcm_la_CPPFLAGS = $(AM_CPPFLAGS) $(SPARSE_XCPPFLAGS)
DLD_FUNCTIONS_symrcm_la_LIBADD += $(SPARSE_XLDFLAGS) $(SPARSE_XLIBS)

DLD-FUNCTIONS/fft.df: CPPFLAGS += $(FFTW_XCPPFLAGS)
DLD_FUNCTIONS_fft_la_CPPFLAGS = $(AM_CPPFLAGS) $(FFTW_XCPPFLAGS)
DLD_FUNCTIONS_fft_la_LIBADD += $(FFTW_XLDFLAGS) $(FFTW_XLIBS)

DLD-FUNCTIONS/fft2.df: CPPFLAGS += $(FFTW_XCPPFLAGS)
DLD_FUNCTIONS_fft2_la_CPPFLAGS = $(AM_CPPFLAGS) $(FFTW_XCPPFLAGS)
DLD_FUNCTIONS_fft2_la_LIBADD += $(FFTW_XLDFLAGS) $(FFTW_XLIBS)

DLD-FUNCTIONS/fftn.df: CPPFLAGS += $(FFTW_XCPPFLAGS)
DLD_FUNCTIONS_fftn_la_CPPFLAGS = $(AM_CPPFLAGS) $(FFTW_XCPPFLAGS)
DLD_FUNCTIONS_fftn_la_LIBADD += $(FFTW_XLDFLAGS) $(FFTW_XLIBS)

DLD-FUNCTIONS/fftw.df: CPPFLAGS += $(FFTW_XCPPFLAGS)
DLD_FUNCTIONS_fftw_la_CPPFLAGS = $(AM_CPPFLAGS) $(FFTW_XCPPFLAGS)
DLD_FUNCTIONS_fftw_la_LIBADD += $(FFTW_XLDFLAGS) $(FFTW_XLIBS)
