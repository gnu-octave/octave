# Makefile for octave's src directory
#
# Copyright (C) 1993-2012 John W. Eaton
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

include $(top_srcdir)/build-aux/common.mk

AM_CPPFLAGS = \
  @CPPFLAGS@ -I../libgnu -I$(top_srcdir)/libgnu \
  -I$(top_srcdir)/libcruft/misc \
  -I../liboctave -I$(top_srcdir)/liboctave \
  -I. -I$(srcdir)

AUTOMAKE_OPTIONS = subdir-objects

octlib_LTLIBRARIES = liboctinterp.la

if AMCOND_BUILD_COMPILED_AUX_PROGRAMS
bin_PROGRAMS = \
  mkoctfile \
  octave \
  octave-config

mkoctfile_SOURCES =
nodist_mkoctfile_SOURCES = mkoctfile.cc
mkoctfile_LDADD = ../libgnu/libgnu.la $(LIBS)

octave_config_SOURCES =
nodist_octave_config_SOURCES = octave-config.cc
octave_config_LDADD = ../libgnu/libgnu.la $(LIBS)

BUILT_SOURCES_EXTRA = \
  mkoctfile.cc \
  octave-config.cc
else
bin_PROGRAMS = \
  octave

bin_SCRIPTS = \
  mkoctfile \
  octave-config
endif

## Order matters here.  Leave builtins.cc last, because it depends on
## $(DEF_FILES), and building those requires all the sources
## (except builtins.cc) to be available.
BUILT_SOURCES = \
  $(BUILT_SOURCES_EXTRA) \
  defaults.h \
  graphics.h \
  graphics-props.cc \
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

## Files that are created during build process and installed,
## BUT not distributed in tarball.
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
  $(ALL_DEF_FILES) \
  builtins.cc

EXTRA_DIST = \
  Makefile.in \
  defaults.h.in \
  DOCSTRINGS \
  find-defun-files.sh \
  genprops.awk \
  gl2ps.c \
  graphics.h.in \
  mk-errno-list \
  mk-pkg-add \
  mkbuiltins \
  mkdefs \
  mkgendoc \
  mkoctfile.cc.in \
  mkoctfile.in \
  mkops \
  mxarray.h.in \
  oct-conf.h.in \
  oct-errno.cc.in \
  octave-config.cc.in \
  octave-config.in \
  octave.gperf \
  version.h.in \
  $(BUILT_DISTFILES)

OPT_HANDLERS = \
  DASPK-opts.cc \
  DASRT-opts.cc \
  DASSL-opts.cc \
  LSODE-opts.cc \
  Quad-opts.cc

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
  ov-lazy-idx.h \
  ov-mex-fcn.h \
  ov-null-mat.h \
  ov-oncleanup.h \
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
  builtins.h \
  c-file-ptr-stream.h \
  comment-list.h \
  cutils.h \
  data.h \
  debug.h \
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
  parse-private.h \
  pr-output.h \
  procstream.h \
  profiler.h \
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
  xdiv.h \
  xnorm.h \
  xpow.h \
  zfstream.h \
  $(OV_INCLUDES) \
  $(OV_SPARSE_INCLUDES) \
  $(PT_INCLUDES)

nodist_octinclude_HEADERS = \
  defaults.h \
  graphics.h \
  oct-conf.h \
  mxarray.h \
  version.h

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
  ov-lazy-idx.cc \
  ov-mex-fcn.cc \
  ov-null-mat.cc \
  ov-oncleanup.cc \
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
  profiler.cc \
  sighandlers.cc \
  siglist.c \
  sparse.cc \
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
  xgl2ps.c \
  xnorm.cc \
  xpow.cc \
  zfstream.cc \
  $(OV_SRC) \
  $(PT_SRC)

include DLD-FUNCTIONS/module.mk

$(srcdir)/DLD-FUNCTIONS/module.mk: $(srcdir)/DLD-FUNCTIONS/config-module.sh $(srcdir)/DLD-FUNCTIONS/config-module.awk $(srcdir)/DLD-FUNCTIONS/module-files
	$(srcdir)/DLD-FUNCTIONS/config-module.sh $(top_srcdir)

include OPERATORS/module.mk
include TEMPLATE-INST/module.mk

if AMCOND_ENABLE_DYNAMIC_LINKING
  OCT_FILES = $(DLD_FUNCTIONS_LIBS:.la=.oct)
  OCT_STAMP_FILES = $(subst DLD-FUNCTIONS/,DLD-FUNCTIONS/$(am__leading_dot),$(DLD_FUNCTIONS_LIBS:.la=.oct-stamp))
  DLD_LIBOCTINTERP_LIBADD = liboctinterp.la
else
  OCT_FILES =
  OCT_STAMP_FILES =
  DLD_LIBOCTINTERP_LIBADD =
endif

liboctinterp_la_SOURCES = \
  $(DIST_SRC) \
  $(OPERATORS_SRC) \
  $(TEMPLATE_INST_SRC)

nodist_liboctinterp_la_SOURCES = \
  builtins.cc \
  defaults.h \
  graphics.h \
  mxarray.h \
  oct-conf.h \
  oct-errno.cc \
  ops.cc \
  version.h \
  $(OPT_INC)

liboctinterp_la_CPPFLAGS = @OCTINTERP_DLL_DEFS@ $(AM_CPPFLAGS)

include link-deps.mk

liboctinterp_la_LIBADD = \
  ../liboctave/liboctave.la \
  ../libcruft/libcruft.la \
  $(LIBOCTINTERP_LINK_DEPS)

# Increment these as needed and according to the rules in the libtool manual:
liboctinterp_current = 1
liboctinterp_revision = 1
liboctinterp_age = 0

liboctinterp_version_info = $(liboctinterp_current):$(liboctinterp_revision):$(liboctinterp_age)

liboctinterp_la_LDFLAGS = \
  -version-info $(liboctinterp_version_info) \
  $(NO_UNDEFINED_LDFLAG) \
  -bindir $(bindir) \
  $(LIBOCTINTERP_LINK_OPTS)

display.df display.lo: CPPFLAGS += $(X11_FLAGS)

octave_SOURCES = main.c

octave_LDADD = \
  liboctinterp.la \
  ../liboctave/liboctave.la \
  ../libcruft/libcruft.la \
  $(OCTAVE_LINK_DEPS)

octave_LDFLAGS = \
  $(NO_UNDEFINED_LDFLAG) \
  $(OCTAVE_LINK_OPTS)

## Section for defining and creating DEF_FILES
SRC_DEF_FILES := $(shell $(srcdir)/find-defun-files.sh "$(srcdir)" $(DIST_SRC))

DLD_FUNCTIONS_DEF_FILES = $(DLD_FUNCTIONS_SRC:.cc=.df)

## builtins.cc depends on $(DEF_FILES), so DEF_FILES should only include
## .df files that correspond to sources included in liboctave.
if AMCOND_ENABLE_DYNAMIC_LINKING
  DEF_FILES = $(SRC_DEF_FILES)
else
  DEF_FILES = $(SRC_DEF_FILES) $(DLD_FUNCTIONS_DEF_FILES)
endif

ALL_DEF_FILES = $(SRC_DEF_FILES) $(DLD_FUNCTIONS_DEF_FILES)

$(SRC_DEF_FILES): mkdefs Makefile

$(DEF_FILES): $(OPT_HANDLERS) $(OPT_INC)

DLL_CDEFS = @OCTINTERP_DLL_DEFS@
DLL_CXXDEFS = @OCTINTERP_DLL_DEFS@

## Rule to build a DEF file from a .cc file
%.df: %.cc
	$(CXXCPP) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
	  $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) \
	  -DMAKE_BUILTINS $< | $(srcdir)/mkdefs $(srcdir) $< > $@-t
	mv $@-t $@

## Special rules:
## Mostly for sources which must be built before rest of compilation.

## defaults.h and oct-conf.h must depend on Makefile.  Calling configure
## may change default/config values.  However, calling configure will also
## regenerate the Makefiles from Makefile.am and trigger the rules below.
defaults.h: defaults.h.in Makefile
	@$(do_subst_default_vals)

graphics.h: graphics.h.in genprops.awk Makefile
	$(AWK) -f $(srcdir)/genprops.awk $< > $@-t
	mv $@-t $@

oct-conf.h: oct-conf.h.in Makefile
	@$(do_subst_config_vals)

## Don't use a pipeline to process gperf output since if gperf
## is missing but sed is not, the exit status of the pipeline
## will still be success and we will end up creating an empty
## oct-gperf.h file.
oct-gperf.h: octave.gperf
	$(GPERF) -t -C -D -G -L C++ -Z octave_kw_hash $< > $@-t1
	$(SED) 's,lookup\[,gperf_lookup[,' < $@-t1 > $@-t
	mv $@-t $@
	rm -f $@-t1

mxarray.h: mxarray.h.in Makefile
	$(SED) < $< \
	  -e "s|%OCTAVE_IDX_TYPE%|${OCTAVE_IDX_TYPE}|" > $@-t
	mv $@-t $@

version.h: version.h.in Makefile
	$(SED) < $< \
	  -e "s|%OCTAVE_API_VERSION_NUMBER%|${OCTAVE_API_VERSION_NUMBER}|" \
	  -e "s|%OCTAVE_API_VERSION%|\"${OCTAVE_API_VERSION}\"|" \
	  -e "s|%OCTAVE_COPYRIGHT%|\"${OCTAVE_COPYRIGHT}\"|" \
	  -e "s|%OCTAVE_RELEASE_DATE%|\"${OCTAVE_RELEASE_DATE}\"|" \
	  -e "s|%OCTAVE_VERSION%|\"${OCTAVE_VERSION}\"|" > $@-t
	mv $@-t $@

builtins.cc: $(DEF_FILES) mkbuiltins
	$(srcdir)/mkbuiltins $(DEF_FILES) > $@-t
	mv $@-t $@

graphics-props.cc: graphics.h.in genprops.awk Makefile
	$(AWK) -v emit_graphics_props=1 -f $(srcdir)/genprops.awk $< > $@-t
	mv $@-t $@

ops.cc: $(OPERATORS_SRC) mkops
	$(srcdir)/mkops $(OPERATORS_SRC) > $@-t
	mv $@-t $@

oct-errno.cc: oct-errno.cc.in Makefile
	if test -n "$(PERL)"; then \
	  $(srcdir)/mk-errno-list --perl "$(PERL)" < $< > $@-t; \
	elif test -n "$(PYTHON)"; then \
	  $(srcdir)/mk-errno-list --python "$(PYTHON)" < $< > $@-t; \
	else \
	  $(SED) '/@SYSDEP_ERRNO_LIST@/D' $< > $@-t; \
	fi
	mv $@-t $@

$(OPT_HANDLERS) : %.cc : $(top_srcdir)/liboctave/%.in $(top_srcdir)/build-aux/mk-opts.pl
	$(PERL) $(top_srcdir)/build-aux/mk-opts.pl --opt-handler-fcns $< > $@-t
	mv $@-t $@

$(OPT_INC) : %.h : %.in
	$(MAKE) -C $(@D) $(@F)

if AMCOND_ENABLE_DYNAMIC_LINKING
DLD_FUNCTIONS_PKG_ADD_FILE = DLD-FUNCTIONS/PKG_ADD

DLD-FUNCTIONS/PKG_ADD: $(DLD_FUNCTIONS_DEF_FILES) mk-pkg-add
	$(srcdir)/mk-pkg-add $(DLD_FUNCTIONS_DEF_FILES) > $@-t
	mv $@-t $@
endif

lex.lo lex.o oct-parse.lo oct-parse.o: \
  AM_CXXFLAGS := $(filter-out -Wold-style-cast, $(AM_CXXFLAGS))

__fltk_uigetfile__.lo __fltk_uigetfile__.o: \
  AM_CXXFLAGS := $(filter-out $(DLL_CXXDEFS), $(AM_CXXFLAGS) $(GRAPHICS_CFLAGS))

__init_fltk__.lo __init_fltk__.o: \
  AM_CXXFLAGS := $(filter-out $(DLL_CXXDEFS), $(AM_CXXFLAGS) $(GRAPHICS_CFLAGS))

.DOCSTRINGS: gendoc$(BUILD_EXEEXT)
	if [ "x$(srcdir)" != "x." ] && [ -f $(srcdir)/DOCSTRINGS ] && [ ! -f DOCSTRINGS ]; then \
		cp $(srcdir)/DOCSTRINGS DOCSTRINGS; \
		touch -r $(srcdir)/DOCSTRINGS DOCSTRINGS; \
	fi
	@echo "creating .DOCSTRINGS from .cc source files"
	@./gendoc > $@
	$(top_srcdir)/build-aux/move-if-change $@ DOCSTRINGS
	touch $@

doc-files: $(ALL_DEF_FILES)
	echo $(ALL_DEF_FILES) > $@-t
	mv $@-t $@

gendoc.cc: doc-files mkgendoc
	$(srcdir)/mkgendoc doc-files > $@-t
	mv $@-t $@

gendoc$(BUILD_EXEEXT): gendoc.cc
	$(BUILD_CXX) $(BUILD_CXXFLAGS) -o $@ $^ $(BUILD_LDFLAGS)

all-local: $(OCT_STAMP_FILES) $(DLD_FUNCTIONS_PKG_ADD_FILE) .DOCSTRINGS

if AMCOND_BUILD_COMPILED_AUX_PROGRAMS
octave-config.cc: octave-config.cc.in Makefile
	@$(do_subst_default_vals)

mkoctfile.cc: mkoctfile.cc.in Makefile
	@$(do_subst_config_vals)
else
octave-config: octave-config.in Makefile
	@$(do_subst_default_vals)
	chmod a+rx $@

mkoctfile: mkoctfile.in Makefile
	@$(do_subst_config_vals)
	chmod a+rx $@
endif

install-exec-hook: make-version-links

install-data-hook: install-oct

uninstall-local: remove-version-links uninstall-oct

make-version-links:
	cd $(DESTDIR)$(bindir) && \
	for f in $(basename $(bin_PROGRAMS)); do \
	  mv $$f$(EXEEXT) $$f-$(version)$(EXEEXT) && \
	    $(LN_S) $$f-$(version)$(EXEEXT) $$f$(EXEEXT); \
	done
if ! AMCOND_BUILD_COMPILED_AUX_PROGRAMS
	cd $(DESTDIR)$(bindir) && \
	for f in $(basename $(bin_SCRIPTS)); do \
	  mv $$f $$f-$(version) && \
	    $(LN_S) $$f-$(version) $$f; \
	done
endif

remove-version-links:
	for f in $(basename $(bin_PROGRAMS)); do \
	  rm -f $(DESTDIR)$(bindir)/$$f-$(version)$(EXEEXT); \
	done
if ! AMCOND_BUILD_COMPILED_AUX_PROGRAMS
	for f in $(basename $(bin_SCRIPTS)); do \
	  rm -f $(DESTDIR)$(bindir)/$$f-$(version); \
	done
endif

.PHONY: make-version-links remove-version-links

if AMCOND_ENABLE_DYNAMIC_LINKING
install-oct:
	$(top_srcdir)/build-aux/mkinstalldirs $(DESTDIR)$(octfiledir)
	if [ -n "`cat $(DLD_FUNCTIONS_PKG_ADD_FILE)`" ]; then \
	  $(INSTALL_DATA) $(DLD_FUNCTIONS_PKG_ADD_FILE) $(DESTDIR)$(octfiledir)/PKG_ADD; \
	fi
	cd $(DESTDIR)$(octlibdir) && \
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
	    rm -f $$f $$lnames $$dl; \
	  fi \
	done

uninstall-oct:
	for f in $(notdir $(OCT_FILES)); do \
	  rm -f $(DESTDIR)$(octfiledir)/$$f; \
	done
	rm -f $(DESTDIR)$(octfiledir)/PKG_ADD
endif
.PHONY: install-oct uninstall-oct

CLEANFILES = \
  $(bin_SCRIPTS) \
  $(DLD_FUNCTIONS_PKG_ADD_FILE) \
  doc-files \
  gendoc.cc \
  gendoc$(BUILD_EXEEXT) \
  graphics-props.cc \
  oct-parse.output

DISTCLEANFILES = \
  .DOCSTRINGS \
  DOCSTRINGS \
  $(BUILT_NODISTFILES) \
  $(OCT_FILES) \
  $(OCT_STAMP_FILES)

MAINTAINERCLEANFILES = \
  $(BUILT_DISTFILES)
