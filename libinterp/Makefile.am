# Makefile for Octave's libinterp directory
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

AUTOMAKE_OPTIONS = subdir-objects

## Search local directories before those specified by the user.
AM_CPPFLAGS = \
  -I$(top_srcdir)/liboctave/cruft/misc \
  -I$(top_srcdir)/liboctave/array \
  -I$(top_builddir)/liboctave/numeric -I$(top_srcdir)/liboctave/numeric \
  -I$(top_builddir)/liboctave/operators -I$(top_srcdir)/liboctave/operators \
  -I$(top_srcdir)/liboctave/system \
  -I$(top_srcdir)/liboctave/util \
  -I$(srcdir)/octave-value \
  -I$(srcdir)/operators \
  -Iparse-tree -I$(srcdir)/parse-tree \
  -Icorefcn -I$(srcdir)/corefcn \
  -I$(top_builddir)/libgnu -I$(top_srcdir)/libgnu

AM_CFLAGS += $(WARN_CFLAGS)

AM_CXXFLAGS += $(WARN_CXXFLAGS)

octlib_LTLIBRARIES = liboctinterp.la

## Order matters here.  Leave builtins.cc last, because it depends on
## $(DEF_FILES), and building those requires all the sources
## (except builtins.cc) to be available.
BUILT_SOURCES = \
  corefcn/mxarray.h \
  corefcn/oct-errno.cc \
  corefcn/defaults.h \
  corefcn/graphics-props.cc \
  corefcn/graphics.h \
  corefcn/oct-tex-lexer.cc \
  corefcn/oct-tex-parser.cc \
  operators/ops.cc \
  parse-tree/lex.cc \
  parse-tree/oct-gperf.h \
  parse-tree/oct-parse.cc \
  oct-conf.h \
  oct-conf-features.h \
  version.h \
  builtin-defun-decls.h \
  builtins.cc

BUILT_DISTFILES = \
  corefcn/oct-tex-parser.h \
  parse-tree/oct-gperf.h \
  parse-tree/oct-parse.h \
  parse-tree/oct-parse.yy

## Files that are created during build process and installed,
## BUT not distributed in tarball.
BUILT_NODISTFILES = \
  corefcn/mxarray.h \
  corefcn/oct-errno.cc \
  corefcn/defaults.h \
  corefcn/graphics.h \
  builtin-defun-decls.h \
  operators/ops.cc \
  oct-conf.h \
  oct-conf-features.h \
  version.h \
  $(OPT_HANDLERS) \
  $(OPT_INC) \
  $(ALL_DEF_FILES) \
  builtins.cc

EXTRA_DIST = \
  Makefile.in \
  DOCSTRINGS \
  config-features.sh \
  find-defun-files.sh \
  gendoc.pl \
  genprops.awk \
  mk-errno-list \
  mk-pkg-add \
  mkbuiltins \
  mkdefs \
  mkops \
  oct-conf.in.h \
  parse-tree/oct-parse.in.yy \
  version.in.h \
  $(BUILT_DISTFILES)

octinclude_HEADERS = \
  corefcn/graphics-props.cc \
  parse-tree/oct-gperf.h \
  builtins.h \
  builtin-defun-decls.h \
  octave.h \
  $(OCTAVE_VALUE_INC) \
  $(PARSE_TREE_INC) \
  $(PARSER_INC) \
  $(OPERATORS_INC) \
  $(COREFCN_INC)

nodist_octinclude_HEADERS = \
  corefcn/mxarray.h \
  corefcn/defaults.h \
  corefcn/graphics.h \
  oct-conf.h \
  version.h

DIST_SRC = \
  octave.cc \
  $(OCTAVE_VALUE_SRC) \
  $(PARSE_TREE_SRC) \
  $(PARSER_SRC) \
  $(COREFCN_SRC)

noinst_LTLIBRARIES =

include parse-tree/module.mk
include octave-value/module.mk
include operators/module.mk
include template-inst/module.mk
include corefcn/module.mk
include dldfcn/module.mk

$(srcdir)/dldfcn/module.mk: $(srcdir)/dldfcn/config-module.sh $(srcdir)/dldfcn/config-module.awk $(srcdir)/dldfcn/module-files
	$(srcdir)/dldfcn/config-module.sh $(top_srcdir)

if AMCOND_ENABLE_DYNAMIC_LINKING
  OCT_FILES = $(DLDFCN_LIBS:.la=.oct)
  OCT_STAMP_FILES = $(subst dldfcn/,dldfcn/$(am__leading_dot),$(DLDFCN_LIBS:.la=.oct-stamp))
  DLD_LIBOCTINTERP_LIBADD = liboctinterp.la
else
  OCT_FILES =
  OCT_STAMP_FILES =
  DLD_LIBOCTINTERP_LIBADD =
endif

liboctinterp_la_SOURCES = \
  octave.cc \
  $(OPERATORS_SRC) \
  $(TEMPLATE_INST_SRC)

nodist_liboctinterp_la_SOURCES = \
  corefcn/mxarray.h \
  corefcn/oct-errno.cc \
  corefcn/defaults.h \
  corefcn/graphics.h \
  operators/ops.cc \
  builtin-defun-decls.h \
  builtins.cc \
  oct-conf.h \
  oct-conf-features.h \
  version.h \
  $(OPT_INC)

liboctinterp_la_CPPFLAGS = @OCTINTERP_DLL_DEFS@ $(AM_CPPFLAGS)

include link-deps.mk

liboctinterp_la_LIBADD = \
  octave-value/liboctave-value.la \
  parse-tree/libparse-tree.la \
  parse-tree/libparser.la \
  corefcn/libcorefcn.la \
  $(top_builddir)/liboctave/liboctave.la \
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

## Section for defining and creating DEF_FILES

ULT_DIST_SRC := \
  $(filter-out parse-tree/oct-parse.yy, $(DIST_SRC)) parse-tree/oct-parse.in.yy

SRC_DEF_FILES := $(shell $(srcdir)/find-defun-files.sh "$(srcdir)" $(ULT_DIST_SRC))

DLDFCN_DEF_FILES = $(DLDFCN_SRC:.cc=.df)

## builtins.cc depends on $(DEF_FILES), so DEF_FILES should only include
## .df files that correspond to sources included in liboctave.
if AMCOND_ENABLE_DYNAMIC_LINKING
  DEF_FILES = $(SRC_DEF_FILES)
else
  DEF_FILES = $(SRC_DEF_FILES) $(DLDFCN_DEF_FILES)
endif

ALL_DEF_FILES = $(SRC_DEF_FILES) $(DLDFCN_DEF_FILES)

$(SRC_DEF_FILES): mkdefs Makefile

$(DEF_FILES): $(OPT_HANDLERS) $(OPT_INC)

## FIXME: The following two variables are deprecated and should be removed
##        in Octave version 3.12.
DLL_CDEFS = @OCTINTERP_DLL_DEFS@
DLL_CXXDEFS = @OCTINTERP_DLL_DEFS@

## Rule to build a DEF file from a .cc file
##
## FIXME -- This rule should be fixed to avoid using special
## flags like JAVA_CPPFLAGS for every file.  How could it be written
## so that it uses octave_value_liboctave_value_la_CPPFLAGS when
## generating the octave-value/*.df files?
## 
## FIXME -- Shouldn't the build stop if CPP fails here?
%.df: %.cc
	$(CXXCPP) $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
	  $(AM_CPPFLAGS) $(JAVA_CPPFLAGS) $(CPPFLAGS) \
	  $(AM_CXXFLAGS) $(CXXFLAGS) \
	  -DMAKE_BUILTINS $< | $(srcdir)/mkdefs $(srcdir) $< > $@-t
	mv $@-t $@

## Rules to build test files

TST_FILES_SRC := $(shell $(top_srcdir)/build-aux/find-files-with-tests.sh "$(srcdir)" $(ULT_DIST_SRC) $(DLDFCN_SRC))

TST_FILES := $(addsuffix -tst,$(TST_FILES_SRC))

libinterptestsdir := $(octtestsdir)/libinterp

nobase_libinterptests_DATA = $(TST_FILES)

## Override Automake's rule that forces a .hh extension on us even
## though we don't want it.  It would be super awesome if automake
## would allow users to choose the header file extension.
.yy.cc:
	$(am__skipyacc) $(SHELL) $(YLWRAP) $< y.tab.c $@ y.tab.h $*.h y.output $*.output -- $(YACCCOMPILE)

## Special rules:
## Mostly for sources which must be built before rest of compilation.

## oct-conf.h must depend on Makefile.
## Calling configure may change default/config values.
## However, calling configure will also regenerate the Makefiles from
## Makefile.am and trigger the rules below.
oct-conf.h: oct-conf.in.h Makefile
	@$(do_subst_config_vals)

oct-conf-features.h: $(top_builddir)/config.h config-features.sh
	$(srcdir)/config-features.sh $< > $@-t
	mv $@-t $@

version.h: version.in.h Makefile
	$(SED) < $< \
	  -e "s|%NO_EDIT_WARNING%|DO NOT EDIT!  Generated automatically from $(<F) by Make.|" \
	  -e "s|%OCTAVE_API_VERSION_NUMBER%|${OCTAVE_API_VERSION_NUMBER}|" \
	  -e "s|%OCTAVE_API_VERSION%|\"${OCTAVE_API_VERSION}\"|" \
	  -e "s|%OCTAVE_COPYRIGHT%|\"${OCTAVE_COPYRIGHT}\"|" \
	  -e "s|%OCTAVE_RELEASE_DATE%|\"${OCTAVE_RELEASE_DATE}\"|" \
	  -e "s|%OCTAVE_VERSION%|\"${OCTAVE_VERSION}\"|" > $@-t
	mv $@-t $@

builtins.cc: $(DEF_FILES) mkbuiltins
	$(srcdir)/mkbuiltins --source $(DEF_FILES) > $@-t
	mv $@-t $@

builtin-defun-decls.h: $(SRC_DEF_FILES) mkbuiltins
	$(srcdir)/mkbuiltins --header $(SRC_DEF_FILES) > $@-t
	mv $@-t $@

if AMCOND_ENABLE_DYNAMIC_LINKING
DLDFCN_PKG_ADD_FILE = dldfcn/PKG_ADD

dldfcn/PKG_ADD: $(DLDFCN_DEF_FILES) mk-pkg-add
	$(srcdir)/mk-pkg-add $(DLDFCN_DEF_FILES) > $@-t
	mv $@-t $@
endif

if AMCOND_BUILD_DOCS
.DOCSTRINGS: $(ALL_DEF_FILES) gendoc.pl
	if [ "x$(srcdir)" != "x." ] && [ -f $(srcdir)/DOCSTRINGS ] && [ ! -f DOCSTRINGS ]; then \
		cp $(srcdir)/DOCSTRINGS DOCSTRINGS; \
		touch -r $(srcdir)/DOCSTRINGS DOCSTRINGS; \
	fi
	@echo "creating .DOCSTRINGS from .cc source files"
	@$(PERL) $(srcdir)/gendoc.pl $(ALL_DEF_FILES) > $@
	$(top_srcdir)/build-aux/move-if-change $@ DOCSTRINGS
	touch $@

all-local: $(OCT_STAMP_FILES) $(DLDFCN_PKG_ADD_FILE) .DOCSTRINGS
else
all-local: $(OCT_STAMP_FILES) $(DLDFCN_PKG_ADD_FILE)
endif

if AMCOND_BUILD_DOCS
install-data-hook: install-oct install-built-in-docstrings
else
install-data-hook: install-oct uninstall-built-in-docstrings
endif

uninstall-local: uninstall-oct uninstall-built-in-docstrings

if AMCOND_ENABLE_DYNAMIC_LINKING
install-oct:
	$(MKDIR_P) $(DESTDIR)$(octfiledir)
	if [ -n "`cat $(DLDFCN_PKG_ADD_FILE)`" ]; then \
	  $(INSTALL_DATA) $(DLDFCN_PKG_ADD_FILE) $(DESTDIR)$(octfiledir)/PKG_ADD; \
	fi
	cd $(DESTDIR)$(octlibdir) && \
	for ltlib in $(DLDFCN_LIBS); do \
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

if AMCOND_BUILD_DOCS
install-built-in-docstrings:
	$(MKDIR_P) $(DESTDIR)$(octetcdir)
	$(INSTALL_DATA) DOCSTRINGS $(DESTDIR)$(octetcdir)/built-in-docstrings

uninstall-built-in-docstrings:
	rm -f $(DESTDIR)$(octetcdir)/built-in-docstrings
endif
.PHONY: install-built-in-docstrings uninstall-built-in-docstrings

CLEANFILES = \
  $(DLDFCN_PKG_ADD_FILE) \
  corefcn/graphics-props.cc \
  parse-tree/oct-parse.output

DISTCLEANFILES = \
  .DOCSTRINGS \
  DOCSTRINGS \
  $(BUILT_NODISTFILES) \
  $(OCT_FILES) \
  $(OCT_STAMP_FILES) \
  $(TST_FILES)

MAINTAINERCLEANFILES = \
  $(BUILT_DISTFILES)

