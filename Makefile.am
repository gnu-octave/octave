# Makefile for Octave

########################################################################
##
## Copyright (C) 1993-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

# This Makefile requires GNU Make features.

export AWK
export GREP
export FIND
export SED
export SHELL
export PERL
export TAR_OPTIONS

version := ${OCTAVE_VERSION}
api_version := ${OCTAVE_API_VERSION}

## AM_LIBTOOLFLAGS = --silent

AM_LFLAGS = @LFLAGS@

# Until modern Bison rules are handled by autoconf/automake.

AM_V_BISON = $(am__v_BISON_$(V))
am__v_BISON_ = $(am__v_BISON_$(AM_DEFAULT_VERBOSITY))
am__v_BISON_0 = @echo "  BISON   " $@;
am__v_BISON_1 =

BISON = @BISON@
BISONCOMPILE = $(BISON) $(AM_BISONFLAGS) $(BISONFLAGS)

# Define YACC to pacify automake

YACC = $(BISON)

if AMCOND_LIB_VISIBILITY_FLAGS
  OCTAVE_VISIBILITY_FLAGS = ${CFLAG_VISIBILITY}
endif

# Fortran compiler flags.

AM_FFLAGS = ${FPICFLAG} @FFLAGS@ $(OCTAVE_VISIBILITY_FLAGS)

# C compiler flags.

AM_CFLAGS = ${CPICFLAG} ${XTRA_CFLAGS} ${WARN_CFLAGS} $(OCTAVE_VISIBILITY_FLAGS)

# ifeq (${INCLUDE_DEPS},no)
#   omit_deps = true;
# endif

# C++ compiler flags.

AM_CXXFLAGS = ${CXXPICFLAG} ${XTRA_CXXFLAGS} ${WARN_CXXFLAGS} $(OCTAVE_VISIBILITY_FLAGS)

ADDRESS_SANITIZER_ENABLED = @ADDRESS_SANITIZER_ENABLED@
ADDRESS_SANITIZER_OPTIONS = @ADDRESS_SANITIZER_OPTIONS@

FFTW_XCPPFLAGS = @FFTW_XCPPFLAGS@
FFTW_XLDFLAGS = @FFTW_XLDFLAGS@
FFTW_XLIBS = @FFTW_XLIBS@

SPARSE_XCPPFLAGS = @SPARSE_XCPPFLAGS@
SPARSE_XLDFLAGS = @SPARSE_XLDFLAGS@
SPARSE_XLIBS = @SPARSE_XLIBS@

GNULIB_LINK_DEPS = @GNULIB_LINK_DEPS@

LIBOCTAVE_LINK_DEPS = @LIBOCTAVE_LINK_DEPS@
LIBOCTAVE_LINK_OPTS = @LIBOCTAVE_LINK_OPTS@

LIBOCTINTERP_LINK_DEPS = @LIBOCTINTERP_LINK_DEPS@
LIBOCTINTERP_LINK_OPTS = @LIBOCTINTERP_LINK_OPTS@

OCTAVE_LINK_DEPS = @OCTAVE_LINK_DEPS@
OCTAVE_LINK_OPTS = @OCTAVE_LINK_OPTS@

OCT_LINK_DEPS = @OCT_LINK_DEPS@
OCT_LINK_OPTS = @OCT_LINK_OPTS@

LIBOCTGUI_LINK_DEPS = @LIBOCTGUI_LINK_DEPS@
LIBOCTGUI_LINK_OPTS = @LIBOCTGUI_LINK_OPTS@

OCTAVE_GUI_LINK_DEPS = @OCTAVE_GUI_LINK_DEPS@
OCTAVE_GUI_LINK_OPTS = @OCTAVE_GUI_LINK_OPTS@

# Options used for creating the source distribution.

GZIP_ENV = '--best --no-name'
SOURCE_MTIME := \
  $(shell $(SHELL) $(top_srcdir)/build-aux/get-source-mtime.sh "$(srcdir)")
TAR_OPTIONS = $(REPRODUCIBLE_TAR_FLAGS) --mtime=@$(SOURCE_MTIME)

# The arguments passed to configure.

CONFIG_SUBDIRS = @subdirs@

null =
ldpreloadsep = ${null}@ldpreloadsep@${null}

image_DATA =
octdata_DATA =
octdoc_DATA =
octetc_DATA =
octfonts_DATA =
octlocale_DATA =

OCT_FILES =
OCT_FILE_LIBS =
OCT_FILE_PKG_ADD_FILES =

DOC_TARGETS =

BUILT_DISTFILES =
BUILT_NODISTFILES =
EXTRA_DIST =

GEN_CONFIG_SHELL =

BUILT_DISTFILES += \
  AUTHORS \
  BUGS \
  ChangeLog \
  HG-ID \
  INSTALL.OCTAVE \
  NEWS

EXTRA_DIST += \
  CITATION \
  COPYING \
  INSTALL \
  README \
  octave.doap \
  run-octave.in \
  $(BUILT_DISTFILES)

AUTOCONF_SUBST_VARS = @AUTOCONF_SUBST_VARS@
ALL_SUBST_VARS = \
  $(AUTOCONF_SUBST_VARS) \
  abs_top_builddir \
  abs_top_srcdir \
  api_version \
  version

DIRSTAMP_FILES =

octave_dirstamp = $(am__leading_dot)octave-dirstamp

$(DIRSTAMP_FILES):
	$(AM_V_GEN)$(MKDIR_P) $(@D) && \
	: > $@

CLEANFILES =
DISTCLEANFILES =
MAINTAINERCLEANFILES =
info_TEXINFOS =
BUILT_SOURCES =
TEST_FILES =
noinst_TEST_FILES =

DOC_IMAGES_SRC =
BUILT_DOC_IMAGES =
BUILT_DOC_IMAGES_EPS =
BUILT_DOC_IMAGES_PDF =
BUILT_DOC_IMAGES_PNG =
BUILT_DOC_IMAGES_TXT =
DOC_IMAGES =
DOC_IMAGES_EPS =
DOC_IMAGES_PDF =
DOC_IMAGES_PNG =
DOC_IMAGES_TXT =

FCN_FILE_DIRS =
FCN_FILES =
GEN_FCN_FILES =
PKG_ADD_FILES =
SCRIPTS_IMAGES =
JAR_FILES =
DOCSTRING_FILES =

bin_PROGRAMS =
archlib_PROGRAMS =
noinst_HEADERS =
nodist_noinst_HEADERS =
OCTAVE_VERSION_LINKS =
OCTAVE_CROSS_TOOLS =
OCTAVE_INTERPRETER_TARGETS =

octlib_LTLIBRARIES =
noinst_LTLIBRARIES =

octinclude_HEADERS =
nodist_octinclude_HEADERS =

DIST_SRC =

ALL_LOCAL_TARGETS =

# Subdirectories in which to run make recursively.
# Other directories are handled directly from this Makefile,
# but also review the included module.mk makefile fragments.

SUBDIRS = libgnu test

# All of build depends on having libgnu.
# Add the library to BUILT_SOURCES so it is created early in the build process
# This is only a partial solution which works when 'make all' is used.
# See bug #45578.
BUILT_SOURCES += libgnu/libgnu.la

libgnu/libgnu.la: oct-conf-post-private.h oct-conf-post-public.h
	cd libgnu && $(MAKE) $(AM_MAKEFLAGS) all

include liboctave/module.mk
include libinterp/module.mk
include libgui/module.mk
include src/module.mk
include scripts/module.mk
include doc/module.mk
include etc/module.mk
include examples/module.mk
include m4/module.mk
include build-aux/module.mk

DIST_HOOKS := \
  doc-interpreter-dist-hook \
  docs-dist-hook \
  fix-file-perms-dist-hook \
  hg-id-dist-hook \
  appdata-dist-hook \
  icons-dist-hook \
  scripts-dist-hook

dist-hook: $(DIST_HOOKS)

if AMCOND_BUILD_DOCS
docs-dist-hook:
else
docs-dist-hook:
	@echo "Documentation disabled.  Cannot package distribution!" ; exit 1;
endif

## Ensure file permissions are consistent on all files included in the
## distribution.  Automake takes care of normalizing some permissions of
## normal files and directories.  Automake does not ensure that files don't
## have unnecessarily lax write permissions.  It also does not ensure that
## executable permissions are set for group and other users.
fix-file-perms-dist-hook:
	-chmod -R go-w "$(distdir)"
	-find "$(distdir)" -type f -perm -100 -exec chmod a+rx {} \;
.PHONY: fix-file-perms-dist-hook

if AMCOND_ENABLE_HG_ID
hg-id-dist-hook:
	@test x"$(DIST_IGNORE_HG_STATE)" != x \
	  || echo $(HG_ID_VAR) | $(GREP) '^[0-9a-f]\{12\}$$' >/dev/null 2>&1 \
	  || { echo ; \
	       echo "Packaging distribution requires a clean hg working tree with no uncommitted changes." ; \
	       echo "Please commit or revert your changes first, or pass DIST_IGNORE_HG_STATE=1." ; \
	       echo "Cannot package distribution!" ; \
	       echo ; exit 1; }
else
hg-id-dist-hook:
	@echo "WARNING: Octave was configured with --disable-hg-id" 1>&2
endif
.PHONY: hg-id-dist-hook

if AMCOND_HAVE_ICON_TOOLS
icons-dist-hook:
else
icons-dist-hook:
	@echo "Packaging distribution requires icotool and rsvg-convert." ; exit 1;
endif

BUILT_SOURCES += \
  oct-conf-post-private.h \
  oct-conf-post-public.h \
  octave-config.h \
  run-octave \
  $(DIRSTAMP_FILES)

if AMCOND_HAVE_BROKEN_STL_ALGO_H
  BUILT_SOURCES += bits/stl_algo.h
else
  BUILT_SOURCES += nonexistent-file
endif

noinst_SCRIPTS = run-octave

CLEANFILES += \
  config-vars \
  make-vars \
  oct-file-pkg-add \
  octave-config.h \
  run-octave

DISTCLEANFILES += \
  $(DIRSTAMP_FILES) \
  $(GEN_CONFIG_INC) \
  $(GEN_CONFIG_SHELL) \
  .gdbinit

MAINTAINERCLEANFILES += \
  $(BUILT_DISTFILES)

CONFIG_FILES = @ac_config_headers@ @ac_config_files@

nodist_octinclude_HEADERS += \
  oct-conf-post-public.h \
  octave-config.h

OCTAVE_INTERPRETER_TARGETS += \
  $(OCT_FILE_PKG_ADD_FILES)

ALL_LOCAL_TARGETS += \
  $(OCTAVE_INTERPRETER_TARGETS) \
  .gdbinit \
  $(DOC_TARGETS)

all-local: $(ALL_LOCAL_TARGETS)
	@echo ""
	@echo "Octave successfully built.  Now choose from the following:"
	@echo ""
	@echo "   ./run-octave    - to run in place to test before installing"
	@echo "   make check      - to run the tests"
	@echo "   make install    - to install (PREFIX=$(prefix))"
	@echo ""
	@echo "   HG ID for this build is \"$(HG_ID_VAR)\""
	@echo ""

run-octave: run-octave.in build-aux/subst-script-vals.sh
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-script-vals.sh) && \
	chmod a+rx $@

octave-config.h: config.h build-aux/mk-octave-config-h.sh
	$(AM_V_GEN)$(SHELL) $(srcdir)/build-aux/mk-octave-config-h.sh $< > $@-t && \
	$(simple_move_if_change_rule)

bits/stl_algo.h: build-aux/stl_algo.h-fixed
	$(AM_V_GEN)$(MKDIR_P) bits && \
	$(INSTALL_HEADER) $< $@

config-vars: $(GEN_CONFIG_SHELL)
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(SED) -n 's/  *"$$/"/; s/^\([A-Za-z_][A-Za-z0-9_]*\)=" *\(.*\)" *$$/\1 \2/p' $^ | sort -u > $@-t && \
	mv $@-t $@

## We always have to create this file because values for Make variables
## may be passed on the command line.

make-vars:
	$(file >$@-t) $(foreach v, $(ALL_SUBST_VARS), $(file >>$@-t,$(v) $(value $(v))))
	$(AM_V_GEN)mv $@-t $@
.PHONY: make-vars

check-subst-vars: build-aux/check-subst-vars.sh make-vars config-vars
	@$(SHELL) -f build-aux/check-subst-vars.sh make-vars config-vars
.PHONY: check-subst-vars

## If we aren't trying to fix stl_algo.h, then try to ensure that
## there isn't a stray copy sitting in the build tree.

nonexistent-file:
	$(AM_V_at)rm -f bits/stl_algo.h
.PHONY: nonexistent-file

.gdbinit: etc/gdbinit
	$(AM_V_GEN)$(gdbinit-install-rule)

NEWS: etc/NEWS.$(OCTAVE_MAJOR_VERSION).md
	$(AM_V_GEN)rm -f $@ && \
	cp $< $@

define changelog-from-hg-log
  rm -f $@-t && \
  if [ -d $(srcdir)/.hg ]; then \
    ( cd $(srcdir); \
      hg log --no-graph --style=build-aux/changelog.tmpl --prune=b0e60ad4ae26 --only-branch=`hg branch`; \
      echo ""; \
      echo "See the files in the directory etc/OLD-ChangeLogs for changes before 2011-04-19"; \
    ) > $@-t && \
    mv $@-t $@; \
  elif [ ! -f $@ ] && [ ! -f $(srcdir)/$@ ]; then \
    echo "Empty ChangeLog generated because no hg log available" > $@-t && \
    mv $@-t $@; \
  fi
endef

ChangeLog:
	$(AM_V_GEN)$(changelog-from-hg-log)
.PHONY: ChangeLog

## The mk-hg-id.sh script will be executed each time Make runs.  It will
## update the HG-ID file in the build tree if it is out of date.  As a side
## effect, HG_ID_VAR is assigned the contents of the file.

if AMCOND_ENABLE_HG_ID
HG_ID_VAR := \
  $(shell $(SHELL) $(top_srcdir)/build-aux/mk-hg-id.sh "$(srcdir)")
else
HG_ID_VAR := \
  $(shell $(SHELL) $(top_srcdir)/build-aux/mk-hg-id.sh "$(srcdir)" --disable)
endif

octetc_DATA += \
  CITATION \
  NEWS

if AMCOND_INSTALL_BUILD_LOGS
octetc_DATA += config.log
endif

DIRS_TO_MAKE = \
  $(localfcnfiledir) \
  $(localapifcnfiledir) \
  $(localverfcnfiledir) \
  $(localoctfiledir) \
  $(localapioctfiledir) \
  $(localveroctfiledir) \
  $(localarchlibdir) \
  $(localapiarchlibdir) \
  $(localverarchlibdir)

installdirs-local:
	$(MKDIR_P) $(addprefix $(DESTDIR), $(DIRS_TO_MAKE))

install-data-local: installdirs-local

oct-file-pkg-add: $(OCT_FILE_PKG_ADD_FILES)
	cat $(OCT_FILE_PKG_ADD_FILES) > $@-t \
	  && mv $@-t $@

install-oct: oct-file-pkg-add
	$(MKDIR_P) $(DESTDIR)$(octfiledir)
	if [ -n "`cat $(OCT_FILE_PKG_ADD_FILES)`" ]; then \
	  $(INSTALL_DATA) oct-file-pkg-add $(DESTDIR)$(octfiledir)/PKG_ADD; \
	fi
	top_build_dir=`pwd` && \
	cd $(DESTDIR)$(octlibdir) && \
	for ltlib in $(OCT_FILE_LIBS); do \
	  f=`echo $$ltlib | $(SED) 's,.*/,,'`; \
	  dl=`$(SED) -n -e "s/dlname='\([^']*\)'/\1/p" < $$top_build_dir/$$ltlib`; \
	  if [ -n "$$dl" ]; then \
	    $(INSTALL_PROGRAM) $$dl $(DESTDIR)$(octfiledir)/`echo $$f | $(SED) 's,^lib,,; s,\.la$$,.oct,'`; \
	  else \
	    echo "error: dlname is empty in $$ltlib!"; \
	    exit 1; \
	  fi; \
	  lnames=`$(SED) -n -e "s/library_names='\([^']*\)'/\1/p" < $$top_build_dir/$$ltlib`; \
	  if [ -n "$$lnames" ]; then \
	    rm -f $$f $$lnames $$dl; \
	  fi \
	done
.PHONY: install-oct

uninstall-oct:
	for f in $(notdir $(OCT_FILES)); do \
	  rm -f $(DESTDIR)$(octfiledir)/$$f; \
	done
	rm -f $(DESTDIR)$(octfiledir)/PKG_ADD
.PHONY: uninstall-oct

clean-local: doc-clean

distclean-local:
	if [ "x${srcdir}" != "x." ]; then rm -f HG-ID; fi

maintainer-clean-local: doc-maintainer-clean

## The 'clean-aminfo' target is defined by Automake >= 1.11.  We want to
## distribute all Texinfo docs with the source distribution and not delete
## them on 'clean', so we override this target to do nothing by default.

clean-aminfo:

define move_if_change_rule
  if [ -s $(1) ]; then \
    ${SHELL} ${top_srcdir}/build-aux/move-if-change $(1) $(2); \
  else \
    echo "$(1) is empty!" 1>&2; \
    rm -f $(1); \
    exit 1; \
  fi
endef

define simple_move_if_change_rule
  $(call move_if_change_rule,$@-t,$@)
endef

define build-info-commands
  rm -f $@-t && \
  $(SED) \
    -e "s|%NO_EDIT_WARNING%|DO NOT EDIT!  Generated automatically by Makefile|" \
    -e "s|%OCTAVE_HG_ID%|$(HG_ID_VAR)|" $< > $@-t && \
  $(simple_move_if_change_rule)
endef

define simple-filter-rule
  rm -f $@-t $@ && \
  ${SHELL} $(1) < $< > $@-t && \
  mv $@-t $@
endef

define gdbinit-install-rule
  if [ -f $@ ] && ! cmp -s $< $@; then \
    echo "refusing to overwrite $@ with newer version from $<" 1>&2; \
  else \
    cp $< $@; \
  fi
endef

define test-file-commands
  rm -f $@-t $@ && \
  ( echo "## DO NOT EDIT!  Generated automatically from $(<F) by Make."; \
    $(GREP) '^%!' $< \
  ) > $@-t && \
  mv $@-t $@
endef

%.cc-tst : %.cc
	$(AM_V_GEN)$(test-file-commands)

%.yy-tst : %.yy
	$(AM_V_GEN)$(test-file-commands)

%.ll-tst : %.ll
	$(AM_V_GEN)$(test-file-commands)
