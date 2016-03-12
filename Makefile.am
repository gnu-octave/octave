# Makefile for Octave
#
# Copyright (C) 1993-2015 John W. Eaton
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

include build-aux/common.mk

image_DATA =
octdata_DATA =
octetc_DATA =
octlocale_DATA =

ACLOCAL_AMFLAGS = -I m4

DOC_TARGETS =

BUILT_DISTFILES =
BUILT_NODISTFILES =
EXTRA_DIST =

BUILT_DISTFILES += \
  AUTHORS \
  BUGS \
  ChangeLog \
  INSTALL.OCTAVE

EXTRA_DIST += \
  CITATION \
  COPYING \
  INSTALL \
  NEWS \
  README \
  build-aux/OctJavaQry.class \
  build-aux/check-subst-vars.in.sh \
  build-aux/find-files-with-tests.sh \
  build-aux/mk-default-qt-settings.in.sh \
  build-aux/mk-f77-def.in.sh \
  build-aux/mk-mxarray-h.in.sh \
  build-aux/mk-version-h.in.sh \
  build-aux/mk-octave-config-h.sh \
  build-aux/mk-opts.pl \
  build-aux/move-if-change \
  build-aux/stl_algo.h-fixed \
  build-aux/subst-config-vals.in.sh \
  build-aux/subst-cross-config-vals.in.sh \
  build-aux/subst-default-vals.in.sh \
  build-aux/subst-f77-isnan-macro.in.sh \
  build-aux/subst-script-vals.in.sh \
  run-octave.in \
  $(BUILT_DISTFILES)

GEN_CONFIG_SHELL = \
  build-aux/mk-default-qt-settings.sh \
  build-aux/mk-f77-def.sh \
  build-aux/mk-mxarray-h.sh \
  build-aux/mk-version-h.sh \
  build-aux/subst-config-vals.sh \
  build-aux/subst-cross-config-vals.sh \
  build-aux/subst-default-vals.sh \
  build-aux/subst-f77-isnan-macro.sh \
  build-aux/subst-script-vals.sh

$(GEN_CONFIG_SHELL) : %.sh : %.in.sh config.status
	$(AM_V_GEN)$(SHELL) config.status $@-tmp $@

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

SUBDIRS = libgnu

# All of build depends on having libgnu.
# Add the library to BUILT_SOURCES so it is created early in the build process
# This is only a partial solution which works when 'make all' is used.
# See bug #45578.
BUILT_SOURCES += libgnu/libgnu.la

libgnu/libgnu.la:
	cd libgnu; $(MAKE) all

include liboctave/module.mk
include libinterp/module.mk
include libgui/module.mk
include src/module.mk
include scripts/module.mk
include doc/module.mk
include doc/interpreter/images.mk
include etc/module.mk
include examples/module.mk
include m4/module.mk
include test/module.mk

dist-hook: doc-interpreter-dist-hook docs-dist-hook icons-dist-hook scripts-dist-hook

if AMCOND_BUILD_DOCS
docs-dist-hook:
else
docs-dist-hook:
	@echo "Documentation disabled.  Cannot package distribution!" ; exit 1;
endif

if AMCOND_HAVE_ICON_TOOLS
icons-dist-hook:
else
icons-dist-hook:
	@echo "Packaging distribution requires icotool and rsvg-convert." ; exit 1;
endif

BUILT_SOURCES += \
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
  $(BUILT_SOURCES) \
  config-vars \
  make-vars

DISTCLEANFILES += \
  .gdbinit \
  $(DIRSTAMP_FILES)

MAINTAINERCLEANFILES += \
  ChangeLog \
  $(BUILT_DISTFILES)

CONFIG_FILES = @ac_config_headers@ @ac_config_files@

octinclude_HEADERS += oct-conf-post.h
nodist_octinclude_HEADERS += config.h octave-config.h

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
	@$(gdbinit_install_rule)

define changelog-from-hg-log
  rm -f $@-t && \
  if [ -d $(srcdir)/.hg ]; then \
    ( cd $(srcdir); \
      hg log --style=build-aux/changelog.tmpl --prune=b0e60ad4ae26 --only-branch=`hg branch`; \
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

clean-local: doc-clean

distclean-local:

maintainer-clean-local: doc-maintainer-clean

