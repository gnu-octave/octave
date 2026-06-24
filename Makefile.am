# Makefile for Octave

########################################################################
##
## Copyright (C) 1993-2026 The Octave Project Developers
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

# This Makefile *requires* GNU Make features.

## NOTE: Comments that begin with "##" document Automake code.
##       Comments that begin with '#' are passed through to Makefile

# Delete partially-formed files if a rule fails.
.DELETE_ON_ERROR:

# Exported variables (from configure) can be used in shell scripts as $PROG.

export AWK
export EGREP
export GREP
export FIND
export SED
export SHELL
export PERL
export TAR_OPTIONS

version := ${OCTAVE_VERSION}
api_version := ${OCTAVE_API_VERSION}

# Lex flags

AM_LFLAGS := @LFLAGS@

# Define modern Bison rules until they are handled by autoconf/automake.

AM_V_BISON = $(am__v_BISON_$(V))
am__v_BISON_ = $(am__v_BISON_$(AM_DEFAULT_VERBOSITY))
am__v_BISON_0 = @echo "  BISON   " $@;
am__v_BISON_1 =

BISON := @BISON@
BISONCOMPILE := $(BISON) $(AM_BISONFLAGS) $(BISONFLAGS)
# Define YACC to pacify automake
YACC := $(BISON)

# Control whether symbols are visible by default across all compilers

if AMCOND_LIB_VISIBILITY_FLAGS
	OCTAVE_VISIBILITY_FLAGS := ${CFLAG_VISIBILITY}
endif

# Fortran compiler flags

AM_FFLAGS := ${FPICFLAG} @FFLAGS@ $(OCTAVE_VISIBILITY_FLAGS)

# C compiler flags

AM_CFLAGS := ${CPICFLAG} ${XTRA_CFLAGS} ${WARN_CFLAGS} $(OCTAVE_VISIBILITY_FLAGS)

# C++ compiler flags

AM_CXXFLAGS := ${CXXPICFLAG} ${XTRA_CXXFLAGS} ${WARN_CXXFLAGS} $(OCTAVE_VISIBILITY_FLAGS)

ADDRESS_SANITIZER_ENABLED := @ADDRESS_SANITIZER_ENABLED@
ADDRESS_SANITIZER_OPTIONS := @ADDRESS_SANITIZER_OPTIONS@

FFTW_XCPPFLAGS := @FFTW_XCPPFLAGS@
FFTW_XLDFLAGS := @FFTW_XLDFLAGS@
FFTW_XLIBS := @FFTW_XLIBS@

SPARSE_XCPPFLAGS := @SPARSE_XCPPFLAGS@
SPARSE_XLDFLAGS := @SPARSE_XLDFLAGS@
SPARSE_XLIBS := @SPARSE_XLIBS@

# Linker flags and dependencies

# Suppress unnecessary warning from clang when compling with PTHREAD library.
if AMCOND_CLANG_PTHREAD_WARNING
  AM_LDFLAGS = -Wc,-Qunused-arguments
endif

GNULIB_LINK_DEPS := @GNULIB_LINK_DEPS@

LIBOCTAVE_LINK_DEPS := @LIBOCTAVE_LINK_DEPS@
LIBOCTAVE_LINK_OPTS := @LIBOCTAVE_LINK_OPTS@

LIBOCTINTERP_LINK_DEPS := @LIBOCTINTERP_LINK_DEPS@
LIBOCTINTERP_LINK_OPTS := @LIBOCTINTERP_LINK_OPTS@

LIBOCTGUI_LINK_DEPS := @LIBOCTGUI_LINK_DEPS@
LIBOCTGUI_LINK_OPTS := @LIBOCTGUI_LINK_OPTS@

OCTAVE_LINK_DEPS := @OCTAVE_LINK_DEPS@
OCTAVE_LINK_OPTS := @OCTAVE_LINK_OPTS@

OCTAVE_GUI_LINK_DEPS := @OCTAVE_GUI_LINK_DEPS@
OCTAVE_GUI_LINK_OPTS := @OCTAVE_GUI_LINK_OPTS@

# DEPS for linking .oct files
OCT_LINK_DEPS := @OCT_LINK_DEPS@
OCT_LINK_OPTS := @OCT_LINK_OPTS@

# Options used for creating the source distribution.

GZIP_ENV = '--best --no-name'
## XZ_OPT values have been tuned after benchmarking (2026-06-04).
XZ_OPT = -7 -T0
export XZ_OPT

# Make reproducible builds by seting modification time of all files
# to that of the last HG changeset.
SOURCE_MTIME := \
  $(shell $(SHELL) $(top_srcdir)/build-aux/get-source-mtime.sh "$(srcdir)")
TAR_OPTIONS := $(REPRODUCIBLE_TAR_FLAGS) --mtime=@$(SOURCE_MTIME)

# The arguments passed to configure.

CONFIG_SUBDIRS := @subdirs@

## Ensure that if configure expands @ldpreloadsep to nothing then the result is
## still defined, just empty.  Result will be exactly one space character.
null :=
ldpreloadsep := ${null}@ldpreloadsep@${null}

## The file HG-ID contains the Mercurial ID of the sources used to build
## Octave.  This ID is included by default in all built libraries unless
## the configure option --disable-hg-id was given.
## The rule to make HG-ID is executed each time Make runs.
if AMCOND_ENABLE_HG_ID
HG-ID: FORCE
	$(AM_V_GEN)rm -f $@-t && \
	$(SHELL) $(top_srcdir)/build-aux/mk-hg-id.sh "$(srcdir)" > $@-t && \
	$(simple-move-if-change-rule)
else
HG-ID: FORCE
	@rm -f $@-t && \
	echo "hg-id-disabled" > $@-t && \
	$(simple-move-if-change-rule)
endif

## We cannot use .PHONY on target HG-ID because the file is a prerequisite
## for build-info.cc rules which would cause them to always fire, rather than
## just when timestamp on HG-ID is updated.
FORCE:

HG_ID_VAR = $(shell cat ${top_builddir}/HG-ID)

## Used by gnulib, not necessary for Octave (2026-06-24).
# AUTOCONF_SUBST_VARS := @AUTOCONF_SUBST_VARS@

## Directories which have built sources must be created for VPATH builds
## *before* Makefile rules begin creating files.  We accomplish this by
## creating a 0B file in the required directory which can then be used as a
## prerequisite for build rules.

# Directory stamp for directories with built sources
octave_dirstamp := $(am__leading_dot)octave-dirstamp

DIRSTAMP_FILES := $(octave_dirstamp)

$(DIRSTAMP_FILES):
	$(AM_V_GEN)$(MKDIR_P) $(@D) && \
	: > $@

## Initialize variables before reading in Automake fragments for individual
## directories.

## Source code variables
BUILT_SOURCES =
BUILT_DISTFILES =

OCT_FILES =
OCT_FILE_LIBS =
OCT_FILE_PKG_ADD_FILES =

FCN_FILES =
FCN_FILE_DIRS =
PKG_ADD_FILES =
SCRIPTS_IMAGES =
JAR_FILES =
DOCSTRING_FILES =
## This holds the list of built documentation targets.
## Prerequisite for 'make all'
DOC_TARGETS =

## Executable variables
bin_PROGRAMS =
archlib_PROGRAMS =
OCTAVE_VERSION_LINKS =
OCTAVE_CROSS_TOOLS =

## Library and Header variables
octlib_LTLIBRARIES =
noinst_LTLIBRARIES =

noinst_HEADERS =
octinclude_HEADERS =
nodist_octinclude_HEADERS =

## Distribution variables
EXTRA_DIST =

image_DATA =
octdata_DATA =
octdoc_DATA =
octetc_DATA =
octfonts_DATA =
octlocale_DATA =

DIST_SRC =

# Octave-created automake primitive for test files
TEST_FILES =
noinst_TEST_FILES =

## Clean variables
CLEANFILES =
DISTCLEANFILES =
MAINTAINERCLEANFILES =

## Makefile-related variables

# List of targets required for 'make all'
ALL_LOCAL_TARGETS =
# List of targets required for executable octave binary
OCTAVE_INTERPRETER_TARGETS =

## Subdirectories in which to run make recursively.
## Other directories below toplevel dir are handled directly from this Makefile.
## Review the included module.mk makefile fragments to understand build process.
## Order is significant for SUBDIRS.
SUBDIRS = libgnu . test

include liboctave/module.mk
include liboctinterp/module.mk
include liboctmex/module.mk
include liboctgui/module.mk
include src/module.mk
include scripts/module.mk
include doc/module.mk
include etc/module.mk
include examples/module.mk
include m4/module.mk
include build-aux/module.mk

# Octave distributes so many files that Make overflows the maximum string
# length of 131072 (hardcoded in kernel) when attempting to pass this file list
# to the shell.  "make dist" is the only target with this problem.
# HACK: Override Automake-generated DISTFILES variable with a list of the files
# that have the $srcdir prefix stripped and the $top_srcdir prefix replaced
# with $top_builddir.  This is the first action taken by the distdir-am target.
# The resuling file list is ~4.6KB smaller which is enough to work.
# Alternatively, only run "make dist" from in-source-tree builds.
DISTFILES_RAW := $(DIST_COMMON) $(DIST_SOURCES) $(TEXINFOS) $(EXTRA_DIST)

DISTFILES := $(patsubst $(top_srcdir)/%,$(top_builddir)/, $(patsubst $(srcdir)/%,%,$(DISTFILES_RAW)))

## Order of list is order of execution (still subject to running in parallel).
## Do not alphabetize.
## .WAIT block forces jobs above to complete before more expensive dist-hook
DIST_HOOKS := \
  hg-id-dist-hook \
  docs-dist-hook \
  icons-dist-hook \
  scripts-dist-hook \
  metainfo-dist-hook \
  .WAIT \
  fix-file-perms-dist-hook

dist-hook: $(DIST_HOOKS)
.PHONY: $(DIST_HOOKS)

if AMCOND_ENABLE_HG_ID
hg-id-dist-hook:
	@if [ -z "$(DIST_IGNORE_HG_STATE)" ]; then \
	  if ! echo $(HG_ID_VAR) | $(GREP) '^[0-9a-f]\{12\}$$' >/dev/null 2>&1 ; then \
	    echo 1>&2 ""; \
	    echo 1>&2 "Packaging distribution requires a clean hg working tree with no uncommitted changes."; \
	    echo 1>&2 "Please commit or revert your changes first, or pass DIST_IGNORE_HG_STATE=1"; \
	    echo 1>&2 "Cannot package distribution!"; \
	    echo 1>&2 ""; \
	    exit 1; \
	  fi; \
	fi
else
hg-id-dist-hook:
	@echo 1>&2 "Octave was configured with --disable-hg-id.  Cannot package distribution!"; \
	exit 1;
endif

if AMCOND_BUILD_DOCS
# Prevent packaging of distribution unless all libraries necessary to create
# documentation are present.
docs-dist-hook:
	@$(GREP) '#define HAVE_COLAMD 1' $(top_builddir)/config.h > /dev/null \
	  || { echo 1>&2 "Documentation creation requires missing COLAMD library.  Cannot package distribution!"; \
	       exit 1; }
	@$(GREP) '#define HAVE_CHOLMOD 1' $(top_builddir)/config.h > /dev/null \
	  || { echo 1>&2 "Documentation creation requires missing CHOLMOD library.  Cannot package distribution!"; \
	       exit 1; }
	@$(GREP) '#define HAVE_UMFPACK 1' $(top_builddir)/config.h > /dev/null \
	  || { echo 1>&2 "Documentation creation requires missing UMFPACK library.  Cannot package distribution!"; \
	       exit 1; }
	@$(GREP) '#define HAVE_QHULL 1' $(top_builddir)/config.h > /dev/null \
	  || { echo 1>&2 "Documentation creation requires missing QHULL library.  Cannot package distribution!"; \
	       exit 1; }
else
docs-dist-hook:
	@echo 1>&2 "Documentation disabled.  Cannot package distribution!"; \
	exit 1;
endif

if AMCOND_HAVE_ICON_TOOLS
icons-dist-hook:
else
icons-dist-hook:
	@echo 1>&2 "Packaging distribution requires icotool and rsvg-convert."; \
	exit 1;
endif

if AMCOND_HAVE_JAVA
scripts-dist-hook:
else
scripts-dist-hook:
	@echo 1>&2 "Packaging distribution requires Java."; exit 1;
endif

## Ensure file permissions are consistent on all files included in the
## distribution.  Automake takes care of normalizing some permissions of
## normal files and directories.  BUT, Automake does not ensure that files
## don't have unnecessarily lax write permissions.  It also does not ensure
## that executable permissions are set for group and other users.
fix-file-perms-dist-hook:
	-chmod -R go-w "$(distdir)"
	-$(FIND) "$(distdir)" -type f -perm -100 -exec chmod a+rx {} +


BUILT_SOURCES += \
  octave-config.h \
  run-octave \
  $(DIRSTAMP_FILES)

noinst_SCRIPTS = run-octave

## FIXME: 2026-05-22: Not used by Makefile (configure does!).
## It gets embedded in Makefile, so one can review later, but this information
## is also in configure.ac.
#CONFIG_FILES := @ac_config_headers@ @ac_config_files@

nodist_octinclude_HEADERS += \
  oct-conf-post-public.h \
  octave-config.h

OCTAVE_INTERPRETER_TARGETS += \
  $(OCT_FILE_PKG_ADD_FILES)

ALL_LOCAL_TARGETS += \
  $(OCTAVE_INTERPRETER_TARGETS) \
  $(DOC_TARGETS) \
  .gdbinit

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

run-octave: run-octave.in build-aux/subst-script-vals.sh | $(octave_dirstamp)
	$(AM_V_GEN)$(call simple-filter-rule,build-aux/subst-script-vals.sh) && \
	chmod a+rx $@

octave-config.h: config.h build-aux/mk-octave-config-h.sh | $(octave_dirstamp)
	$(AM_V_at)rm -f $@-t && \
	$(SHELL) $(srcdir)/build-aux/mk-octave-config-h.sh $< > $@-t && \
	$(simple-gen-if-change-rule)

.gdbinit: etc/gdbinit
	$(AM_V_GEN)if [ -f $@ ] && ! cmp -s $< $@; then \
	  echo 1>&2 "refusing to overwrite $@ with newer version from $<"; \
	else \
	  cp $< $@; \
	fi

## Command replaces target file with newer file, but only if change detected.
## Prevents unnecessary rebuilds when the timestamp on a key file, e.g.
## "config.h", changes, but there is no actual change to file.
## However, if no move occurs then target will always be out-of-date and
## will be remade on every invocation of make.  This can be a problem if the
## rule to remake the target is high-cost.
define move-if-change-rule
  if [ -s "$(1)" ]; then \
    ${SHELL} ${top_srcdir}/build-aux/move-if-change "$(1)" "$(2)"; \
  else \
    echo 1>&2 "$(1) is empty!"; \
    rm -f "$(1)"; \
    exit 1; \
  fi
endef

## Convenience wrapper to call move-if-change-rule with default filenames
## used in Octave Makefile rules: $@-t = temporary file, $@ = target.
define simple-move-if-change-rule
  $(call move-if-change-rule,$@-t,$@)
endef

## Command replaces target file with newer file and emits message "GEN FILE",
## but only if change detected.  Prevents unnecessary rebuilds when the
## timestamp on a key file changes, but there is no actual change to file.
## This is "move-if-change-rule" with the addition of output message if a
## change occurs.
## NOTE: If move fails it may have been because of parallel build.  In that
## case, verify files are the same and remove the intermediate @-t file.
define simple-gen-if-change-rule
  if test -r "$@" && cmp -s "$@-t" "$@" ; then \
    rm -f -- "$@-t" ; \
  else \
    echo "  GEN     " $@ ; \
    if mv -f -- "$@-t" "$@"; then :; else \
      test -r "$@" && cmp -s "$@-t" "$@" && rm -f -- "$@-t" ; \
    fi \
  fi
endef

## Commands to embed HG-ID in libraries and binaries.
define lib-build-info-commands
  rm -f $@-t && \
  $(SED) \
    -e "s|%NO_EDIT_WARNING%|DO NOT EDIT!  Generated automatically by Makefile.|" \
    -e "s|%OCTAVE_HG_ID%|$(HG_ID_VAR)|" $< > $@-t && \
  mv $@-t $@
endef

define simple-filter-rule
  rm -f $@-t $@ && \
  ${SHELL} $(1) < $< > $@-t && \
  mv $@-t $@
endef

## Commands to extract BIST tests from source file and place in -tst file.
define test-file-commands
  rm -f $@-t $@ && \
  ( echo "## DO NOT EDIT!  Generated automatically from $(<F) by Make."; \
    $(GREP) '^%!' $< \
  ) > $@-t && \
  mv $@-t $@
endef

## Pattern rules to create -tst files from various sources
%.cc-tst : %.cc
	$(AM_V_GEN)$(test-file-commands)

%.yy-tst : %.yy
	$(AM_V_GEN)$(test-file-commands)

%.ll-tst : %.ll
	$(AM_V_GEN)$(test-file-commands)

## NEWS created by copying current versioned NEWS file without
## MAJOR_VERSION number.
NEWS: etc/NEWS.$(OCTAVE_MAJOR_VERSION).md
	$(AM_V_GEN)rm -f $@ && \
	cp $< $@

## Build ordinary ChangeLog from hg log
ChangeLog:
	$(AM_V_GEN)rm -f $@-t && \
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
.PHONY: ChangeLog

octetc_DATA += \
  CITATION \
  NEWS

if AMCOND_INSTALL_BUILD_LOGS
octetc_DATA += config.log
endif

# Extra non-code files to distribute.

# generated files for distribution
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


DIRS_TO_MAKE = \
  $(localfcnfiledir) \
  $(localapifcnfiledir) \
  $(localapipkgdir) \
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

CLEANFILES += \
  run-octave

DISTCLEANFILES += \
  .gdbinit \
  .octave-config.h-t \
  oct-conf-post-private.h \
  oct-conf-post-public.h \
  octave-config.h \
  $(DIRSTAMP_FILES)

MAINTAINERCLEANFILES += \
  $(BUILT_DISTFILES)

clean-local: doc-clean

distclean-local: doc-distclean
	if [ "x${srcdir}" != "x." ]; then rm -f HG-ID; fi

maintainer-clean-local: doc-maintainer-clean

## test/ directory uses it's own Makefile.  Provide targets for cleaning
## which can be accessed from this top-level Makefile.
TEST_CLEAN_TARGETS := $(addprefix test-,clean distclean maintainer-clean)

$(TEST_CLEAN_TARGETS):
	cd test && $(MAKE) $(AM_MAKEFLAGS) $(patsubst test-%,%,$@)
.PHONY: $(TEST_CLEAN_TARGETS)

## The 'clean-aminfo' target is defined by Automake >= 1.11.  We want to
## distribute all Texinfo docs with the source distribution and not delete
## them on 'clean', so we override this target to do nothing by default.

clean-aminfo:

