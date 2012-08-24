# Makefile for octave
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

include build-aux/common.mk

## Avoid making multiple subdirs in parallel which can lead 
## to a confusing error message stream
.NOTPARALLEL:

ACLOCAL_AMFLAGS = -I m4

BUILT_DISTFILES = AUTHORS BUGS ChangeLog INSTALL.OCTAVE

EXTRA_DIST = \
  AUTHORS \
  BUGS \
  COPYING \
  ChangeLog \
  INSTALL \
  INSTALL.OCTAVE \
  NEWS \
  README \
  autogen.sh \
  build-aux/bootstrap \
  build-aux/bootstrap.conf \
  build-aux/mk-opts.pl \
  build-aux/mkinstalldirs \
  build-aux/move-if-change \
  etc/NEWS.1 \
  etc/NEWS.2 \
  etc/NEWS.3 \
  etc/OLD-ChangeLogs/ChangeLog \
  etc/OLD-ChangeLogs/ChangeLog.1 \
  etc/OLD-ChangeLogs/doc-ChangeLog \
  etc/OLD-ChangeLogs/libcruft-ChangeLog \
  etc/OLD-ChangeLogs/liboctave-ChangeLog \
  etc/OLD-ChangeLogs/scripts-ChangeLog \
  etc/OLD-ChangeLogs/src-ChangeLog \
  etc/OLD-ChangeLogs/test-ChangeLog \
  etc/PROJECTS \
  etc/README.Cygwin \
  etc/README.Linux \
  etc/README.MacOS \
  etc/README.MinGW \
  etc/README.Windows \
  etc/README.gnuplot \
  etc/README.kpathsea \
  etc/gdbinit \
  run-octave.in

include m4/module.mk

if AMCOND_BUILD_GUI
GUIDIR = libgui
else
GUIDIR =
endif

# Subdirectories in which to run `make all'.
SUBDIRS = libgnu libcruft liboctave libinterp $(GUIDIR) src scripts @DOCDIR@ examples test

if ! AMCOND_BUILD_DOCS
dist-hook:
	echo "Documentation disabled.  Cannot package distribution!" ; exit 1;
endif

BUILT_SOURCES = run-octave

noinst_SCRIPTS = run-octave

INFO_FILES = \
  AUTHORS \
  BUGS \
  INSTALL.OCTAVE

CLEANFILES = $(BUILT_SOURCES)

DISTCLEANFILES = \
  .gdbinit \
  ChangeLog

MAINTAINERCLEANFILES = $(BUILT_DISTFILES)

CONFIG_FILES = @ac_config_headers@ @ac_config_files@

nodist_octinclude_HEADERS = config.h

all-local: $(noinst_SCRIPTS) $(INFO_FILES) .gdbinit
	@echo ""
	@echo "Octave successfully built.  Now choose from the following:"
	@echo ""
	@echo "   ./run-octave    - to run in place to test before installing"
	@echo "   make check      - to run the tests"
	@echo "   make install    - to install (PREFIX=$(prefix))"
	@echo ""

check: all
	$(MAKE) -C test check

run-octave: run-octave.in Makefile
	@$(do_subst_script_vals)
	chmod a+rx "$@"

.gdbinit: etc/gdbinit
	@if [ -f .gdbinit ]; then \
	  echo "refusing to overwrite .gdbinit with newer version from $<" 1>&2; \
	else \
	  echo "Installing .gdbinit from version at $<" ; \
	  cp $< $@; \
	fi

if AMCOND_BUILD_DOCS
AUTHORS BUGS INSTALL.OCTAVE:
	$(MAKE) -C doc/interpreter ../../$@
endif
.PHONY: AUTHORS BUGS INSTALL.OCTAVE

ChangeLog:
	(cd $(srcdir); hg log --style=build-aux/changelog.tmpl --prune=b0e60ad4ae26 --only-branch=`hg branch`; echo ""; echo "See the files in the directory etc/OLD-ChangeLogs for changes before 2011-04-19") > $@.t
	mv $@.t $@
.PHONY: ChangeLog

octetc_DATA = NEWS

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
