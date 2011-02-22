# Makefile for octave
#
# Copyright (C) 1993-2011 John W. Eaton
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

include common.mk

## Avoid making multiple subdirs in parallel which can lead 
## to a confusing error message stream
.NOTPARALLEL:

ACLOCAL_AMFLAGS = -I m4

BUILT_DISTFILES = AUTHORS BUGS INSTALL.OCTAVE

EXTRA_DIST = \
  AUTHORS \
  BUGS \
  COPYING \
  ChangeLog \
  ChangeLog.1 \
  INSTALL \
  INSTALL.OCTAVE \
  NEWS \
  NEWS.1 \
  NEWS.2 \
  NEWS.3 \
  PROJECTS \
  README \
  README.Cygwin \
  README.Linux \
  README.MacOS \
  README.Windows \
  README.kpathsea \
  autogen.sh \
  bootstrap \
  bootstrap.conf \
  gdbinit \
  missing \
  mk-opts.pl \
  mkinstalldirs \
  mkoctfile.cc.in \
  mkoctfile.in \
  move-if-change \
  octave-config.cc.in \
  octave-config.in \
  octave-sh \
  run-octave.in

include m4/module.mk

# Subdirectories in which to run `make all'.
SUBDIRS = libgnu libcruft liboctave src scripts @DOCDIR@ examples test

if AMCOND_BUILD_DOCS
else
dist-hook:
	echo "Documentation disabled.  Cannot package distribution!" ; exit 1;
endif

if AMCOND_BUILD_COMPILED_AUX_PROGRAMS
bin_PROGRAMS = \
  mkoctfile \
  octave-config

mkoctfile_SOURCES =
nodist_mkoctfile_SOURCES = mkoctfile.cc

octave_config_SOURCES =
nodist_octave_config_SOURCES = octave-config.cc

BUILT_SOURCES = \
  mkoctfile.cc \
  octave-config.cc \
  run-octave

else
bin_SCRIPTS = \
  mkoctfile \
  octave-config

BUILT_SOURCES = \
  run-octave
endif

noinst_SCRIPTS = run-octave

INFO_FILES = \
  AUTHORS \
  BUGS \
  INSTALL.OCTAVE

CLEANFILES = $(bin_PROGRAMS) $(bin_SCRIPTS) $(BUILT_SOURCES)

DISTCLEANFILES = $(INFO_FILES) .gdbinit 

CONFIG_FILES = @ac_config_headers@ @ac_config_files@

nodist_octinclude_HEADERS = config.h

all-local: $(bin_PROGRAMS) $(bin_SCRIPTS) $(noinst_SCRIPTS) $(INFO_FILES) .gdbinit
	@echo ""
	@echo "Octave successfully built.  Now choose from the following:"
	@echo ""
	@echo "   ./run-octave    - to run in place to test before installing"
	@echo "   make check      - to run the tests"
	@echo "   make install    - to install (PREFIX=$(prefix))"
	@echo ""

check: all
	$(MAKE) -C test check

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

run-octave: run-octave.in Makefile
	@$(do_subst_script_vals)
	chmod a+rx "$@"

.gdbinit: gdbinit
	if [ -f .gdbinit ]; then \
	  echo "refusing to overwrite .gdbinit with newer version from $<" 1>&2; \
	else \
	  cp $< $@; \
	fi

AUTHORS BUGS INSTALL.OCTAVE:
	$(MAKE) -C doc/interpreter ../../$@
.PHONY: AUTHORS BUGS INSTALL.OCTAVE

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

install-exec-hook: make-version-links

uninstall-local: remove-version-links

if AMCOND_BUILD_COMPILED_AUX_PROGRAMS
make-version-links:
	cd $(DESTDIR)$(bindir) && \
	for f in $(basename $(bin_PROGRAMS)); do \
	  mv $$f$(EXEEXT) $$f-$(version)$(EXEEXT) && \
	    $(LN_S) $$f-$(version)$(EXEEXT) $$f$(EXEEXT); \
	done

remove-version-links:
	for f in $(basename $(bin_PROGRAMS)); do \
	  rm -f $(DESTDIR)$(bindir)/$$f-$(version)$(EXEEXT); \
	done
else
make-version-links:
	cd $(DESTDIR)$(bindir) && \
	for f in $(basename $(bin_SCRIPTS)); do \
	  mv $$f $$f-$(version) && \
	    $(LN_S) $$f-$(version) $$f; \
	done

remove-version-links:
	for f in $(basename $(bin_SCRIPTS)); do \
	  rm -f $(DESTDIR)$(bindir)/$$f-$(version); \
	done
endif

.PHONY: make-version-links remove-version-links

