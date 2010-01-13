# Makefile for octave
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

include common.mk

## Avoid making multiple subdirs in parallel which can lead 
## to a confusing error message stream
.NOTPARALLEL:

ACLOCAL_AMFLAGS = -I m4

SCRIPTS_EXE_SUFFIX = @SCRIPTS_EXE_SUFFIX@

BUILT_DISTFILES = BUGS INSTALL.OCTAVE

EXTRA_DIST = \
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
  README.MSVC \
  README.Windows \
  README.kpathsea \
  ROADMAP \
  SENDING-PATCHES \
  autogen.sh \
  missing \
  mk-opts.pl \
  mkinstalldirs \
  mkoctfile.cc.in \
  mkoctfile.in \
  move-if-change \
  octave-bug.cc.in \
  octave-bug.in \
  octave-config.cc.in \
  octave-config.in \
  octave-sh \
  run-octave.in

include emacs/module.mk
include m4/module.mk

# Subdirectories in which to run `make all'.
SUBDIRS = libgnu libcruft liboctave src scripts @DOCDIR@ examples test

if AMCOND_BUILD_DOCS
else
dist-hook:
	echo "Documentation disabled.  Cannot package distribution!" ; exit 1;
endif

BUILT_SHELL_FILES = \
  mkoctfile$(SCRIPTS_EXE_SUFFIX) \
  octave-config$(SCRIPTS_EXE_SUFFIX) \
  octave-bug$(SCRIPTS_EXE_SUFFIX) \
  run-octave

bin_SCRIPTS = \
  mkoctfile$(SCRIPTS_EXE_SUFFIX) \
  octave-config$(SCRIPTS_EXE_SUFFIX) \
  octave-bug$(SCRIPTS_EXE_SUFFIX)

INFO_FILES = \
  INSTALL.OCTAVE \
  BUGS

CLEANFILES = $(BUILT_SHELL_FILES)

DISTCLEANFILES = $(INFO_FILES) 

CONFIG_FILES = @ac_config_headers@ @ac_config_files@

BUILT_SOURCES = run-octave

nodist_octinclude_HEADERS = config.h

all-local: $(bin_SCRIPTS) $(INFO_FILES)
	@echo ""
	@echo "Octave successfully built.  Now choose from the following:"
	@echo ""
	@echo "   ./run-octave    - to run in place to test before installing"
	@echo "   make check      - to run the tests"
	@echo "   make install    - to install (PREFIX=$(prefix))"
	@echo ""

octave-bug: octave-bug.in Makefile
	@$(do_subst_config_vals)
	chmod a+rx $@

octave-bug.cc: octave-bug.cc.in Makefile
	@$(do_subst_config_vals)

# ifneq ($(EXEEXT),)
# octave-bug$(EXEEXT): octave-bug.o
# 	$(LD_CXX) $(CPPFLAGS) $(ALL_CXXFLAGS) $(RDYNAMIC_FLAG) \
# 	    $(ALL_LDFLAGS) -o $@ octave-bug.o
# endif

octave-config: octave-config.in Makefile
	@$(do_subst_default_vals)
	chmod a+rx $@

octave-config.cc: octave-config.cc.in Makefile
	@$(do_subst_default_vals)

# ifneq ($(EXEEXT),)
# octave-config$(EXEEXT): octave-config.o
# 	$(LD_CXX) $(CPPFLAGS) $(ALL_CXXFLAGS) $(RDYNAMIC_FLAG) \
# 	    $(ALL_LDFLAGS) -o $@ octave-config.o
# endif

mkoctfile: mkoctfile.in Makefile
	@$(do_subst_config_vals)
	chmod a+rx $@

mkoctfile.cc: mkoctfile.cc.in Makefile
	@$(do_subst_config_vals)

# ifneq ($(EXEEXT),)
# mkoctfile$(EXEEXT): mkoctfile.o
# 	$(LD_CXX) $(CPPFLAGS) $(ALL_CXXFLAGS) $(RDYNAMIC_FLAG) \
# 	    $(ALL_LDFLAGS) -o $@ mkoctfile.o
# endif

run-octave: run-octave.in Makefile
	@$(do_subst_script_vals)
	chmod a+rx "$@"

BUGS INSTALL.OCTAVE:
	$(MAKE) -C doc ../$@
.PHONY: BUGS INSTALL.OCTAVE

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

make-version-links:
	cd $(DESTDIR)$(bindir) && \
	for f in $(basename $(bin_SCRIPTS)); do \
	  mv $$f$(EXEEXT) $$f-$(version)$(EXEEXT) && \
	    $(LN_S) $$f-$(version)$(EXEEXT) $$f$(EXEEXT); \
	done
.PHONY: make-version-links

remove-version-links:
	for f in $(basename $(bin_SCRIPTS)); do \
	  rm -f $(DESTDIR)$(bindir)/$$f-$(version)$(EXEEXT); \
	done
.PHONY: remove-version-links

