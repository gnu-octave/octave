# Makefile for Octave's src directory
#
# Copyright (C) 1993-2013 John W. Eaton
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

## Search local directories before those specified by the user.
AM_CPPFLAGS = \
  -I$(top_srcdir)/liboctave/array \
  -I$(top_srcdir)/liboctave/cruft/misc \
  -I$(top_srcdir)/liboctave/numeric \
  -I$(top_srcdir)/liboctave/system \
  -I$(top_srcdir)/liboctave/util \
  -I$(top_srcdir)/libinterp \
  -I$(top_builddir)/libinterp/corefcn -I$(top_srcdir)/libinterp/corefcn \
  -I$(top_builddir)/libgnu -I$(top_srcdir)/libgnu

AM_CFLAGS += $(WARN_CFLAGS)

AM_CXXFLAGS += $(WARN_CXXFLAGS)

EXTRA_DIST = \
  main.in.cc \
  mkoctfile.in.cc \
  mkoctfile.in.sh \
  octave-config.in.cc \
  octave-config.in.sh

DISTCLEANFILES = \
  main.cc

OCTAVE_BINARIES = \
  octave \
  octave-cli

if AMCOND_BUILD_GUI
  OCTAVE_BINARIES += octave-gui
endif

OCTAVE_CORE_LIBS = \
  $(top_builddir)/libinterp/liboctinterp.la \
  $(top_builddir)/liboctave/liboctave.la

include ../libgui/link-deps.mk

nodist_octave_SOURCES = main.cc

octave_LDADD = \
  $(top_builddir)/libgnu/libgnu.la \
  $(GNULIB_LINK_DEPS)

octave_LDFLAGS = \
  $(NO_UNDEFINED_LDFLAG) \
  $(OCTAVE_LINK_OPTS)

octave_cli_SOURCES = main-cli.cc

octave_cli_LDADD = \
  $(OCTAVE_CORE_LIBS)
  $(OCTAVE_LINK_DEPS)

octave_cli_LDFLAGS = \
  $(NO_UNDEFINED_LDFLAG) \
  $(OCTAVE_LINK_OPTS)

if AMCOND_BUILD_GUI
  octave_gui_SOURCES = main-gui.cc
  OCTAVE_GUI_LIBS = $(top_builddir)/libgui/liboctgui.la
  OCTAVE_GUI_CPPFLAGS = -I$(top_srcdir)/libgui/src
endif

octave_gui_CPPFLAGS = \
  $(AM_CPPFLAGS) \
  $(OCTAVE_GUI_CPPFLAGS)

octave_gui_LDADD = \
  $(OCTAVE_GUI_LIBS) \
  $(OCTAVE_CORE_LIBS) \
  $(OCTAVE_GUI_LINK_DEPS)

octave_gui_LDFLAGS = \
  $(NO_UNDEFINED_LDFLAG) \
  $(OCTAVE_GUI_LINK_OPTS)

if AMCOND_BUILD_COMPILED_AUX_PROGRAMS
bin_PROGRAMS = \
  $(OCTAVE_BINARIES) \
  mkoctfile \
  octave-config

mkoctfile_SOURCES =
nodist_mkoctfile_SOURCES = mkoctfile.cc
mkoctfile_LDADD = $(top_builddir)/libgnu/libgnu.la $(LIBS)

octave_config_SOURCES =
nodist_octave_config_SOURCES = octave-config.cc
octave_config_LDADD = \
  $(top_builddir)/libinterp/corefcn/libcorefcn.la \
  $(top_builddir)/libgnu/libgnu.la \
  $(LIBS)

DISTCLEANFILES += mkoctfile.cc octave-config.cc

else
bin_PROGRAMS = \
  $(OCTAVE_BINARIES)

bin_SCRIPTS = \
  mkoctfile \
  octave-config
endif

if AMCOND_BUILD_COMPILED_AUX_PROGRAMS
octave-config.cc: octave-config.in.cc Makefile
	@$(do_subst_default_vals)

mkoctfile.cc: mkoctfile.in.cc Makefile
	@$(do_subst_config_vals)

else
octave-config: octave-config.in.sh Makefile
	@$(do_subst_default_vals)
	chmod a+rx $@

mkoctfile: mkoctfile.in.sh Makefile
	@$(do_subst_config_vals)
	chmod a+rx $@
endif

## main.cc must depend on Makefile.  Calling configure may change
## default/config values.  However, calling configure will also 
## regenerate the Makefiles from Makefile.am and trigger the rules below.
main.cc: main.in.cc Makefile
	@$(do_subst_default_vals)

install-exec-hook: make-version-links

uninstall-local: remove-version-links

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

CLEANFILES = \
  $(bin_SCRIPTS)
