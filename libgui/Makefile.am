# Makefile for Octave's libgui directory
#
# Copyright (C) 2012 John W. Eaton
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

QT_INCDIR = @QT_INCDIR@

QT_LIBDIR = @QT_LIBDIR@

QT_LIBS = -lQtCore -lQtGui -lQtNetwork -lqscintilla2

QT_LDFLAGS = -L$(QT_LIBDIR)

MOC_CPPFLAGS =

octlib_LTLIBRARIES = liboctgui.la

liboctgui_la_CPPFLAGS = \
  @OCTGUI_DLL_DEFS@ \
  -I$(QT_INCDIR) \
  -I$(QT_INCDIR)/QtCore \
  -I$(QT_INCDIR)/QtGui \
  -I$(QT_INCDIR)/QtNetwork \
  -I$(srcdir)/qterminal/libqterminal \
  -Isrc -I$(srcdir)/src \
  -I$(srcdir)/src/m-editor \
  -I$(srcdir)/src/octave-adapter \
  -I$(srcdir)/src/qtinfo \
  -I$(top_srcdir)/liboctave/cruft/misc \
  -I$(top_srcdir)/liboctave/array \
  -I$(top_builddir)/liboctave/numeric -I$(top_srcdir)/liboctave/numeric \
  -I$(top_builddir)/liboctave/operators -I$(top_srcdir)/liboctave/operators \
  -I$(top_srcdir)/liboctave/system \
  -I$(top_srcdir)/liboctave/util \
  -I$(top_srcdir)/libinterp \
  -I$(top_builddir)/libinterp/parse-tree -I$(top_srcdir)/libinterp/parse-tree \
  -I$(top_builddir)/libinterp/interp-core -I$(top_srcdir)/libinterp/interp-core \
  -I$(top_builddir)/libinterp/interpfcn -I$(top_srcdir)/libinterp/interpfcn \
  -I$(top_srcdir)/libinterp/octave-value

EXTRA_DIST = default-qt-settings

BUILT_SOURCES =

CLEANFILES =

noinst_HEADERS =

noinst_LTLIBRARIES =

include src/module.mk
include qterminal-module.mk

include link-deps.mk

liboctgui_la_LIBADD = \
  qterminal/libqterminal.la \
  src/libgui-src.la \
  $(top_builddir)/libinterp/liboctinterp.la \
  $(top_builddir)/liboctave/liboctave.la \
  $(LIBOCTGUI_LINK_DEPS)

# Increment these as needed and according to the rules in the libtool manual:
liboctgui_current = 0
liboctgui_revision = 0
liboctgui_age = 0

liboctgui_version_info = $(liboctgui_current):$(liboctgui_revision):$(liboctgui_age)

liboctgui_la_LDFLAGS = \
  -version-info $(liboctgui_version_info) \
  $(NO_UNDEFINED_LDFLAG) \
  -bindir $(bindir) \
  $(LIBOCTGUI_LINK_OPTS)

octetc_DATA = default-qt-settings

moc-%.cc: %.h
	@MOC@ -o$@ $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) $(MOC_CPPFLAGS) $(liboctgui_la_CPPFLAGS) $<

ui-%.h: %.ui
	@UIC@ -o $@ $<

qrc-%.cc: %.qrc
	@RCC@ -o $@ $<

