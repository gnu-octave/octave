# Makefile for Octave's liboctave directory
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

## Run cruft dir with stand-alone Makefile.
## Eventually this will use module.mk syntax.
SUBDIRS = cruft

## Search local directories before those specified by the user.
AM_CPPFLAGS = \
  -I$(srcdir)/array \
  -I$(srcdir)/cruft/misc \
  -Inumeric -I$(srcdir)/numeric \
  -Ioperators -I$(srcdir)/operators \
  -I$(srcdir)/system \
  -I$(srcdir)/util \
  -I$(top_builddir)/libgnu -I$(top_srcdir)/libgnu

octlib_LTLIBRARIES = liboctave.la

EXTRA_DIST =

DISTCLEANFILES =

BUILT_INCS = \
  operators/mx-ops.h \
  $(OPT_INC) \
  $(MX_OP_INC) \
  $(VX_OP_INC) \
  $(SMX_OP_INC)

BUILT_SOURCES = $(BUILT_INCS)

octinclude_HEADERS = \
  $(ARRAY_INC) \
  $(NUMERIC_INC) \
  $(OPERATORS_INC) \
  $(SYSTEM_INC) \
  $(UTIL_INC) \
  $(OTHER_INC) \
  $(TEMPLATE_SRC)

nodist_octinclude_HEADERS = \
  $(BUILT_INCS)

noinst_LTLIBRARIES =

## C++ files that are #included, not compiled
OTHER_INC =

## C++ files with templates that are #included, not compiled
TEMPLATE_SRC =

include array/module.mk
include numeric/module.mk
include operators/module.mk
include system/module.mk
include util/module.mk

## liboctave merely collects a bunch of compiled convenience libraries.
## It has no source code itself.
liboctave_la_SOURCES = 

## FIXME: SPARSE, FFTW, and ARPACK flags should probably only be in the
##        particular convenience library that needs them.  
liboctave_la_CPPFLAGS = \
  @OCTAVE_DLL_DEFS@ \
  $(SPARSE_XCPPFLAGS) \
  $(FFTW_XCPPFLAGS) \
  $(ARPACK_CPPFLAGS) \
  $(AM_CPPFLAGS)

include link-deps.mk

liboctave_la_LIBADD = \
  array/libarray.la \
  cruft/libcruft.la \
  cruft/ranlib/libranlib.la \
  numeric/libnumeric.la \
  operators/liboperators.la \
  system/libsystem.la \
  util/libutil.la \
  $(top_builddir)/libgnu/libgnu.la \
  $(LIBOCTAVE_LINK_DEPS)

# Increment these as needed and according to the rules in the libtool manual:
liboctave_current = 1
liboctave_revision = 1
liboctave_age = 0

liboctave_version_info = $(liboctave_current):$(liboctave_revision):$(liboctave_age)

liboctave_la_LDFLAGS = \
  -version-info $(liboctave_version_info) \
  $(NO_UNDEFINED_LDFLAG) \
  -bindir $(bindir) \
  $(LIBOCTAVE_LINK_OPTS)

DISTCLEANFILES += $(BUILT_INCS)

