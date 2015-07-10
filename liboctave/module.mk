# Makefile for Octave's liboctave directory
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

## Search local directories before those specified by the user.
liboctave_liboctave_la_CPPFLAGS = \
  @OCTAVE_DLL_DEFS@ \
  @CRUFT_DLL_DEFS@ \
  -I$(srcdir)/liboctave/array \
  -I$(srcdir)/liboctave/cruft/misc \
  -Iliboctave/numeric -I$(srcdir)/liboctave/numeric \
  -Iliboctave/operators -I$(srcdir)/liboctave/operators \
  -I$(srcdir)/liboctave/system \
  -I$(srcdir)/liboctave/util \
  -I$(top_builddir)/libgnu -I$(top_srcdir)/libgnu

liboctave_liboctave_la_CFLAGS = $(AM_CFLAGS) $(WARN_CFLAGS)

liboctave_liboctave_la_CXXFLAGS = $(AM_CXXFLAGS) $(WARN_CXXFLAGS)

octlib_LTLIBRARIES += liboctave/liboctave.la

BUILT_INCS = \
  liboctave/operators/mx-ops.h \
  $(LIBOCTAVE_OPT_INC) \
  $(MX_OP_INC) \
  $(VX_OP_INC) \
  $(SMX_OP_INC)

BUILT_SOURCES += $(BUILT_INCS)

octinclude_HEADERS += \
  $(ARRAY_INC) \
  $(CRUFT_INC) \
  $(NUMERIC_INC) \
  $(LIBOCTAVE_OPERATORS_INC) \
  $(SYSTEM_INC) \
  $(UTIL_INC) \
  $(OTHER_INC) \
  $(LIBOCTAVE_TEMPLATE_SRC)

nodist_octinclude_HEADERS += \
  $(BUILT_INCS)

## C++ files that are #included, not compiled
OTHER_INC =

## C++ files with templates that are #included, not compiled
LIBOCTAVE_TEMPLATE_SRC =

## A list of all files that could include tests

liboctave_liboctave_la_LIBADD =

include liboctave/array/module.mk
include liboctave/cruft/module.mk
include liboctave/numeric/module.mk
include liboctave/operators/module.mk
include liboctave/system/module.mk
include liboctave/util/module.mk

## liboctave merely collects a bunch of compiled convenience libraries.
## It has no source code itself.
liboctave_liboctave_la_SOURCES =

# Dummy C++ source to force C++ linking.
nodist_EXTRA_liboctave_liboctave_la_SOURCES = dummy.cc

liboctave_liboctave_la_LIBADD += \
  $(top_builddir)/libgnu/libgnu.la \
  $(LIBOCTAVE_LINK_DEPS)

# Increment these as needed and according to the rules in the libtool manual:
liboctave_liboctave_current = 3
liboctave_liboctave_revision = 0
liboctave_liboctave_age = 0

liboctave_liboctave_version_info = $(liboctave_liboctave_current):$(liboctave_liboctave_revision):$(liboctave_age)

liboctave_liboctave_la_LDFLAGS = \
  -version-info $(liboctave_liboctave_version_info) \
  $(NO_UNDEFINED_LDFLAG) \
  @XTRA_CRUFT_SH_LDFLAGS@ \
  -bindir $(bindir) \
  $(LIBOCTAVE_LINK_OPTS)

## Rules to build test files

LIBOCTAVE_TST_SRC = \
  $(liboctave_array_libarray_la_SOURCES) \
  $(liboctave_numeric_libnumeric_la_SOURCES) \
  $(liboctave_system_libsystem_la_SOURCES) \
  $(liboctave_util_libutil_la_SOURCES) \
  $(TEMPLATE_SRC)

LIBOCTAVE_TST_FILES_SRC := $(shell $(top_srcdir)/build-aux/find-files-with-tests.sh "$(srcdir)" $(LIBOCTAVE_TST_SRC))

LIBOCTAVE_TST_FILES := $(addsuffix -tst,$(LIBOCTAVE_TST_FILES_SRC))

liboctavetestsdir := $(octtestsdir)/liboctave

nobase_liboctavetests_DATA = $(LIBOCTAVE_TST_FILES)

DISTCLEANFILES += \
  $(BUILT_INCS) \
  $(LIBOCTAVE_TST_FILES)

