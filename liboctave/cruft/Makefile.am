# Makefile for Octave's liboctave/cruft directory
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

## Search local directories before those specified by the user.
AM_CPPFLAGS = \
  -I$(top_builddir)/libgnu -I$(top_srcdir)/libgnu

EXTRA_DIST = mkf77def.in

noinst_LTLIBRARIES = libcruft.la

libcruft_la_SOURCES =

nodist_libcruft_la_SOURCES =

libcruft_la_FFLAGS = $(F77_INTEGER_8_FLAG)

libcruft_la_CPPFLAGS = \
  @CRUFT_DLL_DEFS@ \
  $(AM_CPPFLAGS)

libcruft_la_DEPENDENCIES = cruft.def

octinclude_HEADERS =

include amos/module.mk
include blas-xtra/module.mk
include daspk/module.mk
include dasrt/module.mk
include dassl/module.mk
include fftpack/module.mk
include lapack-xtra/module.mk
include misc/module.mk
include odepack/module.mk
include ordered-qz/module.mk
include quadpack/module.mk
include ranlib/module.mk
include slatec-err/module.mk
include slatec-fn/module.mk

## Special rules for files which must be built before compilation
cruft.def: $(libcruft_la_SOURCES) mkf77def
	chmod a+rx mkf77def
	./mkf77def $(srcdir) $(libcruft_la_SOURCES) > $@-t
	mv $@-t $@

DISTCLEANFILES = \
  cruft.def \
	ranlib/ranlib.def
	$(nodist_libcruft_la_SOURCES)

