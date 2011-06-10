# Makefile for octave's libcruft directory
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

include $(top_srcdir)/build-aux/common.mk

AM_CPPFLAGS = @CPPFLAGS@ -I../libgnu -I$(top_srcdir)/libgnu

AUTOMAKE_OPTIONS = subdir-objects

octlib_LTLIBRARIES = libcruft.la

libcruft_la_SOURCES =

libcruft_la_FFLAGS = $(F77_INTEGER_8_FLAG)

libcruft_la_CPPFLAGS = \
  @CRUFT_DLL_DEFS@ \
  $(AM_CPPFLAGS)

# Increment these as needed and according to the rules in the libtool
# manual:
libcruft_current = 0
libcruft_revision = 0
libcruft_age = 0

libcruft_version_info = $(libcruft_current):$(libcruft_revision):$(libcruft_age)

libcruft_la_LDFLAGS = \
  -version-info $(libcruft_version_info) \
  $(NO_UNDEFINED_LDFLAG) \
  @XTRA_CRUFT_SH_LDFLAGS@ \
  -bindir $(bindir)

libcruft_la_LIBADD = \
  ../libgnu/libgnu.la \
  libranlib.la \
  $(LAPACK_LIBS) $(BLAS_LIBS) \
  $(FLIBS)

libcruft_la_DEPENDENCIES = cruft.def

noinst_LTLIBRARIES = libranlib.la

libranlib_la_SOURCES =

libranlib_la_DEPENDENCIES = ranlib.def

octinclude_HEADERS =

EXTRA_DIST =

include amos/module.mk
include arpack/module.mk
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

cruft.def: $(libcruft_la_SOURCES) mkf77def
	chmod a+rx mkf77def
	./mkf77def $(srcdir) $(libcruft_la_SOURCES) > $@-t
	mv $@-t $@

ranlib.def: $(libranlib_la_SOURCES) mkf77def
	chmod a+rx mkf77def
	./mkf77def $(srcdir) $(libranlib_la_SOURCES) > $@-t
	mv $@-t $@

EXTRA_DIST += mkf77def.in

DISTCLEANFILES = cruft.def ranlib.def

