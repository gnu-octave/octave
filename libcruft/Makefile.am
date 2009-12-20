# Makefile for octave's libcruft directory
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

include $(top_srcdir)/common.mk

AUTOMAKE_OPTIONS = subdir-objects

octlib_LTLIBRARIES = libcruft.la

libcruft_la_SOURCES =

libcruft_la_CPPFLAGS = @CRUFT_DLL_DEFS@

libcruft_la_LDFLAGS = \
  -release $(version) $(NO_UNDEFINED_LDFLAG) @XTRA_CRUFT_SH_LDFLAGS@

libcruft_la_LIBADD = $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS)

libcruft_la_DEPENDENCIES = cruft.def

octinclude_HEADERS =

EXTRA_DIST =

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
include villad/module.mk

cruft.def: $(libcruft_la_SOURCES) mkf77def
	chmod a+rx mkf77def
	./mkf77def $(srcdir) $(libcruft_la_SOURCES) > $@-t
	$(simple_move_if_change_rule)

EXTRA_DIST += ChangeLog STOP.patch mkf77def.in

DISTCLEANFILES = cruft.def

