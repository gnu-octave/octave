########################################################################
##
## Copyright (C) 2009-2026 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

DLDFCN_SRC = \
  %reldir%/__delaunayn__.cc \
  %reldir%/__glpk__.cc \
  %reldir%/__init_gnuplot__.cc \
  %reldir%/__ode15__.cc \
  %reldir%/__voronoi__.cc \
  %reldir%/audiodevinfo.cc \
  %reldir%/audioread.cc \
  %reldir%/convhulln.cc \
  %reldir%/fftw.cc \
  %reldir%/gzip.cc

DLDFCN_LIBS := $(DLDFCN_SRC:.cc=.la)

octlib_LTLIBRARIES += $(DLDFCN_LIBS)

## Use stamp files to avoid problems with checking timestamps of symbolic links

%.oct : %.la
	$(AM_V_GEN)$(INSTALL_PROGRAM) %reldir%/.libs/$(shell $(SED) -n -e "s/dlname='\([^']*\)'/\1/p" < $<) $@

%canon_reldir%___delaunayn___la_SOURCES = %reldir%/__delaunayn__.cc
%canon_reldir%___delaunayn___la_CPPFLAGS = $(liboctinterp_liboctinterp_la_CPPFLAGS) $(QHULL_CPPFLAGS)
%canon_reldir%___delaunayn___la_LDFLAGS := $(AM_LDFLAGS) $(WARN_LDFLAGS) $(NO_UNDEFINED_LDFLAG) -avoid-version -module $(QHULL_LDFLAGS) $(OCT_LINK_OPTS)
%canon_reldir%___delaunayn___la_LIBADD = $(QHULL_LIBS) $(DLDFCN_LINK_DEPS)
## Let Automake compute _DEPENDENCIES.  Add EXTRA_xxx_DEPENDENCIES to
## force required targets to be linked before the dldfcn module.
EXTRA_%canon_reldir%___delaunayn___la_DEPENDENCIES := $(DLDFCN_DEPENDENCIES)

%canon_reldir%___glpk___la_SOURCES = %reldir%/__glpk__.cc
%canon_reldir%___glpk___la_CPPFLAGS = $(liboctinterp_liboctinterp_la_CPPFLAGS) $(GLPK_CPPFLAGS)
%canon_reldir%___glpk___la_LDFLAGS := $(AM_LDFLAGS) $(WARN_LDFLAGS) $(NO_UNDEFINED_LDFLAG) -avoid-version -module $(GLPK_LDFLAGS) $(OCT_LINK_OPTS)
%canon_reldir%___glpk___la_LIBADD = $(GLPK_LIBS) $(DLDFCN_LINK_DEPS)
EXTRA_%canon_reldir%___glpk___la_DEPENDENCIES := $(DLDFCN_DEPENDENCIES)

%canon_reldir%___init_gnuplot___la_SOURCES = %reldir%/__init_gnuplot__.cc
%canon_reldir%___init_gnuplot___la_CPPFLAGS = $(liboctinterp_liboctinterp_la_CPPFLAGS) $(FONTCONFIG_CPPFLAGS) $(FT2_CPPFLAGS)
%canon_reldir%___init_gnuplot___la_LDFLAGS := $(AM_LDFLAGS) $(WARN_LDFLAGS) $(NO_UNDEFINED_LDFLAG) -avoid-version -module  $(OCT_LINK_OPTS)
%canon_reldir%___init_gnuplot___la_LIBADD =  $(DLDFCN_LINK_DEPS)
EXTRA_%canon_reldir%___init_gnuplot___la_DEPENDENCIES := $(DLDFCN_DEPENDENCIES)

%canon_reldir%___ode15___la_SOURCES = %reldir%/__ode15__.cc
%canon_reldir%___ode15___la_CPPFLAGS = $(liboctinterp_liboctinterp_la_CPPFLAGS) $(SUNDIALS_XCPPFLAGS)
%canon_reldir%___ode15___la_LDFLAGS := $(AM_LDFLAGS) $(WARN_LDFLAGS) $(NO_UNDEFINED_LDFLAG) -avoid-version -module $(SUNDIALS_XLDFLAGS) $(OCT_LINK_OPTS)
%canon_reldir%___ode15___la_LIBADD = $(SUNDIALS_XLIBS) $(DLDFCN_LINK_DEPS)
EXTRA_%canon_reldir%___ode15___la_DEPENDENCIES := $(DLDFCN_DEPENDENCIES)

%canon_reldir%___voronoi___la_SOURCES = %reldir%/__voronoi__.cc
%canon_reldir%___voronoi___la_CPPFLAGS = $(liboctinterp_liboctinterp_la_CPPFLAGS) $(QHULL_CPPFLAGS)
%canon_reldir%___voronoi___la_LDFLAGS := $(AM_LDFLAGS) $(WARN_LDFLAGS) $(NO_UNDEFINED_LDFLAG) -avoid-version -module $(QHULL_LDFLAGS) $(OCT_LINK_OPTS)
%canon_reldir%___voronoi___la_LIBADD = $(QHULL_LIBS) $(DLDFCN_LINK_DEPS)
EXTRA_%canon_reldir%___voronoi___la_DEPENDENCIES := $(DLDFCN_DEPENDENCIES)

%canon_reldir%_audiodevinfo_la_SOURCES = %reldir%/audiodevinfo.cc
%canon_reldir%_audiodevinfo_la_CPPFLAGS = $(liboctinterp_liboctinterp_la_CPPFLAGS) $(PORTAUDIO_CPPFLAGS)
%canon_reldir%_audiodevinfo_la_LDFLAGS := $(AM_LDFLAGS) $(WARN_LDFLAGS) $(NO_UNDEFINED_LDFLAG) -avoid-version -module $(PORTAUDIO_LDFLAGS) $(OCT_LINK_OPTS)
%canon_reldir%_audiodevinfo_la_LIBADD = $(PORTAUDIO_LIBS) $(DLDFCN_LINK_DEPS)
EXTRA_%canon_reldir%_audiodevinfo_la_DEPENDENCIES := $(DLDFCN_DEPENDENCIES)

%canon_reldir%_audioread_la_SOURCES = %reldir%/audioread.cc
%canon_reldir%_audioread_la_CPPFLAGS = $(liboctinterp_liboctinterp_la_CPPFLAGS) $(SNDFILE_CPPFLAGS)
%canon_reldir%_audioread_la_LDFLAGS := $(AM_LDFLAGS) $(WARN_LDFLAGS) $(NO_UNDEFINED_LDFLAG) -avoid-version -module $(SNDFILE_LDFLAGS) $(OCT_LINK_OPTS)
%canon_reldir%_audioread_la_LIBADD = $(SNDFILE_LIBS) $(DLDFCN_LINK_DEPS)
EXTRA_%canon_reldir%_audioread_la_DEPENDENCIES := $(DLDFCN_DEPENDENCIES)

%canon_reldir%_convhulln_la_SOURCES = %reldir%/convhulln.cc
%canon_reldir%_convhulln_la_CPPFLAGS = $(liboctinterp_liboctinterp_la_CPPFLAGS) $(QHULL_CPPFLAGS)
%canon_reldir%_convhulln_la_LDFLAGS := $(AM_LDFLAGS) $(WARN_LDFLAGS) $(NO_UNDEFINED_LDFLAG) -avoid-version -module $(QHULL_LDFLAGS) $(OCT_LINK_OPTS)
%canon_reldir%_convhulln_la_LIBADD = $(QHULL_LIBS) $(DLDFCN_LINK_DEPS)
EXTRA_%canon_reldir%_convhulln_la_DEPENDENCIES := $(DLDFCN_DEPENDENCIES)

%canon_reldir%_fftw_la_SOURCES = %reldir%/fftw.cc
%canon_reldir%_fftw_la_CPPFLAGS = $(liboctinterp_liboctinterp_la_CPPFLAGS) $(FFTW_XCPPFLAGS)
%canon_reldir%_fftw_la_LDFLAGS := $(AM_LDFLAGS) $(WARN_LDFLAGS) $(NO_UNDEFINED_LDFLAG) -avoid-version -module $(FFTW_XLDFLAGS) $(OCT_LINK_OPTS)
%canon_reldir%_fftw_la_LIBADD = $(FFTW_XLIBS) $(DLDFCN_LINK_DEPS)
EXTRA_%canon_reldir%_fftw_la_DEPENDENCIES := $(DLDFCN_DEPENDENCIES)

%canon_reldir%_gzip_la_SOURCES = %reldir%/gzip.cc
%canon_reldir%_gzip_la_CPPFLAGS = $(liboctinterp_liboctinterp_la_CPPFLAGS) $(Z_CPPFLAGS) $(BZ2_CPPFLAGS)
%canon_reldir%_gzip_la_LDFLAGS := $(AM_LDFLAGS) $(WARN_LDFLAGS) $(NO_UNDEFINED_LDFLAG) -avoid-version -module $(Z_LDFLAGS) $(BZ2_LDFLAGS) $(OCT_LINK_OPTS)
%canon_reldir%_gzip_la_LIBADD = $(Z_LIBS) $(BZ2_LIBS) $(DLDFCN_LINK_DEPS)
EXTRA_%canon_reldir%_gzip_la_DEPENDENCIES := $(DLDFCN_DEPENDENCIES)

## Special rules

DLDFCN_OCT_FILES := $(DLDFCN_LIBS:.la=.oct)

DLDFCN_DEFUN_FILES := $(DLDFCN_SRC)

DLDFCN_PKG_ADD_FILE = %reldir%/PKG_ADD

%reldir%/PKG_ADD: $(DLDFCN_DEFUN_FILES) $(srcdir)/%reldir%/mk-pkg-add.sh | %reldir%/$(octave_dirstamp)
	$(AM_V_GEN)rm -f $@-t && \
	$(SHELL) $(srcdir)/%reldir%/mk-pkg-add.sh "$(srcdir)" $(DLDFCN_DEFUN_FILES) > $@-t && \
	mv $@-t $@

LIBOCTINTERP_DEFUN_FILES += $(DLDFCN_DEFUN_FILES)

OCT_FILE_PKG_ADD_FILES += $(DLDFCN_PKG_ADD_FILE)

OCTAVE_INTERPRETER_TARGETS += $(DLDFCN_OCT_FILES)

OCT_FILE_LIBS += $(DLDFCN_LIBS)

OCT_FILES += $(DLDFCN_OCT_FILES)

DIRSTAMP_FILES += %reldir%/$(octave_dirstamp)

EXTRA_DIST += \
  %reldir%/mk-pkg-add.sh \
  %reldir%/oct-qhull.h

liboctinterp_CLEANFILES += \
  $(DLDFCN_PKG_ADD_FILE) \
  $(DLDFCN_OCT_FILES)
