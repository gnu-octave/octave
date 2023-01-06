////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_build_env_h)
#define octave_build_env_h 1

#include "octave-config.h"

#include "oct-map.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(build_env)

extern OCTINTERP_API octave_scalar_map features (void);

extern OCTINTERP_API const char *AMD_CPPFLAGS;
extern OCTINTERP_API const char *AMD_LDFLAGS;
extern OCTINTERP_API const char *AMD_LIBS;
extern OCTINTERP_API const char *ARFLAGS;
extern OCTINTERP_API const char *AR;
extern OCTINTERP_API const char *ARPACK_CPPFLAGS;
extern OCTINTERP_API const char *ARPACK_LDFLAGS;
extern OCTINTERP_API const char *ARPACK_LIBS;
extern OCTINTERP_API const char *BLAS_LIBS;
extern OCTINTERP_API const char *CAMD_CPPFLAGS;
extern OCTINTERP_API const char *CAMD_LDFLAGS;
extern OCTINTERP_API const char *CAMD_LIBS;
extern OCTINTERP_API const char *CARBON_LIBS;
extern OCTINTERP_API const char *CC;
extern OCTINTERP_API const char *CCOLAMD_CPPFLAGS;
extern OCTINTERP_API const char *CCOLAMD_LDFLAGS;
extern OCTINTERP_API const char *CCOLAMD_LIBS;
extern OCTINTERP_API const char *CFLAGS;
extern OCTINTERP_API const char *CHOLMOD_CPPFLAGS;
extern OCTINTERP_API const char *CHOLMOD_LDFLAGS;
extern OCTINTERP_API const char *CHOLMOD_LIBS;
extern OCTINTERP_API const char *COLAMD_CPPFLAGS;
extern OCTINTERP_API const char *COLAMD_LDFLAGS;
extern OCTINTERP_API const char *COLAMD_LIBS;
extern OCTINTERP_API const char *CPICFLAG;
extern OCTINTERP_API const char *CPPFLAGS;
extern OCTINTERP_API const char *CURL_CPPFLAGS;
extern OCTINTERP_API const char *CURL_LDFLAGS;
extern OCTINTERP_API const char *CURL_LIBS;
extern OCTINTERP_API const char *CXSPARSE_CPPFLAGS;
extern OCTINTERP_API const char *CXSPARSE_LDFLAGS;
extern OCTINTERP_API const char *CXSPARSE_LIBS;
extern OCTINTERP_API const char *CXXCPP;
extern OCTINTERP_API const char *CXXFLAGS;
extern OCTINTERP_API const char *CXXPICFLAG;
extern OCTINTERP_API const char *CXX;
extern OCTINTERP_API const char *DEFAULT_PAGER;
extern OCTINTERP_API const char *DEFS;
extern OCTINTERP_API const char *DL_LDFLAGS;
extern OCTINTERP_API const char *EXEEXT;
extern OCTINTERP_API const char *GCC_VERSION;
extern OCTINTERP_API const char *GXX_VERSION;
extern OCTINTERP_API const char *F77;
extern OCTINTERP_API const char *F77_FLOAT_STORE_FLAG;
extern OCTINTERP_API const char *F77_INTEGER_8_FLAG;
extern OCTINTERP_API const char *FFLAGS;
extern OCTINTERP_API const char *FFTW3_CPPFLAGS;
extern OCTINTERP_API const char *FFTW3_LDFLAGS;
extern OCTINTERP_API const char *FFTW3_LIBS;
extern OCTINTERP_API const char *FFTW3F_CPPFLAGS;
extern OCTINTERP_API const char *FFTW3F_LDFLAGS;
extern OCTINTERP_API const char *FFTW3F_LIBS;
extern OCTINTERP_API const char *FLIBS;
extern OCTINTERP_API const char *FLTK_CPPFLAGS;
extern OCTINTERP_API const char *FLTK_LDFLAGS;
extern OCTINTERP_API const char *FLTK_LIBS;
extern OCTINTERP_API const char *FONTCONFIG_CPPFLAGS;
extern OCTINTERP_API const char *FONTCONFIG_LIBS;
extern OCTINTERP_API const char *FPICFLAG;
extern OCTINTERP_API const char *FT2_CPPFLAGS;
extern OCTINTERP_API const char *FT2_LIBS;
extern OCTINTERP_API const char *GLPK_CPPFLAGS;
extern OCTINTERP_API const char *GLPK_LDFLAGS;
extern OCTINTERP_API const char *GLPK_LIBS;
extern OCTINTERP_API const char *GNUPLOT;
extern OCTINTERP_API const char *HDF5_CPPFLAGS;
extern OCTINTERP_API const char *HDF5_LDFLAGS;
extern OCTINTERP_API const char *HDF5_LIBS;
extern OCTINTERP_API const char *INCLUDEDIR;
extern OCTINTERP_API const char *KLU_CPPFLAGS;
extern OCTINTERP_API const char *KLU_LDFLAGS;
extern OCTINTERP_API const char *KLU_LIBS;
extern OCTINTERP_API const char *LAPACK_LIBS;
extern OCTINTERP_API const char *LDFLAGS;
extern OCTINTERP_API const char *LD_STATIC_FLAG;
extern OCTINTERP_API const char *LEXLIB;
extern OCTINTERP_API const char *LEX;
extern OCTINTERP_API const char *LFLAGS;
extern OCTINTERP_API const char *LIBOCTAVE;
extern OCTINTERP_API const char *LIBOCTINTERP;
extern OCTINTERP_API const char *LIBS;
extern OCTINTERP_API const char *LN_S;
extern OCTINTERP_API const char *MAGICK_CPPFLAGS;
extern OCTINTERP_API const char *MAGICK_LDFLAGS;
extern OCTINTERP_API const char *MAGICK_LIBS;
extern OCTINTERP_API const char *MKOCTFILE_DL_LDFLAGS;
extern OCTINTERP_API const char *OCTAVE_LINK_DEPS;
extern OCTINTERP_API const char *OCTAVE_LINK_OPTS;
extern OCTINTERP_API const char *OCTINCLUDEDIR;
extern OCTINTERP_API const char *OCTLIBDIR;
extern OCTINTERP_API const char *OCT_LINK_DEPS;
extern OCTINTERP_API const char *OCT_LINK_OPTS;
extern OCTINTERP_API const char *OPENGL_LIBS;
extern OCTINTERP_API const char *PCRE_CPPFLAGS;
extern OCTINTERP_API const char *PCRE_LDFLAGS;
extern OCTINTERP_API const char *PCRE_LIBS;
extern OCTINTERP_API const char *PREFIX;
extern OCTINTERP_API const char *PTHREAD_CFLAGS;
extern OCTINTERP_API const char *PTHREAD_LIBS;
extern OCTINTERP_API const char *QHULL_CPPFLAGS;
extern OCTINTERP_API const char *QHULL_LDFLAGS;
extern OCTINTERP_API const char *QHULL_LIBS;
extern OCTINTERP_API const char *QRUPDATE_CPPFLAGS;
extern OCTINTERP_API const char *QRUPDATE_LDFLAGS;
extern OCTINTERP_API const char *QRUPDATE_LIBS;
extern OCTINTERP_API const char *QT_CPPFLAGS;
extern OCTINTERP_API const char *QT_LDFLAGS;
extern OCTINTERP_API const char *QT_LIBS;
extern OCTINTERP_API const char *QT_OPENGL_LIBS;
extern OCTINTERP_API const char *RANLIB;
extern OCTINTERP_API const char *RDYNAMIC_FLAG;
extern OCTINTERP_API const char *READLINE_LIBS;
extern OCTINTERP_API const char *SHARED_LIBS;
extern OCTINTERP_API const char *SH_LDFLAGS;
extern OCTINTERP_API const char *STATIC_LIBS;
extern OCTINTERP_API const char *SUITESPARSECONFIG_LIBS;
extern OCTINTERP_API const char *SUNDIALS_IDA_CPPFLAGS;
extern OCTINTERP_API const char *SUNDIALS_IDA_LDFLAGS;
extern OCTINTERP_API const char *SUNDIALS_IDA_LIBS;
extern OCTINTERP_API const char *SUNDIALS_NVECSERIAL_CPPFLAGS;
extern OCTINTERP_API const char *SUNDIALS_NVECSERIAL_LDFLAGS;
extern OCTINTERP_API const char *SUNDIALS_NVECSERIAL_LIBS;
extern OCTINTERP_API const char *SUNDIALS_SUNLINSOLKLU_CPPFLAGS;
extern OCTINTERP_API const char *SUNDIALS_SUNLINSOLKLU_LDFLAGS;
extern OCTINTERP_API const char *SUNDIALS_SUNLINSOLKLU_LIBS;
extern OCTINTERP_API const char *UMFPACK_CPPFLAGS;
extern OCTINTERP_API const char *UMFPACK_LDFLAGS;
extern OCTINTERP_API const char *UMFPACK_LIBS;
extern OCTINTERP_API const char *WARN_CFLAGS;
extern OCTINTERP_API const char *WARN_CXXFLAGS;
extern OCTINTERP_API const char *X11_INCFLAGS;
extern OCTINTERP_API const char *X11_LIBS;
extern OCTINTERP_API const char *XTRA_CFLAGS;
extern OCTINTERP_API const char *XTRA_CXXFLAGS;
extern OCTINTERP_API const char *YACC;
extern OCTINTERP_API const char *YFLAGS;
extern OCTINTERP_API const char *Z_CPPFLAGS;
extern OCTINTERP_API const char *Z_LDFLAGS;
extern OCTINTERP_API const char *Z_LIBS;
extern OCTINTERP_API const char *config_opts;

OCTAVE_END_NAMESPACE(build_env)
OCTAVE_END_NAMESPACE(octave)

#endif
