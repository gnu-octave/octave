/*

Copyright (C) 1996-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if ! defined (octave_build_env_h)
#define octave_build_env_h 1

#include "octave-config.h"

#include "oct-map.h"

namespace octave
{
  namespace build_env
  {
    extern OCTAVE_API octave_scalar_map features (void);

    extern const char *AMD_CPPFLAGS;
    extern const char *AMD_LDFLAGS;
    extern const char *AMD_LIBS;
    extern const char *ARFLAGS;
    extern const char *AR;
    extern const char *ARPACK_CPPFLAGS;
    extern const char *ARPACK_LDFLAGS;
    extern const char *ARPACK_LIBS;
    extern const char *BLAS_LIBS;
    extern const char *CAMD_CPPFLAGS;
    extern const char *CAMD_LDFLAGS;
    extern const char *CAMD_LIBS;
    extern const char *CARBON_LIBS;
    extern const char *CC;
    extern const char *CCOLAMD_CPPFLAGS;
    extern const char *CCOLAMD_LDFLAGS;
    extern const char *CCOLAMD_LIBS;
    extern const char *CFLAGS;
    extern const char *CHOLMOD_CPPFLAGS;
    extern const char *CHOLMOD_LDFLAGS;
    extern const char *CHOLMOD_LIBS;
    extern const char *COLAMD_CPPFLAGS;
    extern const char *COLAMD_LDFLAGS;
    extern const char *COLAMD_LIBS;
    extern const char *CPICFLAG;
    extern const char *CPPFLAGS;
    extern const char *CURL_CPPFLAGS;
    extern const char *CURL_LDFLAGS;
    extern const char *CURL_LIBS;
    extern const char *CXSPARSE_CPPFLAGS;
    extern const char *CXSPARSE_LDFLAGS;
    extern const char *CXSPARSE_LIBS;
    extern const char *CXXCPP;
    extern const char *CXXFLAGS;
    extern const char *CXXPICFLAG;
    extern const char *CXX;
    extern const char *DEFAULT_PAGER;
    extern const char *DEFS;
    extern const char *DL_LD;
    extern const char *DL_LDFLAGS;
    extern const char *DL_LIBS;
    extern const char *EXEEXT;
    extern const char *GCC_VERSION;
    extern const char *GXX_VERSION;
    extern const char *F77;
    extern const char *F77_FLOAT_STORE_FLAG;
    extern const char *F77_INTEGER_8_FLAG;
    extern const char *FFLAGS;
    extern const char *FFTW3_CPPFLAGS;
    extern const char *FFTW3_LDFLAGS;
    extern const char *FFTW3_LIBS;
    extern const char *FFTW3F_CPPFLAGS;
    extern const char *FFTW3F_LDFLAGS;
    extern const char *FFTW3F_LIBS;
    extern const char *FLIBS;
    extern const char *FLTK_CPPFLAGS;
    extern const char *FLTK_LDFLAGS;
    extern const char *FLTK_LIBS;
    extern const char *FONTCONFIG_CPPFLAGS;
    extern const char *FONTCONFIG_LIBS;
    extern const char *FPICFLAG;
    extern const char *FT2_CPPFLAGS;
    extern const char *FT2_LIBS;
    extern const char *GLPK_CPPFLAGS;
    extern const char *GLPK_LDFLAGS;
    extern const char *GLPK_LIBS;
    extern const char *GNUPLOT;
    extern const char *HDF5_CPPFLAGS;
    extern const char *HDF5_LDFLAGS;
    extern const char *HDF5_LIBS;
    extern const char *INCLUDEDIR;
    extern const char *KLU_CPPFLAGS;
    extern const char *KLU_LDFLAGS;
    extern const char *KLU_LIBS;
    extern const char *LAPACK_LIBS;
    extern const char *LDFLAGS;
    extern const char *LD_CXX;
    extern const char *LD_STATIC_FLAG;
    extern const char *LEXLIB;
    extern const char *LEX;
    extern const char *LFLAGS;
    extern const char *LIBOCTAVE;
    extern const char *LIBOCTINTERP;
    extern const char *LIBS;
    extern const char *LN_S;
    extern const char *MAGICK_CPPFLAGS;
    extern const char *MAGICK_LDFLAGS;
    extern const char *MAGICK_LIBS;
    extern const char *LLVM_CPPFLAGS;
    extern const char *LLVM_LDFLAGS;
    extern const char *LLVM_LIBS;
    extern const char *MKOCTFILE_DL_LDFLAGS;
    extern const char *OCTAVE_LINK_DEPS;
    extern const char *OCTAVE_LINK_OPTS;
    extern const char *OCTINCLUDEDIR;
    extern const char *OCTLIBDIR;
    extern const char *OCT_LINK_DEPS;
    extern const char *OCT_LINK_OPTS;
    extern const char *OPENGL_LIBS;
    extern const char *OSMESA_CPPFLAGS;
    extern const char *OSMESA_LDFLAGS;
    extern const char *OSMESA_LIBS;
    extern const char *PCRE_CPPFLAGS;
    extern const char *PCRE_LDFLAGS;
    extern const char *PCRE_LIBS;
    extern const char *PREFIX;
    extern const char *PTHREAD_CFLAGS;
    extern const char *PTHREAD_LIBS;
    extern const char *QHULL_CPPFLAGS;
    extern const char *QHULL_LDFLAGS;
    extern const char *QHULL_LIBS;
    extern const char *QRUPDATE_CPPFLAGS;
    extern const char *QRUPDATE_LDFLAGS;
    extern const char *QRUPDATE_LIBS;
    extern const char *QT_CPPFLAGS;
    extern const char *QT_LDFLAGS;
    extern const char *QT_LIBS;
    extern const char *RANLIB;
    extern const char *RDYNAMIC_FLAG;
    extern const char *READLINE_LIBS;
    extern const char *SED;
    extern const char *SHARED_LIBS;
    extern const char *SH_LD;
    extern const char *SH_LDFLAGS;
    extern const char *STATIC_LIBS;
    extern const char *SUITESPARSECONFIG_LIBS;
    extern const char *SUNDIALS_IDA_CPPFLAGS;
    extern const char *SUNDIALS_IDA_LDFLAGS;
    extern const char *SUNDIALS_IDA_LIBS;
    extern const char *SUNDIALS_NVECSERIAL_CPPFLAGS;
    extern const char *SUNDIALS_NVECSERIAL_LDFLAGS;
    extern const char *SUNDIALS_NVECSERIAL_LIBS;
    extern const char *TERM_LIBS;
    extern const char *UMFPACK_CPPFLAGS;
    extern const char *UMFPACK_LDFLAGS;
    extern const char *UMFPACK_LIBS;
    extern const char *WARN_CFLAGS;
    extern const char *WARN_CXXFLAGS;
    extern const char *X11_INCFLAGS;
    extern const char *X11_LIBS;
    extern const char *XTRA_CFLAGS;
    extern const char *XTRA_CXXFLAGS;
    extern const char *YACC;
    extern const char *YFLAGS;
    extern const char *Z_CPPFLAGS;
    extern const char *Z_LDFLAGS;
    extern const char *Z_LIBS;
    extern const char *config_opts;
  }
}

#endif
