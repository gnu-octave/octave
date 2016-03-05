#! /bin/sh
#
# Copyright (C) 2016 John W. Eaton
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

: ${SED=@SED@}

## The subst-config-vals and subst-cross-config-vals scripts differ only
## in the definitions of the following variables.
##
##   OCTAVE_CONF_MKOCTFILE_AR
##   OCTAVE_CONF_MKOCTFILE_CC
##   OCTAVE_CONF_MKOCTFILE_CXX
##   OCTAVE_CONF_MKOCTFILE_DL_LD
##   OCTAVE_CONF_MKOCTFILE_DL_LDFLAGS
##   OCTAVE_CONF_MKOCTFILE_F77
##   OCTAVE_CONF_MKOCTFILE_LD_CXX
##   OCTAVE_CONF_MKOCTFILE_RANLIB

## Use two steps so that we can interpolate values without having to
## determine the order in which to set variable values.

## These must use ' so that embedded variables are not interpolated
## (the values they reference may not be defined before they are used).

NO_OCT_FILE_STRIP='@NO_OCT_FILE_STRIP@'
bindir='@bindir@'
AMD_CPPFLAGS='@AMD_CPPFLAGS@'
AMD_LDFLAGS='@AMD_LDFLAGS@'
AMD_LIBS='@AMD_LIBS@'
AR='@AR@'
ARFLAGS='@ARFLAGS@'
ARPACK_CPPFLAGS='@ARPACK_CPPFLAGS@'
ARPACK_LDFLAGS='@ARPACK_LDFLAGS@'
ARPACK_LIBS='@ARPACK_LIBS@'
BLAS_LIBS='@BLAS_LIBS@'
CAMD_CPPFLAGS='@CAMD_CPPFLAGS@'
CAMD_LDFLAGS='@CAMD_LDFLAGS@'
CAMD_LIBS='@CAMD_LIBS@'
CARBON_LIBS='@CARBON_LIBS@'
CC='@CC@'
CCOLAMD_CPPFLAGS='@CCOLAMD_CPPFLAGS@'
CCOLAMD_LDFLAGS='@CCOLAMD_LDFLAGS@'
CCOLAMD_LIBS='@CCOLAMD_LIBS@'
CFLAGS='@CFLAGS@'
CHOLMOD_CPPFLAGS='@CHOLMOD_CPPFLAGS@'
CHOLMOD_LDFLAGS='@CHOLMOD_LDFLAGS@'
CHOLMOD_LIBS='@CHOLMOD_LIBS@'
COLAMD_CPPFLAGS='@COLAMD_CPPFLAGS@'
COLAMD_LDFLAGS='@COLAMD_LDFLAGS@'
COLAMD_LIBS='@COLAMD_LIBS@'
CPICFLAG='@CPICFLAG@'
CPPFLAGS='@CPPFLAGS@'
CURL_CPPFLAGS='@CURL_CPPFLAGS@'
CURL_LDFLAGS='@CURL_LDFLAGS@'
CURL_LIBS='@CURL_LIBS@'
CXSPARSE_CPPFLAGS='@CXSPARSE_CPPFLAGS@'
CXSPARSE_LDFLAGS='@CXSPARSE_LDFLAGS@'
CXSPARSE_LIBS='@CXSPARSE_LIBS@'
CXX='@CXX@'
CXXCPP='@CXXCPP@'
CXXFLAGS='@CXXFLAGS@'
CXXPICFLAG='@CXXPICFLAG@'
DEFAULT_PAGER='@DEFAULT_PAGER@'
DEFS='@DEFS@'
DEPEND_FLAGS='@DEPEND_FLAGS@'
DEPEND_EXTRA_SED_PATTERN='@DEPEND_EXTRA_SED_PATTERN@'
DL_LD='@DL_LD@'
DL_LDFLAGS='@DL_LDFLAGS@'
DL_LIBS='@DL_LIBS@'
EXEEXT='@EXEEXT@'
GCC_VERSION='@GCC_VERSION@'
GXX_VERSION='@GXX_VERSION@'
F77='@F77@'
F77_FLOAT_STORE_FLAG='@F77_FLOAT_STORE_FLAG@'
F77_INTEGER_8_FLAG='@F77_INTEGER_8_FLAG@'
FFLAGS='@FFLAGS@'
FFTW3_CPPFLAGS='@FFTW3_CPPFLAGS@'
FFTW3_LDFLAGS='@FFTW3_LDFLAGS@'
FFTW3_LIBS='@FFTW3_LIBS@'
FFTW3F_CPPFLAGS='@FFTW3F_CPPFLAGS@'
FFTW3F_LDFLAGS='@FFTW3F_LDFLAGS@'
FFTW3F_LIBS='@FFTW3F_LIBS@'
FLIBS='@FLIBS@'
FLTK_CPPFLAGS='@FLTK_CPPFLAGS@'
FLTK_LDFLAGS='@FLTK_LDFLAGS@'
FLTK_LIBS='@FLTK_LIBS@'
FONTCONFIG_CPPFLAGS='@FONTCONFIG_CPPFLAGS@'
FONTCONFIG_LIBS='@FONTCONFIG_LIBS@'
FPICFLAG='@FPICFLAG@'
FT2_CPPFLAGS='@FT2_CPPFLAGS@'
FT2_LIBS='@FT2_LIBS@'
GLPK_CPPFLAGS='@GLPK_CPPFLAGS@'
GLPK_LDFLAGS='@GLPK_LDFLAGS@'
GLPK_LIBS='@GLPK_LIBS@'
GNUPLOT='@GNUPLOT@'
HDF5_CPPFLAGS='@HDF5_CPPFLAGS@'
HDF5_LDFLAGS='@HDF5_LDFLAGS@'
HDF5_LIBS='@HDF5_LIBS@'
includedir='@includedir@'
LAPACK_LIBS='@LAPACK_LIBS@'
LD_CXX='@LD_CXX@'
LDFLAGS='@LDFLAGS@'
LD_STATIC_FLAG='@LD_STATIC_FLAG@'
LEX='@LEX@'
LEXLIB='@LEXLIB@'
LFLAGS='@LFLAGS@'
libdir='@libdir@'
LIBEXT='@LIBEXT@'
LIBOCTAVE='@LIBOCTAVE@'
LIBOCTINTERP='@LIBOCTINTERP@'
LIBS='@LIBS@'
LLVM_CPPFLAGS='@LLVM_CPPFLAGS@'
LLVM_LDFLAGS='@LLVM_LDFLAGS@'
LLVM_LIBS='@LLVM_LIBS@'
LN_S='@LN_S@'
MAGICK_CPPFLAGS='@MAGICK_CPPFLAGS@'
MAGICK_LDFLAGS='@MAGICK_LDFLAGS@'
MAGICK_LIBS='@MAGICK_LIBS@'
OCTAVE_LINK_DEPS='@OCTAVE_LINK_DEPS@'
OCTAVE_LINK_OPTS='@OCTAVE_LINK_OPTS@'
octincludedir='@octincludedir@'
octlibdir='@octlibdir@'
OCT_LINK_DEPS='@OCT_LINK_DEPS@'
OCT_LINK_OPTS='@OCT_LINK_OPTS@'
OPENGL_LIBS='@OPENGL_LIBS@'
OSMESA_CPPFLAGS='@OSMESA_CPPFLAGS@'
OSMESA_LDFLAGS='@OSMESA_LDFLAGS@'
OSMESA_LIBS='@OSMESA_LIBS@'
PCRE_CPPFLAGS='@PCRE_CPPFLAGS@'
PCRE_LIBS='@PCRE_LIBS@'
prefix='@prefix@'
PTHREAD_CFLAGS='@PTHREAD_CFLAGS@'
PTHREAD_LIBS='@PTHREAD_LIBS@'
QHULL_CPPFLAGS='@QHULL_CPPFLAGS@'
QHULL_LDFLAGS='@QHULL_LDFLAGS@'
QHULL_LIBS='@QHULL_LIBS@'
QRUPDATE_CPPFLAGS='@QRUPDATE_CPPFLAGS@'
QRUPDATE_LDFLAGS='@QRUPDATE_LDFLAGS@'
QRUPDATE_LIBS='@QRUPDATE_LIBS@'
QT_CPPFLAGS='@QT_CPPFLAGS@'
QT_LDFLAGS='@QT_LDFLAGS@'
QT_LIBS='@QT_LIBS@'
RANLIB='@RANLIB@'
RDYNAMIC_FLAG='@RDYNAMIC_FLAG@'
READLINE_LIBS='@READLINE_LIBS@'
SED='@SED@'
SHARED_LIBS='@SHARED_LIBS@'
SHLEXT='@SHLEXT@'
SHLLINKEXT='@SHLLINKEXT@'
SHLEXT_VER='@SHLEXT_VER@'
SH_LD='@SH_LD@'
SH_LDFLAGS='@SH_LDFLAGS@'
SONAME_FLAGS='@SONAME_FLAGS@'
STATIC_LIBS='@STATIC_LIBS@'
TERM_LIBS='@TERM_LIBS@'
UMFPACK_CPPFLAGS='@UMFPACK_CPPFLAGS@'
UMFPACK_LDFLAGS='@UMFPACK_LDFLAGS@'
UMFPACK_LIBS='@UMFPACK_LIBS@'
version='@PACKAGE_VERSION@'
WARN_CFLAGS='@WARN_CFLAGS@'
WARN_CXXFLAGS='@WARN_CXXFLAGS@'
X11_INCFLAGS='@X11_INCFLAGS@'
X11_LIBS='@X11_LIBS@'
XTRA_CFLAGS='@XTRA_CFLAGS@'
XTRA_CXXFLAGS='@XTRA_CXXFLAGS@'
YACC='@YACC@'
YFLAGS='@YFLAGS@'
Z_CPPFLAGS='@Z_CPPFLAGS@'
Z_LDFLAGS='@Z_LDFLAGS@'
Z_LIBS='@Z_LIBS@'
## this on uses ' to quote individual arguments, so use " here.
config_opts="@config_opts@"

## These must use " so that embedded variables are interpolated.

## Is there a better way?

function expand_var ()
{
  eval tmp="\$$1"
  while echo "$tmp" | grep '\${[A-Za-z_][A-Za-z0-9_]*}' > /dev/null; do
    eval tmp="$tmp"
  done
  eval $1="\"$tmp\""
}

expand_var NO_OCT_FILE_STRIP
expand_var bindir
expand_var AMD_CPPFLAGS
expand_var AMD_LDFLAGS
expand_var AMD_LIBS
expand_var AR
expand_var ARFLAGS
expand_var ARPACK_CPPFLAGS
expand_var ARPACK_LDFLAGS
expand_var ARPACK_LIBS
expand_var BLAS_LIBS
expand_var CAMD_CPPFLAGS
expand_var CAMD_LDFLAGS
expand_var CAMD_LIBS
expand_var CARBON_LIBS
expand_var CC
expand_var CCOLAMD_CPPFLAGS
expand_var CCOLAMD_LDFLAGS
expand_var CCOLAMD_LIBS
expand_var CFLAGS
expand_var CHOLMOD_CPPFLAGS
expand_var CHOLMOD_LDFLAGS
expand_var CHOLMOD_LIBS
expand_var COLAMD_CPPFLAGS
expand_var COLAMD_LDFLAGS
expand_var COLAMD_LIBS
expand_var CPICFLAG
expand_var CPPFLAGS
expand_var CURL_CPPFLAGS
expand_var CURL_LDFLAGS
expand_var CURL_LIBS
expand_var CXSPARSE_CPPFLAGS
expand_var CXSPARSE_LDFLAGS
expand_var CXSPARSE_LIBS
expand_var CXX
expand_var CXXCPP
expand_var CXXFLAGS
expand_var CXXPICFLAG
expand_var DEFAULT_PAGER
expand_var DEFS
expand_var DEPEND_FLAGS
expand_var DEPEND_EXTRA_SED_PATTERN
expand_var DL_LD
expand_var DL_LDFLAGS
expand_var DL_LIBS
expand_var EXEEXT
expand_var GCC_VERSION
expand_var GXX_VERSION
expand_var F77
expand_var F77_FLOAT_STORE_FLAG
expand_var F77_INTEGER_8_FLAG
expand_var FFLAGS
expand_var FFTW3_CPPFLAGS
expand_var FFTW3_LDFLAGS
expand_var FFTW3_LIBS
expand_var FFTW3F_CPPFLAGS
expand_var FFTW3F_LDFLAGS
expand_var FFTW3F_LIBS
expand_var FLIBS
expand_var FLTK_CPPFLAGS
expand_var FLTK_LDFLAGS
expand_var FLTK_LIBS
expand_var FONTCONFIG_CPPFLAGS
expand_var FONTCONFIG_LIBS
expand_var FPICFLAG
expand_var FT2_CPPFLAGS
expand_var FT2_LIBS
expand_var GLPK_CPPFLAGS
expand_var GLPK_LDFLAGS
expand_var GLPK_LIBS
expand_var GNUPLOT
expand_var HDF5_CPPFLAGS
expand_var HDF5_LDFLAGS
expand_var HDF5_LIBS
expand_var includedir
expand_var LAPACK_LIBS
expand_var LD_CXX
expand_var LDFLAGS
expand_var LD_STATIC_FLAG
expand_var LEX
expand_var LEXLIB
expand_var LFLAGS
expand_var libdir
expand_var LIBEXT
expand_var LIBOCTAVE
expand_var LIBOCTINTERP
expand_var LIBS
expand_var LLVM_CPPFLAGS
expand_var LLVM_LDFLAGS
expand_var LLVM_LIBS
expand_var LN_S
expand_var MAGICK_CPPFLAGS
expand_var MAGICK_LDFLAGS
expand_var MAGICK_LIBS
expand_var OCTAVE_LINK_DEPS
expand_var OCTAVE_LINK_OPTS
expand_var octincludedir
expand_var octlibdir
expand_var OCT_LINK_DEPS
expand_var OCT_LINK_OPTS
expand_var OPENGL_LIBS
expand_var OSMESA_CPPFLAGS
expand_var OSMESA_LDFLAGS
expand_var OSMESA_LIBS
expand_var PCRE_CPPFLAGS
expand_var PCRE_LIBS
expand_var prefix
expand_var PTHREAD_CFLAGS
expand_var PTHREAD_LIBS
expand_var QHULL_CPPFLAGS
expand_var QHULL_LDFLAGS
expand_var QHULL_LIBS
expand_var QRUPDATE_CPPFLAGS
expand_var QRUPDATE_LDFLAGS
expand_var QRUPDATE_LIBS
expand_var QT_CPPFLAGS
expand_var QT_LDFLAGS
expand_var QT_LIBS
expand_var RANLIB
expand_var RDYNAMIC_FLAG
expand_var READLINE_LIBS
expand_var SED
expand_var SHARED_LIBS
expand_var SHLEXT
expand_var SHLLINKEXT
expand_var SHLEXT_VER
expand_var SH_LD
expand_var SH_LDFLAGS
expand_var SONAME_FLAGS
expand_var STATIC_LIBS
expand_var TERM_LIBS
expand_var UMFPACK_CPPFLAGS
expand_var UMFPACK_LDFLAGS
expand_var UMFPACK_LIBS
expand_var version
expand_var WARN_CFLAGS
expand_var WARN_CXXFLAGS
expand_var X11_INCFLAGS
expand_var X11_LIBS
expand_var XTRA_CFLAGS
expand_var XTRA_CXXFLAGS
expand_var YACC
expand_var YFLAGS
expand_var Z_CPPFLAGS
expand_var Z_LDFLAGS
expand_var Z_LIBS
expand_var config_opts

$SED \
  -e "s|%NO_EDIT_WARNING%|DO NOT EDIT!  Generated automatically by subst-config-vals.|" \
    -e "s|%NO_OCT_FILE_STRIP%|${NO_OCT_FILE_STRIP}|" \
    -e "s|%OCTAVE_BINDIR%|\"${bindir}\"|" \
    -e "s|%OCTAVE_CONF_AMD_CPPFLAGS%|\"${AMD_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_AMD_LDFLAGS%|\"${AMD_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_AMD_LIBS%|\"${AMD_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_AR%|\"${AR}\"|" \
    -e "s|%OCTAVE_CONF_ARFLAGS%|\"${ARFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_ARPACK_CPPFLAGS%|\"${ARPACK_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_ARPACK_LDFLAGS%|\"${ARPACK_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_ARPACK_LIBS%|\"${ARPACK_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_BLAS_LIBS%|\"${BLAS_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_CAMD_CPPFLAGS%|\"${CAMD_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_CAMD_LDFLAGS%|\"${CAMD_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_CAMD_LIBS%|\"${CAMD_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_CARBON_LIBS%|\"${CARBON_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_CC%|\"${CC}\"|" \
    -e "s|%OCTAVE_CONF_CCOLAMD_CPPFLAGS%|\"${CCOLAMD_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_CCOLAMD_LDFLAGS%|\"${CCOLAMD_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_CCOLAMD_LIBS%|\"${CCOLAMD_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_CFLAGS%|\"${CFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_CHOLMOD_CPPFLAGS%|\"${CHOLMOD_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_CHOLMOD_LDFLAGS%|\"${CHOLMOD_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_CHOLMOD_LIBS%|\"${CHOLMOD_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_COLAMD_CPPFLAGS%|\"${COLAMD_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_COLAMD_LDFLAGS%|\"${COLAMD_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_COLAMD_LIBS%|\"${COLAMD_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_CPICFLAG%|\"${CPICFLAG}\"|" \
    -e "s|%OCTAVE_CONF_CPPFLAGS%|\"${CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_CURL_CPPFLAGS%|\"${CURL_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_CURL_LDFLAGS%|\"${CURL_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_CURL_LIBS%|\"${CURL_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_CXSPARSE_CPPFLAGS%|\"${CXSPARSE_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_CXSPARSE_LDFLAGS%|\"${CXSPARSE_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_CXSPARSE_LIBS%|\"${CXSPARSE_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_CXX%|\"${CXX}\"|" \
    -e "s|%OCTAVE_CONF_CXXCPP%|\"${CXXCPP}\"|" \
    -e "s|%OCTAVE_CONF_CXXFLAGS%|\"${CXXFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_CXXPICFLAG%|\"${CXXPICFLAG}\"|" \
    -e "s|%OCTAVE_CONF_DEFAULT_PAGER%|\"${DEFAULT_PAGER}\"|" \
    -e "s|%OCTAVE_CONF_DEFS%|\"${DEFS}\"|" \
    -e "s|%OCTAVE_CONF_DEPEND_FLAGS%|\"${DEPEND_FLAGS}\"|" \
    -e "s|%OCTAVE_CONF_DEPEND_EXTRA_SED_PATTERN%|\"${DEPEND_EXTRA_SED_PATTERN}\"|" \
    -e "s|%OCTAVE_CONF_DL_LD%|\"${DL_LD}\"|" \
    -e "s|%OCTAVE_CONF_DL_LDFLAGS%|\"${DL_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_DL_LIBS%|\"${DL_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_EXEEXT%|\"${EXEEXT}\"|" \
    -e "s|%OCTAVE_CONF_GCC_VERSION%|\"${GCC_VERSION}\"|" \
    -e "s|%OCTAVE_CONF_GXX_VERSION%|\"${GXX_VERSION}\"|" \
    -e "s|%OCTAVE_CONF_F77%|\"${F77}\"|" \
    -e "s|%OCTAVE_CONF_F77_FLOAT_STORE_FLAG%|\"${F77_FLOAT_STORE_FLAG}\"|" \
    -e "s|%OCTAVE_CONF_F77_INTEGER_8_FLAG%|\"${F77_INTEGER_8_FLAG}\"|" \
    -e "s|%OCTAVE_CONF_FFLAGS%|\"${FFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_FFTW3_CPPFLAGS%|\"${FFTW3_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_FFTW3_LDFLAGS%|\"${FFTW3_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_FFTW3_LIBS%|\"${FFTW3_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_FFTW3F_CPPFLAGS%|\"${FFTW3F_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_FFTW3F_LDFLAGS%|\"${FFTW3F_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_FFTW3F_LIBS%|\"${FFTW3F_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_FLIBS%|\"${FLIBS}\"|" \
    -e "s|%OCTAVE_CONF_FLTK_CPPFLAGS%|\"${FLTK_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_FLTK_LDFLAGS%|\"${FLTK_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_FLTK_LIBS%|\"${FLTK_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_FONTCONFIG_CPPFLAGS%|\"${FONTCONFIG_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_FONTCONFIG_LIBS%|\"${FONTCONFIG_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_FPICFLAG%|\"${FPICFLAG}\"|" \
    -e "s|%OCTAVE_CONF_FT2_CPPFLAGS%|\"${FT2_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_FT2_LIBS%|\"${FT2_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_GLPK_CPPFLAGS%|\"${GLPK_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_GLPK_LDFLAGS%|\"${GLPK_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_GLPK_LIBS%|\"${GLPK_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_GNUPLOT%|\"${GNUPLOT}\"|" \
    -e "s|%OCTAVE_CONF_HDF5_CPPFLAGS%|\"${HDF5_CPPFLAGS}\"|" | \
    $SED -e "s|%OCTAVE_CONF_HDF5_LDFLAGS%|\"${HDF5_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_HDF5_LIBS%|\"${HDF5_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_INCLUDEDIR%|\"${includedir}\"|" \
    -e "s|%OCTAVE_CONF_LAPACK_LIBS%|\"${LAPACK_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_LD_CXX%|\"${LD_CXX}\"|" \
    -e "s|%OCTAVE_CONF_LDFLAGS%|\"${LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_LD_STATIC_FLAG%|\"${LD_STATIC_FLAG}\"|" \
    -e "s|%OCTAVE_CONF_LEX%|\"${LEX}\"|" \
    -e "s|%OCTAVE_CONF_LEXLIB%|\"${LEXLIB}\"|" \
    -e "s|%OCTAVE_CONF_LFLAGS%|\"${LFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_LIBDIR%|\"${libdir}\"|" \
    -e "s|%OCTAVE_CONF_LIBEXT%|\"${LIBEXT}\"|" \
    -e "s|%OCTAVE_CONF_LIBOCTAVE%|\"${LIBOCTAVE}\"|" \
    -e "s|%OCTAVE_CONF_LIBOCTINTERP%|\"${LIBOCTINTERP}\"|" \
    -e "s|%OCTAVE_CONF_LIBS%|\"${LIBS}\"|" \
    -e "s|%OCTAVE_CONF_LLVM_CPPFLAGS%|\"${LLVM_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_LLVM_LDFLAGS%|\"${LLVM_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_LLVM_LIBS%|\"${LLVM_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_LN_S%|\"${LN_S}\"|" \
    -e "s|%OCTAVE_CONF_MAGICK_CPPFLAGS%|\"${MAGICK_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_MAGICK_LDFLAGS%|\"${MAGICK_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_MAGICK_LIBS%|\"${MAGICK_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_MKOCTFILE_AR%|\"${AR}\"|" \
    -e "s|%OCTAVE_CONF_MKOCTFILE_CC%|\"${CC}\"|" \
    -e "s|%OCTAVE_CONF_MKOCTFILE_CXX%|\"${CXX}\"|" \
    -e "s|%OCTAVE_CONF_MKOCTFILE_DL_LD%|\"${DL_LD}\"|" \
    -e "s|%OCTAVE_CONF_MKOCTFILE_DL_LDFLAGS%|\"${DL_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_MKOCTFILE_F77%|\"${F77}\"|" \
    -e "s|%OCTAVE_CONF_MKOCTFILE_LD_CXX%|\"${LD_CXX}\"|" \
    -e "s|%OCTAVE_CONF_MKOCTFILE_RANLIB%|\"${RANLIB}\"|" \
    -e "s|%OCTAVE_CONF_OCTAVE_LINK_DEPS%|\"${OCTAVE_LINK_DEPS}\"|" \
    -e "s|%OCTAVE_CONF_OCTAVE_LINK_OPTS%|\"${OCTAVE_LINK_OPTS}\"|" \
    -e "s|%OCTAVE_CONF_OCTINCLUDEDIR%|\"${octincludedir}\"|" \
    -e "s|%OCTAVE_CONF_OCTLIBDIR%|\"${octlibdir}\"|" \
    -e "s|%OCTAVE_CONF_OCT_LINK_DEPS%|\"${OCT_LINK_DEPS}\"|" \
    -e "s|%OCTAVE_CONF_OCT_LINK_OPTS%|\"${OCT_LINK_OPTS}\"|" \
    -e "s|%OCTAVE_CONF_OPENGL_LIBS%|\"${OPENGL_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_OSMESA_CPPFLAGS%|\"${OSMESA_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_OSMESA_LDFLAGS%|\"${OSMESA_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_OSMESA_LIBS%|\"${OSMESA_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_PCRE_CPPFLAGS%|\"${PCRE_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_PCRE_LIBS%|\"${PCRE_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_PREFIX%|\"${prefix}\"|" \
    -e "s|%OCTAVE_CONF_PTHREAD_CFLAGS%|\"${PTHREAD_CFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_PTHREAD_LIBS%|\"${PTHREAD_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_QHULL_CPPFLAGS%|\"${QHULL_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_QHULL_LDFLAGS%|\"${QHULL_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_QHULL_LIBS%|\"${QHULL_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_QRUPDATE_CPPFLAGS%|\"${QRUPDATE_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_QRUPDATE_LDFLAGS%|\"${QRUPDATE_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_QRUPDATE_LIBS%|\"${QRUPDATE_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_QT_CPPFLAGS%|\"${QT_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_QT_LDFLAGS%|\"${QT_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_QT_LIBS%|\"${QT_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_RANLIB%|\"${RANLIB}\"|" \
    -e "s|%OCTAVE_CONF_RDYNAMIC_FLAG%|\"${RDYNAMIC_FLAG}\"|" \
    -e "s|%OCTAVE_CONF_READLINE_LIBS%|\"${READLINE_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_SED%|\"${SED}\"|" \
    -e "s|%OCTAVE_CONF_SHARED_LIBS%|\"${SHARED_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_SHLEXT%|\"${SHLEXT}\"|" \
    -e "s|%OCTAVE_CONF_SHLLINKEXT%|\"${SHLLINKEXT}\"|" \
    -e "s|%OCTAVE_CONF_SHLEXT_VER%|\"${SHLEXT_VER}\"|" \
    -e "s|%OCTAVE_CONF_SH_LD%|\"${SH_LD}\"|" \
    -e "s|%OCTAVE_CONF_SH_LDFLAGS%|\"${SH_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_SONAME_FLAGS%|\"${SONAME_FLAGS}\"|" \
    -e "s|%OCTAVE_CONF_STATIC_LIBS%|\"${STATIC_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_TERM_LIBS%|\"${TERM_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_UMFPACK_CPPFLAGS%|\"${UMFPACK_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_UMFPACK_LDFLAGS%|\"${UMFPACK_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_UMFPACK_LIBS%|\"${UMFPACK_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_VERSION%|\"${version}\"|" \
    -e "s|%OCTAVE_CONF_WARN_CFLAGS%|\"${WARN_CFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_WARN_CXXFLAGS%|\"${WARN_CXXFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_X11_INCFLAGS%|\"${X11_INCFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_X11_LIBS%|\"${X11_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_XTRA_CFLAGS%|\"${XTRA_CFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_XTRA_CXXFLAGS%|\"${XTRA_CXXFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_YACC%|\"${YACC}\"|" \
    -e "s|%OCTAVE_CONF_YFLAGS%|\"${YFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_Z_CPPFLAGS%|\"${Z_CPPFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_Z_LDFLAGS%|\"${Z_LDFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_Z_LIBS%|\"${Z_LIBS}\"|" \
    -e "s|%OCTAVE_CONF_config_opts%|\"${config_opts}\"|"
