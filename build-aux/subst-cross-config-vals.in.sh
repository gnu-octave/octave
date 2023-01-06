#! /bin/sh

########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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

: ${SED=@SED@}

## The subst-config-vals and subst-cross-config-vals scripts differ only
## in the definitions of the following variables.
##
##   OCTAVE_CONF_MKOCTFILE_AR
##   OCTAVE_CONF_MKOCTFILE_CC
##   OCTAVE_CONF_MKOCTFILE_CXX
##   OCTAVE_CONF_MKOCTFILE_DL_LDFLAGS
##   OCTAVE_CONF_MKOCTFILE_F77
##   OCTAVE_CONF_MKOCTFILE_RANLIB

canonical_host_type="@canonical_host_type@"
DEFAULT_PAGER="@DEFAULT_PAGER@"
EXEEXT="@EXEEXT@"
man1ext="@man1ext@"
api_version="@OCTAVE_API_VERSION@"
OCTAVE_RELEASE=""
version="@PACKAGE_VERSION@"

prefix="@prefix@"
exec_prefix="@exec_prefix@"

archlibdir=`echo "@archlibdir@" | $SED "s|^${exec_prefix}/\\+||"`
bindir=`echo "@bindir@" | $SED "s|^${exec_prefix}/\\+||"`
libdir=`echo "@libdir@" | $SED "s|^${exec_prefix}/\\+||"`
libexecdir=`echo "@libexecdir@" | $SED "s|^${exec_prefix}/\\+||"`
localapiarchlibdir=`echo "@localapiarchlibdir@" | $SED "s|^${exec_prefix}/\\+||"`
localapioctfiledir=`echo "@localapioctfiledir@" | $SED "s|^${exec_prefix}/\\+||"`
localarchlibdir=`echo "@localarchlibdir@" | $SED "s|^${exec_prefix}/\\+||"`
localoctfiledir=`echo "@localoctfiledir@" | $SED "s|^${exec_prefix}/\\+||"`
localverarchlibdir=`echo "@localverarchlibdir@" | $SED "s|^${exec_prefix}/\\+||"`
localveroctfiledir=`echo "@localveroctfiledir@" | $SED "s|^${exec_prefix}/\\+||"`
octfiledir=`echo "@octfiledir@" | $SED "s|^${exec_prefix}/\\+||"`
octlibdir=`echo "@octlibdir@" | $SED "s|^${exec_prefix}/\\+||"`

datadir=`echo "@datadir@" | $SED "s|^${prefix}/\\+||"`
datarootdir=`echo "@datarootdir@" | $SED "s|^${prefix}/\\+||"`
doc_cache_file=`echo "@doc_cache_file@" | $SED "s|^${prefix}/\\+||"`
exec_prefix=`echo "@exec_prefix@" | $SED "s|^${prefix}/\\+||"`
fcnfiledir=`echo "@fcnfiledir@" | $SED "s|^${prefix}/\\+||"`
imagedir=`echo "@imagedir@" | $SED "s|^${prefix}/\\+||"`
includedir=`echo "@includedir@" | $SED "s|^${prefix}/\\+||"`
infodir=`echo "@infodir@" | $SED "s|^${prefix}/\\+||"`
infofile=`echo "@infofile@" | $SED "s|^${prefix}/\\+||"`
localapifcnfiledir=`echo "@localapifcnfiledir@" | $SED "s|^${prefix}/\\+||"`
localfcnfiledir=`echo "@localfcnfiledir@" | $SED "s|^${prefix}/\\+||"`
localstartupfiledir=`echo "@localstartupfiledir@" | $SED "s|^${prefix}/\\+||"`
localapiarchlibdir=`echo "@localapiarchlibdir@" | $SED "s|^${prefix}/\\+||"`
localverfcnfiledir=`echo "@localverfcnfiledir@" | $SED "s|^${prefix}/\\+||"`
man1dir=`echo "@man1dir@" | $SED "s|^${prefix}/\\+||"`
mandir=`echo "@mandir@" | $SED "s|^${prefix}/\\+||"`
octdatadir=`echo "@octdatadir@" | $SED "s|^${prefix}/\\+||"`
octdocdir=`echo "@octdocdir@" | $SED "s|^${prefix}/\\+||"`
octetcdir=`echo "@octetcdir@" | $SED "s|^${prefix}/\\+||"`
octfontsdir=`echo "@octfontsdir@" | $SED "s|^${prefix}/\\+||"`
octincludedir=`echo "@octincludedir@" | $SED "s|^${prefix}/\\+||"`
octlocaledir=`echo "@octlocaledir@" | $SED "s|^${prefix}/\\+||"`
octtestsdir=`echo "@octtestsdir@" | $SED "s|^${prefix}/\\+||"`
startupfiledir=`echo "@startupfiledir@" | $SED "s|^${prefix}/\\+||"`
texi_macros_file=`echo "@texi_macros_file@" | $SED "s|^${prefix}/\\+||"`

## FIXME: Some of these flags might contain double quotes.
##        Is it ok to use single quotes here?
if [ "x@OCTAVE_RELOCATE_ALL@" = "xyes" ]; then
  ## Replace portions of compiler flags that depend on prefix on target
  cppflags=`echo '@CPPFLAGS@' | $SED "s|@prefix@|\$\{prefix\}|g" | $SED 's|\"|\\\\\\\"|g'`
  fftw3f_ldflags=`echo '@FFTW3F_LDFLAGS@' | $SED "s|@prefix@|\$\{prefix\}|g" | $SED 's|\"|\\\\\\\"|g'`
  fftw3_ldflags=`echo '@FFTW3_LDFLAGS@' | $SED "s|@prefix@|\$\{prefix\}|g" | $SED 's|\"|\\\\\\\"|g'`
  flibs=`echo '@FLIBS@' | $SED "s|@prefix@|\$\{prefix\}|g" | $SED 's|\"|\\\\\\\"|g'`
  ldflags=`echo '@LDFLAGS@' | $SED "s|@prefix@|\$\{prefix\}|g" | $SED 's|\"|\\\\\\\"|g'`
  oct_link_opts=`echo '@OCT_LINK_OPTS@' | $SED "s|@prefix@|\$\{prefix\}|g" | $SED 's|\"|\\\\\\\"|g'`
else
  cppflags=`echo '@CPPFLAGS@' | $SED 's|\"|\\\\\\\"|g'`
  fftw3f_ldflags=`echo '@FFTW3F_LDFLAGS@' | $SED 's|\"|\\\\\\\"|g'`
  fftw3_ldflags=`echo '@FFTW3_LDFLAGS@' | $SED 's|\"|\\\\\\\"|g'`
  flibs=`echo '@FLIBS@' | $SED 's|\"|\\\\\\\"|g'`
  ldflags=`echo '@LDFLAGS@' | $SED 's|\"|\\\\\\\"|g'`
  oct_link_opts=`echo '@OCT_LINK_OPTS@' | $SED 's|\"|\\\\\\\"|g'`
fi


srcdir="@srcdir@"
top_srcdir="@top_srcdir@"
abs_srcdir="@abs_srcdir@"
abs_top_srcdir="@abs_top_srcdir@"

NO_OCT_FILE_STRIP="@NO_OCT_FILE_STRIP@"
AMD_CPPFLAGS="@AMD_CPPFLAGS@"
AMD_LDFLAGS="@AMD_LDFLAGS@"
AMD_LIBS="@AMD_LIBS@"
AR="@AR@"
ARFLAGS="@ARFLAGS@"
ARPACK_CPPFLAGS="@ARPACK_CPPFLAGS@"
ARPACK_LDFLAGS="@ARPACK_LDFLAGS@"
ARPACK_LIBS="@ARPACK_LIBS@"
BLAS_LIBS="@BLAS_LIBS@"
CAMD_CPPFLAGS="@CAMD_CPPFLAGS@"
CAMD_LDFLAGS="@CAMD_LDFLAGS@"
CAMD_LIBS="@CAMD_LIBS@"
CARBON_LIBS="@CARBON_LIBS@"
CC="@CC@"
CCOLAMD_CPPFLAGS="@CCOLAMD_CPPFLAGS@"
CCOLAMD_LDFLAGS="@CCOLAMD_LDFLAGS@"
CCOLAMD_LIBS="@CCOLAMD_LIBS@"
CFLAGS="@CFLAGS@"
CHOLMOD_CPPFLAGS="@CHOLMOD_CPPFLAGS@"
CHOLMOD_LDFLAGS="@CHOLMOD_LDFLAGS@"
CHOLMOD_LIBS="@CHOLMOD_LIBS@"
COLAMD_CPPFLAGS="@COLAMD_CPPFLAGS@"
COLAMD_LDFLAGS="@COLAMD_LDFLAGS@"
COLAMD_LIBS="@COLAMD_LIBS@"
CPICFLAG="@CPICFLAG@"
CURL_CPPFLAGS="@CURL_CPPFLAGS@"
CURL_LDFLAGS="@CURL_LDFLAGS@"
CURL_LIBS="@CURL_LIBS@"
CXSPARSE_CPPFLAGS="@CXSPARSE_CPPFLAGS@"
CXSPARSE_LDFLAGS="@CXSPARSE_LDFLAGS@"
CXSPARSE_LIBS="@CXSPARSE_LIBS@"
CXX="@CXX@"
CXXCPP="@CXXCPP@"
CXXFLAGS="@CXXFLAGS@"
CXXPICFLAG="@CXXPICFLAG@"
DEFAULT_PAGER="@DEFAULT_PAGER@"
DEFS="@DEFS@"
DEPEND_FLAGS="@DEPEND_FLAGS@"
DEPEND_EXTRA_SED_PATTERN="@DEPEND_EXTRA_SED_PATTERN@"
DL_LDFLAGS="@DL_LDFLAGS@"
EXEEXT="@EXEEXT@"
GCC_VERSION="@GCC_VERSION@"
GXX_VERSION="@GXX_VERSION@"
F77="@F77@"
F77_FLOAT_STORE_FLAG="@F77_FLOAT_STORE_FLAG@"
F77_INTEGER_8_FLAG="@F77_INTEGER_8_FLAG@"
FFLAGS="@FFLAGS@"
FFTW3_CPPFLAGS="@FFTW3_CPPFLAGS@"
FFTW3_LIBS="@FFTW3_LIBS@"
FFTW3F_CPPFLAGS="@FFTW3F_CPPFLAGS@"
FFTW3F_LIBS="@FFTW3F_LIBS@"
FLTK_CPPFLAGS="@FLTK_CPPFLAGS@"
FLTK_LDFLAGS="@FLTK_LDFLAGS@"
FLTK_LIBS="@FLTK_LIBS@"
FONTCONFIG_CPPFLAGS="@FONTCONFIG_CPPFLAGS@"
FONTCONFIG_LIBS="@FONTCONFIG_LIBS@"
FPICFLAG="@FPICFLAG@"
FT2_CPPFLAGS="@FT2_CPPFLAGS@"
FT2_LIBS="@FT2_LIBS@"
GLPK_CPPFLAGS="@GLPK_CPPFLAGS@"
GLPK_LDFLAGS="@GLPK_LDFLAGS@"
GLPK_LIBS="@GLPK_LIBS@"
GNUPLOT="@GNUPLOT@"
HDF5_CPPFLAGS="@HDF5_CPPFLAGS@"
HDF5_LDFLAGS="@HDF5_LDFLAGS@"
HDF5_LIBS="@HDF5_LIBS@"
KLU_CPPFLAGS="@KLU_CPPFLAGS@"
KLU_LDFLAGS="@KLU_LDFLAGS@"
KLU_LIBS="@KLU_LIBS@"
LAPACK_LIBS="@LAPACK_LIBS@"
LD_STATIC_FLAG="@LD_STATIC_FLAG@"
LEX="@LEX@"
LEXLIB="@LEXLIB@"
LFLAGS="@LFLAGS@"
LIBOCTAVE="@LIBOCTAVE@"
LIBOCTINTERP="@LIBOCTINTERP@"
LIBS="@LIBS@"
LN_S="@LN_S@"
MAGICK_CPPFLAGS="@MAGICK_CPPFLAGS@"
MAGICK_LDFLAGS="@MAGICK_LDFLAGS@"
MAGICK_LIBS="@MAGICK_LIBS@"
MKOCTFILE_OCTAVE_LINK_DEPS="@MKOCTFILE_OCTAVE_LINK_DEPS@"
MKOCTFILE_OCT_LINK_DEPS="@MKOCTFILE_OCT_LINK_DEPS@"
OCTAVE_LINK_DEPS="@OCTAVE_LINK_DEPS@"
OCTAVE_LINK_OPTS="@OCTAVE_LINK_OPTS@"
OCT_LINK_DEPS="@OCT_LINK_DEPS@"
OPENGL_LIBS="@OPENGL_LIBS@"
PCRE_CPPFLAGS="@PCRE_CPPFLAGS@"
PCRE_LDFLAGS="@PCRE_LDFLAGS@"
PCRE_LIBS="@PCRE_LIBS@"
PTHREAD_CFLAGS="@PTHREAD_CFLAGS@"
PTHREAD_LIBS="@PTHREAD_LIBS@"
QHULL_CPPFLAGS="@QHULL_CPPFLAGS@"
QHULL_LDFLAGS="@QHULL_LDFLAGS@"
QHULL_LIBS="@QHULL_LIBS@"
QRUPDATE_CPPFLAGS="@QRUPDATE_CPPFLAGS@"
QRUPDATE_LDFLAGS="@QRUPDATE_LDFLAGS@"
QRUPDATE_LIBS="@QRUPDATE_LIBS@"
QT_CPPFLAGS="@QT_CPPFLAGS@"
QT_LDFLAGS="@QT_LDFLAGS@"
QT_LIBS="@QT_LIBS@"
QT_OPENGL_LIBS="@QT_OPENGL_LIBS@"
RANLIB="@RANLIB@"
RDYNAMIC_FLAG="@RDYNAMIC_FLAG@"
READLINE_LIBS="@LIBREADLINE@"
SHARED_LIBS="@SHARED_LIBS@"
SH_LDFLAGS="@SH_LDFLAGS@"
STATIC_LIBS="@STATIC_LIBS@"
SUITESPARSECONFIG_LIBS="@SUITESPARSECONFIG_LIBS@"
SUNDIALS_IDA_CPPFLAGS="@SUNDIALS_IDA_CPPFLAGS@"
SUNDIALS_IDA_LDFLAGS="@SUNDIALS_IDA_LDFLAGS@"
SUNDIALS_IDA_LIBS="@SUNDIALS_IDA_LIBS@"
SUNDIALS_NVECSERIAL_CPPFLAGS="@SUNDIALS_NVECSERIAL_CPPFLAGS@"
SUNDIALS_NVECSERIAL_LDFLAGS="@SUNDIALS_NVECSERIAL_LDFLAGS@"
SUNDIALS_NVECSERIAL_LIBS="@SUNDIALS_NVECSERIAL_LIBS@"
SUNDIALS_SUNLINSOLKLU_CPPFLAGS="@SUNDIALS_SUNLINSOLKLU_CPPFLAGS@"
SUNDIALS_SUNLINSOLKLU_LDFLAGS="@SUNDIALS_SUNLINSOLKLU_LDFLAGS@"
SUNDIALS_SUNLINSOLKLU_LIBS="@SUNDIALS_SUNLINSOLKLU_LIBS@"
UMFPACK_CPPFLAGS="@UMFPACK_CPPFLAGS@"
UMFPACK_LDFLAGS="@UMFPACK_LDFLAGS@"
UMFPACK_LIBS="@UMFPACK_LIBS@"
version="@PACKAGE_VERSION@"
WARN_CFLAGS="@WARN_CFLAGS@"
WARN_CXXFLAGS="@WARN_CXXFLAGS@"
X11_INCFLAGS="@X11_INCFLAGS@"
X11_LIBS="@X11_LIBS@"
XTRA_CFLAGS="@XTRA_CFLAGS@"
XTRA_CXXFLAGS="@XTRA_CXXFLAGS@"
YACC="@YACC@"
YFLAGS="@YFLAGS@"
Z_CPPFLAGS="@Z_CPPFLAGS@"
Z_LDFLAGS="@Z_LDFLAGS@"
Z_LIBS="@Z_LIBS@"
config_opts="@config_opts@"

$SED \
  -e "s|%NO_EDIT_WARNING%|DO NOT EDIT!  Generated automatically by subst-cross-config-vals.|" \
  -e "s|%NO_OCT_FILE_STRIP%|${NO_OCT_FILE_STRIP}|" \
  -e "s|%OCTAVE_API_VERSION%|\"${api_version}\"|" \
  -e "s|%OCTAVE_ARCHLIBDIR%|\"${archlibdir}\"|" \
  -e "s|%OCTAVE_BINDIR%|\"${bindir}\"|" \
  -e "s|%OCTAVE_BINDIR%|\"${bindir}\"|" \
  -e "s|%OCTAVE_CANONICAL_HOST_TYPE%|\"${canonical_host_type}\"|" \
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
  -e "s|%OCTAVE_CONF_CPPFLAGS%|\"${cppflags}\"|" \
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
  -e "s|%OCTAVE_CONF_DEPEND_EXTRA_SED_PATTERN%|\"${DEPEND_EXTRA_SED_PATTERN}\"|" \
  -e "s|%OCTAVE_CONF_DEPEND_FLAGS%|\"${DEPEND_FLAGS}\"|" \
  -e "s|%OCTAVE_CONF_DL_LDFLAGS%|\"${DL_LDFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_EXEC_PREFIX%|\"${exec_prefix}\"|" \
  -e "s|%OCTAVE_CONF_EXEEXT%|\"${EXEEXT}\"|" \
  -e "s|%OCTAVE_CONF_F77%|\"${F77}\"|" \
  -e "s|%OCTAVE_CONF_F77_FLOAT_STORE_FLAG%|\"${F77_FLOAT_STORE_FLAG}\"|" \
  -e "s|%OCTAVE_CONF_F77_INTEGER_8_FLAG%|\"${F77_INTEGER_8_FLAG}\"|" \
  -e "s|%OCTAVE_CONF_FFLAGS%|\"${FFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_FFTW3F_CPPFLAGS%|\"${FFTW3F_CPPFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_FFTW3F_LDFLAGS%|\"${fftw3f_ldflags}\"|" \
  -e "s|%OCTAVE_CONF_FFTW3F_LIBS%|\"${FFTW3F_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_FFTW3_CPPFLAGS%|\"${FFTW3_CPPFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_FFTW3_LDFLAGS%|\"${fftw3_ldflags}\"|" \
  -e "s|%OCTAVE_CONF_FFTW3_LIBS%|\"${FFTW3_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_FLIBS%|\"${flibs}\"|" \
  -e "s|%OCTAVE_CONF_FLTK_CPPFLAGS%|\"${FLTK_CPPFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_FLTK_LDFLAGS%|\"${FLTK_LDFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_FLTK_LIBS%|\"${FLTK_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_FONTCONFIG_CPPFLAGS%|\"${FONTCONFIG_CPPFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_FONTCONFIG_LIBS%|\"${FONTCONFIG_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_FPICFLAG%|\"${FPICFLAG}\"|" \
  -e "s|%OCTAVE_CONF_FT2_CPPFLAGS%|\"${FT2_CPPFLAGS}\"|" | \
  $SED \
  -e "s|%OCTAVE_CONF_FT2_LIBS%|\"${FT2_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_GCC_VERSION%|\"${GCC_VERSION}\"|" \
  -e "s|%OCTAVE_CONF_GLPK_CPPFLAGS%|\"${GLPK_CPPFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_GLPK_LDFLAGS%|\"${GLPK_LDFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_GLPK_LIBS%|\"${GLPK_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_GNUPLOT%|\"${GNUPLOT}\"|" \
  -e "s|%OCTAVE_CONF_GXX_VERSION%|\"${GXX_VERSION}\"|" \
  -e "s|%OCTAVE_CONF_HDF5_CPPFLAGS%|\"${HDF5_CPPFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_HDF5_LDFLAGS%|\"${HDF5_LDFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_HDF5_LIBS%|\"${HDF5_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_INCLUDEDIR%|\"${includedir}\"|" \
  -e "s|%OCTAVE_CONF_KLU_CPPFLAGS%|\"${KLU_CPPFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_KLU_LDFLAGS%|\"${KLU_LDFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_KLU_LIBS%|\"${KLU_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_LAPACK_LIBS%|\"${LAPACK_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_LDFLAGS%|\"${ldflags}\"|" \
  -e "s|%OCTAVE_CONF_LD_STATIC_FLAG%|\"${LD_STATIC_FLAG}\"|" \
  -e "s|%OCTAVE_CONF_LEX%|\"${LEX}\"|" \
  -e "s|%OCTAVE_CONF_LEXLIB%|\"${LEXLIB}\"|" \
  -e "s|%OCTAVE_CONF_LFLAGS%|\"${LFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_LIBDIR%|\"${libdir}\"|" \
  -e "s|%OCTAVE_CONF_LIBOCTAVE%|\"${LIBOCTAVE}\"|" \
  -e "s|%OCTAVE_CONF_LIBOCTINTERP%|\"${LIBOCTINTERP}\"|" \
  -e "s|%OCTAVE_CONF_LIBS%|\"${LIBS}\"|" \
  -e "s|%OCTAVE_CONF_LN_S%|\"${LN_S}\"|" \
  -e "s|%OCTAVE_CONF_MAGICK_CPPFLAGS%|\"${MAGICK_CPPFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_MAGICK_LDFLAGS%|\"${MAGICK_LDFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_MAGICK_LIBS%|\"${MAGICK_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_MKOCTFILE_AR%|\"${AR}\"|" \
  -e "s|%OCTAVE_CONF_MKOCTFILE_CC%|\"${CC}\"|" \
  -e "s|%OCTAVE_CONF_MKOCTFILE_CXX%|\"${CXX}\"|" \
  -e "s|%OCTAVE_CONF_MKOCTFILE_DL_LDFLAGS%|\"${DL_LDFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_MKOCTFILE_F77%|\"${F77}\"|" \
  -e "s|%OCTAVE_CONF_MKOCTFILE_OCTAVE_LINK_DEPS%|\"${MKOCTFILE_OCTAVE_LINK_DEPS}\"|" \
  -e "s|%OCTAVE_CONF_MKOCTFILE_OCT_LINK_DEPS%|\"${MKOCTFILE_OCT_LINK_DEPS}\"|" \
  -e "s|%OCTAVE_CONF_MKOCTFILE_RANLIB%|\"${RANLIB}\"|" \
  -e "s|%OCTAVE_CONF_OCTAVE_LINK_DEPS%|\"${OCTAVE_LINK_DEPS}\"|" \
  -e "s|%OCTAVE_CONF_OCTAVE_LINK_OPTS%|\"${OCTAVE_LINK_OPTS}\"|" \
  -e "s|%OCTAVE_CONF_OCTINCLUDEDIR%|\"${octincludedir}\"|" \
  -e "s|%OCTAVE_CONF_OCTLIBDIR%|\"${octlibdir}\"|" \
  -e "s|%OCTAVE_CONF_OCT_LINK_DEPS%|\"${OCT_LINK_DEPS}\"|" \
  -e "s|%OCTAVE_CONF_OCT_LINK_OPTS%|\"${oct_link_opts}\"|" \
  -e "s|%OCTAVE_CONF_OPENGL_LIBS%|\"${OPENGL_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_PCRE_CPPFLAGS%|\"${PCRE_CPPFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_PCRE_LDFLAGS%|\"${PCRE_LDFLAGS}\"|" \
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
  -e "s|%OCTAVE_CONF_QT_OPENGL_LIBS%|\"${QT_OPENGL_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_RANLIB%|\"${RANLIB}\"|" \
  -e "s|%OCTAVE_CONF_RDYNAMIC_FLAG%|\"${RDYNAMIC_FLAG}\"|" \
  -e "s|%OCTAVE_CONF_READLINE_LIBS%|\"${READLINE_LIBS}\"|" | \
  $SED \
  -e "s|%OCTAVE_CONF_SHARED_LIBS%|\"${SHARED_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_SH_LDFLAGS%|\"${SH_LDFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_STATIC_LIBS%|\"${STATIC_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_SUITESPARSECONFIG_LIBS%|\"${SUITESPARSECONFIG_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_SUNDIALS_IDA_CPPFLAGS%|\"${SUNDIALS_IDA_CPPFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_SUNDIALS_IDA_LDFLAGS%|\"${SUNDIALS_IDA_LDFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_SUNDIALS_IDA_LIBS%|\"${SUNDIALS_IDA_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_SUNDIALS_NVECSERIAL_CPPFLAGS%|\"${SUNDIALS_NVECSERIAL_CPPFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_SUNDIALS_NVECSERIAL_LDFLAGS%|\"${SUNDIALS_NVECSERIAL_LDFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_SUNDIALS_NVECSERIAL_LIBS%|\"${SUNDIALS_NVECSERIAL_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_SUNDIALS_SUNLINSOLKLU_CPPFLAGS%|\"${SUNDIALS_SUNLINSOLKLU_CPPFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_SUNDIALS_SUNLINSOLKLU_LDFLAGS%|\"${SUNDIALS_SUNLINSOLKLU_LDFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_SUNDIALS_SUNLINSOLKLU_LIBS%|\"${SUNDIALS_SUNLINSOLKLU_LIBS}\"|" \
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
  -e "s|%OCTAVE_CONF_config_opts%|\"${config_opts}\"|" \
  -e "s|%OCTAVE_DATADIR%|\"${datadir}\"|" \
  -e "s|%OCTAVE_DATAROOTDIR%|\"${datarootdir}\"|" \
  -e "s|%OCTAVE_DEFAULT_PAGER%|\"${DEFAULT_PAGER}\"|" \
  -e "s|%OCTAVE_DOCDIR%|\"${docdir}\"|" \
  -e "s|%OCTAVE_DOC_CACHE_FILE%|\"${doc_cache_file}\"|" \
  -e "s|%OCTAVE_EXEC_PREFIX%|\"${exec_prefix}\"|" \
  -e "s|%OCTAVE_EXEEXT%|\"${EXEEXT}\"|" \
  -e "s|%OCTAVE_FCNFILEDIR%|\"${fcnfiledir}\"|" \
  -e "s|%OCTAVE_IMAGEDIR%|\"${imagedir}\"|" \
  -e "s|%OCTAVE_INCLUDEDIR%|\"${includedir}\"|" \
  -e "s|%OCTAVE_INFODIR%|\"${infodir}\"|" \
  -e "s|%OCTAVE_INFOFILE%|\"${infofile}\"|" \
  -e "s|%OCTAVE_LIBDIR%|\"${libdir}\"|" \
  -e "s|%OCTAVE_LIBEXECDIR%|\"${libexecdir}\"|" \
  -e "s|%OCTAVE_LOCALAPIARCHLIBDIR%|\"${localapiarchlibdir}\"|" \
  -e "s|%OCTAVE_LOCALAPIFCNFILEDIR%|\"${localapifcnfiledir}\"|" \
  -e "s|%OCTAVE_LOCALAPIOCTFILEDIR%|\"${localapioctfiledir}\"|" \
  -e "s|%OCTAVE_LOCALARCHLIBDIR%|\"${localarchlibdir}\"|" \
  -e "s|%OCTAVE_LOCALFCNFILEDIR%|\"${localfcnfiledir}\"|" \
  -e "s|%OCTAVE_LOCALOCTFILEDIR%|\"${localoctfiledir}\"|" \
  -e "s|%OCTAVE_LOCALSTARTUPFILEDIR%|\"${localstartupfiledir}\"|" \
  -e "s|%OCTAVE_LOCALVERARCHLIBDIR%|\"${localverarchlibdir}\"|" \
  -e "s|%OCTAVE_LOCALVERFCNFILEDIR%|\"${localverfcnfiledir}\"|" \
  -e "s|%OCTAVE_LOCALVEROCTFILEDIR%|\"${localveroctfiledir}\"|" \
  -e "s|%OCTAVE_MAN1DIR%|\"${man1dir}\"|" \
  -e "s|%OCTAVE_MAN1EXT%|\"${man1ext}\"|" \
  -e "s|%OCTAVE_MANDIR%|\"${mandir}\"|" \
  -e "s|%OCTAVE_OCTDATADIR%|\"${octdatadir}\"|" \
  -e "s|%OCTAVE_OCTDOCDIR%|\"${octdocdir}\"|" \
  -e "s|%OCTAVE_OCTETCDIR%|\"${octetcdir}\"|" \
  -e "s|%OCTAVE_OCTFILEDIR%|\"${octfiledir}\"|" \
  -e "s|%OCTAVE_OCTFONTSDIR%|\"${octfontsdir}\"|" \
  -e "s|%OCTAVE_OCTINCLUDEDIR%|\"${octincludedir}\"|" \
  -e "s|%OCTAVE_OCTLIBDIR%|\"${octlibdir}\"|" \
  -e "s|%OCTAVE_OCTLOCALEDIR%|\"${octlocaledir}\"|" \
  -e "s|%OCTAVE_OCTTESTSDIR%|\"${octtestsdir}\"|" \
  -e "s|%OCTAVE_PREFIX%|\"${prefix}\"|" \
  -e "s|%OCTAVE_RELEASE%|\"${OCTAVE_RELEASE}\"|" \
  -e "s|%OCTAVE_STARTUPFILEDIR%|\"${startupfiledir}\"|" \
  -e "s|%OCTAVE_TEXI_MACROS_FILE%|\"${texi_macros_file}\"|" \
  -e "s|%OCTAVE_VERSION%|\"${version}\"|"
