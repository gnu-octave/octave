export AWK
export GREP
export FIND
export SED
export SHELL
export PERL

version = $(OCTAVE_VERSION)
api_version = $(OCTAVE_API_VERSION)

## AM_LIBTOOLFLAGS = --silent

AM_LFLAGS = @LFLAGS@

AM_YFLAGS = -dv

SHLLINKEXT=

LIBEXT = a

# Fortran compiler flags.

AM_FFLAGS = @FFLAGS@
ALL_FFLAGS = $(FFLAGS)

# C compiler flags.

AM_CFLAGS = $(XTRA_CFLAGS)

# ifeq ($(INCLUDE_DEPS),no)
#   omit_deps = true;
# endif

# C++ compiler flags.

AM_CXXFLAGS = $(XTRA_CXXFLAGS)

FFTW_XCPPFLAGS = $(FFTW3_CPPFLAGS) $(FFTW3F_CPPFLAGS)
FFTW_XLDFLAGS = $(FFTW3_LDFLAGS) $(FFTW3F_LDFLAGS)
FFTW_XLIBS = $(FFTW3_LIBS) $(FFTW3F_LIBS)

## Alias CPPFLAGS to CFLAGS for fontconfig and freetype.
## This is closer to the true meaning of `pkg-config --cflags` output.
FONTCONFIG_CPPFLAGS = $(FONTCONFIG_CFLAGS)
FT2_CPPFLAGS = $(FT2_CFLAGS)

SPARSE_XCPPFLAGS = \
  $(CHOLMOD_CPPFLAGS) $(UMFPACK_CPPFLAGS) \
  $(AMD_CPPFLAGS) $(CAMD_CPPFLAGS) $(COLAMD_CPPFLAGS) \
  $(CCOLAMD_CPPFLAGS) $(CXSPARSE_CPPFLAGS)

SPARSE_XLDFLAGS = \
  $(CHOLMOD_LDFLAGS) $(UMFPACK_LDFLAGS) \
  $(AMD_LDFLAGS) $(CAMD_LDFLAGS) $(COLAMD_LDFLAGS) \
  $(CCOLAMD_LDFLAGS) $(CXSPARSE_LDFLAGS)

## Order matters, at least on some systems (Cygwin, for example).
SPARSE_XLIBS = \
  $(CHOLMOD_LIBS) $(UMFPACK_LIBS) \
  $(AMD_LIBS) $(CAMD_LIBS) $(COLAMD_LIBS) \
  $(CCOLAMD_LIBS) $(CXSPARSE_LIBS)

# Miscellaneous

if AMCOND_ADDRESS_SANITIZER_ENABLED
  ADDRESS_SANITIZER_OPTIONS="symbolize=1"
endif

# The arguments passed to configure.

CONFIG_SUBDIRS = @subdirs@

# Where Octave will look for startup files
startupfiledir = ${fcnfiledir}/startup
localstartupfiledir = ${localfcnfiledir}/startup

null =
ldpreloadsep = $(null)@ldpreloadsep@$(null)

# ==================== Octave-specific Makefile rules ====================

# The following pattern rules and the substitution functions require
# GNU make.  If you don't have it, get it!

define simple_move_if_change_rule
  if [ -s $@-t ]; then \
    $(SHELL) $(top_srcdir)/build-aux/move-if-change $@-t $@; \
  else \
    echo "$@-t is empty!" 1>&2; \
    rm -f $@-t; \
    exit 1; \
  fi
endef

define cp_update_rule
  if [ "x$(srcdir)" != "x." ] && [ -f $(srcdir)/$@ ] && [ ! -f $@ ]; then \
    cp $(srcdir)/$@ $@; \
    touch -r $(srcdir)/$@ $@; \
  fi
endef

## The do_subst_config_vals and do_subst_cross_config_vals differ only in
## the definitions of the following variables:
##
##   OCTAVE_CONF_MKOCTFILE_AR
##   OCTAVE_CONF_MKOCTFILE_CC
##   OCTAVE_CONF_MKOCTFILE_CXX
##   OCTAVE_CONF_MKOCTFILE_DL_LD
##   OCTAVE_CONF_MKOCTFILE_DL_LDFLAGS
##   OCTAVE_CONF_MKOCTFILE_F77
##   OCTAVE_CONF_MKOCTFILE_LD_CXX
##   OCTAVE_CONF_MKOCTFILE_RANLIB

## To avoid shell command line limits, break the replacement patterns
## into two roughly equal sized parts.

define do_subst_config_vals
  $(SED) < $< \
    -e "s|%NO_EDIT_WARNING%|DO NOT EDIT!  Generated automatically from $(<F) by Make.|" \
    -e "s|%NO_OCT_FILE_STRIP%|$NO_OCT_FILE_STRIP}|" \
    -e "s|%OCTAVE_BINDIR%|\"$bindir}\"|" \
    -e "s|%OCTAVE_CONF_ALL_CFLAGS%|\"${ALL_CFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_ALL_CXXFLAGS%|\"${ALL_CXXFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_ALL_FFLAGS%|\"${ALL_FFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_ALL_LDFLAGS%|\"${ALL_LDFLAGS}\"|" \
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
    -e "s|%OCTAVE_CONF_CANONICAL_HOST_TYPE%|\"${canonical_host_type}\"|" \
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
    $(SED) -e "s|%OCTAVE_CONF_HDF5_LDFLAGS%|\"${HDF5_LDFLAGS}\"|" \
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
    -e "s|%OCTAVE_CONF_LIBFLAGS%|\"${LIBFLAGS}\"|" \
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
    -e 's|%OCTAVE_CONF_MKOCTFILE_AR%|\"${MKOCTFILE_AR}\"|' \
    -e 's|%OCTAVE_CONF_MKOCTFILE_CC%|\"${MKOCTFILE_CC}\"|' \
    -e 's|%OCTAVE_CONF_MKOCTFILE_CXX%|\"${MKOCTFILE_CXX}\"|' \
    -e 's|%OCTAVE_CONF_MKOCTFILE_DL_LD%|\"${MKOCTFILE_DL_LD}\"|' \
    -e 's|%OCTAVE_CONF_MKOCTFILE_DL_LDFLAGS%|\"${MKOCTFILE_DL_LDFLAGS}\"|' \
    -e 's|%OCTAVE_CONF_MKOCTFILE_F77%|\"${MKOCTFILE_F77}\"|' \
    -e 's|%OCTAVE_CONF_MKOCTFILE_LD_CXX%|\"${MKOCTFILE_LD_CXX}\"|' \
    -e 's|%OCTAVE_CONF_MKOCTFILE_RANLIB%|\"${MKOCTFILE_RANLIB}\"|' \
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
    -e "s|%OCTAVE_CONF_QHULL_CPPFLAGS%|\"${QHULL_CPPFLAGSS}\"|" \
    -e "s|%OCTAVE_CONF_QHULL_LDFLAGS%|\"${QHULL_LDFLAGSS}\"|" \
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
    -e "s|%OCTAVE_CONF_config_opts%|\"${config_opts}\"|" > $@-t && \
  $(simple_move_if_change_rule)
endef

define do_subst_cross_config_vals
  $(SED) < $< \
    -e "s|%NO_EDIT_WARNING%|DO NOT EDIT!  Generated automatically from $(<F) by Make.|" \
    -e "s|%NO_OCT_FILE_STRIP%|${NO_OCT_FILE_STRIP}|" \
    -e "s|%OCTAVE_BINDIR%|\"${bindir}\"|" \
    -e "s|%OCTAVE_CONF_ALL_CFLAGS%|\"${ALL_CFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_ALL_CXXFLAGS%|\"${ALL_CXXFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_ALL_FFLAGS%|\"${ALL_FFLAGS}\"|" \
    -e "s|%OCTAVE_CONF_ALL_LDFLAGS%|\"${ALL_LDFLAGS}\"|" \
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
    -e "s|%OCTAVE_CONF_CANONICAL_HOST_TYPE%|\"${canonical_host_type}\"|" \
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
    $(SED) -e "s|%OCTAVE_CONF_HDF5_LDFLAGS%|\"${HDF5_LDFLAGS}\"|" \
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
    -e "s|%OCTAVE_CONF_LIBFLAGS%|\"${LIBFLAGS}\"|" \
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
    -e 's|%OCTAVE_CONF_MKOCTFILE_AR%|\"${AR}\"|' \
    -e 's|%OCTAVE_CONF_MKOCTFILE_CC%|\"${CC}\"|' \
    -e 's|%OCTAVE_CONF_MKOCTFILE_CXX%|\"${CXX}\"|' \
    -e 's|%OCTAVE_CONF_MKOCTFILE_DL_LD%|\"${DL_LD}\"|' \
    -e 's|%OCTAVE_CONF_MKOCTFILE_DL_LDFLAGS%|\"${DL_LDFLAGS}\"|' \
    -e 's|%OCTAVE_CONF_MKOCTFILE_F77%|\"${F77}\"|' \
    -e 's|%OCTAVE_CONF_MKOCTFILE_LD_CXX%|\"${LD_CXX}\"|' \
    -e 's|%OCTAVE_CONF_MKOCTFILE_RANLIB%|\"${RANLIB}\"|' \
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
    -e "s|%OCTAVE_CONF_QHULL_CPPFLAGS%|\"${QHULL_CPPFLAGSS}\"|" \
    -e "s|%OCTAVE_CONF_QHULL_LDFLAGS%|\"${QHULL_LDFLAGSS}\"|" \
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
    -e "s|%OCTAVE_CONF_config_opts%|\"${config_opts}\"|" > $@-t && \
  $(simple_move_if_change_rule)
endef

define do_subst_default_vals
  $(SED) < $< > $@-t \
    -e "s|%NO_EDIT_WARNING%|DO NOT EDIT!  Generated automatically from $(<F) by Make.|" \
    -e "s|%OCTAVE_ARCHLIBDIR%|\"${archlibdir}\"|" \
    -e "s|%OCTAVE_BINDIR%|\"${bindir}\"|" \
    -e "s|%OCTAVE_CANONICAL_HOST_TYPE%|\"${canonical_host_type}\"|" \
    -e "s|%OCTAVE_DATADIR%|\"${datadir}\"|" \
    -e "s|%OCTAVE_DATAROOTDIR%|\"${datarootdir}\"|" \
    -e "s|%OCTAVE_DEFAULT_PAGER%|\"${DEFAULT_PAGER}\"|" \
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
    -e "s|%OCTAVE_LOCALAPIFCNFILEDIR%|\"${localapifcnfiledir}\"|" \
    -e "s|%OCTAVE_LOCALAPIOCTFILEDIR%|\"${localapioctfiledir}\"|" \
    -e "s|%OCTAVE_LOCALARCHLIBDIR%|\"${localarchlibdir}\"|" \
    -e "s|%OCTAVE_LOCALFCNFILEDIR%|\"${localfcnfiledir}\"|" \
    -e "s|%OCTAVE_LOCALOCTFILEDIR%|\"${localoctfiledir}\"|" \
    -e "s|%OCTAVE_LOCALSTARTUPFILEDIR%|\"${localstartupfiledir}\"|" \
    -e "s|%OCTAVE_LOCALAPIARCHLIBDIR%|\"${localapiarchlibdir}\"|" \
    -e "s|%OCTAVE_LOCALVERARCHLIBDIR%|\"${localverarchlibdir}\"|" \
    -e "s|%OCTAVE_LOCALVERFCNFILEDIR%|\"${localverfcnfiledir}\"|" \
    -e "s|%OCTAVE_LOCALVEROCTFILEDIR%|\"${localveroctfiledir}\"|" \
    -e "s|%OCTAVE_MAN1DIR%|\"${man1dir}\"|" \
    -e "s|%OCTAVE_MAN1EXT%|\"${man1ext}\"|" \
    -e "s|%OCTAVE_MANDIR%|\"${mandir}\"|" \
    -e "s|%OCTAVE_OCTDATADIR%|\"${octdatadir}\"|" \
    -e "s|%OCTAVE_OCTFILEDIR%|\"${octfiledir}\"|" \
    -e "s|%OCTAVE_OCTETCDIR%|\"${octetcdir}\"|" \
    -e "s|%OCTAVE_OCTINCLUDEDIR%|\"${octincludedir}\"|" \
    -e "s|%OCTAVE_OCTLIBDIR%|\"${octlibdir}\"|" \
    -e "s|%OCTAVE_OCTLOCALEDIR%|\"${octlocaledir}\"|" \
    -e "s|%OCTAVE_OCTTESTSDIR%|\"${octtestsdir}\"|" \
    -e "s|%OCTAVE_STARTUPFILEDIR%|\"${startupfiledir}\"|" \
    -e "s|%OCTAVE_PREFIX%|\"${prefix}\"|" \
    -e "s|%OCTAVE_API_VERSION%|\"${api_version}\"|" \
    -e "s|%OCTAVE_RELEASE%|\"${OCTAVE_RELEASE}\"|" \
    -e "s|%OCTAVE_SHLEXT%|\"${SHLEXT}\"|" \
    -e "s|%OCTAVE_TEXI_MACROS_FILE%|\"${texi_macros_file}\"|" \
    -e "s|%OCTAVE_VERSION%|\"${version}\"|" && \
  $(simple_move_if_change_rule)
endef

define do_subst_script_vals
  $(SED) < $< \
    -e "s|%AWK%|${AWK}|g" \
    -e "s|%FIND%|${FIND}|g" \
    -e "s|%SED%|${SED}|g" \
    -e "s|%ADDRESS_SANITIZER_OPTIONS%|${ADDRESS_SANITIZER_OPTIONS}|g" \
    -e "s|%abs_top_srcdir%|${abs_top_srcdir}|" \
    -e "s|%builddir%|$(shell pwd)|" > $@-t && \
  $(simple_move_if_change_rule)
endef

define do_subst_qt_settings
  $(SED) < $< \
    -e "s|%DEFAULT_TERMINAL_FONT%|${DEFAULT_TERMINAL_FONT}|" \
    -e "s|%DEFAULT_TERMINAL_FONT_SIZE%|${DEFAULT_TERMINAL_FONT_SIZE}|" > $@-t && \
  $(simple_move_if_change_rule)
endef

define subst-bison-api-decls
  case "$(BISON_API_PREFIX_DECL_STYLE)" in \
    *api*) \
      case "$(BISON_API_PREFIX_DECL_STYLE)" in \
       *brace*) \
         api_prefix_decl='%define api.prefix {$(1)}'; ;; \
       *) \
         api_prefix_decl='%define api.prefix "$(1)"'; ;; \
       esac; \
      ;; \
    *name*) \
      case "$(BISON_API_PREFIX_DECL_STYLE)" in \
        *brace*) \
          api_prefix_decl='%name-prefix {$(1)}'; ;; \
        *) \
          api_prefix_decl='%name-prefix="$(1)"'; ;; \
      esac; \
    ;; \
  esac; \
  case "$(BISON_PUSH_PULL_DECL_STYLE)" in \
    *quote*) quote='"' ;; \
    *) quote="" ;; \
  esac; \
  case "$(BISON_PUSH_PULL_DECL_STYLE)" in \
    *dash*) push_pull_decl="%define api.push-pull $${quote}both$${quote}"; ;; \
    *underscore*) push_pull_decl="%define api.push_pull $${quote}both$${quote}"; ;; \
  esac; \
  $(SED) -e "s/%PUSH_PULL_DECL%/$$push_pull_decl/" \
         -e "s/%API_PREFIX_DECL%/$$api_prefix_decl/" $< > $@-t && \
  mv $@-t $@
endef

define gdbinit_install_rule
  if [ -f $@ ]; then \
    echo "refusing to overwrite $@ with newer version from $<" 1>&2; \
  else \
    echo "Installing $@ from version at $<" ; \
    cp $< $@; \
  fi
endef

define test-file-commands
  rm -f $@-t $@ && \
  ( echo "## DO NOT EDIT!  Generated automatically from $(<F) by Make."; \
    $(GREP) '^%!' $< \
  ) > $@-t && \
  mv $@-t $@
endef

%.cc-tst : %.cc
	$(AM_V_GEN)$(test-file-commands)

%.yy-tst : %.yy
	$(AM_V_GEN)$(test-file-commands)

%.ll-tst : %.ll
	$(AM_V_GEN)$(test-file-commands)
