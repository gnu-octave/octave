AWK = @AWK@
export AWK

SED = @SED@
export SED

FIND = @FIND@
export FIND

PERL = @PERL@
export PERL

PYTHON = @PYTHON@

GNUPLOT = @GNUPLOT@

DESKTOP_FILE_INSTALL = @DESKTOP_FILE_INSTALL@

OCTAVE_VERSION = @OCTAVE_VERSION@
OCTAVE_API_VERSION_NUMBER = @OCTAVE_API_VERSION_NUMBER@
OCTAVE_API_VERSION = @OCTAVE_API_VERSION@
OCTAVE_RELEASE_DATE = @OCTAVE_RELEASE_DATE@
OCTAVE_COPYRIGHT = @OCTAVE_COPYRIGHT@

version = $(OCTAVE_VERSION)
api_version = $(OCTAVE_API_VERSION)

## AM_LIBTOOLFLAGS = --silent

#### Start of system configuration section. ####

EXEEXT = @EXEEXT@

BUILD_EXEEXT = @BUILD_EXEEXT@

LEX = @LEX@
AM_LFLAGS = @LFLAGS@
LEXLIB = @LEXLIB@

YACC = @YACC@
AM_YFLAGS = -dv

GPERF = @GPERF@

AR = @AR@
ARFLAGS = @ARFLAGS@ 

TEMPLATE_AR = @TEMPLATE_AR@
TEMPLATE_ARFLAGS = @TEMPLATE_ARFLAGS@

RANLIB = @RANLIB@

LN_S = @LN_S@

MAKEINFO = @MAKEINFO@
TEXI2DVI = @TEXI2DVI@
TEXI2PDF = @TEXI2PDF@

GHOSTSCRIPT = @GHOSTSCRIPT@

DEFAULT_PAGER = @DEFAULT_PAGER@

ENABLE_DYNAMIC_LINKING = @ENABLE_DYNAMIC_LINKING@

SHLEXT = @SHLEXT@
SHLEXT_VER = @SHLEXT_VER@
SHLLIB = @SHLLIB@
SHLLIB_VER = @SHLLIB_VER@
SHLBIN = @SHLBIN@
SHLBIN_VER = @SHLBIN_VER@
SHLLINKEXT=

LIBEXT = a
LIBPRE = @LIBPRE@
SHLPRE = @SHLPRE@
SHLLIBPRE = @SHLLIBPRE@
SHLBINPRE = @SHLBINPRE@

# Fortran compiler flags.

FC = @FC@
F77 = @F77@
AM_FFLAGS = @FFLAGS@
FPICFLAG = @FPICFLAG@
ALL_FFLAGS = $(FFLAGS)
F77_FLOAT_STORE_FLAG = @F77_FLOAT_STORE_FLAG@
F77_INTEGER_8_FLAG = @F77_INTEGER_8_FLAG@

F77_TOLOWER=@F77_TOLOWER@
F77_APPEND_UNDERSCORE=@F77_TOLOWER@
F77_APPEND_EXTRA_UNDERSCORE=@F77_TOLOWER@

F77_ISNAN_MACRO=@F77_ISNAN_MACRO@

X11_INCFLAGS = @X11_INCFLAGS@
X11_LIBS = @X11_LIBS@

CARBON_LIBS = @CARBON_LIBS@

MAGICK_CPPFLAGS = @MAGICK_CPPFLAGS@
MAGICK_LDFLAGS = @MAGICK_LDFLAGS@
MAGICK_LIBS = @MAGICK_LIBS@

PTHREAD_CFLAGS = @PTHREAD_CFLAGS@
PTHREAD_LIBS = @PTHREAD_LIBS@

LIBFLAGS = -L$(top_builddir)

DEFS = @DEFS@

UGLY_DEFS = @UGLY_DEFS@

CC = @CC@
## FIXME: CC_VERSION is deprecated and should be removed in version 3.12
CC_VERSION = @CC_VERSION@
GCC_VERSION = @GCC_VERSION@
CONFIGURE_CFLAGS = @CFLAGS@
CPICFLAG = @CPICFLAG@
XTRA_CFLAGS = @XTRA_CFLAGS@
WARN_CFLAGS = @WARN_CFLAGS@
AM_CFLAGS = $(CONFIGURE_CFLAGS) \
  $(XTRA_CFLAGS) $(WARN_CFLAGS)
BUG_CFLAGS = $(XTRA_CFLAGS) $(WARN_CFLAGS) $(CFLAGS)

BUILD_CC = @BUILD_CC@
BUILD_CFLAGS = @BUILD_CFLAGS@

DEPEND_FLAGS = @DEPEND_FLAGS@
DEPEND_EXTRA_SED_PATTERN = @DEPEND_EXTRA_SED_PATTERN@
INCLUDE_DEPS = @INCLUDE_DEPS@
# ifeq ($(INCLUDE_DEPS),false)
#   omit_deps = true;
# endif

GRAPHICS_CFLAGS = @GRAPHICS_CFLAGS@

CXX = @CXX@
## FIXME: CXX_VERSION is deprecated and should be removed in version 3.12
CXX_VERSION = @CXX_VERSION@
GXX_VERSION = @GXX_VERSION@
CXXCPP = @CXXCPP@
CONFIGURE_CXXFLAGS = @CXXFLAGS@
CXXPICFLAG = @CXXPICFLAG@
XTRA_CXXFLAGS = @XTRA_CXXFLAGS@
WARN_CXXFLAGS = @WARN_CXXFLAGS@
AM_CXXFLAGS = $(CONFIGURE_CXXFLAGS) \
  $(XTRA_CXXFLAGS) $(WARN_CXXFLAGS)
BUG_CXXFLAGS = $(XTRA_CXXFLAGS) $(WARN_CXXFLAGS) $(CXXFLAGS)

BUILD_CXX = @BUILD_CXX@
BUILD_CXXFLAGS = @BUILD_CXXFLAGS@

NO_UNDEFINED_LDFLAG = @NO_UNDEFINED_LDFLAG@

LD_CXX = @LD_CXX@
LD_STATIC_FLAG = @LD_STATIC_FLAG@
#ALL_LDFLAGS = $(LIBFLAGS) $(LD_STATIC_FLAG) $(CPICFLAG) $(LDFLAGS)

BUILD_LDFLAGS = @BUILD_LDFLAGS@

SH_LD = @SH_LD@
SH_LDFLAGS = @SH_LDFLAGS@

DL_LD = @DL_LD@
DL_LDFLAGS = @DL_LDFLAGS@

SONAME_FLAGS = @SONAME_FLAGS@

RDYNAMIC_FLAG = @RDYNAMIC_FLAG@

FLIBS = @FLIBS@

LIBOCTINTERP = @LIBOCTINTERP@
LIBOCTAVE = @LIBOCTAVE@
LIBCRUFT = @LIBCRUFT@

FT2_CFLAGS = @FT2_CFLAGS@
FT2_LIBS = @FT2_LIBS@

HDF5_CPPFLAGS = @HDF5_CPPFLAGS@
HDF5_LDFLAGS = @HDF5_LDFLAGS@
HDF5_LIBS = @HDF5_LIBS@

Z_CPPFLAGS = @Z_CPPFLAGS@
Z_LDFLAGS = @Z_LDFLAGS@
Z_LIBS = @Z_LIBS@

LLVM_CPPFLAGS = @LLVM_CPPFLAGS@
LLVM_LDFLAGS = @LLVM_LDFLAGS@
LLVM_LIBS = @LLVM_LIBS@

GRAPHICS_LIBS = @GRAPHICS_LIBS@

QHULL_CPPFLAGS = @QHULL_CPPFLAGS@
QHULL_LDFLAGS = @QHULL_LDFLAGS@
QHULL_LIBS = @QHULL_LIBS@

REGEX_LIBS = @REGEX_LIBS@

LAPACK_LIBS = @LAPACK_LIBS@
BLAS_LIBS = @BLAS_LIBS@

FFTW3_CPPFLAGS = @FFTW3_CPPFLAGS@
FFTW3_LDFLAGS = @FFTW3_LDFLAGS@
FFTW3_LIBS = @FFTW3_LIBS@

FFTW3F_CPPFLAGS = @FFTW3F_CPPFLAGS@
FFTW3F_LDFLAGS = @FFTW3F_LDFLAGS@
FFTW3F_LIBS = @FFTW3F_LIBS@

GLPK_CPPFLAGS = @GLPK_CPPFLAGS@
GLPK_LDFLAGS = @GLPK_LDFLAGS@
GLPK_LIBS = @GLPK_LIBS@

CURL_CPPFLAGS = @CURL_CPPFLAGS@
CURL_LDFLAGS = @CURL_LDFLAGS@
CURL_LIBS = @CURL_LIBS@

AMD_CPPFLAGS = @AMD_CPPFLAGS@
AMD_LDFLAGS = @AMD_LDFLAGS@
AMD_LIBS = @AMD_LIBS@

CAMD_CPPFLAGS = @CAMD_CPPFLAGS@
CAMD_LDFLAGS = @CAMD_LDFLAGS@
CAMD_LIBS = @CAMD_LIBS@

COLAMD_CPPFLAGS = @COLAMD_CPPFLAGS@
COLAMD_LDFLAGS = @COLAMD_LDFLAGS@
COLAMD_LIBS = @COLAMD_LIBS@

CCOLAMD_CPPFLAGS = @CCOLAMD_CPPFLAGS@
CCOLAMD_LDFLAGS = @CCOLAMD_LDFLAGS@
CCOLAMD_LIBS = @CCOLAMD_LIBS@

CHOLMOD_CPPFLAGS = @CHOLMOD_CPPFLAGS@
CHOLMOD_LDFLAGS = @CHOLMOD_LDFLAGS@
CHOLMOD_LIBS = @CHOLMOD_LIBS@

CXSPARSE_CPPFLAGS = @CXSPARSE_CPPFLAGS@
CXSPARSE_LDFLAGS = @CXSPARSE_LDFLAGS@
CXSPARSE_LIBS = @CXSPARSE_LIBS@

UMFPACK_CPPFLAGS = @UMFPACK_CPPFLAGS@
UMFPACK_LDFLAGS = @UMFPACK_LDFLAGS@
UMFPACK_LIBS = @UMFPACK_LIBS@

OPENGL_LIBS = @OPENGL_LIBS@

QRUPDATE_CPPFLAGS = @QRUPDATE_CPPFLAGS@
QRUPDATE_LDFLAGS = @QRUPDATE_LDFLAGS@
QRUPDATE_LIBS = @QRUPDATE_LIBS@

READLINE_LIBS = @READLINE_LIBS@
TERM_LIBS = @TERM_LIBS@

ARPACK_CPPFLAGS = @ARPACK_CPPFLAGS@
ARPACK_LDFLAGS = @ARPACK_LDFLAGS@
ARPACK_LIBS = @ARPACK_LIBS@

DL_LIBS = @DL_LIBS@
LIBS = @LIBS@

ALL_CPPFLAGS = $(CPPFLAGS) $(HDF5_CPPFLAGS) $(Z_CPPFLAGS) $(LLVM_CPPFLAGS)

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

FFTW_XCPPFLAGS = $(FFTW3_CPPFLAGS) $(FFTW3F_CPPFLAGS)

FFTW_XLDFLAGS = $(FFTW3_LDFLAGS) $(FFTW3F_LDFLAGS)

FFTW_XLIBS = $(FFTW3_LIBS) $(FFTW3F_LIBS)

USE_64_BIT_IDX_T = @USE_64_BIT_IDX_T@
OCTAVE_IDX_TYPE = @OCTAVE_IDX_TYPE@

TEXINFO_COLAMD = @TEXINFO_COLAMD@
TEXINFO_CHOLMOD = @TEXINFO_CHOLMOD@
TEXINFO_UMFPACK = @TEXINFO_UMFPACK@
TEXINFO_QHULL = @TEXINFO_QHULL@

# The arguments passed to configure.
config_opts = @config_opts@

CONFIG_SUBDIRS = @subdirs@

# ==================== Where To Install Things ====================

# The default location for installation.  Everything is placed in
# subdirectories of this directory.  The default values for many of
# the variables below are expressed in terms of this one, so you may
# not need to change them.  This defaults to /usr/local.
prefix = @prefix@

# Like `prefix', but used for architecture-specific files.
exec_prefix = @exec_prefix@

# Where to install Octave and other binaries that people will want to
# run directly.
bindir = @bindir@

# Normally the directory for installing executables that system
# administrators run.  This is the same as libexecdir on Cygwin systems.
sbindir = @sbindir@

# The root of the directory tree for read-only
# architecture-independent data files.
datarootdir = @datarootdir@

# Where to install architecture-independent data files.  ${fcnfiledir}
# and ${localfcnfiledir} are subdirectories of this.
datadir = @datadir@

libdir = @libdir@

# Where to install and expect extra files like NEWS and doc-cache.
octetcdir = @octetcdir@

# Where to install and expect libraries like libcruft.a, liboctave.a,
# and other architecture-dependent data.
octlibdir = @octlibdir@

# Where to install and expect executable programs to be run by Octave
# rather than directly by users.
libexecdir = @libexecdir@

# The prefix for Octave's include file directory.  The default is
# ${prefix}/include
includedir = @includedir@

# Where to install Octave's man pages, and what extension they should
# have.  The default is ${prefix}/man/man1
mandir = @mandir@
man1dir = @man1dir@
man1ext = @man1ext@

# The full path to the default doc cache file.
doc_cache_file = @doc_cache_file@

# The full path to the default texi macros file.
texi_macros_file_file = @texi_macros_file@

# Where to install and expect the info files describing Octave..
infodir = @infodir@

# The full path to the default info file.
infofile = @infofile@

# ==================== Octave-specific directories ====================

# These variables hold the values specific to Octave.  They are
# based on the values of the standard Make variables above.

# What is the path separation character
sepchar = @sepchar@

# Where to install Octave's include files.  The default is
# ${includedir}/octave-${version}/octave
octincludedir = @octincludedir@

# Where to install the function file distributed with
# Octave.  This includes the Octave version, so that the
# function files for different versions of Octave will install
# themselves in separate directories.
fcnfiledir = @fcnfiledir@

# Directories Octave should search for function files specific
# to this site (i.e. customizations), before consulting
# ${fcnfiledir}.  This should be a colon-separated list of
# directories.
localfcnfiledir = @localfcnfiledir@
localapifcnfiledir = @localapifcnfiledir@
localverfcnfiledir = @localverfcnfiledir@

# Where to put executables to be run by Octave rather than
# the user.  This path usually includes the Octave version
# and configuration name, so that multiple configurations
# for multiple versions of Octave may be installed at once.
archlibdir = @archlibdir@

# Where to put executables to be run by Octave rather than by the
# user that are specific to this site.
localarchlibdir = @localarchlibdir@
localapiarchlibdir = @localapiarchlibdir@
localverarchlibdir = @localverarchlibdir@

# Where to put object files that will by dynamically loaded.
# This path usually includes the Octave version and configuration
# name, so that multiple configurations for multiple versions of
# Octave may be installed at once. 
octfiledir = @octfiledir@

# Directories Octave should search for object files that will be
# dynamically loaded and that are specific to this site
# (i.e. customizations), before consulting ${octfiledir}.  This should
# be a colon-separated list of directories.
localoctfiledir = @localoctfiledir@
localapioctfiledir = @localapioctfiledir@
localveroctfiledir = @localveroctfiledir@

# Where Octave will search to find image files.
imagedir = @imagedir@

# The type of computer we are running on.
canonical_host_type = @canonical_host_type@

# Where Octave will look for startup files
startupfiledir = ${fcnfiledir}/startup
localstartupfiledir = ${localfcnfiledir}/startup

# LD_LIBRARY_PATH, DYLD_LIBRARY_PATH, PATH, ...
library_path_var = @library_path_var@

# The separator used for elements of the LD_PRELOAD variable (might be
# a space, so protect with $(null))
null =
ldpreloadsep = $(null)@ldpreloadsep@$(null)

NO_OCT_FILE_STRIP = @NO_OCT_FILE_STRIP@

# The following pattern rules and the substitution functions require
# GNU make.  If you don't have it, get it!

define simple_move_if_change_rule
if [ -s $@-t ]; then \
  $(top_srcdir)/build-aux/move-if-change $@-t $@; \
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

# Yes, the second sed command near the end is needed, to avoid limits
# in command lengths for some versions of sed.  UGLY_DEFS is often
# quite large, so it makes sense to split this command there.

define do_subst_config_vals
echo "making $@ from $<"
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
  -e "s|%OCTAVE_CONF_CC_VERSION%|\"${CC_VERSION}\"|" \
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
  -e "s|%OCTAVE_CONF_CXX_VERSION%|\"${CXX_VERSION}\"|" \
  -e "s|%OCTAVE_CONF_DEFAULT_PAGER%|\"${DEFAULT_PAGER}\"|" \
  -e "s|%OCTAVE_CONF_DEPEND_FLAGS%|\"${DEPEND_FLAGS}\"|" \
  -e "s|%OCTAVE_CONF_DEPEND_EXTRA_SED_PATTERN%|\"${DEPEND_EXTRA_SED_PATTERN}\"|" \
  -e "s|%OCTAVE_CONF_DL_LD%|\"${DL_LD}\"|" \
  -e "s|%OCTAVE_CONF_DL_LDFLAGS%|\"${DL_LDFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_DL_LIBS%|\"${DL_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_GCC_VERSION%|\"${GCC_VERSION}\"|" \
  -e "s|%OCTAVE_CONF_GXX_VERSION%|\"${GXX_VERSION}\"|" \
  -e "s|%OCTAVE_CONF_EXEEXT%|\"${EXEEXT}\"|" \
  -e "s|%OCTAVE_CONF_F77%|\"${F77}\"|" \
  -e "s|%OCTAVE_CONF_F77_FLOAT_STORE_FLAG%|\"${F77_FLOAT_STORE_FLAG}\"|" \
  -e "s|%OCTAVE_CONF_F77_INTEGER_8_FLAG%|\"${F77_INTEGER_8_FLAG}\"|" \
  -e "s|%OCTAVE_CONF_FC%|\"${FC}\"|" \
  -e "s|%OCTAVE_CONF_FFLAGS%|\"${FFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_FFTW3_CPPFLAGS%|\"${FFTW3_CPPFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_FFTW3_LDFLAGS%|\"${FFTW3_LDFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_FFTW3_LIBS%|\"${FFTW3_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_FFTW3F_CPPFLAGS%|\"${FFTW3F_CPPFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_FFTW3F_LDFLAGS%|\"${FFTW3F_LDFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_FFTW3F_LIBS%|\"${FFTW3F_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_FLIBS%|\"${FLIBS}\"|" \
  -e "s|%OCTAVE_CONF_FPICFLAG%|\"${FPICFLAG}\"|" \
  -e "s|%OCTAVE_CONF_FT2_LIBS%|\"${FT2_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_GLPK_CPPFLAGS%|\"${GLPK_CPPFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_GLPK_LDFLAGS%|\"${GLPK_LDFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_GLPK_LIBS%|\"${GLPK_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_GNUPLOT%|\"${GNUPLOT}\"|" \
  -e "s|%OCTAVE_CONF_GRAPHICS_LIBS%|\"${GRAPHICS_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_HDF5_CPPFLAGS%|\"${HDF5_CPPFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_HDF5_LDFLAGS%|\"${HDF5_LDFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_HDF5_LIBS%|\"${HDF5_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_INCLUDEDIR%|\"${includedir}\"|" \
  -e "s|%OCTAVE_CONF_LAPACK_LIBS%|\"${LAPACK_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_LD_CXX%|\"${LD_CXX}\"|" \
  -e "s|%OCTAVE_CONF_LDFLAGS%|\"${LDFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_LD_STATIC_FLAG%|\"${LD_STATIC_FLAG}\"|" \
  -e "s|%OCTAVE_CONF_LEX%|\"${LEX}\"|" \
  -e "s|%OCTAVE_CONF_LEXLIB%|\"${LEXLIB}\"|" \
  -e "s|%OCTAVE_CONF_LFLAGS%|\"${LFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_LIBCRUFT%|\"${LIBCRUFT}\"|" \
  -e "s|%OCTAVE_CONF_LIBDIR%|\"${libdir}\"|" \
  -e "s|%OCTAVE_CONF_LIBEXT%|\"${LIBEXT}\"|" \
  -e "s|%OCTAVE_CONF_LIBFLAGS%|\"${LIBFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_LIBOCTAVE%|\"${LIBOCTAVE}\"|" \
  -e "s|%OCTAVE_CONF_LIBOCTINTERP%|\"${LIBOCTINTERP}\"|" \
  -e "s|%OCTAVE_CONF_LIBS%|\"${LIBS}\"|" \
  -e "s|%OCTAVE_CONF_LN_S%|\"${LN_S}\"|" \
  -e "s|%OCTAVE_CONF_MAGICK_CPPFLAGS%|\"${MAGICK_CPPFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_MAGICK_LDFLAGS%|\"${MAGICK_LDFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_MAGICK_LIBS%|\"${MAGICK_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_LLVM_CPPFLAGS%|\"${LLVM_CPPFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_LLVM_LDFLAGS%|\"${LLVM_LDFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_LLVM_LIBS%|\"${LLVM_LIBS}\"|" \
  -e 's|%OCTAVE_CONF_MKOCTFILE_DL_LDFLAGS%|\"@MKOCTFILE_DL_LDFLAGS@\"|' \
  -e "s|%OCTAVE_CONF_OCTAVE_LINK_DEPS%|\"${OCTAVE_LINK_DEPS}\"|" \
  -e "s|%OCTAVE_CONF_OCTAVE_LINK_OPTS%|\"${OCTAVE_LINK_OPTS}\"|" \
  -e "s|%OCTAVE_CONF_OCTINCLUDEDIR%|\"${octincludedir}\"|" \
  -e "s|%OCTAVE_CONF_OCTLIBDIR%|\"${octlibdir}\"|" \
  -e "s|%OCTAVE_CONF_OCT_LINK_DEPS%|\"${OCT_LINK_DEPS}\"|" \
  -e "s|%OCTAVE_CONF_OCT_LINK_OPTS%|\"${OCT_LINK_OPTS}\"|" \
  -e "s|%OCTAVE_CONF_OPENGL_LIBS%|\"${OPENGL_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_PREFIX%|\"${prefix}\"|" \
  -e "s|%OCTAVE_CONF_PTHREAD_CFLAGS%|\"${PTHREAD_CFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_PTHREAD_LIBS%|\"${PTHREAD_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_QHULL_CPPFLAGS%|\"${QHULL_CPPFLAGSS}\"|" \
  -e "s|%OCTAVE_CONF_QHULL_LDFLAGS%|\"${QHULL_LDFLAGSS}\"|" \
  -e "s|%OCTAVE_CONF_QHULL_LIBS%|\"${QHULL_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_QRUPDATE_CPPFLAGS%|\"${QRUPDATE_CPPFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_QRUPDATE_LDFLAGS%|\"${QRUPDATE_LDFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_QRUPDATE_LIBS%|\"${QRUPDATE_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_QT_INCDIR%|\"${QT_INCDIR}\"|" \
  -e "s|%OCTAVE_CONF_QT_LIBDIR%|\"${QT_LIBDIR}\"|" \
  -e "s|%OCTAVE_CONF_RANLIB%|\"${RANLIB}\"|" \
  -e "s|%OCTAVE_CONF_RDYNAMIC_FLAG%|\"${RDYNAMIC_FLAG}\"|" \
  -e "s|%OCTAVE_CONF_READLINE_LIBS%|\"${READLINE_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_REGEX_LIBS%|\"${REGEX_LIBS}\"|" \
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
  -e "s|%OCTAVE_CONF_UGLY_DEFS%|\"${UGLY_DEFS}\"|" \
  -e "s|%OCTAVE_CONF_UMFPACK_CPPFLAGS%|\"${UMFPACK_CPPFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_UMFPACK_LDFLAGS%|\"${UMFPACK_LDFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_UMFPACK_LIBS%|\"${UMFPACK_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_USE_64_BIT_IDX_T%|\"${USE_64_BIT_IDX_T}\"|" \
  -e "s|%OCTAVE_CONF_VERSION%|\"${version}\"|" \
  -e "s|%OCTAVE_CONF_ENABLE_DYNAMIC_LINKING%|\"${ENABLE_DYNAMIC_LINKING}\"|" \
  -e "s|%OCTAVE_CONF_X11_INCFLAGS%|\"${X11_INCFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_X11_LIBS%|\"${X11_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_XTRA_CFLAGS%|\"${XTRA_CFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_XTRA_CXXFLAGS%|\"${XTRA_CXXFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_YACC%|\"${YACC}\"|" \
  -e "s|%OCTAVE_CONF_YFLAGS%|\"${YFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_Z_CPPFLAGS%|\"${Z_CPPFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_Z_LDFLAGS%|\"${Z_LDFLAGS}\"|" \
  -e "s|%OCTAVE_CONF_Z_LIBS%|\"${Z_LIBS}\"|" \
  -e "s|%OCTAVE_CONF_config_opts%|\"${config_opts}\"|" | \
  $(SED)  -e "s|%OCTAVE_CONF_DEFS%|\"${UGLY_DEFS}\"|" > $@-t
$(simple_move_if_change_rule)
endef

define do_subst_default_vals
echo "making $@ from $<"
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
  -e "s|%OCTAVE_OCTFILEDIR%|\"${octfiledir}\"|" \
  -e "s|%OCTAVE_OCTETCDIR%|\"${octetcdir}\"|" \
  -e "s|%OCTAVE_OCTINCLUDEDIR%|\"${octincludedir}\"|" \
  -e "s|%OCTAVE_OCTLIBDIR%|\"${octlibdir}\"|" \
  -e "s|%OCTAVE_STARTUPFILEDIR%|\"${startupfiledir}\"|" \
  -e "s|%OCTAVE_PREFIX%|\"${prefix}\"|" \
  -e "s|%OCTAVE_API_VERSION%|\"${api_version}\"|" \
  -e "s|%OCTAVE_RELEASE%|\"${OCTAVE_RELEASE}\"|" \
  -e "s|%OCTAVE_TEXI_MACROS_FILE%|\"${texi_macros_file}\"|" \
  -e "s|%OCTAVE_VERSION%|\"${version}\"|"
$(simple_move_if_change_rule)
endef

define do_subst_script_vals
echo "making $@ from $<"
$(SED) < $< \
  -e "s|%AWK%|${AWK}|g" \
  -e "s|%FIND%|${FIND}|g" \
  -e "s|%SED%|${SED}|g" \
  -e "s|%library_path_var%|${library_path_var}|g" \
  -e "s|%liboctinterp%|${SHLPRE}octinterp.${SHLEXT}|g" \
  -e "s|%liboctave%|${SHLPRE}octave.${SHLEXT}|g" \
  -e "s|%libcruft%|${SHLPRE}cruft.${SHLEXT}|g" \
  -e "s|%ldpreloadsep%|${ldpreloadsep}|g" \
  -e "s|%srcdir%|${srcdir}|" \
  -e "s|%top_srcdir%|${top_srcdir}|" \
  -e "s|%abs_top_srcdir%|${abs_top_srcdir}|" \
  -e "s|%builddir%|$(shell pwd)|" > $@-t
$(simple_move_if_change_rule)
endef

define do_script_install
$(top_srcdir)/build-aux/mkinstalldirs $(DESTDIR)$(fcnfiledir)/$(script_sub_dir)
for f in $(FCN_FILES); do \
  fbase=`basename $$f`; \
  rm -f $(DESTDIR)$(fcnfiledir)/$(script_sub_dir)/$$fbase; \
  $(INSTALL_DATA) $$f $(DESTDIR)$(fcnfiledir)/$(script_sub_dir)/$$fbase; \
done
$(top_srcdir)/mkpkgadd $(DESTDIR)$(fcnfiledir)/$(script_sub_dir) > $(DESTDIR)$(fcnfiledir)/$(script_sub_dir)/PKG_ADD.t
if [ -n "`cat $(DESTDIR)$(fcnfiledir)/$(script_sub_dir)/PKG_ADD.t`" ]; then \
  $(INSTALL_DATA) $(DESTDIR)$(fcnfiledir)/$(script_sub_dir)/PKG_ADD.t $(DESTDIR)$(fcnfiledir)/$(script_sub_dir)/PKG_ADD ; \
else \
  rm -f $(DESTDIR)$(fcnfiledir)/$(script_sub_dir)/PKG_ADD.t ; \
fi
endef

define do_script_uninstall
for f in $(FCN_FILES_NO_DIR); \
  do rm -f $(DESTDIR)$(fcnfiledir)/$(script_sub_dir)/$$f; \
done
rm -f $(DESTDIR)$(fcnfiledir)/$(script_sub_dir)/PKG_ADD
-rmdir $(DESTDIR)$(fcnfiledir)/$(script_sub_dir)
endef
