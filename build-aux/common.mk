CROSS_TOOL_PREFIX = @CROSS_TOOL_PREFIX@

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
MKDIR_P = @MKDIR_P@

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
F77_FLOAT_STORE_FLAG = @F77_FLOAT_STORE_FLAG@
F77_INTEGER_8_FLAG = @F77_INTEGER_8_FLAG@
ALL_FFLAGS = $(FFLAGS)

F77_TOLOWER=@F77_TOLOWER@
F77_APPEND_UNDERSCORE=@F77_TOLOWER@
F77_APPEND_EXTRA_UNDERSCORE=@F77_TOLOWER@

F77_ISNAN_MACRO=@F77_ISNAN_MACRO@

# C compiler flags.

CC = @CC@
## FIXME: CC_VERSION is deprecated and should be removed in version 3.12
CC_VERSION = @CC_VERSION@
GCC_VERSION = @GCC_VERSION@
CPICFLAG = @CPICFLAG@
XTRA_CFLAGS = @XTRA_CFLAGS@
WARN_CFLAGS = @WARN_CFLAGS@
AM_CFLAGS = $(XTRA_CFLAGS)
ALL_CPPFLAGS = $(CPPFLAGS) $(HDF5_CPPFLAGS) $(Z_CPPFLAGS) $(LLVM_CPPFLAGS)

BUILD_CC = @BUILD_CC@
BUILD_CFLAGS = @BUILD_CFLAGS@

DEPEND_FLAGS = @DEPEND_FLAGS@
DEPEND_EXTRA_SED_PATTERN = @DEPEND_EXTRA_SED_PATTERN@
INCLUDE_DEPS = @INCLUDE_DEPS@
# ifeq ($(INCLUDE_DEPS),false)
#   omit_deps = true;
# endif

DEFS = @DEFS@
UGLY_DEFS = @UGLY_DEFS@

# C++ compiler flags.

CXX = @CXX@
## FIXME: CXX_VERSION is deprecated and should be removed in version 3.12
CXX_VERSION = @CXX_VERSION@
GXX_VERSION = @GXX_VERSION@
CXXCPP = @CXXCPP@
CXXPICFLAG = @CXXPICFLAG@
XTRA_CXXFLAGS = @XTRA_CXXFLAGS@
WARN_CXXFLAGS = @WARN_CXXFLAGS@
AM_CXXFLAGS = $(XTRA_CXXFLAGS)

BUILD_CXX = @BUILD_CXX@
BUILD_CXXFLAGS = @BUILD_CXXFLAGS@

# Linker and library flags

LD_CXX = @LD_CXX@
LD_STATIC_FLAG = @LD_STATIC_FLAG@
LIBFLAGS = -L$(top_builddir)
#ALL_LDFLAGS = $(LIBFLAGS) $(LD_STATIC_FLAG) $(CPICFLAG) $(LDFLAGS)

BUILD_LDFLAGS = @BUILD_LDFLAGS@

SH_LD = @SH_LD@
SH_LDFLAGS = @SH_LDFLAGS@

DL_LD = @DL_LD@
DL_LDFLAGS = @DL_LDFLAGS@

SONAME_FLAGS = @SONAME_FLAGS@

RDYNAMIC_FLAG = @RDYNAMIC_FLAG@

NO_UNDEFINED_LDFLAG = @NO_UNDEFINED_LDFLAG@

MKOCTFILE_AR = @MKOCTFILE_AR@
MKOCTFILE_CC = @MKOCTFILE_CC@
MKOCTFILE_CXX = @MKOCTFILE_CXX@
MKOCTFILE_DL_LD = @MKOCTFILE_DL_LD@
MKOCTFILE_DL_LDFLAGS = @MKOCTFILE_DL_LDFLAGS@
MKOCTFILE_F77 = @MKOCTFILE_F77@
MKOCTFILE_LD_CXX = @MKOCTFILE_LD_CXX@
MKOCTFILE_RANLIB = @MKOCTFILE_RANLIB@

# List of libraries and their special compilation flags

LIBOCTINTERP = @LIBOCTINTERP@
LIBOCTAVE = @LIBOCTAVE@

DL_LIBS = @DL_LIBS@
FLIBS = @FLIBS@
LIBS = @LIBS@

AMD_CPPFLAGS = @AMD_CPPFLAGS@
AMD_LDFLAGS = @AMD_LDFLAGS@
AMD_LIBS = @AMD_LIBS@

ARPACK_CPPFLAGS = @ARPACK_CPPFLAGS@
ARPACK_LDFLAGS = @ARPACK_LDFLAGS@
ARPACK_LIBS = @ARPACK_LIBS@

BLAS_LIBS = @BLAS_LIBS@

CAMD_CPPFLAGS = @CAMD_CPPFLAGS@
CAMD_LDFLAGS = @CAMD_LDFLAGS@
CAMD_LIBS = @CAMD_LIBS@

CARBON_LIBS = @CARBON_LIBS@

COLAMD_CPPFLAGS = @COLAMD_CPPFLAGS@
COLAMD_LDFLAGS = @COLAMD_LDFLAGS@
COLAMD_LIBS = @COLAMD_LIBS@

CCOLAMD_CPPFLAGS = @CCOLAMD_CPPFLAGS@
CCOLAMD_LDFLAGS = @CCOLAMD_LDFLAGS@
CCOLAMD_LIBS = @CCOLAMD_LIBS@

CHOLMOD_CPPFLAGS = @CHOLMOD_CPPFLAGS@
CHOLMOD_LDFLAGS = @CHOLMOD_LDFLAGS@
CHOLMOD_LIBS = @CHOLMOD_LIBS@

CURL_CPPFLAGS = @CURL_CPPFLAGS@
CURL_LDFLAGS = @CURL_LDFLAGS@
CURL_LIBS = @CURL_LIBS@

CXSPARSE_CPPFLAGS = @CXSPARSE_CPPFLAGS@
CXSPARSE_LDFLAGS = @CXSPARSE_LDFLAGS@
CXSPARSE_LIBS = @CXSPARSE_LIBS@

FFTW3_CPPFLAGS = @FFTW3_CPPFLAGS@
FFTW3_LDFLAGS = @FFTW3_LDFLAGS@
FFTW3_LIBS = @FFTW3_LIBS@

FFTW3F_CPPFLAGS = @FFTW3F_CPPFLAGS@
FFTW3F_LDFLAGS = @FFTW3F_LDFLAGS@
FFTW3F_LIBS = @FFTW3F_LIBS@

FFTW_XCPPFLAGS = $(FFTW3_CPPFLAGS) $(FFTW3F_CPPFLAGS)
FFTW_XLDFLAGS = $(FFTW3_LDFLAGS) $(FFTW3F_LDFLAGS)
FFTW_XLIBS = $(FFTW3_THREADS_LIBS) $(FFTW3F_THREADS_LIBS) $(FFTW3_LIBS) $(FFTW3F_LIBS)

FT2_CFLAGS = @FT2_CFLAGS@
FT2_LIBS = @FT2_LIBS@

GLPK_CPPFLAGS = @GLPK_CPPFLAGS@
GLPK_LDFLAGS = @GLPK_LDFLAGS@
GLPK_LIBS = @GLPK_LIBS@

GRAPHICS_CFLAGS = @GRAPHICS_CFLAGS@
GRAPHICS_LIBS = @GRAPHICS_LIBS@

HDF5_CPPFLAGS = @HDF5_CPPFLAGS@
HDF5_LDFLAGS = @HDF5_LDFLAGS@
HDF5_LIBS = @HDF5_LIBS@

JAVA_CPPFLAGS = @JAVA_CPPFLAGS@
JAVA_LIBS = @JAVA_LIBS@

LAPACK_LIBS = @LAPACK_LIBS@

LLVM_CPPFLAGS = @LLVM_CPPFLAGS@
LLVM_LDFLAGS = @LLVM_LDFLAGS@
LLVM_LIBS = @LLVM_LIBS@

MAGICK_CPPFLAGS = @MAGICK_CPPFLAGS@
MAGICK_LDFLAGS = @MAGICK_LDFLAGS@
MAGICK_LIBS = @MAGICK_LIBS@

OPENGL_LIBS = @OPENGL_LIBS@

PTHREAD_CFLAGS = @PTHREAD_CFLAGS@
PTHREAD_LIBS = @PTHREAD_LIBS@

QHULL_CPPFLAGS = @QHULL_CPPFLAGS@
QHULL_LDFLAGS = @QHULL_LDFLAGS@
QHULL_LIBS = @QHULL_LIBS@

QRUPDATE_CPPFLAGS = @QRUPDATE_CPPFLAGS@
QRUPDATE_LDFLAGS = @QRUPDATE_LDFLAGS@
QRUPDATE_LIBS = @QRUPDATE_LIBS@

REGEX_LIBS = @REGEX_LIBS@

READLINE_LIBS = @READLINE_LIBS@

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

TERM_LIBS = @TERM_LIBS@

UMFPACK_CPPFLAGS = @UMFPACK_CPPFLAGS@
UMFPACK_LDFLAGS = @UMFPACK_LDFLAGS@
UMFPACK_LIBS = @UMFPACK_LIBS@

X11_INCFLAGS = @X11_INCFLAGS@
X11_LIBS = @X11_LIBS@

Z_CPPFLAGS = @Z_CPPFLAGS@
Z_LDFLAGS = @Z_LDFLAGS@
Z_LIBS = @Z_LIBS@

# Miscellaneous

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

# Where to install and expect libraries like liboctave.a, liboctinterp.a,
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

# ==================== Octave-specific Makefile rules ====================

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
$(PERL) -p $(top_srcdir)/build-aux/config_vals.pl $< > $@-t
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
