// %NO_EDIT_WARNING%
/*

Copyright (C) 1993-2017 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_defaults_h)
#define octave_defaults_h 1

#include "octave-config.h"

#include <string>

#include "pathsearch.h"

#if ! defined (OCTAVE_CANONICAL_HOST_TYPE)
#  define OCTAVE_CANONICAL_HOST_TYPE %OCTAVE_CANONICAL_HOST_TYPE%
#endif

#if ! defined (OCTAVE_DEFAULT_PAGER)
#  define OCTAVE_DEFAULT_PAGER %OCTAVE_DEFAULT_PAGER%
#endif

#if ! defined (OCTAVE_ARCHLIBDIR)
#  define OCTAVE_ARCHLIBDIR %OCTAVE_ARCHLIBDIR%
#endif

#if ! defined (OCTAVE_BINDIR)
#  define OCTAVE_BINDIR %OCTAVE_BINDIR%
#endif

#if ! defined (OCTAVE_DATADIR)
#  define OCTAVE_DATADIR %OCTAVE_DATADIR%
#endif

#if ! defined (OCTAVE_DATAROOTDIR)
#  define OCTAVE_DATAROOTDIR %OCTAVE_DATAROOTDIR%
#endif

#if ! defined (OCTAVE_DOC_CACHE_FILE)
#  define OCTAVE_DOC_CACHE_FILE %OCTAVE_DOC_CACHE_FILE%
#endif

#if ! defined (OCTAVE_TEXI_MACROS_FILE)
#  define OCTAVE_TEXI_MACROS_FILE %OCTAVE_TEXI_MACROS_FILE%
#endif

#if ! defined (OCTAVE_EXEC_PREFIX)
#  define OCTAVE_EXEC_PREFIX %OCTAVE_EXEC_PREFIX%
#endif

#if ! defined (OCTAVE_FCNFILEDIR)
#  define OCTAVE_FCNFILEDIR %OCTAVE_FCNFILEDIR%
#endif

#if ! defined (OCTAVE_IMAGEDIR)
#  define OCTAVE_IMAGEDIR %OCTAVE_IMAGEDIR%
#endif

#if ! defined (OCTAVE_INCLUDEDIR)
#  define OCTAVE_INCLUDEDIR %OCTAVE_INCLUDEDIR%
#endif

#if ! defined (OCTAVE_INFODIR)
#  define OCTAVE_INFODIR %OCTAVE_INFODIR%
#endif

#if ! defined (OCTAVE_INFOFILE)
#  define OCTAVE_INFOFILE %OCTAVE_INFOFILE%
#endif

#if ! defined (OCTAVE_LIBDIR)
#  define OCTAVE_LIBDIR %OCTAVE_LIBDIR%
#endif

#if ! defined (OCTAVE_LIBEXECDIR)
#  define OCTAVE_LIBEXECDIR %OCTAVE_LIBEXECDIR%
#endif

#if ! defined (OCTAVE_LIBEXECDIR)
#  define OCTAVE_LIBEXECDIR %OCTAVE_LIBEXECDIR%
#endif

#if ! defined (OCTAVE_LOCALAPIFCNFILEDIR)
#  define OCTAVE_LOCALAPIFCNFILEDIR %OCTAVE_LOCALAPIFCNFILEDIR%
#endif

#if ! defined (OCTAVE_LOCALAPIOCTFILEDIR)
#  define OCTAVE_LOCALAPIOCTFILEDIR %OCTAVE_LOCALAPIOCTFILEDIR%
#endif

#if ! defined (OCTAVE_LOCALARCHLIBDIR)
#  define OCTAVE_LOCALARCHLIBDIR %OCTAVE_LOCALARCHLIBDIR%
#endif

#if ! defined (OCTAVE_LOCALFCNFILEDIR)
#  define OCTAVE_LOCALFCNFILEDIR %OCTAVE_LOCALFCNFILEDIR%
#endif

#if ! defined (OCTAVE_LOCALOCTFILEDIR)
#  define OCTAVE_LOCALOCTFILEDIR %OCTAVE_LOCALOCTFILEDIR%
#endif

#if ! defined (OCTAVE_LOCALSTARTUPFILEDIR)
#  define OCTAVE_LOCALSTARTUPFILEDIR %OCTAVE_LOCALSTARTUPFILEDIR%
#endif

#if ! defined (OCTAVE_LOCALAPIARCHLIBDIR)
#  define OCTAVE_LOCALAPIARCHLIBDIR %OCTAVE_LOCALAPIARCHLIBDIR%
#endif

#if ! defined (OCTAVE_LOCALVERARCHLIBDIR)
#  define OCTAVE_LOCALVERARCHLIBDIR %OCTAVE_LOCALVERARCHLIBDIR%
#endif

#if ! defined (OCTAVE_LOCALVERFCNFILEDIR)
#  define OCTAVE_LOCALVERFCNFILEDIR %OCTAVE_LOCALVERFCNFILEDIR%
#endif

#if ! defined (OCTAVE_LOCALVEROCTFILEDIR)
#  define OCTAVE_LOCALVEROCTFILEDIR %OCTAVE_LOCALVEROCTFILEDIR%
#endif

#if ! defined (OCTAVE_MAN1DIR)
#  define OCTAVE_MAN1DIR %OCTAVE_MAN1DIR%
#endif

#if ! defined (OCTAVE_MAN1EXT)
#  define OCTAVE_MAN1EXT %OCTAVE_MAN1EXT%
#endif

#if ! defined (OCTAVE_MANDIR)
#  define OCTAVE_MANDIR %OCTAVE_MANDIR%
#endif

#if ! defined (OCTAVE_OCTDATADIR)
#  define OCTAVE_OCTDATADIR %OCTAVE_OCTDATADIR%
#endif

#if ! defined (OCTAVE_OCTFILEDIR)
#  define OCTAVE_OCTFILEDIR %OCTAVE_OCTFILEDIR%
#endif

#if ! defined (OCTAVE_OCTETCDIR)
#  define OCTAVE_OCTETCDIR %OCTAVE_OCTETCDIR%
#endif

#if ! defined (OCTAVE_OCTLOCALEDIR)
#  define OCTAVE_OCTLOCALEDIR %OCTAVE_OCTLOCALEDIR%
#endif

#if ! defined (OCTAVE_OCTINCLUDEDIR)
#  define OCTAVE_OCTINCLUDEDIR %OCTAVE_OCTINCLUDEDIR%
#endif

#if ! defined (OCTAVE_OCTLIBDIR)
#  define OCTAVE_OCTLIBDIR %OCTAVE_OCTLIBDIR%
#endif

#if ! defined (OCTAVE_OCTTESTSDIR)
#  define OCTAVE_OCTTESTSDIR %OCTAVE_OCTTESTSDIR%
#endif

#if ! defined (OCTAVE_PREFIX)
#  define OCTAVE_PREFIX %OCTAVE_PREFIX%
#endif

#if ! defined (OCTAVE_STARTUPFILEDIR)
#  define OCTAVE_STARTUPFILEDIR %OCTAVE_STARTUPFILEDIR%
#endif

#if ! defined (OCTAVE_RELEASE)
#  define OCTAVE_RELEASE %OCTAVE_RELEASE%
#endif

extern OCTINTERP_API std::string Voctave_home;

extern OCTINTERP_API std::string Vbin_dir;
extern OCTINTERP_API std::string Vinfo_dir;
extern OCTINTERP_API std::string Vdata_dir;
extern OCTINTERP_API std::string Vlibexec_dir;
extern OCTINTERP_API std::string Varch_lib_dir;
extern OCTINTERP_API std::string Vlocal_arch_lib_dir;
extern OCTINTERP_API std::string Vlocal_ver_arch_lib_dir;

extern OCTINTERP_API std::string Vlocal_ver_oct_file_dir;
extern OCTINTERP_API std::string Vlocal_api_oct_file_dir;
extern OCTINTERP_API std::string Vlocal_oct_file_dir;

extern OCTINTERP_API std::string Vlocal_ver_fcn_file_dir;
extern OCTINTERP_API std::string Vlocal_api_fcn_file_dir;
extern OCTINTERP_API std::string Vlocal_fcn_file_dir;

extern OCTINTERP_API std::string Voct_data_dir;
extern OCTINTERP_API std::string Voct_etc_dir;
extern OCTINTERP_API std::string Voct_locale_dir;

extern OCTINTERP_API std::string Voct_file_dir;
extern OCTINTERP_API std::string Vfcn_file_dir;

extern OCTINTERP_API std::string Vimage_dir;

// Name of the editor to be invoked by the edit_history command.
extern OCTINTERP_API std::string VEDITOR;

extern OCTINTERP_API std::string Vlocal_site_defaults_file;
extern OCTINTERP_API std::string Vsite_defaults_file;

extern OCTINTERP_API std::string Vbuilt_in_docstrings_file;

// Name of the FFTW wisdom program.
extern OCTINTERP_API std::string Vfftw_wisdom_program;

extern OCTINTERP_API std::string subst_octave_home (const std::string&);

extern OCTINTERP_API void install_defaults (void);

extern OCTINTERP_API void
set_exec_path (const std::string& path = "");

extern OCTINTERP_API void
set_image_path (const std::string& path = "");

#endif
