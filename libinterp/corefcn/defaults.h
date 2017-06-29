// DO NOT EDIT!  Generated automatically by subst-default-vals.
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

extern OCTINTERP_API std::string Voctave_home;
extern OCTINTERP_API std::string Voctave_exec_home;

extern OCTINTERP_API std::string Vbin_dir;
extern OCTINTERP_API std::string Vinfo_dir;
extern OCTINTERP_API std::string Vdata_dir;
extern OCTINTERP_API std::string Vlibexec_dir;
extern OCTINTERP_API std::string Varch_lib_dir;

extern OCTINTERP_API std::string Vdataroot_dir;
extern OCTINTERP_API std::string Vinclude_dir;
extern OCTINTERP_API std::string Vlib_dir;
extern OCTINTERP_API std::string Vman1_dir;
extern OCTINTERP_API std::string Vman1_ext;
extern OCTINTERP_API std::string Vman_dir;

extern OCTINTERP_API std::string Vlocal_api_arch_lib_dir;
extern OCTINTERP_API std::string Vlocal_startup_file_dir;
extern OCTINTERP_API std::string Voct_include_dir;
extern OCTINTERP_API std::string Voct_lib_dir;
extern OCTINTERP_API std::string Voct_tests_dir;
extern OCTINTERP_API std::string Vstartupfile_dir;

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

extern OCTINTERP_API std::string Vdefault_pager;
extern OCTINTERP_API std::string Vcanonical_host_type;
extern OCTINTERP_API std::string Voctave_release;

extern OCTINTERP_API std::string Vlocal_site_defaults_file;
extern OCTINTERP_API std::string Vsite_defaults_file;

extern OCTINTERP_API std::string Vbuilt_in_docstrings_file;

// Name of the FFTW wisdom program.
extern OCTINTERP_API std::string Vfftw_wisdom_program;

extern OCTINTERP_API void install_defaults (void);

extern OCTINTERP_API void
set_exec_path (const std::string& path = "");

extern OCTINTERP_API void
set_image_path (const std::string& path = "");

#endif
