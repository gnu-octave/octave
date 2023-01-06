////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#if ! defined (octave_defaults_h)
#define octave_defaults_h 1

#include "octave-config.h"

#include <string>

#include "pathsearch.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(config)

extern OCTINTERP_API std::string
prepend_octave_home (const std::string& s);

extern OCTINTERP_API std::string
prepend_octave_exec_home (const std::string& s);

// These could be defined as pure constants, but we'll use
// functions to be consistent with the values that require
// initialization.

extern OCTINTERP_API std::string canonical_host_type (void);
extern OCTINTERP_API std::string release (void);
extern OCTINTERP_API std::string default_pager (void);

// These require initialization, so can't be defined as pure
// constants.  We use functions to access these values so that
// they can't be modified by users.

extern OCTINTERP_API std::string octave_home (void);
extern OCTINTERP_API std::string octave_exec_home (void);

extern OCTINTERP_API std::string bin_dir (void);
extern OCTINTERP_API std::string data_dir (void);
extern OCTINTERP_API std::string dataroot_dir (void);
extern OCTINTERP_API std::string include_dir (void);
extern OCTINTERP_API std::string lib_dir (void);
extern OCTINTERP_API std::string libexec_dir (void);

extern OCTINTERP_API std::string local_ver_arch_lib_dir (void);
extern OCTINTERP_API std::string local_api_arch_lib_dir (void);
extern OCTINTERP_API std::string local_arch_lib_dir (void);
extern OCTINTERP_API std::string arch_lib_dir (void);

extern OCTINTERP_API std::string local_ver_oct_file_dir (void);
extern OCTINTERP_API std::string local_api_oct_file_dir (void);
extern OCTINTERP_API std::string local_oct_file_dir (void);
extern OCTINTERP_API std::string oct_file_dir (void);

extern OCTINTERP_API std::string local_ver_fcn_file_dir (void);
extern OCTINTERP_API std::string local_api_fcn_file_dir (void);
extern OCTINTERP_API std::string local_fcn_file_dir (void);
extern OCTINTERP_API std::string fcn_file_dir (void);

extern OCTINTERP_API std::string oct_data_dir (void);
extern OCTINTERP_API std::string oct_doc_dir (void);
extern OCTINTERP_API std::string oct_etc_dir (void);
extern OCTINTERP_API std::string oct_fonts_dir (void);
extern OCTINTERP_API std::string oct_include_dir (void);
extern OCTINTERP_API std::string oct_lib_dir (void);
extern OCTINTERP_API std::string oct_locale_dir (void);
extern OCTINTERP_API std::string oct_tests_dir (void);

extern OCTINTERP_API std::string info_dir (void);

extern OCTINTERP_API std::string man_dir (void);
extern OCTINTERP_API std::string man1_dir (void);
extern OCTINTERP_API std::string man1_ext (void);

extern OCTINTERP_API std::string image_dir (void);

extern OCTINTERP_API std::string local_startupfile_dir (void);
extern OCTINTERP_API std::string startupfile_dir (void);

extern OCTINTERP_API std::string local_site_defaults_file (void);
extern OCTINTERP_API std::string site_defaults_file (void);

OCTAVE_END_NAMESPACE(config)

OCTAVE_END_NAMESPACE(octave)

#endif
