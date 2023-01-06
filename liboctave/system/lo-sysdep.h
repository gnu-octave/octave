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

#if ! defined (octave_lo_sysdep_h)
#define octave_lo_sysdep_h 1

#include "octave-config.h"

#include <fstream>
#include <string>

#include <sys/types.h>

#include "lo-ieee.h"

class string_vector;

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(sys)

extern OCTAVE_API int system (const std::string& cmd_str);

extern OCTAVE_API std::string getcwd (void);

extern OCTAVE_API int chdir (const std::string&);

extern OCTAVE_API bool
get_dirlist (const std::string& dirname, string_vector& dirlist,
             std::string& msg);

extern OCTAVE_API std::FILE *
fopen (const std::string& name, const std::string& mode);

extern OCTAVE_API std::FILE *
fopen_tmp (const std::string& name, const std::string& mode);

extern OCTAVE_API std::fstream
fstream (const std::string& name,
         const std::ios::openmode mode = std::ios::in | std::ios::out);

extern OCTAVE_API std::ifstream
ifstream (const std::string& name,
          const std::ios::openmode mode = std::ios::in);

extern OCTAVE_API std::ofstream
ofstream (const std::string& name,
          const std::ios::openmode mode = std::ios::out);

extern OCTAVE_API void
putenv_wrapper (const std::string& name, const std::string& value);

extern OCTAVE_API std::string getenv_wrapper (const std::string&);

extern OCTAVE_API int unsetenv_wrapper (const std::string&);

extern OCTAVE_API std::wstring u8_to_wstring (const std::string&);

extern OCTAVE_API std::string u8_from_wstring (const std::wstring&);

extern OCTAVE_API std::string
get_ASCII_filename (const std::string& long_file_name,
                    const bool allow_locale = false);

OCTAVE_END_NAMESPACE(sys)
OCTAVE_END_NAMESPACE(octave)

#endif
