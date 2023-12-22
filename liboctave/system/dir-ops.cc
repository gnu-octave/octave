////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2024 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cerrno>
#include <cstdlib>
#include <cstring>

#include <list>
#include <string>

#if defined (OCTAVE_USE_WINDOWS_API)
#  include "windows.h"
#endif

#include "dirent-wrappers.h"

#include "dir-ops.h"
#include "file-ops.h"
#include "lo-error.h"
#include "lo-sysdep.h"
#include "str-vec.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(sys)

bool
dir_entry::open (const std::string& n)
{
  if (! n.empty ())
    m_name = n;

  if (! m_name.empty ())
    {
      close ();

      std::string fullname = sys::file_ops::tilde_expand (m_name);

#if defined (OCTAVE_USE_WINDOWS_API)
      std::string msg;

      if (sys::dir_exists (fullname, msg))
        // Never dereference this pointer!
        // We just need something that is non-null.
        m_dir = reinterpret_cast<void *> (1);
      else
        {
          m_dir = nullptr;
          m_errmsg = msg;
        }
#else
      m_dir = octave_opendir_wrapper (fullname.c_str ());

      if (! m_dir)
        m_errmsg = std::strerror (errno);
#endif
    }
  else
    m_errmsg = "dir_entry::open: empty filename";

  return m_dir != nullptr;
}

string_vector
dir_entry::read ()
{
  string_vector retval;

  if (ok ())
    {
      std::list<std::string> dirlist;

#if defined (OCTAVE_USE_WINDOWS_API)
      WIN32_FIND_DATAW find_file_data;
      std::wstring pattern = u8_to_wstring (m_name + "\\*");
      std::string file_name;

      HANDLE h_find_file = FindFirstFileW (pattern.c_str (), &find_file_data);
      if (h_find_file != INVALID_HANDLE_VALUE)
        {
          do
            {
              file_name = u8_from_wstring (find_file_data.cFileName);
              dirlist.push_back (file_name);
            }
          while (FindNextFileW (h_find_file, &find_file_data));

          FindClose (h_find_file);
        }
#else
      char *fname;

      while ((fname = octave_readdir_wrapper (m_dir)))
        dirlist.push_back (fname);
#endif

      retval = string_vector (dirlist);
    }

  return retval;
}

bool
dir_entry::close ()
{
  bool retval = true;

#if defined (OCTAVE_USE_WINDOWS_API)
  m_dir = nullptr;
#else
  if (m_dir)
    {
      retval = (octave_closedir_wrapper (m_dir) == 0);

      m_dir = nullptr;
    }
#endif

  return retval;
}

unsigned int
dir_entry::max_name_length ()
{
  return octave_name_max_wrapper ();
}

OCTAVE_END_NAMESPACE(sys)
OCTAVE_END_NAMESPACE(octave)
