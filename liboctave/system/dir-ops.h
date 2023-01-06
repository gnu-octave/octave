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

#if ! defined (octave_dir_ops_h)
#define octave_dir_ops_h 1

#include "octave-config.h"

#include <string>

#include "str-vec.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(sys)

class
OCTAVE_API
dir_entry
{

  // NOTE: This class cannot be used safely cross-platform (Windows) with
  // non-ASCII characters in paths.
  // Consider replacing the implementation using std::filesystem (C++ 17).
  // In the meantime, consider using sys::get_dirlist instead.

public:

  dir_entry (const std::string& n = "")
    : m_name (n), m_dir (nullptr), m_fail (false), m_errmsg ()
  {
    if (! m_name.empty ())
      open ();
  }

  dir_entry (const dir_entry& d)
    : m_name (d.m_name), m_dir (d.m_dir), m_fail (d.m_fail),
      m_errmsg (d.m_errmsg)
  { }

  dir_entry& operator = (const dir_entry& d)
  {
    if (this != &d)
      {
        m_name = d.m_name;
        m_dir = d.m_dir;
        m_fail = d.m_fail;
        m_errmsg = d.m_errmsg;
      }

    return *this;
  }

  ~dir_entry (void) { close (); }

  bool open (const std::string& = "");

  string_vector read (void);

  bool close (void);

  bool ok (void) const { return m_dir && ! m_fail; }

  operator bool () const { return ok (); }

  std::string error (void) const { return ok () ? "" : m_errmsg; }

  static unsigned int max_name_length (void);

private:

  // Name of the directory.
  std::string m_name;

  // A pointer to the contents of the directory.  We use void here to
  // avoid possible conflicts with the way some systems declare the
  // type DIR.
  void *m_dir;

  // TRUE means the open for this directory failed.
  bool m_fail;

  // If a failure occurs, this contains the system error text.
  std::string m_errmsg;
};

OCTAVE_END_NAMESPACE(sys)
OCTAVE_END_NAMESPACE(octave)

#endif
