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

#if ! defined (octave_oct_group_h)
#define octave_oct_group_h 1

#include "octave-config.h"

#include <string>

#include <sys/types.h>

#include "str-vec.h"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(sys)

class
OCTAVE_API
group
{
public:

  group ()
    : m_name (), m_passwd (), m_gid (0), m_mem (), m_valid (false)
  { }

  OCTAVE_DEFAULT_COPY_MOVE_DELETE (group)

  std::string name () const;

  std::string passwd () const;

  gid_t gid () const;

  string_vector mem () const;

  bool ok () const { return m_valid; }

  operator bool () const { return ok (); }

  static group getgrent ();
  static group getgrent (std::string& msg);

  static group getgrgid (gid_t gid);
  static group getgrgid (gid_t gid, std::string& msg);

  static group getgrnam (const std::string& nm);
  static group getgrnam (const std::string& nm, std::string& msg);

  static int setgrent ();
  static int setgrent (std::string& msg);

  static int endgrent ();
  static int endgrent (std::string& msg);

private:

  // The group name.
  std::string m_name;

  // The group password.
  std::string m_passwd;

  // The numeric group id.
  gid_t m_gid;

  // The members of the group;
  string_vector m_mem;

  // Flag that says whether we have been properly initialized.
  bool m_valid;

  // This is how we will create an group object from a pointer
  // to a struct group.
  group (void *p, std::string& msg);
};

OCTAVE_END_NAMESPACE(sys)
OCTAVE_END_NAMESPACE(octave)

#endif
