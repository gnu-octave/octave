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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <sys/types.h>

#if defined (HAVE_GRP_H)
#  include <grp.h>
#endif

#include "lo-error.h"
#include "oct-group.h"
#include "str-vec.h"

#define NOT_SUPPORTED(nm)                       \
  nm ": not supported on this system"

OCTAVE_NORETURN static
void
err_invalid (void)
{
  (*current_liboctave_error_handler) ("invalid group object");
}

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(sys)

std::string
group::name (void) const
{
  if (! ok ())
    err_invalid ();

  return m_name;
}

std::string
group::passwd (void) const
{
  if (! ok ())
    err_invalid ();

  return m_passwd;
}

gid_t
group::gid (void) const
{
  if (! ok ())
    err_invalid ();

  return m_gid;
}

string_vector
group::mem (void) const
{
  if (! ok ())
    err_invalid ();

  return m_mem;
}

group
group::getgrent (void)
{
  std::string msg;
  return getgrent (msg);
}

group
group::getgrent (std::string& msg)
{
#if defined (HAVE_GETGRENT)
  msg = "";
  return group (::getgrent (), msg);
#else
  msg = NOT_SUPPORTED ("getgrent");
  return group ();
#endif
}

group
group::getgrgid (gid_t gid)
{
  std::string msg;
  return getgrgid (gid, msg);
}

group
group::getgrgid (gid_t gid, std::string& msg)
{
#if defined (HAVE_GETGRGID)
  msg = "";
  return group (::getgrgid (gid), msg);
#else
  octave_unused_parameter (gid);

  msg = NOT_SUPPORTED ("getgruid");
  return group ();
#endif
}

group
group::getgrnam (const std::string& nm)
{
  std::string msg;
  return getgrnam (nm, msg);
}

group
group::getgrnam (const std::string& nm, std::string& msg)
{
#if defined (HAVE_GETGRNAM)
  msg = "";
  return group (::getgrnam (nm.c_str ()), msg);
#else
  octave_unused_parameter (nm);

  msg = NOT_SUPPORTED ("getgrnam");
  return group ();
#endif
}

int
group::setgrent (void)
{
  std::string msg;
  return setgrent (msg);
}

int
group::setgrent (std::string& msg)
{
#if defined (HAVE_SETGRENT)
  msg = "";
  ::setgrent ();
  return 0;
#else
  msg = NOT_SUPPORTED ("setgrent");
  return -1;
#endif
}

int
group::endgrent (void)
{
  std::string msg;
  return endgrent (msg);
}

int
group::endgrent (std::string& msg)
{
#if defined (HAVE_ENDGRENT)
  msg = "";
  ::endgrent ();
  return 0;
#else
  msg = NOT_SUPPORTED ("endgrent");
  return -1;
#endif
}

group::group (void *p, std::string& msg)
  : m_name (), m_passwd (), m_gid (0), m_mem (), m_valid (false)
{
#if defined (HAVE_GRP_H)
  msg = "";

  if (p)
    {
      struct ::group *gr = static_cast<struct ::group *> (p);

      m_name = gr->gr_name;

#if defined (HAVE_GR_PASSWD)
      m_passwd = gr->gr_passwd;
#endif

      m_gid = gr->gr_gid;

      // FIXME: Maybe there should be a string_vector constructor
      //        that takes a NUL terminated list of C strings?

      const char *const *tmp = gr->gr_mem;

      int k = 0;
      while (*tmp++)
        k++;

      if (k > 0)
        {
          tmp = gr->gr_mem;

          m_mem.resize (k);

          for (int i = 0; i < k; i++)
            m_mem[i] = tmp[i];
        }

      m_valid = true;
    }
#else
  octave_unused_parameter (p);

  msg = NOT_SUPPORTED ("group functions");
#endif
}

OCTAVE_END_NAMESPACE(sys)
OCTAVE_END_NAMESPACE(octave)
