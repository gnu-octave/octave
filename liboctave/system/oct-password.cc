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

#if defined (HAVE_PWD_H)
#  include <pwd.h>
#endif

#include "lo-error.h"
#include "oct-password.h"

#define NOT_SUPPORTED(nm)                       \
  nm ": not supported on this system"

OCTAVE_NORETURN static
void
err_invalid (void)
{
  (*current_liboctave_error_handler) ("invalid password object");
}

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(sys)

std::string
password::name (void) const
{
  if (! ok ())
    err_invalid ();

  return m_name;
}

std::string
password::passwd (void) const
{
  if (! ok ())
    err_invalid ();

  return m_passwd;
}

uid_t
password::uid (void) const
{
  if (! ok ())
    err_invalid ();

  return m_uid;
}

gid_t
password::gid (void) const
{
  if (! ok ())
    err_invalid ();

  return m_gid;
}

std::string
password::gecos (void) const
{
  if (! ok ())
    err_invalid ();

  return m_gecos;
}

std::string
password::dir (void) const
{
  if (! ok ())
    err_invalid ();

  return m_dir;
}

std::string
password::shell (void) const
{
  if (! ok ())
    err_invalid ();

  return m_shell;
}

password
password::getpwent (void)
{
  std::string msg;
  return getpwent (msg);
}

password
password::getpwent (std::string& msg)
{
#if defined HAVE_GETPWENT
  msg = "";
  return password (::getpwent (), msg);
#else
  msg = NOT_SUPPORTED ("getpwent");
  return password ();
#endif
}

password
password::getpwuid (uid_t uid)
{
  std::string msg;
  return getpwuid (uid, msg);
}

password
password::getpwuid (uid_t uid, std::string& msg)
{
#if defined (HAVE_GETPWUID)
  msg = "";
  return password (::getpwuid (uid), msg);
#else
  octave_unused_parameter (uid);

  msg = NOT_SUPPORTED ("getpwuid");
  return password ();
#endif
}

password
password::getpwnam (const std::string& nm)
{
  std::string msg;
  return getpwnam (nm, msg);
}

password
password::getpwnam (const std::string& nm, std::string& msg)
{
#if defined (HAVE_GETPWNAM)
  msg = "";
  return password (::getpwnam (nm.c_str ()), msg);
#else
  octave_unused_parameter (nm);

  msg = NOT_SUPPORTED ("getpwnam");
  return password ();
#endif
}

int
password::setpwent (void)
{
  std::string msg;
  return setpwent (msg);
}

int
password::setpwent (std::string& msg)
{
#if defined (HAVE_SETPWENT)
  msg = "";
  ::setpwent ();
  return 0;
#else
  msg = NOT_SUPPORTED ("setpwent");
  return -1;
#endif
}

int
password::endpwent (void)
{
  std::string msg;
  return endpwent (msg);
}

int
password::endpwent (std::string& msg)
{
#if defined (HAVE_ENDPWENT)
  msg = "";
  ::endpwent ();
  return 0;
#else
  msg = NOT_SUPPORTED ("endpwent");
  return -1;
#endif
}

password::password (void *p, std::string& msg)
  : m_name (), m_passwd (), m_uid (0), m_gid (0), m_gecos (),
    m_dir (), m_shell (), m_valid (false)
{
#if defined (HAVE_PWD_H)
  msg = "";

  if (p)
    {
      struct ::passwd *pw = static_cast<struct ::passwd *> (p);

      m_name = pw->pw_name;
      m_passwd = pw->pw_passwd;
      m_uid = pw->pw_uid;
      m_gid = pw->pw_gid;
      m_gecos = pw->pw_gecos;
      m_dir = pw->pw_dir;
      m_shell = pw->pw_shell;

      m_valid = true;
    }
#else
  octave_unused_parameter (p);

  msg = NOT_SUPPORTED ("password functions");
#endif
}

OCTAVE_END_NAMESPACE(sys)
OCTAVE_END_NAMESPACE(octave)
