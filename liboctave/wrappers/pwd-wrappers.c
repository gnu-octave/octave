////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023 The Octave Project Developers
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

// pwd.h might set some preprocessor definitions that replace Octave functions
// in compilation units where it is included.  Use these wrappers to avoid that.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#if defined (HAVE_PWD_H)
#  include <pwd.h>
#endif

#include "pwd-wrappers.h"

struct passwd *octave_getpwent_wrapper (void)
{
#if defined HAVE_GETPWENT
  return getpwent ();
#else
  return NULL;
#endif
}

struct passwd *octave_getpwuid_wrapper (uid_t uid)
{
#if defined (HAVE_GETPWUID)
  return getpwuid (uid);
#else
  octave_unused_parameter (uid);

  return NULL;
#endif
}

struct passwd *octave_getpwnam_wrapper (const char *nam)
{
#if defined (HAVE_GETPWNAM)
  return getpwnam (nam);
#else
  octave_unused_parameter (nam);

  return NULL;
#endif
}

void octave_setpwent_wrapper (void)
{
#if defined (HAVE_SETPWENT)
  setpwent ();
#endif
  return;
}

void octave_endpwent_wrapper (void)
{
#if defined (HAVE_ENDPWENT)
  endpwent ();
#endif
  return;
}

void octave_from_passwd (const struct passwd *pw,
                         struct octave_passwd_wrapper *oct_pw)
{
#if defined (HAVE_PWD_H)
  oct_pw->pw_name = pw->pw_name;
  oct_pw->pw_passwd = pw->pw_passwd;
  oct_pw->pw_uid = pw->pw_uid;
  oct_pw->pw_gid = pw->pw_gid;
  oct_pw->pw_gecos = pw->pw_gecos;
  oct_pw->pw_dir = pw->pw_dir;
  oct_pw->pw_shell = pw->pw_shell;
#else
  octave_unused_parameter (pw);
  octave_unused_parameter (oct_pw);
#endif
  return;
}
