/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_PWD_H
#include <pwd.h>
#endif

#include "lo-error.h"
#include "oct-passwd.h"

string
octave_passwd::name (void) const
{
  if (! ok ())
    gripe_invalid ();

  return pw_name;
}

string
octave_passwd::passwd (void) const
{
  if (! ok ())
    gripe_invalid ();

  return pw_passwd;
}

uid_t
octave_passwd::uid (void) const
{
  if (! ok ())
    gripe_invalid ();

  return pw_uid;
}

gid_t
octave_passwd::gid (void) const
{
  if (! ok ())
    gripe_invalid ();

  return pw_gid;
}

string
octave_passwd::gecos (void) const
{
  if (! ok ())
    gripe_invalid ();

  return pw_gecos;
}

string
octave_passwd::dir (void) const
{
  if (! ok ())
    gripe_invalid ();

  return pw_dir;
}

string
octave_passwd::shell (void) const
{
  if (! ok ())
    gripe_invalid ();

  return pw_shell;
}

octave_passwd
octave_passwd::getpwent (void)
{
#ifdef HAVE_GETPWENT
  return octave_passwd (::getpwent ());
#else
  gripe_not_implemented ("getpwent");

  return octave_passwd ();
#endif
}

octave_passwd
octave_passwd::getpwuid (uid_t uid)
{
#ifdef HAVE_GETPWUID
  return octave_passwd (::getpwuid (uid));
#else
  gripe_not_implemented ("getpwuid");

  return octave_passwd ();
#endif
}

octave_passwd
octave_passwd::getpwnam (const string& nm)
{
#ifdef HAVE_GETPWNAM
  return octave_passwd (::getpwnam (nm.c_str ()));
#else
  gripe_not_implemented ("getpwnam");

  return octave_passwd ();
#endif
}

void
octave_passwd::setpwent (void)
{
#ifdef HAVE_SETPWENT
  ::setpwent ();
#else
  gripe_not_implemented ("setpwent");
#endif
}

void
octave_passwd::endpwent (void)
{
#ifdef HAVE_ENDPWENT
  ::endpwent ();
#else
  gripe_not_implemented ("endpwent");
#endif
}

octave_passwd::octave_passwd (void *p)
  : pw_name (), pw_passwd (), pw_uid (0), pw_gid (0), pw_gecos (),
    pw_dir (), pw_shell (), valid (false)
{
#ifdef HAVE_PWD_H
  if (p)
    {
      struct passwd *pw = static_cast<struct passwd *> (p);

      pw_name = pw->pw_name;
      pw_passwd = pw->pw_passwd;
      pw_uid = pw->pw_uid;
      pw_gid = pw->pw_gid;
      pw_gecos = pw->pw_gecos;
      pw_dir = pw->pw_dir;
      pw_shell = pw->pw_shell;

      valid = true;
    }
#endif
}

void
octave_passwd::gripe_invalid (void) const
{
  (*current_liboctave_error_handler) ("invalid password object");
}

void
octave_passwd::gripe_not_supported (const string& fcn) const
{
  (*current_liboctave_error_handler)
    ("%s: not supported on this system", fcn.c_str ());
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
