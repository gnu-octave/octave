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

#if !defined (octave_passwd_h)
#define octave_passwd_h 1

#include <string>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

class
octave_passwd
{
public:

  octave_passwd (void)
    : pw_name (), pw_passwd (), pw_uid (0), pw_gid (0), pw_gecos (),
      pw_dir (), pw_shell (), valid (false)
  { }

  octave_passwd (const octave_passwd& pw)
    : pw_name (pw.pw_name), pw_passwd (pw.pw_passwd),
      pw_uid (pw.pw_uid), pw_gid (pw.pw_gid), pw_gecos (pw.pw_gecos),
      pw_dir (pw.pw_dir), pw_shell (pw.pw_shell), valid (pw.valid)
  { }

  octave_passwd& operator = (const octave_passwd& pw)
  {
    if (this != &pw)
      {
	pw_name = pw.pw_name;
	pw_passwd = pw.pw_passwd;
	pw_uid = pw.pw_uid;
	pw_gid = pw.pw_gid;
	pw_gecos = pw.pw_gecos;
	pw_dir = pw.pw_dir;
	pw_shell = pw.pw_shell;
	valid = pw.valid;
      }

    return *this;
  }

  ~octave_passwd (void) { }

  string name (void) const;

  string passwd (void) const;

  uid_t uid (void) const;

  gid_t gid (void) const;

  string gecos (void) const;

  string dir (void) const;

  string shell (void) const;

  bool ok (void) const { return valid; }

  operator void* () const
    { return ok ()
	? static_cast<void *> (-1) : static_cast<void *> (0); }

  static octave_passwd getpwent (void);
  static octave_passwd getpwent (string& msg);

  static octave_passwd getpwuid (uid_t uid);
  static octave_passwd getpwuid (uid_t uid, string& msg);

  static octave_passwd getpwnam (const string& nm);
  static octave_passwd getpwnam (const string& nm, string& msg);

  static int setpwent (void);
  static int setpwent (string& msg);

  static int endpwent (void);
  static int endpwent (string& msg);

private:

  // User name.
  string pw_name;

  // Encrypted password.
  string pw_passwd;

  // Numeric user id.
  uid_t pw_uid;

  // Numeric group id.
  gid_t pw_gid;

  // Miscellaneous junk.
  string pw_gecos;

  // Home directory.
  string pw_dir;

  // Login shell.
  string pw_shell;

  // Flag that says whether we have been properly initialized.
  bool valid;

  // This is how we will create an octave_passwd object from a pointer
  // to a struct passwd.
  octave_passwd (void *p, string& msg);

  void gripe_invalid (void) const;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
