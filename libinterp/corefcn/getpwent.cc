/*

Copyright (C) 1996-2015 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>

#include <sys/types.h>

#include "oct-passwd.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-map.h"
#include "ov.h"
#include "ovl.h"
#include "utils.h"

// Password file functions.  (Why not?)

static octave_value
mk_pw_map (const octave_passwd& pw)
{
  octave_value retval;

  if (pw)
    {
      octave_scalar_map m;

      m.assign ("name", pw.name ());
      m.assign ("passwd", pw.passwd ());
      m.assign ("uid", static_cast<double> (pw.uid ()));
      m.assign ("gid", static_cast<double> (pw.gid ()));
      m.assign ("gecos", pw.gecos ());
      m.assign ("dir", pw.dir ());
      m.assign ("shell", pw.shell ());

      retval = ovl (m);
    }
  else
    retval = ovl (0);

  return retval;
}

DEFUN (getpwent, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {@var{pw_struct} =} getpwent ()\n\
Return a structure containing an entry from the password database,\n\
opening it if necessary.\n\
\n\
Once the end of the data has been reached, @code{getpwent} returns 0.\n\
@seealso{setpwent, endpwent}\n\
@end deftypefn")
{
  if (args.length () != 0)
    print_usage ();

  std::string msg;

  // octave_passwd::getpwent may set msg.
  octave_value val = mk_pw_map (octave_passwd::getpwent (msg));

  return ovl (val, msg);
}

DEFUN (getpwuid, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {@var{pw_struct} =} getpwuid (@var{uid}).\n\
Return a structure containing the first entry from the password database\n\
with the user ID @var{uid}.\n\
\n\
If the user ID does not exist in the database, @code{getpwuid} returns 0.\n\
@seealso{getpwnam}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  double dval = args(0).double_value ();

  if (D_NINT (dval) != dval)
    error ("getpwuid: UID must be an integer");

  uid_t uid = static_cast<uid_t> (dval);

  std::string msg;

  // octave_passwd::getpwuid may set msg.
  octave_value val = mk_pw_map (octave_passwd::getpwuid (uid, msg));

  return ovl (val, msg);
}

DEFUN (getpwnam, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {@var{pw_struct} =} getpwnam (@var{name})\n\
Return a structure containing the first entry from the password database\n\
with the user name @var{name}.\n\
\n\
If the user name does not exist in the database, @code{getpwname} returns 0.\n\
@seealso{getpwuid}\n\
@end deftypefn")
{
  if (args.length () != 1)
    print_usage ();

  std::string s = args(0).string_value ();

  std::string msg;

  // octave_passwd::getpwnam may set msg.
  octave_value val = mk_pw_map (octave_passwd::getpwnam (s, msg));

  return ovl (val, msg);
}

DEFUN (setpwent, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {} setpwent ()\n\
Return the internal pointer to the beginning of the password database.\n\
@seealso{getpwent, endpwent}\n\
@end deftypefn")
{
  if (args.length () != 0)
    print_usage ();

  std::string msg;

  // octave_passwd::setpwent may set msg.
  int status = octave_passwd::setpwent (msg);

  return ovl (static_cast<double> (status), msg);
}

DEFUN (endpwent, args, ,
       "-*- texinfo -*-\n\
@deftypefn {} {} endpwent ()\n\
Close the password database.\n\
@seealso{getpwent, setpwent}\n\
@end deftypefn")
{
  if (args.length () != 0)
    print_usage ();

  std::string msg;

  // octave_passwd::endpwent may set msg.
  int status = octave_passwd::endpwent (msg);

  return ovl (static_cast<double> (status), msg);
}
