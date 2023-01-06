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

#include <string>

#include <sys/types.h>

#include "oct-password.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "oct-map.h"
#include "ov.h"
#include "ovl.h"
#include "utils.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Password file functions.  (Why not?)

static octave_value
mk_pw_map (const sys::password& pw)
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

      return octave_value (m);
    }
  else
    return octave_value (0);
}

DEFUN (getpwent, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{pw_struct} =} getpwent ()
Return a structure containing an entry from the password database,
opening it if necessary.

Once the end of the data has been reached, @code{getpwent} returns 0.
@seealso{setpwent, endpwent}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  std::string msg;

  // sys::password::getpwent may set msg.
  octave_value val = mk_pw_map (sys::password::getpwent (msg));

  return ovl (val, msg);
}

DEFUN (getpwuid, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{pw_struct} =} getpwuid (@var{uid}).
Return a structure containing the first entry from the password database
with the user ID @var{uid}.

If the user ID does not exist in the database, @code{getpwuid} returns 0.
@seealso{getpwnam}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  double dval = args(0).double_value ();

  if (math::x_nint (dval) != dval)
    error ("getpwuid: UID must be an integer");

  uid_t uid = static_cast<uid_t> (dval);

  std::string msg;

  // sys::password::getpwuid may set msg.
  octave_value val = mk_pw_map (sys::password::getpwuid (uid, msg));

  return ovl (val, msg);
}

DEFUN (getpwnam, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{pw_struct} =} getpwnam (@var{name})
Return a structure containing the first entry from the password database
with the user name @var{name}.

If the user name does not exist in the database, @code{getpwname} returns 0.
@seealso{getpwuid}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  std::string s = args(0).string_value ();

  std::string msg;

  // sys::password::getpwnam may set msg.
  octave_value val = mk_pw_map (sys::password::getpwnam (s, msg));

  return ovl (val, msg);
}

DEFUN (setpwent, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {[@var{status}, @var{msg}] =} setpwent ()
Return the internal pointer to the beginning of the password database.
@seealso{getpwent, endpwent}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  std::string msg;

  // sys::password::setpwent may set msg.
  int status = sys::password::setpwent (msg);

  return ovl (static_cast<double> (status), msg);
}

DEFUN (endpwent, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {[@var{status}, @var{msg}] =} endpwent ()
Close the password database.
@seealso{getpwent, setpwent}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  std::string msg;

  // sys::password::endpwent may set msg.
  int status = sys::password::endpwent (msg);

  return ovl (static_cast<double> (status), msg);
}

OCTAVE_END_NAMESPACE(octave)
