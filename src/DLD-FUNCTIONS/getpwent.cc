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

#include <string>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "oct-passwd.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-map.h"
#include "ov.h"
#include "oct-obj.h"
#include "utils.h"

// Password file functions.  (Why not?)

static octave_value
mk_pw_map (const octave_passwd& pw)
{
  octave_value retval;

  if (pw)
    {
      Octave_map m;

      m ["name"] = pw.name ();
      m ["passwd"] = pw.passwd ();
      m ["uid"] = static_cast<double> (pw.uid ());
      m ["gid"] = static_cast<double> (pw.gid ());
      m ["gecos"] = pw.gecos ();
      m ["dir"] = pw.dir ();
      m ["shell"] = pw.shell ();

      retval = m;
    }
  else
    retval = 0.0;

  return retval;
}

DEFUN_DLD (getpwent, args, ,
 "getpwent ()\n\
\n\
Read an entry from the password-file stream, opening it if necessary.")
{
  octave_value_list retval;

  retval(1) = string ();
  retval(0) = 0.0;

  int nargin = args.length ();

  if (nargin == 0)
    {
      string msg;

      retval(0) = mk_pw_map (octave_passwd::getpwent (msg));
      retval(1) = msg;
    }
  else
    print_usage ("getpwent");

  return retval;
}

DEFUN_DLD (getpwuid, args, ,
  "getpwuid (UID)\n\
\n\
Search for a password entry with a matching user ID.")
{
  octave_value_list retval;

  retval(1) = string ();
  retval(0) = 0.0;

  int nargin = args.length ();

  if (nargin == 1)
    {
      double dval = args(0).double_value ();

      if (! error_state)
	{
	  if (D_NINT (dval) == dval)
	    {
	      uid_t uid = static_cast<uid_t> (dval);

	      string msg;

	      retval(0) = mk_pw_map (octave_passwd::getpwuid (uid, msg));
	      retval(1) = msg;
	    }
	  else
	    error ("getpwuid: argument must be an integer");
	}
    }
  else
    print_usage ("getpwuid");

  return retval;
}

DEFUN_DLD (getpwnam, args, ,
  "getpwnam (NAME)\n\
\n\
Search for password entry with a matching username.")
{
  octave_value_list retval;

  retval(1) = string ();
  retval(0) = 0.0;

  int nargin = args.length ();

  if (nargin == 1)
    {
      string s = args(0).string_value ();

      if (! error_state)
	{
	  string msg;

	  retval(0) = mk_pw_map (octave_passwd::getpwnam (s.c_str (), msg));
	  retval(1) = msg;
	}
    }
  else
    print_usage ("getpwnam");

  return retval;
}

DEFUN_DLD (setpwent, args, ,
  "setpwent ()\n\
\n\
Rewind the password-file stream.")
{
  octave_value_list retval;

  retval(1) = string ();
  retval(0) = -1.0;

  int nargin = args.length ();

  if (nargin == 0)
    {
      string msg;

      retval(0) = static_cast<double> (octave_passwd::setpwent (msg));
      retval(1) = msg;
    }
  else
    print_usage ("setpwent");

  return retval;
}

DEFUN_DLD (endpwent, args, ,
  "endpwent ()\n\
\n\
Close the password-file stream.")
{
  octave_value_list retval;

  retval(1) = string ();
  retval(0) = -1.0;

  int nargin = args.length ();

  if (nargin == 0)
    {
      string msg;

      retval(0) = static_cast<double> (octave_passwd::endpwent (msg));
      retval(1) = msg;
    }
  else
    print_usage ("endpwent");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
