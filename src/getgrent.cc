/*

Copyright (C) 1996 John W. Eaton

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

#ifdef HAVE_GRP_H
#include <grp.h>
#endif

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "oct-map.h"
#include "ov.h"
#include "oct-obj.h"
#include "utils.h"

// Group file functions.  (Why not?)

static octave_value
mk_gr_map (struct group *gr)
{
  octave_value retval;

  if (gr)
    {
      Octave_map m;

      m ["name"] = gr->gr_name;
#if defined (HAVE_GR_PASSWD)
      m ["passwd"] = gr->gr_passwd;
#else
      m ["passwd"] = "";
#endif
      m ["gid"] = STATIC_CAST (double, gr->gr_gid);

      if (gr->gr_mem)
	{
	  // XXX FIXME XXX -- maybe there should be a string_vector
	  // constructor that takes a NULL terminated list of C
	  // strings.

	  char **tmp = gr->gr_mem;

	  int k = 0;
	  while (*tmp++)
	    k++;

	  if (k > 0)
	    {
	      tmp = gr->gr_mem;

	      string_vector members (k);

	      for (int i = 0; i < k; i++)
		members[i] = tmp[i];

	      m ["mem"] = members;
	    }
	  else
	    m ["mem"] = "";
	}

      retval = m;
    }
  else
    retval = 0.0;

  return retval;
}

DEFUN_DLD (getgrent, args, ,
 "getgrent ()\n\
\n\
Read an entry from the group-file stream, opening it if necessary.")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
#ifdef HAVE_GETGRENT
      retval = mk_gr_map (getgrent ());
#else
      gripe_not_supported ("getgrent");
#endif
    }
  else
    print_usage ("getgrent");

  return retval;
}

DEFUN_DLD (getgrgid, args, ,
  "getgrgid (GID)\n\
\n\
Search for a group entry with a matching group ID.")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
#ifdef HAVE_GETGRGID
      double dval = args(0).double_value ();

      if (! error_state)
	{
	  if (D_NINT (dval) == dval)
	    {
	      gid_t gid = STATIC_CAST (gid_t, dval);

	      retval = mk_gr_map (getgrgid (gid));
	    }
	  else
	    error ("getgrgid: argument must be an integer");
	}
#else
      gripe_not_supported ("getgrgid");
#endif
    }
  else
    print_usage ("getgrgid");

  return retval;
}

DEFUN_DLD (getgrnam, args, ,
  "getgrnam (NAME)\n\
\n\
Search for group entry with a matching group name.")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
#ifdef HAVE_GETGRNAM
      string s = args(0).string_value ();

      if (! error_state)
	retval = mk_gr_map (getgrnam (s.c_str ()));
#else
      gripe_not_supported ("getgrnam");
#endif
    }
  else
    print_usage ("getgrnam");

  return retval;
}

DEFUN_DLD (setgrent, args, ,
  "setgrent ()\n\
\n\
Rewind the group-file stream.")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
#ifdef HAVE_SETGRENT
      setgrent ();
#else
      gripe_not_supported ("setgrent");
#endif
    }
  else
    print_usage ("setgrent");

  return retval;
}

DEFUN_DLD (endgrent, args, ,
  "endgrent ()\n\
\n\
Close the group-file stream.")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
#ifdef HAVE_ENDGRENT
      endgrent ();
#else
      gripe_not_supported ("endgrent");
#endif
    }
  else
    print_usage ("endgrent");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
