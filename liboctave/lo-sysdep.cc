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

/*

The function gethostname was adapted from a similar function from GNU
Bash, the Bourne Again SHell, copyright (C) 1987, 1989, 1991 Free
Software Foundation, Inc.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>
#include <string>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#if ! defined (HAVE_GETHOSTNAME) && defined (HAVE_SYS_UTSNAME_H)
#include <sys/utsname.h>
#endif

#include "lo-error.h"
#include "pathlen.h"

string
octave_getcwd (void)
{
  string retval;

  char buf[MAXPATHLEN];

  char *tmp = 0;

#if defined (__EMX__)
  tmp = _getcwd2 (buf, MAXPATHLEN);
#elif defined (HAVE_GETCWD)
  tmp = getcwd (buf, MAXPATHLEN);
#elif defined (HAVE_GETWD)
  tmp = getwd (buf);
#endif

  if (tmp)
    retval = tmp;
  else
    (*current_liboctave_error_handler) ("unable to find current directory");

  return retval;
}

int
octave_chdir (const string& path)
{
#if defined (__EMX__)
  int retval = -1;

  char *tmp_path = strsave (path.c_str ());

  if (path.length () == 2 && path[1] == ':')
    {
      char *upper_case_dir_name = strupr (tmp_path);
      _chdrive (upper_case_dir_name[0]);
      if (_getdrive () == upper_case_dir_name[0])
	retval = _chdir2 ("/");
    }
  else
    retval = _chdir2 (tmp_path);

  delete [] tmp_path;

  return retval;
#else
  return chdir (path.c_str ());
#endif
}

#if ! defined (HAVE_GETHOSTNAME) && defined (HAVE_SYS_UTSNAME_H)

int
gethostname (char *name, int namelen)
{
  int i;
  struct utsname ut;

  --namelen;

  uname (&ut);
  i = strlen (ut.nodename) + 1;
  strncpy (name, ut.nodename, i < namelen ? i : namelen);
  name[namelen] = '\0';

  return 0;
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
