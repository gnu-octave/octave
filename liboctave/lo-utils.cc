// utils.cc
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

#include <climits>
#include <cstdlib>
#include <cstdio>

#include <string>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "lo-error.h"
#include "lo-mappers.h"
#include "lo-utils.h"

// Convert X to the nearest integer value.  Should not pass NaN to
// this function.

int
NINT (double x)
{
  if (x > INT_MAX)
    return INT_MAX;
  else if (x < INT_MIN)
    return INT_MIN;
  else
    return (x > 0) ? ((int) (x + 0.5)) : ((int) (x - 0.5));
}

double
D_NINT (double x)
{
  if (xisinf (x) || xisnan (x))
    return x;
  else
    return floor (x + 0.5);
}

// Save a string.

char *
strsave (const char *s)
{
  if (! s)
    return 0;

  int len = strlen (s);
  char *tmp = new char [len+1];
  tmp = strcpy (tmp, s);
  return tmp;
}

// This function was adapted from xputenv from Karl Berry's kpathsearch
// library.

// XXX FIXME XXX -- make this do the right thing if we don't have a
// SMART_PUTENV.

void
octave_putenv (const std::string& name, const std::string& value)
{
  int new_len = name.length () + value.length () + 2;

  char *new_item = static_cast<char*> (malloc (new_len));

  sprintf (new_item, "%s=%s", name.c_str (), value.c_str ());

  // As far as I can see there's no way to distinguish between the
  // various errors; putenv doesn't have errno values.

  if (putenv (new_item) < 0)
    (*current_liboctave_error_handler) ("putenv (%s) failed", new_item);
}

std::string
octave_fgets (FILE *f)
{
  std::string retval;

  int grow_size = 1024;
  int max_size = grow_size;

  char *buf = static_cast<char *> (malloc (max_size));
  char *bufptr = buf;
  int len = 0;

  do
    {
      if (fgets (bufptr, grow_size, f))
	{
	  len = strlen (bufptr);

	  if (len == grow_size - 1)
	    {
	      int tmp = bufptr - buf + grow_size - 1;
	      grow_size *= 2;
	      max_size += grow_size;
	      buf = static_cast<char *> (realloc (buf, max_size));
	      bufptr = buf + tmp;

	      if (*(bufptr-1) == '\n')
		{
		  *bufptr = '\0';
		  retval = buf;
		}
	    }
	  else if (bufptr[len-1] != '\n')
	    {
	      bufptr[len++] = '\n';
	      bufptr[len] = '\0';
	      retval = buf;
	    }
	  else
	    retval = buf;
	}
      else
	{
	  if (len == 0)
	    {
	      free (buf);

	      buf = 0;
	    }

	  break;
	}
    }
  while (retval.empty ());

  if (buf)
    free (buf);

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
