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

#include <fnmatch.h>
#include <glob.h>

#include <iostream.h>

#include <string>

#include "file-ops.h"
#include "oct-glob.h"
#include "str-vec.h"

bool
glob_match::match (const string& s)
{
  int npat = pat.length ();

  const char *str = s.c_str ();

  for (int i = 0; i < npat; i++)
    if (fnmatch (pat(i).c_str (), str, flags) != FNM_NOMATCH)
      return true;

  return false;
}

Array<bool>
glob_match::match (const string_vector& s)
{
  int n = s.length ();

  Array<bool> retval (n);

  for (int i = 0; i < n; i++)
    retval(i) = match (s[i]);

  return retval;
}

static bool
single_match_exists (const string& file)
{
  file_stat s (file);

  return s.exists ();
}

string_vector
glob_match::glob (void)
{
  string_vector retval;

  int npat = pat.length ();

  int k = 0;

  for (int i = 0; i < npat; i++)
    {
      string xpat = pat(i);

      if (! xpat.empty ())
	{
	  glob_t glob_info;

	  int err = ::glob (xpat.c_str (), GLOB_NOSORT, 0, &glob_info);

	  if (! err)
	    {
	      int n = glob_info.gl_pathc;

	      char **matches = glob_info.gl_pathv;

	      // XXX FIXME XXX -- we shouldn't have to check to see if
	      // a single match exists, but it seems that glob() won't
	      // check for us unless the pattern contains globbing
	      // characters.  Hmm.

	      if (n > 1
		  || (n == 1 && single_match_exists (string (matches[0]))))
		{
		  retval.resize (k+n);

		  for (int j = 0; j < n; j++)
		    retval[k++] = matches[j];
		}

	      globfree (&glob_info);
	    }
	}
    }

  return retval.qsort ();
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

