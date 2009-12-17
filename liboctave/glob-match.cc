/*

Copyright (C) 1996, 1997, 2000, 2005, 2006, 2007, 2009 John W. Eaton

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

#include <fnmatch.h>
#include <glob.h>

#include <iostream>
#include <string>

#include "file-stat.h"
#include "glob-match.h"
#include "str-vec.h"

bool
glob_match::match (const std::string& s)
{
  int npat = pat.length ();

  const char *str = s.c_str ();

  int fnmatch_flags = 0;

  if (flags & pathname)
    fnmatch_flags |= FNM_PATHNAME;

  if (flags & noescape)
    fnmatch_flags |= FNM_NOESCAPE;

  if (flags & period)
    fnmatch_flags |= FNM_PERIOD;

  for (int i = 0; i < npat; i++)
    if (fnmatch (pat(i).c_str (), str, fnmatch_flags) != FNM_NOMATCH)
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
single_match_exists (const std::string& file)
{
  file_stat s (file);

  return s.exists ();
}

string_vector
glob_match::glob_internal (void)
{
  string_vector retval;

  int npat = pat.length ();

  int k = 0;

  for (int i = 0; i < npat; i++)
    {
      std::string xpat = pat(i);

      if (! xpat.empty ())
	{
	  glob_t glob_info;

	  int err = ::glob (xpat.c_str (), GLOB_NOSORT, 0, &glob_info);

	  if (! err)
	    {
	      int n = glob_info.gl_pathc;

	      const char * const *matches = glob_info.gl_pathv;

	      // FIXME -- we shouldn't have to check to see if
	      // a single match exists, but it seems that glob() won't
	      // check for us unless the pattern contains globbing
	      // characters.  Hmm.

	      if (n > 1
		  || (n == 1
		      && single_match_exists (std::string (matches[0]))))
		{
		  retval.resize (k+n);

		  for (int j = 0; j < n; j++)
		    retval[k++] = matches[j];
		}

	      globfree (&glob_info);
	    }
	}
    }

  return retval.sort ();
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

