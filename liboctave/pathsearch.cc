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

#include <cstdlib>

#include <string>

#include "pathsearch.h"
#include "str-vec.h"

extern "C"
{
// Have to redefine these because they conflict with new C++ keywords
// or otherwise cause trouble...
#define boolean kpse_boolean
#define true kpse_true
#define false kpse_false
#define string kpse_string

#include <kpathsea/pathsearch.h>

extern unsigned int kpathsea_debug;

#undef true
#undef false
#undef boolean
#undef string
}

bool dir_path::kpathsea_debug_initialized = false;

string_vector
dir_path::elements (void)
{
  return initialized ? pv : string_vector ();
}

string_vector
dir_path::all_directories (void)
{
  int count = 0;
  string_vector retval;

  if (initialized)
    {
      int len = pv.length ();

      int nmax = len > 32 ? len : 32;

      retval.resize (len);

      for (int i = 0; i < len; i++)
	{
	  str_llist_type *elt_dirs = kpse_element_dirs (pv[i].c_str ());

	  str_llist_elt_type *dir;
	  for (dir = *elt_dirs; dir; dir = STR_LLIST_NEXT (*dir))
	    {
	      char *elt_dir = STR_LLIST (*dir);

	      if (elt_dir)
		{
		  if (count == nmax)
		    nmax *= 2;

		  retval.resize (nmax);

		  retval[count++] = elt_dir;
		}
	    }
	}

      retval.resize (count);
    }

  return retval;
}

string
dir_path::find_first (const string& nm)
{
  string retval;

  if (initialized)
    {
      char *tmp = kpse_path_search (p.c_str (), nm.c_str (),
				    kpse_true);
      if (tmp)
	{
	  retval = tmp;
	  free (tmp);
	}
    }

  return retval;
}

string_vector
dir_path::find_all (const string& nm)
{
  string_vector retval;

  kpse_string *tmp = kpse_all_path_search (p.c_str (), nm.c_str ());

  if (tmp)
    {
      int count = 0;
      kpse_string *ptr = tmp;
      while (*ptr++)
	count++;

      retval.resize (count);

      for (int i = 0; i < count; i++)
	retval[i] = tmp[i];
    }

  return retval;
}

void
dir_path::init (void)
{
  initialized = false;

  if (! kpathsea_debug_initialized)
    {
      char *s = getenv ("KPATHSEA_DEBUG");

      if (s)
	kpathsea_debug |= atoi (s);
    }

  int count = 0;
  char *path_elt = kpse_path_element (p.c_str ());
  while (path_elt)
    {
      path_elt = kpse_path_element (0);
      count++;
    }

  pv.resize (count);

  path_elt = kpse_path_element (p.c_str ());

  for (int i = 0; i < count; i++)
    {
      pv[i] = path_elt;
      path_elt = kpse_path_element (0);
    }

  initialized = true;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
