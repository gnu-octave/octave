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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>

#include "file-stat.h"
#include "str-vec.h"

#include <defaults.h>
#include "dir-ops.h"
#include "dirfns.h"
#include "error.h"
#include "fn-cache.h"
#include "pathsearch.h"

octave_fcn_file_name_cache *octave_fcn_file_name_cache::instance = 0;

// Update the cache.  Returns TRUE if something needed to be updated.

// We just accumulate all directories ever referenced in the cache and
// we don't delete any old ones.

bool
octave_fcn_file_name_cache::update (const std::string& path)
{
  bool something_changed = false;

  dir_path p /* = path.empty () ? Vload_path_dir_path : dir_path (path) */;

  string_vector dirs = p.all_directories ();

  int len = dirs.length ();

  for (int i = 0; i < len; i++)
    {
      std::string d = dirs[i];

      if (cache.find (d) != cache.end ())
	{
	  if (cache[d].update (d))
	    something_changed = true;
	}
      else
	{
	  cache[d] = file_name_cache_elt (d);
	  something_changed = true;
	}
    }

  return something_changed;
}

string_vector
octave_fcn_file_name_cache::list (const std::string& path, bool no_suffix)
{
  string_vector retval;

  if (! instance)
    instance = new octave_fcn_file_name_cache ();

  if (instance)
    retval = instance->do_list (path, no_suffix);
  else
    error ("unable to create file name cache object!");

  return retval;
}

// Check to see if any of the elements in the cache need to be
// updated, then return the list of names in the cache.

string_vector
octave_fcn_file_name_cache::do_list (const std::string& path, bool no_suffix)
{
  update (path);

  string_vector fcn_file_names;
  string_vector fcn_file_names_no_suffix;

  // For now, always generate the list of function files on each
  // call.

  // FIXME -- this could probably be improved by keeping lists
  // of all the function files for the current load path and only
  // updating that when the load path changes.  Have to be careful to
  // return the right thing when we are only looking for a subset of
  // all the files in the load path.

  int total_len = 0;

  dir_path p /* = path.empty () ? Vload_path_dir_path : dir_path (path) */;

  string_vector dirs = p.all_directories ();

  int ndirs = dirs.length ();

  if (ndirs > 1)
    {
      for (int i = 0; i < ndirs; i++)
	{
	  std::string d = dirs[i];

	  total_len += cache[d].length ();
	}

      fcn_file_names.resize (total_len);
      fcn_file_names_no_suffix.resize (total_len);

      int k = 0;

      for (int j = 0; j < ndirs; j++)
	{
	  std::string d = dirs[j];

	  file_name_cache_elt elt = cache[d];

	  int len = elt.length ();

	  string_vector ffn = elt.fcn_file_names;
	  string_vector ffnns = elt.fcn_file_names_no_suffix;

	  for (int i = 0; i < len; i++)
	    {
	      fcn_file_names[k] = ffn[i];
	      fcn_file_names_no_suffix[k] = ffnns[i];

	      k++;
	    }
	}
    }
  else if (ndirs == 1)
    {
      std::string d = dirs[0];

      file_name_cache_elt elt = cache[d];

      fcn_file_names = elt.fcn_file_names;
      fcn_file_names_no_suffix = elt.fcn_file_names_no_suffix;
    }

  return no_suffix ? fcn_file_names_no_suffix : fcn_file_names;
}

// Create a list of the function names in a given directory.  Returns
// TRUE if the cache element was out of date.

bool
file_name_cache_elt::update (const std::string& dir_name)
{
  bool retval = false;

  file_stat file (dir_name);

  // If the directory doesn't exist, delete the names in the cache.
  // If it does exist,read it only if it is out of date.

  if (file)
    {
      if (file.is_newer (timestamp))
	{
	  retval = true;

	  timestamp = file.mtime ();

	  dir_entry dir (dir_name);

	  if (dir)
	    {
	      string_vector tmp = dir.read ();

	      int max_len = tmp.length ();

	      fcn_file_names.resize (max_len);
	      fcn_file_names_no_suffix.resize (max_len);

	      int k = 0;
	      int i;
	      for (i = 0; i < max_len; i++)
		{
		  std::string entry = tmp[i];

		  int len = entry.length ();

#if defined (ENABLE_DYNAMIC_LINKING)
		  if ((len > 2
		       && entry[len-2] == '.' && entry[len-1] == 'm')
		      || (len > 4
			  && entry[len-4] == '.' && entry[len-3] == 'o'
			  && entry[len-2] == 'c' && entry[len-1] == 't'))
#else
		  if (len > 2
		      && entry[len-2] == '.' && entry[len-1] == 'm')
#endif
		    {
		      fcn_file_names[k] = entry;

		      fcn_file_names_no_suffix[k] = (entry[len-1] == 'm')
			? std::string (entry, 0, len-2)
			: std::string (entry, 0, len-4);

		      k++;
		    }
		}

	      fcn_file_names.resize (k);
	      fcn_file_names_no_suffix.resize (k);
	    }
	  else
	    {
	      fcn_file_names.resize (0);
	      fcn_file_names_no_suffix.resize (0);
	    }
	}
    }
  else
    {
      fcn_file_names.resize (0);
      fcn_file_names_no_suffix.resize (0);
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
