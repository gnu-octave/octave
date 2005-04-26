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

#if !defined (octave_fn_cache_h)
#define octave_fn_cache_h 1

#include <string>
#include <map>

#include "oct-time.h"

class string_vector;

// XXX FIXME XXX -- this should maybe be nested in the
// octave_fcn_file_name_cache class...

class
file_name_cache_elt
{
public:

  file_name_cache_elt (void)
    : timestamp (static_cast<time_t> (0)), fcn_file_names (),
      fcn_file_names_no_suffix ()
  { update (std::string ()); }

  file_name_cache_elt (const std::string& dir_name)
    : timestamp (static_cast<time_t> (0)), fcn_file_names (),
      fcn_file_names_no_suffix ()
  { update (dir_name); }

  file_name_cache_elt (const file_name_cache_elt& elt)
  {
    timestamp = elt.timestamp;
    fcn_file_names = elt.fcn_file_names;
    fcn_file_names_no_suffix = elt.fcn_file_names_no_suffix;
  }

  file_name_cache_elt& operator = (const file_name_cache_elt& elt)
  {
    if (&elt != this)
      {
	timestamp = elt.timestamp;
	fcn_file_names = elt.fcn_file_names;
	fcn_file_names_no_suffix = elt.fcn_file_names_no_suffix;
      }
    return *this;
  }

  ~file_name_cache_elt (void) { }

  int length (void) { return fcn_file_names.length (); }

  bool update (const std::string& dir_name);

  // The time we last read this directory.
  octave_time timestamp;

  // The list of file names in this directory.
  string_vector fcn_file_names;

  // The list of file names in this directory without the .m or .oct
  // suffixes.
  string_vector fcn_file_names_no_suffix;
};

class
octave_fcn_file_name_cache
{
protected:

  octave_fcn_file_name_cache (void) : cache () { update (std::string ()); }

public:

  ~octave_fcn_file_name_cache (void) { }

  bool update (const std::string& path);

  static string_vector list (bool no_suffix = false)
    { return list (std::string (), no_suffix); }

  static string_vector list (const std::string& path, bool no_suffix = false);

  static string_vector list_no_suffix (void)
    { return list (true); }

  static string_vector list_no_suffix (const std::string& path)
    { return list (path, true); }

private:

  static octave_fcn_file_name_cache* instance;

  // An associative array of all the directory names in the load path
  // and the corresponding cache elements.
  std::map<std::string, file_name_cache_elt> cache;

  string_vector do_list (const std::string& path, bool no_suffix);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
