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

#if !defined (octave_pathsearch_h)
#define octave_pathsearch_h 1

#include <string>

#include "str-vec.h"

class
OCTAVE_API
dir_path
{
public:

  dir_path (const std::string& s = std::string (),
	    const std::string& d = std::string ())
    : p_orig (s), p_default (d), initialized (false)
    {
      if (! p_orig.empty ())
	init ();
    }

  dir_path (const dir_path& dp)
    : p_orig (dp.p_orig), p_default (dp.p_default),
      initialized (dp.initialized), p (dp.p), pv (dp.pv)
  { }

  dir_path& operator = (const dir_path& dp)
    {
      p_orig = dp.p_orig;
      p_default = dp.p_default;
      initialized = dp.initialized;
      p = dp.p;
      pv = dp.pv;
      return *this;
    }

  ~dir_path (void) { }

  void set (const std::string& s)
    {
      initialized = false;
      p_orig = s;
      init ();
    }

  string_vector elements (void);
  string_vector all_directories (void);

  std::string find_first (const std::string&);
  std::string find (const std::string& nm) { return find_first (nm); }

  string_vector find_all (const std::string&);

  std::string find_first_of (const string_vector& names);
  string_vector find_all_first_of (const string_vector& names);

  void rehash (void)
    {
      initialized = false;
      init ();
    }

  static bool is_path_sep (char c) { return c == path_sep_char; }

  static char path_sep_char;
  static std::string path_sep_str;

private:

  // The colon separated list that we were given.
  std::string p_orig;

  // The default path.  If specified, replaces leading, trailing, or
  // doubled colons in p_orig.
  std::string p_default;

  // TRUE means we've unpacked p.
  bool initialized;

  // A version of the colon separate list on which we have performed
  // tilde, variable, and possibly default path expansion.
  std::string p;

  // The elements of the list.
  string_vector pv;

  void init (void);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
