// pathsearch.h                                           -*- C++ -*-
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

#if !defined (octave_pathsearch_h)
#define octave_pathsearch_h 1

#include <string>

#include "str-vec.h"

class
dir_path
{
public:

  dir_path (const string& s = string ()) : p (s), initialized (false)
    {
      if (! p.empty ())
	init ();
    }

  dir_path (const dir_path& dp)
    : p (dp.p), initialized (dp.initialized), pv (dp.pv) { }

  dir_path& operator = (const dir_path& dp)
    {
      p = dp.p;
      initialized = dp.initialized;
      pv = dp.pv;
      return *this;
    }

  ~dir_path (void) { }

  void set (const string& s)
    {
      initialized = false;
      p = s;
      init ();
    }

  string_vector elements (void);
  string_vector all_directories (void);

  string find_first (const string&);
  string find (const string& nm) { return find_first (nm); }

  string_vector find_all (const string&);

private:

  // The colon separated list.
  string p;

  // TRUE means we've unpacked p.
  bool initialized;

  // The elements of the list.
  string_vector pv;

  // TRUE means we've set the global value of kpathsea_debug.
  static bool kpathsea_debug_initialized;

  void init (void);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
