// oct-map.h                                            -*- C -*-
/*

Copyright (C) 1994, 1995 John W. Eaton

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

#if !defined (octave_oct_map_h)
#define octave_oct_map_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include "Map.h"

#include "pt-const.h"

class string_vector;

class
Octave_map : public CHMap<tree_constant>
{
 public:
  Octave_map (void) : CHMap<tree_constant> (tree_constant ()) { }

  Octave_map (const string& key, const tree_constant& value)
    : CHMap<tree_constant> (tree_constant ())
      {
	CHMap<tree_constant>::operator [] (key) = value;
      }

  Octave_map (const Octave_map& m) : CHMap<tree_constant> (m) { }

  ~Octave_map (void) { }

  string_vector make_name_list (void);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
