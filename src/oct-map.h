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

#if !defined (octave_oct_map_h)
#define octave_oct_map_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include "Map.h"

#include "ov.h"

class string_vector;

class
Octave_map : public CHMap<octave_value>
{
 public:
  Octave_map (void) : CHMap<octave_value> (octave_value ()) { }

  Octave_map (const string& key, const octave_value& value)
    : CHMap<octave_value> (octave_value ())
      {
	CHMap<octave_value>::operator [] (key) = value;
      }

  Octave_map (const Octave_map& m) : CHMap<octave_value> (m) { }

  ~Octave_map (void) { }

  string_vector make_name_list (void);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
