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

#if !defined (octave_oct_map_h)
#define octave_oct_map_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include "Map.h"

#include "oct-obj.h"

class string_vector;

class
Octave_map
{
 public:
  Octave_map (void) : map (octave_value_list ()) { }

  Octave_map (const std::string& key, const octave_value& value)
    : map (octave_value_list ())
      {
	map[key] = octave_value_list (value);
      }

  Octave_map (const Octave_map& m) : map (m.map) { }

  Octave_map& operator = (const Octave_map& m)
    {
      if (this != &m)
	map = m.map;

      return *this;
    }

  ~Octave_map (void) { }

  int length (void) const { return map.length (); }

  int empty (void) const { return map.empty (); }

  octave_value& operator [] (const std::string& key) { return map[key](0); }

  void del (const std::string& key) { map.del (key); }

  Pix first (void) const { return map.first (); }
  void next (Pix& i) const { map.next (i); }

  std::string key (Pix p) const { return map.key (p); }

  octave_value& contents (Pix p) const { return map.contents (p)(0); }

  Pix seek (const std::string& key) const { return map.seek (key); }

  int contains (const std::string& key) const { return map.contains (key); }

  void clear (void) { map.clear (); }

  string_vector make_name_list (void);

private:

  // The map of names to values.
  CHMap<octave_value_list> map;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
