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

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include "Map.h"

#include "oct-obj.h"

class string_vector;

class
Octave_map
{
 public:
  Octave_map (void) : map (octave_value_list ()), array_len (0) { }

  Octave_map (const std::string& key, const octave_value& value)
    : map (octave_value_list ()), array_len (1)
      {
	map[key] = octave_value_list (value);
      }

  Octave_map (const Octave_map& m)
    : map (m.map), array_len (m.array_len) { }

  Octave_map& operator = (const Octave_map& m)
    {
      if (this != &m)
	{
	  map = m.map;
	  array_len = m.array_len;
	}
      return *this;
    }

  ~Octave_map (void) { }

  // This is the number of keys.
  int length (void) const { return map.length (); }

  int empty (void) const { return map.empty (); }

  octave_value_list& operator [] (const std::string& key) { return map[key]; }

  void del (const std::string& key) { map.del (key); }

  Pix first (void) const { return map.first (); }
  void next (Pix& i) const { map.next (i); }

  std::string key (Pix p) const { return map.key (p); }

  octave_value_list& contents (Pix p) const { return map.contents (p); }

  Pix seek (const std::string& key) const { return map.seek (key); }

  int contains (const std::string& key) const { return map.contains (key); }

  void clear (void) { map.clear (); }

  string_vector keys (void) const;

  int array_length () const;

  Octave_map& assign (const idx_vector& idx, const std::string& key,
		      const octave_value_list& rhs);

  Octave_map& assign (const std::string& key, const octave_value_list& rhs);

  Octave_map index (idx_vector& idx);

private:

  // The map of names to values.
  CHMap<octave_value_list> map;

  // The current size of this struct array;
  mutable int array_len;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
