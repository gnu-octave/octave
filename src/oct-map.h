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

#include <map>

#include "Cell.h"
#include "oct-obj.h"

class string_vector;

class
Octave_map
{
 public:

  typedef std::map<std::string, Cell>::iterator iterator;
  typedef std::map<std::string, Cell>::const_iterator const_iterator;

  Octave_map (void) : map (), array_len (0) { }

  Octave_map (const std::string& key, const octave_value& value)
    : map (), array_len (1)
      {
	map[key] = Cell (value);
      }

  Octave_map (const std::string& key, const Cell& vals)
    : map (), array_len (vals.length ())
      {
	map[key] = vals;
      }

  Octave_map (const std::string& key, const octave_value_list& val_list)
    : map (), array_len (val_list.length ())
      {
	map[key] = val_list;
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
  int length (void) const { return map.size (); }

  int empty (void) const { return map.empty (); }

  Cell& operator [] (const std::string& key) { return map[key]; }

  Cell operator [] (const std::string& key) const;

  void del (const std::string& key)
    {
      iterator p = map.find (key);
      if (p != map.end ())
	map.erase (p);
    }

  iterator begin (void) { return iterator (map.begin ()); }
  const_iterator begin (void) const { return const_iterator (map.begin ()); }

  iterator end (void) { return iterator (map.end ()); }
  const_iterator end (void) const { return const_iterator (map.end ()); }

  std::string key (const_iterator p) const { return p->first; }

  Cell& contents (const_iterator p)
    { return operator [] (key(p)); }

  Cell contents (const_iterator p) const
    { return operator [] (key(p)); }

  const_iterator seek (const std::string& key) const { return map.find (key); }

  int contains (const std::string& key) const
    { return (seek (key) != map.end ()); }

  void clear (void) { map.clear (); }

  string_vector keys (void) const;

  int rows (void) const { return 1; }

  int columns (void) const { return array_length (); }

  int array_length (void) const;

  Octave_map& assign (const octave_value_list& idx, const Octave_map& rhs);

  Octave_map& assign (const octave_value_list& idx, const std::string& key,
		      const Cell& rhs);

  Octave_map& assign (const std::string& key, const Cell& rhs);

  Octave_map index (const octave_value_list& idx);

private:

  // The map of names to values.
  std::map<std::string, Cell> map;

  // The current size of this struct array;
  mutable int array_len;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
