/*

Copyright (C) 2002 John W. Eaton

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

#if !defined (octave_base_list_h)
#define octave_base_list_h 1

#include <list>

template <typename elt_type>
class
octave_base_list
{
public:

  typedef typename std::list<elt_type>::iterator iterator;
  typedef typename std::list<elt_type>::const_iterator const_iterator;

  bool empty (void) const { return lst.empty (); }

  size_t length (void) const { return lst.size (); }

  iterator erase (iterator pos) { return lst.erase (pos); }

  template <class P>
  void remove_if (P pred) { lst.remove_if (pred); }

  void clear (void) { lst.clear (); }

  void append (const elt_type& s) { lst.push_back (s); }

  iterator begin (void) { return iterator (lst.begin ()); }
  const_iterator begin (void) const { return const_iterator (lst.begin ()); }

  iterator end (void) { return iterator (lst.end ()); }
  const_iterator end (void) const { return const_iterator (lst.end ()); }

  elt_type& front (void) { return lst.front (); }
  elt_type& back (void) { return lst.back (); }

  const elt_type& front (void) const { return lst.front (); }
  const elt_type& back (void) const { return lst.back (); }

private:

  std::list<elt_type> lst;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
