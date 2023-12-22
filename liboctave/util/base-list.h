////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2002-2024 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_base_list_h)
#define octave_base_list_h 1

#include "octave-config.h"

#include <cstdlib>

#include <list>

OCTAVE_BEGIN_NAMESPACE(octave)

template <typename elt_type>
class base_list
{
public:

  typedef typename std::list<elt_type>::iterator iterator;
  typedef typename std::list<elt_type>::const_iterator const_iterator;

  typedef typename std::list<elt_type>::reverse_iterator reverse_iterator;
  typedef typename std::list<elt_type>::const_reverse_iterator
    const_reverse_iterator;

  bool empty () const { return m_lst.empty (); }

  std::size_t size () const { return m_lst.size (); }
  std::size_t length () const { return size (); }

  iterator erase (iterator pos) { return m_lst.erase (pos); }

  template <typename P>
  void remove_if (P pred)
  {
    m_lst.remove_if (pred);
  }

  void clear () { m_lst.clear (); }

  iterator begin () { return iterator (m_lst.begin ()); }
  const_iterator begin () const { return const_iterator (m_lst.begin ()); }

  iterator end () { return iterator (m_lst.end ()); }
  const_iterator end () const { return const_iterator (m_lst.end ()); }

  reverse_iterator rbegin () { return reverse_iterator (m_lst.rbegin ()); }
  const_reverse_iterator rbegin () const
  { return const_reverse_iterator (m_lst.rbegin ()); }

  reverse_iterator rend () { return reverse_iterator (m_lst.rend ()); }
  const_reverse_iterator rend () const
  { return const_reverse_iterator (m_lst.rend ()); }

  elt_type& front () { return m_lst.front (); }
  elt_type& back () { return m_lst.back (); }

  const elt_type& front () const { return m_lst.front (); }
  const elt_type& back () const { return m_lst.back (); }

  void push_front (const elt_type& s) { m_lst.push_front (s); }
  void push_back (const elt_type& s) { m_lst.push_back (s); }

  void pop_front () { m_lst.pop_front (); }
  void pop_back () { m_lst.pop_back (); }

  // For backward compatibility.
  void append (const elt_type& s) { m_lst.push_back (s); }

  base_list () = default;

  base_list (const std::list<elt_type>& l) : m_lst (l) { }

  base_list (const base_list& bl) = default;

  base_list& operator = (const base_list& bl) = default;

  virtual ~base_list () = default;

protected:

  std::list<elt_type> m_lst;
};

OCTAVE_END_NAMESPACE(octave)

#endif
