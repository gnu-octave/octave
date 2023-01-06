////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1994-2023 The Octave Project Developers
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

#if ! defined (octave_ovl_h)
#define octave_ovl_h 1

#include "octave-config.h"

#include <string>
#include <vector>
#include <initializer_list>

#include "str-vec.h"
#include "Array.h"

#include "ov.h"
#include "Cell.h"

class
OCTINTERP_API
octave_value_list
{
public:

  octave_value_list (void) = default;

  explicit octave_value_list (octave_idx_type n)
    : m_data (n), m_names () { }

  octave_value_list (octave_idx_type n, const octave_value& val)
    : m_data (n, val), m_names () { }

  octave_value_list (const octave_value& tc)
    : m_data (1, tc), m_names () { }

  template<template <typename...> class OV_Container>
  octave_value_list (const OV_Container<octave_value>& args)
    : m_data (args.begin (), args.end ()), m_names () { }

  octave_value_list (const Array<octave_value>& a)
    : m_data (a.numel ()), m_names ()
  {
    for (octave_idx_type i = 0; i < a.numel (); i++)
      m_data[i] = a(i);
  }

  octave_value_list (const Cell& c)
    : m_data (c.numel ()), m_names ()
  {
    for (octave_idx_type i = 0; i < c.numel (); i++)
      m_data[i] = c(i);
  }

  octave_value_list (const octave_value_list& obj) = default;

  octave_value_list (octave_value_list&& obj) = default;

  // Concatenation constructors.
  octave_value_list (const std::list<octave_value>&);
  octave_value_list (const std::list<octave_value_list>&);

  ~octave_value_list (void) = default;

  octave_value_list& operator = (const octave_value_list& obj) = default;

  octave_value_list& operator = (octave_value_list&& obj) = default;

  Array<octave_value> array_value (void) const
  {
    Array<octave_value> retval;

    if (! m_data.empty ())
      {
        retval.resize (dim_vector (1, length ()));

        for (octave_idx_type i = 0; i < retval.numel (); i++)
          retval.xelem (i) = m_data[i];
      }

    return retval;
  }

  Cell cell_value (void) const { return array_value (); }

  // Assignment will resize on range errors.

  octave_value& operator () (octave_idx_type n) { return elem (n); }

  const octave_value& operator () (octave_idx_type n) const { return elem (n); }

  octave_idx_type length (void) const { return m_data.size (); }

  bool empty (void) const { return length () == 0; }

  void resize (octave_idx_type n, const octave_value& rfv = octave_value ())
  {
    m_data.resize (n, rfv);
  }

  octave_value_list& prepend (const octave_value& val);

  octave_value_list& append (const octave_value& val);

  octave_value_list& append (const octave_value_list& lst);

  octave_value_list& reverse (void);

  octave_value_list
  slice (octave_idx_type offset, octave_idx_type len, bool tags = false) const
  {
    // linear_slice uses begin/end indices instead of offset and length.
    // Avoid calling with upper bound out of range.

    octave_idx_type tlen = len > 0 ? len : 0;
    std::vector<octave_value> slice_data (tlen);
    auto beg = m_data.begin () + offset;
    auto end = beg + len;
    std::copy (beg, end, slice_data.begin ());

    octave_value_list retval = slice_data;

    if (tags && len > 0 && m_names.numel () > 0)
      retval.m_names = m_names.linear_slice (offset, std::min (offset + len,
                                             m_names.numel ()));

    return retval;
  }

  octave_value_list
  splice (octave_idx_type offset, octave_idx_type len,
          const octave_value_list& lst = octave_value_list ()) const;

  bool all_strings_p (void) const;

  bool all_scalars (void) const;

  bool any_cell (void) const;

  bool has_magic_colon (void) const;

  string_vector make_argv (const std::string& = "") const;

  void stash_name_tags (const string_vector& nm) { m_names = nm; }

  string_vector name_tags (void) const { return m_names; }

  void make_storable_values (void);

  octave_value& xelem (octave_idx_type i) { return m_data[i]; }

  void clear (void) { m_data.clear (); }

private:

  std::vector<octave_value> m_data;

  // The list of strings can be used to tag each element of m_data with a name.
  // By default, it is empty.
  string_vector m_names;

  // elem will automatically resize array for out-of-bounds requests.
  octave_value& elem (octave_idx_type n)
  {
    if (n >= length ())
      resize (n + 1);

    return m_data[n];
  }

  const octave_value& elem (octave_idx_type n) const { return m_data[n]; }

};


//! Construct an octave_value_list with less typing.
//!
//! Historically, this made it easier to create an octave_value_list
//! from multiple octave_value arguments.  It is no longer useful since
//! octave_value_list has now a constructor accepting an initializer_list
//! so all it does is save some typing.  The following are equivalent:
//!
//! @code{.cc}
//! return octave_value_list ({ov0, ov1, ov2});
//! return ovl (ov0, ov1, ov2);
//! @endcode

template<typename... OV_Args>
inline octave_value_list
ovl (const OV_Args& ... args)
{
  return octave_value_list (std::initializer_list<octave_value> ({args...}));
}

#endif
