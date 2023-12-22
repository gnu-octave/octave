////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1999-2024 The Octave Project Developers
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

#if ! defined (octave_Cell_h)
#define octave_Cell_h 1

#include "octave-config.h"

#include <list>
#include <string>

#include "Array.h"
#include "str-vec.h"
#include "ov.h"

class octave_value_list;

class
OCTINTERP_API
Cell : public Array<octave_value>
{
public:

  Cell () = default;

  Cell (const Cell& c) = default;

  Cell& operator = (const Cell& c) = default;

  ~Cell () = default;

  Cell (const octave_value& val)
    : Array<octave_value> (dim_vector (1, 1), val) { }

  Cell (const octave_value_list& ovl);

  Cell (octave_idx_type n, octave_idx_type m,
        const octave_value& val = Matrix ())
    : Array<octave_value> (dim_vector (n, m), val) { }

  Cell (const dim_vector& dv, const octave_value& val = Matrix ())
    : Array<octave_value> (dv, val) { }

  Cell (const Array<octave_value>& c)
    : Array<octave_value> (c) { }

  Cell (const Array<octave_value>& c, octave_idx_type nr, octave_idx_type nc)
    : Array<octave_value> (c, dim_vector (nr, nc)) { }

  Cell (const string_vector& sv, bool trim = false);

  // Constructor for standard containers.  V must be convertible to an
  // octave_value object.
  template <typename V, template <typename...> class C>
  explicit
  Cell (const C<V>& container)
    : Array<octave_value> ()
  {
    std::size_t n = container.size ();

    if (n > 0)
      {
        resize (dim_vector (n, 1));

        octave_idx_type i = 0;

        for (const auto& val : container)
          elem(i++, 0) = val;
      }
  }

  Cell (const std::list<std::string>& sl);

  Cell (const Array<std::string>& sa);

  Cell (const dim_vector& dv, const string_vector& sv, bool trim = false);

  bool iscellstr () const;

  Array<std::string> cellstr_value () const;

  string_vector string_vector_value () const;

  using Array<octave_value>::index;

  Cell index (const octave_value_list& idx, bool resize_ok = false) const;

  using Array<octave_value>::delete_elements;

  void delete_elements (const octave_value_list& idx);

  using Array<octave_value>::assign;

  void assign (const octave_value_list& idx, const Cell& rhs,
               const octave_value& fill_val = Matrix ());

  Cell reshape (const dim_vector& new_dims) const
  { return Array<octave_value>::reshape (new_dims); }

  octave_idx_type nnz () const;

  Cell column (octave_idx_type i) const;

  // FIXME
  boolMatrix all (int /* dim */ = 0) const { return boolMatrix (); }

  // FIXME
  boolMatrix any (int /* dim */ = 0) const { return boolMatrix (); }

  Cell concat (const Cell& rb, const Array<octave_idx_type>& ra_idx);

  Cell& insert (const Cell& a, octave_idx_type r, octave_idx_type c);
  Cell& insert (const Cell& a, const Array<octave_idx_type>& ra_idx);

  // FIXME
  bool any_element_is_nan () const { return false; }
  bool is_true () const { return false; }

  octave_value resize_fill_value () const;

  Cell diag (octave_idx_type k = 0) const;

  Cell diag (octave_idx_type m, octave_idx_type n) const;

  Cell xisalnum () const { return map (&octave_value::xisalnum); }
  Cell xisalpha () const { return map (&octave_value::xisalpha); }
  Cell xisascii () const { return map (&octave_value::xisascii); }
  Cell xiscntrl () const { return map (&octave_value::xiscntrl); }
  Cell xisdigit () const { return map (&octave_value::xisdigit); }
  Cell xisgraph () const { return map (&octave_value::xisgraph); }
  Cell xislower () const { return map (&octave_value::xislower); }
  Cell xisprint () const { return map (&octave_value::xisprint); }
  Cell xispunct () const { return map (&octave_value::xispunct); }
  Cell xisspace () const { return map (&octave_value::xisspace); }
  Cell xisupper () const { return map (&octave_value::xisupper); }
  Cell xisxdigit () const { return map (&octave_value::xisxdigit); }
  Cell xtolower () const { return map (&octave_value::xtolower); }
  Cell xtoupper () const { return map (&octave_value::xtoupper); }

private:

  typedef octave_value (octave_value::*ctype_mapper) () const;

  Cell map (ctype_mapper) const;
};

template <>
inline Cell octave_value_extract<Cell> (const octave_value& v)
{ return v.cell_value (); }

#endif
