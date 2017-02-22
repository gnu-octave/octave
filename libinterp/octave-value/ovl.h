/*

Copyright (C) 1994-2017 John W. Eaton
Copyright (C) 2009 VZLU Prague

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

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

  octave_value_list (void)
    : data (), names () { }

  explicit octave_value_list (octave_idx_type n)
    : data (dim_vector (1, n)), names () { }

  octave_value_list (octave_idx_type n, const octave_value& val)
    : data (dim_vector (1, n), val), names () { }

  octave_value_list (const octave_value& tc)
    : data (dim_vector (1, 1), tc), names () { }

  template<template <typename...> class OV_Container>
  octave_value_list (const OV_Container<octave_value>& args)
    : data (args, dim_vector (1, args.size ())), names () { }

  octave_value_list (const Array<octave_value>& d)
    : data (d.as_row ()), names () { }

  octave_value_list (const Cell& tc)
    : data (tc.as_row ()), names () { }

  octave_value_list (const octave_value_list& obj)
    : data (obj.data), names (obj.names) { }

  // Concatenation constructor.
  octave_value_list (const std::list<octave_value_list>&);

  ~octave_value_list (void) = default;

  octave_value_list& operator = (const octave_value_list& obj)
  {
    if (this != &obj)
      {
        data = obj.data;
        names = obj.names;
      }

    return *this;
  }

  Array<octave_value> array_value (void) const { return data; }

  Cell cell_value (void) const { return array_value (); }

  // Assignment will resize on range errors.

  octave_value& operator () (octave_idx_type n) { return elem (n); }

  const octave_value& operator () (octave_idx_type n) const { return elem (n); }

  octave_idx_type length (void) const { return data.numel (); }

  bool empty (void) const { return length () == 0; }

  void resize (octave_idx_type n, const octave_value& rfv = octave_value ())
  {
    data.resize (dim_vector (1, n), rfv);
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
    // linear_slice handles the case of len < 0.

    octave_value_list retval
      = data.linear_slice (offset, std::min (offset + len, length ()));

    if (tags && len > 0 && names.numel () > 0)
      retval.names = names.linear_slice (offset, std::min (offset + len,
                                                           names.numel ()));

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

  void stash_name_tags (const string_vector& nm) { names = nm; }

  string_vector name_tags (void) const { return names; }

  void make_storable_values (void);

  octave_value& xelem (octave_idx_type i) { return data.xelem (i); }

  void clear (void) { data.clear (); }

private:

  Array<octave_value> data;

  // This list of strings can be used to tag each element of data with a name.
  // By default, it is empty.
  string_vector names;

  octave_value& elem (octave_idx_type n)
  {
    if (n >= length ())
      resize (n + 1);

    return data(n);
  }

  const octave_value& elem (octave_idx_type n) const
  { return data(n); }

};


//! Construct an octave_value_list with less typing.
/*!
  Historically, this made it easier to create an octave_value_list
  from multiple octave_value arguments.  It is no longer useful since
  octave_value_list has now a constructor accepting an initializer_list
  so all it does is save some typing.  The following are equivalent:

  @code{.cc}
  return octave_value_list ({ov0, ov1, ov2});
  return ovl (ov0, ov1, ov2);
  @endcode
*/
template<typename... OV_Args>
inline octave_value_list
ovl (const OV_Args&... args)
{
  return octave_value_list (std::initializer_list<octave_value> ({args...}));
}

#endif
