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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_struct_h)
#define octave_struct_h 1

#include <cstdlib>

#include <iostream>
#include <string>

#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "oct-alloc.h"
#include "oct-map.h"
#include "ov-base.h"
#include "ov-typeinfo.h"

class octave_value_list;

class tree_walker;

// Data structures.

class
octave_struct : public octave_base_value
{
public:

  octave_struct (void)
    : octave_base_value () { }

  octave_struct (const Octave_map& m)
    : octave_base_value (), map (m) { }

  octave_struct (const octave_struct& s)
    : octave_base_value (), map (s.map) { }

  ~octave_struct (void) { }

  octave_base_value *clone (void) const { return new octave_struct (*this); }
  octave_base_value *empty_clone (void) const { return new octave_struct (); }

  Cell dotref (const octave_value_list& idx);

  octave_value subsref (const std::string&,
			const std::list<octave_value_list>&)
    {
      panic_impossible ();
      return octave_value_list ();
    }

  octave_value_list subsref (const std::string& type,
			     const std::list<octave_value_list>& idx,
			     int nargout);

  static octave_value numeric_conv (const Cell& val,
				    const std::string& type);

  octave_value subsasgn (const std::string& type,
			 const std::list<octave_value_list>& idx,
			 const octave_value& rhs);

  dim_vector dims (void) const { return map.dims (); }

  size_t byte_size (void) const;

  // This is the number of elements in each field.  The total number
  // of elements is numel () * nfields ().
  octave_idx_type numel (void) const
  {
    dim_vector dv = dims ();
    return dv.numel ();
  }

  octave_idx_type nfields (void) const { return map.length (); }

  octave_value reshape (const dim_vector& new_dims) const
    { return map.reshape (new_dims); }

  octave_value resize (const dim_vector& dv, bool = false) const
    { Octave_map tmap = map; tmap.resize (dv); return tmap; }

  bool is_defined (void) const { return true; }

  bool is_map (void) const { return true; }

  Octave_map map_value (void) const { return map; }

  string_vector map_keys (void) const { return map.keys (); }

  void print (std::ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  bool print_name_tag (std::ostream& os, const std::string& name) const;

  bool save_ascii (std::ostream& os, bool& infnan_warned);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool& save_as_floats);

  bool load_binary (std::istream& is, bool swap, 
		    oct_mach_info::float_format fmt);

#if defined (HAVE_HDF5)
  bool save_hdf5 (hid_t loc_id, const char *name, bool save_as_floats);

  bool load_hdf5 (hid_t loc_id, const char *name, bool have_h5giterate_bug);
#endif

  mxArray *as_mxArray (void) const;

private:

  // The associative array used to manage the structure data.
  Octave_map map;

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
