/*

Copyright (C) 1999, 2000, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
              John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_cell_h)
#define octave_cell_h 1

#include <cstdlib>

#include <iosfwd>
#include <string>

#include "mx-base.h"
#include "oct-alloc.h"
#include "str-vec.h"

#include "Cell.h"
#include "error.h"
#include "ov-base-mat.h"
#include "ov-typeinfo.h"

class Octave_map;
class octave_value_list;

class tree_walker;

// Cells.

class
octave_cell : public octave_base_matrix<Cell>
{
public:

  octave_cell (void)
    : octave_base_matrix<Cell> () { }

  octave_cell (const Cell& c)
    : octave_base_matrix<Cell> (c) { }

  octave_cell (const Array<std::string>& str)
    : octave_base_matrix<Cell> (Cell (str)), cellstr_cache (str) { }

  octave_cell (const octave_cell& c)
    : octave_base_matrix<Cell> (c) { }

  ~octave_cell (void) { }

  octave_base_value *clone (void) const { return new octave_cell (*this); }
  octave_base_value *empty_clone (void) const { return new octave_cell (); }

#if 0
  octave_base_value *try_narrowing_conversion (void);
#endif

  octave_value subsref (const std::string& type,
			const std::list<octave_value_list>& idx)
    {
      octave_value_list tmp = subsref (type, idx, 1);
      return tmp.length () > 0 ? tmp(0) : octave_value ();
    }

  octave_value_list subsref (const std::string& type,
			     const std::list<octave_value_list>& idx, int);

  octave_value subsref (const std::string& type,
			const std::list<octave_value_list>& idx,
                        bool auto_add);

  octave_value subsasgn (const std::string& type,
			 const std::list<octave_value_list>& idx,
			 const octave_value& rhs);

  void assign (const octave_value_list& idx, const Cell& rhs);

  void assign (const octave_value_list& idx, const octave_value& rhs);

  void delete_elements (const octave_value_list& idx);

  size_t byte_size (void) const;

  octave_value sort (octave_idx_type dim = 0, sortmode mode = ASCENDING) const;

  octave_value sort (Array<octave_idx_type> &sidx, octave_idx_type dim = 0,
		     sortmode mode = ASCENDING) const;

  sortmode is_sorted (sortmode mode = UNSORTED) const;

  Array<octave_idx_type> sort_rows_idx (sortmode mode = ASCENDING) const;

  sortmode is_sorted_rows (sortmode mode = UNSORTED) const;

  bool is_matrix_type (void) const { return false; }

  bool is_numeric_type (void) const { return false; }

  bool is_defined (void) const { return true; }

  bool is_constant (void) const { return true; }

  bool is_cell (void) const { return true; }

  bool is_cellstr (void) const;

  bool is_true (void) const;

  Cell cell_value (void) const { return matrix; }

  octave_value_list list_value (void) const;

  octave_value convert_to_str_internal (bool pad, bool, char type) const
    { return octave_value (all_strings (pad), type); }

  string_vector all_strings (bool pad = false) const;

  Array<std::string> cellstr_value (void) const;

  bool print_as_scalar (void) const;

  void print (std::ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;


  bool save_ascii (std::ostream& os);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool& save_as_floats);

  bool load_binary (std::istream& is, bool swap, 
		    oct_mach_info::float_format fmt);

#if defined (HAVE_HDF5)
  bool save_hdf5 (hid_t loc_id, const char *name, bool save_as_floats);

  bool load_hdf5 (hid_t loc_id, const char *name, bool have_h5giterate_bug);
#endif

  octave_value xisalnum (void) const { return matrix.xisalnum (); }
  octave_value xisalpha (void) const { return matrix.xisalpha (); }
  octave_value xisascii (void) const { return matrix.xisascii (); }
  octave_value xiscntrl (void) const { return matrix.xiscntrl (); }
  octave_value xisdigit (void) const { return matrix.xisdigit (); }
  octave_value xisgraph (void) const { return matrix.xisgraph (); }
  octave_value xislower (void) const { return matrix.xislower (); }
  octave_value xisprint (void) const { return matrix.xisprint (); }
  octave_value xispunct (void) const { return matrix.xispunct (); }
  octave_value xisspace (void) const { return matrix.xisspace (); }
  octave_value xisupper (void) const { return matrix.xisupper (); }
  octave_value xisxdigit (void) const { return matrix.xisxdigit (); }
  octave_value xtoascii (void) const { return matrix.xtoascii (); }
  octave_value xtolower (void) const { return matrix.xtolower (); }
  octave_value xtoupper (void) const { return matrix.xtoupper (); }

  mxArray *as_mxArray (void) const;

  // Unsafe.  This function exists to support the MEX interface.
  // You should not use it anywhere else.
  void *mex_get_data (void) const; 

private:

  void clear_cellstr_cache (void) const;

  mutable Array<std::string> cellstr_cache;

  void make_cellstr_cache (void) const;

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
