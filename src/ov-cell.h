/*

Copyright (C) 1999 John W. Eaton

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

#if !defined (octave_cell_h)
#define octave_cell_h 1

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include <cstdlib>

#include <iostream>
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

  octave_cell (const octave_cell& c)
    : octave_base_matrix<Cell> (c) { }

  ~octave_cell (void) { }

  void assign (const octave_value_list& idx, const octave_value& rhs);

  octave_value *clone (void) const { return new octave_cell (*this); }
  octave_value *empty_clone (void) const { return new octave_cell (); }

#if 0
  octave_value *try_narrowing_conversion (void);
#endif

  octave_value subsref (const std::string& type,
			const std::list<octave_value_list>& idx);

  octave_value_list subsref (const std::string& type,
			     const std::list<octave_value_list>& idx,
    			     int nargout)
    {
      panic_impossible ();
      return octave_value_list ();
    }

  octave_value subsasgn (const std::string& type,
			 const std::list<octave_value_list>& idx,
			 const octave_value& rhs);

  bool is_matrix_type (void) const { return false; }

  bool is_numeric_type (void) const { return false; }

  bool is_defined (void) const { return true; }

  bool is_cell (void) const { return true; }

  Cell cell_value (void) const { return matrix; }

  octave_value_list list_value (void) const;

  octave_value convert_to_str_internal (bool pad, bool force) const
    { return all_strings (pad, force); }

  string_vector all_strings (bool pad = false, bool force = false) const;

  bool print_as_scalar (void) const;

  void print (std::ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

private:

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
