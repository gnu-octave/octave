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

#if defined (__GNUG__)
#pragma interface
#endif

#include <cstdlib>

#include <string>

class ostream;

#include "mx-base.h"
#include "oct-alloc.h"
#include "str-vec.h"

#include "Cell.h"
#include "error.h"
#include "ov-base.h"
#include "ov-typeinfo.h"

class Octave_map;
class octave_value_list;

class tree_walker;

// Cells.

class
octave_cell : public octave_base_value
{
public:

  octave_cell (void)
    : octave_base_value () { }

  octave_cell (const Cell& c)
    : octave_base_value (), cell_val (c) { }

  octave_cell (const octave_cell& c)
    : octave_base_value (), cell_val (c.cell_val) { }

  ~octave_cell (void) { }

  octave_value *clone (void) { return new octave_cell (*this); }

  octave_value do_index_op (const octave_value_list& idx);

  void assign (const octave_value_list& idx, const octave_value& rhs);

  bool is_defined (void) const { return true; }

  bool is_constant (void) const { return true; }

  void print (ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (ostream& os, bool pr_as_read_syntax = false) const;

  bool print_name_tag (ostream& os, const string& name) const;

private:

  Cell cell_val;

  DECLARE_OCTAVE_ALLOCATOR

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
