/*

Copyright (C) 2000 John W. Eaton

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

#if !defined (octave_base_nd_array_h)
#define octave_base_nd_array_h 1

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include <cstdlib>

#include <iostream>
#include <string>

#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "ov-base.h"
#include "ov-typeinfo.h"

class Octave_map;
class octave_value_list;

class tree_walker;

// ND array values values.

template <class AT>
class
octave_base_nd_array : public octave_base_value
{
public:

  octave_base_nd_array (void)
    : octave_base_value () { }

  octave_base_nd_array (const AT& a)
    : octave_base_value (), array (a) { }

  octave_base_nd_array (const octave_base_nd_array& a)
    : octave_base_value (), array (a.array) { }

  ~octave_base_nd_array (void) { }

  octave_value *clone (void) const { return new octave_base_nd_array (*this); }
  octave_value *empty_clone (void) const { return new octave_base_nd_array (); }

  octave_value do_index_op (const octave_value_list& idx, int resize_ok);

  bool is_matrix_type (void) const { return false; }

  bool is_numeric_type (void) const { return true; }

  bool is_defined (void) const { return true; }

  bool is_constant (void) const { return true; }

  bool is_true (void) const;

  virtual bool print_as_scalar (void) const;

  void print (std::ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  bool print_name_tag (std::ostream& os, const std::string& name) const;

protected:

  AT array;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
