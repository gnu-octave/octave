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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_char_matrix_str_h)
#define octave_char_matrix_str_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <cstdlib>

#include <string>

class ostream;

#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "ov.h"
#include "ov-ch-mat.h"
#include "ov-typeinfo.h"

class Octave_map;
class octave_value_list;

class tree_walker;

// Character matrix values with special properties for use as
// strings.

class
octave_char_matrix_str : public octave_char_matrix
{
public:

  octave_char_matrix_str (void)
    : octave_char_matrix () { }

  octave_char_matrix_str (const charMatrix& chm)
    : octave_char_matrix (chm) { }

  octave_char_matrix_str (char c)
    : octave_char_matrix (c) { }

  octave_char_matrix_str (const char *s)
    : octave_char_matrix (s) { }

  octave_char_matrix_str (const string& s)
    : octave_char_matrix (s) { }

  octave_char_matrix_str (const string_vector& s)
    : octave_char_matrix (s) { }

  octave_char_matrix_str (const octave_char_matrix& chm)
    : octave_char_matrix (chm) { }

  octave_char_matrix_str (const octave_char_matrix_str& chms)
    : octave_char_matrix (chms) { }

  ~octave_char_matrix_str (void) { }

  octave_value *clone (void) { return new octave_char_matrix_str (*this); }

  type_conv_fcn numeric_conversion_function (void) const;

  octave_value do_index_op (const octave_value_list& idx);

  void assign (const octave_value_list& idx, const charMatrix& rhs);

  bool is_string (void) const { return true; }

  bool is_real_type (void) const { return true; }

  bool is_matrix_type (void) const { return Vimplicit_str_to_num_ok; }

  bool is_numeric_type (void) const { return Vimplicit_str_to_num_ok; }

  bool valid_as_scalar_index (void) const;

  Matrix matrix_value (bool = false) const;

  string_vector all_strings (void) const;

  string string_value (void) const;

  bool print_as_scalar (void) const { return (rows () <= 1); }

  void print_raw (ostream& os, bool pr_as_read_syntax = false) const;

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
