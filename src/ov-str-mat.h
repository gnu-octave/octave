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

  bool valid_as_scalar_index (void) const;
  bool valid_as_zero_index (void) const;

  Matrix matrix_value (bool = false) const;

  string_vector all_strings (void) const;

  string string_value (void) const;

  octave_value transpose (void) const
    { return octave_value (matrix.transpose (), true); }

  octave_value hermitian (void) const
    { return octave_value (matrix.transpose (), true); }

  void print (ostream& os, bool pr_as_read_syntax = false) const;

  void print_raw (ostream& os, bool pr_as_read_syntax = false) const;

  bool print_name_tag (ostream& os, const string& name) const;

  int type_id (void) const { return t_id; }

  string type_name (void) const { return t_name; }

  static int static_type_id (void) { return t_id; }

  static void register_type (void)
    { t_id = octave_value_typeinfo::register_type (t_name); }

private:

  // Type id of char_matrix_str objects, set by register_type().
  static int t_id;

  // Type name of char_matrix_strXX objects, defined in ov-str-mat.cc.
  static const string t_name;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
