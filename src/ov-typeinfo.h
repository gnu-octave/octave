/*

Copyright (C) 1996 John W. Eaton

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

#if !defined (octave_value_typeinfo_h)
#define octave_value_typeinfo_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <string>

#include "Array.h"
#include "Array2.h"
#include "Array3.h"

#include "ov.h"

class string_vector;

class
octave_value_typeinfo
{
public:

  static int register_type (const string&);

  static bool register_binary_op (octave_value::binary_op, int, int,
				  binary_op_fcn);

  static bool register_assign_op (int, int, assign_op_fcn);

  static bool register_pref_assign_conv (int, int, int);

  static bool register_widening_op (int, int, type_conv_fcn);

  static binary_op_fcn
  lookup_binary_op (octave_value::binary_op op, int t1, int t2)
  {
    return instance->do_lookup_binary_op (op, t1, t2);
  }

  static assign_op_fcn
  lookup_assign_op (int t_lhs, int t_rhs)
  {
    return instance->do_lookup_assign_op (t_lhs, t_rhs);
  }

  static int
  lookup_pref_assign_conv (int t_lhs, int t_rhs)
  {
    return instance->do_lookup_pref_assign_conv (t_lhs, t_rhs);
  }

  static type_conv_fcn
  lookup_widening_op (int t, int t_result)
  {
    return instance->do_lookup_widening_op (t, t_result);
  }

  static string_vector installed_type_names (void)
  {
    return instance->do_installed_type_names ();
  }

protected:

  octave_value_typeinfo (void)
    : num_types (0), types (init_tab_sz, string ()),
      binary_ops (octave_value::num_binary_ops, init_tab_sz,
		  init_tab_sz, (binary_op_fcn) 0),
      assign_ops (init_tab_sz, init_tab_sz, (assign_op_fcn) 0),
      pref_assign_conv (init_tab_sz, init_tab_sz, -1),
      widening_ops (init_tab_sz, init_tab_sz, (type_conv_fcn) 0)  { }

private:

  static const int init_tab_sz;

  static octave_value_typeinfo *instance;

  int num_types;

  Array<string> types;

  Array3<binary_op_fcn> binary_ops;

  Array2<assign_op_fcn> assign_ops;

  Array2<int> pref_assign_conv;

  Array2<type_conv_fcn> widening_ops;

  int do_register_type (const string&);

  bool do_register_binary_op (octave_value::binary_op, int, int,
			      binary_op_fcn);

  bool do_register_assign_op (int, int, assign_op_fcn);

  bool do_register_pref_assign_conv (int, int, int);

  bool do_register_widening_op (int, int, type_conv_fcn);

  binary_op_fcn
  do_lookup_binary_op (octave_value::binary_op, int, int);

  assign_op_fcn do_lookup_assign_op (int, int);

  int do_lookup_pref_assign_conv (int, int);

  type_conv_fcn do_lookup_widening_op (int, int);

  string_vector do_installed_type_names (void);

  // No copying!

  octave_value_typeinfo (const octave_value_typeinfo&);

  octave_value_typeinfo& operator = (const octave_value_typeinfo&);
};

#endif

/*
;; Local Variables: ***
;; mode: C++ ***
;; End: ***
*/
