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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "ov-typeinfo.h"

#include "defun.h"
#include "error.h"
#include "help.h"
#include "oct-obj.h"

const int
octave_value_typeinfo::init_tab_sz (16);

octave_value_typeinfo *
octave_value_typeinfo::instance (0);

#include <Array.cc>
#include <Array2.cc>
#include <Array3.cc>

template class Array<binary_op_fcn>;
template class Array2<binary_op_fcn>;
template class Array3<binary_op_fcn>;

template class Array<assign_op_fcn>;
template class Array2<assign_op_fcn>;
template class Array3<assign_op_fcn>;

template class Array<type_conv_fcn>;
template class Array2<type_conv_fcn>;

int
octave_value_typeinfo::register_type (const string& name)
{
  if (! instance)
    instance = new octave_value_typeinfo ();

  return instance->do_register_type (name);
}

bool
octave_value_typeinfo::register_binary_op (octave_value::binary_op op,
					   int t1, int t2,
					   binary_op_fcn f)
{
  if (! instance)
    instance = new octave_value_typeinfo ();

  return instance->do_register_binary_op (op, t1, t2, f);
}

bool
octave_value_typeinfo::register_assign_op (octave_value::assign_op op,
					   int t_lhs, int t_rhs,
					   assign_op_fcn f)
{
  if (! instance)
    instance = new octave_value_typeinfo ();

  return instance->do_register_assign_op (op, t_lhs, t_rhs, f);
}

bool
octave_value_typeinfo::register_pref_assign_conv (int t_lhs, int t_rhs,
						  int t_result) 
{
  if (! instance)
    instance = new octave_value_typeinfo ();

  return instance->do_register_pref_assign_conv (t_lhs, t_rhs, t_result);
}

bool
octave_value_typeinfo::register_widening_op (int t, int t_result,
					     type_conv_fcn f)
{
  if (! instance)
    instance = new octave_value_typeinfo ();

  return instance->do_register_widening_op (t, t_result, f);
}

int
octave_value_typeinfo::do_register_type (const string& name)
{
  int i = 0;

  for (i = 0; i < num_types; i++)
    if (name == types (i))
      return i;

  int len = types.length ();

  if (i == len)
    {
      len *= 2;

      types.resize (len, string ());

      binary_ops.resize (static_cast<int> (octave_value::num_binary_ops),
			 len, len, static_cast<binary_op_fcn> (0));

      assign_ops.resize (static_cast<int> (octave_value::num_assign_ops),
			 len, len, static_cast<assign_op_fcn> (0));

      pref_assign_conv.resize (len, len, -1);

      widening_ops.resize (len, len, static_cast<type_conv_fcn> (0));
    }

  types (i) = name;

  num_types++;

  return i;
}

bool
octave_value_typeinfo::do_register_binary_op (octave_value::binary_op op,
					      int t1, int t2,
					      binary_op_fcn f)
{
  binary_ops.checkelem (static_cast<int> (op), t1, t2) = f;

  return false;
}

bool
octave_value_typeinfo::do_register_assign_op (octave_value::assign_op op,
					      int t_lhs, int t_rhs,
					      assign_op_fcn f)
{
  assign_ops.checkelem (static_cast<int> (op), t_lhs, t_rhs) = f;

  return false;
}

bool
octave_value_typeinfo::do_register_pref_assign_conv (int t_lhs, int t_rhs,
						     int t_result) 
{
  pref_assign_conv.checkelem (t_lhs, t_rhs) = t_result;

  return false;
}

bool
octave_value_typeinfo::do_register_widening_op
  (int t, int t_result, type_conv_fcn f)
{
  widening_ops.checkelem (t, t_result) = f;

  return false;
}

#include <iostream.h>

binary_op_fcn
octave_value_typeinfo::do_lookup_binary_op (octave_value::binary_op op,
					    int t1, int t2)
{
  return binary_ops.checkelem (static_cast<int> (op), t1, t2);
}

assign_op_fcn
octave_value_typeinfo::do_lookup_assign_op (octave_value::assign_op op,
					    int t_lhs, int t_rhs)
{
  return assign_ops.checkelem (static_cast<int> (op), t_lhs, t_rhs);
}

int
octave_value_typeinfo::do_lookup_pref_assign_conv (int t_lhs, int t_rhs)
{
  return pref_assign_conv.checkelem (t_lhs, t_rhs);
}

type_conv_fcn
octave_value_typeinfo::do_lookup_widening_op (int t, int t_result)
{
  return widening_ops.checkelem (t, t_result);
}

string_vector
octave_value_typeinfo::do_installed_type_names (void)
{
  string_vector retval (num_types);

  for (int i = 0;i < num_types; i++)
    retval (i) = types (i);

  return retval;
}

DEFUN (typeinfo, args, ,
  "usage: typeinfo (expr)\n\
\n\
Return the type of the expression EXPR, as a string.  If EXPR is\n\
omitted, return an array of strings containing all the currently\n\
installed data types.")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    retval = octave_value_typeinfo::installed_type_names ();
  else if (nargin == 1)
    retval = args(0).type_name ();
  else
    print_usage ("typeinfo");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
