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

#if !defined (octave_mapper_h)
#define octave_mapper_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include <string>

#include "ov-fcn.h"
#include "ov-typeinfo.h"

class octave_value;
class octave_value_list;

// Builtin mapper functions.

class
octave_mapper : public octave_function
{
public:

  typedef int (*ch_mapper) (int);
  typedef double (*d_d_mapper) (double);
  typedef double (*d_c_mapper) (const Complex&);
  typedef Complex (*c_c_mapper) (const Complex&);

  octave_mapper (ch_mapper ch, d_d_mapper dd, d_c_mapper dc,
		 c_c_mapper cc, double ll, double ul, int f,
		 const string& nm = string (),
		 const string& ds = string ())
    : octave_function (nm, ds), ch_map_fcn (ch), d_d_map_fcn (dd),
      d_c_map_fcn (dc), c_c_map_fcn (cc),
      lower_limit (ll), upper_limit (ul), flag (f) { }

  ~octave_mapper (void) { }

  void *operator new (size_t size)
    { return allocator.alloc (size); }

  void operator delete (void *p, size_t size)
    { allocator.free (p, size); }

  octave_function *function_value (bool) { return this; }

  octave_value_list do_index_op (int nargout, const octave_value_list& args);

  int type_id (void) const { return t_id; }

  string type_name (void) const { return t_name; }

  static int static_type_id (void) { return t_id; }

  static void register_type (void)
    { t_id = octave_value_typeinfo::register_type (t_name); }

private:

  octave_mapper (void);

  octave_mapper (const octave_mapper& m);

  octave_value apply (const octave_value& arg) const;

  // ch_map_fcn is a kluge.

  ch_mapper ch_map_fcn;
  d_d_mapper d_d_map_fcn;
  d_c_mapper d_c_map_fcn;
  c_c_mapper c_c_map_fcn;

  // If flag is nonzero and we are not calling ch_map_fcn, lower_limit
  // and  upper_limit specify the range of values for which a real arg
  // returns a real value.  Outside that range, we have to convert args
  // to complex, and call the complex valued function.

  double lower_limit;
  double upper_limit;

  // For ch_map_fcn, flag has the following meanings:
  //
  //   0  =>  this function returns a matrix of ones and zeros
  //   1  =>  this function returns a numeric matrix (any values)
  //   2  =>  this function returns a string array
  //
  // For other mappers, nonzero means that this function can return a
  // complex value for some real arguments.

  int flag;

  // For custom memory management.
  static octave_allocator allocator;

  // Type id of list objects, set by register_type().
  static int t_id;

  // Type name of list objects, defined in ov-list.cc.
  static const string t_name;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
