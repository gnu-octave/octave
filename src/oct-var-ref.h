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

#if !defined (octave_variable_reference_h)
#define octave_variable_reference_h 1

class octave_value;
class octave_value_list;

#include <string>

#include "oct-obj.h"
#include "symtab.h"

class
octave_variable_reference
{
public:

  octave_variable_reference (octave_value *v = 0,
			     symbol_record::sv_function f = 0)
    : val (v), idx (), chg_fcn (f), struct_elt_name () { }

  octave_variable_reference (octave_value *v, const string& nm,
			     symbol_record::sv_function f = 0)
    : val (v), idx (), chg_fcn (f), struct_elt_name (nm) { }

  octave_variable_reference (const octave_variable_reference& vr)
    : val (vr.val), idx (vr.idx), chg_fcn (vr.chg_fcn),
      struct_elt_name (vr.struct_elt_name) { }

  octave_variable_reference& operator = (const octave_variable_reference& vr)
    {
      if (this != &vr)
	{
	  val = vr.val;
	  idx = vr.idx;
	  chg_fcn = vr.chg_fcn;
	  struct_elt_name = vr.struct_elt_name;
	}

      return *this;
    }

  ~octave_variable_reference (void) { }

  bool is_defined (void) { return val->is_defined (); }

  bool is_undefined (void) { return val->is_undefined (); }

  bool is_map (void) { return val->is_map (); }

  void define (const octave_value& v) { *val = v; }

  void assign (octave_value::assign_op, const octave_value&);

  octave_variable_reference struct_elt_ref (const string& nm)
    { return val->struct_elt_ref (nm); }

  void index (const octave_value_list& i) { idx = i; }

  void increment (void) { val->increment (); }

  void decrement (void) { val->decrement (); }

  octave_value value (void)
    {
      return struct_elt_name.empty ()
	? (idx.empty ()
	   ? *val
	   : val->do_index_op (idx))
	: (idx.empty ()
	   ? val->do_struct_elt_index_op (struct_elt_name)
	   : val->do_struct_elt_index_op (struct_elt_name, idx));
    }

private:

  octave_value *val;

  octave_value_list idx;

  symbol_record::sv_function chg_fcn;

  string struct_elt_name;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
