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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_lvalue_h)
#define octave_lvalue_h 1

class octave_value;
class octave_value_list;

#include <string>

#include "oct-obj.h"
#include "pt-idx.h"
#include "symtab.h"

// FIXME -- eliminate the following kluge?

// This variable is used when creating dummy octave_lvalue objects.
static octave_value dummy_val;

class
octave_lvalue
{
public:

  octave_lvalue (octave_value *v = &dummy_val,
		 symbol_record::change_function f = 0)
    : val (v), type (), idx (), chg_fcn (f), index_set (false) { }

  octave_lvalue (const octave_lvalue& vr)
    : val (vr.val), type (vr.type), idx (vr.idx), chg_fcn (vr.chg_fcn),
      index_set (vr.index_set) { }

  octave_lvalue& operator = (const octave_lvalue& vr)
    {
      if (this != &vr)
	{
	  val = vr.val;
	  type = vr.type;
	  idx = vr.idx;
	  chg_fcn = vr.chg_fcn;
	  index_set = vr.index_set;
	}

      return *this;
    }

  ~octave_lvalue (void) { }

  bool is_defined (void) { return val->is_defined (); }

  bool is_undefined (void) { return val->is_undefined (); }

  bool is_map (void) { return val->is_map (); }

  void define (const octave_value& v) { *val = v; }

  void assign (octave_value::assign_op, const octave_value&);

  void set_index (const std::string& t, const std::list<octave_value_list>& i);

  void clear_index (void) { type = std::string (); idx.clear (); }

  void do_unary_op (octave_value::unary_op op);

  octave_value value (void);

  const octave_value *object (void) const { return val; }

private:

  octave_value *val;

  std::string type;

  std::list<octave_value_list> idx;

  symbol_record::change_function chg_fcn;

  bool index_set;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
