/*

Copyright (C) 1996, 1997, 1998, 1999, 2000, 2002, 2004, 2005, 2006,
              2007 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_lvalue_h)
#define octave_lvalue_h 1

class octave_value;
class octave_value_list;

#include <string>

#include "oct-obj.h"
#include "pt-idx.h"

class
octave_lvalue
{
public:

  octave_lvalue (octave_value *v = 0)
    : val (v), type (), idx (), nel (1) 
    { }

  octave_lvalue (const octave_lvalue& vr)
    : val (vr.val), type (vr.type), idx (vr.idx), nel (vr.nel) 
    { 
    }

  octave_lvalue& operator = (const octave_lvalue& vr)
    {
      if (this != &vr)
	{
	  val = vr.val;
	  type = vr.type;
	  idx = vr.idx;
	  nel = vr.nel;
	}

      return *this;
    }

  ~octave_lvalue (void) { }

  bool is_black_hole (void) const { return val == 0; }

  bool is_defined (void) const { return val && val->is_defined (); }

  bool is_undefined (void) const { return ! val || val->is_undefined (); }

  bool is_map (void) const { return val && val->is_map (); }

  void define (const octave_value& v) 
    { 
      if (val)
        *val = v; 
    }

  void assign (octave_value::assign_op, const octave_value&);

  void numel (octave_idx_type n) { nel = n; }

  octave_idx_type numel (void) const { return nel; }

  void set_index (const std::string& t, const std::list<octave_value_list>& i);

  void clear_index (void) { type = std::string (); idx.clear (); }

  void do_unary_op (octave_value::unary_op op);

  octave_value value (void);

  const octave_value *object (void) const { return val; }

private:

  octave_value *val;

  std::string type;

  std::list<octave_value_list> idx;

  octave_idx_type nel;
};

#endif
