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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "error.h"
#include "oct-obj.h"
#include "oct-lvalue.h"
#include "ov.h"

void
octave_lvalue::assign (octave_value::assign_op op, const octave_value& rhs)
{
  octave_value saved_val;

  if (chg_fcn)
    saved_val = *val;

  if (idx.empty ())
    {
      if (struct_elt_name.empty ())
	val->assign (op, rhs);
      else
	val->assign_struct_elt (op, struct_elt_name, rhs);
    }
  else
    {
      if (struct_elt_name.empty ())
	val->assign (op, idx, rhs);
      else
	val->assign_struct_elt (op, struct_elt_name, idx, rhs);
    }

  if (chg_fcn && ! error_state && chg_fcn () < 0)
    *val = saved_val;
}

void
octave_lvalue::set_index (const octave_value_list& i)
{
  if (! index_set)
    {
      idx = i;
      index_set = true;
    }
  else
    error ("invalid index expression in assignment");
}

void
octave_lvalue::do_unary_op (octave_value::unary_op op)
{
  octave_value saved_val;

  if (chg_fcn)
    saved_val = *val;

  if (idx.empty ())
    val->do_non_const_unary_op (op);
  else
    val->do_non_const_unary_op (op, idx);

  if (chg_fcn && ! error_state && chg_fcn () < 0)
    *val = saved_val;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
