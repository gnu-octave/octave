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

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "gripes.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-int32.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// matrix unary ops.

DEFNDUNOP_OP (not, int32_matrix, int32_array, !)
DEFNDUNOP_OP (uminus, int32_matrix, int32_array, -)

DEFUNOP (transpose, int32_matrix)
{
  CAST_UNOP_ARG (const octave_int32_matrix&);

  if (v.ndims () > 2)
    {
      error ("transpose not defined for N-d objects");
      return octave_value ();
    }
  else
    return octave_value (v.int32_array_value().transpose ());
}

//DEFNCUNOP_METHOD (incr, int32_matrix, increment)
//DEFNCUNOP_METHOD (decr, int32_matrix, decrement)

// matrix by matrix ops.

DEFNDBINOP_OP (add, int32_matrix, int32_matrix, int32_array, int32_array, +)
DEFNDBINOP_OP (sub, int32_matrix, int32_matrix, int32_array, int32_array, -)

//DEFBINOP_OP (mul, int32_matrix, int32_matrix, *)
//DEFBINOP_FN (div, int32_matrix, int32_matrix, xdiv)

DEFBINOPX (pow, int32_matrix, int32_matrix)
{
  error ("can't do A ^ B for A and B both matrices");
  return octave_value ();
}

//DEFBINOP_FN (ldiv, int32_matrix, int32_matrix, xleftdiv)

DEFNDBINOP_FN (lt, int32_matrix, int32_matrix, int32_array, int32_array, mx_el_lt)
DEFNDBINOP_FN (le, int32_matrix, int32_matrix, int32_array, int32_array, mx_el_le)
DEFNDBINOP_FN (eq, int32_matrix, int32_matrix, int32_array, int32_array, mx_el_eq)
DEFNDBINOP_FN (ge, int32_matrix, int32_matrix, int32_array, int32_array, mx_el_ge)
DEFNDBINOP_FN (gt, int32_matrix, int32_matrix, int32_array, int32_array, mx_el_gt)
DEFNDBINOP_FN (ne, int32_matrix, int32_matrix, int32_array, int32_array, mx_el_ne)

DEFNDBINOP_FN (el_mul, int32_matrix, int32_matrix, int32_array, int32_array, product)

DEFNDBINOP_FN (el_div, int32_matrix, int32_matrix, int32_array, int32_array, quotient)

//DEFNDBINOP_FN (el_pow, int32_matrix, int32_matrix, int32_array, int32_array, elem_xpow)

//DEFBINOP (el_ldiv, int32_matrix, int32_matrix)
//{
//  CAST_BINOP_ARGS (const octave_matrix&, const octave_matrix&);
//
//  return octave_value (quotient (v2.array_value (), v1.array_value ()));
//}

DEFNDBINOP_FN (el_and, int32_matrix, int32_matrix, int32_array, int32_array, mx_el_and)
DEFNDBINOP_FN (el_or,  int32_matrix, int32_matrix, int32_array, int32_array, mx_el_or)

DEFNDASSIGNOP_FN (assign, int32_matrix, int32_matrix, int32_array, assign)

void
install_i32_i32_ops (void)
{
  INSTALL_UNOP (op_not, octave_int32_matrix, not);
  INSTALL_UNOP (op_uminus, octave_int32_matrix, uminus);
  INSTALL_UNOP (op_transpose, octave_int32_matrix, transpose);
  INSTALL_UNOP (op_hermitian, octave_int32_matrix, transpose);

  //  INSTALL_NCUNOP (op_incr, octave_int32_matrix, incr);
  //  INSTALL_NCUNOP (op_decr, octave_int32_matrix, decr);

  INSTALL_BINOP (op_add, octave_int32_matrix, octave_int32_matrix, add);
  INSTALL_BINOP (op_sub, octave_int32_matrix, octave_int32_matrix, sub);
  //  INSTALL_BINOP (op_mul, octave_int32_matrix, octave_int32_matrix, mul);
  //  INSTALL_BINOP (op_div, octave_int32_matrix, octave_int32_matrix, div);
  INSTALL_BINOP (op_pow, octave_int32_matrix, octave_int32_matrix, pow);
  //  INSTALL_BINOP (op_ldiv, octave_int32_matrix, octave_int32_matrix, ldiv);
  INSTALL_BINOP (op_lt, octave_int32_matrix, octave_int32_matrix, lt);
  INSTALL_BINOP (op_le, octave_int32_matrix, octave_int32_matrix, le);
  INSTALL_BINOP (op_eq, octave_int32_matrix, octave_int32_matrix, eq);
  INSTALL_BINOP (op_ge, octave_int32_matrix, octave_int32_matrix, ge);
  INSTALL_BINOP (op_gt, octave_int32_matrix, octave_int32_matrix, gt);
  INSTALL_BINOP (op_ne, octave_int32_matrix, octave_int32_matrix, ne);
  INSTALL_BINOP (op_el_mul, octave_int32_matrix, octave_int32_matrix, el_mul);
  INSTALL_BINOP (op_el_div, octave_int32_matrix, octave_int32_matrix, el_div);
  //  INSTALL_BINOP (op_el_pow, octave_int32_matrix, octave_int32_matrix, el_pow);
  //  INSTALL_BINOP (op_el_ldiv, octave_int32_matrix, octave_int32_matrix, el_ldiv);
  INSTALL_BINOP (op_el_and, octave_int32_matrix, octave_int32_matrix, el_and);
  INSTALL_BINOP (op_el_or, octave_int32_matrix, octave_int32_matrix, el_or);

  INSTALL_ASSIGNOP (op_asn_eq, octave_int32_matrix, octave_int32_matrix, assign);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
