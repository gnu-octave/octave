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
#include "ov-uint8.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// matrix unary ops.

DEFNDUNOP_OP (not, uint8_matrix, uint8_array, !)
DEFNDUNOP_OP (uminus, uint8_matrix, uint8_array, -)

DEFUNOP (transpose, uint8_matrix)
{
  CAST_UNOP_ARG (const octave_uint8_matrix&);

  if (v.ndims () > 2)
    {
      error ("transpose not defined for N-d objects");
      return octave_value ();
    }
  else
    return octave_value (v.uint8_array_value().transpose ());
}

//DEFNCUNOP_METHOD (incr, uint8_matrix, increment)
//DEFNCUNOP_METHOD (decr, uint8_matrix, decrement)

// matrix by matrix ops.

DEFNDBINOP_OP (add, uint8_matrix, uint8_matrix, uint8_array, uint8_array, +)
DEFNDBINOP_OP (sub, uint8_matrix, uint8_matrix, uint8_array, uint8_array, -)

//DEFBINOP_OP (mul, uint8_matrix, uint8_matrix, *)
//DEFBINOP_FN (div, uint8_matrix, uint8_matrix, xdiv)

DEFBINOPX (pow, uint8_matrix, uint8_matrix)
{
  error ("can't do A ^ B for A and B both matrices");
  return octave_value ();
}

//DEFBINOP_FN (ldiv, uint8_matrix, uint8_matrix, xleftdiv)

DEFNDBINOP_FN (lt, uint8_matrix, uint8_matrix, uint8_array, uint8_array, mx_el_lt)
DEFNDBINOP_FN (le, uint8_matrix, uint8_matrix, uint8_array, uint8_array, mx_el_le)
DEFNDBINOP_FN (eq, uint8_matrix, uint8_matrix, uint8_array, uint8_array, mx_el_eq)
DEFNDBINOP_FN (ge, uint8_matrix, uint8_matrix, uint8_array, uint8_array, mx_el_ge)
DEFNDBINOP_FN (gt, uint8_matrix, uint8_matrix, uint8_array, uint8_array, mx_el_gt)
DEFNDBINOP_FN (ne, uint8_matrix, uint8_matrix, uint8_array, uint8_array, mx_el_ne)

DEFNDBINOP_FN (el_mul, uint8_matrix, uint8_matrix, uint8_array, uint8_array, product)

DEFNDBINOP_FN (el_div, uint8_matrix, uint8_matrix, uint8_array, uint8_array, quotient)

//DEFNDBINOP_FN (el_pow, uint8_matrix, uint8_matrix, uint8_array, uint8_array, elem_xpow)

//DEFBINOP (el_ldiv, uint8_matrix, uint8_matrix)
//{
//  CAST_BINOP_ARGS (const octave_matrix&, const octave_matrix&);
//
//  return octave_value (quotient (v2.array_value (), v1.array_value ()));
//}

DEFNDBINOP_FN (el_and, uint8_matrix, uint8_matrix, uint8_array, uint8_array, mx_el_and)
DEFNDBINOP_FN (el_or,  uint8_matrix, uint8_matrix, uint8_array, uint8_array, mx_el_or)

DEFNDASSIGNOP_FN (assign, uint8_matrix, uint8_matrix, uint8_array, assign)

void
install_ui8_ui8_ops (void)
{
  INSTALL_UNOP (op_not, octave_uint8_matrix, not);
  INSTALL_UNOP (op_uminus, octave_uint8_matrix, uminus);
  INSTALL_UNOP (op_transpose, octave_uint8_matrix, transpose);
  INSTALL_UNOP (op_hermitian, octave_uint8_matrix, transpose);

  //  INSTALL_NCUNOP (op_incr, octave_uint8_matrix, incr);
  //  INSTALL_NCUNOP (op_decr, octave_uint8_matrix, decr);

  INSTALL_BINOP (op_add, octave_uint8_matrix, octave_uint8_matrix, add);
  INSTALL_BINOP (op_sub, octave_uint8_matrix, octave_uint8_matrix, sub);
  //  INSTALL_BINOP (op_mul, octave_uint8_matrix, octave_uint8_matrix, mul);
  //  INSTALL_BINOP (op_div, octave_uint8_matrix, octave_uint8_matrix, div);
  //  INSTALL_BINOP (op_pow, octave_uint8_matrix, octave_uint8_matrix, pow);
  //  INSTALL_BINOP (op_ldiv, octave_uint8_matrix, octave_uint8_matrix, ldiv);
  INSTALL_BINOP (op_lt, octave_uint8_matrix, octave_uint8_matrix, lt);
  INSTALL_BINOP (op_le, octave_uint8_matrix, octave_uint8_matrix, le);
  INSTALL_BINOP (op_eq, octave_uint8_matrix, octave_uint8_matrix, eq);
  INSTALL_BINOP (op_ge, octave_uint8_matrix, octave_uint8_matrix, ge);
  INSTALL_BINOP (op_gt, octave_uint8_matrix, octave_uint8_matrix, gt);
  INSTALL_BINOP (op_ne, octave_uint8_matrix, octave_uint8_matrix, ne);
  INSTALL_BINOP (op_el_mul, octave_uint8_matrix, octave_uint8_matrix, el_mul);
  INSTALL_BINOP (op_el_div, octave_uint8_matrix, octave_uint8_matrix, el_div);
  //  INSTALL_BINOP (op_el_pow, octave_uint8_matrix, octave_uint8_matrix, el_pow);
  //  INSTALL_BINOP (op_el_ldiv, octave_uint8_matrix, octave_uint8_matrix, el_ldiv);
  INSTALL_BINOP (op_el_and, octave_uint8_matrix, octave_uint8_matrix, el_and);
  INSTALL_BINOP (op_el_or, octave_uint8_matrix, octave_uint8_matrix, el_or);

  INSTALL_ASSIGNOP (op_asn_eq, octave_uint8_matrix, octave_uint8_matrix, assign);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
