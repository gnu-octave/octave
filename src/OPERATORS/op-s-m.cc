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

#include "gripes.h"
#include "ov.h"
#include "ov-scalar.h"
#include "ov-re-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// scalar by matrix ops.

DEFBINOP_OP (add, scalar, matrix, +)
DEFBINOP_OP (sub, scalar, matrix, -)
DEFBINOP_OP (mul, scalar, matrix, *)

DEFBINOP (div, scalar, matrix)
{
  BINOP_NONCONFORMANT ("operator /");
}

DEFBINOP_FN (pow, scalar, matrix, xpow)

DEFBINOP (ldiv, scalar, matrix)
{
  CAST_BINOP_ARGS (const octave_scalar&, const octave_matrix&);

  double d = v1.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v2.matrix_value () / d);
}

DEFBINOP_FN (lt, scalar, matrix, mx_el_lt)
DEFBINOP_FN (le, scalar, matrix, mx_el_le)
DEFBINOP_FN (eq, scalar, matrix, mx_el_eq)
DEFBINOP_FN (ge, scalar, matrix, mx_el_ge)
DEFBINOP_FN (gt, scalar, matrix, mx_el_gt)
DEFBINOP_FN (ne, scalar, matrix, mx_el_ne)

DEFBINOP_OP (el_mul, scalar, matrix, *)
DEFBINOP_FN (el_div, scalar, matrix, x_el_div)
DEFBINOP_FN (el_pow, scalar, matrix, elem_xpow)

DEFBINOP (el_ldiv, scalar, matrix)
{
  CAST_BINOP_ARGS (const octave_scalar&, const octave_matrix&);

  double d = v1.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v2.matrix_value () / d);
}

DEFBINOP_FN (el_and, scalar, matrix, mx_el_and)
DEFBINOP_FN (el_or, scalar, matrix, mx_el_or)

DEFCONV (matrix_conv, scalar, matrix)
{
  CAST_CONV_ARG (const octave_scalar&);

  return new octave_matrix (v.matrix_value ());
}

void
install_s_m_ops (void)
{
  INSTALL_BINOP (op_add, octave_scalar, octave_matrix, add);
  INSTALL_BINOP (op_sub, octave_scalar, octave_matrix, sub);
  INSTALL_BINOP (op_mul, octave_scalar, octave_matrix, mul);
  INSTALL_BINOP (op_div, octave_scalar, octave_matrix, div);
  INSTALL_BINOP (op_pow, octave_scalar, octave_matrix, pow);
  INSTALL_BINOP (op_ldiv, octave_scalar, octave_matrix, ldiv);
  INSTALL_BINOP (op_lt, octave_scalar, octave_matrix, lt);
  INSTALL_BINOP (op_le, octave_scalar, octave_matrix, le);
  INSTALL_BINOP (op_eq, octave_scalar, octave_matrix, eq);
  INSTALL_BINOP (op_ge, octave_scalar, octave_matrix, ge);
  INSTALL_BINOP (op_gt, octave_scalar, octave_matrix, gt);
  INSTALL_BINOP (op_ne, octave_scalar, octave_matrix, ne);
  INSTALL_BINOP (op_el_mul, octave_scalar, octave_matrix, el_mul);
  INSTALL_BINOP (op_el_div, octave_scalar, octave_matrix, el_div);
  INSTALL_BINOP (op_el_pow, octave_scalar, octave_matrix, el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_scalar, octave_matrix, el_ldiv);
  INSTALL_BINOP (op_el_and, octave_scalar, octave_matrix, el_and);
  INSTALL_BINOP (op_el_or, octave_scalar, octave_matrix, el_or);

  INSTALL_ASSIGNCONV (octave_scalar, octave_matrix, octave_matrix);

  INSTALL_WIDENOP (octave_scalar, octave_matrix, matrix_conv);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
