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

#include "mx-m-cs.h"
#include "mx-cs-m.h"

#include "gripes.h"
#include "ov.h"
#include "ov-re-mat.h"
#include "ov-cx-mat.h"
#include "ov-complex.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// matrix by complex scalar ops.

DEFBINOP_OP (add, matrix, complex, +)
DEFBINOP_OP (sub, matrix, complex, -)
DEFBINOP_OP (mul, matrix, complex, *)

DEFBINOP (div, matrix, complex)
{
  CAST_BINOP_ARGS (const octave_matrix&, const octave_complex&);

  Complex d = v2.complex_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.matrix_value () / d);
}

DEFBINOP_FN (pow, matrix, complex, xpow)

DEFBINOP (ldiv, matrix, complex)
{
  CAST_BINOP_ARGS (const octave_matrix&, const octave_complex&);

  Matrix m1 = v1.matrix_value ();
  ComplexMatrix m2 = v2.complex_matrix_value ();

  return octave_value (xleftdiv (m1, m2));
}

DEFBINOP_FN (lt, matrix, complex, mx_el_lt)
DEFBINOP_FN (le, matrix, complex, mx_el_le)
DEFBINOP_FN (eq, matrix, complex, mx_el_eq)
DEFBINOP_FN (ge, matrix, complex, mx_el_ge)
DEFBINOP_FN (gt, matrix, complex, mx_el_gt)
DEFBINOP_FN (ne, matrix, complex, mx_el_ne)

DEFBINOP_OP (el_mul, matrix, complex, *)

DEFBINOP (el_div, matrix, complex)
{
  CAST_BINOP_ARGS (const octave_matrix&, const octave_complex&);

  Complex d = v2.complex_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.matrix_value () / d);
}

DEFBINOP_FN (el_pow, matrix, complex, elem_xpow)

DEFBINOP (el_ldiv, matrix, complex)
{
  CAST_BINOP_ARGS (const octave_matrix&, const octave_complex&);

  return x_el_div (v2.complex_value (), v1.matrix_value ());
}

DEFBINOP_FN (el_and, matrix, complex, mx_el_and)
DEFBINOP_FN (el_or, matrix, complex, mx_el_or)

DEFCONV (complex_matrix_conv, matrix, complex_matrix)
{
  CAST_CONV_ARG (const octave_matrix&);

  return new octave_complex_matrix (ComplexMatrix (v.matrix_value ()));
}

void
install_m_cs_ops (void)
{
  INSTALL_BINOP (op_add, octave_matrix, octave_complex, add);
  INSTALL_BINOP (op_sub, octave_matrix, octave_complex, sub);
  INSTALL_BINOP (op_mul, octave_matrix, octave_complex, mul);
  INSTALL_BINOP (op_div, octave_matrix, octave_complex, div);
  INSTALL_BINOP (op_pow, octave_matrix, octave_complex, pow);
  INSTALL_BINOP (op_ldiv, octave_matrix, octave_complex, ldiv);
  INSTALL_BINOP (op_lt, octave_matrix, octave_complex, lt);
  INSTALL_BINOP (op_le, octave_matrix, octave_complex, le);
  INSTALL_BINOP (op_eq, octave_matrix, octave_complex, eq);
  INSTALL_BINOP (op_ge, octave_matrix, octave_complex, ge);
  INSTALL_BINOP (op_gt, octave_matrix, octave_complex, gt);
  INSTALL_BINOP (op_ne, octave_matrix, octave_complex, ne);
  INSTALL_BINOP (op_el_mul, octave_matrix, octave_complex, el_mul);
  INSTALL_BINOP (op_el_div, octave_matrix, octave_complex, el_div);
  INSTALL_BINOP (op_el_pow, octave_matrix, octave_complex, el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_matrix, octave_complex, el_ldiv);
  INSTALL_BINOP (op_el_and, octave_matrix, octave_complex, el_and);
  INSTALL_BINOP (op_el_or, octave_matrix, octave_complex, el_or);

  INSTALL_ASSIGNCONV (octave_matrix, octave_complex, octave_complex_matrix);

  INSTALL_WIDENOP (octave_matrix, octave_complex_matrix, complex_matrix_conv);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
