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

#include "mx-cs-m.h"
#include "mx-m-cs.h"

#include "gripes.h"
#include "ov.h"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-re-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// complex scalar by matrix ops.

DEFBINOP_OP (add, complex, matrix, +)
DEFBINOP_OP (sub, complex, matrix, -)
DEFBINOP_OP (mul, complex, matrix, *)

DEFBINOP (div, complex, matrix)
{
  BINOP_NONCONFORMANT ("operator /");
}

DEFBINOP_FN (pow, complex, matrix, xpow)

DEFBINOP (ldiv, complex, matrix)
{
  CAST_BINOP_ARGS (const octave_complex&, const octave_matrix&);

  Complex d = v1.complex_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v2.matrix_value () / d);
}

DEFBINOP_FN (lt, complex, matrix, mx_el_lt)
DEFBINOP_FN (le, complex, matrix, mx_el_le)
DEFBINOP_FN (eq, complex, matrix, mx_el_eq)
DEFBINOP_FN (ge, complex, matrix, mx_el_ge)
DEFBINOP_FN (gt, complex, matrix, mx_el_gt)
DEFBINOP_FN (ne, complex, matrix, mx_el_ne)

DEFBINOP_OP (el_mul, complex, matrix, *)
DEFBINOP_FN (el_div, complex, matrix, x_el_div)
DEFBINOP_FN (el_pow, complex, matrix, elem_xpow)

DEFBINOP (el_ldiv, complex, matrix)
{
  CAST_BINOP_ARGS (const octave_complex&, const octave_matrix&);

  Complex d = v1.complex_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v2.matrix_value () / d);
}

DEFBINOP_FN (el_and, complex, matrix, mx_el_and)
DEFBINOP_FN (el_or, complex, matrix, mx_el_or)

DEFCONV (complex_matrix_conv, complex, complex_matrix)
{
  CAST_CONV_ARG (const octave_complex&);

  return new octave_complex_matrix (v.complex_matrix_value ());
}

void
install_cs_m_ops (void)
{
  INSTALL_BINOP (op_add, octave_complex, octave_matrix, add);
  INSTALL_BINOP (op_sub, octave_complex, octave_matrix, sub);
  INSTALL_BINOP (op_mul, octave_complex, octave_matrix, mul);
  INSTALL_BINOP (op_div, octave_complex, octave_matrix, div);
  INSTALL_BINOP (op_pow, octave_complex, octave_matrix, pow);
  INSTALL_BINOP (op_ldiv, octave_complex, octave_matrix, ldiv);
  INSTALL_BINOP (op_lt, octave_complex, octave_matrix, lt);
  INSTALL_BINOP (op_le, octave_complex, octave_matrix, le);
  INSTALL_BINOP (op_eq, octave_complex, octave_matrix, eq);
  INSTALL_BINOP (op_ge, octave_complex, octave_matrix, ge);
  INSTALL_BINOP (op_gt, octave_complex, octave_matrix, gt);
  INSTALL_BINOP (op_ne, octave_complex, octave_matrix, ne);
  INSTALL_BINOP (op_el_mul, octave_complex, octave_matrix, el_mul);
  INSTALL_BINOP (op_el_div, octave_complex, octave_matrix, el_div);
  INSTALL_BINOP (op_el_pow, octave_complex, octave_matrix, el_pow);
  INSTALL_BINOP (op_el_ldiv, octave_complex, octave_matrix, el_ldiv);
  INSTALL_BINOP (op_el_and, octave_complex, octave_matrix, el_and);
  INSTALL_BINOP (op_el_or, octave_complex, octave_matrix, el_or);

  INSTALL_ASSIGNCONV (octave_complex, octave_matrix, octave_complex_matrix);

  INSTALL_WIDENOP (op_octave_complex, octave_complex_matrix, complex_matrix_conv);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
