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

// scalar unary ops.

DEFUNOP_OP (not, scalar, !)
DEFUNOP_OP (uminus, scalar, -)
DEFUNOP_OP (transpose, scalar, /* no-op */)
DEFUNOP_OP (hermitian, scalar, /* no-op */)

DEFNCUNOP_METHOD (incr, scalar, increment)
DEFNCUNOP_METHOD (decr, scalar, decrement)

// scalar by scalar ops.

DEFBINOP_OP (add, scalar, scalar, +)
DEFBINOP_OP (sub, scalar, scalar, -)
DEFBINOP_OP (mul, scalar, scalar, *)

DEFBINOP (div, scalar, scalar)
{
  CAST_BINOP_ARGS (const octave_scalar&, const octave_scalar&);

  double d = v2.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.double_value () / d);
}

DEFBINOP_FN (pow, scalar, scalar, xpow)

DEFBINOP (ldiv, scalar, scalar)
{
  CAST_BINOP_ARGS (const octave_scalar&, const octave_scalar&);

  double d = v1.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v2.double_value () / d);
}

DEFBINOP_OP (lt, scalar, scalar, <)
DEFBINOP_OP (le, scalar, scalar, <=)
DEFBINOP_OP (eq, scalar, scalar, ==)
DEFBINOP_OP (ge, scalar, scalar, >=)
DEFBINOP_OP (gt, scalar, scalar, >)
DEFBINOP_OP (ne, scalar, scalar, !=)

DEFBINOP_OP (el_mul, scalar, scalar, *)

DEFBINOP (el_div, scalar, scalar)
{
  CAST_BINOP_ARGS (const octave_scalar&, const octave_scalar&);

  double d = v2.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.double_value () / d);
}

DEFBINOP_FN (el_pow, scalar, scalar, xpow)

DEFBINOP (el_ldiv, scalar, scalar)
{
  CAST_BINOP_ARGS (const octave_scalar&, const octave_scalar&);

  double d = v1.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v2.double_value () / d);
}

DEFBINOP (el_and, scalar, scalar)
{
  CAST_BINOP_ARGS (const octave_scalar&, const octave_scalar&);

  double result = v1.double_value () && v2.double_value ();

  return octave_value (result);
}

DEFBINOP (el_or, scalar, scalar)
{
  CAST_BINOP_ARGS (const octave_scalar&, const octave_scalar&);

  double result = v1.double_value () || v2.double_value ();

  return octave_value (result);
}

DEFCONV (matrix_conv, scalar, matrix)
{
  CAST_CONV_ARG (const octave_scalar&);

  return new octave_matrix (v.matrix_value ());
}

void
install_s_s_ops (void)
{
  INSTALL_UNOP (not, octave_scalar, not);
  INSTALL_UNOP (uminus, octave_scalar, uminus);
  INSTALL_UNOP (transpose, octave_scalar, transpose);
  INSTALL_UNOP (hermitian, octave_scalar, hermitian);

  INSTALL_NCUNOP (incr, octave_scalar, incr);
  INSTALL_NCUNOP (decr, octave_scalar, decr);

  INSTALL_BINOP (add, octave_scalar, octave_scalar, add);
  INSTALL_BINOP (sub, octave_scalar, octave_scalar, sub);
  INSTALL_BINOP (mul, octave_scalar, octave_scalar, mul);
  INSTALL_BINOP (div, octave_scalar, octave_scalar, div);
  INSTALL_BINOP (pow, octave_scalar, octave_scalar, pow);
  INSTALL_BINOP (ldiv, octave_scalar, octave_scalar, ldiv);
  INSTALL_BINOP (lt, octave_scalar, octave_scalar, lt);
  INSTALL_BINOP (le, octave_scalar, octave_scalar, le);
  INSTALL_BINOP (eq, octave_scalar, octave_scalar, eq);
  INSTALL_BINOP (ge, octave_scalar, octave_scalar, ge);
  INSTALL_BINOP (gt, octave_scalar, octave_scalar, gt);
  INSTALL_BINOP (ne, octave_scalar, octave_scalar, ne);
  INSTALL_BINOP (el_mul, octave_scalar, octave_scalar, el_mul);
  INSTALL_BINOP (el_div, octave_scalar, octave_scalar, el_div);
  INSTALL_BINOP (el_pow, octave_scalar, octave_scalar, el_pow);
  INSTALL_BINOP (el_ldiv, octave_scalar, octave_scalar, el_ldiv);
  INSTALL_BINOP (el_and, octave_scalar, octave_scalar, el_and);
  INSTALL_BINOP (el_or, octave_scalar, octave_scalar, el_or);

  INSTALL_ASSIGNCONV (octave_scalar, octave_scalar, octave_matrix);

  INSTALL_WIDENOP (octave_scalar, octave_matrix, matrix_conv);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
