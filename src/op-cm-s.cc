/*

Copyright (C) 1996 John W. Eaton

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

#include "mx-cm-s.h"

#include "gripes.h"
#include "ov.h"
#include "ov-cx-mat.h"
#include "ov-scalar.h"
#include "ov-typeinfo.h"
#include "op-cm-s.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// complex matrix by scalar ops.

static octave_value
add (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_scalar&);

  return octave_value (v1.complex_matrix_value () + v2.double_value ());
}

static octave_value
sub (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_scalar&);

  return octave_value (v1.complex_matrix_value () - v2.double_value ());
}

static octave_value
mul (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_scalar&);

  return octave_value (v1.complex_matrix_value () * v2.double_value ());
}

static octave_value
div (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_scalar&);

  double d = v2.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.complex_matrix_value () / d);
}

static octave_value
pow (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_scalar&);

  return xpow (v1.complex_matrix_value (), v2.double_value ());
}

static octave_value
ldiv (const octave_value& v1, const octave_value&)
{
  gripe_nonconformant ("operator \\", v1.rows (), v1.columns (), 1, 1);
  return octave_value ();
}

#define BOOL_OP(OP, EMPTY_RESULT) \
  MX_SC_BOOL_OP (ComplexMatrix, m, v1.complex_matrix_value (), \
		 double, s, v2.double_value (), \
		 real (m (i, j)) OP s, EMPTY_RESULT)

static octave_value
lt (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_scalar&);

  BOOL_OP (<, boolMatrix ());
}

static octave_value
le (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_scalar&);

  BOOL_OP (<=, boolMatrix ());
}

static octave_value
eq (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_scalar&);

  MX_SC_BOOL_OP (ComplexMatrix, m, v1.complex_matrix_value (),
		 double, s, v2.double_value (),
		 m (i, j) == s, 0.0);
}

static octave_value
ge (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_scalar&);

  BOOL_OP (>=, boolMatrix ());
}

static octave_value
gt (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_scalar&);

  BOOL_OP (>, boolMatrix ());
}

static octave_value
ne (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_scalar&);

  MX_SC_BOOL_OP (ComplexMatrix, m, v1.complex_matrix_value (),
		 double, s, v2.double_value (),
		 m (i, j) != s, 1.0);
}

static octave_value
el_mul (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_scalar&);

  return octave_value (v1.complex_matrix_value () * v2.double_value ());
}

static octave_value
el_div (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_scalar&);

  double d = v2.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.complex_matrix_value () / d);
}

static octave_value
el_pow (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_scalar&);

  return elem_xpow (v1.complex_matrix_value (), v2.double_value ());
}

static octave_value
el_ldiv (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_scalar&);

  return x_el_div (v2.double_value (), v1.complex_matrix_value ());
}

static octave_value
el_and (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_scalar&);

  MX_SC_BOOL_OP (ComplexMatrix, m, v1.complex_matrix_value (),
		 double, s, v2.double_value (),
		 m (i, j) != 0.0 && s, boolMatrix ());
}

static octave_value
el_or (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_complex_matrix&, const octave_scalar&);

  MX_SC_BOOL_OP (ComplexMatrix, m, v1.complex_matrix_value (),
		 double, s, v2.double_value (),
		 m (i, j) != 0.0 || s, boolMatrix ());
}

static octave_value
assign (octave_value& a1, const octave_value_list& idx,
	const octave_value& a2)
{
  CAST_BINOP_ARGS (octave_complex_matrix&, const octave_scalar&);

  v1.assign (idx, v2.complex_matrix_value ());
  return octave_value ();
}

void
install_cm_s_ops (void)
{
  INSTALL_BINOP (add, octave_complex_matrix, octave_scalar, add);
  INSTALL_BINOP (sub, octave_complex_matrix, octave_scalar, sub);
  INSTALL_BINOP (mul, octave_complex_matrix, octave_scalar, mul);
  INSTALL_BINOP (div, octave_complex_matrix, octave_scalar, div);
  INSTALL_BINOP (pow, octave_complex_matrix, octave_scalar, pow);
  INSTALL_BINOP (ldiv, octave_complex_matrix, octave_scalar, ldiv);
  INSTALL_BINOP (lt, octave_complex_matrix, octave_scalar, lt);
  INSTALL_BINOP (le, octave_complex_matrix, octave_scalar, le);
  INSTALL_BINOP (eq, octave_complex_matrix, octave_scalar, eq);
  INSTALL_BINOP (ge, octave_complex_matrix, octave_scalar, ge);
  INSTALL_BINOP (gt, octave_complex_matrix, octave_scalar, gt);
  INSTALL_BINOP (ne, octave_complex_matrix, octave_scalar, ne);
  INSTALL_BINOP (el_mul, octave_complex_matrix, octave_scalar, el_mul);
  INSTALL_BINOP (el_div, octave_complex_matrix, octave_scalar, el_div);
  INSTALL_BINOP (el_pow, octave_complex_matrix, octave_scalar, el_pow);
  INSTALL_BINOP (el_ldiv, octave_complex_matrix, octave_scalar, el_ldiv);
  INSTALL_BINOP (el_and, octave_complex_matrix, octave_scalar, el_and);
  INSTALL_BINOP (el_or, octave_complex_matrix, octave_scalar, el_or);

  INSTALL_ASSIGNOP (octave_complex_matrix, octave_scalar, assign);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
