/*

Copyright (C) 2003 Petter Risholm and John W. Eaton

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
#include "ov-re-nd-array.h"
#include "ov-scalar.h"
#include "ov-typeinfo.h"
//Kluge

#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// ndArray by scalar ops.

/*
DEFBINOP_OP (add, double_nd_array, scalar, +)
DEFBINOP_OP (sub, double_nd_array, scalar, -)
DEFBINOP_OP (mul, double_nd_array, scalar, *)

DEFBINOP (div, double_nd_array, scalar)
{
  CAST_BINOP_ARGS (const octave_double_nd_array&, const octave_scalar&);
  double d = v2.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.double_nd_array_value () / d);
}

DEFBINOP_OP (el_mul, double_nd_array, scalar, *)

DEFBINOP (el_div, double_nd_array, scalar)
{
  CAST_BINOP_ARGS (const octave_double_nd_array&, const octave_scalar&);

  double d = v2.double_value ();

  if (d == 0.0)
    gripe_divide_by_zero ();

  return octave_value (v1.double_nd_array_value () / d);
}

DEFBINOP_FN (el_pow, double_nd_array, scalar, elem_xpow)
*/
DEFASSIGNOP_FN (assign, double_nd_array, scalar, assign)

void
install_nd_s_ops (void)
{
  /*  INSTALL_BINOP (op_add, octave_double_nd_array, octave_scalar, add);
  INSTALL_BINOP (op_sub, octave_double_nd_array, octave_scalar, sub);
  INSTALL_BINOP (op_mul, octave_double_nd_array, octave_scalar, mul);
  INSTALL_BINOP (op_div, octave_double_nd_array, octave_scalar, div);
  INSTALL_BINOP (op_el_pow, octave_double_nd_array, octave_scalar, el_pow);
  INSTALL_BINOP (op_el_mul, octave_double_nd_array, octave_scalar, el_mul);
  INSTALL_BINOP (op_el_div, octave_double_nd_array, octave_scalar, el_div);
  */
  INSTALL_ASSIGNOP (op_asn_eq, octave_double_nd_array, octave_scalar, assign);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
