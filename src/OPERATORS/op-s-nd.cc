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
#include "ov-scalar.h"
#include "ov-re-nd-array.h"
//Kluge
//#include "MArrayN.cc"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// scalar by matrix ops.

/*
DEFBINOP_OP (add, scalar, double_nd_array, +)
DEFBINOP_OP (sub, scalar, double_nd_array, -)
DEFBINOP_OP (mul, scalar, double_nd_array, *)

  DEFBINOP_OP(el_mul, scalar, double_nd_array, *)
*/
DEFCONV (array_conv, scalar, double_nd_array)
{
  CAST_CONV_ARG (const octave_scalar&);

  return new octave_double_nd_array (v.double_nd_array_value ());
}

void
install_s_nd_ops (void)
{
  /* 
 INSTALL_BINOP (op_add, octave_scalar, octave_double_nd_array, add);
  INSTALL_BINOP (op_sub, octave_scalar, octave_double_nd_array, sub);
  INSTALL_BINOP (op_mul, octave_scalar, octave_double_nd_array, mul);
  INSTALL_BINOP (op_el_mul, octave_scalar, octave_double_nd_array, el_mul);
  */  
  INSTALL_ASSIGNCONV (octave_scalar, octave_double_nd_array, octave_double_nd_array);

  INSTALL_WIDENOP (octave_scalar, octave_double_nd_array, array_conv);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
