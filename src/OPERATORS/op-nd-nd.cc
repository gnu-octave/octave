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
#include "ov-typeinfo.h"

#include "ops.h"
#include "xdiv.h"
#include "xpow.h"
 
//Using the ! operator causes a segmentation fault
//DEFUNOP_OP (not, double_nd_array, !)
/*
DEFUNOP_OP (uminus, double_nd_array, -)

DEFUNOP (transpose, double_nd_array)
{
  error ("Transpose on ND array is not defined.");
  return octave_value ();
}

DEFBINOP_OP (add, double_nd_array, double_nd_array, +)
DEFBINOP_OP (sub, double_nd_array, double_nd_array, -)

DEFBINOP_FN (el_pow, double_nd_array, double_nd_array, elem_xpow)

DEFBINOPX (pow, double_nd_array, double_nd_array)
{
  error ("can't do A ^ B for A and B both N-D arrays");
  return octave_value ();
}
*/

DEFASSIGNOP_FN (assign, double_nd_array, double_nd_array, assign)

void
install_nd_nd_ops (void)
{
  //  INSTALL_UNOP (op_not, octave_double_nd_array, not);
  /*INSTALL_UNOP (op_uminus, octave_double_nd_array, uminus);
  INSTALL_UNOP (op_transpose, octave_double_nd_array, transpose);

  INSTALL_BINOP (op_add, octave_double_nd_array, octave_double_nd_array, add);
  INSTALL_BINOP (op_sub, octave_double_nd_array, octave_double_nd_array, sub);

  INSTALL_BINOP (op_el_pow, octave_double_nd_array, octave_double_nd_array, el_pow);
  */
  INSTALL_ASSIGNOP (op_asn_eq, octave_double_nd_array, octave_double_nd_array, assign);
  INSTALL_ASSIGNCONV (octave_double_nd_array, octave_double_nd_array, octave_double_nd_array);
}
