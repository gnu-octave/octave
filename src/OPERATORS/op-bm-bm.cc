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
#include "ov-bool-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// unary bool matrix ops.

DEFUNOP_OP (not, bool_matrix, !)

DEFUNOP (transpose, bool_matrix)
{
  CAST_UNOP_ARG (const octave_bool_matrix&);

  return octave_value (v.bool_matrix_value().transpose ());
}

// bool matrix by bool matrix ops.

DEFBINOP_OP (eq, bool_matrix, bool_matrix, ==)
DEFBINOP_OP (ne, bool_matrix, bool_matrix, !=)

void
install_bm_bm_ops (void)
{
  INSTALL_UNOP (not, octave_bool_matrix, not);
  INSTALL_UNOP (transpose, octave_bool_matrix, transpose);
  INSTALL_UNOP (hermitian, octave_bool_matrix, transpose);

  INSTALL_BINOP (eq, octave_bool_matrix, octave_bool_matrix, eq);
  INSTALL_BINOP (ne, octave_bool_matrix, octave_bool_matrix, ne);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
