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
#include "ov-range.h"
#include "ov-typeinfo.h"
#include "ops.h"

// range unary ops.

DEFUNOP (not, range)
{
  CAST_UNOP_ARG (const octave_range&);

  return octave_value (! v.matrix_value());
}

DEFUNOP_OP (uminus, range, -)

DEFUNOP (transpose, range)
{
  CAST_UNOP_ARG (const octave_range&);

  return octave_value (v.matrix_value().transpose ());
}

void
install_range_ops (void)
{
  INSTALL_UNOP (op_not, octave_range, not);
  INSTALL_UNOP (op_uminus, octave_range, uminus);
  INSTALL_UNOP (op_transpose, octave_range, transpose);
  INSTALL_UNOP (op_hermitian, octave_range, transpose);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
