/*

Copyright (C) 1996, 1997, 2002, 2003, 2004, 2005, 2007 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "gripes.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-cell.h"
#include "ov-scalar.h"
#include "ov-re-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"

// cell ops.

DEFUNOP (transpose, cell)
{
  CAST_UNOP_ARG (const octave_cell&);

  if (v.ndims () > 2)
    {
      error ("transpose not defined for N-d objects");
      return octave_value ();
    }
  else
    return octave_value (Cell (v.cell_value().transpose ()));
}

DEFCATOP_FN (c_c, cell, cell, concat)

DEFASSIGNANYOP_FN (assign, cell, assign);

void
install_cell_ops (void)
{
  INSTALL_UNOP (op_transpose, octave_cell, transpose);
  INSTALL_UNOP (op_hermitian, octave_cell, transpose);

  INSTALL_CATOP (octave_cell, octave_cell, c_c);

  INSTALL_ASSIGNANYOP (op_asn_eq, octave_cell, assign);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
