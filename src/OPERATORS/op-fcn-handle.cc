/*

Copyright (C) 2003 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "gripes.h"
#include "ov.h"
#include "ov-fcn-handle.h"
#include "ov-typeinfo.h"
#include "ops.h"

// fcn_handle unary ops.

DEFUNOP (transpose, fcn_handle)
{
  CAST_UNOP_ARG (const octave_fcn_handle&);

  return octave_value (fcn_handle_array (v.fcn_handle_array_value().transpose ()));
}

DEFASSIGNOP (assign, fcn_handle, fcn_handle)
{
  CAST_BINOP_ARGS (octave_fcn_handle&, const octave_fcn_handle&);

  v1.assign (idx, v2.fcn_handle_array_value ());
  return octave_value ();
}

void
install_fcn_handle_ops (void)
{
  INSTALL_UNOP (op_transpose, octave_fcn_handle, transpose);
  INSTALL_UNOP (op_hermitian, octave_fcn_handle, transpose);

  INSTALL_ASSIGNOP (op_asn_eq, octave_fcn_handle, octave_fcn_handle, assign);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
