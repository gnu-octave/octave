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

#include "gripes.h"
#include "ov.h"
#include "ov-str-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"

// string by string ops.

static octave_value
eq (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_char_matrix_str&,
		   const octave_char_matrix_str&);

  return octave_value (v1.char_matrix_value () == v2.char_matrix_value ());
}

static octave_value
ne (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_char_matrix_str&,
		   const octave_char_matrix_str&);

  return octave_value (v1.char_matrix_value () != v2.char_matrix_value ());
}

static octave_value
assign (octave_value& a1, const octave_value_list& idx,
	const octave_value& a2)
{
  CAST_BINOP_ARGS (octave_char_matrix_str&, const octave_char_matrix_str&);

  v1.assign (idx, v2.char_matrix_value ());
  return octave_value ();
}

void
install_str_str_ops (void)
{
  INSTALL_BINOP (eq, octave_char_matrix_str, octave_char_matrix_str, eq);
  INSTALL_BINOP (ne, octave_char_matrix_str, octave_char_matrix_str, ne);

  INSTALL_ASSIGNOP (octave_char_matrix_str, octave_char_matrix_str, assign);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
