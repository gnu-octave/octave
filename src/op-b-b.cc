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
#include "ov-bool.h"
#include "ov-typeinfo.h"
#include "op-b-b.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// bool by bool ops.

static octave_value
eq (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_bool&, const octave_bool&);

  return octave_value (v1.bool_value () == v2.bool_value ());
}

static octave_value
ne (const octave_value& a1, const octave_value& a2)
{
  CAST_BINOP_ARGS (const octave_bool&, const octave_bool&);

  return octave_value (v1.bool_value () != v2.bool_value ());
}

void
install_b_b_ops (void)
{
  INSTALL_BINOP (eq, octave_bool, octave_bool, eq);
  INSTALL_BINOP (ne, octave_bool, octave_bool, ne);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
