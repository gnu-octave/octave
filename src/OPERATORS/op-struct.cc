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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "gripes.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-struct.h"
#include "ov-typeinfo.h"
#include "ops.h"

// struct ops.

DEFNDCATOP_FN (struct_struct, struct, struct, map, map, concat)

void
install_struct_ops (void)
{
  INSTALL_CATOP (octave_struct, octave_struct, struct_struct);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
