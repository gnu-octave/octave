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

#include "LPsolve.h"
#include "dColVector.h"
#include "lo-error.h"

ColumnVector
LPsolve::do_minimize (double&, octave_idx_type&, ColumnVector&)
{
  (*current_liboctave_error_handler) ("sorry, not implemented");
  return ColumnVector ();
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
