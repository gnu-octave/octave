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
#include "ov-uint64.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

#include "op-int.h"

OCTAVE_S_INT_UNOPS (uint64)
OCTAVE_SS_INT_CMP_OPS (uint64, uint64)
OCTAVE_SS_INT_BOOL_OPS (uint64, uint64)

OCTAVE_SM_INT_CMP_OPS (uint64, uint64)
OCTAVE_SM_INT_BOOL_OPS (uint64, uint64)

OCTAVE_MS_INT_CMP_OPS (uint64, uint64)
OCTAVE_MS_INT_BOOL_OPS (uint64, uint64)

OCTAVE_M_INT_UNOPS (uint64)
OCTAVE_MM_INT_CMP_OPS (uint64, uint64)
OCTAVE_MM_INT_BOOL_OPS (uint64, uint64)
OCTAVE_MM_INT_ASSIGN_OPS (uint64)

void
install_ui64_ui64_ops (void)
{
  OCTAVE_INSTALL_S_INT_UNOPS (uint64);
  OCTAVE_INSTALL_SS_INT_CMP_OPS (uint64, uint64);
  OCTAVE_INSTALL_SS_INT_BOOL_OPS (uint64, uint64);

  OCTAVE_INSTALL_SM_INT_CMP_OPS (uint64, uint64);
  OCTAVE_INSTALL_SM_INT_BOOL_OPS (uint64, uint64);

  OCTAVE_INSTALL_MS_INT_CMP_OPS (uint64, uint64);
  OCTAVE_INSTALL_MS_INT_BOOL_OPS (uint64, uint64);

  OCTAVE_INSTALL_M_INT_UNOPS (uint64);
  OCTAVE_INSTALL_MM_INT_CMP_OPS (uint64, uint64);
  OCTAVE_INSTALL_MM_INT_BOOL_OPS (uint64, uint64);
  OCTAVE_INSTALL_MM_INT_ASSIGN_OPS (uint64);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
