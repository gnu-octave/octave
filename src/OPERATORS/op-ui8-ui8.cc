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
#include "ov-int16.h"
#include "ov-int32.h"
#include "ov-int64.h"
#include "ov-int8.h"
#include "ov-uint16.h"
#include "ov-uint32.h"
#include "ov-uint64.h"
#include "ov-uint8.h"
#include "ov-scalar.h"
#include "ov-complex.h"
#include "ov-re-mat.h"
#include "ov-cx-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

#include "op-int.h"

OCTAVE_INT_OPS (uint8)

OCTAVE_MS_INT_ASSIGN_OPS (mi8, uint8_, int8_, int8_)
OCTAVE_MS_INT_ASSIGN_OPS (mi16, uint8_, int16_, int16_)
OCTAVE_MS_INT_ASSIGN_OPS (mui16, uint8_, uint16_, uint16_)
OCTAVE_MS_INT_ASSIGN_OPS (mi32, uint8_, int32_, int32_)
OCTAVE_MS_INT_ASSIGN_OPS (mui32, uint8_, uint32_, uint32_)
OCTAVE_MS_INT_ASSIGN_OPS (mi64, uint8_, int64_, int64_)
OCTAVE_MS_INT_ASSIGN_OPS (mui64, uint8_, uint64_, uint64_)

OCTAVE_MM_INT_ASSIGN_OPS (mmi8, uint8_, int8_, int8_)
OCTAVE_MM_INT_ASSIGN_OPS (mmi16, uint8_, int16_, int16_)
OCTAVE_MM_INT_ASSIGN_OPS (mmui16, uint8_, uint16_, uint16_)
OCTAVE_MM_INT_ASSIGN_OPS (mmi32, uint8_, int32_, int32_)
OCTAVE_MM_INT_ASSIGN_OPS (mmui32, uint8_, uint32_, uint32_)
OCTAVE_MM_INT_ASSIGN_OPS (mmi64, uint8_, int64_, int64_)
OCTAVE_MM_INT_ASSIGN_OPS (mmui64, uint8_, uint64_, uint64_)

void
install_ui8_ui8_ops (void)
{
  OCTAVE_INSTALL_INT_OPS (uint8)

  OCTAVE_INSTALL_MS_INT_ASSIGN_OPS (mi8, uint8_, int8_);
  OCTAVE_INSTALL_MS_INT_ASSIGN_OPS (mi16, uint8_, int16_);
  OCTAVE_INSTALL_MS_INT_ASSIGN_OPS (mui16, uint8_, uint16_);
  OCTAVE_INSTALL_MS_INT_ASSIGN_OPS (mi32, uint8_, int32_);
  OCTAVE_INSTALL_MS_INT_ASSIGN_OPS (mui32, uint8_, uint32_);
  OCTAVE_INSTALL_MS_INT_ASSIGN_OPS (mi64, uint8_, int64_);
  OCTAVE_INSTALL_MS_INT_ASSIGN_OPS (mui64, uint8_, uint64_);

  OCTAVE_INSTALL_MM_INT_ASSIGN_OPS (mmi8, uint8_, int8_);
  OCTAVE_INSTALL_MM_INT_ASSIGN_OPS (mmi16, uint8_, int16_);
  OCTAVE_INSTALL_MM_INT_ASSIGN_OPS (mmui16, uint8_, uint16_);
  OCTAVE_INSTALL_MM_INT_ASSIGN_OPS (mmi32, uint8_, int32_);
  OCTAVE_INSTALL_MM_INT_ASSIGN_OPS (mmui32, uint8_, uint32_);
  OCTAVE_INSTALL_MM_INT_ASSIGN_OPS (mmi64, uint8_, int64_);
  OCTAVE_INSTALL_MM_INT_ASSIGN_OPS (mmui64, uint8_, uint64_);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
