/*

Copyright (C) 2002 John W. Eaton

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

#if !defined (octave_mx_base_h)
#define octave_mx_base_h 1

// Declarations for operators that work on matrix objects.

// ComplexDiagMatrix by X ops.

#include "mx-cdm-cm.h"
#include "mx-cdm-cs.h"
#include "mx-cdm-dm.h"
#include "mx-cdm-m.h"
#include "mx-cdm-s.h"

// ComplexMatrix by X ops.

#include "mx-cm-cdm.h"
#include "mx-cm-dm.h"
#include "mx-cm-m.h"
#include "mx-cm-s.h"

// Complex scalar by X ops.

#include "mx-cs-cdm.h"
#include "mx-cs-dm.h"
#include "mx-cs-m.h"

// DiagMatrix by X ops.

#include "mx-dm-cdm.h"
#include "mx-dm-cm.h"
#include "mx-dm-cs.h"
#include "mx-dm-m.h"
#include "mx-dm-s.h"

// Matrix by X ops.

#include "mx-m-cdm.h"
#include "mx-m-cm.h"
#include "mx-m-cs.h"
#include "mx-m-dm.h"

// Real scalar by X ops.

#include "mx-s-cdm.h"
#include "mx-s-cm.h"
#include "mx-s-dm.h"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
