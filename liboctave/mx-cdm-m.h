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

#if !defined (octave_cdm_m_h)
#define octave_cdm_m_h 1

class Matrix;
class ComplexMatrix;
class ComplexDiagMatrix;

#include "mx-op-defs.h"

DMM_OP_DECLS (ComplexMatrix, ComplexDiagMatrix, Matrix)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
