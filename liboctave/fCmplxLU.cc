/*

Copyright (C) 1994, 1995, 1996, 1997, 1999, 2002, 2003, 2004, 2005,
              2007 John W. Eaton

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

#include "fCmplxLU.h"
#include "f77-fcn.h"
#include "lo-error.h"

// Instantiate the base LU class for the types we need.

#include <base-lu.h>
#include <base-lu.cc>

template class base_lu <FloatComplexMatrix>;

// Define the constructor for this particular derivation.

extern "C"
{
  F77_RET_T
  F77_FUNC (cgetrf, CGETRF) (const octave_idx_type&, const octave_idx_type&, FloatComplex*,
			     const octave_idx_type&, octave_idx_type*, octave_idx_type&);
}

FloatComplexLU::FloatComplexLU (const FloatComplexMatrix& a)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();
  octave_idx_type mn = (a_nr < a_nc ? a_nr : a_nc);

  ipvt.resize (mn);
  octave_idx_type *pipvt = ipvt.fortran_vec ();

  a_fact = a;
  FloatComplex *tmp_data = a_fact.fortran_vec ();

  octave_idx_type info = 0;

  F77_XFCN (cgetrf, CGETRF, (a_nr, a_nc, tmp_data, a_nr, pipvt, info));

  ipvt -= static_cast<octave_idx_type> (1);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
