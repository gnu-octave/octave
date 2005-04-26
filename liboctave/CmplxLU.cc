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

#include "CmplxLU.h"
#include "f77-fcn.h"
#include "lo-error.h"

// Instantiate the base LU class for the types we need.

#include <base-lu.h>
#include <base-lu.cc>

template class base_lu <ComplexMatrix, Complex, Matrix, double>;

// Define the constructor for this particular derivation.

extern "C"
{
  F77_RET_T
  F77_FUNC (zgetrf, ZGETRF) (const octave_idx_type&, const octave_idx_type&, Complex*,
			     const octave_idx_type&, octave_idx_type*, octave_idx_type&);
}

ComplexLU::ComplexLU (const ComplexMatrix& a)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();
  octave_idx_type mn = (a_nr < a_nc ? a_nr : a_nc);

  ipvt.resize (mn);
  octave_idx_type *pipvt = ipvt.fortran_vec ();

  a_fact = a;
  Complex *tmp_data = a_fact.fortran_vec ();

  octave_idx_type info = 0;

  F77_XFCN (zgetrf, ZGETRF, (a_nr, a_nc, tmp_data, a_nr, pipvt, info));

  if (f77_exception_encountered)
    (*current_liboctave_error_handler) ("unrecoverable error in zgetrf");
  else
    ipvt -= static_cast<octave_idx_type> (1);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
