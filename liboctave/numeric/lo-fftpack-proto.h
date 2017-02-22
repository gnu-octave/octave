/*

Copyright (C) 2016-2017 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_lo_fftpack_proto_h)
#define octave_lo_fftpack_proto_h 1

#include "octave-config.h"

#include "f77-fcn.h"

extern "C"
{
  // Note that the original complex fft routines were not written for
  // float or double complex arguments.  They have been modified by
  // adding an implicit float or double precision (a-h,o-z) statement
  // at the beginning of each subroutine.

  // FFTB

  F77_RET_T
  F77_FUNC (cfftb, CFFTB) (const F77_INT&, F77_CMPLX*, F77_CMPLX*);

  F77_RET_T
  F77_FUNC (zfftb, ZFFTB) (const F77_INT&, F77_DBLE_CMPLX*, F77_DBLE_CMPLX*);

  // FFTF

  F77_RET_T
  F77_FUNC (cfftf, CFFTF) (const F77_INT&, F77_CMPLX*, F77_CMPLX*);

  F77_RET_T
  F77_FUNC (zfftf, ZFFTF) (const F77_INT&, F77_DBLE_CMPLX*, F77_DBLE_CMPLX*);

  // FFTI

  F77_RET_T
  F77_FUNC (cffti, CFFTI) (const F77_INT&, F77_CMPLX*);

  F77_RET_T
  F77_FUNC (zffti, ZFFTI) (const F77_INT&, F77_DBLE_CMPLX*);
}

#endif
