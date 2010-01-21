/*

Copyright (C) 1995, 1996, 1997, 2000, 2003, 2005, 2006, 2007, 2008, 2009
              John W. Eaton

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

// Instantiate MArrays of FloatComplex values.

#include "oct-cmplx.h"

#include "MArray.h"
#include "MArray.cc"
template <> OCTAVE_API float MArray<FloatComplex>::norm (float p) const;
#include "fCColVector.h"
#include "oct-norm.h"

template <>
OCTAVE_API float
MArray<FloatComplex>::norm (float p) const
{
  return xnorm (FloatComplexColumnVector (*this), p);
}

template class OCTAVE_API MArray<FloatComplex>;

INSTANTIATE_MARRAY_FRIENDS (FloatComplex, OCTAVE_API)

#include "MArray2.h"
#include "MArray2.cc"

template class OCTAVE_API MArray2<FloatComplex>;

INSTANTIATE_MARRAY2_FRIENDS (FloatComplex, OCTAVE_API)

#include "MArrayN.h"
#include "MArrayN.cc"

template class OCTAVE_API MArrayN<FloatComplex>;

INSTANTIATE_MARRAYN_FRIENDS (FloatComplex, OCTAVE_API)

#include "MDiagArray2.h"
#include "MDiagArray2.cc"

template class OCTAVE_API MDiagArray2<FloatComplex>;

INSTANTIATE_MDIAGARRAY2_FRIENDS (FloatComplex, OCTAVE_API)
