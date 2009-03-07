/*

Copyright (C) 1994, 1995, 1996, 1997, 2000, 2002, 2003, 2004, 2005,
              2006, 2007, 2008 John W. Eaton

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

#if !defined (octave_FloatColumnVector_h)
#define octave_FloatColumnVector_h 1

#include "MArray.h"

#include "mx-defs.h"

class
OCTAVE_API
FloatColumnVector : public MArray<float>
{
public:

  FloatColumnVector (void) : MArray<float> () { }

  explicit FloatColumnVector (octave_idx_type n) : MArray<float> (n) { }

  FloatColumnVector (octave_idx_type n, float val) : MArray<float> (n, val) { }

  FloatColumnVector (const FloatColumnVector& a) : MArray<float> (a) { }

  FloatColumnVector (const MArray<float>& a) : MArray<float> (a) { }

  FloatColumnVector& operator = (const FloatColumnVector& a)
    {
      MArray<float>::operator = (a);
      return *this;
    }

  bool operator == (const FloatColumnVector& a) const;
  bool operator != (const FloatColumnVector& a) const;

  // destructive insert/delete/reorder operations

  FloatColumnVector& insert (const FloatColumnVector& a, octave_idx_type r);

  FloatColumnVector& fill (float val);
  FloatColumnVector& fill (float val, octave_idx_type r1, octave_idx_type r2);

  FloatColumnVector stack (const FloatColumnVector& a) const;

  FloatRowVector transpose (void) const;

  friend OCTAVE_API FloatColumnVector real (const FloatComplexColumnVector& a);
  friend OCTAVE_API FloatColumnVector imag (const FloatComplexColumnVector& a);

  // resize is the destructive equivalent for this one

  FloatColumnVector extract (octave_idx_type r1, octave_idx_type r2) const;

  FloatColumnVector extract_n (octave_idx_type r1, octave_idx_type n) const;

  // matrix by column vector -> column vector operations

  friend OCTAVE_API FloatColumnVector operator * (const FloatMatrix& a, const FloatColumnVector& b);

  // diagonal matrix by column vector -> column vector operations

  friend OCTAVE_API FloatColumnVector operator * (const FloatDiagMatrix& a, const FloatColumnVector& b);

  // other operations

  typedef float (*dmapper) (float);
  typedef FloatComplex (*cmapper) (const FloatComplex&);

  FloatColumnVector map (dmapper fcn) const;
  FloatComplexColumnVector map (cmapper fcn) const;

  float min (void) const;
  float max (void) const;

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os, const FloatColumnVector& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is, FloatColumnVector& a);

private:

  FloatColumnVector (float *d, octave_idx_type l) : MArray<float> (d, l) { }
};

// Publish externally used friend functions.

extern OCTAVE_API FloatColumnVector real (const FloatComplexColumnVector& a);
extern OCTAVE_API FloatColumnVector imag (const FloatComplexColumnVector& a);

MARRAY_FORWARD_DEFS (MArray, FloatColumnVector, float)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
