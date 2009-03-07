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

#if !defined (octave_FloatRowVector_h)
#define octave_FloatRowVector_h 1

#include "MArray.h"

#include "mx-defs.h"

class
OCTAVE_API
FloatRowVector : public MArray<float>
{
public:

  FloatRowVector (void) : MArray<float> () { }

  explicit FloatRowVector (octave_idx_type n) : MArray<float> (n) { }

  FloatRowVector (octave_idx_type n, float val) : MArray<float> (n, val) { }

  FloatRowVector (const FloatRowVector& a) : MArray<float> (a) { }

  FloatRowVector (const MArray<float>& a) : MArray<float> (a) { }

  FloatRowVector& operator = (const FloatRowVector& a)
    {
      MArray<float>::operator = (a);
      return *this;
    }

  bool operator == (const FloatRowVector& a) const;
  bool operator != (const FloatRowVector& a) const;

  // destructive insert/delete/reorder operations

  FloatRowVector& insert (const FloatRowVector& a, octave_idx_type c);

  FloatRowVector& fill (float val);
  FloatRowVector& fill (float val, octave_idx_type c1, octave_idx_type c2);

  FloatRowVector append (const FloatRowVector& a) const;

  FloatColumnVector transpose (void) const;

  friend OCTAVE_API FloatRowVector real (const FloatComplexRowVector& a);
  friend OCTAVE_API FloatRowVector imag (const FloatComplexRowVector& a);

  // resize is the destructive equivalent for this one

  FloatRowVector extract (octave_idx_type c1, octave_idx_type c2) const;

  FloatRowVector extract_n (octave_idx_type c1, octave_idx_type n) const;

  // row vector by matrix -> row vector

  friend OCTAVE_API FloatRowVector operator * (const FloatRowVector& a, const FloatMatrix& b);

  // other operations

  typedef float (*dmapper) (float);
  typedef FloatComplex (*cmapper) (const FloatComplex&);

  FloatRowVector map (dmapper fcn) const;
  FloatComplexRowVector map (cmapper fcn) const;

  float min (void) const;
  float max (void) const;

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os, const FloatRowVector& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is, FloatRowVector& a);

private:

  FloatRowVector (float *d, octave_idx_type l) : MArray<float> (d, l) { }
};

// row vector by column vector -> scalar

float OCTAVE_API operator * (const FloatRowVector& a, const FloatColumnVector& b);

Complex OCTAVE_API operator * (const FloatRowVector& a, const ComplexColumnVector& b);

// other operations

OCTAVE_API FloatRowVector linspace (float x1, float x2, octave_idx_type n);

MARRAY_FORWARD_DEFS (MArray, FloatRowVector, float)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
