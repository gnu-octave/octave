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

#if !defined (octave_FloatComplexColumnVector_h)
#define octave_FloatComplexColumnVector_h 1

#include "MArray.h"

#include "mx-defs.h"

class
OCTAVE_API
FloatComplexColumnVector : public MArray<FloatComplex>
{
friend class FloatComplexMatrix;
friend class FloatComplexRowVector;

public:

  FloatComplexColumnVector (void) : MArray<FloatComplex> () { }

  explicit FloatComplexColumnVector (octave_idx_type n) : MArray<FloatComplex> (n) { }

  FloatComplexColumnVector (octave_idx_type n, const FloatComplex& val)
    : MArray<FloatComplex> (n, val) { }

  FloatComplexColumnVector (const FloatComplexColumnVector& a) : MArray<FloatComplex> (a) { }

  FloatComplexColumnVector (const MArray<FloatComplex>& a) : MArray<FloatComplex> (a) { }

  explicit FloatComplexColumnVector (const FloatColumnVector& a);

  FloatComplexColumnVector& operator = (const FloatComplexColumnVector& a)
    {
      MArray<FloatComplex>::operator = (a);
      return *this;
    }

  bool operator == (const FloatComplexColumnVector& a) const;
  bool operator != (const FloatComplexColumnVector& a) const;

  // destructive insert/delete/reorder operations

  FloatComplexColumnVector& insert (const FloatColumnVector& a, octave_idx_type r);
  FloatComplexColumnVector& insert (const FloatComplexColumnVector& a, octave_idx_type r);

  FloatComplexColumnVector& fill (float val);
  FloatComplexColumnVector& fill (const FloatComplex& val);
  FloatComplexColumnVector& fill (float val, octave_idx_type r1, octave_idx_type r2);
  FloatComplexColumnVector& fill (const FloatComplex& val, octave_idx_type r1, octave_idx_type r2);

  FloatComplexColumnVector stack (const FloatColumnVector& a) const;
  FloatComplexColumnVector stack (const FloatComplexColumnVector& a) const;

  FloatComplexRowVector hermitian (void) const;
  FloatComplexRowVector transpose (void) const;

  friend OCTAVE_API FloatComplexColumnVector conj (const FloatComplexColumnVector& a);

  // resize is the destructive equivalent for this one

  FloatComplexColumnVector extract (octave_idx_type r1, octave_idx_type r2) const;

  FloatComplexColumnVector extract_n (octave_idx_type r1, octave_idx_type n) const;

  // column vector by column vector -> column vector operations

  FloatComplexColumnVector& operator += (const FloatColumnVector& a);
  FloatComplexColumnVector& operator -= (const FloatColumnVector& a);

  // matrix by column vector -> column vector operations

  friend OCTAVE_API FloatComplexColumnVector operator * (const FloatComplexMatrix& a,
					 const FloatColumnVector& b);

  friend OCTAVE_API FloatComplexColumnVector operator * (const FloatComplexMatrix& a,
					 const FloatComplexColumnVector& b);

  // matrix by column vector -> column vector operations

  friend OCTAVE_API FloatComplexColumnVector operator * (const FloatMatrix& a,
					 const FloatComplexColumnVector& b);

  // diagonal matrix by column vector -> column vector operations

  friend OCTAVE_API FloatComplexColumnVector operator * (const FloatDiagMatrix& a,
					 const FloatComplexColumnVector& b);

  friend OCTAVE_API FloatComplexColumnVector operator * (const FloatComplexDiagMatrix& a,
					 const ColumnVector& b);

  friend OCTAVE_API FloatComplexColumnVector operator * (const FloatComplexDiagMatrix& a,
					 const FloatComplexColumnVector& b);

  // other operations

  typedef float (*dmapper) (const FloatComplex&);
  typedef FloatComplex (*cmapper) (const FloatComplex&);

  FloatColumnVector map (dmapper fcn) const;
  FloatComplexColumnVector map (cmapper fcn) const;

  FloatComplex min (void) const;
  FloatComplex max (void) const;

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os, const FloatComplexColumnVector& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is, FloatComplexColumnVector& a);

private:

  FloatComplexColumnVector (FloatComplex *d, octave_idx_type l) : MArray<FloatComplex> (d, l) { }
};

MARRAY_FORWARD_DEFS (MArray, FloatComplexColumnVector, FloatComplex)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
