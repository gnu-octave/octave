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

#if !defined (octave_FloatComplexRowVector_h)
#define octave_FloatComplexRowVector_h 1

#include "MArray.h"

#include "mx-defs.h"

class
OCTAVE_API
FloatComplexRowVector : public MArray<FloatComplex>
{
friend class FloatComplexColumnVector;

public:

  FloatComplexRowVector (void) : MArray<FloatComplex> () { }

  explicit FloatComplexRowVector (octave_idx_type n) : MArray<FloatComplex> (n) { }

  FloatComplexRowVector (octave_idx_type n, const FloatComplex& val) : MArray<FloatComplex> (n, val) { }

  FloatComplexRowVector (const FloatComplexRowVector& a) : MArray<FloatComplex> (a) { }

  FloatComplexRowVector (const MArray<FloatComplex>& a) : MArray<FloatComplex> (a) { }

  explicit FloatComplexRowVector (const FloatRowVector& a);

  FloatComplexRowVector& operator = (const FloatComplexRowVector& a)
    {
      MArray<FloatComplex>::operator = (a);
      return *this;
    }

  bool operator == (const FloatComplexRowVector& a) const;
  bool operator != (const FloatComplexRowVector& a) const;

  // destructive insert/delete/reorder operations

  FloatComplexRowVector& insert (const FloatRowVector& a, octave_idx_type c);
  FloatComplexRowVector& insert (const FloatComplexRowVector& a, octave_idx_type c);

  FloatComplexRowVector& fill (float val);
  FloatComplexRowVector& fill (const FloatComplex& val);
  FloatComplexRowVector& fill (float val, octave_idx_type c1, octave_idx_type c2);
  FloatComplexRowVector& fill (const FloatComplex& val, octave_idx_type c1, octave_idx_type c2);

  FloatComplexRowVector append (const FloatRowVector& a) const;
  FloatComplexRowVector append (const FloatComplexRowVector& a) const;

  FloatComplexColumnVector hermitian (void) const;
  FloatComplexColumnVector transpose (void) const;

  friend FloatComplexRowVector conj (const FloatComplexRowVector& a);

  // resize is the destructive equivalent for this one

  FloatComplexRowVector extract (octave_idx_type c1, octave_idx_type c2) const;

  FloatComplexRowVector extract_n (octave_idx_type c1, octave_idx_type n) const;

  // row vector by row vector -> row vector operations

  FloatComplexRowVector& operator += (const FloatRowVector& a);
  FloatComplexRowVector& operator -= (const FloatRowVector& a);

  // row vector by matrix -> row vector

  friend FloatComplexRowVector operator * (const FloatComplexRowVector& a,
				      const FloatComplexMatrix& b);

  friend FloatComplexRowVector operator * (const FloatRowVector& a,
				      const FloatComplexMatrix& b);

  // other operations

  typedef float (*dmapper) (const FloatComplex&);
  typedef FloatComplex (*cmapper) (const FloatComplex&);

  FloatRowVector map (dmapper fcn) const;
  FloatComplexRowVector map (cmapper fcn) const;

  FloatComplex min (void) const;
  FloatComplex max (void) const;

  // i/o

  friend std::ostream& operator << (std::ostream& os, const FloatComplexRowVector& a);
  friend std::istream& operator >> (std::istream& is, FloatComplexRowVector& a);

private:

  FloatComplexRowVector (FloatComplex *d, octave_idx_type l) : MArray<FloatComplex> (d, l) { }
};

// row vector by column vector -> scalar

FloatComplex operator * (const FloatComplexRowVector& a, const ColumnVector& b);

FloatComplex operator * (const FloatComplexRowVector& a, const FloatComplexColumnVector& b);

// other operations

OCTAVE_API FloatComplexRowVector linspace (const FloatComplex& x1, const FloatComplex& x2, octave_idx_type n);

MARRAY_FORWARD_DEFS (MArray, FloatComplexRowVector, FloatComplex)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
