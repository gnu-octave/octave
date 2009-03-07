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

#if !defined (octave_ComplexRowVector_h)
#define octave_ComplexRowVector_h 1

#include "MArray.h"

#include "mx-defs.h"

class
OCTAVE_API
ComplexRowVector : public MArray<Complex>
{
friend class ComplexColumnVector;

public:

  ComplexRowVector (void) : MArray<Complex> () { }

  explicit ComplexRowVector (octave_idx_type n) : MArray<Complex> (n) { }

  ComplexRowVector (octave_idx_type n, const Complex& val) : MArray<Complex> (n, val) { }

  ComplexRowVector (const ComplexRowVector& a) : MArray<Complex> (a) { }

  ComplexRowVector (const MArray<Complex>& a) : MArray<Complex> (a) { }

  explicit ComplexRowVector (const RowVector& a);

  ComplexRowVector& operator = (const ComplexRowVector& a)
    {
      MArray<Complex>::operator = (a);
      return *this;
    }

  bool operator == (const ComplexRowVector& a) const;
  bool operator != (const ComplexRowVector& a) const;

  // destructive insert/delete/reorder operations

  ComplexRowVector& insert (const RowVector& a, octave_idx_type c);
  ComplexRowVector& insert (const ComplexRowVector& a, octave_idx_type c);

  ComplexRowVector& fill (double val);
  ComplexRowVector& fill (const Complex& val);
  ComplexRowVector& fill (double val, octave_idx_type c1, octave_idx_type c2);
  ComplexRowVector& fill (const Complex& val, octave_idx_type c1, octave_idx_type c2);

  ComplexRowVector append (const RowVector& a) const;
  ComplexRowVector append (const ComplexRowVector& a) const;

  ComplexColumnVector hermitian (void) const;
  ComplexColumnVector transpose (void) const;

  friend ComplexRowVector conj (const ComplexRowVector& a);

  // resize is the destructive equivalent for this one

  ComplexRowVector extract (octave_idx_type c1, octave_idx_type c2) const;

  ComplexRowVector extract_n (octave_idx_type c1, octave_idx_type n) const;

  // row vector by row vector -> row vector operations

  ComplexRowVector& operator += (const RowVector& a);
  ComplexRowVector& operator -= (const RowVector& a);

  // row vector by matrix -> row vector

  friend ComplexRowVector operator * (const ComplexRowVector& a,
				      const ComplexMatrix& b);

  friend ComplexRowVector operator * (const RowVector& a,
				      const ComplexMatrix& b);

  // other operations

  typedef double (*dmapper) (const Complex&);
  typedef Complex (*cmapper) (const Complex&);

  RowVector map (dmapper fcn) const;
  ComplexRowVector map (cmapper fcn) const;

  Complex min (void) const;
  Complex max (void) const;

  // i/o

  friend std::ostream& operator << (std::ostream& os, const ComplexRowVector& a);
  friend std::istream& operator >> (std::istream& is, ComplexRowVector& a);

private:

  ComplexRowVector (Complex *d, octave_idx_type l) : MArray<Complex> (d, l) { }
};

// row vector by column vector -> scalar

Complex operator * (const ComplexRowVector& a, const ColumnVector& b);

Complex operator * (const ComplexRowVector& a, const ComplexColumnVector& b);

// other operations

OCTAVE_API ComplexRowVector linspace (const Complex& x1, const Complex& x2, octave_idx_type n);

MARRAY_FORWARD_DEFS (MArray, ComplexRowVector, Complex)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
