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

#if !defined (octave_ComplexDiagMatrix_h)
#define octave_ComplexDiagMatrix_h 1

#include "MDiagArray2.h"

#include "dRowVector.h"
#include "CRowVector.h"
#include "dColVector.h"
#include "CColVector.h"

#include "mx-defs.h"

class
ComplexDiagMatrix : public MDiagArray2<Complex>
{
public:

  ComplexDiagMatrix (void) : MDiagArray2<Complex> () { }

  ComplexDiagMatrix (octave_idx_type r, octave_idx_type c) : MDiagArray2<Complex> (r, c) { }

  ComplexDiagMatrix (octave_idx_type r, octave_idx_type c, const Complex& val)
    : MDiagArray2<Complex> (r, c, val) { }

  explicit ComplexDiagMatrix (const RowVector& a)
    : MDiagArray2<Complex> (ComplexRowVector (a)) { }

  explicit ComplexDiagMatrix (const ComplexRowVector& a)
    : MDiagArray2<Complex> (a) { }

  explicit ComplexDiagMatrix (const ColumnVector& a)
    : MDiagArray2<Complex> (ComplexColumnVector (a)) { }

  explicit ComplexDiagMatrix (const ComplexColumnVector& a)
    : MDiagArray2<Complex> (a) { }

  explicit ComplexDiagMatrix (const DiagMatrix& a);

  ComplexDiagMatrix (const MDiagArray2<Complex>& a)
    : MDiagArray2<Complex> (a) { }

  ComplexDiagMatrix (const ComplexDiagMatrix& a)
    : MDiagArray2<Complex> (a) { }

  ComplexDiagMatrix& operator = (const ComplexDiagMatrix& a)
    {
      MDiagArray2<Complex>::operator = (a);
      return *this;
    }

  bool operator == (const ComplexDiagMatrix& a) const;
  bool operator != (const ComplexDiagMatrix& a) const;

  ComplexDiagMatrix& fill (double val);
  ComplexDiagMatrix& fill (const Complex& val);
  ComplexDiagMatrix& fill (double val, octave_idx_type beg, octave_idx_type end);
  ComplexDiagMatrix& fill (const Complex& val, octave_idx_type beg, octave_idx_type end);
  ComplexDiagMatrix& fill (const ColumnVector& a);
  ComplexDiagMatrix& fill (const ComplexColumnVector& a);
  ComplexDiagMatrix& fill (const RowVector& a);
  ComplexDiagMatrix& fill (const ComplexRowVector& a);
  ComplexDiagMatrix& fill (const ColumnVector& a, octave_idx_type beg);
  ComplexDiagMatrix& fill (const ComplexColumnVector& a, octave_idx_type beg);
  ComplexDiagMatrix& fill (const RowVector& a, octave_idx_type beg);
  ComplexDiagMatrix& fill (const ComplexRowVector& a, octave_idx_type beg);

  ComplexDiagMatrix hermitian (void) const;  // complex conjugate transpose
  ComplexDiagMatrix transpose (void) const;

  friend ComplexDiagMatrix conj (const ComplexDiagMatrix& a);

  // resize is the destructive analog for this one

  ComplexMatrix extract (octave_idx_type r1, octave_idx_type c1, octave_idx_type r2, octave_idx_type c2) const;

  // extract row or column i

  ComplexRowVector row (octave_idx_type i) const;
  ComplexRowVector row (char *s) const;

  ComplexColumnVector column (octave_idx_type i) const;
  ComplexColumnVector column (char *s) const;

  ComplexDiagMatrix inverse (int& info) const;
  ComplexDiagMatrix inverse (void) const;

  // diagonal matrix by diagonal matrix -> diagonal matrix operations

  ComplexDiagMatrix& operator += (const DiagMatrix& a);
  ComplexDiagMatrix& operator -= (const DiagMatrix& a);

  // other operations

  ComplexColumnVector diag (void) const;
  ComplexColumnVector diag (octave_idx_type k) const;

  // i/o

  friend std::ostream& operator << (std::ostream& os, const ComplexDiagMatrix& a);

private:

  ComplexDiagMatrix (Complex *d, octave_idx_type nr, octave_idx_type nc)
    : MDiagArray2<Complex> (d, nr, nc) { }
};

// diagonal matrix by diagonal matrix -> diagonal matrix operations

ComplexDiagMatrix
operator * (const ComplexDiagMatrix& a, const ComplexDiagMatrix& b);

ComplexDiagMatrix
operator * (const ComplexDiagMatrix& a, const DiagMatrix& b);

ComplexDiagMatrix
operator * (const DiagMatrix& a, const ComplexDiagMatrix& b);

MDIAGARRAY2_FORWARD_DEFS (MDiagArray2, ComplexDiagMatrix, Complex)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
