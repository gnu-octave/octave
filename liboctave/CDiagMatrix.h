//                                  -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#if !defined (octave_ComplexDiagMatrix_h)
#define octave_ComplexDiagMatrix_h 1

#include "Array.h"

#include "dRowVector.h"
#include "CRowVector.h"
#include "dColVector.h"
#include "CColVector.h"

#include "mx-defs.h"

extern "C++" {

class ComplexDiagMatrix : public DiagArray<Complex>
{
  friend DiagMatrix;

public:

  ComplexDiagMatrix (void) : DiagArray<Complex> () { }
  ComplexDiagMatrix (int n) : DiagArray<Complex> (n) { }
  ComplexDiagMatrix (int n, const Complex& val)
    : DiagArray<Complex> (n, val) { }
  ComplexDiagMatrix (int r, int c) : DiagArray<Complex> (r, c) { }
  ComplexDiagMatrix (int r, int c, const Complex& val)
    : DiagArray<Complex> (r, c, val) { }
  ComplexDiagMatrix (const RowVector& a);
  ComplexDiagMatrix (const ComplexRowVector& a) : DiagArray<Complex> (a) { }
  ComplexDiagMatrix (const ColumnVector& a);
  ComplexDiagMatrix (const ComplexColumnVector& a)
    : DiagArray<Complex> (a) { }
  ComplexDiagMatrix (const DiagMatrix& a);
  ComplexDiagMatrix (const DiagArray<Complex>& a)
    : DiagArray<Complex> (a) { }
  ComplexDiagMatrix (const ComplexDiagMatrix& a) : DiagArray<Complex> (a) { }
//  ComplexDiagMatrix (const Complex& a) : DiagArray<Complex> (1, a) { }

  ComplexDiagMatrix& operator = (const ComplexDiagMatrix& a)
    {
      DiagArray<Complex>::operator = (a);
      return *this;
    }

//  operator DiagArray<Complex>& () const { return *this; }

  int operator == (const ComplexDiagMatrix& a) const;
  int operator != (const ComplexDiagMatrix& a) const;

  ComplexDiagMatrix& fill (double val);
  ComplexDiagMatrix& fill (const Complex& val);
  ComplexDiagMatrix& fill (double val, int beg, int end);
  ComplexDiagMatrix& fill (const Complex& val, int beg, int end);
  ComplexDiagMatrix& fill (const ColumnVector& a);
  ComplexDiagMatrix& fill (const ComplexColumnVector& a);
  ComplexDiagMatrix& fill (const RowVector& a);
  ComplexDiagMatrix& fill (const ComplexRowVector& a);
  ComplexDiagMatrix& fill (const ColumnVector& a, int beg);
  ComplexDiagMatrix& fill (const ComplexColumnVector& a, int beg);
  ComplexDiagMatrix& fill (const RowVector& a, int beg);
  ComplexDiagMatrix& fill (const ComplexRowVector& a, int beg);

  ComplexDiagMatrix hermitian (void) const;  // complex conjugate transpose
  ComplexDiagMatrix transpose (void) const;

  friend DiagMatrix real (const ComplexDiagMatrix& a);
  friend DiagMatrix imag (const ComplexDiagMatrix& a);
  friend ComplexDiagMatrix conj (const ComplexDiagMatrix& a);

// resize is the destructive analog for this one

  ComplexMatrix extract (int r1, int c1, int r2, int c2) const;

// extract row or column i.

  ComplexRowVector row (int i) const;
  ComplexRowVector row (char *s) const;

  ComplexColumnVector column (int i) const;
  ComplexColumnVector column (char *s) const;

  ComplexDiagMatrix inverse (int& info) const;
  ComplexDiagMatrix inverse (void) const;

// diagonal matrix by diagonal matrix -> diagonal matrix operations

  ComplexDiagMatrix& operator += (const DiagMatrix& a);
  ComplexDiagMatrix& operator -= (const DiagMatrix& a);

  ComplexDiagMatrix& operator += (const ComplexDiagMatrix& a);
  ComplexDiagMatrix& operator -= (const ComplexDiagMatrix& a);

// diagonal matrix by scalar -> matrix operations

  friend ComplexMatrix operator + (const ComplexDiagMatrix& a, double s);
  friend ComplexMatrix operator - (const ComplexDiagMatrix& a, double s);

  friend ComplexMatrix operator + (const ComplexDiagMatrix& a,
				   const Complex& s);
  friend ComplexMatrix operator - (const ComplexDiagMatrix& a,
				   const Complex& s);

// diagonal matrix by scalar -> diagonal matrix operations

  friend ComplexDiagMatrix operator * (const ComplexDiagMatrix& a, double s);
  friend ComplexDiagMatrix operator / (const ComplexDiagMatrix& a, double s);

// scalar by diagonal matrix -> matrix operations

  friend ComplexMatrix operator + (double s, const ComplexDiagMatrix& a);
  friend ComplexMatrix operator - (double s, const ComplexDiagMatrix& a);

  friend ComplexMatrix operator + (const Complex& s,
				   const ComplexDiagMatrix& a);
  friend ComplexMatrix operator - (const Complex& s,
				   const ComplexDiagMatrix& a);

// scalar by diagonal matrix -> diagonal matrix operations

  friend ComplexDiagMatrix operator * (double s, const ComplexDiagMatrix& a);

// diagonal matrix by column vector -> column vector operations

  friend ComplexColumnVector operator * (const ComplexDiagMatrix& a,
					 const ColumnVector& b);

  friend ComplexColumnVector operator * (const ComplexDiagMatrix& a,
					 const ComplexColumnVector& b);

// diagonal matrix by diagonal matrix -> diagonal matrix operations

  friend ComplexDiagMatrix operator * (const ComplexDiagMatrix& a,
				       const ComplexDiagMatrix& b);

  friend ComplexDiagMatrix operator + (const ComplexDiagMatrix& a,
				       const DiagMatrix& b);
  friend ComplexDiagMatrix operator - (const ComplexDiagMatrix& a,
				       const DiagMatrix& b);
  friend ComplexDiagMatrix operator * (const ComplexDiagMatrix& a,
				       const DiagMatrix& b);

  friend ComplexDiagMatrix product (const ComplexDiagMatrix& a,
				    const DiagMatrix& b); 

// diagonal matrix by matrix -> matrix operations

  friend ComplexMatrix operator + (const ComplexDiagMatrix& a,
				   const Matrix& b); 
  friend ComplexMatrix operator - (const ComplexDiagMatrix& a,
				   const Matrix& b);
  friend ComplexMatrix operator * (const ComplexDiagMatrix& a,
				   const Matrix& b);

  friend ComplexMatrix operator + (const ComplexDiagMatrix& a,
				   const ComplexMatrix& b);
  friend ComplexMatrix operator - (const ComplexDiagMatrix& a,
				   const ComplexMatrix& b);
  friend ComplexMatrix operator * (const ComplexDiagMatrix& a,
				   const ComplexMatrix& b);

// other operations

  ComplexColumnVector diag (void) const;
  ComplexColumnVector diag (int k) const;

// i/o

  friend ostream& operator << (ostream& os, const ComplexDiagMatrix& a);

#define KLUDGE_DIAG_MATRICES
#define TYPE Complex
#define KL_DMAT_TYPE ComplexDiagMatrix
#include "mx-kludge.h"
#undef KLUDGE_DIAG_MATRICES
#undef TYPE
#undef KL_DMAT_TYPE

private:

  ComplexDiagMatrix (Complex *d, int nr, int nc)
    : DiagArray<Complex> (d, nr, nc) { }
};

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
