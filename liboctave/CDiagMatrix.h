//                                  -*- C++ -*-
/*

Copyright (C) 1996 John W. Eaton

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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_ComplexDiagMatrix_h)
#define octave_ComplexDiagMatrix_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include "MDiagArray2.h"

#include "dRowVector.h"
#include "CRowVector.h"
#include "dColVector.h"
#include "CColVector.h"

#include "mx-defs.h"

class ComplexDiagMatrix : public MDiagArray2<Complex>
{
public:

  ComplexDiagMatrix (void) : MDiagArray2<Complex> () { }
  ComplexDiagMatrix (int r, int c) : MDiagArray2<Complex> (r, c) { }
  ComplexDiagMatrix (int r, int c, const Complex& val)
    : MDiagArray2<Complex> (r, c, val) { }
  ComplexDiagMatrix (const RowVector& a)
    : MDiagArray2<Complex> (ComplexRowVector (a)) { }
  ComplexDiagMatrix (const ComplexRowVector& a) : MDiagArray2<Complex> (a) { }
  ComplexDiagMatrix (const ColumnVector& a)
    : MDiagArray2<Complex> (ComplexColumnVector (a)) { }
  ComplexDiagMatrix (const ComplexColumnVector& a)
    : MDiagArray2<Complex> (a) { }
  ComplexDiagMatrix (const DiagMatrix& a);
  ComplexDiagMatrix (const MDiagArray2<Complex>& a)
    : MDiagArray2<Complex> (a) { }
  ComplexDiagMatrix (const ComplexDiagMatrix& a) : MDiagArray2<Complex> (a) { }

  ComplexDiagMatrix& operator = (const ComplexDiagMatrix& a)
    {
      MDiagArray2<Complex>::operator = (a);
      return *this;
    }

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

  friend ComplexDiagMatrix conj (const ComplexDiagMatrix& a);

  // resize is the destructive analog for this one

  ComplexMatrix extract (int r1, int c1, int r2, int c2) const;

  // extract row or column i

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

  // diagonal matrix by scalar -> diagonal matrix operations

  friend ComplexDiagMatrix operator * (const ComplexDiagMatrix& a, double s);
  friend ComplexDiagMatrix operator / (const ComplexDiagMatrix& a, double s);

  friend ComplexDiagMatrix operator * (const DiagMatrix& a, const Complex& s);
  friend ComplexDiagMatrix operator / (const DiagMatrix& a, const Complex& s);

  // scalar by diagonal matrix -> diagonal matrix operations

  friend ComplexDiagMatrix operator * (double s, const ComplexDiagMatrix& a);

  friend ComplexDiagMatrix operator * (const Complex& s, const DiagMatrix& a);

  // diagonal matrix by diagonal matrix -> diagonal matrix operations

  friend ComplexDiagMatrix operator * (const ComplexDiagMatrix& a,
				       const ComplexDiagMatrix& b);

  friend ComplexDiagMatrix operator + (const ComplexDiagMatrix& a,
				       const DiagMatrix& b);
  friend ComplexDiagMatrix operator - (const ComplexDiagMatrix& a,
				       const DiagMatrix& b);
  friend ComplexDiagMatrix operator * (const ComplexDiagMatrix& a,
				       const DiagMatrix& b);

  friend ComplexDiagMatrix operator + (const DiagMatrix& a,
				       const ComplexDiagMatrix& b);
  friend ComplexDiagMatrix operator - (const DiagMatrix& a,
				       const ComplexDiagMatrix& b);
  friend ComplexDiagMatrix operator * (const DiagMatrix& a,
				       const ComplexDiagMatrix& b);

  friend ComplexDiagMatrix product (const ComplexDiagMatrix& a,
				    const DiagMatrix& b); 

  friend ComplexDiagMatrix product (const DiagMatrix& a,
				    const ComplexDiagMatrix& b);

  // other operations

  ComplexColumnVector diag (void) const;
  ComplexColumnVector diag (int k) const;

  // i/o

  friend ostream& operator << (ostream& os, const ComplexDiagMatrix& a);

private:

  ComplexDiagMatrix (Complex *d, int nr, int nc)
    : MDiagArray2<Complex> (d, nr, nc) { }
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
