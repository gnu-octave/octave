//                                  -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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

#if !defined (octave_DiagMatrix_h)
#define octave_DiagMatrix_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include "Array.h"

#include "dRowVector.h"
#include "dColVector.h"

#include "mx-defs.h"

extern "C++" {

class DiagMatrix : public DiagArray<double>
{
friend class SVD;
friend class ComplexSVD;

public:

  DiagMatrix (void) : DiagArray<double> () { }
  DiagMatrix (int n) : DiagArray<double> (n) { }
  DiagMatrix (int n, double val) : DiagArray<double> (n, val) { }
  DiagMatrix (int r, int c) : DiagArray<double> (r, c) { }
  DiagMatrix (int r, int c, double val) : DiagArray<double> (r, c, val) { }
  DiagMatrix (const RowVector& a) : DiagArray<double> (a) { }
  DiagMatrix (const ColumnVector& a) : DiagArray<double> (a) { }
  DiagMatrix (const DiagArray<double>& a) : DiagArray<double> (a) { }
  DiagMatrix (const DiagMatrix& a) : DiagArray<double> (a) { }
//  DiagMatrix (double a) : DiagArray<double> (1, a) { }

  DiagMatrix& operator = (const DiagMatrix& a)
    {
      DiagArray<double>::operator = (a);
      return *this;
    }

//  operator DiagArray<double>& () const { return *this; }

  int operator == (const DiagMatrix& a) const;
  int operator != (const DiagMatrix& a) const;

  DiagMatrix& fill (double val);
  DiagMatrix& fill (double val, int beg, int end);
  DiagMatrix& fill (const ColumnVector& a);
  DiagMatrix& fill (const RowVector& a);
  DiagMatrix& fill (const ColumnVector& a, int beg);
  DiagMatrix& fill (const RowVector& a, int beg);

  DiagMatrix transpose (void) const;

// resize is the destructive analog for this one

  Matrix extract (int r1, int c1, int r2, int c2) const;

// extract row or column i.

  RowVector row (int i) const;
  RowVector row (char *s) const;

  ColumnVector column (int i) const;
  ColumnVector column (char *s) const;

  DiagMatrix inverse (void) const;
  DiagMatrix inverse (int& info) const;

// diagonal matrix by diagonal matrix -> diagonal matrix operations

  DiagMatrix& operator += (const DiagMatrix& a);
  DiagMatrix& operator -= (const DiagMatrix& a);

// diagonal matrix by scalar -> matrix operations

  friend Matrix operator + (const DiagMatrix& a, double s);
  friend Matrix operator - (const DiagMatrix& a, double s);

  friend ComplexMatrix operator + (const DiagMatrix& a, const Complex& s);
  friend ComplexMatrix operator - (const DiagMatrix& a, const Complex& s);

// diagonal matrix by scalar -> diagonal matrix operations

  friend ComplexDiagMatrix operator * (const DiagMatrix& a, const Complex& s);
  friend ComplexDiagMatrix operator / (const DiagMatrix& a, const Complex& s);

// scalar by diagonal matrix -> matrix operations

  friend Matrix operator + (double s, const DiagMatrix& a);
  friend Matrix operator - (double s, const DiagMatrix& a);

  friend ComplexMatrix operator + (const Complex& s, const DiagMatrix& a);
  friend ComplexMatrix operator - (const Complex& s, const DiagMatrix& a);

// scalar by diagonal matrix -> diagonal matrix operations

  friend ComplexDiagMatrix operator * (const Complex& s, const DiagMatrix& a);

// diagonal matrix by column vector -> column vector operations

  friend ColumnVector operator * (const DiagMatrix& a, const ColumnVector& b);

  friend ComplexColumnVector operator * (const DiagMatrix& a,
					 const ComplexColumnVector& b);

// diagonal matrix by diagonal matrix -> diagonal matrix operations

  friend DiagMatrix operator * (const DiagMatrix& a,
				const DiagMatrix& b);

  friend ComplexDiagMatrix operator + (const DiagMatrix& a,
				       const ComplexDiagMatrix& b);
  friend ComplexDiagMatrix operator - (const DiagMatrix& a,
				       const ComplexDiagMatrix& b);
  friend ComplexDiagMatrix operator * (const DiagMatrix& a,
				       const ComplexDiagMatrix& b);

  friend ComplexDiagMatrix product (const DiagMatrix& a,
				    const ComplexDiagMatrix& b);

// diagonal matrix by matrix -> matrix operations

  friend Matrix operator + (const DiagMatrix& a, const Matrix& b);
  friend Matrix operator - (const DiagMatrix& a, const Matrix& b);
  friend Matrix operator * (const DiagMatrix& a, const Matrix& b);

  friend ComplexMatrix operator + (const DiagMatrix& a,
				   const ComplexMatrix& b);
  friend ComplexMatrix operator - (const DiagMatrix& a,
				   const ComplexMatrix& b);
  friend ComplexMatrix operator * (const DiagMatrix& a,
				   const ComplexMatrix& b);

// other operations

  ColumnVector diag (void) const;
  ColumnVector diag (int k) const;

// i/o

  friend ostream& operator << (ostream& os, const DiagMatrix& a);

#define KLUDGE_DIAG_MATRICES
#define TYPE double
#define KL_DMAT_TYPE DiagMatrix
#include "mx-kludge.h"
#undef KLUDGE_DIAG_MATRICES
#undef TYPE
#undef KL_DMAT_TYPE

private:

  DiagMatrix (double *d, int nr, int nc) : DiagArray<double> (d, nr, nc) { }
};

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
