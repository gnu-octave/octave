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

#if !defined (octave_ComplexMatrix_h)
#define octave_ComplexMatrix_h 1

#include <Complex.h>

#include "Array.h"

#include "mx-defs.h"

extern "C++" {

class ComplexMatrix : public Array2<Complex>
{
friend class ComplexCHOL;
friend class ComplexHESS;
friend class ComplexLU;
friend class ComplexQR;
friend class ComplexQRP;
friend class ComplexSCHUR;
friend class ComplexSVD;
friend class ComplexColumnVector;
friend class Matrix;

public:
 
  ComplexMatrix (void) : Array2<Complex> () { }
  ComplexMatrix (int r, int c) : Array2<Complex> (r, c) { }
  ComplexMatrix (int r, int c, const Complex& val)
    : Array2<Complex> (r, c, val) { }
  ComplexMatrix (const Matrix& a);
  ComplexMatrix (const Array2<Complex>& a) : Array2<Complex> (a) { }
  ComplexMatrix (const ComplexMatrix& a) : Array2<Complex> (a) { }
  ComplexMatrix (const DiagMatrix& a);
  ComplexMatrix (const DiagArray<Complex>& a) : Array2<Complex> (a) { }
  ComplexMatrix (const ComplexDiagMatrix& a);
//  ComplexMatrix (double a) : Array2<Complex> (1, 1, a) { }
//  ComplexMatrix (const Complex& a) : Array2<Complex> (1, 1, a) { }

  ComplexMatrix& operator = (const ComplexMatrix& a)
    {
      Array2<Complex>::operator = (a);
      return *this;
    }

//  operator Array2<Complex>& () const { return *this; }

  int operator == (const ComplexMatrix& a) const;
  int operator != (const ComplexMatrix& a) const;

// destructive insert/delete/reorder operations

  ComplexMatrix& insert (const Matrix& a, int r, int c);
  ComplexMatrix& insert (const RowVector& a, int r, int c);
  ComplexMatrix& insert (const ColumnVector& a, int r, int c);
  ComplexMatrix& insert (const DiagMatrix& a, int r, int c);

  ComplexMatrix& insert (const ComplexMatrix& a, int r, int c);
  ComplexMatrix& insert (const ComplexRowVector& a, int r, int c);
  ComplexMatrix& insert (const ComplexColumnVector& a, int r, int c);
  ComplexMatrix& insert (const ComplexDiagMatrix& a, int r, int c);

  ComplexMatrix& fill (double val);
  ComplexMatrix& fill (const Complex& val);
  ComplexMatrix& fill (double val, int r1, int c1, int r2, int c2);
  ComplexMatrix& fill (const Complex& val, int r1, int c1, int r2, int c2);

  ComplexMatrix append (const Matrix& a) const;
  ComplexMatrix append (const RowVector& a) const;
  ComplexMatrix append (const ColumnVector& a) const;
  ComplexMatrix append (const DiagMatrix& a) const;

  ComplexMatrix append (const ComplexMatrix& a) const;
  ComplexMatrix append (const ComplexRowVector& a) const;
  ComplexMatrix append (const ComplexColumnVector& a) const;
  ComplexMatrix append (const ComplexDiagMatrix& a) const;

  ComplexMatrix stack (const Matrix& a) const;
  ComplexMatrix stack (const RowVector& a) const;
  ComplexMatrix stack (const ColumnVector& a) const;
  ComplexMatrix stack (const DiagMatrix& a) const;

  ComplexMatrix stack (const ComplexMatrix& a) const;
  ComplexMatrix stack (const ComplexRowVector& a) const;
  ComplexMatrix stack (const ComplexColumnVector& a) const;
  ComplexMatrix stack (const ComplexDiagMatrix& a) const;

  ComplexMatrix hermitian (void) const;  // complex conjugate transpose
  ComplexMatrix transpose (void) const;

  friend Matrix real (const ComplexMatrix& a);
  friend Matrix imag (const ComplexMatrix& a);
  friend ComplexMatrix conj (const ComplexMatrix& a);

// resize is the destructive equivalent for this one

  ComplexMatrix extract (int r1, int c1, int r2, int c2) const;

// extract row or column i.

  ComplexRowVector row (int i) const;
  ComplexRowVector row (char *s) const;

  ComplexColumnVector column (int i) const;
  ComplexColumnVector column (char *s) const;

  ComplexMatrix inverse (void) const;
  ComplexMatrix inverse (int& info) const;
  ComplexMatrix inverse (int& info, double& rcond) const;

  ComplexMatrix pseudo_inverse (double tol = 0.0);

  ComplexMatrix fourier (void) const;
  ComplexMatrix ifourier (void) const;

  ComplexMatrix fourier2d (void) const;
  ComplexMatrix ifourier2d (void) const;

  ComplexDET determinant (void) const;
  ComplexDET determinant (int& info) const;
  ComplexDET determinant (int& info, double& rcond) const;

  ComplexMatrix solve (const Matrix& b) const;
  ComplexMatrix solve (const Matrix& b, int& info) const;
  ComplexMatrix solve (const Matrix& b, int& info, double& rcond) const;

  ComplexMatrix solve (const ComplexMatrix& b) const;
  ComplexMatrix solve (const ComplexMatrix& b, int& info) const;
  ComplexMatrix solve (const ComplexMatrix& b, int& info, double& rcond) const;

  ComplexColumnVector solve (const ComplexColumnVector& b) const;
  ComplexColumnVector solve (const ComplexColumnVector& b, int& info) const;
  ComplexColumnVector solve (const ComplexColumnVector& b, int& info,
			     double& rcond) const;

  ComplexMatrix lssolve (const ComplexMatrix& b) const;
  ComplexMatrix lssolve (const ComplexMatrix& b, int& info) const;
  ComplexMatrix lssolve (const ComplexMatrix& b, int& info,
			 int& rank) const;

  ComplexColumnVector lssolve (const ComplexColumnVector& b) const;
  ComplexColumnVector lssolve (const ComplexColumnVector& b, int& info) const;
  ComplexColumnVector lssolve (const ComplexColumnVector& b, int& info,
			       int& rank) const;

// matrix by diagonal matrix -> matrix operations

  ComplexMatrix& operator += (const DiagMatrix& a);
  ComplexMatrix& operator -= (const DiagMatrix& a);

  ComplexMatrix& operator += (const ComplexDiagMatrix& a);
  ComplexMatrix& operator -= (const ComplexDiagMatrix& a);

// matrix by matrix -> matrix operations

  ComplexMatrix& operator += (const Matrix& a);
  ComplexMatrix& operator -= (const Matrix& a);

  ComplexMatrix& operator += (const ComplexMatrix& a);
  ComplexMatrix& operator -= (const ComplexMatrix& a);

// unary operations

  Matrix operator ! (void) const;

// matrix by scalar -> matrix operations

  friend ComplexMatrix operator + (const ComplexMatrix& a, double s);
  friend ComplexMatrix operator - (const ComplexMatrix& a, double s);
  friend ComplexMatrix operator * (const ComplexMatrix& a, double s);
  friend ComplexMatrix operator / (const ComplexMatrix& a, double s);

// scalar by matrix -> matrix operations

  friend ComplexMatrix operator + (double s, const ComplexMatrix& a);
  friend ComplexMatrix operator - (double s, const ComplexMatrix& a);
  friend ComplexMatrix operator * (double s, const ComplexMatrix& a);
  friend ComplexMatrix operator / (double s, const ComplexMatrix& a);

// matrix by column vector -> column vector operations

  friend ComplexColumnVector operator * (const ComplexMatrix& a,
					 const ColumnVector& b);

  friend ComplexColumnVector operator * (const ComplexMatrix& a,
					 const ComplexColumnVector& b);

// matrix by diagonal matrix -> matrix operations

  friend ComplexMatrix operator + (const ComplexMatrix& a,
				   const DiagMatrix& b);
  friend ComplexMatrix operator - (const ComplexMatrix& a,
				   const DiagMatrix& b);
  friend ComplexMatrix operator * (const ComplexMatrix& a,
				   const DiagMatrix& b);

  friend ComplexMatrix operator + (const ComplexMatrix& a,
				   const ComplexDiagMatrix& b);
  friend ComplexMatrix operator - (const ComplexMatrix& a,
				   const ComplexDiagMatrix& b);
  friend ComplexMatrix operator * (const ComplexMatrix& a,
				   const ComplexDiagMatrix& b);

// matrix by matrix -> matrix operations

  friend ComplexMatrix operator + (const ComplexMatrix& a, const Matrix& b);
  friend ComplexMatrix operator - (const ComplexMatrix& a, const Matrix& b);

  friend ComplexMatrix operator * (const ComplexMatrix& a, const Matrix& b);
  friend ComplexMatrix operator * (const ComplexMatrix& a,
				   const ComplexMatrix& b);

  friend ComplexMatrix product (const ComplexMatrix& a, const Matrix& b);
  friend ComplexMatrix quotient (const ComplexMatrix& a, const Matrix& b);

// other operations

  friend ComplexMatrix map (c_c_Mapper f, const ComplexMatrix& a);
  friend Matrix map (d_c_Mapper f, const ComplexMatrix& a);
  void map (c_c_Mapper f);

  Matrix all (void) const;
  Matrix any (void) const;

  ComplexMatrix cumprod (void) const;
  ComplexMatrix cumsum (void) const;
  ComplexMatrix prod (void) const;
  ComplexMatrix sum (void) const;
  ComplexMatrix sumsq (void) const;

  ComplexColumnVector diag (void) const;
  ComplexColumnVector diag (int k) const;

  ComplexColumnVector row_min (void) const;
  ComplexColumnVector row_min_loc (void) const;

  ComplexColumnVector row_max (void) const;
  ComplexColumnVector row_max_loc (void) const;

  ComplexRowVector column_min (void) const;
  ComplexRowVector column_min_loc (void) const;

  ComplexRowVector column_max (void) const;
  ComplexRowVector column_max_loc (void) const;

// i/o

  friend ostream& operator << (ostream& os, const ComplexMatrix& a);
  friend istream& operator >> (istream& is, ComplexMatrix& a);

#define KLUDGE_MATRICES
#define TYPE Complex
#define KL_MAT_TYPE ComplexMatrix
#include "mx-kludge.h"
#undef KLUDGE_MATRICES
#undef TYPE
#undef KL_MAT_TYPE

private:

  ComplexMatrix (Complex *d, int r, int c) : Array2<Complex> (d, r, c) { }
};

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
