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

#if !defined (octave_ComplexMatrix_h)
#define octave_ComplexMatrix_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include "MArray2.h"
#include "MDiagArray2.h"

#include "mx-defs.h"
#include "oct-cmplx.h"

class ComplexMatrix : public MArray2<Complex>
{
friend class Matrix;
friend class ComplexCHOL;
friend class ComplexHESS;
friend class ComplexLU;
friend class ComplexQR;
friend class ComplexQRP;
friend class ComplexSCHUR;
friend class ComplexSVD;

public:
 
  ComplexMatrix (void) : MArray2<Complex> () { }
  ComplexMatrix (int r, int c) : MArray2<Complex> (r, c) { }
  ComplexMatrix (int r, int c, const Complex& val)
    : MArray2<Complex> (r, c, val) { }
  ComplexMatrix (const Matrix& a);
  ComplexMatrix (const MArray2<Complex>& a) : MArray2<Complex> (a) { }
  ComplexMatrix (const ComplexMatrix& a) : MArray2<Complex> (a) { }
  ComplexMatrix (const RowVector& rv);
  ComplexMatrix (const ColumnVector& cv);
  ComplexMatrix (const DiagMatrix& a);
  //  ComplexMatrix (const MDiagArray2<Complex>& a) : MArray2<Complex> (a) { }
  ComplexMatrix (const ComplexRowVector& rv);
  ComplexMatrix (const ComplexColumnVector& cv);
  ComplexMatrix (const ComplexDiagMatrix& a);

  ComplexMatrix (const charMatrix& a);

  ComplexMatrix& operator = (const ComplexMatrix& a)
    {
      MArray2<Complex>::operator = (a);
      return *this;
    }

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
  ComplexMatrix inverse (int& info, double& rcond, int force = 0) const;

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

  ComplexMatrix expm (void) const;

  // column vector by row vector -> matrix operations

  friend ComplexMatrix operator * (const ColumnVector& a,
				   const ComplexRowVector& b);

  friend ComplexMatrix operator * (const ComplexColumnVector& a,
				   const RowVector& b);

  friend ComplexMatrix operator * (const ComplexColumnVector& a,
				   const ComplexRowVector& b);

  // diagonal matrix by scalar -> matrix operations

  friend ComplexMatrix operator + (const DiagMatrix& a, const Complex& s);
  friend ComplexMatrix operator - (const DiagMatrix& a, const Complex& s);

  friend ComplexMatrix operator + (const ComplexDiagMatrix& a, double s);
  friend ComplexMatrix operator - (const ComplexDiagMatrix& a, double s);

  friend ComplexMatrix operator + (const ComplexDiagMatrix& a,
				   const Complex& s);
  friend ComplexMatrix operator - (const ComplexDiagMatrix& a,
				   const Complex& s);

  // scalar by diagonal matrix -> matrix operations

  friend ComplexMatrix operator + (const Complex& s, const DiagMatrix& a);
  friend ComplexMatrix operator - (const Complex& s, const DiagMatrix& a);

  friend ComplexMatrix operator + (double s, const ComplexDiagMatrix& a);
  friend ComplexMatrix operator - (double s, const ComplexDiagMatrix& a);

  friend ComplexMatrix operator + (const Complex& s,
				   const ComplexDiagMatrix& a);
  friend ComplexMatrix operator - (const Complex& s,
				   const ComplexDiagMatrix& a);

  // matrix by diagonal matrix -> matrix operations

  ComplexMatrix& operator += (const DiagMatrix& a);
  ComplexMatrix& operator -= (const DiagMatrix& a);

  ComplexMatrix& operator += (const ComplexDiagMatrix& a);
  ComplexMatrix& operator -= (const ComplexDiagMatrix& a);

  friend ComplexMatrix operator + (const Matrix& a,
				   const ComplexDiagMatrix& b); 
  friend ComplexMatrix operator - (const Matrix& a,
				   const ComplexDiagMatrix& b);
  friend ComplexMatrix operator * (const Matrix& a,
				   const ComplexDiagMatrix& b);

  // diagonal matrix by matrix -> matrix operations

  friend ComplexMatrix operator + (const DiagMatrix& a,
				   const ComplexMatrix& b);
  friend ComplexMatrix operator - (const DiagMatrix& a,
				   const ComplexMatrix& b);
  friend ComplexMatrix operator * (const DiagMatrix& a,
				   const ComplexMatrix& b);

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

  // matrix by matrix -> matrix operations

  ComplexMatrix& operator += (const Matrix& a);
  ComplexMatrix& operator -= (const Matrix& a);

  ComplexMatrix& operator += (const ComplexMatrix& a);
  ComplexMatrix& operator -= (const ComplexMatrix& a);

  // unary operations

  Matrix operator ! (void) const;

  // matrix by scalar -> matrix operations

  friend ComplexMatrix operator + (const Matrix& a, const Complex& s);
  friend ComplexMatrix operator - (const Matrix& a, const Complex& s);
  friend ComplexMatrix operator * (const Matrix& a, const Complex& s);
  friend ComplexMatrix operator / (const Matrix& a, const Complex& s);

  friend ComplexMatrix operator + (const ComplexMatrix& a, double s);
  friend ComplexMatrix operator - (const ComplexMatrix& a, double s);
  friend ComplexMatrix operator * (const ComplexMatrix& a, double s);
  friend ComplexMatrix operator / (const ComplexMatrix& a, double s);

  // scalar by matrix -> matrix operations

  friend ComplexMatrix operator + (double s, const ComplexMatrix& a);
  friend ComplexMatrix operator - (double s, const ComplexMatrix& a);
  friend ComplexMatrix operator * (double s, const ComplexMatrix& a);
  friend ComplexMatrix operator / (double s, const ComplexMatrix& a);

  friend ComplexMatrix operator + (const Complex& s, const Matrix& a);
  friend ComplexMatrix operator - (const Complex& s, const Matrix& a);
  friend ComplexMatrix operator * (const Complex& s, const Matrix& a);
  friend ComplexMatrix operator / (const Complex& s, const Matrix& a);

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

  friend ComplexMatrix operator + (const Matrix& a, const ComplexMatrix& b);
  friend ComplexMatrix operator - (const Matrix& a, const ComplexMatrix& b);

  friend ComplexMatrix operator * (const ComplexMatrix& a, const Matrix& b);

  friend ComplexMatrix operator * (const Matrix& a, const ComplexMatrix& b);

  friend ComplexMatrix operator * (const ComplexMatrix& a,
				   const ComplexMatrix& b);

  friend ComplexMatrix product (const ComplexMatrix& a, const Matrix& b);
  friend ComplexMatrix quotient (const ComplexMatrix& a, const Matrix& b);

  friend ComplexMatrix product (const Matrix& a, const ComplexMatrix& b);
  friend ComplexMatrix quotient (const Matrix& a, const ComplexMatrix& b);

  // other operations

  friend ComplexMatrix map (c_c_Mapper f, const ComplexMatrix& a);
  void map (c_c_Mapper f);

  int all_integers (double& max_val, double& min_val) const;
  int too_large_for_float (void) const;

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

private:

  ComplexMatrix (Complex *d, int r, int c) : MArray2<Complex> (d, r, c) { }
};

ComplexMatrix Givens (const Complex&, const Complex&);

ComplexMatrix Sylvester (const ComplexMatrix&, const ComplexMatrix&,
			 const ComplexMatrix&);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
