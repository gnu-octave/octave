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

#if !defined (octave_Matrix_int_h)
#define octave_Matrix_int_h 1

#if defined (__GNUG__)
#pragma interface
#endif

#include "Array.h"

#include "mx-defs.h"

extern "C++" {

class Matrix : public Array2<double>
{
friend class LU;
friend class SVD;

public:

  Matrix (void) : Array2<double> () { }
  Matrix (int r, int c) : Array2<double> (r, c) { }
  Matrix (int r, int c, double val) : Array2<double> (r, c, val) { }
  Matrix (const Array2<double>& a) : Array2<double> (a) { }
  Matrix (const Matrix& a) : Array2<double> (a) { }
  Matrix (const DiagArray<double>& a) : Array2<double> (a) { }
  Matrix (const DiagMatrix& a);
//  Matrix (double a) : Array2<double> (1, 1, a) { }

  Matrix& operator = (const Matrix& a)
    {
      Array2<double>::operator = (a);
      return *this;
    }

  int operator == (const Matrix& a) const;
  int operator != (const Matrix& a) const;

// destructive insert/delete/reorder operations

  Matrix& insert (const Matrix& a, int r, int c);
  Matrix& insert (const RowVector& a, int r, int c);
  Matrix& insert (const ColumnVector& a, int r, int c);
  Matrix& insert (const DiagMatrix& a, int r, int c);

  Matrix& fill (double val);
  Matrix& fill (double val, int r1, int c1, int r2, int c2);

  Matrix append (const Matrix& a) const;
  Matrix append (const RowVector& a) const;
  Matrix append (const ColumnVector& a) const;
  Matrix append (const DiagMatrix& a) const;

  Matrix stack (const Matrix& a) const;
  Matrix stack (const RowVector& a) const;
  Matrix stack (const ColumnVector& a) const;
  Matrix stack (const DiagMatrix& a) const;

  Matrix transpose (void) const;

// resize is the destructive equivalent for this one

  Matrix extract (int r1, int c1, int r2, int c2) const;

// extract row or column i.

  RowVector row (int i) const;
  RowVector row (char *s) const;

  ColumnVector column (int i) const;
  ColumnVector column (char *s) const;

  Matrix inverse (void) const;
  Matrix inverse (int& info) const;
  Matrix inverse (int& info, double& rcond) const;

  ComplexMatrix fourier (void) const;
  ComplexMatrix ifourier (void) const;

  DET determinant (void) const;
  DET determinant (int& info) const;
  DET determinant (int& info, double& rcond) const;

  Matrix solve (const Matrix& b) const;
  Matrix solve (const Matrix& b, int& info) const;
  Matrix solve (const Matrix& b, int& info, double& rcond) const;

  ComplexMatrix solve (const ComplexMatrix& b) const;
  ComplexMatrix solve (const ComplexMatrix& b, int& info) const;
  ComplexMatrix solve (const ComplexMatrix& b, int& info, double& rcond) const;

  ColumnVector solve (const ColumnVector& b) const;
  ColumnVector solve (const ColumnVector& b, int& info) const;
  ColumnVector solve (const ColumnVector& b, int& info, double& rcond) const;

  ComplexColumnVector solve (const ComplexColumnVector& b) const;
  ComplexColumnVector solve (const ComplexColumnVector& b, int& info) const;
  ComplexColumnVector solve (const ComplexColumnVector& b, int& info,
			     double& rcond) const;

  Matrix lssolve (const Matrix& b) const;
  Matrix lssolve (const Matrix& b, int& info) const;
  Matrix lssolve (const Matrix& b, int& info, int& rank) const;

  ComplexMatrix lssolve (const ComplexMatrix& b) const;
  ComplexMatrix lssolve (const ComplexMatrix& b, int& info) const;
  ComplexMatrix lssolve (const ComplexMatrix& b, int& info,
			 int& rank) const;

  ColumnVector lssolve (const ColumnVector& b) const;
  ColumnVector lssolve (const ColumnVector& b, int& info) const;
  ColumnVector lssolve (const ColumnVector& b, int& info, int& rank) const;

  ComplexColumnVector lssolve (const ComplexColumnVector& b) const;
  ComplexColumnVector lssolve (const ComplexColumnVector& b, int& info) const;
  ComplexColumnVector lssolve (const ComplexColumnVector& b, int& info,
			       int& rank) const;

  Matrix& operator += (const Matrix& a);
  Matrix& operator -= (const Matrix& a);

  Matrix& operator += (const DiagMatrix& a);
  Matrix& operator -= (const DiagMatrix& a);

// unary operations

  Matrix operator ! (void) const;

// matrix by scalar -> matrix operations

  friend ComplexMatrix operator + (const Matrix& a, const Complex& s);
  friend ComplexMatrix operator - (const Matrix& a, const Complex& s);
  friend ComplexMatrix operator * (const Matrix& a, const Complex& s);
  friend ComplexMatrix operator / (const Matrix& a, const Complex& s);

// scalar by matrix -> matrix operations

  friend ComplexMatrix operator + (const Complex& s, const Matrix& a);
  friend ComplexMatrix operator - (const Complex& s, const Matrix& a);
  friend ComplexMatrix operator * (const Complex& s, const Matrix& a);
  friend ComplexMatrix operator / (const Complex& s, const Matrix& a);

// matrix by column vector -> column vector operations

  friend ColumnVector operator * (const Matrix& a, const ColumnVector& b);
  friend ComplexColumnVector operator * (const Matrix& a,
					 const ComplexColumnVector& b);

// matrix by diagonal matrix -> matrix operations

  friend Matrix operator + (const Matrix& a, const DiagMatrix& b);
  friend Matrix operator - (const Matrix& a, const DiagMatrix& b);
  friend Matrix operator * (const Matrix& a, const DiagMatrix& b);

  friend ComplexMatrix operator + (const Matrix& a,
				   const ComplexDiagMatrix& b); 
  friend ComplexMatrix operator - (const Matrix& a,
				   const ComplexDiagMatrix& b);
  friend ComplexMatrix operator * (const Matrix& a,
				   const ComplexDiagMatrix& b);

// matrix by matrix -> matrix operations

  friend Matrix operator * (const Matrix& a, const Matrix& b);
  friend ComplexMatrix operator * (const Matrix& a, const ComplexMatrix& b);

  friend ComplexMatrix operator + (const Matrix& a, const ComplexMatrix& b);
  friend ComplexMatrix operator - (const Matrix& a, const ComplexMatrix& b);

  friend ComplexMatrix product (const Matrix& a, const ComplexMatrix& b);
  friend ComplexMatrix quotient (const Matrix& a, const ComplexMatrix& b);

// other operations

  friend Matrix map (d_d_Mapper f, const Matrix& a);
  void map (d_d_Mapper f);

  Matrix all (void) const;
  Matrix any (void) const;

  Matrix cumprod (void) const;
  Matrix cumsum (void) const;
  Matrix prod (void) const;
  Matrix sum (void) const;
  Matrix sumsq (void) const;

  ColumnVector diag (void) const;
  ColumnVector diag (int k) const;

  ColumnVector row_min (void) const;
  ColumnVector row_min_loc (void) const;

  ColumnVector row_max (void) const;
  ColumnVector row_max_loc (void) const;

  RowVector column_min (void) const;
  RowVector column_min_loc (void) const;

  RowVector column_max (void) const;
  RowVector column_max_loc (void) const;

// i/o

  friend ostream& operator << (ostream& os, const Matrix& a);
  friend istream& operator >> (istream& is, Matrix& a);

  int read (FILE *fptr, char *type);
  int write (FILE *fptr, char *type);

// Until templates really work with g++:

#define KLUDGE_MATRICES
#define TYPE double
#define KL_MAT_TYPE Matrix
#include "mx-kludge.h"
#undef KLUDGE_MATRICES
#undef TYPE
#undef KL_MAT_TYPE

private:

  Matrix (double *d, int r, int c) : Array2<double> (d, r, c) { }
};

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
