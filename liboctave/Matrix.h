// Matrix manipulations.                                 -*- C++ -*-
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

/*

Should probably say something here about why these classes are not
represented by some sort of inheritance tree...

*/

#if !defined (octave_Matrix_h)
#define octave_Matrix_h 1

#include <stdlib.h>
#include <stddef.h>
#include <math.h>
#include <assert.h>
#include <iostream.h>
// #include <iomanip.h>  // We don\'t use this yet.
#include <Complex.h>

extern "C++" {

class ostream;
class istream;

#ifndef MAPPER_FCN_TYPEDEFS
#define MAPPER_FCN_TYPEDEFS 1

typedef double (*d_d_Mapper)(double);
typedef double (*d_c_Mapper)(const Complex&);
typedef Complex (*c_c_Mapper)(const Complex&);

#endif

#include "Array.h"

// Classes we declare.

class Matrix;
class ColumnVector;
class RowVector;
class DiagMatrix;
class ComplexMatrix;
class ComplexColumnVector;
class ComplexRowVector;
class ComplexDiagMatrix;
class AEPBALANCE;
class ComplexAEPBALANCE;
class GEPBALANCE;
class CHOL;
class ComplexCHOL;
class DET;
class ComplexDET;
class EIG;
class HESS;
class ComplexHESS;
class SCHUR;
class ComplexSCHUR;
class SVD;
class ComplexSVD;
class LU;
class ComplexLU;
class QR;
class ComplexQR;

/*
 * Matrix class
 */

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

/*
 * Column Vector class
 */

class ColumnVector : public Array<double>
{
friend class RowVector;

public:

  ColumnVector (void) : Array<double> () { }
  ColumnVector (int n) : Array<double> (n) { }
  ColumnVector (int n, double val) : Array<double> (n, val) { }
  ColumnVector (const Array<double>& a) : Array<double> (a) { }
  ColumnVector (const ColumnVector& a) : Array<double> (a) { }
//  ColumnVector (double a) : Array<double> (1, a) { }

  ColumnVector& operator = (const ColumnVector& a)
    {
      Array<double>::operator = (a);
      return *this;
    }

//  operator Array<double>& () const { return *this; }

  int operator == (const ColumnVector& a) const;
  int operator != (const ColumnVector& a) const;

// destructive insert/delete/reorder operations

  ColumnVector& insert (const ColumnVector& a, int r);

  ColumnVector& fill (double val);
  ColumnVector& fill (double val, int r1, int r2);

  ColumnVector stack (const ColumnVector& a) const;

  RowVector transpose (void) const;

// resize is the destructive equivalent for this one

  ColumnVector extract (int r1, int r2) const;

// column vector by column vector -> column vector operations

  ColumnVector& operator += (const ColumnVector& a);
  ColumnVector& operator -= (const ColumnVector& a);

// column vector by scalar -> column vector operations

  friend ComplexColumnVector operator + (const ColumnVector& a,
					 const Complex& s);  
  friend ComplexColumnVector operator - (const ColumnVector& a,
					 const Complex& s);
  friend ComplexColumnVector operator * (const ColumnVector& a,
					 const Complex& s);
  friend ComplexColumnVector operator / (const ColumnVector& a,
					 const Complex& s);

// scalar by column vector -> column vector operations

  friend ComplexColumnVector operator + (const Complex& s,
					 const ColumnVector& a); 
  friend ComplexColumnVector operator - (const Complex& s,
					 const ColumnVector& a);
  friend ComplexColumnVector operator * (const Complex& s,
					 const ColumnVector& a);
  friend ComplexColumnVector operator / (const Complex& s,
					 const ColumnVector& a);

// column vector by row vector -> matrix operations

  friend Matrix operator * (const ColumnVector& a, const RowVector& a);

  friend ComplexMatrix operator * (const ColumnVector& a,
				   const ComplexRowVector& b);

// column vector by column vector -> column vector operations

  friend ComplexColumnVector operator + (const ComplexColumnVector& a,
					 const ComplexColumnVector& b);

  friend ComplexColumnVector operator - (const ComplexColumnVector& a,
					 const ComplexColumnVector& b); 

  friend ComplexColumnVector product (const ComplexColumnVector& a,
				      const ComplexColumnVector& b); 

  friend ComplexColumnVector quotient (const ComplexColumnVector& a,
				       const ComplexColumnVector& b); 

// other operations

  friend ColumnVector map (d_d_Mapper f, const ColumnVector& a);
  void map (d_d_Mapper f);

  double min (void) const;
  double max (void) const;

// i/o

  friend ostream& operator << (ostream& os, const ColumnVector& a);
  friend ostream& operator >> (ostream& is, ColumnVector& a);

#define KLUDGE_VECTORS
#define TYPE double
#define KL_VEC_TYPE ColumnVector
#include "mx-kludge.h"
#undef KLUDGE_VECTORS
#undef TYPE
#undef KL_VEC_TYPE

private:

  ColumnVector (double *d, int l) : Array<double> (d, l) { }
};

/*
 * Row Vector class
 */

class RowVector : public Array<double>
{
friend class ColumnVector;

public:

  RowVector (void) : Array<double> () { }
  RowVector (int n) : Array<double> (n) { }
  RowVector (int n, double val) : Array<double> (n, val) { }
  RowVector (const Array<double>& a) : Array<double> (a) { }
  RowVector (const RowVector& a) : Array<double> (a) { }
//  RowVector (double a) : Array<double> (1, a) { }

  RowVector& operator = (const RowVector& a)
    {
      Array<double>::operator = (a);
      return *this;
    }

//  operator Array<double>& () const { return *this; }

  int operator == (const RowVector& a) const;
  int operator != (const RowVector& a) const;

// destructive insert/delete/reorder operations

  RowVector& insert (const RowVector& a, int c);

  RowVector& fill (double val);
  RowVector& fill (double val, int c1, int c2);

  RowVector append (const RowVector& a) const;

  ColumnVector transpose (void) const;

// resize is the destructive equivalent for this one

  RowVector extract (int c1, int c2) const;

// row vector by row vector -> row vector operations

  RowVector& operator += (const RowVector& a);
  RowVector& operator -= (const RowVector& a);

// row vector by scalar -> row vector operations

  friend ComplexRowVector operator + (const RowVector& a, const Complex& s);
  friend ComplexRowVector operator - (const RowVector& a, const Complex& s);
  friend ComplexRowVector operator * (const RowVector& a, const Complex& s);
  friend ComplexRowVector operator / (const RowVector& a, const Complex& s);

// scalar by row vector -> row vector operations

  friend ComplexRowVector operator + (const Complex& s, const RowVector& a);
  friend ComplexRowVector operator - (const Complex& s, const RowVector& a);
  friend ComplexRowVector operator * (const Complex& s, const RowVector& a);
  friend ComplexRowVector operator / (const Complex& s, const RowVector& a);

// row vector by column vector -> scalar

  friend double operator * (const RowVector& a, const ColumnVector& b);

  friend Complex operator * (const RowVector& a, const ComplexColumnVector& b);

// row vector by matrix -> row vector

  friend RowVector operator * (const RowVector& a, const Matrix& b);

  friend ComplexRowVector operator * (const RowVector& a,
				      const ComplexMatrix& b);

// row vector by row vector -> row vector operations

  friend ComplexRowVector operator + (const RowVector& a,
				      const ComplexRowVector& b);
  friend ComplexRowVector operator - (const RowVector& a,
				      const ComplexRowVector& b);

  friend ComplexRowVector product (const RowVector& a,
				   const ComplexRowVector& b);
  friend ComplexRowVector quotient (const RowVector& a,
				    const ComplexRowVector& b);

// other operations

  friend RowVector map (d_d_Mapper f, const RowVector& a);
  void map (d_d_Mapper f);

  double min (void) const;
  double max (void) const;

// i/o

  friend ostream& operator << (ostream& os, const RowVector& a);
  friend ostream& operator >> (ostream& is, RowVector& a);

#define KLUDGE_VECTORS
#define TYPE double
#define KL_VEC_TYPE RowVector
#include "mx-kludge.h"
#undef KLUDGE_VECTORS
#undef TYPE
#undef KL_VEC_TYPE

private:

  RowVector (double *d, int l) : Array<double> (d, l) { }
};

/*
 * Diagonal Matrix class
 */

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

/*
 * Complex Matrix class
 */

class ComplexMatrix : public Array2<Complex>
{
friend class ComplexLU;
friend class ComplexSVD;

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

  ComplexMatrix fourier (void) const;
  ComplexMatrix ifourier (void) const;

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

/*
 * Complex Column Vector class
 */

class ComplexColumnVector : public Array<Complex>
{
friend class ComplexRowVector;

public:

  ComplexColumnVector (void) : Array<Complex> () { }
  ComplexColumnVector (int n) : Array<Complex> (n) { }
  ComplexColumnVector (int n, const Complex& val)
    : Array<Complex> (n, val) { }
  ComplexColumnVector (const ColumnVector& a);
  ComplexColumnVector (const Array<Complex>& a) : Array<Complex> (a) { }
  ComplexColumnVector (const ComplexColumnVector& a) : Array<Complex> (a) { }
//  ComplexColumnVector (double a) : Array<Complex> (1, a) { }
//  ComplexColumnVector (const Complex& a) : Array<Complex> (1, a) { }

  ComplexColumnVector& operator = (const ComplexColumnVector& a)
    {
      Array<Complex>::operator = (a);
      return *this;
    }

//  operator Array<Complex>& () const { return *this; }

  int operator == (const ComplexColumnVector& a) const;
  int operator != (const ComplexColumnVector& a) const;

// destructive insert/delete/reorder operations

  ComplexColumnVector& insert (const ColumnVector& a, int r);
  ComplexColumnVector& insert (const ComplexColumnVector& a, int r);

  ComplexColumnVector& fill (double val);
  ComplexColumnVector& fill (const Complex& val);
  ComplexColumnVector& fill (double val, int r1, int r2);
  ComplexColumnVector& fill (const Complex& val, int r1, int r2);

  ComplexColumnVector stack (const ColumnVector& a) const;
  ComplexColumnVector stack (const ComplexColumnVector& a) const;

  ComplexRowVector hermitian (void) const;  // complex conjugate transpose.
  ComplexRowVector transpose (void) const;

  friend ColumnVector real (const ComplexColumnVector& a);
  friend ColumnVector imag (const ComplexColumnVector& a);
  friend ComplexColumnVector conj (const ComplexColumnVector& a);

// resize is the destructive equivalent for this one

  ComplexColumnVector extract (int r1, int r2) const;

// column vector by column vector -> column vector operations

  ComplexColumnVector& operator += (const ColumnVector& a);
  ComplexColumnVector& operator -= (const ColumnVector& a);

  ComplexColumnVector& operator += (const ComplexColumnVector& a);
  ComplexColumnVector& operator -= (const ComplexColumnVector& a);

// column vector by scalar -> column vector operations

  friend ComplexColumnVector operator + (const ComplexColumnVector& a,
					 double s);
  friend ComplexColumnVector operator - (const ComplexColumnVector& a,
					 double s);
  friend ComplexColumnVector operator * (const ComplexColumnVector& a,
					 double s);
  friend ComplexColumnVector operator / (const ComplexColumnVector& a,
					 double s);

// scalar by column vector -> column vector operations

  friend ComplexColumnVector operator + (double s,
					 const ComplexColumnVector& a); 
  friend ComplexColumnVector operator - (double s,
					 const ComplexColumnVector& a);
  friend ComplexColumnVector operator * (double s,
					 const ComplexColumnVector& a);
  friend ComplexColumnVector operator / (double s,
					 const ComplexColumnVector& a);

// column vector by row vector -> matrix operations

  friend ComplexMatrix operator * (const ComplexColumnVector& a,
				   const ComplexRowVector& b);

// column vector by column vector -> column vector operations

  friend ComplexColumnVector operator + (const ComplexColumnVector& a,
					 const ColumnVector& b);
  friend ComplexColumnVector operator - (const ComplexColumnVector& a,
					 const ColumnVector& b);

  friend ComplexColumnVector product (const ComplexColumnVector& a,
				      const ColumnVector& b);
  friend ComplexColumnVector quotient (const ComplexColumnVector& a,
				       const ColumnVector& b);

// other operations

  friend ComplexColumnVector map (c_c_Mapper f, const ComplexColumnVector& a);
  friend ColumnVector map (d_c_Mapper f, const ComplexColumnVector& a);
  void map (c_c_Mapper f);

  Complex min (void) const;
  Complex max (void) const;

// i/o

  friend ostream& operator << (ostream& os, const ComplexColumnVector& a);
  friend ostream& operator >> (ostream& is, ComplexColumnVector& a);

#define KLUDGE_VECTORS
#define TYPE Complex
#define KL_VEC_TYPE ComplexColumnVector
#include "mx-kludge.h"
#undef KLUDGE_VECTORS
#undef TYPE
#undef KL_VEC_TYPE

private:

  ComplexColumnVector (Complex *d, int l) : Array<Complex> (d, l) { }
};

/*
 * Complex Row Vector class
 */

class ComplexRowVector : public Array<Complex>
{
friend class ComplexColumnVector;

public:

  ComplexRowVector (void) : Array<Complex> () { }
  ComplexRowVector (int n) : Array<Complex> (n) { }
  ComplexRowVector (int n, const Complex& val) : Array<Complex> (n, val) { }
  ComplexRowVector (const RowVector& a);
  ComplexRowVector (const Array<Complex>& a) : Array<Complex> (a) { }
  ComplexRowVector (const ComplexRowVector& a) : Array<Complex> (a) { }
//  ComplexRowVector (double a) : Array<Complex> (1, a) { }
//  ComplexRowVector (const Complex& a) : Array<Complex> (1, a) { }

  ComplexRowVector& operator = (const ComplexRowVector& a)
    {
      Array<Complex>::operator = (a);
      return *this;
    }

//  operator Array<Complex>& () const { return *this; }

  int operator == (const ComplexRowVector& a) const;
  int operator != (const ComplexRowVector& a) const;

// destructive insert/delete/reorder operations

  ComplexRowVector& insert (const RowVector& a, int c);
  ComplexRowVector& insert (const ComplexRowVector& a, int c);

  ComplexRowVector& fill (double val);
  ComplexRowVector& fill (const Complex& val);
  ComplexRowVector& fill (double val, int c1, int c2);
  ComplexRowVector& fill (const Complex& val, int c1, int c2);

  ComplexRowVector append (const RowVector& a) const;
  ComplexRowVector append (const ComplexRowVector& a) const;

  ComplexColumnVector hermitian (void) const;  // complex conjugate transpose.
  ComplexColumnVector transpose (void) const;

  friend RowVector real (const ComplexRowVector& a);
  friend RowVector imag (const ComplexRowVector& a);
  friend ComplexRowVector conj (const ComplexRowVector& a);

// resize is the destructive equivalent for this one

  ComplexRowVector extract (int c1, int c2) const;

// row vector by row vector -> row vector operations

  ComplexRowVector& operator += (const RowVector& a);
  ComplexRowVector& operator -= (const RowVector& a);

  ComplexRowVector& operator += (const ComplexRowVector& a);
  ComplexRowVector& operator -= (const ComplexRowVector& a);

// row vector by scalar -> row vector operations

  friend ComplexRowVector operator + (const ComplexRowVector& a, double s);
  friend ComplexRowVector operator - (const ComplexRowVector& a, double s);
  friend ComplexRowVector operator * (const ComplexRowVector& a, double s);
  friend ComplexRowVector operator / (const ComplexRowVector& a, double s);

// scalar by row vector -> row vector operations

  friend ComplexRowVector operator + (double s, const ComplexRowVector& a);
  friend ComplexRowVector operator - (double s, const ComplexRowVector& a);
  friend ComplexRowVector operator * (double s, const ComplexRowVector& a);
  friend ComplexRowVector operator / (double s, const ComplexRowVector& a);

// row vector by column vector -> scalar

  friend Complex operator * (const ComplexRowVector& a, const ColumnVector& b);

  friend Complex operator * (const ComplexRowVector& a,
			     const ComplexColumnVector& b);

// row vector by matrix -> row vector

  friend ComplexRowVector operator * (const ComplexRowVector& a,
				      const ComplexMatrix& b);

// row vector by row vector -> row vector operations

  friend ComplexRowVector operator + (const ComplexRowVector& a,
				      const RowVector& b);
  friend ComplexRowVector operator - (const ComplexRowVector& a,
				      const RowVector& b);

  friend ComplexRowVector product (const ComplexRowVector& a,
				   const RowVector& b);
  friend ComplexRowVector quotient (const ComplexRowVector& a,
				    const RowVector& b);

// other operations

  friend ComplexRowVector map (c_c_Mapper f, const ComplexRowVector& a);
  friend RowVector map (d_c_Mapper f, const ComplexRowVector& a);
  void map (c_c_Mapper f);

  Complex min (void) const;
  Complex max (void) const;

// i/o

  friend ostream& operator << (ostream& os, const ComplexRowVector& a);
  friend ostream& operator >> (ostream& is, ComplexRowVector& a);

#define KLUDGE_VECTORS
#define TYPE Complex
#define KL_VEC_TYPE ComplexRowVector
#include "mx-kludge.h"
#undef KLUDGE_VECTORS
#undef TYPE
#undef KL_VEC_TYPE

private:

  ComplexRowVector (Complex *d, int l) : Array<Complex> (d, l) { }
};

/*
 * Complex Diagonal Matrix class
 */

class ComplexDiagMatrix : public DiagArray<Complex>
{
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

/*
 * Result of a AEP Balance operation
 */

class AEPBALANCE
{
friend class Matrix;

public:

  AEPBALANCE (void) {}

  AEPBALANCE (const Matrix& a, const char *balance_job);

  AEPBALANCE (const AEPBALANCE& a);

  AEPBALANCE& operator = (const AEPBALANCE& a);
  Matrix balanced_matrix (void) const;
  Matrix balancing_matrix (void) const;
  friend ostream& operator << (ostream& os, const AEPBALANCE& a);

private:

  int init (const Matrix& a, const char * balance_job);

  Matrix balanced_mat;
  Matrix balancing_mat;
};

inline AEPBALANCE::AEPBALANCE (const Matrix& a,const char * balance_job) 
{
  init (a, balance_job); 
}

inline AEPBALANCE::AEPBALANCE (const AEPBALANCE& a)
{
  balanced_mat = a.balanced_mat;
  balancing_mat = a.balancing_mat;
}

inline AEPBALANCE&
AEPBALANCE::operator = (const AEPBALANCE& a)
{
  balanced_mat = a.balanced_mat;
  balancing_mat = a.balancing_mat;

  return *this;
}

inline Matrix AEPBALANCE::balanced_matrix (void) const
{
  return balanced_mat;
}

inline Matrix AEPBALANCE::balancing_matrix (void) const
{
  return balancing_mat;
}

/*
 * Result of a Complex balancing operation
 */

class ComplexAEPBALANCE
{
friend class ComplexMatrix;

public:

  ComplexAEPBALANCE (void) {}
  ComplexAEPBALANCE (const ComplexMatrix& a, const char *balance_job);
  ComplexAEPBALANCE (const ComplexAEPBALANCE& a);
  ComplexAEPBALANCE& operator = (const ComplexAEPBALANCE& a);
  ComplexMatrix balanced_matrix (void) const;
  ComplexMatrix balancing_matrix (void) const;

  friend ostream& operator << (ostream& os, const ComplexAEPBALANCE& a);

private:

  int init (const ComplexMatrix& a, const char * balance_job);

  ComplexMatrix balanced_mat;
  ComplexMatrix balancing_mat;
};

inline ComplexAEPBALANCE::ComplexAEPBALANCE (const ComplexMatrix& a,
					     const char * balance_job)
{
  init (a, balance_job); 
}

inline ComplexAEPBALANCE::ComplexAEPBALANCE (const ComplexAEPBALANCE& a)
{
  balanced_mat = a.balanced_mat;
  balancing_mat = a.balancing_mat;
}

inline ComplexAEPBALANCE&
ComplexAEPBALANCE::operator = (const ComplexAEPBALANCE& a)
{
  balanced_mat = a.balanced_mat;
  balancing_mat = a.balancing_mat;

  return *this;
}

inline ComplexMatrix ComplexAEPBALANCE::balanced_matrix (void) const
{
  return balanced_mat;
}

inline ComplexMatrix ComplexAEPBALANCE::balancing_matrix (void) const
{
  return balancing_mat;
}

/*
 * Result of a Determinant calculation.
 */

class DET
{
public:

  DET (void) {}

  DET (const DET& a);

  DET& operator = (const DET& a);

  int value_will_overflow (void) const;
  int value_will_underflow (void) const;
  double coefficient (void) const;
  int exponent (void) const;
  double value (void) const;

  friend ostream&  operator << (ostream& os, const DET& a);

private:

  DET (const double *d);

  double det [2];
};

inline DET::DET (const DET& a)
{
  det[0] = a.det[0];
  det[1] = a.det[1];
}

inline DET& DET::operator = (const DET& a)
{
  det[0] = a.det[0];
  det[1] = a.det[1];
  return *this;
}

inline DET::DET (const double *d)
{
  det[0] = d[0];
  det[1] = d[1];
}

/*
 * Result of a Determinant calculation.
 */

class ComplexDET
{
public:

  ComplexDET (void) {}

  ComplexDET (const ComplexDET& a);

  ComplexDET& operator = (const ComplexDET& a);

  int value_will_overflow (void) const;
  int value_will_underflow (void) const;
  Complex coefficient (void) const;
  int exponent (void) const;
  Complex value (void) const;

  friend ostream&  operator << (ostream& os, const ComplexDET& a);

private:

  ComplexDET (const Complex *d);

  Complex det [2];
};

inline ComplexDET::ComplexDET (const ComplexDET& a)
{
  det[0] = a.det[0];
  det[1] = a.det[1];
}

inline ComplexDET& ComplexDET::operator = (const ComplexDET& a)
{
  det[0] = a.det[0];
  det[1] = a.det[1];
  return *this;
}

inline ComplexDET::ComplexDET (const Complex *d)
{
  det[0] = d[0];
  det[1] = d[1];
}

/*
 * Result of a GEP Balance operation
 * Note: currenlty only do balancing on real data.  Complex balancing
 * done on magnitudes of complex data.
 */

class GEPBALANCE
{
friend class Matrix;

public:

  GEPBALANCE (void) {}

  GEPBALANCE (const Matrix& a, const Matrix &, const char *balance_job);

  GEPBALANCE (const GEPBALANCE& a);

  GEPBALANCE& operator = (const GEPBALANCE& a);
  Matrix balanced_a_matrix (void) const;
  Matrix balanced_b_matrix (void) const;
  Matrix left_balancing_matrix (void) const;
  Matrix right_balancing_matrix (void) const;
  friend ostream& operator << (ostream& os, const GEPBALANCE& a);

private:

  int init (const Matrix& a, const Matrix& b, const char * balance_job);

  Matrix balanced_a_mat;
  Matrix balanced_b_mat;
  Matrix left_balancing_mat;
  Matrix right_balancing_mat;
};

inline GEPBALANCE::GEPBALANCE (const Matrix& a, const Matrix& b, 
			       const char * balance_job) 
{
  init (a, b, balance_job); 
}

inline GEPBALANCE::GEPBALANCE (const GEPBALANCE& a)
{
  balanced_a_mat = a.balanced_a_mat;
  balanced_b_mat = a.balanced_b_mat;
  left_balancing_mat = a.left_balancing_mat;
  right_balancing_mat = a.right_balancing_mat;
}

inline GEPBALANCE&
GEPBALANCE::operator = (const GEPBALANCE& a)
{
  balanced_a_mat = a.balanced_a_mat;
  balanced_b_mat = a.balanced_b_mat;
  left_balancing_mat = a.left_balancing_mat;
  right_balancing_mat = a.right_balancing_mat;

  return *this;
}

inline Matrix GEPBALANCE::balanced_a_matrix (void) const 
{
  return balanced_a_mat;
}

inline Matrix GEPBALANCE::balanced_b_matrix (void) const 
{
  return balanced_b_mat;
}

inline Matrix GEPBALANCE::left_balancing_matrix (void) const 
{
  return left_balancing_mat;
}

inline Matrix GEPBALANCE::right_balancing_matrix (void) const 
{
  return right_balancing_mat;
}

/*
 * Result of a Cholesky Factorization
 */

class CHOL
{
friend class Matrix;

public:

  CHOL (void) {}

  CHOL (const Matrix& a);
  CHOL (const Matrix& a, int& info);

  CHOL (const CHOL& a);

  CHOL& operator = (const CHOL& a);
  Matrix chol_matrix (void) const;
  friend ostream& operator << (ostream& os, const CHOL& a);

private:

  int init (const Matrix& a);

  Matrix chol_mat;
};

inline CHOL::CHOL (const Matrix& a)
{
  init (a);
}

inline CHOL::CHOL (const Matrix& a, int& info)
{
  info = init (a);
}

inline CHOL::CHOL (const CHOL& a)
{
  chol_mat = a.chol_mat;
}

inline CHOL&
CHOL::operator = (const CHOL& a)
{
  chol_mat = a.chol_mat;

  return *this;
}

inline Matrix CHOL::chol_matrix (void) const { return chol_mat; }

/*
 * Result of a Cholesky Factorization
 */

class ComplexCHOL
{
friend class ComplexMatrix;

public:

  ComplexCHOL (void) {}
  ComplexCHOL (const ComplexMatrix& a);
  ComplexCHOL (const ComplexMatrix& a, int& info);
  ComplexCHOL (const ComplexCHOL& a);
  ComplexCHOL& operator = (const ComplexCHOL& a);
  ComplexMatrix chol_matrix (void) const;

  friend ostream& operator << (ostream& os, const ComplexCHOL& a);

private:

  int init (const ComplexMatrix& a);

  ComplexMatrix chol_mat;
};

inline ComplexCHOL::ComplexCHOL (const ComplexMatrix& a)
{
  init (a);
}

inline ComplexCHOL::ComplexCHOL (const ComplexMatrix& a, int& info)
{
  info = init (a);
}

inline ComplexCHOL::ComplexCHOL (const ComplexCHOL& a)
{
  chol_mat = a.chol_mat;
}

inline ComplexCHOL&
ComplexCHOL::operator = (const ComplexCHOL& a)
{
  chol_mat = a.chol_mat;

  return *this;
}

inline ComplexMatrix ComplexCHOL::chol_matrix (void) const
{
  return chol_mat;
}

  
/*
 * Result of a Hessenberg Decomposition
 */

class HESS
{
friend class Matrix;

public:

  HESS (void) {}

  HESS (const Matrix& a);
  HESS (const Matrix&a, int& info);

  HESS (const HESS& a);

  HESS& operator = (const HESS& a);
  Matrix hess_matrix (void) const;
  Matrix unitary_hess_matrix (void) const;
  friend ostream& operator << (ostream& os, const HESS& a);

private:

  int init (const Matrix& a);

  Matrix hess_mat;
  Matrix unitary_hess_mat;
};

inline HESS::HESS (const Matrix& a)
{
  init (a);
}

inline HESS::HESS (const Matrix& a, int& info)
{
  info = init (a);
}

inline HESS::HESS (const HESS& a)
{
  hess_mat = a.hess_mat;
  unitary_hess_mat = a.unitary_hess_mat;
}

inline HESS&
HESS::operator = (const HESS& a)
{
  hess_mat = a.hess_mat;
  unitary_hess_mat = a.unitary_hess_mat;

  return *this;
}

inline Matrix HESS::hess_matrix (void) const
{
  return hess_mat;
}

inline Matrix HESS::unitary_hess_matrix (void) const
{
  return unitary_hess_mat;
}

/*
 * Result of a Hessenberg Decomposition
 */

class ComplexHESS
{
friend class ComplexMatrix;

public:

  ComplexHESS (void) {}
  ComplexHESS (const ComplexMatrix& a);
  ComplexHESS (const ComplexMatrix& a, int& info);
  ComplexHESS (const ComplexHESS& a);
  ComplexHESS& operator = (const ComplexHESS& a);
  ComplexMatrix hess_matrix (void) const;
  ComplexMatrix unitary_hess_matrix (void) const;

  friend ostream& operator << (ostream& os, const ComplexHESS& a);

private:

  int init (const ComplexMatrix& a);

  ComplexMatrix hess_mat;
  ComplexMatrix unitary_hess_mat;
};

inline ComplexHESS::ComplexHESS (const ComplexMatrix& a)
{
  init (a);
}

inline ComplexHESS::ComplexHESS (const ComplexMatrix& a, int& info)
{
  info = init (a);
}

inline ComplexHESS::ComplexHESS (const ComplexHESS& a)
{
  hess_mat = a.hess_mat;
  unitary_hess_mat = a.unitary_hess_mat;
}

inline ComplexHESS&
ComplexHESS::operator = (const ComplexHESS& a)
{
  hess_mat = a.hess_mat;
  unitary_hess_mat = a.unitary_hess_mat;

  return *this;
}

inline ComplexMatrix ComplexHESS::hess_matrix (void) const
{
  return hess_mat;
}

inline ComplexMatrix ComplexHESS::unitary_hess_matrix (void) const
{
  return unitary_hess_mat;
}

/*
 * Result of a Schur Decomposition
 */

class SCHUR
{
friend class Matrix;

public:

  SCHUR (void) {}

  SCHUR (const Matrix& a, const char *ord);
  SCHUR (const Matrix& a, const char *ord, int& info);

  SCHUR (const SCHUR& a, const char *ord);

  SCHUR& operator = (const SCHUR& a);

  Matrix schur_matrix (void) const;
  Matrix unitary_matrix (void) const;

  friend ostream& operator << (ostream& os, const SCHUR& a);

private:

  int init (const Matrix& a, const char *ord);

  Matrix schur_mat;
  Matrix unitary_mat;
};

inline SCHUR::SCHUR (const Matrix& a, const char *ord)
{
  init (a, ord);
}

inline SCHUR::SCHUR (const Matrix& a, const char *ord, int& info) 
{
  info = init (a, ord);
}

inline SCHUR::SCHUR (const SCHUR& a, const char *ord)
{
  schur_mat = a.schur_mat;
  unitary_mat = a.unitary_mat;
}

inline SCHUR&
SCHUR::operator = (const SCHUR& a)
{
  schur_mat = a.schur_mat;
  unitary_mat = a.unitary_mat;
  
  return *this;
}

inline Matrix SCHUR::schur_matrix (void) const
{
  return schur_mat;
}

inline Matrix SCHUR::unitary_matrix (void) const
{
  return unitary_mat;
}

/*
 * Result of a Schur Decomposition
 */

class ComplexSCHUR
{
friend class ComplexMatrix;

public:

  ComplexSCHUR (void) {}

  ComplexSCHUR (const ComplexMatrix& a, const char *ord);
  ComplexSCHUR (const ComplexMatrix& a, const char *ord, int& info);

  ComplexSCHUR (const ComplexSCHUR& a, const char *ord);

  ComplexSCHUR& operator = (const ComplexSCHUR& a);

  ComplexMatrix schur_matrix (void) const;
  ComplexMatrix unitary_matrix (void) const;

  friend ostream& operator << (ostream& os, const ComplexSCHUR& a);

private:

  int init (const ComplexMatrix& a, const char *ord);

  ComplexMatrix schur_mat;
  ComplexMatrix unitary_mat;
};

inline ComplexSCHUR::ComplexSCHUR (const ComplexMatrix& a, const char *ord) 
{
  init (a,ord);
}

inline ComplexSCHUR::ComplexSCHUR (const ComplexMatrix& a, const char *ord,
				   int& info)
{
  info = init (a,ord);
}

inline ComplexSCHUR::ComplexSCHUR (const ComplexSCHUR& a, const char *ord)
{
  schur_mat = a.schur_mat;
  unitary_mat = a.unitary_mat;
}

inline ComplexSCHUR&
ComplexSCHUR::operator = (const ComplexSCHUR& a)
{
  schur_mat = a.schur_mat;
  unitary_mat = a.unitary_mat;

  return *this;
}

inline ComplexMatrix ComplexSCHUR::schur_matrix (void) const
{
  return schur_mat;
}

inline ComplexMatrix ComplexSCHUR::unitary_matrix (void) const
{
  return unitary_mat;
}


/*
 * Result of a Singular Value Decomposition.
 */

class SVD
{
friend class Matrix;

public:

  SVD (void) {}

  SVD (const Matrix& a);
  SVD (const Matrix& a, int& info);

  SVD (const SVD& a);

  SVD& operator = (const SVD& a);

  DiagMatrix singular_values (void) const;
  Matrix left_singular_matrix (void) const;
  Matrix right_singular_matrix (void) const;

  friend ostream&  operator << (ostream& os, const SVD& a);

private:

  int init (const Matrix& a);

  DiagMatrix sigma;
  Matrix left_sm;
  Matrix right_sm;
};

inline SVD::SVD (const Matrix& a)
{
  init (a);
}

inline SVD::SVD (const Matrix& a, int& info)
{
  info = init (a);
}

inline SVD::SVD (const SVD& a)
{
  sigma = a.sigma;
  left_sm = a.left_sm;
  right_sm = a.right_sm;
}

inline SVD&
SVD::operator = (const SVD& a)
{
  sigma = a.sigma;
  left_sm = a.left_sm;
  right_sm = a.right_sm;

  return *this;
}

inline DiagMatrix SVD::singular_values (void) const
{
  return sigma;
}

inline Matrix SVD::left_singular_matrix (void) const
{
  return left_sm;
}

inline Matrix SVD::right_singular_matrix (void) const
{
  return right_sm;
}

/*
 * Result of a Singular Value Decomposition.
 */

class ComplexSVD
{
friend class ComplexMatrix;

public:

  ComplexSVD (void) {}

  ComplexSVD (const ComplexMatrix& a);
  ComplexSVD (const ComplexMatrix& a, int& info);

  ComplexSVD (const ComplexSVD& a);

  ComplexSVD& operator = (const ComplexSVD& a);

  DiagMatrix singular_values (void) const;
  ComplexMatrix left_singular_matrix (void) const;
  ComplexMatrix right_singular_matrix (void) const;

  friend ostream&  operator << (ostream& os, const ComplexSVD& a);

private:

  int init (const ComplexMatrix& a);

  DiagMatrix sigma;
  ComplexMatrix left_sm;
  ComplexMatrix right_sm;
};

inline ComplexSVD::ComplexSVD (const ComplexMatrix& a)
{
  init (a);
}

inline ComplexSVD::ComplexSVD (const ComplexMatrix& a, int& info)
{
  info = init (a);
} 

inline ComplexSVD::ComplexSVD (const ComplexSVD& a)
{
  sigma = a.sigma;
  left_sm = a.left_sm;
  right_sm = a.right_sm;
}

inline ComplexSVD&
ComplexSVD::operator = (const ComplexSVD& a)
{
  sigma = a.sigma;
  left_sm = a.left_sm;
  right_sm = a.right_sm;

  return *this;
}

inline DiagMatrix ComplexSVD::singular_values (void) const
{
  return sigma;
}

inline ComplexMatrix ComplexSVD::left_singular_matrix (void) const
{
  return left_sm;
}

inline ComplexMatrix ComplexSVD::right_singular_matrix (void) const
{
  return right_sm;
}

/*
 * Result of an Eigenvalue computation.
 */

class EIG
{
friend class Matrix;
friend class ComplexMatrix;

public:

  EIG (void) {}

  EIG (const Matrix& a);
  EIG (const Matrix& a, int& info);

  EIG (const ComplexMatrix& a);
  EIG (const ComplexMatrix& a, int& info);

  EIG (const EIG& a);

  EIG& operator = (const EIG& a);

  ComplexColumnVector eigenvalues (void) const;
  ComplexMatrix eigenvectors (void) const;

  friend ostream&  operator << (ostream& os, const EIG& a);

private:

  int init (const Matrix& a);
  int init (const ComplexMatrix& a);

  ComplexColumnVector lambda;
  ComplexMatrix v;
};

inline EIG::EIG (const Matrix& a)
{
  init (a);
}

inline EIG::EIG (const Matrix& a, int& info)
{
  info = init (a);
}

inline EIG::EIG (const ComplexMatrix& a)
{
  init (a);
}

inline EIG::EIG (const ComplexMatrix& a, int& info)
{
  info = init (a);
}

inline EIG::EIG (const EIG& a)
{
  lambda = a.lambda;
  v = a.v;
}

inline EIG& EIG::operator = (const EIG& a)
{
  lambda = a.lambda;
  v = a.v;
  return *this;
}

inline ComplexColumnVector EIG::eigenvalues (void) const
{
  return lambda;
}

inline ComplexMatrix EIG::eigenvectors (void) const
{
  return v;
}

/*
 * Result of an LU decomposition.
 */

class LU
{
friend class Matrix;

public:

  LU (void) {}

  LU (const Matrix& a);

  LU (const LU& a);

  LU& operator = (const LU& a);

  Matrix L (void) const;
  Matrix U (void) const;
  Matrix P (void) const;

  friend ostream&  operator << (ostream& os, const LU& a);

private:

  Matrix l;
  Matrix u;
  Matrix p;
};

inline LU::LU (const LU& a)
{
  l = a.l;
  u = a.u;
  p = a.p;
}

inline LU& LU::operator = (const LU& a)
{
  l = a.l;
  u = a.u;
  p = a.p;
  return *this;
}

inline Matrix LU::L (void) const
{
  return l;
}

inline Matrix LU::U (void) const
{
  return u;
}

inline Matrix LU::P (void) const
{
  return p;
}

class ComplexLU
{
friend class ComplexMatrix;

public:

  ComplexLU (void) {}

  ComplexLU (const ComplexMatrix& a);

  ComplexLU (const ComplexLU& a);

  ComplexLU& operator = (const ComplexLU& a);

  ComplexMatrix L (void) const;
  ComplexMatrix U (void) const;
  Matrix P (void) const;

  friend ostream&  operator << (ostream& os, const ComplexLU& a);

private:

  ComplexMatrix l;
  ComplexMatrix u;
  Matrix p;
};

inline ComplexLU::ComplexLU (const ComplexLU& a)
{
  l = a.l;
  u = a.u;
  p = a.p;
}

inline ComplexLU& ComplexLU::operator = (const ComplexLU& a)
{
  l = a.l;
  u = a.u;
  p = a.p;
  return *this;
}

inline ComplexMatrix ComplexLU::L (void) const
{
  return l;
}

inline ComplexMatrix ComplexLU::U (void) const
{
  return u;
}

inline Matrix ComplexLU::P (void) const
{
  return p;
}

/*
 * Result of a QR decomposition.
 */

class QR
{
public:

  QR (void) {}

  QR (const Matrix& A);

  QR (const QR& a);

  QR& operator = (const QR& a);

  Matrix Q (void) const;
  Matrix R (void) const;

  friend ostream&  operator << (ostream& os, const QR& a);

private:

  Matrix q;
  Matrix r;
};

inline QR::QR (const QR& a)
{
  q = a.q;
  r = a.r;
}

inline QR& QR::operator = (const QR& a)
{
  q = a.q;
  r = a.r;
  return *this;
}

inline Matrix QR::Q (void) const
{
  return q;
}

inline Matrix QR::R (void) const
{
  return r;
}

class ComplexQR
{
public:

  ComplexQR (void) {}

  ComplexQR (const ComplexMatrix& A);

  ComplexQR (const ComplexQR& a);

  ComplexQR& operator = (const ComplexQR& a);

  ComplexMatrix Q (void) const;
  ComplexMatrix R (void) const;

  friend ostream&  operator << (ostream& os, const ComplexQR& a);

private:

  ComplexMatrix q;
  ComplexMatrix r;
};

inline ComplexQR::ComplexQR (const ComplexQR& a)
{
  q = a.q;
  r = a.r;
}

inline ComplexQR& ComplexQR::operator = (const ComplexQR& a)
{
  q = a.q;
  r = a.r;
  return *this;
}

inline ComplexMatrix ComplexQR::Q (void) const
{
  return q;
}

inline ComplexMatrix ComplexQR::R (void) const
{
  return r;
}

} // extern "C++"

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
