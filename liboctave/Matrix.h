// Matrix manipulations.                                 -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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

#if !defined (_Matrix_h)
#define _Matrix_h 1

// I\'m not sure how this is supposed to work if the .h file declares
// several classes, each of which is defined in a separate file...
//
// #ifdef __GNUG__
// #pragma interface
// #endif

#include <stdlib.h>
#include <stddef.h>
#include <math.h>
#include <values.h>
#include <assert.h>
#include <iostream.h>
// #include <iomanip.h>  // We don\'t use this yet.
#include <Complex.h>

#define FAIL assert(0) /* XXX FIXME XXX */

#ifndef MAPPER_FCN_TYPEDEFS
#define MAPPER_FCN_TYPEDEFS 1

typedef double (*d_d_Mapper)(double);
typedef double (*d_c_Mapper)(const Complex&);
typedef Complex (*c_c_Mapper)(const Complex&);

#endif

#include "f77-uscore.h"

// Fortran functions we call.

extern "C"
{
  int F77_FCN (dgemm) (const char*, const char*, const int*,
		       const int*, const int*, const double*,
		       const double*, const int*, const double*,
		       const int*, const double*, double*, const int*,
		       long, long);

  int F77_FCN (dgemv) (const char*, const int*, const int*,
		       const double*, const double*, const int*,
		       const double*, const int*, const double*,
		       double*, const int*, long);

  int F77_FCN (dgeco) (double*, const int*, const int*, int*, double*,
		       double*);

  int F77_FCN (dgesv) (const int*, const int*, double*, const int*,
		       int*, double*, const int*, int*);

  int F77_FCN (dgeqrf) (const int*, const int*, double*, const int*,
			double*, double*, const int*, int*);

  int F77_FCN (dorgqr) (const int*, const int*, const int*, double*,
			const int*, double*, double*, const int*, int*);

  int F77_FCN (dgesl) (const double*, const int*, const int*,
		       const int*, double*, const int*); 

  int F77_FCN (dgedi) (double*, const int*, const int*, const int*,
		       double*, double*, const int*);

  double F77_FCN (ddot) (const int*, const double*, const int*,
			 const double*, const int*);

  int F77_FCN (dgeev) (const char*, const char*, const int*, double*,
		       const int*, double*, double*, double*,
		       const int*, double*, const int*, double*,
		       const int*, int*, long, long);

  int F77_FCN (dgeesx) (const char*, const char*, int (*)(), const char*,
			const int*, double*, const int*, int*, double*,
			double*, double*, const int*, double*, double*, 
			double*, const int*, int*, const int*, int*,
			int*, long, long);

  int F77_FCN (dhseqr) (const char*, const char*, const int*,
                        const int*, const int*, double*,
                        const int*, double*, double*,
                        double*, const int*, double*, const int*,
                        int*, long, long);

  int F77_FCN (dgebal) (const char*, const int*, double*,
                        const int*, int*, int*, double*,
                        int*, long, long);

  int F77_FCN (dgebak) (const char*, const char*, const int*, const int*,
			const int*, double*, const int*, double*, const int*,
			int*, long, long);

  int F77_FCN (dgehrd) (const int*, const int*, const int*,
                        double*, const int*, double*, double*,
                        const int*, int*, long, long);

  int F77_FCN (dorghr) (const int*, const int*, const int*,
                        double*, const int*, double*, double*,
                        const int*, int*, long, long);

  int F77_FCN (dgesvd) (const char*, const char*, const int*,
			const int*, double*, const int*, double*,
			double*, const int*, double*, const int*,
			double*, const int*, int*, long, long);

  int F77_FCN (dgelss) (const int*, const int*, const int*, double*,
			const int*, double*, const int*, double*,
			const double*, int*, double*, const int*,
			int*);

  int F77_FCN (dpotrf) (const char*, const int*, double*, const int*,
			int*, long);

//
// fortran functions for generalized eigenvalue problems
//
  int F77_FCN (reduce) (const int*, const int*, double*,
	   	        const int*, double*,
			int*, int*, double*, double*);

  int F77_FCN (scaleg) (const int*, const int*, double*,
	   	        const int*, double*,
			const int*, const int*, double*, double*, double*);

  int F77_FCN (gradeq) (const int*, const int*, double*,
	   	        const int*, double*,
			int*, int*, double*, double*);

/*
 * f2c translates complex*16 as
 *
 *   typedef struct { doublereal re, im; } doublecomplex;
 *
 * and Complex.h from libg++ uses
 *
 *   protected:
 *     double re;
 *     double im;
 *
 * as the only data members, so this should work (fingers crossed that
 * things don't change).
 */

  int F77_FCN (zgemm) (const char*, const char*, const int*,
		       const int*, const int*, const Complex*,
		       const Complex*, const int*, const Complex*,
		       const int*, const Complex*, Complex*, const int*,
		       long, long);

  int F77_FCN (zgemv) (const char*, const int*, const int*,
		       const Complex*, const Complex*, const int*,
		       const Complex*, const int*, const Complex*,
		       Complex*, const int*, long);

  int F77_FCN (zgeco) (Complex*, const int*, const int*, int*,
		       double*, Complex*);

  int F77_FCN (zgesv) (const int*, const int*, Complex*, const int*,
		       int*, Complex*, const int*, int*);

  int F77_FCN (zgeqrf) (const int*, const int*, Complex*, const int*,
			Complex*, Complex*, const int*, int*);

  int F77_FCN (zgeesx) (const char*, const char*, int (*)(), const char*,
			const int*, Complex*, const int*, int*,
			Complex*, Complex*, const int*, double*, double*,
			Complex*, const int*, double*, int*, int*,
			long, long);

  int F77_FCN (zhseqr) (const char*, const char*, const int*,
                        const int*, const int*, Complex*, const int*,
                        Complex*, Complex*, const int*, Complex*,
                        const int*, int*, long, long);

  int F77_FCN (zgebal) (const char*, const int*, Complex*, const int*,
                        int*, int*, double*, int*, long, long);
 
  int F77_FCN (zgebak) (const char*, const char*, const int*, const int*,
			const int*, double*, const int*, Complex*, 
			const int*, int*, long, long);

  int F77_FCN (zgehrd) (const int*, const int*, const int*, Complex*,
                        const int*, Complex*, Complex*, const int*,
                        int*, long, long);
 
  int F77_FCN (zunghr) (const int*, const int*, const int*, Complex*,
                        const int*, Complex*, Complex*, const int*,
                        int*, long, long);

  int F77_FCN (zungqr) (const int*, const int*, const int*, Complex*,
			const int*, Complex*, Complex*, const int*, int*);

  int F77_FCN (zgedi) (Complex*, const int*, const int*, int*,
		       Complex*, Complex*, const int*);

  int F77_FCN (zgesl) (Complex*, const int*, const int*, int*,
		       Complex*, const int*);

  int F77_FCN (zgeev) (const char*, const char*, const int*, Complex*,
		       const int*, Complex*, Complex*, const int*,
		       Complex*, const int*, Complex*, const int*,
		       double*, int*, long, long);

  int F77_FCN (zgesvd) (const char*, const char*, const int*,
			const int*, Complex*, const int*, double*,
			Complex*, const int*, Complex*, const int*,
			Complex*, const int*, double*, int*, long, long);

  int F77_FCN (zgelss) (const int*, const int*, const int*, Complex*,
			const int*, Complex*, const int*, double*,
			const double*, int*, Complex*, const int*,
			double*, int*);

  int F77_FCN (zpotrf) (const char*, const int*, Complex*, const int*,
			int*, long);


// Note that the original complex fft routines were not written for
// double complex arguments.  They have been modified by adding an
// implicit double precision (a-h,o-z) statement at the beginning of
// each subroutine.

  int F77_FCN (cffti) (const int*, Complex*);

  int F77_FCN (cfftf) (const int*, Complex*, Complex*);

  int F77_FCN (cfftb) (const int*, Complex*, Complex*);

}

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

class Matrix
{
friend class RowVector;
friend class DiagMatrix;
friend class ComplexMatrix;
friend class ComplexDiagMatrix;
friend class AEPBALANCE;
friend class CHOL;
friend class EIG;
friend class GEPBALANCE;
friend class HESS;
friend class SCHUR;
friend class SVD;
friend class LU;
friend class QR;

public:
  Matrix (void);
  Matrix (int r, int c);
  Matrix (int r, int c, double val);
  Matrix (const Matrix& a);
  Matrix (const DiagMatrix& a);
  Matrix (double a);
 ~Matrix (void);

#if defined (MDEBUG)
  void *operator new (size_t size)
    {
      Matrix *p = ::new Matrix;
      cerr << "Matrix::new(): " << p << "\n";
      return p;
    }

  void operator delete (void *p, size_t size)
    {
      cerr << "Matrix::delete(): " << p << "\n";
      ::delete p;
    }
#endif

  Matrix& operator = (const Matrix& a);

  int rows (void) const;
  int cols (void) const;
  int columns (void) const;

  double& elem (int r, int c);
  double& checkelem (int r, int c);
  double& operator () (int r, int c);

  double elem (int r, int c) const; // const access
  double checkelem (int r, int c) const;
  double operator () (int r, int c) const;

  Matrix& resize (int r, int c);
  Matrix& resize (int r, int c, double val);

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

  Matrix inverse (int& info, double& rcond) const;
  Matrix inverse (int& info) const;
  Matrix inverse (void) const;

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

// matrix by scalar -> matrix operations

  Matrix operator + (double s) const;
  Matrix operator - (double s) const;
  Matrix operator * (double s) const;
  Matrix operator / (double s) const;

  ComplexMatrix operator + (const Complex& s) const;
  ComplexMatrix operator - (const Complex& s) const;
  ComplexMatrix operator * (const Complex& s) const;
  ComplexMatrix operator / (const Complex& s) const;

// scalar by matrix -> matrix operations

  friend Matrix operator + (double s, const Matrix& a);
  friend Matrix operator - (double s, const Matrix& a);
  friend Matrix operator * (double s, const Matrix& a);
  friend Matrix operator / (double s, const Matrix& a);

// matrix by column vector -> column vector operations

  ColumnVector operator * (const ColumnVector& a) const;

  ComplexColumnVector operator * (const ComplexColumnVector& a) const;

// matrix by diagonal matrix -> matrix operations

  Matrix operator + (const DiagMatrix& a) const;
  Matrix operator - (const DiagMatrix& a) const;
  Matrix operator * (const DiagMatrix& a) const;

  ComplexMatrix operator + (const ComplexDiagMatrix& a) const;
  ComplexMatrix operator - (const ComplexDiagMatrix& a) const;
  ComplexMatrix operator * (const ComplexDiagMatrix& a) const;

  Matrix& operator += (const DiagMatrix& a);
  Matrix& operator -= (const DiagMatrix& a);

// matrix by matrix -> matrix operations

  Matrix operator + (const Matrix& a) const;
  Matrix operator - (const Matrix& a) const;
  Matrix operator * (const Matrix& a) const;

  ComplexMatrix operator + (const ComplexMatrix& a) const;
  ComplexMatrix operator - (const ComplexMatrix& a) const;
  ComplexMatrix operator * (const ComplexMatrix& a) const;

  Matrix product (const Matrix& a) const;    // element by element
  Matrix quotient (const Matrix& a) const;   // element by element

  ComplexMatrix product (const ComplexMatrix& a) const;  // element by element
  ComplexMatrix quotient (const ComplexMatrix& a) const; // element by element

  Matrix& operator += (const Matrix& a);
  Matrix& operator -= (const Matrix& a);

// unary operations

  Matrix operator - (void) const;
  Matrix operator ! (void) const;

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

// conversions

  double *fortran_vec (void) const;

private:
  int nr;
  int nc;
  int len;
  double *data;

  Matrix (double *d, int r, int c);
};

inline Matrix::Matrix (void) { nr = 0; nc = 0; len = 0; data = 0; }

inline Matrix::Matrix (double *d, int r, int c)
  { nr = r; nc = c; len = nr*nc; data = d; }

inline Matrix::~Matrix (void) { delete [] data; data = 0; }

inline int Matrix::rows (void) const { return nr; }
inline int Matrix::cols (void) const { return nc; }
inline int Matrix::columns (void) const { return nc; } 

inline double& Matrix::elem (int r, int c) { return data[nr*c+r]; }

inline double& Matrix::checkelem (int r, int c)
{
#ifndef NO_RANGE_CHECK
  if (r < 0 || r >= nr || c < 0 || c >= nc)
    FAIL;
#endif

  return elem (r, c);
}

inline double& Matrix::operator () (int r, int c)
  { return checkelem (r, c); }

inline double Matrix::elem (int r, int c) const { return data[nr*c+r]; }

inline double Matrix::checkelem (int r, int c) const
{
#ifndef NO_RANGE_CHECK
  if (r < 0 || r >= nr || c < 0 || c >= nc)
    FAIL;
#endif

  return elem (r, c);
}

inline double Matrix::operator () (int r, int c) const
  { return checkelem (r, c); }

inline double *Matrix::fortran_vec (void) const { return data; }

/*
 * Column Vector class
 */

class ColumnVector
{
friend class Matrix;
friend class RowVector;
friend class DiagMatrix;
friend class ComplexMatrix;
friend class ComplexColumnVector;
friend class ComplexDiagMatrix;

public:
  ColumnVector (void);
  ColumnVector (int n);
  ColumnVector (int n, double val);
  ColumnVector (const ColumnVector& a);
  ColumnVector (double a);
 ~ColumnVector (void);

  ColumnVector& operator = (const ColumnVector& a);

  int capacity (void) const;
  int length (void) const;

  double& elem (int n);
  double& checkelem (int n);
  double& operator () (int n);

  double elem (int n) const; // const access
  double checkelem (int n) const;
  double operator () (int n) const;

  ColumnVector& resize (int n);
  ColumnVector& resize (int n, double val);

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

// column vector by scalar -> column vector operations

  ColumnVector operator + (double s) const;
  ColumnVector operator - (double s) const;
  ColumnVector operator * (double s) const;
  ColumnVector operator / (double s) const;

  ComplexColumnVector operator + (const Complex& s) const;
  ComplexColumnVector operator - (const Complex& s) const;
  ComplexColumnVector operator * (const Complex& s) const;
  ComplexColumnVector operator / (const Complex& s) const;

// scalar by column vector -> column vector operations

  friend ColumnVector operator + (double s, const ColumnVector& a);
  friend ColumnVector operator - (double s, const ColumnVector& a);
  friend ColumnVector operator * (double s, const ColumnVector& a);
  friend ColumnVector operator / (double s, const ColumnVector& a);

// column vector by row vector -> matrix operations

  Matrix operator * (const RowVector& a) const;

  ComplexMatrix operator * (const ComplexRowVector& a) const;

// column vector by column vector -> column vector operations

  ColumnVector operator + (const ColumnVector& a) const;
  ColumnVector operator - (const ColumnVector& a) const;

  ComplexColumnVector operator + (const ComplexColumnVector& a) const;
  ComplexColumnVector operator - (const ComplexColumnVector& a) const;

  ColumnVector product (const ColumnVector& a) const;  // element by element
  ColumnVector quotient (const ColumnVector& a) const; // element by element

  ComplexColumnVector product (const ComplexColumnVector& a) const;
  ComplexColumnVector quotient (const ComplexColumnVector& a) const;

  ColumnVector& operator += (const ColumnVector& a);
  ColumnVector& operator -= (const ColumnVector& a);

// unary operations

  ColumnVector operator - (void) const;

  friend ColumnVector map (d_d_Mapper f, const ColumnVector& a);
  void map (d_d_Mapper f);

  double min (void) const;
  double max (void) const;

// i/o

  friend ostream& operator << (ostream& os, const ColumnVector& a);

// conversions

  double *fortran_vec (void) const;

private:
  int len;
  double *data;

  ColumnVector (double *d, int l);
};

inline ColumnVector::ColumnVector (void) { len = 0; data = 0; }
inline ColumnVector::ColumnVector (double *d, int l) { len = l; data = d; }
inline ColumnVector::~ColumnVector (void) { delete [] data; data = 0; }

inline int ColumnVector::capacity (void) const { return len; }
inline int ColumnVector::length (void) const { return len; }

inline double& ColumnVector::elem (int n) { return data[n]; }

inline double&
ColumnVector::checkelem (int n)
{
#ifndef NO_RANGE_CHECK
  if (n < 0 || n >= len)
    FAIL;
#endif

  return elem (n);
}

inline double& ColumnVector::operator () (int n) { return checkelem (n); }

inline double ColumnVector::elem (int n) const { return data[n]; }

inline double
ColumnVector::checkelem (int n) const
{
#ifndef NO_RANGE_CHECK
  if (n < 0 || n >= len)
    FAIL;
#endif

  return elem (n);
}

inline double ColumnVector::operator () (int n) const { return checkelem (n); }

inline double *ColumnVector::fortran_vec (void) const { return data; }

/*
 * Row Vector class
 */

class RowVector
{
friend class Matrix;
friend class DiagMatrix;
friend class ColumnVector;
friend class ComplexMatrix;
friend class ComplexRowVector;
friend class ComplexDiagMatrix;

public:
  RowVector (void);
  RowVector (int n);
  RowVector (int n, double val);
  RowVector (const RowVector& a);
  RowVector (double a);
 ~RowVector (void);

  RowVector& operator = (const RowVector& a);

  int capacity (void) const;
  int length (void) const;

  double& elem (int n);
  double& checkelem (int n);
  double& operator () (int n);

  double elem (int n) const; // const access
  double checkelem (int n) const;
  double operator () (int n) const;

  RowVector& resize (int n);
  RowVector& resize (int n, double val);

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

// row vector by scalar -> row vector operations

  RowVector operator + (double s) const;
  RowVector operator - (double s) const;
  RowVector operator * (double s) const;
  RowVector operator / (double s) const;

  ComplexRowVector operator + (const Complex& s) const;
  ComplexRowVector operator - (const Complex& s) const;
  ComplexRowVector operator * (const Complex& s) const;
  ComplexRowVector operator / (const Complex& s) const;

// scalar by row vector -> row vector operations

  friend RowVector operator + (double s, const RowVector& a);
  friend RowVector operator - (double s, const RowVector& a);
  friend RowVector operator * (double s, const RowVector& a);
  friend RowVector operator / (double s, const RowVector& a);

// row vector by column vector -> scalar

  double operator * (const ColumnVector& a) const;

  Complex operator * (const ComplexColumnVector& a) const;

// row vector by matrix -> row vector

  RowVector operator * (const Matrix& a) const;

  ComplexRowVector operator * (const ComplexMatrix& a) const;

// row vector by row vector -> row vector operations

  RowVector operator + (const RowVector& a) const;
  RowVector operator - (const RowVector& a) const;

  ComplexRowVector operator + (const ComplexRowVector& a) const;
  ComplexRowVector operator - (const ComplexRowVector& a) const;

  RowVector product (const RowVector& a) const;  // element by element
  RowVector quotient (const RowVector& a) const; // element by element

  ComplexRowVector product (const ComplexRowVector& a) const;  // el by el
  ComplexRowVector quotient (const ComplexRowVector& a) const; // el by el

  RowVector& operator += (const RowVector& a);
  RowVector& operator -= (const RowVector& a);

// unary operations

  RowVector operator - (void) const;

  friend RowVector map (d_d_Mapper f, const RowVector& a);
  void map (d_d_Mapper f);

  double min (void) const;
  double max (void) const;

// i/o

  friend ostream& operator << (ostream& os, const RowVector& a);

// conversions

  double *fortran_vec (void) const;

private:
  int len;
  double *data;

  RowVector (double *d, int l);
};

inline RowVector::RowVector (void) { len = 0; data = 0; }
inline RowVector::RowVector (double *d, int l) { len = l; data = d; }
inline RowVector::~RowVector (void) { delete [] data; data = 0; }

inline int RowVector::capacity (void) const { return len; }
inline int RowVector::length (void) const { return len; }

inline double& RowVector::elem (int n) { return data[n]; }

inline double&
RowVector::checkelem (int n)
{
#ifndef NO_RANGE_CHECK
  if (n < 0 || n >= len)
    FAIL;
#endif

  return elem (n);
}

inline double& RowVector::operator () (int n) { return checkelem (n); }

inline double RowVector::elem (int n) const { return data[n]; }

inline double
RowVector::checkelem (int n) const
{
#ifndef NO_RANGE_CHECK
  if (n < 0 || n >= len)
    FAIL;
#endif

  return elem (n);
}

inline double RowVector::operator () (int n) const { return checkelem (n); }

inline double *RowVector::fortran_vec (void) const { return data; }

/*
 * Diagonal Matrix class
 */

class DiagMatrix
{
friend class Matrix;
friend class ComplexMatrix;
friend class ComplexDiagMatrix;

public:
  DiagMatrix (void);
  DiagMatrix (int n);
  DiagMatrix (int n, double val);
  DiagMatrix (int r, int c);
  DiagMatrix (int r, int c, double val);
  DiagMatrix (const RowVector& a);
  DiagMatrix (const ColumnVector& a);
  DiagMatrix (const DiagMatrix& a);
  DiagMatrix (double a);
 ~DiagMatrix (void);

  DiagMatrix& operator = (const DiagMatrix& a);

  int rows (void) const;
  int cols (void) const;
  int columns (void) const;

  double& elem (int r, int c);
  double& checkelem (int r, int c);
  double& operator () (int r, int c);

  double elem (int r, int c) const; // const access
  double checkelem (int r, int c) const;
  double operator () (int r, int c) const;

  DiagMatrix& resize (int r, int c);
  DiagMatrix& resize (int r, int c, double val);

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

  DiagMatrix inverse (int& info) const;
  DiagMatrix inverse (void) const;

// diagonal matrix by scalar -> matrix operations

  Matrix operator + (double s) const;
  Matrix operator - (double s) const;

  ComplexMatrix operator + (const Complex& s) const;
  ComplexMatrix operator - (const Complex& s) const;

// diagonal matrix by scalar -> diagonal matrix operations

  DiagMatrix operator * (double s) const;
  DiagMatrix operator / (double s) const;

  ComplexDiagMatrix operator * (const Complex& s) const;
  ComplexDiagMatrix operator / (const Complex& s) const;

// scalar by diagonal matrix -> matrix operations

  friend Matrix operator + (double s, const DiagMatrix& a);
  friend Matrix operator - (double s, const DiagMatrix& a);

// scalar by diagonal matrix -> diagonal matrix operations

  friend DiagMatrix operator * (double s, const DiagMatrix& a);
  friend DiagMatrix operator / (double s, const DiagMatrix& a);

// diagonal matrix by column vector -> column vector operations

  ColumnVector operator * (const ColumnVector& a) const;

  ComplexColumnVector operator * (const ComplexColumnVector& a) const;

// diagonal matrix by diagonal matrix -> diagonal matrix operations

  DiagMatrix operator + (const DiagMatrix& a) const;
  DiagMatrix operator - (const DiagMatrix& a) const;
  DiagMatrix operator * (const DiagMatrix& a) const;

  ComplexDiagMatrix operator + (const ComplexDiagMatrix& a) const;
  ComplexDiagMatrix operator - (const ComplexDiagMatrix& a) const;
  ComplexDiagMatrix operator * (const ComplexDiagMatrix& a) const;

  DiagMatrix product (const DiagMatrix& a) const;    // element by element
  DiagMatrix quotient (const DiagMatrix& a) const;   // element by element

  ComplexDiagMatrix product (const ComplexDiagMatrix& a) const;  // el by el
  ComplexDiagMatrix quotient (const ComplexDiagMatrix& a) const; // el by el

  DiagMatrix& operator += (const DiagMatrix& a);
  DiagMatrix& operator -= (const DiagMatrix& a);

// diagonal matrix by matrix -> matrix operations

  Matrix operator + (const Matrix& a) const;
  Matrix operator - (const Matrix& a) const;
  Matrix operator * (const Matrix& a) const;

  ComplexMatrix operator + (const ComplexMatrix& a) const;
  ComplexMatrix operator - (const ComplexMatrix& a) const;
  ComplexMatrix operator * (const ComplexMatrix& a) const;

// unary operations

  DiagMatrix operator - (void) const;

  ColumnVector diag (void) const;
  ColumnVector diag (int k) const;

// i/o

  friend ostream& operator << (ostream& os, const DiagMatrix& a);

private:
  int nr;
  int nc;
  int len;
  double *data;

  DiagMatrix (double *d, int nr, int nc);
};

inline DiagMatrix::DiagMatrix (void)
  { nr = 0; nc = 0; len = 0; data = 0; }

inline DiagMatrix::DiagMatrix (double *d, int r, int c)
  { nr = r; nc = c; len = nr < nc ? nr : nc; data = d; }

inline DiagMatrix::~DiagMatrix (void) { delete [] data; data = 0; }

inline int DiagMatrix::rows (void) const { return nr; }
inline int DiagMatrix::cols (void) const { return nc; }
inline int DiagMatrix::columns (void) const { return nc; } 

// Would be nice to be able to avoid compiler warning and make this
// fail on assignment.
inline double& DiagMatrix::elem (int r, int c)
  { return (r == c) ? data[r] : 0; }

inline double& DiagMatrix::checkelem (int r, int c)
{
#ifndef NO_RANGE_CHECK
  if (r < 0 || r >= nr || c < 0 || c >= nc)
    FAIL;
#endif

  return elem (r, c);
}

inline double& DiagMatrix::operator () (int r, int c)
  { return checkelem (r, c); }

inline double DiagMatrix::elem (int r, int c) const
  { return (r == c) ? data[r] : 0; }

inline double DiagMatrix::checkelem (int r, int c) const
{
#ifndef NO_RANGE_CHECK
  if (r < 0 || r >= nr || c < 0 || c >= nc)
    FAIL;
#endif

  return elem (r, c);
}

inline double DiagMatrix::operator () (int r, int c) const
  { return checkelem (r, c); }

/*
 * Complex Matrix class
 */

class ComplexMatrix
{
friend class Matrix;
friend class DiagMatrix;
friend class ComplexRowVector;
friend class ComplexDiagMatrix;
friend class ComplexAEPBALANCE;
friend class ComplexCHOL;
friend class EIG;
friend class ComplexHESS;
friend class ComplexSVD;
friend class ComplexSCHUR;
friend class ComplexLU;
friend class ComplexQR;

public:
  ComplexMatrix (void);
  ComplexMatrix (int r, int c);
  ComplexMatrix (int r, int c, double val);
  ComplexMatrix (int r, int c, const Complex& val);
  ComplexMatrix (const Matrix& a);
  ComplexMatrix (const ComplexMatrix& a);
  ComplexMatrix (const DiagMatrix& a);
  ComplexMatrix (const ComplexDiagMatrix& a);
  ComplexMatrix (double a);
  ComplexMatrix (const Complex& a);
 ~ComplexMatrix (void);

  ComplexMatrix& operator = (const Matrix& a);
  ComplexMatrix& operator = (const ComplexMatrix& a);

  int rows (void) const;
  int cols (void) const;
  int columns (void) const;

  Complex& elem (int r, int c);
  Complex& checkelem (int r, int c);
  Complex& operator () (int r, int c);

  Complex elem (int r, int c) const; // const access
  Complex checkelem (int r, int c) const;
  Complex operator () (int r, int c) const;

  ComplexMatrix& resize (int r, int c);
  ComplexMatrix& resize (int r, int c, double val);
  ComplexMatrix& resize (int r, int c, const Complex& val);

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

  ComplexMatrix inverse (int& info, double& rcond) const;
  ComplexMatrix inverse (int& info) const;
  ComplexMatrix inverse (void) const;

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

  ComplexColumnVector solve (const ColumnVector& b) const;
  ComplexColumnVector solve (const ColumnVector& b, int& info) const;
  ComplexColumnVector solve (const ColumnVector& b, int& info,
			     double& rcond) const;

  ComplexColumnVector solve (const ComplexColumnVector& b) const;
  ComplexColumnVector solve (const ComplexColumnVector& b, int& info) const;
  ComplexColumnVector solve (const ComplexColumnVector& b, int& info,
			     double& rcond) const;

  ComplexMatrix lssolve (const Matrix& b) const;
  ComplexMatrix lssolve (const Matrix& b, int& info) const;
  ComplexMatrix lssolve (const Matrix& b, int& info, int& rank) const;

  ComplexMatrix lssolve (const ComplexMatrix& b) const;
  ComplexMatrix lssolve (const ComplexMatrix& b, int& info) const;
  ComplexMatrix lssolve (const ComplexMatrix& b, int& info,
			 int& rank) const;

  ComplexColumnVector lssolve (const ColumnVector& b) const;
  ComplexColumnVector lssolve (const ColumnVector& b, int& info) const;
  ComplexColumnVector lssolve (const ColumnVector& b, int& info,
			       int& rank) const;

  ComplexColumnVector lssolve (const ComplexColumnVector& b) const;
  ComplexColumnVector lssolve (const ComplexColumnVector& b, int& info) const;
  ComplexColumnVector lssolve (const ComplexColumnVector& b, int& info,
			       int& rank) const;

// matrix by scalar -> matrix operations

  ComplexMatrix operator + (double s) const;
  ComplexMatrix operator - (double s) const;
  ComplexMatrix operator * (double s) const;
  ComplexMatrix operator / (double s) const;

  ComplexMatrix operator + (const Complex& s) const;
  ComplexMatrix operator - (const Complex& s) const;
  ComplexMatrix operator * (const Complex& s) const;
  ComplexMatrix operator / (const Complex& s) const;

// scalar by matrix -> matrix operations

  friend ComplexMatrix operator + (double s, const ComplexMatrix& a);
  friend ComplexMatrix operator - (double s, const ComplexMatrix& a);
  friend ComplexMatrix operator * (double s, const ComplexMatrix& a);
  friend ComplexMatrix operator / (double s, const ComplexMatrix& a);

  friend ComplexMatrix operator + (const Complex& s, const ComplexMatrix& a);
  friend ComplexMatrix operator - (const Complex& s, const ComplexMatrix& a);
  friend ComplexMatrix operator * (const Complex& s, const ComplexMatrix& a);
  friend ComplexMatrix operator / (const Complex& s, const ComplexMatrix& a);

// matrix by column vector -> column vector operations

  ComplexColumnVector operator * (const ColumnVector& a) const;

  ComplexColumnVector operator * (const ComplexColumnVector& a) const;

// matrix by diagonal matrix -> matrix operations

  ComplexMatrix operator + (const DiagMatrix& a) const;
  ComplexMatrix operator - (const DiagMatrix& a) const;
  ComplexMatrix operator * (const DiagMatrix& a) const;

  ComplexMatrix operator + (const ComplexDiagMatrix& a) const;
  ComplexMatrix operator - (const ComplexDiagMatrix& a) const;
  ComplexMatrix operator * (const ComplexDiagMatrix& a) const;

  ComplexMatrix& operator += (const DiagMatrix& a);
  ComplexMatrix& operator -= (const DiagMatrix& a);

  ComplexMatrix& operator += (const ComplexDiagMatrix& a);
  ComplexMatrix& operator -= (const ComplexDiagMatrix& a);

// matrix by matrix -> matrix operations

  ComplexMatrix operator + (const Matrix& a) const;
  ComplexMatrix operator - (const Matrix& a) const;
  ComplexMatrix operator * (const Matrix& a) const;

  ComplexMatrix operator + (const ComplexMatrix& a) const;
  ComplexMatrix operator - (const ComplexMatrix& a) const;
  ComplexMatrix operator * (const ComplexMatrix& a) const;

  ComplexMatrix product (const Matrix& a) const;    // element by element
  ComplexMatrix quotient (const Matrix& a) const;   // element by element

  ComplexMatrix product (const ComplexMatrix& a) const;  // element by element
  ComplexMatrix quotient (const ComplexMatrix& a) const; // element by element

  ComplexMatrix& operator += (const Matrix& a);
  ComplexMatrix& operator -= (const Matrix& a);

  ComplexMatrix& operator += (const ComplexMatrix& a);
  ComplexMatrix& operator -= (const ComplexMatrix& a);

// unary operations

  ComplexMatrix operator - (void) const;
  Matrix operator ! (void) const;

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

// conversions

  Complex *fortran_vec (void) const;

private:
  int nr;
  int nc;
  int len;
  Complex *data;

  ComplexMatrix (Complex *d, int r, int c);
};

inline ComplexMatrix::ComplexMatrix (void)
   { nr = 0; nc = 0; len = 0; data = 0; }

inline ComplexMatrix::ComplexMatrix (Complex *d, int r, int c)
  { nr = r; nc = c; len = nr*nc; data = d; }

inline ComplexMatrix::~ComplexMatrix (void) { delete [] data; data = 0; }

inline int ComplexMatrix::rows (void) const { return nr; }
inline int ComplexMatrix::cols (void) const { return nc; }
inline int ComplexMatrix::columns (void) const { return nc; } 

inline Complex& ComplexMatrix::elem (int r, int c) { return data[nr*c+r]; }

inline Complex& ComplexMatrix::checkelem (int r, int c)
{
#ifndef NO_RANGE_CHECK
  if (r < 0 || r >= nr || c < 0 || c >= nc)
    FAIL;
#endif

  return elem (r, c);
}

inline Complex& ComplexMatrix::operator () (int r, int c)
  { return checkelem (r, c); }

inline Complex ComplexMatrix::elem (int r, int c) const
  { return data[nr*c+r]; }

inline Complex ComplexMatrix::checkelem (int r, int c) const
{
#ifndef NO_RANGE_CHECK
  if (r < 0 || r >= nr || c < 0 || c >= nc)
    FAIL;
#endif

  return elem (r, c);
}

inline Complex ComplexMatrix::operator () (int r, int c) const
  { return checkelem (r, c); }

inline Complex *ComplexMatrix::fortran_vec (void) const { return data; }

/*
 * Complex Column Vector class
 */

class ComplexColumnVector
{
friend class DiagMatrix;
friend class ComplexMatrix;
friend class ColumnVector;
friend class ComplexDiagMatrix;

public:
  ComplexColumnVector (void);
  ComplexColumnVector (int n);
  ComplexColumnVector (int n, double val);
  ComplexColumnVector (int n, const Complex& val);
  ComplexColumnVector (const ColumnVector& a);
  ComplexColumnVector (const ComplexColumnVector& a);
  ComplexColumnVector (double a);
  ComplexColumnVector (const Complex& a);
 ~ComplexColumnVector (void);

  ComplexColumnVector& operator = (const ColumnVector& a);
  ComplexColumnVector& operator = (const ComplexColumnVector& a);

  int capacity (void) const;
  int length (void) const;

  Complex& elem (int n);
  Complex& checkelem (int n);
  Complex& operator () (int n);

  Complex elem (int n) const; // const access
  Complex checkelem (int n) const;
  Complex operator () (int n) const;

  ComplexColumnVector& resize (int n);
  ComplexColumnVector& resize (int n, double val);
  ComplexColumnVector& resize (int n, const Complex& val);

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

// column vector by scalar -> column vector operations

  ComplexColumnVector operator + (double s) const;
  ComplexColumnVector operator - (double s) const;
  ComplexColumnVector operator * (double s) const;
  ComplexColumnVector operator / (double s) const;

  ComplexColumnVector operator + (const Complex& s) const;
  ComplexColumnVector operator - (const Complex& s) const;
  ComplexColumnVector operator * (const Complex& s) const;
  ComplexColumnVector operator / (const Complex& s) const;

// scalar by column vector -> column vector operations

  friend ComplexColumnVector operator + (double s,
					 const ComplexColumnVector& a); 
  friend ComplexColumnVector operator - (double s,
					 const ComplexColumnVector& a);
  friend ComplexColumnVector operator * (double s,
					 const ComplexColumnVector& a);
  friend ComplexColumnVector operator / (double s,
					 const ComplexColumnVector& a);

  friend ComplexColumnVector operator + (const Complex& s,
					 const ComplexColumnVector& a);
  friend ComplexColumnVector operator - (const Complex& s,
					 const ComplexColumnVector& a);
  friend ComplexColumnVector operator * (const Complex& s,
					 const ComplexColumnVector& a);
  friend ComplexColumnVector operator / (const Complex& s,
					 const ComplexColumnVector& a);

// column vector by row vector -> matrix operations

  ComplexMatrix operator * (const RowVector& a) const;

  ComplexMatrix operator * (const ComplexRowVector& a) const;

// column vector by column vector -> column vector operations

  ComplexColumnVector operator + (const ColumnVector& a) const;
  ComplexColumnVector operator - (const ColumnVector& a) const;

  ComplexColumnVector operator + (const ComplexColumnVector& a) const;
  ComplexColumnVector operator - (const ComplexColumnVector& a) const;

  ComplexColumnVector product (const ColumnVector& a) const;  // el by el
  ComplexColumnVector quotient (const ColumnVector& a) const; // el by el

  ComplexColumnVector product (const ComplexColumnVector& a) const;
  ComplexColumnVector quotient (const ComplexColumnVector& a) const;

  ComplexColumnVector& operator += (const ColumnVector& a);
  ComplexColumnVector& operator -= (const ColumnVector& a);

  ComplexColumnVector& operator += (const ComplexColumnVector& a);
  ComplexColumnVector& operator -= (const ComplexColumnVector& a);

// unary operations

  ComplexColumnVector operator - (void) const;

  friend ComplexColumnVector map (c_c_Mapper f, const ComplexColumnVector& a);
  friend ColumnVector map (d_c_Mapper f, const ComplexColumnVector& a);
  void map (c_c_Mapper f);

  Complex min (void) const;
  Complex max (void) const;

// i/o

  friend ostream& operator << (ostream& os, const ComplexColumnVector& a);

// conversions

  Complex *fortran_vec (void) const;

private:
  int len;
  Complex *data;

  ComplexColumnVector (Complex *d, int l);
};

inline ComplexColumnVector::ComplexColumnVector (void) { len = 0; data = 0; }
inline ComplexColumnVector::ComplexColumnVector (Complex *d, int l)
  { len = l; data = d; }
inline ComplexColumnVector::~ComplexColumnVector (void)
  { delete [] data; data = 0; }

inline int ComplexColumnVector::capacity (void) const { return len; }
inline int ComplexColumnVector::length (void) const { return len; }

inline Complex& ComplexColumnVector::elem (int n) { return data[n]; }

inline Complex&
ComplexColumnVector::checkelem (int n)
{
#ifndef NO_RANGE_CHECK
  if (n < 0 || n >= len)
    FAIL;
#endif

  return elem (n);
}

inline Complex& ComplexColumnVector::operator () (int n)
  { return checkelem (n); }

inline Complex ComplexColumnVector::elem (int n) const { return data[n]; }

inline Complex
ComplexColumnVector::checkelem (int n) const
{
#ifndef NO_RANGE_CHECK
  if (n < 0 || n >= len)
    FAIL;
#endif

  return elem (n);
}

inline Complex ComplexColumnVector::operator () (int n) const
  { return checkelem (n); }

inline Complex *ComplexColumnVector::fortran_vec (void) const { return data; }

/*
 * Complex Row Vector class
 */

class ComplexRowVector
{
friend class RowVector;
friend class ComplexMatrix;
friend class ComplexColumnVector;
friend class ComplexDiagMatrix;

public:
  ComplexRowVector (void);
  ComplexRowVector (int n);
  ComplexRowVector (int n, double val);
  ComplexRowVector (int n, const Complex& val);
  ComplexRowVector (const RowVector& a);
  ComplexRowVector (const ComplexRowVector& a);
  ComplexRowVector (double a);
  ComplexRowVector (const Complex& a);
 ~ComplexRowVector (void);

  ComplexRowVector& operator = (const RowVector& a);
  ComplexRowVector& operator = (const ComplexRowVector& a);

  int capacity (void) const;
  int length (void) const;

  Complex& checkelem (int n);
  Complex& elem (int n);
  Complex& operator () (int n);

  Complex checkelem (int n) const; // const access
  Complex elem (int n) const;
  Complex operator () (int n) const;

  ComplexRowVector& resize (int n);
  ComplexRowVector& resize (int n, double val);
  ComplexRowVector& resize (int n, const Complex& val);

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

// row vector by scalar -> row vector operations

  ComplexRowVector operator + (double s) const;
  ComplexRowVector operator - (double s) const;
  ComplexRowVector operator * (double s) const;
  ComplexRowVector operator / (double s) const;

  ComplexRowVector operator + (const Complex& s) const;
  ComplexRowVector operator - (const Complex& s) const;
  ComplexRowVector operator * (const Complex& s) const;
  ComplexRowVector operator / (const Complex& s) const;

// scalar by row vector -> row vector operations

  friend ComplexRowVector operator + (double s, const ComplexRowVector& a);
  friend ComplexRowVector operator - (double s, const ComplexRowVector& a);
  friend ComplexRowVector operator * (double s, const ComplexRowVector& a);
  friend ComplexRowVector operator / (double s, const ComplexRowVector& a);

  friend ComplexRowVector operator + (const Complex& s, const
				      ComplexRowVector& a);
  friend ComplexRowVector operator - (const Complex& s, const
				      ComplexRowVector& a);
  friend ComplexRowVector operator * (const Complex& s, const
				      ComplexRowVector& a);
  friend ComplexRowVector operator / (const Complex& s, const
				      ComplexRowVector& a);

// row vector by column vector -> scalar

  Complex operator * (const ColumnVector& a) const;

  Complex operator * (const ComplexColumnVector& a) const;

// row vector by matrix -> row vector

  ComplexRowVector operator * (const Matrix& a) const;

  ComplexRowVector operator * (const ComplexMatrix& a) const;

// row vector by row vector -> row vector operations

  ComplexRowVector operator + (const RowVector& a) const;
  ComplexRowVector operator - (const RowVector& a) const;

  ComplexRowVector operator + (const ComplexRowVector& a) const;
  ComplexRowVector operator - (const ComplexRowVector& a) const;

  ComplexRowVector product (const RowVector& a) const;  // element by element
  ComplexRowVector quotient (const RowVector& a) const; // element by element

  ComplexRowVector product (const ComplexRowVector& a) const;  // el by el
  ComplexRowVector quotient (const ComplexRowVector& a) const; // el by el

  ComplexRowVector& operator += (const RowVector& a);
  ComplexRowVector& operator -= (const RowVector& a);

  ComplexRowVector& operator += (const ComplexRowVector& a);
  ComplexRowVector& operator -= (const ComplexRowVector& a);

// unary operations

  ComplexRowVector operator - (void) const;

  friend ComplexRowVector map (c_c_Mapper f, const ComplexRowVector& a);
  friend RowVector map (d_c_Mapper f, const ComplexRowVector& a);
  void map (c_c_Mapper f);

  Complex min (void) const;
  Complex max (void) const;

// i/o

  friend ostream& operator << (ostream& os, const ComplexRowVector& a);

// conversions

  Complex *fortran_vec (void) const;

private:
  int len;
  Complex *data;

  ComplexRowVector (Complex *d, int l);
};

inline ComplexRowVector::ComplexRowVector (void) { len = 0; data = 0; }
inline ComplexRowVector::ComplexRowVector (Complex *d, int l)
  { len = l; data = d; }
inline ComplexRowVector::~ComplexRowVector (void) { delete [] data; data = 0; }

inline int ComplexRowVector::capacity (void) const { return len; }
inline int ComplexRowVector::length (void) const { return len; }

inline Complex& ComplexRowVector::elem (int n) { return data[n]; }

inline Complex&
ComplexRowVector::checkelem (int n)
{
#ifndef NO_RANGE_CHECK
  if (n < 0 || n >= len)
    FAIL;
#endif

  return elem (n);
}

inline Complex& ComplexRowVector::operator () (int n) { return checkelem (n); }

inline Complex ComplexRowVector::elem (int n) const { return data[n]; }

inline Complex
ComplexRowVector::checkelem (int n) const
{
#ifndef NO_RANGE_CHECK
  if (n < 0 || n >= len)
    FAIL;
#endif

  return elem (n);
}

inline Complex ComplexRowVector::operator () (int n) const
  { return checkelem (n); }

inline Complex *ComplexRowVector::fortran_vec (void) const { return data; }

/*
 * Complex Diagonal Matrix class
 */

class ComplexDiagMatrix
{
friend class Matrix;
friend class DiagMatrix;
friend class ComplexMatrix;

public:
  ComplexDiagMatrix (void);
  ComplexDiagMatrix (int n);
  ComplexDiagMatrix (int n, double val);
  ComplexDiagMatrix (int n, const Complex& val);
  ComplexDiagMatrix (int r, int c);
  ComplexDiagMatrix (int r, int c, double val);
  ComplexDiagMatrix (int r, int c, const Complex& val);
  ComplexDiagMatrix (const RowVector& a);
  ComplexDiagMatrix (const ComplexRowVector& a);
  ComplexDiagMatrix (const ColumnVector& a);
  ComplexDiagMatrix (const ComplexColumnVector& a);
  ComplexDiagMatrix (const DiagMatrix& a);
  ComplexDiagMatrix (const ComplexDiagMatrix& a);
  ComplexDiagMatrix (double a);
  ComplexDiagMatrix (const Complex& a);
 ~ComplexDiagMatrix (void);

  ComplexDiagMatrix& operator = (const DiagMatrix& a);
  ComplexDiagMatrix& operator = (const ComplexDiagMatrix& a);

  int rows (void) const;
  int cols (void) const;
  int columns (void) const;

  Complex& checkelem (int r, int c);
  Complex& elem (int r, int c);
  Complex& operator () (int r, int c);

  Complex checkelem (int r, int c) const; // const access
  Complex elem (int r, int c) const;
  Complex operator () (int r, int c) const;

  ComplexDiagMatrix& resize (int r, int c);
  ComplexDiagMatrix& resize (int r, int c, double val);
  ComplexDiagMatrix& resize (int r, int c, const Complex& val);

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

// diagonal matrix by scalar -> matrix operations

  ComplexMatrix operator + (double s) const;
  ComplexMatrix operator - (double s) const;

  ComplexMatrix operator + (const Complex& s) const;
  ComplexMatrix operator - (const Complex& s) const;

// diagonal matrix by scalar -> diagonal matrix operations

  ComplexDiagMatrix operator * (double s) const;
  ComplexDiagMatrix operator / (double s) const;

  ComplexDiagMatrix operator * (const Complex& s) const;
  ComplexDiagMatrix operator / (const Complex& s) const;

// scalar by diagonal matrix -> matrix operations

  friend ComplexMatrix operator + (double s, const ComplexDiagMatrix& a);
  friend ComplexMatrix operator - (double s, const ComplexDiagMatrix& a);

  friend ComplexMatrix operator + (const Complex& s, const
				   ComplexDiagMatrix& a);
  friend ComplexMatrix operator - (const Complex& s, const
				   ComplexDiagMatrix& a);

// scalar by diagonal matrix -> diagonal matrix operations

  friend ComplexDiagMatrix operator * (double s, const ComplexDiagMatrix& a);
  friend ComplexDiagMatrix operator / (double s, const ComplexDiagMatrix& a);

  friend ComplexDiagMatrix operator * (const Complex& s, const
				       ComplexDiagMatrix& a);
  friend ComplexDiagMatrix operator / (const Complex& s, const
				       ComplexDiagMatrix& a);

// diagonal matrix by column vector -> column vector operations

  ComplexColumnVector operator * (const ColumnVector& a) const;

  ComplexColumnVector operator * (const ComplexColumnVector& a) const;

// diagonal matrix by diagonal matrix -> diagonal matrix operations

  ComplexDiagMatrix operator + (const DiagMatrix& a) const;
  ComplexDiagMatrix operator - (const DiagMatrix& a) const;
  ComplexDiagMatrix operator * (const DiagMatrix& a) const;

  ComplexDiagMatrix operator + (const ComplexDiagMatrix& a) const;
  ComplexDiagMatrix operator - (const ComplexDiagMatrix& a) const;
  ComplexDiagMatrix operator * (const ComplexDiagMatrix& a) const;

  ComplexDiagMatrix product (const DiagMatrix& a) const;  // element by element
  ComplexDiagMatrix quotient (const DiagMatrix& a) const; // element by element

  ComplexDiagMatrix product (const ComplexDiagMatrix& a) const;  // el by el
  ComplexDiagMatrix quotient (const ComplexDiagMatrix& a) const; // el by el

  ComplexDiagMatrix& operator += (const DiagMatrix& a);
  ComplexDiagMatrix& operator -= (const DiagMatrix& a);

  ComplexDiagMatrix& operator += (const ComplexDiagMatrix& a);
  ComplexDiagMatrix& operator -= (const ComplexDiagMatrix& a);

// diagonal matrix by matrix -> matrix operations

  ComplexMatrix operator + (const Matrix& a) const;
  ComplexMatrix operator - (const Matrix& a) const;
  ComplexMatrix operator * (const Matrix& a) const;

  ComplexMatrix operator + (const ComplexMatrix& a) const;
  ComplexMatrix operator - (const ComplexMatrix& a) const;
  ComplexMatrix operator * (const ComplexMatrix& a) const;

// unary operations

  ComplexDiagMatrix operator - (void) const;

  ComplexColumnVector diag (void) const;
  ComplexColumnVector diag (int k) const;

// i/o

  friend ostream& operator << (ostream& os, const ComplexDiagMatrix& a);

private:
  int nr;
  int nc;
  int len;
  Complex *data;

  ComplexDiagMatrix (Complex *d, int nr, int nc);
};

inline ComplexDiagMatrix::ComplexDiagMatrix (void)
  { nr = 0; nc = 0; len = 0; data = 0; }

inline ComplexDiagMatrix::ComplexDiagMatrix (Complex *d, int r, int c)
  { nr = r; nc = c; len = nr < nc ? nr : nc; data = d; }

inline ComplexDiagMatrix::~ComplexDiagMatrix (void)
  { delete [] data; data = 0; }

inline int ComplexDiagMatrix::rows (void) const { return nr; }
inline int ComplexDiagMatrix::cols (void) const { return nc; }
inline int ComplexDiagMatrix::columns (void) const { return nc; } 

// Would be nice to be able to avoid compiler warning and make this
// fail on assignment.
inline Complex& ComplexDiagMatrix::elem (int r, int c)
  { Complex czero (0.0, 0.0); return (r == c) ? data[r] : czero; }

inline Complex& ComplexDiagMatrix::checkelem (int r, int c)
{
#ifndef NO_RANGE_CHECK
  if (r < 0 || r >= nr || c < 0 || c >= nc)
    FAIL;
#endif

  return elem (r, c);
}

inline Complex& ComplexDiagMatrix::operator () (int r, int c)
  { return checkelem (r, c); }

inline Complex ComplexDiagMatrix::elem (int r, int c) const
  { Complex czero (0.0, 0.0); return (r == c) ? data[r] : czero; }

inline Complex ComplexDiagMatrix::checkelem (int r, int c) const
{
#ifndef NO_RANGE_CHECK
  if (r < 0 || r >= nr || c < 0 || c >= nc)
    FAIL;
#endif

  return elem (r, c);
}

inline Complex ComplexDiagMatrix::operator () (int r, int c) const
  { return checkelem (r, c); }

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
  init (a,balance_job); 
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
  { return balanced_mat; }

inline Matrix AEPBALANCE::balancing_matrix (void) const
  { return balancing_mat; }

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
  init(a,balance_job); 
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
  { return balanced_mat; }

inline ComplexMatrix ComplexAEPBALANCE::balancing_matrix (void) const
  { return balancing_mat; }

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

inline DET::DET (const DET& a) { det[0] = a.det[0]; det[1] = a.det[1]; }

inline DET& DET::operator = (const DET& a)
  { det[0] = a.det[0]; det[1] = a.det[1]; return *this; }

inline int DET::value_will_overflow (void) const
  { return det[2] + 1 > log10 (MAXDOUBLE) ? 1 : 0; }

inline int DET::value_will_underflow (void) const
  { return det[2] - 1 < log10 (MINDOUBLE) ? 1 : 0; }

inline double DET::coefficient (void) const { return det[0]; }
inline int DET::exponent (void) const { return (int) det[1]; }
inline double DET::value (void) const { return det[0] * pow (10.0, det[1]); }

inline DET::DET (const double *d) { det[0] = d[0]; det[1] = d[1]; }

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
  { det[0] = a.det[0]; det[1] = a.det[1]; }

inline ComplexDET& ComplexDET::operator = (const ComplexDET& a)
  { det[0] = a.det[0]; det[1] = a.det[1]; return *this; }

inline int ComplexDET::value_will_overflow (void) const
  { return real (det[2]) + 1 > log10 (MAXDOUBLE) ? 1 : 0; }

inline int ComplexDET::value_will_underflow (void) const
  { return real (det[2]) - 1 < log10 (MINDOUBLE) ? 1 : 0; }

inline Complex ComplexDET::coefficient (void) const { return det[0]; }

inline int ComplexDET::exponent (void) const { return (int) real (det[1]); }

inline Complex ComplexDET::value (void) const
  { return det[0] * pow (10.0, real (det[1])); }

inline ComplexDET::ComplexDET (const Complex *d)
  { det[0] = d[0]; det[1] = d[1]; }

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
  init (a,b,balance_job); 
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
  { return balanced_a_mat; }

inline Matrix GEPBALANCE::balanced_b_matrix (void) const 
  { return balanced_b_mat; }

inline Matrix GEPBALANCE::left_balancing_matrix (void) const 
  { return left_balancing_mat; }

inline Matrix GEPBALANCE::right_balancing_matrix (void) const 
  { return right_balancing_mat; }

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

inline CHOL::CHOL (const Matrix& a) {init (a); }
inline CHOL::CHOL (const Matrix& a, int& info) { info = init (a); }
inline CHOL::CHOL (const CHOL& a) { chol_mat = a.chol_mat; }

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

inline ComplexCHOL::ComplexCHOL (const ComplexMatrix& a) { init (a); }
inline ComplexCHOL::ComplexCHOL (const ComplexMatrix& a, int& info)
  { info = init (a); }

inline ComplexCHOL::ComplexCHOL (const ComplexCHOL& a)
  { chol_mat = a.chol_mat; }

inline ComplexCHOL&
ComplexCHOL::operator = (const ComplexCHOL& a)
{
  chol_mat = a.chol_mat;

  return *this;
}

inline ComplexMatrix ComplexCHOL::chol_matrix (void) const
  { return chol_mat; }

  
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

inline HESS::HESS (const Matrix& a) {init (a); }
inline HESS::HESS (const Matrix& a, int& info) { info = init(a); }

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

inline Matrix HESS::hess_matrix (void) const { return hess_mat; }
inline Matrix HESS::unitary_hess_matrix (void) const {return unitary_hess_mat;}

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

inline ComplexHESS::ComplexHESS (const ComplexMatrix& a) { init(a); }
inline ComplexHESS::ComplexHESS (const ComplexMatrix& a, int& info)
  { info = init(a); }

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
  { return hess_mat; }

inline ComplexMatrix ComplexHESS::unitary_hess_matrix (void) const
  { return unitary_hess_mat; }

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

  SCHUR& operator = (const SCHUR& a, const char *ord);

  Matrix schur_matrix (void) const;
  Matrix unitary_matrix (void) const;

  friend ostream& operator << (ostream& os, const SCHUR& a);

private:
  int init (const Matrix& a, const char *ord);

  Matrix schur_mat;
  Matrix unitary_mat;
};

inline SCHUR::SCHUR (const Matrix& a, const char *ord) { init (a, ord); }
inline SCHUR::SCHUR (const Matrix& a, const char *ord, int& info) 
  { info = init (a, ord); }

inline SCHUR::SCHUR (const SCHUR& a, const char *ord)
{
  schur_mat = a.schur_mat;
  unitary_mat = a.unitary_mat;
}

inline SCHUR&
SCHUR::operator = (const SCHUR& a, const char *ord)
{
  schur_mat = a.schur_mat;
  unitary_mat = a.unitary_mat;
  
  return *this;
}

inline Matrix SCHUR::schur_matrix (void) const { return schur_mat; }
inline Matrix SCHUR::unitary_matrix (void) const { return unitary_mat; }

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

  ComplexSCHUR& operator = (const ComplexSCHUR& a, const char *ord);

  ComplexMatrix schur_matrix (void) const;
  ComplexMatrix unitary_matrix (void) const;

  friend ostream& operator << (ostream& os, const ComplexSCHUR& a);

private:
  int init (const ComplexMatrix& a, const char *ord);

  ComplexMatrix schur_mat;
  ComplexMatrix unitary_mat;
};

inline ComplexSCHUR::ComplexSCHUR (const ComplexMatrix& a, const char *ord) 
  { init (a,ord); }

inline ComplexSCHUR::ComplexSCHUR (const ComplexMatrix& a, const char *ord,
				   int& info)
  { info = init (a,ord); }

inline ComplexSCHUR::ComplexSCHUR (const ComplexSCHUR& a, const char *ord)
{
  schur_mat = a.schur_mat;
  unitary_mat = a.unitary_mat;
}

inline ComplexSCHUR&
ComplexSCHUR::operator = (const ComplexSCHUR& a, const char *ord)
{
  schur_mat = a.schur_mat;
  unitary_mat = a.unitary_mat;

  return *this;
}

inline ComplexMatrix ComplexSCHUR::schur_matrix (void) const
  { return schur_mat; }

inline ComplexMatrix ComplexSCHUR::unitary_matrix (void) const
  { return unitary_mat; }


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

inline SVD::SVD (const Matrix& a) { init (a); }
inline SVD::SVD (const Matrix& a, int& info) { info = init (a); }

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

inline DiagMatrix SVD::singular_values (void) const { return sigma; }
inline Matrix SVD::left_singular_matrix (void) const { return left_sm; }
inline Matrix SVD::right_singular_matrix (void) const { return right_sm; }

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

inline ComplexSVD::ComplexSVD (const ComplexMatrix& a) { init (a); }
inline ComplexSVD::ComplexSVD (const ComplexMatrix& a, int& info)
  { info = init (a); } 

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
  { return sigma; }

inline ComplexMatrix ComplexSVD::left_singular_matrix (void) const
  { return left_sm; }

inline ComplexMatrix ComplexSVD::right_singular_matrix (void) const
  { return right_sm; }

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

inline EIG::EIG (const Matrix& a) { init (a); }
inline EIG::EIG (const Matrix& a, int& info) { info = init (a); }

inline EIG::EIG (const ComplexMatrix& a) { init (a); }
inline EIG::EIG (const ComplexMatrix& a, int& info) { info = init (a); }

inline EIG::EIG (const EIG& a) { lambda = a.lambda; v = a.v; }

inline EIG& EIG::operator = (const EIG& a)
  { lambda = a.lambda; v = a.v; return *this; }

inline ComplexColumnVector EIG::eigenvalues (void) const { return lambda; } 

inline ComplexMatrix EIG::eigenvectors (void) const { return v; }

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

inline LU::LU (const LU& a) { l = a.l; u = a.u; p = a.p; }

inline LU& LU::operator = (const LU& a)
  { l = a.l; u = a.u; p = a.p; return *this; }

inline Matrix LU::L (void) const { return l; }
inline Matrix LU::U (void) const { return u; }
inline Matrix LU::P (void) const { return p; }

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

inline ComplexLU::ComplexLU (const ComplexLU& a) { l = a.l; u = a.u; p = a.p; }

inline ComplexLU& ComplexLU::operator = (const ComplexLU& a)
  { l = a.l; u = a.u; p = a.p; return *this; }

inline ComplexMatrix ComplexLU::L (void) const { return l; }
inline ComplexMatrix ComplexLU::U (void) const { return u; }
inline Matrix ComplexLU::P (void) const { return p; }

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

inline QR::QR (const QR& a) { q = a.q; r = a.r; }

inline QR& QR::operator = (const QR& a) { q = a.q; r = a.r; return *this; }

inline Matrix QR::Q (void) const { return q; }
inline Matrix QR::R (void) const { return r; }

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

inline ComplexQR::ComplexQR (const ComplexQR& a) { q = a.q; r = a.r; }

inline ComplexQR& ComplexQR::operator = (const ComplexQR& a)
  { q = a.q; r = a.r; return *this; }

inline ComplexMatrix ComplexQR::Q (void) const { return q; }
inline ComplexMatrix ComplexQR::R (void) const { return r; }

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
