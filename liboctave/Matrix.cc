// Matrix manipulations.                              -*- C++ -*-
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <iostream.h>

#include "Matrix.h"
#include "mx-inlines.cc"
#include "lo-error.h"
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

  int F77_FCN (dgesl) (const double*, const int*, const int*,
		       const int*, double*, const int*); 

  int F77_FCN (dgedi) (double*, const int*, const int*, const int*,
		       double*, double*, const int*);

  int F77_FCN (dgelss) (const int*, const int*, const int*, double*,
			const int*, double*, const int*, double*,
			const double*, int*, double*, const int*,
			int*);

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

  int F77_FCN (zgedi) (Complex*, const int*, const int*, int*,
		       Complex*, Complex*, const int*);

  int F77_FCN (zgesl) (Complex*, const int*, const int*, int*,
		       Complex*, const int*);

  int F77_FCN (zgelss) (const int*, const int*, const int*, Complex*,
			const int*, Complex*, const int*, double*,
			const double*, int*, Complex*, const int*,
			double*, int*);

// Note that the original complex fft routines were not written for
// double complex arguments.  They have been modified by adding an
// implicit double precision (a-h,o-z) statement at the beginning of
// each subroutine.

  int F77_FCN (cffti) (const int*, Complex*);

  int F77_FCN (cfftf) (const int*, Complex*, Complex*);

  int F77_FCN (cfftb) (const int*, Complex*, Complex*);
}

// Since this is only temporary, put all of this here, rather than
// putting each type where it logically belongs.  This way, it will be
// easier to delete.

#define KLUDGE_MATRICES
#define TYPE double
#define KL_MAT_TYPE Matrix
#include "mx-kludge.cc"
#undef KLUDGE_MATRICES
#undef TYPE
#undef KL_MAT_TYPE

#define KLUDGE_VECTORS
#define TYPE double
#define KL_VEC_TYPE ColumnVector
#include "mx-kludge.cc"
#undef KLUDGE_VECTORS
#undef TYPE
#undef KL_VEC_TYPE

#define KLUDGE_VECTORS
#define TYPE double
#define KL_VEC_TYPE RowVector
#include "mx-kludge.cc"
#undef KLUDGE_VECTORS
#undef TYPE
#undef KL_VEC_TYPE

#define KLUDGE_DIAG_MATRICES
#define TYPE double
#define KL_DMAT_TYPE DiagMatrix
#include "mx-kludge.cc"
#undef KLUDGE_DIAG_MATRICES
#undef TYPE
#undef KL_DMAT_TYPE

#define KLUDGE_MATRICES
#define TYPE Complex
#define KL_MAT_TYPE ComplexMatrix
#include "mx-kludge.cc"
#undef KLUDGE_MATRICES
#undef TYPE
#undef KL_MAT_TYPE

#define KLUDGE_VECTORS
#define TYPE Complex
#define KL_VEC_TYPE ComplexColumnVector
#include "mx-kludge.cc"
#undef KLUDGE_VECTORS
#undef TYPE
#undef KL_VEC_TYPE

#define KLUDGE_VECTORS
#define TYPE Complex
#define KL_VEC_TYPE ComplexRowVector
#include "mx-kludge.cc"
#undef KLUDGE_VECTORS
#undef TYPE
#undef KL_VEC_TYPE

#define KLUDGE_DIAG_MATRICES
#define TYPE Complex
#define KL_DMAT_TYPE ComplexDiagMatrix
#include "mx-kludge.cc"
#undef KLUDGE_DIAG_MATRICES
#undef TYPE
#undef KL_DMAT_TYPE

/*
 * Matrix class.
 */

Matrix::Matrix (const DiagMatrix& a) : Array2<double> (a.rows (), a.cols ())
{
  for (int i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

#if 0
Matrix&
Matrix::resize (int r, int c)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimensions");
      return *this;
    }

  int new_len = r * c;
  double* new_data = (double *) NULL;
  if (new_len > 0)
    {
      new_data = new double [new_len];

      int min_r = nr < r ? nr : r;
      int min_c = nc < c ? nc : c;

      for (int j = 0; j < min_c; j++)
	for (int i = 0; i < min_r; i++)
	  new_data[r*j+i] = elem (i, j);
    }

  delete [] data;
  nr = r;
  nc = c;
  len = new_len;
  data = new_data;

  return *this;
}

Matrix&
Matrix::resize (int r, int c, double val)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimensions");
      return *this;
    }

  int new_len = r * c;
  double *new_data = (double *) NULL;
  if (new_len > 0)
    {
      new_data = new double [new_len];

// There may be faster or cleaner ways to do this.

      if (r > nr || c > nc)
	copy (new_data, new_len, val);

      int min_r = nr < r ? nr : r;
      int min_c = nc < c ? nc : c;

      for (int j = 0; j < min_c; j++)
	for (int i = 0; i < min_r; i++)
	  new_data[r*j+i] = elem (i, j);
    }

  delete [] data;
  nr = r;
  nc = c;
  len = new_len;
  data = new_data;

  return *this;
}
#endif

int
Matrix::operator == (const Matrix& a) const
{
  if (rows () != a.rows () || cols () != a.cols ())
    return 0;

  return equal (data (), a.data (), length ());
}

int
Matrix::operator != (const Matrix& a) const
{
  return !(*this == a);
}

Matrix&
Matrix::insert (const Matrix& a, int r, int c)
{
  int a_rows = a.rows ();
  int a_cols = a.cols ();
  if (r < 0 || r + a_rows - 1 > rows ()
      || c < 0 || c + a_cols - 1 > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  for (int j = 0; j < a_cols; j++)
    for (int i = 0; i < a_rows; i++)
      elem (r+i, c+j) = a.elem (i, j);

  return *this;
}

Matrix&
Matrix::insert (const RowVector& a, int r, int c)
{
  int a_len = a.length ();
  if (r < 0 || r >= rows () || c < 0 || c + a_len - 1 > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  for (int i = 0; i < a_len; i++)
    elem (r, c+i) = a.elem (i);

  return *this;
}

Matrix&
Matrix::insert (const ColumnVector& a, int r, int c)
{
  int a_len = a.length ();
  if (r < 0 || r + a_len - 1 > rows () || c < 0 || c >= cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  for (int i = 0; i < a_len; i++)
    elem (r+i, c) = a.elem (i);

  return *this;
}

Matrix&
Matrix::insert (const DiagMatrix& a, int r, int c)
{
  if (r < 0 || r + a.rows () - 1 > rows ()
      || c < 0 || c + a.cols () - 1 > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  for (int i = 0; i < a.length (); i++)
    elem (r+i, c+i) = a.elem (i, i);

  return *this;
}

Matrix&
Matrix::fill (double val)
{
  int nr = rows ();
  int nc = cols ();
  if (nr > 0 && nc > 0)
    for (int j = 0; j < nc; j++)
      for (int i = 0; i < nr; i++)
	elem (i, j) = val;

  return *this;
}

Matrix&
Matrix::fill (double val, int r1, int c1, int r2, int c2)
{
  int nr = rows ();
  int nc = rows ();
  if (r1 < 0 || r2 < 0 || c1 < 0 || c2 < 0
      || r1 >= nr || r2 >= nr || c1 >= nc || c2 >= nc)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  if (r1 > r2) { int tmp = r1; r1 = r2; r2 = tmp; }
  if (c1 > c2) { int tmp = c1; c1 = c2; c2 = tmp; }

  for (int j = c1; j <= c2; j++)
    for (int i = r1; i <= r2; i++)
      elem (i, j) = val;

  return *this;
}

Matrix
Matrix::append (const Matrix& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.rows ())
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return Matrix ();
    }

  int nc_insert = nc;
  Matrix retval (nr, nc + a.cols ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

Matrix
Matrix::append (const RowVector& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nr != 1)
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return Matrix ();
    }

  int nc_insert = nc;
  Matrix retval (nr, nc + a.length ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

Matrix
Matrix::append (const ColumnVector& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.length ())
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return Matrix ();
    }

  int nc_insert = nc;
  Matrix retval (nr, nc + 1);
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

Matrix
Matrix::append (const DiagMatrix& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.rows ())
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return *this;
    }

  int nc_insert = nc;
  Matrix retval (nr, nc + a.cols ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

Matrix
Matrix::stack (const Matrix& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("column dimension mismatch for stack");
      return Matrix ();
    }

  int nr_insert = nr;
  Matrix retval (nr + a.rows (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

Matrix
Matrix::stack (const RowVector& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nc != a.length ())
    {
      (*current_liboctave_error_handler)
	("column dimension mismatch for stack");
      return Matrix ();
    }

  int nr_insert = nr;
  Matrix retval (nr + 1, nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

Matrix
Matrix::stack (const ColumnVector& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nc != 1)
    {
      (*current_liboctave_error_handler)
	("column dimension mismatch for stack");
      return Matrix ();
    }

  int nr_insert = nr;
  Matrix retval (nr + a.length (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

Matrix
Matrix::stack (const DiagMatrix& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("column dimension mismatch for stack");
      return Matrix ();
    }

  int nr_insert = nr;
  Matrix retval (nr + a.rows (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

Matrix
Matrix::transpose (void) const
{
  int nr = rows ();
  int nc = cols ();
  Matrix result (nc, nr);
  if (length () > 0)
    {
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  result.elem (j, i) = elem (i, j);
    }
  return result;
}

Matrix
Matrix::extract (int r1, int c1, int r2, int c2) const
{
  if (r1 > r2) { int tmp = r1; r1 = r2; r2 = tmp; }
  if (c1 > c2) { int tmp = c1; c1 = c2; c2 = tmp; }

  int new_r = r2 - r1 + 1;
  int new_c = c2 - c1 + 1;

  Matrix result (new_r, new_c);

  for (int j = 0; j < new_c; j++)
    for (int i = 0; i < new_r; i++)
      result.elem (i, j) = elem (r1+i, c1+j);

  return result;
}

// extract row or column i.

RowVector
Matrix::row (int i) const
{
  int nc = cols ();
  if (i < 0 || i >= rows ())
    {
      (*current_liboctave_error_handler) ("invalid row selection");
      return RowVector ();
    }

  RowVector retval (nc);
  for (int j = 0; j < nc; j++)
    retval.elem (j) = elem (i, j);

  return retval;
}

RowVector
Matrix::row (char *s) const
{
  if (s == (char *) NULL)
    {
      (*current_liboctave_error_handler) ("invalid row selection");
      return RowVector ();
    }

  char c = *s;
  if (c == 'f' || c == 'F')
    return row (0);
  else if (c == 'l' || c == 'L')
    return row (rows () - 1);
  else
    {
      (*current_liboctave_error_handler) ("invalid row selection");
      return RowVector ();
    }
}

ColumnVector
Matrix::column (int i) const
{
  int nr = rows ();
  if (i < 0 || i >= cols ())
    {
      (*current_liboctave_error_handler) ("invalid column selection");
      return ColumnVector ();
    }

  ColumnVector retval (nr);
  for (int j = 0; j < nr; j++)
    retval.elem (j) = elem (j, i);

  return retval;
}

ColumnVector
Matrix::column (char *s) const
{
  if (s == (char *) NULL)
    {
      (*current_liboctave_error_handler) ("invalid column selection");
      return ColumnVector ();
    }

  char c = *s;
  if (c == 'f' || c == 'F')
    return column (0);
  else if (c == 'l' || c == 'L')
    return column (cols () - 1);
  else
    {
      (*current_liboctave_error_handler) ("invalid column selection");
      return ColumnVector ();
    }
}

Matrix
Matrix::inverse (void) const
{
  int info;
  double rcond;
  return inverse (info, rcond);
}

Matrix
Matrix::inverse (int& info) const
{
  double rcond;
  return inverse (info, rcond);
}

Matrix
Matrix::inverse (int& info, double& rcond) const
{
  int nr = rows ();
  int nc = cols ();
  int len = length ();
  if (nr != nc || nr == 0 || nc == 0)
    {
      (*current_liboctave_error_handler) ("inverse requires square matrix");
      return Matrix ();
    }

  info = 0;

  int *ipvt = new int [nr];
  double *z = new double [nr];
  double *tmp_data = dup (data (), len);

  F77_FCN (dgeco) (tmp_data, &nr, &nc, ipvt, &rcond, z);

  if (rcond + 1.0 == 1.0)
    {
      info = -1;
      copy (tmp_data, data (), len);  // Restore matrix contents.
    }
  else
    {
      int job = 1;
      double dummy;

      F77_FCN (dgedi) (tmp_data, &nr, &nc, ipvt, &dummy, z, &job);
    }

  delete [] ipvt;
  delete [] z;

  return Matrix (tmp_data, nr, nc);
}

ComplexMatrix
Matrix::fourier (void) const
{
  int nr = rows ();
  int nc = cols ();
  int npts, nsamples;
  if (nr == 1 || nc == 1)
    {
      npts = nr > nc ? nr : nc;
      nsamples = 1;
    }
  else
    {
      npts = nr;
      nsamples = nc;
    }

  int nn = 4*npts+15;
  Complex *wsave = new Complex [nn];
  Complex *tmp_data = make_complex (data (), length ());

  F77_FCN (cffti) (&npts, wsave);

  for (int j = 0; j < nsamples; j++)
    F77_FCN (cfftf) (&npts, &tmp_data[npts*j], wsave);

  delete [] wsave;

  return ComplexMatrix (tmp_data, nr, nc);
}

ComplexMatrix
Matrix::ifourier (void) const
{
  int nr = rows ();
  int nc = cols ();
  int npts, nsamples;
  if (nr == 1 || nc == 1)
    {
      npts = nr > nc ? nr : nc;
      nsamples = 1;
    }
  else
    {
      npts = nr;
      nsamples = nc;
    }

  int nn = 4*npts+15;
  Complex *wsave = new Complex [nn];
  Complex *tmp_data = make_complex (data (), length ());

  F77_FCN (cffti) (&npts, wsave);

  for (int j = 0; j < nsamples; j++)
    F77_FCN (cfftb) (&npts, &tmp_data[npts*j], wsave);

  for (j = 0; j < npts*nsamples; j++)
    tmp_data[j] = tmp_data[j] / (double) npts;

  delete [] wsave;

  return ComplexMatrix (tmp_data, nr, nc);
}

DET
Matrix::determinant (void) const
{
  int info;
  double rcond;
  return determinant (info, rcond);
}

DET
Matrix::determinant (int& info) const
{
  double rcond;
  return determinant (info, rcond);
}

DET
Matrix::determinant (int& info, double& rcond) const
{
  DET retval;

  int nr = rows ();
  int nc = cols ();
  if (nr == 0 || nc == 0)
    {
      double d[2];
      d[0] = 1.0;
      d[1] = 0.0;
      return DET (d);
    }

  info = 0;
  int *ipvt = new int [nr];

  double *z = new double [nr];
  double *tmp_data = dup (data (), length ());

  F77_FCN (dgeco) (tmp_data, &nr, &nr, ipvt, &rcond, z);

  if (rcond + 1.0 == 1.0)
    {
      info = -1;
    }
  else
    {
      int job = 10;
      double d[2];
      F77_FCN (dgedi) (tmp_data, &nr, &nr, ipvt, d, z, &job);
      retval = DET (d);
    }

  delete [] tmp_data;
  delete [] ipvt;
  delete [] z;

  return retval;
}

Matrix
Matrix::solve (const Matrix& b) const
{
  int info;
  double rcond;
  return solve (b, info, rcond);
}

Matrix
Matrix::solve (const Matrix& b, int& info) const
{
  double rcond;
  return solve (b, info, rcond);
}

Matrix
Matrix::solve (const Matrix& b, int& info, double& rcond) const
{
  Matrix retval;

  int nr = rows ();
  int nc = cols ();
  if (nr == 0 || nc == 0 || nr != nc || nr != b.rows ())
    {
      (*current_liboctave_error_handler)
	("matrix dimension mismatch solution of linear equations");
      return Matrix ();
    }

  info = 0;
  int *ipvt = new int [nr];

  double *z = new double [nr];
  double *tmp_data = dup (data (), length ());

  F77_FCN (dgeco) (tmp_data, &nr, &nr, ipvt, &rcond, z);

  if (rcond + 1.0 == 1.0)
    {
      info = -2;
    }
  else
    {
      int job = 0;

      double *result = dup (b.data (), b.length ());

      int b_nc = b.cols ();
      for (int j = 0; j < b_nc; j++)
	F77_FCN (dgesl) (tmp_data, &nr, &nr, ipvt, &result[nr*j], &job);

      retval = Matrix (result, b.rows (), b_nc);
    }

  delete [] tmp_data;
  delete [] ipvt;
  delete [] z;

  return retval;
}

ComplexMatrix
Matrix::solve (const ComplexMatrix& b) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b);
}

ComplexMatrix
Matrix::solve (const ComplexMatrix& b, int& info) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b, info);
}

ComplexMatrix
Matrix::solve (const ComplexMatrix& b, int& info, double& rcond) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b, info, rcond);
}

ColumnVector
Matrix::solve (const ColumnVector& b) const
{
  int info; double rcond;
  return solve (b, info, rcond);
}

ColumnVector
Matrix::solve (const ColumnVector& b, int& info) const
{
  double rcond;
  return solve (b, info, rcond);
}

ColumnVector
Matrix::solve (const ColumnVector& b, int& info, double& rcond) const
{
  ColumnVector retval;

  int nr = rows ();
  int nc = cols ();
  if (nr == 0 || nc == 0 || nr != nc || nr != b.length ())
    {
      (*current_liboctave_error_handler)
	("matrix dimension mismatch solution of linear equations");
      return ColumnVector ();
    }

  info = 0;
  int *ipvt = new int [nr];

  double *z = new double [nr];
  double *tmp_data = dup (data (), length ());

  F77_FCN (dgeco) (tmp_data, &nr, &nr, ipvt, &rcond, z);

  if (rcond + 1.0 == 1.0)
    {
      info = -2;
    }
  else
    {
      int job = 0;

      int b_len = b.length ();

      double *result = dup (b.data (), b_len);

      F77_FCN (dgesl) (tmp_data, &nr, &nr, ipvt, result, &job);

      retval = ColumnVector (result, b_len);
    }

  delete [] tmp_data;
  delete [] ipvt;
  delete [] z;

  return retval;
}

ComplexColumnVector
Matrix::solve (const ComplexColumnVector& b) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b);
}

ComplexColumnVector
Matrix::solve (const ComplexColumnVector& b, int& info) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b, info);
}

ComplexColumnVector
Matrix::solve (const ComplexColumnVector& b, int& info, double& rcond) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b, info, rcond);
}

Matrix
Matrix::lssolve (const Matrix& b) const
{
  int info;
  int rank;
  return lssolve (b, info, rank);
}

Matrix
Matrix::lssolve (const Matrix& b, int& info) const
{
  int rank;
  return lssolve (b, info, rank);
}

Matrix
Matrix::lssolve (const Matrix& b, int& info, int& rank) const
{
  int nrhs = b.cols ();

  int m = rows ();
  int n = cols ();

  if (m == 0 || n == 0 || m != b.rows ())
    {
      (*current_liboctave_error_handler)
	("matrix dimension mismatch in solution of least squares problem");
      return Matrix ();
    }

  double *tmp_data = dup (data (), length ());

  int nrr = m > n ? m : n;
  Matrix result (nrr, nrhs);

  int i, j;
  for (j = 0; j < nrhs; j++)
    for (i = 0; i < m; i++)
      result.elem (i, j) = b.elem (i, j);

  double *presult = result.fortran_vec ();

  int len_s = m < n ? m : n;
  double *s = new double [len_s];
  double rcond = -1.0;
  int lwork;
  if (m < n)
    lwork = 3*m + (2*m > nrhs ? (2*m > n ? 2*m : n) : (nrhs > n ? nrhs : n));
  else
    lwork = 3*n + (2*n > nrhs ? (2*n > m ? 2*n : m) : (nrhs > m ? nrhs : m));

  double *work = new double [lwork];

  F77_FCN (dgelss) (&m, &n, &nrhs, tmp_data, &m, presult, &nrr, s,
		    &rcond, &rank, work, &lwork, &info);

  Matrix retval (n, nrhs);
  for (j = 0; j < nrhs; j++)
    for (i = 0; i < n; i++)
      retval.elem (i, j) = result.elem (i, j);

  delete [] tmp_data;
  delete [] s;
  delete [] work;

  return retval;
}

ComplexMatrix
Matrix::lssolve (const ComplexMatrix& b) const
{
  ComplexMatrix tmp (*this);
  return tmp.lssolve (b);
}

ComplexMatrix
Matrix::lssolve (const ComplexMatrix& b, int& info) const
{
  ComplexMatrix tmp (*this);
  return tmp.lssolve (b);
}

ComplexMatrix
Matrix::lssolve (const ComplexMatrix& b, int& info, int& rank) const
{
  ComplexMatrix tmp (*this);
  return tmp.lssolve (b);
}

ColumnVector
Matrix::lssolve (const ColumnVector& b) const
{
  int info;
  int rank; return lssolve (b, info, rank);
}

ColumnVector
Matrix::lssolve (const ColumnVector& b, int& info) const
{
  int rank;
  return lssolve (b, info, rank);
}

ColumnVector
Matrix::lssolve (const ColumnVector& b, int& info, int& rank) const
{
  int nrhs = 1;

  int m = rows ();
  int n = cols ();

  if (m == 0 || n == 0 || m != b.length ())
    {
      (*current_liboctave_error_handler)
	("matrix dimension mismatch in solution of least squares problem");
      return ColumnVector ();
    }

  double *tmp_data = dup (data (), length ());

  int nrr = m > n ? m : n;
  ColumnVector result (nrr);

  int i;
  for (i = 0; i < m; i++)
    result.elem (i) = b.elem (i);

  double *presult = result.fortran_vec ();

  int len_s = m < n ? m : n;
  double *s = new double [len_s];
  double rcond = -1.0;
  int lwork;
  if (m < n)
    lwork = 3*m + (2*m > nrhs ? (2*m > n ? 2*m : n) : (nrhs > n ? nrhs : n));
  else
    lwork = 3*n + (2*n > nrhs ? (2*n > m ? 2*n : m) : (nrhs > m ? nrhs : m));

  double *work = new double [lwork];

  F77_FCN (dgelss) (&m, &n, &nrhs, tmp_data, &m, presult, &nrr, s,
		    &rcond, &rank, work, &lwork, &info);

  ColumnVector retval (n);
  for (i = 0; i < n; i++)
    retval.elem (i) = result.elem (i);

  delete [] tmp_data;
  delete [] s;
  delete [] work;

  return retval;
}

ComplexColumnVector
Matrix::lssolve (const ComplexColumnVector& b) const
{
  ComplexMatrix tmp (*this);
  return tmp.lssolve (b);
}

ComplexColumnVector
Matrix::lssolve (const ComplexColumnVector& b, int& info) const
{
  ComplexMatrix tmp (*this);
  return tmp.lssolve (b, info);
}

ComplexColumnVector
Matrix::lssolve (const ComplexColumnVector& b, int& info, int& rank) const
{
  ComplexMatrix tmp (*this);
  return tmp.lssolve (b, info, rank);
}

Matrix&
Matrix::operator += (const Matrix& a)
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix += operation attempted");
      return *this;
    }

  if (nr == 0 || nc == 0)
    return *this;

  double *d = fortran_vec (); // Ensures only one reference to my privates!

  add2 (d, a.data (), length ());

  return *this;
}

Matrix&
Matrix::operator -= (const Matrix& a)
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix -= operation attempted");
      return *this;
    }

  if (nr == 0 || nc == 0)
    return *this;

  double *d = fortran_vec (); // Ensures only one reference to my privates!

  subtract2 (d, a.data (), length ());

  return *this;
}

Matrix&
Matrix::operator += (const DiagMatrix& a)
{
  if (rows () != a.rows () || cols () != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix += operation attempted");
      return *this;
    }

  for (int i = 0; i < a.length (); i++)
    elem (i, i) += a.elem (i, i);

  return *this;
}

Matrix&
Matrix::operator -= (const DiagMatrix& a)
{
  if (rows () != a.rows () || cols () != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix += operation attempted");
      return *this;
    }

  for (int i = 0; i < a.length (); i++)
    elem (i, i) -= a.elem (i, i);

  return *this;
}

// unary operations

Matrix
Matrix::operator ! (void) const
{
  int nr = rows ();
  int nc = cols ();

  Matrix b (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      b.elem (i, j) = ! elem (i, j);

  return b;
}

// matrix by scalar -> matrix operations.

ComplexMatrix
operator + (const Matrix& a, const Complex& s)
{
  return ComplexMatrix (add (a.data (), a.length (), s),
			a.rows (), a.cols ());
}

ComplexMatrix
operator - (const Matrix& a, const Complex& s)
{
  return ComplexMatrix (subtract (a.data (), a.length (), s),
			a.rows (), a.cols ());
}

ComplexMatrix
operator * (const Matrix& a, const Complex& s)
{
  return ComplexMatrix (multiply (a.data (), a.length (), s),
			a.rows (), a.cols ());
}

ComplexMatrix
operator / (const Matrix& a, const Complex& s)
{
  return ComplexMatrix (divide (a.data (), a.length (), s),
			a.rows (), a.cols ());
}

// scalar by matrix -> matrix operations.

ComplexMatrix
operator + (const Complex& s, const Matrix& a)
{
  assert (0);
  return ComplexMatrix ();
}

ComplexMatrix
operator - (const Complex& s, const Matrix& a)
{
  assert (0);
  return ComplexMatrix ();
}

ComplexMatrix
operator * (const Complex& s, const Matrix& a)
{
  assert (0);
  return ComplexMatrix ();
}

ComplexMatrix
operator / (const Complex& s, const Matrix& a)
{
  assert (0);
  return ComplexMatrix ();
}

// matrix by column vector -> column vector operations

ColumnVector
operator * (const Matrix& m, const ColumnVector& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nc != a.length ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix multiplication attempted");
      return ColumnVector ();
    }

  if (nr == 0 || nc == 0)
    return ColumnVector (0);

  char trans = 'N';
  int ld = nr;
  double alpha = 1.0;
  double beta  = 0.0;
  int i_one = 1;

  double *y = new double [nr];

  F77_FCN (dgemv) (&trans, &nr, &nc, &alpha, m.data (), &ld, a.data (),
		   &i_one, &beta, y, &i_one, 1L); 

  return ColumnVector (y, nr);
}

ComplexColumnVector
operator * (const Matrix& m, const ComplexColumnVector& a)
{
  ComplexMatrix tmp (m);
  return tmp * a;
}

// matrix by diagonal matrix -> matrix operations

Matrix
operator + (const Matrix& m, const DiagMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix addition attempted");
      return Matrix ();
    }

  if (nr == 0 || nc == 0)
    return Matrix (nr, nc);

  Matrix result (m);
  int a_len = a.length ();
  for (int i = 0; i < a_len; i++)
    result.elem (i, i) += a.elem (i, i);

  return result;
}

Matrix
operator - (const Matrix& m, const DiagMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix subtraction attempted");
      return Matrix ();
    }

  if (nr == 0 || nc == 0)
    return Matrix (nr, nc);

  Matrix result (m);
  int a_len = a.length ();
  for (int i = 0; i < a_len; i++)
    result.elem (i, i) -= a.elem (i, i);

  return result;
}

Matrix
operator * (const Matrix& m, const DiagMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  int a_nr = a.rows ();
  int a_nc = a.cols ();
  if (nc != a_nr)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix multiplication attempted");
      return Matrix ();
    }

  if (nr == 0 || nc == 0 || a_nc == 0)
    return Matrix (nr, a_nc, 0.0);

  double *c = new double [nr*a_nc];
  double *ctmp = (double *) NULL;

  int a_len = a.length ();
  for (int j = 0; j < a_len; j++)
    {
      int idx = j * nr;
      ctmp = c + idx;
      if (a.elem (j, j) == 1.0)
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = m.elem (i, j);
	}
      else if (a.elem (j, j) == 0.0)
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = 0.0;
	}
      else
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = a.elem (j, j) * m.elem (i, j);
	}
    }

  if (a_nr < a_nc)
    {
      for (int i = nr * nc; i < nr * a_nc; i++)
	ctmp[i] = 0.0;
    }

  return Matrix (c, nr, a_nc);
}

ComplexMatrix
operator + (const Matrix& m, const ComplexDiagMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix addition attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  ComplexMatrix result (m);
  for (int i = 0; i < a.length (); i++)
    result.elem (i, i) += a.elem (i, i);

  return result;
}

ComplexMatrix
operator - (const Matrix& m, const ComplexDiagMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix subtraction attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  ComplexMatrix result (m);
  for (int i = 0; i < a.length (); i++)
    result.elem (i, i) -= a.elem (i, i);

  return result;
}

ComplexMatrix
operator * (const Matrix& m, const ComplexDiagMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  int a_nr = a.rows ();
  int a_nc = a.cols ();
  if (nc != a_nr)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix multiplication attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0 || a_nc == 0)
    return ComplexMatrix (nr, a_nc, 0.0);

  Complex *c = new Complex [nr*a_nc];
  Complex *ctmp = (Complex *) NULL;

  for (int j = 0; j < a.length (); j++)
    {
      int idx = j * nr;
      ctmp = c + idx;
      if (a.elem (j, j) == 1.0)
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = m.elem (i, j);
	}
      else if (a.elem (j, j) == 0.0)
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = 0.0;
	}
      else
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = a.elem (j, j) * m.elem (i, j);
	}
    }

  if (a_nr < a_nc)
    {
      for (int i = nr * nc; i < nr * a_nc; i++)
	ctmp[i] = 0.0;
    }

  return ComplexMatrix (c, nr, a_nc);
}

// matrix by matrix -> matrix operations

Matrix
operator * (const Matrix& m, const Matrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  int a_nr = a.rows ();
  int a_nc = a.cols ();
  if (nc != a_nr)
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix multiplication attempted");
      return Matrix ();
    }

  if (nr == 0 || nc == 0 || a_nc == 0)
    return Matrix (nr, a_nc, 0.0);

  char trans  = 'N';
  char transa = 'N';

  int ld  = nr;
  int lda = a_nr;

  double alpha = 1.0;
  double beta  = 0.0;

  double *c = new double [nr*a_nc];

  F77_FCN (dgemm) (&trans, &transa, &nr, &a_nc, &nc, &alpha, m.data (),
		   &ld, a.data (), &lda, &beta, c, &nr, 1L, 1L);

  return Matrix (c, nr, a_nc);
}

ComplexMatrix
operator * (const Matrix& m, const ComplexMatrix& a)
{
  ComplexMatrix tmp (m);
  return tmp * a;
}

ComplexMatrix
operator + (const Matrix& m, const ComplexMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix addition attempted");
      return ComplexMatrix ();
    }

  return ComplexMatrix (add (m.data (), a.data (), m.length ()), nr, nc);
}

ComplexMatrix
operator - (const Matrix& m, const ComplexMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix subtraction attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  return ComplexMatrix (subtract (m.data (), a.data (), m.length ()), nr, nc);
}

ComplexMatrix
product (const Matrix& m, const ComplexMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix product attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  return ComplexMatrix (multiply (m.data (), a.data (), m.length ()), nr, nc);
}

ComplexMatrix
quotient (const Matrix& m, const ComplexMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix quotient attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  return ComplexMatrix (divide (m.data (), a.data (), m.length ()), nr, nc);
}

// other operations.

Matrix
map (d_d_Mapper f, const Matrix& a)
{
  Matrix b (a);
  b.map (f);
  return b;
}

void
Matrix::map (d_d_Mapper f)
{
  double *d = fortran_vec (); // Ensures only one reference to my privates!

  for (int i = 0; i < length (); i++)
    d[i] = f (d[i]);
}

// XXX FIXME XXX Do these really belong here?  They should maybe be
// cleaned up a bit, no?  What about corresponding functions for the
// Vectors?

Matrix
Matrix::all (void) const
{
  int nr = rows ();
  int nc = cols ();
  Matrix retval;
  if (nr > 0 && nc > 0)
    {
      if (nr == 1)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = 1.0;
	  for (int j = 0; j < nc; j++)
	    {
	      if (elem (0, j) == 0.0)
		{
		  retval.elem (0, 0) = 0.0;
		  break;
		}
	    }
	}
      else if (nc == 1)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = 1.0;
	  for (int i = 0; i < nr; i++)
	    {
	      if (elem (i, 0) == 0.0)
		{
		  retval.elem (0, 0) = 0.0;
		  break;
		}
	    }
	}
      else
	{
	  retval.resize (1, nc);
	  for (int j = 0; j < nc; j++)
	    {
	      retval.elem (0, j) = 1.0;
	      for (int i = 0; i < nr; i++)
		{
		  if (elem (i, j) == 0.0)
		    {
		      retval.elem (0, j) = 0.0;
		      break;
		    }
		}
	    }
	}
    }
  return retval;
}

Matrix
Matrix::any (void) const
{
  int nr = rows ();
  int nc = cols ();
  Matrix retval;
  if (nr > 0 && nc > 0)
    {
      if (nr == 1)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = 0.0;
	  for (int j = 0; j < nc; j++)
	    {
	      if (elem (0, j) != 0.0)
		{
		  retval.elem (0, 0) = 1.0;
		  break;
		}
	    }
	}
      else if (nc == 1)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = 0.0;
	  for (int i = 0; i < nr; i++)
	    {
	      if (elem (i, 0) != 0.0)
		{
		  retval.elem (0, 0) = 1.0;
		  break;
		}
	    }
	}
      else
	{
	  retval.resize (1, nc);
	  for (int j = 0; j < nc; j++)
	    {
	      retval.elem (0, j) = 0.0;
	      for (int i = 0; i < nr; i++)
		{
		  if (elem (i, j) != 0.0)
		    {
		      retval.elem (0, j) = 1.0;
		      break;
		    }
		}
	    }
	}
    }
  return retval;
}

Matrix
Matrix::cumprod (void) const
{
  Matrix retval;

  int nr = rows ();
  int nc = cols ();

  if (nr == 1)
    {
      retval.resize (1, nc);
      if (nc > 0)
	{
	  double prod = elem (0, 0);
	  for (int j = 0; j < nc; j++)
	    {
	      retval.elem (0, j) = prod;
	      if (j < nc - 1)
		prod *= elem (0, j+1);
	    }
	}
    }
  else if (nc == 1)
    {
      retval.resize (nr, 1);
      if (nr > 0)
	{
	  double prod = elem (0, 0);
	  for (int i = 0; i < nr; i++)
	    {
	      retval.elem (i, 0) = prod;
	      if (i < nr - 1)
		prod *= elem (i+1, 0);
	    }
	}
    }
  else
    {
      retval.resize (nr, nc);
      if (nr > 0 && nc > 0)
	{
	  for (int j = 0; j < nc; j++)
	    {
	      double prod = elem (0, j);
	      for (int i = 0; i < nr; i++)
		{
		  retval.elem (i, j) = prod;
		  if (i < nr - 1)
		    prod *= elem (i+1, j);
		}
	    }
	}
    }
  return retval;
}

Matrix
Matrix::cumsum (void) const
{
  Matrix retval;

  int nr = rows ();
  int nc = cols ();

  if (nr == 1)
    {
      retval.resize (1, nc);
      if (nc > 0)
	{
	  double sum = elem (0, 0);
	  for (int j = 0; j < nc; j++)
	    {
	      retval.elem (0, j) = sum;
	      if (j < nc - 1)
		sum += elem (0, j+1);
	    }
	}
    }
  else if (nc == 1)
    {
      retval.resize (nr, 1);
      if (nr > 0)
	{
	  double sum = elem (0, 0);
	  for (int i = 0; i < nr; i++)
	    {
	      retval.elem (i, 0) = sum;
	      if (i < nr - 1)
		sum += elem (i+1, 0);
	    }
	}
    }
  else
    {
      retval.resize (nr, nc);
      if (nr > 0 && nc > 0)
	{
	  for (int j = 0; j < nc; j++)
	    {
	      double sum = elem (0, j);
	      for (int i = 0; i < nr; i++)
		{
		  retval.elem (i, j) = sum;
		  if (i < nr - 1)
		    sum += elem (i+1, j);
		}
	    }
	}
    }
  return retval;
}

Matrix
Matrix::prod (void) const
{
  Matrix retval;

  int nr = rows ();
  int nc = cols ();

  if (nr == 1)
    {
      retval.resize (1, 1);
      retval.elem (0, 0) = 1.0;
      for (int j = 0; j < nc; j++)
	retval.elem (0, 0) *= elem (0, j);
    }
  else if (nc == 1)
    {
      retval.resize (1, 1);
      retval.elem (0, 0) = 1.0;
      for (int i = 0; i < nr; i++)
	retval.elem (0, 0) *= elem (i, 0);
    }
  else
    {
      if (nc == 0)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = 1.0;
	}
      else
	retval.resize (1, nc);

      for (int j = 0; j < nc; j++)
	{
	  retval.elem (0, j) = 1.0;
	  for (int i = 0; i < nr; i++)
	    retval.elem (0, j) *= elem (i, j);
	}
    }
  return retval;
}

Matrix
Matrix::sum (void) const
{
  Matrix retval;

  int nr = rows ();
  int nc = cols ();

  if (nr == 1)
    {
      retval.resize (1, 1);
      retval.elem (0, 0) = 0.0;
      for (int j = 0; j < nc; j++)
	retval.elem (0, 0) += elem (0, j);
    }
  else if (nc == 1)
    {
      retval.resize (1, 1);
      retval.elem (0, 0) = 0.0;
      for (int i = 0; i < nr; i++)
	retval.elem (0, 0) += elem (i, 0);
    }
  else
    {
      if (nc == 0)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = 0.0;
	}
      else
	retval.resize (1, nc);

      for (int j = 0; j < nc; j++)
	{
	  retval.elem (0, j) = 0.0;
	  for (int i = 0; i < nr; i++)
	    retval.elem (0, j) += elem (i, j);
	}
    }
  return retval;
}

Matrix
Matrix::sumsq (void) const
{
  Matrix retval;

  int nr = rows ();
  int nc = cols ();

  if (nr == 1)
    {
      retval.resize (1, 1);
      retval.elem (0, 0) = 0.0;
      for (int j = 0; j < nc; j++)
	{
	  double d = elem (0, j);
	  retval.elem (0, 0) += d * d;
	}
    }
  else if (nc == 1)
    {
      retval.resize (1, 1);
      retval.elem (0, 0) = 0.0;
      for (int i = 0; i < nr; i++)
	{
	  double d = elem (i, 0);
	  retval.elem (0, 0) += d * d;
	}
    }
  else
    {
      retval.resize (1, nc);
      for (int j = 0; j < nc; j++)
	{
	  retval.elem (0, j) = 0.0;
	  for (int i = 0; i < nr; i++)
	    {
	      double d = elem (i, j);
	      retval.elem (0, j) += d * d;
	    }
	}
    }
  return retval;
}

ColumnVector
Matrix::diag (void) const
{
  return diag (0);
}

ColumnVector
Matrix::diag (int k) const
{
  int nnr = rows ();
  int nnc = cols ();
  if (k > 0)
    nnc -= k;
  else if (k < 0)
    nnr += k;

  ColumnVector d;

  if (nnr > 0 && nnc > 0)
    {
      int ndiag = (nnr < nnc) ? nnr : nnc;

      d.resize (ndiag);

      if (k > 0)
	{
	  for (int i = 0; i < ndiag; i++)
	    d.elem (i) = elem (i, i+k);
	}
      else if ( k < 0)
	{
	  for (int i = 0; i < ndiag; i++)
	    d.elem (i) = elem (i-k, i);
	}
      else
	{
	  for (int i = 0; i < ndiag; i++)
	    d.elem (i) = elem (i, i);
	}
    }
  else
    cerr << "diag: requested diagonal out of range\n";

  return d;
}

ColumnVector
Matrix::row_min (void) const
{
  ColumnVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nr);

      for (int i = 0; i < nr; i++)
	{
	  double res = elem (i, 0);
	  for (int j = 1; j < nc; j++)
	    if (elem (i, j) < res)
	      res = elem (i, j);
	  result.elem (i) = res;
	}
    }

  return result;
}

ColumnVector
Matrix::row_min_loc (void) const
{
  ColumnVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nr);

      for (int i = 0; i < nr; i++)
        {
          int res = 0;
          for (int j = 0; j < nc; j++)
            if (elem (i, j) < elem (i, res))
              res = j;
          result.elem (i) = (double) (res + 1);
        }
    }

  return result;
}

ColumnVector
Matrix::row_max (void) const
{
  ColumnVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nr);

      for (int i = 0; i < nr; i++)
	{
	  double res = elem (i, 0);
	  for (int j = 1; j < nc; j++)
	    if (elem (i, j) > res)
	      res = elem (i, j);
	  result.elem (i) = res;
	}
    }

  return result;
}

ColumnVector
Matrix::row_max_loc (void) const
{
  ColumnVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nr);

      for (int i = 0; i < nr; i++)
        {
          int res = 0;
          for (int j = 0; j < nc; j++)
            if (elem (i, j) > elem (i, res))
              res = j;
          result.elem (i) = (double) (res + 1);
        }
    }

  return result;
}

RowVector
Matrix::column_min (void) const
{
  RowVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nc);

      for (int j = 0; j < nc; j++)
	{
	  double res = elem (0, j);
	  for (int i = 1; i < nr; i++)
	    if (elem (i, j) < res)
	      res = elem (i, j);
	  result.elem (j) = res;
	}
    }

  return result;
}
RowVector
Matrix::column_min_loc (void) const
{
  RowVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nc);

      for (int j = 0; j < nc; j++)
        {
          int res = 0;
          for (int i = 0; i < nr; i++)
            if (elem (i, j) < elem (res, j))
              res = i;
          result.elem (j) = (double) (res + 1);
        }
    }

  return result;
}


RowVector
Matrix::column_max (void) const
{
  RowVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nc);

      for (int j = 0; j < nc; j++)
	{
	  double res = elem (0, j);
	  for (int i = 1; i < nr; i++)
	    if (elem (i, j) > res)
	      res = elem (i, j);
	  result.elem (j) = res;
	}
    }

  return result;
}

RowVector
Matrix::column_max_loc (void) const
{
  RowVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nc);

      for (int j = 0; j < nc; j++)
        {
          int res = 0;
          for (int i = 0; i < nr; i++)
            if (elem (i, j) > elem (res, j))
              res = i;
          result.elem (j) = (double) (res + 1);
        }
    }

  return result;
}

ostream&
operator << (ostream& os, const Matrix& a)
{
//  int field_width = os.precision () + 7;
  for (int i = 0; i < a.rows (); i++)
    {
      for (int j = 0; j < a.cols (); j++)
	os << " " /* setw (field_width) */ << a.elem (i, j);
      os << "\n";
    }
  return os;
}

istream&
operator >> (istream& is, Matrix& a)
{
  int nr = a.rows ();
  int nc = a.cols ();

  if (nr < 1 || nc < 1)
    is.clear (ios::badbit);
  else
    {
      double tmp;
      for (int i = 0; i < nr; i++)
	for (int j = 0; j < nc; j++)
	  {
	    is >> tmp;
	    if (is)
	      a.elem (i, j) = tmp;
	    else
	      break;
	  }
    }

  return is;
}

/*
 * Complex Matrix class
 */

ComplexMatrix::ComplexMatrix (const Matrix& a)
  : Array2<Complex> (a.rows (), a.cols ())
{
  for (int j = 0; j < cols (); j++)
    for (int i = 0; i < rows (); i++)
      elem (i, j) = a.elem (i, j);
}

ComplexMatrix::ComplexMatrix (const DiagMatrix& a)
  : Array2<Complex> (a.rows (), a.cols ())
{
  for (int i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

ComplexMatrix::ComplexMatrix (const ComplexDiagMatrix& a)
  : Array2<Complex> (a.rows (), a.cols ())
{
  for (int i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

#if 0
ComplexMatrix&
ComplexMatrix::resize (int r, int c)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimensions");
      return *this;
    }

  int new_len = r * c;
  Complex* new_data = (Complex *) NULL;
  if (new_len > 0)
    {
      new_data = new Complex [new_len];

      int min_r = nr < r ? nr : r;
      int min_c = nc < c ? nc : c;

      for (int j = 0; j < min_c; j++)
	for (int i = 0; i < min_r; i++)
	  new_data[r*j+i] = elem (i, j);
    }

  delete [] data;
  nr = r;
  nc = c;
  len = new_len;
  data = new_data;

  return *this;
}

ComplexMatrix&
ComplexMatrix::resize (int r, int c, double val)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimensions");
      return *this;
    }

  int new_len = r * c;
  Complex *new_data = (Complex *) NULL;
  if (new_len > 0)
    {
      new_data = new Complex [new_len];

// There may be faster or cleaner ways to do this.

      if (r > nr || c > nc)
	copy (new_data, new_len, val);

      int min_r = nr < r ? nr : r;
      int min_c = nc < c ? nc : c;

      for (int j = 0; j < min_c; j++)
	for (int i = 0; i < min_r; i++)
	  new_data[r*j+i] = elem (i, j);
    }

  delete [] data;
  nr = r;
  nc = c;
  len = new_len;
  data = new_data;

  return *this;
}

ComplexMatrix&
ComplexMatrix::resize (int r, int c, const Complex& val)
{
  if (r < 0 || c < 0)
    {
      (*current_liboctave_error_handler)
	("can't resize to negative dimensions");
      return *this;
    }

  int new_len = r * c;
  Complex *new_data = (Complex *) NULL;
  if (new_len > 0)
    {
      new_data = new Complex [new_len];

// There may be faster or cleaner ways to do this.

      if (r > nr || c > nc)
	copy (new_data, new_len, val);

      int min_r = nr < r ? nr : r;
      int min_c = nc < c ? nc : c;

      for (int j = 0; j < min_c; j++)
	for (int i = 0; i < min_r; i++)
	  new_data[r*j+i] = elem (i, j);
    }

  delete [] data;
  nr = r;
  nc = c;
  len = new_len;
  data = new_data;

  return *this;
}
#endif

int
ComplexMatrix::operator == (const ComplexMatrix& a) const
{
  if (rows () != a.rows () || cols () != a.cols ())
    return 0;

  return equal (data (), a.data (), length ());
}

int
ComplexMatrix::operator != (const ComplexMatrix& a) const
{
  return !(*this == a);
}

// destructive insert/delete/reorder operations

ComplexMatrix&
ComplexMatrix::insert (const Matrix& a, int r, int c)
{
  int a_nr = a.rows ();
  int a_nc = a.cols ();
  if (r < 0 || r + a_nr - 1 > rows () || c < 0 || c + a_nc - 1 > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  for (int j = 0; j < a_nc; j++)
    for (int i = 0; i < a_nr; i++)
      elem (r+i, c+j) = a.elem (i, j);

  return *this;
}

ComplexMatrix&
ComplexMatrix::insert (const RowVector& a, int r, int c)
{
  int a_len = a.length ();
  if (r < 0 || r >= rows () || c < 0 || c + a_len - 1 > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  for (int i = 0; i < a_len; i++)
    elem (r, c+i) = a.elem (i);

  return *this;
}

ComplexMatrix&
ComplexMatrix::insert (const ColumnVector& a, int r, int c)
{
  int a_len = a.length ();
  if (r < 0 || r + a_len - 1 > rows () || c < 0 || c >= cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  for (int i = 0; i < a_len; i++)
    elem (r+i, c) = a.elem (i);

  return *this;
}

ComplexMatrix&
ComplexMatrix::insert (const DiagMatrix& a, int r, int c)
{
  if (r < 0 || r + a.rows () - 1 > rows ()
      || c < 0 || c + a.cols () - 1 > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  for (int i = 0; i < a.length (); i++)
    elem (r+i, c+i) = a.elem (i, i);

  return *this;
}

ComplexMatrix&
ComplexMatrix::insert (const ComplexMatrix& a, int r, int c)
{
  int a_nr = a.rows ();
  int a_nc = a.cols ();
  if (r < 0 || r + a_nr - 1 > rows () || c < 0 || c + a_nc - 1 > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  for (int j = 0; j < a_nc; j++)
    for (int i = 0; i < a_nr; i++)
      elem (r+i, c+j) = a.elem (i, j);

  return *this;
}

ComplexMatrix&
ComplexMatrix::insert (const ComplexRowVector& a, int r, int c)
{
  int a_len = a.length ();
  if (r < 0 || r >= rows () || c < 0 || c + a_len - 1 > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  for (int i = 0; i < a_len; i++)
    elem (r, c+i) = a.elem (i);

  return *this;
}

ComplexMatrix&
ComplexMatrix::insert (const ComplexColumnVector& a, int r, int c)
{
  int a_len = a.length ();
  if (r < 0 || r + a_len - 1 > rows () || c < 0 || c >= cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  for (int i = 0; i < a_len; i++)
    elem (r+i, c) = a.elem (i);

  return *this;
}

ComplexMatrix&
ComplexMatrix::insert (const ComplexDiagMatrix& a, int r, int c)
{
  if (r < 0 || r + a.rows () - 1 > rows ()
      || c < 0 || c + a.cols () - 1 > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  for (int i = 0; i < a.length (); i++)
    elem (r+i, c+i) = a.elem (i, i);

  return *this;
}

ComplexMatrix&
ComplexMatrix::fill (double val)
{
  int nr = rows ();
  int nc = cols ();
  if (nr > 0 && nc > 0)
    for (int j = 0; j < nc; j++)
      for (int i = 0; i < nr; i++)
	elem (i, j) = val;

  return *this;
}

ComplexMatrix&
ComplexMatrix::fill (const Complex& val)
{
  int nr = rows ();
  int nc = cols ();
  if (nr > 0 && nc > 0)
    for (int j = 0; j < nc; j++)
      for (int i = 0; i < nr; i++)
	elem (i, j) = val;

  return *this;
}

ComplexMatrix&
ComplexMatrix::fill (double val, int r1, int c1, int r2, int c2)
{
  int nr = rows ();
  int nc = rows ();
  if (r1 < 0 || r2 < 0 || c1 < 0 || c2 < 0
      || r1 >= nr || r2 >= nr || c1 >= nc || c2 >= nc)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  if (r1 > r2) { int tmp = r1; r1 = r2; r2 = tmp; }
  if (c1 > c2) { int tmp = c1; c1 = c2; c2 = tmp; }

  for (int j = c1; j <= c2; j++)
    for (int i = r1; i <= r2; i++)
      elem (i, j) = val;

  return *this;
}

ComplexMatrix&
ComplexMatrix::fill (const Complex& val, int r1, int c1, int r2, int c2)
{
  int nr = rows ();
  int nc = rows ();
  if (r1 < 0 || r2 < 0 || c1 < 0 || c2 < 0
      || r1 >= nr || r2 >= nr || c1 >= nc || c2 >= nc)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  if (r1 > r2) { int tmp = r1; r1 = r2; r2 = tmp; }
  if (c1 > c2) { int tmp = c1; c1 = c2; c2 = tmp; }

  for (int j = c1; j <= c2; j++)
    for (int i = r1; i <= r2; i++)
      elem (i, j) = val;

  return *this;
}

ComplexMatrix
ComplexMatrix::append (const Matrix& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.rows ())
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return *this;
    }

  int nc_insert = nc;
  ComplexMatrix retval (nr, nc + a.cols ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::append (const RowVector& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nr != 1)
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return *this;
    }

  int nc_insert = nc;
  ComplexMatrix retval (nr, nc + a.length ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::append (const ColumnVector& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.length ())
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return *this;
    }

  int nc_insert = nc;
  ComplexMatrix retval (nr, nc + 1);
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::append (const DiagMatrix& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.rows ())
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return *this;
    }

  int nc_insert = nc;
  ComplexMatrix retval (nr, nc + a.cols ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::append (const ComplexMatrix& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.rows ())
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return *this;
    }

  int nc_insert = nc;
  ComplexMatrix retval (nr, nc + a.cols ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::append (const ComplexRowVector& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nr != 1)
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return *this;
    }

  int nc_insert = nc;
  ComplexMatrix retval (nr, nc + a.length ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::append (const ComplexColumnVector& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.length ())
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return *this;
    }

  int nc_insert = nc;
  ComplexMatrix retval (nr, nc + 1);
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::append (const ComplexDiagMatrix& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.rows ())
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return *this;
    }

  int nc_insert = nc;
  ComplexMatrix retval (nr, nc + a.cols ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const Matrix& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("column dimension mismatch for stack");
      return *this;
    }

  int nr_insert = nr;
  ComplexMatrix retval (nr + a.rows (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const RowVector& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nc != a.length ())
    {
      (*current_liboctave_error_handler)
	("column dimension mismatch for stack");
      return *this;
    }

  int nr_insert = nr;
  ComplexMatrix retval (nr + 1, nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const ColumnVector& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nc != 1)
    {
      (*current_liboctave_error_handler)
	("column dimension mismatch for stack");
      return *this;
    }

  int nr_insert = nr;
  ComplexMatrix retval (nr + a.length (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const DiagMatrix& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("column dimension mismatch for stack");
      return *this;
    }

  int nr_insert = nr;
  ComplexMatrix retval (nr + a.rows (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const ComplexMatrix& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("column dimension mismatch for stack");
      return *this;
    }

  int nr_insert = nr;
  ComplexMatrix retval (nr + a.rows (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const ComplexRowVector& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nc != a.length ())
    {
      (*current_liboctave_error_handler)
	("column dimension mismatch for stack");
      return *this;
    }

  int nr_insert = nr;
  ComplexMatrix retval (nr + 1, nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const ComplexColumnVector& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nc != 1)
    {
      (*current_liboctave_error_handler)
	("column dimension mismatch for stack");
      return *this;
    }

  int nr_insert = nr;
  ComplexMatrix retval (nr + a.length (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const ComplexDiagMatrix& a) const
{
  int nr = rows ();
  int nc = cols ();
  if (nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("column dimension mismatch for stack");
      return *this;
    }

  int nr_insert = nr;
  ComplexMatrix retval (nr + a.rows (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::hermitian (void) const
{
  int nr = rows ();
  int nc = cols ();
  ComplexMatrix result;
  if (length () > 0)
    {
      result.resize (nc, nr);
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  result.elem (j, i) = conj (elem (i, j));
    }
  return result;
}

ComplexMatrix
ComplexMatrix::transpose (void) const
{
  int nr = rows ();
  int nc = cols ();
  ComplexMatrix result (nc, nr);
  if (length () > 0)
    {
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  result.elem (j, i) = elem (i, j);
    }
  return result;
}

Matrix
real (const ComplexMatrix& a)
{
  int a_len = a.length ();
  Matrix retval;
  if (a_len > 0)
    retval = Matrix (real_dup (a.data (), a_len), a.rows (), a.cols ());
  return retval;
}

Matrix
imag (const ComplexMatrix& a)
{
  int a_len = a.length ();
  Matrix retval;
  if (a_len > 0)
    retval = Matrix (imag_dup (a.data (), a_len), a.rows (), a.cols ());
  return retval;
}

ComplexMatrix
conj (const ComplexMatrix& a)
{
  int a_len = a.length ();
  ComplexMatrix retval;
  if (a_len > 0)
    retval = ComplexMatrix (conj_dup (a.data (), a_len), a.rows (),
			    a.cols ());
  return retval;
}

// resize is the destructive equivalent for this one

ComplexMatrix
ComplexMatrix::extract (int r1, int c1, int r2, int c2) const
{
  if (r1 > r2) { int tmp = r1; r1 = r2; r2 = tmp; }
  if (c1 > c2) { int tmp = c1; c1 = c2; c2 = tmp; }

  int new_r = r2 - r1 + 1;
  int new_c = c2 - c1 + 1;

  ComplexMatrix result (new_r, new_c);

  for (int j = 0; j < new_c; j++)
    for (int i = 0; i < new_r; i++)
      result.elem (i, j) = elem (r1+i, c1+j);

  return result;
}

// extract row or column i.

ComplexRowVector
ComplexMatrix::row (int i) const
{
  int nc = cols ();
  if (i < 0 || i >= rows ())
    {
      (*current_liboctave_error_handler) ("invalid row selection");
      return ComplexRowVector ();
    }

  ComplexRowVector retval (nc);
  for (int j = 0; j < cols (); j++)
    retval.elem (j) = elem (i, j);

  return retval;
}

ComplexRowVector
ComplexMatrix::row (char *s) const
{
  if (s == (char *) NULL)
    {
      (*current_liboctave_error_handler) ("invalid row selection");
      return ComplexRowVector ();
    }

  char c = *s;
  if (c == 'f' || c == 'F')
    return row (0);
  else if (c == 'l' || c == 'L')
    return row (rows () - 1);
  else
    {
      (*current_liboctave_error_handler) ("invalid row selection");
      return ComplexRowVector ();
    }
}

ComplexColumnVector
ComplexMatrix::column (int i) const
{
  int nr = rows ();
  if (i < 0 || i >= cols ())
    {
      (*current_liboctave_error_handler) ("invalid column selection");
      return ComplexColumnVector ();
    }

  ComplexColumnVector retval (nr);
  for (int j = 0; j < nr; j++)
    retval.elem (j) = elem (j, i);

  return retval;
}

ComplexColumnVector
ComplexMatrix::column (char *s) const
{
  if (s == (char *) NULL)
    {
      (*current_liboctave_error_handler) ("invalid column selection");
      return ComplexColumnVector ();
    }

  char c = *s;
  if (c == 'f' || c == 'F')
    return column (0);
  else if (c == 'l' || c == 'L')
    return column (cols () - 1);
  else
    {
      (*current_liboctave_error_handler) ("invalid column selection");
      return ComplexColumnVector ();
    }
}

ComplexMatrix
ComplexMatrix::inverse (void) const
{
  int info;
  double rcond; return inverse (info, rcond);
}

ComplexMatrix
ComplexMatrix::inverse (int& info) const
{
  double rcond;
  return inverse (info, rcond);
}

ComplexMatrix
ComplexMatrix::inverse (int& info, double& rcond) const
{
  int nr = rows ();
  int nc = cols ();
  int len = length ();
  if (nr != nc)
    {
      (*current_liboctave_error_handler) ("inverse requires square matrix");
      return ComplexMatrix ();
    }

  info = 0;

  int *ipvt = new int [nr];
  Complex *z = new Complex [nr];
  Complex *tmp_data = dup (data (), len);

  F77_FCN (zgeco) (tmp_data, &nr, &nc, ipvt, &rcond, z);

  if (rcond + 1.0 == 1.0)
    {
      info = -1;
      copy (tmp_data, data (), len);  // Restore contents.
    }
  else
    {
      int job = 1;
      Complex dummy;

      F77_FCN (zgedi) (tmp_data, &nr, &nc, ipvt, &dummy, z, &job);
    }

  delete [] ipvt;
  delete [] z;

  return ComplexMatrix (tmp_data, nr, nc);
}

ComplexMatrix
ComplexMatrix::fourier (void) const
{
  int nr = rows ();
  int nc = cols ();
  int npts, nsamples;
  if (nr == 1 || nc == 1)
    {
      npts = nr > nc ? nr : nc;
      nsamples = 1;
    }
  else
    {
      npts = nr;
      nsamples = nc;
    }

  int nn = 4*npts+15;
  Complex *wsave = new Complex [nn];
  Complex *tmp_data = dup (data (), length ());

  F77_FCN (cffti) (&npts, wsave);

  for (int j = 0; j < nsamples; j++)
    F77_FCN (cfftf) (&npts, &tmp_data[npts*j], wsave);

  delete [] wsave;

  return ComplexMatrix (tmp_data, nr, nc);
}

ComplexMatrix
ComplexMatrix::ifourier (void) const
{
  int nr = rows ();
  int nc = cols ();
  int npts, nsamples;
  if (nr == 1 || nc == 1)
    {
      npts = nr > nc ? nr : nc;
      nsamples = 1;
    }
  else
    {
      npts = nr;
      nsamples = nc;
    }

  int nn = 4*npts+15;
  Complex *wsave = new Complex [nn];
  Complex *tmp_data = dup (data (), length ());

  F77_FCN (cffti) (&npts, wsave);

  for (int j = 0; j < nsamples; j++)
    F77_FCN (cfftb) (&npts, &tmp_data[npts*j], wsave);

  for (j = 0; j < npts*nsamples; j++)
    tmp_data[j] = tmp_data[j] / (double) npts;

  delete [] wsave;

  return ComplexMatrix (tmp_data, nr, nc);
}

ComplexDET
ComplexMatrix::determinant (void) const
{
  int info;
  double rcond;
  return determinant (info, rcond);
}

ComplexDET
ComplexMatrix::determinant (int& info) const
{
  double rcond;
  return determinant (info, rcond);
}

ComplexDET
ComplexMatrix::determinant (int& info, double& rcond) const
{
  ComplexDET retval;

  int nr = rows ();
  int nc = cols ();
  if (nr == 0 || nc == 0)
    {
      Complex d[2];
      d[0] = 1.0;
      d[1] = 0.0;
      return ComplexDET (d);
    }

  info = 0;
  int *ipvt = new int [nr];

  Complex *z = new Complex [nr];
  Complex *tmp_data = dup (data (), length ());

  F77_FCN (zgeco) (tmp_data, &nr, &nr, ipvt, &rcond, z);

  if (rcond + 1.0 == 1.0)
    {
      info = -1;
    }
  else
    {
      int job = 10;
      Complex d[2];
      F77_FCN (zgedi) (tmp_data, &nr, &nr, ipvt, d, z, &job);
      retval = ComplexDET (d);
    }

  delete [] tmp_data;
  delete [] ipvt;
  delete [] z;

  return retval;
}

ComplexMatrix
ComplexMatrix::solve (const Matrix& b) const
{
  int info;
  double rcond;
  return solve (b, info, rcond);
}

ComplexMatrix
ComplexMatrix::solve (const Matrix& b, int& info) const
{
  double rcond;
  return solve (b, info, rcond);
}

ComplexMatrix
ComplexMatrix::solve (const Matrix& b, int& info, double& rcond) const
{
  ComplexMatrix tmp (b);
  return solve (tmp, info, rcond);
}

ComplexMatrix
ComplexMatrix::solve (const ComplexMatrix& b) const
{
  int info;
  double rcond;
  return solve (b, info, rcond);
}

ComplexMatrix
ComplexMatrix::solve (const ComplexMatrix& b, int& info) const
{
  double rcond;
  return solve (b, info, rcond);
}
ComplexMatrix
ComplexMatrix::solve (const ComplexMatrix& b, int& info, double& rcond) const
{
  ComplexMatrix retval;

  int nr = rows ();
  int nc = cols ();
  int b_nr = b.rows ();
  int b_nc = b.cols ();
  if (nr == 0 || nc == 0 || nr != nc || nr != b_nr)
    {
      (*current_liboctave_error_handler)
	("matrix dimension mismatch in solution of linear equations");
      return ComplexMatrix ();
    }

  info = 0;
  int *ipvt = new int [nr];

  Complex *z = new Complex [nr];
  Complex *tmp_data = dup (data (), length ());

  F77_FCN (zgeco) (tmp_data, &nr, &nr, ipvt, &rcond, z);

  if (rcond + 1.0 == 1.0)
    {
      info = -2;
    }
  else
    {
      int job = 0;

      Complex *result = dup (b.data (), b.length ());

      for (int j = 0; j < b_nc; j++)
	F77_FCN (zgesl) (tmp_data, &nr, &nr, ipvt, &result[nr*j], &job);

      retval = ComplexMatrix (result, b_nr, b_nc);
    }

  delete [] tmp_data;
  delete [] ipvt;
  delete [] z;

  return retval;
}

ComplexColumnVector
ComplexMatrix::solve (const ComplexColumnVector& b) const
{
  int info;
  double rcond;
  return solve (b, info, rcond);
}

ComplexColumnVector
ComplexMatrix::solve (const ComplexColumnVector& b, int& info) const
{
  double rcond;
  return solve (b, info, rcond);
}

ComplexColumnVector
ComplexMatrix::solve (const ComplexColumnVector& b, int& info,
		      double& rcond) const
{
  ComplexColumnVector retval;

  int nr = rows ();
  int nc = cols ();
  int b_len = b.length ();
  if (nr == 0 || nc == 0 || nr != nc || nr != b_len)
    {
      (*current_liboctave_error_handler)
	("matrix dimension mismatch in solution of linear equations");
      return ComplexColumnVector ();
    }

  info = 0;
  int *ipvt = new int [nr];

  Complex *z = new Complex [nr];
  Complex *tmp_data = dup (data (), length ());

  F77_FCN (zgeco) (tmp_data, &nr, &nr, ipvt, &rcond, z);

  if (rcond + 1.0 == 1.0)
    {
      info = -2;
    }
  else
    {
      int job = 0;

      Complex *result = dup (b.data (), b_len);

      F77_FCN (zgesl) (tmp_data, &nr, &nr, ipvt, result, &job);

      retval = ComplexColumnVector (result, b_len);
    }

  delete [] tmp_data;
  delete [] ipvt;
  delete [] z;

  return retval;
}

ComplexMatrix
ComplexMatrix::lssolve (const ComplexMatrix& b) const
{
  int info;
  int rank;
  return lssolve (b, info, rank);
}

ComplexMatrix
ComplexMatrix::lssolve (const ComplexMatrix& b, int& info) const
{
  int rank;
  return lssolve (b, info, rank);
}

ComplexMatrix
ComplexMatrix::lssolve (const ComplexMatrix& b, int& info, int& rank) const
{
  int nrhs = b.cols ();

  int m = rows ();
  int n = cols ();

  if (m == 0 || n == 0 || m != b.rows ())
    {
      (*current_liboctave_error_handler)
	("matrix dimension mismatch solution of linear equations");
      return Matrix ();
    }

  Complex *tmp_data = dup (data (), length ());

  int nrr = m > n ? m : n;
  ComplexMatrix result (nrr, nrhs);

  int i, j;
  for (j = 0; j < nrhs; j++)
    for (i = 0; i < m; i++)
      result.elem (i, j) = b.elem (i, j);

  Complex *presult = result.fortran_vec ();

  int len_s = m < n ? m : n;
  double *s = new double [len_s];
  double rcond = -1.0;
  int lwork;
  if (m < n)
    lwork = 2*m + (nrhs > n ? nrhs : n);
  else
    lwork = 2*n + (nrhs > m ? nrhs : m);

  Complex *work = new Complex [lwork];

  int lrwork = (5 * (m < n ? m : n)) - 4;
  lrwork = lrwork > 1 ? lrwork : 1;
  double *rwork = new double [lrwork];

  F77_FCN (zgelss) (&m, &n, &nrhs, tmp_data, &m, presult, &nrr, s,
		    &rcond, &rank, work, &lwork, rwork, &info);

  ComplexMatrix retval (n, nrhs);
  for (j = 0; j < nrhs; j++)
    for (i = 0; i < n; i++)
      retval.elem (i, j) = result.elem (i, j);

  delete [] tmp_data;
  delete [] s;
  delete [] work;
  delete [] rwork;

  return retval;
}

ComplexColumnVector
ComplexMatrix::lssolve (const ComplexColumnVector& b) const
{
  int info;
  int rank;
  return lssolve (b, info, rank);
}

ComplexColumnVector
ComplexMatrix::lssolve (const ComplexColumnVector& b, int& info) const
{
  int rank;
  return lssolve (b, info, rank);
}

ComplexColumnVector
ComplexMatrix::lssolve (const ComplexColumnVector& b, int& info,
			int& rank) const
{
  int nrhs = 1;

  int m = rows ();
  int n = cols ();

  if (m == 0 || n == 0 || m != b.length ())
    {
      (*current_liboctave_error_handler)
	("matrix dimension mismatch solution of least squares problem");
      return ComplexColumnVector ();
    }

  Complex *tmp_data = dup (data (), length ());

  int nrr = m > n ? m : n;
  ComplexColumnVector result (nrr);

  int i;
  for (i = 0; i < m; i++)
    result.elem (i) = b.elem (i);

  Complex *presult = result.fortran_vec ();

  int len_s = m < n ? m : n;
  double *s = new double [len_s];
  double rcond = -1.0;
  int lwork;
  if (m < n)
    lwork = 2*m + (nrhs > n ? nrhs : n);
  else
    lwork = 2*n + (nrhs > m ? nrhs : m);

  Complex *work = new Complex [lwork];

  int lrwork = (5 * (m < n ? m : n)) - 4;
  lrwork = lrwork > 1 ? lrwork : 1;
  double *rwork = new double [lrwork];

  F77_FCN (zgelss) (&m, &n, &nrhs, tmp_data, &m, presult, &nrr, s,
		    &rcond, &rank, work, &lwork, rwork, &info);

  ComplexColumnVector retval (n);
  for (i = 0; i < n; i++)
    retval.elem (i) = result.elem (i);

  delete [] tmp_data;
  delete [] s;
  delete [] work;
  delete [] rwork;

  return retval;
}

// matrix by diagonal matrix -> matrix operations

ComplexMatrix&
ComplexMatrix::operator += (const DiagMatrix& a)
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix += operation attempted");
      return ComplexMatrix ();
    }

  for (int i = 0; i < a.length (); i++)
    elem (i, i) += a.elem (i, i);

  return *this;
}

ComplexMatrix&
ComplexMatrix::operator -= (const DiagMatrix& a)
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix -= operation attempted");
      return ComplexMatrix ();
    }

  for (int i = 0; i < a.length (); i++)
    elem (i, i) -= a.elem (i, i);

  return *this;
}

ComplexMatrix&
ComplexMatrix::operator += (const ComplexDiagMatrix& a)
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix += operation attempted");
      return ComplexMatrix ();
    }

  for (int i = 0; i < a.length (); i++)
    elem (i, i) += a.elem (i, i);

  return *this;
}

ComplexMatrix&
ComplexMatrix::operator -= (const ComplexDiagMatrix& a)
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix -= operation attempted");
      return ComplexMatrix ();
    }

  for (int i = 0; i < a.length (); i++)
    elem (i, i) -= a.elem (i, i);

  return *this;
}

// matrix by matrix -> matrix operations

ComplexMatrix&
ComplexMatrix::operator += (const Matrix& a)
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix += operation attempted");
      return *this;
    }

  if (nr == 0 || nc == 0)
    return *this;

  Complex *d = fortran_vec (); // Ensures only one reference to my privates!

  add2 (d, a.data (), length ());
  return *this;
}

ComplexMatrix&
ComplexMatrix::operator -= (const Matrix& a)
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix -= operation attempted");
      return *this;
    }

  if (nr == 0 || nc == 0)
    return *this;

  Complex *d = fortran_vec (); // Ensures only one reference to my privates!

  subtract2 (d, a.data (), length ());
  return *this;
}

ComplexMatrix&
ComplexMatrix::operator += (const ComplexMatrix& a)
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix += operation attempted");
      return *this;
    }

  if (nr == 0 || nc == 0)
    return *this;

  Complex *d = fortran_vec (); // Ensures only one reference to my privates!

  add2 (d, a.data (), length ());
  return *this;
}

ComplexMatrix&
ComplexMatrix::operator -= (const ComplexMatrix& a)
{
  int nr = rows ();
  int nc = cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix -= operation attempted");
      return *this;
    }

  if (nr == 0 || nc == 0)
    return *this;

  Complex *d = fortran_vec (); // Ensures only one reference to my privates!

  subtract2 (d, a.data (), length ());
  return *this;
}

// unary operations

Matrix
ComplexMatrix::operator ! (void) const
{
  return Matrix (not (data (), length ()), rows (), cols ());
}

// matrix by scalar -> matrix operations

ComplexMatrix
operator + (const ComplexMatrix& a, double s)
{
  return ComplexMatrix (add (a.data (), a.length (), s),
			a.rows (), a.cols ());
}

ComplexMatrix
operator - (const ComplexMatrix& a, double s)
{
  return ComplexMatrix (subtract (a.data (), a.length (), s),
			a.rows (), a.cols ());
}

ComplexMatrix
operator * (const ComplexMatrix& a, double s)
{
  return ComplexMatrix (multiply (a.data (), a.length (), s),
			a.rows (), a.cols ());
}

ComplexMatrix
operator / (const ComplexMatrix& a, double s)
{
  return ComplexMatrix (divide (a.data (), a.length (), s),
			a.rows (), a.cols ());
}

// scalar by matrix -> matrix operations

ComplexMatrix
operator + (double s, const ComplexMatrix& a)
{
  return ComplexMatrix (add (a.data (), a.length (), s), a.rows (),
			a.cols ());
}

ComplexMatrix
operator - (double s, const ComplexMatrix& a)
{
  return ComplexMatrix (subtract (s, a.data (), a.length ()),
			a.rows (), a.cols ());
}

ComplexMatrix
operator * (double s, const ComplexMatrix& a)
{
  return ComplexMatrix (multiply (a.data (), a.length (), s),
			a.rows (), a.cols ());
}

ComplexMatrix
operator / (double s, const ComplexMatrix& a)
{
  return ComplexMatrix (divide (s, a.data (), a.length ()),
			a.rows (), a.cols ());
}

// matrix by column vector -> column vector operations

ComplexColumnVector
operator * (const ComplexMatrix& m, const ColumnVector& a)
{
  ComplexColumnVector tmp (a);
  return m * tmp;
}

ComplexColumnVector
operator * (const ComplexMatrix& m, const ComplexColumnVector& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nc != a.length ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix multiplication attempted");
      return ComplexColumnVector ();
    }

  if (nc == 0 || nr == 0)
    return ComplexColumnVector (0);

  char trans = 'N';
  int ld = nr;
  Complex alpha (1.0);
  Complex beta (0.0);
  int i_one = 1;

  Complex *y = new Complex [nr];

  F77_FCN (zgemv) (&trans, &nr, &nc, &alpha, m.data (), &ld, a.data (),
		   &i_one, &beta, y, &i_one, 1L); 

  return ComplexColumnVector (y, nr);
}

// matrix by diagonal matrix -> matrix operations

ComplexMatrix
operator + (const ComplexMatrix& m, const DiagMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix addition attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  ComplexMatrix result (m);
  for (int i = 0; i < a.length (); i++)
    result.elem (i, i) += a.elem (i, i);

  return result;
}

ComplexMatrix
operator - (const ComplexMatrix& m, const DiagMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix subtraction attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  ComplexMatrix result (m);
  for (int i = 0; i < a.length (); i++)
    result.elem (i, i) -= a.elem (i, i);

  return result;
}

ComplexMatrix
operator * (const ComplexMatrix& m, const DiagMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  int a_nc = a.cols ();
  if (nc != a.rows ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix multiplication attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0 || a_nc == 0)
    return ComplexMatrix (nr, nc, 0.0);

  Complex *c = new Complex [nr*a_nc];
  Complex *ctmp = (Complex *) NULL;

  for (int j = 0; j < a.length (); j++)
    {
      int idx = j * nr;
      ctmp = c + idx;
      if (a.elem (j, j) == 1.0)
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = m.elem (i, j);
	}
      else if (a.elem (j, j) == 0.0)
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = 0.0;
	}
      else
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = a.elem (j, j) * m.elem (i, j);
	}
    }

  if (a.rows () < a_nc)
    {
      for (int i = nr * nc; i < nr * a_nc; i++)
	ctmp[i] = 0.0;
    }

  return ComplexMatrix (c, nr, a_nc);
}

ComplexMatrix
operator + (const ComplexMatrix& m, const ComplexDiagMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix addition attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  ComplexMatrix result (m);
  for (int i = 0; i < a.length (); i++)
    result.elem (i, i) += a.elem (i, i);

  return result;
}

ComplexMatrix
operator - (const ComplexMatrix& m, const ComplexDiagMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix subtraction attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  ComplexMatrix result (m);
  for (int i = 0; i < a.length (); i++)
    result.elem (i, i) -= a.elem (i, i);

  return result;
}

ComplexMatrix
operator * (const ComplexMatrix& m, const ComplexDiagMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  int a_nc = a.cols ();
  if (nc != a.rows ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix multiplication attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0 || a_nc == 0)
    return ComplexMatrix (nr, nc, 0.0);

  Complex *c = new Complex [nr*a_nc];
  Complex *ctmp = (Complex *) NULL;

  for (int j = 0; j < a.length (); j++)
    {
      int idx = j * nr;
      ctmp = c + idx;
      if (a.elem (j, j) == 1.0)
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = m.elem (i, j);
	}
      else if (a.elem (j, j) == 0.0)
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = 0.0;
	}
      else
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = a.elem (j, j) * m.elem (i, j);
	}
    }

  if (a.rows () < a_nc)
    {
      for (int i = nr * nc; i < nr * a_nc; i++)
	ctmp[i] = 0.0;
    }

  return ComplexMatrix (c, nr, a_nc);
}

// matrix by matrix -> matrix operations

ComplexMatrix
operator + (const ComplexMatrix& m, const Matrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix addition attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  return ComplexMatrix (add (m.data (), a.data (), m.length ()), nr, nc);
}

ComplexMatrix
operator - (const ComplexMatrix& m, const Matrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix subtraction attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  return ComplexMatrix (subtract (m.data (), a.data (), m.length ()), nr, nc);
}

ComplexMatrix
operator * (const ComplexMatrix& m, const Matrix& a)
{
  ComplexMatrix tmp (a);
  return m * tmp;
}

ComplexMatrix
operator * (const ComplexMatrix& m, const ComplexMatrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  int a_nc = a.cols ();
  if (nc != a.rows ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix multiplication attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0 || a_nc == 0)
    return ComplexMatrix (nr, nc, 0.0);

  char trans  = 'N';
  char transa = 'N';

  int ld  = nr;
  int lda = a.rows ();

  Complex alpha (1.0);
  Complex beta (0.0);

  Complex *c = new Complex [nr*a_nc];

  F77_FCN (zgemm) (&trans, &transa, &nr, &a_nc, &nc, &alpha, m.data (),
		   &ld, a.data (), &lda, &beta, c, &nr, 1L, 1L);

  return ComplexMatrix (c, nr, a_nc);
}

ComplexMatrix
product (const ComplexMatrix& m, const Matrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix product attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  return ComplexMatrix (multiply (m.data (), a.data (), m.length ()), nr, nc);
}

ComplexMatrix
quotient (const ComplexMatrix& m, const Matrix& a)
{
  int nr = m.rows ();
  int nc = m.cols ();
  if (nr != a.rows () || nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("nonconformant matrix quotient attempted");
      return ComplexMatrix ();
    }

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  return ComplexMatrix (divide (m.data (), a.data (), m.length ()), nr, nc);
}

// other operations

ComplexMatrix
map (c_c_Mapper f, const ComplexMatrix& a)
{
  ComplexMatrix b (a);
  b.map (f);
  return b;
}

Matrix
map (d_c_Mapper f, const ComplexMatrix& a)
{
  int a_nc = a.cols ();
  int a_nr = a.rows ();
  Matrix b (a_nr, a_nc);
  for (int j = 0; j < a_nc; j++)
    for (int i = 0; i < a_nr; i++)
      b.elem (i, j) = f (a.elem (i, j));
  return b;
}

void
ComplexMatrix::map (c_c_Mapper f)
{
  for (int j = 0; j < cols (); j++)
    for (int i = 0; i < rows (); i++)
      elem (i, j) = f (elem (i, j));
}

Matrix
ComplexMatrix::all (void) const
{
  int nr = rows ();
  int nc = cols ();
  Matrix retval;
  if (nr > 0 && nc > 0)
    {
      if (nr == 1)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = 1.0;
	  for (int j = 0; j < nc; j++)
	    {
	      if (elem (0, j) == 0.0)
		{
		  retval.elem (0, 0) = 0.0;
		  break;
		}
	    }
	}
      else if (nc == 1)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = 1.0;
	  for (int i = 0; i < nr; i++)
	    {
	      if (elem (i, 0) == 0.0)
		{
		  retval.elem (0, 0) = 0.0;
		  break;
		}
	    }
	}
      else
	{
	  retval.resize (1, nc);
	  for (int j = 0; j < nc; j++)
	    {
	      retval.elem (0, j) = 1.0;
	      for (int i = 0; i < nr; i++)
		{
		  if (elem (i, j) == 0.0)
		    {
		      retval.elem (0, j) = 0.0;
		      break;
		    }
		}
	    }
	}
    }
  return retval;
}

Matrix
ComplexMatrix::any (void) const
{
  int nr = rows ();
  int nc = cols ();
  Matrix retval;
  if (nr > 0 && nc > 0)
    {
      if (nr == 1)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = 0.0;
	  for (int j = 0; j < nc; j++)
	    {
	      if (elem (0, j) != 0.0)
		{
		  retval.elem (0, 0) = 1.0;
		  break;
		}
	    }
	}
      else if (nc == 1)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = 0.0;
	  for (int i = 0; i < nr; i++)
	    {
	      if (elem (i, 0) != 0.0)
		{
		  retval.elem (0, 0) = 1.0;
		  break;
		}
	    }
	}
      else
	{
	  retval.resize (1, nc);
	  for (int j = 0; j < nc; j++)
	    {
	      retval.elem (0, j) = 0.0;
	      for (int i = 0; i < nr; i++)
		{
		  if (elem (i, j) != 0.0)
		    {
		      retval.elem (0, j) = 1.0;
		      break;
		    }
		}
	    }
	}
    }
  return retval;
}

ComplexMatrix
ComplexMatrix::cumprod (void) const
{
  int nr = rows ();
  int nc = cols ();
  ComplexMatrix retval;
  if (nr > 0 && nc > 0)
    {
      if (nr == 1)
	{
	  retval.resize (1, nc);
	  Complex prod = elem (0, 0);
	  for (int j = 0; j < nc; j++)
	    {
	      retval.elem (0, j) = prod;
	      if (j < nc - 1)
		prod *= elem (0, j+1);
	    }
	}
      else if (nc == 1)
	{
	  retval.resize (nr, 1);
	  Complex prod = elem (0, 0);
	  for (int i = 0; i < nr; i++)
	    {
	      retval.elem (i, 0) = prod;
	      if (i < nr - 1)
		prod *= elem (i+1, 0);
	    }
	}
      else
	{
	  retval.resize (nr, nc);
	  for (int j = 0; j < nc; j++)
	    {
	      Complex prod = elem (0, j);
	      for (int i = 0; i < nr; i++)
		{
		  retval.elem (i, j) = prod;
		  if (i < nr - 1)
		    prod *= elem (i+1, j);
		}
	    }
	}
    }
  return retval;
}

ComplexMatrix
ComplexMatrix::cumsum (void) const
{
  int nr = rows ();
  int nc = cols ();
  ComplexMatrix retval;
  if (nr > 0 && nc > 0)
    {
      if (nr == 1)
	{
	  retval.resize (1, nc);
	  Complex sum = elem (0, 0);
	  for (int j = 0; j < nc; j++)
	    {
	      retval.elem (0, j) = sum;
	      if (j < nc - 1)
		sum += elem (0, j+1);
	    }
	}
      else if (nc == 1)
	{
	  retval.resize (nr, 1);
	  Complex sum = elem (0, 0);
	  for (int i = 0; i < nr; i++)
	    {
	      retval.elem (i, 0) = sum;
	      if (i < nr - 1)
		sum += elem (i+1, 0);
	    }
	}
      else
	{
	  retval.resize (nr, nc);
	  for (int j = 0; j < nc; j++)
	    {
	      Complex sum = elem (0, j);
	      for (int i = 0; i < nr; i++)
		{
		  retval.elem (i, j) = sum;
		  if (i < nr - 1)
		    sum += elem (i+1, j);
		}
	    }
	}
    }
  return retval;
}

ComplexMatrix
ComplexMatrix::prod (void) const
{
  int nr = rows ();
  int nc = cols ();
  ComplexMatrix retval;
  if (nr > 0 && nc > 0)
    {
      if (nr == 1)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = 1.0;
	  for (int j = 0; j < nc; j++)
	    retval.elem (0, 0) *= elem (0, j);
	}
      else if (nc == 1)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = 1.0;
	  for (int i = 0; i < nr; i++)
	    retval.elem (0, 0) *= elem (i, 0);
	}
      else
	{
	  retval.resize (1, nc);
	  for (int j = 0; j < nc; j++)
	    {
	      retval.elem (0, j) = 1.0;
	      for (int i = 0; i < nr; i++)
		retval.elem (0, j) *= elem (i, j);
	    }
	}
    }
  return retval;
}

ComplexMatrix
ComplexMatrix::sum (void) const
{
  int nr = rows ();
  int nc = cols ();
  ComplexMatrix retval;
  if (nr > 0 && nc > 0)
    {
      if (nr == 1)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = 0.0;
	  for (int j = 0; j < nc; j++)
	    retval.elem (0, 0) += elem (0, j);
	}
      else if (nc == 1)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = 0.0;
	  for (int i = 0; i < nr; i++)
	    retval.elem (0, 0) += elem (i, 0);
	}
      else
	{
	  retval.resize (1, nc);
	  for (int j = 0; j < nc; j++)
	    {
	      retval.elem (0, j) = 0.0;
	      for (int i = 0; i < nr; i++)
		retval.elem (0, j) += elem (i, j);
	    }
	}
    }
  return retval;
}

ComplexMatrix
ComplexMatrix::sumsq (void) const
{
  int nr = rows ();
  int nc = cols ();
  ComplexMatrix retval;
  if (nr > 0 && nc > 0)
    {
      if (nr == 1)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = 0.0;
	  for (int j = 0; j < nc; j++)
	    {
	      Complex d = elem (0, j);
	      retval.elem (0, 0) += d * d;
	    }
	}
      else if (nc == 1)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = 0.0;
	  for (int i = 0; i < nr; i++)
	    {
	      Complex d = elem (i, 0);
	      retval.elem (0, 0) += d * d;
	    }
	}
      else
	{
	  retval.resize (1, nc);
	  for (int j = 0; j < nc; j++)
	    {
	      retval.elem (0, j) = 0.0;
	      for (int i = 0; i < nr; i++)
		{
		  Complex d = elem (i, j);
		  retval.elem (0, j) += d * d;
		}
	    }
	}
    }
  return retval;
}

ComplexColumnVector
ComplexMatrix::diag (void) const
{
  return diag (0);
}

ComplexColumnVector
ComplexMatrix::diag (int k) const
{
  int nnr = rows ();
  int nnc = cols ();
  if (k > 0)
    nnc -= k;
  else if (k < 0)
    nnr += k;

  ComplexColumnVector d;

  if (nnr > 0 && nnc > 0)
    {
      int ndiag = (nnr < nnc) ? nnr : nnc;

      d.resize (ndiag);

      if (k > 0)
	{
	  for (int i = 0; i < ndiag; i++)
	    d.elem (i) = elem (i, i+k);
	}
      else if ( k < 0)
	{
	  for (int i = 0; i < ndiag; i++)
	    d.elem (i) = elem (i-k, i);
	}
      else
	{
	  for (int i = 0; i < ndiag; i++)
	    d.elem (i) = elem (i, i);
	}
    }
  else
    cerr << "diag: requested diagonal out of range\n";

  return d;
}

ComplexColumnVector
ComplexMatrix::row_min (void) const
{
  ComplexColumnVector result;

  int nr = rows ();
  int nc = cols ();
  if (nr > 0 && nc > 0)
    {
      result.resize (nr);

      for (int i = 0; i < nr; i++)
	{
	  Complex res = elem (i, 0);
	  double absres = abs (res);
	  for (int j = 1; j < nc; j++)
	    if (abs (elem (i, j)) < absres)
	      {
		res = elem (i, j);
		absres = abs (res);
	      }
	  result.elem (i) = res;
	}
    }

  return result;
}

ComplexColumnVector
ComplexMatrix::row_min_loc (void) const
{
  ComplexColumnVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nr);

      for (int i = 0; i < nr; i++)
        {
          Complex res = 0;
          double absres = abs (elem (i, 0));
          for (int j = 0; j < nc; j++)
            if (abs (elem (i, j)) < absres)
              {
                res = j;
                absres = abs (elem (i, j));
              }
          result.elem (i) = res + 1;
        }
    }

  return result;
}

ComplexColumnVector
ComplexMatrix::row_max (void) const
{
  ComplexColumnVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nr);

      for (int i = 0; i < nr; i++)
	{
	  Complex res = elem (i, 0);
	  double absres = abs (res);
	  for (int j = 1; j < nc; j++)
	    if (abs (elem (i, j)) > absres)
	      {
		res = elem (i, j);
		absres = abs (res);
	      }
	  result.elem (i) = res;
	}
    }

  return result;
}

ComplexColumnVector
ComplexMatrix::row_max_loc (void) const
{
  ComplexColumnVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nr);

      for (int i = 0; i < nr; i++)
        {
          Complex res = 0;
          double absres = abs (elem (i, 0));
          for (int j = 0; j < nc; j++)
            if (abs (elem (i, j)) > absres)
              {
                res = j;
                absres = abs (elem (i, j));
              }
          result.elem (i) = res + 1;
        }
    }

  return result;
}

ComplexRowVector
ComplexMatrix::column_min (void) const
{
  ComplexRowVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nc);

      for (int j = 0; j < nc; j++)
	{
	  Complex res = elem (0, j);
	  double absres = abs (res);
	  for (int i = 1; i < nr; i++)
	    if (abs (elem (i, j)) < absres)
	      {
		res = elem (i, j);
		absres = abs (res);
	      }
	  result.elem (j) = res;
	}
    }

  return result;
}

ComplexRowVector
ComplexMatrix::column_min_loc (void) const
{
  ComplexRowVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nc);

      for (int j = 0; j < nc; j++)
        {
          Complex res = 0;
          double absres = abs (elem (0, j));
          for (int i = 0; i < nr; i++)
            if (abs (elem (i, j)) < absres)
              {
                res = i;
                absres = abs (elem (i, j));
              }
          result.elem (j) = res + 1;
        }
    }

  return result;
}

ComplexRowVector
ComplexMatrix::column_max (void) const
{
  ComplexRowVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nc);

      for (int j = 0; j < nc; j++)
	{
	  Complex res = elem (0, j);
	  double absres = abs (res);
	  for (int i = 1; i < nr; i++)
	    if (abs (elem (i, j)) > absres)
	      {
		res = elem (i, j);
		absres = abs (res);
	      }
	  result.elem (j) = res;
	}
    }

  return result;
}

ComplexRowVector
ComplexMatrix::column_max_loc (void) const
{
  ComplexRowVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nc);

      for (int j = 0; j < nc; j++)
        {
          Complex res = 0;
          double absres = abs (elem (0, j));
          for (int i = 0; i < nr; i++)
            if (abs (elem (i, j)) > absres)
              {
                res = i;
                absres = abs (elem (i, j));
              }
          result.elem (j) = res + 1;
        }
    }

  return result;
}

// i/o

ostream&
operator << (ostream& os, const ComplexMatrix& a)
{
//  int field_width = os.precision () + 7;
  for (int i = 0; i < a.rows (); i++)
    {
      for (int j = 0; j < a.cols (); j++)
	os << " " /* setw (field_width) */ << a.elem (i, j);
      os << "\n";
    }
  return os;
}

istream&
operator >> (istream& is, ComplexMatrix& a)
{
  int nr = a.rows ();
  int nc = a.cols ();

  if (nr < 1 || nc < 1)
    is.clear (ios::badbit);
  else
    {
      Complex tmp;
      for (int i = 0; i < nr; i++)
	for (int j = 0; j < nc; j++)
	  {
	    is >> tmp;
	    if (is)
	      a.elem (i, j) = tmp;
	    else
	      break;
	  }
    }

  return is;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
