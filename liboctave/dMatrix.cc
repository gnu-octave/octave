// Matrix manipulations.                              -*- C++ -*-
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if defined (__GNUG__)
#pragma implementation
#endif

#include <sys/types.h>
#include <iostream.h>
#include <stdio.h>

#include <Complex.h>

#include "mx-base.h"
#include "dbleDET.h"
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

// Note that the original complex fft routines were not written for
// double complex arguments.  They have been modified by adding an
// implicit double precision (a-h,o-z) statement at the beginning of
// each subroutine.

  int F77_FCN (cffti) (const int*, Complex*);

  int F77_FCN (cfftf) (const int*, Complex*, Complex*);

  int F77_FCN (cfftb) (const int*, Complex*, Complex*);
}

#define KLUDGE_MATRICES
#define TYPE double
#define KL_MAT_TYPE Matrix
#include "mx-kludge.cc"
#undef KLUDGE_MATRICES
#undef TYPE
#undef KL_MAT_TYPE

/*
 * Matrix class.
 */

Matrix::Matrix (const DiagMatrix& a)
  : Array2<double> (a.rows (), a.cols (), 0.0)
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
  double* new_data = 0;
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
  double *new_data = 0;
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
  int nc = cols ();
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
  if (! s)
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
  if (! s)
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

  volatile double tmp_rcond = rcond;
  if (tmp_rcond + 1.0 == 1.0)
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
      retval = DET (d);
    }
  else
    {
      info = 0;
      int *ipvt = new int [nr];

      double *z = new double [nr];
      double *tmp_data = dup (data (), length ());

      F77_FCN (dgeco) (tmp_data, &nr, &nr, ipvt, &rcond, z);

      volatile double tmp_rcond = rcond;
      if (tmp_rcond + 1.0 == 1.0)
	{
	  info = -1;
	  retval = DET ();
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
    }

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

  volatile double tmp_rcond = rcond;
  if (tmp_rcond + 1.0 == 1.0)
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

  volatile double tmp_rcond = rcond;
  if (tmp_rcond + 1.0 == 1.0)
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
  return ComplexMatrix (add (s, a.data (), a.length ()),
			a.rows (), a.cols ());
}

ComplexMatrix
operator - (const Complex& s, const Matrix& a)
{
  return ComplexMatrix (subtract (s, a.data (), a.length ()),
			a.rows (), a.cols ());
}

ComplexMatrix
operator * (const Complex& s, const Matrix& a)
{
  return ComplexMatrix (multiply (a.data (), a.length (), s),
			a.rows (), a.cols ());
}

ComplexMatrix
operator / (const Complex& s, const Matrix& a)
{
  return ComplexMatrix (divide (s, a.data (), a.length ()),
			a.rows (), a.cols ());
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
  double *ctmp = 0;

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
  Complex *ctmp = 0;

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
 * Read an array of data froma file in binary format.
 */
int
Matrix::read (FILE *fptr, char *type)
{
// Allocate buffer pointers.

  union
    {
      void *vd;
      char *ch;
      u_char *uc;
      short *sh;
      u_short *us;
      int *in;
      u_int *ui;
      long *ln;
      u_long *ul;
      float *fl;
      double *db;
    }
  buf;

// Convert data to double.

  if (! type)
    {
      (*current_liboctave_error_handler)
	("fread: invalid NULL type parameter");
      return 0;
    }    

  int count;
  int nitems = length ();

  double *d = fortran_vec (); // Ensures only one reference to my privates!

#define DO_FREAD(TYPE,ELEM) \
  do \
    { \
      size_t size = sizeof (TYPE); \
      buf.ch = new char [size * nitems]; \
      count = fread (buf.ch, size, nitems, fptr); \
      for (int k = 0; k < count; k++) \
	d[k] = buf.ELEM[k]; \
      delete [] buf.ch; \
    } \
  while (0)

  if (strcasecmp (type, "double") == 0)
    DO_FREAD (double, db);
  else if (strcasecmp (type, "char") == 0)
    DO_FREAD (char, ch);
  else if (strcasecmp (type, "uchar") == 0)
    DO_FREAD (u_char, uc);
  else if (strcasecmp (type, "short") == 0)
    DO_FREAD (short, sh);
  else if (strcasecmp (type, "ushort") == 0)
    DO_FREAD (u_short, us);
  else if (strcasecmp (type, "int") == 0)
    DO_FREAD (int, in);
  else if (strcasecmp (type, "uint") == 0)
    DO_FREAD (u_int, ui);
  else if (strcasecmp (type, "long") == 0)
    DO_FREAD (long, ul);
  else if (strcasecmp (type, "float") == 0)
    DO_FREAD (float, fl);
  else
    {
      (*current_liboctave_error_handler)
	("fread: invalid NULL type parameter");
      return 0;
    }

  return count;
}

/*
 * Write the data array to a file in binary format.
 */
int
Matrix::write (FILE *fptr, char *type)
{
// Allocate buffer pointers.

  union
    {
      void *vd;
      char *ch;
      u_char *uc;
      short *sh;
      u_short *us;
      int *in;
      u_int *ui;
      long *ln;
      u_long *ul;
      float *fl;
      double *db;
    }
  buf;

  int nitems = length ();

  double *d = fortran_vec ();

// Convert from double to correct size.

  if (! type)
    {
      (*current_liboctave_error_handler)
	("fwrite: invalid NULL type parameter");
      return 0;
    }    

  size_t size;
  int count;

#define DO_FWRITE(TYPE,ELEM) \
  do \
    { \
      size = sizeof (TYPE); \
      buf.ELEM = new TYPE [nitems]; \
      for (int k = 0; k < nitems; k++) \
	buf.ELEM[k] = (TYPE) d[k]; \
      count = fwrite (buf.ELEM, size, nitems, fptr); \
      delete [] buf.ELEM; \
    } \
  while (0)

  if (strcasecmp (type, "double") == 0)
    DO_FWRITE (double, db);
  else if (strcasecmp (type, "char") == 0)
    DO_FWRITE (char, ch);
  else if (strcasecmp (type, "uchar") == 0)
    DO_FWRITE (u_char, uc);
  else if (strcasecmp (type, "short") == 0)
    DO_FWRITE (short, sh);
  else if (strcasecmp (type, "ushort") == 0)
    DO_FWRITE (u_short, us);
  else if (strcasecmp (type, "int") == 0)
    DO_FWRITE (int, in);
  else if (strcasecmp (type, "uint") == 0)
    DO_FWRITE (u_int, ui);
  else if (strcasecmp (type, "long") == 0)
    DO_FWRITE (long, ln);
  else if (strcasecmp (type, "ulong") == 0)
    DO_FWRITE (u_long, ul);
  else if (strcasecmp (type, "float") == 0)
    DO_FWRITE (float, fl);
  else
    {
      (*current_liboctave_error_handler)
	("fwrite: unrecognized type parameter %s", type);
      return 0;
    }

  return count;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
