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

// I\'m not sure how this is supposed to work if the .h file declares
// several classes, each of which is defined in a separate file...
//
// #ifdef __GNUG__
// #pragma implementation
// #endif

#include "Matrix.h"
#include "mx-inlines.cc"

/*
 * Matrix class.
 */

Matrix::Matrix (int r, int c)
{
  if (r < 0 || c < 0)
    FAIL;

  nr = r;
  nc = c;
  len = nr * nc;
  if (len > 0)
    data = new double [len];
  else
    data = (double *) NULL;
}

Matrix::Matrix (int r, int c, double val)
{
  if (r < 0 || c < 0)
    FAIL;

  nr = r;
  nc = c;
  len = nr * nc;
  if (len > 0)
    {
      data = new double [len];
      copy (data, len, val);
    }
  else
    data = (double *) NULL;
}

Matrix::Matrix (const Matrix& a)
{
  nr = a.nr;
  nc = a.nc;
  len = a.len;
  if (len > 0)
    {
      data = new double [len];
      copy (data, a.data, len);
    }
  else
    data = (double *) NULL;
}

Matrix::Matrix (const DiagMatrix& a)
{
  nr = a.nr;
  nc = a.nc;
  len = nr * nc;
  if (len > 0)
    {
      data = new double [len];
      copy (data, len, 0.0);
      for (int i = 0; i < a.len; i++)
	data[nr*i+i] = a.data[i];
    }
  else
    data = (double *) NULL;
}

Matrix::Matrix (double a)
{
  nr = 1;
  nc = 1;
  len = 1;
  data = new double [1];
  data[0] = a;
}

Matrix&
Matrix::operator = (const Matrix& a)
{
  if (this != &a)
    {
      delete [] data;
      nr = a.nr;
      nc = a.nc;
      len = a.len;
      if (len > 0)
	{
	  data = new double [len];
	  copy (data, a.data, len);
	}
      else
	data = (double *) NULL;
    }
  return *this;
}

Matrix&
Matrix::resize (int r, int c)
{
  if (r < 0 || c < 0)
    FAIL;

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
    FAIL;

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

int
Matrix::operator == (const Matrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    return 0;

  return equal (data, a.data, len);
}

int
Matrix::operator != (const Matrix& a) const
{
  return !(*this == a);
}

Matrix&
Matrix::insert (const Matrix& a, int r, int c)
{
  if (r < 0 || r + a.nr - 1 > nr || c < 0 || c + a.nc - 1 > nc)
    FAIL;

  for (int j = 0; j < a.nc; j++)
    for (int i = 0; i < a.nr; i++)
      elem (r+i, c+j) = a.elem (i, j);

  return *this;
}

Matrix&
Matrix::insert (const RowVector& a, int r, int c)
{
  if (r < 0 || r >= nr || c < 0 || c + a.len - 1 > nc)
    FAIL;

  for (int i = 0; i < a.len; i++)
    elem (r, c+i) = a.data[i];

  return *this;
}

Matrix&
Matrix::insert (const ColumnVector& a, int r, int c)
{
  if (r < 0 || r + a.len - 1 > nr || c < 0 || c >= nc)
    FAIL;

  for (int i = 0; i < a.len; i++)
    elem (r+i, c) = a.data[i];

  return *this;
}

Matrix&
Matrix::insert (const DiagMatrix& a, int r, int c)
{
  if (r < 0 || r + a.nr - 1 > nr || c < 0 || c + a.nc - 1 > nc)
    FAIL;

  for (int i = 0; i < a.len; i++)
    elem (r+i, c+i) = a.data[i];

  return *this;
}

Matrix&
Matrix::fill (double val)
{
  if (nr > 0 && nc > 0)
    copy (data, len, val);
  return *this;
}

Matrix&
Matrix::fill (double val, int r1, int c1, int r2, int c2)
{
  if (r1 < 0 || r2 < 0 || c1 < 0 || c2 < 0
      || r1 >= nr || r2 >= nr || c1 >= nc || c2 >= nc)
    FAIL;

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
  if (nr != a.nr)
    FAIL;

  int nc_insert = nc;
  Matrix retval (nr, nc + a.nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;;
}

Matrix
Matrix::append (const RowVector& a) const
{
  if (nr != 1)
    FAIL;

  int nc_insert = nc;
  Matrix retval (nr, nc + a.len);
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

Matrix
Matrix::append (const ColumnVector& a) const
{
  if (nr != a.len)
    FAIL;

  int nc_insert = nc;
  Matrix retval (nr, nc + 1);
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

Matrix
Matrix::append (const DiagMatrix& a) const
{
  if (nr != a.nr)
    FAIL;

  int nc_insert = nc;
  Matrix retval (nr, nc + a.nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

Matrix
Matrix::stack (const Matrix& a) const
{
  if (nc != a.nc)
    FAIL;

  int nr_insert = nr;
  Matrix retval (nr + a.nr, nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

Matrix
Matrix::stack (const RowVector& a) const
{
  if (nc != a.len)
    FAIL;

  int nr_insert = nr;
  Matrix retval (nr + 1, nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

Matrix
Matrix::stack (const ColumnVector& a) const
{
  if (nc != 1)
    FAIL;

  int nr_insert = nr;
  Matrix retval (nr + a.len, nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

Matrix
Matrix::stack (const DiagMatrix& a) const
{
  if (nc != a.nc)
    FAIL;

  int nr_insert = nr;
  Matrix retval (nr + a.nr, nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

Matrix
Matrix::transpose (void) const
{
  Matrix result (nc, nr);
  if (len > 0)
    {
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  result.data[nc*i+j] = data[nr*j+i];
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
      result.data[new_r*j+i] = elem (r1+i, c1+j);

  return result;
}

// extract row or column i.

RowVector
Matrix::row (int i) const
{
  if (i < 0 || i >= nr)
    FAIL;

  RowVector retval (nc);
  for (int j = 0; j < nc; j++)
    retval.elem (j) = elem (i, j);

  return retval;
}

RowVector
Matrix::row (char *s) const
{
  if (s == (char *) NULL)
    FAIL;

  char c = *s;
  if (c == 'f' || c == 'F')
    return row (0);
  else if (c == 'l' || c == 'L')
    return row (nr - 1);
  else
    FAIL;
}

ColumnVector
Matrix::column (int i) const
{
  if (i < 0 || i >= nc)
    FAIL;

  ColumnVector retval (nr);
  for (int j = 0; j < nr; j++)
    retval.elem (j) = elem (j, i);

  return retval;
}

ColumnVector
Matrix::column (char *s) const
{
  if (s == (char *) NULL)
    FAIL;

  char c = *s;
  if (c == 'f' || c == 'F')
    return column (0);
  else if (c == 'l' || c == 'L')
    return column (nc - 1);
  else
    FAIL;
}

Matrix
Matrix::inverse (int& info, double& rcond) const
{
  if (nr != nc)
    FAIL;

  info = 0;

  int *ipvt = new int [nr];
  double *z = new double [nr];
  double *tmp_data = dup (data, len);

  F77_FCN (dgeco) (tmp_data, &nr, &nc, ipvt, &rcond, z);

  if (rcond + 1.0 == 1.0)
    {
      info = -1;
      copy (tmp_data, data, len);  // Restore matrix contents.
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

Matrix
Matrix::inverse (int& info) const
{
  double rcond;
  return inverse (info, rcond);
}

Matrix
Matrix::inverse (void) const
{
  int info;
  double rcond;
  return inverse (info, rcond);
}

ComplexMatrix
Matrix::fourier (void) const
{
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
  Complex *tmp_data = make_complex (data, len);

  F77_FCN (cffti) (&npts, wsave);

  for (int j = 0; j < nsamples; j++)
    F77_FCN (cfftf) (&npts, &tmp_data[npts*j], wsave);

  delete [] wsave;

  return ComplexMatrix (tmp_data, nr, nc);
}

ComplexMatrix
Matrix::ifourier (void) const
{
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
  Complex *tmp_data = make_complex (data, len);

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
  double *tmp_data = dup (data, len);

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

  if (nr == 0 || nc == 0 || nr != nc || nr != b.nr)
    FAIL;

  info = 0;
  int *ipvt = new int [nr];

  double *z = new double [nr];
  double *tmp_data = dup (data, len);

  F77_FCN (dgeco) (tmp_data, &nr, &nr, ipvt, &rcond, z);

  if (rcond + 1.0 == 1.0)
    {
      info = -2;
    }
  else
    {
      int job = 0;

      double *result = dup (b.data, b.len);

      for (int j = 0; j < b.nc; j++)
	F77_FCN (dgesl) (tmp_data, &nr, &nr, ipvt, &result[nr*j], &job);

      retval = Matrix (result, b.nr, b.nc);
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
  int info;
  double rcond;
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

  if (nr == 0 || nc == 0 || nr != nc || nr != b.len)
    FAIL;

  info = 0;
  int *ipvt = new int [nr];

  double *z = new double [nr];
  double *tmp_data = dup (data, len);

  F77_FCN (dgeco) (tmp_data, &nr, &nr, ipvt, &rcond, z);

  if (rcond + 1.0 == 1.0)
    {
      info = -2;
    }
  else
    {
      int job = 0;

      double *result = dup (b.data, b.len);

      F77_FCN (dgesl) (tmp_data, &nr, &nr, ipvt, result, &job);

      retval = ColumnVector (result, b.len);
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
  int nrhs = b.nc;

  int m = nr;
  int n = nc;

  if (m == 0 || n == 0 || m != b.nr)
    FAIL;

  double *tmp_data = dup (data, len);

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
  return tmp.lssolve (b, info);
}

ComplexMatrix
Matrix::lssolve (const ComplexMatrix& b, int& info, int& rank) const
{
  ComplexMatrix tmp (*this);
  return tmp.lssolve (b, info, rank);
}

ColumnVector
Matrix::lssolve (const ColumnVector& b) const
{
  int info;
  int rank;
  return lssolve (b, info, rank);
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

  int m = nr;
  int n = nc;

  if (m == 0 || n == 0 || m != b.len)
    FAIL;

  double *tmp_data = dup (data, len);

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

// matrix by scalar -> matrix operations.

Matrix
Matrix::operator + (double s) const
{
  return Matrix (add (data, len, s), nr, nc);
}

Matrix
Matrix::operator - (double s) const
{
  return Matrix (subtract (data, len, s), nr, nc);
}

Matrix
Matrix::operator * (double s) const
{
  return Matrix (multiply (data, len, s), nr, nc);
}

Matrix
Matrix::operator / (double s) const
{
  return Matrix (divide (data, len, s), nr, nc);
}

ComplexMatrix
Matrix::operator + (Complex s) const
{
  return ComplexMatrix (add (data, len, s), nr, nc);
}

ComplexMatrix
Matrix::operator - (Complex s) const
{
  return ComplexMatrix (subtract (data, len, s), nr, nc);
}

ComplexMatrix
Matrix::operator * (Complex s) const
{
  return ComplexMatrix (multiply (data, len, s), nr, nc);
}

ComplexMatrix
Matrix::operator / (Complex s) const
{
  return ComplexMatrix (divide (data, len, s), nr, nc);
}

// scalar by matrix -> matrix operations

Matrix
operator + (double s, const Matrix& a)
{
  return Matrix (add (a.data, a.len, s), a.nr, a.nc);
}

Matrix
operator - (double s, const Matrix& a)
{
  return Matrix (subtract (s, a.data, a.len), a.nr, a.nc);
}

Matrix
operator * (double s, const Matrix& a)
{
  return Matrix (multiply (a.data, a.len, s), a.nr, a.nc);
}

Matrix
operator / (double s, const Matrix& a)
{
  return Matrix (divide (s, a.data, a.len), a.nr, a.nc);
}

// matrix by column vector -> column vector operations

ColumnVector
Matrix::operator * (const ColumnVector& a) const
{
  if (nc != a.len)
    FAIL;

  if (nr == 0 || nc == 0)
    return ColumnVector (0);

  char trans = 'N';
  int ld = nr;
  double alpha = 1.0;
  double beta  = 0.0;
  int i_one = 1;

  double *y = new double [nr];

  F77_FCN (dgemv) (&trans, &nr, &nc, &alpha, data, &ld, a.data,
		   &i_one, &beta, y, &i_one, 1L); 

  return ColumnVector (y, nr);
}

ComplexColumnVector
Matrix::operator * (const ComplexColumnVector& a) const
{
  ComplexMatrix tmp (*this);
  return tmp * a;
}

// matrix by diagonal matrix -> matrix operations

Matrix
Matrix::operator + (const DiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return Matrix (nr, nc);

  Matrix result (*this);
  for (int i = 0; i < a.len; i++)
    result.elem (i, i) += a.data[i];

  return result;
}

Matrix
Matrix::operator - (const DiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return Matrix (nr, nc);

  Matrix result (*this);
  for (int i = 0; i < a.len; i++)
    result.elem (i, i) -= a.data[i];

  return result;
}

Matrix
Matrix::operator * (const DiagMatrix& a) const
{
  if (nc != a.nr)
    FAIL;

  if (nr == 0 || nc == 0 || a.nc == 0)
    return Matrix (nr, a.nc, 0.0);

  double *c = new double [nr*a.nc];
  double *ctmp = (double *) NULL;

  for (int j = 0; j < a.len; j++)
    {
      int idx = j * nr;
      ctmp = c + idx;
      if (a.data[j] == 1.0)
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = elem (i, j);
	}
      else if (a.data[j] == 0.0)
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = 0.0;
	}
      else
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = a.data[j] * elem (i, j);
	}
    }

  if (a.nr < a.nc)
    {
      for (int i = nr * nc; i < nr * a.nc; i++)
	ctmp[i] = 0.0;
    }

  return Matrix (c, nr, a.nc);
}

ComplexMatrix
Matrix::operator + (const ComplexDiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  ComplexMatrix result (*this);
  for (int i = 0; i < a.len; i++)
    result.elem (i, i) += a.data[i];

  return result;
}

ComplexMatrix
Matrix::operator - (const ComplexDiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  ComplexMatrix result (*this);
  for (int i = 0; i < a.len; i++)
    result.elem (i, i) -= a.data[i];

  return result;
}

ComplexMatrix
Matrix::operator * (const ComplexDiagMatrix& a) const
{
  if (nc != a.nr)
    FAIL;

  if (nr == 0 || nc == 0 || a.nc == 0)
    return ComplexMatrix (nr, a.nc, 0.0);

  Complex *c = new Complex [nr*a.nc];
  Complex *ctmp = (Complex *) NULL;

  for (int j = 0; j < a.len; j++)
    {
      int idx = j * nr;
      ctmp = c + idx;
      if (a.data[j] == 1.0)
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = elem (i, j);
	}
      else if (a.data[j] == 0.0)
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = 0.0;
	}
      else
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = a.data[j] * elem (i, j);
	}
    }

  if (a.nr < a.nc)
    {
      for (int i = nr * nc; i < nr * a.nc; i++)
	ctmp[i] = 0.0;
    }

  return ComplexMatrix (c, nr, a.nc);
}

Matrix&
Matrix::operator += (const DiagMatrix& a)
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  for (int i = 0; i < a.len; i++)
    elem (i, i) += a.data[i];

  return *this;
}

Matrix&
Matrix::operator -= (const DiagMatrix& a)
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  for (int i = 0; i < a.len; i++)
    elem (i, i) -= a.data[i];

  return *this;
}

// matrix by matrix -> matrix operations

Matrix
Matrix::operator + (const Matrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return Matrix (nr, nc);

  return Matrix (add (data, a.data, len), nr, nc);
}

Matrix
Matrix::operator - (const Matrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return Matrix (nr, nc);

  return Matrix (subtract (data, a.data, len), nr, nc);
}

Matrix
Matrix::operator * (const Matrix& a) const
{
  if (nc != a.nr)
    FAIL;

  if (nr == 0 || nc == 0 || a.nc == 0)
    return Matrix (nr, a.nc, 0.0);

  char trans  = 'N';
  char transa = 'N';

  int ld  = nr;
  int lda = a.nr;

  double alpha = 1.0;
  double beta  = 0.0;
  int anc = a.nc;

  double *c = new double [nr*a.nc];

  F77_FCN (dgemm) (&trans, &transa, &nr, &anc, &nc, &alpha, data, &ld,
		   a.data, &lda, &beta, c, &nr, 1L, 1L);

  return Matrix (c, nr, a.nc);
}

ComplexMatrix
Matrix::operator + (const ComplexMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  return ComplexMatrix (add (data, a.data, len), nr, nc);
}

ComplexMatrix
Matrix::operator - (const ComplexMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  return ComplexMatrix (subtract (data, a.data, len), nr, nc);
}

ComplexMatrix
Matrix::operator * (const ComplexMatrix& a) const
{
  ComplexMatrix tmp (*this);
  return tmp * a;
}

Matrix
Matrix::product (const Matrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return Matrix (nr, nc);

  return Matrix (multiply (data, a.data, len), nr, nc);
}

Matrix
Matrix::quotient (const Matrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return Matrix (nr, nc);

  return Matrix (divide (data, a.data, len), nr, nc);
}

ComplexMatrix
Matrix::product (const ComplexMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  return ComplexMatrix (multiply (data, a.data, len), nr, nc);
}

ComplexMatrix
Matrix::quotient (const ComplexMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  return ComplexMatrix (divide (data, a.data, len), nr, nc);
}

Matrix&
Matrix::operator += (const Matrix& a)
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return *this;

  add2 (data, a.data, len);
  return *this;
}

Matrix&
Matrix::operator -= (const Matrix& a)
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return *this;

  subtract2 (data, a.data, len);
  return *this;
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
  for (int i = 0; i < len; i++)
    data[i] = f (data[i]);
}

// XXX FIXME XXX Do these really belong here?  They should maybe be
// cleaned up a bit, no?  What about corresponding functions for the
// Vectors?

Matrix
Matrix::all (void) const
{
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
  int nnr = nr;
  int nnc = nc;
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

// unary operations

Matrix
Matrix::operator - (void) const
{
  return Matrix (negate (data, len), nr, nc);
}

Matrix
Matrix::operator ! (void) const
{
  Matrix b (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      b.elem (i, j) = ! elem (i, j);

  return b;
}

ColumnVector
Matrix::row_min (void) const
{
  ColumnVector result;

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
Matrix::row_max (void) const
{
  ColumnVector result;

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

RowVector
Matrix::column_min (void) const
{
  RowVector result;

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
Matrix::column_max (void) const
{
  RowVector result;

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

ostream&
operator << (ostream& os, const Matrix& a)
{
//  int field_width = os.precision () + 7;
  for (int i = 0; i < a.nr; i++)
    {
      for (int j = 0; j < a.nc; j++)
	os << " " /* setw (field_width) */ << a.elem (i, j);
      os << "\n";
    }
  return os;
}

istream&
operator >> (istream& is, Matrix& a)
{
  int nr = a.rows ();
  int nc = a.columns ();

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

ComplexMatrix::ComplexMatrix (int r, int c)
{
  if (r < 0 || c < 0)
    FAIL;

  nr = r;
  nc = c;
  len = nr * nc;
  if (len > 0)
    data = new Complex [len];
  else
    data = (Complex *) NULL;
}

ComplexMatrix::ComplexMatrix (int r, int c, double val)
{
  if (r < 0 || c < 0)
    FAIL;

  nr = r;
  nc = c;
  len = nr * nc;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, len, val);
    }
  else
    data = (Complex *) NULL;
}

ComplexMatrix::ComplexMatrix (int r, int c, Complex val)
{
  if (r < 0 || c < 0)
    FAIL;

  nr = r;
  nc = c;
  len = nr * nc;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, len, val);
    }
  else
    data = (Complex *) NULL;
}

ComplexMatrix::ComplexMatrix (const Matrix& a)
{
  nr = a.nr;
  nc = a.nc;
  len = a.len;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, a.data, len);
    }
  else
    data = (Complex *) NULL;
}

ComplexMatrix::ComplexMatrix (const ComplexMatrix& a)
{
  nr = a.nr;
  nc = a.nc;
  len = a.len;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, a.data, len);
    }
  else
    data = (Complex *) NULL;
}

ComplexMatrix::ComplexMatrix (const DiagMatrix& a)
{
  nr = a.nr;
  nc = a.nc;
  len = nr * nc;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, len, 0.0);
      for (int i = 0; i < a.len; i++)
	data[nr*i+i] = a.data[i];
    }
  else
    data = (Complex *) NULL;
}

ComplexMatrix::ComplexMatrix (const ComplexDiagMatrix& a)
{
  nr = a.nr;
  nc = a.nc;
  len = nr * nc;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, len, 0.0);
      for (int i = 0; i < a.len; i++)
	data[nr*i+i] = a.data[i];
    }
  else
    data = (Complex *) NULL;
}

ComplexMatrix::ComplexMatrix (double a)
{
  nr = 1;
  nc = 1;
  len = 1;
  data = new Complex [1];
  data[0] = a;
}

ComplexMatrix::ComplexMatrix (Complex a)
{
  nr = 1;
  nc = 1;
  len = 1;
  data = new Complex [1];
  data[0] = Complex (a);
}

ComplexMatrix&
ComplexMatrix::operator = (const Matrix& a)
{
  delete [] data;
  nr = a.nr;
  nc = a.nc;
  len = a.len;
  if (len > 0)
    {
      data = new Complex [len];
      copy (data, a.data, len);
    }
  else
    data = (Complex *) NULL;
  return *this;
}

ComplexMatrix&
ComplexMatrix::operator = (const ComplexMatrix& a)
{
  if (this != &a)
    {
      delete [] data;
      nr = a.nr;
      nc = a.nc;
      len = a.len;
      if (len > 0)
	{
	  data = new Complex [len];
	  copy (data, a.data, len);
	}
      else
	data = (Complex *) NULL;
    }
  return *this;
}

ComplexMatrix&
ComplexMatrix::resize (int r, int c)
{
  if (r < 0 || c < 0)
    FAIL;

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
    FAIL;

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
ComplexMatrix::resize (int r, int c, Complex val)
{
  if (r < 0 || c < 0)
    FAIL;

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

int
ComplexMatrix::operator == (const ComplexMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    return 0;

  return equal (data, a.data, len);
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
  if (r < 0 || r + a.nr - 1 > nr || c < 0 || c + a.nc - 1 > nc)
    FAIL;

  for (int j = 0; j < a.nc; j++)
    for (int i = 0; i < a.nr; i++)
      elem (r+i, c+j) = a.elem (i, j);

  return *this;
}

ComplexMatrix&
ComplexMatrix::insert (const RowVector& a, int r, int c)
{
  if (r < 0 || r >= nr || c < 0 || c + a.len - 1 > nc)
    FAIL;

  for (int i = 0; i < a.len; i++)
    elem (r, c+i) = a.data[i];

  return *this;
}

ComplexMatrix&
ComplexMatrix::insert (const ColumnVector& a, int r, int c)
{
  if (r < 0 || r + a.len - 1 > nr || c < 0 || c >= nc)
    FAIL;

  for (int i = 0; i < a.len; i++)
    elem (r+i, c) = a.data[i];

  return *this;
}

ComplexMatrix&
ComplexMatrix::insert (const DiagMatrix& a, int r, int c)
{
  if (r < 0 || r + a.nr - 1 > nr || c < 0 || c + a.nc - 1 > nc)
    FAIL;

  for (int i = 0; i < a.len; i++)
    elem (r+i, c+i) = a.data[i];

  return *this;
}

ComplexMatrix&
ComplexMatrix::insert (const ComplexMatrix& a, int r, int c)
{
  if (r < 0 || r + a.nr - 1 > nr || c < 0 || c + a.nc - 1 > nc)
    FAIL;

  for (int j = 0; j < a.nc; j++)
    for (int i = 0; i < a.nr; i++)
      elem (r+i, c+j) = a.elem (i, j);

  return *this;
}

ComplexMatrix&
ComplexMatrix::insert (const ComplexRowVector& a, int r, int c)
{
  if (r < 0 || r >= nr || c < 0 || c + a.len - 1 > nc)
    FAIL;

  for (int i = 0; i < a.len; i++)
    elem (r, c+i) = a.data[i];

  return *this;
}

ComplexMatrix&
ComplexMatrix::insert (const ComplexColumnVector& a, int r, int c)
{
  if (r < 0 || r + a.len - 1 > nr || c < 0 || c >= nc)
    FAIL;

  for (int i = 0; i < a.len; i++)
    elem (r+i, c) = a.data[i];

  return *this;
}

ComplexMatrix&
ComplexMatrix::insert (const ComplexDiagMatrix& a, int r, int c)
{
  if (r < 0 || r + a.nr - 1 > nr || c < 0 || c + a.nc - 1 > nc)
    FAIL;

  for (int i = 0; i < a.len; i++)
    elem (r+i, c+i) = a.data[i];

  return *this;
}

ComplexMatrix&
ComplexMatrix::fill (double val)
{
  if (nr > 0 && nc > 0)
    copy (data, len, val);
  return *this;
}

ComplexMatrix&
ComplexMatrix::fill (Complex val)
{
  if (nr > 0 && nc > 0)
    copy (data, len, val);
  return *this;
}

ComplexMatrix&
ComplexMatrix::fill (double val, int r1, int c1, int r2, int c2)
{
  if (r1 < 0 || r2 < 0 || c1 < 0 || c2 < 0
      || r1 >= nr || r2 >= nr || c1 >= nc || c2 >= nc)
    FAIL;

  if (r1 > r2) { int tmp = r1; r1 = r2; r2 = tmp; }
  if (c1 > c2) { int tmp = c1; c1 = c2; c2 = tmp; }

  for (int j = c1; j <= c2; j++)
    for (int i = r1; i <= r2; i++)
      elem (i, j) = val;

  return *this;
}

ComplexMatrix&
ComplexMatrix::fill (Complex val, int r1, int c1, int r2, int c2)
{
  if (r1 < 0 || r2 < 0 || c1 < 0 || c2 < 0
      || r1 >= nr || r2 >= nr || c1 >= nc || c2 >= nc)
    FAIL;

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
  if (nr != a.nr)
    FAIL;

  int nc_insert = nc;
  ComplexMatrix retval (nr, nc + a.nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::append (const RowVector& a) const
{
  if (nr != 1)
    FAIL;

  int nc_insert = nc;
  ComplexMatrix retval (nr, nc + a.len);
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::append (const ColumnVector& a) const
{
  if (nr != a.len)
    FAIL;

  int nc_insert = nc;
  ComplexMatrix retval (nr, nc + 1);
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::append (const DiagMatrix& a) const
{
  if (nr != a.nr)
    FAIL;

  int nc_insert = nc;
  ComplexMatrix retval (nr, nc + a.nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::append (const ComplexMatrix& a) const
{
  if (nr != a.nr)
    FAIL;

  int nc_insert = nc;
  ComplexMatrix retval (nr, nc + a.nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::append (const ComplexRowVector& a) const
{
  if (nr != 1)
    FAIL;

  int nc_insert = nc;
  ComplexMatrix retval (nr, nc + a.len);
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::append (const ComplexColumnVector& a) const
{
  if (nr != a.len)
    FAIL;

  int nc_insert = nc;
  ComplexMatrix retval (nr, nc + 1);
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::append (const ComplexDiagMatrix& a) const
{
  if (nr != a.nr)
    FAIL;

  int nc_insert = nc;
  ComplexMatrix retval (nr, nc + a.nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const Matrix& a) const
{
  if (nc != a.nc)
    FAIL;

  int nr_insert = nr;
  ComplexMatrix retval (nr + a.nr, nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const RowVector& a) const
{
  if (nc != a.len)
    FAIL;

  int nr_insert = nr;
  ComplexMatrix retval (nr + 1, nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const ColumnVector& a) const
{
  if (nc != 1)
    FAIL;

  int nr_insert = nr;
  ComplexMatrix retval (nr + a.len, nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const DiagMatrix& a) const
{
  if (nc != a.nc)
    FAIL;

  int nr_insert = nr;
  ComplexMatrix retval (nr + a.nr, nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const ComplexMatrix& a) const
{
  if (nc != a.nc)
    FAIL;

  int nr_insert = nr;
  ComplexMatrix retval (nr + a.nr, nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const ComplexRowVector& a) const
{
  if (nc != a.len)
    FAIL;

  int nr_insert = nr;
  ComplexMatrix retval (nr + 1, nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const ComplexColumnVector& a) const
{
  if (nc != 1)
    FAIL;

  int nr_insert = nr;
  ComplexMatrix retval (nr + a.len, nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::stack (const ComplexDiagMatrix& a) const
{
  if (nc != a.nc)
    FAIL;

  int nr_insert = nr;
  ComplexMatrix retval (nr + a.nr, nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

ComplexMatrix
ComplexMatrix::hermitian (void) const
{
  ComplexMatrix result;
  if (len > 0)
    {
      result.resize (nc, nr);
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  result.data[nc*i+j] = conj (data[nr*j+i]);
    }
  return result;
}

ComplexMatrix
ComplexMatrix::transpose (void) const
{
  ComplexMatrix result (nc, nr);
  if (len > 0)
    {
      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  result.data[nc*i+j] = data[nr*j+i];
    }
  return result;
}

Matrix
real (const ComplexMatrix& a)
{
  Matrix retval;
  if (a.len > 0)
    retval = Matrix (real_dup (a.data, a.len), a.nr, a.nc);
  return retval;
}

Matrix
imag (const ComplexMatrix& a)
{
  Matrix retval;
  if (a.len > 0)
    retval = Matrix (imag_dup (a.data, a.len), a.nr, a.nc);
  return retval;
}

ComplexMatrix
conj (const ComplexMatrix& a)
{
  ComplexMatrix retval;
  if (a.len > 0)
    retval = ComplexMatrix (conj_dup (a.data, a.len), a.nr, a.nc);
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
      result.data[new_r*j+i] = elem (r1+i, c1+j);

  return result;
}

// extract row or column i.

ComplexRowVector
ComplexMatrix::row (int i) const
{
  if (i < 0 || i >= nr)
    FAIL;

  ComplexRowVector retval (nc);
  for (int j = 0; j < nc; j++)
    retval.elem (j) = elem (i, j);

  return retval;
}

ComplexRowVector
ComplexMatrix::row (char *s) const
{
  if (s == (char *) NULL)
    FAIL;

  char c = *s;
  if (c == 'f' || c == 'F')
    return row (0);
  else if (c == 'l' || c == 'L')
    return row (nr - 1);
  else
    FAIL;
}

ComplexColumnVector
ComplexMatrix::column (int i) const
{
  if (i < 0 || i >= nc)
    FAIL;

  ComplexColumnVector retval (nr);
  for (int j = 0; j < nr; j++)
    retval.elem (j) = elem (j, i);

  return retval;
}

ComplexColumnVector
ComplexMatrix::column (char *s) const
{
  if (s == (char *) NULL)
    FAIL;

  char c = *s;
  if (c == 'f' || c == 'F')
    return column (0);
  else if (c == 'l' || c == 'L')
    return column (nc - 1);
  else
    FAIL;
}

ComplexMatrix
ComplexMatrix::inverse (int& info, double& rcond) const
{
  if (nr != nc)
    FAIL;

  info = 0;

  int *ipvt = new int [nr];
  Complex *z = new Complex [nr];
  Complex *tmp_data = dup (data, len);

  F77_FCN (zgeco) (tmp_data, &nr, &nc, ipvt, &rcond, z);

  if (rcond + 1.0 == 1.0)
    {
      info = -1;
      copy (tmp_data, data, len);  // Restore contents.
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
ComplexMatrix::inverse (int& info) const
{
  double rcond;
  return inverse (info, rcond);
}

ComplexMatrix
ComplexMatrix::inverse (void) const
{
  int info;
  double rcond;
  return inverse (info, rcond);
}

ComplexMatrix
ComplexMatrix::fourier (void) const
{
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
  Complex *tmp_data = dup (data, len);

  F77_FCN (cffti) (&npts, wsave);

  for (int j = 0; j < nsamples; j++)
    F77_FCN (cfftf) (&npts, &tmp_data[npts*j], wsave);

  delete [] wsave;

  return ComplexMatrix (tmp_data, nr, nc);
}

ComplexMatrix
ComplexMatrix::ifourier (void) const
{
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
  Complex *tmp_data = dup (data, len);

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
  Complex *tmp_data = dup (data, len);

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

  if (nr == 0 || nc == 0 || nr != nc || nr != b.nr)
    FAIL;

  info = 0;
  int *ipvt = new int [nr];

  Complex *z = new Complex [nr];
  Complex *tmp_data = dup (data, len);

  F77_FCN (zgeco) (tmp_data, &nr, &nr, ipvt, &rcond, z);

  if (rcond + 1.0 == 1.0)
    {
      info = -2;
    }
  else
    {
      int job = 0;

      Complex *result = dup (b.data, b.len);

      for (int j = 0; j < b.nc; j++)
	F77_FCN (zgesl) (tmp_data, &nr, &nr, ipvt, &result[nr*j], &job);

      retval = ComplexMatrix (result, b.nr, b.nc);
    }

  delete [] tmp_data;
  delete [] ipvt;
  delete [] z;

  return retval;
}

ComplexColumnVector
ComplexMatrix::solve (const ColumnVector& b) const
{
  int info;
  double rcond;
  return solve (b, info, rcond);
}

ComplexColumnVector
ComplexMatrix::solve (const ColumnVector& b, int& info) const
{
  double rcond;
  return solve (b, info, rcond);
}

ComplexColumnVector
ComplexMatrix::solve (const ColumnVector& b, int& info, double& rcond) const
{
  ComplexColumnVector tmp (b);
  return solve (tmp, info, rcond);
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

  if (nr == 0 || nc == 0 || nr != nc || nr != b.len)
    FAIL;

  info = 0;
  int *ipvt = new int [nr];

  Complex *z = new Complex [nr];
  Complex *tmp_data = dup (data, len);

  F77_FCN (zgeco) (tmp_data, &nr, &nr, ipvt, &rcond, z);

  if (rcond + 1.0 == 1.0)
    {
      info = -2;
    }
  else
    {
      int job = 0;

      Complex *result = dup (b.data, b.len);

      F77_FCN (zgesl) (tmp_data, &nr, &nr, ipvt, result, &job);

      retval = ComplexColumnVector (result, b.len);
    }

  delete [] tmp_data;
  delete [] ipvt;
  delete [] z;

  return retval;
}

ComplexMatrix
ComplexMatrix::lssolve (const Matrix& b) const
{
  int info;
  int rank;
  return lssolve (b, info, rank);
}

ComplexMatrix
ComplexMatrix::lssolve (const Matrix& b, int& info) const
{
  int rank;
  return lssolve (b, info, rank);
}

ComplexMatrix
ComplexMatrix::lssolve (const Matrix& b, int& info, int& rank) const
{
  ComplexMatrix tmp (b);
  return lssolve (tmp, info, rank);
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
  int nrhs = b.nc;

  int m = nr;
  int n = nc;

  if (m == 0 || n == 0 || m != b.nr)
    FAIL;

  Complex *tmp_data = dup (data, len);

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
ComplexMatrix::lssolve (const ColumnVector& b) const
{
  int info;
  int rank;
  return lssolve (b, info, rank);
}

ComplexColumnVector
ComplexMatrix::lssolve (const ColumnVector& b, int& info) const
{
  int rank;
  return lssolve (b, info, rank);
}

ComplexColumnVector
ComplexMatrix::lssolve (const ColumnVector& b, int& info, int& rank) const
{
  ComplexColumnVector tmp (b);
  return lssolve (tmp, info, rank);
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

  int m = nr;
  int n = nc;

  if (m == 0 || n == 0 || m != b.len)
    FAIL;

  Complex *tmp_data = dup (data, len);

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

// matrix by scalar -> matrix operations

ComplexMatrix
ComplexMatrix::operator + (double s) const
{
  return ComplexMatrix (add (data, len, s), nr, nc);
}

ComplexMatrix
ComplexMatrix::operator - (double s) const
{
  return ComplexMatrix (subtract (data, len, s), nr, nc);
}

ComplexMatrix
ComplexMatrix::operator * (double s) const
{
  return ComplexMatrix (multiply (data, len, s), nr, nc);
}

ComplexMatrix
ComplexMatrix::operator / (double s) const
{
  return ComplexMatrix (divide (data, len, s), nr, nc);
}

ComplexMatrix
ComplexMatrix::operator + (Complex s) const
{
  return ComplexMatrix (add (data, len, s), nr, nc);
}

ComplexMatrix
ComplexMatrix::operator - (Complex s) const
{
  return ComplexMatrix (subtract (data, len, s), nr, nc);
}

ComplexMatrix
ComplexMatrix::operator * (Complex s) const
{
  return ComplexMatrix (multiply (data, len, s), nr, nc);
}

ComplexMatrix
ComplexMatrix::operator / (Complex s) const
{
  return ComplexMatrix (divide (data, len, s), nr, nc);
}

// scalar by matrix -> matrix operations

ComplexMatrix
operator + (double s, const ComplexMatrix& a)
{
  return ComplexMatrix (add (a.data, a.len, s), a.nr, a.nc);
}

ComplexMatrix
operator - (double s, const ComplexMatrix& a)
{
  return ComplexMatrix (subtract (s, a.data, a.len), a.nr, a.nc);
}

ComplexMatrix
operator * (double s, const ComplexMatrix& a)
{
  return ComplexMatrix (multiply (a.data, a.len, s), a.nr, a.nc);
}

ComplexMatrix
operator / (double s, const ComplexMatrix& a)
{
  return ComplexMatrix (divide (s, a.data, a.len), a.nr, a.nc);
}

ComplexMatrix
operator + (Complex s, const ComplexMatrix& a)
{
  return ComplexMatrix (add (s, a.data, a.len), a.nr, a.nc);
}

ComplexMatrix
operator - (Complex s, const ComplexMatrix& a)
{
  return ComplexMatrix (subtract (s, a.data, a.len), a.nr, a.nc);
}

ComplexMatrix
operator * (Complex s, const ComplexMatrix& a)
{
  return ComplexMatrix (multiply (s, a.data, a.len), a.nr, a.nc);
}

ComplexMatrix
operator / (Complex s, const ComplexMatrix& a)
{
  return ComplexMatrix (divide (s, a.data, a.len), a.nr, a.nc);
}

// matrix by column vector -> column vector operations

ComplexColumnVector
ComplexMatrix::operator * (const ColumnVector& a) const
{
  ComplexColumnVector tmp (a);
  return *this * tmp;
}

ComplexColumnVector
ComplexMatrix::operator * (const ComplexColumnVector& a) const
{
  if (nc != a.len)
    FAIL;

  if (nc == 0 || nr == 0)
    return ComplexColumnVector (0);

  char trans = 'N';
  int ld = nr;
  Complex alpha (1.0);
  Complex beta (0.0);
  int i_one = 1;

  Complex *y = new Complex [nr];

  F77_FCN (zgemv) (&trans, &nr, &nc, &alpha, data, &ld, a.data,
		   &i_one, &beta, y, &i_one, 1L); 

  return ComplexColumnVector (y, nr);
}

// matrix by diagonal matrix -> matrix operations

ComplexMatrix
ComplexMatrix::operator + (const DiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  ComplexMatrix result (*this);
  for (int i = 0; i < a.len; i++)
    result.elem (i, i) += a.data[i];

  return result;
}

ComplexMatrix
ComplexMatrix::operator - (const DiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  ComplexMatrix result (*this);
  for (int i = 0; i < a.len; i++)
    result.elem (i, i) -= a.data[i];

  return result;
}

ComplexMatrix
ComplexMatrix::operator * (const DiagMatrix& a) const
{
  if (nc != a.nr)
    FAIL;

  if (nr == 0 || nc == 0 || a.nc == 0)
    return ComplexMatrix (nr, nc, 0.0);

  Complex *c = new Complex [nr*a.nc];
  Complex *ctmp = (Complex *) NULL;

  for (int j = 0; j < a.len; j++)
    {
      int idx = j * nr;
      ctmp = c + idx;
      if (a.data[j] == 1.0)
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = elem (i, j);
	}
      else if (a.data[j] == 0.0)
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = 0.0;
	}
      else
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = a.data[j] * elem (i, j);
	}
    }

  if (a.nr < a.nc)
    {
      for (int i = nr * nc; i < nr * a.nc; i++)
	ctmp[i] = 0.0;
    }

  return ComplexMatrix (c, nr, a.nc);
}

ComplexMatrix
ComplexMatrix::operator + (const ComplexDiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  ComplexMatrix result (*this);
  for (int i = 0; i < a.len; i++)
    result.elem (i, i) += a.data[i];

  return result;
}

ComplexMatrix
ComplexMatrix::operator - (const ComplexDiagMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  ComplexMatrix result (*this);
  for (int i = 0; i < a.len; i++)
    result.elem (i, i) -= a.data[i];

  return result;
}

ComplexMatrix
ComplexMatrix::operator * (const ComplexDiagMatrix& a) const
{
  if (nc != a.nr)
    FAIL;

  if (nr == 0 || nc == 0 || a.nc == 0)
    return ComplexMatrix (nr, nc, 0.0);

  Complex *c = new Complex [nr*a.nc];
  Complex *ctmp = (Complex *) NULL;

  for (int j = 0; j < a.len; j++)
    {
      int idx = j * nr;
      ctmp = c + idx;
      if (a.data[j] == 1.0)
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = elem (i, j);
	}
      else if (a.data[j] == 0.0)
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = 0.0;
	}
      else
	{
	  for (int i = 0; i < nr; i++)
	    ctmp[i] = a.data[j] * elem (i, j);
	}
    }

  if (a.nr < a.nc)
    {
      for (int i = nr * nc; i < nr * a.nc; i++)
	ctmp[i] = 0.0;
    }

  return ComplexMatrix (c, nr, a.nc);
}

ComplexMatrix&
ComplexMatrix::operator += (const DiagMatrix& a)
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  for (int i = 0; i < a.len; i++)
    elem (i, i) += a.data[i];

  return *this;
}

ComplexMatrix&
ComplexMatrix::operator -= (const DiagMatrix& a)
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  for (int i = 0; i < a.len; i++)
    elem (i, i) -= a.data[i];

  return *this;
}

ComplexMatrix&
ComplexMatrix::operator += (const ComplexDiagMatrix& a)
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  for (int i = 0; i < a.len; i++)
    elem (i, i) += a.data[i];

  return *this;
}

ComplexMatrix&
ComplexMatrix::operator -= (const ComplexDiagMatrix& a)
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  for (int i = 0; i < a.len; i++)
    elem (i, i) -= a.data[i];

  return *this;
}

// matrix by matrix -> matrix operations

ComplexMatrix
ComplexMatrix::operator + (const Matrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  return ComplexMatrix (add (data, a.data, len), nr, nc);
}

ComplexMatrix
ComplexMatrix::operator - (const Matrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  return ComplexMatrix (subtract (data, a.data, len), nr, nc);
}

ComplexMatrix
ComplexMatrix::operator * (const Matrix& a) const
{
  ComplexMatrix tmp (a);
  return *this * tmp;
}

ComplexMatrix
ComplexMatrix::operator + (const ComplexMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  return ComplexMatrix (add (data, a.data, len), nr, nc);
}

ComplexMatrix
ComplexMatrix::operator - (const ComplexMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  return ComplexMatrix (subtract (data, a.data, len), nr, nc);
}

ComplexMatrix
ComplexMatrix::operator * (const ComplexMatrix& a) const
{
  if (nc != a.nr)
    FAIL;

  if (nr == 0 || nc == 0 || a.nc == 0)
    return ComplexMatrix (nr, nc, 0.0);

  char trans  = 'N';
  char transa = 'N';

  int ld  = nr;
  int lda = a.nr;

  Complex alpha (1.0);
  Complex beta (0.0);
  int anc = a.nc;

  Complex *c = new Complex [nr*a.nc];

  F77_FCN (zgemm) (&trans, &transa, &nr, &anc, &nc, &alpha, data, &ld,
		   a.data, &lda, &beta, c, &nr, 1L, 1L);

  return ComplexMatrix (c, nr, a.nc);
}

ComplexMatrix
ComplexMatrix::product (const Matrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  return ComplexMatrix (multiply (data, a.data, len), nr, nc);
}

ComplexMatrix
ComplexMatrix::quotient (const Matrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  return ComplexMatrix (divide (data, a.data, len), nr, nc);
}

ComplexMatrix
ComplexMatrix::product (const ComplexMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  return ComplexMatrix (multiply (data, a.data, len), nr, nc);
}

ComplexMatrix
ComplexMatrix::quotient (const ComplexMatrix& a) const
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return ComplexMatrix (nr, nc);

  return ComplexMatrix (divide (data, a.data, len), nr, nc);
}

ComplexMatrix&
ComplexMatrix::operator += (const Matrix& a)
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return *this;

  add2 (data, a.data, len);
  return *this;
}

ComplexMatrix&
ComplexMatrix::operator -= (const Matrix& a)
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return *this;

  subtract2 (data, a.data, len);
  return *this;
}

ComplexMatrix&
ComplexMatrix::operator += (const ComplexMatrix& a)
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return *this;

  add2 (data, a.data, len);
  return *this;
}

ComplexMatrix&
ComplexMatrix::operator -= (const ComplexMatrix& a)
{
  if (nr != a.nr || nc != a.nc)
    FAIL;

  if (nr == 0 || nc == 0)
    return *this;

  subtract2 (data, a.data, len);
  return *this;
}

// unary operations

ComplexMatrix
ComplexMatrix::operator - (void) const
{
  return ComplexMatrix (negate (data, len), nr, nc);
}

Matrix
ComplexMatrix::operator ! (void) const
{
  return Matrix (not (data, len), nr, nc);
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
  Matrix b (a.nr, a.nc);
  for (int j = 0; j < a.nc; j++)
    for (int i = 0; i < a.nr; i++)
      b.elem (i, j) = f (a.elem (i, j));
  return b;
}

void
ComplexMatrix::map (c_c_Mapper f)
{
  for (int i = 0; i < len; i++)
    data[i] = f (data[i]);
}

Matrix
ComplexMatrix::all (void) const
{
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
  int nnr = nr;
  int nnc = nc;
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
ComplexMatrix::row_max (void) const
{
  ComplexColumnVector result;

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

ComplexRowVector
ComplexMatrix::column_min (void) const
{
  ComplexRowVector result;

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
ComplexMatrix::column_max (void) const
{
  ComplexRowVector result;

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

// i/o

ostream&
operator << (ostream& os, const ComplexMatrix& a)
{
//  int field_width = os.precision () + 7;
  for (int i = 0; i < a.nr; i++)
    {
      for (int j = 0; j < a.nc; j++)
	os << " " /* setw (field_width) */ << a.elem (i, j);
      os << "\n";
    }
  return os;
}

istream&
operator >> (istream& is, ComplexMatrix& a)
{
  int nr = a.rows ();
  int nc = a.columns ();

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
