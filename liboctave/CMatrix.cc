// Matrix manipulations.                              -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>

#include <iostream.h>

#include <sys/types.h> // XXX FIXME XXX

#include "CmplxDET.h"
#include "CmplxSVD.h"
#include "f77-uscore.h"
#include "lo-error.h"
#include "mx-base.h"
#include "mx-inlines.cc"
#include "oct-cmplx.h"

// Fortran functions we call.

extern "C"
{
  int F77_FCN (zgemm, ZGEMM) (const char*, const char*, const int&,
			      const int&, const int&, const Complex&,
			      const Complex*, const int&,
			      const Complex*, const int&,
			      const Complex&, Complex*, const int&, 
			      long, long);

  int F77_FCN (zgeco, ZGECO) (Complex*, const int&, const int&, int*,
			      double&, Complex*);

  int F77_FCN (zgedi, ZGEDI) (Complex*, const int&, const int&, int*,
			      Complex*, Complex*, const int&);

  int F77_FCN (zgesl, ZGESL) (Complex*, const int&, const int&, int*,
			      Complex*, const int&);

  int F77_FCN (zgelss, ZGELSS) (const int&, const int&, const int&,
				Complex*, const int&, Complex*,
				const int&, double*, double&, int&,
				Complex*, const int&, double*, int&);

  // Note that the original complex fft routines were not written for
  // double complex arguments.  They have been modified by adding an
  // implicit double precision (a-h,o-z) statement at the beginning of
  // each subroutine.

  int F77_FCN (cffti, CFFTI) (const int&, Complex*);

  int F77_FCN (cfftf, CFFTF) (const int&, Complex*, Complex*);

  int F77_FCN (cfftb, CFFTB) (const int&, Complex*, Complex*);
}

// Complex Matrix class

ComplexMatrix::ComplexMatrix (const Matrix& a)
  : MArray2<Complex> (a.rows (), a.cols ())
{
  for (int j = 0; j < cols (); j++)
    for (int i = 0; i < rows (); i++)
      elem (i, j) = a.elem (i, j);
}

ComplexMatrix::ComplexMatrix (const DiagMatrix& a)
  : MArray2<Complex> (a.rows (), a.cols (), 0.0)
{
  for (int i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

ComplexMatrix::ComplexMatrix (const ComplexDiagMatrix& a)
  : MArray2<Complex> (a.rows (), a.cols (), 0.0)
{
  for (int i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

// XXX FIXME XXX -- could we use a templated mixed-type copy function
// here?

ComplexMatrix::ComplexMatrix (const charMatrix& a)
{
  for (int i = 0; i < a.cols (); i++)
    for (int j = 0; j < a.rows (); j++)
      elem (i, j) = a.elem (i, j);
}

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
  Array2<Complex>::insert (a, r, c);
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

ComplexMatrix&
ComplexMatrix::fill (const Complex& val, int r1, int c1, int r2, int c2)
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
  if (! s)
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
  if (! s)
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
  double rcond;
  return inverse (info, rcond);
}

ComplexMatrix
ComplexMatrix::inverse (int& info) const
{
  double rcond;
  return inverse (info, rcond);
}

ComplexMatrix
ComplexMatrix::inverse (int& info, double& rcond, int force) const
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

  F77_FCN (zgeco, ZGECO) (tmp_data, nr, nc, ipvt, rcond, z);

  volatile double rcond_plus_one = rcond + 1.0;
  if (rcond_plus_one == 1.0 && ! force)
    {
      info = -1;
      copy (tmp_data, data (), len);  // Restore contents.
    }
  else
    {
      Complex *dummy = 0;

      F77_FCN (zgedi, ZGEDI) (tmp_data, nr, nc, ipvt, dummy, z, 1);
    }

  delete [] ipvt;
  delete [] z;

  return ComplexMatrix (tmp_data, nr, nc);
}

ComplexMatrix
ComplexMatrix::pseudo_inverse (double tol)
{
  ComplexMatrix retval;

  ComplexSVD result (*this);

  DiagMatrix S = result.singular_values ();
  ComplexMatrix U = result.left_singular_matrix ();
  ComplexMatrix V = result.right_singular_matrix ();

  ColumnVector sigma = S.diag ();

  int r = sigma.length () - 1;
  int nr = rows ();
  int nc = cols ();

  if (tol <= 0.0)
    {
      if (nr > nc)
	tol = nr * sigma.elem (0) * DBL_EPSILON;
      else
	tol = nc * sigma.elem (0) * DBL_EPSILON;
    }

  while (r >= 0 && sigma.elem (r) < tol)
    r--;

  if (r < 0)
    retval = ComplexMatrix (nc, nr, 0.0);
  else
    {
      ComplexMatrix Ur = U.extract (0, 0, nr-1, r);
      DiagMatrix D = DiagMatrix (sigma.extract (0, r)) . inverse ();
      ComplexMatrix Vr = V.extract (0, 0, nc-1, r);
      retval = Vr * D * Ur.hermitian ();
    }

  return retval;
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

  F77_FCN (cffti, CFFTI) (npts, wsave);

  for (int j = 0; j < nsamples; j++)
    F77_FCN (cfftf, CFFTF) (npts, &tmp_data[npts*j], wsave);

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

  F77_FCN (cffti, CFFTI) (npts, wsave);

  for (int j = 0; j < nsamples; j++)
    F77_FCN (cfftb, CFFTB) (npts, &tmp_data[npts*j], wsave);

  for (int j = 0; j < npts*nsamples; j++)
    tmp_data[j] = tmp_data[j] / (double) npts;

  delete [] wsave;

  return ComplexMatrix (tmp_data, nr, nc);
}

ComplexMatrix
ComplexMatrix::fourier2d (void) const
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

  F77_FCN (cffti, CFFTI) (npts, wsave);

  for (int j = 0; j < nsamples; j++)
    F77_FCN (cfftf, CFFTF) (npts, &tmp_data[npts*j], wsave);

  delete [] wsave;

  npts = nc;
  nsamples = nr;
  nn = 4*npts+15;
  wsave = new Complex [nn];
  Complex *row = new Complex[npts];

  F77_FCN (cffti, CFFTI) (npts, wsave);

  for (int j = 0; j < nsamples; j++)
    {
      for (int i = 0; i < npts; i++)
	row[i] = tmp_data[i*nr + j];

      F77_FCN (cfftf, CFFTF) (npts, row, wsave);

      for (int i = 0; i < npts; i++)
	tmp_data[i*nr + j] = row[i];
    }

  delete [] wsave;
  delete [] row;

  return ComplexMatrix (tmp_data, nr, nc);
}

ComplexMatrix
ComplexMatrix::ifourier2d (void) const
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

  F77_FCN (cffti, CFFTI) (npts, wsave);

  for (int j = 0; j < nsamples; j++)
    F77_FCN (cfftb, CFFTB) (npts, &tmp_data[npts*j], wsave);

  delete [] wsave;

  for (int j = 0; j < npts*nsamples; j++)
    tmp_data[j] = tmp_data[j] / (double) npts;

  npts = nc;
  nsamples = nr;
  nn = 4*npts+15;
  wsave = new Complex [nn];
  Complex *row = new Complex[npts];

  F77_FCN (cffti, CFFTI) (npts, wsave);

  for (int j = 0; j < nsamples; j++)
    {
      for (int i = 0; i < npts; i++)
	row[i] = tmp_data[i*nr + j];

      F77_FCN (cfftb, CFFTB) (npts, row, wsave);

      for (int i = 0; i < npts; i++)
	tmp_data[i*nr + j] = row[i] / (double) npts;
    }

  delete [] wsave;
  delete [] row;

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
      retval = ComplexDET (d);
    }
  else
    {
      info = 0;
      int *ipvt = new int [nr];

      Complex *z = new Complex [nr];
      Complex *tmp_data = dup (data (), length ());

      F77_FCN (zgeco, ZGECO) (tmp_data, nr, nr, ipvt, rcond, z);

      volatile double rcond_plus_one = rcond + 1.0;
      if (rcond_plus_one == 1.0)
	{
	  info = -1;
	  retval = ComplexDET ();
	}
      else
	{
	  Complex d[2];
	  F77_FCN (zgedi, ZGEDI) (tmp_data, nr, nr, ipvt, d, z, 10);
	  retval = ComplexDET (d);
	}

      delete [] tmp_data;
      delete [] ipvt;
      delete [] z;
    }

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

  F77_FCN (zgeco, ZGECO) (tmp_data, nr, nr, ipvt, rcond, z);

  volatile double rcond_plus_one = rcond + 1.0;
  if (rcond_plus_one == 1.0)
    {
      info = -2;
    }
  else
    {
      Complex *result = dup (b.data (), b.length ());

      for (int j = 0; j < b_nc; j++)
	F77_FCN (zgesl, ZGESL) (tmp_data, nr, nr, ipvt, &result[nr*j], 0);

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

  F77_FCN (zgeco, ZGECO) (tmp_data, nr, nr, ipvt, rcond, z);

  volatile double rcond_plus_one = rcond + 1.0;
  if (rcond_plus_one == 1.0)
    {
      info = -2;
    }
  else
    {
      Complex *result = dup (b.data (), b_len);

      F77_FCN (zgesl, ZGESL) (tmp_data, nr, nr, ipvt, result, 0);

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

  for (int j = 0; j < nrhs; j++)
    for (int i = 0; i < m; i++)
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

  F77_FCN (zgelss, ZGELSS) (m, n, nrhs, tmp_data, m, presult, nrr, s,
			    rcond, rank, work, lwork, rwork, info);

  ComplexMatrix retval (n, nrhs);
  for (int j = 0; j < nrhs; j++)
    for (int i = 0; i < n; i++)
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

  for (int i = 0; i < m; i++)
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

  F77_FCN (zgelss, ZGELSS) (m, n, nrhs, tmp_data, m, presult, nrr, s,
			    rcond, rank, work, lwork, rwork, info);

  ComplexColumnVector retval (n);
  for (int i = 0; i < n; i++)
    retval.elem (i) = result.elem (i);

  delete [] tmp_data;
  delete [] s;
  delete [] work;
  delete [] rwork;

  return retval;
}

// column vector by row vector -> matrix operations

ComplexMatrix
operator * (const ColumnVector& v, const ComplexRowVector& a)
{
  ComplexColumnVector tmp (v);
  return tmp * a;
}

ComplexMatrix
operator * (const ComplexColumnVector& a, const RowVector& b)
{
  ComplexRowVector tmp (b);
  return a * tmp;
}

ComplexMatrix
operator * (const ComplexColumnVector& v, const ComplexRowVector& a)
{
  int len = v.length ();
  int a_len = a.length ();
  if (len != a_len)
    {
      (*current_liboctave_error_handler)
	("nonconformant vector multiplication attempted");
      return ComplexMatrix ();
    }

  if (len == 0)
    return ComplexMatrix (len, len, 0.0);

  Complex *c = new Complex [len * a_len];

  F77_FCN (zgemm, ZGEMM) ("N", "N", len, a_len, 1, 1.0, v.data (),
			  len, a.data (), 1, 0.0, c, len, 1L, 1L);

  return ComplexMatrix (c, len, a_len);
}

// diagonal matrix by scalar -> matrix operations

ComplexMatrix
operator + (const DiagMatrix& a, const Complex& s)
{
  ComplexMatrix tmp (a.rows (), a.cols (), s);
  return a + tmp;
}

ComplexMatrix
operator - (const DiagMatrix& a, const Complex& s)
{
  ComplexMatrix tmp (a.rows (), a.cols (), -s);
  return a + tmp;
}

ComplexMatrix
operator + (const ComplexDiagMatrix& a, double s)
{
  ComplexMatrix tmp (a.rows (), a.cols (), s);
  return a + tmp;
}

ComplexMatrix
operator - (const ComplexDiagMatrix& a, double s)
{
  ComplexMatrix tmp (a.rows (), a.cols (), -s);
  return a + tmp;
}

ComplexMatrix
operator + (const ComplexDiagMatrix& a, const Complex& s)
{
  ComplexMatrix tmp (a.rows (), a.cols (), s);
  return a + tmp;
}

ComplexMatrix
operator - (const ComplexDiagMatrix& a, const Complex& s)
{
  ComplexMatrix tmp (a.rows (), a.cols (), -s);
  return a + tmp;
}

// scalar by diagonal matrix -> matrix operations

ComplexMatrix
operator + (const Complex& s, const DiagMatrix& a)
{
  ComplexMatrix tmp (a.rows (), a.cols (), s);
  return tmp + a;
}

ComplexMatrix
operator - (const Complex& s, const DiagMatrix& a)
{
  ComplexMatrix tmp (a.rows (), a.cols (), s);
  return tmp - a;
}

ComplexMatrix
operator + (double s, const ComplexDiagMatrix& a)
{
  ComplexMatrix tmp (a.rows (), a.cols (), s);
  return tmp + a;
}

ComplexMatrix
operator - (double s, const ComplexDiagMatrix& a)
{
  ComplexMatrix tmp (a.rows (), a.cols (), s);
  return tmp - a;
}

ComplexMatrix
operator + (const Complex& s, const ComplexDiagMatrix& a)
{
  ComplexMatrix tmp (a.rows (), a.cols (), s);
  return tmp + a;
}

ComplexMatrix
operator - (const Complex& s, const ComplexDiagMatrix& a)
{
  ComplexMatrix tmp (a.rows (), a.cols (), s);
  return tmp - a;
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
      return *this;
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
      return *this;
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
      return *this;
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
      return *this;
    }

  for (int i = 0; i < a.length (); i++)
    elem (i, i) -= a.elem (i, i);

  return *this;
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

// diagonal matrix by matrix -> matrix operations

ComplexMatrix
operator + (const DiagMatrix& m, const ComplexMatrix& a)
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

  ComplexMatrix result (a);
  for (int i = 0; i < m.length (); i++)
    result.elem (i, i) += m.elem (i, i);

  return result;
}

ComplexMatrix
operator - (const DiagMatrix& m, const ComplexMatrix& a)
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

  ComplexMatrix result (-a);
  for (int i = 0; i < m.length (); i++)
    result.elem (i, i) += m.elem (i, i);

  return result;
}

ComplexMatrix
operator * (const DiagMatrix& m, const ComplexMatrix& a)
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
    return ComplexMatrix (nr, nc, 0.0);

  ComplexMatrix c (nr, a_nc);

  for (int i = 0; i < m.length (); i++)
    {
      if (m.elem (i, i) == 1.0)
	{
	  for (int j = 0; j < a_nc; j++)
	    c.elem (i, j) = a.elem (i, j);
	}
      else if (m.elem (i, i) == 0.0)
	{
	  for (int j = 0; j < a_nc; j++)
	    c.elem (i, j) = 0.0;
	}
      else
	{
	  for (int j = 0; j < a_nc; j++)
	    c.elem (i, j) = m.elem (i, i) * a.elem (i, j);
	}
    }

  if (nr > nc)
    {
      for (int j = 0; j < a_nc; j++)
	for (int i = a_nr; i < nr; i++)
	  c.elem (i, j) = 0.0;
    }

  return c;
}

ComplexMatrix
operator + (const ComplexDiagMatrix& m, const Matrix& a)
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

  ComplexMatrix result (a);
  for (int i = 0; i < m.length (); i++)
    result.elem (i, i) += m.elem (i, i);

  return result;
}

ComplexMatrix
operator - (const ComplexDiagMatrix& m, const Matrix& a)
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

  ComplexMatrix result (-a);
  for (int i = 0; i < m.length (); i++)
    result.elem (i, i) += m.elem (i, i);

  return result;
}

ComplexMatrix
operator * (const ComplexDiagMatrix& m, const Matrix& a)
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

  ComplexMatrix c (nr, a_nc);

  for (int i = 0; i < m.length (); i++)
    {
      if (m.elem (i, i) == 1.0)
	{
	  for (int j = 0; j < a_nc; j++)
	    c.elem (i, j) = a.elem (i, j);
	}
      else if (m.elem (i, i) == 0.0)
	{
	  for (int j = 0; j < a_nc; j++)
	    c.elem (i, j) = 0.0;
	}
      else
	{
	  for (int j = 0; j < a_nc; j++)
	    c.elem (i, j) = m.elem (i, i) * a.elem (i, j);
	}
    }

  if (nr > nc)
    {
      for (int j = 0; j < a_nc; j++)
	for (int i = a_nr; i < nr; i++)
	  c.elem (i, j) = 0.0;
    }

  return c;
}

ComplexMatrix
operator + (const ComplexDiagMatrix& m, const ComplexMatrix& a)
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

  ComplexMatrix result (a);
  for (int i = 0; i < m.length (); i++)
    result.elem (i, i) += m.elem (i, i);

  return result;
}

ComplexMatrix
operator - (const ComplexDiagMatrix& m, const ComplexMatrix& a)
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

  ComplexMatrix result (-a);
  for (int i = 0; i < m.length (); i++)
    result.elem (i, i) += m.elem (i, i);

  return result;
}

ComplexMatrix
operator * (const ComplexDiagMatrix& m, const ComplexMatrix& a)
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

  ComplexMatrix c (nr, a_nc);

  for (int i = 0; i < m.length (); i++)
    {
      if (m.elem (i, i) == 1.0)
	{
	  for (int j = 0; j < a_nc; j++)
	    c.elem (i, j) = a.elem (i, j);
	}
      else if (m.elem (i, i) == 0.0)
	{
	  for (int j = 0; j < a_nc; j++)
	    c.elem (i, j) = 0.0;
	}
      else
	{
	  for (int j = 0; j < a_nc; j++)
	    c.elem (i, j) = m.elem (i, i) * a.elem (i, j);
	}
    }

  if (nr > nc)
    {
      for (int j = 0; j < a_nc; j++)
	for (int i = a_nr; i < nr; i++)
	  c.elem (i, j) = 0.0;
    }

  return c;
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
operator * (const ComplexMatrix& m, const Matrix& a)
{
  ComplexMatrix tmp (a);
  return m * tmp;
}

ComplexMatrix
operator * (const Matrix& m, const ComplexMatrix& a)
{
  ComplexMatrix tmp (m);
  return tmp * a;
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

  int ld  = nr;
  int lda = a.rows ();

  Complex *c = new Complex [nr*a_nc];

  F77_FCN (zgemm, ZGEMM) ("N", "N", nr, a_nc, nc, 1.0, m.data (), ld,
			  a.data (), lda, 0.0, c, nr, 1L, 1L);

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

// other operations

ComplexMatrix
map (c_c_Mapper f, const ComplexMatrix& a)
{
  ComplexMatrix b (a);
  b.map (f);
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

// XXX FIXME XXX -- it would be nice to share some code among all the
// min/max functions below.  It would also be nice to combine the
// min/max and min_loc/max_loc functions.

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
	  int row_is_real_only = 1;
	  for (int j = 0; j < nc; j++)
	    if (imag (elem (i, j)) != 0.0)
	      {
		row_is_real_only = 0;
		break;
	      }
	      
	  if (row_is_real_only)
	    {
	      double res = real (elem (i, 0));
	      for (int j = 1; j < nc; j++)
		{
		  double tmp = real (elem (i, j));
		  if (tmp < res)
		    res = tmp;
		}
	      result.elem (i) = res;
	    }
	  else
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
	  int column_is_real_only = 1;
	  for (int j = 0; j < nc; j++)
	    if (imag (elem (i, j)) != 0.0)
	      {
		column_is_real_only = 0;
		break;
	      }
	      
	  if (column_is_real_only)
	    {
	      double res = 0;
	      double tmp = real (elem (i, 0));
	      for (int j = 1; j < nc; j++)
		if (real (elem (i, j)) < tmp)
		  res = j;

	      result.elem (i) = res + 1;
	    }
	  else
	    {
	      Complex res = 0;
	      double absres = abs (elem (i, 0));
	      for (int j = 1; j < nc; j++)
		if (abs (elem (i, j)) < absres)
		  {
		    res = j;
		    absres = abs (elem (i, j));
		  }
	      result.elem (i) = res + 1;
	    }
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
	  int row_is_real_only = 1;
	  for (int j = 0; j < nc; j++)
	    if (imag (elem (i, j)) != 0.0)
	      {
		row_is_real_only = 0;
		break;
	      }
	      
	  if (row_is_real_only)
	    {
	      double res = real (elem (i, 0));
	      for (int j = 1; j < nc; j++)
		{
		  double tmp = real (elem (i, j));
		  if (tmp > res)
		    res = tmp;
		}
	      result.elem (i) = res;
	    }
	  else
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
	  int column_is_real_only = 1;
	  for (int j = 0; j < nc; j++)
	    if (imag (elem (i, j)) != 0.0)
	      {
		column_is_real_only = 0;
		break;
	      }
	      
	  if (column_is_real_only)
	    {
	      double res = 0;
	      double tmp = real (elem (i, 0));
	      for (int j = 1; j < nc; j++)
		if (real (elem (i, j)) > tmp)
		  res = j;

	      result.elem (i) = res + 1;
	    }
	  else
	    {
	      Complex res = 0;
	      double absres = abs (elem (i, 0));
	      for (int j = 1; j < nc; j++)
		if (abs (elem (i, j)) > absres)
		  {
		    res = j;
		    absres = abs (elem (i, j));
		  }
	      result.elem (i) = res + 1;
	    }
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
	  int column_is_real_only = 1;
	  for (int i = 0; i < nr; i++)
	    if (imag (elem (i, j)) != 0.0)
	      {
		column_is_real_only = 0;
		break;
	      }
	      
	  if (column_is_real_only)
	    {
	      double res = real (elem (0, j));
	      for (int i = 1; i < nr; i++)
		{
		  double tmp = real (elem (i, j));
		  if (tmp < res)
		    res = tmp;
		}
	      result.elem (j) = res;
	    }
	  else
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
	  int column_is_real_only = 1;
	  for (int i = 0; i < nr; i++)
	    if (imag (elem (i, j)) != 0.0)
	      {
		column_is_real_only = 0;
		break;
	      }
	      
	  if (column_is_real_only)
	    {
	      double res = 0;
	      double tmp = real (elem (0, j));
	      for (int i = 1; i < nr; i++)
		if (real (elem (i, j)) < tmp)
		  res = i;

	      result.elem (j) = res + 1;
	    }
	  else
	    {
	      Complex res = 0;
	      double absres = abs (elem (0, j));
	      for (int i = 1; i < nr; i++)
		if (abs (elem (i, j)) < absres)
		  {
		    res = i;
		    absres = abs (elem (i, j));
		  }
	      result.elem (j) = res + 1;
	    }
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
	  int column_is_real_only = 1;
	  for (int i = 0; i < nr; i++)
	    if (imag (elem (i, j)) != 0.0)
	      {
		column_is_real_only = 0;
		break;
	      }
	      
	  if (column_is_real_only)
	    {
	      double res = real (elem (0, j));
	      for (int i = 1; i < nr; i++)
		{
		  double tmp = real (elem (i, j));
		  if (tmp > res)
		    res = tmp;
		}
	      result.elem (j) = res;
	    }
	  else
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
	  int column_is_real_only = 1;
	  for (int i = 0; i < nr; i++)
	    if (imag (elem (i, j)) != 0.0)
	      {
		column_is_real_only = 0;
		break;
	      }
	      
	  if (column_is_real_only)
	    {
	      double res = 0;
	      double tmp = real (elem (0, j));
	      for (int i = 1; i < nr; i++)
		if (real (elem (i, j)) > tmp)
		  res = i;

	      result.elem (j) = res + 1;
	    }
	  else
	    {
	      Complex res = 0;
	      double absres = abs (elem (0, j));
	      for (int i = 1; i < nr; i++)
		if (abs (elem (i, j)) > absres)
		  {
		    res = i;
		    absres = abs (elem (i, j));
		  }
	      result.elem (j) = res + 1;
	    }
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
