// Matrix manipulations.
/*

Copyright (C) 1996, 1997 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>

#include <iostream>

#include "Array-util.h"
#include "byte-swap.h"
#include "dMatrix.h"
#include "dbleAEPBAL.h"
#include "dbleDET.h"
#include "dbleSCHUR.h"
#include "dbleSVD.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "lo-utils.h"
#include "mx-base.h"
#include "mx-m-dm.h"
#include "mx-dm-m.h"
#include "mx-inlines.cc"
#include "oct-cmplx.h"
#include "quit.h"

#if defined (HAVE_FFTW3)
#include "oct-fftw.h"
#endif

// Fortran functions we call.

extern "C"
{
  F77_RET_T
  F77_FUNC (dgebal, DGEBAL) (F77_CONST_CHAR_ARG_DECL,
			     const int&, double*, const int&, int&,
			     int&, double*, int&
			     F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgebak, DGEBAK) (F77_CONST_CHAR_ARG_DECL,
			     F77_CONST_CHAR_ARG_DECL,
			     const int&, const int&, const int&, double*,
			     const int&, double*, const int&, int&
			     F77_CHAR_ARG_LEN_DECL
			     F77_CHAR_ARG_LEN_DECL);


  F77_RET_T
  F77_FUNC (dgemm, DGEMM) (F77_CONST_CHAR_ARG_DECL,
			   F77_CONST_CHAR_ARG_DECL,
			   const int&, const int&, const int&,
			   const double&, const double*, const int&,
			   const double*, const int&, const double&,
			   double*, const int&
			   F77_CHAR_ARG_LEN_DECL
			   F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgetrf, DGETRF) (const int&, const int&, double*, const int&,
		      int*, int&);

  F77_RET_T
  F77_FUNC (dgetrs, DGETRS) (F77_CONST_CHAR_ARG_DECL, const int&, const int&, 
			     const double*, const int&,
			     const int*, double*, const int&, int&
			     F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgetri, DGETRI) (const int&, double*, const int&, const int*,
			     double*, const int&, int&);

  F77_RET_T
  F77_FUNC (dgecon, DGECON) (F77_CONST_CHAR_ARG_DECL, const int&, double*, 
			     const int&, const double&, double&, 
			     double*, int*, int&
			     F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgelss, DGELSS) (const int&, const int&, const int&,
			     double*, const int&, double*,
			     const int&, double*, double&, int&,
			     double*, const int&, int&);

  // Note that the original complex fft routines were not written for
  // double complex arguments.  They have been modified by adding an
  // implicit double precision (a-h,o-z) statement at the beginning of
  // each subroutine.

  F77_RET_T
  F77_FUNC (cffti, CFFTI) (const int&, Complex*);

  F77_RET_T
  F77_FUNC (cfftf, CFFTF) (const int&, Complex*, Complex*);

  F77_RET_T
  F77_FUNC (cfftb, CFFTB) (const int&, Complex*, Complex*);

  F77_RET_T
  F77_FUNC (dlartg, DLARTG) (const double&, const double&, double&,
			     double&, double&);

  F77_RET_T
  F77_FUNC (dtrsyl, DTRSYL) (F77_CONST_CHAR_ARG_DECL,
			     F77_CONST_CHAR_ARG_DECL,
			     const int&, const int&, const int&,
			     const double*, const int&, const double*,
			     const int&, const double*, const int&,
			     double&, int&
			     F77_CHAR_ARG_LEN_DECL
			     F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (xdlange, XDLANGE) (F77_CONST_CHAR_ARG_DECL, const int&,
			       const int&, const double*,
			       const int&, double*, double&
			       F77_CHAR_ARG_LEN_DECL); 
}

// Matrix class.

Matrix::Matrix (const RowVector& rv)
  : MArray2<double> (1, rv.length (), 0.0)
{
  for (int i = 0; i < rv.length (); i++)
    elem (0, i) = rv.elem (i);
}

Matrix::Matrix (const ColumnVector& cv)
  : MArray2<double> (cv.length (), 1, 0.0)
{
  for (int i = 0; i < cv.length (); i++)
    elem (i, 0) = cv.elem (i);
}

Matrix::Matrix (const DiagMatrix& a)
  : MArray2<double> (a.rows (), a.cols (), 0.0)
{
  for (int i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

// XXX FIXME XXX -- could we use a templated mixed-type copy function
// here?

Matrix::Matrix (const boolMatrix& a)
  : MArray2<double> (a.rows (), a.cols ())
{
  for (int i = 0; i < a.rows (); i++)
    for (int j = 0; j < a.cols (); j++)
      elem (i, j) = a.elem (i, j);
}

Matrix::Matrix (const charMatrix& a)
  : MArray2<double> (a.rows (), a.cols ())
{
  for (int i = 0; i < a.rows (); i++)
    for (int j = 0; j < a.cols (); j++)
      elem (i, j) = a.elem (i, j);
}

bool
Matrix::operator == (const Matrix& a) const
{
  if (rows () != a.rows () || cols () != a.cols ())
    return false;

  return mx_inline_equal (data (), a.data (), length ());
}

bool
Matrix::operator != (const Matrix& a) const
{
  return !(*this == a);
}

bool
Matrix::is_symmetric (void) const
{
  if (is_square () && rows () > 0)
    {
      for (int i = 0; i < rows (); i++)
	for (int j = i+1; j < cols (); j++)
	  if (elem (i, j) != elem (j, i))
	    return false;

      return true;
    }

  return false;
}

Matrix&
Matrix::insert (const Matrix& a, int r, int c)
{
  Array2<double>::insert (a, r, c);
  return *this;
}

Matrix&
Matrix::insert (const RowVector& a, int r, int c)
{
  int a_len = a.length ();

  if (r < 0 || r >= rows () || c < 0 || c + a_len > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  if (a_len > 0)
    {
      make_unique ();

      for (int i = 0; i < a_len; i++)
	xelem (r, c+i) = a.elem (i);
    }

  return *this;
}

Matrix&
Matrix::insert (const ColumnVector& a, int r, int c)
{
  int a_len = a.length ();

  if (r < 0 || r + a_len > rows () || c < 0 || c >= cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  if (a_len > 0)
    {
      make_unique ();

      for (int i = 0; i < a_len; i++)
	xelem (r+i, c) = a.elem (i);
    }

  return *this;
}

Matrix&
Matrix::insert (const DiagMatrix& a, int r, int c)
{
  int a_nr = a.rows ();
  int a_nc = a.cols ();

  if (r < 0 || r + a_nr > rows () || c < 0 || c + a_nc > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  fill (0.0, r, c, r + a_nr - 1, c + a_nc - 1);

  int a_len = a.length ();

  if (a_len > 0)
    {
      make_unique ();

      for (int i = 0; i < a_len; i++)
	xelem (r+i, c+i) = a.elem (i, i);
    }

  return *this;
}

Matrix&
Matrix::fill (double val)
{
  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      make_unique ();

      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  xelem (i, j) = val;
    }

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

  if (r2 >= r1 && c2 >= c1)
    {
      make_unique ();

      for (int j = c1; j <= c2; j++)
	for (int i = r1; i <= r2; i++)
	  xelem (i, j) = val;
    }

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
real (const ComplexMatrix& a)
{
  int a_len = a.length ();
  Matrix retval;
  if (a_len > 0)
    retval = Matrix (mx_inline_real_dup (a.data (), a_len),
		     a.rows (), a.cols ());
  return retval;
}

Matrix
imag (const ComplexMatrix& a)
{
  int a_len = a.length ();
  Matrix retval;
  if (a_len > 0)
    retval = Matrix (mx_inline_imag_dup (a.data (), a_len),
		     a.rows (), a.cols ());
  return retval;
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
      result.xelem (i, j) = elem (r1+i, c1+j);

  return result;
}

Matrix
Matrix::extract_n (int r1, int c1, int nr, int nc) const
{
  Matrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      result.xelem (i, j) = elem (r1+i, c1+j);

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
    retval.xelem (j) = elem (i, j);

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
    retval.xelem (j) = elem (j, i);

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
  return inverse (info, rcond, 0, 0);
}

Matrix
Matrix::inverse (int& info) const
{
  double rcond;
  return inverse (info, rcond, 0, 0);
}

Matrix
Matrix::inverse (int& info, double& rcond, int force, int calc_cond) const
{
  Matrix retval;

  int nr = rows ();
  int nc = cols ();

  if (nr != nc || nr == 0 || nc == 0)
    (*current_liboctave_error_handler) ("inverse requires square matrix");
  else
    {
      Array<int> ipvt (nr);
      int *pipvt = ipvt.fortran_vec ();

      retval = *this;
      double *tmp_data = retval.fortran_vec ();

      Array<double> z(1);
      int lwork = -1;

      // Query the optimum work array size.
      F77_XFCN (dgetri, DGETRI, (nc, tmp_data, nr, pipvt, 
				 z.fortran_vec (), lwork, info));

      if (f77_exception_encountered) 
	{
	  (*current_liboctave_error_handler)
	    ("unrecoverable error in dgetri");
	  return retval;
	}

      lwork = static_cast<int> (z(0));
      lwork = (lwork < 2 *nc ? 2*nc : lwork);
      z.resize (lwork);
      double *pz = z.fortran_vec ();

      info = 0;

      // Calculate the norm of the matrix, for later use.
      double anorm = 0;
      if (calc_cond) 
	anorm = retval.abs().sum().row(0).max();

      F77_XFCN (dgetrf, DGETRF, (nc, nc, tmp_data, nr, pipvt, info));

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in dgetrf");
      else
	{
	  // Throw-away extra info LAPACK gives so as to not change output.
	  rcond = 0.0;
	  if (info != 0) 
	    info = -1;
	  else if (calc_cond) 
	    {
	      int dgecon_info = 0;

	      // Now calculate the condition number for non-singular matrix.
	      char job = '1';
	      Array<int> iz (nc);
	      int *piz = iz.fortran_vec ();
	      F77_XFCN (dgecon, DGECON, (F77_CONST_CHAR_ARG2 (&job, 1),
					 nc, tmp_data, nr, anorm, 
					 rcond, pz, piz, dgecon_info
					 F77_CHAR_ARG_LEN (1)));

	      if (f77_exception_encountered)
		(*current_liboctave_error_handler) 
		  ("unrecoverable error in dgecon");

	      if (dgecon_info != 0) 
		info = -1;
	    }

	  if (info == -1 && ! force)
	    retval = *this; // Restore matrix contents.
	  else
	    {
	      int dgetri_info = 0;

	      F77_XFCN (dgetri, DGETRI, (nc, tmp_data, nr, pipvt,
					 pz, lwork, dgetri_info));

	      if (f77_exception_encountered)
		(*current_liboctave_error_handler)
		  ("unrecoverable error in dgetri");

	      if (dgetri_info != 0) 
		info = -1;
	    }
	}
    }

  return retval;
}

Matrix
Matrix::pseudo_inverse (double tol) const
{
  SVD result (*this, SVD::economy);

  DiagMatrix S = result.singular_values ();
  Matrix U = result.left_singular_matrix ();
  Matrix V = result.right_singular_matrix ();

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
    return Matrix (nc, nr, 0.0);
  else
    {
      Matrix Ur = U.extract (0, 0, nr-1, r);
      DiagMatrix D = DiagMatrix (sigma.extract (0, r)) . inverse ();
      Matrix Vr = V.extract (0, 0, nc-1, r);
      return Vr * D * Ur.transpose ();
    }
}

#if defined (HAVE_FFTW3)

ComplexMatrix
Matrix::fourier (void) const
{
  size_t nr = rows ();
  size_t nc = cols ();

  ComplexMatrix retval (nr, nc);

  size_t npts, nsamples;

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

  const double *in (fortran_vec ());
  Complex *out (retval.fortran_vec ());

  octave_fftw::fft (in, out, npts, nsamples); 

  return retval;
}

ComplexMatrix
Matrix::ifourier (void) const
{
  size_t nr = rows ();
  size_t nc = cols ();

  ComplexMatrix retval (nr, nc);

  size_t npts, nsamples;

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

  ComplexMatrix tmp (*this);
  Complex *in (tmp.fortran_vec ());
  Complex *out (retval.fortran_vec ());

  octave_fftw::ifft (in, out, npts, nsamples); 

  return retval;
}

ComplexMatrix
Matrix::fourier2d (void) const
{
  dim_vector dv(rows (), cols ());

  const double *in = fortran_vec ();
  ComplexMatrix retval (rows (), cols ());
  octave_fftw::fftNd (in, retval.fortran_vec (), 2, dv);

  return retval;
}

ComplexMatrix
Matrix::ifourier2d (void) const
{
  dim_vector dv(rows (), cols ());

  ComplexMatrix retval (*this);
  Complex *out (retval.fortran_vec ());

  octave_fftw::ifftNd (out, out, 2, dv);

  return retval;
}

#else

ComplexMatrix
Matrix::fourier (void) const
{
  ComplexMatrix retval;

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

  Array<Complex> wsave (nn);
  Complex *pwsave = wsave.fortran_vec ();

  retval = ComplexMatrix (*this);
  Complex *tmp_data = retval.fortran_vec ();

  F77_FUNC (cffti, CFFTI) (npts, pwsave);

  for (int j = 0; j < nsamples; j++)
    {
      OCTAVE_QUIT;

      F77_FUNC (cfftf, CFFTF) (npts, &tmp_data[npts*j], pwsave);
    }

  return retval;
}

ComplexMatrix
Matrix::ifourier (void) const
{
  ComplexMatrix retval;

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

  Array<Complex> wsave (nn);
  Complex *pwsave = wsave.fortran_vec ();

  retval = ComplexMatrix (*this);
  Complex *tmp_data = retval.fortran_vec ();

  F77_FUNC (cffti, CFFTI) (npts, pwsave);

  for (int j = 0; j < nsamples; j++)
    {
      OCTAVE_QUIT;

      F77_FUNC (cfftb, CFFTB) (npts, &tmp_data[npts*j], pwsave);
    }

  for (int j = 0; j < npts*nsamples; j++)
    tmp_data[j] = tmp_data[j] / static_cast<double> (npts);

  return retval;
}

ComplexMatrix
Matrix::fourier2d (void) const
{
  ComplexMatrix retval;

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

  Array<Complex> wsave (nn);
  Complex *pwsave = wsave.fortran_vec ();

  retval = ComplexMatrix (*this);
  Complex *tmp_data = retval.fortran_vec ();

  F77_FUNC (cffti, CFFTI) (npts, pwsave);

  for (int j = 0; j < nsamples; j++)
    {
      OCTAVE_QUIT;

      F77_FUNC (cfftf, CFFTF) (npts, &tmp_data[npts*j], pwsave);
    }

  npts = nc;
  nsamples = nr;
  nn = 4*npts+15;

  wsave.resize (nn);
  pwsave = wsave.fortran_vec ();

  Array<Complex> tmp (npts);
  Complex *prow = tmp.fortran_vec ();

  F77_FUNC (cffti, CFFTI) (npts, pwsave);

  for (int j = 0; j < nsamples; j++)
    {
      OCTAVE_QUIT;

      for (int i = 0; i < npts; i++)
	prow[i] = tmp_data[i*nr + j];

      F77_FUNC (cfftf, CFFTF) (npts, prow, pwsave);

      for (int i = 0; i < npts; i++)
	tmp_data[i*nr + j] = prow[i];
    }

  return retval;
}

ComplexMatrix
Matrix::ifourier2d (void) const
{
  ComplexMatrix retval;

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

  Array<Complex> wsave (nn);
  Complex *pwsave = wsave.fortran_vec ();

  retval = ComplexMatrix (*this);
  Complex *tmp_data = retval.fortran_vec ();

  F77_FUNC (cffti, CFFTI) (npts, pwsave);

  for (int j = 0; j < nsamples; j++)
    {
      OCTAVE_QUIT;

      F77_FUNC (cfftb, CFFTB) (npts, &tmp_data[npts*j], pwsave);
    }

  for (int j = 0; j < npts*nsamples; j++)
    tmp_data[j] = tmp_data[j] / static_cast<double> (npts);

  npts = nc;
  nsamples = nr;
  nn = 4*npts+15;

  wsave.resize (nn);
  pwsave = wsave.fortran_vec ();

  Array<Complex> tmp (npts);
  Complex *prow = tmp.fortran_vec ();

  F77_FUNC (cffti, CFFTI) (npts, pwsave);

  for (int j = 0; j < nsamples; j++)
    {
      OCTAVE_QUIT;

      for (int i = 0; i < npts; i++)
	prow[i] = tmp_data[i*nr + j];

      F77_FUNC (cfftb, CFFTB) (npts, prow, pwsave);

      for (int i = 0; i < npts; i++)
	tmp_data[i*nr + j] = prow[i] / static_cast<double> (npts);
    }

  return retval;
}

#endif

DET
Matrix::determinant (void) const
{
  int info;
  double rcond;
  return determinant (info, rcond, 0);
}

DET
Matrix::determinant (int& info) const
{
  double rcond;
  return determinant (info, rcond, 0);
}

DET
Matrix::determinant (int& info, double& rcond, int calc_cond) const
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
      Array<int> ipvt (nr);
      int *pipvt = ipvt.fortran_vec ();

      Matrix atmp = *this;
      double *tmp_data = atmp.fortran_vec ();

      info = 0;

      // Calculate the norm of the matrix, for later use.
      double anorm = 0;
      if (calc_cond) 
	anorm = atmp.abs().sum().row(0).max();

      F77_XFCN (dgetrf, DGETRF, (nr, nr, tmp_data, nr, pipvt, info));

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in dgetrf");
      else
	{
	  // Throw-away extra info LAPACK gives so as to not change output.
	  rcond = 0.0;
	  if (info != 0) 
	    {
	      info = -1;
	      retval = DET ();
	    } 
	  else 
	    {
	      if (calc_cond) 
		{
		  // Now calc the condition number for non-singular matrix.
		  char job = '1';
		  Array<double> z (4 * nc);
		  double *pz = z.fortran_vec ();
		  Array<int> iz (nc);
		  int *piz = iz.fortran_vec ();

		  F77_XFCN (dgecon, DGECON, (F77_CONST_CHAR_ARG2 (&job, 1),
					     nc, tmp_data, nr, anorm, 
					     rcond, pz, piz, info
					     F77_CHAR_ARG_LEN (1)));

		  if (f77_exception_encountered)
		    (*current_liboctave_error_handler) 
		      ("unrecoverable error in dgecon");
		}

	      if (info != 0) 
		{
		  info = -1;
		  retval = DET ();
		} 
	      else 
		{
		  double d[2] = { 1., 0.};
		  for (int i=0; i<nc; i++) 
		    {
		      if (ipvt(i) != (i+1)) d[0] = -d[0];
		      d[0] *= atmp(i,i);
		      if (d[0] == 0.) break;
		      while (fabs(d[0]) < 1.) 
			{
			  d[0] = 10. * d[0];
			  d[1] = d[1] - 1.0;
			}
		      while (fabs(d[0]) >= 10.) 
			{
			  d[0] = 0.1 * d[0];
			  d[1] = d[1] + 1.0;
			}
		    }
		  retval = DET (d);
		}
	    }
	}
    }

  return retval;
}

Matrix
Matrix::solve (const Matrix& b) const
{
  int info;
  double rcond;
  return solve (b, info, rcond, 0);
}

Matrix
Matrix::solve (const Matrix& b, int& info) const
{
  double rcond;
  return solve (b, info, rcond, 0);
}

Matrix
Matrix::solve (const Matrix& b, int& info, double& rcond) const
{
  return solve (b, info, rcond, 0);
}

Matrix
Matrix::solve (const Matrix& b, int& info, double& rcond,
	       solve_singularity_handler sing_handler) const
{
  Matrix retval;

  int nr = rows ();
  int nc = cols ();

  if (nr == 0 || nc == 0 || nr != nc || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      info = 0;

      Array<int> ipvt (nr);
      int *pipvt = ipvt.fortran_vec ();

      Matrix atmp = *this;
      double *tmp_data = atmp.fortran_vec ();

      Array<double> z (4 * nc);
      double *pz = z.fortran_vec ();
      Array<int> iz (nc);
      int *piz = iz.fortran_vec ();

      // Calculate the norm of the matrix, for later use.
      double anorm = atmp.abs().sum().row(0).max();

      F77_XFCN (dgetrf, DGETRF, (nr, nr, tmp_data, nr, pipvt, info));

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in dgetrf");
      else
	{
	  // Throw-away extra info LAPACK gives so as to not change output.
	  rcond = 0.0;
	  if (info != 0) 
	    {
	      info = -2;

	      if (sing_handler)
		sing_handler (rcond);
	      else
		(*current_liboctave_error_handler)
		  ("matrix singular to machine precision");

	    } 
	  else 
	    {
	      // Now calculate the condition number for non-singular matrix.
	      char job = '1';
	      F77_XFCN (dgecon, DGECON, (F77_CONST_CHAR_ARG2 (&job, 1),
					 nc, tmp_data, nr, anorm, 
					 rcond, pz, piz, info
					 F77_CHAR_ARG_LEN (1)));
	      
	      if (f77_exception_encountered)
		(*current_liboctave_error_handler) 
		  ("unrecoverable error in dgecon");
	      
	      if (info != 0) 
		info = -2;

	      volatile double rcond_plus_one = rcond + 1.0;

	      if (rcond_plus_one == 1.0 || xisnan (rcond))
		{
		  info = -2;

		  if (sing_handler)
		    sing_handler (rcond);
		  else
		    (*current_liboctave_error_handler)
		      ("matrix singular to machine precision, rcond = %g",
		       rcond);
		}
	      else
		{
		  retval = b;
		  double *result = retval.fortran_vec ();

		  int b_nc = b.cols ();

		  job = 'N';
		  F77_XFCN (dgetrs, DGETRS, (F77_CONST_CHAR_ARG2 (&job, 1),
					     nr, b_nc, tmp_data, nr,
					     pipvt, result, b.rows(), info
					     F77_CHAR_ARG_LEN (1)));
		
		  if (f77_exception_encountered)
		    (*current_liboctave_error_handler)
		      ("unrecoverable error in dgetrs");
		}
	    }
	}
    }

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

ComplexMatrix
Matrix::solve (const ComplexMatrix& b, int& info, double& rcond,
	       solve_singularity_handler sing_handler) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b, info, rcond, sing_handler);
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
  return solve (b, info, rcond, 0);
}

ColumnVector
Matrix::solve (const ColumnVector& b, int& info, double& rcond,
	       solve_singularity_handler sing_handler) const
{
  ColumnVector retval;

  int nr = rows ();
  int nc = cols ();

  if (nr == 0 || nc == 0 || nr != nc || nr != b.length ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      info = 0;

      Array<int> ipvt (nr);
      int *pipvt = ipvt.fortran_vec ();

      Matrix atmp = *this;
      double *tmp_data = atmp.fortran_vec ();

      Array<double> z (4 * nc);
      double *pz = z.fortran_vec ();
      Array<int> iz (nc);
      int *piz = iz.fortran_vec ();

      // Calculate the norm of the matrix, for later use.
      double anorm = atmp.abs().sum().row(0).max();

      F77_XFCN (dgetrf, DGETRF, (nr, nr, tmp_data, nr, pipvt, info));

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in dgetrf");
      else
	{
	  // Throw-away extra info LAPACK gives so as to not change output.
	  rcond = 0.0;
	  if (info > 0) 
	    {
	      info = -2;

	      if (sing_handler)
		sing_handler (rcond);
	      else
		(*current_liboctave_error_handler)
		  ("matrix singular to machine precision");

	    } 
	  else 
	    {
	      // Now calculate the condition number for non-singular matrix.
	      char job = '1';
	      F77_XFCN (dgecon, DGECON, (F77_CONST_CHAR_ARG2 (&job, 1),
					 nc, tmp_data, nr, anorm, 
					 rcond, pz, piz, info
					 F77_CHAR_ARG_LEN (1)));
	      
	      if (f77_exception_encountered)
		(*current_liboctave_error_handler) 
		  ("unrecoverable error in dgecon");

	      if (info != 0) 
		info = -2;

	      volatile double rcond_plus_one = rcond + 1.0;

	      if (rcond_plus_one == 1.0 || xisnan (rcond))
		{
		  info = -2;

		  if (sing_handler)
		    sing_handler (rcond);
		  else
		    (*current_liboctave_error_handler)
		      ("matrix singular to machine precision, rcond = %g",
		       rcond);
		}
	      else
		{
		  retval = b;
		  double *result = retval.fortran_vec ();

		  job = 'N';
		  F77_XFCN (dgetrs, DGETRS, (F77_CONST_CHAR_ARG2 (&job, 1),
					     nr, 1, tmp_data, nr, pipvt,
					     result, b.length(), info
					     F77_CHAR_ARG_LEN (1)));

		  if (f77_exception_encountered)
		    (*current_liboctave_error_handler)
		      ("unrecoverable error in dgetrs");
		}
	    }
	}
    }
  
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

ComplexColumnVector
Matrix::solve (const ComplexColumnVector& b, int& info, double& rcond,
	       solve_singularity_handler sing_handler) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b, info, rcond, sing_handler);
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
  Matrix retval;

  int nrhs = b.cols ();

  int m = rows ();
  int n = cols ();

  if (m == 0 || n == 0 || m != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch in solution of least squares problem");
  else
    {
      Matrix atmp = *this;
      double *tmp_data = atmp.fortran_vec ();

      int nrr = m > n ? m : n;
      Matrix result (nrr, nrhs, 0.0);

      for (int j = 0; j < nrhs; j++)
	for (int i = 0; i < m; i++)
	  result.elem (i, j) = b.elem (i, j);

      double *presult = result.fortran_vec ();

      int len_s = m < n ? m : n;
      Array<double> s (len_s);
      double *ps = s.fortran_vec ();

      double rcond = -1.0;

      // Ask DGELSS what the dimension of WORK should be.

      int lwork = -1;

      Array<double> work (1);

      F77_XFCN (dgelss, DGELSS, (m, n, nrhs, tmp_data, m, presult, nrr, ps,
				 rcond, rank, work.fortran_vec (),
				 lwork, info));

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in dgelss");
      else
	{
	  lwork = static_cast<int> (work(0));
	  work.resize (lwork);

	  F77_XFCN (dgelss, DGELSS, (m, n, nrhs, tmp_data, m, presult,
				     nrr, ps, rcond, rank,
				     work.fortran_vec (), lwork, info));

	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler)
	      ("unrecoverable error in dgelss");
	  else
	    {
	      retval.resize (n, nrhs);
	      for (int j = 0; j < nrhs; j++)
		for (int i = 0; i < n; i++)
		  retval.elem (i, j) = result.elem (i, j);
	    }
	}
    }

  return retval;
}

ComplexMatrix
Matrix::lssolve (const ComplexMatrix& b) const
{
  ComplexMatrix tmp (*this);
  int info;
  int rank;
  return tmp.lssolve (b, info, rank);
}

ComplexMatrix
Matrix::lssolve (const ComplexMatrix& b, int& info) const
{
  ComplexMatrix tmp (*this);
  int rank;
  return tmp.lssolve (b, info, rank);
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
  ColumnVector retval;

  int nrhs = 1;

  int m = rows ();
  int n = cols ();

  if (m == 0 || n == 0 || m != b.length ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch in solution of least squares problem");
  else
    {
      Matrix atmp = *this;
      double *tmp_data = atmp.fortran_vec ();

      int nrr = m > n ? m : n;
      ColumnVector result (nrr);

      for (int i = 0; i < m; i++)
	result.elem (i) = b.elem (i);

      double *presult = result.fortran_vec ();

      int len_s = m < n ? m : n;
      Array<double> s (len_s);
      double *ps = s.fortran_vec ();

      double rcond = -1.0;

      // Ask DGELSS what the dimension of WORK should be.

      int lwork = -1;

      Array<double> work (1);

      F77_XFCN (dgelss, DGELSS, (m, n, nrhs, tmp_data, m, presult, nrr, ps,
				 rcond, rank, work.fortran_vec (),
				 lwork, info));

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in dgelss");
      else
	{
	  lwork = static_cast<int> (work(0));
	  work.resize (lwork);

	  F77_XFCN (dgelss, DGELSS, (m, n, nrhs, tmp_data, m, presult,
				     nrr, ps, rcond, rank,
				     work.fortran_vec (), lwork, info));

	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler)
	      ("unrecoverable error in dgelss");
	  else
	    {
	      retval.resize (n);
	      for (int i = 0; i < n; i++)
		retval.elem (i) = result.elem (i);
	    }
	}
    }

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

// Constants for matrix exponential calculation.

static double padec [] =
{
  5.0000000000000000e-1,
  1.1666666666666667e-1,
  1.6666666666666667e-2,
  1.6025641025641026e-3,
  1.0683760683760684e-4,
  4.8562548562548563e-6,
  1.3875013875013875e-7,
  1.9270852604185938e-9,
};

Matrix
Matrix::expm (void) const
{
  Matrix retval;

  Matrix m = *this;

  int nc = columns ();

  // Preconditioning step 1: trace normalization to reduce dynamic
  // range of poles, but avoid making stable eigenvalues unstable.

  // trace shift value
  volatile double trshift = 0.0;

  for (int i = 0; i < nc; i++)
    trshift += m.elem (i, i);

  trshift /= nc;

  if (trshift > 0.0)
    {
      for (int i = 0; i < nc; i++)
	m.elem (i, i) -= trshift;
    }

  // Preconditioning step 2: balancing; code follows development
  // in AEPBAL

  double *p_m = m.fortran_vec ();

  int info, ilo, ihi, ilos, ihis;
  Array<double> dpermute (nc);
  Array<double> dscale (nc);

  // permutation first
  char job = 'P';
  F77_XFCN (dgebal, DGEBAL, (F77_CONST_CHAR_ARG2 (&job, 1),
			     nc, p_m, nc, ilo, ihi,
			     dpermute.fortran_vec (), info
			     F77_CHAR_ARG_LEN (1)));

  // then scaling
  job = 'S';
  F77_XFCN (dgebal, DGEBAL, (F77_CONST_CHAR_ARG2 (&job, 1),
			     nc, p_m, nc, ilos, ihis,
			     dscale.fortran_vec (), info
			     F77_CHAR_ARG_LEN (1)));

  if (f77_exception_encountered)
    {
      (*current_liboctave_error_handler) ("unrecoverable error in dgebal");
      return retval;
    }

  // Preconditioning step 3: scaling.
  
  ColumnVector work(nc);
  double inf_norm;
  
  F77_XFCN (xdlange, XDLANGE, (F77_CONST_CHAR_ARG2 ("I", 1),
			       nc, nc, m.fortran_vec (), nc,
			       work.fortran_vec (), inf_norm
			       F77_CHAR_ARG_LEN (1)));
  
  if (f77_exception_encountered)
    {
      (*current_liboctave_error_handler) ("unrecoverable error in dlange");
      return retval;
    }

  int sqpow = (int) (inf_norm > 0.0
		     ? (1.0 + log (inf_norm) / log (2.0))
		     : 0.0);
  
  // Check whether we need to square at all.
  
  if (sqpow < 0)
    sqpow = 0;
  
  if (sqpow > 0)
    {
      double scale_factor = 1.0;
      for (int i = 0; i < sqpow; i++)
	scale_factor *= 2.0;
  
      m = m / scale_factor;
    }
  
  // npp, dpp: pade' approx polynomial matrices.
  
  Matrix npp (nc, nc, 0.0);
  Matrix dpp = npp;
  
  // Now powers a^8 ... a^1.
  
  int minus_one_j = -1;
  for (int j = 7; j >= 0; j--)
    {
      npp = m * npp + padec[j] * m;
      dpp = m * dpp + (minus_one_j * padec[j]) * m;
      minus_one_j *= -1;
    }
  
  // Zero power.
  
  dpp = -dpp;
  for (int j = 0; j < nc; j++)
    {
      npp.elem (j, j) += 1.0;
      dpp.elem (j, j) += 1.0;
    }
  
  // Compute pade approximation = inverse (dpp) * npp.

  retval = dpp.solve (npp, info);
  
  // Reverse preconditioning step 3: repeated squaring.
  
  while (sqpow)
    {
      retval = retval * retval;
      sqpow--;
    }
  
  // Reverse preconditioning step 2: inverse balancing.
  // apply inverse scaling to computed exponential
  for (int i = 0; i < nc; i++)
    for (int j = 0; j < nc; j++)
       retval(i,j) *= dscale(i) / dscale(j);

  OCTAVE_QUIT;

  // construct balancing permutation vector
  Array<int> iperm (nc);
  for (int i = 0; i < nc; i++)
    iperm(i) = i;  // identity permutation

  // leading permutations in forward order
  for (int i = 0; i < (ilo-1); i++)
    {
      int swapidx = static_cast<int> (dpermute(i)) - 1;
      int tmp = iperm(i);
      iperm(i) = iperm (swapidx);
      iperm(swapidx) = tmp;
    }

  // trailing permutations must be done in reverse order
  for (int i = nc - 1; i >= ihi; i--)
    {
      int swapidx = static_cast<int> (dpermute(i)) - 1;
      int tmp = iperm(i);
      iperm(i) = iperm(swapidx);
      iperm(swapidx) = tmp;
    }

  // construct inverse balancing permutation vector
  Array<int> invpvec (nc);
  for (int i = 0; i < nc; i++)
    invpvec(iperm(i)) = i;     // Thanks to R. A. Lippert for this method

  OCTAVE_QUIT;
 
  Matrix tmpMat = retval;
  for (int i = 0; i < nc; i++)
    for (int j = 0; j < nc; j++)
      retval(i,j) = tmpMat(invpvec(i),invpvec(j));

  // Reverse preconditioning step 1: fix trace normalization.
  
  if (trshift > 0.0)
    retval = exp (trshift) * retval;

  return retval;
}

Matrix&
Matrix::operator += (const DiagMatrix& a)
{
  int nr = rows ();
  int nc = cols ();

  int a_nr = a.rows ();
  int a_nc = a.cols ();

  if (nr != a_nr || nc != a_nc)
    {
      gripe_nonconformant ("operator +=", nr, nc, a_nr, a_nc);
      return *this;
    }

  for (int i = 0; i < a.length (); i++)
    elem (i, i) += a.elem (i, i);

  return *this;
}

Matrix&
Matrix::operator -= (const DiagMatrix& a)
{
  int nr = rows ();
  int nc = cols ();

  int a_nr = a.rows ();
  int a_nc = a.cols ();

  if (nr != a_nr || nc != a_nc)
    {
      gripe_nonconformant ("operator -=", nr, nc, a_nr, a_nc);
      return *this;
    }

  for (int i = 0; i < a.length (); i++)
    elem (i, i) -= a.elem (i, i);

  return *this;
}

// unary operations

boolMatrix
Matrix::operator ! (void) const
{
  int nr = rows ();
  int nc = cols ();

  boolMatrix b (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      b.elem (i, j) = ! elem (i, j);

  return b;
}

// column vector by row vector -> matrix operations

Matrix
operator * (const ColumnVector& v, const RowVector& a)
{
  Matrix retval;

  int len = v.length ();

  if (len != 0)
    {
      int a_len = a.length ();

      retval.resize (len, a_len);
      double *c = retval.fortran_vec ();
	  
      F77_XFCN (dgemm, DGEMM, (F77_CONST_CHAR_ARG2 ("N", 1),
			       F77_CONST_CHAR_ARG2 ("N", 1),
			       len, a_len, 1, 1.0, v.data (), len,
			       a.data (), 1, 0.0, c, len
			       F77_CHAR_ARG_LEN (1)
			       F77_CHAR_ARG_LEN (1)));

      if (f77_exception_encountered)
	(*current_liboctave_error_handler)
	  ("unrecoverable error in dgemm");
    }

  return retval;
}

// other operations.

Matrix
Matrix::map (d_d_Mapper f) const
{
  Matrix b (*this);
  return b.apply (f);
}

boolMatrix
Matrix::map (b_d_Mapper f) const
{
  int nr = rows ();
  int nc = cols ();

  boolMatrix retval (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      retval(i,j) = f (elem(i,j));

  return retval;
}

Matrix&
Matrix::apply (d_d_Mapper f)
{
  double *d = fortran_vec (); // Ensures only one reference to my privates!

  for (int i = 0; i < length (); i++)
    d[i] = f (d[i]);

  return *this;
}

bool
Matrix::any_element_is_negative (bool neg_zero) const
{
  int nel = nelem ();

  if (neg_zero)
    {
      for (int i = 0; i < nel; i++)
	if (lo_ieee_signbit (elem (i)))
	  return true;
    }
  else
    {
      for (int i = 0; i < nel; i++)
	if (elem (i) < 0)
	  return true;
    }

  return false;
}


bool
Matrix::any_element_is_inf_or_nan (void) const
{
  int nel = nelem ();

  for (int i = 0; i < nel; i++)
    {
      double val = elem (i);
      if (xisinf (val) || xisnan (val))
	return true;
    }

  return false;
}

bool
Matrix::all_elements_are_int_or_inf_or_nan (void) const
{
  int nel = nelem ();

  for (int i = 0; i < nel; i++)
    {
      double val = elem (i);
      if (xisnan (val) || D_NINT (val) == val)
	continue;
      else
	return false;
    }

  return true;
}

// Return nonzero if any element of M is not an integer.  Also extract
// the largest and smallest values and return them in MAX_VAL and MIN_VAL.

bool
Matrix::all_integers (double& max_val, double& min_val) const
{
  int nel = nelem ();

  if (nel > 0)
    {
      max_val = elem (0);
      min_val = elem (0);
    }
  else
    return false;

  for (int i = 0; i < nel; i++)
    {
      double val = elem (i);

      if (val > max_val)
	max_val = val;

      if (val < min_val)
	min_val = val;

      if (D_NINT (val) != val)
	return false;
    }

  return true;
}

bool
Matrix::too_large_for_float (void) const
{
  int nel = nelem ();

  for (int i = 0; i < nel; i++)
    {
      double val = elem (i);

      if (val > FLT_MAX || val < FLT_MIN)
	return true;
    }

  return false;
}

// XXX FIXME XXX Do these really belong here?  Maybe they should be
// in a base class?

boolMatrix
Matrix::all (int dim) const
{
  MX_ALL_OP (dim);
}

boolMatrix
Matrix::any (int dim) const
{
  MX_ANY_OP (dim);
}

Matrix
Matrix::cumprod (int dim) const
{
  MX_CUMULATIVE_OP (Matrix, double, *=);
}

Matrix
Matrix::cumsum (int dim) const
{
  MX_CUMULATIVE_OP (Matrix, double, +=);
}

Matrix
Matrix::prod (int dim) const
{
  MX_REDUCTION_OP (Matrix, *=, 1.0, 1.0);
}

Matrix
Matrix::sum (int dim) const
{
  MX_REDUCTION_OP (Matrix, +=, 0.0, 0.0);
}

Matrix
Matrix::sumsq (int dim) const
{
#define ROW_EXPR \
  double d = elem (i, j); \
  retval.elem (i, 0) += d * d

#define COL_EXPR \
  double d = elem (i, j); \
  retval.elem (0, j) += d * d

  MX_BASE_REDUCTION_OP (Matrix, ROW_EXPR, COL_EXPR, 0.0, 0.0);

#undef ROW_EXPR
#undef COL_EXPR
}

Matrix
Matrix::abs (void) const
{
  int nr = rows ();
  int nc = cols ();

  Matrix retval (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      retval (i, j) = fabs (elem (i, j));

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
      else if (k < 0)
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
    (*current_liboctave_error_handler)
      ("diag: requested diagonal out of range");

  return d;
}

ColumnVector
Matrix::row_min (void) const
{
  Array<int> dummy_idx;
  return row_min (dummy_idx);
}

ColumnVector
Matrix::row_min (Array<int>& idx_arg) const
{
  ColumnVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nr);
      idx_arg.resize (nr);

      for (int i = 0; i < nr; i++)
        {
	  int idx_j;

	  double tmp_min = octave_NaN;

	  for (idx_j = 0; idx_j < nc; idx_j++)
	    {
	      tmp_min = elem (i, idx_j);

	      if (! octave_is_NaN_or_NA (tmp_min))
		break;
	    }

	  for (int j = idx_j+1; j < nc; j++)
	    {
	      double tmp = elem (i, j);

	      if (octave_is_NaN_or_NA (tmp))
		continue;
	      else if (tmp < tmp_min)
		{
		  idx_j = j;
		  tmp_min = tmp;
		}
	    }

	  result.elem (i) = tmp_min;
	  idx_arg.elem (i) = octave_is_NaN_or_NA (tmp_min) ? 0 : idx_j;
        }
    }

  return result;
}

ColumnVector
Matrix::row_max (void) const
{
  Array<int> dummy_idx;
  return row_max (dummy_idx);
}

ColumnVector
Matrix::row_max (Array<int>& idx_arg) const
{
  ColumnVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nr);
      idx_arg.resize (nr);

      for (int i = 0; i < nr; i++)
        {
	  int idx_j;

	  double tmp_max = octave_NaN;

	  for (idx_j = 0; idx_j < nc; idx_j++)
	    {
	      tmp_max = elem (i, idx_j);

	      if (! octave_is_NaN_or_NA (tmp_max))
		break;
	    }

	  for (int j = idx_j+1; j < nc; j++)
	    {
	      double tmp = elem (i, j);

	      if (octave_is_NaN_or_NA (tmp))
		continue;
	      else if (tmp > tmp_max)
		{
		  idx_j = j;
		  tmp_max = tmp;
		}
	    }

	  result.elem (i) = tmp_max;
	  idx_arg.elem (i) = octave_is_NaN_or_NA (tmp_max) ? 0 : idx_j;
        }
    }

  return result;
}

RowVector
Matrix::column_min (void) const
{
  Array<int> dummy_idx;
  return column_min (dummy_idx);
}

RowVector
Matrix::column_min (Array<int>& idx_arg) const
{
  RowVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nc);
      idx_arg.resize (nc);

      for (int j = 0; j < nc; j++)
        {
	  int idx_i;

	  double tmp_min = octave_NaN;

	  for (idx_i = 0; idx_i < nr; idx_i++)
	    {
	      tmp_min = elem (idx_i, j);

	      if (! octave_is_NaN_or_NA (tmp_min))
		break;
	    }

	  for (int i = idx_i+1; i < nr; i++)
	    {
	      double tmp = elem (i, j);

	      if (octave_is_NaN_or_NA (tmp))
		continue;
	      else if (tmp < tmp_min)
		{
		  idx_i = i;
		  tmp_min = tmp;
		}
	    }

	  result.elem (j) = tmp_min;
	  idx_arg.elem (j) = octave_is_NaN_or_NA (tmp_min) ? 0 : idx_i;
        }
    }

  return result;
}

RowVector
Matrix::column_max (void) const
{
  Array<int> dummy_idx;
  return column_max (dummy_idx);
}

RowVector
Matrix::column_max (Array<int>& idx_arg) const
{
  RowVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nc);
      idx_arg.resize (nc);

      for (int j = 0; j < nc; j++)
        {
	  int idx_i;

	  double tmp_max = octave_NaN;

	  for (idx_i = 0; idx_i < nr; idx_i++)
	    {
	      tmp_max = elem (idx_i, j);

	      if (! octave_is_NaN_or_NA (tmp_max))
		break;
	    }

	  for (int i = idx_i+1; i < nr; i++)
	    {
	      double tmp = elem (i, j);

	      if (octave_is_NaN_or_NA (tmp))
		continue;
	      else if (tmp > tmp_max)
		{
		  idx_i = i;
		  tmp_max = tmp;
		}
	    }

	  result.elem (j) = tmp_max;
	  idx_arg.elem (j) = octave_is_NaN_or_NA (tmp_max) ? 0 : idx_i;
        }
    }

  return result;
}

std::ostream&
operator << (std::ostream& os, const Matrix& a)
{
  for (int i = 0; i < a.rows (); i++)
    {
      for (int j = 0; j < a.cols (); j++)
	{
	  os << " ";
	  octave_write_double (os, a.elem (i, j));
	}
      os << "\n";
    }
  return os;
}

std::istream&
operator >> (std::istream& is, Matrix& a)
{
  int nr = a.rows ();
  int nc = a.cols ();

  if (nr < 1 || nc < 1)
    is.clear (std::ios::badbit);
  else
    {
      double tmp;
      for (int i = 0; i < nr; i++)
	for (int j = 0; j < nc; j++)
	  {
	    tmp = octave_read_double (is);
	    if (is)
	      a.elem (i, j) = tmp;
	    else
	      goto done;
	  }
    }

 done:

  return is;
}

Matrix
Givens (double x, double y)
{
  double cc, s, temp_r;

  F77_FUNC (dlartg, DLARTG) (x, y, cc, s, temp_r);

  Matrix g (2, 2);

  g.elem (0, 0) = cc;
  g.elem (1, 1) = cc;
  g.elem (0, 1) = s;
  g.elem (1, 0) = -s;

  return g;
}

Matrix
Sylvester (const Matrix& a, const Matrix& b, const Matrix& c)
{
  Matrix retval;

  // XXX FIXME XXX -- need to check that a, b, and c are all the same
  // size.

  // Compute Schur decompositions.

  SCHUR as (a, "U");
  SCHUR bs (b, "U");
  
  // Transform c to new coordinates.

  Matrix ua = as.unitary_matrix ();
  Matrix sch_a = as.schur_matrix ();

  Matrix ub = bs.unitary_matrix ();
  Matrix sch_b = bs.schur_matrix ();
  
  Matrix cx = ua.transpose () * c * ub;
  
  // Solve the sylvester equation, back-transform, and return the
  // solution.

  int a_nr = a.rows ();
  int b_nr = b.rows ();

  double scale;
  int info;

  double *pa = sch_a.fortran_vec ();
  double *pb = sch_b.fortran_vec ();
  double *px = cx.fortran_vec ();

  F77_XFCN (dtrsyl, DTRSYL, (F77_CONST_CHAR_ARG2 ("N", 1),
			     F77_CONST_CHAR_ARG2 ("N", 1),
			     1, a_nr, b_nr, pa, a_nr, pb,
			     b_nr, px, a_nr, scale, info
			     F77_CHAR_ARG_LEN (1)
			     F77_CHAR_ARG_LEN (1)));


  if (f77_exception_encountered)
    (*current_liboctave_error_handler) ("unrecoverable error in dtrsyl");
  else
    {
      // XXX FIXME XXX -- check info?
  
      retval = -ua*cx*ub.transpose ();
    }

  return retval;
}

// matrix by matrix -> matrix operations

Matrix
operator * (const Matrix& m, const Matrix& a)
{
  Matrix retval;

  int nr = m.rows ();
  int nc = m.cols ();

  int a_nr = a.rows ();
  int a_nc = a.cols ();

  if (nc != a_nr)
    gripe_nonconformant ("operator *", nr, nc, a_nr, a_nc);
  else
    {
      if (nr == 0 || nc == 0 || a_nc == 0)
	retval.resize (nr, a_nc, 0.0);
      else
	{
	  int ld  = nr;
	  int lda = a_nr;

	  retval.resize (nr, a_nc);
	  double *c = retval.fortran_vec ();

	  F77_XFCN (dgemm, DGEMM, (F77_CONST_CHAR_ARG2 ("N", 1),
				   F77_CONST_CHAR_ARG2 ("N", 1),
				   nr, a_nc, nc, 1.0, m.data (),
				   ld, a.data (), lda, 0.0, c, nr
				   F77_CHAR_ARG_LEN (1)
				   F77_CHAR_ARG_LEN (1)));

	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler)
	      ("unrecoverable error in dgemm");
	}
    }

  return retval;
}

// XXX FIXME XXX -- it would be nice to share code among the min/max
// functions below.

#define EMPTY_RETURN_CHECK(T) \
  if (nr == 0 || nc == 0) \
    return T (nr, nc);

Matrix
min (double d, const Matrix& m)
{
  int nr = m.rows ();
  int nc = m.columns ();

  EMPTY_RETURN_CHECK (Matrix);

  Matrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	OCTAVE_QUIT;
	result (i, j) = xmin (d, m (i, j));
      }

  return result;
}

Matrix
min (const Matrix& m, double d)
{
  int nr = m.rows ();
  int nc = m.columns ();

  EMPTY_RETURN_CHECK (Matrix);

  Matrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	OCTAVE_QUIT;
	result (i, j) = xmin (m (i, j), d);
      }

  return result;
}

Matrix
min (const Matrix& a, const Matrix& b)
{
  int nr = a.rows ();
  int nc = a.columns ();

  if (nr != b.rows () || nc != b.columns ())
    {
      (*current_liboctave_error_handler)
	("two-arg min expecting args of same size");
      return Matrix ();
    }

  EMPTY_RETURN_CHECK (Matrix);

  Matrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	OCTAVE_QUIT;
	result (i, j) = xmin (a (i, j), b (i, j));
      }

  return result;
}

Matrix
max (double d, const Matrix& m)
{
  int nr = m.rows ();
  int nc = m.columns ();

  EMPTY_RETURN_CHECK (Matrix);

  Matrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	OCTAVE_QUIT;
	result (i, j) = xmax (d, m (i, j));
      }

  return result;
}

Matrix
max (const Matrix& m, double d)
{
  int nr = m.rows ();
  int nc = m.columns ();

  EMPTY_RETURN_CHECK (Matrix);

  Matrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	OCTAVE_QUIT;
	result (i, j) = xmax (m (i, j), d);
      }

  return result;
}

Matrix
max (const Matrix& a, const Matrix& b)
{
  int nr = a.rows ();
  int nc = a.columns ();

  if (nr != b.rows () || nc != b.columns ())
    {
      (*current_liboctave_error_handler)
	("two-arg max expecting args of same size");
      return Matrix ();
    }

  EMPTY_RETURN_CHECK (Matrix);

  Matrix result (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	OCTAVE_QUIT;
	result (i, j) = xmax (a (i, j), b (i, j));
      }

  return result;
}

MS_CMP_OPS(Matrix, , double, )
MS_BOOL_OPS(Matrix, double, 0.0)

SM_CMP_OPS(double, , Matrix, )
SM_BOOL_OPS(double, Matrix, 0.0)

MM_CMP_OPS(Matrix, , Matrix, )
MM_BOOL_OPS(Matrix, Matrix, 0.0)

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
