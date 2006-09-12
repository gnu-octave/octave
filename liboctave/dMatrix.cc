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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

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
			     const octave_idx_type&, double*, const octave_idx_type&, octave_idx_type&,
			     octave_idx_type&, double*, octave_idx_type&
			     F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgebak, DGEBAK) (F77_CONST_CHAR_ARG_DECL,
			     F77_CONST_CHAR_ARG_DECL,
			     const octave_idx_type&, const octave_idx_type&, const octave_idx_type&, double*,
			     const octave_idx_type&, double*, const octave_idx_type&, octave_idx_type&
			     F77_CHAR_ARG_LEN_DECL
			     F77_CHAR_ARG_LEN_DECL);


  F77_RET_T
  F77_FUNC (dgemm, DGEMM) (F77_CONST_CHAR_ARG_DECL,
			   F77_CONST_CHAR_ARG_DECL,
			   const octave_idx_type&, const octave_idx_type&, const octave_idx_type&,
			   const double&, const double*, const octave_idx_type&,
			   const double*, const octave_idx_type&, const double&,
			   double*, const octave_idx_type&
			   F77_CHAR_ARG_LEN_DECL
			   F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgemv, DGEMV) (F77_CONST_CHAR_ARG_DECL,
			   const octave_idx_type&, const octave_idx_type&, const double&,
			   const double*, const octave_idx_type&, const double*,
			   const octave_idx_type&, const double&, double*,
			   const octave_idx_type&
			   F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (xddot, XDDOT) (const octave_idx_type&, const double*, const octave_idx_type&,
			   const double*, const octave_idx_type&, double&);

  F77_RET_T
  F77_FUNC (dgetrf, DGETRF) (const octave_idx_type&, const octave_idx_type&, double*, const octave_idx_type&,
		      octave_idx_type*, octave_idx_type&);

  F77_RET_T
  F77_FUNC (dgetrs, DGETRS) (F77_CONST_CHAR_ARG_DECL, const octave_idx_type&, const octave_idx_type&, 
			     const double*, const octave_idx_type&,
			     const octave_idx_type*, double*, const octave_idx_type&, octave_idx_type&
			     F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgetri, DGETRI) (const octave_idx_type&, double*, const octave_idx_type&, const octave_idx_type*,
			     double*, const octave_idx_type&, octave_idx_type&);

  F77_RET_T
  F77_FUNC (dgecon, DGECON) (F77_CONST_CHAR_ARG_DECL, const octave_idx_type&, double*, 
			     const octave_idx_type&, const double&, double&, 
			     double*, octave_idx_type*, octave_idx_type&
			     F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgelss, DGELSS) (const octave_idx_type&, const octave_idx_type&, const octave_idx_type&,
			     double*, const octave_idx_type&, double*,
			     const octave_idx_type&, double*, double&, octave_idx_type&,
			     double*, const octave_idx_type&, octave_idx_type&);

  F77_RET_T
  F77_FUNC (dpotrf, DPOTRF) (F77_CONST_CHAR_ARG_DECL, const octave_idx_type&, 
			     double *, const octave_idx_type&, 
			     octave_idx_type& F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dpocon, DPOCON) (F77_CONST_CHAR_ARG_DECL, const octave_idx_type&, 
			     double*, const octave_idx_type&, const double&,
			     double&, double*, octave_idx_type*,
			     octave_idx_type& F77_CHAR_ARG_LEN_DECL);
  F77_RET_T
  F77_FUNC (dpotrs, DPOTRS) (F77_CONST_CHAR_ARG_DECL, const octave_idx_type&, 
			     const octave_idx_type&, const double*, 
			     const octave_idx_type&, double*, 
			     const octave_idx_type&, octave_idx_type&
			     F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dtrcon, DTRCON) (F77_CONST_CHAR_ARG_DECL, F77_CONST_CHAR_ARG_DECL, 
			     F77_CONST_CHAR_ARG_DECL, const octave_idx_type&, 
			     const double*, const octave_idx_type&, double&,
			     double*, octave_idx_type*, octave_idx_type& 
			     F77_CHAR_ARG_LEN_DECL
			     F77_CHAR_ARG_LEN_DECL
			     F77_CHAR_ARG_LEN_DECL);
  F77_RET_T
  F77_FUNC (dtrtrs, DTRTRS) (F77_CONST_CHAR_ARG_DECL, F77_CONST_CHAR_ARG_DECL, 
			     F77_CONST_CHAR_ARG_DECL, const octave_idx_type&, 
			     const octave_idx_type&, const double*, 
			     const octave_idx_type&, double*, 
			     const octave_idx_type&, octave_idx_type&
			     F77_CHAR_ARG_LEN_DECL
			     F77_CHAR_ARG_LEN_DECL
			     F77_CHAR_ARG_LEN_DECL);

  // Note that the original complex fft routines were not written for
  // double complex arguments.  They have been modified by adding an
  // implicit double precision (a-h,o-z) statement at the beginning of
  // each subroutine.

  F77_RET_T
  F77_FUNC (cffti, CFFTI) (const octave_idx_type&, Complex*);

  F77_RET_T
  F77_FUNC (cfftf, CFFTF) (const octave_idx_type&, Complex*, Complex*);

  F77_RET_T
  F77_FUNC (cfftb, CFFTB) (const octave_idx_type&, Complex*, Complex*);

  F77_RET_T
  F77_FUNC (dlartg, DLARTG) (const double&, const double&, double&,
			     double&, double&);

  F77_RET_T
  F77_FUNC (dtrsyl, DTRSYL) (F77_CONST_CHAR_ARG_DECL,
			     F77_CONST_CHAR_ARG_DECL,
			     const octave_idx_type&, const octave_idx_type&, const octave_idx_type&,
			     const double*, const octave_idx_type&, const double*,
			     const octave_idx_type&, const double*, const octave_idx_type&,
			     double&, octave_idx_type&
			     F77_CHAR_ARG_LEN_DECL
			     F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (xdlange, XDLANGE) (F77_CONST_CHAR_ARG_DECL, const octave_idx_type&,
			       const octave_idx_type&, const double*,
			       const octave_idx_type&, double*, double&
			       F77_CHAR_ARG_LEN_DECL); 
}

// Matrix class.

Matrix::Matrix (const RowVector& rv)
  : MArray2<double> (1, rv.length (), 0.0)
{
  for (octave_idx_type i = 0; i < rv.length (); i++)
    elem (0, i) = rv.elem (i);
}

Matrix::Matrix (const ColumnVector& cv)
  : MArray2<double> (cv.length (), 1, 0.0)
{
  for (octave_idx_type i = 0; i < cv.length (); i++)
    elem (i, 0) = cv.elem (i);
}

Matrix::Matrix (const DiagMatrix& a)
  : MArray2<double> (a.rows (), a.cols (), 0.0)
{
  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

// FIXME -- could we use a templated mixed-type copy function
// here?

Matrix::Matrix (const boolMatrix& a)
  : MArray2<double> (a.rows (), a.cols ())
{
  for (octave_idx_type i = 0; i < a.rows (); i++)
    for (octave_idx_type j = 0; j < a.cols (); j++)
      elem (i, j) = a.elem (i, j);
}

Matrix::Matrix (const charMatrix& a)
  : MArray2<double> (a.rows (), a.cols ())
{
  for (octave_idx_type i = 0; i < a.rows (); i++)
    for (octave_idx_type j = 0; j < a.cols (); j++)
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
      for (octave_idx_type i = 0; i < rows (); i++)
	for (octave_idx_type j = i+1; j < cols (); j++)
	  if (elem (i, j) != elem (j, i))
	    return false;

      return true;
    }

  return false;
}

Matrix&
Matrix::insert (const Matrix& a, octave_idx_type r, octave_idx_type c)
{
  Array2<double>::insert (a, r, c);
  return *this;
}

Matrix&
Matrix::insert (const RowVector& a, octave_idx_type r, octave_idx_type c)
{
  octave_idx_type a_len = a.length ();

  if (r < 0 || r >= rows () || c < 0 || c + a_len > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  if (a_len > 0)
    {
      make_unique ();

      for (octave_idx_type i = 0; i < a_len; i++)
	xelem (r, c+i) = a.elem (i);
    }

  return *this;
}

Matrix&
Matrix::insert (const ColumnVector& a, octave_idx_type r, octave_idx_type c)
{
  octave_idx_type a_len = a.length ();

  if (r < 0 || r + a_len > rows () || c < 0 || c >= cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  if (a_len > 0)
    {
      make_unique ();

      for (octave_idx_type i = 0; i < a_len; i++)
	xelem (r+i, c) = a.elem (i);
    }

  return *this;
}

Matrix&
Matrix::insert (const DiagMatrix& a, octave_idx_type r, octave_idx_type c)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (r < 0 || r + a_nr > rows () || c < 0 || c + a_nc > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  fill (0.0, r, c, r + a_nr - 1, c + a_nc - 1);

  octave_idx_type a_len = a.length ();

  if (a_len > 0)
    {
      make_unique ();

      for (octave_idx_type i = 0; i < a_len; i++)
	xelem (r+i, c+i) = a.elem (i, i);
    }

  return *this;
}

Matrix&
Matrix::fill (double val)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      make_unique ();

      for (octave_idx_type j = 0; j < nc; j++)
	for (octave_idx_type i = 0; i < nr; i++)
	  xelem (i, j) = val;
    }

  return *this;
}

Matrix&
Matrix::fill (double val, octave_idx_type r1, octave_idx_type c1, octave_idx_type r2, octave_idx_type c2)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (r1 < 0 || r2 < 0 || c1 < 0 || c2 < 0
      || r1 >= nr || r2 >= nr || c1 >= nc || c2 >= nc)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  if (r1 > r2) { octave_idx_type tmp = r1; r1 = r2; r2 = tmp; }
  if (c1 > c2) { octave_idx_type tmp = c1; c1 = c2; c2 = tmp; }

  if (r2 >= r1 && c2 >= c1)
    {
      make_unique ();

      for (octave_idx_type j = c1; j <= c2; j++)
	for (octave_idx_type i = r1; i <= r2; i++)
	  xelem (i, j) = val;
    }

  return *this;
}

Matrix
Matrix::append (const Matrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != a.rows ())
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return Matrix ();
    }

  octave_idx_type nc_insert = nc;
  Matrix retval (nr, nc + a.cols ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

Matrix
Matrix::append (const RowVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != 1)
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return Matrix ();
    }

  octave_idx_type nc_insert = nc;
  Matrix retval (nr, nc + a.length ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

Matrix
Matrix::append (const ColumnVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != a.length ())
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return Matrix ();
    }

  octave_idx_type nc_insert = nc;
  Matrix retval (nr, nc + 1);
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

Matrix
Matrix::append (const DiagMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != a.rows ())
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return *this;
    }

  octave_idx_type nc_insert = nc;
  Matrix retval (nr, nc + a.cols ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

Matrix
Matrix::stack (const Matrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("column dimension mismatch for stack");
      return Matrix ();
    }

  octave_idx_type nr_insert = nr;
  Matrix retval (nr + a.rows (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

Matrix
Matrix::stack (const RowVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != a.length ())
    {
      (*current_liboctave_error_handler)
	("column dimension mismatch for stack");
      return Matrix ();
    }

  octave_idx_type nr_insert = nr;
  Matrix retval (nr + 1, nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

Matrix
Matrix::stack (const ColumnVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != 1)
    {
      (*current_liboctave_error_handler)
	("column dimension mismatch for stack");
      return Matrix ();
    }

  octave_idx_type nr_insert = nr;
  Matrix retval (nr + a.length (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

Matrix
Matrix::stack (const DiagMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != a.cols ())
    {
      (*current_liboctave_error_handler)
	("column dimension mismatch for stack");
      return Matrix ();
    }

  octave_idx_type nr_insert = nr;
  Matrix retval (nr + a.rows (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

Matrix
real (const ComplexMatrix& a)
{
  octave_idx_type a_len = a.length ();
  Matrix retval;
  if (a_len > 0)
    retval = Matrix (mx_inline_real_dup (a.data (), a_len),
		     a.rows (), a.cols ());
  return retval;
}

Matrix
imag (const ComplexMatrix& a)
{
  octave_idx_type a_len = a.length ();
  Matrix retval;
  if (a_len > 0)
    retval = Matrix (mx_inline_imag_dup (a.data (), a_len),
		     a.rows (), a.cols ());
  return retval;
}

Matrix
Matrix::extract (octave_idx_type r1, octave_idx_type c1, octave_idx_type r2, octave_idx_type c2) const
{
  if (r1 > r2) { octave_idx_type tmp = r1; r1 = r2; r2 = tmp; }
  if (c1 > c2) { octave_idx_type tmp = c1; c1 = c2; c2 = tmp; }

  octave_idx_type new_r = r2 - r1 + 1;
  octave_idx_type new_c = c2 - c1 + 1;

  Matrix result (new_r, new_c);

  for (octave_idx_type j = 0; j < new_c; j++)
    for (octave_idx_type i = 0; i < new_r; i++)
      result.xelem (i, j) = elem (r1+i, c1+j);

  return result;
}

Matrix
Matrix::extract_n (octave_idx_type r1, octave_idx_type c1, octave_idx_type nr, octave_idx_type nc) const
{
  Matrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      result.xelem (i, j) = elem (r1+i, c1+j);

  return result;
}

// extract row or column i.

RowVector
Matrix::row (octave_idx_type i) const
{
  octave_idx_type nc = cols ();
  if (i < 0 || i >= rows ())
    {
      (*current_liboctave_error_handler) ("invalid row selection");
      return RowVector ();
    }

  RowVector retval (nc);
  for (octave_idx_type j = 0; j < nc; j++)
    retval.xelem (j) = elem (i, j);

  return retval;
}

ColumnVector
Matrix::column (octave_idx_type i) const
{
  octave_idx_type nr = rows ();
  if (i < 0 || i >= cols ())
    {
      (*current_liboctave_error_handler) ("invalid column selection");
      return ColumnVector ();
    }

  ColumnVector retval (nr);
  for (octave_idx_type j = 0; j < nr; j++)
    retval.xelem (j) = elem (j, i);

  return retval;
}

Matrix
Matrix::inverse (void) const
{
  octave_idx_type info;
  double rcond;
  return inverse (info, rcond, 0, 0);
}

Matrix
Matrix::inverse (octave_idx_type& info) const
{
  double rcond;
  return inverse (info, rcond, 0, 0);
}

Matrix
Matrix::inverse (octave_idx_type& info, double& rcond, int force, int calc_cond) const
{
  Matrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr != nc || nr == 0 || nc == 0)
    (*current_liboctave_error_handler) ("inverse requires square matrix");
  else
    {
      Array<octave_idx_type> ipvt (nr);
      octave_idx_type *pipvt = ipvt.fortran_vec ();

      retval = *this;
      double *tmp_data = retval.fortran_vec ();

      Array<double> z(1);
      octave_idx_type lwork = -1;

      // Query the optimum work array size.
      F77_XFCN (dgetri, DGETRI, (nc, tmp_data, nr, pipvt, 
				 z.fortran_vec (), lwork, info));

      if (f77_exception_encountered) 
	{
	  (*current_liboctave_error_handler)
	    ("unrecoverable error in dgetri");
	  return retval;
	}

      lwork = static_cast<octave_idx_type> (z(0));
      lwork = (lwork < 2 *nc ? 2*nc : lwork);
      z.resize (lwork);
      double *pz = z.fortran_vec ();

      info = 0;

      // Calculate the norm of the matrix, for later use.
      double anorm = 0;
      if (calc_cond) 
	anorm = retval.abs().sum().row(static_cast<octave_idx_type>(0)).max();

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
	      octave_idx_type dgecon_info = 0;

	      // Now calculate the condition number for non-singular matrix.
	      char job = '1';
	      Array<octave_idx_type> iz (nc);
	      octave_idx_type *piz = iz.fortran_vec ();
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
	      octave_idx_type dgetri_info = 0;

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

  octave_idx_type r = sigma.length () - 1;
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

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

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type npts, nsamples;

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

  octave_idx_type nn = 4*npts+15;

  Array<Complex> wsave (nn);
  Complex *pwsave = wsave.fortran_vec ();

  retval = ComplexMatrix (*this);
  Complex *tmp_data = retval.fortran_vec ();

  F77_FUNC (cffti, CFFTI) (npts, pwsave);

  for (octave_idx_type j = 0; j < nsamples; j++)
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

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type npts, nsamples;

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

  octave_idx_type nn = 4*npts+15;

  Array<Complex> wsave (nn);
  Complex *pwsave = wsave.fortran_vec ();

  retval = ComplexMatrix (*this);
  Complex *tmp_data = retval.fortran_vec ();

  F77_FUNC (cffti, CFFTI) (npts, pwsave);

  for (octave_idx_type j = 0; j < nsamples; j++)
    {
      OCTAVE_QUIT;

      F77_FUNC (cfftb, CFFTB) (npts, &tmp_data[npts*j], pwsave);
    }

  for (octave_idx_type j = 0; j < npts*nsamples; j++)
    tmp_data[j] = tmp_data[j] / static_cast<double> (npts);

  return retval;
}

ComplexMatrix
Matrix::fourier2d (void) const
{
  ComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type npts, nsamples;

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

  octave_idx_type nn = 4*npts+15;

  Array<Complex> wsave (nn);
  Complex *pwsave = wsave.fortran_vec ();

  retval = ComplexMatrix (*this);
  Complex *tmp_data = retval.fortran_vec ();

  F77_FUNC (cffti, CFFTI) (npts, pwsave);

  for (octave_idx_type j = 0; j < nsamples; j++)
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

  for (octave_idx_type j = 0; j < nsamples; j++)
    {
      OCTAVE_QUIT;

      for (octave_idx_type i = 0; i < npts; i++)
	prow[i] = tmp_data[i*nr + j];

      F77_FUNC (cfftf, CFFTF) (npts, prow, pwsave);

      for (octave_idx_type i = 0; i < npts; i++)
	tmp_data[i*nr + j] = prow[i];
    }

  return retval;
}

ComplexMatrix
Matrix::ifourier2d (void) const
{
  ComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type npts, nsamples;

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

  octave_idx_type nn = 4*npts+15;

  Array<Complex> wsave (nn);
  Complex *pwsave = wsave.fortran_vec ();

  retval = ComplexMatrix (*this);
  Complex *tmp_data = retval.fortran_vec ();

  F77_FUNC (cffti, CFFTI) (npts, pwsave);

  for (octave_idx_type j = 0; j < nsamples; j++)
    {
      OCTAVE_QUIT;

      F77_FUNC (cfftb, CFFTB) (npts, &tmp_data[npts*j], pwsave);
    }

  for (octave_idx_type j = 0; j < npts*nsamples; j++)
    tmp_data[j] = tmp_data[j] / static_cast<double> (npts);

  npts = nc;
  nsamples = nr;
  nn = 4*npts+15;

  wsave.resize (nn);
  pwsave = wsave.fortran_vec ();

  Array<Complex> tmp (npts);
  Complex *prow = tmp.fortran_vec ();

  F77_FUNC (cffti, CFFTI) (npts, pwsave);

  for (octave_idx_type j = 0; j < nsamples; j++)
    {
      OCTAVE_QUIT;

      for (octave_idx_type i = 0; i < npts; i++)
	prow[i] = tmp_data[i*nr + j];

      F77_FUNC (cfftb, CFFTB) (npts, prow, pwsave);

      for (octave_idx_type i = 0; i < npts; i++)
	tmp_data[i*nr + j] = prow[i] / static_cast<double> (npts);
    }

  return retval;
}

#endif

DET
Matrix::determinant (void) const
{
  octave_idx_type info;
  double rcond;
  return determinant (info, rcond, 0);
}

DET
Matrix::determinant (octave_idx_type& info) const
{
  double rcond;
  return determinant (info, rcond, 0);
}

DET
Matrix::determinant (octave_idx_type& info, double& rcond, int calc_cond) const
{
  DET retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr == 0 || nc == 0)
    {
      retval = DET (1.0, 0);
    }
  else
    {
      Array<octave_idx_type> ipvt (nr);
      octave_idx_type *pipvt = ipvt.fortran_vec ();

      Matrix atmp = *this;
      double *tmp_data = atmp.fortran_vec ();

      info = 0;

      // Calculate the norm of the matrix, for later use.
      double anorm = 0;
      if (calc_cond) 
	anorm = atmp.abs().sum().row(static_cast<octave_idx_type>(0)).max();

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
		  Array<octave_idx_type> iz (nc);
		  octave_idx_type *piz = iz.fortran_vec ();

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
		  double c = 1.0;
		  int e = 0;

		  for (octave_idx_type i = 0; i < nc; i++) 
		    {
		      if (ipvt(i) != (i+1))
			c = -c;

		      c *= atmp(i,i);

		      if (c == 0.0)
			break;

		      while (fabs (c) < 0.5)
			{
			  c *= 2.0;
			  e--;
			}

		      while (fabs (c) >= 2.0)
			{
			  c /= 2.0;
			  e++;
			}
		    }

		  retval = DET (c, e);
		}
	    }
	}
    }

  return retval;
}

Matrix
Matrix::utsolve (MatrixType &mattype, const Matrix& b, octave_idx_type& info,
		double& rcond, solve_singularity_handler sing_handler,
		bool calc_cond) const
{
  Matrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr == 0 || nc == 0 || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      volatile int typ = mattype.type ();

      if (typ == MatrixType::Permuted_Upper ||
	  typ == MatrixType::Upper)
	{
	  octave_idx_type b_nc = b.cols ();
	  rcond = 1.;
	  info = 0;

	  if (typ == MatrixType::Permuted_Upper)
	    {
	      (*current_liboctave_error_handler)
		("Permuted triangular matrix not implemented");
	    }
	  else
	    {
	      const double *tmp_data = fortran_vec ();

	      if (calc_cond)
		{
		  char norm = '1';
		  char uplo = 'U';
		  char dia = 'N';

		  Array<double> z (3 * nc);
		  double *pz = z.fortran_vec ();
		  Array<octave_idx_type> iz (nc);
		  octave_idx_type *piz = iz.fortran_vec ();

		  F77_XFCN (dtrcon, DTRCON, (F77_CONST_CHAR_ARG2 (&norm, 1), 
					     F77_CONST_CHAR_ARG2 (&uplo, 1), 
					     F77_CONST_CHAR_ARG2 (&dia, 1), 
					     nr, tmp_data, nr, rcond,
					     pz, piz, info
					     F77_CHAR_ARG_LEN (1)
					     F77_CHAR_ARG_LEN (1)
					     F77_CHAR_ARG_LEN (1)));

		  if (f77_exception_encountered)
		    (*current_liboctave_error_handler) 
		      ("unrecoverable error in dtrcon");

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
		}

	      if (info == 0)
		{
		  retval = b;
		  double *result = retval.fortran_vec ();

		  char uplo = 'U';
		  char trans = 'N';
		  char dia = 'N';

		  F77_XFCN (dtrtrs, DTRTRS, (F77_CONST_CHAR_ARG2 (&uplo, 1), 
					     F77_CONST_CHAR_ARG2 (&trans, 1), 
					     F77_CONST_CHAR_ARG2 (&dia, 1), 
					     nr, b_nc, tmp_data, nr,
					     result, nr, info
					     F77_CHAR_ARG_LEN (1)
					     F77_CHAR_ARG_LEN (1)
					     F77_CHAR_ARG_LEN (1)));

		  if (f77_exception_encountered)
		    (*current_liboctave_error_handler) 
		      ("unrecoverable error in dtrtrs");
		}
	    }
	}
      else
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

Matrix
Matrix::ltsolve (MatrixType &mattype, const Matrix& b, octave_idx_type& info,
		double& rcond, solve_singularity_handler sing_handler,
		bool calc_cond) const
{
  Matrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr == 0 || nc == 0 || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      volatile int typ = mattype.type ();

      if (typ == MatrixType::Permuted_Lower ||
	  typ == MatrixType::Lower)
	{
	  octave_idx_type b_nc = b.cols ();
	  rcond = 1.;
	  info = 0;

	  if (typ == MatrixType::Permuted_Lower)
	    {
	      (*current_liboctave_error_handler)
		("Permuted triangular matrix not implemented");
	    }
	  else
	    {
	      const double *tmp_data = fortran_vec ();

	      if (calc_cond)
		{
		  char norm = '1';
		  char uplo = 'L';
		  char dia = 'N';

		  Array<double> z (3 * nc);
		  double *pz = z.fortran_vec ();
		  Array<octave_idx_type> iz (nc);
		  octave_idx_type *piz = iz.fortran_vec ();

		  F77_XFCN (dtrcon, DTRCON, (F77_CONST_CHAR_ARG2 (&norm, 1), 
					     F77_CONST_CHAR_ARG2 (&uplo, 1), 
					     F77_CONST_CHAR_ARG2 (&dia, 1), 
					     nr, tmp_data, nr, rcond,
					     pz, piz, info
					     F77_CHAR_ARG_LEN (1)
					     F77_CHAR_ARG_LEN (1)
					     F77_CHAR_ARG_LEN (1)));

		  if (f77_exception_encountered)
		    (*current_liboctave_error_handler) 
		      ("unrecoverable error in dtrcon");

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
		}

	      if (info == 0)
		{
		  retval = b;
		  double *result = retval.fortran_vec ();

		  char uplo = 'L';
		  char trans = 'N';
		  char dia = 'N';

		  F77_XFCN (dtrtrs, DTRTRS, (F77_CONST_CHAR_ARG2 (&uplo, 1), 
					     F77_CONST_CHAR_ARG2 (&trans, 1), 
					     F77_CONST_CHAR_ARG2 (&dia, 1), 
					     nr, b_nc, tmp_data, nr,
					     result, nr, info
					     F77_CHAR_ARG_LEN (1)
					     F77_CHAR_ARG_LEN (1)
					     F77_CHAR_ARG_LEN (1)));

		  if (f77_exception_encountered)
		    (*current_liboctave_error_handler) 
		      ("unrecoverable error in dtrtrs");
		}
	    }
	}
      else
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

Matrix
Matrix::fsolve (MatrixType &mattype, const Matrix& b, octave_idx_type& info,
		double& rcond, solve_singularity_handler sing_handler,
		bool calc_cond) const
{
  Matrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr == 0 || nc == 0 || nr != nc || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      volatile int typ = mattype.type ();
 
     // Calculate the norm of the matrix, for later use.
      double anorm = -1.;

      if (typ == MatrixType::Hermitian)
	{
	  info = 0;
	  char job = 'L';
	  Matrix atmp = *this;
	  double *tmp_data = atmp.fortran_vec ();
	  anorm = atmp.abs().sum().row(static_cast<octave_idx_type>(0)).max();

	  F77_XFCN (dpotrf, DPOTRF, (F77_CONST_CHAR_ARG2 (&job, 1), nr, 
				     tmp_data, nr, info
				     F77_CHAR_ARG_LEN (1)));

	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler) 
	      ("unrecoverable error in dpotrf");
	  else
	    {
	      // Throw-away extra info LAPACK gives so as to not change output.
	      rcond = 0.0;
	      if (info != 0) 
		{
		  info = -2;

		  mattype.mark_as_unsymmetric ();
		  typ = MatrixType::Full;
		}
	      else 
		{
		  if (calc_cond)
		    {
		      Array<double> z (3 * nc);
		      double *pz = z.fortran_vec ();
		      Array<octave_idx_type> iz (nc);
		      octave_idx_type *piz = iz.fortran_vec ();

		      F77_XFCN (dpocon, DPOCON, (F77_CONST_CHAR_ARG2 (&job, 1),
						 nr, tmp_data, nr, anorm,
						 rcond, pz, piz, info
						 F77_CHAR_ARG_LEN (1)));

		      if (f77_exception_encountered)
			(*current_liboctave_error_handler) 
			  ("unrecoverable error in dpocon");
	      
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
		    }

		  if (info == 0)
		    {
		      retval = b;
		      double *result = retval.fortran_vec ();

		      octave_idx_type b_nc = b.cols ();

		      F77_XFCN (dpotrs, DPOTRS, (F77_CONST_CHAR_ARG2 (&job, 1),
						 nr, b_nc, tmp_data, nr,
						 result, b.rows(), info
						 F77_CHAR_ARG_LEN (1)));
		
		      if (f77_exception_encountered)
			(*current_liboctave_error_handler)
			  ("unrecoverable error in dpotrs");
		    }
		  else
		    {
		      mattype.mark_as_unsymmetric ();
		      typ = MatrixType::Full;
		    }		    
		}
	    }
	}

      if (typ == MatrixType::Full)
	{
	  info = 0;

	  Array<octave_idx_type> ipvt (nr);
	  octave_idx_type *pipvt = ipvt.fortran_vec ();

	  Matrix atmp = *this;
	  double *tmp_data = atmp.fortran_vec ();
	  if(anorm < 0.)
	    anorm = atmp.abs().sum().row(static_cast<octave_idx_type>(0)).max();

	  Array<double> z (4 * nc);
	  double *pz = z.fortran_vec ();
	  Array<octave_idx_type> iz (nc);
	  octave_idx_type *piz = iz.fortran_vec ();

	  F77_XFCN (dgetrf, DGETRF, (nr, nr, tmp_data, nr, pipvt, info));

	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler) 
	      ("unrecoverable error in dgetrf");
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

		  mattype.mark_as_rectangular ();
		}
	      else 
		{
		  if (calc_cond)
		    {
		      // Now calculate the condition number for 
		      // non-singular matrix.
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
		    }

		  if (info == 0)
		    {
		      retval = b;
		      double *result = retval.fortran_vec ();

		      octave_idx_type b_nc = b.cols ();

		      char job = 'N';
		      F77_XFCN (dgetrs, DGETRS, (F77_CONST_CHAR_ARG2 (&job, 1),
						 nr, b_nc, tmp_data, nr,
						 pipvt, result, b.rows(), info
						 F77_CHAR_ARG_LEN (1)));
		
		      if (f77_exception_encountered)
			(*current_liboctave_error_handler)
			  ("unrecoverable error in dgetrs");
		    }
		  else
		    mattype.mark_as_rectangular ();
		}
	    }
	}
      else if (typ != MatrixType::Hermitian)
	(*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

Matrix
Matrix::solve (MatrixType &typ, const Matrix& b) const
{
  octave_idx_type info;
  double rcond;
  return solve (typ, b, info, rcond, 0);
}

Matrix
Matrix::solve (MatrixType &typ, const Matrix& b, octave_idx_type& info, 
	       double& rcond) const
{
  return solve (typ, b, info, rcond, 0);
}

Matrix
Matrix::solve (MatrixType &mattype, const Matrix& b, octave_idx_type& info,
	       double& rcond, solve_singularity_handler sing_handler,
	       bool singular_fallback) const
{
  Matrix retval;
  int typ = mattype.type ();

  if (typ == MatrixType::Unknown)
    typ = mattype.type (*this);

  // Only calculate the condition number for LU/Cholesky
  if (typ == MatrixType::Upper || typ == MatrixType::Permuted_Upper)
    retval = utsolve (mattype, b, info, rcond, sing_handler, false);
  else if (typ == MatrixType::Lower || typ == MatrixType::Permuted_Lower)
    retval = ltsolve (mattype, b, info, rcond, sing_handler, false);
  else if (typ == MatrixType::Full || typ == MatrixType::Hermitian)
    retval = fsolve (mattype, b, info, rcond, sing_handler, true);
  else if (typ != MatrixType::Rectangular)
    {
      (*current_liboctave_error_handler) ("unknown matrix type");
      return Matrix ();
    }

  // Rectangular or one of the above solvers flags a singular matrix
  if (singular_fallback && mattype.type () == MatrixType::Rectangular)
    {
      octave_idx_type rank;
      retval = lssolve (b, info, rank);
    }

  return retval;
}

ComplexMatrix
Matrix::solve (MatrixType &typ, const ComplexMatrix& b) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (typ, b);
}

ComplexMatrix
Matrix::solve (MatrixType &typ, const ComplexMatrix& b, 
  octave_idx_type& info) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (typ, b, info);
}

ComplexMatrix
Matrix::solve (MatrixType &typ, const ComplexMatrix& b, octave_idx_type& info,
	       double& rcond) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (typ, b, info, rcond);
}

ComplexMatrix
Matrix::solve (MatrixType &typ, const ComplexMatrix& b, octave_idx_type& info,
	       double& rcond, solve_singularity_handler sing_handler,
	       bool singular_fallback) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (typ, b, info, rcond, sing_handler, singular_fallback);
}

ColumnVector
Matrix::solve (MatrixType &typ, const ColumnVector& b) const
{
  octave_idx_type info; double rcond;
  return solve (typ, b, info, rcond);
}

ColumnVector
Matrix::solve (MatrixType &typ, const ColumnVector& b, 
	       octave_idx_type& info) const
{
  double rcond;
  return solve (typ, b, info, rcond);
}

ColumnVector
Matrix::solve (MatrixType &typ, const ColumnVector& b, octave_idx_type& info,
	       double& rcond) const
{
  return solve (typ, b, info, rcond, 0);
}

ColumnVector
Matrix::solve (MatrixType &typ, const ColumnVector& b, octave_idx_type& info,
	       double& rcond, solve_singularity_handler sing_handler) const
{
  Matrix tmp (b);
  return solve (typ, tmp, info, rcond, sing_handler).column(static_cast<octave_idx_type> (0));
}

ComplexColumnVector
Matrix::solve (MatrixType &typ, const ComplexColumnVector& b) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (typ, b);
}

ComplexColumnVector
Matrix::solve (MatrixType &typ, const ComplexColumnVector& b, 
	       octave_idx_type& info) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (typ, b, info);
}

ComplexColumnVector
Matrix::solve (MatrixType &typ, const ComplexColumnVector& b, 
	       octave_idx_type& info, double& rcond) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (typ, b, info, rcond);
}

ComplexColumnVector
Matrix::solve (MatrixType &typ, const ComplexColumnVector& b, 
	       octave_idx_type& info, double& rcond,
	       solve_singularity_handler sing_handler) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve(typ, b, info, rcond, sing_handler);
}

Matrix
Matrix::solve (const Matrix& b) const
{
  octave_idx_type info;
  double rcond;
  return solve (b, info, rcond, 0);
}

Matrix
Matrix::solve (const Matrix& b, octave_idx_type& info) const
{
  double rcond;
  return solve (b, info, rcond, 0);
}

Matrix
Matrix::solve (const Matrix& b, octave_idx_type& info, double& rcond) const
{
  return solve (b, info, rcond, 0);
}

Matrix
Matrix::solve (const Matrix& b, octave_idx_type& info,
	       double& rcond, solve_singularity_handler sing_handler) const
{
  MatrixType mattype (*this);
  return solve (mattype, b, info, rcond, sing_handler);
}

ComplexMatrix
Matrix::solve (const ComplexMatrix& b) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b);
}

ComplexMatrix
Matrix::solve (const ComplexMatrix& b, octave_idx_type& info) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b, info);
}

ComplexMatrix
Matrix::solve (const ComplexMatrix& b, octave_idx_type& info, double& rcond) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b, info, rcond);
}

ComplexMatrix
Matrix::solve (const ComplexMatrix& b, octave_idx_type& info, double& rcond,
	       solve_singularity_handler sing_handler) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b, info, rcond, sing_handler);
}

ColumnVector
Matrix::solve (const ColumnVector& b) const
{
  octave_idx_type info; double rcond;
  return solve (b, info, rcond);
}

ColumnVector
Matrix::solve (const ColumnVector& b, octave_idx_type& info) const
{
  double rcond;
  return solve (b, info, rcond);
}

ColumnVector
Matrix::solve (const ColumnVector& b, octave_idx_type& info, double& rcond) const
{
  return solve (b, info, rcond, 0);
}

ColumnVector
Matrix::solve (const ColumnVector& b, octave_idx_type& info, double& rcond,
	       solve_singularity_handler sing_handler) const
{
  MatrixType mattype (*this);
  return solve (mattype, b, info, rcond, sing_handler);
}

ComplexColumnVector
Matrix::solve (const ComplexColumnVector& b) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b);
}

ComplexColumnVector
Matrix::solve (const ComplexColumnVector& b, octave_idx_type& info) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b, info);
}

ComplexColumnVector
Matrix::solve (const ComplexColumnVector& b, octave_idx_type& info, double& rcond) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b, info, rcond);
}

ComplexColumnVector
Matrix::solve (const ComplexColumnVector& b, octave_idx_type& info, double& rcond,
	       solve_singularity_handler sing_handler) const
{
  ComplexMatrix tmp (*this);
  return tmp.solve (b, info, rcond, sing_handler);
}

Matrix
Matrix::lssolve (const Matrix& b) const
{
  octave_idx_type info;
  octave_idx_type rank;
  return lssolve (b, info, rank);
}

Matrix
Matrix::lssolve (const Matrix& b, octave_idx_type& info) const
{
  octave_idx_type rank;
  return lssolve (b, info, rank);
}

Matrix
Matrix::lssolve (const Matrix& b, octave_idx_type& info, octave_idx_type& rank) const
{
  Matrix retval;

  octave_idx_type nrhs = b.cols ();

  octave_idx_type m = rows ();
  octave_idx_type n = cols ();

  if (m == 0 || n == 0 || m != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch in solution of least squares problem");
  else
    {
      Matrix atmp = *this;
      double *tmp_data = atmp.fortran_vec ();

      octave_idx_type nrr = m > n ? m : n;
      Matrix result (nrr, nrhs, 0.0);

      for (octave_idx_type j = 0; j < nrhs; j++)
	for (octave_idx_type i = 0; i < m; i++)
	  result.elem (i, j) = b.elem (i, j);

      double *presult = result.fortran_vec ();

      octave_idx_type len_s = m < n ? m : n;
      Array<double> s (len_s);
      double *ps = s.fortran_vec ();

      double rcond = -1.0;

      // Ask DGELSS what the dimension of WORK should be.

      octave_idx_type lwork = -1;

      Array<double> work (1);

      F77_XFCN (dgelss, DGELSS, (m, n, nrhs, tmp_data, m, presult, nrr, ps,
				 rcond, rank, work.fortran_vec (),
				 lwork, info));

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in dgelss");
      else
	{
	  lwork = static_cast<octave_idx_type> (work(0));
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
	      for (octave_idx_type j = 0; j < nrhs; j++)
		for (octave_idx_type i = 0; i < n; i++)
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
  octave_idx_type info;
  octave_idx_type rank;
  return tmp.lssolve (b, info, rank);
}

ComplexMatrix
Matrix::lssolve (const ComplexMatrix& b, octave_idx_type& info) const
{
  ComplexMatrix tmp (*this);
  octave_idx_type rank;
  return tmp.lssolve (b, info, rank);
}

ComplexMatrix
Matrix::lssolve (const ComplexMatrix& b, octave_idx_type& info, octave_idx_type& rank) const
{
  ComplexMatrix tmp (*this);
  return tmp.lssolve (b, info, rank);
}

ColumnVector
Matrix::lssolve (const ColumnVector& b) const
{
  octave_idx_type info;
  octave_idx_type rank;
  return lssolve (b, info, rank);
}

ColumnVector
Matrix::lssolve (const ColumnVector& b, octave_idx_type& info) const
{
  octave_idx_type rank;
  return lssolve (b, info, rank);
}

ColumnVector
Matrix::lssolve (const ColumnVector& b, octave_idx_type& info, octave_idx_type& rank) const
{
  ColumnVector retval;

  octave_idx_type nrhs = 1;

  octave_idx_type m = rows ();
  octave_idx_type n = cols ();

  if (m == 0 || n == 0 || m != b.length ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch in solution of least squares problem");
  else
    {
      Matrix atmp = *this;
      double *tmp_data = atmp.fortran_vec ();

      octave_idx_type nrr = m > n ? m : n;
      ColumnVector result (nrr);

      for (octave_idx_type i = 0; i < m; i++)
	result.elem (i) = b.elem (i);

      double *presult = result.fortran_vec ();

      octave_idx_type len_s = m < n ? m : n;
      Array<double> s (len_s);
      double *ps = s.fortran_vec ();

      double rcond = -1.0;

      // Ask DGELSS what the dimension of WORK should be.

      octave_idx_type lwork = -1;

      Array<double> work (1);

      F77_XFCN (dgelss, DGELSS, (m, n, nrhs, tmp_data, m, presult, nrr, ps,
				 rcond, rank, work.fortran_vec (),
				 lwork, info));

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in dgelss");
      else
	{
	  lwork = static_cast<octave_idx_type> (work(0));
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
	      for (octave_idx_type i = 0; i < n; i++)
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
Matrix::lssolve (const ComplexColumnVector& b, octave_idx_type& info) const
{
  ComplexMatrix tmp (*this);
  return tmp.lssolve (b, info);
}

ComplexColumnVector
Matrix::lssolve (const ComplexColumnVector& b, octave_idx_type& info, octave_idx_type& rank) const
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

  octave_idx_type nc = columns ();

  // Preconditioning step 1: trace normalization to reduce dynamic
  // range of poles, but avoid making stable eigenvalues unstable.

  // trace shift value
  volatile double trshift = 0.0;

  for (octave_idx_type i = 0; i < nc; i++)
    trshift += m.elem (i, i);

  trshift /= nc;

  if (trshift > 0.0)
    {
      for (octave_idx_type i = 0; i < nc; i++)
	m.elem (i, i) -= trshift;
    }

  // Preconditioning step 2: balancing; code follows development
  // in AEPBAL

  double *p_m = m.fortran_vec ();

  octave_idx_type info, ilo, ihi, ilos, ihis;
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

  octave_idx_type sqpow = static_cast<octave_idx_type> (inf_norm > 0.0
		     ? (1.0 + log (inf_norm) / log (2.0))
		     : 0.0);
  
  // Check whether we need to square at all.
  
  if (sqpow < 0)
    sqpow = 0;
  
  if (sqpow > 0)
    {
      double scale_factor = 1.0;
      for (octave_idx_type i = 0; i < sqpow; i++)
	scale_factor *= 2.0;
  
      m = m / scale_factor;
    }
  
  // npp, dpp: pade' approx polynomial matrices.
  
  Matrix npp (nc, nc, 0.0);
  Matrix dpp = npp;
  
  // Now powers a^8 ... a^1.
  
  octave_idx_type minus_one_j = -1;
  for (octave_idx_type j = 7; j >= 0; j--)
    {
      npp = m * npp + padec[j] * m;
      dpp = m * dpp + (minus_one_j * padec[j]) * m;
      minus_one_j *= -1;
    }
  
  // Zero power.
  
  dpp = -dpp;
  for (octave_idx_type j = 0; j < nc; j++)
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
  for (octave_idx_type i = 0; i < nc; i++)
    for (octave_idx_type j = 0; j < nc; j++)
       retval(i,j) *= dscale(i) / dscale(j);

  OCTAVE_QUIT;

  // construct balancing permutation vector
  Array<octave_idx_type> iperm (nc);
  for (octave_idx_type i = 0; i < nc; i++)
    iperm(i) = i;  // identity permutation

  // leading permutations in forward order
  for (octave_idx_type i = 0; i < (ilo-1); i++)
    {
      octave_idx_type swapidx = static_cast<octave_idx_type> (dpermute(i)) - 1;
      octave_idx_type tmp = iperm(i);
      iperm(i) = iperm (swapidx);
      iperm(swapidx) = tmp;
    }

  // trailing permutations must be done in reverse order
  for (octave_idx_type i = nc - 1; i >= ihi; i--)
    {
      octave_idx_type swapidx = static_cast<octave_idx_type> (dpermute(i)) - 1;
      octave_idx_type tmp = iperm(i);
      iperm(i) = iperm(swapidx);
      iperm(swapidx) = tmp;
    }

  // construct inverse balancing permutation vector
  Array<octave_idx_type> invpvec (nc);
  for (octave_idx_type i = 0; i < nc; i++)
    invpvec(iperm(i)) = i;     // Thanks to R. A. Lippert for this method

  OCTAVE_QUIT;
 
  Matrix tmpMat = retval;
  for (octave_idx_type i = 0; i < nc; i++)
    for (octave_idx_type j = 0; j < nc; j++)
      retval(i,j) = tmpMat(invpvec(i),invpvec(j));

  // Reverse preconditioning step 1: fix trace normalization.
  
  if (trshift > 0.0)
    retval = exp (trshift) * retval;

  return retval;
}

Matrix&
Matrix::operator += (const DiagMatrix& a)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (nr != a_nr || nc != a_nc)
    {
      gripe_nonconformant ("operator +=", nr, nc, a_nr, a_nc);
      return *this;
    }

  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) += a.elem (i, i);

  return *this;
}

Matrix&
Matrix::operator -= (const DiagMatrix& a)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (nr != a_nr || nc != a_nc)
    {
      gripe_nonconformant ("operator -=", nr, nc, a_nr, a_nc);
      return *this;
    }

  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) -= a.elem (i, i);

  return *this;
}

// unary operations

boolMatrix
Matrix::operator ! (void) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  boolMatrix b (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      b.elem (i, j) = ! elem (i, j);

  return b;
}

// column vector by row vector -> matrix operations

Matrix
operator * (const ColumnVector& v, const RowVector& a)
{
  Matrix retval;

  octave_idx_type len = v.length ();

  if (len != 0)
    {
      octave_idx_type a_len = a.length ();

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
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  boolMatrix retval (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      retval(i,j) = f (elem(i,j));

  return retval;
}

Matrix&
Matrix::apply (d_d_Mapper f)
{
  double *d = fortran_vec (); // Ensures only one reference to my privates!

  for (octave_idx_type i = 0; i < length (); i++)
    d[i] = f (d[i]);

  return *this;
}

bool
Matrix::any_element_is_negative (bool neg_zero) const
{
  octave_idx_type nel = nelem ();

  if (neg_zero)
    {
      for (octave_idx_type i = 0; i < nel; i++)
	if (lo_ieee_signbit (elem (i)))
	  return true;
    }
  else
    {
      for (octave_idx_type i = 0; i < nel; i++)
	if (elem (i) < 0)
	  return true;
    }

  return false;
}


bool
Matrix::any_element_is_inf_or_nan (void) const
{
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      double val = elem (i);
      if (xisinf (val) || xisnan (val))
	return true;
    }

  return false;
}

bool
Matrix::any_element_not_one_or_zero (void) const
{
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      double val = elem (i);
      if (val != 0 && val != 1)
	return true;
    }

  return false;
}

bool
Matrix::all_elements_are_int_or_inf_or_nan (void) const
{
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
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
  octave_idx_type nel = nelem ();

  if (nel > 0)
    {
      max_val = elem (0);
      min_val = elem (0);
    }
  else
    return false;

  for (octave_idx_type i = 0; i < nel; i++)
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
  octave_idx_type nel = nelem ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      double val = elem (i);

      if (! (xisnan (val) || xisinf (val))
	  && fabs (val) > FLT_MAX)
	return true;
    }

  return false;
}

// FIXME Do these really belong here?  Maybe they should be
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
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  Matrix retval (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      retval (i, j) = fabs (elem (i, j));

  return retval;
}

ColumnVector
Matrix::diag (void) const
{
  return diag (0);
}

ColumnVector
Matrix::diag (octave_idx_type k) const
{
  octave_idx_type nnr = rows ();
  octave_idx_type nnc = cols ();
  if (k > 0)
    nnc -= k;
  else if (k < 0)
    nnr += k;

  ColumnVector d;

  if (nnr > 0 && nnc > 0)
    {
      octave_idx_type ndiag = (nnr < nnc) ? nnr : nnc;

      d.resize (ndiag);

      if (k > 0)
	{
	  for (octave_idx_type i = 0; i < ndiag; i++)
	    d.elem (i) = elem (i, i+k);
	}
      else if (k < 0)
	{
	  for (octave_idx_type i = 0; i < ndiag; i++)
	    d.elem (i) = elem (i-k, i);
	}
      else
	{
	  for (octave_idx_type i = 0; i < ndiag; i++)
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
  Array<octave_idx_type> dummy_idx;
  return row_min (dummy_idx);
}

ColumnVector
Matrix::row_min (Array<octave_idx_type>& idx_arg) const
{
  ColumnVector result;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nr);
      idx_arg.resize (nr);

      for (octave_idx_type i = 0; i < nr; i++)
        {
	  octave_idx_type idx_j;

	  double tmp_min = octave_NaN;

	  for (idx_j = 0; idx_j < nc; idx_j++)
	    {
	      tmp_min = elem (i, idx_j);

	      if (! xisnan (tmp_min))
		break;
	    }

	  for (octave_idx_type j = idx_j+1; j < nc; j++)
	    {
	      double tmp = elem (i, j);

	      if (xisnan (tmp))
		continue;
	      else if (tmp < tmp_min)
		{
		  idx_j = j;
		  tmp_min = tmp;
		}
	    }

	  result.elem (i) = tmp_min;
	  idx_arg.elem (i) = xisnan (tmp_min) ? 0 : idx_j;
        }
    }

  return result;
}

ColumnVector
Matrix::row_max (void) const
{
  Array<octave_idx_type> dummy_idx;
  return row_max (dummy_idx);
}

ColumnVector
Matrix::row_max (Array<octave_idx_type>& idx_arg) const
{
  ColumnVector result;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nr);
      idx_arg.resize (nr);

      for (octave_idx_type i = 0; i < nr; i++)
        {
	  octave_idx_type idx_j;

	  double tmp_max = octave_NaN;

	  for (idx_j = 0; idx_j < nc; idx_j++)
	    {
	      tmp_max = elem (i, idx_j);

	      if (! xisnan (tmp_max))
		break;
	    }

	  for (octave_idx_type j = idx_j+1; j < nc; j++)
	    {
	      double tmp = elem (i, j);

	      if (xisnan (tmp))
		continue;
	      else if (tmp > tmp_max)
		{
		  idx_j = j;
		  tmp_max = tmp;
		}
	    }

	  result.elem (i) = tmp_max;
	  idx_arg.elem (i) = xisnan (tmp_max) ? 0 : idx_j;
        }
    }

  return result;
}

RowVector
Matrix::column_min (void) const
{
  Array<octave_idx_type> dummy_idx;
  return column_min (dummy_idx);
}

RowVector
Matrix::column_min (Array<octave_idx_type>& idx_arg) const
{
  RowVector result;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nc);
      idx_arg.resize (nc);

      for (octave_idx_type j = 0; j < nc; j++)
        {
	  octave_idx_type idx_i;

	  double tmp_min = octave_NaN;

	  for (idx_i = 0; idx_i < nr; idx_i++)
	    {
	      tmp_min = elem (idx_i, j);

	      if (! xisnan (tmp_min))
		break;
	    }

	  for (octave_idx_type i = idx_i+1; i < nr; i++)
	    {
	      double tmp = elem (i, j);

	      if (xisnan (tmp))
		continue;
	      else if (tmp < tmp_min)
		{
		  idx_i = i;
		  tmp_min = tmp;
		}
	    }

	  result.elem (j) = tmp_min;
	  idx_arg.elem (j) = xisnan (tmp_min) ? 0 : idx_i;
        }
    }

  return result;
}

RowVector
Matrix::column_max (void) const
{
  Array<octave_idx_type> dummy_idx;
  return column_max (dummy_idx);
}

RowVector
Matrix::column_max (Array<octave_idx_type>& idx_arg) const
{
  RowVector result;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nc);
      idx_arg.resize (nc);

      for (octave_idx_type j = 0; j < nc; j++)
        {
	  octave_idx_type idx_i;

	  double tmp_max = octave_NaN;

	  for (idx_i = 0; idx_i < nr; idx_i++)
	    {
	      tmp_max = elem (idx_i, j);

	      if (! xisnan (tmp_max))
		break;
	    }

	  for (octave_idx_type i = idx_i+1; i < nr; i++)
	    {
	      double tmp = elem (i, j);

	      if (xisnan (tmp))
		continue;
	      else if (tmp > tmp_max)
		{
		  idx_i = i;
		  tmp_max = tmp;
		}
	    }

	  result.elem (j) = tmp_max;
	  idx_arg.elem (j) = xisnan (tmp_max) ? 0 : idx_i;
        }
    }

  return result;
}

std::ostream&
operator << (std::ostream& os, const Matrix& a)
{
  for (octave_idx_type i = 0; i < a.rows (); i++)
    {
      for (octave_idx_type j = 0; j < a.cols (); j++)
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
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr < 1 || nc < 1)
    is.clear (std::ios::badbit);
  else
    {
      double tmp;
      for (octave_idx_type i = 0; i < nr; i++)
	for (octave_idx_type j = 0; j < nc; j++)
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

  // FIXME -- need to check that a, b, and c are all the same
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

  octave_idx_type a_nr = a.rows ();
  octave_idx_type b_nr = b.rows ();

  double scale;
  octave_idx_type info;

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
      // FIXME -- check info?
  
      retval = -ua*cx*ub.transpose ();
    }

  return retval;
}

// matrix by matrix -> matrix operations

Matrix
operator * (const Matrix& m, const Matrix& a)
{
  Matrix retval;

  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.cols ();

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (nc != a_nr)
    gripe_nonconformant ("operator *", nr, nc, a_nr, a_nc);
  else
    {
      if (nr == 0 || nc == 0 || a_nc == 0)
	retval.resize (nr, a_nc, 0.0);
      else
	{
	  octave_idx_type ld  = nr;
	  octave_idx_type lda = a_nr;

	  retval.resize (nr, a_nc);
	  double *c = retval.fortran_vec ();

	  if (a_nc == 1)
	    {
	      if (nr == 1)
		F77_FUNC (xddot, XDDOT) (nc, m.data (), 1, a.data (), 1, *c);
	      else
		F77_XFCN (dgemv, DGEMV, (F77_CONST_CHAR_ARG2 ("N", 1),
					 nr, nc, 1.0,  m.data (), ld,
					 a.data (), 1, 0.0, c, 1
					 F77_CHAR_ARG_LEN (1)));
            }
	  else
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

// FIXME -- it would be nice to share code among the min/max
// functions below.

#define EMPTY_RETURN_CHECK(T) \
  if (nr == 0 || nc == 0) \
    return T (nr, nc);

Matrix
min (double d, const Matrix& m)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  EMPTY_RETURN_CHECK (Matrix);

  Matrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
	OCTAVE_QUIT;
	result (i, j) = xmin (d, m (i, j));
      }

  return result;
}

Matrix
min (const Matrix& m, double d)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  EMPTY_RETURN_CHECK (Matrix);

  Matrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
	OCTAVE_QUIT;
	result (i, j) = xmin (m (i, j), d);
      }

  return result;
}

Matrix
min (const Matrix& a, const Matrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.columns ();

  if (nr != b.rows () || nc != b.columns ())
    {
      (*current_liboctave_error_handler)
	("two-arg min expecting args of same size");
      return Matrix ();
    }

  EMPTY_RETURN_CHECK (Matrix);

  Matrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
	OCTAVE_QUIT;
	result (i, j) = xmin (a (i, j), b (i, j));
      }

  return result;
}

Matrix
max (double d, const Matrix& m)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  EMPTY_RETURN_CHECK (Matrix);

  Matrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
	OCTAVE_QUIT;
	result (i, j) = xmax (d, m (i, j));
      }

  return result;
}

Matrix
max (const Matrix& m, double d)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  EMPTY_RETURN_CHECK (Matrix);

  Matrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
	OCTAVE_QUIT;
	result (i, j) = xmax (m (i, j), d);
      }

  return result;
}

Matrix
max (const Matrix& a, const Matrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.columns ();

  if (nr != b.rows () || nc != b.columns ())
    {
      (*current_liboctave_error_handler)
	("two-arg max expecting args of same size");
      return Matrix ();
    }

  EMPTY_RETURN_CHECK (Matrix);

  Matrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
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
