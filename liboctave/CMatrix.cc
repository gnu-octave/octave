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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>

#include <iostream>

// XXX FIXME XXX
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#include "CMatrix.h"
#include "CmplxAEPBAL.h"
#include "CmplxDET.h"
#include "CmplxSCHUR.h"
#include "CmplxSVD.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "lo-utils.h"
#include "mx-base.h"
#include "mx-cm-dm.h"
#include "mx-dm-cm.h"
#include "mx-cm-s.h"
#include "mx-inlines.cc"
#include "oct-cmplx.h"

// Fortran functions we call.

extern "C"
{
  int F77_FCN (zgebal, ZGEBAL) (const char*, const int&, Complex*,
                                const int&, int&, int&, double*, int&,
                                long, long);

  int F77_FCN (dgebak, DGEBAK) (const char*, const char*, const int&,
                                const int&, const int&, double*,
                                const int&, double*, const int&,
                                int&, long, long);

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

  int F77_FCN (zlartg, ZLARTG) (const Complex&, const Complex&,
				double&, Complex&, Complex&);

  int F77_FCN (ztrsyl, ZTRSYL) (const char*, const char*, const int&,
				const int&, const int&,
				const Complex*, const int&,
				const Complex*, const int&, 
				const Complex*, const int&, double&,
				int&, long, long);

  int F77_FCN (xzlange, XZLANGE) (const char*, const int&,
				  const int&, const Complex*,
				  const int&, double*, double&); 
}

static const Complex Complex_NaN_result (octave_NaN, octave_NaN);

// Complex Matrix class

ComplexMatrix::ComplexMatrix (const Matrix& a)
  : MArray2<Complex> (a.rows (), a.cols ())
{
  for (int j = 0; j < cols (); j++)
    for (int i = 0; i < rows (); i++)
      elem (i, j) = a.elem (i, j);
}

ComplexMatrix::ComplexMatrix (const RowVector& rv)
  : MArray2<Complex> (1, rv.length (), 0.0)
{
  for (int i = 0; i < rv.length (); i++)
    elem (0, i) = rv.elem (i);
}

ComplexMatrix::ComplexMatrix (const ColumnVector& cv)
  : MArray2<Complex> (cv.length (), 1, 0.0)
{
  for (int i = 0; i < cv.length (); i++)
    elem (i, 0) = cv.elem (i);
}

ComplexMatrix::ComplexMatrix (const DiagMatrix& a)
  : MArray2<Complex> (a.rows (), a.cols (), 0.0)
{
  for (int i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

ComplexMatrix::ComplexMatrix (const ComplexRowVector& rv)
  : MArray2<Complex> (1, rv.length (), 0.0)
{
  for (int i = 0; i < rv.length (); i++)
    elem (0, i) = rv.elem (i);
}

ComplexMatrix::ComplexMatrix (const ComplexColumnVector& cv)
  : MArray2<Complex> (cv.length (), 1, 0.0)
{
  for (int i = 0; i < cv.length (); i++)
    elem (i, 0) = cv.elem (i);
}

ComplexMatrix::ComplexMatrix (const ComplexDiagMatrix& a)
  : MArray2<Complex> (a.rows (), a.cols (), 0.0)
{
  for (int i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

// XXX FIXME XXX -- could we use a templated mixed-type copy function
// here?

ComplexMatrix::ComplexMatrix (const boolMatrix& a)
  : MArray2<Complex> (a.rows (), a.cols (), 0.0)
{
  for (int i = 0; i < a.cols (); i++)
    for (int j = 0; j < a.rows (); j++)
      elem (i, j) = a.elem (i, j);
}

ComplexMatrix::ComplexMatrix (const charMatrix& a)
  : MArray2<Complex> (a.rows (), a.cols (), 0.0)
{
  for (int i = 0; i < a.cols (); i++)
    for (int j = 0; j < a.rows (); j++)
      elem (i, j) = a.elem (i, j);
}

bool
ComplexMatrix::operator == (const ComplexMatrix& a) const
{
  if (rows () != a.rows () || cols () != a.cols ())
    return false;

  return equal (data (), a.data (), length ());
}

bool
ComplexMatrix::operator != (const ComplexMatrix& a) const
{
  return !(*this == a);
}

bool
ComplexMatrix::is_hermitian (void) const
{
  int nr = rows ();
  int nc = cols ();

  if (is_square () && nr > 0)
    {
      for (int i = 0; i < nr; i++)
	for (int j = i; j < nc; j++)
	  if (elem (i, j) != conj (elem (j, i)))
	    return false;

      return true;
    }

  return false;
}

// destructive insert/delete/reorder operations

ComplexMatrix&
ComplexMatrix::insert (const Matrix& a, int r, int c)
{
  int a_nr = a.rows ();
  int a_nc = a.cols ();

  if (r < 0 || r + a_nr > rows () || c < 0 || c + a_nc > cols ())
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
  if (r < 0 || r >= rows () || c < 0 || c + a_len > cols ())
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
  if (r < 0 || r + a_len > rows () || c < 0 || c >= cols ())
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
  int a_nr = a.rows ();
  int a_nc = a.cols ();

  if (r < 0 || r + a_nr > rows () || c < 0 || c + a_nc > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  fill (0.0, r, c, r + a_nr - 1, c + a_nc - 1);

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
  if (r < 0 || r >= rows () || c < 0 || c + a_len > cols ())
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
  if (r < 0 || r + a_len > rows () || c < 0 || c >= cols ())
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
  int a_nr = a.rows ();
  int a_nc = a.cols ();

  if (r < 0 || r + a_nr > rows () || c < 0 || c + a_nc > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  fill (0.0, r, c, r + a_nr - 1, c + a_nc - 1);

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
  ComplexMatrix retval;

  int nr = rows ();
  int nc = cols ();

  if (nr != nc)
    (*current_liboctave_error_handler) ("inverse requires square matrix");
  else
    {
      info = 0;

      Array<int> ipvt (nr);
      int *pipvt = ipvt.fortran_vec ();

      Array<Complex> z (nr);
      Complex *pz = z.fortran_vec ();

      retval = *this;
      Complex *tmp_data = retval.fortran_vec ();

      F77_XFCN (zgeco, ZGECO, (tmp_data, nr, nc, pipvt, rcond, pz));

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in zgeco");
      else
	{
	  volatile double rcond_plus_one = rcond + 1.0;

	  if (rcond_plus_one == 1.0)
	    info = -1;

	  if (info == -1 && ! force)
	    retval = *this;  // Restore contents.
	  else
	    {
	      Complex *dummy = 0;

	      F77_XFCN (zgedi, ZGEDI, (tmp_data, nr, nc, pipvt, dummy,
				       pz, 1));

	      if (f77_exception_encountered)
		(*current_liboctave_error_handler)
		  ("unrecoverable error in zgedi");
	    }
	}
    }

  return retval;
}

ComplexMatrix
ComplexMatrix::pseudo_inverse (double tol)
{
  ComplexMatrix retval;

  ComplexSVD result (*this, SVD::economy);

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

  retval = *this;
  Complex *tmp_data = retval.fortran_vec ();

  F77_FCN (cffti, CFFTI) (npts, pwsave);

  for (int j = 0; j < nsamples; j++)
    F77_FCN (cfftf, CFFTF) (npts, &tmp_data[npts*j], pwsave);

  return retval;
}

ComplexMatrix
ComplexMatrix::ifourier (void) const
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

  retval = *this;
  Complex *tmp_data = retval.fortran_vec ();

  F77_FCN (cffti, CFFTI) (npts, pwsave);

  for (int j = 0; j < nsamples; j++)
    F77_FCN (cfftb, CFFTB) (npts, &tmp_data[npts*j], pwsave);

  for (int j = 0; j < npts*nsamples; j++)
    tmp_data[j] = tmp_data[j] / static_cast<double> (npts);

  return retval;
}

ComplexMatrix
ComplexMatrix::fourier2d (void) const
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

  retval = *this;
  Complex *tmp_data = retval.fortran_vec ();

  F77_FCN (cffti, CFFTI) (npts, pwsave);

  for (int j = 0; j < nsamples; j++)
    F77_FCN (cfftf, CFFTF) (npts, &tmp_data[npts*j], pwsave);

  npts = nc;
  nsamples = nr;
  nn = 4*npts+15;

  wsave.resize (nn);
  pwsave = wsave.fortran_vec ();

  Array<Complex> row (npts);
  Complex *prow = row.fortran_vec ();

  F77_FCN (cffti, CFFTI) (npts, pwsave);

  for (int j = 0; j < nsamples; j++)
    {
      for (int i = 0; i < npts; i++)
	prow[i] = tmp_data[i*nr + j];

      F77_FCN (cfftf, CFFTF) (npts, prow, pwsave);

      for (int i = 0; i < npts; i++)
	tmp_data[i*nr + j] = prow[i];
    }

  return retval;
}

ComplexMatrix
ComplexMatrix::ifourier2d (void) const
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

  retval = *this;
  Complex *tmp_data = retval.fortran_vec ();

  F77_FCN (cffti, CFFTI) (npts, pwsave);

  for (int j = 0; j < nsamples; j++)
    F77_FCN (cfftb, CFFTB) (npts, &tmp_data[npts*j], pwsave);

  for (int j = 0; j < npts*nsamples; j++)
    tmp_data[j] = tmp_data[j] / static_cast<double> (npts);

  npts = nc;
  nsamples = nr;
  nn = 4*npts+15;

  wsave.resize (nn);
  pwsave = wsave.fortran_vec ();

  Array<Complex> row (npts);
  Complex *prow = row.fortran_vec ();

  F77_FCN (cffti, CFFTI) (npts, pwsave);

  for (int j = 0; j < nsamples; j++)
    {
      for (int i = 0; i < npts; i++)
	prow[i] = tmp_data[i*nr + j];

      F77_FCN (cfftb, CFFTB) (npts, prow, pwsave);

      for (int i = 0; i < npts; i++)
	tmp_data[i*nr + j] = prow[i] / static_cast<double> (npts);
    }

  return retval;
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

      Array<int> ipvt (nr);
      int *pipvt = ipvt.fortran_vec ();

      Array<Complex> z (nr);
      Complex *pz = z.fortran_vec ();

      ComplexMatrix atmp = *this;
      Complex *tmp_data = atmp.fortran_vec ();

      F77_XFCN (zgeco, ZGECO, (tmp_data, nr, nr, pipvt, rcond, pz));

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in zgeco");
      else
	{
	  volatile double rcond_plus_one = rcond + 1.0;

	  if (rcond_plus_one == 1.0)
	    {
	      info = -1;
	      retval = ComplexDET ();
	    }
	  else
	    {
	      Complex d[2];

	      F77_XFCN (zgedi, ZGEDI, (tmp_data, nr, nr, pipvt, d, pz, 10));

	      if (f77_exception_encountered)
		(*current_liboctave_error_handler)
		  ("unrecoverable error in dgedi");
	      else
		retval = ComplexDET (d);
	    }
	}
    }

  return retval;
}

ComplexMatrix
ComplexMatrix::solve (const Matrix& b) const
{
  int info;
  double rcond;
  return solve (b, info, rcond, 0);
}

ComplexMatrix
ComplexMatrix::solve (const Matrix& b, int& info) const
{
  double rcond;
  return solve (b, info, rcond, 0);
}

ComplexMatrix
ComplexMatrix::solve (const Matrix& b, int& info, double& rcond) const
{
  return solve (b, info, rcond, 0);
}

ComplexMatrix
ComplexMatrix::solve (const Matrix& b, int& info, double& rcond,
		      solve_singularity_handler sing_handler) const
{
  ComplexMatrix tmp (b);
  return solve (tmp, info, rcond, sing_handler);
}

ComplexMatrix
ComplexMatrix::solve (const ComplexMatrix& b) const
{
  int info;
  double rcond;
  return solve (b, info, rcond, 0);
}

ComplexMatrix
ComplexMatrix::solve (const ComplexMatrix& b, int& info) const
{
  double rcond;
  return solve (b, info, rcond, 0);
}

ComplexMatrix
ComplexMatrix::solve (const ComplexMatrix& b, int& info, double& rcond) const
{
  return solve (b, info, rcond, 0);
}

ComplexMatrix
ComplexMatrix::solve (const ComplexMatrix& b, int& info, double& rcond,
		      solve_singularity_handler sing_handler) const
{
  ComplexMatrix retval;

  int nr = rows ();
  int nc = cols ();

  if (nr == 0 || nc == 0 || nr != nc || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch in solution of linear equations");
  else
    {
      info = 0;

      Array<int> ipvt (nr);
      int *pipvt = ipvt.fortran_vec ();

      Array<Complex> z (nr);
      Complex *pz = z.fortran_vec ();

      ComplexMatrix atmp = *this;
      Complex *tmp_data = atmp.fortran_vec ();

      F77_XFCN (zgeco, ZGECO, (tmp_data, nr, nr, pipvt, rcond, pz));

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in zgeco");
      else
	{
	  volatile double rcond_plus_one = rcond + 1.0;

	  if (rcond_plus_one == 1.0)
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
	      Complex *result = retval.fortran_vec ();

	      int b_nc = b.cols ();

	      for (volatile int j = 0; j < b_nc; j++)
		{
		  F77_XFCN (zgesl, ZGESL, (tmp_data, nr, nr, pipvt,
					   &result[nr*j], 0));

		  if (f77_exception_encountered)
		    {
		      (*current_liboctave_error_handler)
			("unrecoverable error in dgesl");

		      break;
		    }
		}
	    }
	}
    }

  return retval;
}

ComplexColumnVector
ComplexMatrix::solve (const ColumnVector& b) const
{
  int info;
  double rcond;
  return solve (ComplexColumnVector (b), info, rcond, 0);
}

ComplexColumnVector
ComplexMatrix::solve (const ColumnVector& b, int& info) const
{
  double rcond;
  return solve (ComplexColumnVector (b), info, rcond, 0);
}

ComplexColumnVector
ComplexMatrix::solve (const ColumnVector& b, int& info, double& rcond) const
{
  return solve (ComplexColumnVector (b), info, rcond, 0);
}

ComplexColumnVector
ComplexMatrix::solve (const ColumnVector& b, int& info, double& rcond,
		      solve_singularity_handler sing_handler) const
{
  return solve (ComplexColumnVector (b), info, rcond, sing_handler);
}

ComplexColumnVector
ComplexMatrix::solve (const ComplexColumnVector& b) const
{
  int info;
  double rcond;
  return solve (b, info, rcond, 0);
}

ComplexColumnVector
ComplexMatrix::solve (const ComplexColumnVector& b, int& info) const
{
  double rcond;
  return solve (b, info, rcond, 0);
}

ComplexColumnVector
ComplexMatrix::solve (const ComplexColumnVector& b, int& info,
		      double& rcond) const
{
  return solve (b, info, rcond, 0);
}

ComplexColumnVector
ComplexMatrix::solve (const ComplexColumnVector& b, int& info,
		      double& rcond,
		      solve_singularity_handler sing_handler) const
{
  ComplexColumnVector retval;

  int nr = rows ();
  int nc = cols ();

  if (nr == 0 || nc == 0 || nr != nc || nr != b.length ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch in solution of linear equations");
  else
    {
      info = 0;

      Array<int> ipvt (nr);
      int *pipvt = ipvt.fortran_vec ();

      Array<Complex> z (nr);
      Complex *pz = z.fortran_vec ();

      ComplexMatrix atmp = *this;
      Complex *tmp_data = atmp.fortran_vec ();

      F77_XFCN (zgeco, ZGECO, (tmp_data, nr, nr, pipvt, rcond, pz));

      if (f77_exception_encountered)
	(*current_liboctave_error_handler)
	  ("unrecoverable error in dgeco");
      else
	{
	  volatile double rcond_plus_one = rcond + 1.0;

	  if (rcond_plus_one == 1.0)
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
	      Complex *result = retval.fortran_vec ();

	      F77_XFCN (zgesl, ZGESL, (tmp_data, nr, nr, pipvt, result, 0));

	      if (f77_exception_encountered)
		(*current_liboctave_error_handler)
		  ("unrecoverable error in dgesl");
	    }
	}
    }

  return retval;
}

ComplexMatrix
ComplexMatrix::lssolve (const Matrix& b) const
{
  int info;
  int rank;
  return lssolve (ComplexMatrix (b), info, rank);
}

ComplexMatrix
ComplexMatrix::lssolve (const Matrix& b, int& info) const
{
  int rank;
  return lssolve (ComplexMatrix (b), info, rank);
}

ComplexMatrix
ComplexMatrix::lssolve (const Matrix& b, int& info, int& rank) const
{
  return lssolve (ComplexMatrix (b), info, rank);
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
  ComplexMatrix retval;

  int nrhs = b.cols ();

  int m = rows ();
  int n = cols ();

  if (m == 0 || n == 0 || m != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else
    {
      ComplexMatrix atmp = *this;
      Complex *tmp_data = atmp.fortran_vec ();

      int nrr = m > n ? m : n;
      ComplexMatrix result (nrr, nrhs);

      for (int j = 0; j < nrhs; j++)
	for (int i = 0; i < m; i++)
	  result.elem (i, j) = b.elem (i, j);

      Complex *presult = result.fortran_vec ();

      int len_s = m < n ? m : n;
      Array<double> s (len_s);
      double *ps = s.fortran_vec ();

      double rcond = -1.0;

      int lrwork = (5 * (m < n ? m : n)) - 4;
      lrwork = lrwork > 1 ? lrwork : 1;
      Array<double> rwork (lrwork);
      double *prwork = rwork.fortran_vec ();

      // Ask ZGELSS what the dimension of WORK should be.

      int lwork = -1;

      Array<Complex> work (1);

      F77_XFCN (zgelss, ZGELSS, (m, n, nrhs, tmp_data, m, presult,
				 nrr, ps, rcond, rank,
				 work.fortran_vec (), lwork, prwork,
				 info));

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in zgelss");
      else
	{
	  lwork = static_cast<int> (real (work(0)));
	  work.resize (lwork);

	  F77_XFCN (zgelss, ZGELSS, (m, n, nrhs, tmp_data, m, presult,
				     nrr, ps, rcond, rank,
				     work.fortran_vec (), lwork,
				     prwork, info));

	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler)
	      ("unrecoverable error in zgelss");
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

ComplexColumnVector
ComplexMatrix::lssolve (const ColumnVector& b) const
{
  int info;
  int rank;
  return lssolve (ComplexColumnVector (b), info, rank);
}

ComplexColumnVector
ComplexMatrix::lssolve (const ColumnVector& b, int& info) const
{
  int rank;
  return lssolve (ComplexColumnVector (b), info, rank);
}

ComplexColumnVector
ComplexMatrix::lssolve (const ColumnVector& b, int& info, int& rank) const
{
  return lssolve (ComplexColumnVector (b), info, rank);
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
  ComplexColumnVector retval;

  int nrhs = 1;

  int m = rows ();
  int n = cols ();

  if (m == 0 || n == 0 || m != b.length ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of least squares problem");
  else
    {
      ComplexMatrix atmp = *this;
      Complex *tmp_data = atmp.fortran_vec ();

      int nrr = m > n ? m : n;
      ComplexColumnVector result (nrr);

      for (int i = 0; i < m; i++)
	result.elem (i) = b.elem (i);

      Complex *presult = result.fortran_vec ();

      int len_s = m < n ? m : n;
      Array<double> s (len_s);
      double *ps = s.fortran_vec ();

      double rcond = -1.0;

      int lrwork = (5 * (m < n ? m : n)) - 4;
      lrwork = lrwork > 1 ? lrwork : 1;
      Array<double> rwork (lrwork);
      double *prwork = rwork.fortran_vec ();

      // Ask ZGELSS what the dimension of WORK should be.

      int lwork = -1;

      Array<Complex> work (1);

      F77_XFCN (zgelss, ZGELSS, (m, n, nrhs, tmp_data, m, presult,
				 nrr, ps, rcond, rank,
				 work.fortran_vec (), lwork, prwork,
				 info));

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in zgelss");
      else
	{
	  lwork = static_cast<int> (real (work(0)));
	  work.resize (lwork);

	  F77_XFCN (zgelss, ZGELSS, (m, n, nrhs, tmp_data, m, presult,
				     nrr, ps, rcond, rank,
				     work.fortran_vec (), lwork,
				     prwork, info));

	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler)
	      ("unrecoverable error in zgelss");
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

ComplexMatrix
ComplexMatrix::expm (void) const
{
  ComplexMatrix retval;

  ComplexMatrix m = *this;

  int nc = columns ();

  // Preconditioning step 1: trace normalization to reduce dynamic
  // range of poles, but avoid making stable eigenvalues unstable.

  // trace shift value
  Complex trshift = 0.0;

  for (int i = 0; i < nc; i++)
    trshift += m.elem (i, i);

  trshift /= nc;

  if (trshift.real () < 0.0)
    trshift = trshift.imag ();

  for (int i = 0; i < nc; i++)
    m.elem (i, i) -= trshift;

  // Preconditioning step 2: eigenvalue balancing.
  // code follows development in AEPBAL

  Complex *mp = m.fortran_vec ();

  int info, ilo, ihi,ilos,ihis;
  Array<double> dpermute (nc);
  Array<double> dscale (nc);

  // XXX FIXME XXX -- should pass job as a parameter in expm

  // Permute first
  char job = 'P';
  F77_XFCN (zgebal, ZGEBAL, (&job, nc, mp, nc, ilo, ihi,
            dpermute.fortran_vec (), info, 1L, 1L));

  if (f77_exception_encountered)
    {
      (*current_liboctave_error_handler) ("unrecoverable error in zgebal");
      return retval;
    }

  // then scale
  job = 'S';
  F77_XFCN (zgebal, ZGEBAL, (&job, nc, mp, nc, ilos, ihis,
            dscale.fortran_vec (), info, 1L, 1L));

  if (f77_exception_encountered)
    {
      (*current_liboctave_error_handler) ("unrecoverable error in zgebal");
      return retval;
    }

  // Preconditioning step 3: scaling.

  ColumnVector work (nc);
  double inf_norm;

  F77_XFCN (xzlange, XZLANGE, ("I", nc, nc, m.fortran_vec (), nc,
			       work.fortran_vec (), inf_norm));

  if (f77_exception_encountered)
    {
      (*current_liboctave_error_handler) ("unrecoverable error in zlange");
      return retval;
    }

  int sqpow = (inf_norm > 0.0
	       ? static_cast<int> (1.0 + log (inf_norm) / log (2.0)) : 0);

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

  ComplexMatrix npp (nc, nc, 0.0);
  ComplexMatrix dpp = npp;

  // Now powers a^8 ... a^1.

  int minus_one_j = -1;
  for (int j = 7; j >= 0; j--)
    {
      npp = m * npp + m * padec[j];
      dpp = m * dpp + m * (minus_one_j * padec[j]);
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

  retval = dpp.solve (npp);
	
  // Reverse preconditioning step 3: repeated squaring.

  while (sqpow)
    {
      retval = retval * retval;
      sqpow--;
    }

  // Reverse preconditioning step 2: inverse balancing.
  // Done in two steps: inverse scaling, then inverse permutation

  // inverse scaling (diagonal transformation)
  for (int i = 0; i < nc; i++)
    for (int j = 0; j < nc; j++)
       retval(i,j) *= dscale(i) / dscale(j);

  // construct balancing permutation vector
  Array<int> ipermute (nc);
  for (int i = 0; i < nc; i++)
    ipermute(i) = i;  // initialize to identity permutation

  // leading permutations in forward order
  for (int i = 0; i < (ilo-1); i++)
    {
      int swapidx = static_cast<int> (dpermute(i)) - 1;
      int tmp = ipermute(i);
      ipermute(i) = ipermute(swapidx);
      ipermute(swapidx) = tmp;
    }

  // trailing permutations must be done in reverse order
  for (int i = nc - 1; i >= ihi; i--)
    {
      int swapidx = static_cast<int> (dpermute(i)) - 1;
      int tmp = ipermute(i);
      ipermute(i) = ipermute(swapidx);
      ipermute(swapidx) = tmp;
    }

  // construct inverse balancing permutation vector
  Array<int> invpvec (nc);
  for (int i = 0; i < nc; i++)
    invpvec(ipermute(i)) = i;     // Thanks to R. A. Lippert for this method

  ComplexMatrix tmpMat = retval;
  for (int i = 0; i < nc; i++)
    for (int j = 0; j < nc; j++)
      retval(i,j) = tmpMat(invpvec(i),invpvec(j));

  // Reverse preconditioning step 1: fix trace normalization.

  return exp (trshift) * retval;
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
  ComplexMatrix retval;

  int len = v.length ();

  if (len != 0)
    {
      int a_len = a.length ();

      retval.resize (len, a_len);
      Complex *c = retval.fortran_vec ();

      F77_XFCN (zgemm, ZGEMM, ("N", "N", len, a_len, 1, 1.0,
			       v.data (), len, a.data (), 1, 0.0,
			       c, len, 1L, 1L)); 

      if (f77_exception_encountered)
	(*current_liboctave_error_handler)
	  ("unrecoverable error in zgemm");
    }

  return retval;
}

// matrix by diagonal matrix -> matrix operations

ComplexMatrix&
ComplexMatrix::operator += (const DiagMatrix& a)
{
  int nr = rows ();
  int nc = cols ();

  int a_nr = rows ();
  int a_nc = cols ();

  if (nr != a_nr || nc != a_nc)
    {
      gripe_nonconformant ("operator +=", nr, nc, a_nr, a_nc);
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

  int a_nr = rows ();
  int a_nc = cols ();

  if (nr != a_nr || nc != a_nc)
    {
      gripe_nonconformant ("operator -=", nr, nc, a_nr, a_nc);
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

  int a_nr = rows ();
  int a_nc = cols ();

  if (nr != a_nr || nc != a_nc)
    {
      gripe_nonconformant ("operator +=", nr, nc, a_nr, a_nc);
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

  int a_nr = rows ();
  int a_nc = cols ();

  if (nr != a_nr || nc != a_nc)
    {
      gripe_nonconformant ("operator -=", nr, nc, a_nr, a_nc);
      return *this;
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

  int a_nr = a.rows ();
  int a_nc = a.cols ();

  if (nr != a_nr || nc != a_nc)
    {
      gripe_nonconformant ("operator +=", nr, nc, a_nr, a_nc);
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

  int a_nr = a.rows ();
  int a_nc = a.cols ();

  if (nr != a_nr || nc != a_nc)
    {
      gripe_nonconformant ("operator -=", nr, nc, a_nr, a_nc);
      return *this;
    }

  if (nr == 0 || nc == 0)
    return *this;

  Complex *d = fortran_vec (); // Ensures only one reference to my privates!

  subtract2 (d, a.data (), length ());
  return *this;
}

// unary operations

boolMatrix
ComplexMatrix::operator ! (void) const
{
  int nr = rows ();
  int nc = cols ();

  boolMatrix b (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      b.elem (i, j) = elem (i, j) != 0.0;

  return b;
}

// other operations

ComplexMatrix
ComplexMatrix::map (c_c_Mapper f) const
{
  ComplexMatrix b (*this);
  return b.apply (f);
}

Matrix
ComplexMatrix::map (d_c_Mapper f) const
{
  int nr = rows ();
  int nc = cols ();

  Matrix retval (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      retval(i,j) = f (elem(i,j));

  return retval;
}

boolMatrix
ComplexMatrix::map (b_c_Mapper f) const
{
  int nr = rows ();
  int nc = cols ();

  boolMatrix retval (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      retval(i,j) = f (elem(i,j));

  return retval;
}

ComplexMatrix&
ComplexMatrix::apply (c_c_Mapper f)
{
  Complex *d = fortran_vec (); // Ensures only one reference to my privates!

  for (int i = 0; i < length (); i++)
    d[i] = f (d[i]);

  return *this;
}

bool
ComplexMatrix::any_element_is_inf_or_nan (void) const
{
  int nr = rows ();
  int nc = cols ();

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	Complex val = elem (i, j);
	if (xisinf (val) || xisnan (val))
	  return true;
      }

  return false;
}

// Return true if no elements have imaginary components.

bool
ComplexMatrix::all_elements_are_real (void) const
{
  int nr = rows ();
  int nc = cols ();

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      if (imag (elem (i, j)) != 0.0)
	return false;

  return true;
}

// Return nonzero if any element of CM has a non-integer real or
// imaginary part.  Also extract the largest and smallest (real or
// imaginary) values and return them in MAX_VAL and MIN_VAL. 

bool
ComplexMatrix::all_integers (double& max_val, double& min_val) const
{
  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      Complex val = elem (0, 0);

      double r_val = real (val);
      double i_val = imag (val);

      max_val = r_val;
      min_val = r_val;

      if (i_val > max_val)
	max_val = i_val;

      if (i_val < max_val)
	min_val = i_val;
    }
  else
    return false;

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	Complex val = elem (i, j);

	double r_val = real (val);
	double i_val = imag (val);

	if (r_val > max_val)
	  max_val = r_val;

	if (i_val > max_val)
	  max_val = i_val;

	if (r_val < min_val)
	  min_val = r_val;

	if (i_val < min_val)
	  min_val = i_val;

	if (D_NINT (r_val) != r_val || D_NINT (i_val) != i_val)
	  return false;
      }

  return true;
}

bool
ComplexMatrix::too_large_for_float (void) const
{
  int nr = rows ();
  int nc = cols ();

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	Complex val = elem (i, j);

	double r_val = real (val);
	double i_val = imag (val);

	if (r_val > FLT_MAX
	    || i_val > FLT_MAX
	    || r_val < FLT_MIN
	    || i_val < FLT_MIN)
	  return true;
      }

  return false;
}

boolMatrix
ComplexMatrix::all (void) const
{
  int nr = rows ();
  int nc = cols ();
  boolMatrix retval;
  if (nr > 0 && nc > 0)
    {
      if (nr == 1)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = true;
	  for (int j = 0; j < nc; j++)
	    {
	      if (elem (0, j) == 0.0)
		{
		  retval.elem (0, 0) = false;
		  break;
		}
	    }
	}
      else if (nc == 1)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = true;
	  for (int i = 0; i < nr; i++)
	    {
	      if (elem (i, 0) == 0.0)
		{
		  retval.elem (0, 0) = false;
		  break;
		}
	    }
	}
      else
	{
	  retval.resize (1, nc);
	  for (int j = 0; j < nc; j++)
	    {
	      retval.elem (0, j) = true;
	      for (int i = 0; i < nr; i++)
		{
		  if (elem (i, j) == 0.0)
		    {
		      retval.elem (0, j) = false;
		      break;
		    }
		}
	    }
	}
    }
  return retval;
}

boolMatrix
ComplexMatrix::any (void) const
{
  int nr = rows ();
  int nc = cols ();
  boolMatrix retval;
  if (nr > 0 && nc > 0)
    {
      if (nr == 1)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = false;
	  for (int j = 0; j < nc; j++)
	    {
	      if (elem (0, j) != 0.0)
		{
		  retval.elem (0, 0) = true;
		  break;
		}
	    }
	}
      else if (nc == 1)
	{
	  retval.resize (1, 1);
	  retval.elem (0, 0) = false;
	  for (int i = 0; i < nr; i++)
	    {
	      if (elem (i, 0) != 0.0)
		{
		  retval.elem (0, 0) = true;
		  break;
		}
	    }
	}
      else
	{
	  retval.resize (1, nc);
	  for (int j = 0; j < nc; j++)
	    {
	      retval.elem (0, j) = false;
	      for (int i = 0; i < nr; i++)
		{
		  if (elem (i, j) != 0.0)
		    {
		      retval.elem (0, j) = true;
		      break;
		    }
		}
	    }
	}
    }
  return retval;
}

ComplexMatrix
ComplexMatrix::cumprod (int dim) const
{
  int nr = rows ();
  int nc = cols ();
  ComplexMatrix retval (nr, nc);
  if (nr > 0 && nc > 0)
    {
      if ((nr == 1 && dim == 0) || dim == 1)
	{
	  for (int i = 0; i < nr; i++)
	    {
	      Complex prod = elem (i, 0);
	      for (int j = 0; j < nc; j++)
		{
		  retval.elem (i, j) = prod;
		  if (j < nc - 1)
		    prod *= elem (i, j+1);
		}
	    }
	}
      else
	{
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
ComplexMatrix::cumsum (int dim) const
{
  int nr = rows ();
  int nc = cols ();
  ComplexMatrix retval (nr, nc);
  if (nr > 0 && nc > 0)
    {
      if ((nr == 1 && dim == 0) || dim == 1)
	{
	  for (int i = 0; i < nr; i++)
	    {
	      Complex sum = elem (i, 0);
	      for (int j = 0; j < nc; j++)
		{
		  retval.elem (i, j) = sum;
		  if (j < nc - 1)
		    sum += elem (i, j+1);
		}
	    }
	}
      else
	{
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
ComplexMatrix::prod (int dim) const
{
  int nr = rows ();
  int nc = cols ();
  ComplexMatrix retval;
  if (nr > 0 && nc > 0)
    {
      if ((nr == 1 && dim == 0) || dim == 1)
	{
	  retval.resize(nr, 1);
	  for (int i = 0; i < nr; i++)
	    {
	      retval.elem (i, 0) = 1.0;
	      for (int j = 0; j < nc; j++)
		retval.elem (i, 0) *= elem (i, j);
	    }
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
  else
    {
      retval.resize (1,1);
      retval.elem (0,0) = 1.0;
    }
  return retval;
}

ComplexMatrix
ComplexMatrix::sum (int dim) const
{
  int nr = rows ();
  int nc = cols ();
  ComplexMatrix retval;
  if (nr > 0 && nc > 0)
    {
      if ((nr == 1 && dim == 0) || dim == 1)
	{
	  retval.resize (nr, 1);
	  for (int i = 0; i < nr; i++)
	    {
	      retval.elem (i, 0) = 0.0;
	      for (int j = 0; j < nc; j++)
		retval.elem (i, 0) += elem (i, j);
	    }
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
  else
    {
      retval.resize (1, 1);
      retval.elem (0, 0) = 0.0;
    }
  return retval;
}

ComplexMatrix
ComplexMatrix::sumsq (int dim) const
{
  int nr = rows ();
  int nc = cols ();
  ComplexMatrix retval;
  if (nr > 0 && nc > 0)
    {
      if ((nr == 1 && dim == 0) || dim == 1)
	{
	  retval.resize (nr, 1);
	  for (int i = 0; i < nr; i++)
	    {
	      retval.elem (i, 0) = 0.0;
	      for (int j = 0; j < nc; j++)
		{
		  Complex d = elem (i, j);
		  retval.elem (i, 0) += d * conj (d);
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
		  Complex d = elem (i, j);
		  retval.elem (0, j) += d * conj (d);
		}
	    }
	}
    }
  else
    {
      retval.resize (1, 1);
      retval.elem (0, 0) = 0.0;
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
    std::cerr << "diag: requested diagonal out of range\n";

  return d;
}

bool
ComplexMatrix::row_is_real_only (int i) const
{
  bool retval = true;

  int nc = columns ();

  for (int j = 0; j < nc; j++)
    {
      if (imag (elem (i, j)) != 0.0)
	{
	  retval = false;
	  break;
	}
    }

  return retval;	      
}

bool
ComplexMatrix::column_is_real_only (int j) const
{
  bool retval = true;

  int nr = rows ();

  for (int i = 0; i < nr; i++)
    {
      if (imag (elem (i, j)) != 0.0)
	{
	  retval = false;
	  break;
	}
    }

  return retval;	      
}

ComplexColumnVector
ComplexMatrix::row_min (void) const
{
  Array<int> index;
  return row_min (index);
}

ComplexColumnVector
ComplexMatrix::row_min (Array<int>& index) const
{
  ComplexColumnVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nr);
      index.resize (nr);

      for (int i = 0; i < nr; i++)
        {
	  int idx_j = 0;

	  Complex tmp_min = elem (i, idx_j);

	  bool real_only = row_is_real_only (i);

	  double abs_min = real_only ? real (tmp_min) : abs (tmp_min);

	  if (xisnan (tmp_min))
	    idx_j = -1;
	  else
	    {
	      for (int j = 1; j < nc; j++)
		{
		  Complex tmp = elem (i, j);

		  double abs_tmp = real_only ? real (tmp) : abs (tmp);

		  if (xisnan (tmp))
		    {
		      idx_j = -1;
		      break;
		    }
		  else if (abs_tmp < abs_min)
		    {
		      idx_j = j;
		      tmp_min = tmp;
		      abs_min = abs_tmp;
		    }
		}

	      result.elem (i) = (idx_j < 0) ? Complex_NaN_result : tmp_min;
	      index.elem (i) = idx_j;
	    }
        }
    }

  return result;
}

ComplexColumnVector
ComplexMatrix::row_max (void) const
{
  Array<int> index;
  return row_max (index);
}

ComplexColumnVector
ComplexMatrix::row_max (Array<int>& index) const
{
  ComplexColumnVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nr);
      index.resize (nr);

      for (int i = 0; i < nr; i++)
        {
	  int idx_j = 0;

	  Complex tmp_max = elem (i, idx_j);

	  bool real_only = row_is_real_only (i);

	  double abs_max = real_only ? real (tmp_max) : abs (tmp_max);

	  if (xisnan (tmp_max))
	    idx_j = -1;
	  else
	    {
	      for (int j = 1; j < nc; j++)
		{
		  Complex tmp = elem (i, j);

		  double abs_tmp = real_only ? real (tmp) : abs (tmp);

		  if (xisnan (tmp))
		    {
		      idx_j = -1;
		      break;
		    }
		  else if (abs_tmp > abs_max)
		    {
		      idx_j = j;
		      tmp_max = tmp;
		      abs_max = abs_tmp;
		    }
		}

	      result.elem (i) = (idx_j < 0) ? Complex_NaN_result : tmp_max;
	      index.elem (i) = idx_j;
	    }
        }
    }

  return result;
}

ComplexRowVector
ComplexMatrix::column_min (void) const
{
  Array<int> index;
  return column_min (index);
}

ComplexRowVector
ComplexMatrix::column_min (Array<int>& index) const
{
  ComplexRowVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nc);
      index.resize (nc);

      for (int j = 0; j < nc; j++)
        {
	  int idx_i = 0;

	  Complex tmp_min = elem (idx_i, j);

	  bool real_only = column_is_real_only (j);

	  double abs_min = real_only ? real (tmp_min) : abs (tmp_min);

	  if (xisnan (tmp_min))
	    idx_i = -1;
	  else
	    {
	      for (int i = 1; i < nr; i++)
		{
		  Complex tmp = elem (i, j);

		  double abs_tmp = real_only ? real (tmp) : abs (tmp);

		  if (xisnan (tmp))
		    {
		      idx_i = -1;
		      break;
		    }
		  else if (abs_tmp < abs_min)
		    {
		      idx_i = i;
		      tmp_min = tmp;
		      abs_min = abs_tmp;
		    }
		}

	      result.elem (j) = (idx_i < 0) ? Complex_NaN_result : tmp_min;
	      index.elem (j) = idx_i;
	    }
        }
    }

  return result;
}

ComplexRowVector
ComplexMatrix::column_max (void) const
{
  Array<int> index;
  return column_max (index);
}

ComplexRowVector
ComplexMatrix::column_max (Array<int>& index) const
{
  ComplexRowVector result;

  int nr = rows ();
  int nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nc);
      index.resize (nc);

      for (int j = 0; j < nc; j++)
        {
	  int idx_i = 0;

	  Complex tmp_max = elem (idx_i, j);

	  bool real_only = column_is_real_only (j);

	  double abs_max = real_only ? real (tmp_max) : abs (tmp_max);

	  if (xisnan (tmp_max))
	    idx_i = -1;
	  else
	    {
	      for (int i = 1; i < nr; i++)
		{
		  Complex tmp = elem (i, j);

		  double abs_tmp = real_only ? real (tmp) : abs (tmp);

		  if (xisnan (tmp))
		    {
		      idx_i = -1;
		      break;
		    }
		  else if (abs_tmp > abs_max)
		    {
		      idx_i = i;
		      tmp_max = tmp;
		      abs_max = abs_tmp;
		    }
		}

	      result.elem (j) = (idx_i < 0) ? Complex_NaN_result : tmp_max;
	      index.elem (j) = idx_i;
	    }
        }
    }

  return result;
}

// i/o

std::ostream&
operator << (std::ostream& os, const ComplexMatrix& a)
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

std::istream&
operator >> (std::istream& is, ComplexMatrix& a)
{
  int nr = a.rows ();
  int nc = a.cols ();

  if (nr < 1 || nc < 1)
    is.clear (std::ios::badbit);
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
	      goto done;
	  }
    }

done:

  return is;
}

ComplexMatrix
Givens (const Complex& x, const Complex& y)
{
  double cc;
  Complex cs, temp_r;
 
  F77_FCN (zlartg, ZLARTG) (x, y, cc, cs, temp_r);

  ComplexMatrix g (2, 2);

  g.elem (0, 0) = cc;
  g.elem (1, 1) = cc;
  g.elem (0, 1) = cs;
  g.elem (1, 0) = -conj (cs);

  return g;
}

ComplexMatrix
Sylvester (const ComplexMatrix& a, const ComplexMatrix& b,
	   const ComplexMatrix& c)
{
  ComplexMatrix retval;

  // XXX FIXME XXX -- need to check that a, b, and c are all the same
  // size.

  // Compute Schur decompositions

  ComplexSCHUR as (a, "U");
  ComplexSCHUR bs (b, "U");
  
  // Transform c to new coordinates.

  ComplexMatrix ua = as.unitary_matrix ();
  ComplexMatrix sch_a = as.schur_matrix ();

  ComplexMatrix ub = bs.unitary_matrix ();
  ComplexMatrix sch_b = bs.schur_matrix ();
  
  ComplexMatrix cx = ua.hermitian () * c * ub;

  // Solve the sylvester equation, back-transform, and return the
  // solution.

  int a_nr = a.rows ();
  int b_nr = b.rows ();

  double scale;
  int info;

  Complex *pa = sch_a.fortran_vec ();
  Complex *pb = sch_b.fortran_vec ();
  Complex *px = cx.fortran_vec ();
  
  F77_XFCN (ztrsyl, ZTRSYL, ("N", "N", 1, a_nr, b_nr, pa, a_nr, pb,
			     b_nr, px, a_nr, scale,
			     info, 1L, 1L));

  if (f77_exception_encountered)
    (*current_liboctave_error_handler) ("unrecoverable error in ztrsyl");
  else
    {
      // XXX FIXME XXX -- check info?

      retval = -ua * cx * ub.hermitian ();
    }

  return retval;
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
  ComplexMatrix retval;

  int nr = m.rows ();
  int nc = m.cols ();

  int a_nr = a.rows ();
  int a_nc = a.cols ();

  if (nc != a_nr)
    gripe_nonconformant ("operator *", nr, nc, a_nr, a_nc);
  else
    {
      if (nr == 0 || nc == 0 || a_nc == 0)
	retval.resize (nr, nc, 0.0);
      else
	{
	  int ld  = nr;
	  int lda = a.rows ();

	  retval.resize (nr, a_nc);
	  Complex *c = retval.fortran_vec ();

	  F77_XFCN (zgemm, ZGEMM, ("N", "N", nr, a_nc, nc, 1.0,
				   m.data (), ld, a.data (), lda, 0.0,
				   c, nr, 1L, 1L));

	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler)
	      ("unrecoverable error in zgemm");
	}
    }

  return retval;
}

MS_CMP_OPS(ComplexMatrix, real, Complex, real)
MS_BOOL_OPS(ComplexMatrix, Complex, 0.0)

SM_CMP_OPS(Complex, real, ComplexMatrix, real)
SM_BOOL_OPS(Complex, ComplexMatrix, 0.0)

MM_CMP_OPS(ComplexMatrix, real, ComplexMatrix, real)
MM_BOOL_OPS(ComplexMatrix, ComplexMatrix, 0.0)

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
