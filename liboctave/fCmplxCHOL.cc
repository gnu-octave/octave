/*

Copyright (C) 1994, 1995, 1996, 1997, 2002, 2003, 2004, 2005, 2007
              John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

// updating/downdating by Jaroslav Hajek 2008

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <vector>

#include "fMatrix.h"
#include "fRowVector.h"
#include "fCmplxCHOL.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "oct-locbuf.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (cpotrf, CPOTRF) (F77_CONST_CHAR_ARG_DECL, const octave_idx_type&,
			     FloatComplex*, const octave_idx_type&, octave_idx_type&
			     F77_CHAR_ARG_LEN_DECL);
  F77_RET_T
  F77_FUNC (cpotri, CPOTRI) (F77_CONST_CHAR_ARG_DECL, const octave_idx_type&,
			     FloatComplex*, const octave_idx_type&, octave_idx_type&
			     F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cpocon, CPOCON) (F77_CONST_CHAR_ARG_DECL, const octave_idx_type&,
			     FloatComplex*, const octave_idx_type&, const float&,
			     float&, FloatComplex*, float*, 
			     octave_idx_type& F77_CHAR_ARG_LEN_DECL);
  F77_RET_T
  F77_FUNC (cch1up, CCH1UP) (const octave_idx_type&, FloatComplex*, FloatComplex*, float*);

  F77_RET_T
  F77_FUNC (cch1dn, CCH1DN) (const octave_idx_type&, FloatComplex*, FloatComplex*, float*, 
                             octave_idx_type&);

  F77_RET_T
  F77_FUNC (cqrshc, CQRSHC) (const octave_idx_type&, const octave_idx_type&, const octave_idx_type&,
                             FloatComplex*, FloatComplex*, const octave_idx_type&, const octave_idx_type&);

  F77_RET_T
  F77_FUNC (cchinx, CCHINX) (const octave_idx_type&, const FloatComplex*, FloatComplex*, const octave_idx_type&,
                             const FloatComplex*, octave_idx_type&);

  F77_RET_T
  F77_FUNC (cchdex, CCHDEX) (const octave_idx_type&, const FloatComplex*, FloatComplex*, const octave_idx_type&);
}

octave_idx_type
FloatComplexCHOL::init (const FloatComplexMatrix& a, bool calc_cond)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (a_nr != a_nc)
    {
      (*current_liboctave_error_handler)
	("FloatComplexCHOL requires square matrix");
      return -1;
    }

  octave_idx_type n = a_nc;
  octave_idx_type info;

  chol_mat = a;
  FloatComplex *h = chol_mat.fortran_vec ();

  // Calculate the norm of the matrix, for later use.
  float anorm = 0;
  if (calc_cond) 
    anorm = chol_mat.abs().sum().row(static_cast<octave_idx_type>(0)).max();

  F77_XFCN (cpotrf, CPOTRF, (F77_CONST_CHAR_ARG2 ("U", 1), n, h, n, info
			     F77_CHAR_ARG_LEN (1)));

  xrcond = 0.0;
  if (info != 0)
    info = -1;
  else if (calc_cond) 
    {
      octave_idx_type cpocon_info = 0;

      // Now calculate the condition number for non-singular matrix.
      Array<FloatComplex> z (2*n);
      FloatComplex *pz = z.fortran_vec ();
      Array<float> rz (n);
      float *prz = rz.fortran_vec ();
      F77_XFCN (cpocon, CPOCON, (F77_CONST_CHAR_ARG2 ("U", 1), n, h,
				 n, anorm, xrcond, pz, prz, cpocon_info
				 F77_CHAR_ARG_LEN (1)));

      if (cpocon_info != 0) 
	info = -1;
    }
  else
    {
      // If someone thinks of a more graceful way of doing this (or
      // faster for that matter :-)), please let me know!

      if (n > 1)
	for (octave_idx_type j = 0; j < a_nc; j++)
	  for (octave_idx_type i = j+1; i < a_nr; i++)
	    chol_mat.xelem (i, j) = 0.0;
    }

  return info;
}

static FloatComplexMatrix
chol2inv_internal (const FloatComplexMatrix& r)
{
  FloatComplexMatrix retval;

  octave_idx_type r_nr = r.rows ();
  octave_idx_type r_nc = r.cols ();

  if (r_nr == r_nc)
    {
      octave_idx_type n = r_nc;
      octave_idx_type info;

      FloatComplexMatrix tmp = r;

      F77_XFCN (cpotri, CPOTRI, (F77_CONST_CHAR_ARG2 ("U", 1), n,
				 tmp.fortran_vec (), n, info
				 F77_CHAR_ARG_LEN (1)));

      // If someone thinks of a more graceful way of doing this (or
      // faster for that matter :-)), please let me know!

      if (n > 1)
	for (octave_idx_type j = 0; j < r_nc; j++)
	  for (octave_idx_type i = j+1; i < r_nr; i++)
	    tmp.xelem (i, j) = std::conj (tmp.xelem (j, i));

      retval = tmp;
    }
  else
    (*current_liboctave_error_handler) ("chol2inv requires square matrix");

  return retval;
}

// Compute the inverse of a matrix using the Cholesky factorization.
FloatComplexMatrix
FloatComplexCHOL::inverse (void) const
{
  return chol2inv_internal (chol_mat);
}

void
FloatComplexCHOL::set (const FloatComplexMatrix& R)
{
  if (R.is_square ()) 
    chol_mat = R;
  else
    (*current_liboctave_error_handler) ("CHOL requires square matrix");
}

void
FloatComplexCHOL::update (const FloatComplexMatrix& u)
{
  octave_idx_type n = chol_mat.rows ();

  if (u.length () == n)
    {
      FloatComplexMatrix tmp = u;

      OCTAVE_LOCAL_BUFFER (float, w, n);

      F77_XFCN (cch1up, CCH1UP, (n, chol_mat.fortran_vec (),
				 tmp.fortran_vec (), w));
    }
  else
    (*current_liboctave_error_handler) ("CHOL update dimension mismatch");
}

octave_idx_type
FloatComplexCHOL::downdate (const FloatComplexMatrix& u)
{
  octave_idx_type info = -1;

  octave_idx_type n = chol_mat.rows ();

  if (u.length () == n)
    {
      FloatComplexMatrix tmp = u;

      OCTAVE_LOCAL_BUFFER (float, w, n);

      F77_XFCN (cch1dn, CCH1DN, (n, chol_mat.fortran_vec (),
				 tmp.fortran_vec (), w, info));
    }
  else
    (*current_liboctave_error_handler) ("CHOL downdate dimension mismatch");

  return info;
}

octave_idx_type
FloatComplexCHOL::insert_sym (const FloatComplexMatrix& u, octave_idx_type j)
{
  octave_idx_type info = -1;

  octave_idx_type n = chol_mat.rows ();
  
  if (u.length () != n+1)
    (*current_liboctave_error_handler) ("CHOL insert dimension mismatch");
  else if (j < 0 || j > n)
    (*current_liboctave_error_handler) ("CHOL insert index out of range");
  else
    {
      FloatComplexMatrix chol_mat1 (n+1, n+1);

      F77_XFCN (cchinx, CCHINX, (n, chol_mat.data (), chol_mat1.fortran_vec (), 
                                 j+1, u.data (), info));

      chol_mat = chol_mat1;
    }

  return info;
}

void
FloatComplexCHOL::delete_sym (octave_idx_type j)
{
  octave_idx_type n = chol_mat.rows ();
  
  if (j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("CHOL delete index out of range");
  else
    {
      FloatComplexMatrix chol_mat1 (n-1, n-1);

      F77_XFCN (cchdex, CCHDEX, (n, chol_mat.data (), chol_mat1.fortran_vec (), j+1));

      chol_mat = chol_mat1;
    }
}

void
FloatComplexCHOL::shift_sym (octave_idx_type i, octave_idx_type j)
{
  octave_idx_type n = chol_mat.rows ();
  FloatComplex dummy;
  
  if (i < 0 || i > n-1 || j < 0 || j > n-1) 
    (*current_liboctave_error_handler) ("CHOL shift index out of range");
  else
    F77_XFCN (cqrshc, CQRSHC, (0, n, n, &dummy, chol_mat.fortran_vec (), i+1, j+1));
}

FloatComplexMatrix
chol2inv (const FloatComplexMatrix& r)
{
  return chol2inv_internal (r);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
