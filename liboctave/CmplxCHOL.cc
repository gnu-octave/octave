/*

Copyright (C) 1996, 1997 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "dMatrix.h"
#include "dRowVector.h"
#include "CmplxCHOL.h"
#include "f77-fcn.h"
#include "lo-error.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (zpotrf, ZPOTRF) (F77_CONST_CHAR_ARG_DECL, const octave_idx_type&,
			     Complex*, const octave_idx_type&, octave_idx_type&
			     F77_CHAR_ARG_LEN_DECL);
  F77_RET_T
  F77_FUNC (zpotri, ZPOTRI) (F77_CONST_CHAR_ARG_DECL, const octave_idx_type&,
			     Complex*, const octave_idx_type&, octave_idx_type&
			     F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (zpocon, ZPOCON) (F77_CONST_CHAR_ARG_DECL, const octave_idx_type&,
			     Complex*, const octave_idx_type&, const double&,
			     double&, Complex*, double*, 
			     octave_idx_type& F77_CHAR_ARG_LEN_DECL);
}

octave_idx_type
ComplexCHOL::init (const ComplexMatrix& a, bool calc_cond)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (a_nr != a_nc)
    {
      (*current_liboctave_error_handler)
	("ComplexCHOL requires square matrix");
      return -1;
    }

  octave_idx_type n = a_nc;
  octave_idx_type info;

  chol_mat = a;
  Complex *h = chol_mat.fortran_vec ();

  // Calculate the norm of the matrix, for later use.
  double anorm = 0;
  if (calc_cond) 
    anorm = chol_mat.abs().sum().row(static_cast<octave_idx_type>(0)).max();

  F77_XFCN (zpotrf, ZPOTRF, (F77_CONST_CHAR_ARG2 ("U", 1), n, h, n, info
			     F77_CHAR_ARG_LEN (1)));

  if (f77_exception_encountered)
    (*current_liboctave_error_handler) ("unrecoverable error in zpotrf");
  else
    {
      xrcond = 0.0;
      if (info != 0)
	info = -1;
      else if (calc_cond) 
	{
	  octave_idx_type zpocon_info = 0;

	  // Now calculate the condition number for non-singular matrix.
	  Array<Complex> z (2*n);
	  Complex *pz = z.fortran_vec ();
	  Array<double> rz (n);
	  double *prz = rz.fortran_vec ();
	  F77_XFCN (zpocon, ZPOCON, (F77_CONST_CHAR_ARG2 ("U", 1), n, h,
				     n, anorm, xrcond, pz, prz, zpocon_info
				     F77_CHAR_ARG_LEN (1)));

	  if (f77_exception_encountered)
	    (*current_liboctave_error_handler) 
	      ("unrecoverable error in zpocon");

	  if (zpocon_info != 0) 
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
    }

  return info;
}

static ComplexMatrix
chol2inv_internal (const ComplexMatrix& r)
{
  ComplexMatrix retval;

  octave_idx_type r_nr = r.rows ();
  octave_idx_type r_nc = r.cols ();

  if (r_nr == r_nc)
    {
      octave_idx_type n = r_nc;
      octave_idx_type info;

      ComplexMatrix tmp = r;

      F77_XFCN (zpotri, ZPOTRI, (F77_CONST_CHAR_ARG2 ("U", 1), n,
				 tmp.fortran_vec (), n, info
				 F77_CHAR_ARG_LEN (1)));

      if (f77_exception_encountered)
	(*current_liboctave_error_handler) ("unrecoverable error in zpotri");
      else
	{
	  // If someone thinks of a more graceful way of doing this (or
	  // faster for that matter :-)), please let me know!

	  if (n > 1)
	    for (octave_idx_type j = 0; j < r_nc; j++)
	      for (octave_idx_type i = j+1; i < r_nr; i++)
		tmp.xelem (i, j) = std::conj (tmp.xelem (j, i));

	  retval = tmp;
	}
    }
  else
    (*current_liboctave_error_handler) ("chol2inv requires square matrix");

  return retval;
}

// Compute the inverse of a matrix using the Cholesky factorization.
ComplexMatrix
ComplexCHOL::inverse (void) const
{
  return chol2inv_internal (chol_mat);
}

ComplexMatrix
chol2inv (const ComplexMatrix& r)
{
  return chol2inv_internal (r);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
