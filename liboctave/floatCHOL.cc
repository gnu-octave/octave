/*

Copyright (C) 1994, 1995, 1996, 1997, 2002, 2003, 2004, 2005, 2007
              John W. Eaton
Copyright (C) 2008, 2009 Jaroslav Hajek

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

#include <vector>

#include "fRowVector.h"
#include "floatCHOL.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "oct-locbuf.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (spotrf, SPOTRF) (F77_CONST_CHAR_ARG_DECL, const octave_idx_type&,
			     float*, const octave_idx_type&, octave_idx_type&
			     F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (spotri, SPOTRI) (F77_CONST_CHAR_ARG_DECL, const octave_idx_type&,
			     float*, const octave_idx_type&, octave_idx_type&
			     F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (spocon, SPOCON) (F77_CONST_CHAR_ARG_DECL, const octave_idx_type&,
			     float*, const octave_idx_type&, const float&,
			     float&, float*, octave_idx_type*, 
			     octave_idx_type& F77_CHAR_ARG_LEN_DECL);
#ifdef HAVE_QRUPDATE

  F77_RET_T
  F77_FUNC (sch1up, SCH1UP) (const octave_idx_type&, float*, const octave_idx_type&,
                             float*, float*);

  F77_RET_T
  F77_FUNC (sch1dn, SCH1DN) (const octave_idx_type&, float*, const octave_idx_type&,
                             float*, float*, octave_idx_type&);

  F77_RET_T
  F77_FUNC (schinx, SCHINX) (const octave_idx_type&, float*, const octave_idx_type&,
                             const octave_idx_type&, float*, float*, 
                             octave_idx_type&);

  F77_RET_T
  F77_FUNC (schdex, SCHDEX) (const octave_idx_type&, float*, const octave_idx_type&,
                             const octave_idx_type&, float*);

  F77_RET_T
  F77_FUNC (schshx, SCHSHX) (const octave_idx_type&, float*, const octave_idx_type&,
                             const octave_idx_type&, const octave_idx_type&, 
                             float*);
#endif
}

octave_idx_type
FloatCHOL::init (const FloatMatrix& a, bool calc_cond)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (a_nr != a_nc)
    {
      (*current_liboctave_error_handler) ("FloatCHOL requires square matrix");
      return -1;
    }

  octave_idx_type n = a_nc;
  octave_idx_type info;

  chol_mat = a;
  float *h = chol_mat.fortran_vec ();

  // Calculate the norm of the matrix, for later use.
  float anorm = 0;
  if (calc_cond) 
    anorm = chol_mat.abs().sum().row(static_cast<octave_idx_type>(0)).max();

  F77_XFCN (spotrf, SPOTRF, (F77_CONST_CHAR_ARG2 ("U", 1),
			     n, h, n, info
			     F77_CHAR_ARG_LEN (1)));

  xrcond = 0.0;
  if (info != 0)
    info = -1;
  else if (calc_cond) 
    {
      octave_idx_type spocon_info = 0;

      // Now calculate the condition number for non-singular matrix.
      Array<float> z (3*n);
      float *pz = z.fortran_vec ();
      Array<octave_idx_type> iz (n);
      octave_idx_type *piz = iz.fortran_vec ();
      F77_XFCN (spocon, SPOCON, (F77_CONST_CHAR_ARG2 ("U", 1), n, h,
				 n, anorm, xrcond, pz, piz, spocon_info
				 F77_CHAR_ARG_LEN (1)));

      if (spocon_info != 0) 
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

static FloatMatrix
chol2inv_internal (const FloatMatrix& r)
{
  FloatMatrix retval;

  octave_idx_type r_nr = r.rows ();
  octave_idx_type r_nc = r.cols ();

  if (r_nr == r_nc)
    {
      octave_idx_type n = r_nc;
      octave_idx_type info = 0;

      FloatMatrix tmp = r;
      float *v = tmp.fortran_vec();

      if (info == 0)
	{
	  F77_XFCN (spotri, SPOTRI, (F77_CONST_CHAR_ARG2 ("U", 1), n,
				     v, n, info
				     F77_CHAR_ARG_LEN (1)));

	  // If someone thinks of a more graceful way of doing this (or
	  // faster for that matter :-)), please let me know!

	  if (n > 1)
	    for (octave_idx_type j = 0; j < r_nc; j++)
	      for (octave_idx_type i = j+1; i < r_nr; i++)
		tmp.xelem (i, j) = tmp.xelem (j, i);

	  retval = tmp;
	}
    }
  else
    (*current_liboctave_error_handler) ("chol2inv requires square matrix");

  return retval;
}

// Compute the inverse of a matrix using the Cholesky factorization.
FloatMatrix
FloatCHOL::inverse (void) const
{
  return chol2inv_internal (chol_mat);
}

void
FloatCHOL::set (const FloatMatrix& R)
{
  if (R.is_square ()) 
    chol_mat = R;
  else
    (*current_liboctave_error_handler) ("FloatCHOL requires square matrix");
}

#ifdef HAVE_QRUPDATE

void
FloatCHOL::update (const FloatColumnVector& u)
{
  octave_idx_type n = chol_mat.rows ();

  if (u.length () == n)
    {
      FloatColumnVector utmp = u;

      OCTAVE_LOCAL_BUFFER (float, w, n);

      F77_XFCN (sch1up, SCH1UP, (n, chol_mat.fortran_vec (), chol_mat.rows (),
                                 utmp.fortran_vec (), w));
    }
  else
    (*current_liboctave_error_handler) ("cholupdate: dimension mismatch");
}

octave_idx_type
FloatCHOL::downdate (const FloatColumnVector& u)
{
  octave_idx_type info = -1;

  octave_idx_type n = chol_mat.rows ();

  if (u.length () == n)
    {
      FloatColumnVector utmp = u;

      OCTAVE_LOCAL_BUFFER (float, w, n);

      F77_XFCN (sch1dn, SCH1DN, (n, chol_mat.fortran_vec (), chol_mat.rows (),
                                 utmp.fortran_vec (), w, info));
    }
  else
    (*current_liboctave_error_handler) ("cholupdate: dimension mismatch");

  return info;
}

octave_idx_type
FloatCHOL::insert_sym (const FloatColumnVector& u, octave_idx_type j)
{
  octave_idx_type info = -1;

  octave_idx_type n = chol_mat.rows ();
  
  if (u.length () != n + 1)
    (*current_liboctave_error_handler) ("cholinsert: dimension mismatch");
  else if (j < 0 || j > n)
    (*current_liboctave_error_handler) ("cholinsert: index out of range");
  else
    {
      FloatColumnVector utmp = u;

      OCTAVE_LOCAL_BUFFER (float, w, n);

      chol_mat.resize (n+1, n+1);

      F77_XFCN (schinx, SCHINX, (n, chol_mat.fortran_vec (), chol_mat.rows (),
                                 j + 1, utmp.fortran_vec (), w, info));
    }

  return info;
}

void
FloatCHOL::delete_sym (octave_idx_type j)
{
  octave_idx_type n = chol_mat.rows ();
  
  if (j < 0 || j > n-1)
    (*current_liboctave_error_handler) ("choldelete: index out of range");
  else
    {
      OCTAVE_LOCAL_BUFFER (float, w, n);

      F77_XFCN (schdex, SCHDEX, (n, chol_mat.fortran_vec (), chol_mat.rows (), 
                                 j + 1, w));

      chol_mat.resize (n-1, n-1);
    }
}

void
FloatCHOL::shift_sym (octave_idx_type i, octave_idx_type j)
{
  octave_idx_type n = chol_mat.rows ();
  
  if (i < 0 || i > n-1 || j < 0 || j > n-1) 
    (*current_liboctave_error_handler) ("cholshift: index out of range");
  else
    {
      OCTAVE_LOCAL_BUFFER (float, w, 2*n);

      F77_XFCN (schshx, SCHSHX, (n, chol_mat.fortran_vec (), chol_mat.rows (),
                                 i + 1, j + 1, w));
    }
}

#endif

FloatMatrix
chol2inv (const FloatMatrix& r)
{
  return chol2inv_internal (r);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
