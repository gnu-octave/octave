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

#include "Quad.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "quit.h"
#include "sun-utils.h"

static integrand_fcn user_fcn;

// XXX FIXME XXX -- would be nice to not have to have this global
// variable.
// Nonzero means an error occurred in the calculation of the integrand
// function, and the user wants us to quit.
int quad_integration_error = 0;

typedef octave_idx_type (*quad_fcn_ptr) (double*, int&, double*);
			      
extern "C"
{
  F77_RET_T
  F77_FUNC (dqagp, DQAGP) (quad_fcn_ptr, const double&, const double&,
			   const octave_idx_type&, const double*, const double&,
			   const double&, double&, double&, octave_idx_type&,
			   octave_idx_type&, const octave_idx_type&, const octave_idx_type&, octave_idx_type&, octave_idx_type*,
			   double*);

  F77_RET_T
  F77_FUNC (dqagi, DQAGI) (quad_fcn_ptr, const double&, const octave_idx_type&,
			   const double&, const double&, double&,
			   double&, octave_idx_type&, octave_idx_type&, const octave_idx_type&,
			   const octave_idx_type&, octave_idx_type&, octave_idx_type*, double*); 
}

static octave_idx_type
user_function (double *x, int& ierr, double *result)
{
  BEGIN_INTERRUPT_WITH_EXCEPTIONS;

#if defined (sun) && defined (__GNUC__)
  double xx = access_double (x);
#else
  double xx = *x;
#endif

  quad_integration_error = 0;

  double xresult = (*user_fcn) (xx);

#if defined (sun) && defined (__GNUC__)
  assign_double (result, xresult);
#else
  *result = xresult;
#endif

  if (quad_integration_error)
    ierr = -1;

  END_INTERRUPT_WITH_EXCEPTIONS;

  return 0;
}

double
DefQuad::do_integrate (octave_idx_type& ier, octave_idx_type& neval, double& abserr)
{
  octave_idx_type npts = singularities.capacity () + 2;
  double *points = singularities.fortran_vec ();
  double result = 0.0;

  octave_idx_type leniw = 183*npts - 122;
  Array<octave_idx_type> iwork (leniw);
  octave_idx_type *piwork = iwork.fortran_vec ();

  octave_idx_type lenw = 2*leniw - npts;
  Array<double> work (lenw);
  double *pwork = work.fortran_vec ();

  user_fcn = f;
  octave_idx_type last;

  double abs_tol = absolute_tolerance ();
  double rel_tol = relative_tolerance ();

  F77_XFCN (dqagp, DQAGP, (user_function, lower_limit, upper_limit,
			   npts, points, abs_tol, rel_tol, result,
			   abserr, neval, ier, leniw, lenw, last,
			   piwork, pwork));

  if (f77_exception_encountered)
    (*current_liboctave_error_handler) ("unrecoverable error in dqagp");

  return result;
}

double
IndefQuad::do_integrate (octave_idx_type& ier, octave_idx_type& neval, double& abserr)
{
  double result = 0.0;

  octave_idx_type leniw = 128;
  Array<octave_idx_type> iwork (leniw);
  octave_idx_type *piwork = iwork.fortran_vec ();

  octave_idx_type lenw = 8*leniw;
  Array<double> work (lenw);
  double *pwork = work.fortran_vec ();

  user_fcn = f;
  octave_idx_type last;

  octave_idx_type inf;
  switch (type)
    {
    case bound_to_inf:
      inf = 1;
      break;

    case neg_inf_to_bound:
      inf = -1;
      break;

    case doubly_infinite:
      inf = 2;
      break;

    default:
      assert (0);
      break;
    }

  double abs_tol = absolute_tolerance ();
  double rel_tol = relative_tolerance ();

  F77_XFCN (dqagi, DQAGI, (user_function, bound, inf, abs_tol, rel_tol,
			   result, abserr, neval, ier, leniw, lenw,
			   last, piwork, pwork));

  if (f77_exception_encountered)
    (*current_liboctave_error_handler) ("unrecoverable error in dqagi");

  return result;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
