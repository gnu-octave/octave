/*

Copyright (C) 1996 John W. Eaton

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

#include "Quad.h"
#include "f77-fcn.h"
#include "sun-utils.h"

static integrand_fcn user_fcn;

// XXX FIXME XXX -- would be nice to not have to have this global
// variable.
// Nonzero means an error occurred in the calculation of the integrand
// function, and the user wants us to quit.
int quad_integration_error = 0;

extern "C"
{
  int F77_FCN (dqagp, DQAGP) (const double (*)(double*, int&),
			      const double&, const double&,
			      const int&, const double*,
			      const double&, const double&, double&,
			      double&, int&, int&, const int&,
			      const int&, int&, int*, double*);

  int F77_FCN (dqagi, DQAGI) (const double (*)(double*, int&),
			      const double&, const int&,
			      const double&, const double&, double&,
			      double&, int&, int&, const int&,
			      const int&, int&, int*, double*); 
}

static double
user_function (double *x, int& ierr)
{
#if defined (sun) && defined (__GNUC__)
  double xx = access_double (x);
#else
  double xx = *x;
#endif

  quad_integration_error = 0;

  double retval = (*user_fcn) (xx);

  if (quad_integration_error)
    ierr = -1;

  return retval;
}

double
DefQuad::integrate (int& ier, int& neval, double& abserr)
{
  int npts = singularities.capacity () + 2;
  double *points = singularities.fortran_vec ();
  double result = 0.0;

  int leniw = 183*npts - 122;
  Array<int> iwork (leniw);
  int *piwork = iwork.fortran_vec ();

  int lenw = 2*leniw - npts;
  Array<double> work (lenw);
  double *pwork = work.fortran_vec ();

  user_fcn = f;
  int last;

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
IndefQuad::integrate (int& ier, int& neval, double& abserr)
{
  double result = 0.0;

  int leniw = 128;
  Array<int> iwork (leniw);
  int *piwork = iwork.fortran_vec ();

  int lenw = 8*leniw;
  Array<double> work (lenw);
  double *pwork = work.fortran_vec ();

  user_fcn = f;
  int last;

  int inf;
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
