// Quad.cc                                                -*- C++ -*-
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

#ifdef __GNUG__
#pragma implementation
#endif

#include <iostream.h>
#include "Quad.h"
#include "f77-uscore.h"
#include "sun-utils.h"

static integrand_fcn user_fcn;

extern "C"
{
  int F77_FCN (dqagp) (const double (*)(double*), const double*,
		       const double*, const int*, const double*,
		       const double*, const double*, double*, double*,
		       int*, int*, const int*, const int*, int*, int*,
		       double*);

  int F77_FCN (dqagi) (const double (*)(double*), const double*,
		       const int*, const double*, const double*,
		       double*, double*, int*, int*, const int*,
		       const int*, int*, int*, double*);
}

Quad::Quad (integrand_fcn fcn)
{
  absolute_tolerance = 1.0e-6;
  relative_tolerance = 1.0e-6;
  f = fcn;
}

Quad::Quad (integrand_fcn fcn, double abs, double rel)
{
  absolute_tolerance = abs;
  relative_tolerance = rel;
  f = fcn;
}

double
Quad::integrate (void)
{
  int ier, neval;
  double abserr;
  return integrate (ier, neval, abserr);
}

double
Quad::integrate (int& ier)
{
  int neval;
  double abserr;
  return integrate (ier, neval, abserr);
}

double
Quad::integrate (int& ier, int& neval)
{
  double abserr;
  return integrate (ier, neval, abserr);
}

static double
user_function (double *x)
{
#if defined (sun) && defined (__GNUC__)
  double xx = access_double (x);
#else
  double xx = *x;
#endif

  return (*user_fcn) (xx);
}

DefQuad::DefQuad (integrand_fcn fcn) : Quad (fcn)
{
  lower_limit = 0.0;
  upper_limit = 1.0;
}

DefQuad::DefQuad (integrand_fcn fcn, double ll, double ul)
  : Quad (fcn) 
{
  lower_limit = ll;
  upper_limit = ul;
}

DefQuad::DefQuad (integrand_fcn fcn, double ll, double ul,
		  double abs, double rel) : Quad (fcn, abs, rel)
{
  lower_limit = ll;
  upper_limit = ul;
}

DefQuad::DefQuad (integrand_fcn fcn, double ll, double ul,
		  const Vector& sing) : Quad (fcn)
{
  lower_limit = ll;
  upper_limit = ul;
  singularities = sing;
}

DefQuad::DefQuad (integrand_fcn fcn, const Vector& sing,
		  double abs, double rel) : Quad (fcn, abs, rel)
{
  lower_limit = 0.0;
  upper_limit = 1.0;
  singularities = sing;
}

DefQuad::DefQuad (integrand_fcn fcn, const Vector& sing)
  : Quad (fcn)
{
  lower_limit = 0.0;
  upper_limit = 1.0;
  singularities = sing;
}

DefQuad::DefQuad (integrand_fcn fcn, double ll, double ul,
		  const Vector& sing, double abs, double rel)
  : Quad (fcn, abs, rel)
{
  lower_limit = ll;
  upper_limit = ul;
  singularities = sing;
}

double
DefQuad::integrate (int& ier, int& neval, double& abserr)
{
  int npts = singularities.capacity () + 2;
  double *points = singularities.fortran_vec ();
  double result = 0.0;
  int leniw = 183*npts - 122;
  int lenw = 2*leniw - npts;
  int *iwork = new int [leniw];
  double *work = new double [lenw];
  user_fcn = f;
  int last;

  F77_FCN (dqagp) (user_function, &lower_limit, &upper_limit, &npts,
		   points, &absolute_tolerance, &relative_tolerance,
		   &result, &abserr, &neval, &ier, &leniw, &lenw,
		   &last, iwork, work);

  delete [] iwork;
  delete [] work;

  return result;
}

IndefQuad::IndefQuad (integrand_fcn fcn) : Quad (fcn)
{
  bound = 0.0;
  type = bound_to_inf;
}

IndefQuad::IndefQuad (integrand_fcn fcn, double b, IntegralType t)
  : Quad (fcn)
{
  bound = b;
  type = t;
}

IndefQuad::IndefQuad (integrand_fcn fcn, double b, IntegralType t,
		      double abs, double rel) : Quad (fcn, abs, rel)
{
  bound = b;
  type = t;
}

IndefQuad::IndefQuad (integrand_fcn fcn, double abs, double rel)
  : Quad (fcn, abs, rel)
{
  bound = 0.0;
  type = bound_to_inf;
}

double
IndefQuad::integrate (int& ier, int& neval, double& abserr)
{
  double result = 0.0;
  int leniw = 128;
  int lenw = 8*leniw;
  int *iwork = new int [leniw];
  double *work = new double [lenw];
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

  F77_FCN (dqagi) (user_function, &bound, &inf, &absolute_tolerance,
		   &relative_tolerance, &result, &abserr, &neval,
		   &ier, &leniw, &lenw, &last, iwork, work);

  delete [] iwork;
  delete [] work;

  return result;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
