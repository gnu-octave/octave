// Quad.cc                                                -*- C++ -*-
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
#include <cmath>

#include "Quad.h"
#include "f77-uscore.h"
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

Quad::Quad (integrand_fcn fcn)
{
  f = fcn;
}

Quad::Quad (integrand_fcn fcn, double abs, double rel)
  : Quad_options (abs, rel)
{
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

  double abs_tol = absolute_tolerance ();
  double rel_tol = relative_tolerance ();

  F77_FCN (dqagp, DQAGP) (user_function, lower_limit, upper_limit,
			  npts, points, abs_tol, rel_tol, result,
			  abserr, neval, ier, leniw, lenw, last,
			  iwork, work);

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

  double abs_tol = absolute_tolerance ();
  double rel_tol = relative_tolerance ();

  F77_FCN (dqagi, DQAGI) (user_function, bound, inf, abs_tol, rel_tol,
			  result, abserr, neval, ier, leniw, lenw,
			  last, iwork, work);

  delete [] iwork;
  delete [] work;

  return result;
}

Quad_options::Quad_options (void)
{
  init ();
}

Quad_options::Quad_options (double abs, double rel)
{
  x_absolute_tolerance = abs;
  x_relative_tolerance = rel;
}

Quad_options::Quad_options (const Quad_options& opt)
{
  copy (opt);
}

Quad_options&
Quad_options::operator = (const Quad_options& opt)
{
  if (this != &opt)
    copy (opt);

  return *this;
}

Quad_options::~Quad_options (void)
{
}

void
Quad_options::init (void)
{
  double sqrt_eps = sqrt (DBL_EPSILON);
  x_absolute_tolerance = sqrt_eps;
  x_relative_tolerance = sqrt_eps;
}

void
Quad_options::copy (const Quad_options& opt)
{
  x_absolute_tolerance = opt.x_absolute_tolerance;
  x_relative_tolerance = opt.x_relative_tolerance;
}

void
Quad_options::set_default_options (void)
{
  init ();
}

void
Quad_options::set_absolute_tolerance (double val)
{
  x_absolute_tolerance = (val > 0.0) ? val : sqrt (DBL_EPSILON);
}

void
Quad_options::set_relative_tolerance (double val)
{
  x_relative_tolerance = (val > 0.0) ? val : sqrt (DBL_EPSILON);
}

double
Quad_options::absolute_tolerance (void)
{
  return x_absolute_tolerance;
}

double
Quad_options::relative_tolerance (void)
{
  return x_relative_tolerance;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
