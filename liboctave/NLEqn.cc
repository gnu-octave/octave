// NLEqn.cc                                              -*- C++ -*-
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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <float.h>
#include <math.h>

#include "NLEqn.h"
#include "dMatrix.h"
#include "f77-uscore.h"
#include "lo-error.h"

extern "C"
{
  int F77_FCN (hybrd1) (int (*)(int*, double*, double*, int*),
			const int*, double*, double*, const double*,
			int*, double*, const int*);

  int F77_FCN (hybrj1) (int (*)(int*, double*, double*, double*, int*, int*),
			const int*, double*, double*, double*, const int*,
			const double*, int*, double*, const int*);
}

static nonlinear_fcn user_fun;
static jacobian_fcn user_jac;

// error handling

void
NLEqn::error (const char* msg)
{
  (*current_liboctave_error_handler) ("fatal NLEqn error: %s", msg);
}

// Constructors

NLEqn::NLEqn (void) : NLFunc (), n (0), x () { }

NLEqn::NLEqn (const Vector& xvec, const NLFunc f) 
  : NLFunc (f), n (xvec.capacity ()), x (xvec) { }

NLEqn::NLEqn (const NLEqn& a) : NLFunc (a.fun, a.jac), n (a.n), x (a.x) { }

void
NLEqn::resize (int nn)
{
  if (n != nn)
    {
      n = nn;
      x.resize (n);
    }
}

int
NLEqn::size (void) const
{
  return n;
}

// Assignment

NLEqn&
NLEqn::operator = (const NLEqn& a)
{
  fun = a.fun;
  jac = a.jac;
  x = a.n;

  return *this;
}

Vector
NLEqn::states (void) const
{
  return x;
}

void
NLEqn::set_states (const Vector& xvec)
{
  if (xvec.capacity () != n)
    {
      error ("dimension error");
      return;
    }

  x = xvec;
}

// Other operations

Vector
NLEqn::solve (const Vector& xvec)
{
  set_states (xvec);
  int info;
  return solve (info);
}

Vector
NLEqn::solve (const Vector& xvec, int& info)
{
  set_states (xvec);
  return solve (info);
}

Vector
NLEqn::solve (void)
{
  int info;
  return solve (info);
}

int
hybrd1_fcn (int *n, double *x, double *fvec, int *iflag)
{
  int nn = *n;
  Vector tmp_f (nn);
  Vector tmp_x (nn);

  for (int i = 0; i < nn; i++)
    tmp_x.elem (i) = x[i];

  tmp_f = (*user_fun) (tmp_x);

  if (tmp_f.length () == 0)
    *iflag = -1;
  else
    {
      for (i = 0; i < nn; i++)
	fvec[i] = tmp_f.elem (i);
    }

  return 0;
}

int
hybrj1_fcn (int *n, double *x, double *fvec, double *fjac,
	    int *ldfjac, int *iflag)
{
  int nn = *n;
  Vector tmp_x (nn);

  for (int i = 0; i < nn; i++)
    tmp_x.elem (i) = x[i];

  int flag = *iflag;
  if (flag == 1)
    {
      Vector tmp_f (nn);

      tmp_f = (*user_fun) (tmp_x);

      if (tmp_f.length () == 0)
	*iflag = -1;
      else
	{
	  for (i = 0; i < nn; i++)
	    fvec[i] = tmp_f.elem (i);
	}
    }
  else
    {
      Matrix tmp_fj (nn, nn);

      tmp_fj = (*user_jac) (tmp_x);

      if (tmp_fj.rows () == 0 || tmp_fj.columns () == 0)
	*iflag = -1;
      else
	{
	  int ld = *ldfjac;
	  for (int j = 0; j < nn; j++)
	    for (i = 0; i < nn; i++)
	      fjac[j*ld+i] = tmp_fj.elem (i, j);
	}
    }

  return 0;
}

Vector
NLEqn::solve (int& info)
{
  int tmp_info = 0;

  if (n == 0)
    {
      error ("equation set not initialized");
      return Vector ();
    }

  double tol = tolerance ();

  double *fvec = new double [n];
  double *px = new double [n];
  for (int i = 0; i < n; i++)
    px[i] = x.elem (i);

  user_fun = fun;
  user_jac = jac;

  if (jac)
    {
      int lwa = (n*(n+13))/2;
      double *wa = new double [lwa];
      double *fjac = new double [n*n];

      F77_FCN (hybrj1) (hybrj1_fcn, &n, px, fvec, fjac, &n, &tol,
			&tmp_info, wa, &lwa);

      delete [] wa;
      delete [] fjac;
    }
  else
    {
      int lwa = (n*(3*n+13))/2;
      double *wa = new double [lwa];

      F77_FCN (hybrd1) (hybrd1_fcn, &n, px, fvec, &tol, &tmp_info, wa, &lwa);

      delete [] wa;
    }

  Vector retval;

  info = tmp_info;

  if (info >= 0)
    {
      retval.resize (n);

      for (i = 0; i < n; i++)
	retval.elem (i) = px[i];
    }

  delete [] fvec;
  delete [] px;

  return retval;
}

NLEqn_options::NLEqn_options (void)
{
  init ();
}

NLEqn_options::NLEqn_options (const NLEqn_options& opt)
{
  copy (opt);
}

NLEqn_options&
NLEqn_options::operator = (const NLEqn_options& opt)
{
  if (this != &opt)
    copy (opt);

  return *this;
}

NLEqn_options::~NLEqn_options (void)
{
}

void
NLEqn_options::init (void)
{
  double sqrt_eps = sqrt (DBL_EPSILON);
  x_tolerance = sqrt_eps;
}

void
NLEqn_options::copy (const NLEqn_options& opt)
{
  x_tolerance = opt.x_tolerance;
}

void
NLEqn_options::set_default_options (void)
{
  init ();
}

void
NLEqn_options::set_tolerance (double val)
{
  x_tolerance = (val > 0.0) ? val : sqrt (DBL_EPSILON);
}

double
NLEqn_options::tolerance (void)
{
  return x_tolerance;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
