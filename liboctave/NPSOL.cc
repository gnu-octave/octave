// NPSOL.cc                                                -*- C++ -*-
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

#ifndef NPSOL_MISSING

#ifdef __GNUG__
#pragma implementation
#endif

#include <iostream.h>
#include <math.h>
#include "NPSOL.h"
#include "f77-uscore.h"
#include "sun-utils.h"

extern "C"
{
  int F77_FCN (npoptn) (char *, long);

  int F77_FCN (npsol) (int *, int *, int *, int *, int *, int *,
		       double *, double *, double *, int (*)(),
		       int (*)(), int *, int *, int *, double *,
		       double *, double *, double *, double *,
		       double *, double *, int *, int *, double *,
		       int *); 
}

static objective_fcn user_phi;
static gradient_fcn user_grad;
static nonlinear_fcn user_g;
static jacobian_fcn user_jac;

int
npsol_objfun (int *mode, int *n, double *xx, double *objf,
	      double *objgrd, int *nstate) 
{
  int nn = *n;
  Vector tmp_x (nn);

  for (int i = 0; i < nn; i++)
    tmp_x.elem (i) = xx[i];

  if (*mode == 0 || *mode == 2)
    {
      double value = (*user_phi) (tmp_x);
#if defined (sun) && defined (__GNUC__)
      assign_double (objf, value);
#else
      *objf = value;
#endif
    }

  if ((*mode == 1 || *mode == 2) && user_grad != NULL)
    {
      Vector tmp_grad (nn);

      tmp_grad = (*user_grad) (tmp_x);

      for (i = 0; i < nn; i++)
	objgrd[i] = tmp_grad.elem (i);
    }

  return 0;
}

int
npsol_confun (int *mode, int *ncnln, int *n, int *nrowj, int *needc,
	      double *xx, double *cons, double *cjac, int *nstate)
{
  int nn = *n, nncnln = *ncnln;
  Vector tmp_x (nn);
  Vector tmp_c (nncnln);

  for (int i = 0; i < nn; i++)
    tmp_x.elem (i) = xx[i];

  tmp_c = (*user_g) (tmp_x);

  for (i = 0; i < nncnln; i++)
    cons[i] = tmp_c.elem (i);

  if (user_jac != NULL)
    {
      Matrix tmp_jac (nncnln, nn);

      tmp_jac = (*user_jac) (tmp_x);

      int ld = *nrowj;
      for (int j = 0; j < nn; j++)
	for (i = 0; i < nncnln; i++)
	  cjac[i+j*ld] = tmp_jac (i, j);
    }

  return 0;
}

Vector
NPSOL::minimize (void)
{
  double objf;
  int inform;
  Vector lambda;
  return minimize (objf, inform, lambda);
}

Vector
NPSOL::minimize (double& objf)
{
  int inform;
  Vector lambda;
  return minimize (objf, inform, lambda);
}

Vector
NPSOL::minimize (double& objf, int& inform)
{
  Vector lambda;
  return minimize (objf, inform, lambda);
}

Vector
NPSOL::minimize (double& objf, int& inform, Vector& lambda)
{
  // Dimensions of various things.

  int n     = x.capacity ();
  int nclin = lc.size ();
  int ncnln = nlc.size ();
  int nrowa = 1 > nclin ? 1 : nclin;
  int nrowj = 1 > ncnln ? 1 : ncnln;
  int nrowr = n;

  // Informative stuff.

  int iter;
  int *istate = new int [n+nclin+ncnln];

  // User defined function stuff is defined above in the functions
  // npsol_confun() and npsol_objfun();

  // Constraint stuff.

  double dummy;
  double *pclin = &dummy;
  Matrix clin;
  if (nclin > 0)
    {
      clin = lc.constraint_matrix ();
      pclin  = clin.fortran_vec ();
    }

  double *clow = new double [n+nclin+ncnln];
  double *cup = new double [n+nclin+ncnln];

  if (bnds.size () > 0)
    {
      for (int i = 0; i < n; i++)
	{
	  clow[i] = bnds.lower_bound (i);
	  cup[i] = bnds.upper_bound (i);
	}
    }
  else
    {
      double huge = 1.0e30;
      for (int i = 0; i < n; i++)
	{
	  clow[i] = -huge;
	  cup[i] = huge;
	}
    }

  for (int i = 0; i < nclin; i++)
    {
      clow[i+n] = lc.lower_bound (i);
      cup[i+n] = lc.upper_bound (i);
    }

  for (i = 0; i < ncnln; i++)
    {
      clow[i+n+nclin] = nlc.lower_bound (i);
      cup[i+n+nclin] = nlc.upper_bound (i);
    }

  double *c = &dummy;
  double *cjac = &dummy;
  if (ncnln > 0)
    {
      c = new double [ncnln];
      cjac = new double [nrowj*n];
    }

  // Objective stuff.

  double *objgrd = new double [n];

  // Other stuff.

  double *r = new double [n*n];

  lambda.resize (n+nclin+ncnln);
  double *pclambda = lambda.fortran_vec ();

  // Decision variable stuff.

  double *px = x.fortran_vec ();

  // Workspace parameters.

  int lenw;
  int leniw = 3 * n + nclin + 2 * ncnln;
  if (nclin == 0 && ncnln == 0)
    lenw = 20*n;
  else if (ncnln == 0)
    lenw = 2*n*(10 + n) + 11*nclin;
  else
    lenw = 2*n*(n + 10) + nclin*(n + 11) + ncnln*(2*n + 21);

  int *iw = new int [leniw];
  double *w = new double [lenw];

  user_phi  = phi.objective_function ();
  user_grad = phi.gradient_function ();
  user_g    = nlc.function ();
  user_jac  = nlc.jacobian_function ();

  // Solve the damn thing.

  if (user_jac == NULL && user_grad == NULL)
    F77_FCN (npoptn) ("Derivative Level = 0", 20L);
  else if (user_jac == NULL && user_grad != NULL)
    F77_FCN (npoptn) ("Derivative Level = 1", 20L);
  else if (user_jac != NULL && user_grad == NULL)
    F77_FCN (npoptn) ("Derivative Level = 2", 20L);
  else if (user_jac != NULL && user_grad != NULL)
    F77_FCN (npoptn) ("Derivative Level = 3", 20L);

  int attempt = 0;
  while (attempt++ < 5)
    {

      F77_FCN (npsol) (&n, &nclin, &ncnln, &nrowa, &nrowj, &nrowr, pclin,
		       clow, cup, npsol_confun, npsol_objfun, &inform,
		       &iter, istate, c, cjac, pclambda, &objf, objgrd, r,
		       px, iw, &leniw, w, &lenw);

      if (inform == 6 || inform == 1)
	continue;
      else
	break;
    }

  // See how it went.

  return x;
}

Vector
NPSOL::minimize (const Vector& xnew)
{
  x = xnew;
  return minimize ();
}

Vector
NPSOL::minimize (const Vector& xnew, double& objf)
{
  x = xnew;
  return minimize (objf);
}

Vector
NPSOL::minimize (const Vector& xnew, double& objf, int& inform)
{
  x = xnew;
  return minimize (objf, inform);
}

Vector
NPSOL::minimize (const Vector& xnew, double& objf, int& inform, Vector& lambda)
{
  x = xnew;
  return minimize (objf, inform, lambda);
}

NPSOL&
NPSOL::option (char *s)
{
  long len = strlen (s);
  F77_FCN (npoptn) (s, len);
  return *this;
}

void
NPSOL::set_default_options (void)
{
  F77_FCN (npoptn) ("Nolist", 6L);
  F77_FCN (npoptn) ("Defaults", 8L);
  F77_FCN (npoptn) ("Print Level 0", 13L);
}

#endif /* NPSOL_MISSING */

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
