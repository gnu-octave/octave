// QPSOL.cc                                                -*- C++ -*-
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
#include "config.h"
#endif

#include <math.h>
#include <float.h>

#ifndef QPSOL_MISSING

#include "QPSOL.h"
#include "f77-uscore.h"

extern "C"
{
  int F77_FCN (qpsol) (int*, int*, int*, int*, int*, int*, int*, int*,
		       double*, double*, double*, double*, double*,
		       double*, double*,
		       int (*)(int*, int*, int*, int*, double*,
			       double*, double*),
		       int*, int*, int*, int*, double*, int*, int*,
		       double*, double*, int*, int*, double*, int*);

  int F77_FCN (dgemv) (const char*, const int*, const int*,
		       const double*, const double*, const int*,
		       const double*, const int*, const double*,
		       double*, const int*, long);
}

int
qphess (int *pn, int *pnrowh, int *ncolh, int *pcol, double *hess,
	double *x, double *hx)
{
  int n = *pn;
  int nrowh = *pnrowh;
  int jthcol = *pcol;

  if (jthcol > 0)
    {
      int hp = (jthcol - 1) * nrowh;
      for (int i = 0; i < n; i++)
	hx[i] = hess[hp+i];
    }
  else
    {
      char trans = 'N';
      double alpha = 1.0;
      double beta  = 0.0;
      int i_one = 1;

      F77_FCN (dgemv) (&trans, pn, pn, &alpha, hess, pn, x, &i_one,
		       &beta, hx, &i_one, 1L);
    }

  return 0;
}

Vector
QPSOL::minimize (double& objf, int& inform, Vector& lambda)
{
  int i;

  int n = x.capacity ();
 
  int itmax = (iteration_limit () < 0) ? 50 * n : iteration_limit ();
  int msglvl = print_level ();
  int nclin = lc.size ();
  int nctotl = nclin + n;

  double bigbnd = infinite_bound ();

  double dummy;
  double *pa = &dummy;
  Matrix clin;
  if (nclin > 0)
    {
      clin = lc.constraint_matrix ();
      pa = clin.fortran_vec ();
    }

  double *pbl = new double [nctotl];
  double *pbu = new double [nctotl];

  if (bnds.size () > 0)
    {
      for (i = 0; i < n; i++)
	{
	  pbl[i] = bnds.lower_bound (i);
	  pbu[i] = bnds.upper_bound (i);
	}
    }
  else
    {
      for (i = 0; i < n; i++)
	{
	  pbl[i] = -bigbnd;
	  pbu[i] = bigbnd;
	}
    }

  for (i = 0; i < nclin; i++)
    {
      pbl[i+n] = lc.lower_bound (i);
      pbu[i+n] = lc.upper_bound (i);
    }

  double *pc = c.fortran_vec ();

  double *featol = new double [nctotl];
  double tmp = feasibility_tolerance ();
  for (i = 0; i < nctotl; i++)
    featol[i] = tmp;

  double *ph = H.fortran_vec ();

  int cold = 1;
  int lp = 0;
  int orthog = 1;

  int *istate = new int [nctotl];

  double *px = x.fortran_vec ();

  int iter = 0;
  lambda.resize (nctotl);
  double *pclambda = lambda.fortran_vec ();

  int leniw = 2 * n;

  int lenw;
  int ncon = nclin > 1 ? nclin : 1;
  if (lp == 0 || nclin >= n)
    lenw = 2*n*(n + 2) + nclin + 2*ncon;
  else
    lenw = 2*ncon*(1 + ncon) + 4*n + nclin;

  int *iw = new int [leniw];
  double *w = new double [lenw];

  F77_FCN (qpsol) (&itmax, &msglvl, &n, &nclin, &nctotl, &ncon, &n,
		   &n, &bigbnd, pa, pbl, pbu, pc, featol, ph, qphess,
		   &cold, &lp, &orthog, istate, px, &inform, &iter,
		   &objf, pclambda, iw, &leniw, w, &lenw);

  delete [] pbl;
  delete [] pbu;
  delete [] featol;
  delete [] istate;
  delete [] iw;
  delete [] w;

  return x;
}

QPSOL_options::QPSOL_options (void)
{
  init ();
}

QPSOL_options::QPSOL_options (const QPSOL_options& opt)
{
  copy (opt);
}

QPSOL_options&
QPSOL_options::operator = (const QPSOL_options& opt)
{
  if (this != &opt)
    copy (opt);

  return *this;
}

QPSOL_options::~QPSOL_options (void)
{
}

void
QPSOL_options::init (void)
{
  x_feasibility_tolerance = sqrt (DBL_EPSILON);
  x_infinite_bound = 1.0e+30;
  x_iteration_limit = -1;
  x_print_level = 0;
}

void
QPSOL_options::copy (const QPSOL_options& opt)
{
  x_feasibility_tolerance = opt.x_feasibility_tolerance;
  x_infinite_bound = opt.x_infinite_bound;
  x_iteration_limit = opt.x_iteration_limit;
  x_print_level = opt.x_print_level;
}

void
QPSOL_options::set_default_options (void)
{
  init ();
}

void
QPSOL_options::set_feasibility_tolerance (double val)
{
  x_feasibility_tolerance = (val > 0.0) ? val : sqrt (DBL_EPSILON);
}

void
QPSOL_options::set_infinite_bound (double val)
{
  x_infinite_bound = (val > 0.0) ? val : 1.0e+30;
}

void
QPSOL_options::set_iteration_limit (int val)
{
  x_iteration_limit = (val > 0) ? val : -1;
}

void
QPSOL_options::set_print_level (int val)
{
  x_print_level = (val >= 0) ? val : 0;
}

double
QPSOL_options::feasibility_tolerance (void)
{
  return x_feasibility_tolerance;
}

double
QPSOL_options::infinite_bound (void)
{
  return x_infinite_bound;
}

int
QPSOL_options::iteration_limit (void)
{
  return x_iteration_limit;
}

int
QPSOL_options::print_level (void)
{
  return x_print_level;
}

#endif /* QPSOL_MISSING */

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
