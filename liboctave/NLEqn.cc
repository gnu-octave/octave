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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "NLEqn.h"
#include "dMatrix.h"
#include "f77-fcn.h"
#include "lo-error.h"

extern "C"
{
  int F77_FCN (hybrd1, HYBRD1) (int (*)(int*, double*, double*, int*),
				const int&, double*, double*,
				const double&, int&, double*,
				const int&);

  int F77_FCN (hybrj1, HYBRJ1) (int (*)(int*, double*, double*,
					double*, int*, int*),
				const int&, double*, double*, double*,
				const int&, const double&, int&,
				double*, const int&);
}

static NLFunc::nonlinear_fcn user_fun;
static NLFunc::jacobian_fcn user_jac;

// error handling

void
NLEqn::error (const char* msg)
{
  (*current_liboctave_error_handler) ("fatal NLEqn error: %s", msg);
}

// Other operations

int
hybrd1_fcn (int *n, double *x, double *fvec, int *iflag)
{
  int nn = *n;
  ColumnVector tmp_f (nn);
  ColumnVector tmp_x (nn);

  for (int i = 0; i < nn; i++)
    tmp_x.elem (i) = x[i];

  tmp_f = (*user_fun) (tmp_x);

  if (tmp_f.length () == 0)
    *iflag = -1;
  else
    {
      for (int i = 0; i < nn; i++)
	fvec[i] = tmp_f.elem (i);
    }

  return 0;
}

int
hybrj1_fcn (int *n, double *x, double *fvec, double *fjac,
	    int *ldfjac, int *iflag)
{
  int nn = *n;
  ColumnVector tmp_x (nn);

  for (int i = 0; i < nn; i++)
    tmp_x.elem (i) = x[i];

  int flag = *iflag;
  if (flag == 1)
    {
      ColumnVector tmp_f (nn);

      tmp_f = (*user_fun) (tmp_x);

      if (tmp_f.length () == 0)
	*iflag = -1;
      else
	{
	  for (int i = 0; i < nn; i++)
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
	    for (int i = 0; i < nn; i++)
	      fjac[j*ld+i] = tmp_fj.elem (i, j);
	}
    }

  return 0;
}

ColumnVector
NLEqn::solve (int& info)
{
  ColumnVector retval;

  int n = x.capacity ();

  if (n == 0)
    {
      error ("equation set not initialized");
      return retval;
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

      F77_FCN (hybrj1, HYBRJ1) (hybrj1_fcn, n, px, fvec, fjac, n, tol,
				info, wa, lwa);

      delete [] wa;
      delete [] fjac;
    }
  else
    {
      int lwa = (n*(3*n+13))/2;
      double *wa = new double [lwa];

      F77_FCN (hybrd1, HYBRD1) (hybrd1_fcn, n, px, fvec, tol, info,
				wa, lwa);

      delete [] wa;
    }

  if (info >= 0)
    {
      retval.resize (n);

      for (int i = 0; i < n; i++)
	retval.elem (i) = px[i];
    }

  delete [] fvec;
  delete [] px;

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
