// QLD.cc                                                -*- C++ -*-
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

#include <cmath>

#include <iostream.h>

#include "QLD.h"
#include "dColVector.h"
#include "dMatrix.h"
#include "dRowVector.h"
#include "f77-fcn.h"

extern "C"
{
  int F77_FCN (qld, QLD) (int&, int&, int&, int&, int&, double*,
			  double*, double*, double*, double*, double*,
			  double*, double*, int&, int&, int&, double*,
			  int&, int*, int&);
}

ColumnVector
QLD::minimize (double& objf, int& inform)
{
  int n = x.capacity ();

  Matrix A1 = lc.eq_constraint_matrix ();
  ColumnVector b1 = lc.eq_constraint_vector ();

  Matrix A2 = lc.ineq_constraint_matrix ();
  ColumnVector b2 = lc.ineq_constraint_vector ();

  int me = A1.rows ();
  int m = me + A2.rows ();

  cout << "n:  " << n << "\n";
  cout << "m:  " << m << "\n";
  cout << "me: " << me << "\n";

  A1.stack (A2);
  b1.stack (b2);

  int lwar = n*(3*n + 15)/2 + m + 100;
  int liwar = n + 100;

  double *war = new double [lwar];
  int *iwar = new int [liwar];

  iwar[0] = 0;

  double *u = new double [m+n+n + 100];

  int iout = 0;

  double *px = x.fortran_vec ();
  double *ph = H.fortran_vec ();

  cout << x;
  cout << H;
  cout << c;

  double *pc = 0;
  if (c.capacity () > 0)
    pc = c.fortran_vec ();

  double *pa = 0;
  if (A1.rows () > 0 && A1.columns () > 0)
    pa = A1.fortran_vec ();

  double *pb = 0;
  if (b1.capacity () > 0)
    pb = b1.fortran_vec ();

  ColumnVector xlb = bnds.lower_bounds ();
  ColumnVector xub = bnds.upper_bounds ();
  if (xlb.capacity () <= 0)
    {
      xlb.resize (n, -1.0e30);
      xub.resize (n, 1.0e30);
    }
  double *pxl = xlb.fortran_vec ();
  double *pxu = xub.fortran_vec ();

  int mmax = m > 0 ? m : 1;

  iprint = 1;
  F77_FCN (qld, QLD) (m, me, mmax, n, n, ph, pc, pa, pb, pxl, pxu, px,
		      u, iout, inform, iprint, war, lwar, iwar,
		      liwar);

  delete [] war;
  delete [] iwar;
  delete [] u;

  objf = (x.transpose () * H * x) / 2.0;
  if (c.capacity () > 0)
    objf += c.transpose () * x;

  return x;
}

void
QLD::set_default_options (void)
{
  iprint = 0;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
