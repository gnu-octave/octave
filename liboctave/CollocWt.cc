// CollocWt.cc                                                -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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

#if defined (__GNUG__)
#pragma implementation
#endif

#include <iostream.h>

#include "CollocWt.h"
#include "f77-uscore.h"
#include "lo-error.h"

extern "C"
{
  int F77_FCN (jcobi) (int*, int*, int*, int*, double*, double*,
		       double*, double*, double*, double*); 

  int F77_FCN (dfopr) (int*, int*, int*, int*, int*, int*,
		       double*, double*, double*, double*, double*);
}

// Error handling.

void
CollocWt::error (const char* msg)
{
  (*current_liboctave_error_handler) ("fatal CollocWt error: %s", msg);
}

CollocWt::CollocWt (void)
{
  n = 0;
  inc_left = 0;
  inc_right = 0;
  lb = 0.0;
  rb = 1.0;

  Alpha = 0.0;
  Beta = 0.0;

  initialized = 0;
}

CollocWt::CollocWt (int nc, int il, int ir)
{
  n = nc;
  inc_left = il;
  inc_right = ir;
  lb = 0.0;
  rb = 1.0;

  Alpha = 0.0;
  Beta = 0.0;

  initialized = 0;
}

CollocWt::CollocWt (int nc, int ir, int il, double l, double r)
{
  n = nc;
  inc_left = il;
  inc_right = ir;
  lb = l;
  rb = r;

  Alpha = 0.0;
  Beta = 0.0;

  initialized = 0;
}

CollocWt::CollocWt (int nc, double a, double b, int il, int ir)
{
  n = nc;
  inc_left = il;
  inc_right = ir;
  lb = 0.0;
  rb = 1.0;

  Alpha = a;
  Beta = b;

  initialized = 0;
}

CollocWt::CollocWt (int nc, double a, double b, int ir, int il,
		    double l, double r)  
{
  n = nc;
  inc_left = il;
  inc_right = ir;
  lb = l;
  rb = r;

  Alpha = a;
  Beta = b;

  initialized = 0;
}

CollocWt::CollocWt (const CollocWt& a)
{
  n = a.n;
  inc_left = a.inc_left;
  inc_right = a.inc_right;
  lb = a.lb;
  rb = a.rb;
  r = a.r;
  q = a.q;
  A = a.A;
  B = a.B;

  nt = n + inc_left + inc_right;

  initialized = a.initialized;
}

CollocWt&
CollocWt::operator = (const CollocWt& a)
{
  n = a.n;
  inc_left = a.inc_left;
  inc_right = a.inc_right;
  lb = a.lb;
  rb = a.rb;
  r = a.r;
  q = a.q;
  A = a.A;
  B = a.B;

  nt = a.nt;

  initialized = a.initialized;

  return *this;
}

CollocWt&
CollocWt::resize (int ncol)
{
  n = ncol;
  initialized = 0;
  return *this;
}

CollocWt&
CollocWt::add_left (void)
{
  inc_left = 1;
  initialized = 0;
  return *this;
}

CollocWt&
CollocWt::delete_left (void)
{
  inc_left = 0;
  initialized = 0;
  return *this;
}

CollocWt&
CollocWt::set_left (double val)
{
  if (val >= rb)
    {
      error ("left bound greater than right bound");
      return *this;
    }

  lb = val;
  initialized = 0;
  return *this;
}

CollocWt&
CollocWt::add_right (void)
{
  inc_right = 1;
  initialized = 0;
  return *this;
}

CollocWt&
CollocWt::delete_right (void)
{
  inc_right = 0;
  initialized = 0;
  return *this;
}

CollocWt&
CollocWt::set_right (double val)
{
  if (val <= lb)
    {
      error ("right bound less than left bound");
      return *this;
    }

  rb = val;
  initialized = 0;
  return *this;
}

CollocWt&
CollocWt::set_alpha (double val)
{
  Alpha = val;
  initialized = 0;
  return *this;
}

CollocWt&
CollocWt::set_beta (double val)
{
  Beta = val;
  initialized = 0;
  return *this;
}

void
CollocWt::init (void)
{
// Check for possible errors.

  double wid = rb - lb;
  if (wid <= 0.0)
    {
      error ("width less than or equal to zero");
      return;
    }

  nt = n + inc_left + inc_right;
  if (nt < 0)
    {
      error ("total number of collocation points less than zero");
      return;
    }
  else if (nt == 0)
    return;

  double *dif1 = new double [nt];
  double *dif2 = new double [nt];
  double *dif3 = new double [nt];
  double *vect = new double [nt];

  r.resize (nt);
  q.resize (nt);
  A.resize (nt, nt);
  B.resize (nt, nt);

  double *pr = r.fortran_vec ();

// Compute roots.

  F77_FCN (jcobi) (&nt, &n, &inc_left, &inc_right, &Alpha, &Beta,
		   dif1, dif2, dif3, pr);

  int id;
  int i, j;

// First derivative weights.

  id = 1;
  for (i = 1; i <= nt; i++)
    {
      F77_FCN (dfopr) (&nt, &n, &inc_left, &inc_right, &i, &id, dif1,
		       dif2, dif3, pr, vect); 

      for (j = 0; j < nt; j++)
	A (i-1, j) = vect[j];
    }

// Second derivative weights.

  id = 2;
  for (i = 1; i <= nt; i++)
    {
      F77_FCN (dfopr) (&nt, &n, &inc_left, &inc_right, &i, &id, dif1,
		       dif2, dif3, pr, vect); 

      for (j = 0; j < nt; j++)
	B (i-1, j) = vect[j];
    }

// Gaussian quadrature weights.

  id = 3;
  double *pq = q.fortran_vec ();
  F77_FCN (dfopr) (&nt, &n, &inc_left, &inc_right, &i, &id, dif1,
		   dif2, dif3, pr, pq);

  delete dif1;
  delete dif2;
  delete dif3;
  delete vect;

  initialized = 1;
}

ostream&
operator << (ostream& os, const CollocWt& a)
{
  if (a.left_included ())
    os << "left  boundary is included\n";
  else
    os << "left  boundary is not included\n";

  if (a.right_included ())
    os << "right boundary is included\n";
  else
    os << "right boundary is not included\n";

  os << "\n";

  os << a.Alpha << " " << a.Beta << "\n\n"
     << a.r << "\n\n"
     << a.q << "\n\n"
     << a.A << "\n"
     << a.B << "\n";

  return os;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
