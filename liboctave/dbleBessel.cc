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

#include "Range.h"
#include "dColVector.h"
#include "dMatrix.h"
#include "dbleBessel.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "mx-inlines.cc"

extern "C"
{
  int F77_FCN (rjbesl, RJBESL) (const double&, const double&,
				const int&, double*, int&);

  int F77_FCN (rybesl, RYBESL) (const double&, const double&,
				const int&, double*, int&);

  int F77_FCN (ribesl, RIBESL) (const double&, const double&,
				const int&, const int&, double*, int&);

  int F77_FCN (rkbesl, RKBESL) (const double&, const double&,
				const int&, const int&, double*, int&);
}

int
F77_FCN (ribesl, RIBESL) (const double& x, const double& alpha,
			  const int& nb, double *result, int& ncalc)
{
  int retval = 0;
  F77_FCN (ribesl, RIBESL) (x, alpha, nb, 1, result, ncalc);
  return retval;
}

int
F77_FCN (rkbesl, RKBESL) (const double& x, const double& alpha,
			  const int& nb, double *result, int& ncalc)
{
  int retval = 0;
  F77_FCN (rkbesl, RKBESL) (x, alpha, nb, 1, result, ncalc);
  return retval;
}

typedef int (*fptr) (const double&, const double&, const int&, double*, int&);

static Matrix
do_bessel (fptr f, const char *fn, double alpha, const Matrix& x)
{
  Matrix retval;

  if (alpha >= 0.0)
    {
      int nb = 1;

      if (alpha >= 1.0)
	{
	  double integral;
	  alpha = modf (alpha, &integral);
	  nb = static_cast<int> (integral) + 1;
	}

      Array<double> tmp (nb);

      double *ptmp = tmp.fortran_vec ();

      int x_nr = x.rows ();
      int x_nc = x.cols ();

      retval.resize (x_nr, x_nc);

      int ncalc;

      for (int j = 0; j < x_nc; j++)
	{
	  for (int i = 0; i < x_nr; i++)
	    {
	      f (x(i,j), alpha, nb, ptmp, ncalc);

	      // XXX FIXME XXX -- check ncalc.

	      retval(i,j) = tmp(nb-1);
	    }
	}
    }
  else
    (*current_liboctave_error_handler)
      ("%s: alpha must be greater than zero", fn);

  return retval;
}

static Matrix
do_bessel (fptr f, const char *fn, const Range& alpha_range,
	   const ColumnVector& x)
{
  Matrix retval;

  double alpha_base = alpha_range.base ();

  if (alpha_base >= 0.0)
    {
      int nb_orig = alpha_range.nelem ();
      int nb = nb_orig;
      int offset = 0;

      if (alpha_base >= 1.0)
	{
	  double integral;
	  alpha_base = modf (alpha_base, &integral);
	  offset = static_cast<int> (integral);
	  nb += offset;
	}

      Array<double> tmp (nb);

      double *ptmp = tmp.fortran_vec ();

      int x_len = x.length ();

      retval.resize (x_len, nb_orig);

      int ncalc;

      for (int i = 0; i < x_len; i++)
	{
	  f (x(i), alpha_base, nb, ptmp, ncalc);

	  // XXX FIXME XXX -- check ncalc.

	  for (int j = 0; j < nb_orig; j++)
	    retval(i,j) = tmp(offset+j);
	}
    }
  else
    (*current_liboctave_error_handler)
      ("%s: alpha must be greater than zero", fn);

  return retval;
}

Matrix
besselj (double alpha, const Matrix& x)
{
  return do_bessel (F77_FCN (rjbesl, RJBESL), "besselj", alpha, x);
}

Matrix
bessely (double alpha, const Matrix& x)
{
  return do_bessel (F77_FCN (rybesl, RYBESL), "bessely", alpha, x);
}

Matrix
besseli (double alpha, const Matrix& x)
{
  return do_bessel (F77_FCN (ribesl, RIBESL), "besseli", alpha, x);
}

Matrix
besselk (double alpha, const Matrix& x)
{
  return do_bessel (F77_FCN (rkbesl, RKBESL), "besselk", alpha, x);
}

Matrix
besselj (const Range& alpha, const ColumnVector& x)
{
  return do_bessel (F77_FCN (rjbesl, RJBESL), "besselj", alpha, x);
}

Matrix
bessely (const Range& alpha, const ColumnVector& x)
{
  return do_bessel (F77_FCN (rybesl, RYBESL), "bessely", alpha, x);
}

Matrix
besseli (const Range& alpha, const ColumnVector& x)
{
  return do_bessel (F77_FCN (ribesl, RIBESL), "besseli", alpha, x);
}

Matrix
besselk (const Range& alpha, const ColumnVector& x)
{
  return do_bessel (F77_FCN (rkbesl, RKBESL), "besselk", alpha, x);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
