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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Range.h"
#include "dColVector.h"
#include "dMatrix.h"
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

  int F77_FCN (xdacosh, XDACOSH) (const double&, double&);

  int F77_FCN (xdasinh, XDASINH) (const double&, double&);

  int F77_FCN (xdatanh, XDATANH) (const double&, double&);

  int F77_FCN (xderf, XDERF) (const double&, double&);

  int F77_FCN (xderfc, XDERFC) (const double&, double&);

  int F77_FCN (xdbetai, XDBETAI) (const double&, const double&,
				  const double&, double&);

  int F77_FCN (xdgamma, XDGAMMA) (const double&, double&);

  int F77_FCN (xdgami, XDGAMI) (const double&, const double&, double&);

  int F77_FCN (dlgams, DLGAMS) (const double&, double&, double&);
}

#if !defined (HAVE_ACOSH)
double
acosh (double x)
{
  double retval;
  F77_XFCN (dacosh, DACOSH, (x, retval));
  return retval;
}
#endif

#if !defined (HAVE_ASINH)
double
asinh (double x)
{
  double retval;
  F77_XFCN (dasinh, DASINH, (x, retval));
  return retval;
}
#endif

#if !defined (HAVE_ATANH)
double
atanh (double x)
{
  double retval;
  F77_XFCN (datanh, DATANH, (x, retval));
  return retval;
}
#endif

#if !defined (HAVE_ERF)
double
erf (double x)
{
  double retval;
  F77_XFCN (derf, DERF, (x, retval));
  return retval;
}
#endif

#if !defined (HAVE_ERFC)
double
erfc (double x)
{
  double retval;
  F77_XFCN (derfc, DERFC, (x, retval));
  return retval;
}
#endif

double
xgamma (double x)
{
  double result;
  F77_XFCN (xdgamma, XDGAMMA, (x, result));
  return result;
}

double
xlgamma (double x)
{
  double result;
  double sgngam;
  F77_XFCN (dlgams, DLGAMS, (x, result, sgngam));
  return result;
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

	      if (ncalc < 0)
		{
		  (*current_liboctave_error_handler)
		    ("%s: error computing function values", fn);

		  return Matrix ();
		}

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

	  if (ncalc < 0)
	    {
	      (*current_liboctave_error_handler)
		("%s: error computing function values", fn);

	      return Matrix ();
	    }

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

static void
gripe_betainc_nonconformant (int r1, int c1, int r2, int c2, int r3,
			     int c3)
{
  (*current_liboctave_error_handler)
   ("betainc: nonconformant arguments (x is %dx%d, a is %dx%d, b is %dx%d)",
     r1, c1, r2, c2, r3, c3);
}

double
betainc (double x, double a, double b)
{
  double retval;
  F77_XFCN (xdbetai, XDBETAI, (x, a, b, retval));
  return retval;
}

Matrix
betainc (double x, double a, const Matrix& b)
{
  int nr = b.rows ();
  int nc = b.cols ();

  Matrix retval (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      retval(i,j) = betainc (x, a, b(i,j));

  return retval;
}

Matrix
betainc (double x, const Matrix& a, double b)
{
  int nr = a.rows ();
  int nc = a.cols ();

  Matrix retval (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      retval(i,j) = betainc (x, a(i,j), b);

  return retval;
}

Matrix
betainc (double x, const Matrix& a, const Matrix& b)
{
  Matrix retval;

  int a_nr = a.rows ();
  int a_nc = a.cols ();

  int b_nr = b.rows ();
  int b_nc = b.cols ();

  if (a_nr == b_nr && a_nc == b_nc)
    {
      retval.resize (a_nr, a_nc);

      for (int j = 0; j < a_nc; j++)
	for (int i = 0; i < a_nr; i++)
	  retval(i,j) = betainc (x, a(i,j), b(i,j));
    }
  else
    gripe_betainc_nonconformant (1, 1, a_nr, a_nc, b_nr, b_nc);

  return retval;
}

Matrix
betainc (const Matrix& x, double a, double b)
{
  int nr = x.rows ();
  int nc = x.cols ();

  Matrix retval (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      retval(i,j) = betainc (x(i,j), a, b);

  return retval;
}

Matrix
betainc (const Matrix& x, double a, const Matrix& b)
{
  Matrix retval;

  int nr = x.rows ();
  int nc = x.cols ();

  int b_nr = b.rows ();
  int b_nc = b.cols ();

  if (nr == b_nr && nc == b_nc)
    {
      retval.resize (nr, nc);

      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  retval(i,j) = betainc (x(i,j), a, b(i,j));
    }
  else
    gripe_betainc_nonconformant (nr, nc, 1, 1, b_nr, b_nc);

  return retval;
}

Matrix
betainc (const Matrix& x, const Matrix& a, double b)
{
  Matrix retval;

  int nr = x.rows ();
  int nc = x.cols ();

  int a_nr = a.rows ();
  int a_nc = a.cols ();

  if (nr == a_nr && nc == a_nc)
    {
      retval.resize (nr, nc);

      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  retval(i,j) = betainc (x(i,j), a(i,j), b);
    }
  else
    gripe_betainc_nonconformant (nr, nc, a_nr, a_nc, 1, 1);

  return retval;
}

Matrix
betainc (const Matrix& x, const Matrix& a, const Matrix& b)
{
  Matrix retval;

  int nr = x.rows ();
  int nc = x.cols ();

  int a_nr = a.rows ();
  int a_nc = a.cols ();

  int b_nr = b.rows ();
  int b_nc = b.cols ();

  if (nr == a_nr && nr == b_nr && nc == a_nc && nc == b_nc)
    {
      retval.resize (nr, nc);

      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  retval(i,j) = betainc (x(i,j), a(i,j), b(i,j));
    }
  else
    gripe_betainc_nonconformant (nr, nc, a_nr, a_nc, b_nr, b_nc);

  return retval;
}

double
gammainc (double x, double a)
{
  double retval;
  F77_XFCN (xdgami, XDGAMI, (a, x, retval));
  return retval;
}

Matrix
gammainc (double x, const Matrix& a)
{
  int nr = a.rows ();
  int nc = a.cols ();

  Matrix retval (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      retval(i,j) = gammainc (x, a(i,j));

  return retval;
}

Matrix
gammainc (const Matrix& x, double a)
{
  int nr = x.rows ();
  int nc = x.cols ();

  Matrix retval (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      retval(i,j) = gammainc (x(i,j), a);

  return retval;
}

Matrix
gammainc (const Matrix& x, const Matrix& a)
{
  Matrix retval;

  int nr = x.rows ();
  int nc = x.cols ();

  int a_nr = a.rows ();
  int a_nc = a.cols ();

  if (nr == a_nr && nc == a_nc)
    {
      retval.resize (nr, nc);

      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  retval(i,j) = gammainc (x(i,j), a(i,j));
    }
  else
    (*current_liboctave_error_handler)
      ("gammainc: nonconformant arguments (arg 1 is %dx%d, arg 2 is %dx%d)",
       nr, nc, a_nr, a_nc);

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
