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
#include "CColVector.h"
#include "CMatrix.h"
#include "dRowVector.h"
#include "dMatrix.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-specfun.h"
#include "mx-inlines.cc"

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

extern "C"
{
  int F77_FUNC (zbesj, ZBESJ) (const double&, const double&,
			       const double&, const int&, const int&,
			       double*, double*, int&, int&);

  int F77_FUNC (zbesy, ZBESY) (const double&, const double&,
			       const double&, const int&, const int&,
			       double*, double*, int&,
			       double*, double*, int&);

  int F77_FUNC (zbesi, ZBESI) (const double&, const double&,
			       const double&, const int&, const int&,
			       double*, double*, int&, int&);

  int F77_FUNC (zbesk, ZBESK) (const double&, const double&,
			       const double&, const int&, const int&,
			       double*, double*, int&, int&);

  int F77_FUNC (zbesh, ZBESH) (const double&, const double&,
			       const double&, const int&, const int&,
			       const int&, double*, double*, int&, int&);

  int F77_FUNC (zairy, ZAIRY) (const double&, const double&,
			       const int&, const int&,
			       double&, double&, int&, int&);

  int F77_FUNC (zbiry, ZBIRY) (const double&, const double&,
			       const int&, const int&,
			       double&, double&, int&);

  int F77_FUNC (xdacosh, XDACOSH) (const double&, double&);

  int F77_FUNC (xdasinh, XDASINH) (const double&, double&);

  int F77_FUNC (xdatanh, XDATANH) (const double&, double&);

  int F77_FUNC (xderf, XDERF) (const double&, double&);

  int F77_FUNC (xderfc, XDERFC) (const double&, double&);

  int F77_FUNC (xdbetai, XDBETAI) (const double&, const double&,
				   const double&, double&);

  int F77_FUNC (xdgamma, XDGAMMA) (const double&, double&);

  int F77_FUNC (xgammainc, XGAMMAINC) (const double&, const double&, double&);

  int F77_FUNC (dlgams, DLGAMS) (const double&, double&, double&);
}

#if !defined (HAVE_ACOSH)
double
acosh (double x)
{
  double retval;
  F77_FUNC (xdacosh, XDACOSH) (x, retval);
  return retval;
}
#endif

#if !defined (HAVE_ASINH)
double
asinh (double x)
{
  double retval;
  F77_FUNC (xdasinh, XDASINH) (x, retval);
  return retval;
}
#endif

#if !defined (HAVE_ATANH)
double
atanh (double x)
{
  double retval;
  F77_FUNC (xdatanh, XDATANH) (x, retval);
  return retval;
}
#endif

#if !defined (HAVE_ERF)
double
erf (double x)
{
  double retval;
  F77_FUNC (xderf, XDERF) (x, retval);
  return retval;
}
#endif

#if !defined (HAVE_ERFC)
double
erfc (double x)
{
  double retval;
  F77_FUNC (xderfc, XDERFC) (x, retval);
  return retval;
}
#endif

double
xgamma (double x)
{
  double result;
  F77_FUNC (xdgamma, XDGAMMA) (x, result);
  return result;
}

double
xlgamma (double x)
{
  double result;
  double sgngam;
  F77_FUNC (dlgams, DLGAMS) (x, result, sgngam);
  return result;
}

static inline Complex
zbesj (const Complex& z, double alpha, int kode, int& ierr);

static inline Complex
zbesy (const Complex& z, double alpha, int kode, int& ierr);

static inline Complex
zbesi (const Complex& z, double alpha, int kode, int& ierr);

static inline Complex
zbesk (const Complex& z, double alpha, int kode, int& ierr);

static inline Complex
zbesh1 (const Complex& z, double alpha, int kode, int& ierr);

static inline Complex
zbesh2 (const Complex& z, double alpha, int kode, int& ierr);

static inline Complex
bessel_return_value (const Complex& val, int ierr)
{
  static const Complex inf_val = Complex (octave_Inf, octave_Inf);
  static const Complex nan_val = Complex (octave_NaN, octave_NaN);

  Complex retval;

  switch (ierr)
    {
    case 0:
    case 3:
      retval = val;
      break;

    case 2:
      retval = inf_val;
      break;

    default:
      retval = nan_val;
      break;
    }

  return retval;
}

static inline Complex
zbesj (const Complex& z, double alpha, int kode, int& ierr)
{
  Complex retval;

  if (alpha >= 0.0)
    {
      double yr = 0.0;
      double yi = 0.0;

      int nz;

      double zr = z.real ();
      double zi = z.imag ();

      F77_FUNC (zbesj, ZBESJ) (zr, zi, alpha, kode, 1, &yr, &yi, nz, ierr);

      if (zi == 0.0 && zr >= 0.0)
	yi = 0.0;

      retval = bessel_return_value (Complex (yr, yi), ierr);
    }
  else
    {
      alpha = -alpha;

      Complex tmp = cos (M_PI * alpha) * zbesj (z, alpha, kode, ierr);

      if (ierr == 0 || ierr == 3)
	{
	  tmp -= sin (M_PI * alpha) * zbesy (z, alpha, kode, ierr);

	  retval = bessel_return_value (tmp, ierr);
	}
      else
	retval = Complex (octave_NaN, octave_NaN);
    }

  return retval;
}

static inline Complex
zbesy (const Complex& z, double alpha, int kode, int& ierr)
{
  Complex retval;

  if (alpha >= 0.0)
    {
      double yr = 0.0;
      double yi = 0.0;

      int nz;

      double wr, wi;

      double zr = z.real ();
      double zi = z.imag ();

      ierr = 0;

      if (zr == 0.0 && zi == 0.0)
	{
	  yr = -octave_Inf;
	  yi = 0.0;
	}
      else
	{
	  F77_FUNC (zbesy, ZBESY) (zr, zi, alpha, kode, 1, &yr, &yi, nz,
				  &wr, &wi, ierr);

	  if (zi == 0.0 && zr >= 0.0)
	    yi = 0.0;
	}

      return bessel_return_value (Complex (yr, yi), ierr);
    }
  else
    {
      alpha = -alpha;

      Complex tmp = cos (M_PI * alpha) * zbesy (z, alpha, kode, ierr);

      if (ierr == 0 || ierr == 3)
	{
	  tmp += sin (M_PI * alpha) * zbesj (z, alpha, kode, ierr);

	  retval = bessel_return_value (tmp, ierr);
	}
      else
	retval = Complex (octave_NaN, octave_NaN);
    }

  return retval;
}

static inline Complex
zbesi (const Complex& z, double alpha, int kode, int& ierr)
{
  Complex retval;

  if (alpha >= 0.0)
    {
      double yr = 0.0;
      double yi = 0.0;

      int nz;

      double zr = z.real ();
      double zi = z.imag ();

      F77_FUNC (zbesi, ZBESI) (zr, zi, alpha, kode, 1, &yr, &yi, nz, ierr);

      if (zi == 0.0 && zr >= 0.0)
	yi = 0.0;

      retval = bessel_return_value (Complex (yr, yi), ierr);
    }
  else
    {
      alpha = -alpha;

      Complex tmp = zbesi (z, alpha, kode, ierr);

      if (ierr == 0 || ierr == 3)
	{
	  tmp += (2.0 / M_PI) * sin (M_PI * alpha)
	    * zbesk (z, alpha, kode, ierr);

	  retval = bessel_return_value (tmp, ierr);
	}
      else
	retval = Complex (octave_NaN, octave_NaN);
    }

  return retval;
}

static inline Complex
zbesk (const Complex& z, double alpha, int kode, int& ierr)
{
  Complex retval;

  if (alpha >= 0.0)
    {
      double yr = 0.0;
      double yi = 0.0;

      int nz;

      double zr = z.real ();
      double zi = z.imag ();

      ierr = 0;

      if (zr == 0.0 && zi == 0.0)
	{
	  yr = octave_Inf;
	  yi = 0.0;
	}
      else
	{
	  F77_FUNC (zbesk, ZBESK) (zr, zi, alpha, kode, 1, &yr, &yi, nz, ierr);

	  if (zi == 0.0 && zr >= 0.0)
	    yi = 0.0;
	}

      retval = bessel_return_value (Complex (yr, yi), ierr);
    }
  else
    {
      Complex tmp = zbesk (z, -alpha, kode, ierr);

      retval = bessel_return_value (tmp, ierr);
    }

  return retval;
}

static inline Complex
zbesh1 (const Complex& z, double alpha, int kode, int& ierr)
{
  Complex retval;

  if (alpha >= 0.0)
    {
      double yr = 0.0;
      double yi = 0.0;

      int nz;

      double zr = z.real ();
      double zi = z.imag ();

      F77_FUNC (zbesh, ZBESH) (zr, zi, alpha, kode, 1, 1, &yr, &yi, nz, ierr);

      retval = bessel_return_value (Complex (yr, yi), ierr);
    }
  else
    {
      alpha = -alpha;

      static const Complex eye = Complex (0.0, 1.0);

      Complex tmp = exp (M_PI * alpha * eye) * zbesh1 (z, alpha, kode, ierr);

      retval = bessel_return_value (tmp, ierr);
    }

  return retval;
}

static inline Complex
zbesh2 (const Complex& z, double alpha, int kode, int& ierr)
{
  Complex retval;

  if (alpha >= 0.0)
    {
      double yr = 0.0;
      double yi = 0.0;

      int nz;

      double zr = z.real ();
      double zi = z.imag ();

      F77_FUNC (zbesh, ZBESH) (zr, zi, alpha, kode, 2, 1, &yr, &yi, nz, ierr);

      retval = bessel_return_value (Complex (yr, yi), ierr);
    }
  else
    {
      alpha = -alpha;

      static const Complex eye = Complex (0.0, 1.0);

      Complex tmp = exp (-M_PI * alpha * eye) * zbesh2 (z, alpha, kode, ierr);

      retval = bessel_return_value (tmp, ierr);
    }

  return retval;
}

typedef Complex (*fptr) (const Complex&, double, int, int&);

static inline Complex
do_bessel (fptr f, const char *, double alpha, const Complex& x,
	   bool scaled, int& ierr)
{
  Complex retval;

  retval = f (x, alpha, (scaled ? 2 : 1), ierr);

  return retval;
}

static inline ComplexMatrix
do_bessel (fptr f, const char *, double alpha, const ComplexMatrix& x,
	   bool scaled, Array2<int>& ierr)
{
  int nr = x.rows ();
  int nc = x.cols ();

  ComplexMatrix retval (nr, nc);

  ierr.resize (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      retval(i,j) = f (x(i,j), alpha, (scaled ? 2 : 1), ierr(i,j));

  return retval;
}

static inline ComplexMatrix
do_bessel (fptr f, const char *, const Matrix& alpha, const Complex& x,
	   bool scaled, Array2<int>& ierr)
{
  int nr = alpha.rows ();
  int nc = alpha.cols ();

  ComplexMatrix retval (nr, nc);

  ierr.resize (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      retval(i,j) = f (x, alpha(i,j), (scaled ? 2 : 1), ierr(i,j));

  return retval;
}

static inline ComplexMatrix
do_bessel (fptr f, const char *fn, const Matrix& alpha,
	   const ComplexMatrix& x, bool scaled, Array2<int>& ierr)
{
  ComplexMatrix retval;

  int x_nr = x.rows ();
  int x_nc = x.cols ();

  int alpha_nr = alpha.rows ();
  int alpha_nc = alpha.cols ();

  if (x_nr == alpha_nr && x_nc == alpha_nc)
    {
      int nr = x_nr;
      int nc = x_nc;

      retval.resize (nr, nc);

      ierr.resize (nr, nc);

      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  retval(i,j) = f (x(i,j), alpha(i,j), (scaled ? 2 : 1), ierr(i,j));
    }
  else
    (*current_liboctave_error_handler)
      ("%s: the sizes of alpha and x must conform", fn);

  return retval;
}

static inline ComplexMatrix
do_bessel (fptr f, const char *, const RowVector& alpha,
	   const ComplexColumnVector& x, bool scaled, Array2<int>& ierr)
{
  int nr = x.length ();
  int nc = alpha.length ();

  ComplexMatrix retval (nr, nc);

  ierr.resize (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      retval(i,j) = f (x(i), alpha(j), (scaled ? 2 : 1), ierr(i,j));

  return retval;
}

#define SS_BESSEL(name, fcn) \
  Complex \
  name (double alpha, const Complex& x, bool scaled, int& ierr) \
  { \
    return do_bessel (fcn, #name, alpha, x, scaled, ierr); \
  }

#define SM_BESSEL(name, fcn) \
  ComplexMatrix \
  name (double alpha, const ComplexMatrix& x, bool scaled, \
	Array2<int>& ierr) \
  { \
    return do_bessel (fcn, #name, alpha, x, scaled, ierr); \
  }

#define MS_BESSEL(name, fcn) \
  ComplexMatrix \
  name (const Matrix& alpha, const Complex& x, bool scaled, \
	Array2<int>& ierr) \
  { \
    return do_bessel (fcn, #name, alpha, x, scaled, ierr); \
  }

#define MM_BESSEL(name, fcn) \
  ComplexMatrix \
  name (const Matrix& alpha, const ComplexMatrix& x, bool scaled, \
	Array2<int>& ierr) \
  { \
    return do_bessel (fcn, #name, alpha, x, scaled, ierr); \
  }

#define RC_BESSEL(name, fcn) \
  ComplexMatrix \
  name (const RowVector& alpha, const ComplexColumnVector& x, bool scaled, \
        Array2<int>& ierr) \
  { \
    return do_bessel (fcn, #name, alpha, x, scaled, ierr); \
  }

#define ALL_BESSEL(name, fcn) \
  SS_BESSEL (name, fcn) \
  SM_BESSEL (name, fcn) \
  MS_BESSEL (name, fcn) \
  MM_BESSEL (name, fcn) \
  RC_BESSEL (name, fcn)

ALL_BESSEL (besselj, zbesj)
ALL_BESSEL (bessely, zbesy)
ALL_BESSEL (besseli, zbesi)
ALL_BESSEL (besselk, zbesk)
ALL_BESSEL (besselh1, zbesh1)
ALL_BESSEL (besselh2, zbesh2)

Complex
airy (const Complex& z, bool deriv, bool scaled, int& ierr)
{
  double ar = 0.0;
  double ai = 0.0;

  int nz;

  double zr = z.real ();
  double zi = z.imag ();

  int id = deriv ? 1 : 0;

  int kode = scaled ? 2 : 1;

  F77_FUNC (zairy, ZAIRY) (zr, zi, id, kode, ar, ai, nz, ierr);

  if (zi == 0.0 && (! scaled || zr >= 0.0))
    ai = 0.0;

  return bessel_return_value (Complex (ar, ai), ierr);
}

Complex
biry (const Complex& z, bool deriv, bool scaled, int& ierr)
{
  double ar = 0.0;
  double ai = 0.0;

  double zr = z.real ();
  double zi = z.imag ();

  int id = deriv ? 1 : 0;

  int kode = scaled ? 2 : 1;

  F77_FUNC (zbiry, ZBIRY) (zr, zi, id, kode, ar, ai, ierr);

  if (zi == 0.0 && (! scaled || zr >= 0.0))
    ai = 0.0;

  return bessel_return_value (Complex (ar, ai), ierr);
}

ComplexMatrix
airy (const ComplexMatrix& z, bool deriv, bool scaled, Array2<int>& ierr)
{
  int nr = z.rows ();
  int nc = z.cols ();

  ComplexMatrix retval (nr, nc);

  ierr.resize (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      retval(i,j) = airy (z(i,j), deriv, scaled, ierr(i,j));

  return retval;
}

ComplexMatrix
biry (const ComplexMatrix& z, bool deriv, bool scaled, Array2<int>& ierr)
{
  int nr = z.rows ();
  int nc = z.cols ();

  ComplexMatrix retval (nr, nc);

  ierr.resize (nr, nc);

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      retval(i,j) = biry (z(i,j), deriv, scaled, ierr(i,j));

  return retval;
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
  F77_FUNC (xdbetai, XDBETAI) (x, a, b, retval);
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

// XXX FIXME XXX -- there is still room for improvement here...

double
gammainc (double x, double a, bool& err)
{
  double retval;

  err = false;

  if (a < 0.0 || x < 0.0)
    {
      (*current_liboctave_error_handler)
	("gammainc: A and X must be non-negative");

      err = true;
    }
  else
    F77_FUNC (xgammainc, XGAMMAINC) (a, x, retval);

  return retval;
}

Matrix
gammainc (double x, const Matrix& a)
{
  int nr = a.rows ();
  int nc = a.cols ();

  Matrix result (nr, nc);
  Matrix retval;

  bool err;

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	result(i,j) = gammainc (x, a(i,j), err);

	if (err)
	  goto done;
      }

  retval = result;

 done:

  return retval;
}

Matrix
gammainc (const Matrix& x, double a)
{
  int nr = x.rows ();
  int nc = x.cols ();

  Matrix result (nr, nc);
  Matrix retval;

  bool err;

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      {
	result(i,j) = gammainc (x(i,j), a, err);

	if (err)
	  goto done;
      }

  retval = result;

 done:

  return retval;
}

Matrix
gammainc (const Matrix& x, const Matrix& a)
{
  Matrix result;
  Matrix retval;

  int nr = x.rows ();
  int nc = x.cols ();

  int a_nr = a.rows ();
  int a_nc = a.cols ();

  if (nr == a_nr && nc == a_nc)
    {
      result.resize (nr, nc);

      bool err;

      for (int j = 0; j < nc; j++)
	for (int i = 0; i < nr; i++)
	  {
	    result(i,j) = gammainc (x(i,j), a(i,j), err);

	    if (err)
	      goto done;
	  }

      retval = result;
    }
  else
    (*current_liboctave_error_handler)
      ("gammainc: nonconformant arguments (arg 1 is %dx%d, arg 2 is %dx%d)",
       nr, nc, a_nr, a_nc);

 done:

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
