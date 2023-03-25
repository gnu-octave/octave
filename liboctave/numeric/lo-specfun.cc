////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cmath>

#include <algorithm>
#include <limits>
#include <string>

#include "CColVector.h"
#include "CMatrix.h"
#include "CNDArray.h"
#include "Faddeeva.hh"
#include "dMatrix.h"
#include "dNDArray.h"
#include "dRowVector.h"
#include "f77-fcn.h"
#include "fCColVector.h"
#include "fCMatrix.h"
#include "fCNDArray.h"
#include "fMatrix.h"
#include "fNDArray.h"
#include "fRowVector.h"
#include "lo-amos-proto.h"
#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "lo-slatec-proto.h"
#include "lo-specfun.h"
#include "mx-inlines.cc"

OCTAVE_BEGIN_NAMESPACE(octave)

OCTAVE_BEGIN_NAMESPACE(math)

static inline Complex
bessel_return_value (const Complex& val, octave_idx_type ierr)
{
  static const Complex inf_val
    = Complex (numeric_limits<double>::Inf (),
               numeric_limits<double>::Inf ());

  static const Complex nan_val
    = Complex (numeric_limits<double>::NaN (),
               numeric_limits<double>::NaN ());

  Complex retval;

  switch (ierr)
    {
    case 0:
    case 3:
    case 4:
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

static inline FloatComplex
bessel_return_value (const FloatComplex& val, octave_idx_type ierr)
{
  static const FloatComplex inf_val
    = FloatComplex (numeric_limits<float>::Inf (),
                    numeric_limits<float>::Inf ());

  static const FloatComplex nan_val
    = FloatComplex (numeric_limits<float>::NaN (),
                    numeric_limits<float>::NaN ());

  FloatComplex retval;

  switch (ierr)
    {
    case 0:
    case 3:
    case 4:
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

Complex
airy (const Complex& z, bool deriv, bool scaled, octave_idx_type& ierr)
{
  double ar = 0.0;
  double ai = 0.0;

  double zr = z.real ();
  double zi = z.imag ();

  F77_INT id = (deriv ? 1 : 0);
  F77_INT nz, t_ierr;
  F77_INT sc = (scaled ? 2 : 1);

  F77_FUNC (zairy, ZAIRY) (zr, zi, id, sc, ar, ai, nz, t_ierr);

  ierr = t_ierr;

  if (zi == 0.0 && (! scaled || zr >= 0.0))
    ai = 0.0;

  return bessel_return_value (Complex (ar, ai), ierr);
}

ComplexMatrix
airy (const ComplexMatrix& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr)
{
  octave_idx_type nr = z.rows ();
  octave_idx_type nc = z.cols ();

  ComplexMatrix retval (nr, nc);

  ierr.resize (dim_vector (nr, nc));

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      retval(i, j) = airy (z(i, j), deriv, scaled, ierr(i, j));

  return retval;
}

ComplexNDArray
airy (const ComplexNDArray& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr)
{
  dim_vector dv = z.dims ();
  octave_idx_type nel = dv.numel ();
  ComplexNDArray retval (dv);

  ierr.resize (dv);

  for (octave_idx_type i = 0; i < nel; i++)
    retval(i) = airy (z(i), deriv, scaled, ierr(i));

  return retval;
}

FloatComplex
airy (const FloatComplex& z, bool deriv, bool scaled,
      octave_idx_type& ierr)
{
  FloatComplex a;

  F77_INT id = (deriv ? 1 : 0);
  F77_INT nz, t_ierr;
  F77_INT sc = (scaled ? 2 : 1);

  F77_FUNC (cairy, CAIRY) (F77_CONST_CMPLX_ARG (&z), id, sc,
                           F77_CMPLX_ARG (&a), nz, t_ierr);

  ierr = t_ierr;

  float ar = a.real ();
  float ai = a.imag ();

  if (z.imag () == 0.0 && (! scaled || z.real () >= 0.0))
    ai = 0.0;

  return bessel_return_value (FloatComplex (ar, ai), ierr);
}

FloatComplexMatrix
airy (const FloatComplexMatrix& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr)
{
  octave_idx_type nr = z.rows ();
  octave_idx_type nc = z.cols ();

  FloatComplexMatrix retval (nr, nc);

  ierr.resize (dim_vector (nr, nc));

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      retval(i, j) = airy (z(i, j), deriv, scaled, ierr(i, j));

  return retval;
}

FloatComplexNDArray
airy (const FloatComplexNDArray& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr)
{
  dim_vector dv = z.dims ();
  octave_idx_type nel = dv.numel ();
  FloatComplexNDArray retval (dv);

  ierr.resize (dv);

  for (octave_idx_type i = 0; i < nel; i++)
    retval(i) = airy (z(i), deriv, scaled, ierr(i));

  return retval;
}

static inline bool
is_integer_value (double x)
{
  return x == static_cast<double> (static_cast<long> (x));
}

static inline Complex
zbesj (const Complex& z, double alpha, int kode, octave_idx_type& ierr);

static inline Complex
zbesy (const Complex& z, double alpha, int kode, octave_idx_type& ierr);

static inline Complex
zbesi (const Complex& z, double alpha, int kode, octave_idx_type& ierr);

static inline Complex
zbesk (const Complex& z, double alpha, int kode, octave_idx_type& ierr);

static inline Complex
zbesh1 (const Complex& z, double alpha, int kode, octave_idx_type& ierr);

static inline Complex
zbesh2 (const Complex& z, double alpha, int kode, octave_idx_type& ierr);

static inline Complex
zbesj (const Complex& z, double alpha, int kode, octave_idx_type& ierr)
{
  Complex retval;

  if (alpha >= 0.0)
    {
      double yr = 0.0;
      double yi = 0.0;

      F77_INT nz, t_ierr;

      double zr = z.real ();
      double zi = z.imag ();

      F77_FUNC (zbesj, ZBESJ) (zr, zi, alpha, kode, 1, &yr, &yi, nz, t_ierr);

      ierr = t_ierr;

      if (zi == 0.0 && zr >= 0.0)
        yi = 0.0;

      retval = bessel_return_value (Complex (yr, yi), ierr);
    }
  else if (is_integer_value (alpha))
    {
      // zbesy can overflow as z->0, and cause troubles for generic case below
      alpha = -alpha;
      Complex tmp = zbesj (z, alpha, kode, ierr);
      if ((static_cast<long> (alpha)) & 1)
        tmp = - tmp;
      retval = bessel_return_value (tmp, ierr);
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
        retval = Complex (numeric_limits<double>::NaN (),
                          numeric_limits<double>::NaN ());
    }

  return retval;
}

static inline Complex
zbesy (const Complex& z, double alpha, int kode, octave_idx_type& ierr)
{
  Complex retval;

  if (alpha >= 0.0)
    {
      double yr = 0.0;
      double yi = 0.0;

      F77_INT nz, t_ierr;

      double wr, wi;

      double zr = z.real ();
      double zi = z.imag ();

      ierr = 0;

      if (zr == 0.0 && zi == 0.0)
        {
          yr = -numeric_limits<double>::Inf ();
          yi = 0.0;
        }
      else
        {
          F77_FUNC (zbesy, ZBESY) (zr, zi, alpha, kode, 1, &yr, &yi, nz,
                                   &wr, &wi, t_ierr);

          ierr = t_ierr;

          if (zi == 0.0 && zr >= 0.0)
            yi = 0.0;
        }

      return bessel_return_value (Complex (yr, yi), ierr);
    }
  else if (is_integer_value (alpha - 0.5))
    {
      // zbesy can overflow as z->0, and cause troubles for generic case below
      alpha = -alpha;
      Complex tmp = zbesj (z, alpha, kode, ierr);
      if ((static_cast<long> (alpha - 0.5)) & 1)
        tmp = - tmp;
      retval = bessel_return_value (tmp, ierr);
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
        retval = Complex (numeric_limits<double>::NaN (),
                          numeric_limits<double>::NaN ());
    }

  return retval;
}

static inline Complex
zbesi (const Complex& z, double alpha, int kode, octave_idx_type& ierr)
{
  Complex retval;

  if (alpha >= 0.0)
    {
      double yr = 0.0;
      double yi = 0.0;

      F77_INT nz, t_ierr;

      double zr = z.real ();
      double zi = z.imag ();

      F77_FUNC (zbesi, ZBESI) (zr, zi, alpha, kode, 1, &yr, &yi, nz, t_ierr);

      ierr = t_ierr;

      if (zi == 0.0 && zr >= 0.0)
        yi = 0.0;

      retval = bessel_return_value (Complex (yr, yi), ierr);
    }
  else if (is_integer_value (alpha))
    {
      // zbesi can overflow as z->0, and cause troubles for generic case below
      alpha = -alpha;
      Complex tmp = zbesi (z, alpha, kode, ierr);
      retval = bessel_return_value (tmp, ierr);
    }
  else
    {
      alpha = -alpha;

      Complex tmp = zbesi (z, alpha, kode, ierr);

      if (ierr == 0 || ierr == 3)
        {
          Complex tmp2 = (2.0 / M_PI) * sin (M_PI * alpha)
                         * zbesk (z, alpha, kode, ierr);

          if (kode == 2)
            {
              // Compensate for different scaling factor of besk.
              tmp2 *= exp (-z - std::abs (z.real ()));
            }

          tmp += tmp2;

          retval = bessel_return_value (tmp, ierr);
        }
      else
        retval = Complex (numeric_limits<double>::NaN (),
                          numeric_limits<double>::NaN ());
    }

  return retval;
}

static inline Complex
zbesk (const Complex& z, double alpha, int kode, octave_idx_type& ierr)
{
  Complex retval;

  if (alpha >= 0.0)
    {
      double yr = 0.0;
      double yi = 0.0;

      F77_INT nz, t_ierr;

      double zr = z.real ();
      double zi = z.imag ();

      ierr = 0;

      if (zr == 0.0 && zi == 0.0)
        {
          yr = numeric_limits<double>::Inf ();
          yi = 0.0;
        }
      else
        {
          F77_FUNC (zbesk, ZBESK) (zr, zi, alpha, kode, 1, &yr, &yi, nz,
                                   t_ierr);

          ierr = t_ierr;

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
zbesh1 (const Complex& z, double alpha, int kode, octave_idx_type& ierr)
{
  Complex retval;

  if (alpha >= 0.0)
    {
      double yr = 0.0;
      double yi = 0.0;

      F77_INT nz, t_ierr;

      double zr = z.real ();
      double zi = z.imag ();

      F77_FUNC (zbesh, ZBESH) (zr, zi, alpha, kode, 1, 1, &yr, &yi, nz,
                               t_ierr);

      ierr = t_ierr;

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
zbesh2 (const Complex& z, double alpha, int kode, octave_idx_type& ierr)
{
  Complex retval;

  if (alpha >= 0.0)
    {
      double yr = 0.0;
      double yi = 0.0;

      F77_INT nz, t_ierr;

      double zr = z.real ();
      double zi = z.imag ();

      F77_FUNC (zbesh, ZBESH) (zr, zi, alpha, kode, 2, 1, &yr, &yi, nz,
                               t_ierr);

      ierr = t_ierr;

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

typedef Complex (*dptr) (const Complex&, double, int, octave_idx_type&);

static inline Complex
do_bessel (dptr f, const char *, double alpha, const Complex& x,
           bool scaled, octave_idx_type& ierr)
{
  Complex retval;

  retval = f (x, alpha, (scaled ? 2 : 1), ierr);

  return retval;
}

static inline ComplexMatrix
do_bessel (dptr f, const char *, double alpha, const ComplexMatrix& x,
           bool scaled, Array<octave_idx_type>& ierr)
{
  octave_idx_type nr = x.rows ();
  octave_idx_type nc = x.cols ();

  ComplexMatrix retval (nr, nc);

  ierr.resize (dim_vector (nr, nc));

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      retval(i, j) = f (x(i, j), alpha, (scaled ? 2 : 1), ierr(i, j));

  return retval;
}

static inline ComplexMatrix
do_bessel (dptr f, const char *, const Matrix& alpha, const Complex& x,
           bool scaled, Array<octave_idx_type>& ierr)
{
  octave_idx_type nr = alpha.rows ();
  octave_idx_type nc = alpha.cols ();

  ComplexMatrix retval (nr, nc);

  ierr.resize (dim_vector (nr, nc));

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      retval(i, j) = f (x, alpha(i, j), (scaled ? 2 : 1), ierr(i, j));

  return retval;
}

static inline ComplexMatrix
do_bessel (dptr f, const char *fn, const Matrix& alpha,
           const ComplexMatrix& x, bool scaled, Array<octave_idx_type>& ierr)
{
  ComplexMatrix retval;

  octave_idx_type x_nr = x.rows ();
  octave_idx_type x_nc = x.cols ();

  octave_idx_type alpha_nr = alpha.rows ();
  octave_idx_type alpha_nc = alpha.cols ();

  if (x_nr != alpha_nr || x_nc != alpha_nc)
    (*current_liboctave_error_handler)
      ("%s: the sizes of alpha and x must conform", fn);

  octave_idx_type nr = x_nr;
  octave_idx_type nc = x_nc;

  retval.resize (nr, nc);

  ierr.resize (dim_vector (nr, nc));

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      retval(i, j) = f (x(i, j), alpha(i, j), (scaled ? 2 : 1), ierr(i, j));

  return retval;
}

static inline ComplexNDArray
do_bessel (dptr f, const char *, double alpha, const ComplexNDArray& x,
           bool scaled, Array<octave_idx_type>& ierr)
{
  dim_vector dv = x.dims ();
  octave_idx_type nel = dv.numel ();
  ComplexNDArray retval (dv);

  ierr.resize (dv);

  for (octave_idx_type i = 0; i < nel; i++)
    retval(i) = f (x(i), alpha, (scaled ? 2 : 1), ierr(i));

  return retval;
}

static inline ComplexNDArray
do_bessel (dptr f, const char *, const NDArray& alpha, const Complex& x,
           bool scaled, Array<octave_idx_type>& ierr)
{
  dim_vector dv = alpha.dims ();
  octave_idx_type nel = dv.numel ();
  ComplexNDArray retval (dv);

  ierr.resize (dv);

  for (octave_idx_type i = 0; i < nel; i++)
    retval(i) = f (x, alpha(i), (scaled ? 2 : 1), ierr(i));

  return retval;
}

static inline ComplexNDArray
do_bessel (dptr f, const char *fn, const NDArray& alpha,
           const ComplexNDArray& x, bool scaled, Array<octave_idx_type>& ierr)
{
  dim_vector dv = x.dims ();
  ComplexNDArray retval;

  if (dv != alpha.dims ())
    (*current_liboctave_error_handler)
      ("%s: the sizes of alpha and x must conform", fn);

  octave_idx_type nel = dv.numel ();

  retval.resize (dv);
  ierr.resize (dv);

  for (octave_idx_type i = 0; i < nel; i++)
    retval(i) = f (x(i), alpha(i), (scaled ? 2 : 1), ierr(i));

  return retval;
}

static inline ComplexMatrix
do_bessel (dptr f, const char *, const RowVector& alpha,
           const ComplexColumnVector& x, bool scaled,
           Array<octave_idx_type>& ierr)
{
  octave_idx_type nr = x.numel ();
  octave_idx_type nc = alpha.numel ();

  ComplexMatrix retval (nr, nc);

  ierr.resize (dim_vector (nr, nc));

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      retval(i, j) = f (x(i), alpha(j), (scaled ? 2 : 1), ierr(i, j));

  return retval;
}

#define SS_BESSEL(name, fcn)                                            \
    Complex                                                             \
    name (double alpha, const Complex& x, bool scaled, octave_idx_type& ierr) \
    {                                                                   \
      return do_bessel (fcn, #name, alpha, x, scaled, ierr);            \
    }

#define SM_BESSEL(name, fcn)                                    \
    ComplexMatrix                                               \
    name (double alpha, const ComplexMatrix& x, bool scaled,    \
          Array<octave_idx_type>& ierr)                         \
    {                                                           \
      return do_bessel (fcn, #name, alpha, x, scaled, ierr);    \
    }

#define MS_BESSEL(name, fcn)                                    \
    ComplexMatrix                                               \
    name (const Matrix& alpha, const Complex& x, bool scaled,   \
          Array<octave_idx_type>& ierr)                         \
    {                                                           \
      return do_bessel (fcn, #name, alpha, x, scaled, ierr);    \
    }

#define MM_BESSEL(name, fcn)                                            \
    ComplexMatrix                                                       \
    name (const Matrix& alpha, const ComplexMatrix& x, bool scaled,     \
          Array<octave_idx_type>& ierr)                                 \
    {                                                                   \
      return do_bessel (fcn, #name, alpha, x, scaled, ierr);            \
    }

#define SN_BESSEL(name, fcn)                                    \
    ComplexNDArray                                              \
    name (double alpha, const ComplexNDArray& x, bool scaled,   \
          Array<octave_idx_type>& ierr)                         \
    {                                                           \
      return do_bessel (fcn, #name, alpha, x, scaled, ierr);    \
    }

#define NS_BESSEL(name, fcn)                                    \
    ComplexNDArray                                              \
    name (const NDArray& alpha, const Complex& x, bool scaled,  \
          Array<octave_idx_type>& ierr)                         \
    {                                                           \
      return do_bessel (fcn, #name, alpha, x, scaled, ierr);    \
    }

#define NN_BESSEL(name, fcn)                                            \
    ComplexNDArray                                                      \
    name (const NDArray& alpha, const ComplexNDArray& x, bool scaled,   \
          Array<octave_idx_type>& ierr)                                 \
    {                                                                   \
      return do_bessel (fcn, #name, alpha, x, scaled, ierr);            \
    }

#define RC_BESSEL(name, fcn)                                            \
    ComplexMatrix                                                       \
    name (const RowVector& alpha, const ComplexColumnVector& x, bool scaled, \
          Array<octave_idx_type>& ierr)                                 \
    {                                                                   \
      return do_bessel (fcn, #name, alpha, x, scaled, ierr);            \
    }

#define ALL_BESSEL(name, fcn)                   \
    SS_BESSEL (name, fcn)                       \
    SM_BESSEL (name, fcn)                       \
    MS_BESSEL (name, fcn)                       \
    MM_BESSEL (name, fcn)                       \
    SN_BESSEL (name, fcn)                       \
    NS_BESSEL (name, fcn)                       \
    NN_BESSEL (name, fcn)                       \
    RC_BESSEL (name, fcn)

ALL_BESSEL (besselj, zbesj)
ALL_BESSEL (bessely, zbesy)
ALL_BESSEL (besseli, zbesi)
ALL_BESSEL (besselk, zbesk)
ALL_BESSEL (besselh1, zbesh1)
ALL_BESSEL (besselh2, zbesh2)

#undef ALL_BESSEL
#undef SS_BESSEL
#undef SM_BESSEL
#undef MS_BESSEL
#undef MM_BESSEL
#undef SN_BESSEL
#undef NS_BESSEL
#undef NN_BESSEL
#undef RC_BESSEL

static inline FloatComplex
cbesj (const FloatComplex& z, float alpha, int kode, octave_idx_type& ierr);

static inline FloatComplex
cbesy (const FloatComplex& z, float alpha, int kode, octave_idx_type& ierr);

static inline FloatComplex
cbesi (const FloatComplex& z, float alpha, int kode, octave_idx_type& ierr);

static inline FloatComplex
cbesk (const FloatComplex& z, float alpha, int kode, octave_idx_type& ierr);

static inline FloatComplex
cbesh1 (const FloatComplex& z, float alpha, int kode, octave_idx_type& ierr);

static inline FloatComplex
cbesh2 (const FloatComplex& z, float alpha, int kode, octave_idx_type& ierr);

static inline bool
is_integer_value (float x)
{
  return x == static_cast<float> (static_cast<long> (x));
}

static inline FloatComplex
cbesj (const FloatComplex& z, float alpha, int kode, octave_idx_type& ierr)
{
  FloatComplex retval;

  if (alpha >= 0.0)
    {
      FloatComplex y = 0.0;

      F77_INT nz, t_ierr;

      F77_FUNC (cbesj, CBESJ) (F77_CONST_CMPLX_ARG (&z), alpha, kode, 1,
                               F77_CMPLX_ARG (&y), nz, t_ierr);

      ierr = t_ierr;

      if (z.imag () == 0.0 && z.real () >= 0.0)
        y = FloatComplex (y.real (), 0.0);

      retval = bessel_return_value (y, ierr);
    }
  else if (is_integer_value (alpha))
    {
      // zbesy can overflow as z->0, and cause troubles for generic case below
      alpha = -alpha;
      FloatComplex tmp = cbesj (z, alpha, kode, ierr);
      if ((static_cast<long> (alpha)) & 1)
        tmp = - tmp;
      retval = bessel_return_value (tmp, ierr);
    }
  else
    {
      alpha = -alpha;

      FloatComplex tmp = cosf (static_cast<float> (M_PI) * alpha)
                         * cbesj (z, alpha, kode, ierr);

      if (ierr == 0 || ierr == 3)
        {
          tmp -= sinf (static_cast<float> (M_PI) * alpha)
                 * cbesy (z, alpha, kode, ierr);

          retval = bessel_return_value (tmp, ierr);
        }
      else
        retval = FloatComplex (numeric_limits<float>::NaN (),
                               numeric_limits<float>::NaN ());
    }

  return retval;
}

static inline FloatComplex
cbesy (const FloatComplex& z, float alpha, int kode, octave_idx_type& ierr)
{
  FloatComplex retval;

  if (alpha >= 0.0)
    {
      FloatComplex y = 0.0;

      F77_INT nz, t_ierr;

      FloatComplex w;

      ierr = 0;

      if (z.real () == 0.0 && z.imag () == 0.0)
        {
          y = FloatComplex (-numeric_limits<float>::Inf (), 0.0);
        }
      else
        {
          F77_FUNC (cbesy, CBESY) (F77_CONST_CMPLX_ARG (&z), alpha, kode, 1,
                                   F77_CMPLX_ARG (&y), nz,
                                   F77_CMPLX_ARG (&w), t_ierr);

          ierr = t_ierr;

          if (z.imag () == 0.0 && z.real () >= 0.0)
            y = FloatComplex (y.real (), 0.0);
        }

      return bessel_return_value (y, ierr);
    }
  else if (is_integer_value (alpha - 0.5))
    {
      // zbesy can overflow as z->0, and cause troubles for generic case below
      alpha = -alpha;
      FloatComplex tmp = cbesj (z, alpha, kode, ierr);
      if ((static_cast<long> (alpha - 0.5)) & 1)
        tmp = - tmp;
      retval = bessel_return_value (tmp, ierr);
    }
  else
    {
      alpha = -alpha;

      FloatComplex tmp = cosf (static_cast<float> (M_PI) * alpha)
                         * cbesy (z, alpha, kode, ierr);

      if (ierr == 0 || ierr == 3)
        {
          tmp += sinf (static_cast<float> (M_PI) * alpha)
                 * cbesj (z, alpha, kode, ierr);

          retval = bessel_return_value (tmp, ierr);
        }
      else
        retval = FloatComplex (numeric_limits<float>::NaN (),
                               numeric_limits<float>::NaN ());
    }

  return retval;
}

static inline FloatComplex
cbesi (const FloatComplex& z, float alpha, int kode, octave_idx_type& ierr)
{
  FloatComplex retval;

  if (alpha >= 0.0)
    {
      FloatComplex y = 0.0;

      F77_INT nz, t_ierr;

      F77_FUNC (cbesi, CBESI) (F77_CONST_CMPLX_ARG (&z), alpha, kode, 1,
                               F77_CMPLX_ARG (&y), nz, t_ierr);

      ierr = t_ierr;

      if (z.imag () == 0.0 && z.real () >= 0.0)
        y = FloatComplex (y.real (), 0.0);

      retval = bessel_return_value (y, ierr);
    }
  else
    {
      alpha = -alpha;

      FloatComplex tmp = cbesi (z, alpha, kode, ierr);

      if (ierr == 0 || ierr == 3)
        {
          FloatComplex tmp2 = static_cast<float> (2.0 / M_PI)
                              * sinf (static_cast<float> (M_PI) * alpha)
                              * cbesk (z, alpha, kode, ierr);

          if (kode == 2)
            {
              // Compensate for different scaling factor of besk.
              tmp2 *= exp (-z - std::abs (z.real ()));
            }

          tmp += tmp2;

          retval = bessel_return_value (tmp, ierr);
        }
      else
        retval = FloatComplex (numeric_limits<float>::NaN (),
                               numeric_limits<float>::NaN ());
    }

  return retval;
}

static inline FloatComplex
cbesk (const FloatComplex& z, float alpha, int kode, octave_idx_type& ierr)
{
  FloatComplex retval;

  if (alpha >= 0.0)
    {
      FloatComplex y = 0.0;

      F77_INT nz, t_ierr;

      ierr = 0;

      if (z.real () == 0.0 && z.imag () == 0.0)
        {
          y = FloatComplex (numeric_limits<float>::Inf (), 0.0);
        }
      else
        {
          F77_FUNC (cbesk, CBESK) (F77_CONST_CMPLX_ARG (&z), alpha, kode, 1,
                                   F77_CMPLX_ARG (&y), nz, t_ierr);

          ierr = t_ierr;

          if (z.imag () == 0.0 && z.real () >= 0.0)
            y = FloatComplex (y.real (), 0.0);
        }

      retval = bessel_return_value (y, ierr);
    }
  else
    {
      FloatComplex tmp = cbesk (z, -alpha, kode, ierr);

      retval = bessel_return_value (tmp, ierr);
    }

  return retval;
}

static inline FloatComplex
cbesh1 (const FloatComplex& z, float alpha, int kode, octave_idx_type& ierr)
{
  FloatComplex retval;

  if (alpha >= 0.0)
    {
      FloatComplex y = 0.0;

      F77_INT nz, t_ierr;

      F77_FUNC (cbesh, CBESH) (F77_CONST_CMPLX_ARG (&z), alpha, kode, 1, 1,
                               F77_CMPLX_ARG (&y), nz, t_ierr);

      ierr = t_ierr;

      retval = bessel_return_value (y, ierr);
    }
  else
    {
      alpha = -alpha;

      static const FloatComplex eye = FloatComplex (0.0, 1.0);

      FloatComplex tmp = exp (static_cast<float> (M_PI) * alpha * eye)
                         * cbesh1 (z, alpha, kode, ierr);

      retval = bessel_return_value (tmp, ierr);
    }

  return retval;
}

static inline FloatComplex
cbesh2 (const FloatComplex& z, float alpha, int kode, octave_idx_type& ierr)
{
  FloatComplex retval;

  if (alpha >= 0.0)
    {
      FloatComplex y = 0.0;

      F77_INT nz, t_ierr;

      F77_FUNC (cbesh, CBESH) (F77_CONST_CMPLX_ARG (&z), alpha, kode, 2, 1,
                               F77_CMPLX_ARG (&y), nz, t_ierr);

      ierr = t_ierr;

      retval = bessel_return_value (y, ierr);
    }
  else
    {
      alpha = -alpha;

      static const FloatComplex eye = FloatComplex (0.0, 1.0);

      FloatComplex tmp = exp (-static_cast<float> (M_PI) * alpha * eye)
                         * cbesh2 (z, alpha, kode, ierr);

      retval = bessel_return_value (tmp, ierr);
    }

  return retval;
}

typedef FloatComplex (*fptr) (const FloatComplex&, float, int,
                              octave_idx_type&);

static inline FloatComplex
do_bessel (fptr f, const char *, float alpha, const FloatComplex& x,
           bool scaled, octave_idx_type& ierr)
{
  FloatComplex retval;

  retval = f (x, alpha, (scaled ? 2 : 1), ierr);

  return retval;
}

static inline FloatComplexMatrix
do_bessel (fptr f, const char *, float alpha, const FloatComplexMatrix& x,
           bool scaled, Array<octave_idx_type>& ierr)
{
  octave_idx_type nr = x.rows ();
  octave_idx_type nc = x.cols ();

  FloatComplexMatrix retval (nr, nc);

  ierr.resize (dim_vector (nr, nc));

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      retval(i, j) = f (x(i, j), alpha, (scaled ? 2 : 1), ierr(i, j));

  return retval;
}

static inline FloatComplexMatrix
do_bessel (fptr f, const char *, const FloatMatrix& alpha,
           const FloatComplex& x,
           bool scaled, Array<octave_idx_type>& ierr)
{
  octave_idx_type nr = alpha.rows ();
  octave_idx_type nc = alpha.cols ();

  FloatComplexMatrix retval (nr, nc);

  ierr.resize (dim_vector (nr, nc));

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      retval(i, j) = f (x, alpha(i, j), (scaled ? 2 : 1), ierr(i, j));

  return retval;
}

static inline FloatComplexMatrix
do_bessel (fptr f, const char *fn, const FloatMatrix& alpha,
           const FloatComplexMatrix& x, bool scaled,
           Array<octave_idx_type>& ierr)
{
  FloatComplexMatrix retval;

  octave_idx_type x_nr = x.rows ();
  octave_idx_type x_nc = x.cols ();

  octave_idx_type alpha_nr = alpha.rows ();
  octave_idx_type alpha_nc = alpha.cols ();

  if (x_nr != alpha_nr || x_nc != alpha_nc)
    (*current_liboctave_error_handler)
      ("%s: the sizes of alpha and x must conform", fn);

  octave_idx_type nr = x_nr;
  octave_idx_type nc = x_nc;

  retval.resize (nr, nc);

  ierr.resize (dim_vector (nr, nc));

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      retval(i, j) = f (x(i, j), alpha(i, j), (scaled ? 2 : 1), ierr(i, j));

  return retval;
}

static inline FloatComplexNDArray
do_bessel (fptr f, const char *, float alpha, const FloatComplexNDArray& x,
           bool scaled, Array<octave_idx_type>& ierr)
{
  dim_vector dv = x.dims ();
  octave_idx_type nel = dv.numel ();
  FloatComplexNDArray retval (dv);

  ierr.resize (dv);

  for (octave_idx_type i = 0; i < nel; i++)
    retval(i) = f (x(i), alpha, (scaled ? 2 : 1), ierr(i));

  return retval;
}

static inline FloatComplexNDArray
do_bessel (fptr f, const char *, const FloatNDArray& alpha,
           const FloatComplex& x, bool scaled, Array<octave_idx_type>& ierr)
{
  dim_vector dv = alpha.dims ();
  octave_idx_type nel = dv.numel ();
  FloatComplexNDArray retval (dv);

  ierr.resize (dv);

  for (octave_idx_type i = 0; i < nel; i++)
    retval(i) = f (x, alpha(i), (scaled ? 2 : 1), ierr(i));

  return retval;
}

static inline FloatComplexNDArray
do_bessel (fptr f, const char *fn, const FloatNDArray& alpha,
           const FloatComplexNDArray& x, bool scaled,
           Array<octave_idx_type>& ierr)
{
  dim_vector dv = x.dims ();
  FloatComplexNDArray retval;

  if (dv != alpha.dims ())
    (*current_liboctave_error_handler)
      ("%s: the sizes of alpha and x must conform", fn);

  octave_idx_type nel = dv.numel ();

  retval.resize (dv);
  ierr.resize (dv);

  for (octave_idx_type i = 0; i < nel; i++)
    retval(i) = f (x(i), alpha(i), (scaled ? 2 : 1), ierr(i));

  return retval;
}

static inline FloatComplexMatrix
do_bessel (fptr f, const char *, const FloatRowVector& alpha,
           const FloatComplexColumnVector& x, bool scaled,
           Array<octave_idx_type>& ierr)
{
  octave_idx_type nr = x.numel ();
  octave_idx_type nc = alpha.numel ();

  FloatComplexMatrix retval (nr, nc);

  ierr.resize (dim_vector (nr, nc));

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      retval(i, j) = f (x(i), alpha(j), (scaled ? 2 : 1), ierr(i, j));

  return retval;
}

#define SS_BESSEL(name, fcn)                                    \
    FloatComplex                                                \
    name (float alpha, const FloatComplex& x, bool scaled,      \
          octave_idx_type& ierr)                                \
    {                                                           \
      return do_bessel (fcn, #name, alpha, x, scaled, ierr);    \
    }

#define SM_BESSEL(name, fcn)                                            \
    FloatComplexMatrix                                                  \
    name (float alpha, const FloatComplexMatrix& x, bool scaled,        \
          Array<octave_idx_type>& ierr)                                 \
    {                                                                   \
      return do_bessel (fcn, #name, alpha, x, scaled, ierr);            \
    }

#define MS_BESSEL(name, fcn)                                            \
    FloatComplexMatrix                                                  \
    name (const FloatMatrix& alpha, const FloatComplex& x, bool scaled, \
          Array<octave_idx_type>& ierr)                                 \
    {                                                                   \
      return do_bessel (fcn, #name, alpha, x, scaled, ierr);            \
    }

#define MM_BESSEL(name, fcn)                                            \
    FloatComplexMatrix                                                  \
    name (const FloatMatrix& alpha, const FloatComplexMatrix& x,        \
          bool scaled, Array<octave_idx_type>& ierr)                    \
    {                                                                   \
      return do_bessel (fcn, #name, alpha, x, scaled, ierr);            \
    }

#define SN_BESSEL(name, fcn)                                            \
    FloatComplexNDArray                                                 \
    name (float alpha, const FloatComplexNDArray& x, bool scaled,       \
          Array<octave_idx_type>& ierr)                                 \
    {                                                                   \
      return do_bessel (fcn, #name, alpha, x, scaled, ierr);            \
    }

#define NS_BESSEL(name, fcn)                                    \
    FloatComplexNDArray                                         \
    name (const FloatNDArray& alpha, const FloatComplex& x,     \
          bool scaled, Array<octave_idx_type>& ierr)            \
    {                                                           \
      return do_bessel (fcn, #name, alpha, x, scaled, ierr);    \
    }

#define NN_BESSEL(name, fcn)                                            \
    FloatComplexNDArray                                                 \
    name (const FloatNDArray& alpha, const FloatComplexNDArray& x,      \
          bool scaled, Array<octave_idx_type>& ierr)                    \
    {                                                                   \
      return do_bessel (fcn, #name, alpha, x, scaled, ierr);            \
    }

#define RC_BESSEL(name, fcn)                                    \
    FloatComplexMatrix                                          \
    name (const FloatRowVector& alpha,                          \
          const FloatComplexColumnVector& x, bool scaled,       \
          Array<octave_idx_type>& ierr)                         \
    {                                                           \
      return do_bessel (fcn, #name, alpha, x, scaled, ierr);    \
    }

#define ALL_BESSEL(name, fcn)                   \
    SS_BESSEL (name, fcn)                       \
    SM_BESSEL (name, fcn)                       \
    MS_BESSEL (name, fcn)                       \
    MM_BESSEL (name, fcn)                       \
    SN_BESSEL (name, fcn)                       \
    NS_BESSEL (name, fcn)                       \
    NN_BESSEL (name, fcn)                       \
    RC_BESSEL (name, fcn)

ALL_BESSEL (besselj, cbesj)
ALL_BESSEL (bessely, cbesy)
ALL_BESSEL (besseli, cbesi)
ALL_BESSEL (besselk, cbesk)
ALL_BESSEL (besselh1, cbesh1)
ALL_BESSEL (besselh2, cbesh2)

#undef ALL_BESSEL
#undef SS_BESSEL
#undef SM_BESSEL
#undef MS_BESSEL
#undef MM_BESSEL
#undef SN_BESSEL
#undef NS_BESSEL
#undef NN_BESSEL
#undef RC_BESSEL

Complex
biry (const Complex& z, bool deriv, bool scaled, octave_idx_type& ierr)
{
  double ar = 0.0;
  double ai = 0.0;

  double zr = z.real ();
  double zi = z.imag ();

  F77_INT id = (deriv ? 1 : 0);
  F77_INT t_ierr;
  F77_INT sc = (scaled ? 2 : 1);

  F77_FUNC (zbiry, ZBIRY) (zr, zi, id, sc, ar, ai, t_ierr);

  ierr = t_ierr;

  if (zi == 0.0 && (! scaled || zr >= 0.0))
    ai = 0.0;

  return bessel_return_value (Complex (ar, ai), ierr);
}

ComplexMatrix
biry (const ComplexMatrix& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr)
{
  octave_idx_type nr = z.rows ();
  octave_idx_type nc = z.cols ();

  ComplexMatrix retval (nr, nc);

  ierr.resize (dim_vector (nr, nc));

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      retval(i, j) = biry (z(i, j), deriv, scaled, ierr(i, j));

  return retval;
}

ComplexNDArray
biry (const ComplexNDArray& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr)
{
  dim_vector dv = z.dims ();
  octave_idx_type nel = dv.numel ();
  ComplexNDArray retval (dv);

  ierr.resize (dv);

  for (octave_idx_type i = 0; i < nel; i++)
    retval(i) = biry (z(i), deriv, scaled, ierr(i));

  return retval;
}

FloatComplex
biry (const FloatComplex& z, bool deriv, bool scaled,
      octave_idx_type& ierr)
{
  FloatComplex a;

  F77_INT id = (deriv ? 1 : 0);
  F77_INT t_ierr;
  F77_INT sc = (scaled ? 2 : 1);

  F77_FUNC (cbiry, CBIRY) (F77_CONST_CMPLX_ARG (&z), id, sc,
                           F77_CMPLX_ARG (&a), t_ierr);

  ierr = t_ierr;

  float ar = a.real ();
  float ai = a.imag ();

  if (z.imag () == 0.0 && (! scaled || z.real () >= 0.0))
    ai = 0.0;

  return bessel_return_value (FloatComplex (ar, ai), ierr);
}

FloatComplexMatrix
biry (const FloatComplexMatrix& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr)
{
  octave_idx_type nr = z.rows ();
  octave_idx_type nc = z.cols ();

  FloatComplexMatrix retval (nr, nc);

  ierr.resize (dim_vector (nr, nc));

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      retval(i, j) = biry (z(i, j), deriv, scaled, ierr(i, j));

  return retval;
}

FloatComplexNDArray
biry (const FloatComplexNDArray& z, bool deriv, bool scaled,
      Array<octave_idx_type>& ierr)
{
  dim_vector dv = z.dims ();
  octave_idx_type nel = dv.numel ();
  FloatComplexNDArray retval (dv);

  ierr.resize (dv);

  for (octave_idx_type i = 0; i < nel; i++)
    retval(i) = biry (z(i), deriv, scaled, ierr(i));

  return retval;
}

// Real and complex Dawson function (= scaled erfi) from Faddeeva package
double dawson (double x) { return Faddeeva::Dawson (x); }
float dawson (float x) { return Faddeeva::Dawson (x); }

Complex
dawson (const Complex& x)
{
  return Faddeeva::Dawson (x);
}

FloatComplex
dawson (const FloatComplex& x)
{
  Complex xd (x.real (), x.imag ());
  Complex ret;
  ret = Faddeeva::Dawson (xd, std::numeric_limits<float>::epsilon ());
  return FloatComplex (ret.real (), ret.imag ());
}

void
ellipj (double u, double m, double& sn, double& cn, double& dn, double& err)
{
  static const int Nmax = 16;
  double m1, t=0, si_u, co_u, se_u, ta_u, b, c[Nmax], a[Nmax], phi;
  int n, Nn, ii;

  if (m < 0 || m > 1)
    {
      (*current_liboctave_warning_with_id_handler)
        ("Octave:ellipj-invalid-m",
         "ellipj: invalid M value, required value 0 <= M <= 1");

      sn = cn = dn = lo_ieee_nan_value ();

      return;
    }

  double sqrt_eps = std::sqrt (std::numeric_limits<double>::epsilon ());
  if (m < sqrt_eps)
    {
      // For small m, (Abramowitz and Stegun, Section 16.13)
      si_u = sin (u);
      co_u = cos (u);
      t = 0.25*m*(u - si_u*co_u);
      sn = si_u - t * co_u;
      cn = co_u + t * si_u;
      dn = 1 - 0.5*m*si_u*si_u;
    }
  else if ((1 - m) < sqrt_eps)
    {
      // For m1 = (1-m) small (Abramowitz and Stegun, Section 16.15)
      m1 = 1 - m;
      si_u = sinh (u);
      co_u = cosh (u);
      ta_u = tanh (u);
      se_u = 1/co_u;
      sn = ta_u + 0.25*m1*(si_u*co_u - u)*se_u*se_u;
      cn = se_u - 0.25*m1*(si_u*co_u - u)*ta_u*se_u;
      dn = se_u + 0.25*m1*(si_u*co_u + u)*ta_u*se_u;
    }
  else
    {
      // Arithmetic-Geometric Mean (AGM) algorithm
      //   (Abramowitz and Stegun, Section 16.4)
      a[0] = 1;
      b    = std::sqrt (1 - m);
      c[0] = std::sqrt (m);
      for (n = 1; n < Nmax; ++n)
        {
          a[n] = (a[n - 1] + b)/2;
          c[n] = (a[n - 1] - b)/2;
          b = std::sqrt (a[n - 1]*b);
          if (c[n]/a[n] < std::numeric_limits<double>::epsilon ()) break;
        }
      if (n >= Nmax - 1)
        {
          err = 1;
          return;
        }
      Nn = n;
      for (ii = 1; n > 0; ii *= 2, --n) {}  // ii = pow(2,Nn)
      phi = ii*a[Nn]*u;
      for (n = Nn; n > 0; --n)
        {
          phi = (std::asin ((c[n]/a[n])* sin (phi)) + phi)/2;
        }
      sn = sin (phi);
      cn = cos (phi);
      dn = std::sqrt (1 - m*sn*sn);
    }
}

void
ellipj (const Complex& u, double m, Complex& sn, Complex& cn, Complex& dn,
        double& err)
{
  double m1 = 1 - m, ss1, cc1, dd1;

  ellipj (u.imag (), m1, ss1, cc1, dd1, err);
  if (u.real () == 0)
    {
      // u is pure imag: Jacoby imag. transf.
      sn = Complex (0, ss1/cc1);
      cn = 1/cc1;         //    cn.imag = 0;
      dn = dd1/cc1;       //    dn.imag = 0;
    }
  else
    {
      // u is generic complex
      double ss, cc, dd, ddd;

      ellipj (u.real (), m, ss, cc, dd, err);
      ddd = cc1*cc1 + m*ss*ss*ss1*ss1;
      sn = Complex (ss*dd1/ddd, cc*dd*ss1*cc1/ddd);
      cn = Complex (cc*cc1/ddd, -ss*dd*ss1*dd1/ddd);
      dn = Complex (dd*cc1*dd1/ddd, -m*ss*cc*ss1/ddd);
    }
}

// Complex error function from the Faddeeva package
Complex
erf (const Complex& x)
{
  return Faddeeva::erf (x);
}

FloatComplex
erf (const FloatComplex& x)
{
  Complex xd (x.real (), x.imag ());
  Complex ret = Faddeeva::erf (xd, std::numeric_limits<float>::epsilon ());
  return FloatComplex (ret.real (), ret.imag ());
}

// Complex complementary error function from the Faddeeva package
Complex
erfc (const Complex& x)
{
  return Faddeeva::erfc (x);
}

FloatComplex
erfc (const FloatComplex& x)
{
  Complex xd (x.real (), x.imag ());
  Complex ret = Faddeeva::erfc (xd, std::numeric_limits<float>::epsilon ());
  return FloatComplex (ret.real (), ret.imag ());
}

// The algorithm for erfcinv is an adaptation of the erfinv algorithm
// above from P. J. Acklam.  It has been modified to run over the
// different input domain of erfcinv.  See the notes for erfinv for an
// explanation.

static double do_erfcinv (double x, bool refine)
{
  // Coefficients of rational approximation.
  static const double a[] =
  {
    -2.806989788730439e+01,  1.562324844726888e+02,
      -1.951109208597547e+02,  9.783370457507161e+01,
      -2.168328665628878e+01,  1.772453852905383e+00
    };
  static const double b[] =
  {
    -5.447609879822406e+01,  1.615858368580409e+02,
      -1.556989798598866e+02,  6.680131188771972e+01,
      -1.328068155288572e+01
    };
  static const double c[] =
  {
    -5.504751339936943e-03, -2.279687217114118e-01,
      -1.697592457770869e+00, -1.802933168781950e+00,
      3.093354679843505e+00,  2.077595676404383e+00
    };
  static const double d[] =
  {
    7.784695709041462e-03,  3.224671290700398e-01,
    2.445134137142996e+00,  3.754408661907416e+00
  };

  static const double spi2 = 8.862269254527579e-01; // sqrt(pi)/2.
  static const double pbreak_lo = 0.04850;  // 1-pbreak
  static const double pbreak_hi = 1.95150;  // 1+pbreak
  double y;

  // Select case.
  if (x >= pbreak_lo && x <= pbreak_hi)
    {
      // Middle region.
      const double q = 0.5*(1-x), r = q*q;
      const double yn = (((((a[0]*r + a[1])*r + a[2])*r + a[3])*r + a[4])*r + a[5])*q;
      const double yd = ((((b[0]*r + b[1])*r + b[2])*r + b[3])*r + b[4])*r + 1.0;
      y = yn / yd;
    }
  else if (x > 0.0 && x < 2.0)
    {
      // Tail region.
      const double q = (x < 1
                        ? std::sqrt (-2*std::log (0.5*x))
                        : std::sqrt (-2*std::log (0.5*(2-x))));

      const double yn = ((((c[0]*q + c[1])*q + c[2])*q + c[3])*q + c[4])*q + c[5];

      const double yd = (((d[0]*q + d[1])*q + d[2])*q + d[3])*q + 1.0;

      y = yn / yd;

      if (x < pbreak_lo)
        y = -y;
    }
  else if (x == 0.0)
    return numeric_limits<double>::Inf ();
  else if (x == 2.0)
    return -numeric_limits<double>::Inf ();
  else
    return numeric_limits<double>::NaN ();

  if (refine)
    {
      // One iteration of Halley's method gives full precision.
      double u = (erf (y) - (1-x)) * spi2 * exp (y*y);
      y -= u / (1 + y*u);
    }

  return y;
}

double erfcinv (double x)
{
  return do_erfcinv (x, true);
}

float erfcinv (float x)
{
  return do_erfcinv (x, false);
}

// Real and complex scaled complementary error function from Faddeeva pkg.
double erfcx (double x) { return Faddeeva::erfcx (x); }
float erfcx (float x) { return Faddeeva::erfcx (x); }

Complex
erfcx (const Complex& x)
{
  return Faddeeva::erfcx (x);
}

FloatComplex
erfcx (const FloatComplex& x)
{
  Complex xd (x.real (), x.imag ());
  Complex ret;
  ret = Faddeeva::erfcx (xd, std::numeric_limits<float>::epsilon ());
  return FloatComplex (ret.real (), ret.imag ());
}

// Real and complex imaginary error function from Faddeeva package
double erfi (double x) { return Faddeeva::erfi (x); }
float erfi (float x) { return Faddeeva::erfi (x); }

Complex
erfi (const Complex& x)
{
  return Faddeeva::erfi (x);
}

FloatComplex
erfi (const FloatComplex& x)
{
  Complex xd (x.real (), x.imag ());
  Complex ret = Faddeeva::erfi (xd, std::numeric_limits<float>::epsilon ());
  return FloatComplex (ret.real (), ret.imag ());
}

// This algorithm is due to P. J. Acklam.
//
// See http://home.online.no/~pjacklam/notes/invnorm/
//
// The rational approximation has relative accuracy 1.15e-9 in the whole
// region.  For doubles, it is refined by a single step of Halley's 3rd
// order method.  For single precision, the accuracy is already OK, so
// we skip it to get faster evaluation.

static double do_erfinv (double x, bool refine)
{
  // Coefficients of rational approximation.
  static const double a[] =
  {
    -2.806989788730439e+01,  1.562324844726888e+02,
      -1.951109208597547e+02,  9.783370457507161e+01,
      -2.168328665628878e+01,  1.772453852905383e+00
    };
  static const double b[] =
  {
    -5.447609879822406e+01,  1.615858368580409e+02,
      -1.556989798598866e+02,  6.680131188771972e+01,
      -1.328068155288572e+01
    };
  static const double c[] =
  {
    -5.504751339936943e-03, -2.279687217114118e-01,
      -1.697592457770869e+00, -1.802933168781950e+00,
      3.093354679843505e+00,  2.077595676404383e+00
    };
  static const double d[] =
  {
    7.784695709041462e-03,  3.224671290700398e-01,
    2.445134137142996e+00,  3.754408661907416e+00
  };

  static const double spi2 = 8.862269254527579e-01; // sqrt(pi)/2.
  static const double pbreak = 0.95150;
  double ax = fabs (x), y;

  // Select case.
  if (ax <= pbreak)
    {
      // Middle region.
      const double q = 0.5 * x, r = q*q;
      const double yn = (((((a[0]*r + a[1])*r + a[2])*r + a[3])*r + a[4])*r + a[5])*q;
      const double yd = ((((b[0]*r + b[1])*r + b[2])*r + b[3])*r + b[4])*r + 1.0;
      y = yn / yd;
    }
  else if (ax < 1.0)
    {
      // Tail region.
      const double q = std::sqrt (-2*std::log (0.5*(1-ax)));
      const double yn = ((((c[0]*q + c[1])*q + c[2])*q + c[3])*q + c[4])*q + c[5];
      const double yd = (((d[0]*q + d[1])*q + d[2])*q + d[3])*q + 1.0;
      y = yn / yd * math::signum (-x);
    }
  else if (ax == 1.0)
    return numeric_limits<double>::Inf () * math::signum (x);
  else
    return numeric_limits<double>::NaN ();

  if (refine)
    {
      // One iteration of Halley's method gives full precision.
      double u = (erf (y) - x) * spi2 * exp (y*y);
      y -= u / (1 + y*u);
    }

  return y;
}

double erfinv (double x)
{
  return do_erfinv (x, true);
}

float erfinv (float x)
{
  return do_erfinv (x, false);
}

Complex
expm1 (const Complex& x)
{
  Complex retval;

  if (std::abs (x) < 1)
    {
      double im = x.imag ();
      double u = expm1 (x.real ());
      double v = sin (im/2);
      v = -2*v*v;
      retval = Complex (u*v + u + v, (u+1) * sin (im));
    }
  else
    retval = std::exp (x) - Complex (1);

  return retval;
}

FloatComplex
expm1 (const FloatComplex& x)
{
  FloatComplex retval;

  if (std::abs (x) < 1)
    {
      float im = x.imag ();
      float u = expm1 (x.real ());
      float v = sin (im/2);
      v = -2*v*v;
      retval = FloatComplex (u*v + u + v, (u+1) * sin (im));
    }
  else
    retval = std::exp (x) - FloatComplex (1);

  return retval;
}

double
gamma (double x)
{
  double result;

  // Special cases for (near) compatibility with Matlab instead of tgamma.
  // Matlab does not have -0.

  if (x == 0)
    result = (math::negative_sign (x)
              ? -numeric_limits<double>::Inf ()
              : numeric_limits<double>::Inf ());
  else if ((x < 0 && math::x_nint (x) == x)
           || math::isinf (x))
    result = numeric_limits<double>::Inf ();
  else if (math::isnan (x))
    result = numeric_limits<double>::NaN ();
  else
    result = std::tgamma (x);

  return result;
}

float
gamma (float x)
{
  float result;

  // Special cases for (near) compatibility with Matlab instead of tgamma.
  // Matlab does not have -0.

  if (x == 0)
    result = (math::negative_sign (x)
              ? -numeric_limits<float>::Inf ()
              : numeric_limits<float>::Inf ());
  else if ((x < 0 && math::x_nint (x) == x)
           || math::isinf (x))
    result = numeric_limits<float>::Inf ();
  else if (math::isnan (x))
    result = numeric_limits<float>::NaN ();
  else
    result = std::tgammaf (x);

  return result;
}

Complex
log1p (const Complex& x)
{
  Complex retval;

  double r = x.real (), i = x.imag ();

  if (fabs (r) < 0.5 && fabs (i) < 0.5)
    {
      double u = 2*r + r*r + i*i;
      retval = Complex (log1p (u / (1+std::sqrt (u+1))),
                        atan2 (i, 1 + r));
    }
  else
    retval = std::log (Complex (1) + x);

  return retval;
}

FloatComplex
log1p (const FloatComplex& x)
{
  FloatComplex retval;

  float r = x.real (), i = x.imag ();

  if (fabs (r) < 0.5 && fabs (i) < 0.5)
    {
      float u = 2*r + r*r + i*i;
      retval = FloatComplex (log1p (u / (1+std::sqrt (u+1))),
                             atan2 (i, 1 + r));
    }
  else
    retval = std::log (FloatComplex (1) + x);

  return retval;
}

static const double pi = 3.14159265358979323846;

template <typename T>
static inline T
xlog (const T& x)
{
  return log (x);
}

template <>
inline double
xlog (const double& x)
{
  return std::log (x);
}

template <>
inline float
xlog (const float& x)
{
  return std::log (x);
}

template <typename T>
static T
lanczos_approximation_psi (const T zc)
{
  // Coefficients for C.Lanczos expansion of psi function from XLiFE++
  // gammaFunctions psi_coef[k] = - (2k+1) * lg_coef[k] (see melina++
  // gamma functions -1/12, 3/360,-5/1260, 7/1680,-9/1188,
  // 11*691/360360,-13/156, 15*3617/122400, ? , ?
  static const T dg_coeff[10] =
  {
    -0.83333333333333333e-1, 0.83333333333333333e-2,
      -0.39682539682539683e-2, 0.41666666666666667e-2,
      -0.75757575757575758e-2, 0.21092796092796093e-1,
      -0.83333333333333333e-1, 0.4432598039215686,
      -0.3053954330270122e+1,  0.125318899521531e+2
    };

  T overz2  = T (1.0) / (zc * zc);
  T overz2k = overz2;

  T p = 0;
  for (octave_idx_type k = 0; k < 10; k++, overz2k *= overz2)
    p += dg_coeff[k] * overz2k;
  p += xlog (zc) - T (0.5) / zc;
  return p;
}

template <typename T>
T
xpsi (T z)
{
  static const double euler_mascheroni
    = 0.577215664901532860606512090082402431042;

  const bool is_int = (std::floor (z) == z);

  T p = 0;
  if (z <= 0)
    {
      // limits - zeros of the gamma function
      if (is_int)
        p = -numeric_limits<T>::Inf (); // Matlab returns -Inf for psi (0)
      else
        // Abramowitz and Stegun, page 259, eq 6.3.7
        p = psi (1 - z) - (pi / tan (pi * z));
    }
  else if (is_int)
    {
      // Abramowitz and Stegun, page 258, eq 6.3.2
      p = - euler_mascheroni;
      for (octave_idx_type k = z - 1; k > 0; k--)
        p += 1.0 / k;
    }
  else if (std::floor (z + 0.5) == z + 0.5)
    {
      // Abramowitz and Stegun, page 258, eq 6.3.3 and 6.3.4
      for (octave_idx_type k = z; k > 0; k--)
        p += 1.0 / (2 * k - 1);

      p = - euler_mascheroni - 2 * std::log (2) + 2 * (p);
    }
  else
    {
      // adapted from XLiFE++ gammaFunctions

      T zc = z;
      // Use formula for derivative of LogGamma(z)
      if (z < 10)
        {
          const signed char n = 10 - z;
          for (signed char k = n - 1; k >= 0; k--)
            p -= 1.0 / (k + z);
          zc += n;
        }
      p += lanczos_approximation_psi (zc);
    }

  return p;
}

// explicit instantiations
double psi (double z) { return xpsi (z); }
float psi (float z) { return xpsi (z); }

template <typename T>
std::complex<T>
xpsi (const std::complex<T>& z)
{
  // adapted from XLiFE++ gammaFunctions

  typedef typename std::complex<T>::value_type P;

  P z_r  = z.real ();
  P z_ra = z_r;

  std::complex<T> dgam (0.0, 0.0);
  if (z.imag () == 0)
    dgam = std::complex<T> (psi (z_r), 0.0);
  else if (z_r < 0)
    dgam = psi (P (1.0) - z)- (P (pi) / tan (P (pi) * z));
  else
    {
      // Use formula for derivative of LogGamma(z)
      std::complex<T> z_m = z;
      if (z_ra < 8)
        {
          unsigned char n = 8 - z_ra;
          z_m = z + std::complex<T> (n, 0.0);

          // Recurrence formula.  For | Re(z) | < 8, use recursively
          //
          //   DiGamma(z) = DiGamma(z+1) - 1/z
          std::complex<T> z_p = z + P (n - 1);
          for (unsigned char k = n; k > 0; k--, z_p -= 1.0)
            dgam -= P (1.0) / z_p;
        }

      // for | Re(z) | > 8, use derivative of C.Lanczos expansion for
      // LogGamma
      //
      //   psi(z) = log(z) - 1/(2z) - 1/12z^2 + 3/360z^4 - 5/1260z^6
      //     + 7/1680z^8 - 9/1188z^10 + ...
      //
      // (Abramowitz&Stegun, page 259, formula 6.3.18
      dgam += lanczos_approximation_psi (z_m);
    }
  return dgam;
}

// explicit instantiations
Complex psi (const Complex& z) { return xpsi (z); }
FloatComplex psi (const FloatComplex& z) { return xpsi (z); }

template <typename T>
static inline void
fortran_psifn (T z, octave_idx_type n, T& ans, octave_idx_type& ierr);

template <>
inline void
fortran_psifn<double> (double z, octave_idx_type n_arg,
                       double& ans, octave_idx_type& ierr)
{
  F77_INT n = to_f77_int (n_arg);
  F77_INT flag = 0;
  F77_INT t_ierr;
  F77_XFCN (dpsifn, DPSIFN, (z, n, 1, 1, ans, flag, t_ierr));
  ierr = t_ierr;
}

template <>
inline void
fortran_psifn<float> (float z, octave_idx_type n_arg,
                      float& ans, octave_idx_type& ierr)
{
  F77_INT n = to_f77_int (n_arg);
  F77_INT flag = 0;
  F77_INT t_ierr;
  F77_XFCN (psifn, PSIFN, (z, n, 1, 1, ans, flag, t_ierr));
  ierr = t_ierr;
}

template <typename T>
T
xpsi (octave_idx_type n, T z)
{
  T ans;
  octave_idx_type ierr = 0;
  fortran_psifn<T> (z, n, ans, ierr);
  if (ierr == 0)
    {
      // Remember that psifn and dpsifn return scales values
      // When n is 1: do nothing since ((-1)**(n+1)/gamma(n+1)) == 1
      // When n is 0: change sign since ((-1)**(n+1)/gamma(n+1)) == -1
      if (n > 1)
        // FIXME: xgamma here is a killer for our precision since it grows
        //        way too fast.
        ans = ans / (std::pow (-1.0, n + 1) / gamma (double (n+1)));
      else if (n == 0)
        ans = -ans;
    }
  else if (ierr == 2)
    ans = - numeric_limits<T>::Inf ();
  else // we probably never get here
    ans = numeric_limits<T>::NaN ();

  return ans;
}

double psi (octave_idx_type n, double z) { return xpsi (n, z); }
float psi (octave_idx_type n, float z) { return xpsi (n, z); }

Complex
rc_lgamma (double x)
{
  double result;

#if defined (HAVE_LGAMMA_R)
  int sgngam;
  result = lgamma_r (x, &sgngam);
#else
  result = std::lgamma (x);
  int sgngam = signgam;
#endif

  if (sgngam < 0)
    return result + Complex (0., M_PI);
  else
    return result;
}

FloatComplex
rc_lgamma (float x)
{
  float result;

#if defined (HAVE_LGAMMAF_R)
  int sgngam;
  result = lgammaf_r (x, &sgngam);
#else
  result = std::lgammaf (x);
  int sgngam = signgam;
#endif

  if (sgngam < 0)
    return result + FloatComplex (0., M_PI);
  else
    return result;
}

Complex rc_log1p (double x)
{
  return (x < -1.0
          ? Complex (std::log (-(1.0 + x)), M_PI)
          : Complex (log1p (x)));
}

FloatComplex rc_log1p (float x)
{
  return (x < -1.0f
          ? FloatComplex (std::log (-(1.0f + x)), M_PI)
          : FloatComplex (log1p (x)));
}

OCTAVE_END_NAMESPACE(math)
OCTAVE_END_NAMESPACE(octave)
