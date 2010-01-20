/*

Copyright (C) 2001, 2003, 2005, 2006, 2007, 2008 Ross Lippert and Paul Kienzle

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <float.h>

#include "CmplxSCHUR.h"
#include "fCmplxSCHUR.h"
#include "lo-ieee.h"
#include "lo-mappers.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "utils.h"

template <class T>
static inline T
getmin (T x, T y)
{
  return x < y ? x : y;
}

template <class T>
static inline T
getmax (T x, T y)
{
  return x > y ? x : y;
}

static double
frobnorm (const ComplexMatrix& A)
{
  double sum = 0;

  for (octave_idx_type i = 0; i < A.rows (); i++)
    for (octave_idx_type j = 0; j < A.columns (); j++)
      sum += real (A(i,j) * conj (A(i,j)));

  return sqrt (sum);
}

static double
frobnorm (const Matrix& A)
{
  double sum = 0;
  for (octave_idx_type i = 0; i < A.rows (); i++)
    for (octave_idx_type j = 0; j < A.columns (); j++)
      sum += A(i,j) * A(i,j);

  return sqrt (sum);
}

static float
frobnorm (const FloatComplexMatrix& A)
{
  float sum = 0;

  for (octave_idx_type i = 0; i < A.rows (); i++)
    for (octave_idx_type j = 0; j < A.columns (); j++)
      sum += real (A(i,j) * conj (A(i,j)));

  return sqrt (sum);
}

static float
frobnorm (const FloatMatrix& A)
{
  float sum = 0;
  for (octave_idx_type i = 0; i < A.rows (); i++)
    for (octave_idx_type j = 0; j < A.columns (); j++)
      sum += A(i,j) * A(i,j);

  return sqrt (sum);
}

static ComplexMatrix
sqrtm_from_schur (const ComplexMatrix& U, const ComplexMatrix& T)
{
  const octave_idx_type n = U.rows ();

  ComplexMatrix R (n, n, 0.0);

  for (octave_idx_type j = 0; j < n; j++)
    R(j,j) = sqrt (T(j,j));

  const double fudge = sqrt (DBL_MIN);

  for (octave_idx_type p = 0; p < n-1; p++)
    {
      for (octave_idx_type i = 0; i < n-(p+1); i++)
        {
          const octave_idx_type j = i + p + 1;

          Complex s = T(i,j);

          for (octave_idx_type k = i+1; k < j; k++)
            s -= R(i,k) * R(k,j);

          // dividing
          //     R(i,j) = s/(R(i,i)+R(j,j));
          // screwing around to not / 0

          const Complex d = R(i,i) + R(j,j) + fudge;
          const Complex conjd = conj (d);

          R(i,j) =  (s*conjd)/(d*conjd);
        }
    }

  return U * R * U.hermitian ();
}

static FloatComplexMatrix
sqrtm_from_schur (const FloatComplexMatrix& U, const FloatComplexMatrix& T)
{
  const octave_idx_type n = U.rows ();

  FloatComplexMatrix R (n, n, 0.0);

  for (octave_idx_type j = 0; j < n; j++)
    R(j,j) = sqrt (T(j,j));

  const float fudge = sqrt (FLT_MIN);

  for (octave_idx_type p = 0; p < n-1; p++)
    {
      for (octave_idx_type i = 0; i < n-(p+1); i++)
        {
          const octave_idx_type j = i + p + 1;

          FloatComplex s = T(i,j);

          for (octave_idx_type k = i+1; k < j; k++)
            s -= R(i,k) * R(k,j);

          // dividing
          //     R(i,j) = s/(R(i,i)+R(j,j));
          // screwing around to not / 0

          const FloatComplex d = R(i,i) + R(j,j) + fudge;
          const FloatComplex conjd = conj (d);

          R(i,j) =  (s*conjd)/(d*conjd);
        }
    }

  return U * R * U.hermitian ();
}

DEFUN_DLD (sqrtm, args, nargout,
 "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{result}, @var{error_estimate}] =} sqrtm (@var{a})\n\
Compute the matrix square root of the square matrix @var{a}.\n\
\n\
Ref: Nicholas J. Higham.  A new sqrtm for @sc{matlab}.  Numerical Analysis\n\
Report No. 336, Manchester Centre for Computational Mathematics,\n\
Manchester, England, January 1999.\n\
@seealso{expm, logm}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin != 1)
    {
      print_usage ();
      return retval;
    }

  octave_value arg = args(0);

  octave_idx_type n = arg.rows ();
  octave_idx_type nc = arg.columns ();

  int arg_is_empty = empty_arg ("sqrtm", n, nc);

  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return octave_value (Matrix ());

  if (n != nc)
    {
      gripe_square_matrix_required ("sqrtm");
      return retval;
    }

  retval(1) = lo_ieee_inf_value ();
  retval(0) = lo_ieee_nan_value ();


  if (arg.is_single_type ())
    {
      if (arg.is_real_scalar ())
        {
          float d = arg.float_value ();
          if (d > 0.0)
            {
              retval(0) = sqrt (d);
              retval(1) = 0.0;
            }
          else
            {
              retval(0) = FloatComplex (0.0, sqrt (d));
              retval(1) = 0.0;
            }
        }
      else if (arg.is_complex_scalar ())
        {
          FloatComplex c = arg.float_complex_value ();
          retval(0) = sqrt (c);
          retval(1) = 0.0;
        }
      else if (arg.is_matrix_type ())
        {
          float err, minT;

          if (arg.is_real_matrix ())
            {
              FloatMatrix A = arg.float_matrix_value();

              if (error_state)
                return retval;

              // FIXME -- eventually, FloatComplexSCHUR will accept a
              // real matrix arg.

              FloatComplexMatrix Ac (A);

              const FloatComplexSCHUR schur (Ac, std::string ());

              if (error_state)
                return retval;

              const FloatComplexMatrix U (schur.unitary_matrix ());
              const FloatComplexMatrix T (schur.schur_matrix ());
              const FloatComplexMatrix X (sqrtm_from_schur (U, T));

              // Check for minimal imaginary part
              float normX = 0.0;
              float imagX = 0.0;
              for (octave_idx_type i = 0; i < n; i++)
                for (octave_idx_type j = 0; j < n; j++)
                  {
                    imagX = getmax (imagX, imag (X(i,j)));
                    normX = getmax (normX, abs (X(i,j)));
                  }

              if (imagX < normX * 100 * FLT_EPSILON)
                retval(0) = real (X);
              else
                retval(0) = X;

              // Compute error
              // FIXME can we estimate the error without doing the
              // matrix multiply?

              err = frobnorm (X*X - FloatComplexMatrix (A)) / frobnorm (A);

              if (xisnan (err))
                err = lo_ieee_float_inf_value ();

              // Find min diagonal
              minT = lo_ieee_float_inf_value ();
              for (octave_idx_type i=0; i < n; i++)
                minT = getmin(minT, abs(T(i,i)));
            }
          else
            {
              FloatComplexMatrix A = arg.float_complex_matrix_value ();

              if (error_state)
                return retval;

              const FloatComplexSCHUR schur (A, std::string ());

              if (error_state)
                return retval;

              const FloatComplexMatrix U (schur.unitary_matrix ());
              const FloatComplexMatrix T (schur.schur_matrix ());
              const FloatComplexMatrix X (sqrtm_from_schur (U, T));

              retval(0) = X;

              err = frobnorm (X*X - A) / frobnorm (A);

              if (xisnan (err))
                err = lo_ieee_float_inf_value ();

              minT = lo_ieee_float_inf_value ();
              for (octave_idx_type i = 0; i < n; i++)
                minT = getmin (minT, abs (T(i,i)));
            }

          retval(1) = err;

          if (nargout < 2)
            {
              if (err > 100*(minT+FLT_EPSILON)*n)
                {
                  if (minT == 0.0)
                    error ("sqrtm: A is singular, sqrt may not exist");
                  else if (minT <= sqrt (FLT_MIN))
                    error ("sqrtm: A is nearly singular, failed to find sqrt");
                  else
                    error ("sqrtm: failed to find sqrt");
                }
            }
        }
    }
  else
    {
      if (arg.is_real_scalar ())
        {
          double d = arg.double_value ();
          if (d > 0.0)
            {
              retval(0) = sqrt (d);
              retval(1) = 0.0;
            }
          else
            {
              retval(0) = Complex (0.0, sqrt (d));
              retval(1) = 0.0;
            }
        }
      else if (arg.is_complex_scalar ())
        {
          Complex c = arg.complex_value ();
          retval(0) = sqrt (c);
          retval(1) = 0.0;
        }
      else if (arg.is_matrix_type ())
        {
          double err, minT;

          if (arg.is_real_matrix ())
            {
              Matrix A = arg.matrix_value();

              if (error_state)
                return retval;

              // FIXME -- eventually, ComplexSCHUR will accept a
              // real matrix arg.

              ComplexMatrix Ac (A);

              const ComplexSCHUR schur (Ac, std::string ());

              if (error_state)
                return retval;

              const ComplexMatrix U (schur.unitary_matrix ());
              const ComplexMatrix T (schur.schur_matrix ());
              const ComplexMatrix X (sqrtm_from_schur (U, T));

              // Check for minimal imaginary part
              double normX = 0.0;
              double imagX = 0.0;
              for (octave_idx_type i = 0; i < n; i++)
                for (octave_idx_type j = 0; j < n; j++)
                  {
                    imagX = getmax (imagX, imag (X(i,j)));
                    normX = getmax (normX, abs (X(i,j)));
                  }

              if (imagX < normX * 100 * DBL_EPSILON)
                retval(0) = real (X);
              else
                retval(0) = X;

              // Compute error
              // FIXME can we estimate the error without doing the
              // matrix multiply?

              err = frobnorm (X*X - ComplexMatrix (A)) / frobnorm (A);

              if (xisnan (err))
                err = lo_ieee_inf_value ();

              // Find min diagonal
              minT = lo_ieee_inf_value ();
              for (octave_idx_type i=0; i < n; i++)
                minT = getmin(minT, abs(T(i,i)));
            }
          else
            {
              ComplexMatrix A = arg.complex_matrix_value ();

              if (error_state)
                return retval;

              const ComplexSCHUR schur (A, std::string ());

              if (error_state)
                return retval;

              const ComplexMatrix U (schur.unitary_matrix ());
              const ComplexMatrix T (schur.schur_matrix ());
              const ComplexMatrix X (sqrtm_from_schur (U, T));

              retval(0) = X;

              err = frobnorm (X*X - A) / frobnorm (A);

              if (xisnan (err))
                err = lo_ieee_inf_value ();

              minT = lo_ieee_inf_value ();
              for (octave_idx_type i = 0; i < n; i++)
                minT = getmin (minT, abs (T(i,i)));
            }

          retval(1) = err;

          if (nargout < 2)
            {
              if (err > 100*(minT+DBL_EPSILON)*n)
                {
                  if (minT == 0.0)
                    error ("sqrtm: A is singular, sqrt may not exist");
                  else if (minT <= sqrt (DBL_MIN))
                    error ("sqrtm: A is nearly singular, failed to find sqrt");
                  else
                    error ("sqrtm: failed to find sqrt");
                }
            }
        }
      else
        gripe_wrong_type_arg ("sqrtm", arg);
    }

  return retval;
}
