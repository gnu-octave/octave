////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#include <cassert>

#include <limits>

#include "Array-util.h"
#include "CColVector.h"
#include "CDiagMatrix.h"
#include "fCDiagMatrix.h"
#include "fCMatrix.h"
#include "CMatrix.h"
#include "EIG.h"
#include "fEIG.h"
#include "dDiagMatrix.h"
#include "fDiagMatrix.h"
#include "dMatrix.h"
#include "PermMatrix.h"
#include "mx-cm-cdm.h"
#include "mx-fcm-fcdm.h"
#include "oct-cmplx.h"
#include "Range.h"
#include "quit.h"

#include "error.h"
#include "ovl.h"
#include "utils.h"
#include "xpow.h"

#include "bsxfun.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static void
err_failed_diagonalization (void)
{
  error ("Failed to diagonalize matrix while calculating matrix exponential");
}

static void
err_nonsquare_matrix (void)
{
  error ("for x^y, only square matrix arguments are permitted and one " \
         "argument must be scalar.  Use .^ for elementwise power.");
}

template <typename T>
static inline bool
xisint (T x)
{
  return (octave::math::x_nint (x) == x
          && x <= std::numeric_limits<int>::max ()
          && x >= std::numeric_limits<int>::min ());
}

static inline bool
xisint (float x)
{
  static const float out_of_range_top
    = static_cast<float>(std::numeric_limits<int>::max ()) + 1.;
  return (octave::math::x_nint (x) == x
          && x < out_of_range_top
          && x >= std::numeric_limits<int>::min ());
}

// Safer pow functions.
//
//       op2 \ op1:   s   m   cs   cm
//            +--   +---+---+----+----+
//   scalar   |     | 1 | 5 |  7 | 11 |
//                  +---+---+----+----+
//   matrix         | 2 | * |  8 |  * |
//                  +---+---+----+----+
//   complex_scalar | 3 | 6 |  9 | 12 |
//                  +---+---+----+----+
//   complex_matrix | 4 | * | 10 |  * |
//                  +---+---+----+----+

// -*- 1 -*-
octave_value
xpow (double a, double b)
{
  double retval;

  if (a < 0.0 && ! xisint (b))
    {
      Complex acplx (a);

      return std::pow (acplx, b);
    }
  else
    retval = std::pow (a, b);

  return retval;
}

// -*- 2 -*-
octave_value
xpow (double a, const Matrix& b)
{
  octave_value retval;

  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  if (nr == 0 || nc == 0)
    return Matrix ();

  if (nr != nc)
    err_nonsquare_matrix ();

  EIG b_eig (b);

  try
    {
      ComplexColumnVector lambda (b_eig.eigenvalues ());
      ComplexMatrix Q (b_eig.right_eigenvectors ());

      for (octave_idx_type i = 0; i < nr; i++)
        lambda(i) = std::pow (a, lambda(i));

      ComplexDiagMatrix D (lambda);

      ComplexMatrix C = Q * D * Q.inverse ();
      if (a > 0)
        retval = real (C);
      else
        retval = C;
    }
  catch (const octave::execution_exception&)
    {
      err_failed_diagonalization ();
    }

  return retval;
}

// -*- 3 -*-
octave_value
xpow (double a, const Complex& b)
{
  Complex result = std::pow (a, b);
  return result;
}

// -*- 4 -*-
octave_value
xpow (double a, const ComplexMatrix& b)
{
  octave_value retval;

  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  if (nr == 0 || nc == 0)
    return Matrix ();

  if (nr != nc)
    err_nonsquare_matrix ();

  EIG b_eig (b);

  try
    {
      ComplexColumnVector lambda (b_eig.eigenvalues ());
      ComplexMatrix Q (b_eig.right_eigenvectors ());

      for (octave_idx_type i = 0; i < nr; i++)
        lambda(i) = std::pow (a, lambda(i));

      ComplexDiagMatrix D (lambda);

      retval = ComplexMatrix (Q * D * Q.inverse ());
    }
  catch (const octave::execution_exception&)
    {
      err_failed_diagonalization ();
    }

  return retval;
}

// -*- 5 -*-
octave_value
xpow (const Matrix& a, double b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0)
    return Matrix ();

  if (nr != nc)
    err_nonsquare_matrix ();

  if (xisint (b))
    {
      int bint = static_cast<int> (b);
      if (bint == 0)
        {
          retval = DiagMatrix (nr, nr, 1.0);
        }
      else
        {
          // Too much copying?
          // FIXME: we shouldn't do this if the exponent is large...

          Matrix atmp;
          if (bint < 0)
            {
              bint = -bint;

              octave_idx_type info;
              double rcond = 0.0;
              MatrixType mattype (a);

              atmp = a.inverse (mattype, info, rcond, 1);

              if (info == -1)
                warning ("inverse: matrix singular to machine precision, rcond = %g", rcond);
            }
          else
            atmp = a;

          Matrix result (atmp);

          bint--;

          while (bint > 0)
            {
              if (bint & 1)
                // Use atmp * result instead of result * atmp
                // for ML compatibility (bug #52706).
                result = atmp * result;

              bint >>= 1;

              if (bint > 0)
                atmp = atmp * atmp;
            }

          retval = result;
        }
    }
  else
    {
      EIG a_eig (a);

      try
        {
          ComplexColumnVector lambda (a_eig.eigenvalues ());
          ComplexMatrix Q (a_eig.right_eigenvectors ());

          for (octave_idx_type i = 0; i < nr; i++)
            lambda(i) = std::pow (lambda(i), b);

          ComplexDiagMatrix D (lambda);

          retval = ComplexMatrix (Q * D * Q.inverse ());
        }
      catch (const octave::execution_exception&)
        {
          err_failed_diagonalization ();
        }
    }

  return retval;
}

// -*- 5d -*-
octave_value
xpow (const DiagMatrix& a, double b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0)
    return Matrix ();

  if (nr != nc)
    err_nonsquare_matrix ();

  if (xisint (b))
    {
      int bint = static_cast<int> (b);
      DiagMatrix r (nr, nc);
      for (octave_idx_type i = 0; i < nc; i++)
        r.dgxelem (i) = std::pow (a.dgxelem (i), bint);
      retval = r;
    }
  else
    {
      ComplexDiagMatrix r (nr, nc);
      for (octave_idx_type i = 0; i < nc; i++)
        r.dgxelem (i) = std::pow (static_cast<Complex> (a.dgxelem (i)), b);
      retval = r;
    }

  return retval;
}

// -*- 5p -*-
octave_value
xpow (const PermMatrix& a, double b)
{
  if (xisint (b))
    return a.power (static_cast<int> (b));
  else
    return xpow (Matrix (a), b);
}

// -*- 6 -*-
octave_value
xpow (const Matrix& a, const Complex& b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0)
    return Matrix ();

  if (nr != nc)
    err_nonsquare_matrix ();

  EIG a_eig (a);

  try
    {
      ComplexColumnVector lambda (a_eig.eigenvalues ());
      ComplexMatrix Q (a_eig.right_eigenvectors ());

      for (octave_idx_type i = 0; i < nr; i++)
        lambda(i) = std::pow (lambda(i), b);

      ComplexDiagMatrix D (lambda);

      retval = ComplexMatrix (Q * D * Q.inverse ());
    }
  catch (const octave::execution_exception&)
    {
      err_failed_diagonalization ();
    }

  return retval;
}

// -*- 7 -*-
octave_value
xpow (const Complex& a, double b)
{
  Complex result;

  if (xisint (b))
    result = std::pow (a, static_cast<int> (b));
  else
    result = std::pow (a, b);

  return result;
}

// -*- 8 -*-
octave_value
xpow (const Complex& a, const Matrix& b)
{
  octave_value retval;

  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  if (nr == 0 || nc == 0)
    return Matrix ();

  if (nr != nc)
    err_nonsquare_matrix ();

  EIG b_eig (b);

  try
    {
      ComplexColumnVector lambda (b_eig.eigenvalues ());
      ComplexMatrix Q (b_eig.right_eigenvectors ());

      for (octave_idx_type i = 0; i < nr; i++)
        lambda(i) = std::pow (a, lambda(i));

      ComplexDiagMatrix D (lambda);

      retval = ComplexMatrix (Q * D * Q.inverse ());
    }
  catch (const octave::execution_exception&)
    {
      err_failed_diagonalization ();
    }

  return retval;
}

// -*- 9 -*-
octave_value
xpow (const Complex& a, const Complex& b)
{
  Complex result;
  result = std::pow (a, b);
  return result;
}

// -*- 10 -*-
octave_value
xpow (const Complex& a, const ComplexMatrix& b)
{
  octave_value retval;

  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  if (nr == 0 || nc == 0)
    return Matrix ();

  if (nr != nc)
    err_nonsquare_matrix ();

  EIG b_eig (b);

  try
    {
      ComplexColumnVector lambda (b_eig.eigenvalues ());
      ComplexMatrix Q (b_eig.right_eigenvectors ());

      for (octave_idx_type i = 0; i < nr; i++)
        lambda(i) = std::pow (a, lambda(i));

      ComplexDiagMatrix D (lambda);

      retval = ComplexMatrix (Q * D * Q.inverse ());
    }
  catch (const octave::execution_exception&)
    {
      err_failed_diagonalization ();
    }

  return retval;
}

// -*- 11 -*-
octave_value
xpow (const ComplexMatrix& a, double b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0)
    return Matrix ();

  if (nr != nc)
    err_nonsquare_matrix ();

  if (xisint (b))
    {
      int bint = static_cast<int> (b);
      if (bint == 0)
        {
          retval = DiagMatrix (nr, nr, 1.0);
        }
      else
        {
          // Too much copying?
          // FIXME: we shouldn't do this if the exponent is large...

          ComplexMatrix atmp;
          if (bint < 0)
            {
              bint = -bint;

              octave_idx_type info;
              double rcond = 0.0;
              MatrixType mattype (a);

              atmp = a.inverse (mattype, info, rcond, 1);

              if (info == -1)
                warning ("inverse: matrix singular to machine precision, rcond = %g", rcond);
            }
          else
            atmp = a;

          ComplexMatrix result (atmp);

          bint--;

          while (bint > 0)
            {
              if (bint & 1)
                // Use atmp * result instead of result * atmp
                // for ML compatibility (bug #52706).
                result = atmp * result;

              bint >>= 1;

              if (bint > 0)
                atmp = atmp * atmp;
            }

          retval = result;
        }
    }
  else
    {
      EIG a_eig (a);

      try
        {
          ComplexColumnVector lambda (a_eig.eigenvalues ());
          ComplexMatrix Q (a_eig.right_eigenvectors ());

          for (octave_idx_type i = 0; i < nr; i++)
            lambda(i) = std::pow (lambda(i), b);

          ComplexDiagMatrix D (lambda);

          retval = ComplexMatrix (Q * D * Q.inverse ());
        }
      catch (const octave::execution_exception&)
        {
          err_failed_diagonalization ();
        }
    }

  return retval;
}

// -*- 12 -*-
octave_value
xpow (const ComplexMatrix& a, const Complex& b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0)
    return Matrix ();

  if (nr != nc)
    err_nonsquare_matrix ();

  EIG a_eig (a);

  try
    {
      ComplexColumnVector lambda (a_eig.eigenvalues ());
      ComplexMatrix Q (a_eig.right_eigenvectors ());

      for (octave_idx_type i = 0; i < nr; i++)
        lambda(i) = std::pow (lambda(i), b);

      ComplexDiagMatrix D (lambda);

      retval = ComplexMatrix (Q * D * Q.inverse ());
    }
  catch (const octave::execution_exception&)
    {
      err_failed_diagonalization ();
    }

  return retval;
}

// -*- 12d -*-
octave_value
xpow (const ComplexDiagMatrix& a, const Complex& b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0)
    return Matrix ();

  if (nr != nc)
    err_nonsquare_matrix ();

  ComplexDiagMatrix r (nr, nc);
  for (octave_idx_type i = 0; i < nc; i++)
    r.dgxelem (i) = std::pow (a.dgxelem (i), b);
  retval = r;

  return retval;
}

// mixed
octave_value
xpow (const ComplexDiagMatrix& a, double b)
{
  return xpow (a, static_cast<Complex> (b));
}

octave_value
xpow (const DiagMatrix& a, const Complex& b)
{
  return xpow (ComplexDiagMatrix (a), b);
}

// Safer pow functions that work elementwise for matrices.
//
//       op2 \ op1:   s   m   cs   cm
//            +--   +---+---+----+----+
//   scalar   |     | * | 3 |  * |  9 |
//                  +---+---+----+----+
//   matrix         | 1 | 4 |  7 | 10 |
//                  +---+---+----+----+
//   complex_scalar | * | 5 |  * | 11 |
//                  +---+---+----+----+
//   complex_matrix | 2 | 6 |  8 | 12 |
//                  +---+---+----+----+
//
//   * -> not needed.

// FIXME: these functions need to be fixed so that things like
//
//   a = -1; b = [ 0, 0.5, 1 ]; r = a .^ b
//
// and
//
//   a = -1; b = [ 0, 0.5, 1 ]; for i = 1:3, r(i) = a .^ b(i), end
//
// produce identical results.  Also, it would be nice if -1^0.5
// produced a pure imaginary result instead of a complex number with a
// small real part.  But perhaps that's really a problem with the math
// library...

// -*- 1 -*-
octave_value
elem_xpow (double a, const Matrix& b)
{
  octave_value retval;

  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  double d1, d2;

  if (a < 0.0 && ! b.all_integers (d1, d2))
    {
      Complex acplx (a);
      ComplexMatrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result(i, j) = std::pow (acplx, b(i, j));
          }

      retval = result;
    }
  else
    {
      Matrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result(i, j) = std::pow (a, b(i, j));
          }

      retval = result;
    }

  return retval;
}

// -*- 2 -*-
octave_value
elem_xpow (double a, const ComplexMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  ComplexMatrix result (nr, nc);
  Complex acplx (a);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = std::pow (acplx, b(i, j));
      }

  return result;
}

static inline bool
same_sign (double a, double b)
{
  return (a >= 0 && b >= 0) || (a <= 0 && b <= 0);
}

octave_value
elem_xpow (double a, const octave::range<double>& r)
{
  octave_value retval;

  // Only optimize powers with ranges that are integer and monotonic in
  // magnitude.
  if (r.numel () > 1 && r.all_elements_are_ints ()
      && same_sign (r.base (), r.limit ()))
    {
      octave_idx_type n = r.numel ();
      Matrix result (1, n);
      if (same_sign (r.base (), r.increment ()))
        {
          double base = std::pow (a, r.base ());
          double inc = std::pow (a, r.increment ());
          result(0) = base;
          for (octave_idx_type i = 1; i < n; i++)
            result(i) = (base *= inc);
        }
      else
        {
          double limit = std::pow (a, r.final_value ());
          double inc = std::pow (a, -r.increment ());
          result(n-1) = limit;
          for (octave_idx_type i = n-2; i >= 0; i--)
            result(i) = (limit *= inc);
        }

      retval = result;
    }
  else
    {
      Matrix tmp = r.array_value ();
      retval = elem_xpow (a, tmp);
    }

  return retval;
}

// -*- 3 -*-
octave_value
elem_xpow (const Matrix& a, double b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (! xisint (b) && a.any_element_is_negative ())
    {
      ComplexMatrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();

            Complex acplx (a(i, j));

            result(i, j) = std::pow (acplx, b);
          }

      retval = result;
    }
  else
    {
      Matrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result(i, j) = std::pow (a(i, j), b);
          }

      retval = result;
    }

  return retval;
}

// -*- 4 -*-
octave_value
elem_xpow (const Matrix& a, const Matrix& b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (nr != b_nr || nc != b_nc)
    octave::err_nonconformant ("operator .^", nr, nc, b_nr, b_nc);

  bool convert_to_complex = false;
  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        double atmp = a(i, j);
        double btmp = b(i, j);
        if (atmp < 0.0 && ! xisint (btmp))
          {
            convert_to_complex = true;
            goto done;
          }
      }

done:

  if (convert_to_complex)
    {
      ComplexMatrix complex_result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            Complex acplx (a(i, j));
            Complex bcplx (b(i, j));
            complex_result(i, j) = std::pow (acplx, bcplx);
          }

      retval = complex_result;
    }
  else
    {
      Matrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result(i, j) = std::pow (a(i, j), b(i, j));
          }

      retval = result;
    }

  return retval;
}

// -*- 5 -*-
octave_value
elem_xpow (const Matrix& a, const Complex& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = std::pow (Complex (a(i, j)), b);
      }

  return result;
}

// -*- 6 -*-
octave_value
elem_xpow (const Matrix& a, const ComplexMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (nr != b_nr || nc != b_nc)
    octave::err_nonconformant ("operator .^", nr, nc, b_nr, b_nc);

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = std::pow (Complex (a(i, j)), b(i, j));
      }

  return result;
}

// -*- 7 -*-
octave_value
elem_xpow (const Complex& a, const Matrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        double btmp = b(i, j);
        if (xisint (btmp))
          result(i, j) = std::pow (a, static_cast<int> (btmp));
        else
          result(i, j) = std::pow (a, btmp);
      }

  return result;
}

// -*- 8 -*-
octave_value
elem_xpow (const Complex& a, const ComplexMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = std::pow (a, b(i, j));
      }

  return result;
}

octave_value
elem_xpow (const Complex& a, const octave::range<double>& r)
{
  octave_value retval;

  // Only optimize powers with ranges that are integer and monotonic in
  // magnitude.
  if (r.numel () > 1 && r.all_elements_are_ints ()
      && same_sign (r.base (), r.limit ()))
    {
      octave_idx_type n = r.numel ();
      ComplexMatrix result (1, n);

      if (same_sign (r.base (), r.increment ()))
        {
          Complex base = std::pow (a, r.base ());
          Complex inc = std::pow (a, r.increment ());
          result(0) = base;
          for (octave_idx_type i = 1; i < n; i++)
            result(i) = (base *= inc);
        }
      else
        {
          Complex limit = std::pow (a, r.final_value ());
          Complex inc = std::pow (a, -r.increment ());
          result(n-1) = limit;
          for (octave_idx_type i = n-2; i >= 0; i--)
            result(i) = (limit *= inc);
        }

      retval = result;
    }
  else
    {
      Matrix tmp = r.array_value ();
      retval = elem_xpow (a, tmp);
    }

  return retval;
}

// -*- 9 -*-
octave_value
elem_xpow (const ComplexMatrix& a, double b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  ComplexMatrix result (nr, nc);

  if (xisint (b))
    {
      int bint = static_cast<int> (b);
      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result(i, j) = std::pow (a(i, j), bint);
          }
    }
  else
    {
      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result(i, j) = std::pow (a(i, j), b);
          }
    }

  return result;
}

// -*- 10 -*-
octave_value
elem_xpow (const ComplexMatrix& a, const Matrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (nr != b_nr || nc != b_nc)
    octave::err_nonconformant ("operator .^", nr, nc, b_nr, b_nc);

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        double btmp = b(i, j);
        if (xisint (btmp))
          result(i, j) = std::pow (a(i, j), static_cast<int> (btmp));
        else
          result(i, j) = std::pow (a(i, j), btmp);
      }

  return result;
}

// -*- 11 -*-
octave_value
elem_xpow (const ComplexMatrix& a, const Complex& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = std::pow (a(i, j), b);
      }

  return result;
}

// -*- 12 -*-
octave_value
elem_xpow (const ComplexMatrix& a, const ComplexMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (nr != b_nr || nc != b_nc)
    octave::err_nonconformant ("operator .^", nr, nc, b_nr, b_nc);

  ComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = std::pow (a(i, j), b(i, j));
      }

  return result;
}

// Safer pow functions that work elementwise for N-D arrays.
//
//       op2 \ op1:   s   nd  cs   cnd
//            +--   +---+---+----+----+
//   scalar   |     | * | 3 |  * |  9 |
//                  +---+---+----+----+
//   N_d            | 1 | 4 |  7 | 10 |
//                  +---+---+----+----+
//   complex_scalar | * | 5 |  * | 11 |
//                  +---+---+----+----+
//   complex_N_d    | 2 | 6 |  8 | 12 |
//                  +---+---+----+----+
//
//   * -> not needed.

// FIXME: these functions need to be fixed so that things like
//
//   a = -1; b = [ 0, 0.5, 1 ]; r = a .^ b
//
// and
//
//   a = -1; b = [ 0, 0.5, 1 ]; for i = 1:3, r(i) = a .^ b(i), end
//
// produce identical results.  Also, it would be nice if -1^0.5
// produced a pure imaginary result instead of a complex number with a
// small real part.  But perhaps that's really a problem with the math
// library...

// -*- 1 -*-
octave_value
elem_xpow (double a, const NDArray& b)
{
  octave_value retval;

  if (a < 0.0 && ! b.all_integers ())
    {
      Complex acplx (a);
      ComplexNDArray result (b.dims ());
      for (octave_idx_type i = 0; i < b.numel (); i++)
        {
          octave_quit ();
          result(i) = std::pow (acplx, b(i));
        }

      retval = result;
    }
  else
    {
      NDArray result (b.dims ());
      for (octave_idx_type i = 0; i < b.numel (); i++)
        {
          octave_quit ();
          result(i) = std::pow (a, b(i));
        }

      retval = result;
    }

  return retval;
}

// -*- 2 -*-
octave_value
elem_xpow (double a, const ComplexNDArray& b)
{
  ComplexNDArray result (b.dims ());

  for (octave_idx_type i = 0; i < b.numel (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a, b(i));
    }

  return result;
}

// -*- 3 -*-
octave_value
elem_xpow (const NDArray& a, double b)
{
  octave_value retval;

  if (xisint (b))
    {
      NDArray result (a.dims ());

      int bint = static_cast<int> (b);
      if (bint == 2)
        {
          for (octave_idx_type i = 0; i < a.numel (); i++)
            result.xelem (i) = a(i) * a(i);
        }
      else if (bint == 3)
        {
          for (octave_idx_type i = 0; i < a.numel (); i++)
            result.xelem (i) = a(i) * a(i) * a(i);
        }
      else if (bint == -1)
        {
          for (octave_idx_type i = 0; i < a.numel (); i++)
            result.xelem (i) = 1.0 / a(i);
        }
      else
        {
          for (octave_idx_type i = 0; i < a.numel (); i++)
            {
              octave_quit ();
              result.xelem (i) = std::pow (a(i), bint);
            }
        }

      retval = result;
    }
  else
    {
      if (a.any_element_is_negative ())
        {
          ComplexNDArray result (a.dims ());

          for (octave_idx_type i = 0; i < a.numel (); i++)
            {
              octave_quit ();
              Complex acplx (a(i));
              result(i) = std::pow (acplx, b);
            }

          retval = result;
        }
      else
        {
          NDArray result (a.dims ());
          for (octave_idx_type i = 0; i < a.numel (); i++)
            {
              octave_quit ();
              result(i) = std::pow (a(i), b);
            }

          retval = result;
        }
    }

  return retval;
}

// -*- 4 -*-
octave_value
elem_xpow (const NDArray& a, const NDArray& b)
{
  octave_value retval;

  dim_vector a_dims = a.dims ();
  dim_vector b_dims = b.dims ();

  if (a_dims != b_dims)
    {
      if (! is_valid_bsxfun ("operator .^", a_dims, b_dims))
        octave::err_nonconformant ("operator .^", a_dims, b_dims);

      // Potentially complex results
      NDArray xa = octave_value_extract<NDArray> (a);
      NDArray xb = octave_value_extract<NDArray> (b);
      if (! xb.all_integers () && xa.any_element_is_negative ())
        return octave_value (bsxfun_pow (ComplexNDArray (xa), xb));
      else
        return octave_value (bsxfun_pow (xa, xb));
    }

  int len = a.numel ();

  bool convert_to_complex = false;

  for (octave_idx_type i = 0; i < len; i++)
    {
      octave_quit ();
      double atmp = a(i);
      double btmp = b(i);
      if (atmp < 0.0 && ! xisint (btmp))
        {
          convert_to_complex = true;
          goto done;
        }
    }

done:

  if (convert_to_complex)
    {
      ComplexNDArray complex_result (a_dims);

      for (octave_idx_type i = 0; i < len; i++)
        {
          octave_quit ();
          Complex acplx (a(i));
          complex_result(i) = std::pow (acplx, b(i));
        }

      retval = complex_result;
    }
  else
    {
      NDArray result (a_dims);

      for (octave_idx_type i = 0; i < len; i++)
        {
          octave_quit ();
          result(i) = std::pow (a(i), b(i));
        }

      retval = result;
    }

  return retval;
}

// -*- 5 -*-
octave_value
elem_xpow (const NDArray& a, const Complex& b)
{
  ComplexNDArray result (a.dims ());

  for (octave_idx_type i = 0; i < a.numel (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a(i), b);
    }

  return result;
}

// -*- 6 -*-
octave_value
elem_xpow (const NDArray& a, const ComplexNDArray& b)
{
  dim_vector a_dims = a.dims ();
  dim_vector b_dims = b.dims ();

  if (a_dims != b_dims)
    {
      if (! is_valid_bsxfun ("operator .^", a_dims, b_dims))
        octave::err_nonconformant ("operator .^", a_dims, b_dims);

      return bsxfun_pow (a, b);
    }

  ComplexNDArray result (a_dims);

  for (octave_idx_type i = 0; i < a.numel (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a(i), b(i));
    }

  return result;
}

// -*- 7 -*-
octave_value
elem_xpow (const Complex& a, const NDArray& b)
{
  ComplexNDArray result (b.dims ());

  for (octave_idx_type i = 0; i < b.numel (); i++)
    {
      octave_quit ();
      double btmp = b(i);
      if (xisint (btmp))
        result(i) = std::pow (a, static_cast<int> (btmp));
      else
        result(i) = std::pow (a, btmp);
    }

  return result;
}

// -*- 8 -*-
octave_value
elem_xpow (const Complex& a, const ComplexNDArray& b)
{
  ComplexNDArray result (b.dims ());

  for (octave_idx_type i = 0; i < b.numel (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a, b(i));
    }

  return result;
}

// -*- 9 -*-
octave_value
elem_xpow (const ComplexNDArray& a, double b)
{
  ComplexNDArray result (a.dims ());

  if (xisint (b))
    {
      int bint = static_cast<int> (b);
      if (bint == -1)
        {
          for (octave_idx_type i = 0; i < a.numel (); i++)
            result.xelem (i) = 1.0 / a(i);
        }
      else
        {
          for (octave_idx_type i = 0; i < a.numel (); i++)
            {
              octave_quit ();
              result(i) = std::pow (a(i), bint);
            }
        }
    }
  else
    {
      for (octave_idx_type i = 0; i < a.numel (); i++)
        {
          octave_quit ();
          result(i) = std::pow (a(i), b);
        }
    }

  return result;
}

// -*- 10 -*-
octave_value
elem_xpow (const ComplexNDArray& a, const NDArray& b)
{
  dim_vector a_dims = a.dims ();
  dim_vector b_dims = b.dims ();

  if (a_dims != b_dims)
    {
      if (! is_valid_bsxfun ("operator .^", a_dims, b_dims))
        octave::err_nonconformant ("operator .^", a_dims, b_dims);

      return bsxfun_pow (a, b);
    }

  ComplexNDArray result (a_dims);

  for (octave_idx_type i = 0; i < a.numel (); i++)
    {
      octave_quit ();
      double btmp = b(i);
      if (xisint (btmp))
        result(i) = std::pow (a(i), static_cast<int> (btmp));
      else
        result(i) = std::pow (a(i), btmp);
    }

  return result;
}

// -*- 11 -*-
octave_value
elem_xpow (const ComplexNDArray& a, const Complex& b)
{
  ComplexNDArray result (a.dims ());

  for (octave_idx_type i = 0; i < a.numel (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a(i), b);
    }

  return result;
}

// -*- 12 -*-
octave_value
elem_xpow (const ComplexNDArray& a, const ComplexNDArray& b)
{
  dim_vector a_dims = a.dims ();
  dim_vector b_dims = b.dims ();

  if (a_dims != b_dims)
    {
      if (! is_valid_bsxfun ("operator .^", a_dims, b_dims))
        octave::err_nonconformant ("operator .^", a_dims, b_dims);

      return bsxfun_pow (a, b);
    }

  ComplexNDArray result (a_dims);

  for (octave_idx_type i = 0; i < a.numel (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a(i), b(i));
    }

  return result;
}

// Safer pow functions.
//
//       op2 \ op1:   s   m   cs   cm
//            +--   +---+---+----+----+
//   scalar   |     | 1 | 5 |  7 | 11 |
//                  +---+---+----+----+
//   matrix         | 2 | * |  8 |  * |
//                  +---+---+----+----+
//   complex_scalar | 3 | 6 |  9 | 12 |
//                  +---+---+----+----+
//   complex_matrix | 4 | * | 10 |  * |
//                  +---+---+----+----+

// -*- 1 -*-
octave_value
xpow (float a, float b)
{
  float retval;

  if (a < 0.0 && ! xisint (b))
    {
      FloatComplex acplx (a);

      return std::pow (acplx, b);
    }
  else
    retval = std::pow (a, b);

  return retval;
}

// -*- 2 -*-
octave_value
xpow (float a, const FloatMatrix& b)
{
  octave_value retval;

  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  if (nr == 0 || nc == 0)
    return FloatMatrix ();

  if (nr != nc)
    err_nonsquare_matrix ();

  FloatEIG b_eig (b);

  try
    {
      FloatComplexColumnVector lambda (b_eig.eigenvalues ());
      FloatComplexMatrix Q (b_eig.right_eigenvectors ());

      for (octave_idx_type i = 0; i < nr; i++)
        lambda(i) = std::pow (a, lambda(i));

      FloatComplexDiagMatrix D (lambda);

      FloatComplexMatrix C = Q * D * Q.inverse ();

      if (a > 0)
        retval = real (C);
      else
        retval = C;
    }
  catch (const octave::execution_exception&)
    {
      err_failed_diagonalization ();
    }

  return retval;
}

// -*- 3 -*-
octave_value
xpow (float a, const FloatComplex& b)
{
  FloatComplex result = std::pow (a, b);
  return result;
}

// -*- 4 -*-
octave_value
xpow (float a, const FloatComplexMatrix& b)
{
  octave_value retval;

  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  if (nr == 0 || nc == 0)
    return FloatMatrix ();

  if (nr != nc)
    err_nonsquare_matrix ();

  FloatEIG b_eig (b);

  try
    {
      FloatComplexColumnVector lambda (b_eig.eigenvalues ());
      FloatComplexMatrix Q (b_eig.right_eigenvectors ());

      for (octave_idx_type i = 0; i < nr; i++)
        lambda(i) = std::pow (a, lambda(i));

      FloatComplexDiagMatrix D (lambda);

      retval = FloatComplexMatrix (Q * D * Q.inverse ());
    }
  catch (const octave::execution_exception&)
    {
      err_failed_diagonalization ();
    }

  return retval;
}

// -*- 5 -*-
octave_value
xpow (const FloatMatrix& a, float b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0)
    return FloatMatrix ();

  if (nr != nc)
    err_nonsquare_matrix ();

  if (xisint (b))
    {
      int bint = static_cast<int> (b);
      if (bint == 0)
        {
          retval = FloatDiagMatrix (nr, nr, 1.0f);
        }
      else
        {
          // Too much copying?
          // FIXME: we shouldn't do this if the exponent is large...

          FloatMatrix atmp;
          if (bint < 0)
            {
              bint = -bint;

              octave_idx_type info;
              float rcond = 0.0;
              MatrixType mattype (a);

              atmp = a.inverse (mattype, info, rcond, 1);

              if (info == -1)
                warning ("inverse: matrix singular to machine precision, rcond = %g", rcond);
            }
          else
            atmp = a;

          FloatMatrix result (atmp);

          bint--;

          while (bint > 0)
            {
              if (bint & 1)
                // Use atmp * result instead of result * atmp
                // for ML compatibility (bug #52706).
                result = atmp * result;

              bint >>= 1;

              if (bint > 0)
                atmp = atmp * atmp;
            }

          retval = result;
        }
    }
  else
    {
      FloatEIG a_eig (a);

      try
        {
          FloatComplexColumnVector lambda (a_eig.eigenvalues ());
          FloatComplexMatrix Q (a_eig.right_eigenvectors ());

          for (octave_idx_type i = 0; i < nr; i++)
            lambda(i) = std::pow (lambda(i), b);

          FloatComplexDiagMatrix D (lambda);

          retval = FloatComplexMatrix (Q * D * Q.inverse ());
        }
      catch (const octave::execution_exception&)
        {
          err_failed_diagonalization ();
        }
    }

  return retval;
}

// -*- 5d -*-
octave_value
xpow (const FloatDiagMatrix& a, float b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0)
    return FloatMatrix ();

  if (nr != nc)
    err_nonsquare_matrix ();

  if (xisint (b))
    {
      int bint = static_cast<int> (b);
      FloatDiagMatrix r (nr, nc);
      for (octave_idx_type i = 0; i < nc; i++)
        r.dgxelem (i) = std::pow (a.dgxelem (i), bint);
      retval = r;
    }
  else
    {
      FloatComplexDiagMatrix r (nr, nc);
      for (octave_idx_type i = 0; i < nc; i++)
        r.dgxelem (i) = std::pow (static_cast<FloatComplex> (a.dgxelem (i)), b);
      retval = r;
    }

  return retval;
}

// -*- 6 -*-
octave_value
xpow (const FloatMatrix& a, const FloatComplex& b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0)
    return FloatMatrix ();

  if (nr != nc)
    err_nonsquare_matrix ();

  FloatEIG a_eig (a);

  try
    {
      FloatComplexColumnVector lambda (a_eig.eigenvalues ());
      FloatComplexMatrix Q (a_eig.right_eigenvectors ());

      for (octave_idx_type i = 0; i < nr; i++)
        lambda(i) = std::pow (lambda(i), b);

      FloatComplexDiagMatrix D (lambda);

      retval = FloatComplexMatrix (Q * D * Q.inverse ());
    }
  catch (const octave::execution_exception&)
    {
      err_failed_diagonalization ();
    }

  return retval;
}

// -*- 7 -*-
octave_value
xpow (const FloatComplex& a, float b)
{
  FloatComplex result;

  if (xisint (b))
    result = std::pow (a, static_cast<int> (b));
  else
    result = std::pow (a, b);

  return result;
}

// -*- 8 -*-
octave_value
xpow (const FloatComplex& a, const FloatMatrix& b)
{
  octave_value retval;

  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  if (nr == 0 || nc == 0)
    return FloatMatrix ();

  if (nr != nc)
    err_nonsquare_matrix ();

  FloatEIG b_eig (b);

  try
    {
      FloatComplexColumnVector lambda (b_eig.eigenvalues ());
      FloatComplexMatrix Q (b_eig.right_eigenvectors ());

      for (octave_idx_type i = 0; i < nr; i++)
        lambda(i) = std::pow (a, lambda(i));

      FloatComplexDiagMatrix D (lambda);

      retval = FloatComplexMatrix (Q * D * Q.inverse ());
    }
  catch (const octave::execution_exception&)
    {
      err_failed_diagonalization ();
    }

  return retval;
}

// -*- 9 -*-
octave_value
xpow (const FloatComplex& a, const FloatComplex& b)
{
  FloatComplex result;
  result = std::pow (a, b);
  return result;
}

// -*- 10 -*-
octave_value
xpow (const FloatComplex& a, const FloatComplexMatrix& b)
{
  octave_value retval;

  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  if (nr == 0 || nc == 0)
    return FloatMatrix ();

  if (nr != nc)
    err_nonsquare_matrix ();

  FloatEIG b_eig (b);

  try
    {
      FloatComplexColumnVector lambda (b_eig.eigenvalues ());
      FloatComplexMatrix Q (b_eig.right_eigenvectors ());

      for (octave_idx_type i = 0; i < nr; i++)
        lambda(i) = std::pow (a, lambda(i));

      FloatComplexDiagMatrix D (lambda);

      retval = FloatComplexMatrix (Q * D * Q.inverse ());
    }
  catch (const octave::execution_exception&)
    {
      err_failed_diagonalization ();
    }

  return retval;
}

// -*- 11 -*-
octave_value
xpow (const FloatComplexMatrix& a, float b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0)
    return FloatMatrix ();

  if (nr != nc)
    err_nonsquare_matrix ();

  if (xisint (b))
    {
      int bint = static_cast<int> (b);
      if (bint == 0)
        {
          retval = FloatDiagMatrix (nr, nr, 1.0);
        }
      else
        {
          // Too much copying?
          // FIXME: we shouldn't do this if the exponent is large...

          FloatComplexMatrix atmp;
          if (bint < 0)
            {
              bint = -bint;

              octave_idx_type info;
              float rcond = 0.0;
              MatrixType mattype (a);

              atmp = a.inverse (mattype, info, rcond, 1);

              if (info == -1)
                warning ("inverse: matrix singular to machine precision, rcond = %g", rcond);
            }
          else
            atmp = a;

          FloatComplexMatrix result (atmp);

          bint--;

          while (bint > 0)
            {
              if (bint & 1)
                // Use atmp * result instead of result * atmp
                // for ML compatibility (bug #52706).
                result = atmp * result;

              bint >>= 1;

              if (bint > 0)
                atmp = atmp * atmp;
            }

          retval = result;
        }
    }
  else
    {
      FloatEIG a_eig (a);

      try
        {
          FloatComplexColumnVector lambda (a_eig.eigenvalues ());
          FloatComplexMatrix Q (a_eig.right_eigenvectors ());

          for (octave_idx_type i = 0; i < nr; i++)
            lambda(i) = std::pow (lambda(i), b);

          FloatComplexDiagMatrix D (lambda);

          retval = FloatComplexMatrix (Q * D * Q.inverse ());
        }
      catch (const octave::execution_exception&)
        {
          err_failed_diagonalization ();
        }
    }

  return retval;
}

// -*- 12 -*-
octave_value
xpow (const FloatComplexMatrix& a, const FloatComplex& b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0)
    return FloatMatrix ();

  if (nr != nc)
    err_nonsquare_matrix ();

  FloatEIG a_eig (a);

  try
    {
      FloatComplexColumnVector lambda (a_eig.eigenvalues ());
      FloatComplexMatrix Q (a_eig.right_eigenvectors ());

      for (octave_idx_type i = 0; i < nr; i++)
        lambda(i) = std::pow (lambda(i), b);

      FloatComplexDiagMatrix D (lambda);

      retval = FloatComplexMatrix (Q * D * Q.inverse ());
    }
  catch (const octave::execution_exception&)
    {
      err_failed_diagonalization ();
    }

  return retval;
}

// -*- 12d -*-
octave_value
xpow (const FloatComplexDiagMatrix& a, const FloatComplex& b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr == 0 || nc == 0)
    return FloatMatrix ();

  if (nr != nc)
    err_nonsquare_matrix ();

  FloatComplexDiagMatrix r (nr, nc);
  for (octave_idx_type i = 0; i < nc; i++)
    r.dgxelem (i) = std::pow (a.dgxelem (i), b);
  retval = r;

  return retval;
}

// mixed
octave_value
xpow (const FloatComplexDiagMatrix& a, float b)
{
  return xpow (a, static_cast<FloatComplex> (b));
}

octave_value
xpow (const FloatDiagMatrix& a, const FloatComplex& b)
{
  return xpow (FloatComplexDiagMatrix (a), b);
}

// Safer pow functions that work elementwise for matrices.
//
//       op2 \ op1:   s   m   cs   cm
//            +--   +---+---+----+----+
//   scalar   |     | * | 3 |  * |  9 |
//                  +---+---+----+----+
//   matrix         | 1 | 4 |  7 | 10 |
//                  +---+---+----+----+
//   complex_scalar | * | 5 |  * | 11 |
//                  +---+---+----+----+
//   complex_matrix | 2 | 6 |  8 | 12 |
//                  +---+---+----+----+
//
//   * -> not needed.

// FIXME: these functions need to be fixed so that things like
//
//   a = -1; b = [ 0, 0.5, 1 ]; r = a .^ b
//
// and
//
//   a = -1; b = [ 0, 0.5, 1 ]; for i = 1:3, r(i) = a .^ b(i), end
//
// produce identical results.  Also, it would be nice if -1^0.5
// produced a pure imaginary result instead of a complex number with a
// small real part.  But perhaps that's really a problem with the math
// library...

// -*- 1 -*-
octave_value
elem_xpow (float a, const FloatMatrix& b)
{
  octave_value retval;

  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  float d1, d2;

  if (a < 0.0 && ! b.all_integers (d1, d2))
    {
      FloatComplex acplx (a);
      FloatComplexMatrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result(i, j) = std::pow (acplx, b(i, j));
          }

      retval = result;
    }
  else
    {
      FloatMatrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result(i, j) = std::pow (a, b(i, j));
          }

      retval = result;
    }

  return retval;
}

// -*- 2 -*-
octave_value
elem_xpow (float a, const FloatComplexMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  FloatComplexMatrix result (nr, nc);
  FloatComplex acplx (a);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = std::pow (acplx, b(i, j));
      }

  return result;
}

// -*- 3 -*-
octave_value
elem_xpow (const FloatMatrix& a, float b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (! xisint (b) && a.any_element_is_negative ())
    {
      FloatComplexMatrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();

            FloatComplex acplx (a(i, j));

            result(i, j) = std::pow (acplx, b);
          }

      retval = result;
    }
  else
    {
      FloatMatrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result(i, j) = std::pow (a(i, j), b);
          }

      retval = result;
    }

  return retval;
}

// -*- 4 -*-
octave_value
elem_xpow (const FloatMatrix& a, const FloatMatrix& b)
{
  octave_value retval;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (nr != b_nr || nc != b_nc)
    octave::err_nonconformant ("operator .^", nr, nc, b_nr, b_nc);

  bool convert_to_complex = false;
  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        float atmp = a(i, j);
        float btmp = b(i, j);
        if (atmp < 0.0 && ! xisint (btmp))
          {
            convert_to_complex = true;
            goto done;
          }
      }

done:

  if (convert_to_complex)
    {
      FloatComplexMatrix complex_result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            FloatComplex acplx (a(i, j));
            FloatComplex bcplx (b(i, j));
            complex_result(i, j) = std::pow (acplx, bcplx);
          }

      retval = complex_result;
    }
  else
    {
      FloatMatrix result (nr, nc);

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result(i, j) = std::pow (a(i, j), b(i, j));
          }

      retval = result;
    }

  return retval;
}

// -*- 5 -*-
octave_value
elem_xpow (const FloatMatrix& a, const FloatComplex& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  FloatComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = std::pow (FloatComplex (a(i, j)), b);
      }

  return result;
}

// -*- 6 -*-
octave_value
elem_xpow (const FloatMatrix& a, const FloatComplexMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (nr != b_nr || nc != b_nc)
    octave::err_nonconformant ("operator .^", nr, nc, b_nr, b_nc);

  FloatComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = std::pow (FloatComplex (a(i, j)), b(i, j));
      }

  return result;
}

// -*- 7 -*-
octave_value
elem_xpow (const FloatComplex& a, const FloatMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  FloatComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        float btmp = b(i, j);
        if (xisint (btmp))
          result(i, j) = std::pow (a, static_cast<int> (btmp));
        else
          result(i, j) = std::pow (a, btmp);
      }

  return result;
}

// -*- 8 -*-
octave_value
elem_xpow (const FloatComplex& a, const FloatComplexMatrix& b)
{
  octave_idx_type nr = b.rows ();
  octave_idx_type nc = b.cols ();

  FloatComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = std::pow (a, b(i, j));
      }

  return result;
}

// -*- 9 -*-
octave_value
elem_xpow (const FloatComplexMatrix& a, float b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  FloatComplexMatrix result (nr, nc);

  if (xisint (b))
    {
      int bint = static_cast<int> (b);
      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result(i, j) = std::pow (a(i, j), bint);
          }
    }
  else
    {
      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          {
            octave_quit ();
            result(i, j) = std::pow (a(i, j), b);
          }
    }

  return result;
}

// -*- 10 -*-
octave_value
elem_xpow (const FloatComplexMatrix& a, const FloatMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (nr != b_nr || nc != b_nc)
    octave::err_nonconformant ("operator .^", nr, nc, b_nr, b_nc);

  FloatComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        float btmp = b(i, j);
        if (xisint (btmp))
          result(i, j) = std::pow (a(i, j), static_cast<int> (btmp));
        else
          result(i, j) = std::pow (a(i, j), btmp);
      }

  return result;
}

// -*- 11 -*-
octave_value
elem_xpow (const FloatComplexMatrix& a, const FloatComplex& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  FloatComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = std::pow (a(i, j), b);
      }

  return result;
}

// -*- 12 -*-
octave_value
elem_xpow (const FloatComplexMatrix& a, const FloatComplexMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  if (nr != b_nr || nc != b_nc)
    octave::err_nonconformant ("operator .^", nr, nc, b_nr, b_nc);

  FloatComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = std::pow (a(i, j), b(i, j));
      }

  return result;
}

// Safer pow functions that work elementwise for N-D arrays.
//
//       op2 \ op1:   s   nd  cs   cnd
//            +--   +---+---+----+----+
//   scalar   |     | * | 3 |  * |  9 |
//                  +---+---+----+----+
//   N_d            | 1 | 4 |  7 | 10 |
//                  +---+---+----+----+
//   complex_scalar | * | 5 |  * | 11 |
//                  +---+---+----+----+
//   complex_N_d    | 2 | 6 |  8 | 12 |
//                  +---+---+----+----+
//
//   * -> not needed.

// FIXME: these functions need to be fixed so that things like
//
//   a = -1; b = [ 0, 0.5, 1 ]; r = a .^ b
//
// and
//
//   a = -1; b = [ 0, 0.5, 1 ]; for i = 1:3, r(i) = a .^ b(i), end
//
// produce identical results.  Also, it would be nice if -1^0.5
// produced a pure imaginary result instead of a complex number with a
// small real part.  But perhaps that's really a problem with the math
// library...

// -*- 1 -*-
octave_value
elem_xpow (float a, const FloatNDArray& b)
{
  octave_value retval;

  if (a < 0.0 && ! b.all_integers ())
    {
      FloatComplex acplx (a);
      FloatComplexNDArray result (b.dims ());
      for (octave_idx_type i = 0; i < b.numel (); i++)
        {
          octave_quit ();
          result(i) = std::pow (acplx, b(i));
        }

      retval = result;
    }
  else
    {
      FloatNDArray result (b.dims ());
      for (octave_idx_type i = 0; i < b.numel (); i++)
        {
          octave_quit ();
          result(i) = std::pow (a, b(i));
        }

      retval = result;
    }

  return retval;
}

// -*- 2 -*-
octave_value
elem_xpow (float a, const FloatComplexNDArray& b)
{
  FloatComplexNDArray result (b.dims ());

  for (octave_idx_type i = 0; i < b.numel (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a, b(i));
    }

  return result;
}

// -*- 3 -*-
octave_value
elem_xpow (const FloatNDArray& a, float b)
{
  octave_value retval;

  if (xisint (b))
    {
      FloatNDArray result (a.dims ());

      int bint = static_cast<int> (b);
      if (bint == 2)
        {
          for (octave_idx_type i = 0; i < a.numel (); i++)
            result.xelem (i) = a(i) * a(i);
        }
      else if (bint == 3)
        {
          for (octave_idx_type i = 0; i < a.numel (); i++)
            result.xelem (i) = a(i) * a(i) * a(i);
        }
      else if (bint == -1)
        {
          for (octave_idx_type i = 0; i < a.numel (); i++)
            result.xelem (i) = 1.0f / a(i);
        }
      else
        {
          for (octave_idx_type i = 0; i < a.numel (); i++)
            {
              octave_quit ();
              result.xelem (i) = std::pow (a(i), bint);
            }
        }

      retval = result;
    }
  else
    {
      if (a.any_element_is_negative ())
        {
          FloatComplexNDArray result (a.dims ());

          for (octave_idx_type i = 0; i < a.numel (); i++)
            {
              octave_quit ();

              FloatComplex acplx (a(i));

              result(i) = std::pow (acplx, b);
            }

          retval = result;
        }
      else
        {
          FloatNDArray result (a.dims ());
          for (octave_idx_type i = 0; i < a.numel (); i++)
            {
              octave_quit ();
              result(i) = std::pow (a(i), b);
            }

          retval = result;
        }
    }

  return retval;
}

// -*- 4 -*-
octave_value
elem_xpow (const FloatNDArray& a, const FloatNDArray& b)
{
  octave_value retval;

  dim_vector a_dims = a.dims ();
  dim_vector b_dims = b.dims ();

  if (a_dims != b_dims)
    {
      if (! is_valid_bsxfun ("operator .^", a_dims, b_dims))
        octave::err_nonconformant ("operator .^", a_dims, b_dims);

      // Potentially complex results
      FloatNDArray xa = octave_value_extract<FloatNDArray> (a);
      FloatNDArray xb = octave_value_extract<FloatNDArray> (b);
      if (! xb.all_integers () && xa.any_element_is_negative ())
        return octave_value (bsxfun_pow (FloatComplexNDArray (xa), xb));
      else
        return octave_value (bsxfun_pow (xa, xb));
    }

  int len = a.numel ();

  bool convert_to_complex = false;

  for (octave_idx_type i = 0; i < len; i++)
    {
      octave_quit ();
      float atmp = a(i);
      float btmp = b(i);
      if (atmp < 0.0 && ! xisint (btmp))
        {
          convert_to_complex = true;
          goto done;
        }
    }

done:

  if (convert_to_complex)
    {
      FloatComplexNDArray complex_result (a_dims);

      for (octave_idx_type i = 0; i < len; i++)
        {
          octave_quit ();
          FloatComplex acplx (a(i));
          complex_result(i) = std::pow (acplx, b(i));
        }

      retval = complex_result;
    }
  else
    {
      FloatNDArray result (a_dims);

      for (octave_idx_type i = 0; i < len; i++)
        {
          octave_quit ();
          result(i) = std::pow (a(i), b(i));
        }

      retval = result;
    }

  return retval;
}

// -*- 5 -*-
octave_value
elem_xpow (const FloatNDArray& a, const FloatComplex& b)
{
  FloatComplexNDArray result (a.dims ());

  for (octave_idx_type i = 0; i < a.numel (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a(i), b);
    }

  return result;
}

// -*- 6 -*-
octave_value
elem_xpow (const FloatNDArray& a, const FloatComplexNDArray& b)
{
  dim_vector a_dims = a.dims ();
  dim_vector b_dims = b.dims ();

  if (a_dims != b_dims)
    {
      if (! is_valid_bsxfun ("operator .^", a_dims, b_dims))
        octave::err_nonconformant ("operator .^", a_dims, b_dims);

      return bsxfun_pow (a, b);
    }

  FloatComplexNDArray result (a_dims);

  for (octave_idx_type i = 0; i < a.numel (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a(i), b(i));
    }

  return result;
}

// -*- 7 -*-
octave_value
elem_xpow (const FloatComplex& a, const FloatNDArray& b)
{
  FloatComplexNDArray result (b.dims ());

  for (octave_idx_type i = 0; i < b.numel (); i++)
    {
      octave_quit ();
      float btmp = b(i);
      if (xisint (btmp))
        result(i) = std::pow (a, static_cast<int> (btmp));
      else
        result(i) = std::pow (a, btmp);
    }

  return result;
}

// -*- 8 -*-
octave_value
elem_xpow (const FloatComplex& a, const FloatComplexNDArray& b)
{
  FloatComplexNDArray result (b.dims ());

  for (octave_idx_type i = 0; i < b.numel (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a, b(i));
    }

  return result;
}

// -*- 9 -*-
octave_value
elem_xpow (const FloatComplexNDArray& a, float b)
{
  FloatComplexNDArray result (a.dims ());

  if (xisint (b))
    {
      int bint = static_cast<int> (b);
      if (bint == -1)
        {
          for (octave_idx_type i = 0; i < a.numel (); i++)
            result.xelem (i) = 1.0f / a(i);
        }
      else
        {
          for (octave_idx_type i = 0; i < a.numel (); i++)
            {
              octave_quit ();
              result(i) = std::pow (a(i), bint);
            }
        }
    }
  else
    {
      for (octave_idx_type i = 0; i < a.numel (); i++)
        {
          octave_quit ();
          result(i) = std::pow (a(i), b);
        }
    }

  return result;
}

// -*- 10 -*-
octave_value
elem_xpow (const FloatComplexNDArray& a, const FloatNDArray& b)
{
  dim_vector a_dims = a.dims ();
  dim_vector b_dims = b.dims ();

  if (a_dims != b_dims)
    {
      if (! is_valid_bsxfun ("operator .^", a_dims, b_dims))
        octave::err_nonconformant ("operator .^", a_dims, b_dims);

      return bsxfun_pow (a, b);
    }

  FloatComplexNDArray result (a_dims);

  for (octave_idx_type i = 0; i < a.numel (); i++)
    {
      octave_quit ();
      float btmp = b(i);
      if (xisint (btmp))
        result(i) = std::pow (a(i), static_cast<int> (btmp));
      else
        result(i) = std::pow (a(i), btmp);
    }

  return result;
}

// -*- 11 -*-
octave_value
elem_xpow (const FloatComplexNDArray& a, const FloatComplex& b)
{
  FloatComplexNDArray result (a.dims ());

  for (octave_idx_type i = 0; i < a.numel (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a(i), b);
    }

  return result;
}

// -*- 12 -*-
octave_value
elem_xpow (const FloatComplexNDArray& a, const FloatComplexNDArray& b)
{
  dim_vector a_dims = a.dims ();
  dim_vector b_dims = b.dims ();

  if (a_dims != b_dims)
    {
      if (! is_valid_bsxfun ("operator .^", a_dims, b_dims))
        octave::err_nonconformant ("operator .^", a_dims, b_dims);

      return bsxfun_pow (a, b);
    }

  FloatComplexNDArray result (a_dims);

  for (octave_idx_type i = 0; i < a.numel (); i++)
    {
      octave_quit ();
      result(i) = std::pow (a(i), b(i));
    }

  return result;
}

OCTAVE_END_NAMESPACE(octave)
