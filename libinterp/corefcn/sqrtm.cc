////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2001-2023 The Octave Project Developers
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

#include "schur.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "oct-norm.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "utils.h"
#include "xnorm.h"

OCTAVE_BEGIN_NAMESPACE(octave)

template <typename Matrix>
static void
sqrtm_utri_inplace (Matrix& T)
{
  typedef typename Matrix::element_type element_type;

  const element_type zero = element_type ();

  bool singular = false;

  // The following code is equivalent to this triple loop:
  //
  //   n = rows (T);
  //   for j = 1:n
  //     T(j,j) = sqrt (T(j,j));
  //     for i = j-1:-1:1
  //       if T(i,j) != 0
  //         T(i,j) /= (T(i,i) + T(j,j));
  //       endif
  //       k = 1:i-1;
  //       T(k,j) -= T(k,i) * T(i,j);
  //     endfor
  //   endfor
  //
  // this is an in-place, cache-aligned variant of the code
  // given in Higham's paper.

  const octave_idx_type n = T.rows ();
  element_type *Tp = T.fortran_vec ();
  for (octave_idx_type j = 0; j < n; j++)
    {
      element_type *colj = Tp + n*j;
      if (colj[j] != zero)
        colj[j] = sqrt (colj[j]);
      else
        singular = true;

      for (octave_idx_type i = j-1; i >= 0; i--)
        {
          const element_type *coli = Tp + n*i;
          if (colj[i] != zero)
            colj[i] /= (coli[i] + colj[j]);
          const element_type colji = colj[i];
          for (octave_idx_type k = 0; k < i; k++)
            colj[k] -= coli[k] * colji;
        }
    }

  if (singular)
    warning_with_id ("Octave:sqrtm:SingularMatrix",
                     "sqrtm: matrix is singular, may not have a square root");
}

template <typename Matrix, typename ComplexMatrix, typename ComplexSCHUR>
static octave_value
do_sqrtm (const octave_value& arg)
{

  octave_value retval;

  MatrixType mt = arg.matrix_type ();

  bool iscomplex = arg.iscomplex ();

  typedef typename Matrix::element_type real_type;

  real_type cutoff = 0;
  real_type one = 1;
  real_type eps = std::numeric_limits<real_type>::epsilon ();

  if (! iscomplex)
    {
      Matrix x = octave_value_extract<Matrix> (arg);

      if (mt.is_unknown ()) // if type is not known, compute it now.
        arg.matrix_type (mt = MatrixType (x));

      switch (mt.type ())
        {
        case MatrixType::Upper:
        case MatrixType::Diagonal:
          if (! x.diag ().any_element_is_negative ())
            {
              // Do it in real arithmetic.
              sqrtm_utri_inplace (x);
              retval = x;
              retval.matrix_type (mt);
            }
          else
            iscomplex = true;
          break;

        case MatrixType::Lower:
          if (! x.diag ().any_element_is_negative ())
            {
              x = x.transpose ();
              sqrtm_utri_inplace (x);
              retval = x.transpose ();
              retval.matrix_type (mt);
            }
          else
            iscomplex = true;
          break;

        default:
          iscomplex = true;
          break;
        }

      if (iscomplex)
        cutoff = 10 * x.rows () * eps * xnorm (x, one);
    }

  if (iscomplex)
    {
      ComplexMatrix x = octave_value_extract<ComplexMatrix> (arg);

      if (mt.is_unknown ()) // if type is not known, compute it now.
        arg.matrix_type (mt = MatrixType (x));

      switch (mt.type ())
        {
        case MatrixType::Upper:
        case MatrixType::Diagonal:
          sqrtm_utri_inplace (x);
          retval = x;
          retval.matrix_type (mt);
          break;

        case MatrixType::Lower:
          x = x.transpose ();
          sqrtm_utri_inplace (x);
          retval = x.transpose ();
          retval.matrix_type (mt);
          break;

        default:
          {
            ComplexMatrix u;

            do
              {
                ComplexSCHUR schur_fact (x, "", true);
                x = schur_fact.schur_matrix ();
                u = schur_fact.unitary_schur_matrix ();
              }
            while (0); // schur no longer needed.

            sqrtm_utri_inplace (x);

            x = u * x; // original x no longer needed.
            ComplexMatrix res = xgemm (x, u, blas_no_trans, blas_conj_trans);

            if (cutoff > 0 && xnorm (imag (res), one) <= cutoff)
              retval = real (res);
            else
              retval = res;
          }
          break;
        }
    }

  return retval;
}

DEFUN (sqrtm, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{s} =} sqrtm (@var{A})
@deftypefnx {} {[@var{s}, @var{error_estimate}] =} sqrtm (@var{A})
Compute the matrix square root of the square matrix @var{A}.

Ref: @nospell{N.J. Higham}.  @cite{A New sqrtm for @sc{matlab}}.  Numerical
Analysis Report No.@: 336, Manchester @nospell{Centre} for Computational
Mathematics, Manchester, England, January 1999.
@seealso{expm, logm}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  octave_value arg = args(0);

  octave_idx_type n = arg.rows ();
  octave_idx_type nc = arg.columns ();

  if (n != nc || arg.ndims () > 2)
    err_square_matrix_required ("sqrtm", "A");

  octave_value_list retval (nargout > 1 ? 3 : 1);

  if (nargout > 1)
    {
      // FIXME: Octave does not calculate a condition number with respect to
      //        sqrtm.  Should this return NaN instead of -1?
      retval(2) = -1.0;
    }

  if (arg.is_diag_matrix ())
    // sqrtm of a diagonal matrix is just sqrt.
    retval(0) = arg.sqrt ();
  else if (arg.is_single_type ())
    retval(0) = do_sqrtm<FloatMatrix, FloatComplexMatrix,
    math::schur<FloatComplexMatrix>> (arg);
  else if (arg.isnumeric ())
    retval(0) = do_sqrtm<Matrix, ComplexMatrix,
    math::schur<ComplexMatrix>> (arg);

  if (nargout > 1)
    {
      // This corresponds to generic code
      //
      //   norm (s*s - x, "fro") / norm (x, "fro");

      octave_value s = retval(0);
      retval(1) = xfrobnorm (s*s - arg) / xfrobnorm (arg);
    }

  return retval;
}

/*
%!assert (sqrtm (2* ones (2)), ones (2), 3*eps)
%!assert <*60797> (sqrtm (ones (4))^2, ones (4), 5*eps)

## The following two tests are from the reference in the docstring above.
%!test
%! warning ("off", "Octave:sqrtm:SingularMatrix", "local");
%! x = [0 1; 0 0];
%! assert (any (isnan (sqrtm (x))(:)));

%!test
%! x = eye (4);  x(2,2) = x(3,3) = 2^-26;  x(1,4) = 1;
%! z = eye (4);  z(2,2) = z(3,3) = 2^-13;  z(1,4) = 0.5;
%! [y, err] = sqrtm (x);
%! assert (y, z);
%! assert (err, 0);   # Yes, this one has to hold exactly
*/

OCTAVE_END_NAMESPACE(octave)
