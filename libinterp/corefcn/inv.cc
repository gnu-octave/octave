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

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"
#include "ops.h"
#include "ov-re-diag.h"
#include "ov-cx-diag.h"
#include "ov-flt-re-diag.h"
#include "ov-flt-cx-diag.h"
#include "ov-perm.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (inv, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{x} =} inv (@var{A})
@deftypefnx {} {[@var{x}, @var{rcond}] =} inv (@var{A})
@deftypefnx {} {[@dots{}] =} inverse (@dots{})
Compute the inverse of the square matrix @var{A}.

Return an estimate of the reciprocal condition number if requested,
otherwise warn of an ill-conditioned matrix if the reciprocal condition
number is small.

In general it is best to avoid calculating the inverse of a matrix directly.
For example, it is both faster and more accurate to solve systems of
equations (@var{A}*@math{x} = @math{b}) with
@code{@var{y} = @var{A} \ @math{b}}, rather than
@code{@var{y} = inv (@var{A}) * @math{b}}.

If called with a sparse matrix, then in general @var{x} will be a full
matrix requiring significantly more storage.  Avoid forming the inverse of a
sparse matrix if possible.

@code{inverse} is an alias and may be used identically in place of @code{inv}.
@seealso{ldivide, rdivide, pinv}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  octave_value arg = args(0);

  if (! arg.isnumeric ())
    err_wrong_type_arg ("inv", arg);

  if (arg.isempty ())
    return ovl (Matrix ());

  if (arg.rows () != arg.columns ())
    err_square_matrix_required ("inv", "A");

  octave_value result;
  octave_idx_type info = 0;
  double rcond = 0.0;
  float frcond = 0.0;
  bool isfloat = arg.is_single_type ();

  if (arg.is_diag_matrix ())
    {
      rcond = 1.0;
      frcond = 1.0f;
      if (arg.iscomplex ())
        {
          if (isfloat)
            {
              result = arg.float_complex_diag_matrix_value ().inverse (info);
              if (info == -1)
                frcond = 0.0f;
              else if (nargout > 1)
                frcond = arg.float_complex_diag_matrix_value ().rcond ();
            }
          else
            {
              result = arg.complex_diag_matrix_value ().inverse (info);
              if (info == -1)
                rcond = 0.0;
              else if (nargout > 1)
                rcond = arg.complex_diag_matrix_value ().rcond ();
            }
        }
      else
        {
          if (isfloat)
            {
              result = arg.float_diag_matrix_value ().inverse (info);
              if (info == -1)
                frcond = 0.0f;
              else if (nargout > 1)
                frcond = arg.float_diag_matrix_value ().rcond ();
            }
          else
            {
              result = arg.diag_matrix_value ().inverse (info);
              if (info == -1)
                rcond = 0.0;
              else if (nargout > 1)
                rcond = arg.diag_matrix_value ().rcond ();
            }
        }
    }
  else if (arg.is_perm_matrix ())
    {
      info = 0;
      rcond = 1.0;
      result = arg.perm_matrix_value ().inverse ();
    }
  else if (isfloat)
    {
      if (arg.isreal ())
        {
          FloatMatrix m = arg.float_matrix_value ();

          MatrixType mattyp = args(0).matrix_type ();
          result = m.inverse (mattyp, info, frcond, true, true);
          args(0).matrix_type (mattyp);
        }
      else if (arg.iscomplex ())
        {
          FloatComplexMatrix m = arg.float_complex_matrix_value ();

          MatrixType mattyp = args(0).matrix_type ();
          result = m.inverse (mattyp, info, frcond, true, true);
          args(0).matrix_type (mattyp);
        }
    }
  else
    {
      if (arg.isreal ())
        {
          if (arg.issparse ())
            {
              SparseMatrix m = arg.sparse_matrix_value ();

              MatrixType mattyp = args(0).matrix_type ();
              result = m.inverse (mattyp, info, rcond, true, true);
              args(0).matrix_type (mattyp);
            }
          else
            {
              Matrix m = arg.matrix_value ();

              MatrixType mattyp = args(0).matrix_type ();
              result = m.inverse (mattyp, info, rcond, true, true);
              args(0).matrix_type (mattyp);
            }
        }
      else if (arg.iscomplex ())
        {
          if (arg.issparse ())
            {
              SparseComplexMatrix m = arg.sparse_complex_matrix_value ();

              MatrixType mattyp = args(0).matrix_type ();
              result = m.inverse (mattyp, info, rcond, true, true);
              args(0).matrix_type (mattyp);
            }
          else
            {
              ComplexMatrix m = arg.complex_matrix_value ();

              MatrixType mattyp = args(0).matrix_type ();
              result = m.inverse (mattyp, info, rcond, true, true);
              args(0).matrix_type (mattyp);
            }
        }
      else
        // Shouldn't get here since we checked for suitable arg earlier.
        // Maybe for some user-defined classes?
        err_wrong_type_arg ("inv", arg);
    }

  octave_value_list retval (nargout > 1 ? 2 : 1);

  retval(0) = result;
  if (nargout > 1)
    retval(1) = (isfloat ? octave_value (frcond) : octave_value (rcond));

  if (nargout < 2)
    {
      bool is_singular;

      if (isfloat)
        is_singular = ((frcond + 1.0f == 1.0f) || octave::math::isnan (frcond))
                      && ! arg.is_scalar_type ();
      else
        is_singular = ((rcond + 1.0 == 1.0) || octave::math::isnan (rcond))
                      && ! arg.is_scalar_type ();

      if (info == -1 || is_singular)
        warn_singular_matrix (isfloat ? frcond : rcond);
    }

  return retval;
}

/*
## Basic test for double/single matrices
%!assert (inv ([1, 2; 3, 4]), [-2, 1; 1.5, -0.5], 5*eps)
%!test
%! [xinv, rcond] = inv ([1,2;3,4]);
%! assert (xinv, [-2, 1; 1.5, -0.5], 5*eps);
%! assert (isa (rcond, "double"));

%!assert (inv (single ([1, 2; 3, 4])), single ([-2, 1; 1.5, -0.5]),
%!        5* eps ("single"))
%!test
%! [xinv, rcond] = inv (single ([1,2;3,4]));
%! assert (xinv, single ([-2, 1; 1.5, -0.5]), 5* eps ("single"));
%! assert (isa (rcond, "single"));

## Basic test for integer inputs
%!assert (inv (int32 (2)), 0.5)
%!assert (inv (uint32 (2)), 0.5)
%!assert (inv (int64 (2)), 0.5)
%!assert (inv (uint64 (2)), 0.5)

## Normal scalar cases
%!assert (inv (2), 0.5)
%!test
%! [xinv, rcond] = inv (2);
%! assert (xinv, 0.5);
%! assert (rcond, 1);
%!assert (inv (single (2)), single (0.5))
%!test
%! [xinv, rcond] = inv (single (2));
%! assert (xinv, single (0.5));
%! assert (rcond, single (1));
%!assert (inv (complex (1, -1)), 0.5+0.5i)
%!test
%! [xinv, rcond] = inv (complex (1, -1));
%! assert (xinv, 0.5+0.5i);
%! assert (rcond, 1);
%!assert (inv (complex (single (1), -1)), single (0.5+0.5i))
%!test
%! [xinv, rcond] = inv (complex (single (1), -1));
%! assert (xinv, single (0.5+0.5i));
%! assert (rcond, single (1));

## Test special inputs
## Empty matrix
%!assert (inv (zeros (2,0)), [])

## Scalar "0"
%!assert (inv (0), Inf)
%!test
%! [xinv, rcond] = inv (0);
%! assert (xinv, Inf);
%! assert (rcond, 0);
%!assert (inv (single (0)), single (Inf))
%!test
%! [xinv, rcond] = inv (single (0));
%! assert (xinv, single (Inf));
%! assert (rcond, single (0));
%!assert (inv (complex (0, 0)), Inf)
%!test
%! [xinv, rcond] = inv (complex (0, 0));
%! assert (xinv, Inf);
%! assert (rcond, 0);
%!assert (inv (complex (single (0), 0)), single (Inf))
%!test
%! [xinv, rcond] = inv (complex (single (0), 0));
%! assert (xinv, single (Inf));
%! assert (rcond, single (0));
## NOTE: Matlab returns +Inf for -0 input, but it returns -Inf for 1/-0.
## These should be the same, and in Octave they are.
%!assert (inv (-0), -Inf)
%!test
%! [xinv, rcond] = inv (-0);
%! assert (xinv, -Inf);
%! assert (rcond, 0);

## Scalar "Inf"
%!assert (inv (Inf), 0)
%!test
%! [xinv, rcond] = inv (Inf);
%! assert (xinv, 0);
%! assert (rcond, 0);
%!assert (inv (single (Inf)), single (0))
%!test
%! [xinv, rcond] = inv (single (Inf));
%! assert (xinv, single (0));
%! assert (rcond, single (0));
%!assert (inv (complex (1, Inf)), 0)
%!test
%! [xinv, rcond] = inv (complex (1, Inf));
%! assert (xinv, 0);
%! assert (rcond, 0);
%!assert (inv (complex (single (1), Inf)), single (0))
%!test
%! [xinv, rcond] = inv (complex (single (1), Inf));
%! assert (xinv, single (0));
%! assert (rcond, single (0));

## Scalar "NaN"
%!assert (inv (NaN), NaN)
%!test
%! [xinv, rcond] = inv (NaN);
%! assert (xinv, NaN);
%! assert (rcond, NaN);
%!assert (inv (single (NaN)), single (NaN))
%!test
%! [xinv, rcond] = inv (single (NaN));
%! assert (xinv, single (NaN));
%! assert (rcond, single (NaN));
%!assert (inv (complex (1, NaN)), complex (NaN, NaN))
%!test
%! [xinv, rcond] = inv (complex (1, NaN));
%! assert (xinv, complex (NaN, NaN));
%! assert (rcond, NaN);
%!assert (inv (complex (single (1), NaN)), complex (single (NaN), NaN))
%!test
%! [xinv, rcond] = inv (complex (single (1), NaN));
%! assert (xinv, complex (single (NaN), NaN));
%! assert (rcond, single (NaN));

## Matrix special values
## Matrix of all zeroes
%!warning <matrix singular> assert (inv (zeros (2,2)), Inf (2,2))
%!test
%! [xinv, rcond] = inv (zeros (2,2));
%! assert (xinv, Inf (2,2));
%! assert (rcond, 0);
## Matrix of all Inf
%!warning <rcond = > assert (inv (Inf (2,2)), NaN (2,2))
%!test
%! [xinv, rcond] = inv (Inf (2,2));
%! assert (xinv, NaN (2,2));
%! assert (rcond, NaN);
## Matrix of all NaN
%!warning <rcond = > assert (inv (NaN (2,2)), NaN (2,2))
%!test
%! [xinv, rcond] = inv (NaN (2,2));
%! assert (xinv, NaN (2,2));
%! assert (rcond, NaN);

## Special diagonal matrices
%!test
%! fail ("A = inv (diag ([1, 0, 1]))", "warning", "matrix singular");
%! assert (A, diag ([Inf, Inf, Inf]));

## Special sparse matrices
%!testif HAVE_UMFPACK <*56232>
%! fail ("A = inv (sparse ([1, 2;0 ,0]))", "warning", "matrix singular");
%! assert (A, sparse ([Inf, Inf; 0, 0]));

%!testif HAVE_UMFPACK <*56232>
%! fail ("A = inv (sparse ([1i, 2;0 ,0]))", "warning", "matrix singular");
%! assert (A, sparse ([Inf, Inf; 0, 0]));

%!testif HAVE_UMFPACK <*56232>
%! fail ("A = inv (sparse ([1, 0, 0; 0, 0, 0; 0, 0, 1]))",
%!       "warning", "matrix singular");
%! assert (A, sparse ([Inf, 0, 0; 0, 0, 0; 0, 0, Inf]));

%!error <Invalid call> inv ()
%!error <Invalid call> inv ([1, 2; 3, 4], 2)
%!error <wrong type argument> inv ("Hello World")
%!error <wrong type argument> inv ({1})
%!error <wrong type argument> inv (true)
%!error <must be a square matrix> inv ([1, 2; 3, 4; 5, 6])
%!error <inverse of the null matrix not defined> inv (sparse (2, 2, 0))
%!error <inverse of the null matrix not defined> inv (diag ([0, 0]))
%!error <inverse of the null matrix not defined> inv (diag (complex ([0, 0])))
*/

DEFALIAS (inverse, inv);

OCTAVE_END_NAMESPACE(octave)
