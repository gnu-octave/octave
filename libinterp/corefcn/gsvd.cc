////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1997-2023 The Octave Project Developers
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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include "dMatrix.h"
#include "CMatrix.h"
#include "dDiagMatrix.h"
#include "gsvd.h"

#include "defun.h"
#include "defun-int.h"
#include "error.h"
#include "errwarn.h"
#include "utils.h"
#include "ovl.h"
#include "ov.h"

OCTAVE_BEGIN_NAMESPACE(octave)

template <typename T>
static typename math::gsvd<T>::Type
gsvd_type (int nargout, int nargin)
{
  if (nargout == 0 || nargout == 1)
    return octave::math::gsvd<T>::Type::sigma_only;
  else if (nargin < 3)
    return octave::math::gsvd<T>::Type::std;
  else
    return octave::math::gsvd<T>::Type::economy;
}

// Named do_gsvd to avoid conflicts with the gsvd class itself.
template <typename T>
static octave_value_list
do_gsvd (const T& A, const T& B,
         const octave_idx_type nargout, const octave_idx_type nargin,
         bool is_single = false)
{
  math::gsvd<T> result (A, B, gsvd_type<T> (nargout, nargin));

  octave_value_list retval (nargout);
  if (nargout <= 1)
    {
      if (is_single)
        {
          FloatMatrix sigA = result.singular_values_A ();
          FloatMatrix sigB = result.singular_values_B ();
          for (int i = sigA.rows () - 1; i >= 0; i--)
            sigA.xelem (i) /= sigB.xelem (i);
          retval(0) = sigA.sort ();
        }
      else
        {
          Matrix sigA = result.singular_values_A ();
          Matrix sigB = result.singular_values_B ();
          for (int i = sigA.rows () - 1; i >= 0; i--)
            sigA.xelem (i) /= sigB.xelem (i);
          retval(0) = sigA.sort ();
        }
    }
  else
    {
      switch (nargout)
        {
        case 5:
          retval(4) = result.singular_values_B ();
          OCTAVE_FALLTHROUGH;

        case 4:
          retval(3) = result.singular_values_A ();
          OCTAVE_FALLTHROUGH;

        case 3:
          retval(2) = result.right_singular_matrix ();
        }

      retval(1) = result.left_singular_matrix_B ();
      retval(0) = result.left_singular_matrix_A ();
    }

  return retval;
}

DEFUN (gsvd, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{S} =} gsvd (@var{A}, @var{B})
@deftypefnx {} {[@var{U}, @var{V}, @var{X}, @var{C}, @var{S}] =} gsvd (@var{A}, @var{B})
@deftypefnx {} {[@var{U}, @var{V}, @var{X}, @var{C}, @var{S}] =} gsvd (@var{A}, @var{B}, 0)
Compute the generalized singular value decomposition of (@var{A}, @var{B}).

The generalized singular value decomposition is defined by the following
relations:

@tex
$$ A = U C X^\dagger $$
$$ B = V S X^\dagger $$
$$ C^\dagger C + S^\dagger S = eye (columns (A)) $$
@end tex
@ifnottex

@example
@group
A = U*C*X'
B = V*S*X'
C'*C + S'*S = eye (columns (A))
@end group
@end example

@end ifnottex

The function @code{gsvd} normally returns just the vector of generalized
singular values
@tex
$$ \sqrt{{{diag (C^\dagger C)} \over {diag (S^\dagger S)}}} $$
@end tex
@ifnottex
@code{sqrt (diag (C'*C) ./ diag (S'*S))}.
@end ifnottex
If asked for five return values, it also computes
@tex
$U$, $V$, $X$, and $C$.
@end tex
@ifnottex
U, V, X, and C.
@end ifnottex

If the optional third input is present, @code{gsvd} constructs the
"economy-sized" decomposition where the number of columns of @var{U}, @var{V}
and the number of rows of @var{C}, @var{S} is less than or equal to the number
of columns of @var{A}.  This option is not yet implemented.

Programming Note: the code is a wrapper to the corresponding @sc{lapack} dggsvd
and zggsvd routines.  If matrices @var{A} and @var{B} are @emph{both} rank
deficient then @sc{lapack} will return an incorrect factorization.  Programmers
should avoid this combination.
@seealso{svd}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    print_usage ();
  else if (nargin == 3)
    {
      // FIXME: when "economy" is implemented delete this code
      warning ("gsvd: economy-sized decomposition is not yet implemented, returning full decomposition");
      nargin = 2;
    }

  octave_value_list retval;

  octave_value argA = args(0);
  octave_value argB = args(1);

  if (argA.columns () != argB.columns ())
    error ("gsvd: A and B must have the same number of columns");

  if (argA.is_single_type () || argB.is_single_type ())
    {
      if (argA.isreal () && argB.isreal ())
        {
          FloatMatrix tmpA = argA.xfloat_matrix_value ("gsvd: A must be a real or complex matrix");
          FloatMatrix tmpB = argB.xfloat_matrix_value ("gsvd: B must be a real or complex matrix");

          if (tmpA.any_element_is_inf_or_nan ())
            error ("gsvd: A cannot have Inf or NaN values");
          if (tmpB.any_element_is_inf_or_nan ())
            error ("gsvd: B cannot have Inf or NaN values");

          retval = do_gsvd (tmpA, tmpB, nargout, nargin, true);
        }
      else if (argA.iscomplex () || argB.iscomplex ())
        {
          FloatComplexMatrix ctmpA =
            argA.xfloat_complex_matrix_value ("gsvd: A must be a real or complex matrix");
          FloatComplexMatrix ctmpB =
            argB.xfloat_complex_matrix_value ("gsvd: B must be a real or complex matrix");

          if (ctmpA.any_element_is_inf_or_nan ())
            error ("gsvd: A cannot have Inf or NaN values");
          if (ctmpB.any_element_is_inf_or_nan ())
            error ("gsvd: B cannot have Inf or NaN values");

          retval = do_gsvd (ctmpA, ctmpB, nargout, nargin, true);
        }
      else
        error ("gsvd: A and B must be real or complex matrices");
    }
  else
    {
      if (argA.isreal () && argB.isreal ())
        {
          Matrix tmpA = argA.xmatrix_value ("gsvd: A must be a real or complex matrix");
          Matrix tmpB = argB.xmatrix_value ("gsvd: B must be a real or complex matrix");

          if (tmpA.any_element_is_inf_or_nan ())
            error ("gsvd: A cannot have Inf or NaN values");
          if (tmpB.any_element_is_inf_or_nan ())
            error ("gsvd: B cannot have Inf or NaN values");

          retval = do_gsvd (tmpA, tmpB, nargout, nargin);
        }
      else if (argA.iscomplex () || argB.iscomplex ())
        {
          ComplexMatrix ctmpA = argA.xcomplex_matrix_value ("gsvd: A must be a real or complex matrix");
          ComplexMatrix ctmpB = argB.xcomplex_matrix_value ("gsvd: B must be a real or complex matrix");

          if (ctmpA.any_element_is_inf_or_nan ())
            error ("gsvd: A cannot have Inf or NaN values");
          if (ctmpB.any_element_is_inf_or_nan ())
            error ("gsvd: B cannot have Inf or NaN values");

          retval = do_gsvd (ctmpA, ctmpB, nargout, nargin);
        }
      else
        error ("gsvd: A and B must be real or complex matrices");
    }

  return retval;
}

/*

## Basic tests of decomposition
%!test <60273>
%! A = reshape (1:15,5,3);
%! B = magic (3);
%! [U,V,X,C,S] = gsvd (A,B);
%! assert (size (U), [5, 5]);
%! assert (size (V), [3, 3]);
%! assert (size (X), [3, 3]);
%! assert (size (C), [5, 3]);
%! assert (C(4:5, :), zeros (2,3));
%! assert (size (S), [3, 3]);
%! assert (U*C*X', A, 50*eps);
%! assert (V*S*X', B, 50*eps);
%! S0 = gsvd (A, B);
%! assert (size (S0), [3, 1]);
%! S1 = sort (svd (A / B));
%! assert (S0, S1, 10*eps);

%!test <60273>
%! A = reshape (1:15,3,5);
%! B = magic (5);
%! [U,V,X,C,S] = gsvd (A,B);
%! assert (size (U), [3, 3]);
%! assert (size (V), [5, 5]);
%! assert (size (X), [5, 5]);
%! assert (size (C), [3, 5]);
%! assert (C(:, 4:5), zeros (3,2));
%! assert (size (S), [5, 5]);
%! assert (U*C*X', A, 120*eps);  # less accurate in this orientation
%! assert (V*S*X', B, 150*eps);  # for some reason.
%! S0 = gsvd (A, B);
%! assert (size (S0), [5, 1]);
%! S0 = S0(3:end);
%! S1 = sort (svd (A / B));
%! assert (S0, S1, 20*eps);

## a few tests for gsvd.m
%!shared A, A0, B, B0, U, V, C, S, X, old_state, restore_state
%! old_state = randn ("state");
%! restore_state = onCleanup (@() randn ("state", old_state));
%! randn ("state", 40); # initialize generator to make behavior reproducible
%! A0 = randn (5, 3);
%! B0 = diag ([1 2 4]);
%! A = A0;
%! B = B0;

## A (5x3) and B (3x3) are full rank
%!test <48807>
%! [U, V, X, C, S] = gsvd (A, B);
%! assert (C'*C + S'*S, eye (3), 5*eps);
%! assert (U*C*X', A, 10*eps);
%! assert (V*S*X', B, 20*eps);

## A: 5x3 full rank, B: 3x3 rank deficient
%!test <48807>
%! B(2, 2) = 0;
%! [U, V, X, C, S] = gsvd (A, B);
%! assert (C'*C + S'*S, eye (3), 5*eps);
%! assert (U*C*X', A, 10*eps);
%! assert (V*S*X', B, 20*eps);

## A: 5x3 rank deficient, B: 3x3 full rank
%!test <48807>
%! B = B0;
%! A(:, 3) = 2*A(:, 1) - A(:, 2);
%! [U, V, X, C, S] = gsvd (A, B);
%! assert (C'*C + S'*S, eye (3), 5*eps);
%! assert (U*C*X', A, 10*eps);
%! assert (V*S*X', B, 20*eps);

## A and B are both rank deficient
## FIXME: LAPACK seems to be completely broken for this case
%!#test <48807>
%! B(:, 3) = 2*B(:, 1) - B(:, 2);
%! [U, V, X, C, S] = gsvd (A, B);
%! assert (C'*C + S'*S, eye (3), 5*eps);
%! assert (U*C*X', A, 10*eps);
%! assert (V*S*X', B, 20*eps);

## A (now 3x5) and B (now 5x5) are full rank
%!test <48807>
%! A = A0.';
%! B0 = diag ([1 2 4 8 16]);
%! B = B0;
%! [U, V, X, C, S] = gsvd (A, B);
%! assert (C'*C + S'*S, eye (5), 5*eps);
%! assert (U*C*X', A, 15*eps);
%! assert (V*S*X', B, 85*eps);

## A: 3x5 full rank, B: 5x5 rank deficient
%!test <48807>
%! B(2, 2) = 0;
%! [U, V, X, C, S] = gsvd (A, B);
%! assert (C'*C + S'*S, eye (5), 5*eps);
%! assert (U*C*X', A, 15*eps);
%! assert (V*S*X', B, 85*eps);

## A: 3x5 rank deficient, B: 5x5 full rank
%!test <48807>
%! B = B0;
%! A(3, :) = 2*A(1, :) - A(2, :);
%! [U, V, X, C, S] = gsvd (A, B);
%! assert (C'*C + S'*S, eye (5), 5*eps);
%! assert (U*C*X', A, 15*eps);
%! assert (V*S*X', B, 85*eps);

## A and B are both rank deficient
## FIXME: LAPACK seems to be completely broken for this case
%!#test <48807>
%! A = A0.'; B = B0.';
%! A(:, 3) = 2*A(:, 1) - A(:, 2);
%! B(:, 3) = 2*B(:, 1) - B(:, 2);
%! [U, V, X, C, S] = gsvd (A, B);
%! assert (C'*C + S'*S, eye (3), 5*eps);
%! assert (U*C*X', A, 10*eps);
%! assert (V*S*X', B, 20*eps);

## A: 5x3 complex full rank, B: 3x3 complex full rank
%!test <48807>
%! A0 = A0 + j* randn (5, 3);
%! B0 = diag ([1 2 4]) + j* diag ([4 -2 -1]);
%! A = A0;
%! B = B0;
%! [U, V, X, C, S] = gsvd (A, B);
%! assert (C'*C + S'*S, eye (3), 5*eps);
%! assert (U*C*X', A, 10*eps);
%! assert (V*S*X', B, 25*eps);

## A: 5x3 complex full rank, B: 3x3 complex rank deficient
%!test <48807>
%! B(2, 2) = 0;
%! [U, V, X, C, S] = gsvd (A, B);
%! assert (C'*C + S'*S, eye (3), 5*eps);
%! assert (U*C*X', A, 10*eps);
%! assert (V*S*X', B, 25*eps);

## A: 5x3 complex rank deficient, B: 3x3 complex full rank
%!test <48807>
%! B = B0;
%! A(:, 3) = 2*A(:, 1) - A(:, 2);
%! [U, V, X, C, S] = gsvd (A, B);
%! assert (C'*C + S'*S, eye (3), 5*eps);
%! assert (U*C*X', A, 15*eps);
%! assert (V*S*X', B, 25*eps);

## A (5x3) and B (3x3) are both complex rank deficient
## FIXME: LAPACK seems to be completely broken for this case
%!#test <48807>
%! B(:, 3) = 2*B(:, 1) - B(:, 2);
%! [U, V, X, C, S] = gsvd (A, B);
%! assert (C'*C + S'*S, eye (3), 5*eps);
%! assert (U*C*X', A, 10*eps);
%! assert (V*S*X', B, 20*eps);

## A (now 3x5) complex and B (now 5x5) complex are full rank
## now, A is 3x5
%!test <48807>
%! A = A0.';
%! B0 = diag ([1 2 4 8 16]) + j* diag ([-5 4 -3 2 -1]);
%! B = B0;
%! [U, V, X, C, S] = gsvd (A, B);
%! assert (C'*C + S'*S, eye (5), 5*eps);
%! assert (U*C*X', A, 25*eps);
%! assert (V*S*X', B, 85*eps);

## A: 3x5 complex full rank, B: 5x5 complex rank deficient
%!test <48807>
%! B(2, 2) = 0;
%! [U, V, X, C, S] = gsvd (A, B);
%! assert (C'*C + S'*S, eye (5), 5*eps);
%! assert (U*C*X', A, 10*eps);
%! assert (V*S*X', B, 85*eps);

## A: 3x5 complex rank deficient, B: 5x5 complex full rank
%!test <48807>
%! B = B0;
%! A(3, :) = 2*A(1, :) - A(2, :);
%! [U, V, X, C, S] = gsvd (A, B);
%! assert (C'*C + S'*S, eye (5), 5*eps);
%! assert (U*C*X', A, 10*eps);
%! assert (V*S*X', B, 85*eps);

## A and B are both complex rank deficient
## FIXME: LAPACK seems to be completely broken for this case
%!#test <48807>
%! A = A0.';
%! B = B0.';
%! A(:, 3) = 2*A(:, 1) - A(:, 2);
%! B(:, 3) = 2*B(:, 1) - B(:, 2);
%! [U, V, X, C, S] = gsvd (A, B);
%! assert (C'*C + S'*S, eye (5), 5*eps);
%! assert (U*C*X', A, 10*eps);
%! assert (V*S*X', B, 85*eps);

## Test that single inputs produce single outputs
%!test
%! s = gsvd (single (eye (5)), B);
%! assert (class (s), "single");
%! [U,V,X,C,S] = gsvd (single (eye(5)), B);
%! assert (class (U), "single");
%! assert (class (V), "single");
%! assert (class (X), "single");
%! assert (class (C), "single");
%! assert (class (S), "single");
%!
%! s = gsvd (A, single (eye (5)));
%! assert (class (s), "single");
%! [U,V,X,C,S] = gsvd (A, single (eye (5)));
%! assert (class (U), "single");
%! assert (class (V), "single");
%! assert (class (X), "single");
%! assert (class (C), "single");
%! assert (class (S), "single");

## Test input validation
%!error <Invalid call> gsvd ()
%!error <Invalid call> gsvd (1)
%!error <Invalid call> gsvd (1,2,3,4)
%!warning <economy-sized decomposition is not yet implemented> gsvd (1,2,0);
%!error <A and B must have the same number of columns> gsvd (1,[1, 2])
## Test input validation for single (real and complex) inputs.
%!error <A cannot have Inf or NaN values> gsvd (Inf, single (2))
%!error <A cannot have Inf or NaN values> gsvd (NaN, single (2))
%!error <B cannot have Inf or NaN values> gsvd (single (1), Inf)
%!error <B cannot have Inf or NaN values> gsvd (single (1), NaN)
%!error <A must be a real or complex matrix> gsvd ({1}, single (2i))
%!error <B must be a real or complex matrix> gsvd (single (i), {2})
%!error <A cannot have Inf or NaN values> gsvd (Inf, single (2i))
%!error <A cannot have Inf or NaN values> gsvd (NaN, single (2i))
%!error <B cannot have Inf or NaN values> gsvd (single (i), Inf)
%!error <B cannot have Inf or NaN values> gsvd (single (i), NaN)
## Test input validation for single, but not real or complex, inputs.
%!error <A and B must be real or complex matrices> gsvd ({1}, single (2))
%!error <A and B must be real or complex matrices> gsvd (single (1), {2})
## Test input validation for double (real and complex) inputs.
%!error <A cannot have Inf or NaN values> gsvd (Inf, 2)
%!error <A cannot have Inf or NaN values> gsvd (NaN, 2)
%!error <B cannot have Inf or NaN values> gsvd (1, Inf)
%!error <B cannot have Inf or NaN values> gsvd (1, NaN)
%!error <A must be a real or complex matrix> gsvd ({1}, 2i)
%!error <B must be a real or complex matrix> gsvd (i, {2})
%!error <A cannot have Inf or NaN values> gsvd (Inf, 2i)
%!error <A cannot have Inf or NaN values> gsvd (NaN, 2i)
%!error <B cannot have Inf or NaN values> gsvd (i, Inf)
%!error <B cannot have Inf or NaN values> gsvd (i, NaN)
## Test input validation for double, but not real or complex, inputs.
%!error <A and B must be real or complex matrices> gsvd ({1}, double (2))
%!error <A and B must be real or complex matrices> gsvd (double (1), {2})
## Test input validation in liboctave/numeric/gsvd.cc
%!error <A and B cannot be empty matrices> gsvd (zeros (0,1), 1)
%!error <A and B cannot be empty matrices> gsvd (1, zeros (0,1))

*/

OCTAVE_END_NAMESPACE(octave)
