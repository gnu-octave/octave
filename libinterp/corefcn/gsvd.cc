/*

Copyright (C) 2016-2018 Barbara Lócsi
Copyright (C) 2006, 2010 Pascal Dupuis <Pascal.Dupuis@uclouvain.be>
Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

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


template <typename T>
static typename octave::math::gsvd<T>::Type
gsvd_type (int nargout)
{
  return ((nargout == 0 || nargout == 1)
          ? octave::math::gsvd<T>::Type::sigma_only
          : (nargout > 5) ? octave::math::gsvd<T>::Type::std
                          : octave::math::gsvd<T>::Type::economy);
}

// Named like this to avoid conflicts with the gsvd class.
template <typename T>
static octave_value_list
do_gsvd (const T& A, const T& B, const octave_idx_type nargout,
         bool is_single = false)
{
  octave::math::gsvd<T> result (A, B, gsvd_type<T> (nargout));

  octave_value_list retval (nargout);
  if (nargout < 2)
    {
      if (is_single)
        {
          FloatDiagMatrix sigA = result.singular_values_A ();
          FloatDiagMatrix sigB = result.singular_values_B ();
          for (int i = sigA.rows () - 1; i >= 0; i--)
            sigA.dgxelem(i) /= sigB.dgxelem(i);
          retval(0) = sigA.diag ();
        }
      else
        {
          DiagMatrix sigA = result.singular_values_A ();
          DiagMatrix sigB = result.singular_values_B ();
          for (int i = sigA.rows () - 1; i >= 0; i--)
            sigA.dgxelem(i) /= sigB.dgxelem(i);
          retval(0) = sigA.diag ();
        }
    }
  else
    {
      retval(0) = result.left_singular_matrix_A ();
      retval(1) = result.left_singular_matrix_B ();
      if (nargout > 2)
        retval(2) = result.right_singular_matrix ();
      if (nargout > 3)
        retval(3) = result.singular_values_A ();
      if (nargout > 4)
        retval(4) = result.singular_values_B ();
      if (nargout > 5)
        retval(5) = result.R_matrix ();
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
and zggsvd routines.

@seealso{svd}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    print_usage ();
  else if (nargin == 3)
    warning ("gsvd: economy-sized decomposition is not yet implemented, returning full decomposition");

  octave_value_list retval;

  octave_value argA = args(0);
  octave_value argB = args(1);

  octave_idx_type nr = argA.rows ();
  octave_idx_type nc = argA.columns ();

  octave_idx_type np = argB.columns ();

  // FIXME: This "special" case should be handled in the gsvd class, not here
  if (nr == 0 || nc == 0)
    {
      retval = octave_value_list (nargout);
      if (nargout < 2)  // S = gsvd (A, B)
        {
          if (argA.is_single_type () || argB.is_single_type ())
            retval(0) = FloatMatrix (0, 1);
          else
            retval(0) = Matrix (0, 1);
        }
      else  // [U, V, X, C, S, R] = gsvd (A, B)
        {
          if (argA.is_single_type () || argB.is_single_type ())
            {
              retval(0) = octave::float_identity_matrix (nc, nc);
              retval(1) = octave::float_identity_matrix (nc, nc);
              if (nargout > 2)
                retval(2) = octave::float_identity_matrix (nr, nr);
              if (nargout > 3)
                retval(3) = FloatMatrix (nr, nc);
              if (nargout > 4)
                retval(4) = octave::float_identity_matrix (nr, nr);
              if (nargout > 5)
                retval(5) = octave::float_identity_matrix (nr, nr);
            }
          else
            {
              retval(0) = octave::identity_matrix (nc, nc);
              retval(1) = octave::identity_matrix (nc, nc);
              if (nargout > 2)
                retval(2) = octave::identity_matrix (nr, nr);
              if (nargout > 3)
                retval(3) = Matrix (nr, nc);
              if (nargout > 4)
                retval(4) = octave::identity_matrix (nr, nr);
              if (nargout > 5)
                retval(5) = octave::identity_matrix (nr, nr);
            }
        }
    }
  else
    {
      if (nc != np)
        print_usage ();

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

              retval = do_gsvd (tmpA, tmpB, nargout, true);
            }
          else if (argA.iscomplex () || argB.iscomplex ())
            {
              FloatComplexMatrix ctmpA = argA.xfloat_complex_matrix_value ("gsvd: A must be a real or complex matrix");
              FloatComplexMatrix ctmpB = argB.xfloat_complex_matrix_value ("gsvd: B must be a real or complex matrix");

              if (ctmpA.any_element_is_inf_or_nan ())
                error ("gsvd: A cannot have Inf or NaN values");
              if (ctmpB.any_element_is_inf_or_nan ())
                error ("gsvd: B cannot have Inf or NaN values");

              retval = do_gsvd (ctmpA, ctmpB, nargout, true);
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

              retval = do_gsvd (tmpA, tmpB, nargout);
            }
          else if (argA.iscomplex () || argB.iscomplex ())
            {
              ComplexMatrix ctmpA = argA.xcomplex_matrix_value ("gsvd: A must be a real or complex matrix");
              ComplexMatrix ctmpB = argB.xcomplex_matrix_value ("gsvd: B must be a real or complex matrix");

              if (ctmpA.any_element_is_inf_or_nan ())
                error ("gsvd: A cannot have Inf or NaN values");
              if (ctmpB.any_element_is_inf_or_nan ())
                error ("gsvd: B cannot have Inf or NaN values");

              retval = do_gsvd (ctmpA, ctmpB, nargout);
            }
          else
            error ("gsvd: A and B must be real or complex matrices");
        }
    }

  return retval;
}

/*

## Basic test of decomposition
%!test <48807>
%! A = reshape (1:15,5,3);
%! B = magic (3);
%! [U,V,X,C,S] = gsvd (A,B);
%! assert (U*C*X', A, 50*eps);
%! assert (V*S*X', B, 50*eps);
%! S0 = gsvd (A, B);
%! S1 = svd (A / B);
%! assert (S0, S1, 10*eps);

## a few tests for gsvd.m
%!shared A, A0, B, B0, U, V, C, S, X, R, D1, D2
%! A0 = randn (5, 3);
%! B0 = diag ([1 2 4]);
%! A = A0;
%! B = B0;

## A (5x3) and B (3x3) are full rank
%!test <48807>
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros (5, 3);  D1(1:3, 1:3) = C;
%! D2 = S;
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (3, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*R) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*R) <= 1e-6);

## A: 5x3 full rank, B: 3x3 rank deficient
%!test <48807>
%! B(2, 2) = 0;
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros (5, 3);  D1(1, 1) = 1;  D1(2:3, 2:3) = C;
%! D2 = [zeros(2, 1) S; zeros(1, 3)];
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (2, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*R) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*R) <= 1e-6);

## A: 5x3 rank deficient, B: 3x3 full rank
%!test <48807>
%! B = B0;
%! A(:, 3) = 2*A(:, 1) - A(:, 2);
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros(5, 3);  D1(1:3, 1:3) = C;
%! D2 = S;
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (3, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*R) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*R) <= 1e-6);

## A and B are both rank deficient
%!test <48807>
%! B(:, 3) = 2*B(:, 1) - B(:, 2);
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros(5, 2);  D1(1:2, 1:2) = C;
%! D2 = [S; zeros(1, 2)];
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (2, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*[zeros(2, 1) R]) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*[zeros(2, 1) R]) <= 1e-6);

## A (now 3x5) and B (now 5x5) are full rank
%!test <48807>
%! A = A0.';
%! B0 = diag ([1 2 4 8 16]);
%! B = B0;
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = [C zeros(3,2)];
%! D2 = [S zeros(3,2); zeros(2, 3) eye(2)];
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (3, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*R) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*R) <= 1e-6);

## A: 3x5 full rank, B: 5x5 rank deficient
%!test <48807>
%! B(2, 2) = 0;
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros(3, 5); D1(1, 1) = 1; D1(2:3, 2:3) = C;
%! D2 = zeros(5, 5); D2(1:2, 2:3) = S; D2(3:4, 4:5) = eye (2);
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (2, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*R) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*R) <= 1e-6);

## A: 3x5 rank deficient, B: 5x5 full rank
%!test <48807>
%! B = B0;
%! A(3, :) = 2*A(1, :) - A(2, :);
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros (3, 5);  D1(1:3, 1:3) = C;
%! D2 = zeros (5, 5);  D2(1:3, 1:3) = S;  D2(4:5, 4:5) = eye (2);
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (3, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*R) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*R) <= 1e-6);

## A and B are both rank deficient
%!test <48807>
%! A = A0.'; B = B0.';
%! A(:, 3) = 2*A(:, 1) - A(:, 2);
%! B(:, 3) = 2*B(:, 1) - B(:, 2);
%! [U, V, X, C, S, R]=gsvd (A, B);
%! D1 = zeros(3, 4); D1(1:3, 1:3) = C;
%! D2 = eye (4); D2(1:3, 1:3) = S; D2(5,:) = 0;
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (3, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*[zeros(4, 1) R]) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*[zeros(4, 1) R]) <= 1e-6);

## A: 5x3 complex full rank, B: 3x3 complex full rank
%!test <48807>
%! A0 = A0 + j*randn (5, 3);
%! B0 = diag ([1 2 4]) + j*diag ([4 -2 -1]);
%! A = A0;
%! B = B0;
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros(5, 3);  D1(1:3, 1:3) = C;
%! D2 = S;
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (3, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*R) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*R) <= 1e-6);

## A: 5x3 complex full rank, B: 3x3 complex rank deficient
%!test <48807>
%! B(2, 2) = 0;
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros(5, 3);  D1(1, 1) = 1;  D1(2:3, 2:3) = C;
%! D2 = [zeros(2, 1) S; zeros(1, 3)];
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (2, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*R) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*R) <= 1e-6);

## A: 5x3 complex rank deficient, B: 3x3 complex full rank
%!test <48807>
%! B = B0;
%! A(:, 3) = 2*A(:, 1) - A(:, 2);
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros(5, 3);  D1(1:3, 1:3) = C;
%! D2 = S;
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (3, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*R) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*R) <= 1e-6);

## A (5x3) and B (3x3) are both complex rank deficient
%!test <48807>
%! B(:, 3) = 2*B(:, 1) - B(:, 2);
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros(5, 2);  D1(1:2, 1:2) = C;
%! D2 = [S; zeros(1, 2)];
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (2, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*[zeros(2, 1) R]) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*[zeros(2, 1) R]) <= 1e-6);

## A (now 3x5) complex and B (now 5x5) complex are full rank
## now, A is 3x5
%!test <48807>
%! A = A0.';
%! B0 = diag ([1 2 4 8 16]) + j*diag ([-5 4 -3 2 -1]);
%! B = B0;
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = [C zeros(3,2)];
%! D2 = [S zeros(3,2); zeros(2, 3) eye(2)];
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (3, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*R) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*R) <= 1e-6);

## A: 3x5 complex full rank, B: 5x5 complex rank deficient
%!test <48807>
%! B(2, 2) = 0;
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros(3, 5);  D1(1, 1) = 1;  D1(2:3, 2:3) = C;
%! D2 = zeros(5,5);  D2(1:2, 2:3) = S;  D2(3:4, 4:5) = eye (2);
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (2, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*R) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*R) <= 1e-6);

## A: 3x5 complex rank deficient, B: 5x5 complex full rank
%!test <48807>
%! B = B0;
%! A(3, :) = 2*A(1, :) - A(2, :);
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros(3, 5);  D1(1:3, 1:3) = C;
%! D2 = zeros(5,5);  D2(1:3, 1:3) = S;  D2(4:5, 4:5) = eye (2);
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (3, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*R) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*R) <= 1e-6);

## A and B are both complex rank deficient
%!test <48807>
%! A = A0.';
%! B = B0.';
%! A(:, 3) = 2*A(:, 1) - A(:, 2);
%! B(:, 3) = 2*B(:, 1) - B(:, 2);
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros(3, 4);  D1(1:3, 1:3) = C;
%! D2 = eye (4);  D2(1:3, 1:3) = S;  D2(5,:) = 0;
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (3, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*[zeros(4, 1) R]) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*[zeros(4, 1) R]) <= 1e-6);

## Test that single inputs produce single outputs
%!test
%! s = gsvd (single (ones (0,1)), B);
%! assert (class (s), "single");
%! s = gsvd (single (ones (1,0)), B);
%! assert (class (s), "single");
%! s = gsvd (single (ones (1,0)), B);
%! [U,V,X,C,S,R] = gsvd (single ([]), B);
%! assert (class (U), "single");
%! assert (class (V), "single");
%! assert (class (X), "single");
%! assert (class (C), "single");
%! assert (class (S), "single");
%! assert (class (R), "single");
%!
%! s = gsvd (single (A), B);
%! assert (class (s), "single");
%! [U,V,X,C,S,R] = gsvd (single (A), B);
%! assert (class (U), "single");
%! assert (class (V), "single");
%! assert (class (X), "single");
%! assert (class (C), "single");
%! assert (class (S), "single");
%! assert (class (R), "single");

*/
