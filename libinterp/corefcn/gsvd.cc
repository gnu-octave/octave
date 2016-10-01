// Copyright (C) 2016 Barbara Lócsi
// Copyright (C) 2006, 2010 Pascal Dupuis <Pascal.Dupuis@uclouvain.be>
// Copyright (C) 1996, 1997 John W. Eaton
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 3 of the License, or (at your option) any later
// version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, see <http://www.gnu.org/licenses/>.

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
function_gsvd (const T& A, const T& B, const octave_idx_type nargout)
{
  octave::math::gsvd<T> result (A, B, gsvd_type<T> (nargout));

  octave_value_list retval (nargout);
  if (nargout < 2)
    {
      DiagMatrix sigA = result.singular_values_A ();
      DiagMatrix sigB = result.singular_values_B ();
      for (int i = sigA.rows () - 1; i >= 0; i--)
        sigA.dgxelem(i) /= sigB.dgxelem(i);
      retval(0) = sigA.diag ();
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
@deftypefn  {} {@var{s} =} gsvd (@var{a}, @var{b})
@deftypefnx {} {[@var{u}, @var{v}, @var{x}, @var{c}, @var{s}, @var{r}] =} gsvd (@var{a}, @var{b})
@cindex generalized singular value decomposition
Compute the generalized singular value decomposition of (@var{a}, @var{b}):
@tex
$$
 U^H A X = [I 0; 0 C] [0 R]
 V^H B X = [0 S; 0 0] [0 R]
 C*C + S*S = eye (columns (A))
 I and 0 are padding matrices of suitable size
 R is upper triangular
$$
@end tex
@ifinfo

@example
@group
u' * a * x = [I 0; 0 c] * [0 r]
v' * b * x = [0 s; 0 0] * [0 r]
c * c + s * s = eye (columns (a))
I and 0 are padding matrices of suitable size
r is upper triangular
@end group
@end example

@end ifinfo

The function @code{gsvd} normally returns the vector of generalized singular
values
@tex
diag (C) ./ diag (S).
@end tex
@ifinfo
diag (r) ./ diag (s).
@end ifinfo
If asked for five return values, it computes
@tex
$U$, $V$, and $X$.
@end tex
@ifinfo
U, V, and X.
@end ifinfo
With a sixth output argument, it also returns
@tex
R,
@end tex
@ifinfo
r,
@end ifinfo
The common upper triangular right term.  Other authors, like
@nospell{S. Van Huffel}, define this transformation as the simultaneous
diagonalization of the input matrices, this can be achieved by multiplying
@tex
X
@end tex
@ifinfo
x
@end ifinfo
by the inverse of
@tex
[I 0; 0 R].
@end tex
@ifinfo
[I 0; 0 r].
@end ifinfo

For example,

@example
gsvd (hilb (3), [1 2 3; 3 2 1])

@result{}
  0.1055705
  0.0031759
@end example

@noindent
and

@example
[u, v, c, s, x, r] = gsvd (hilb (3), [1 2 3; 3 2 1])
@result{}

u =

  -0.965609   0.240893   0.097825
  -0.241402  -0.690927  -0.681429
  -0.096561  -0.681609   0.725317

v =

  -0.41974   0.90765
  -0.90765  -0.41974

x =

   0.408248   0.902199   0.139179
  -0.816497   0.429063  -0.386314
   0.408248  -0.044073  -0.911806

c =

   0.10499   0.00000
   0.00000   0.00318

s =
   0.99447   0.00000
   0.00000   0.99999

r =
  -0.14093  -1.24345   0.43737
   0.00000  -3.90043   2.57818
   0.00000   0.00000  -2.52599

@end example

The code is a wrapper to the corresponding @sc{lapack} dggsvd and zggsvd
routines.

@end deftypefn */)
{
  if (args.length () !=  2)
    print_usage ();

  octave_value_list retval;

  octave_value argA = args(0);
  octave_value argB = args(1);

  octave_idx_type nr = argA.rows ();
  octave_idx_type nc = argA.columns ();

  octave_idx_type np = argB.columns ();

  // This "special" case should be handled in the gsvd class, not here
  if (nr == 0 || nc == 0)
    {
      retval = octave_value_list (nargout);
      if (nargout < 2) // S = gsvd (A, B)
        retval(0) = Matrix (0, 1);
      else // [U, V, X, C, S, R] = gsvd (A, B)
        {
          retval(0) = identity_matrix (nc, nc);
          retval(1) = identity_matrix (nc, nc);
          if (nargout > 2)
            retval(2) = identity_matrix (nr, nr);
          if (nargout > 3)
            retval(3) = Matrix (nr, nc);
          if (nargout > 4)
            retval(4) = identity_matrix (nr, nr);
          if (nargout > 5)
            retval(5) = identity_matrix (nr, nr);
        }
    }
  else
    {
      if (nc != np)
        print_usage ();

      if (argA.is_real_type () && argB.is_real_type ())
        {
          Matrix tmpA = argA.matrix_value ();
          Matrix tmpB = argB.matrix_value ();

          // FIXME: This code is still using error_state
          if (! error_state)
            {
              if (tmpA.any_element_is_inf_or_nan ())
                error ("gsvd: B cannot have Inf or NaN values");
              if (tmpB.any_element_is_inf_or_nan ())
                error ("gsvd: B cannot have Inf or NaN values");

              retval = function_gsvd (tmpA, tmpB, nargout);
            }
        }
      else if (argA.is_complex_type () || argB.is_complex_type ())
        {
          ComplexMatrix ctmpA = argA.complex_matrix_value ();
          ComplexMatrix ctmpB = argB.complex_matrix_value ();

          if (! error_state)
            {
              if (ctmpA.any_element_is_inf_or_nan ())
                error ("gsvd: A cannot have Inf or NaN values");
              if (ctmpB.any_element_is_inf_or_nan ())
                error ("gsvd: B cannot have Inf or NaN values");

              retval = function_gsvd (ctmpA, ctmpB, nargout);
            }
        }
      else
        {
          // Actually, can't tell which arg is at fault
          err_wrong_type_arg ("gsvd", argA);
          //err_wrong_type_arg ("gsvd", argB);
        }
    }

  return retval;
}

/*
## a few tests for gsvd.m
%!shared A, A0, B, B0, U, V, C, S, X, R, D1, D2
%! A0 = randn (5, 3);
%! B0 = diag ([1 2 4]);
%! A = A0;
%! B = B0;

## A (5x3) and B (3x3) are full rank
%!test
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros (5, 3);  D1(1:3, 1:3) = C;
%! D2 = S;
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (3, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*R) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*R) <= 1e-6);

## A: 5x3 full rank, B: 3x3 rank deficient
%!test
%! B(2, 2) = 0;
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros (5, 3);  D1(1, 1) = 1;  D1(2:3, 2:3) = C;
%! D2 = [zeros(2, 1) S; zeros(1, 3)];
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (2, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*R) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*R) <= 1e-6);

## A: 5x3 rank deficient, B: 3x3 full rank
%!test
%! B = B0;
%! A(:, 3) = 2*A(:, 1) - A(:, 2);
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros(5, 3);  D1(1:3, 1:3) = C;
%! D2 = S;
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (3, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*R) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*R) <= 1e-6);

## A and B are both rank deficient
%!test
%! B(:, 3) = 2*B(:, 1) - B(:, 2);
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros(5, 2);  D1(1:2, 1:2) = C;
%! D2 = [S; zeros(1, 2)];
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (2, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*[zeros(2, 1) R]) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*[zeros(2, 1) R]) <= 1e-6);

## A (now 3x5) and B (now 5x5) are full rank
%!test
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
%!test
%! B(2, 2) = 0;
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros(3, 5); D1(1, 1) = 1; D1(2:3, 2:3) = C;
%! D2 = zeros(5, 5); D2(1:2, 2:3) = S; D2(3:4, 4:5) = eye (2);
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (2, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*R) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*R) <= 1e-6);

## A: 3x5 rank deficient, B: 5x5 full rank
%!test
%! B = B0;
%! A(3, :) = 2*A(1, :) - A(2, :);
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros (3, 5);  D1(1:3, 1:3) = C;
%! D2 = zeros (5, 5);  D2(1:3, 1:3) = S;  D2(4:5, 4:5) = eye (2);
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (3, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*R) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*R) <= 1e-6);

## A and B are both rank deficient
%!test
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
%!test
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
%!test
%! B(2, 2) = 0;
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros(5, 3);  D1(1, 1) = 1;  D1(2:3, 2:3) = C;
%! D2 = [zeros(2, 1) S; zeros(1, 3)];
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (2, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*R) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*R) <= 1e-6);

## A: 5x3 complex rank deficient, B: 3x3 complex full rank
%!test
%! B = B0;
%! A(:, 3) = 2*A(:, 1) - A(:, 2);
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros(5, 3);  D1(1:3, 1:3) = C;
%! D2 = S;
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (3, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*R) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*R) <= 1e-6);

## A (5x3) and B (3x3) are both complex rank deficient
%!test
%! B(:, 3) = 2*B(:, 1) - B(:, 2);
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros(5, 2);  D1(1:2, 1:2) = C;
%! D2 = [S; zeros(1, 2)];
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (2, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*[zeros(2, 1) R]) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*[zeros(2, 1) R]) <= 1e-6);

## A (now 3x5) complex and B (now 5x5) complex are full rank
## now, A is 3x5
%!test
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
%!test
%! B(2, 2) = 0;
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros(3, 5);  D1(1, 1) = 1;  D1(2:3, 2:3) = C;
%! D2 = zeros(5,5);  D2(1:2, 2:3) = S;  D2(3:4, 4:5) = eye (2);
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (2, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*R) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*R) <= 1e-6);

## A: 3x5 complex rank deficient, B: 5x5 complex full rank
%!test
%! B = B0;
%! A(3, :) = 2*A(1, :) - A(2, :);
%! [U, V, X, C, S, R] = gsvd (A, B);
%! D1 = zeros(3, 5);  D1(1:3, 1:3) = C;
%! D2 = zeros(5,5);  D2(1:3, 1:3) = S;  D2(4:5, 4:5) = eye (2);
%! assert (norm (diag (C).^2 + diag (S).^2 - ones (3, 1)) <= 1e-6);
%! assert (norm ((U'*A*X) - D1*R) <= 1e-6);
%! assert (norm ((V'*B*X) - D2*R) <= 1e-6);

## A and B are both complex rank deficient
%!test
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
*/

