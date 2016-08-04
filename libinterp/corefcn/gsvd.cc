// Copyright (C) 1996, 1997 John W. Eaton
// Copyright (C) 2006, 2010 Pascal Dupuis <Pascal.Dupuis@uclouvain.be>
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

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "pr-output.h"
#include "utils.h"
#include "ovl.h"

#include "CmplxGSVD.h"
#include "dbleGSVD.h"

DEFUN (gsvd, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn {Loadable Function} {@var{s} =} gsvd (@var{a}, @var{b})
@deftypefnx {Loadable Function} {[@var{u}, @var{v}, @var{c}, @var{s}, @var{x} [, @var{r}]] =} gsvd (@var{a}, @var{b})
@cindex generalised singular value decomposition
Compute the generalised singular value decomposition of (@var{a}, @var{b}):
@iftex
@tex
$$
 U^H A X = [I 0; 0 C] [0 R]
 V^H B X = [0 S; 0 0] [0 R]
 C*C + S*S = eye(columns(A))
 I and 0 are padding matrices of suitable size
 R is upper triangular
$$
@end tex
@end iftex
@ifinfo

@example
u' * a * x = [I 0; 0 c] * [0 r]
v' * b * x = [0 s; 0 0] * [0 r]
c * c + s * s = eye(columns(a))
I and 0 are padding matrices of suitable size
r is upper triangular
@end example
@end ifinfo

The function @code{gsvd} normally returns the vector of generalised singular
values
@iftex
@tex
diag(C)./diag(S).
@end tex
@end iftex
@ifinfo
diag(r)./diag(s).
@end ifinfo
If asked for five return values, it computes
@iftex
@tex
$U$, $V$, and $X$.
@end tex
@end iftex
@ifinfo
U, V, and X.
@end ifinfo
With a sixth output argument, it also returns
@iftex
@tex
R,
@end tex
@end iftex
@ifinfo
r,
@end ifinfo
The common upper triangular right term. Other authors, like S. Van Huffel,
define this transformation as the simulatenous diagonalisation of the
input matrices, this can be achieved by multiplying 
@iftex
@tex
X
@end tex
@end iftex
@ifinfo
x
@end ifinfo
by the inverse of
@iftex
@tex
[I 0; 0 R].
@end tex
@end iftex
@ifinfo
[I 0; 0 r].
@end ifinfo

For example,

@example
gsvd (hilb (3), [1 2 3; 3 2 1])
@end example

@noindent
returns

@example
ans =

  0.1055705
  0.0031759
@end example

@noindent
and

@example
[u, v, c, s, x, r] = gsvd (hilb (3),  [1 2 3; 3 2 1])
@end example

@noindent
returns

@example
u =

  -0.965609   0.240893   0.097825
  -0.241402  -0.690927  -0.681429
  -0.096561  -0.681609   0.725317

v =

  -0.41974   0.90765
  -0.90765  -0.41974

c =

   0.10499   0.00000
   0.00000   0.00318

s =
   0.99447   0.00000
   0.00000   0.99999
x =

   0.408248   0.902199   0.139179
  -0.816497   0.429063  -0.386314
   0.408248  -0.044073  -0.911806

r =
  -0.14093  -1.24345   0.43737
   0.00000  -3.90043   2.57818
   0.00000   0.00000  -2.52599

@end example

The code is a wrapper to the corresponding Lapack dggsvd and zggsvd routines.

@end deftypefn */)
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 2 || nargin > 2 || (nargout > 1 && (nargout < 5 || nargout > 6)))
    {
      print_usage ();
      return retval;
    }

  octave_value argA = args(0), argB = args(1);

  octave_idx_type nr = argA.rows ();
  octave_idx_type nc = argA.columns ();

//  octave_idx_type  nn = argB.rows ();
  octave_idx_type  np = argB.columns ();
  
  if (nr == 0 || nc == 0)
    {
      if (nargout == 5)
          retval = ovl (identity_matrix (nc, nc), identity_matrix (nc, nc),
                        Matrix (nr, nc), identity_matrix (nr, nr),
                        identity_matrix (nr, nr));
      else if (nargout == 6)
          retval = ovl (identity_matrix (nc, nc), identity_matrix (nc, nc),
                        Matrix (nr, nc), identity_matrix (nr, nr),
                        identity_matrix (nr, nr),
                        identity_matrix (nr, nr));
      else
        retval = ovl (Matrix (0, 1));
    }
  else
    {
      if ((nc != np))
        {
          print_usage ();
          return retval;
        }

      GSVD::type type = ((nargout == 0 || nargout == 1)
                        ? GSVD::sigma_only
                        : (nargout > 5) ? GSVD::std : GSVD::economy );

      if (argA.is_real_type () && argB.is_real_type ())
        {
          Matrix tmpA = argA.matrix_value ();
          Matrix tmpB = argB.matrix_value ();

          if (! error_state)
            {
              if (tmpA.any_element_is_inf_or_nan ())
                {
                  error ("gsvd: cannot take GSVD of matrix containing Inf or NaN values");
                  return retval;
                }
              
              if (tmpB.any_element_is_inf_or_nan ())
                {
                  error ("gsvd: cannot take GSVD of matrix containing Inf or NaN values");
                  return retval;
                }

              GSVD result (tmpA, tmpB, type);

              // DiagMatrix sigma = result.singular_values ();

              if (nargout == 0 || nargout == 1)
                {
                  DiagMatrix sigA =  result.singular_values_A ();
                  DiagMatrix sigB =  result.singular_values_B ();
                  for (int i = sigA.rows() - 1; i >=0; i--)
                    sigA.dgxelem(i) /= sigB.dgxelem(i);
                  retval = ovl (sigA.diag());
                }
              else
                { 
                  if (nargout > 5)
                    retval = ovl (result.left_singular_matrix_A (),
                                  result.left_singular_matrix_B (),
                                  result.singular_values_A (),
                                  result.singular_values_B (),
                                  result.right_singular_matrix (),
                                  result.R_matrix ());
                  else
                    retval = ovl (result.left_singular_matrix_A (),
                                  result.left_singular_matrix_B (),
                                  result.singular_values_A (),
                                  result.singular_values_B (),
                                  result.right_singular_matrix ());
                }
            }
        }
      else if (argA.is_complex_type () || argB.is_complex_type ())
        {
          ComplexMatrix ctmpA = argA.complex_matrix_value ();
          ComplexMatrix ctmpB = argB.complex_matrix_value ();

          if (! error_state)
            {
              if (ctmpA.any_element_is_inf_or_nan ())
                {
                  error ("gsvd: cannot take GSVD of matrix containing Inf or NaN values");
                  return retval;
                }
              if (ctmpB.any_element_is_inf_or_nan ())
                {
                  error ("gsvd: cannot take GSVD of matrix containing Inf or NaN values");
                  return retval;
                }

              ComplexGSVD result (ctmpA, ctmpB, type);

              // DiagMatrix sigma = result.singular_values ();

              if (nargout == 0 || nargout == 1)
                {
                  DiagMatrix sigA =  result.singular_values_A ();
                  DiagMatrix sigB =  result.singular_values_B ();
                  for (int i = sigA.rows() - 1; i >=0; i--)
                    sigA.dgxelem(i) /= sigB.dgxelem(i);
                  retval = ovl (sigA.diag());
                }
              else
                {
                  if (nargout > 5)
                    retval = ovl (result.left_singular_matrix_A (),
                                  result.left_singular_matrix_B (),
                                  result.singular_values_A (),
                                  result.singular_values_B (),
                                  result.right_singular_matrix (),
                                  result.R_matrix ());
                  else
                    retval = ovl (result.left_singular_matrix_A (),
                                  result.left_singular_matrix_B (),
                                  result.singular_values_A (),
                                  result.singular_values_B (),
                                  result.right_singular_matrix ());
                }
            }
        }
      else
        {
          gripe_wrong_type_arg ("gsvd", argA);
          gripe_wrong_type_arg ("gsvd", argB);
          return retval;
        }
    }

  return retval;
}

/*
%# a few tests for gsvd.m
%!shared A, A0, B, B0, U, V, C, S, X, R, D1, D2

%! A0=randn(5, 3); B0=diag([1 2 4]);
%! A = A0; B = B0;
%! # disp('Full rank, 5x3 by 3x3 matrices');
%! # disp([rank(A) rank(B) rank([A' B'])]);
%! [U, V, C, S, X, R] = gsvd(A, B);
%! D1 = zeros(5, 3); D1(1:3, 1:3) = C;
%! D2 = S;
%!assert(norm(diag(C).^2+diag(S).^2 - ones(3, 1)) <= 1e-6)
%!assert(norm((U'*A*X)-D1*R) <= 1e-6)
%!assert(norm((V'*B*X)-D2*R) <= 1e-6)

%! # disp('A 5x3 full rank, B 3x3 rank deficient');
%! B(2, 2) = 0;
%! [U, V, C, S, X, R] = gsvd(A, B);
%! D1 = zeros(5, 3); D1(1, 1) = 1; D1(2:3, 2:3) = C;
%! D2 = [zeros(2, 1) S; zeros(1, 3)];
%!assert(norm(diag(C).^2+diag(S).^2 - ones(2, 1)) <= 1e-6)
%!assert(norm((U'*A*X)-D1*R) <= 1e-6)
%!assert(norm((V'*B*X)-D2*R) <= 1e-6)

%! # disp('A 5x3 rank deficient, B 3x3 full rank');
%! B = B0;
%! A(:, 3) = 2*A(:, 1) - A(:, 2);
%! [U, V, C, S, X, R] = gsvd(A, B);
%! D1 = zeros(5, 3); D1(1:3, 1:3) = C;
%! D2 = S;
%!assert(norm(diag(C).^2+diag(S).^2 - ones(3, 1)) <= 1e-6)
%!assert(norm((U'*A*X)-D1*R) <= 1e-6)
%!assert(norm((V'*B*X)-D2*R) <= 1e-6)

%! # disp("A 5x3, B 3x3, [A' B'] rank deficient");
%! B(:, 3) = 2*B(:, 1) - B(:, 2);
%! [U, V, C, S, X, R] = gsvd(A, B);
%! D1 = zeros(5, 2); D1(1:2, 1:2) = C;
%! D2 = [S; zeros(1, 2)];
%!assert(norm(diag(C).^2+diag(S).^2 - ones(2, 1)) <= 1e-6)
%!assert(norm((U'*A*X)-D1*[zeros(2, 1) R]) <= 1e-6)
%!assert(norm((V'*B*X)-D2*[zeros(2, 1) R]) <= 1e-6)

%! # now, A is 3x5
%! A = A0.'; B0=diag([1 2 4 8 16]); B = B0;
%! # disp('Full rank, 3x5 by 5x5 matrices');
%! # disp([rank(A) rank(B) rank([A' B'])]);

%! [U, V, C, S, X, R] = gsvd(A, B);
%! D1 = [C zeros(3,2)];
%! D2 = [S zeros(3,2); zeros(2, 3) eye(2)]; 
%!assert(norm(diag(C).^2+diag(S).^2 - ones(3, 1)) <= 1e-6)
%!assert(norm((U'*A*X)-D1*R) <= 1e-6)
%!assert(norm((V'*B*X)-D2*R) <= 1e-6)

%! # disp('A 5x3 full rank, B 5x5 rank deficient');
%! B(2, 2) = 0;
%! # disp([rank(A) rank(B) rank([A' B'])]);

%! [U, V, C, S, X, R] = gsvd(A, B);
%! D1 = zeros(3, 5); D1(1, 1) = 1; D1(2:3, 2:3) = C;
%! D2 = zeros(5, 5); D2(1:2, 2:3) = S; D2(3:4, 4:5) = eye(2);
%!assert(norm(diag(C).^2+diag(S).^2 - ones(2, 1)) <= 1e-6)
%!assert(norm((U'*A*X)-D1*R) <= 1e-6)
%!assert(norm((V'*B*X)-D2*R) <= 1e-6)

%! # disp('A 3x5 rank deficient, B 5x5 full rank');
%! B = B0;
%! A(3, :) = 2*A(1, :) - A(2, :);
%! # disp([rank(A) rank(B) rank([A' B'])]);
%! [U, V, C, S, X, R] = gsvd(A, B);
%! D1 = zeros(3, 5); D1(1:3, 1:3) = C;
%! D2 = zeros(5, 5); D2(1:3, 1:3) = S; D2(4:5, 4:5) = eye(2);
%!assert(norm(diag(C).^2+diag(S).^2 - ones(3, 1)) <= 1e-6)
%!assert(norm((U'*A*X)-D1*R) <= 1e-6)
%!assert(norm((V'*B*X)-D2*R) <= 1e-6)

%! # disp("A 5x3, B 5x5, [A' B'] rank deficient");
%! A = A0.'; B = B0.';
%! A(:, 3) = 2*A(:, 1) - A(:, 2);
%! B(:, 3) = 2*B(:, 1) - B(:, 2);
%! # disp([rank(A) rank(B) rank([A' B'])]);
%! [U, V, C, S, X, R]=gsvd(A, B);
%! D1 = zeros(3, 4); D1(1:3, 1:3) = C;
%! D2 = eye(4); D2(1:3, 1:3) = S; D2(5,:) = 0;
%!assert(norm(diag(C).^2+diag(S).^2 - ones(3, 1)) <= 1e-6)
%!assert(norm((U'*A*X)-D1*[zeros(4, 1) R]) <= 1e-6)
%!assert(norm((V'*B*X)-D2*[zeros(4, 1) R]) <= 1e-6)

%! A0 = A0 +j * randn(5, 3); B0 =  B0=diag([1 2 4]) + j*diag([4 -2 -1]);
%! A = A0; B = B0;
%! # disp('Complex: Full rank, 5x3 by 3x3 matrices');
%! # disp([rank(A) rank(B) rank([A' B'])]);
%! [U, V, C, S, X, R] = gsvd(A, B);
%! D1 = zeros(5, 3); D1(1:3, 1:3) = C;
%! D2 = S;
%!assert(norm(diag(C).^2+diag(S).^2 - ones(3, 1)) <= 1e-6)
%!assert(norm((U'*A*X)-D1*R) <= 1e-6)
%!assert(norm((V'*B*X)-D2*R) <= 1e-6)

%! # disp('Complex: A 5x3 full rank, B 3x3 rank deficient');
%! B(2, 2) = 0;
%! # disp([rank(A) rank(B) rank([A' B'])]);

%! [U, V, C, S, X, R] = gsvd(A, B);
%! D1 = zeros(5, 3); D1(1, 1) = 1; D1(2:3, 2:3) = C;
%! D2 = [zeros(2, 1) S; zeros(1, 3)];
%!assert(norm(diag(C).^2+diag(S).^2 - ones(2, 1)) <= 1e-6)
%!assert(norm((U'*A*X)-D1*R) <= 1e-6)
%!assert(norm((V'*B*X)-D2*R) <= 1e-6)

%! # disp('Complex: A 5x3 rank deficient, B 3x3 full rank');
%! B = B0;
%! A(:, 3) = 2*A(:, 1) - A(:, 2);
%! # disp([rank(A) rank(B) rank([A' B'])]);
%! [U, V, C, S, X, R] = gsvd(A, B);
%! D1 = zeros(5, 3); D1(1:3, 1:3) = C;
%! D2 = S;
%!assert(norm(diag(C).^2+diag(S).^2 - ones(3, 1)) <= 1e-6)
%!assert(norm((U'*A*X)-D1*R) <= 1e-6)
%!assert(norm((V'*B*X)-D2*R) <= 1e-6)

%! # disp("Complex: A 5x3, B 3x3, [A' B'] rank deficient");
%! B(:, 3) = 2*B(:, 1) - B(:, 2);
%! # disp([rank(A) rank(B) rank([A' B'])]);
%! [U, V, C, S, X, R] = gsvd(A, B);
%! D1 = zeros(5, 2); D1(1:2, 1:2) = C;
%! D2 = [S; zeros(1, 2)];
%!assert(norm(diag(C).^2+diag(S).^2 - ones(2, 1)) <= 1e-6)
%!assert(norm((U'*A*X)-D1*[zeros(2, 1) R]) <= 1e-6)
%!assert(norm((V'*B*X)-D2*[zeros(2, 1) R]) <= 1e-6)

%! # now, A is 3x5
%! A = A0.'; B0=diag([1 2 4 8 16])+j*diag([-5 4 -3 2 -1]); 
%! B = B0;
%! # disp('Complex: Full rank, 3x5 by 5x5 matrices');
%! # disp([rank(A) rank(B) rank([A' B'])]);
%! [U, V, C, S, X, R] = gsvd(A, B);
%! D1 = [C zeros(3,2)];
%! D2 = [S zeros(3,2); zeros(2, 3) eye(2)]; 
%!assert(norm(diag(C).^2+diag(S).^2 - ones(3, 1)) <= 1e-6)
%!assert(norm((U'*A*X)-D1*R) <= 1e-6)
%!assert(norm((V'*B*X)-D2*R) <= 1e-6)

%! # disp('Complex: A 5x3 full rank, B 5x5 rank deficient');
%! B(2, 2) = 0;
%! # disp([rank(A) rank(B) rank([A' B'])]);
%! [U, V, C, S, X, R] = gsvd(A, B);
%! D1 = zeros(3, 5); D1(1, 1) = 1; D1(2:3, 2:3) = C;
%! D2 = zeros(5,5); D2(1:2, 2:3) = S; D2(3:4, 4:5) = eye(2);
%!assert(norm(diag(C).^2+diag(S).^2 - ones(2, 1)) <= 1e-6)
%!assert(norm((U'*A*X)-D1*R) <= 1e-6)
%!assert(norm((V'*B*X)-D2*R) <= 1e-6)

%! # disp('Complex: A 3x5 rank deficient, B 5x5 full rank');
%! B = B0;
%! A(3, :) = 2*A(1, :) - A(2, :);
%! # disp([rank(A) rank(B) rank([A' B'])]);
%! [U, V, C, S, X, R] = gsvd(A, B);
%! D1 = zeros(3, 5); D1(1:3, 1:3) = C;
%! D2 = zeros(5,5); D2(1:3, 1:3) = S; D2(4:5, 4:5) = eye(2);
%!assert(norm(diag(C).^2+diag(S).^2 - ones(3, 1)) <= 1e-6)
%!assert(norm((U'*A*X)-D1*R) <= 1e-6)
%!assert(norm((V'*B*X)-D2*R) <= 1e-6)

%! # disp("Complex: A 5x3, B 5x5, [A' B'] rank deficient");
%! A = A0.'; B = B0.';
%! A(:, 3) = 2*A(:, 1) - A(:, 2);
%! B(:, 3) = 2*B(:, 1) - B(:, 2);
%! # disp([rank(A) rank(B) rank([A' B'])]);
%! [U, V, C, S, X, R]=gsvd(A, B);
%! D1 = zeros(3, 4); D1(1:3, 1:3) = C;
%! D2 = eye(4); D2(1:3, 1:3) = S; D2(5,:) = 0;
%!assert(norm(diag(C).^2+diag(S).^2 - ones(3, 1)) <= 1e-6)
%!assert(norm((U'*A*X)-D1*[zeros(4, 1) R]) <= 1e-6)
%!assert(norm((V'*B*X)-D2*[zeros(4, 1) R]) <= 1e-6)

 */
