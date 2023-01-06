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

#include "svd.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"
#include "pr-output.h"
#include "utils.h"
#include "variables.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static std::string Vsvd_driver = "gesvd";

template <typename T>
static typename math::svd<T>::Type
svd_type (int nargin, int nargout, const octave_value_list& args, const T& A)
{
  if (nargout == 0 || nargout == 1)
    return math::svd<T>::Type::sigma_only;
  else if (nargin == 1)
    return math::svd<T>::Type::std;
  else if (! args(1).is_real_scalar ())
    return math::svd<T>::Type::economy;
  else
    {
      if (A.rows () > A.columns ())
        return math::svd<T>::Type::economy;
      else
        return math::svd<T>::Type::std;
    }
}

template <typename T>
static typename math::svd<T>::Driver
svd_driver (void)
{
  if (Vsvd_driver == "gejsv")
    return math::svd<T>::Driver::GEJSV;
  else if (Vsvd_driver == "gesdd")
    return math::svd<T>::Driver::GESDD;
  else
    return math::svd<T>::Driver::GESVD;  // default
}

DEFUN (svd, args, nargout,
       classes: double single
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{s} =} svd (@var{A})
@deftypefnx {} {[@var{U}, @var{S}, @var{V}] =} svd (@var{A})
@deftypefnx {} {[@var{U}, @var{S}, @var{V}] =} svd (@var{A}, "econ")
@deftypefnx {} {[@var{U}, @var{S}, @var{V}] =} svd (@var{A}, 0)
@cindex singular value decomposition
Compute the singular value decomposition of @var{A}.

The singular value decomposition is defined by the relation

@tex
$$
 A = U S V^{\dagger}
$$
@end tex
@ifnottex

@example
A = U*S*V'
@end example

@end ifnottex

The function @code{svd} normally returns only the vector of singular values.
When called with three return values, it computes
@tex
$U$, $S$, and $V$.
@end tex
@ifnottex
@var{U}, @var{S}, and @var{V}.
@end ifnottex
For example,

@example
svd (hilb (3))
@end example

@noindent
returns

@example
@group
ans =

  1.4083189
  0.1223271
  0.0026873
@end group
@end example

@noindent
and

@example
[u, s, v] = svd (hilb (3))
@end example

@noindent
returns

@example
@group
u =

  -0.82704   0.54745   0.12766
  -0.45986  -0.52829  -0.71375
  -0.32330  -0.64901   0.68867

s =

  1.40832  0.00000  0.00000
  0.00000  0.12233  0.00000
  0.00000  0.00000  0.00269

v =

  -0.82704   0.54745   0.12766
  -0.45986  -0.52829  -0.71375
  -0.32330  -0.64901   0.68867
@end group
@end example

When given a second argument that is not 0, @code{svd} returns an economy-sized
decomposition, eliminating the unnecessary rows or columns of @var{U} or
@var{V}.

If the second argument is exactly 0, then the choice of decomposition is based
on the matrix @var{A}.  If @var{A} has more rows than columns then an
economy-sized decomposition is returned, otherwise a regular decomposition
is calculated.

Algorithm Notes: When calculating the full decomposition (left and right
singular matrices in addition to singular values) there is a choice of two
routines in @sc{lapack}.  The default routine used by Octave is @code{gesvd}.
The alternative is @code{gesdd} which is 5X faster, but may use more memory
and may be inaccurate for some input matrices.  There is a third routine
@code{gejsv}, suitable for better accuracy at extreme scale.  See the
documentation for @code{svd_driver} for more information on choosing a driver.
@seealso{svd_driver, svds, eig, lu, chol, hess, qr, qz}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2 || nargout > 3)
    print_usage ();

  octave_value arg = args(0);

  if (arg.ndims () != 2)
    error ("svd: A must be a 2-D matrix");

  octave_value_list retval;

  bool isfloat = arg.is_single_type ();

  if (isfloat)
    {
      if (arg.isreal ())
        {
          FloatMatrix tmp = arg.float_matrix_value ();

          if (tmp.any_element_is_inf_or_nan ())
            error ("svd: cannot take SVD of matrix containing Inf or NaN values");

          math::svd<FloatMatrix> result
          (tmp,
           svd_type<FloatMatrix> (nargin, nargout, args, tmp),
           svd_driver<FloatMatrix> ());

          FloatDiagMatrix sigma = result.singular_values ();

          if (nargout == 0 || nargout == 1)
            retval(0) = sigma.extract_diag ();
          else if (nargout == 2)
            retval = ovl (result.left_singular_matrix (),
                          sigma);
          else
            retval = ovl (result.left_singular_matrix (),
                          sigma,
                          result.right_singular_matrix ());
        }
      else if (arg.iscomplex ())
        {
          FloatComplexMatrix ctmp = arg.float_complex_matrix_value ();

          if (ctmp.any_element_is_inf_or_nan ())
            error ("svd: cannot take SVD of matrix containing Inf or NaN values");

          math::svd<FloatComplexMatrix> result
          (ctmp,
           svd_type<FloatComplexMatrix> (nargin, nargout, args, ctmp),
           svd_driver<FloatComplexMatrix> ());

          FloatDiagMatrix sigma = result.singular_values ();

          if (nargout == 0 || nargout == 1)
            retval(0) = sigma.extract_diag ();
          else if (nargout == 2)
            retval = ovl (result.left_singular_matrix (),
                          sigma);
          else
            retval = ovl (result.left_singular_matrix (),
                          sigma,
                          result.right_singular_matrix ());
        }
    }
  else
    {
      if (arg.isreal ())
        {
          Matrix tmp = arg.matrix_value ();

          if (tmp.any_element_is_inf_or_nan ())
            error ("svd: cannot take SVD of matrix containing Inf or NaN values");

          math::svd<Matrix> result
          (tmp,
           svd_type<Matrix> (nargin, nargout, args, tmp),
           svd_driver<Matrix> ());

          DiagMatrix sigma = result.singular_values ();

          if (nargout == 0 || nargout == 1)
            retval(0) = sigma.extract_diag ();
          else if (nargout == 2)
            retval = ovl (result.left_singular_matrix (),
                          sigma);
          else
            retval = ovl (result.left_singular_matrix (),
                          sigma,
                          result.right_singular_matrix ());
        }
      else if (arg.iscomplex ())
        {
          ComplexMatrix ctmp = arg.complex_matrix_value ();

          if (ctmp.any_element_is_inf_or_nan ())
            error ("svd: cannot take SVD of matrix containing Inf or NaN values");

          math::svd<ComplexMatrix> result
          (ctmp,
           svd_type<ComplexMatrix> (nargin, nargout, args, ctmp),
           svd_driver<ComplexMatrix> ());

          DiagMatrix sigma = result.singular_values ();

          if (nargout == 0 || nargout == 1)
            retval(0) = sigma.extract_diag ();
          else if (nargout == 2)
            retval = ovl (result.left_singular_matrix (),
                          sigma);
          else
            retval = ovl (result.left_singular_matrix (),
                          sigma,
                          result.right_singular_matrix ());
        }
      else
        err_wrong_type_arg ("svd", arg);
    }

  return retval;
}

/*
%!assert (svd ([1, 2; 2, 1]), [3; 1], sqrt (eps))

%!test
%! a = [1, 2; 3, 4] + [5, 6; 7, 8]*i;
%! [u,s,v] = svd (a);
%! assert (a, u * s * v', 128 * eps);

%!test
%! [u, s, v] = svd ([1, 2; 2, 1]);
%! x = 1 / sqrt (2);
%! assert (u, [-x, -x; -x, x], sqrt (eps));
%! assert (s, [3, 0; 0, 1], sqrt (eps));
%! assert (v, [-x, x; -x, -x], sqrt (eps));

%!test
%! a = [1, 2, 3; 4, 5, 6];
%! [u, s, v] = svd (a);
%! assert (u * s * v', a, sqrt (eps));

%!test
%! a = [1, 2; 3, 4; 5, 6];
%! [u, s, v] = svd (a);
%! assert (u * s * v', a, sqrt (eps));

%!test
%! a = [1, 2, 3; 4, 5, 6];
%! [u, s, v] = svd (a, 1);
%! assert (u * s * v', a, sqrt (eps));

%!test
%! a = [1, 2; 3, 4; 5, 6];
%! [u, s, v] = svd (a, 1);
%! assert (u * s * v', a, sqrt (eps));

%!assert (svd (single ([1, 2; 2, 1])), single ([3; 1]), sqrt (eps ("single")))

%!test
%! [u, s, v] = svd (single ([1, 2; 2, 1]));
%! x = single (1 / sqrt (2));
%! assert (u, [-x, -x; -x, x], sqrt (eps ("single")));
%! assert (s, single ([3, 0; 0, 1]), sqrt (eps ("single")));
%! assert (v, [-x, x; -x, -x], sqrt (eps ("single")));

%!test
%! a = single ([1, 2, 3; 4, 5, 6]);
%! [u, s, v] = svd (a);
%! assert (u * s * v', a, sqrt (eps ("single")));

%!test
%! a = single ([1, 2; 3, 4; 5, 6]);
%! [u, s, v] = svd (a);
%! assert (u * s * v', a, sqrt (eps ("single")));

%!test
%! a = single ([1, 2, 3; 4, 5, 6]);
%! [u, s, v] = svd (a, 1);
%! assert (u * s * v', a, sqrt (eps ("single")));

%!test
%! a = single ([1, 2; 3, 4; 5, 6]);
%! [u, s, v] = svd (a, 1);
%! assert (u * s * v', a, sqrt (eps ("single")));

%!test
%! a = zeros (0, 5);
%! [u, s, v] = svd (a);
%! assert (size (u), [0, 0]);
%! assert (size (s), [0, 5]);
%! assert (size (v), [5, 5]);

%!test
%! a = zeros (5, 0);
%! [u, s, v] = svd (a, 1);
%! assert (size (u), [5, 0]);
%! assert (size (s), [0, 0]);
%! assert (size (v), [0, 0]);

%!test <*49309>
%! [~,~,v] = svd ([1, 1, 1], 0);
%! assert (size (v), [3 3]);
%! [~,~,v] = svd ([1, 1, 1], "econ");
%! assert (size (v), [3 1]);

%!assert <*55710> (1 / svd (-0), Inf)

%!test
%! old_driver = svd_driver ("gejsv");
%! s0 = [1e-20; 1e-10; 1];  # only gejsv can pass
%! q = sqrt (0.5);
%! a = s0 .* [q, 0, -q; -0.5, q, -0.5; 0.5, q, 0.5];
%! s1 = svd (a);
%! svd_driver (old_driver);
%! assert (sort (s1), s0, -10 * eps);

%!error svd ()
%!error svd ([1, 2; 4, 5], 2, 3)
*/

DEFUN (svd_driver, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} svd_driver ()
@deftypefnx {} {@var{old_val} =} svd_driver (@var{new_val})
@deftypefnx {} {@var{old_val} =} svd_driver (@var{new_val}, "local")
Query or set the underlying @sc{lapack} driver used by @code{svd}.

Currently recognized values are @qcode{"gesdd"}, @qcode{"gesvd"}, and
@qcode{"gejsv"}.  The default is @qcode{"gesvd"}.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.

Algorithm Notes: The @sc{lapack} library routines @code{gesvd} and @code{gesdd}
are different only when calculating the full singular value decomposition (left
and right singular matrices as well as singular values).  When calculating just
the singular values the following discussion is not relevant.

The newer @code{gesdd} routine is based on a Divide-and-Conquer algorithm that
is 5X faster than the alternative @code{gesvd}, which is based on QR
factorization.  However, the new algorithm can use significantly more memory.
For an @nospell{MxN} input matrix the memory usage is of order O(min(M,N) ^ 2),
whereas the alternative is of order O(max(M,N)).

The routine @code{gejsv} uses a preconditioned Jacobi SVD algorithm.  Unlike
@code{gesvd} and @code{gesdd}, in @code{gejsv}, there is no bidiagonalization
step that could contaminate accuracy in some extreme cases.  Also, @code{gejsv}
is known to be optimally accurate in some sense.  However, the speed is slower
(single threaded at its core) and uses more memory (O(min(M,N) ^ 2 + M + N)).

Beyond speed and memory issues, there have been instances where some input
matrices were not accurately decomposed by @code{gesdd}.  See currently active
bug @url{https://savannah.gnu.org/bugs/?55564}.  Until these accuracy issues
are resolved in a new version of the @sc{lapack} library, the default driver
in Octave has been set to @qcode{"gesvd"}.

@seealso{svd}
@end deftypefn */)
{
  static const char *driver_names[] = { "gesvd", "gesdd", "gejsv", nullptr };

  return set_internal_variable (Vsvd_driver, args, nargout,
                                "svd_driver", driver_names);
}

/*
%!test
%! A = [1+1i, 1-1i, 0; 0, 2, 0; 1i, 1i, 1+2i];
%! old_driver = svd_driver ("gesvd");
%! [U1, S1, V1] = svd (A);
%! svd_driver ("gesdd");
%! [U2, S2, V2] = svd (A);
%! svd_driver ("gejsv");
%! [U3, S3, V3] = svd (A);
%! assert (svd_driver (), "gejsv");
%! svd_driver (old_driver);
%! assert (U1, U2, 6*eps);
%! assert (S1, S2, 6*eps);
%! assert (V1, V2, 6*eps);
%! z = U1(1,:) ./ U3(1,:);
%! assert (U1, U3 .* z, 100*eps);
%! assert (S1, S3, 6*eps);
%! assert (V1, V3 .* z, 100*eps);
*/

OCTAVE_END_NAMESPACE(octave)
