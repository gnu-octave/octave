/*

Copyright (C) 1996-2015 John W. Eaton

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

static std::string Vsvd_driver = "gesvd";

template <typename T>
static typename svd<T>::type
svd_type (int nargin, int nargout)
{
  return ((nargout == 0 || nargout == 1)
          ? svd<T>::sigma_only
          : (nargin == 2) ? svd<T>::economy : svd<T>::std);
}

template <typename T>
static typename svd<T>::driver
svd_driver (void)
{
  return Vsvd_driver == "gesvd" ? svd<T>::GESVD : svd<T>::GESDD;
}

DEFUN (svd, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {} {@var{s} =} svd (@var{A})\n\
@deftypefnx {} {[@var{U}, @var{S}, @var{V}] =} svd (@var{A})\n\
@deftypefnx {} {[@var{U}, @var{S}, @var{V}] =} svd (@var{A}, @var{econ})\n\
@cindex singular value decomposition\n\
Compute the singular value decomposition of @var{A}\n\
@tex\n\
$$\n\
 A = U S V^{\\dagger}\n\
$$\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
A = U*S*V'\n\
@end example\n\
\n\
@end ifnottex\n\
\n\
The function @code{svd} normally returns only the vector of singular values.\n\
When called with three return values, it computes\n\
@tex\n\
$U$, $S$, and $V$.\n\
@end tex\n\
@ifnottex\n\
@var{U}, @var{S}, and @var{V}.\n\
@end ifnottex\n\
For example,\n\
\n\
@example\n\
svd (hilb (3))\n\
@end example\n\
\n\
@noindent\n\
returns\n\
\n\
@example\n\
@group\n\
ans =\n\
\n\
  1.4083189\n\
  0.1223271\n\
  0.0026873\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
and\n\
\n\
@example\n\
[u, s, v] = svd (hilb (3))\n\
@end example\n\
\n\
@noindent\n\
returns\n\
\n\
@example\n\
@group\n\
u =\n\
\n\
  -0.82704   0.54745   0.12766\n\
  -0.45986  -0.52829  -0.71375\n\
  -0.32330  -0.64901   0.68867\n\
\n\
s =\n\
\n\
  1.40832  0.00000  0.00000\n\
  0.00000  0.12233  0.00000\n\
  0.00000  0.00000  0.00269\n\
\n\
v =\n\
\n\
  -0.82704   0.54745   0.12766\n\
  -0.45986  -0.52829  -0.71375\n\
  -0.32330  -0.64901   0.68867\n\
@end group\n\
@end example\n\
\n\
If given a second argument, @code{svd} returns an economy-sized\n\
decomposition, eliminating the unnecessary rows or columns of @var{U} or\n\
@var{V}.\n\
@seealso{svd_driver, svds, eig, lu, chol, hess, qr, qz}\n\
@end deftypefn")
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2 || nargout == 2 || nargout > 3)
    print_usage ();

  octave_value arg = args(0);

  if (arg.ndims () != 2)
    error ("svd: A must be a 2-D matrix");

  octave_value_list retval;

  bool isfloat = arg.is_single_type ();

  if (isfloat)
    {
      if (arg.is_real_type ())
        {
          FloatMatrix tmp = arg.float_matrix_value ();

          if (tmp.any_element_is_inf_or_nan ())
            error ("svd: cannot take SVD of matrix containing Inf or NaN values");

          svd<FloatMatrix> result (tmp,
                                   svd_type<FloatMatrix> (nargin, nargout),
                                   svd_driver<FloatMatrix> ());

          FloatDiagMatrix sigma = result.singular_values ();

          if (nargout == 0 || nargout == 1)
            retval(0) = sigma.extract_diag ();
          else
            retval = ovl (result.left_singular_matrix (),
                          sigma,
                          result.right_singular_matrix ());
        }
      else if (arg.is_complex_type ())
        {
          FloatComplexMatrix ctmp = arg.float_complex_matrix_value ();

          if (ctmp.any_element_is_inf_or_nan ())
            error ("svd: cannot take SVD of matrix containing Inf or NaN values");

          svd<FloatComplexMatrix> result (ctmp,
                                          svd_type<FloatComplexMatrix> (nargin, nargout),
                                          svd_driver<FloatComplexMatrix> ());

          FloatDiagMatrix sigma = result.singular_values ();

          if (nargout == 0 || nargout == 1)
            retval(0) = sigma.extract_diag ();
          else
            retval = ovl (result.left_singular_matrix (),
                          sigma,
                          result.right_singular_matrix ());
        }
    }
  else
    {
      if (arg.is_real_type ())
        {
          Matrix tmp = arg.matrix_value ();

          if (tmp.any_element_is_inf_or_nan ())
            error ("svd: cannot take SVD of matrix containing Inf or NaN values");

          svd<Matrix> result (tmp,
                              svd_type<Matrix> (nargin, nargout),
                              svd_driver<Matrix> ());

          DiagMatrix sigma = result.singular_values ();

          if (nargout == 0 || nargout == 1)
            retval(0) = sigma.extract_diag ();
          else
            retval = ovl (result.left_singular_matrix (),
                          sigma,
                          result.right_singular_matrix ());
        }
      else if (arg.is_complex_type ())
        {
          ComplexMatrix ctmp = arg.complex_matrix_value ();

          if (ctmp.any_element_is_inf_or_nan ())
            error ("svd: cannot take SVD of matrix containing Inf or NaN values");

          svd<ComplexMatrix> result (ctmp,
                                     svd_type<ComplexMatrix> (nargin, nargout),
                                     svd_driver<ComplexMatrix> ());

          DiagMatrix sigma = result.singular_values ();

          if (nargout == 0 || nargout == 1)
            retval(0) = sigma.extract_diag ();
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

%!error svd ()
%!error svd ([1, 2; 4, 5], 2, 3)
%!error [u, v] = svd ([1, 2; 3, 4])
*/

DEFUN (svd_driver, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {} {@var{val} =} svd_driver ()\n\
@deftypefnx {} {@var{old_val} =} svd_driver (@var{new_val})\n\
@deftypefnx {} {} svd_driver (@var{new_val}, \"local\")\n\
Query or set the underlying @sc{lapack} driver used by @code{svd}.\n\
\n\
Currently recognized values are @qcode{\"gesvd\"} and @qcode{\"gesdd\"}.\n\
The default is @qcode{\"gesvd\"}.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{svd}\n\
@end deftypefn")
{
  static const char *driver_names[] = { "gesvd", "gesdd", 0 };

  return SET_INTERNAL_VARIABLE_CHOICES (svd_driver, driver_names);
}

/*
%!test
%! A = [1+1i, 1-1i, 0; 0, 2, 0; 1i, 1i, 1+2i];
%! old_driver = svd_driver ("gesvd");
%! [U1, S1, V1] = svd (A);
%! svd_driver ("gesdd");
%! [U2, S2, V2] = svd (A);
%! assert (U1, U2, 5*eps);
%! assert (S1, S2, 5*eps);
%! assert (V1, V2, 5*eps);
%! svd_driver (old_driver);
*/
