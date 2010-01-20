/*

Copyright (C) 1996, 1997, 1999, 2000, 2003, 2005, 2006, 2007, 2008, 2009
              John W. Eaton

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

#include "CmplxSVD.h"
#include "dbleSVD.h"
#include "fCmplxSVD.h"
#include "floatSVD.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "pr-output.h"
#include "utils.h"

DEFUN_DLD (svd, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{s} =} svd (@var{a})\n\
@deftypefnx {Loadable Function} {[@var{u}, @var{s}, @var{v}] =} svd (@var{a})\n\
@cindex singular value decomposition\n\
Compute the singular value decomposition of @var{a}\n\
@tex\n\
$$\n\
 A = U S V^H\n\
$$\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
A = U*S*V'\n\
@end example\n\
@end ifnottex\n\
\n\
The function @code{svd} normally returns the vector of singular values.\n\
If asked for three return values, it computes\n\
@tex\n\
$U$, $S$, and $V$.\n\
@end tex\n\
@ifnottex\n\
U, S, and V.\n\
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
decomposition, eliminating the unnecessary rows or columns of @var{u} or\n\
@var{v}.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2 || nargout == 2 || nargout > 3)
    {
      print_usage ();
      return retval;
    }

  octave_value arg = args(0);

  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();

  bool isfloat = arg.is_single_type ();

  if (nr == 0 || nc == 0)
    {
      if (isfloat)
        {
          if (nargout == 3)
            {
              retval(3) = float_identity_matrix (nr, nr);
              retval(2) = FloatMatrix (nr, nc);
              retval(1) = float_identity_matrix (nc, nc);
            }
          else
            retval(0) = FloatMatrix (0, 1);
        }
      else
        {
          if (nargout == 3)
            {
              retval(3) = identity_matrix (nr, nr);
              retval(2) = Matrix (nr, nc);
              retval(1) = identity_matrix (nc, nc);
            }
          else
            retval(0) = Matrix (0, 1);
        }
    }
  else
    {
      SVD::type type = ((nargout == 0 || nargout == 1)
                        ? SVD::sigma_only
                        : (nargin == 2) ? SVD::economy : SVD::std);

      if (isfloat)
        {
          if (arg.is_real_type ())
            {
              FloatMatrix tmp = arg.float_matrix_value ();

              if (! error_state)
                {
                  if (tmp.any_element_is_inf_or_nan ())
                    {
                      error ("svd: cannot take SVD of matrix containing Inf or NaN values"); 
                      return retval;
                    }

                  FloatSVD result (tmp, type);

                  FloatDiagMatrix sigma = result.singular_values ();

                  if (nargout == 0 || nargout == 1)
                    {
                      retval(0) = sigma.diag ();
                    }
                  else
                    {
                      retval(2) = result.right_singular_matrix ();
                      retval(1) = sigma;
                      retval(0) = result.left_singular_matrix ();
                    }
                }
            }
          else if (arg.is_complex_type ())
            {
              FloatComplexMatrix ctmp = arg.float_complex_matrix_value ();

              if (! error_state)
                {
                  if (ctmp.any_element_is_inf_or_nan ())
                    {
                      error ("svd: cannot take SVD of matrix containing Inf or NaN values"); 
                      return retval;
                    }

                  FloatComplexSVD result (ctmp, type);

                  FloatDiagMatrix sigma = result.singular_values ();

                  if (nargout == 0 || nargout == 1)
                    {
                      retval(0) = sigma.diag ();
                    }
                  else
                    {
                      retval(2) = result.right_singular_matrix ();
                      retval(1) = sigma;
                      retval(0) = result.left_singular_matrix ();
                    }
                }
            }
        }
      else
        {
          if (arg.is_real_type ())
            {
              Matrix tmp = arg.matrix_value ();

              if (! error_state)
                {
                  if (tmp.any_element_is_inf_or_nan ())
                    {
                      error ("svd: cannot take SVD of matrix containing Inf or NaN values"); 
                      return retval;
                    }

                  SVD result (tmp, type);

                  DiagMatrix sigma = result.singular_values ();

                  if (nargout == 0 || nargout == 1)
                    {
                      retval(0) = sigma.diag ();
                    }
                  else
                    {
                      retval(2) = result.right_singular_matrix ();
                      retval(1) = sigma;
                      retval(0) = result.left_singular_matrix ();
                    }
                }
            }
          else if (arg.is_complex_type ())
            {
              ComplexMatrix ctmp = arg.complex_matrix_value ();

              if (! error_state)
                {
                  if (ctmp.any_element_is_inf_or_nan ())
                    {
                      error ("svd: cannot take SVD of matrix containing Inf or NaN values"); 
                      return retval;
                    }

                  ComplexSVD result (ctmp, type);

                  DiagMatrix sigma = result.singular_values ();

                  if (nargout == 0 || nargout == 1)
                    {
                      retval(0) = sigma.diag ();
                    }
                  else
                    {
                      retval(2) = result.right_singular_matrix ();
                      retval(1) = sigma;
                      retval(0) = result.left_singular_matrix ();
                    }
                }
            }
          else
            {
              gripe_wrong_type_arg ("svd", arg);
              return retval;
            }
        }
    }

  return retval;
}

/*

%!assert(svd ([1, 2; 2, 1]), [3; 1], sqrt (eps));

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

%!assert(svd (single([1, 2; 2, 1])), single([3; 1]), sqrt (eps('single')));

%!test
%! [u, s, v] = svd (single([1, 2; 2, 1]));
%! x = single (1 / sqrt (2));
%! assert (u, [-x, -x; -x, x], sqrt (eps('single')));
%! assert (s, single([3, 0; 0, 1]), sqrt (eps('single')));
%! assert (v, [-x, x; -x, -x], sqrt (eps('single')));

%!test
%! a = single([1, 2, 3; 4, 5, 6]);
%! [u, s, v] = svd (a);
%! assert (u * s * v', a, sqrt (eps('single')));

%!test
%! a = single([1, 2; 3, 4; 5, 6]);
%! [u, s, v] = svd (a);
%! assert (u * s * v', a, sqrt (eps('single')));

%!test
%! a = single([1, 2, 3; 4, 5, 6]);
%! [u, s, v] = svd (a, 1);
%! assert (u * s * v', a, sqrt (eps('single')));

%!test
%! a = single([1, 2; 3, 4; 5, 6]);
%! [u, s, v] = svd (a, 1);
%! assert (u * s * v', a, sqrt (eps('single')));

%!error <Invalid call to svd.*> svd ();
%!error <Invalid call to svd.*> svd ([1, 2; 4, 5], 2, 3);
%!error <Invalid call to svd.*> [u, v] = svd ([1, 2; 3, 4]);

*/
