/*

Copyright (C) 1997-2017 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "lo-specfun.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"
#include "utils.h"

DEFUN (gammainc, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} gammainc (@var{x}, @var{a})
@deftypefnx {} {} gammainc (@var{x}, @var{a}, "lower")
@deftypefnx {} {} gammainc (@var{x}, @var{a}, "upper")
Compute the normalized incomplete gamma function.

This is defined as
@tex
$$
 \gamma (x, a) = {1 \over {\Gamma (a)}}\displaystyle{\int_0^x t^{a-1} e^{-t} dt}
$$
@end tex
@ifnottex

@example
@group
                                x
                       1       /
gammainc (x, a) = ---------    | exp (-t) t^(a-1) dt
                  gamma (a)    /
                            t=0
@end group
@end example

@end ifnottex
with the limiting value of 1 as @var{x} approaches infinity.
The standard notation is @math{P(a,x)}, e.g., @nospell{Abramowitz} and
@nospell{Stegun} (6.5.1).

If @var{a} is scalar, then @code{gammainc (@var{x}, @var{a})} is returned
for each element of @var{x} and vice versa.

If neither @var{x} nor @var{a} is scalar, the sizes of @var{x} and
@var{a} must agree, and @code{gammainc} is applied element-by-element.

By default the incomplete gamma function integrated from 0 to @var{x} is
computed.  If @qcode{"upper"} is given then the complementary function
integrated from @var{x} to infinity is calculated.  It should be noted that

@example
gammainc (@var{x}, @var{a}) @equiv{} 1 - gammainc (@var{x}, @var{a}, "upper")
@end example
@seealso{gamma, gammaln}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    print_usage ();

  bool lower = true;
  if (nargin == 3)
    {
      std::string s = args(2).xstring_value ("gammainc: third argument must be \"lower\" or \"upper\"");

      std::transform (s.begin (), s.end (), s.begin (), tolower);

      if (s == "upper")
        lower = false;
      else if (s == "lower")
        lower = true;
      else
        error ("gammainc: third argument must be \"lower\" or \"upper\"");
    }

  octave_value retval;

  octave_value x_arg = args(0);
  octave_value a_arg = args(1);

  // FIXME: Can we make a template version of the duplicated code below
  if (x_arg.is_single_type () || a_arg.is_single_type ())
    {
      if (x_arg.is_scalar_type ())
        {
          float x = x_arg.float_value ();

          if (a_arg.is_scalar_type ())
            {
              float a = a_arg.float_value ();

              retval = lower ? octave::math::gammainc (x, a)
                             : 1.0f - octave::math::gammainc (x, a);
            }
          else
            {
              FloatNDArray a = a_arg.float_array_value ();

              retval = lower ? octave::math::gammainc (x, a)
                             : 1.0f - octave::math::gammainc (x, a);
            }
        }
      else
        {
          FloatNDArray x = x_arg.float_array_value ();

          if (a_arg.is_scalar_type ())
            {
              float a = a_arg.float_value ();

              retval = lower ? octave::math::gammainc (x, a)
                             : 1.0f - octave::math::gammainc (x, a);
            }
          else
            {
              FloatNDArray a = a_arg.float_array_value ();

              retval = lower ? octave::math::gammainc (x, a)
                             : 1.0f - octave::math::gammainc (x, a);
            }
        }
    }
  else
    {
      if (x_arg.is_scalar_type ())
        {
          double x = x_arg.double_value ();

          if (a_arg.is_scalar_type ())
            {
              double a = a_arg.double_value ();

              retval = lower ? octave::math::gammainc (x, a)
                             : 1.0 - octave::math::gammainc (x, a);
            }
          else
            {
              NDArray a = a_arg.array_value ();

              retval = lower ? octave::math::gammainc (x, a)
                             : 1.0 - octave::math::gammainc (x, a);
            }
        }
      else
        {
          NDArray x = x_arg.array_value ();

          if (a_arg.is_scalar_type ())
            {
              double a = a_arg.double_value ();

              retval = lower ? octave::math::gammainc (x, a)
                             : 1.0 - octave::math::gammainc (x, a);
            }
          else
            {
              NDArray a = a_arg.array_value ();

              retval = lower ? octave::math::gammainc (x, a)
                             : 1.0 - octave::math::gammainc (x, a);
            }
        }
    }

  return retval;
}

/*
%!test
%! a = [.5 .5 .5 .5 .5];
%! x = [0 1 2 3 4];
%! v1 = sqrt (pi)*erf (x)./gamma (a);
%! v3 = gammainc (x.*x, a);
%! assert (v1, v3, sqrt (eps));

%!assert (gammainc (0:4,0.5, "upper"), 1-gammainc (0:4,0.5), 1e-10)

%!test
%! a = single ([.5 .5 .5 .5 .5]);
%! x = single ([0 1 2 3 4]);
%! v1 = sqrt (pi ("single"))*erf (x)./gamma (a);
%! v3 = gammainc (x.*x, a);
%! assert (v1, v3, sqrt (eps ("single")));

%!assert (gammainc (single (0:4), single (0.5), "upper"),
%!        single (1)-gammainc (single (0:4), single (0.5)),
%!        single (1e-7))
*/
