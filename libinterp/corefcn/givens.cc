/*

Copyright (C) 1996-2016 John W. Eaton

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

// Originally written by A. S. Hodel <scotte@eng.auburn.edu>

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "defun.h"
#include "error.h"
#include "ovl.h"

DEFUN (givens, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{G} =} givens (@var{x}, @var{y})
@deftypefnx {} {[@var{c}, @var{s}] =} givens (@var{x}, @var{y})
Compute the Givens rotation matrix @var{G}.

@tex
The Givens matrix is a $2\times 2$ orthogonal matrix
$$
 G = \left[\matrix{c & s\cr -s'& c\cr}\right]
$$
such that
$$
 G \left[\matrix{x\cr y}\right] = \left[\matrix{\ast\cr 0}\right]
$$
with $x$ and $y$ scalars.
@end tex
@ifnottex
The Givens matrix is a 2 by 2 orthogonal matrix

@code{@var{g} = [@var{c} @var{s}; -@var{s}' @var{c}]}

such that

@code{@var{g} [@var{x}; @var{y}] = [*; 0]}

with @var{x} and @var{y} scalars.
@end ifnottex

If two output arguments are requested, return the factors @var{c} and
@var{s} rather than the Givens rotation matrix.

For example:

@example
@group
givens (1, 1)
   @result{}   0.70711   0.70711
       -0.70711   0.70711
@end group
@end example
@seealso{planerot}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  octave_value_list retval;

  if (args(0).is_single_type () || args(1).is_single_type ())
    {
      if (args(0).is_complex_type () || args(1).is_complex_type ())
        {
          FloatComplex cx = args(0).float_complex_value ();
          FloatComplex cy = args(1).float_complex_value ();

          FloatComplexMatrix result = Givens (cx, cy);

          switch (nargout)
            {
            case 0:
            case 1:
              retval = ovl (result);
              break;

            case 2:
              retval = ovl (result(0, 0), result(0, 1));
              break;
            }
        }
      else
        {
          float x = args(0).float_value ();
          float y = args(1).float_value ();

          FloatMatrix result = Givens (x, y);

          switch (nargout)
            {
            case 0:
            case 1:
              retval = ovl (result);
              break;

            case 2:
              retval = ovl (result(0, 0), result(0, 1));
              break;
            }
        }
    }
  else
    {
      if (args(0).is_complex_type () || args(1).is_complex_type ())
        {
          Complex cx = args(0).complex_value ();
          Complex cy = args(1).complex_value ();

          ComplexMatrix result = Givens (cx, cy);

          switch (nargout)
            {
            case 0:
            case 1:
              retval = ovl (result);
              break;

            case 2:
              retval = ovl (result(0, 0), result(0, 1));
              break;
            }
        }
      else
        {
          double x = args(0).double_value ();
          double y = args(1).double_value ();

          Matrix result = Givens (x, y);

          switch (nargout)
            {
            case 0:
            case 1:
              retval = ovl (result);
              break;

            case 2:
              retval = ovl (result(0, 0), result(0, 1));
              break;
            }
        }
    }

  return retval;
}

/*
%!assert (givens (1,1), [1, 1; -1, 1] / sqrt (2), 2*eps)
%!assert (givens (1,0), eye (2))
%!assert (givens (0,1), [0, 1; -1 0])

%!error givens ()
%!error givens (1)
%!error [a,b,c] = givens (1, 1)
*/
