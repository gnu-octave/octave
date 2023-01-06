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

// Originally written by A. S. Hodel <scotte@eng.auburn.edu>

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "defun.h"
#include "error.h"
#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)

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
The Givens matrix is a 2-by-2 orthogonal matrix

@example
@group
@var{G} = [ @var{c} , @var{s}
     -@var{s}', @var{c}]
@end group
@end example

@noindent
such that

@example
@var{G} * [@var{x}; @var{y}] = [*; 0]
@end example

@noindent
with @var{x} and @var{y} scalars.
@end ifnottex

If two output arguments are requested, return the factors @var{c} and @var{s}
rather than the Givens rotation matrix.

For example:

@example
@group
givens (1, 1)
   @result{}   0.70711   0.70711
       -0.70711   0.70711
@end group
@end example

Note: The Givens matrix represents a counterclockwise rotation of a 2-D
plane and can be used to introduce zeros into a matrix prior to complete
factorization.
@seealso{planerot, qr}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  octave_value_list retval;

  if (args(0).is_single_type () || args(1).is_single_type ())
    {
      if (args(0).iscomplex () || args(1).iscomplex ())
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
      if (args(0).iscomplex () || args(1).iscomplex ())
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

OCTAVE_END_NAMESPACE(octave)
