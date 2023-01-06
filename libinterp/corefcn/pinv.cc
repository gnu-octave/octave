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

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"
#include "ops.h"
#include "ov-re-diag.h"
#include "ov-cx-diag.h"
#include "ov-flt-re-diag.h"
#include "ov-flt-cx-diag.h"
#include "ov-perm.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (pinv, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{B} =} pinv (@var{A})
@deftypefnx {} {@var{B} =} pinv (@var{A}, @var{tol})
Return the @nospell{Moore-Penrose} pseudoinverse of @var{A}.

Singular values less than @var{tol} are ignored.

If the second argument is omitted, it is taken to be

@example
tol = max ([rows(@var{x}), columns(@var{x})]) * norm (@var{x}) * eps
@end example

@seealso{inv, ldivide}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  octave_value arg = args(0);

  if (! arg.isnumeric ())
    err_wrong_type_arg ("pinv", arg);

  if (arg.isempty ())
    return ovl (Matrix ());

  octave_value retval;

  bool isfloat = arg.is_single_type ();

  if (arg.is_diag_matrix ())
    {
      if (isfloat)
        {
          float tol = 0.0;
          if (nargin == 2)
            tol = args(1).float_value ();

          if (tol < 0.0)
            error ("pinv: TOL must be greater than zero");

          if (arg.isreal ())
            retval = arg.float_diag_matrix_value ().pseudo_inverse (tol);
          else
            retval = arg.float_complex_diag_matrix_value ().pseudo_inverse (tol);
        }
      else
        {
          double tol = 0.0;
          if (nargin == 2)
            tol = args(1).double_value ();

          if (tol < 0.0)
            error ("pinv: TOL must be greater than zero");

          if (arg.isreal ())
            retval = arg.diag_matrix_value ().pseudo_inverse (tol);
          else
            retval = arg.complex_diag_matrix_value ().pseudo_inverse (tol);
        }
    }
  else if (arg.is_perm_matrix ())
    {
      retval = arg.perm_matrix_value ().inverse ();
    }
  else if (isfloat)
    {
      float tol = 0.0;
      if (nargin == 2)
        tol = args(1).float_value ();

      if (tol < 0.0)
        error ("pinv: TOL must be greater than zero");

      if (arg.isreal ())
        {
          FloatMatrix m = arg.float_matrix_value ();

          retval = m.pseudo_inverse (tol);
        }
      else if (arg.iscomplex ())
        {
          FloatComplexMatrix m = arg.float_complex_matrix_value ();

          retval = m.pseudo_inverse (tol);
        }
      else
        err_wrong_type_arg ("pinv", arg);
    }
  else
    {
      double tol = 0.0;
      if (nargin == 2)
        tol = args(1).double_value ();

      if (tol < 0.0)
        error ("pinv: TOL must be greater than zero");

      if (arg.isreal ())
        {
          Matrix m = arg.matrix_value ();

          retval = m.pseudo_inverse (tol);
        }
      else if (arg.iscomplex ())
        {
          ComplexMatrix m = arg.complex_matrix_value ();

          retval = m.pseudo_inverse (tol);
        }
      else
        err_wrong_type_arg ("pinv", arg);
    }

  return retval;
}

/*
%!shared a, b, tol, hitol, d, u, x, y
%! old_state = rand ("state");
%! restore_state = onCleanup (@() rand ("state", old_state));
%! rand ("state", 42); # initialize generator to make behavior reproducible
%! a = reshape (rand*[1:16], 4, 4);  # Rank 2 matrix
%! b = pinv (a);
%! tol = 4e-14;
%! hitol = 40* sqrt (eps);
%! d = diag ([rand, rand, hitol, hitol]);
%! u = rand (4);                     # Could be singular by freak accident
%! x = inv (u)*d*u;
%! y = pinv (x, sqrt (eps));

## Verify Penrose conditions for pseudoinverse
%!assert (a*b*a, a, tol)
%!assert (b*a*b, b, tol)
%!assert ((b*a)', b*a, tol)
%!assert ((a*b)', a*b, tol)
%!assert (x*y*x, x, -hitol)
%!assert (y*x*y, y, -hitol)
%!assert ((x*y)', x*y, hitol)
%!assert ((y*x)', y*x, hitol)

## Clear shared variables
%!shared

## Test pinv for Diagonal matrices
%!test
%! x = diag ([3 2 1 0 -0.5]);
%! y = pinv (x);
%! assert (typeinfo (y)(1:8), "diagonal");
%! assert (isa (y, "double"));
%! assert (diag (y), [1/3, 1/2, 1, 0  1/-0.5]');
%! y = pinv (x, 1);
%! assert (diag (y), [1/3 1/2 1 0 0]');
%! y = pinv (x, 2);
%! assert (diag (y), [1/3 1/2 0 0 0]');

## Basic test for integer inputs
%!assert (pinv (int32 (2)), 0.5)
%!assert (pinv (uint32 (2)), 0.5)
%!assert (pinv (int64 (2)), 0.5)
%!assert (pinv (uint64 (2)), 0.5)

## Test special case of 0 scalars and vectors
%!assert (pinv (0), 0)
%!assert (pinv ([0, 0, 0]), [0; 0; 0])
%!assert (pinv (single (0)), single (0))
%!assert (pinv (single ([0, 0, 0])), single ([0; 0; 0]))
%!assert (pinv (complex (0,0)), 0)
%!assert (pinv (complex ([0,0,0], [0,0,0])), [0; 0; 0])
%!assert (pinv (complex (single (0),0)), single (0))
%!assert (pinv (complex (single ([0,0,0]), [0,0,0])), single ([0; 0; 0]))

## Test input validation
%!error <wrong type argument> pinv ("Hello World")
%!error <wrong type argument> pinv ({1})
%!error <wrong type argument> pinv (true)

*/

OCTAVE_END_NAMESPACE(octave)
