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

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (sylvester, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{X} =} sylvester (@var{A}, @var{B}, @var{C})
Solve the Sylvester equation.

The Sylvester equation is defined as:
@tex
$$
 A X + X B = C
$$
@end tex
@ifnottex

@example
A X + X B = C
@end example

@end ifnottex
The solution is computed using standard @sc{lapack} subroutines.

For example:

@example
@group
sylvester ([1, 2; 3, 4], [5, 6; 7, 8], [9, 10; 11, 12])
   @result{} [ 0.50000, 0.66667; 0.66667, 0.50000 ]
@end group
@end example
@end deftypefn */)
{
  if (args.length () != 3)
    print_usage ();

  octave_value retval;

  octave_value arg_a = args(0);
  octave_value arg_b = args(1);
  octave_value arg_c = args(2);

  octave_idx_type a_nr = arg_a.rows ();
  octave_idx_type a_nc = arg_a.columns ();

  octave_idx_type b_nr = arg_b.rows ();
  octave_idx_type b_nc = arg_b.columns ();

  octave_idx_type c_nr = arg_c.rows ();
  octave_idx_type c_nc = arg_c.columns ();

  bool isfloat = arg_a.is_single_type ()
                 || arg_b.is_single_type ()
                 || arg_c.is_single_type ();

  if (arg_a.isempty () || arg_b.isempty () || arg_c.isempty ())
    {
      if (isfloat)
        return ovl (FloatMatrix ());
      else
        return ovl (Matrix ());
    }

  // Arguments are not empty, so check for correct dimensions.

  if (a_nr != a_nc)
    err_square_matrix_required ("sylvester", "A");
  if (b_nr != b_nc)
    err_square_matrix_required ("sylvester", "B");
  if (a_nr != c_nr || b_nr != c_nc)
    ::err_nonconformant ();

  if (isfloat)
    {
      if (arg_a.iscomplex ()
          || arg_b.iscomplex ()
          || arg_c.iscomplex ())
        {
          // Do everything in complex arithmetic;

          FloatComplexMatrix ca = arg_a.float_complex_matrix_value ();
          FloatComplexMatrix cb = arg_b.float_complex_matrix_value ();
          FloatComplexMatrix cc = arg_c.float_complex_matrix_value ();

          retval = Sylvester (ca, cb, cc);
        }
      else
        {
          // Do everything in real arithmetic.

          FloatMatrix ca = arg_a.float_matrix_value ();
          FloatMatrix cb = arg_b.float_matrix_value ();
          FloatMatrix cc = arg_c.float_matrix_value ();

          retval = Sylvester (ca, cb, cc);
        }
    }
  else
    {
      if (arg_a.iscomplex ()
          || arg_b.iscomplex ()
          || arg_c.iscomplex ())
        {
          // Do everything in complex arithmetic;

          ComplexMatrix ca = arg_a.complex_matrix_value ();
          ComplexMatrix cb = arg_b.complex_matrix_value ();
          ComplexMatrix cc = arg_c.complex_matrix_value ();

          retval = Sylvester (ca, cb, cc);
        }
      else
        {
          // Do everything in real arithmetic.

          Matrix ca = arg_a.matrix_value ();
          Matrix cb = arg_b.matrix_value ();
          Matrix cc = arg_c.matrix_value ();

          retval = Sylvester (ca, cb, cc);
        }
    }

  return retval;
}

/*
%!assert (sylvester ([1, 2; 3, 4], [5, 6; 7, 8], [9, 10; 11, 12]),
%!        [1/2, 2/3; 2/3, 1/2], sqrt (eps))
%!assert (sylvester (single ([1, 2; 3, 4]), single ([5, 6; 7, 8]), single ([9, 10; 11, 12])),
%!        single ([1/2, 2/3; 2/3, 1/2]), sqrt (eps ("single")))

## Test input validation
%!error sylvester ()
%!error sylvester (1)
%!error sylvester (1,2)
%!error sylvester (1, 2, 3, 4)
%!error <A must be a square matrix> sylvester (ones (2,3), ones (2,2), ones (2,2))
%!error <B must be a square matrix> sylvester (ones (2,2), ones (2,3), ones (2,2))
%!error <nonconformant matrices> sylvester (ones (2,2), ones (2,2), ones (3,3))
*/

OCTAVE_END_NAMESPACE(octave)
