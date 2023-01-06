////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2009-2023 The Octave Project Developers
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

#include "oct-norm.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"

OCTAVE_BEGIN_NAMESPACE(octave)

template <typename ColumnVector, typename Matrix, typename RowVector>
static void
do_mgorth (ColumnVector& x, const Matrix& V, RowVector& h)
{
  octave_idx_type Vc = V.columns ();
  h = RowVector (Vc + 1);
  for (octave_idx_type j = 0; j < Vc; j++)
    {
      ColumnVector Vcj = V.column (j);
      RowVector Vcjh = Vcj.hermitian ();
      h(j) = Vcjh * x;
      x -= h(j) * Vcj;
    }

  h(Vc) = xnorm (x);
  if (std::real (h(Vc)) > 0)
    x /= h(Vc);
}

DEFUN (mgorth, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {[@var{y}, @var{h}] =} mgorth (@var{x}, @var{v})
Orthogonalize a given column vector @var{x} with respect to a set of
orthonormal vectors comprising the columns of @var{v} using the modified
Gram-Schmidt method.

On exit, @var{y} is a unit vector such that:

@example
@group
  norm (@var{y}) = 1
  @var{v}' * @var{y} = 0
  @var{x} = [@var{v}, @var{y}]*@var{h}'
@end group
@end example

@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  octave_value arg_x = args(0);
  octave_value arg_v = args(1);

  if (arg_v.ndims () != 2 || arg_x.ndims () != 2 || arg_x.columns () != 1
      || arg_v.rows () != arg_x.rows ())
    error ("mgorth: V should be a matrix, and X a column vector with"
           " the same number of rows as V.");

  if (! arg_x.isnumeric () && ! arg_v.isnumeric ())
    error ("mgorth: X and V must be numeric");

  octave_value_list retval;

  bool iscomplex = (arg_x.iscomplex () || arg_v.iscomplex ());
  if (arg_x.is_single_type () || arg_v.is_single_type ())
    {
      if (iscomplex)
        {
          FloatComplexColumnVector x
            = arg_x.float_complex_column_vector_value ();
          FloatComplexMatrix V = arg_v.float_complex_matrix_value ();
          FloatComplexRowVector h;
          do_mgorth (x, V, h);
          retval = ovl (x, h);
        }
      else
        {
          FloatColumnVector x = arg_x.float_column_vector_value ();
          FloatMatrix V = arg_v.float_matrix_value ();
          FloatRowVector h;
          do_mgorth (x, V, h);
          retval = ovl (x, h);
        }
    }
  else
    {
      if (iscomplex)
        {
          ComplexColumnVector x = arg_x.complex_column_vector_value ();
          ComplexMatrix V = arg_v.complex_matrix_value ();
          ComplexRowVector h;
          do_mgorth (x, V, h);
          retval = ovl (x, h);
        }
      else
        {
          ColumnVector x = arg_x.column_vector_value ();
          Matrix V = arg_v.matrix_value ();
          RowVector h;
          do_mgorth (x, V, h);
          retval = ovl (x, h);
        }
    }

  return retval;
}

/*
%!test
%! for ii=1:100
%!   assert (abs (mgorth (randn (5, 1), eye (5, 4))), [0 0 0 0 1]', eps);
%! endfor

%!test
%! a = hilb (5);
%! a(:, 1) /= norm (a(:, 1));
%! for ii = 1:5
%!   a(:, ii) = mgorth (a(:, ii), a(:, 1:ii-1));
%! endfor
%! assert (a' * a, eye (5), 1e10);
*/

OCTAVE_END_NAMESPACE(octave)
