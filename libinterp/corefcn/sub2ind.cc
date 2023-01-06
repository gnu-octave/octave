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

#include <utility>

#include "Array-util.h"
#include "oct-locbuf.h"
#include "quit.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static dim_vector
get_dim_vector (const octave_value& val, const char *name)
{
  RowVector dimsv = val.row_vector_value (false, true);
  dim_vector dv;
  octave_idx_type n = dimsv.numel ();

  if (n < 1)
    error ("%s: dimension vector DIMS must not be empty", name);

  dv.resize (std::max (n, static_cast<octave_idx_type> (2)));
  dv(1) = 1;
  for (octave_idx_type i = 0; i < n; i++)
    {
      octave_idx_type ii = dimsv(i);
      if (ii == dimsv(i) && ii >= 0)
        dv(i) = ii;
      else
        error ("%s: dimension vector DIMS must contain integers", name);
    }

  return dv;
}

DEFUN (sub2ind, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{ind} =} sub2ind (@var{dims}, @var{i}, @var{j})
@deftypefnx {} {@var{ind} =} sub2ind (@var{dims}, @var{s1}, @var{s2}, @dots{}, @var{sN})
Convert subscripts to linear indices.

The input @var{dims} is a dimension vector where each element is the size of
the array in the respective dimension (@pxref{XREFsize,,@code{size}}).  The
remaining inputs are scalars or vectors of subscripts to be converted.

The output vector @var{ind} contains the converted linear indices.

Background: Array elements can be specified either by a linear index which
starts at 1 and runs through the number of elements in the array, or they may
be specified with subscripts for the row, column, page, etc.  The functions
@code{ind2sub} and @code{sub2ind} interconvert between the two forms.

The linear index traverses dimension 1 (rows), then dimension 2 (columns), then
dimension 3 (pages), etc.@: until it has numbered all of the elements.
Consider the following 3-by-3 matrices:

@example
@group
[(1,1), (1,2), (1,3)]     [1, 4, 7]
[(2,1), (2,2), (2,3)] ==> [2, 5, 8]
[(3,1), (3,2), (3,3)]     [3, 6, 9]
@end group
@end example

@noindent
The left matrix contains the subscript tuples for each matrix element.  The
right matrix shows the linear indices for the same matrix.

The following example shows how to convert the two-dimensional indices
@code{(2,1)} and @code{(2,3)} of a 3-by-3 matrix to linear indices with a
single call to @code{sub2ind}.

@example
@group
s1 = [2, 2];
s2 = [1, 3];
ind = sub2ind ([3, 3], s1, s2)
    @result{} ind =  2   8
@end group
@end example
@seealso{ind2sub, size}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2)
    print_usage ();

  dim_vector dv = get_dim_vector (args(0), "sub2ind");

  Array<idx_vector> idxa (dim_vector (nargin-1, 1));

  for (int j = 0; j < nargin - 1; j++)
    {
      if (! args(j+1).isnumeric ())
        error ("sub2ind: subscripts must be numeric");

      try
        {
          idxa(j) = args(j+1).index_vector ();

          if (j > 0 && args(j+1).dims () != args(1).dims ())
            error ("sub2ind: all subscripts must be of the same size");
        }
      catch (index_exception& ie)
        {
          ie.set_pos_if_unset (nargin-1, j+1);
          ie.set_var ();
          std::string msg = ie.message ();
          error_with_id (ie.err_id (), "%s", msg.c_str ());
        }
    }

  return ovl (sub2ind (dv, idxa));
}

/*
## Test evaluation
%!test
%! s1 = [ 1   1   1   1 ; 2   2   2   2 ];
%! s2 = [ 1   1   2   2 ; 1   1   2   2 ];
%! s3 = [ 1   2   1   2 ; 1   2   1   2 ];
%! in = [ 1 101  11 111 ; 2 102  12 112 ];
%! assert (sub2ind ([10 10 10], s1, s2, s3), in);

# Test low index
%!assert (sub2ind ([10 10 10], 1, 1, 1), 1)
%!error <index \(0,_,_\)> sub2ind ([10 10 10], 0, 1, 1)
%!error <index \(_,0,_\)> sub2ind ([10 10 10], 1, 0, 1)
%!error <index \(_,_,0\)> sub2ind ([10 10 10], 1, 1, 0)

# Test high index
%!assert (sub2ind ([10 10 10], 10, 10, 10), 1000)
%!error <index \(11,_,_\): out of bound 10> sub2ind ([10 10 10], 11, 10, 10)
%!error <index \(_,11,_\): out of bound 10> sub2ind ([10 10 10], 10, 11, 10)
%!error <index \(_,_,11\): out of bound 10> sub2ind ([10 10 10], 10, 10, 11)

# Test high index in the trailing dimensions
%!assert (sub2ind ([10, 1], 2, 1, 1), 2)
%!error <index \(_,2,_\): out of bound 1> sub2ind ([10, 1], 1, 2, 1)
%!error <index \(_,_,2\): out of bound 1> sub2ind ([10, 1], 1, 1, 2)
%!assert (sub2ind ([10 10], 2, 2, 1), 12)
%!error <index \(_,_,2\): out of bound 1> sub2ind ([10 10], 2, 1, 2)
%!error <index \(_,_,2\): out of bound 1> sub2ind ([10 10], 1, 2, 2)

# Test handling of empty arguments
%!assert (sub2ind ([10 10], zeros (0,0), zeros (0,0)), zeros (0,0))
%!assert (sub2ind ([10 10], zeros (2,0), zeros (2,0)), zeros (2,0))
%!assert (sub2ind ([10 10], zeros (0,2), zeros (0,2)), zeros (0,2))
%!error <all subscripts .* same size> sub2ind ([10 10 10], zeros (0,2), zeros (2,0))

# Test handling of arguments of different size
%!error <all subscripts .* same size> sub2ind ([10 10], ones (1,2), ones (1,3))
%!error <all subscripts .* same size> sub2ind ([10 10], ones (1,2), ones (2,1))

## Test input validation
%!error <dimension vector> sub2ind ([10 10.5], 1, 1)
%!error <index \(1.5,_\)> sub2ind ([10 10], 1.5, 1)
%!error <index \(_,1.5\)> sub2ind ([10 10], 1, 1.5)
*/

DEFUN (ind2sub, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn {} {[@var{s1}, @var{s2}, @dots{}, @var{sN}] =} ind2sub (@var{dims}, @var{ind})
Convert linear indices to subscripts.

The input @var{dims} is a dimension vector where each element is the size of
the array in the respective dimension (@pxref{XREFsize,,@code{size}}).  The
second input @var{ind} contains linear indices to be converted.

The outputs @var{s1}, @dots{}, @var{sN} contain the converted subscripts.

Background: Array elements can be specified either by a linear index which
starts at 1 and runs through the number of elements in the array, or they may
be specified with subscripts for the row, column, page, etc.  The functions
@code{ind2sub} and @code{sub2ind} interconvert between the two forms.

The linear index traverses dimension 1 (rows), then dimension 2 (columns), then
dimension 3 (pages), etc.@: until it has numbered all of the elements.
Consider the following 3-by-3 matrices:

@example
@group
[1, 4, 7]     [(1,1), (1,2), (1,3)]
[2, 5, 8] ==> [(2,1), (2,2), (2,3)]
[3, 6, 9]     [(3,1), (3,2), (3,3)]
@end group
@end example

@noindent
The left matrix contains the linear indices for each matrix element.  The right
matrix shows the subscript tuples for the same matrix.

The following example shows how to convert the linear indices @code{2} and
@code{8} to appropriate subscripts of a 3-by-3 matrix.

@example
@group
ind = [2, 8];
[r, c] = ind2sub ([3, 3], ind)
    @result{} r =  2   2
    @result{} c =  1   3
@end group
@end example

If the number of output subscripts exceeds the number of dimensions, the
exceeded dimensions are set to @code{1}.  On the other hand, if fewer
subscripts than dimensions are provided, the exceeding dimensions are merged
into the final requested dimension.  For clarity, consider the following
examples:

@example
@group
ind  = [2, 8];
dims = [3, 3];
## same as dims = [3, 3, 1]
[r, c, s] = ind2sub (dims, ind)
    @result{} r =  2   2
    @result{} c =  1   3
    @result{} s =  1   1
## same as dims = [9]
r = ind2sub (dims, ind)
    @result{} r =  2   8
@end group
@end example
@seealso{sub2ind, size}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  octave_value_list retval;

  int nd = (nargout == 0) ? 1 : nargout;

  dim_vector dv = get_dim_vector (args(0), "ind2sub").redim (nargout);

  // Redim for 1 will give us a column vector but we want a row vector.
  if (nd == 1)
    std::swap (dv(0), dv(1));

  try
    {
      retval = Array<octave_value> (ind2sub (dv, args(1).index_vector ()));

      if (nd == 1)
        retval(0) = retval(1);
    }
  catch (const index_exception& ie)
    {
      error ("ind2sub: invalid index %s", ie.what ());
    }

  return retval;
}

/*
## Examples
%!test
%! [r, c] = ind2sub ([3, 3], [2, 8]);
%! assert (r, [2, 2]);
%! assert (c, [1, 3]);

%!test
%! [r, c, s] = ind2sub ([3, 3], [2, 8]);
%! assert (r, [2, 2]);
%! assert (c, [1, 3]);
%! assert (s, [1, 1]);
%! [r, c, s] = ind2sub ([3, 3, 1], [2, 8]);
%! assert (r, [2, 2]);
%! assert (c, [1, 3]);
%! assert (s, [1, 1]);

%!test
%! r = ind2sub ([3, 3], [2, 8]);
%! assert (r, [2, 8]);
%! r = ind2sub (9, [2, 8]);
%! assert (r, [2, 8]);

## 3-dimensional test
%!test
%! [r, c, s] = ind2sub ([2, 2, 2], 1:8);
%! assert (r, [1, 2, 1, 2, 1, 2, 1, 2]);
%! assert (c, [1, 1, 2, 2, 1, 1, 2, 2]);
%! assert (s, [1, 1, 1, 1, 2, 2, 2, 2]);
%! [r, c] = ind2sub ([2, 2, 2], 1:8);
%! assert (r, [1, 2, 1, 2, 1, 2, 1, 2]);
%! assert (c, [1, 1, 2, 2, 3, 3, 4, 4]);
%! r = ind2sub ([2, 2, 2], 1:8);
%! assert (r, 1:8);

## Indexing beyond specified size (bug #62184)
%!assert <*62184> (ind2sub (1, 2), 2)
%!assert <*62184> (ind2sub ([3,3], 10), 10)
%!test <*62184>
%! [r,c] = ind2sub ([3,3], 10);
%! assert ([r, c], [1, 4]);
%!test <*62184>
%! [r,c,p] = ind2sub ([3,3], 10);
%! assert ([r, c, p], [1, 1, 2]);

## Test input validation
%!error <DIMS must contain integers> ind2sub ([2, -2], 3)
%!error <invalid index> ind2sub ([2, 2, 2], -1:8)
*/

OCTAVE_END_NAMESPACE(octave)
