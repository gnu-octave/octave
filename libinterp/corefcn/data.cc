////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1994-2023 The Octave Project Developers
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

#include <cmath>
#include <cstddef>
#include <cstdint>
#include <ctime>

#include <algorithm>
#include <limits>
#include <string>

#include "lo-ieee.h"
#include "mx-base.h"
#include "oct-base64.h"
#include "oct-binmap.h"
#include "oct-time.h"
#include "quit.h"

#include "Cell.h"
#include "data.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "interpreter-private.h"
#include "oct-map.h"
#include "ov-class.h"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-cx-sparse.h"
#include "ov-float.h"
#include "ov-flt-complex.h"
#include "ov-flt-cx-mat.h"
#include "ov.h"
#include "ovl.h"
#include "pager.h"
#include "parse.h"
#include "pt-mat.h"
#include "utils.h"
#include "variables.h"
#include "xnorm.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (all, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{tf} =} all (@var{x})
@deftypefnx {} {@var{tf} =} all (@var{x}, @var{dim})
For a vector argument, return true (logical 1) if all elements of the vector
are nonzero.

For a matrix argument, return a row vector of logical ones and zeros with each
element indicating whether all of the elements of the corresponding column of
the matrix are nonzero.  For example:

@example
@group
all ([2, 3; 1, 0])
    @result{} [ 1, 0 ]
@end group
@end example

If the optional argument @var{dim} is supplied, work along dimension @var{dim}.
@seealso{any}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  int dim = (nargin == 1 ? -1
             : args(1).xint_value ("all: DIM must be an integer")-1);

  if (dim < -1)
    error ("all: invalid dimension argument = %d", dim + 1);

  return ovl (args(0).all (dim));
}

/*
%!test
%! x = ones (3);
%! x(1,1) = 0;
%! assert (all (all (rand (3) + 1) == [1, 1, 1]) == 1);
%! assert (all (all (x) == [0, 1, 1]) == 1);
%! assert (all (x, 1) == [0, 1, 1]);
%! assert (all (x, 2) == [0; 1; 1]);

%!test
%! x = ones (3, "single");
%! x(1,1) = 0;
%! assert (all (all (single (rand (3) + 1)) == [1, 1, 1]) == 1);
%! assert (all (all (x) == [0, 1, 1]) == 1);
%! assert (all (x, 1) == [0, 1, 1]);
%! assert (all (x, 2) == [0; 1; 1]);

%!error all ()
%!error all (1, 2, 3)
*/

DEFUN (any, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{tf} =} any (@var{x})
@deftypefnx {} {@var{tf} =} any (@var{x}, @var{dim})
For a vector argument, return true (logical 1) if any element of the vector
is nonzero.

For a matrix argument, return a row vector of logical ones and zeros with each
element indicating whether any of the elements of the corresponding column of
the matrix are nonzero.  For example:

@example
@group
any (eye (2, 4))
 @result{} [ 1, 1, 0, 0 ]
@end group
@end example

If the optional argument @var{dim} is supplied, work along dimension @var{dim}.
For example:

@example
@group
any (eye (2, 4), 2)
 @result{} [ 1; 1 ]
@end group
@end example
@seealso{all}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  int dim = (nargin == 1 ? -1
             : args(1).xint_value ("any: DIM must be an integer")-1);

  if (dim < -1)
    error ("any: invalid dimension argument = %d", dim + 1);

  return ovl (args(0).any (dim));
}

/*
%!test
%! x = zeros (3);
%! x(3,3) = 1;
%! assert (all (any (x) == [0, 0, 1]) == 1);
%! assert (all (any (ones (3)) == [1, 1, 1]) == 1);
%! assert (any (x, 1) == [0, 0, 1]);
%! assert (any (x, 2) == [0; 0; 1]);

%!test
%! x = zeros (3, "single");
%! x(3,3) = 1;
%! assert (all (any (x) == [0, 0, 1]) == 1);
%! assert (all (any (ones (3, "single")) == [1, 1, 1]) == 1);
%! assert (any (x, 1) == [0, 0, 1]);
%! assert (any (x, 2) == [0; 0; 1]);

%!error any ()
%!error any (1, 2, 3)
*/

// These mapping functions may also be useful in other places, eh?

DEFUN (atan2, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{angle} =} atan2 (@var{y}, @var{x})
Compute atan (@var{y} / @var{x}) for corresponding elements of @var{y} and
@var{x}.

@var{y} and @var{x} must match in size and orientation.  The signs of elements
of @var{y} and @var{x} are used to determine the quadrants of each resulting
value.

This function is equivalent to @code{arg (complex (@var{x}, @var{y}))}.
@seealso{tan, tand, tanh, atanh}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  octave_value retval;

  if (! args(0).isnumeric ())
    err_wrong_type_arg ("atan2", args(0));

  if (! args(1).isnumeric ())
    err_wrong_type_arg ("atan2", args(1));

  if (args(0).iscomplex () || args(1).iscomplex ())
    error ("atan2: not defined for complex numbers");

  if (args(0).is_single_type () || args(1).is_single_type ())
    {
      if (args(0).is_scalar_type () && args(1).is_scalar_type ())
        retval = atan2f (args(0).float_value (), args(1).float_value ());
      else
        {
          FloatNDArray a0 = args(0).float_array_value ();
          FloatNDArray a1 = args(1).float_array_value ();
          retval = binmap<float> (a0, a1, std::atan2, "atan2");
        }
    }
  else
    {
      if (args(0).is_scalar_type () && args(1).is_scalar_type ())
        retval = atan2 (args(0).scalar_value (), args(1).scalar_value ());
      else if (args(0).issparse ())
        {
          SparseMatrix m0 = args(0).sparse_matrix_value ();
          SparseMatrix m1 = args(1).sparse_matrix_value ();
          retval = binmap<double> (m0, m1, std::atan2, "atan2");
        }
      else
        {
          NDArray a0 = args(0).array_value ();
          NDArray a1 = args(1).array_value ();
          retval = binmap<double> (a0, a1, std::atan2, "atan2");
        }
    }

  return retval;
}

/*
%!assert (size (atan2 (zeros (0, 2), zeros (0, 2))), [0, 2])
%!assert (size (atan2 (rand (2, 3, 4), zeros (2, 3, 4))), [2, 3, 4])
%!assert (size (atan2 (rand (2, 3, 4), 1)), [2, 3, 4])
%!assert (size (atan2 (1, rand (2, 3, 4))), [2, 3, 4])
%!assert (size (atan2 (1, 2)), [1, 1])

%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! v = [0, pi/6, pi/4, pi/3, -pi/3, -pi/4, -pi/6, 0];
%! y = [0, rt3, 1, rt3, -rt3, -1, -rt3, 0];
%! x = [1, 3, 1, 1, 1, 1, 3, 1];
%! assert (atan2 (y, x), v, sqrt (eps));

%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! v = single ([0, pi/6, pi/4, pi/3, -pi/3, -pi/4, -pi/6, 0]);
%! y = single ([0, rt3, 1, rt3, -rt3, -1, -rt3, 0]);
%! x = single ([1, 3, 1, 1, 1, 1, 3, 1]);
%! assert (atan2 (y, x), v, sqrt (eps ("single")));

## Test sparse implementations
%!shared xs
%! xs = sparse (0:3);
%!test
%! y = atan2 (1, xs);
%! assert (issparse (y), false);
%! assert (nnz (y), 4);
%! assert (y, atan2 (1, 0:3));
%!test
%! y = atan2 (0, xs);
%! assert (issparse (y), false);
%! assert (nnz (y), 0);
%! assert (y, zeros (1,4));
%!test
%! y = atan2 (xs, 1);
%! assert (issparse (y));
%! assert (nnz (y), 3);
%! assert (y, sparse (atan2 (0:3, 1)));
%!test
%! y = atan2 (xs, 0);
%! assert (issparse (y));
%! assert (nnz (y), 3);
%! assert (y, sparse (atan2 (0:3, 0)));
%!test
%! y = atan2 (xs, sparse (ones (1, 4)));
%! assert (issparse (y));
%! assert (nnz (y), 3);
%! assert (y, sparse (atan2 (0:3, ones (1,4))));
%!test
%! y = atan2 (xs, sparse (zeros (1,4)));
%! assert (issparse (y));
%! assert (nnz (y), 3);
%! assert (y, sparse (atan2 (0:3, zeros (1,4))));

%!error atan2 ()
%!error atan2 (1, 2, 3)
*/

static octave_value
do_hypot (const octave_value& x, const octave_value& y)
{
  octave_value retval;

  octave_value arg0 = x;
  octave_value arg1 = y;
  if (! arg0.isnumeric ())
    err_wrong_type_arg ("hypot", arg0);
  if (! arg1.isnumeric ())
    err_wrong_type_arg ("hypot", arg1);

  if (arg0.iscomplex ())
    arg0 = arg0.abs ();
  if (arg1.iscomplex ())
    arg1 = arg1.abs ();

  if (arg0.is_single_type () || arg1.is_single_type ())
    {
      if (arg0.is_scalar_type () && arg1.is_scalar_type ())
        retval = hypotf (arg0.float_value (), arg1.float_value ());
      else
        {
          FloatNDArray a0 = arg0.float_array_value ();
          FloatNDArray a1 = arg1.float_array_value ();
          retval = binmap<float> (a0, a1, std::hypot, "hypot");
        }
    }
  else
    {
      if (arg0.is_scalar_type () && arg1.is_scalar_type ())
        retval = hypot (arg0.scalar_value (), arg1.scalar_value ());
      else if (arg0.issparse () || arg1.issparse ())
        {
          SparseMatrix m0 = arg0.sparse_matrix_value ();
          SparseMatrix m1 = arg1.sparse_matrix_value ();
          retval = binmap<double> (m0, m1, std::hypot, "hypot");
        }
      else
        {
          NDArray a0 = arg0.array_value ();
          NDArray a1 = arg1.array_value ();
          retval = binmap<double> (a0, a1, std::hypot, "hypot");
        }
    }

  return retval;
}

DEFUN (hypot, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{h} =} hypot (@var{x}, @var{y})
@deftypefnx {} {@var{h} =} hypot (@var{x}, @var{y}, @var{z}, @dots{})
Compute the element-by-element square root of the sum of the squares of
@var{x} and @var{y}.

This is equivalent to
@code{sqrt (@var{x}.^2 + @var{y}.^2)}, but is calculated in a manner that
avoids overflows for large values of @var{x} or @var{y}.

@code{hypot} can also be called with more than 2 arguments; in this case,
the arguments are accumulated from left to right:

@example
@group
hypot (hypot (@var{x}, @var{y}), @var{z})
hypot (hypot (hypot (@var{x}, @var{y}), @var{z}), @var{w}), etc.
@end group
@end example
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2)
    print_usage ();

  octave_value retval;

  if (nargin == 2)
    retval = do_hypot (args(0), args(1));
  else
    {
      retval = args(0);

      for (int i = 1; i < nargin; i++)
        retval = do_hypot (retval, args(i));
    }

  return retval;
}

/*
%!assert (size (hypot (zeros (0, 2), zeros (0, 2))), [0, 2])
%!assert (size (hypot (rand (2, 3, 4), zeros (2, 3, 4))), [2, 3, 4])
%!assert (size (hypot (rand (2, 3, 4), 1)), [2, 3, 4])
%!assert (size (hypot (1, rand (2, 3, 4))), [2, 3, 4])
%!assert (size (hypot (1, 2)), [1, 1])
%!assert (hypot (1:10, 1:10), sqrt (2) * [1:10], 16*eps)
%!assert (hypot (single (1:10), single (1:10)), single (sqrt (2) * [1:10]))

## Test sparse implementations
%!shared xs
%! xs = sparse (0:3);
%!test
%! y = hypot (1, xs);
%! assert (nnz (y), 4);
%! assert (y, sparse (hypot (1, 0:3)));
%!test
%! y = hypot (0, xs);
%! assert (nnz (y), 3);
%! assert (y, xs);
%!test
%! y = hypot (xs, 1);
%! assert (nnz (y), 4);
%! assert (y, sparse (hypot (0:3, 1)));
%!test
%! y = hypot (xs, 0);
%! assert (nnz (y), 3);
%! assert (y, xs);
%!test
%! y = hypot (sparse ([0 0]), sparse ([0 1]));
%! assert (nnz (y), 1);
%! assert (y, sparse ([0 1]));
%!test
%! y = hypot (sparse ([0 1]), sparse ([0 0]));
%! assert (nnz (y), 1);
%! assert (y, sparse ([0 1]));

*/

template <typename T, typename ET>
void
map_2_xlog2 (const Array<T>& x, Array<T>& f, Array<ET>& e)
{
  f = Array<T>(x.dims ());
  e = Array<ET>(x.dims ());
  for (octave_idx_type i = 0; i < x.numel (); i++)
    {
      int exp;
      f.xelem (i) = math::log2 (x(i), exp);
      e.xelem (i) = exp;
    }
}

DEFUN (log2, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{y} =} log2 (@var{x})
@deftypefnx {} {[@var{f}, @var{e}] =} log2 (@var{x})
Compute the base-2 logarithm of each element of @var{x}.

If called with one output, compute the base-2 logarithm such that
@tex
$2^y = x$.
@end tex
@ifnottex
@code{2^@var{y} = @var{x}}.
@end ifnottex

If called with two output arguments, split @var{x} into binary mantissa
(@var{f}) and exponent (@var{e}) such that
@tex
$x = f \cdot 2^e$
@end tex
@ifnottex
@code{@var{x} = @var{f} * 2^@var{e}}
@end ifnottex
where
@tex
${1 \over 2} \le \left| f \right| < 1$
@end tex
@ifnottex
@w{@code{1/2 <= abs (@var{f}) < 1}}
@end ifnottex
and @var{e} is an integer.  If
@tex
$x = 0$, $f = e = 0$.
@end tex
@ifnottex
@w{@code{x = 0}}, @w{@code{f = e = 0}}.
@end ifnottex
@seealso{pow2, log, log10, exp}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  octave_value_list retval;

  if (nargout < 2)
    retval = ovl (args(0).log2 ());
  else if (args(0).is_single_type ())
    {
      if (args(0).isreal ())
        {
          FloatNDArray f;
          FloatNDArray x = args(0).float_array_value ();
          // FIXME: should E be an int value?
          FloatMatrix e;
          map_2_xlog2 (x, f, e);
          retval = ovl (f, e);
        }
      else if (args(0).iscomplex ())
        {
          FloatComplexNDArray f;
          FloatComplexNDArray x = args(0).float_complex_array_value ();
          // FIXME: should E be an int value?
          FloatNDArray e;
          map_2_xlog2 (x, f, e);
          retval = ovl (f, e);
        }
    }
  else if (args(0).isreal ())
    {
      NDArray f;
      NDArray x = args(0).array_value ();
      // FIXME: should E be an int value?
      Matrix e;
      map_2_xlog2 (x, f, e);
      retval = ovl (f, e);
    }
  else if (args(0).iscomplex ())
    {
      ComplexNDArray f;
      ComplexNDArray x = args(0).complex_array_value ();
      // FIXME: should E be an int value?
      NDArray e;
      map_2_xlog2 (x, f, e);
      retval = ovl (f, e);
    }
  else
    err_wrong_type_arg ("log2", args(0));

  return retval;
}

/*
%!assert (log2 ([1/4, 1/2, 1, 2, 4]), [-2, -1, 0, 1, 2])
%!assert (log2 (Inf), Inf)
%!assert (isnan (log2 (NaN)))
%!assert (log2 (4*i), 2 + log2 (1*i))
%!assert (log2 (complex (0,Inf)), Inf + log2 (i))

%!test
%! [f, e] = log2 ([0,-1; 2,-4; Inf,-Inf]);
%! assert (f, [0,-0.5; 0.5,-0.5; Inf,-Inf]);
%! assert (e(1:2,:), [0,1;2,3]);

%!test
%! [f, e] = log2 (complex (zeros (3, 2), [0,-1; 2,-4; Inf,-Inf]));
%! assert (f, complex (zeros (3, 2), [0,-0.5; 0.5,-0.5; Inf,-Inf]));
%! assert (e(1:2,:), [0,1; 2,3]);

%!assert <*42583> (all (log2 (pow2 (-1074:1023)) == -1074:1023))
*/

DEFUN (rem, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{r} =} rem (@var{x}, @var{y})
Return the remainder of the division @code{@var{x} / @var{y}}.

The remainder is computed using the expression

@example
x - y .* fix (x ./ y)
@end example

An error message is printed if the dimensions of the arguments do not agree,
or if either argument is complex.

Programming Notes: When calculating with floating point numbers (double,
single), values within a few eps of an integer will be rounded to that
integer before computation for compatibility with @sc{matlab}.  Any floating
point integers greater than @code{flintmax} (2^53 for double) will not compute
correctly.  For larger integer values convert the input to @code{uint64} before
calling this function.

By convention,

@example
@group
rem (@var{x}, 0) = NaN  if @var{x} is a floating point variable
rem (@var{x}, 0) = 0    if @var{x} is an integer variable
rem (@var{x}, @var{y})  returns a value with the signbit from @var{x}
@end group
@end example

For the opposite conventions see the @code{mod} function.  In general,
@code{rem} is best when computing the remainder after division of two
@emph{positive} numbers.  For negative numbers, or when the values are
periodic, @code{mod} is a better choice.
@seealso{mod}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  octave_value retval;

  if (! args(0).isnumeric ())
    err_wrong_type_arg ("rem", args(0));

  if (! args(1).isnumeric ())
    err_wrong_type_arg ("rem", args(1));

  if (args(0).iscomplex () || args(1).iscomplex ())
    error ("rem: not defined for complex numbers");

  if (args(0).isinteger () || args(1).isinteger ())
    {
      builtin_type_t btyp0 = args(0).builtin_type ();
      builtin_type_t btyp1 = args(1).builtin_type ();
      if (btyp0 == btyp_double || btyp0 == btyp_float)
        btyp0 = btyp1;
      if (btyp1 == btyp_double || btyp1 == btyp_float)
        btyp1 = btyp0;

      if (btyp0 != btyp1)
        error ("rem: cannot combine %s and %s",
               args(0).class_name ().c_str (),
               args(1).class_name ().c_str ());

      switch (btyp0)
        {
#define MAKE_INT_BRANCH(X)                                              \
          case btyp_ ## X:                                              \
            {                                                           \
              X##NDArray a0 = args(0).X##_array_value ();               \
              X##NDArray a1 = args(1).X##_array_value ();               \
              retval = binmap<octave_##X,octave_##X,octave_##X> (a0, a1, rem, "rem"); \
            }                                                           \
            break

          MAKE_INT_BRANCH (int8);
          MAKE_INT_BRANCH (int16);
          MAKE_INT_BRANCH (int32);
          MAKE_INT_BRANCH (int64);
          MAKE_INT_BRANCH (uint8);
          MAKE_INT_BRANCH (uint16);
          MAKE_INT_BRANCH (uint32);
          MAKE_INT_BRANCH (uint64);

#undef MAKE_INT_BRANCH

        default:
          panic_impossible ();
        }
    }
  else if (args(0).is_single_type () || args(1).is_single_type ())
    {
      if (args(0).is_scalar_type () && args(1).is_scalar_type ())
        retval = math::rem (args(0).float_value (), args(1).float_value ());
      else
        {
          FloatNDArray a0 = args(0).float_array_value ();
          FloatNDArray a1 = args(1).float_array_value ();
          retval = binmap<float> (a0, a1, math::rem<float>, "rem");
        }
    }
  else
    {
      if (args(0).is_scalar_type () && args(1).is_scalar_type ())
        retval = math::rem (args(0).scalar_value (), args(1).scalar_value ());
      else if (args(0).issparse () || args(1).issparse ())
        {
          SparseMatrix m0 = args(0).sparse_matrix_value ();
          SparseMatrix m1 = args(1).sparse_matrix_value ();
          retval = binmap<double> (m0, m1, math::rem<double>, "rem");
        }
      else
        {
          NDArray a0 = args(0).array_value ();
          NDArray a1 = args(1).array_value ();
          retval = binmap<double> (a0, a1, math::rem<double>, "rem");
        }
    }

  return retval;
}

/*
%!assert (size (rem (zeros (0, 2), zeros (0, 2))), [0, 2])
%!assert (size (rem (rand (2, 3, 4), zeros (2, 3, 4))), [2, 3, 4])
%!assert (size (rem (rand (2, 3, 4), 1)), [2, 3, 4])
%!assert (size (rem (1, rand (2, 3, 4))), [2, 3, 4])
%!assert (size (rem (1, 2)), [1, 1])

%!assert (rem ([1, 2, 3; -1, -2, -3], 2), [1, 0, 1; -1, 0, -1])
%!assert (rem ([1, 2, 3; -1, -2, -3], 2 * ones (2, 3)),[1, 0, 1; -1, 0, -1])
%!assert (rem ([0, 1, 2], [0, 0, 1]), [NaN, NaN, 0])
%!assert (rem (uint8 ([1, 2, 3; -1, -2, -3]), uint8 (2)),
%!        uint8 ([1, 0, 1; -1, 0, -1]))
%!assert (uint8 (rem ([1, 2, 3; -1, -2, -3], 2 * ones (2, 3))),
%!        uint8 ([1, 0, 1; -1, 0, -1]))
%!assert (rem (uint8 ([0, 1, 2]), [0, 0, 1]), uint8 ([0, 0, 0]))

## Test sparse implementations
%!shared xs
%! xs = sparse (0:3);
%!test
%! y = rem (11, xs);
%! assert (isnan (y(1)));
%! assert (y, sparse (rem (11, 0:3)));
%!test
%! y = rem (0, xs);
%! assert (nnz (y), 1);
%! assert (y, sparse ([NaN 0 0 0]));
%!test
%! y = rem (xs, 2);
%! assert (nnz (y), 2);
%! assert (y, sparse (rem (0:3, 2)));
%!test
%! y = rem (xs, 1);
%! assert (nnz (y), 0);
%! assert (y, sparse (rem (0:3, 1)));
%!test
%! y = rem (sparse ([11 11 11 11]), xs);
%! assert (nnz (y), 3);
%! assert (y, sparse (rem (11, 0:3)));
%!test
%! y = rem (sparse ([0 0 0 0]), xs);
%! assert (nnz (y), 1);
%! assert (y, sparse ([NaN 0 0 0]));

%!assert <*45587> (signbit (rem (-0, 1)))
%!assert <*45587> (! signbit (rem (0, 1)))

%!assert <*42627> (rem (0.94, 0.01), 0.0)

%!error rem (uint (8), int8 (5))
%!error rem (uint8 ([1, 2]), uint8 ([3, 4, 5]))
%!error rem ()
%!error rem (1, 2, 3)
%!error rem ([1, 2], [3, 4, 5])
%!error rem (i, 1)
*/

DEFUN (mod, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{m} =} mod (@var{x}, @var{y})
Compute the modulo of @var{x} and @var{y}.

Conceptually this is given by

@example
x - y .* floor (x ./ y)
@end example

@noindent
and is written such that the correct modulus is returned for integer types.
This function handles negative values correctly.  That is,
@w{@code{mod (-1, 3)}} is 2, not -1, as @w{@code{rem (-1, 3)}} returns.

An error results if the dimensions of the arguments do not agree, or if
either of the arguments is complex.

Programming Notes: When calculating with floating point numbers (double,
single), values within a few eps of an integer will be rounded to that
integer before computation for compatibility with @sc{matlab}.  Any floating
point integers greater than @code{flintmax} (2^53 for double) will not compute
correctly.  For larger integer values convert the input to @code{uint64} before
calling this function.

By convention,

@example
@group
mod (@var{x}, 0) = @var{x}
mod (@var{x}, @var{y})      returns a value with the signbit from @var{y}
@end group
@end example

For the opposite conventions see the @code{rem} function.  In general,
@code{mod} is a better choice than @code{rem} when any of the inputs are
negative numbers or when the values are periodic.
@seealso{rem}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  octave_value retval;

  if (! args(0).isnumeric ())
    err_wrong_type_arg ("mod", args(0));

  if (! args(1).isnumeric ())
    err_wrong_type_arg ("mod", args(1));

  if (args(0).iscomplex () || args(1).iscomplex ())
    error ("mod: not defined for complex numbers");

  if (args(0).isinteger () || args(1).isinteger ())
    {
      builtin_type_t btyp0 = args(0).builtin_type ();
      builtin_type_t btyp1 = args(1).builtin_type ();
      if (btyp0 == btyp_double || btyp0 == btyp_float)
        btyp0 = btyp1;
      if (btyp1 == btyp_double || btyp1 == btyp_float)
        btyp1 = btyp0;

      if (btyp0 != btyp1)
        error ("mod: cannot combine %s and %s",
               args(0).class_name ().c_str (),
               args(1).class_name ().c_str ());

      switch (btyp0)
        {
#define MAKE_INT_BRANCH(X)                                              \
          case btyp_ ## X:                                              \
            {                                                           \
              X##NDArray a0 = args(0).X##_array_value ();               \
              X##NDArray a1 = args(1).X##_array_value ();               \
              retval = binmap<octave_##X,octave_##X,octave_##X> (a0, a1, mod, "mod"); \
            }                                                           \
            break

          MAKE_INT_BRANCH (int8);
          MAKE_INT_BRANCH (int16);
          MAKE_INT_BRANCH (int32);
          MAKE_INT_BRANCH (int64);
          MAKE_INT_BRANCH (uint8);
          MAKE_INT_BRANCH (uint16);
          MAKE_INT_BRANCH (uint32);
          MAKE_INT_BRANCH (uint64);

#undef MAKE_INT_BRANCH

        default:
          panic_impossible ();
        }
    }
  else if (args(0).is_single_type () || args(1).is_single_type ())
    {
      if (args(0).is_scalar_type () && args(1).is_scalar_type ())
        retval = math::mod (args(0).float_value (), args(1).float_value ());
      else
        {
          FloatNDArray a0 = args(0).float_array_value ();
          FloatNDArray a1 = args(1).float_array_value ();
          retval = binmap<float> (a0, a1, math::mod<float>, "mod");
        }
    }
  else
    {
      if (args(0).is_scalar_type () && args(1).is_scalar_type ())
        retval = math::mod (args(0).scalar_value (), args(1).scalar_value ());
      else if (args(0).issparse () || args(1).issparse ())
        {
          SparseMatrix m0 = args(0).sparse_matrix_value ();
          SparseMatrix m1 = args(1).sparse_matrix_value ();
          retval = binmap<double> (m0, m1, math::mod<double>, "mod");
        }
      else
        {
          NDArray a0 = args(0).array_value ();
          NDArray a1 = args(1).array_value ();
          retval = binmap<double> (a0, a1, math::mod<double>, "mod");
        }
    }

  return retval;
}

/*
## empty input test
%!assert (isempty (mod ([], [])))

## x mod y, y != 0 tests
%!assert (mod (5, 3), 2)
%!assert (mod (-5, 3), 1)
%!assert (mod (0, 3), 0)
%!assert (mod ([-5, 5, 0], [3, 3, 3]), [1, 2, 0])
%!assert (mod ([-5; 5; 0], [3; 3; 3]), [1; 2; 0])
%!assert (mod ([-5, 5; 0, 3], [3, 3 ; 3, 1]), [1, 2 ; 0, 0])

## x mod 0 tests
%!assert (mod (5, 0), 5)
%!assert (mod (-5, 0), -5)
%!assert (mod ([-5, 5, 0], [3, 0, 3]), [1, 5, 0])
%!assert (mod ([-5; 5; 0], [3; 0; 3]), [1; 5; 0])
%!assert (mod ([-5, 5; 0, 3], [3, 0 ; 3, 1]), [1, 5 ; 0, 0])
%!assert (mod ([-5, 5; 0, 3], [0, 0 ; 0, 0]), [-5, 5; 0, 3])

## mixed scalar/matrix tests
%!assert (mod ([-5, 5; 0, 3], 0), [-5, 5; 0, 3])
%!assert (mod ([-5, 5; 0, 3], 3), [1, 2; 0, 0])
%!assert (mod (-5, [0,0; 0,0]), [-5, -5; -5, -5])
%!assert (mod (-5, [3,0; 3,1]), [1, -5; 1, 0])
%!assert (mod (-5, [3,2; 3,1]), [1, 1; 1, 0])

## integer types
%!assert (mod (uint8 (5), uint8 (4)), uint8 (1))
%!assert (mod (uint8 ([1:5]), uint8 (4)), uint8 ([1,2,3,0,1]))
%!assert (mod (uint8 ([1:5]), uint8 (0)), uint8 ([1:5]))
%!error mod (uint8 (5), int8 (4))

## mixed integer/real types
%!assert (mod (uint8 (5), 4), uint8 (1))
%!assert (mod (5, uint8 (4)), uint8 (1))
%!assert (mod (uint8 ([1:5]), 4), uint8 ([1,2,3,0,1]))

## non-integer real numbers
%!assert (mod (2.1, 0.1), 0)
%!assert (mod (2.1, 0.2), 0.1, eps)

%!assert <*45587> (signbit (mod (-0, 0)))
%!assert <*45587> (! signbit (mod (0, -0)))

%!assert <*42627> (mod (0.94, 0.01), 0.0)

%!assert <*54602> (mod (int8 (125), int8 (-25)), int8 (0))
%!assert <*54602> (mod (int8 (-125), int8 (-25)), int8 (0))
%!assert <*54602> (mod (int8 (-125), int8 (0)), int8 (-125))
%!assert <*54602> (mod (int8 (0), int8 (-25)), int8 (0))

*/

#define DATA_REDUCTION(FCN)                                             \
                                                                        \
  int nargin = args.length ();                                          \
                                                                        \
  if (nargin < 1 || nargin > 2)                                         \
    print_usage ();                                                     \
                                                                        \
  octave_value retval;                                                  \
                                                                        \
  octave_value arg = args(0);                                           \
                                                                        \
  int dim = (nargin == 1 ? -1 : args(1).int_value (true) - 1);          \
                                                                        \
  if (dim < -1)                                                         \
    error (#FCN ": invalid dimension argument = %d", dim + 1);          \
                                                                        \
  if (arg.isreal ())                                              \
    {                                                                   \
      if (arg.issparse ())                                        \
        {                                                               \
          SparseMatrix tmp = arg.sparse_matrix_value ();                \
                                                                        \
          retval = tmp.FCN (dim);                                       \
        }                                                               \
      else if (arg.is_single_type ())                                   \
        {                                                               \
          FloatNDArray tmp = arg.float_array_value ();                  \
                                                                        \
          retval = tmp.FCN (dim);                                       \
        }                                                               \
      else                                                              \
        {                                                               \
          NDArray tmp = arg.array_value ();                             \
                                                                        \
          retval = tmp.FCN (dim);                                       \
        }                                                               \
    }                                                                   \
  else if (arg.iscomplex ())                                      \
    {                                                                   \
      if (arg.issparse ())                                        \
        {                                                               \
          SparseComplexMatrix tmp = arg.sparse_complex_matrix_value (); \
                                                                        \
          retval = tmp.FCN (dim);                                       \
        }                                                               \
      else if (arg.is_single_type ())                                   \
        {                                                               \
          FloatComplexNDArray tmp                                       \
            = arg.float_complex_array_value ();                         \
                                                                        \
          retval = tmp.FCN (dim);                                       \
        }                                                               \
      else                                                              \
        {                                                               \
          ComplexNDArray tmp = arg.complex_array_value ();              \
                                                                        \
          retval = tmp.FCN (dim);                                       \
        }                                                               \
    }                                                                   \
  else                                                                  \
    err_wrong_type_arg (#FCN, arg);                                     \
                                                                        \
  return retval

DEFUN (cumprod, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{y} =} cumprod (@var{x})
@deftypefnx {} {@var{y} =} cumprod (@var{x}, @var{dim})
Cumulative product of elements along dimension @var{dim}.

If @var{dim} is omitted, it defaults to the first non-singleton dimension.
For example:

@example
@group
cumprod ([1, 2; 3, 4; 5, 6])
   @result{}  1   2
       3   8
      15  48
@end group
@end example
@seealso{prod, cumsum}
@end deftypefn */)
{
  DATA_REDUCTION (cumprod);
}

/*
%!assert (cumprod ([1, 2, 3]), [1, 2, 6])
%!assert (cumprod ([-1; -2; -3]), [-1; 2; -6])
%!assert (cumprod ([i, 2+i, -3+2i, 4]), [i, -1+2i, -1-8i, -4-32i])
%!assert (cumprod ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i]),
%!        [1, 2, 3; i, 4i, 9i; -1+i, -8+8i, -27+27i])

%!assert (cumprod (single ([1, 2, 3])), single ([1, 2, 6]))
%!assert (cumprod (single ([-1; -2; -3])), single ([-1; 2; -6]))
%!assert (cumprod (single ([i, 2+i, -3+2i, 4])),
%!        single ([i, -1+2i, -1-8i, -4-32i]))
%!assert (cumprod (single ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i])),
%!        single ([1, 2, 3; i, 4i, 9i; -1+i, -8+8i, -27+27i]))

%!assert (cumprod ([2, 3; 4, 5], 1), [2, 3; 8, 15])
%!assert (cumprod ([2, 3; 4, 5], 2), [2, 6; 4, 20])

%!assert (cumprod (single ([2, 3; 4, 5]), 1), single ([2, 3; 8, 15]))
%!assert (cumprod (single ([2, 3; 4, 5]), 2), single ([2, 6; 4, 20]))

%!error cumprod ()
*/

DEFUN (cumsum, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{y} =} cumsum (@var{x})
@deftypefnx {} {@var{y} =} cumsum (@var{x}, @var{dim})
@deftypefnx {} {@var{y} =} cumsum (@dots{}, "native")
@deftypefnx {} {@var{y} =} cumsum (@dots{}, "double")
Cumulative sum of elements along dimension @var{dim}.

If @var{dim} is omitted, it defaults to the first non-singleton dimension.
For example:

@example
@group
cumsum ([1, 2; 3, 4; 5, 6])
   @result{}  1   2
       4   6
       9  12
@end group
@end example

For an explanation of the optional parameters @qcode{"native"} and
@qcode{"double"}, @pxref{XREFsum,,@code{sum}}.
@seealso{sum, cumprod}
@end deftypefn */)
{
  int nargin = args.length ();

  bool isnative = false;
  bool isdouble = false;

  if (nargin > 1 && args(nargin - 1).is_string ())
    {
      std::string str = args(nargin - 1).string_value ();

      if (str == "native")
        isnative = true;
      else if (str == "double")
        isdouble = true;
      else
        error ("cumsum: unrecognized string argument");

      nargin--;
    }

  if (nargin < 1 || nargin > 2)
    print_usage ();

  int dim = -1;
  if (nargin == 2)
    {
      dim = args(1).int_value () - 1;
      if (dim < 0)
        error ("cumsum: invalid dimension argument = %d", dim + 1);
    }

  octave_value retval;
  octave_value arg = args(0);

  switch (arg.builtin_type ())
    {
    case btyp_double:
      if (arg.issparse ())
        retval = arg.sparse_matrix_value ().cumsum (dim);
      else
        retval = arg.array_value ().cumsum (dim);
      break;
    case btyp_complex:
      if (arg.issparse ())
        retval = arg.sparse_complex_matrix_value ().cumsum (dim);
      else
        retval = arg.complex_array_value ().cumsum (dim);
      break;
    case btyp_float:
      if (isdouble)
        retval = arg.array_value ().cumsum (dim);
      else
        retval = arg.float_array_value ().cumsum (dim);
      break;
    case btyp_float_complex:
      if (isdouble)
        retval = arg.complex_array_value ().cumsum (dim);
      else
        retval = arg.float_complex_array_value ().cumsum (dim);
      break;

#define MAKE_INT_BRANCH(X)                                      \
      case btyp_ ## X:                                          \
        if (isnative)                                           \
          retval = arg.X ## _array_value ().cumsum (dim);       \
        else                                                    \
          retval = arg.array_value ().cumsum (dim);             \
        break;

      MAKE_INT_BRANCH (int8);
      MAKE_INT_BRANCH (int16);
      MAKE_INT_BRANCH (int32);
      MAKE_INT_BRANCH (int64);
      MAKE_INT_BRANCH (uint8);
      MAKE_INT_BRANCH (uint16);
      MAKE_INT_BRANCH (uint32);
      MAKE_INT_BRANCH (uint64);

#undef MAKE_INT_BRANCH

    case btyp_bool:
      if (arg.issparse ())
        {
          SparseMatrix cs = arg.sparse_matrix_value ().cumsum (dim);
          if (isnative)
            retval = (cs != 0.0);
          else
            retval = cs;
        }
      else
        {
          NDArray cs = arg.array_value ().cumsum (dim);
          if (isnative)
            retval = (cs != 0.0);
          else
            retval = cs;
        }
      break;

    default:
      err_wrong_type_arg ("cumsum", arg);
    }

  return retval;
}

/*
%!assert (cumsum ([1, 2, 3]), [1, 3, 6])
%!assert (cumsum ([-1; -2; -3]), [-1; -3; -6])
%!assert (cumsum ([i, 2+i, -3+2i, 4]), [i, 2+2i, -1+4i, 3+4i])
%!assert (cumsum ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i]),
%!        [1, 2, 3; 1+i, 2+2i, 3+3i; 2+2i, 4+4i, 6+6i])

%!assert (cumsum (single ([1, 2, 3])), single ([1, 3, 6]))
%!assert (cumsum (single ([-1; -2; -3])), single ([-1; -3; -6]))
%!assert (cumsum (single ([i, 2+i, -3+2i, 4])),
%!        single ([i, 2+2i, -1+4i, 3+4i]))
%!assert (cumsum (single ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i])),
%!        single ([1, 2, 3; 1+i, 2+2i, 3+3i; 2+2i, 4+4i, 6+6i]))

%!assert (cumsum ([1, 2; 3, 4], 1), [1, 2; 4, 6])
%!assert (cumsum ([1, 2; 3, 4], 2), [1, 3; 3, 7])

%!assert (cumsum (single ([1, 2; 3, 4]), 1), single ([1, 2; 4, 6]))
%!assert (cumsum (single ([1, 2; 3, 4]), 2), single ([1, 3; 3, 7]))

%!error cumsum ()
*/

DEFUN (diag, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{M} =} diag (@var{v})
@deftypefnx {} {@var{M} =} diag (@var{v}, @var{k})
@deftypefnx {} {@var{M} =} diag (@var{v}, @var{m}, @var{n})
@deftypefnx {} {@var{v} =} diag (@var{M})
@deftypefnx {} {@var{v} =} diag (@var{M}, @var{k})
Return a diagonal matrix with vector @var{v} on diagonal @var{k}.

The second argument is optional.  If it is positive, the vector is placed on
the @var{k}-th superdiagonal.  If it is negative, it is placed on the
@var{-k}-th subdiagonal.  The default value of @var{k} is 0, and the vector
is placed on the main diagonal.  For example:

@example
@group
diag ([1, 2, 3], 1)
   @result{}  0  1  0  0
       0  0  2  0
       0  0  0  3
       0  0  0  0
@end group
@end example

@noindent
The 3-input form returns a diagonal matrix with vector @var{v} on the main
diagonal and the resulting matrix being of size @var{m} rows x @var{n}
columns.

Given a matrix argument, instead of a vector, @code{diag} extracts the
@var{k}-th diagonal of the matrix.
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    print_usage ();

  octave_value retval;

  if (nargin == 1)
    retval = args(0).diag ();
  else if (nargin == 2)
    {
      octave_idx_type k = args(1).xidx_type_value ("diag: invalid argument K");

      retval = args(0).diag (k);
    }
  else
    {
      octave_value arg0 = args(0);

      if (arg0.ndims () != 2 || (arg0.rows () != 1 && arg0.columns () != 1))
        error ("diag: V must be a vector");

      octave_idx_type m = args(1).xidx_type_value ("diag: invalid dimension M");
      octave_idx_type n = args(2).xidx_type_value ("diag: invalid dimension N");

      retval = arg0.diag (m, n);
    }

  return retval;
}

/*

%!assert (full (diag ([1; 2; 3])), [1, 0, 0; 0, 2, 0; 0, 0, 3])
%!assert (diag ([1; 2; 3], 1), [0, 1, 0, 0; 0, 0, 2, 0; 0, 0, 0, 3; 0, 0, 0, 0])
%!assert (diag ([1; 2; 3], 2),
%!        [0 0 1 0 0; 0 0 0 2 0; 0 0 0 0 3; 0 0 0 0 0; 0 0 0 0 0])
%!assert (diag ([1; 2; 3],-1),
%!       [0 0 0 0; 1 0 0 0; 0 2 0 0; 0 0 3 0])
%!assert (diag ([1; 2; 3],-2),
%!        [0 0 0 0 0; 0 0 0 0 0; 1 0 0 0 0; 0 2 0 0 0; 0 0 3 0 0])

%!assert (diag ([1, 0, 0; 0, 2, 0; 0, 0, 3]), [1; 2; 3])
%!assert (diag ([0, 1, 0, 0; 0, 0, 2, 0; 0, 0, 0, 3; 0, 0, 0, 0], 1),
%!        [1; 2; 3])
%!assert (diag ([0, 0, 0, 0; 1, 0, 0, 0; 0, 2, 0, 0; 0, 0, 3, 0], -1),
%!        [1; 2; 3])
%!assert (diag (ones (1, 0), 2), zeros (2))
%!assert (diag (1:3, 4, 2), [1, 0; 0, 2; 0, 0; 0, 0])

%!assert (full (diag (single ([1; 2; 3]))),
%!        single ([1, 0, 0; 0, 2, 0; 0, 0, 3]))
%!assert (diag (single ([1; 2; 3]), 1),
%!        single ([0, 1, 0, 0; 0, 0, 2, 0; 0, 0, 0, 3; 0, 0, 0, 0]))
%!assert (diag (single ([1; 2; 3]), 2),
%!        single ([0 0 1 0 0; 0 0 0 2 0; 0 0 0 0 3; 0 0 0 0 0; 0 0 0 0 0]))
%!assert (diag (single ([1; 2; 3]),-1),
%!        single ([0, 0, 0, 0; 1, 0, 0, 0; 0, 2, 0, 0; 0, 0, 3, 0]))
%!assert (diag (single ([1; 2; 3]),-2),
%!        single ([0 0 0 0 0; 0 0 0 0 0; 1 0 0 0 0; 0 2 0 0 0; 0 0 3 0 0]))

%!assert (diag (single ([1, 0, 0; 0, 2, 0; 0, 0, 3])), single ([1; 2; 3]))
%!assert (diag (single ([0, 1, 0, 0; 0, 0, 2, 0; 0, 0, 0, 3; 0, 0, 0, 0]), 1),
%!        single ([1; 2; 3]))
%!assert (diag (single ([0, 0, 0, 0; 1, 0, 0, 0; 0, 2, 0, 0; 0, 0, 3, 0]), -1),
%!        single ([1; 2; 3]))

%!assert (diag (int8 ([1; 2; 3])), int8 ([1, 0, 0; 0, 2, 0; 0, 0, 3]))
%!assert (diag (int8 ([1; 2; 3]), 1),
%!        int8 ([0, 1, 0, 0; 0, 0, 2, 0; 0, 0, 0, 3; 0, 0, 0, 0]))
%!assert (diag (int8 ([1; 2; 3]), 2),
%!        int8 ([0 0 1 0 0; 0 0 0 2 0; 0 0 0 0 3; 0 0 0 0 0; 0 0 0 0 0]))
%!assert (diag (int8 ([1; 2; 3]),-1),
%!        int8 ([0 0 0 0; 1 0 0 0; 0 2 0 0; 0 0 3 0]))
%!assert (diag (int8 ([1; 2; 3]),-2),
%!        int8 ([0 0 0 0 0; 0 0 0 0 0; 1 0 0 0 0; 0 2 0 0 0; 0 0 3 0 0]))

%!assert (diag (int8 ([1, 0, 0; 0, 2, 0; 0, 0, 3])), int8 ([1; 2; 3]))
%!assert (diag (int8 ([0, 1, 0, 0; 0, 0, 2, 0; 0, 0, 0, 3; 0, 0, 0, 0]), 1),
%!        int8 ([1; 2; 3]))
%!assert (diag (int8 ([0, 0, 0, 0; 1, 0, 0, 0; 0, 2, 0, 0; 0, 0, 3, 0]), -1),
%!        int8 ([1; 2; 3]))

%!assert (diag (1, 3, 3), diag ([1, 0, 0]))
%!assert (diag (i, 3, 3), diag ([i, 0, 0]))
%!assert (diag (single (1), 3, 3), diag ([single(1), 0, 0]))
%!assert (diag (single (i), 3, 3), diag ([single(i), 0, 0]))
%!assert (diag ([1, 2], 3, 3), diag ([1, 2, 0]))
%!assert (diag ([1, 2]*i, 3, 3), diag ([1, 2, 0]*i))
%!assert (diag (single ([1, 2]), 3, 3), diag (single ([1, 2, 0])))
%!assert (diag (single ([1, 2]*i), 3, 3), diag (single ([1, 2, 0]*i)))

%!assert <*37411> (diag (diag ([5, 2, 3])(:,1)), diag([5 0 0 ]))
%!assert <*37411> (diag (diag ([5, 2, 3])(:,1), 2),  [0 0 5 0 0; zeros(4, 5)])
%!assert <*37411> (diag (diag ([5, 2, 3])(:,1), -2),
%!                 [[0 0 5 0 0]', zeros(5, 4)])

## Test non-square size
%!assert (diag ([1,2,3], 6, 3), [1 0 0; 0 2 0; 0 0 3; 0 0 0; 0 0 0; 0 0 0])
%!assert (diag (1, 2, 3), [1,0,0; 0,0,0])
%!assert (diag ({1}, 2, 3), {1,[],[]; [],[],[]})
%!assert (diag ({1,2}, 3, 4), {1,[],[],[]; [],2,[],[]; [],[],[],[]})
%!assert <*56711> (diag ({1,2,3}, 2, 1), {1; []})

## Test out-of-range diagonals
%!assert (diag (ones (3,3), 4), zeros (0, 1))
%!assert (diag (cell (3,3), 4), cell (0, 1))
%!assert (diag (sparse (ones (3,3)), 4), sparse (zeros (0, 1)))

## Test input validation
%!error <Invalid call to diag> diag ()
%!error <Invalid call to diag> diag (1,2,3,4)
%!error <V must be a vector> diag (ones (2), 3, 3)
%!error diag (1:3, -4, 3)
%!error diag (1:3, 4, -3)

*/

DEFUN (prod, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{y} =} prod (@var{x})
@deftypefnx {} {@var{y} =} prod (@var{x}, @var{dim})
@deftypefnx {} {@var{y} =} prod (@dots{}, "native")
@deftypefnx {} {@var{y} =} prod (@dots{}, "double")
Product of elements along dimension @var{dim}.

If @var{dim} is omitted, it defaults to the first non-singleton dimension.

The optional @qcode{"type"} input determines the class of the variable
used for calculations.  If the argument @qcode{"native"} is given, then
the operation is performed in the same type as the original argument, rather
than the default double type.

For example:

@example
@group
prod ([true, true])
   @result{} 1
prod ([true, true], "native")
   @result{} true
@end group
@end example

On the contrary, if @qcode{"double"} is given, the operation is performed
in double precision even for single precision inputs.
@seealso{cumprod, sum}
@end deftypefn */)
{
  int nargin = args.length ();

  bool isnative = false;
  bool isdouble = false;

  if (nargin > 1 && args(nargin - 1).is_string ())
    {
      std::string str = args(nargin - 1).string_value ();

      if (str == "native")
        isnative = true;
      else if (str == "double")
        isdouble = true;
      else
        error ("prod: unrecognized type argument '%s'", str.c_str ());

      nargin--;
    }

  if (nargin < 1 || nargin > 2)
    print_usage ();

  octave_value retval;

  octave_value arg = args(0);

  int dim = -1;
  if (nargin == 2)
    {
      dim = args(1).int_value () - 1;
      if (dim < 0)
        error ("prod: invalid dimension DIM = %d", dim + 1);
    }

  switch (arg.builtin_type ())
    {
    case btyp_double:
      if (arg.issparse ())
        retval = arg.sparse_matrix_value ().prod (dim);
      else
        retval = arg.array_value ().prod (dim);
      break;
    case btyp_complex:
      if (arg.issparse ())
        retval = arg.sparse_complex_matrix_value ().prod (dim);
      else
        retval = arg.complex_array_value ().prod (dim);
      break;
    case btyp_float:
      if (isdouble)
        retval = arg.float_array_value ().dprod (dim);
      else
        retval = arg.float_array_value ().prod (dim);
      break;
    case btyp_float_complex:
      if (isdouble)
        retval = arg.float_complex_array_value ().dprod (dim);
      else
        retval = arg.float_complex_array_value ().prod (dim);
      break;

#define MAKE_INT_BRANCH(X)                              \
      case btyp_ ## X:                                  \
        if (isnative)                                   \
          retval = arg.X ## _array_value ().prod (dim); \
        else                                            \
          retval = arg.array_value ().prod (dim);       \
        break;

      MAKE_INT_BRANCH (int8);
      MAKE_INT_BRANCH (int16);
      MAKE_INT_BRANCH (int32);
      MAKE_INT_BRANCH (int64);
      MAKE_INT_BRANCH (uint8);
      MAKE_INT_BRANCH (uint16);
      MAKE_INT_BRANCH (uint32);
      MAKE_INT_BRANCH (uint64);

#undef MAKE_INT_BRANCH

    // GAGME: Accursed Matlab compatibility...
    case btyp_char:
      retval = arg.array_value (true).prod (dim);
      break;

    case btyp_bool:
      if (arg.issparse ())
        {
          if (isnative)
            retval = arg.sparse_bool_matrix_value ().all (dim);
          else
            retval = arg.sparse_matrix_value ().prod (dim);
        }
      else if (isnative)
        retval = arg.bool_array_value ().all (dim);
      else
        retval = NDArray (arg.bool_array_value ().all (dim));
      break;

    default:
      err_wrong_type_arg ("prod", arg);
    }

  return retval;
}

/*
%!assert (prod ([1, 2, 3]), 6)
%!assert (prod ([-1; -2; -3]), -6)
%!assert (prod ([i, 2+i, -3+2i, 4]), -4 - 32i)
%!assert (prod ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i]), [-1+i, -8+8i, -27+27i])

%!assert (prod (single ([1, 2, 3])), single (6))
%!assert (prod (single ([-1; -2; -3])), single (-6))
%!assert (prod (single ([i, 2+i, -3+2i, 4])), single (-4 - 32i))
%!assert (prod (single ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i])),
%!        single ([-1+i, -8+8i, -27+27i]))

## Test sparse
%!assert (prod (sparse ([1, 2, 3])), sparse (6))
%!assert (prod (sparse ([-1; -2; -3])), sparse (-6))
## Commented out until bug #42290 is fixed
#%!assert (prod (sparse ([i, 2+i, -3+2i, 4])), sparse (-4 - 32i))
#%!assert (prod (sparse ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i])),
#%!         sparse ([-1+i, -8+8i, -27+27i]))

%!assert (prod ([1, 2; 3, 4], 1), [3, 8])
%!assert (prod ([1, 2; 3, 4], 2), [2; 12])
%!assert (prod (zeros (1, 0)), 1)
%!assert (prod (zeros (1, 0), 1), zeros (1, 0))
%!assert (prod (zeros (1, 0), 2), 1)
%!assert (prod (zeros (0, 1)), 1)
%!assert (prod (zeros (0, 1), 1), 1)
%!assert (prod (zeros (0, 1), 2), zeros (0, 1))
%!assert (prod (zeros (2, 0)), zeros (1, 0))
%!assert (prod (zeros (2, 0), 1), zeros (1, 0))
%!assert (prod (zeros (2, 0), 2), [1; 1])
%!assert (prod (zeros (0, 2)), [1, 1])
%!assert (prod (zeros (0, 2), 1), [1, 1])
%!assert (prod (zeros (0, 2), 2), zeros (0, 1))

%!assert (prod (single ([1, 2; 3, 4]), 1), single ([3, 8]))
%!assert (prod (single ([1, 2; 3, 4]), 2), single ([2; 12]))
%!assert (prod (zeros (1, 0, "single")), single (1))
%!assert (prod (zeros (1, 0, "single"), 1), zeros (1, 0, "single"))
%!assert (prod (zeros (1, 0, "single"), 2), single (1))
%!assert (prod (zeros (0, 1, "single")), single (1))
%!assert (prod (zeros (0, 1, "single"), 1), single (1))
%!assert (prod (zeros (0, 1, "single"), 2), zeros (0, 1, "single"))
%!assert (prod (zeros (2, 0, "single")), zeros (1, 0, "single"))
%!assert (prod (zeros (2, 0, "single"), 1), zeros (1, 0, "single"))
%!assert (prod (zeros (2, 0, "single"), 2), single ([1; 1]))
%!assert (prod (zeros (0, 2, "single")), single ([1, 1]))
%!assert (prod (zeros (0, 2, "single"), 1), single ([1, 1]))
%!assert (prod (zeros (0, 2, "single"), 2), zeros (0, 1, "single"))

## Test "double" type argument
%!assert (prod (single ([1, 2, 3]), "double"), 6)
%!assert (prod (single ([-1; -2; -3]), "double"), -6)
%!assert (prod (single ([i, 2+i, -3+2i, 4]), "double"), -4 - 32i)
%!assert (prod (single ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i]), "double"),
%!        [-1+i, -8+8i, -27+27i])

## Test "native" type argument
%!assert (prod (uint8 ([1, 2, 3]), "native"), uint8 (6))
%!assert (prod (uint8 ([-1; -2; -3]), "native"), uint8 (0))
%!assert (prod (int8 ([1, 2, 3]), "native"), int8 (6))
%!assert (prod (int8 ([-1; -2; -3]), "native"), int8 (-6))
%!assert (prod ([true false; true true], "native"), [true false])
%!assert (prod ([true false; true true], 2, "native"), [false; true])

## Test input validation
%!error prod ()
%!error prod (1,2,3)
%!error <unrecognized type argument 'foobar'> prod (1, "foobar")
*/

static bool
all_scalar_1x1 (const octave_value_list& args)
{
  int n_args = args.length ();
  for (int i = 0; i < n_args; i++)
    if (args(i).numel () != 1)
      return false;

  return true;
}

template <typename TYPE, typename T>
static void
single_type_concat (Array<T>& result,
                    const octave_value_list& args,
                    int dim)
{
  int n_args = args.length ();
  if (! (equal_types<T, char>::value
         || equal_types<T, octave_value>::value)
      && all_scalar_1x1 (args))
    {
      // Optimize all scalars case.
      dim_vector dv (1, 1);
      if (dim == -1 || dim == -2)
        dim = -dim - 1;
      else if (dim >= 2)
        dv.resize (dim+1, 1);
      dv(dim) = n_args;

      result.clear (dv);

      for (int j = 0; j < n_args; j++)
        {
          octave_quit ();

          result(j) = octave_value_extract<T> (args(j));
        }
    }
  else
    {
      OCTAVE_LOCAL_BUFFER (Array<T>, array_list, n_args);

      for (int j = 0; j < n_args; j++)
        {
          octave_quit ();

          array_list[j] = octave_value_extract<TYPE> (args(j));
        }

      result = Array<T>::cat (dim, n_args, array_list);
    }
}

template <typename TYPE, typename T>
static void
single_type_concat (Sparse<T>& result,
                    const octave_value_list& args,
                    int dim)
{
  int n_args = args.length ();
  OCTAVE_LOCAL_BUFFER (Sparse<T>, sparse_list, n_args);

  for (int j = 0; j < n_args; j++)
    {
      octave_quit ();

      sparse_list[j] = octave_value_extract<TYPE> (args(j));
    }

  result = Sparse<T>::cat (dim, n_args, sparse_list);
}

// Dispatcher.
template <typename TYPE>
static TYPE
do_single_type_concat (const octave_value_list& args, int dim)
{
  TYPE result;

  single_type_concat<TYPE, typename TYPE::element_type> (result, args, dim);

  return result;
}

template <typename MAP>
static void
single_type_concat_map (octave_map& result,
                        const octave_value_list& args,
                        int dim)
{
  int n_args = args.length ();
  OCTAVE_LOCAL_BUFFER (MAP, map_list, n_args);

  for (int j = 0; j < n_args; j++)
    {
      octave_quit ();

      map_list[j] = octave_value_extract<MAP> (args(j));
    }

  result = octave_map::cat (dim, n_args, map_list);
}

static octave_map
do_single_type_concat_map (const octave_value_list& args,
                           int dim)
{
  octave_map result;
  if (all_scalar_1x1 (args)) // optimize all scalars case.
    single_type_concat_map<octave_scalar_map> (result, args, dim);
  else
    single_type_concat_map<octave_map> (result, args, dim);

  return result;
}

static octave_value
attempt_type_conversion (const octave_value& ov, std::string dtype)
{
  octave_value retval;

  // First try to find function in the class of OV that can convert to
  // the dispatch type dtype.  It will have the name of the dispatch
  // type.

  std::string cname = ov.class_name ();

  symbol_table& symtab = __get_symbol_table__ ();

  octave_value fcn = symtab.find_method (dtype, cname);

  if (fcn.is_defined ())
    {
      octave_value_list result;

      try
        {
          result = feval (fcn, ovl (ov), 1);
        }
      catch (execution_exception& ee)
        {
          error (ee, "conversion from %s to %s failed", dtype.c_str (),
                 cname.c_str ());
        }

      if (result.empty ())
        error ("conversion from %s to %s failed", dtype.c_str (),
               cname.c_str ());

      retval = result(0);
    }
  else
    {
      // No conversion function available.  Try the constructor for the
      // dispatch type.

      fcn = symtab.find_method (dtype, dtype);

      if (! fcn.is_defined ())
        error ("no constructor for %s!", dtype.c_str ());

      octave_value_list result;

      try
        {
          result = feval (fcn, ovl (ov), 1);
        }
      catch (execution_exception& ee)
        {
          error (ee, "%s constructor failed for %s argument", dtype.c_str (),
                 cname.c_str ());
        }

      if (result.empty ())
        error ("%s constructor failed for %s argument", dtype.c_str (),
               cname.c_str ());

      retval = result(0);
    }

  return retval;
}

octave_value
do_class_concat (const octave_value_list& ovl,
                 const std::string& cattype, int dim)
{
  octave_value retval;

  // Get dominant type for list

  std::string dtype = get_dispatch_type (ovl);

  symbol_table& symtab = __get_symbol_table__ ();

  octave_value fcn = symtab.find_method (cattype, dtype);

  if (fcn.is_defined ())
    {
      // Have method for dominant type.  Call it and let it handle conversions.

      octave_value_list tmp2;

      try
        {
          tmp2 = feval (fcn, ovl, 1);
        }
      catch (execution_exception& ee)
        {
          error (ee, "%s/%s method failed", dtype.c_str (), cattype.c_str ());
        }

      if (tmp2.empty ())
        error ("%s/%s method did not return a value", dtype.c_str (),
               cattype.c_str ());

      retval = tmp2(0);
    }
  else
    {
      // No method for dominant type, so attempt type conversions for
      // all elements that are not of the dominant type, then do the
      // default operation for octave_class values.

      octave_idx_type j = 0;
      octave_idx_type len = ovl.length ();
      octave_value_list tmp (len, octave_value ());
      for (octave_idx_type k = 0; k < len; k++)
        {
          octave_value elt = ovl(k);

          std::string t1_type = elt.class_name ();

          if (t1_type == dtype)
            tmp(j++) = elt;
          else if (elt.isobject () || ! elt.isempty ())
            tmp(j++) = attempt_type_conversion (elt, dtype);
        }

      tmp.resize (j);

      octave_map m = do_single_type_concat_map (tmp, dim);

      std::string cname = tmp(0).class_name ();
      std::list<std::string> parents = tmp(0).parent_class_name_list ();

      retval = octave_value (new octave_class (m, cname, parents));
    }

  return retval;
}

static octave_value
do_cat (const octave_value_list& xargs, int dim, std::string fname)
{
  octave_value retval;

  // We may need to convert elements of the list to cells, so make a copy.
  // This should be efficient, it is done mostly by incrementing reference
  // counts.
  octave_value_list args = xargs;

  int n_args = args.length ();

  if (n_args == 0)
    retval = Matrix ();
  else if (n_args == 1)
    retval = args(0);
  else if (n_args > 1)
    {
      std::string result_type;

      bool all_strings_p = true;
      bool all_sq_strings_p = true;
      bool all_dq_strings_p = true;
      bool all_real_p = true;
      bool all_cmplx_p = true;
      bool any_sparse_p = false;
      bool any_cell_p = false;
      bool any_class_p = false;

      bool first_elem_is_struct = false;

      for (int i = 0; i < n_args; i++)
        {
          if (i == 0)
            {
              result_type = args(i).class_name ();

              first_elem_is_struct = args(i).isstruct ();
            }
          else
            result_type = get_concat_class (result_type, args(i).class_name ());

          if (all_strings_p && ! args(i).is_string ())
            all_strings_p = false;
          if (all_sq_strings_p && ! args(i).is_sq_string ())
            all_sq_strings_p = false;
          if (all_dq_strings_p && ! args(i).is_dq_string ())
            all_dq_strings_p = false;
          if (all_real_p && ! args(i).isreal ())
            all_real_p = false;
          if (all_cmplx_p && ! (args(i).iscomplex ()
                                || args(i).isreal ()))
            all_cmplx_p = false;
          if (! any_sparse_p && args(i).issparse ())
            any_sparse_p = true;
          if (! any_cell_p && args(i).iscell ())
            any_cell_p = true;
          if (! any_class_p && args(i).isobject ())
            any_class_p = true;
        }

      if (any_cell_p && ! any_class_p && ! first_elem_is_struct)
        {
          int j = 0;
          for (int i = 0; i < n_args; i++)
            {
              if (args(i).iscell ())
                args(j++) = args(i);
              else
                {
                  if (args(i).isempty ())
                    continue;  // Delete empty non-cell arg
                  else
                    args(j++) = Cell (args(i));
                }
            }
          n_args = j;
          args.resize (n_args);
        }

      if (any_class_p)
        {
          retval = do_class_concat (args, fname, dim);
        }
      else if (result_type == "double")
        {
          if (any_sparse_p)
            {
              if (all_real_p)
                retval = do_single_type_concat<SparseMatrix> (args, dim);
              else
                retval = do_single_type_concat<SparseComplexMatrix> (args, dim);
            }
          else
            {
              if (all_real_p)
                retval = do_single_type_concat<NDArray> (args, dim);
              else
                retval = do_single_type_concat<ComplexNDArray> (args, dim);
            }
        }
      else if (result_type == "single")
        {
          if (all_real_p)
            retval = do_single_type_concat<FloatNDArray> (args, dim);
          else
            retval = do_single_type_concat<FloatComplexNDArray> (args, dim);
        }
      else if (result_type == "char")
        {
          char type = (all_dq_strings_p ? '"' : '\'');

          if (! all_strings_p)
            warn_implicit_conversion ("Octave:num-to-str",
                                      "numeric", result_type);
          else
            maybe_warn_string_concat (all_dq_strings_p, all_sq_strings_p);

          charNDArray result = do_single_type_concat<charNDArray> (args, dim);

          retval = octave_value (result, type);
        }
      else if (result_type == "logical")
        {
          if (any_sparse_p)
            retval = do_single_type_concat<SparseBoolMatrix> (args, dim);
          else
            retval = do_single_type_concat<boolNDArray> (args, dim);
        }
      else if (result_type == "int8")
        retval = do_single_type_concat<int8NDArray> (args, dim);
      else if (result_type == "int16")
        retval = do_single_type_concat<int16NDArray> (args, dim);
      else if (result_type == "int32")
        retval = do_single_type_concat<int32NDArray> (args, dim);
      else if (result_type == "int64")
        retval = do_single_type_concat<int64NDArray> (args, dim);
      else if (result_type == "uint8")
        retval = do_single_type_concat<uint8NDArray> (args, dim);
      else if (result_type == "uint16")
        retval = do_single_type_concat<uint16NDArray> (args, dim);
      else if (result_type == "uint32")
        retval = do_single_type_concat<uint32NDArray> (args, dim);
      else if (result_type == "uint64")
        retval = do_single_type_concat<uint64NDArray> (args, dim);
      else if (result_type == "cell")
        retval = do_single_type_concat<Cell> (args, dim);
      else if (result_type == "struct")
        retval = do_single_type_concat_map (args, dim);
      else
        {
          dim_vector dv = args(0).dims ();

          // Default concatenation.
          bool (dim_vector::*concat_rule) (const dim_vector&, int)
            = &dim_vector::concat;

          if (dim == -1 || dim == -2)
            {
              concat_rule = &dim_vector::hvcat;
              dim = -dim - 1;
            }

          for (int i = 1; i < args.length (); i++)
            {
              if (! (dv.*concat_rule) (args(i).dims (), dim))
                error ("cat: dimension mismatch");
            }

          // The lines below might seem crazy, since we take a copy
          // of the first argument, resize it to be empty and then resize
          // it to be full.  This is done since it means that there is no
          // recopying of data, as would happen if we used a single resize.
          // It should be noted that resize operation is also significantly
          // slower than the do_cat_op function, so it makes sense to have
          // an empty matrix and copy all data.
          //
          // We might also start with a empty octave_value using
          //
          //   tmp = type_info::lookup_type (args(1).type_name());
          //
          // and then directly resize.  However, for some types there might
          // be some additional setup needed, and so this should be avoided.

          octave_value tmp = args(0);
          tmp = tmp.resize (dim_vector (0, 0)).resize (dv);

          int dv_len = dv.ndims ();
          Array<octave_idx_type> ra_idx (dim_vector (dv_len, 1), 0);

          for (int j = 0; j < n_args; j++)
            {
              // Can't fast return here to skip empty matrices as something
              // like cat (1,[],single ([])) must return an empty matrix of
              // the right type.
              tmp = cat_op (tmp, args(j), ra_idx);

              dim_vector dv_tmp = args(j).dims ();

              if (dim >= dv_len)
                {
                  if (j > 1)
                    error ("%s: indexing error", fname.c_str ());

                  break;
                }
              else
                ra_idx(dim) += (dim < dv_tmp.ndims () ? dv_tmp(dim) : 1);
            }
          retval = tmp;
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUN (horzcat, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{A} =} horzcat (@var{array1}, @var{array2}, @dots{}, @var{arrayN})
Return the horizontal concatenation of N-D array objects, @var{array1},
@var{array2}, @dots{}, @var{arrayN} along dimension 2.

Arrays may also be concatenated horizontally using the syntax for creating
new matrices.  For example:

@example
@var{A} = [ @var{array1}, @var{array2}, @dots{} ]
@end example

This syntax is slightly more efficient because the Octave parser can
concatenate the arrays without the overhead of a function call.
@seealso{cat, vertcat}
@end deftypefn */)
{
  return do_cat (args, -2, "horzcat");
}

/*
## Test concatenation with all zero matrices
%!test
%! warning ("off", "Octave:num-to-str", "local");
%! assert (horzcat ("", 65* ones (1,10)), "AAAAAAAAAA");
%! assert (horzcat (65* ones (1,10), ""), "AAAAAAAAAA");

%!assert (class (horzcat (int64 (1), int64 (1))), "int64")
%!assert (class (horzcat (int64 (1), int32 (1))), "int64")
%!assert (class (horzcat (int64 (1), int16 (1))), "int64")
%!assert (class (horzcat (int64 (1), int8 (1))), "int64")
%!assert (class (horzcat (int64 (1), uint64 (1))), "int64")
%!assert (class (horzcat (int64 (1), uint32 (1))), "int64")
%!assert (class (horzcat (int64 (1), uint16 (1))), "int64")
%!assert (class (horzcat (int64 (1), uint8 (1))), "int64")
%!assert (class (horzcat (int64 (1), single (1))), "int64")
%!assert (class (horzcat (int64 (1), double (1))), "int64")
%!assert (class (horzcat (int64 (1), cell (1))), "cell")
%!assert (class (horzcat (int64 (1), true)), "int64")
%!test
%! warning ("off", "Octave:num-to-str", "local");
%! assert (class (horzcat (int64 (1), "a")), "char");

%!assert (class (horzcat (int32 (1), int64 (1))), "int32")
%!assert (class (horzcat (int32 (1), int32 (1))), "int32")
%!assert (class (horzcat (int32 (1), int16 (1))), "int32")
%!assert (class (horzcat (int32 (1), int8 (1))), "int32")
%!assert (class (horzcat (int32 (1), uint64 (1))), "int32")
%!assert (class (horzcat (int32 (1), uint32 (1))), "int32")
%!assert (class (horzcat (int32 (1), uint16 (1))), "int32")
%!assert (class (horzcat (int32 (1), uint8 (1))), "int32")
%!assert (class (horzcat (int32 (1), single (1))), "int32")
%!assert (class (horzcat (int32 (1), double (1))), "int32")
%!assert (class (horzcat (int32 (1), cell (1))), "cell")
%!assert (class (horzcat (int32 (1), true)), "int32")
%!test
%! warning ("off", "Octave:num-to-str", "local");
%! assert (class (horzcat (int32 (1), "a")), "char");

%!assert (class (horzcat (int16 (1), int64 (1))), "int16")
%!assert (class (horzcat (int16 (1), int32 (1))), "int16")
%!assert (class (horzcat (int16 (1), int16 (1))), "int16")
%!assert (class (horzcat (int16 (1), int8 (1))), "int16")
%!assert (class (horzcat (int16 (1), uint64 (1))), "int16")
%!assert (class (horzcat (int16 (1), uint32 (1))), "int16")
%!assert (class (horzcat (int16 (1), uint16 (1))), "int16")
%!assert (class (horzcat (int16 (1), uint8 (1))), "int16")
%!assert (class (horzcat (int16 (1), single (1))), "int16")
%!assert (class (horzcat (int16 (1), double (1))), "int16")
%!assert (class (horzcat (int16 (1), cell (1))), "cell")
%!assert (class (horzcat (int16 (1), true)), "int16")
%!test
%! warning ("off", "Octave:num-to-str", "local");
%! assert (class (horzcat (int16 (1), "a")), "char");

%!assert (class (horzcat (int8 (1), int64 (1))), "int8")
%!assert (class (horzcat (int8 (1), int32 (1))), "int8")
%!assert (class (horzcat (int8 (1), int16 (1))), "int8")
%!assert (class (horzcat (int8 (1), int8 (1))), "int8")
%!assert (class (horzcat (int8 (1), uint64 (1))), "int8")
%!assert (class (horzcat (int8 (1), uint32 (1))), "int8")
%!assert (class (horzcat (int8 (1), uint16 (1))), "int8")
%!assert (class (horzcat (int8 (1), uint8 (1))), "int8")
%!assert (class (horzcat (int8 (1), single (1))), "int8")
%!assert (class (horzcat (int8 (1), double (1))), "int8")
%!assert (class (horzcat (int8 (1), cell (1))), "cell")
%!assert (class (horzcat (int8 (1), true)), "int8")
%!test
%! warning ("off", "Octave:num-to-str", "local");
%! assert (class (horzcat (int8 (1), "a")), "char");

%!assert (class (horzcat (uint64 (1), int64 (1))), "uint64")
%!assert (class (horzcat (uint64 (1), int32 (1))), "uint64")
%!assert (class (horzcat (uint64 (1), int16 (1))), "uint64")
%!assert (class (horzcat (uint64 (1), int8 (1))), "uint64")
%!assert (class (horzcat (uint64 (1), uint64 (1))), "uint64")
%!assert (class (horzcat (uint64 (1), uint32 (1))), "uint64")
%!assert (class (horzcat (uint64 (1), uint16 (1))), "uint64")
%!assert (class (horzcat (uint64 (1), uint8 (1))), "uint64")
%!assert (class (horzcat (uint64 (1), single (1))), "uint64")
%!assert (class (horzcat (uint64 (1), double (1))), "uint64")
%!assert (class (horzcat (uint64 (1), cell (1))), "cell")
%!assert (class (horzcat (uint64 (1), true)), "uint64")
%!test
%! warning ("off", "Octave:num-to-str", "local");
%! assert (class (horzcat (uint64 (1), "a")), "char");

%!assert (class (horzcat (uint32 (1), int64 (1))), "uint32")
%!assert (class (horzcat (uint32 (1), int32 (1))), "uint32")
%!assert (class (horzcat (uint32 (1), int16 (1))), "uint32")
%!assert (class (horzcat (uint32 (1), int8 (1))), "uint32")
%!assert (class (horzcat (uint32 (1), uint64 (1))), "uint32")
%!assert (class (horzcat (uint32 (1), uint32 (1))), "uint32")
%!assert (class (horzcat (uint32 (1), uint16 (1))), "uint32")
%!assert (class (horzcat (uint32 (1), uint8 (1))), "uint32")
%!assert (class (horzcat (uint32 (1), single (1))), "uint32")
%!assert (class (horzcat (uint32 (1), double (1))), "uint32")
%!assert (class (horzcat (uint32 (1), cell (1))), "cell")
%!assert (class (horzcat (uint32 (1), true)), "uint32")
%!test
%! warning ("off", "Octave:num-to-str", "local");
%! assert (class (horzcat (uint32 (1), "a")), "char");

%!assert (class (horzcat (uint16 (1), int64 (1))), "uint16")
%!assert (class (horzcat (uint16 (1), int32 (1))), "uint16")
%!assert (class (horzcat (uint16 (1), int16 (1))), "uint16")
%!assert (class (horzcat (uint16 (1), int8 (1))), "uint16")
%!assert (class (horzcat (uint16 (1), uint64 (1))), "uint16")
%!assert (class (horzcat (uint16 (1), uint32 (1))), "uint16")
%!assert (class (horzcat (uint16 (1), uint16 (1))), "uint16")
%!assert (class (horzcat (uint16 (1), uint8 (1))), "uint16")
%!assert (class (horzcat (uint16 (1), single (1))), "uint16")
%!assert (class (horzcat (uint16 (1), double (1))), "uint16")
%!assert (class (horzcat (uint16 (1), cell (1))), "cell")
%!assert (class (horzcat (uint16 (1), true)), "uint16")
%!test
%! warning ("off", "Octave:num-to-str", "local");
%! assert (class (horzcat (uint16 (1), "a")), "char");

%!assert (class (horzcat (uint8 (1), int64 (1))), "uint8")
%!assert (class (horzcat (uint8 (1), int32 (1))), "uint8")
%!assert (class (horzcat (uint8 (1), int16 (1))), "uint8")
%!assert (class (horzcat (uint8 (1), int8 (1))), "uint8")
%!assert (class (horzcat (uint8 (1), uint64 (1))), "uint8")
%!assert (class (horzcat (uint8 (1), uint32 (1))), "uint8")
%!assert (class (horzcat (uint8 (1), uint16 (1))), "uint8")
%!assert (class (horzcat (uint8 (1), uint8 (1))), "uint8")
%!assert (class (horzcat (uint8 (1), single (1))), "uint8")
%!assert (class (horzcat (uint8 (1), double (1))), "uint8")
%!assert (class (horzcat (uint8 (1), cell (1))), "cell")
%!assert (class (horzcat (uint8 (1), true)), "uint8")
%!test
%! warning ("off", "Octave:num-to-str", "local");
%! assert (class (horzcat (uint8 (1), "a")), "char");

%!assert (class (horzcat (single (1), int64 (1))), "int64")
%!assert (class (horzcat (single (1), int32 (1))), "int32")
%!assert (class (horzcat (single (1), int16 (1))), "int16")
%!assert (class (horzcat (single (1), int8 (1))), "int8")
%!assert (class (horzcat (single (1), uint64 (1))), "uint64")
%!assert (class (horzcat (single (1), uint32 (1))), "uint32")
%!assert (class (horzcat (single (1), uint16 (1))), "uint16")
%!assert (class (horzcat (single (1), uint8 (1))), "uint8")
%!assert (class (horzcat (single (1), single (1))), "single")
%!assert (class (horzcat (single (1), double (1))), "single")
%!assert (class (horzcat (single (1), cell (1))), "cell")
%!assert (class (horzcat (single (1), true)), "single")
%!test
%! warning ("off", "Octave:num-to-str", "local");
%! assert (class (horzcat (single (1), "a")), "char");

%!assert (class (horzcat (double (1), int64 (1))), "int64")
%!assert (class (horzcat (double (1), int32 (1))), "int32")
%!assert (class (horzcat (double (1), int16 (1))), "int16")
%!assert (class (horzcat (double (1), int8 (1))), "int8")
%!assert (class (horzcat (double (1), uint64 (1))), "uint64")
%!assert (class (horzcat (double (1), uint32 (1))), "uint32")
%!assert (class (horzcat (double (1), uint16 (1))), "uint16")
%!assert (class (horzcat (double (1), uint8 (1))), "uint8")
%!assert (class (horzcat (double (1), single (1))), "single")
%!assert (class (horzcat (double (1), double (1))), "double")
%!assert (class (horzcat (double (1), cell (1))), "cell")
%!assert (class (horzcat (double (1), true)), "double")
%!test
%! warning ("off", "Octave:num-to-str", "local");
%! assert (class (horzcat (double (1), "a")), "char");

%!assert (class (horzcat (cell (1), int64 (1))), "cell")
%!assert (class (horzcat (cell (1), int32 (1))), "cell")
%!assert (class (horzcat (cell (1), int16 (1))), "cell")
%!assert (class (horzcat (cell (1), int8 (1))), "cell")
%!assert (class (horzcat (cell (1), uint64 (1))), "cell")
%!assert (class (horzcat (cell (1), uint32 (1))), "cell")
%!assert (class (horzcat (cell (1), uint16 (1))), "cell")
%!assert (class (horzcat (cell (1), uint8 (1))), "cell")
%!assert (class (horzcat (cell (1), single (1))), "cell")
%!assert (class (horzcat (cell (1), double (1))), "cell")
%!assert (class (horzcat (cell (1), cell (1))), "cell")
%!assert (class (horzcat (cell (1), true)), "cell")
%!assert (class (horzcat (cell (1), "a")), "cell")

%!assert (class (horzcat (true, int64 (1))), "int64")
%!assert (class (horzcat (true, int32 (1))), "int32")
%!assert (class (horzcat (true, int16 (1))), "int16")
%!assert (class (horzcat (true, int8 (1))), "int8")
%!assert (class (horzcat (true, uint64 (1))), "uint64")
%!assert (class (horzcat (true, uint32 (1))), "uint32")
%!assert (class (horzcat (true, uint16 (1))), "uint16")
%!assert (class (horzcat (true, uint8 (1))), "uint8")
%!assert (class (horzcat (true, single (1))), "single")
%!assert (class (horzcat (true, double (1))), "double")
%!assert (class (horzcat (true, cell (1))), "cell")
%!assert (class (horzcat (true, true)), "logical")
%!test
%! warning ("off", "Octave:num-to-str", "local");
%! assert (class (horzcat (true, "a")), "char");

%!test
%! warning ("off", "Octave:num-to-str", "local");
%! assert (class (horzcat ("a", int64 (1))), "char");
%! assert (class (horzcat ("a", int32 (1))), "char");
%! assert (class (horzcat ("a", int16 (1))), "char");
%! assert (class (horzcat ("a", int8 (1))), "char");
%! assert (class (horzcat ("a", int64 (1))), "char");
%! assert (class (horzcat ("a", int32 (1))), "char");
%! assert (class (horzcat ("a", int16 (1))), "char");
%! assert (class (horzcat ("a", int8 (1))), "char");
%! assert (class (horzcat ("a", single (1))), "char");
%! assert (class (horzcat ("a", double (1))), "char");
%! assert (class (horzcat ("a", cell (1))), "cell");
%! assert (class (horzcat ("a", true)), "char");
%! assert (class (horzcat ("a", "a")), "char");

%!assert (class (horzcat (cell (1), struct ("foo", "bar"))), "cell")

%!error horzcat (struct ("foo", "bar"), cell (1))

%!test <*39041> assert (class (horzcat (cell (0), struct ())), "cell")
%!test <51086> assert (class (horzcat (struct (), cell (0))), "struct")
*/

DEFUN (vertcat, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{A} =} vertcat (@var{array1}, @var{array2}, @dots{}, @var{arrayN})
Return the vertical concatenation of N-D array objects, @var{array1},
@var{array2}, @dots{}, @var{arrayN} along dimension 1.

Arrays may also be concatenated vertically using the syntax for creating
new matrices.  For example:

@example
@var{A} = [ @var{array1}; @var{array2}; @dots{} ]
@end example

This syntax is slightly more efficient because the Octave parser can
concatenate the arrays without the overhead of a function call.
@seealso{cat, horzcat}
@end deftypefn */)
{
  return do_cat (args, -1, "vertcat");
}

/*
%!test
%! c = {"foo"; "bar"; "bazoloa"};
%! assert (vertcat (c, "a", "bc", "def"),
%!         {"foo"; "bar"; "bazoloa"; "a"; "bc"; "def"});
*/

DEFUN (cat, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{A} =} cat (@var{dim}, @var{array1}, @var{array2}, @dots{}, @var{arrayN})
Return the concatenation of N-D array objects, @var{array1}, @var{array2},
@dots{}, @var{arrayN} along dimension @var{dim}.

@example
@group
A = ones (2, 2);
B = zeros (2, 2);
cat (2, A, B)
  @result{} 1 1 0 0
     1 1 0 0
@end group
@end example

Alternatively, we can concatenate @var{A} and @var{B} along the second
dimension in the following way:

@example
@group
[A, B]
@end group
@end example

@var{dim} can be larger than the dimensions of the N-D array objects and the
result will thus have @var{dim} dimensions as the following example shows:

@example
@group
cat (4, ones (2, 2), zeros (2, 2))
  @result{} ans(:,:,1,1) =

       1 1
       1 1

     ans(:,:,1,2) =

       0 0
       0 0
@end group
@end example
@seealso{horzcat, vertcat}
@end deftypefn */)
{
  if (args.length () == 0)
    print_usage ();

  int dim = args(0).xint_value ("cat: DIM must be an integer") - 1;

  if (dim < 0)
    error ("cat: DIM must be a valid dimension");

  return ovl (do_cat (args.slice (1, args.length () - 1), dim, "cat"));
}

/*
%!function ret = __testcat (t1, t2, tr, cmplx)
%!  assert (cat (1, cast ([], t1), cast ([], t2)), cast ([], tr));
%!
%!  assert (cat (1, cast (1, t1), cast (2, t2)), cast ([1; 2], tr));
%!  assert (cat (1, cast (1, t1), cast ([2; 3], t2)), cast ([1; 2; 3], tr));
%!  assert (cat (1, cast ([1; 2], t1), cast (3, t2)), cast ([1; 2; 3], tr));
%!  assert (cat (1, cast ([1; 2], t1), cast ([3; 4], t2)),
%!          cast ([1; 2; 3; 4], tr));
%!  assert (cat (2, cast (1, t1), cast (2, t2)), cast ([1, 2], tr));
%!  assert (cat (2, cast (1, t1), cast ([2, 3], t2)), cast ([1, 2, 3], tr));
%!  assert (cat (2, cast ([1, 2], t1), cast (3, t2)), cast ([1, 2, 3], tr));
%!  assert (cat (2, cast ([1, 2], t1), cast ([3, 4], t2)),
%!          cast ([1, 2, 3, 4], tr));
%!
%!  assert ([cast(1, t1); cast(2, t2)], cast ([1; 2], tr));
%!  assert ([cast(1, t1); cast([2; 3], t2)], cast ([1; 2; 3], tr));
%!  assert ([cast([1; 2], t1); cast(3, t2)], cast ([1; 2; 3], tr));
%!  assert ([cast([1; 2], t1); cast([3; 4], t2)], cast ([1; 2; 3; 4], tr));
%!  assert ([cast(1, t1), cast(2, t2)], cast ([1, 2], tr));
%!  assert ([cast(1, t1), cast([2, 3], t2)], cast ([1, 2, 3], tr));
%!  assert ([cast([1, 2], t1), cast(3, t2)], cast ([1, 2, 3], tr));
%!  assert ([cast([1, 2], t1), cast([3, 4], t2)], cast ([1, 2, 3, 4], tr));
%!
%!  if (nargin == 3 || cmplx)
%!    assert (cat (1, cast (1i, t1), cast (2, t2)), cast ([1i; 2], tr));
%!    assert (cat (1, cast (1i, t1), cast ([2; 3], t2)), cast ([1i; 2; 3], tr));
%!    assert (cat (1, cast ([1i; 2], t1), cast (3, t2)), cast ([1i; 2; 3], tr));
%!    assert (cat (1, cast ([1i; 2], t1), cast ([3; 4], t2)),
%!            cast ([1i; 2; 3; 4], tr));
%!    assert (cat (2, cast (1i, t1), cast (2, t2)), cast ([1i, 2], tr));
%!    assert (cat (2, cast (1i, t1), cast ([2, 3], t2)), cast ([1i, 2, 3], tr));
%!    assert (cat (2, cast ([1i, 2], t1), cast (3, t2)), cast ([1i, 2, 3], tr));
%!    assert (cat (2, cast ([1i, 2], t1), cast ([3, 4], t2)),
%!            cast ([1i, 2, 3, 4], tr));
%!    assert ([cast(1i, t1); cast(2, t2)], cast ([1i; 2], tr));
%!    assert ([cast(1i, t1); cast([2; 3], t2)], cast ([1i; 2; 3], tr));
%!    assert ([cast([1i; 2], t1); cast(3, t2)], cast ([1i; 2; 3], tr));
%!    assert ([cast([1i; 2], t1); cast([3; 4], t2)], cast ([1i; 2; 3; 4], tr));
%!    assert ([cast(1i, t1), cast(2, t2)], cast ([1i, 2], tr));
%!    assert ([cast(1i, t1), cast([2, 3], t2)], cast ([1i, 2, 3], tr));
%!    assert ([cast([1i, 2], t1), cast(3, t2)], cast ([1i, 2, 3], tr));
%!    assert ([cast([1i, 2], t1), cast([3, 4], t2)], cast ([1i, 2, 3, 4], tr));
%!
%!    assert (cat (1, cast (1, t1), cast (2i, t2)), cast ([1; 2i], tr));
%!    assert (cat (1, cast (1, t1), cast ([2i; 3], t2)), cast ([1; 2i; 3], tr));
%!    assert (cat (1, cast ([1; 2], t1), cast (3i, t2)), cast ([1; 2; 3i], tr));
%!    assert (cat (1, cast ([1; 2], t1), cast ([3i; 4], t2)),
%!            cast ([1; 2; 3i; 4], tr));
%!    assert (cat (2, cast (1, t1), cast (2i, t2)), cast ([1, 2i], tr));
%!    assert (cat (2, cast (1, t1), cast ([2i, 3], t2)), cast ([1, 2i, 3], tr));
%!    assert (cat (2, cast ([1, 2], t1), cast (3i, t2)), cast ([1, 2, 3i], tr));
%!    assert (cat (2, cast ([1, 2], t1), cast ([3i, 4], t2)),
%!            cast ([1, 2, 3i, 4], tr));
%!    assert ([cast(1, t1); cast(2i, t2)], cast ([1; 2i], tr));
%!    assert ([cast(1, t1); cast([2i; 3], t2)], cast ([1; 2i; 3], tr));
%!    assert ([cast([1; 2], t1); cast(3i, t2)], cast ([1; 2; 3i], tr));
%!    assert ([cast([1; 2], t1); cast([3i; 4], t2)], cast ([1; 2; 3i; 4], tr));
%!    assert ([cast(1, t1), cast(2i, t2)], cast ([1, 2i], tr));
%!    assert ([cast(1, t1), cast([2i, 3], t2)], cast ([1, 2i, 3], tr));
%!    assert ([cast([1, 2], t1), cast(3i, t2)], cast ([1, 2, 3i], tr));
%!    assert ([cast([1, 2], t1), cast([3i, 4], t2)], cast ([1, 2, 3i, 4], tr));
%!
%!    assert (cat (1, cast (1i, t1), cast (2i, t2)), cast ([1i; 2i], tr));
%!    assert (cat (1, cast (1i, t1), cast ([2i; 3], t2)),
%!            cast ([1i; 2i; 3], tr));
%!    assert (cat (1, cast ([1i; 2], t1), cast (3i, t2)),
%!            cast ([1i; 2; 3i], tr));
%!    assert (cat (1, cast ([1i; 2], t1), cast ([3i; 4], t2)),
%!            cast ([1i; 2; 3i; 4], tr));
%!    assert (cat (2, cast (1i, t1), cast (2i, t2)), cast ([1i, 2i], tr));
%!    assert (cat (2, cast (1i, t1), cast ([2i, 3], t2)),
%!            cast ([1i, 2i, 3], tr));
%!    assert (cat (2, cast ([1i, 2], t1), cast (3i, t2)),
%!            cast ([1i, 2, 3i], tr));
%!    assert (cat (2, cast ([1i, 2], t1), cast ([3i, 4], t2)),
%!            cast ([1i, 2, 3i, 4], tr));
%!
%!    assert ([cast(1i, t1); cast(2i, t2)], cast ([1i; 2i], tr));
%!    assert ([cast(1i, t1); cast([2i; 3], t2)], cast ([1i; 2i; 3], tr));
%!    assert ([cast([1i; 2], t1); cast(3i, t2)], cast ([1i; 2; 3i], tr));
%!    assert ([cast([1i; 2], t1); cast([3i; 4], t2)],
%!            cast ([1i; 2; 3i; 4], tr));
%!    assert ([cast(1i, t1), cast(2i, t2)], cast ([1i, 2i], tr));
%!    assert ([cast(1i, t1), cast([2i, 3], t2)], cast ([1i, 2i, 3], tr));
%!    assert ([cast([1i, 2], t1), cast(3i, t2)], cast ([1i, 2, 3i], tr));
%!    assert ([cast([1i, 2], t1), cast([3i, 4], t2)],
%!            cast ([1i, 2, 3i, 4], tr));
%!  endif
%!  ret = true;
%!endfunction

%!assert (__testcat ("double", "double", "double"))
%!assert (__testcat ("single", "double", "single"))
%!assert (__testcat ("double", "single", "single"))
%!assert (__testcat ("single", "single", "single"))

%!assert (__testcat ("double", "int8", "int8", false))
%!assert (__testcat ("int8", "double", "int8", false))
%!assert (__testcat ("single", "int8", "int8", false))
%!assert (__testcat ("int8", "single", "int8", false))
%!assert (__testcat ("int8", "int8", "int8", false))
%!assert (__testcat ("double", "int16", "int16", false))
%!assert (__testcat ("int16", "double", "int16", false))
%!assert (__testcat ("single", "int16", "int16", false))
%!assert (__testcat ("int16", "single", "int16", false))
%!assert (__testcat ("int16", "int16", "int16", false))
%!assert (__testcat ("double", "int32", "int32", false))
%!assert (__testcat ("int32", "double", "int32", false))
%!assert (__testcat ("single", "int32", "int32", false))
%!assert (__testcat ("int32", "single", "int32", false))
%!assert (__testcat ("int32", "int32", "int32", false))
%!assert (__testcat ("double", "int64", "int64", false))
%!assert (__testcat ("int64", "double", "int64", false))
%!assert (__testcat ("single", "int64", "int64", false))
%!assert (__testcat ("int64", "single", "int64", false))
%!assert (__testcat ("int64", "int64", "int64", false))

%!assert (__testcat ("double", "uint8", "uint8", false))
%!assert (__testcat ("uint8", "double", "uint8", false))
%!assert (__testcat ("single", "uint8", "uint8", false))
%!assert (__testcat ("uint8", "single", "uint8", false))
%!assert (__testcat ("uint8", "uint8", "uint8", false))
%!assert (__testcat ("double", "uint16", "uint16", false))
%!assert (__testcat ("uint16", "double", "uint16", false))
%!assert (__testcat ("single", "uint16", "uint16", false))
%!assert (__testcat ("uint16", "single", "uint16", false))
%!assert (__testcat ("uint16", "uint16", "uint16", false))
%!assert (__testcat ("double", "uint32", "uint32", false))
%!assert (__testcat ("uint32", "double", "uint32", false))
%!assert (__testcat ("single", "uint32", "uint32", false))
%!assert (__testcat ("uint32", "single", "uint32", false))
%!assert (__testcat ("uint32", "uint32", "uint32", false))
%!assert (__testcat ("double", "uint64", "uint64", false))
%!assert (__testcat ("uint64", "double", "uint64", false))
%!assert (__testcat ("single", "uint64", "uint64", false))
%!assert (__testcat ("uint64", "single", "uint64", false))
%!assert (__testcat ("uint64", "uint64", "uint64", false))

%!assert (cat (3, [], [1,2;3,4]), [1,2;3,4])
%!assert (cat (3, [1,2;3,4], []), [1,2;3,4])
%!assert (cat (3, [], [1,2;3,4], []), [1,2;3,4])
%!assert (cat (3, [], [], []), zeros (0, 0, 3))

%!assert (cat (3, [], [], 1, 2), cat (3, 1, 2))
%!assert (cat (3, [], [], [1,2;3,4]), [1,2;3,4])
%!assert (cat (4, [], [], [1,2;3,4]), [1,2;3,4])

%!assert ([zeros(3,2,2); ones(1,2,2)], repmat ([0;0;0;1],[1,2,2]))
%!assert ([zeros(3,2,2); ones(1,2,2)], vertcat (zeros (3,2,2), ones (1,2,2)))

%!test <*49759>
%! A = [];
%! B = {1; 2};
%! assert (cat (1, A, B), {1; 2});
%! assert (cat (2, A, B), {1; 2});

%!error <dimension mismatch> cat (3, cat (3, [], []), [1,2;3,4])
%!error <dimension mismatch> cat (3, zeros (0, 0, 2), [1,2;3,4])
*/

static octave_value
do_permute (const octave_value_list& args, bool inv)
{
  if (args.length () != 2 || args(1).length () < args(1).ndims ())
    print_usage ();

  Array<int> vec = args(1).int_vector_value ();

  // FIXME: maybe we should create an idx_vector object here
  //        and pass that to permute?
  int n = vec.numel ();
  for (int i = 0; i < n; i++)
    vec(i)--;

  return octave_value (args(0).permute (vec, inv));
}

DEFUN (permute, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{B} =} permute (@var{A}, @var{perm})
Return the generalized transpose for an N-D array object @var{A}.

The permutation vector @var{perm} must contain the elements
@w{@code{1:ndims (A)}} (in any order, but each element must appear only
once).  The @var{N}th dimension of @var{A} gets remapped to dimension
@code{@var{PERM}(@var{N})}.  For example:

@example
@group
@var{x} = zeros ([2, 3, 5, 7]);
size (@var{x})
   @result{}  2   3   5   7

size (permute (@var{x}, [2, 1, 3, 4]))
   @result{}  3   2   5   7

size (permute (@var{x}, [1, 3, 4, 2]))
   @result{}  2   5   7   3

## The identity permutation
size (permute (@var{x}, [1, 2, 3, 4]))
   @result{}  2   3   5   7
@end group
@end example
@seealso{ipermute}
@end deftypefn */)
{
  return do_permute (args, false);
}

DEFUN (ipermute, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{A} =} ipermute (@var{B}, @var{iperm})
The inverse of the @code{permute} function.

The expression

@example
ipermute (permute (A, perm), perm)
@end example

@noindent
returns the original array @var{A}.
@seealso{permute}
@end deftypefn */)
{
  return do_permute (args, true);
}

DEFUN (length, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{n} =} length (@var{A})
Return the length of the object @var{A}.

The length is 0 for empty objects, 1 for scalars, and the number of elements
for vectors.  For matrix or N-dimensional objects, the length is the number
of elements along the largest dimension
(equivalent to @w{@code{max (size (@var{A}))}}).
@seealso{numel, size}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).length ());
}

DEFUN (ndims, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{n} =} ndims (@var{A})
Return the number of dimensions of @var{A}.

For any array, the result will always be greater than or equal to 2.
Trailing singleton dimensions are not counted, i.e., trailing dimensions @var{d}
greater than 2 for which @code{size (@var{A}, @var{d}) = 1}.

@example
@group
ndims (ones (4, 1, 2, 1))
    @result{} 3
@end group
@end example
@seealso{size}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  // This function *must* use size() to determine the desired values to be
  // compatible with Matlab and to allow user-defined class overloading.
  Matrix sz = octave_value (args(0)).size ();

  octave_idx_type ndims = sz.numel ();

  // Don't count trailing ones.  Trailing zeros are *not* singleton dimension.
  while ((ndims > 2) && (sz(ndims - 1) == 1))
    ndims--;

  return ovl (ndims);
}

/*
%!assert (ndims (1:5), 2)
%!assert (ndims (ones (4, 1, 2, 1)), 3)
%!assert (ndims (ones (4, 1, 2, 0)), 4)
*/

DEFUN (numel, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{n} =} numel (@var{A})
@deftypefnx {} {@var{n} =} numel (@var{A}, @var{idx1}, @var{idx2}, @dots{})
Return the number of elements in the object @var{A}.

Optionally, if indices @var{idx1}, @var{idx2}, @dots{} are supplied,
return the number of elements that would result from the indexing

@example
@var{A}(@var{idx1}, @var{idx2}, @dots{})
@end example

Note that the indices do not have to be scalar numbers.  For example,

@example
@group
@var{a} = 1;
@var{b} = ones (2, 3);
numel (@var{a}, @var{b})
@end group
@end example

@noindent
will return 6, as this is the number of ways to index with @var{b}.
Or the index could be the string @qcode{":"} which represents the colon
operator.  For example,

@example
@group
@var{A} = ones (5, 3);
numel (@var{A}, 2, ":")
@end group
@end example

@noindent
will return 3 as the second row has three column entries.

This method is also called when an object appears as lvalue with cs-list
indexing, i.e., @code{object@{@dots{}@}} or @code{object(@dots{}).field}.
@seealso{size, length, ndims}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin == 0)
    print_usage ();

  octave_value retval;

  if (nargin == 1)
    retval = args(0).numel ();
  else if (nargin > 1)
    {
      // Don't use numel (const octave_value_list&) here as that corresponds to
      // an overloaded call, not to builtin!
      retval = dims_to_numel (args(0).dims (), args.slice (1, nargin-1));
    }

  return retval;
}

DEFUN (size, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{sz} =} size (@var{A})
@deftypefnx {} {@var{dim_sz} =} size (@var{A}, @var{dim})
@deftypefnx {} {@var{dim_sz} =} size (@var{A}, @var{d1}, @var{d2}, @dots{})
@deftypefnx {} {[@var{rows}, @var{cols}, @dots{}, @var{dim_N_sz}] =} size (@dots{})
Return a row vector with the size (number of elements) of each dimension for
the object @var{A}.

When given a second argument, @var{dim}, return the size of the corresponding
dimension.  If @var{dim} is a vector, return each of the corresponding
dimensions.  Multiple dimensions may also be specified as separate arguments.

With a single output argument, @code{size} returns a row vector.  When called
with multiple output arguments, @code{size} returns the size of dimension N
in the Nth argument.  The number of rows, dimension 1, is returned in the
first argument, the number of columns, dimension 2, is returned in the
second argument, etc.  If there are more dimensions in @var{A} than there are
output arguments, @code{size} returns the total number of elements in the
remaining dimensions in the final output argument.

Example 1: single row vector output

@example
@group
size ([1, 2; 3, 4; 5, 6])
   @result{} [ 3, 2 ]
@end group
@end example

Example 2: number of elements in 2nd dimension (columns)

@example
@group
size ([1, 2; 3, 4; 5, 6], 2)
    @result{} 2
@end group
@end example

Example 3: number of output arguments == number of dimensions

@example
@group
[nr, nc] = size ([1, 2; 3, 4; 5, 6])
    @result{} nr = 3
    @result{} nc = 2
@end group
@end example

Example 4: number of output arguments < number of dimensions

@example
@group
[nr, remainder] = size (ones (2, 3, 4, 5))
    @result{} nr = 2
    @result{} remainder = 60
@end group
@end example

@seealso{numel, ndims, length, rows, columns, size_equal, common_size}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin == 0)
    print_usage ();

  // For compatibility with Matlab, size returns dimensions as doubles.

  Matrix m;

  dim_vector dimensions = args(0).dims ();
  int ndims = dimensions.ndims ();

  if (nargin == 1)
    {
      if (nargout > 1)
        {
          dimensions = dimensions.redim (nargout);
          ndims = dimensions.ndims ();
        }

      m.resize (1, ndims);

      for (octave_idx_type i = 0; i < ndims; i++)
        m(i) = dimensions(i);
    }
  else
    {
      Array<octave_idx_type> query_dims;

      if (nargin > 2)
        {
          query_dims.resize (dim_vector (1, nargin-1));

          for (octave_idx_type i = 0; i < nargin-1; i++)
            query_dims(i) = args(i+1).idx_type_value (true);
        }
      else
        query_dims = args(1).octave_idx_type_vector_value (true);

      if (nargout > 1 && nargout != query_dims.numel ())
        error ("size: nargout > 1 but does not match number of requested dimensions");

      octave_idx_type nidx = query_dims.numel ();

      m.resize (1, nidx);

      for (octave_idx_type i = 0; i < nidx; i++)
        {
          octave_idx_type nd = query_dims.xelem (i);

          if (nd < 1)
            error ("size: requested dimension DIM (= %"
                   OCTAVE_IDX_TYPE_FORMAT ") out of range", nd);

          m(i) = nd <= ndims ? dimensions (nd-1) : 1;
        }
    }

  if (nargout > 1)
    {
      octave_value_list retval (nargout);

      for (octave_idx_type i = 0; i < nargout; i++)
        retval(i) = m(i);

      return retval;
    }

  return ovl (m);
}

/*
## Plain call

%!assert (size ([1, 2; 3, 4; 5, 6]), [3, 2])

%!test
%! [nr, nc] = size ([1, 2; 3, 4; 5, 6]);
%! assert (nr, 3)
%! assert (nc, 2)

%!test
%! [nr, remainder] = size (ones (2, 3, 4, 5));
%! assert (nr, 2)
%! assert (remainder, 60)

## Call for single existing dimension

%!assert (size ([1, 2; 3, 4; 5, 6], 1), 3)
%!assert (size ([1, 2; 3, 4; 5, 6], 2), 2)

## Call for single non-existing dimension

%!assert (size ([1, 2; 3, 4; 5, 6], 3), 1)
%!assert (size ([1, 2; 3, 4; 5, 6], 4), 1)

## Call for more than existing dimensions

%!test
%! [nr, nc, e1, e2] = size ([1, 2; 3, 4; 5, 6]);
%! assert (nr, 3)
%! assert (nc, 2)
%! assert (e1, 1)
%! assert (e2, 1)

## Call for two arbitrary dimensions

%!test
%! dim = [3, 2, 1, 1, 1];
%! for i = 1:5
%!   for j = 1:5
%!     assert (size ([1, 2; 3, 4; 5, 6], i, j), [dim(i), dim(j)])
%!     assert (size ([1, 2; 3, 4; 5, 6], [i, j]), [dim(i), dim(j)])
%!     [a, b] = size ([1, 2; 3, 4; 5, 6], i, j);
%!     assert (a, dim(i));
%!     assert (b, dim(j));
%!     [a, b] = size ([1, 2; 3, 4; 5, 6], [i, j]);
%!     assert (a, dim(i));
%!     assert (b, dim(j));
%!   endfor
%! endfor

## Call for three arbitrary dimensions

%!test
%! dim = [3, 2, 1, 1, 1];
%! for i = 1:5
%!   for j = 1:5
%!     for k = 1:5
%!       assert (size ([1, 2; 3, 4; 5, 6], i, j, k), [dim(i), dim(j), dim(k)])
%!       assert (size ([1, 2; 3, 4; 5, 6], [i, j, k]), [dim(i), dim(j), dim(k)])
%!       [a, b, c] = size ([1, 2; 3, 4; 5, 6], i, j, k);
%!       assert (a, dim(i));
%!       assert (b, dim(j));
%!       assert (c, dim(k));
%!       [a, b, c] = size ([1, 2; 3, 4; 5, 6], [i, j, k]);
%!       assert (a, dim(i));
%!       assert (b, dim(j));
%!       assert (c, dim(k));
%!     endfor
%!   endfor
%! endfor

%!error <does not match number of requested dimensions>
%! [a, b, c] = size ([1, 2; 3, 4; 5, 6], 1:4)
*/

DEFUN (size_equal, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{TF} =} size_equal (@var{A}, @var{B})
@deftypefnx {} {@var{TF} =} size_equal (@var{A}, @var{B}, @dots{})
Return true if the dimensions of all arguments agree.

Trailing singleton dimensions are ignored.  When called with a single argument,
or no argument, @code{size_equal} returns true.
@seealso{size, numel, ndims, common_size}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin >= 1)
    {
      dim_vector a_dims = args(0).dims ();

      for (int i = 1; i < nargin; ++i)
        {
          dim_vector b_dims = args(i).dims ();

          if (a_dims != b_dims)
            return ovl (false);
        }
    }

  return ovl (true);
}

DEFUN (nnz, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{n} =} nnz (@var{A})
Return the number of nonzero elements in @var{A}.
@seealso{nzmax, nonzeros, find}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).nnz ());
}

/*
%!assert (nnz (1:5), 5)
%!assert (nnz (-5:-1), 5)
%!assert (nnz (0:5), 5)
%!assert (nnz (-5:0), 5)
%!assert (nnz (-5:5), 10)
%!assert (nnz (-2:1:2), 4)
%!assert (nnz (-2+eps (2):1:2), 5)
%!assert (nnz (-2-eps (2):1:2), 5)
%!assert (nnz (-2:1+eps (1):2), 5)
%!assert (nnz (-2:1-eps (1):2), 5)
%!assert (nnz ([1:5] * 0), 0)
%!assert (nnz ([-5:-1] * 0), 0)
%!assert (nnz ([-1:1] * 0), 0)
*/

DEFUN (nzmax, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{n} =} nzmax (@var{SM})
Return the amount of storage allocated to the sparse matrix @var{SM}.

Programming Note: Octave tends to crop unused memory at the first opportunity
for sparse objects.  Thus, in general the value of @code{nzmax} will be the
same as @code{nnz}, except for some cases of user-created sparse objects.

Also, note that Octave always reserves storage for at least one value.  Thus,
for empty matrices @code{nnz} will report 0, but @code{nzmax} will report 1.
@seealso{nnz, spalloc, sparse}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).nzmax ());
}

DEFUN (rows, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{nr} =} rows (@var{A})
Return the number of rows of @var{A}.

This is equivalent to @code{size (@var{A}, 1)}.
@seealso{columns, size, length, numel, isscalar, isvector, ismatrix}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  // This function *must* use size() to determine the desired values to
  // allow user-defined class overloading.

  return ovl ((octave_value (args(0)).size ())(0));
}

/*
%!assert (rows (ones (2,5)), 2)
%!assert (rows (ones (5,2)), 5)
%!assert (rows (ones (5,4,3,2)), 5)
%!assert (rows (ones (3,4,5,2)), 3)

%!assert (rows (cell (2,5)), 2)
%!assert (rows (cell (5,2)), 5)
%!assert (rows (cell (5,4,3,2)), 5)
%!assert (rows (cell (3,4,5,2)), 3)

%!test
%! x(2,5,3).a = 1;
%! assert (rows (x), 2);
%! y(5,4,3).b = 2;
%! assert (rows (y), 5);

%!assert (rows ("Hello World"), 1)

%!assert (rows ([]), 0)
%!assert (rows (zeros (2,0)), 2)

## Test input validation
%!error rows ()
%!error rows (1,2)
*/

DEFUN (columns, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{nc} =} columns (@var{A})
Return the number of columns of @var{A}.

This is equivalent to @code{size (@var{A}, 2)}.
@seealso{rows, size, length, numel, isscalar, isvector, ismatrix}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  // This function *must* use size() to determine the desired values to
  // allow user-defined class overloading.

  return ovl ((octave_value (args(0)).size ())(1));
}

DEFUN (sum, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{y} =} sum (@var{x})
@deftypefnx {} {@var{y} =} sum (@var{x}, @var{dim})
@deftypefnx {} {@var{y} =} sum (@dots{}, "native")
@deftypefnx {} {@var{y} =} sum (@dots{}, "double")
@deftypefnx {} {@var{y} =} sum (@dots{}, "extra")
Sum of elements along dimension @var{dim}.

If @var{dim} is omitted, it defaults to the first non-singleton dimension.

The optional @qcode{"type"} input determines the class of the variable
used for calculations.  By default, operations on floating point inputs (double
or single) are performed in their native data type, while operations on
integer, logical, and character data types are performed using doubles.  If the
argument @qcode{"native"} is given, then the operation is performed in the same
type as the original argument.

For example:

@example
@group
sum ([true, true])
   @result{} 2
sum ([true, true], "native")
   @result{} true
@end group
@end example

If @qcode{"double"} is given the sum is performed in double precision even for
single precision inputs.

For double precision inputs, the @qcode{"extra"} option will use a more
accurate algorithm than straightforward summation.  For single precision
inputs, @qcode{"extra"} is the same as @qcode{"double"}.  For all other data
type @qcode{"extra"} has no effect.
@seealso{cumsum, sumsq, prod}
@end deftypefn */)
{
  int nargin = args.length ();

  bool isnative = false;
  bool isdouble = false;
  bool isextra = false;

  if (nargin > 1 && args(nargin - 1).is_string ())
    {
      std::string str = args(nargin - 1).string_value ();

      if (str == "native")
        isnative = true;
      else if (str == "double")
        isdouble = true;
      else if (str == "extra")
        isextra = true;
      else
        error ("sum: unrecognized type argument '%s'", str.c_str ());

      nargin--;
    }

  if (nargin < 1 || nargin > 2)
    print_usage ();

  int dim = -1;
  if (nargin == 2)
    {
      dim = args(1).int_value () - 1;
      if (dim < 0)
        error ("sum: invalid dimension DIM = %d", dim + 1);
    }

  octave_value retval;
  octave_value arg = args(0);

  switch (arg.builtin_type ())
    {
    case btyp_double:
      if (arg.issparse ())
        {
          if (isextra)
            warning ("sum: 'extra' not yet implemented for sparse matrices");
          retval = arg.sparse_matrix_value ().sum (dim);
        }
      else if (isextra)
        retval = arg.array_value ().xsum (dim);
      else
        retval = arg.array_value ().sum (dim);
      break;

    case btyp_complex:
      if (arg.issparse ())
        {
          if (isextra)
            warning ("sum: 'extra' not yet implemented for sparse matrices");
          retval = arg.sparse_complex_matrix_value ().sum (dim);
        }
      else if (isextra)
        retval = arg.complex_array_value ().xsum (dim);
      else
        retval = arg.complex_array_value ().sum (dim);
      break;

    case btyp_float:
      if (isdouble || isextra)
        retval = arg.float_array_value ().dsum (dim);
      else
        retval = arg.float_array_value ().sum (dim);
      break;

    case btyp_float_complex:
      if (isdouble || isextra)
        retval = arg.float_complex_array_value ().dsum (dim);
      else
        retval = arg.float_complex_array_value ().sum (dim);
      break;

#define MAKE_INT_BRANCH(X)                              \
      case btyp_ ## X:                                  \
        if (isnative)                                   \
          retval = arg.X ## _array_value ().sum (dim);  \
        else                                            \
          retval = arg.X ## _array_value ().dsum (dim); \
        break;

      MAKE_INT_BRANCH (int8);
      MAKE_INT_BRANCH (int16);
      MAKE_INT_BRANCH (int32);
      MAKE_INT_BRANCH (int64);
      MAKE_INT_BRANCH (uint8);
      MAKE_INT_BRANCH (uint16);
      MAKE_INT_BRANCH (uint32);
      MAKE_INT_BRANCH (uint64);

#undef MAKE_INT_BRANCH

    // GAGME: Accursed Matlab compatibility...
    case btyp_char:
      if (isextra)
        retval = arg.array_value (true).xsum (dim);
      else
        retval = arg.array_value (true).sum (dim);
      break;

    case btyp_bool:
      if (arg.issparse ())
        {
          if (isnative)
            retval = arg.sparse_bool_matrix_value ().any (dim);
          else
            retval = arg.sparse_bool_matrix_value ().sum (dim);
        }
      else if (isnative)
        retval = arg.bool_array_value ().any (dim);
      else
        retval = arg.array_value ().sum (dim);
      break;

    default:
      err_wrong_type_arg ("sum", arg);
    }

  return retval;
}

/*
%!assert (sum ([1, 2, 3]), 6)
%!assert (sum ([-1; -2; -3]), -6)
%!assert (sum ([i, 2+i, -3+2i, 4]), 3+4i)
%!assert (sum ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i]), [2+2i, 4+4i, 6+6i])

%!assert (sum (single ([1, 2, 3])), single (6))
%!assert (sum (single ([-1; -2; -3])), single (-6))
%!assert (sum (single ([i, 2+i, -3+2i, 4])), single (3+4i))
%!assert (sum (single ([1, 2, 3; i, 2i, 3i; 1+i, 2+2i, 3+3i])),
%!        single ([2+2i, 4+4i, 6+6i]))

%!assert (sum ([1, 2; 3, 4], 1), [4, 6])
%!assert (sum ([1, 2; 3, 4], 2), [3; 7])
%!assert (sum (zeros (1, 0)), 0)
%!assert (sum (zeros (1, 0), 1), zeros (1, 0))
%!assert (sum (zeros (1, 0), 2), 0)
%!assert (sum (zeros (0, 1)), 0)
%!assert (sum (zeros (0, 1), 1), 0)
%!assert (sum (zeros (0, 1), 2), zeros (0, 1))
%!assert (sum (zeros (2, 0)),  zeros (1, 0))
%!assert (sum (zeros (2, 0), 1), zeros (1, 0))
%!assert (sum (zeros (2, 0), 2),  [0; 0])
%!assert (sum (zeros (0, 2)), [0, 0])
%!assert (sum (zeros (0, 2), 1), [0, 0])
%!assert (sum (zeros (0, 2), 2), zeros (0, 1))
%!assert (sum (zeros (2, 2, 0, 3)), zeros (1, 2, 0, 3))
%!assert (sum (zeros (2, 2, 0, 3), 2), zeros (2, 1, 0, 3))
%!assert (sum (zeros (2, 2, 0, 3), 3), zeros (2, 2, 1, 3))
%!assert (sum (zeros (2, 2, 0, 3), 4), zeros (2, 2, 0))
%!assert (sum (zeros (2, 2, 0, 3), 7), zeros (2, 2, 0, 3))

%!assert (sum (single ([1, 2; 3, 4]), 1), single ([4, 6]))
%!assert (sum (single ([1, 2; 3, 4]), 2), single ([3; 7]))
%!assert (sum (zeros (1, 0, "single")), single (0))
%!assert (sum (zeros (1, 0, "single"), 1), zeros (1, 0, "single"))
%!assert (sum (zeros (1, 0, "single"), 2), single (0))
%!assert (sum (zeros (0, 1, "single")), single (0))
%!assert (sum (zeros (0, 1, "single"), 1), single (0))
%!assert (sum (zeros (0, 1, "single"), 2), zeros (0, 1, "single"))
%!assert (sum (zeros (2, 0, "single")),  zeros (1, 0, "single"))
%!assert (sum (zeros (2, 0, "single"), 1), zeros (1, 0, "single"))
%!assert (sum (zeros (2, 0, "single"), 2),  single ([0; 0]))
%!assert (sum (zeros (0, 2, "single")), single ([0, 0]))
%!assert (sum (zeros (0, 2, "single"), 1), single ([0, 0]))
%!assert (sum (zeros (0, 2, "single"), 2), zeros (0, 1, "single"))
%!assert (sum (zeros (2, 2, 0, 3, "single")), zeros (1, 2, 0, 3, "single"))
%!assert (sum (zeros (2, 2, 0, 3, "single"), 2), zeros (2, 1, 0, 3, "single"))
%!assert (sum (zeros (2, 2, 0, 3, "single"), 3), zeros (2, 2, 1, 3, "single"))
%!assert (sum (zeros (2, 2, 0, 3, "single"), 4), zeros (2, 2, 0, "single"))
%!assert (sum (zeros (2, 2, 0, 3, "single"), 7), zeros (2, 2, 0, 3, "single"))

## Test "native"
%!assert (sum ([true,true]), 2)
%!assert (sum ([true,true], "native"), true)
%!assert (sum (int8 ([127,10,-20])), 117)
%!assert (sum (int8 ([127,10,-20]), "native"), int8 (107))

;-)
%!assert (sum ("Octave") + "8", sumsq (primes (17)))

%!error sum ()
%!error sum (1,2,3)
%!error <unrecognized type argument 'foobar'> sum (1, "foobar")
*/

DEFUN (sumsq, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{y} =} sumsq (@var{x})
@deftypefnx {} {@var{y} =} sumsq (@var{x}, @var{dim})
Sum of squares of elements along dimension @var{dim}.

If @var{dim} is omitted, it defaults to the first non-singleton dimension.

This function is conceptually equivalent to computing

@example
sum (x .* conj (x), dim)
@end example

@noindent
but it uses less memory and avoids calling @code{conj} if @var{x} is real.
@seealso{sum, prod}
@end deftypefn */)
{
  DATA_REDUCTION (sumsq);
}

/*
%!assert (sumsq ([1, 2, 3]), 14)
%!assert (sumsq ([-1; -2; 4i]), 21)
%!assert (sumsq ([1, 2, 3; 2, 3, 4; 4i, 6i, 2]), [21, 49, 29])

%!assert (sumsq (single ([1, 2, 3])), single (14))
%!assert (sumsq (single ([-1; -2; 4i])), single (21))
%!assert (sumsq (single ([1, 2, 3; 2, 3, 4; 4i, 6i, 2])), single ([21, 49, 29]))

%!assert (sumsq ([1, 2; 3, 4], 1), [10, 20])
%!assert (sumsq ([1, 2; 3, 4], 2), [5; 25])

%!assert (sumsq (single ([1, 2; 3, 4]), 1), single ([10, 20]))
%!assert (sumsq (single ([1, 2; 3, 4]), 2), single ([5; 25]))

%!error sumsq ()
*/

DEFUN (islogical, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{tf} =} islogical (@var{x})
@deftypefnx {} {@var{tf} =} isbool (@var{x})
Return true if @var{x} is a logical object.
@seealso{ischar, isfloat, isinteger, isstring, isnumeric, isa}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).islogical ());
}

DEFALIAS (isbool, islogical);

/*
%!assert (islogical (true), true)
%!assert (islogical (false), true)
%!assert (islogical ([true, false]), true)
%!assert (islogical (1), false)
%!assert (islogical (1i), false)
%!assert (islogical ([1,1]), false)
%!assert (islogical (single (1)), false)
%!assert (islogical (single (1i)), false)
%!assert (islogical (single ([1,1])), false)
%!assert (islogical (sparse ([true, false])), true)
%!assert (islogical (sparse ([1, 0])), false)
*/

DEFUN (isinteger, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isinteger (@var{x})
Return true if @var{x} is an integer object (int8, uint8, int16, etc.).

Note that @w{@code{isinteger (14)}} is false because numeric constants in
Octave are double precision floating point values.
@seealso{isfloat, ischar, islogical, isstring, isnumeric, isa}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).isinteger ());
}

/*
%!assert (isinteger (int8 (16)))
%!assert (isinteger (int16 (16)))
%!assert (isinteger (int32 (16)))
%!assert (isinteger (int64 (16)))

%!assert (isinteger (uint8 (16)))
%!assert (isinteger (uint16 (16)))
%!assert (isinteger (uint32 (16)))
%!assert (isinteger (uint64 (16)))

%!assert (isinteger (intmax ("int8")))
%!assert (isinteger (intmax ("int16")))
%!assert (isinteger (intmax ("int32")))
%!assert (isinteger (intmax ("int64")))

%!assert (isinteger (intmax ("uint8")))
%!assert (isinteger (intmax ("uint16")))
%!assert (isinteger (intmax ("uint32")))
%!assert (isinteger (intmax ("uint64")))

%!assert (isinteger (intmin ("int8")))
%!assert (isinteger (intmin ("int16")))
%!assert (isinteger (intmin ("int32")))
%!assert (isinteger (intmin ("int64")))

%!assert (isinteger (intmin ("uint8")))
%!assert (isinteger (intmin ("uint16")))
%!assert (isinteger (intmin ("uint32")))
%!assert (isinteger (intmin ("uint64")))

%!assert (isinteger (uint8 ([1:10])))
%!assert (isinteger (uint8 ([1:10; 1:10])))

%!assert (! isinteger (16))
%!assert (! isinteger ("parrot"))
%!assert (! isinteger ([1, 2, 3]))

%!error isinteger ()
%!error isinteger ("multiple", "parameters")
*/

DEFUN (iscomplex, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} iscomplex (@var{x})
Return true if @var{x} is a complex-valued numeric object.
@seealso{isreal, isnumeric, ischar, isfloat, islogical, isstring, isa}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).iscomplex ());
}

/*
%!assert (iscomplex (4), false)
%!assert (iscomplex (i), true)
%!assert (iscomplex (4+3i), true)
%!assert (iscomplex ([1, 2, 3]), false)
%!assert (iscomplex ([1, 2i, 3]), true)

%!assert (iscomplex (0j), false)
%!assert (iscomplex (complex (0,0)), true)
%!assert (iscomplex ("4"), false)
%!assert (iscomplex ({i}), false)

## Test input validation
%!error iscomplex ()
%!error iscomplex (1, 2)
*/

DEFUN (isfloat, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isfloat (@var{x})
Return true if @var{x} is a floating-point numeric object.

Objects of class double or single are floating-point objects.
@seealso{isinteger, ischar, islogical, isnumeric, isstring, isa}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).isfloat ());
}

// FIXME: perhaps this should be implemented with an
// octave_value member function?

DEFUN (complex, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{z} =} complex (@var{x})
@deftypefnx {} {@var{z} =} complex (@var{re}, @var{im})
Return a complex value from real arguments.

With 1 real argument @var{x}, return the complex result
@w{@code{@var{x} + 0i}}.

With 2 real arguments, return the complex result
@w{@code{@var{re} + @var{im}i}}.
@code{complex} can often be more convenient than expressions such as
@w{@code{a + b*i}}.
For example:

@example
@group
complex ([1, 2], [3, 4])
  @result{} [ 1 + 3i   2 + 4i ]
@end group
@end example
@seealso{real, imag, iscomplex, abs, arg}
@end deftypefn */)
// Programming Note: Throughout this function the coding pattern
// octave_value (new XXX)) is used.  This is done specifically because the
// default octave_value constructor would otherwise perform automatic narrowing
// (i.e., complex values with 0 for the imaginary part would be converted
// to real values).  The complex() function *must* return a complex value
// even when the imaginary part is 0.
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  octave_value retval;

  if (nargin == 1)
    {
      octave_value arg = args(0);

      if (arg.iscomplex ())
        retval = arg;
      else
        {
          if (arg.issparse ())
            {
              SparseComplexMatrix val = arg.xsparse_complex_matrix_value ("complex: invalid conversion");

              retval = octave_value (new octave_sparse_complex_matrix (val));
            }
          else if (arg.is_single_type ())
            {
              if (arg.numel () == 1)
                {
                  FloatComplex val = arg.xfloat_complex_value ("complex: invalid conversion");

                  retval = octave_value (new octave_float_complex (val));
                }
              else
                {
                  FloatComplexNDArray val = arg.xfloat_complex_array_value ("complex: invalid conversion");

                  retval = octave_value (new octave_float_complex_matrix (val));
                }
            }
          else
            {
              if (arg.numel () == 1)
                {
                  Complex val = arg.xcomplex_value ("complex: invalid conversion");

                  retval = octave_value (new octave_complex (val));
                }
              else
                {
                  ComplexNDArray val = arg.xcomplex_array_value ("complex: invalid conversion");

                  retval = octave_value (new octave_complex_matrix (val));
                }
            }
        }
    }
  else
    {
      octave_value re = args(0);
      octave_value im = args(1);

      if (re.issparse () && im.issparse ())
        {
          const SparseMatrix re_val = re.sparse_matrix_value ();
          const SparseMatrix im_val = im.sparse_matrix_value ();

          if (re.numel () == 1)
            {
              SparseComplexMatrix result;
              if (re_val.nnz () == 0)
                result = Complex (0, 1) * SparseComplexMatrix (im_val);
              else
                {
                  octave_idx_type nr = im_val.rows ();
                  octave_idx_type nc = im_val.cols ();
                  result = SparseComplexMatrix (nr, nc, re_val(0));

                  for (octave_idx_type j = 0; j < nc; j++)
                    {
                      octave_idx_type off = j * nr;
                      for (octave_idx_type i = im_val.cidx (j);
                           i < im_val.cidx (j + 1); i++)
                        result.data (im_val.ridx (i) + off)
                        += Complex (0, im_val.data (i));
                    }
                }
              retval = octave_value (new octave_sparse_complex_matrix (result));
            }
          else if (im.numel () == 1)
            {
              SparseComplexMatrix result;
              if (im_val.nnz () == 0)
                result = SparseComplexMatrix (re_val);
              else
                {
                  octave_idx_type nr = re_val.rows ();
                  octave_idx_type nc = re_val.cols ();
                  result = SparseComplexMatrix (nr, nc,
                                                Complex (0, im_val(0)));

                  for (octave_idx_type j = 0; j < nc; j++)
                    {
                      octave_idx_type off = j * nr;
                      for (octave_idx_type i = re_val.cidx (j);
                           i < re_val.cidx (j + 1); i++)
                        result.data (re_val.ridx (i) + off)
                        += re_val.data (i);
                    }
                }
              retval = octave_value (new octave_sparse_complex_matrix (result));
            }
          else
            {
              if (re_val.dims () != im_val.dims ())
                error ("complex: dimension mismatch");

              SparseComplexMatrix result;
              result = SparseComplexMatrix (re_val)
                       + Complex (0, 1) * SparseComplexMatrix (im_val);
              retval = octave_value (new octave_sparse_complex_matrix (result));
            }
        }
      else if (re.is_single_type () || im.is_single_type ())
        {
          if (re.numel () == 1)
            {
              float re_val = re.float_value ();

              if (im.numel () == 1)
                {
                  float im_val = im.double_value ();

                  retval = octave_value (new octave_float_complex
                                         (FloatComplex (re_val, im_val)));
                }
              else
                {
                  const FloatNDArray im_val = im.float_array_value ();

                  FloatComplexNDArray result (im_val.dims ());

                  for (octave_idx_type i = 0; i < im_val.numel (); i++)
                    result.xelem (i) = FloatComplex (re_val, im_val.xelem (i));

                  retval = octave_value (new octave_float_complex_matrix
                                         (result));
                }
            }
          else
            {
              const FloatNDArray re_val = re.float_array_value ();

              if (im.numel () == 1)
                {
                  float im_val = im.float_value ();

                  FloatComplexNDArray result (re_val.dims ());

                  for (octave_idx_type i = 0; i < re_val.numel (); i++)
                    result.xelem (i) = FloatComplex (re_val.xelem (i), im_val);

                  retval = octave_value (new octave_float_complex_matrix
                                         (result));
                }
              else
                {
                  const FloatNDArray im_val = im.float_array_value ();

                  if (re_val.dims () != im_val.dims ())
                    error ("complex: dimension mismatch");

                  FloatComplexNDArray result (re_val.dims ());

                  for (octave_idx_type i = 0; i < re_val.numel (); i++)
                    result.xelem (i) = FloatComplex (re_val.xelem (i),
                                                     im_val.xelem (i));

                  retval = octave_value (new octave_float_complex_matrix
                                         (result));
                }
            }
        }
      else if (re.numel () == 1)
        {
          double re_val = re.double_value ();

          if (im.numel () == 1)
            {
              double im_val = im.double_value ();

              retval = octave_value (new octave_complex
                                     (Complex (re_val, im_val)));
            }
          else
            {
              const NDArray im_val = im.array_value ();

              ComplexNDArray result (im_val.dims ());

              for (octave_idx_type i = 0; i < im_val.numel (); i++)
                result.xelem (i) = Complex (re_val, im_val.xelem (i));

              retval = octave_value (new octave_complex_matrix (result));
            }
        }
      else
        {
          const NDArray re_val = re.array_value ();

          if (im.numel () == 1)
            {
              double im_val = im.double_value ();

              ComplexNDArray result (re_val.dims ());

              for (octave_idx_type i = 0; i < re_val.numel (); i++)
                result.xelem (i) = Complex (re_val.xelem (i), im_val);

              retval = octave_value (new octave_complex_matrix (result));
            }
          else
            {
              const NDArray im_val = im.array_value ();

              if (re_val.dims () != im_val.dims ())
                error ("complex: dimension mismatch");

              ComplexNDArray result (re_val.dims (), Complex ());

              for (octave_idx_type i = 0; i < re_val.numel (); i++)
                result.xelem (i) = Complex (re_val.xelem (i),
                                            im_val.xelem (i));

              retval = octave_value (new octave_complex_matrix (result));
            }
        }
    }

  return retval;
}

/*
%!error <undefined> 1+Infj
%!error <undefined> 1+Infi

%!test <31974>
%! assert (Inf + Inf*i, complex (Inf, Inf))
%!
%! assert (1 + Inf*i, complex (1, Inf))
%! assert (1 + Inf*j, complex (1, Inf))
%!
%! ## whitespace should not affect parsing
%! assert (1+Inf*i, complex (1, Inf))
%! assert (1+Inf*j, complex (1, Inf))
%!
%! assert (NaN*j, complex (0, NaN))
%!
%! assert (Inf * 4j, complex (0, Inf))

%!test <31974>
%! x = Inf;
%! assert (x * j, complex (0, Inf))
%! j = complex (0, 1);
%! assert (Inf * j, complex (0, Inf))

%!test <31974>
%! exp = complex (zeros (2, 2), Inf (2, 2));
%! assert (Inf (2, 2) * j, exp)
%! assert (Inf (2, 2) .* j, exp)
%! assert (Inf * (ones (2, 2) * j), exp)
%! assert (Inf (2, 2) .* (ones (2, 2) * j), exp)

%!test <31974>
%! assert ([Inf; 0] * [i, 0], complex ([NaN NaN; 0 0], [Inf NaN; 0 0]))
%! assert ([Inf, 0] * [i; 0], complex (NaN, Inf))
%! assert ([Inf, 0] .* [i, 0], complex ([0 0], [Inf 0]))

%!test <31974>
%! m = @(x, y) x * y;
%! d = @(x, y) x / y;
%! assert (m (Inf, i), complex (0, +Inf))
%! assert (d (Inf, i), complex (0, -Inf))
*/

DEFUN (isreal, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isreal (@var{x})
Return true if @var{x} is a non-complex matrix or scalar.

For compatibility with @sc{matlab}, this includes logical and character
matrices.
@seealso{iscomplex, isnumeric, isa}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).isreal ());
}

DEFUN (isempty, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isempty (@var{A})
Return true if @var{A} is an empty matrix (any one of its dimensions is
zero).
@seealso{isnull, isa}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).isempty ());
}

/*
## Debian bug #706376
%!assert (isempty (speye (2^16)), false)
*/

DEFUN (isnumeric, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isnumeric (@var{x})
Return true if @var{x} is a numeric object, i.e., an integer, real, or
complex array.

Logical and character arrays are not considered to be numeric.
@seealso{isinteger, isfloat, isreal, iscomplex, ischar, islogical, isstring,
iscell, isstruct, isa}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).isnumeric ());
}

/*
%!assert (isnumeric (1), true)
%!assert (isnumeric (1i), true)
%!assert (isnumeric ([1,1]), true)
%!assert (isnumeric (single (1)), true)
%!assert (isnumeric (single (1i)), true)
%!assert (isnumeric (single ([1,1])), true)
%!assert (isnumeric (int8 (1)), true)
%!assert (isnumeric (uint8 ([1,1])), true)
%!assert (isnumeric ("Hello World"), false)
%!assert (isnumeric (true), false)
%!assert (isnumeric (false), false)
%!assert (isnumeric ([true, false]), false)
%!assert (isnumeric (sparse ([true, false])), false)
*/

DEFUN (isscalar, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isscalar (@var{x})
Return true if @var{x} is a scalar.

A scalar is an object with two dimensions for which @code{size (@var{x})}
returns @w{@code{[1, 1]}}.
@seealso{isvector, ismatrix, size}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  // This function *must* use size() to determine the desired values to be
  // compatible with Matlab and to allow user-defined class overloading.
  Matrix sz = octave_value (args(0)).size ();

  return ovl (sz.numel () == 2 && sz(0) == 1 && sz(1) == 1);
}

/*
%!assert (isscalar (1))
%!assert (isscalar ([1, 2]), false)
%!assert (isscalar ([]), false)
%!assert (isscalar ([1, 2; 3, 4]), false)

%!assert (isscalar ("t"))
%!assert (isscalar ("test"), false)
%!assert (isscalar (["test"; "ing"]), false)

%!test
%! s.a = 1;
%! assert (isscalar (s));

## Test input validation
%!error isscalar ()
%!error isscalar (1, 2)
*/

DEFUN (isvector, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isvector (@var{x})
Return true if @var{x} is a vector.

A vector is a 2-D array where one of the dimensions is equal to 1 (either
@nospell{1xN} or @nospell{Nx1}).  As a consequence of this definition, a 1x1
array (a scalar) is also a vector.
@seealso{isscalar, ismatrix, iscolumn, isrow, size}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  // This function *must* use size() to determine the desired values to be
  // compatible with Matlab and to allow user-defined class overloading.
  Matrix sz = octave_value (args(0)).size ();

  return ovl (sz.numel () == 2 && (sz(0) == 1 || sz(1) == 1));
}

/*
%!assert (isvector (1), true)
%!assert (isvector ([1; 2; 3]), true)
%!assert (isvector ([1, 2, 3]), true)
%!assert (isvector ([]), false)
%!assert (isvector ([1, 2; 3, 4]), false)

%!assert (isvector ("t"), true)
%!assert (isvector ("test"), true)
%!assert (isvector (["test"; "ing"]), false)

%!test
%! s.a = 1;
%! assert (isvector (s), true);

## Test input validation
%!error isvector ()
%!error isvector ([1, 2], 2)
*/

DEFUN (isrow, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} isrow (@var{x})
Return true if @var{x} is a row vector.

A row vector is a 2-D array for which @code{size (@var{x})} returns
@w{@code{[1, N]}} with non-negative N.
@seealso{iscolumn, isscalar, isvector, ismatrix, size}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  // This function *must* use size() to determine the desired values to be
  // compatible with Matlab and to allow user-defined class overloading.
  Matrix sz = octave_value (args(0)).size ();

  return ovl (sz.numel () == 2 && sz(0) == 1);
}

/*
%!assert (isrow ([1, 2, 3]))
%!assert (isrow ([1; 2; 3]), false)
%!assert (isrow (1))
%!assert (isrow ([]), false)
%!assert (isrow ([1, 2; 3, 4]), false)

%!assert (isrow (ones (1, 0)), true)
%!assert (isrow (ones (1, 1)), true)
%!assert (isrow (ones (1, 2)), true)
%!assert (isrow (ones (1, 1, 1)), true)
%!assert (isrow (ones (1, 1, 1, 1)), true)

%!assert (isrow (ones (0, 0)), false)
%!assert (isrow (ones (1, 1, 0)), false)

%!assert (isrow ("t"), true)
%!assert (isrow ("test"), true)
%!assert (isrow (["test"; "ing"]), false)

%!test
%! s.a = 1;
%! assert (isrow (s), true);

## Test input validation
%!error isrow ()
%!error isrow ([1, 2], 2)
*/

DEFUN (iscolumn, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} iscolumn (@var{x})
Return true if @var{x} is a column vector.

A column vector is a 2-D array for which @code{size (@var{x})} returns
@w{@code{[N, 1]}} with non-negative N.
@seealso{isrow, isscalar, isvector, ismatrix, size}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  // This function *must* use size() to determine the desired values to be
  // compatible with Matlab and to allow user-defined class overloading.
  Matrix sz = octave_value (args(0)).size ();

  return ovl (sz.numel () == 2 && sz(1) == 1);
}

/*
%!assert (iscolumn ([1, 2, 3]), false)
%!assert (iscolumn ([1; 2; 3]), true)
%!assert (iscolumn (1), true)
%!assert (iscolumn ([]), false)
%!assert (iscolumn ([1, 2; 3, 4]), false)

%!assert (iscolumn ("t"), true)
%!assert (iscolumn ("test"), false)
%!assert (iscolumn (["test"; "ing"]), false)

%!assert (iscolumn (ones (0, 1)), true)
%!assert (iscolumn (ones (1, 1)), true)
%!assert (iscolumn (ones (2, 1)), true)
%!assert (iscolumn (ones (1, 1, 1)), true)
%!assert (iscolumn (ones (1, 1, 1, 1)), true)

%!assert (iscolumn (ones (0, 0)), false)
%!assert (iscolumn (ones (0, 1, 0)), false)

%!test
%! s.a = 1;
%! assert (iscolumn (s));

## Test input validation
%!error iscolumn ()
%!error iscolumn ([1, 2], 2)
*/

DEFUN (ismatrix, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} ismatrix (@var{x})
Return true if @var{x} is a 2-D array.

A matrix is an object with two dimensions (@code{ndims (@var{x}) == 2}) for
which @code{size (@var{x})} returns @w{@code{[M, N]}} with non-negative M and
N.
@seealso{isscalar, isvector, iscell, isstruct, issparse, isa}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  // This function *must* use size() to determine the desired values to be
  // compatible with Matlab and to allow user-defined class overloading.
  Matrix sz = octave_value (args(0)).size ();

  return ovl (sz.numel () == 2 && sz(0) >= 0 && sz(1) >= 0);
}

/*
%!assert (ismatrix ([]), true)
%!assert (ismatrix (1), true)
%!assert (ismatrix ([1, 2, 3]), true)
%!assert (ismatrix ([1, 2; 3, 4]), true)

%!assert (ismatrix (zeros (0)), true)
%!assert (ismatrix (zeros (0, 0)), true)
%!assert (ismatrix (zeros (0, 0, 0)), false)
%!assert (ismatrix (zeros (3, 2, 4)), false)

%!assert (ismatrix (single ([])), true)
%!assert (ismatrix (single (1)), true)
%!assert (ismatrix (single ([1, 2, 3])), true)
%!assert (ismatrix (single ([1, 2; 3, 4])), true)

%!assert (ismatrix ("t"), true)
%!assert (ismatrix ("test"), true)
%!assert (ismatrix (["test"; "ing"]), true)

%!test
%! s.a = 1;
%! assert (ismatrix (s), true);

%!error ismatrix ()
%!error ismatrix ([1, 2; 3, 4], 2)
*/

DEFUN (issquare, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{tf} =} issquare (@var{x})
Return true if @var{x} is a 2-D square array.

A square array is a 2-D object for which @code{size (@var{x})} returns
@w{@code{[N, N]}} where N is a non-negative integer.
@seealso{isscalar, isvector, ismatrix, size}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  // This function *must* use size() to determine the desired values to
  // allow user-defined class overloading.
  Matrix sz = octave_value (args(0)).size ();

  return ovl (sz.numel () == 2 && sz(0) == sz(1));
}

/*
%!assert (issquare ([]))
%!assert (issquare (1))
%!assert (! issquare ([1, 2]))
%!assert (issquare ([1, 2; 3, 4]))
%!assert (! issquare ([1, 2; 3, 4; 5, 6]))
%!assert (! issquare (ones (3,3,3)))
%!assert (issquare ("t"))
%!assert (! issquare ("test"))
%!assert (issquare (["test"; "ing"; "1"; "2"]))
%!test
%! s.a = 1;
%! assert (issquare (s));
%!assert (issquare ({1, 2; 3, 4}))
%!assert (sparse (([1, 2; 3, 4])))

## Test input validation
%!error issquare ()
%!error issquare ([1, 2; 3, 4], 2)
*/

static octave_value
fill_matrix (const octave_value_list& args, int val, const char *fcn)
{
  octave_value retval;

  int nargin = args.length ();

  oct_data_conv::data_type dt = oct_data_conv::dt_double;

  dim_vector dims (1, 1);
  bool issparse = false;
  bool iscomplex = false;

  if (nargin > 0 && args(nargin-1).is_string ())
    {
      std::string nm = args(nargin-1).string_value ();
      nargin--;

      dt = oct_data_conv::string_to_data_type (nm);
    }

  if (nargin > 1 && args(nargin-2).is_string ()
      && args(nargin-2).string_value () == "like")
    {
      std::string nm = args(nargin-1).class_name ();
      issparse = args(nargin-1).issparse ();
      iscomplex = args(nargin-1).iscomplex ();
      nargin -= 2;
      dt = oct_data_conv::string_to_data_type (nm);
    }

  switch (nargin)
    {
    case 0:
      break;

    case 1:
      get_dimensions (args(0), fcn, dims);
      break;

    default:
      {
        dims.resize (nargin);

        for (int i = 0; i < nargin; i++)
          {
            if (args(i).numel () > 1)
              error ("%s: dimensions must be scalars.", fcn);

            dims(i) = (args(i).isempty () ? 0 : args(i).idx_type_value (true));
          }
      }
      break;
    }

  dims.chop_trailing_singletons ();

  check_dimensions (dims, fcn);

  // FIXME: Perhaps this should be made extensible by using the class name
  //        to lookup a function to call to create the new value.

  // Note that automatic narrowing will handle conversion from
  // NDArray to scalar.

  if (issparse)
    {
      if (dims.ndims () > 2)
        error ("%s: sparse ND arrays not supported.", fcn);

      switch (dt)
        {
        case oct_data_conv::dt_double:
          if (iscomplex)
            retval = SparseComplexMatrix (dims(0), dims(1), Complex (val, 0));
          else
            retval = SparseMatrix (dims(0), dims(1), static_cast<double> (val));
          break;

        case oct_data_conv::dt_logical:
          retval = SparseBoolMatrix (dims(0), dims(1), static_cast<bool> (val));
          break;

        default:
          // FIXME: It shouldn't be possible to ever reach this.
          error ("%s: invalid class name for sparse", fcn);
        }

      return retval;
    }

  switch (dt)
    {
    case oct_data_conv::dt_int8:
      retval = int8NDArray (dims, val);
      break;

    case oct_data_conv::dt_uint8:
      retval = uint8NDArray (dims, val);
      break;

    case oct_data_conv::dt_int16:
      retval = int16NDArray (dims, val);
      break;

    case oct_data_conv::dt_uint16:
      retval = uint16NDArray (dims, val);
      break;

    case oct_data_conv::dt_int32:
      retval = int32NDArray (dims, val);
      break;

    case oct_data_conv::dt_uint32:
      retval = uint32NDArray (dims, val);
      break;

    case oct_data_conv::dt_int64:
      retval = int64NDArray (dims, val);
      break;

    case oct_data_conv::dt_uint64:
      retval = uint64NDArray (dims, val);
      break;

    case oct_data_conv::dt_single:
      if (iscomplex)
        retval = FloatComplexNDArray (dims, val);
      else
        retval = FloatNDArray (dims, val);
      break;

    case oct_data_conv::dt_double:
      if (iscomplex)
        retval = ComplexNDArray (dims, Complex (val, 0));
      else
        retval = NDArray (dims, val);
      break;

    case oct_data_conv::dt_logical:
      retval = boolNDArray (dims, val);
      break;

    default:
      error ("%s: invalid class name", fcn);
      break;
    }

  return retval;
}

static octave_value
fill_matrix (const octave_value_list& args, double val, float fval,
             const char *fcn)
{
  octave_value retval;

  int nargin = args.length ();

  oct_data_conv::data_type dt = oct_data_conv::dt_double;

  dim_vector dims (1, 1);
  bool issparse = false;
  bool iscomplex = false;

  if (nargin > 0 && args(nargin-1).is_string ())
    {
      std::string nm = args(nargin-1).string_value ();
      nargin--;

      dt = oct_data_conv::string_to_data_type (nm);
    }

  if (nargin > 1 && args(nargin-2).is_string ()
      && args(nargin-2).string_value () == "like"
      && (std::string(fcn) ==  "Inf"
          || std::string(fcn) == "NaN" || std::string(fcn) == "NA"))
    {
      if (! args(nargin-1).isfloat ())
        error ("%s: input followed by 'like' must be floating point", fcn);
      std::string nm = args(nargin-1).class_name ();
      issparse = args(nargin-1).issparse ();
      iscomplex = args(nargin-1).iscomplex ();
      nargin -= 2;
      dt = oct_data_conv::string_to_data_type (nm);
    }

  switch (nargin)
    {
    case 0:
      break;

    case 1:
      get_dimensions (args(0), fcn, dims);
      break;

    default:
      {
        dims.resize (nargin);

        for (int i = 0; i < nargin; i++)
          {
            if (args(i).numel () > 1)
              error ("%s: dimensions must be scalars.", fcn);

            dims(i) = (args(i).isempty () ? 0 : args(i).idx_type_value (true));
          }
      }
      break;
    }

  dims.chop_trailing_singletons ();

  check_dimensions (dims, fcn);

  // Note that automatic narrowing will handle conversion from
  // NDArray to scalar.

  if (issparse)
    {
      if (dims.ndims () > 2)
        error ("%s: sparse ND arrays not supported", fcn);

      if (iscomplex)
        retval = SparseComplexMatrix (dims(0), dims(1), Complex (val, 0));
      else
        retval = SparseMatrix (dims(0), dims(1), static_cast<double> (val));

      return retval;
    }

  switch (dt)
    {
    case oct_data_conv::dt_single:
      if (iscomplex)
        retval = FloatComplexNDArray (dims, fval);
      else
        retval = FloatNDArray (dims, fval);
      break;

    case oct_data_conv::dt_double:
      if (iscomplex)
        retval = ComplexNDArray (dims, Complex (val, 0));
      else
        retval = NDArray (dims, val);
      break;

    default:
      error ("%s: invalid class name", fcn);
      break;
    }

  return retval;
}

static octave_value
fill_matrix (const octave_value_list& args, double val, const char *fcn)
{
  octave_value retval;

  int nargin = args.length ();

  oct_data_conv::data_type dt = oct_data_conv::dt_double;

  dim_vector dims (1, 1);

  if (nargin > 0 && args(nargin-1).is_string ())
    {
      std::string nm = args(nargin-1).string_value ();
      nargin--;

      dt = oct_data_conv::string_to_data_type (nm);
    }

  switch (nargin)
    {
    case 0:
      break;

    case 1:
      get_dimensions (args(0), fcn, dims);
      break;

    default:
      {
        dims.resize (nargin);

        for (int i = 0; i < nargin; i++)
          {
            if (args(i).numel () > 1)
              error ("%s: dimensions must be scalars.", fcn);

            dims(i) = (args(i).isempty () ? 0 : args(i).idx_type_value (true));
          }
      }
      break;
    }

  dims.chop_trailing_singletons ();

  check_dimensions (dims, fcn);

  // Note that automatic narrowing will handle conversion from
  // NDArray to scalar.

  switch (dt)
    {
    case oct_data_conv::dt_single:
      retval = FloatNDArray (dims, static_cast<float> (val));
      break;

    case oct_data_conv::dt_double:
      retval = NDArray (dims, val);
      break;

    default:
      error ("%s: invalid class name", fcn);
      break;
    }

  return retval;
}

static octave_value
fill_matrix (const octave_value_list& args, const Complex& val,
             const char *fcn)
{
  octave_value retval;

  int nargin = args.length ();

  oct_data_conv::data_type dt = oct_data_conv::dt_double;

  dim_vector dims (1, 1);

  if (nargin > 0 && args(nargin-1).is_string ())
    {
      std::string nm = args(nargin-1).string_value ();
      nargin--;

      dt = oct_data_conv::string_to_data_type (nm);
    }

  switch (nargin)
    {
    case 0:
      break;

    case 1:
      get_dimensions (args(0), fcn, dims);
      break;

    default:
      {
        dims.resize (nargin);

        for (int i = 0; i < nargin; i++)
          {
            if (args(i).numel () > 1)
              error ("%s: dimensions must be scalars.", fcn);

            dims(i) = (args(i).isempty () ? 0 : args(i).idx_type_value (true));
          }
      }
      break;
    }

  dims.chop_trailing_singletons ();

  check_dimensions (dims, fcn);

  // Note that automatic narrowing will handle conversion from
  // NDArray to scalar.

  switch (dt)
    {
    case oct_data_conv::dt_single:
      retval = FloatComplexNDArray (dims,
                                    static_cast<FloatComplex> (val));
      break;

    case oct_data_conv::dt_double:
      retval = ComplexNDArray (dims, val);
      break;

    default:
      error ("%s: invalid class name", fcn);
      break;
    }

  return retval;
}

static octave_value
fill_matrix (const octave_value_list& args, bool val, const char *fcn)
{
  octave_value retval;

  int nargin = args.length ();

  dim_vector dims (1, 1);

  // The TYPE argument is required to be "logical" if present.  This
  // feature appears to be undocumented in Matlab.

  if (nargin > 0 && args(nargin-1).is_string ())
    {
      std::string nm = args(nargin-1).string_value ();
      nargin--;

      if (oct_data_conv::string_to_data_type (nm) != oct_data_conv::dt_logical)
        error ("%s: invalid data type '%s'", fcn, nm.c_str ());
    }

  bool issparse = false;

  if (nargin > 1 && args(nargin-2).is_string ()
      && args(nargin-2).string_value () == "like")
    {
      if (! args(nargin-1).islogical ())
        error (R"(%s: input followed by "like" must be logical)", fcn);

      issparse = args(nargin-1).issparse ();
      nargin -= 2;
    }

  switch (nargin)
    {
    case 0:
      break;

    case 1:
      get_dimensions (args(0), fcn, dims);
      break;

    default:
      {
        dims.resize (nargin);

        for (int i = 0; i < nargin; i++)
          {
            if (args(i).numel () > 1)
              error ("%s: dimensions must be scalars.", fcn);

            dims(i) = (args(i).isempty () ? 0 : args(i).idx_type_value (true));
          }
      }
      break;
    }

  dims.chop_trailing_singletons ();

  check_dimensions (dims, fcn);

  // Note that automatic narrowing will handle conversion from
  // NDArray to scalar.

  if (issparse)
    {
      if (dims.ndims () > 2)
        error ("%s: sparse ND arrays not supported", fcn);

      retval = SparseBoolMatrix (dims(0), dims(1), val);
    }
  else
    retval = boolNDArray (dims, val);

  return retval;
}

DEFUN (ones, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} ones (@var{n})
@deftypefnx {} {@var{val} =} ones (@var{m}, @var{n})
@deftypefnx {} {@var{val} =} ones (@var{m}, @var{n}, @var{k}, @dots{})
@deftypefnx {} {@var{val} =} ones ([@var{m} @var{n} @dots{}])
@deftypefnx {} {@var{val} =} ones (@dots{}, "like", @var{var})
@deftypefnx {} {@var{val} =} ones (@dots{}, @var{class})
Return a matrix or N-dimensional array whose elements are all 1.

If invoked with a single scalar integer argument @var{n}, return a square
@nospell{NxN} matrix.

If invoked with two or more scalar integer arguments, or a vector of integer
values, return an array with the given dimensions.

To create a constant matrix whose values are all the same use an expression
such as

@example
val_matrix = val * ones (m, n)
@end example

If a variable @var{var} is specified after @qcode{"like"}, the output @var{val}
will have the same data type, complexity, and sparsity as @var{var}.

The optional argument @var{class} specifies the class of the return array
and defaults to double.  For example:

@example
val = ones (m,n, "uint8")
@end example
@seealso{zeros}
@end deftypefn */)
{
  return fill_matrix (args, 1, "ones");
}

/*
%!assert (ones (3), [1, 1, 1; 1, 1, 1; 1, 1, 1])
%!assert (ones (2, 3), [1, 1, 1; 1, 1, 1])
%!assert (ones (3, 2), [1, 1; 1, 1; 1, 1])
%!assert (size (ones (3, 4, 5)), [3, 4, 5])

%!assert (ones (3, "single"), single ([1, 1, 1; 1, 1, 1; 1, 1, 1]))
%!assert (ones (2, 3, "single"), single ([1, 1, 1; 1, 1, 1]))
%!assert (ones (3, 2, "single"), single ([1, 1; 1, 1; 1, 1]))
%!assert (size (ones (3, 4, 5, "single")), [3, 4, 5])

%!assert (ones (3, "int8"), int8 ([1, 1, 1; 1, 1, 1; 1, 1, 1]))
%!assert (ones (2, 3, "int8"), int8 ([1, 1, 1; 1, 1, 1]))
%!assert (ones (3, 2, "int8"), int8 ([1, 1; 1, 1; 1, 1]))
%!assert (size (ones (3, 4, 5, "int8")), [3, 4, 5])

%!assert (ones (2, 2, "like", double (1)), double ([1, 1; 1, 1]))
%!assert (ones (2, 2, "like", complex (ones (2, 2))), [1, 1; 1, 1])
%!assert (ones (1, 2, "like", single (1)), single ([1, 1]))
%!assert (ones (1, "like", single (1i)), single (1))
%!assert (ones (2, 2, "like", uint8 (8)), uint8 ([1, 1; 1, 1]))
%!assert (ones (2, "like", speye (2)), sparse ([1, 1; 1, 1]))
%!assert (ones (2, "like", sparse (1i)), sparse (complex ([1, 1; 1, 1])))

%!assert (size (ones (1, -2, 2)), [1, 0, 2])

## Test input validation
%!error <conversion of 1.1 .*failed> ones (1.1)
%!error <conversion of 1.1 .*failed> ones (1, 1.1)
%!error <conversion of 1.1 .*failed> ones ([1, 1.1])
%!error <sparse ND .* not supported> ones (3, 3, 3, "like", speye (1))
%!error <must be scalar> ones (1:3, 1)
%!error <must be scalar> ones (1, 1:3)
%!error <must be scalar> ones (1, 2, 1:3)
%!error <must be scalar> ones (1:3, 1, "like", single (1))
*/

/*
## Tests for bug #47298
## Matlab requires the size to be a row vector.  In that logic, it supports
## n to be a 1x0 vector (returns 0x0) but not a 0x1 vector.  Octave supports
## row and column vectors and therefore must support 0x1, 1x0, and 0x1x1.
## Also any empty input results in a 0x0 output.
%!test <*47298>
%! fcns = {@zeros, @ones, @inf, @nan, @NA, @i, @pi, @e};
%! for idx = 1:numel (fcns)
%!   fcn = fcns{idx};
%!   assert (fcn (zeros (1, 0)), zeros (0, 0));
%!   assert (fcn (zeros (0, 1)), zeros (0, 0));
%!   assert (fcn (zeros (0, 1, 1)), zeros (0, 0));
%!   assert (fcn (zeros ([])), zeros (0, 0));
%!   assert (fcn (zeros (0, 0, 1)), zeros (0, 0));
%! endfor
*/

DEFUN (zeros, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} zeros (@var{n})
@deftypefnx {} {@var{val} =} zeros (@var{m}, @var{n})
@deftypefnx {} {@var{val} =} zeros (@var{m}, @var{n}, @var{k}, @dots{})
@deftypefnx {} {@var{val} =} zeros ([@var{m} @var{n} @dots{}])
@deftypefnx {} {@var{val} =} zeros (@dots{}, "like", @var{var})
@deftypefnx {} {@var{val} =} zeros (@dots{}, @var{class})
Return a matrix or N-dimensional array whose elements are all 0.

If invoked with a single scalar integer argument, return a square
@nospell{NxN} matrix.

If invoked with two or more scalar integer arguments, or a vector of integer
values, return an array with the given dimensions.

If a variable @var{var} is specified after @qcode{"like"}, the output @var{val}
will have the same data type, complexity, and sparsity as @var{var}.

The optional argument @var{class} specifies the class of the return array
and defaults to double.  For example:

@example
val = zeros (m,n, "uint8")
@end example
@seealso{ones}
@end deftypefn */)
{
  return fill_matrix (args, 0, "zeros");
}

/*
%!assert (zeros (3), [0, 0, 0; 0, 0, 0; 0, 0, 0])
%!assert (zeros (2, 3), [0, 0, 0; 0, 0, 0])
%!assert (zeros (3, 2), [0, 0; 0, 0; 0, 0])
%!assert (size (zeros (3, 4, 5)), [3, 4, 5])

%!assert (zeros (2, 2, "like", double (1)), double ([0, 0; 0, 0]))
%!assert (zeros (2, 2, "like", complex (ones (2, 2))), [0, 0; 0, 0])
%!assert (zeros (1, 2, "like", single (1)), single ([0, 0]))
%!assert (zeros (1, 2, "like", single (1i)), single ([0, 0]))
%!assert (zeros (2, 2, "like", uint8 (8)), uint8 ([0, 0; 0, 0]))
%!assert (zeros (2, "like", speye (2)), sparse ([0, 0; 0, 0]))

%!assert (zeros (3, "single"), single ([0, 0, 0; 0, 0, 0; 0, 0, 0]))
%!assert (zeros (2, 3, "single"), single ([0, 0, 0; 0, 0, 0]))
%!assert (zeros (3, 2, "single"), single ([0, 0; 0, 0; 0, 0]))
%!assert (size (zeros (3, 4, 5, "single")), [3, 4, 5])

%!assert (zeros (3, "int8"), int8 ([0, 0, 0; 0, 0, 0; 0, 0, 0]))
%!assert (zeros (2, 3, "int8"), int8 ([0, 0, 0; 0, 0, 0]))
%!assert (zeros (3, 2, "int8"), int8 ([0, 0; 0, 0; 0, 0]))
%!assert (size (zeros (3, 4, 5, "int8")), [3, 4, 5])

## Test input validation
%!error <invalid data type specified> zeros (1, 1, "foobar")
%!error <conversion of 1.1 .*failed> zeros (1.1)
%!error <conversion of 1.1 .*failed> zeros (1, 1.1)
%!error <conversion of 1.1 .*failed> zeros ([1, 1.1])
%!error <conversion of 1.1 .*failed> zeros (1, 1.1, 2)
%!error <conversion of 1.1 .*failed> zeros ([1, 1.1, 2])
%!error <sparse ND .* not supported> zeros (3, 3, 3, "like", speye (1))
%!error <must be scalar> zeros (1:3, 1)
%!error <must be scalar> zeros (1, 1:3)
%!error <must be scalar> zeros (1, 2, 1:3)
%!error <must be scalar> zeros (1:3, 1, "like", single (1))
*/

DEFUN (Inf, args, ,
       doc: /* -*- texinfo -*-
@c List other form of function in documentation index
@findex inf

@deftypefn  {} {@var{A} =} Inf
@deftypefnx {} {@var{A} =} Inf (@var{n})
@deftypefnx {} {@var{A} =} Inf (@var{n}, @var{m})
@deftypefnx {} {@var{A} =} Inf (@var{n}, @var{m}, @var{k}, @dots{})
@deftypefnx {} {@var{A} =} Inf (@dots{}, @var{class})
Return a scalar, matrix or N-dimensional array whose elements are all equal
to the IEEE representation for positive infinity.

Infinity is produced when results are too large to be represented using the
IEEE floating point format for numbers.  Two common examples which produce
infinity are division by zero and overflow.

@example
@group
[ 1/0 e^800 ]
@result{} Inf   Inf
@end group
@end example

When called with no arguments, return a scalar with the value @samp{Inf}.

When called with a single argument, return a square matrix with the
dimension specified.

When called with more than one scalar argument the first two arguments are
taken as the number of rows and columns and any further arguments specify
additional matrix dimensions.

The optional argument @var{class} specifies the return type and may be
either @qcode{"double"} or @qcode{"single"}.
@seealso{isinf, NaN}
@end deftypefn */)
{
  return fill_matrix (args, lo_ieee_inf_value (),
                      lo_ieee_float_inf_value (), "Inf");
}

DEFALIAS (inf, Inf);

/*
%!assert (Inf (3), [Inf, Inf, Inf; Inf, Inf, Inf; Inf, Inf, Inf])
%!assert (Inf (2, 3), [Inf, Inf, Inf; Inf, Inf, Inf])
%!assert (Inf (3, 2), [Inf, Inf; Inf, Inf; Inf, Inf])
%!assert (size (Inf (3, 4, 5)), [3, 4, 5])

%!assert (Inf (3, "single"),
%!        single ([Inf, Inf, Inf; Inf, Inf, Inf; Inf, Inf, Inf]))
%!assert (Inf (2, 3, "single"), single ([Inf, Inf, Inf; Inf, Inf, Inf]))
%!assert (Inf (3, 2, "single"), single ([Inf, Inf; Inf, Inf; Inf, Inf]))
%!assert (size (inf (3, 4, 5, "single")), [3, 4, 5])

%!assert (Inf (2, 2, "like", speye (2)), sparse ([Inf, Inf; Inf, Inf]))
%!assert (Inf (2, 2, "like", complex (ones (2, 2))), [Inf, Inf; Inf, Inf])
%!assert (Inf (2, 2, "like", double (1)), double ([Inf, Inf; Inf, Inf]))
%!assert (Inf (3, 3, "like", single (1)),
%!        single ([Inf, Inf, Inf; Inf, Inf, Inf; Inf, Inf, Inf]))
%!assert (Inf (2, "like", single (1i)), single ([Inf, Inf; Inf, Inf]))

%!error Inf (3, "like", int8 (1))

%!error Inf (3, "int8")
%!error Inf (2, 3, "int8")
%!error Inf (3, 2, "int8")
%!error Inf (3, 4, 5, "int8")
%!error <input .* floating> Inf (3, 3, "like", true)
%!error <input .* floating> Inf (2, "like", uint8 (1))
%!error <must be scalar> Inf (1:3, 1)
%!error <must be scalar> Inf (1, 1:3)
%!error <must be scalar> Inf (1, 2, 1:3)
%!error <must be scalar> Inf (1:3, 1, "like", single (1))
*/

DEFUN (NaN, args, ,
       doc: /* -*- texinfo -*-
@c List other form of function in documentation index
@findex nan

@deftypefn  {} {@var{val} =} NaN
@deftypefnx {} {@var{val} =} NaN (@var{n})
@deftypefnx {} {@var{val} =} NaN (@var{n}, @var{m})
@deftypefnx {} {@var{val} =} NaN (@var{n}, @var{m}, @var{k}, @dots{})
@deftypefnx {} {@var{val} =} NaN (@dots{}, "like", @var{var})
@deftypefnx {} {@var{val} =} NaN (@dots{}, @var{class})
Return a scalar, matrix, or N-dimensional array whose elements are all equal
to the IEEE symbol NaN (Not a Number).

NaN is the result of operations which do not produce a well defined
numerical result.  Common operations which produce a NaN are arithmetic
with infinity
@tex
($\infty - \infty$), zero divided by zero ($0/0$),
@end tex
@ifnottex
(Inf - Inf), zero divided by zero (0/0),
@end ifnottex
and any operation involving another NaN value (5 + NaN).

Note that NaN always compares not equal to NaN (NaN != NaN).  This behavior
is specified by the IEEE standard for floating point arithmetic.  To find
NaN values, use the @code{isnan} function.

When called with no arguments, return a scalar with the value @samp{NaN}.

When called with a single argument, return a square matrix with the
dimension specified.

When called with more than one scalar argument the first two arguments are
taken as the number of rows and columns and any further arguments specify
additional matrix dimensions.

If a variable @var{var} is specified after @qcode{"like"}, the output @var{val}
will have the same data type, complexity, and sparsity as @var{var}.

The optional argument @var{class} specifies the return type and may be
either @qcode{"double"} or @qcode{"single"}.
@seealso{isnan, Inf}
@end deftypefn */)
{
  return fill_matrix (args, lo_ieee_nan_value (),
                      lo_ieee_float_nan_value (), "NaN");
}

DEFALIAS (nan, NaN);

/*
%!assert (NaN (3), [NaN, NaN, NaN; NaN, NaN, NaN; NaN, NaN, NaN])
%!assert (NaN (2, 3), [NaN, NaN, NaN; NaN, NaN, NaN])
%!assert (NaN (3, 2), [NaN, NaN; NaN, NaN; NaN, NaN])
%!assert (size (NaN (3, 4, 5)), [3, 4, 5])

%!assert (NaN (3, "single"),
%!        single ([NaN, NaN, NaN; NaN, NaN, NaN; NaN, NaN, NaN]))
%!assert (NaN (2, 3, "single"), single ([NaN, NaN, NaN; NaN, NaN, NaN]))
%!assert (NaN (3, 2, "single"), single ([NaN, NaN; NaN, NaN; NaN, NaN]))
%!assert (size (NaN (3, 4, 5, "single")), [3, 4, 5])

%!assert (NaN (2, 2, "like", double (1)), double ([NaN, NaN; NaN, NaN]))
%!assert (NaN (2, 2, "like", complex (ones(2, 2))), [NaN, NaN; NaN, NaN])
%!assert (NaN (3, 3, "like", single (1)),
%!        single ([NaN, NaN, NaN; NaN, NaN, NaN; NaN, NaN, NaN]))
%!assert (NaN (2, "like", single (1i)), single ([NaN, NaN; NaN, NaN]))
%!assert (NaN (2, 2, "like", speye (2)), sparse ([NaN, NaN; NaN, NaN]))

%!error NaN (3, 'like', int8 (1))

%!error NaN (3, "int8")
%!error NaN (2, 3, "int8")
%!error NaN (3, 2, "int8")
%!error NaN (3, 4, 5, "int8")
%!error <input .* floating> NaN (3, 3, "like", true)
%!error <input .* floating> NaN (2, "like", uint8 (1))
%!error <must be scalar> NaN (1:3, 1)
%!error <must be scalar> NaN (1, 1:3)
%!error <must be scalar> NaN (1, 2, 1:3)
%!error <must be scalar> NaN (1:3, 1, "like", single (1))
*/

DEFUN (e, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{A} =} e
@deftypefnx {} {@var{A} =} e (@var{n})
@deftypefnx {} {@var{A} =} e (@var{n}, @var{m})
@deftypefnx {} {@var{A} =} e (@var{n}, @var{m}, @var{k}, @dots{})
@deftypefnx {} {@var{A} =} e (@dots{}, @var{class})
Return a scalar, matrix, or N-dimensional array whose elements are all equal
to the base of natural logarithms.

The constant
@tex
$e$ satisfies the equation $\log (e) = 1$.
@end tex
@ifnottex
@samp{e} satisfies the equation @code{log} (e) = 1.
@end ifnottex

When called with no arguments, return a scalar with the value @math{e}.

When called with a single argument, return a square matrix with the dimension
specified.

When called with more than one scalar argument the first two arguments are
taken as the number of rows and columns and any further arguments specify
additional matrix dimensions.

The optional argument @var{class} specifies the return type and may be either
@qcode{"double"} or @qcode{"single"}.
@seealso{log, exp, pi, I}
@end deftypefn */)
{
#if defined (M_E)
  double e_val = M_E;
#else
  double e_val = exp (1.0);
#endif

  return fill_matrix (args, e_val, "e");
}

template <typename T>
T
eps (const T& x)
{
  T epsval = x.abs ();
  typedef typename T::value_type P;
  for (octave_idx_type i = 0; i < x.numel (); i++)
    {
      P val = epsval.xelem (i);
      if (math::isnan (val) || math::isinf (val))
        epsval(i) = numeric_limits<P>::NaN ();
      else if (val < std::numeric_limits<P>::min ())
        epsval(i) = std::numeric_limits<P>::denorm_min ();
      else
        {
          int exponent;
          math::frexp (val, &exponent);
          const P digits = std::numeric_limits<P>::digits;
          epsval(i) = std::pow (static_cast<P> (2.0),
                                static_cast<P> (exponent - digits));
        }
    }
  return epsval;
}

DEFUN (eps, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{d} =} eps
@deftypefnx {} {@var{d} =} eps (@var{x})
@deftypefnx {} {@var{d} =} eps (@var{n}, @var{m})
@deftypefnx {} {@var{d} =} eps (@var{n}, @var{m}, @var{k}, @dots{})
@deftypefnx {} {@var{d} =} eps (@dots{}, @var{class})
Return a scalar, matrix or N-dimensional array whose elements are all eps,
the machine precision.

More precisely, @code{eps} is the relative spacing between any two adjacent
numbers in the machine's floating point system.  This number is obviously
system dependent.  On machines that support IEEE floating point arithmetic,
@code{eps} is approximately
@tex
$2.2204\times10^{-16}$ for double precision and $1.1921\times10^{-7}$
@end tex
@ifnottex
2.2204e-16 for double precision and 1.1921e-07
@end ifnottex
for single precision.

When called with no arguments, return a scalar with the value
@code{eps (1.0)}.

Given a single argument @var{x}, return the distance between @var{x} and the
next largest value.

When called with more than one argument the first two arguments are taken as
the number of rows and columns and any further arguments specify additional
matrix dimensions.  The optional argument @var{class} specifies the return
type and may be either @qcode{"double"} or @qcode{"single"}.
@seealso{realmax, realmin, intmax, flintmax}
@end deftypefn */)
{
  octave_value retval;

  if (args.length () == 1 && ! args(0).is_string ())
    {
      octave_value arg0 = args(0);
      if (arg0.is_single_type ())
        {
          FloatNDArray epsval = eps (arg0.float_array_value ());
          retval = epsval;
        }
      else if (arg0.is_double_type ())
        {
          NDArray epsval = eps (arg0.array_value ());
          retval = epsval;
        }
      else
        error ("eps: X must be of a floating point type");
    }
  else
    retval = fill_matrix (args, std::numeric_limits<double>::epsilon (),
                          std::numeric_limits<float>::epsilon (), "eps");

  return retval;
}

/*
%!assert (eps (1/2), 2^(-53))
%!assert (eps (1), 2^(-52))
%!assert (eps (2), 2^(-51))
%!assert (eps (realmax), 2^971)
%!assert (eps (0), 2^(-1074))
%!assert (eps (realmin/2), 2^(-1074))
%!assert (eps (realmin/16), 2^(-1074))
%!assert (eps (Inf), NaN)
%!assert (eps (NaN), NaN)
%!assert (eps ([1/2 1 2 realmax 0 realmin/2 realmin/16 Inf NaN]),
%!        [2^-53 2^-52 2^-51 2^971 2^-1074 2^-1074 2^-1074 NaN NaN])
%!assert (eps (single (1/2)), single (2^(-24)))
%!assert (eps (single (1)), single (2^(-23)))
%!assert (eps (single (2)), single (2^(-22)))
%!assert (eps (realmax ("single")), single (2^104))
%!assert (eps (single (0)), single (2^(-149)))
%!assert (eps (realmin ("single")/2), single (2^(-149)))
%!assert (eps (realmin ("single")/16), single (2^(-149)))
%!assert (eps (single (Inf)), single (NaN))
%!assert (eps (single (NaN)), single (NaN))
%!assert (eps (single ([1/2 1 2 realmax("single") 0 realmin("single")/2 realmin("single")/16 Inf NaN])),
%!        single ([2^-24 2^-23 2^-22 2^104 2^-149 2^-149 2^-149 NaN NaN]))
%!error <X must be of a floating point type> eps (uint8 ([0 1 2]))
%!error <must be scalar> eps (1:3, 1)
%!error <must be scalar> eps (1, 1:3)
%!error <must be scalar> eps (1, 2, 1:3)
%!error <must be scalar> eps (1:3, 1, "single")
*/

DEFUN (pi, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{p} =} pi
@deftypefnx {} {@var{p} =} pi (@var{n})
@deftypefnx {} {@var{p} =} pi (@var{n}, @var{m})
@deftypefnx {} {@var{p} =} pi (@var{n}, @var{m}, @var{k}, @dots{})
@deftypefnx {} {@var{p} =} pi (@dots{}, @var{class})
Return a scalar, matrix, or N-dimensional array whose elements are all equal
to the ratio of the circumference of a circle to its
@tex
diameter($\pi$).
@end tex
@ifnottex
diameter.
@end ifnottex

When called with no arguments, return a scalar with the value of
@tex
$\pi$.
@end tex
@ifnottex
pi.
@end ifnottex

When called with a single argument, return a square matrix with the dimension
specified.

When called with more than one scalar argument the first two arguments are
taken as the number of rows and columns and any further arguments specify
additional matrix dimensions.

The optional argument @var{class} specifies the return type and may be either
@qcode{"double"} or @qcode{"single"}.
@seealso{e, I}
@end deftypefn */)
{
#if defined (M_PI)
  double pi_val = M_PI;
#else
  double pi_val = 4.0 * atan (1.0);
#endif

  return fill_matrix (args, pi_val, "pi");
}

DEFUN (realmax, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{Rmax} =} realmax
@deftypefnx {} {@var{Rmax} =} realmax (@var{n})
@deftypefnx {} {@var{Rmax} =} realmax (@var{n}, @var{m})
@deftypefnx {} {@var{Rmax} =} realmax (@var{n}, @var{m}, @var{k}, @dots{})
@deftypefnx {} {@var{Rmax} =} realmax (@dots{}, @var{class})
Return a scalar, matrix, or N-dimensional array whose elements are all equal
to the largest floating point number that is representable.

The actual value is system dependent.  On machines that support IEEE
floating point arithmetic, @code{realmax} is approximately
@tex
$1.7977\times10^{308}$ for double precision and $3.4028\times10^{38}$
@end tex
@ifnottex
1.7977e+308 for double precision and 3.4028e+38
@end ifnottex
for single precision.

When called with no arguments, return a scalar with the value
@code{realmax (@qcode{"double"})}.

When called with a single argument, return a square matrix with the
dimension specified.

When called with more than one scalar argument the first two arguments are
taken as the number of rows and columns and any further arguments specify
additional matrix dimensions.

The optional argument @var{class} specifies the return type and may be
either @qcode{"double"} or @qcode{"single"}.
@seealso{realmin, intmax, flintmax, eps}
@end deftypefn */)
{
  return fill_matrix (args, std::numeric_limits<double>::max (),
                      std::numeric_limits<float>::max (), "realmax");
}

DEFUN (realmin, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{Rmin} =} realmin
@deftypefnx {} {@var{Rmin} =} realmin (@var{n})
@deftypefnx {} {@var{Rmin} =} realmin (@var{n}, @var{m})
@deftypefnx {} {@var{Rmin} =} realmin (@var{n}, @var{m}, @var{k}, @dots{})
@deftypefnx {} {@var{Rmin} =} realmin (@dots{}, @var{class})
Return a scalar, matrix, or N-dimensional array whose elements are all equal
to the smallest normalized floating point number that is representable.

The actual value is system dependent.  On machines that support IEEE floating
point arithmetic, @code{realmin} is approximately
@tex
$2.2251\times10^{-308}$ for double precision and $1.1755\times10^{-38}$
@end tex
@ifnottex
2.2251e-308 for double precision and 1.1755e-38
@end ifnottex
for single precision.

When called with no arguments, return a scalar with the value
@code{realmin (@qcode{"double"})}.

When called with a single argument, return a square matrix with the dimension
specified.

When called with more than one scalar argument the first two arguments are
taken as the number of rows and columns and any further arguments specify
additional matrix dimensions.

The optional argument @var{class} specifies the return type and may be either
@qcode{"double"} or @qcode{"single"}.
@seealso{realmax, intmin, eps}
@end deftypefn */)
{
  return fill_matrix (args, std::numeric_limits<double>::min (),
                      std::numeric_limits<float>::min (), "realmin");
}

DEFUN (I, args, ,
       doc: /* -*- texinfo -*-
@c List other forms of function in documentation index
@findex i
@findex j
@findex J

@deftypefn  {} {@var{A} =} I
@deftypefnx {} {@var{A} =} I (@var{n})
@deftypefnx {} {@var{A} =} I (@var{n}, @var{m})
@deftypefnx {} {@var{A} =} I (@var{n}, @var{m}, @var{k}, @dots{})
@deftypefnx {} {@var{A} =} I (@dots{}, @var{class})
Return a scalar, matrix, or N-dimensional array whose elements are all equal
to the pure imaginary unit, defined as
@tex
$\sqrt{-1}$.
@end tex
@ifnottex
@w{@code{sqrt (-1)}}.
@end ifnottex

I, and its equivalents i, j, and J, are functions so any of the names may
be reused for other purposes (such as i for a counter variable).

When called with no arguments, return a scalar with the value @math{i}.

When called with a single argument, return a square matrix with the
dimension specified.

When called with more than one scalar argument the first two arguments are
taken as the number of rows and columns and any further arguments specify
additional matrix dimensions.

The optional argument @var{class} specifies the return type and may be
either @qcode{"double"} or @qcode{"single"}.
@seealso{e, pi, log, exp}
@end deftypefn */)
{
  return fill_matrix (args, Complex (0.0, 1.0), "I");
}

DEFALIAS (i, I);
DEFALIAS (J, I);
DEFALIAS (j, I);

DEFUN (NA, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} NA
@deftypefnx {} {@var{val} =} NA (@var{n})
@deftypefnx {} {@var{val} =} NA (@var{n}, @var{m})
@deftypefnx {} {@var{val} =} NA (@var{n}, @var{m}, @var{k}, @dots{})
@deftypefnx {} {@var{val} =} NA (@dots{}, "like", @var{var})
@deftypefnx {} {@var{val} =} NA (@dots{}, @var{class})
Return a scalar, matrix, or N-dimensional array whose elements are all equal
to the special constant used to designate missing values.

Note that NA always compares not equal to NA (NA != NA).
To find NA values, use the @code{isna} function.

When called with no arguments, return a scalar with the value @samp{NA}.

When called with a single argument, return a square matrix with the
dimension specified.

When called with more than one scalar argument the first two arguments are
taken as the number of rows and columns and any further arguments specify
additional matrix dimensions.

If a variable @var{var} is specified after @qcode{"like"}, the output @var{val}
will have the same data type, complexity, and sparsity as @var{var}.

The optional argument @var{class} specifies the return type and may be
either @qcode{"double"} or @qcode{"single"}.
@seealso{isna}
@end deftypefn */)
{
  return fill_matrix (args, lo_ieee_na_value (),
                      lo_ieee_float_na_value (), "NA");
}

/*
%!assert (single (NA ("double")), NA ("single"))
%!assert (double (NA ("single")), NA ("double"))
*/

DEFUN (false, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} false (@var{x})
@deftypefnx {} {@var{val} =} false (@var{n}, @var{m})
@deftypefnx {} {@var{val} =} false (@var{n}, @var{m}, @var{k}, @dots{})
@deftypefnx {} {@var{val} =} false (@dots{}, "like", @var{var})
Return a matrix or N-dimensional array whose elements are all logical 0.

If invoked with a single scalar integer argument, return a square
matrix of the specified size.

If invoked with two or more scalar integer arguments, or a vector of integer
values, return an array with given dimensions.

If a logical variable @var{var} is specified after @qcode{"like"}, the output
@var{val} will have the same sparsity as @var{var}.
@seealso{true}
@end deftypefn */)
{
  return fill_matrix (args, false, "false");
}

/*
%!assert (false (2, 3), logical (zeros (2, 3)))
%!assert (false (2, 3, "logical"), logical (zeros (2, 3)))
%!assert (false (2, 1, "like", true), [false; false])
%!assert (false (2, 1, "like", sparse (true)), sparse ([false; false]))

%!error false (2, 3, "double")
%!error <input .* logical> false (2, 1, "like", sparse (1))
%!error <must be scalar> false (1:3, 1)
%!error <must be scalar> false (1, 1:3)
%!error <must be scalar> false (1, 2, 1:3)
*/

DEFUN (true, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} true (@var{x})
@deftypefnx {} {@var{val} =} true (@var{n}, @var{m})
@deftypefnx {} {@var{val} =} true (@var{n}, @var{m}, @var{k}, @dots{})
@deftypefnx {} {@var{val} =} true (@dots{}, "like", @var{var})
Return a matrix or N-dimensional array whose elements are all logical 1.

If invoked with a single scalar integer argument, return a square
matrix of the specified size.

If invoked with two or more scalar integer arguments, or a vector of integer
values, return an array with given dimensions.

If a logical variable @var{var} is specified after @qcode{"like"}, the output
@var{val} will have the same sparsity as @var{var}.
@seealso{false}
@end deftypefn */)
{
  return fill_matrix (args, true, "true");
}

/*
%!assert (true (2, 3), logical (ones (2, 3)))
%!assert (true (2, 3, "logical"), logical (ones (2, 3)))
%!assert (true (2, 1, "like", false), [true; true])
%!assert (true (2, 1, "like", sparse (true)), sparse ([true; true]))

%!error true (2, 3, "double")
%!error <input .* logical> true (2, 1, "like", double (1))
%!error <must be scalar> true (1:3, 1)
%!error <must be scalar> true (1, 1:3)
%!error <must be scalar> true (1, 2, 1:3)
*/

template <typename MT>
octave_value
identity_matrix (int nr, int nc)
{
  octave_value retval;

  typename MT::element_type one (1);

  if (nr == 1 && nc == 1)
    retval = one;
  else
    {
      dim_vector dims (nr, nc);

      typename MT::element_type zero (0);

      MT m (dims, zero);

      if (nr > 0 && nc > 0)
        {
          int n = std::min (nr, nc);

          for (int i = 0; i < n; i++)
            m(i, i) = one;
        }

      retval = m;
    }

  return retval;
}

#define INSTANTIATE_EYE(T)                              \
  template octave_value identity_matrix<T> (int, int)

INSTANTIATE_EYE (int8NDArray);
INSTANTIATE_EYE (uint8NDArray);
INSTANTIATE_EYE (int16NDArray);
INSTANTIATE_EYE (uint16NDArray);
INSTANTIATE_EYE (int32NDArray);
INSTANTIATE_EYE (uint32NDArray);
INSTANTIATE_EYE (int64NDArray);
INSTANTIATE_EYE (uint64NDArray);
INSTANTIATE_EYE (FloatNDArray);
INSTANTIATE_EYE (NDArray);
INSTANTIATE_EYE (boolNDArray);

static octave_value
identity_matrix (int nr, int nc, oct_data_conv::data_type dt)
{
  octave_value retval;

  // FIXME: Perhaps this should be made extensible by using the class name
  //        to lookup a function to call to create the new value.

  switch (dt)
    {
    case oct_data_conv::dt_int8:
      retval = identity_matrix<int8NDArray> (nr, nc);
      break;

    case oct_data_conv::dt_uint8:
      retval = identity_matrix<uint8NDArray> (nr, nc);
      break;

    case oct_data_conv::dt_int16:
      retval = identity_matrix<int16NDArray> (nr, nc);
      break;

    case oct_data_conv::dt_uint16:
      retval = identity_matrix<uint16NDArray> (nr, nc);
      break;

    case oct_data_conv::dt_int32:
      retval = identity_matrix<int32NDArray> (nr, nc);
      break;

    case oct_data_conv::dt_uint32:
      retval = identity_matrix<uint32NDArray> (nr, nc);
      break;

    case oct_data_conv::dt_int64:
      retval = identity_matrix<int64NDArray> (nr, nc);
      break;

    case oct_data_conv::dt_uint64:
      retval = identity_matrix<uint64NDArray> (nr, nc);
      break;

    case oct_data_conv::dt_single:
      retval = FloatDiagMatrix (nr, nc, 1.0f);
      break;

    case oct_data_conv::dt_double:
      retval = DiagMatrix (nr, nc, 1.0);
      break;

    case oct_data_conv::dt_logical:
      retval = identity_matrix<boolNDArray> (nr, nc);
      break;

    default:
      error ("eye: invalid class name");
      break;
    }

  return retval;
}

#undef INT_EYE_MATRIX

DEFUN (eye, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{I} =} eye (@var{n})
@deftypefnx {} {@var{I} =} eye (@var{m}, @var{n})
@deftypefnx {} {@var{I} =} eye ([@var{m} @var{n}])
@deftypefnx {} {@var{I} =} eye (@dots{}, @var{class})
Return an identity matrix.

If invoked with a single scalar argument @var{n}, return a square
@nospell{NxN} identity matrix.

If supplied two scalar arguments (@var{m}, @var{n}), @code{eye} takes them
to be the number of rows and columns.  If given a vector with two elements,
@code{eye} uses the values of the elements as the number of rows and
columns, respectively.  For example:

@example
@group
eye (3)
 @result{}  1  0  0
     0  1  0
     0  0  1
@end group
@end example

The following expressions all produce the same result:

@example
@group
eye (2)
@equiv{}
eye (2, 2)
@equiv{}
eye (size ([1, 2; 3, 4]))
@end group
@end example

The optional argument @var{class}, allows @code{eye} to return an array of
the specified type, like

@example
val = zeros (n,m, "uint8")
@end example

Calling @code{eye} with no arguments is equivalent to calling it with an
argument of 1.  Any negative dimensions are treated as zero.  These odd
definitions are for compatibility with @sc{matlab}.
@seealso{speye, ones, zeros}
@end deftypefn */)
{
  int nargin = args.length ();

  oct_data_conv::data_type dt = oct_data_conv::dt_double;

  // Check for type information.

  if (nargin > 0 && args(nargin-1).is_string ())
    {
      std::string nm = args(nargin-1).string_value ();
      nargin--;

      dt = oct_data_conv::string_to_data_type (nm);
    }

  if (nargin > 2)
    print_usage ();

  octave_value retval;

  if (nargin == 0)
    retval = identity_matrix (1, 1, dt);
  else if (nargin == 1)
    {
      octave_idx_type nr, nc;
      get_dimensions (args(0), "eye", nr, nc);

      retval = identity_matrix (nr, nc, dt);
    }
  else
    {
      octave_idx_type nr, nc;
      get_dimensions (args(0), args(1), "eye", nr, nc);

      retval = identity_matrix (nr, nc, dt);
    }

  return retval;
}

/*
%!assert (full (eye (3)), [1, 0, 0; 0, 1, 0; 0, 0, 1])
%!assert (full (eye (2, 3)), [1, 0, 0; 0, 1, 0])

%!assert (full (eye (3,"single")), single ([1, 0, 0; 0, 1, 0; 0, 0, 1]))
%!assert (full (eye (2, 3,"single")), single ([1, 0, 0; 0, 1, 0]))

%!assert (eye (3, "int8"), int8 ([1, 0, 0; 0, 1, 0; 0, 0, 1]))
%!assert (eye (2, 3, "int8"), int8 ([1, 0, 0; 0, 1, 0]))

## Test input validation
%!error eye (1, 2, 3)
%!error <conversion of 1.1 .*failed> eye (1.1)
%!error <conversion of 1.1 .*failed> eye (1, 1.1)
%!error <conversion of 1.1 .*failed> eye ([1, 1.1])
*/

template <typename MT>
static octave_value
do_linspace (const octave_value& base, const octave_value& limit,
             octave_idx_type n)
{
  typedef typename MT::column_vector_type CVT;
  typedef typename MT::element_type T;

  octave_value retval;

  if (base.is_scalar_type ())
    {
      T bs = octave_value_extract<T> (base);
      if (limit.is_scalar_type ())
        {
          T ls = octave_value_extract<T> (limit);
          retval = linspace (bs, ls, n);
        }
      else
        {
          CVT lv = octave_value_extract<CVT> (limit);
          CVT bv (lv.numel (), bs);
          retval = linspace (bv, lv, n);
        }
    }
  else
    {
      CVT bv = octave_value_extract<CVT> (base);
      if (limit.is_scalar_type ())
        {
          T ls = octave_value_extract<T> (limit);
          CVT lv (bv.numel (), ls);
          retval = linspace (bv, lv, n);
        }
      else
        {
          CVT lv = octave_value_extract<CVT> (limit);
          retval = linspace (bv, lv, n);
        }
    }

  return retval;
}

DEFUN (linspace, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{y} =} linspace (@var{start}, @var{end})
@deftypefnx {} {@var{y} =} linspace (@var{start}, @var{end}, @var{n})
Return a row vector with @var{n} linearly spaced elements between @var{start}
and @var{end}.

If the number of elements @var{n} is greater than one, then the endpoints
@var{start} and @var{end} are always included in the range.  If @var{start} is
greater than @var{end}, the elements are stored in decreasing order.  If the
number of points @var{n} is not specified, a value of 100 is used.

The @code{linspace} function returns a row vector when both @var{start} and
@var{end} are scalars.  If one, or both, inputs are vectors, then
@code{linspace} transforms them to column vectors and returns a matrix where
each row is an independent sequence between
@w{@code{@var{start}(@var{row_n}), @var{end}(@var{row_n})}}.

Programming Notes: For compatibility with @sc{matlab}, return the second
argument (@var{end}) when a single value (@var{n} = 1) is requested.  If
@var{n} is not an integer then @code{floor (@var{n})} is used to round the
number of elements.  If @var{n} is zero or negative then an empty 1x0 matrix
is returned.
@seealso{colon, logspace}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin != 2 && nargin != 3)
    print_usage ();

  octave_idx_type npoints = 100;
  if (nargin == 3)
    {
      // Apparently undocumented Matlab.  If the third arg is an empty
      // numeric value, the number of points defaults to 1.
      octave_value arg_3 = args(2);

      if (arg_3.isnumeric () && arg_3.isempty ())
        npoints = 1;
      else if (! arg_3.is_scalar_type ())
        error ("linspace: N must be a scalar");
      else
        // Even if third arg is not an integer, it must be cast to int
        npoints = arg_3.idx_type_value ();
    }

  octave_value arg_1 = args(0);
  octave_value arg_2 = args(1);

  dim_vector sz1 = arg_1.dims ();
  bool isvector1 = sz1.ndims () == 2 && (sz1(0) == 1 || sz1(1) == 1);
  dim_vector sz2 = arg_2.dims ();
  bool isvector2 = sz2.ndims () == 2 && (sz2(0) == 1 || sz2(1) == 1);

  if (! isvector1 || ! isvector2)
    error ("linspace: START, END must be scalars or vectors");

  octave_value retval;

  if (arg_1.is_single_type () || arg_2.is_single_type ())
    {
      if (arg_1.iscomplex () || arg_2.iscomplex ())
        retval = do_linspace<FloatComplexMatrix> (arg_1, arg_2, npoints);
      else
        retval = do_linspace<FloatMatrix> (arg_1, arg_2, npoints);
    }
  else
    {
      if (arg_1.iscomplex () || arg_2.iscomplex ())
        retval = do_linspace<ComplexMatrix> (arg_1, arg_2, npoints);
      else
        retval = do_linspace<Matrix> (arg_1, arg_2, npoints);
    }

  return retval;
}

/*
%!test
%! x1 = linspace (1, 2);
%! x2 = linspace (1, 2, 10);
%! x3 = linspace (1, -2, 10);
%! assert (size (x1) == [1, 100] && x1(1) == 1 && x1(100) == 2);
%! assert (x1(2) - x1(1), (2 - 1)/ (100 - 1), eps);
%! assert (size (x2) == [1, 10] && x2(1) == 1 && x2(10) == 2);
%! assert (x2(2) - x2(1), (2 - 1)/ (10 - 1), eps);
%! assert (size (x3) == [1, 10] && x3(1) == 1 && x3(10) == -2);
%! assert (x3(2) - x3(1), (-2 - 1)/ (10 - 1), eps);

## Test complex values
%!test
%! exp = [1+0i, 2-1.25i, 3-2.5i, 4-3.75i, 5-5i];
%! obs = linspace (1, 5-5i, 5);
%! assert (obs, exp);

## Test support for vectors in START and END
%!assert (linspace ([1 2 3], [7 8 9]),
%!        [linspace(1, 7); linspace(2, 8); linspace(3, 9)], 10*eps)
%!assert (linspace ([1 2 3]', [7 8 9]'),
%!        [linspace(1, 7); linspace(2, 8); linspace(3, 9)], 10*eps)
%!assert (linspace ([1 2 3], 9),
%!        [linspace(1, 9); linspace(2, 9); linspace(3, 9)], 10*eps)
%!assert (linspace ([1 2 3]', 9),
%!        [linspace(1, 9); linspace(2, 9); linspace(3, 9)], 10*eps)
%!assert (linspace (1, [7 8 9]),
%!        [linspace(1, 7); linspace(1, 8); linspace(1, 9)], 10*eps)
%!assert (linspace (1, [7 8 9]'),
%!        [linspace(1, 7); linspace(1, 8); linspace(1, 9)], 10*eps)

## Test class of output
%!assert (class (linspace (1, 2)), "double")
%!assert (class (linspace (single (1), 2)), "single")
%!assert (class (linspace (1, single (2))), "single")

## Test symmetry
%!test <*56659>
%! x = linspace (-1, 1, 10);
%! assert (all (x == -fliplr (x)));
%! x = linspace (-1, 1, 11);
%! assert (all (x == -fliplr (x)));

%!test <*56659>
%! x = linspace (-1-1i, 1+1i, 10);
%! assert (all (x == -fliplr (x)));
%! x = linspace (-1-1i, 1+1i, 11);
%! assert (all (x == -fliplr (x)));

%!test <*56659>
%! x = linspace (single (-1), 1, 10);
%! assert (all (x == -fliplr (x)));
%! x = linspace (single (-1), 1, 11);
%! assert (all (x == -fliplr (x)));

%!test <*56659>
%! x = linspace (single (-1-1i), 1+1i, 10);
%! assert (all (x == -fliplr (x)));
%! x = linspace (single (-1-1i), 1+1i, 11);
%! assert (all (x == -fliplr (x)));

## Test obscure Matlab compatibility options
%!assert (linspace (0, 1, []), 1)
%!assert (linspace (10, 20, 2), [10 20])
%!assert (linspace (10, 20, 1), [20])
%!assert (linspace (10, 20, 0), zeros (1, 0))
%!assert (linspace (10, 20, -1), zeros (1, 0))
%!assert (numel (linspace (0, 1, 2+eps)), 2)
%!assert (numel (linspace (0, 1, 2-eps)), 1)
%!assert (linspace (10, 20, 2.1), [10 20])
%!assert (linspace (10, 20, 2.9), [10 20])
%!assert (1 ./ linspace (-0, 0, 4), [-Inf, Inf, Inf, Inf])
%!assert (linspace (Inf, Inf, 3), [Inf, Inf, Inf])
%!assert (linspace (-Inf, -Inf, 3), [-Inf, -Inf, -Inf])
%!assert (linspace (-Inf, Inf, 3), [-Inf, 0, Inf])
%!assert (linspace (Inf + 1i, Inf + 1i, 3), [Inf + 1i, Inf + 1i, Inf + 1i])
%!assert (linspace (-Inf + 1i, Inf + 1i, 3), [-Inf + 1i, NaN + 1i, Inf + 1i])

## FIXME: Octave is not fully Matlab-compatible for some combinations of
##        Inf/-Inf endpoints.  See bug #56933.  This was dubbed "Won't Fix"
##        so these tests have been removed from the test suite by commenting
##        them out.  If the behavior in the future is made compatible these
##        tests can be re-instated.
##%!assert <56933> (linspace (-Inf, Inf, 4), [-Inf, -Inf, Inf, Inf])
##%!assert <56933> (linspace (-Inf, Inf, 5), [-Inf, -Inf, 0, Inf, Inf])
##%!assert <56933> (linspace (0, Inf, 4), [0, Inf, Inf, Inf])
##%!assert <56933> (linspace (0, -Inf, 4), [0, -Inf, -Inf, -Inf])
##%!assert <56933> (linspace (-Inf, 0, 4), [-Inf, NaN, NaN, 0])
##%!assert <56933> (linspace (Inf, 0, 4), [Inf, NaN, NaN, 0])

%!error linspace ()
%!error linspace (1, 2, 3, 4)
%!error <N must be a scalar> linspace (1, 2, [3, 4])
%!error <START, END must be scalars or vectors> linspace (ones (2,2), 2, 3)
%!error <START, END must be scalars or vectors> linspace (2, ones (2,2), 3)
%!error <START, END must be scalars or vectors> linspace (1, [], 3)
*/

// FIXME: should accept dimensions as separate args for N-D
// arrays as well as 1-D and 2-D arrays.

DEFUN (resize, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{B} =} resize (@var{A}, @var{m})
@deftypefnx {} {@var{B} =} resize (@var{A}, @var{m}, @var{n}, @dots{})
@deftypefnx {} {@var{B} =} resize (@var{A}, [@var{m} @var{n} @dots{}])
Resize @var{A} cutting off elements as necessary.

In the result, element with certain indices is equal to the corresponding
element of @var{A} if the indices are within the bounds of @var{A}; otherwise,
the element is set to zero.

In other words, the statement

@example
B = resize (A, dv)
@end example

@noindent
is equivalent to the following code:

@example
@group
B = zeros (dv, class (A));
sz = min (dv, size (A));
for i = 1:length (sz)
  idx@{i@} = 1:sz(i);
endfor
B(idx@{:@}) = A(idx@{:@});
@end group
@end example

@noindent
but is performed more efficiently.

If only @var{m} is supplied, and it is a scalar, the dimension of the result is
@var{m}-by-@var{m}.  If @var{m}, @var{n}, @dots{} are all scalars, then the
dimensions of the result are @var{m}-by-@var{n}-by-@dots{}.  If given a vector
as input, then the dimensions of the result are given by the elements of that
vector.

An object can be resized to more dimensions than it has; in such case the
missing dimensions are assumed to be 1.  Resizing an object to fewer dimensions
is not possible.
@seealso{reshape, postpad, prepad, cat}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2)
    print_usage ();

  octave_value retval;

  if (nargin == 2)
    {
      Array<double> vec = args(1).vector_value ();
      int ndim = vec.numel ();
      if (ndim == 1)
        {
          octave_idx_type m = static_cast<octave_idx_type> (vec(0));
          retval = args(0);
          retval = retval.resize (dim_vector (m, m), true);
        }
      else
        {
          dim_vector dv;
          dv.resize (ndim);
          for (int i = 0; i < ndim; i++)
            dv(i) = static_cast<octave_idx_type> (vec(i));
          retval = args(0);
          retval = retval.resize (dv, true);
        }
    }
  else
    {
      dim_vector dv;
      dv.resize (nargin - 1);
      for (octave_idx_type i = 1; i < nargin; i++)
        dv(i-1) = static_cast<octave_idx_type> (args(i).scalar_value ());

      retval = args(0);
      retval = retval.resize (dv, true);
    }

  return retval;
}

// FIXME: should use octave_idx_type for dimensions.

DEFUN (reshape, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{B} =} reshape (@var{A}, @var{m}, @var{n}, @dots{})
@deftypefnx {} {@var{B} =} reshape (@var{A}, [@var{m} @var{n} @dots{}])
@deftypefnx {} {@var{B} =} reshape (@var{A}, @dots{}, [], @dots{})
@deftypefnx {} {@var{B} =} reshape (@var{A}, @var{size})
Return a matrix with the specified dimensions (@var{m}, @var{n}, @dots{})
whose elements are taken from the matrix @var{A}.

The elements of the matrix are accessed in column-major order (like Fortran
arrays are stored).

The following code demonstrates reshaping a 1x4 row vector into a 2x2 square
matrix.

@example
@group
reshape ([1, 2, 3, 4], 2, 2)
      @result{}  1  3
          2  4
@end group
@end example

@noindent
Note that the total number of elements in the original matrix
(@code{prod (size (@var{A}))}) must match the total number of elements
in the new matrix (@code{prod ([@var{m} @var{n} @dots{}])}).

A single dimension of the return matrix may be left unspecified and Octave
will determine its size automatically.  An empty matrix ([]) is used to flag
the unspecified dimension.
@seealso{resize, vec, postpad, cat, squeeze}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2)
    print_usage ();

  octave_value retval;

  dim_vector new_dims;

  if (nargin == 2)
    {
      Array<octave_idx_type> new_size = args(1).octave_idx_type_vector_value ();

      if (new_size.numel () < 2)
        error ("reshape: SIZE must have 2 or more dimensions");

      new_dims = dim_vector::alloc (new_size.numel ());

      for (octave_idx_type i = 0; i < new_size.numel (); i++)
        {
          if (new_size(i) < 0)
            error ("reshape: SIZE must be non-negative");

          new_dims(i) = new_size(i);
        }
    }
  else
    {
      new_dims = dim_vector::alloc (nargin-1);
      int empty_dim = -1;

      for (int i = 1; i < nargin; i++)
        {
          if (args(i).isempty ())
            {
              if (empty_dim > 0)
                error ("reshape: only a single dimension can be unknown");

              empty_dim = i;
              new_dims(i-1) = 1;
            }
          else
            {
              new_dims(i-1) = args(i).idx_type_value ();

              if (new_dims(i-1) < 0)
                error ("reshape: SIZE must be non-negative");
            }
        }

      if (empty_dim > 0)
        {
          octave_idx_type nel = new_dims.numel ();

          if (nel == 0)
            new_dims(empty_dim-1) = 0;
          else
            {
              octave_idx_type a_nel = args(0).numel ();
              octave_idx_type size_empty_dim = a_nel / nel;

              if (a_nel != size_empty_dim * nel)
                error ("reshape: SIZE is not divisible by the product of "
                       "known dimensions (= %" OCTAVE_IDX_TYPE_FORMAT ")",
                       nel);

              new_dims(empty_dim-1) = size_empty_dim;
            }
        }
    }

  retval = args(0).reshape (new_dims);

  return retval;
}

/*
%!assert (size (reshape (ones (4, 4), 2, 8)), [2, 8])
%!assert (size (reshape (ones (4, 4), 8, 2)), [8, 2])
%!assert (size (reshape (ones (15, 4), 1, 60)), [1, 60])
%!assert (size (reshape (ones (15, 4), 60, 1)), [60, 1])

%!assert (size (reshape (ones (4, 4, "single"), 2, 8)), [2, 8])
%!assert (size (reshape (ones (4, 4, "single"), 8, 2)), [8, 2])
%!assert (size (reshape (ones (15, 4, "single"), 1, 60)), [1, 60])
%!assert (size (reshape (ones (15, 4, "single"), 60, 1)), [60, 1])

%!test
%! s.a = 1;
%! fail ("reshape (s, 2, 3)", "can't reshape 1x1 array to 2x3 array");

%!error reshape ()
%!error reshape (1, 2, 3, 4)
%!error <SIZE must have 2 or more dimensions> reshape (1:3, 3)
%!error <SIZE must be non-negative> reshape (1:3, [3 -1])
%!error <only a single dimension can be unknown> reshape (1:3, 1,[],[],3)
%!error <SIZE must be non-negative> reshape (1:3, 3, -1)
%!error <SIZE is not divisible> reshape (1:3, 3, [], 2)
*/

DEFUN (vec, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{v} =} vec (@var{x})
@deftypefnx {} {@var{v} =} vec (@var{x}, @var{dim})
Return the vector obtained by stacking the columns of the matrix @var{x}
one above the other.

Without @var{dim} this is equivalent to @code{@var{x}(:)}.

If @var{dim} is supplied, the dimensions of @var{v} are set to @var{dim}
with all elements along the last dimension.  This is equivalent to
@code{shiftdim (@var{x}(:), 1-@var{dim})}.
@seealso{vech, resize, cat}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  int dim = 1;
  if (nargin == 2)
    {
      dim = args(1).idx_type_value ();

      if (dim < 1)
        error ("vec: DIM must be greater than zero");
    }

  octave_value colon (octave_value::magic_colon_t);
  octave_value arg = args(0);

  octave_value retval = arg.single_subsref ("(", colon);

  if (dim > 1)
    {
      dim_vector new_dims = dim_vector::alloc (dim);

      for (int i = 0; i < dim-1; i++)
        new_dims(i) = 1;

      new_dims(dim-1) = retval.numel ();

      retval = retval.reshape (new_dims);
    }

  return retval;
}

/*
%!assert (vec ([1, 2; 3, 4]), [1; 3; 2; 4])
%!assert (vec ([1, 3, 2, 4]), [1; 3; 2; 4])
%!assert (vec ([1, 2, 3, 4], 2), [1, 2, 3, 4])
%!assert (vec ([1, 2; 3, 4]), vec ([1, 2; 3, 4], 1))
%!assert (vec ([1, 2; 3, 4], 1), [1; 3; 2; 4])
%!assert (vec ([1, 2; 3, 4], 2), [1, 3, 2, 4])
%!assert (vec ([1, 3; 2, 4], 3), reshape ([1, 2, 3, 4], 1, 1, 4))
%!assert (vec ([1, 3; 2, 4], 3), shiftdim (vec ([1, 3; 2, 4]), -2))

%!error vec ()
%!error vec (1, 2, 3)
%!error vec ([1, 2; 3, 4], 0)
*/

DEFUN (squeeze, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{B} =} squeeze (@var{A})
Remove singleton dimensions from @var{A} and return the result.

Note that for compatibility with @sc{matlab}, all objects have
a minimum of two dimensions and row vectors are left unchanged.
@seealso{reshape}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).squeeze ());
}

DEFUN (full, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{FM} =} full (@var{SM})
Return a full storage matrix from a sparse, diagonal, or permutation matrix,
or from a range.
@seealso{sparse, issparse}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).full_value ());
}

// Compute various norms of the vector X.

DEFUN (norm, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{n} =} norm (@var{A})
@deftypefnx {} {@var{n} =} norm (@var{A}, @var{p})
@deftypefnx {} {@var{n} =} norm (@var{A}, @var{p}, @var{opt})
Compute the p-norm of the matrix @var{A}.

If the second argument is not given, @w{@code{p = 2}} is used.

If @var{A} is a matrix (or sparse matrix):

@table @asis
@item @var{p} = @code{1}
1-norm, the largest column sum of the absolute values of @var{A}.

@item @var{p} = @code{2}
Largest singular value of @var{A}.

@item @var{p} = @code{Inf} or @qcode{"inf"}
@cindex infinity norm
Infinity norm, the largest row sum of the absolute values of @var{A}.

@item @var{p} = @qcode{"fro"}
@cindex @nospell{Frobenius} norm
@nospell{Frobenius} norm of @var{A},
@code{sqrt (sum (diag (@var{A}' * @var{A})))}.

@item other @var{p}, @code{@var{p} > 1}
@cindex general p-norm
maximum @code{norm (A*x, p)} such that @code{norm (x, p) == 1}
@end table

If @var{A} is a vector or a scalar:

@table @asis
@item @var{p} = @code{Inf} or @qcode{"inf"}
@code{max (abs (@var{A}))}.

@item @var{p} = @code{-Inf}
@code{min (abs (@var{A}))}.

@item @var{p} = @qcode{"fro"}
@nospell{Frobenius} norm of @var{A}, @code{sqrt (sumsq (abs (A)))}.

@item @var{p} = 0
Hamming norm---the number of nonzero elements.

@item other @var{p}, @code{@var{p} > 1}
p-norm of @var{A}, @code{(sum (abs (@var{A}) .^ @var{p})) ^ (1/@var{p})}.

@item other @var{p} @code{@var{p} < 1}
the p-pseudonorm defined as above.
@end table

If @var{opt} is the value @qcode{"rows"}, treat each row as a vector and
compute its norm.  The result is returned as a column vector.
Similarly, if @var{opt} is @qcode{"columns"} or @qcode{"cols"} then
compute the norms of each column and return a row vector.
@seealso{normest, normest1, vecnorm, cond, svd}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    print_usage ();

  octave_value x_arg = args(0);

  if (x_arg.ndims () != 2)
    error ("norm: only valid for 2-D objects");

  enum {sfmatrix, sfcols, sfrows, sffrob, sfinf, sfneginf} strflag = sfmatrix;
  if (nargin > 1 && args(nargin-1).is_string ())
    {
      std::string str = args(nargin-1).string_value ();
      std::transform (str.begin (), str.end (), str.begin (), tolower);
      if (str == "cols" || str == "columns")
        strflag = sfcols;
      else if (str == "rows")
        strflag = sfrows;
      else if (str == "fro")
        strflag = sffrob;
      else if (str == "inf")
        strflag = sfinf;
      else if (str == "-inf")
        strflag = sfneginf;
      else
        error ("norm: unrecognized option: %s", str.c_str ());

      // we've handled the last parameter, so act as if it was removed
      nargin--;
    }

  octave_value p_arg = (nargin > 1) ? args(1) : octave_value (2);

  if (p_arg.isempty ())
    p_arg = octave_value (2);
  else if (p_arg.is_string ())
    {
      std::string str = p_arg.string_value ();
      std::transform (str.begin (), str.end (), str.begin (), tolower);
      if (strflag != sfcols && strflag != sfrows)
        error ("norm: invalid combination of options");

      if (str == "cols" || str == "columns" || str == "rows")
        error ("norm: invalid combination of options");

      if (str == "fro")
        p_arg = octave_value (2);
      else if (str == "inf")
        p_arg = numeric_limits<double>::Inf ();
      else if (str == "-inf")
        p_arg = -numeric_limits<double>::Inf ();
      else
        error ("norm: unrecognized option: %s", str.c_str ());
    }
  else if (! p_arg.is_scalar_type ())
    err_wrong_type_arg ("norm", p_arg);

  octave_value retval;

  switch (strflag)
    {
    case sfmatrix:
      retval = xnorm (x_arg, p_arg);
      break;

    case sfcols:
      retval = xcolnorms (x_arg, p_arg);
      break;

    case sfrows:
      retval = xrownorms (x_arg, p_arg);
      break;

    case sffrob:
      retval = xfrobnorm (x_arg);
      break;

    case sfinf:
      retval = xnorm (x_arg, numeric_limits<double>::Inf ());
      break;

    case sfneginf:
      retval = xnorm (x_arg, -numeric_limits<double>::Inf ());
      break;
    }

  return retval;
}

/*
%!shared x
%! x = [1, -3, 4, 5, -7];
%!assert (norm (x,0), 5)
%!assert (norm (x,1), 20)
%!assert (norm (x,2), 10)
%!assert (norm (x,3), 8.24257059961711, -4*eps)
%!assert (norm (x,Inf), 7)
%!assert (norm (x,-Inf), 1)
%!assert (norm (x,"inf"), 7)
%!assert (norm (x,"-Inf"), 1)
%!assert (norm (x,"fro"), 10, -eps)
%!assert (norm (x), 10)
%!assert (norm ([1e200, 1]), 1e200)
%!assert (norm ([3+4i, 3-4i, sqrt(31)]), 9, -4*eps)
%!shared m
%! m = magic (4);
%!assert (norm (m,1), 34)
%!assert (norm (m,2), 34, -eps)
%!assert (norm (m,3), 34, -sqrt (eps))
%!assert (norm (m,Inf), 34)
%!assert (norm (m,"inf"), 34)
%!shared m2, flo, fhi
%! m2 = [1,2;3,4];
%! flo = 1e-300;
%! fhi = 1e+300;
%!assert (norm (flo*m2,"fro"), sqrt (30)*flo, -eps)
%!assert (norm (fhi*m2,"fro"), sqrt (30)*fhi, -eps)

%!shared x
%! x = single ([1, -3, 4, 5, -7]);
%!assert (norm (x,0), single (5))
%!assert (norm (x,1), single (20))
%!assert (norm (x,2), single (10))
%!assert (norm (x,3), single (8.24257059961711), -4* eps ("single"))
%!assert (norm (x,Inf), single (7))
%!assert (norm (x,-Inf), single (1))
%!assert (norm (x,"inf"), single (7))
%!assert (norm (x,"-Inf"), single (1))
%!assert (norm (x,"fro"), single (10), -eps ("single"))
%!assert (norm (x), single (10))
%!assert (norm (single ([1e38, 1])), single (1e38))
%!assert (norm (single ([3+4i, 3-4i, sqrt(31)])), single (9), -4* eps ("single"))
%!shared m
%! m = single (magic (4));
%!assert (norm (m,1), single (34))
%!assert (norm (m,2), single (34), -eps ("single"))
%!assert (norm (m,3), single (34), -sqrt (eps ("single")))
%!assert (norm (m,Inf), single (34))
%!assert (norm (m,"inf"), single (34))
%!shared m2, flo, fhi
%! m2 = single ([1,2;3,4]);
%! flo = single (1e-300);
%! fhi = single (1e+300);
%!assert (norm (flo*m2,"fro"), single (sqrt (30)*flo), -eps ("single"))
%!assert (norm (fhi*m2,"fro"), single (sqrt (30)*fhi), -eps ("single"))

## Hamming norm (p == 0)
%!assert (norm ([1, 0, 0, 0, 1], 0), 2)

%!shared q
%! q = rand (1e3, 3);
%!assert (norm (q, 3, "rows"), sum (q.^3, 2).^(1/3), sqrt (eps))
%!assert (norm (q, "fro", "rows"), sum (q.^2, 2).^(1/2), sqrt (eps))
%!assert (norm (q, "fro", "rows"), sqrt (sumsq (q, 2)), sqrt (eps))
%!assert (norm (q, "fro", "cols"), sqrt (sumsq (q, 1)), sqrt (eps))
%!assert (norm (q, 3, "cols"), sum (q.^3, 1).^(1/3), sqrt (eps))
%!assert (norm (q, "inf", "rows"), norm (q, Inf, "rows"))
%!assert (norm (q, "inf", "cols"), norm (q, Inf, "cols"))
%!assert (norm (q, [], "rows"), norm (q, 2, "rows"))
%!assert (norm (q, [], "cols"), norm (q, 2, "cols"))

%!test <30631>
%! ## Test for norm returning NaN on sparse matrix
%! A = sparse (2,2);
%! A(2,1) = 1;
%! assert (norm (A), 1);

## Test input validation
%!error norm ()
%!error norm (1,2,3,4)
%!error <unrecognized option> norm (1, "invalid")
%!error <unrecognized option> norm (1, "rows", "invalid")
%!error <unrecognized option> norm (1, "invalid", "rows")
%!error <invalid combination of options> norm (1, "cols", "rows")
%!error <invalid combination of options> norm (1, "rows", "rows")
%!error <p must be .= 1> norm (ones (2,2), -Inf)
*/

static octave_value
unary_op_defun_body (octave_value::unary_op op,
                     const octave_value_list& args)
{
  if (args.length () != 1)
    print_usage ();

  return unary_op (op, args(0));
}

DEFUN (not, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{z} =} not (@var{x})
Return the logical NOT of @var{x}.

This function is equivalent to the operator syntax @w{@code{! @var{x}}}.
@seealso{and, or, xor}
@end deftypefn */)
{
  return unary_op_defun_body (octave_value::op_not, args);
}

DEFUN (uplus, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{B} =} uplus (@var{A})
This function and @w{@tcode{+ @var{A}}} are equivalent.
@seealso{uminus, plus}
@end deftypefn */)
{
  return unary_op_defun_body (octave_value::op_uplus, args);
}

DEFUN (uminus, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{B} =} uminus (@var{A})
This function and @w{@tcode{- @var{A}}} are equivalent.
@seealso{uplus, minus}
@end deftypefn */)
{
  return unary_op_defun_body (octave_value::op_uminus, args);
}

DEFUN (transpose, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{B} =} transpose (@var{A})
Return the transpose of @var{A}.

This function and @tcode{@var{A}.'@:} are equivalent.
@seealso{ctranspose}
@end deftypefn */)
{
  return unary_op_defun_body (octave_value::op_transpose, args);
}

/*
%!assert (2.', 2)
%!assert (2i.', 2i)
%!assert ([1:4].', [1;2;3;4])
%!assert ([1;2;3;4].', [1:4])
%!assert ([1,2;3,4].', [1,3;2,4])
%!assert ([1,2i;3,4].', [1,3;2i,4])

%!assert (transpose ([1,2;3,4]), [1,3;2,4])

%!assert (single (2).', single (2))
%!assert (single (2i).', single (2i))
%!assert (single ([1:4]).', single ([1;2;3;4]))
%!assert (single ([1;2;3;4]).', single ([1:4]))
%!assert (single ([1,2;3,4]).', single ([1,3;2,4]))
%!assert (single ([1,2i;3,4]).', single ([1,3;2i,4]))

%!assert (transpose (single ([1,2;3,4])), single ([1,3;2,4]))
*/

DEFUN (ctranspose, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{B} =} ctranspose (@var{A})
Return the complex conjugate transpose of @var{A}.

This function and @tcode{@var{A}'} are equivalent.
@seealso{transpose}
@end deftypefn */)
{
  return unary_op_defun_body (octave_value::op_hermitian, args);
}

/*
%!assert (2', 2)
%!assert (2i', -2i)
%!assert ([1:4]', [1;2;3;4])
%!assert ([1;2;3;4]', [1:4])
%!assert ([1,2;3,4]', [1,3;2,4])
%!assert ([1,2i;3,4]', [1,3;-2i,4])

%!assert (ctranspose ([1,2i;3,4]), [1,3;-2i,4])

%!assert (single (2)', single (2))
%!assert (single (2i)', single (-2i))
%!assert (single ([1:4])', single ([1;2;3;4]))
%!assert (single ([1;2;3;4])', single ([1:4]))
%!assert (single ([1,2;3,4])', single ([1,3;2,4]))
%!assert (single ([1,2i;3,4])', single ([1,3;-2i,4]))

%!assert (ctranspose (single ([1,2i;3,4])), single ([1,3;-2i,4]))
*/

static octave_value
binary_op_defun_body (octave_value::binary_op op,
                      const octave_value_list& args)
{
  if (args.length () != 2)
    print_usage ();

  return binary_op (op, args(0), args(1));
}

static octave_value
binary_assoc_op_defun_body (octave_value::binary_op op,
                            octave_value::assign_op aop,
                            const octave_value_list& args)
{
  int nargin = args.length ();

  if (nargin < 2)
    print_usage ();

  octave_value retval;

  if (nargin == 2)
    retval = binary_op (op, args(0), args(1));
  else
    {
      retval = binary_op (op, args(0), args(1));

      for (int i = 2; i < nargin; i++)
        retval.assign (aop, args(i));
    }

  return retval;
}

DEFUN (plus, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{C} =} plus (@var{A}, @var{B})
@deftypefnx {} {@var{C} =} plus (@var{A1}, @var{A2}, @dots{})
This function and @w{@tcode{@var{A} + @var{B}}} are equivalent.

If more arguments are given, the summation is applied
cumulatively from left to right:

@example
(@dots{}((@var{A1} + @var{A2}) + @var{A3}) + @dots{})
@end example

@seealso{minus, uplus}
@end deftypefn */)
{
  return binary_assoc_op_defun_body (octave_value::op_add,
                                     octave_value::op_add_eq, args);
}

/*
%!assert (plus (1,1), 2)
%!assert (plus (1:3, 1), 2:4)
%!assert (plus (1:3, 1, 3), 5:7)
%!assert (plus (1,2,3,4,5,6,7,8,9), sum (1:9))

## Test input validation for all functions which use binary_assoc_op_defun_body
%!error plus ()
%!error plus (1)
*/

DEFUN (minus, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{C} =} minus (@var{A}, @var{B})
This function and @w{@tcode{@var{A} - @var{B}}} are equivalent.
@seealso{plus, uminus}
@end deftypefn */)
{
  return binary_op_defun_body (octave_value::op_sub, args);
}

DEFUN (mtimes, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{C} =} mtimes (@var{A}, @var{B})
@deftypefnx {} {@var{C} =} mtimes (@var{A1}, @var{A2}, @dots{})
Return the matrix multiplication product of inputs.

This function and @w{@tcode{@var{A} * @var{B}}} are equivalent.
If more arguments are given, the multiplication is applied
cumulatively from left to right:

@example
(@dots{}((@var{A1} * @var{A2}) * @var{A3}) * @dots{})
@end example

@seealso{times, plus, minus, rdivide, mrdivide, mldivide, mpower}
@end deftypefn */)
{
  return binary_assoc_op_defun_body (octave_value::op_mul,
                                     octave_value::op_mul_eq, args);
}

DEFUN (mrdivide, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{C} =} mrdivide (@var{A}, @var{B})
Return the matrix right division of @var{A} and @var{B}.

This function and @w{@tcode{@var{A} / @var{B}}} are equivalent.

If the system is not square, or if the coefficient matrix is singular, a
minimum norm solution is computed.
@seealso{mldivide, rdivide, plus, minus}
@end deftypefn */)
{
  return binary_op_defun_body (octave_value::op_div, args);
}

DEFUN (mpower, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{C} =} mpower (@var{A}, @var{B})
Return the matrix power operation of @var{A} raised to the @var{B} power.

This function and @w{@tcode{@var{A} ^ @var{B}}} are equivalent.
@seealso{power, mtimes, plus, minus}
@end deftypefn */)
{
  return binary_op_defun_body (octave_value::op_pow, args);
}

DEFUN (mldivide, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{C} =} mldivide (@var{A}, @var{B})
Return the matrix left division of @var{A} and @var{B}.

This function and @w{@tcode{@var{A} @backslashchar{} @var{B}}} are equivalent.

If the system is not square, or if the coefficient matrix is singular, a
minimum norm solution is computed.
@seealso{mrdivide, ldivide, rdivide, linsolve}
@end deftypefn */)
{
  return binary_op_defun_body (octave_value::op_ldiv, args);
}

DEFUN (lt, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{TF} =} lt (@var{A}, @var{B})
This function is equivalent to @w{@code{@var{A} < @var{B}}}.
@seealso{le, eq, ge, gt, ne}
@end deftypefn */)
{
  return binary_op_defun_body (octave_value::op_lt, args);
}

DEFUN (le, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{TF} =} le (@var{A}, @var{B})
This function is equivalent to @w{@code{@var{A} <= @var{B}}}.
@seealso{eq, ge, gt, ne, lt}
@end deftypefn */)
{
  return binary_op_defun_body (octave_value::op_le, args);
}

DEFUN (eq, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{TF} =} eq (@var{A}, @var{B})
Return true if the two inputs are equal.

This function is equivalent to @w{@code{@var{A} == @var{B}}}.
@seealso{ne, isequal, le, ge, gt, ne, lt}
@end deftypefn */)
{
  return binary_op_defun_body (octave_value::op_eq, args);
}

DEFUN (ge, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{TF} =} ge (@var{A}, @var{B})
This function is equivalent to @w{@code{@var{A} >= @var{B}}}.
@seealso{le, eq, gt, ne, lt}
@end deftypefn */)
{
  return binary_op_defun_body (octave_value::op_ge, args);
}

DEFUN (gt, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{TF} =} gt (@var{A}, @var{B})
This function is equivalent to @w{@code{@var{A} > @var{B}}}.
@seealso{le, eq, ge, ne, lt}
@end deftypefn */)
{
  return binary_op_defun_body (octave_value::op_gt, args);
}

DEFUN (ne, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{TF} =} ne (@var{A}, @var{B})
Return true if the two inputs are not equal.

This function is equivalent to @w{@code{@var{A} != @var{B}}}.
@seealso{eq, isequal, le, ge, lt}
@end deftypefn */)
{
  return binary_op_defun_body (octave_value::op_ne, args);
}

DEFUN (times, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{C} =} times (@var{A}, @var{B})
@deftypefnx {} {@var{C} =} times (@var{A1}, @var{A2}, @dots{})
Return the element-by-element multiplication product of inputs.

This function and @w{@tcode{@var{A} .* @var{B}}} are equivalent.
If more arguments are given, the multiplication is applied
cumulatively from left to right:

@example
(@dots{}((@var{A1} .* @var{A2}) .* @var{A3}) .* @dots{})
@end example

@seealso{mtimes, rdivide}
@end deftypefn */)
{
  return binary_assoc_op_defun_body (octave_value::op_el_mul,
                                     octave_value::op_el_mul_eq, args);
}

DEFUN (rdivide, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{C} =} rdivide (@var{A}, @var{B})
Return the element-by-element right division of @var{A} and @var{B}.

This function and @w{@tcode{@var{A} ./ @var{B}}} are equivalent.
@seealso{ldivide, mrdivide, times, plus}
@end deftypefn */)
{
  return binary_op_defun_body (octave_value::op_el_div, args);
}

DEFUN (power, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{C} =} power (@var{A}, @var{B})
Return the element-by-element operation of @var{A} raised to the
@var{B} power.

This function and @w{@tcode{@var{A} .^ @var{B}}} are equivalent.

If several complex results are possible, returns the one with smallest
non-negative argument (angle).  Use @code{realpow}, @code{realsqrt},
@code{cbrt}, or @code{nthroot} if a real result is preferred.

@seealso{mpower, realpow, realsqrt, cbrt, nthroot}
@end deftypefn */)
{
  return binary_op_defun_body (octave_value::op_el_pow, args);
}

DEFUN (ldivide, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{C} =} ldivide (@var{A}, @var{B})
Return the element-by-element left division of @var{A} and @var{B}.

This function and @w{@tcode{@var{A} .@backslashchar{} @var{B}}} are
equivalent.
@seealso{rdivide, mldivide, times, plus}
@end deftypefn */)
{
  return binary_op_defun_body (octave_value::op_el_ldiv, args);
}

DEFUN (and, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{TF} =} and (@var{x}, @var{y})
@deftypefnx {} {@var{TF} =} and (@var{x1}, @var{x2}, @dots{})
Return the logical AND of @var{x} and @var{y}.

This function is equivalent to the operator syntax
@w{@code{@var{x} & @var{y}}}.  If more than two arguments are given, the
logical AND is applied cumulatively from left to right:

@example
(@dots{}((@var{x1} & @var{x2}) & @var{x3}) & @dots{})
@end example

@seealso{or, not, xor}
@end deftypefn */)
{
  return binary_assoc_op_defun_body (octave_value::op_el_and,
                                     octave_value::op_el_and_eq, args);
}

DEFUN (or, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{TF} =} or (@var{x}, @var{y})
@deftypefnx {} {@var{TF} =} or (@var{x1}, @var{x2}, @dots{})
Return the logical OR of @var{x} and @var{y}.

This function is equivalent to the operator syntax
@w{@code{@var{x} | @var{y}}}.  If more than two arguments are given, the
logical OR is applied cumulatively from left to right:

@example
(@dots{}((@var{x1} | @var{x2}) | @var{x3}) | @dots{})
@end example

@seealso{and, not, xor}
@end deftypefn */)
{
  return binary_assoc_op_defun_body (octave_value::op_el_or,
                                     octave_value::op_el_or_eq, args);
}

DEFUN (colon, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{r} =} colon (@var{base}, @var{limit})
@deftypefnx {} {@var{r} =} colon (@var{base}, @var{increment}, @var{limit})
Return the result of the colon expression corresponding to @var{base},
@var{limit}, and optionally, @var{increment}.

This function is equivalent to the operator syntax
@w{@code{@var{base} : @var{limit}}} or
@w{@code{@var{base} : @var{increment} : @var{limit}}}.
@seealso{linspace}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    print_usage ();

  return (nargin == 2
          ? colon_op (args(0), args(1))
          : colon_op (args(0), args(1), args(2)));
}

static double tic_toc_timestamp = -1.0;

DEFUN (tic, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} tic ()
@deftypefnx {} {@var{id} =} tic ()
Initialize a wall-clock timer.

Calling @code{tic} without an output argument resets the internal timer.
Subsequent calls to @code{toc} return the number of seconds since the timer was
set.

If called with one output argument, @code{tic} creates a new timer instance and
returns a timer identifier @var{id}.  The @var{id} is a scalar of type
@code{uint64} that may be passed to @code{toc} to check elapsed time on this
timer, rather than the default internal timer.

Example 1 : benchmarking code with internal timer

@example
@group
tic;
# many computations later@dots{}
elapsed_time = toc;
@end group
@end example

Example 2 : mixed timer id and internal timer

@example
@group
tic;
pause (1);
toc
@result{} Elapsed time is 1.0089 seconds.
id = tic;
pause (2);
toc (id)
@result{} Elapsed time is 2.01142 seconds.
toc
Elapsed time is 3.02308 seconds.
@end group
@end example

@noindent
Calling @code{tic} and @code{toc} in this way allows nested timing calls.

If you are more interested in the CPU time that your process used, you should
use the @code{cputime} function instead.  The @code{tic} and @code{toc}
functions report the actual wall clock time that elapsed between the calls.
This may include time spent processing other jobs or doing nothing at all.
@seealso{toc, cputime}
@end deftypefn */)
{
  if (args.length () != 0)
    warning ("tic: ignoring extra arguments");

  octave_value retval;
  sys::time now;
  double tmp = now.double_value ();

  if (nargout > 0)
    {
      double ip = 0.0;
      double frac = std::modf (tmp, &ip);
      uint64_t microsecs = static_cast<uint64_t> (CLOCKS_PER_SEC * frac);
      microsecs += CLOCKS_PER_SEC * static_cast<uint64_t> (ip);
      retval = octave_uint64 (microsecs);
    }
  else
    tic_toc_timestamp = tmp;

  return retval;
}

DEFUN (toc, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} toc ()
@deftypefnx {} {} toc (@var{id})
@deftypefnx {} {@var{elapsed_time} =} toc (@dots{})
Measure elapsed time on a wall-clock timer.

With no arguments, return the number of seconds elapsed on the internal timer
since the last call to @code{tic}.

When given the identifier @var{id} of a specific timer, return the number of
seconds elapsed since the timer @var{id} was initialized.

@xref{XREFtic,,@code{tic}}, for examples of the use of @code{tic}/@code{toc}.

@seealso{tic, cputime}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  double start_time = tic_toc_timestamp;

  if (nargin == 1)
    {
      octave_uint64 id = args(0).xuint64_scalar_value ("toc: invalid ID");

      uint64_t val = id.value ();

      start_time
        = (static_cast<double> (val / CLOCKS_PER_SEC)
           + static_cast<double> (val % CLOCKS_PER_SEC)
           / CLOCKS_PER_SEC);

      // FIXME: should we also check to see whether the start
      //        time is after the beginning of this Octave session?
    }

  if (start_time < 0)
    error ("toc: function called before timer initialization with tic()");

  sys::time now;

  double etime = now.double_value () - start_time;

  octave_value retval;
  if (nargout > 0)
    retval = etime;
  else
    octave_stdout << "Elapsed time is " << etime << " seconds.\n";

  return retval;
}

/*
%!shared id
%! id = tic ();
%!assert (isa (id, "uint64"))
%!assert (isa (toc (id), "double"))
*/

DEFUN (cputime, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {[@var{total}, @var{user}, @var{system}] =} cputime ();
Return the CPU time used by your Octave session.

The first output is the total time spent executing your process and is equal
to the sum of second and third outputs, which are the number of CPU seconds
spent executing in user mode and the number of CPU seconds spent executing
in system mode, respectively.

If your system does not have a way to report CPU time usage, @code{cputime}
returns 0 for each of its output values.

Note that because Octave used some CPU time to start, it is reasonable
to check to see if @code{cputime} works by checking to see if the total
CPU time used is nonzero.
@seealso{tic, toc}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  sys::cpu_time cpu_tm;

  double usr = cpu_tm.user ();
  double sys = cpu_tm.system ();

  return ovl (usr + sys, usr, sys);
}

DEFUN (sort, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {[@var{s}, @var{i}] =} sort (@var{x})
@deftypefnx {} {[@var{s}, @var{i}] =} sort (@var{x}, @var{dim})
@deftypefnx {} {[@var{s}, @var{i}] =} sort (@var{x}, @var{mode})
@deftypefnx {} {[@var{s}, @var{i}] =} sort (@var{x}, @var{dim}, @var{mode})
Return a copy of @var{x} with the elements arranged in increasing order.

For matrices, @code{sort} orders the elements within columns

For example:

@example
@group
sort ([1, 2; 2, 3; 3, 1])
   @result{}  1  1
       2  2
       3  3
@end group
@end example

If the optional argument @var{dim} is given, then the matrix is sorted
along the dimension defined by @var{dim}.  The optional argument @var{mode}
defines the order in which the values will be sorted.  Valid values of
@var{mode} are @qcode{"ascend"} or @qcode{"descend"}.

The @code{sort} function may also be used to produce a matrix
containing the original row indices of the elements in the sorted
matrix.  For example:

@example
@group
[s, i] = sort ([1, 2; 2, 3; 3, 1])
  @result{} s = 1  1
         2  2
         3  3
  @result{} i = 1  3
         2  1
         3  2
@end group
@end example

For equal elements, the indices are such that equal elements are listed
in the order in which they appeared in the original list.

Sorting of complex entries is done first by magnitude
(@w{@code{abs (@var{z})}}) and for any ties by phase angle
(@w{@code{angle (z)}}).  For example:

@example
@group
sort ([1+i; 1; 1-i])
    @result{} 1 + 0i
       1 - 1i
       1 + 1i
@end group
@end example

NaN values are treated as being greater than any other value and are sorted
to the end of the list.

The @code{sort} function may also be used to sort strings and cell arrays
of strings, in which case ASCII dictionary order (uppercase 'A' precedes
lowercase 'a') of the strings is used.

The algorithm used in @code{sort} is optimized for the sorting of partially
ordered lists.
@seealso{sortrows, issorted}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    print_usage ();

  sortmode smode = ASCENDING;
  bool return_idx = (nargout > 1);
  bool have_sortmode = (nargin > 1 && args(1).is_string ());
  octave_value arg = args(0);

  int dim = 0;
  if (nargin > 1)
    {
      if (have_sortmode)
        {
          std::string mode = args(1).string_value ();
          if (mode == "ascend")
            smode = ASCENDING;
          else if (mode == "descend")
            smode = DESCENDING;
          else
            error (R"(sort: MODE must be either "ascend" or "descend")");
        }
      else
        dim = args(1).nint_value () - 1;
    }

  if (nargin > 2)
    {
      if (have_sortmode)
        error ("sort: DIM must be a valid dimension");

      std::string mode = args(2).xstring_value ("sort: MODE must be a string");

      if (mode == "ascend")
        smode = ASCENDING;
      else if (mode == "descend")
        smode = DESCENDING;
      else
        error (R"(sort: MODE must be either "ascend" or "descend")");
    }

  const dim_vector dv = arg.dims ();
  if (nargin == 1 || have_sortmode)
    {
      dim = dv.first_non_singleton ();
    }
  else
    {
      if (dim < 0)
        error ("sort: DIM must be a valid dimension");
    }

  octave_value_list retval (return_idx ? 2 : 1);

  if (return_idx)
    {
      Array<octave_idx_type> sidx;

      // NOTE: Can not change this to ovl() call because arg.sort changes sidx
      //       and objects are declared const in ovl prototype.
      retval(0) = arg.sort (sidx, dim, smode);
      retval(1) = idx_vector (sidx, dv(dim));  // No checking, extent is known.
    }
  else
    retval = ovl (arg.sort (dim, smode));

  return retval;
}

/*
## Double
%!assert (sort ([NaN, 1, -1, 2, Inf]), [-1, 1, 2, Inf, NaN])
%!assert (sort ([NaN, 1, -1, 2, Inf], 1), [NaN, 1, -1, 2, Inf])
%!assert (sort ([NaN, 1, -1, 2, Inf], 2), [-1, 1, 2, Inf, NaN])
%!assert (sort ([NaN, 1, -1, 2, Inf], 3), [NaN, 1, -1, 2, Inf])
%!assert (sort ([NaN, 1, -1, 2, Inf], "ascend"), [-1, 1, 2, Inf, NaN])
%!assert (sort ([NaN, 1, -1, 2, Inf], 2, "ascend"), [-1, 1, 2, Inf, NaN])
%!assert (sort ([NaN, 1, -1, 2, Inf], "descend"), [NaN, Inf, 2, 1, -1])
%!assert (sort ([NaN, 1, -1, 2, Inf], 2, "descend"), [NaN, Inf, 2, 1, -1])
%!assert (sort ([3, 1, 7, 5; 8, 2, 6, 4]), [3, 1, 6, 4; 8, 2, 7, 5])
%!assert (sort ([3, 1, 7, 5; 8, 2, 6, 4], 1), [3, 1, 6, 4; 8, 2, 7, 5])
%!assert (sort ([3, 1, 7, 5; 8, 2, 6, 4], 2), [1, 3, 5, 7; 2, 4, 6, 8])
%!assert (sort (1), 1)

%!test
%! [v, i] = sort ([NaN, 1, -1, Inf, 1]);
%! assert (v, [-1, 1, 1, Inf, NaN]);
%! assert (i, [3, 2, 5, 4, 1]);

## Complex
%!assert (sort ([NaN, 1i, -1, 2, Inf]), [1i, -1, 2, Inf, NaN])
%!assert (sort ([NaN, 1i, -1, 2, Inf], 1), [NaN, 1i, -1, 2, Inf])
%!assert (sort ([NaN, 1i, -1, 2, Inf], 2), [1i, -1, 2, Inf, NaN])
%!assert (sort ([NaN, 1i, -1, 2, Inf], 3), [NaN, 1i, -1, 2, Inf])
%!assert (sort ([NaN, 1i, -1, 2, Inf], "ascend"), [1i, -1, 2, Inf, NaN])
%!assert (sort ([NaN, 1i, -1, 2, Inf], 2, "ascend"), [1i, -1, 2, Inf, NaN])
%!assert (sort ([NaN, 1i, -1, 2, Inf], "descend"), [NaN, Inf, 2, -1, 1i])
%!assert (sort ([NaN, 1i, -1, 2, Inf], 2, "descend"), [NaN, Inf, 2, -1, 1i])
%!assert (sort ([3, 1i, 7, 5; 8, 2, 6, 4]), [3, 1i, 6, 4; 8, 2, 7, 5])
%!assert (sort ([3, 1i, 7, 5; 8, 2, 6, 4], 1), [3, 1i, 6, 4; 8, 2, 7, 5])
%!assert (sort ([3, 1i, 7, 5; 8, 2, 6, 4], 2), [1i, 3, 5, 7; 2, 4, 6, 8])
%!assert (sort (1i), 1i)

%!test
%! [v, i] = sort ([NaN, 1i, -1, Inf, 1, 1i]);
%! assert (v, [1, 1i, 1i, -1, Inf, NaN]);
%! assert (i, [5, 2, 6, 3, 4, 1]);

## Single
%!assert (sort (single ([NaN, 1, -1, 2, Inf])), single ([-1, 1, 2, Inf, NaN]))
%!assert (sort (single ([NaN, 1, -1, 2, Inf]), 1),
%!        single ([NaN, 1, -1, 2, Inf]))
%!assert (sort (single ([NaN, 1, -1, 2, Inf]), 2),
%!        single ([-1, 1, 2, Inf, NaN]))
%!assert (sort (single ([NaN, 1, -1, 2, Inf]), 3),
%!        single ([NaN, 1, -1, 2, Inf]))
%!assert (sort (single ([NaN, 1, -1, 2, Inf]), "ascend"),
%!        single ([-1, 1, 2, Inf, NaN]))
%!assert (sort (single ([NaN, 1, -1, 2, Inf]), 2, "ascend"),
%!        single ([-1, 1, 2, Inf, NaN]))
%!assert (sort (single ([NaN, 1, -1, 2, Inf]), "descend"),
%!        single ([NaN, Inf, 2, 1, -1]))
%!assert (sort (single ([NaN, 1, -1, 2, Inf]), 2, "descend"),
%!        single ([NaN, Inf, 2, 1, -1]))
%!assert (sort (single ([3, 1, 7, 5; 8, 2, 6, 4])),
%!        single ([3, 1, 6, 4; 8, 2, 7, 5]))
%!assert (sort (single ([3, 1, 7, 5; 8, 2, 6, 4]), 1),
%!        single ([3, 1, 6, 4; 8, 2, 7, 5]))
%!assert (sort (single ([3, 1, 7, 5; 8, 2, 6, 4]), 2),
%!        single ([1, 3, 5, 7; 2, 4, 6, 8]))
%!assert (sort (single (1)), single (1))

%!test
%! [v, i] = sort (single ([NaN, 1, -1, Inf, 1]));
%! assert (v, single ([-1, 1, 1, Inf, NaN]));
%! assert (i, [3, 2, 5, 4, 1]);

## Single Complex
%!assert (sort (single ([NaN, 1i, -1, 2, Inf])), single ([1i, -1, 2, Inf, NaN]))
%!assert (sort (single ([NaN, 1i, -1, 2, Inf]), 1),
%!        single ([NaN, 1i, -1, 2, Inf]))
%!assert (sort (single ([NaN, 1i, -1, 2, Inf]), 2),
%!        single ([1i, -1, 2, Inf, NaN]))
%!assert (sort (single ([NaN, 1i, -1, 2, Inf]), 3),
%!        single ([NaN, 1i, -1, 2, Inf]))
%!assert (sort (single ([NaN, 1i, -1, 2, Inf]), "ascend"),
%!        single ([1i, -1, 2, Inf, NaN]))
%!assert (sort (single ([NaN, 1i, -1, 2, Inf]), 2, "ascend"),
%!        single ([1i, -1, 2, Inf, NaN]))
%!assert (sort (single ([NaN, 1i, -1, 2, Inf]), "descend"),
%!        single ([NaN, Inf, 2, -1, 1i]))
%!assert (sort (single ([NaN, 1i, -1, 2, Inf]), 2, "descend"),
%!        single ([NaN, Inf, 2, -1, 1i]))
%!assert (sort (single ([3, 1i, 7, 5; 8, 2, 6, 4])),
%!        single ([3, 1i, 6, 4; 8, 2, 7, 5]))
%!assert (sort (single ([3, 1i, 7, 5; 8, 2, 6, 4]), 1),
%!        single ([3, 1i, 6, 4; 8, 2, 7, 5]))
%!assert (sort (single ([3, 1i, 7, 5; 8, 2, 6, 4]), 2),
%!        single ([1i, 3, 5, 7; 2, 4, 6, 8]))
%!assert (sort (single (1i)), single (1i))

%!test
%! [v, i] = sort (single ([NaN, 1i, -1, Inf, 1, 1i]));
%! assert (v, single ([1, 1i, 1i, -1, Inf, NaN]));
%! assert (i, [5, 2, 6, 3, 4, 1]);

## Bool
%!assert (sort ([true, false, true, false]), [false, false, true, true])
%!assert (sort ([true, false, true, false], 1), [true, false, true, false])
%!assert (sort ([true, false, true, false], 2), [false, false, true, true])
%!assert (sort ([true, false, true, false], 3), [true, false, true, false])
%!assert (sort ([true, false, true, false], "ascend"),
%!        [false, false, true, true])
%!assert (sort ([true, false, true, false], 2, "ascend"),
%!        [false, false, true, true])
%!assert (sort ([true, false, true, false], "descend"),
%!        [true, true, false, false])
%!assert (sort ([true, false, true, false], 2, "descend"),
%!        [true, true, false, false])
%!assert (sort (true), true)

%!test
%! [v, i] = sort ([true, false, true, false]);
%! assert (v, [false, false, true, true]);
%! assert (i, [2, 4, 1, 3]);

## Sparse Double
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf])),
%!        sparse ([-1, 0, 0, 1, 2, Inf, NaN]))
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf]), 1),
%!        sparse ([0, NaN, 1, 0, -1, 2, Inf]))
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf]), 2),
%!        sparse ([-1, 0, 0, 1, 2, Inf, NaN]))
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf]), 3),
%!        sparse ([0, NaN, 1, 0, -1, 2, Inf]))
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf]), "ascend"),
%!        sparse ([-1, 0, 0, 1, 2, Inf, NaN]))
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf]), 2, "ascend"),
%!        sparse ([-1, 0, 0, 1, 2, Inf, NaN]))
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf]), "descend"),
%!        sparse ([NaN, Inf, 2, 1, 0, 0, -1]))
%!assert (sort (sparse ([0, NaN, 1, 0, -1, 2, Inf]), 2, "descend"),
%!        sparse ([NaN, Inf, 2, 1, 0, 0, -1]))

%!shared a
%! a = randn (10, 10);
%! a(a < 0) = 0;
%!assert (sort (sparse (a)), sparse (sort (a)))
%!assert (sort (sparse (a), 1), sparse (sort (a, 1)))
%!assert (sort (sparse (a), 2), sparse (sort (a, 2)))
%!test
%! [v, i] = sort (a);
%! [vs, is] = sort (sparse (a));
%! assert (vs, sparse (v));
%! assert (is, i);

## Sparse Complex
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf])),
%!        sparse ([0, 0, 1i, -1, 2, Inf, NaN]))
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf]), 1),
%!        sparse ([0, NaN, 1i, 0, -1, 2, Inf]))
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf]), 2),
%!        sparse ([0, 0, 1i, -1, 2, Inf, NaN]))
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf]), 3),
%!        sparse ([0, NaN, 1i, 0, -1, 2, Inf]))
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf]), "ascend"),
%!        sparse ([0, 0, 1i, -1, 2, Inf, NaN]))
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf]), 2, "ascend"),
%!        sparse ([0, 0, 1i, -1, 2, Inf, NaN]))
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf]), "descend"),
%!        sparse ([NaN, Inf, 2, -1, 1i, 0, 0]))
%!assert (sort (sparse ([0, NaN, 1i, 0, -1, 2, Inf]), 2, "descend"),
%!        sparse ([NaN, Inf, 2, -1, 1i, 0, 0]))

%!shared a
%! a = randn (10, 10);
%! a(a < 0) = 0;
%! a = 1i * a;
%!assert (sort (sparse (a)), sparse (sort (a)))
%!assert (sort (sparse (a), 1), sparse (sort (a, 1)))
%!assert (sort (sparse (a), 2), sparse (sort (a, 2)))
%!test
%! [v, i] = sort (a);
%! [vs, is] = sort (sparse (a));
%! assert (vs, sparse (v));
%! assert (is, i);

## Sparse Bool
%!assert (sort (sparse ([true, false, true, false])),
%!        sparse ([false, false, true, true]))
%!assert (sort (sparse ([true, false, true, false]), 1),
%!        sparse ([true, false, true, false]))
%!assert (sort (sparse ([true, false, true, false]), 2),
%!        sparse ([false, false, true, true]))
%!assert (sort (sparse ([true, false, true, false]), 3),
%!        sparse ([true, false, true, false]))
%!assert (sort (sparse ([true, false, true, false]), "ascend"),
%!        sparse ([false, false, true, true]))
%!assert (sort (sparse ([true, false, true, false]), 2, "ascend"),
%!        sparse ([false, false, true, true]))
%!assert (sort (sparse ([true, false, true, false]), "descend"),
%!        sparse ([true, true, false, false]))
%!assert (sort (sparse ([true, false, true, false]), 2, "descend"),
%!        sparse ([true, true, false, false]))

%!test
%! [v, i] = sort (sparse ([true, false, true, false]));
%! assert (v, sparse ([false, false, true, true]));
%! assert (i, [2, 4, 1, 3]);

## Cell string array
%!shared a, b, c
%! a = {"Alice", "Cecile", "Eric", "Barry", "David"};
%! b = {"Alice", "Barry", "Cecile", "David", "Eric"};
%! c = {"Eric", "David", "Cecile", "Barry", "Alice"};
%!assert (sort (a), b)
%!assert (sort (a, 1), a)
%!assert (sort (a, 2), b)
%!assert (sort (a, 3), a)
%!assert (sort (a, "ascend"), b)
%!assert (sort (a, 2, "ascend"), b)
%!assert (sort (a, "descend"), c)
%!assert (sort (a, 2, "descend"), c)

%!test
%! [v, i] = sort (a);
%! assert (i, [1, 4, 2, 5, 3]);

%!error sort ()
%!error sort (1, 2, 3, 4)
*/

// Sort the rows of the matrix @var{a} according to the order
// specified by @var{mode}, which can either be 'ascend' or 'descend'
// and return the index vector corresponding to the sort order.
//
// FIXME: This function does not yet support sparse matrices.

DEFUN (__sort_rows_idx__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{idx} =} __sort_rows_idx__ (@var{A}, @var{mode})
Called internally from @file{sortrows.m}.
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  if (nargin == 2 && ! args(1).is_string ())
    error ("__sort_rows_idx__: second argument must be a string");

  sortmode smode = ASCENDING;
  if (nargin > 1)
    {
      std::string mode = args(1).string_value ();
      if (mode == "ascend")
        smode = ASCENDING;
      else if (mode == "descend")
        smode = DESCENDING;
      else
        error (R"(__sort_rows_idx__: MODE must be either "ascend" or "descend")");
    }

  octave_value arg = args(0);

  if (arg.issparse ())
    error ("__sort_rows_idx__: sparse matrices not yet supported");

  if (arg.ndims () != 2)
    error ("__sort_rows_idx__: needs a 2-D object");

  Array<octave_idx_type> idx = arg.sort_rows_idx (smode);

  // This cannot be ovl(), relies on special overloaded octave_value call.
  return octave_value (idx, true, true);
}

static sortmode
get_sort_mode_option (const octave_value& arg)
{
  // FIXME: we initialize to UNSORTED here to avoid a GCC warning
  //        about possibly using sortmode uninitialized.
  // FIXME: shouldn't these modes be scoped inside a class?
  sortmode smode = UNSORTED;

  std::string mode = arg.xstring_value ("issorted: MODE must be a string");

  if (mode == "ascend")
    smode = ASCENDING;
  else if (mode == "descend")
    smode = DESCENDING;
  else if (mode == "either")
    smode = UNSORTED;
  else
    error (R"(issorted: MODE must be "ascend", "descend", or "either")");

  return smode;
}

DEFUN (issorted, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{tf} =} issorted (@var{A})
@deftypefnx {} {@var{tf} =} issorted (@var{A}, @var{mode})
@deftypefnx {} {@var{tf} =} issorted (@var{A}, "rows", @var{mode})
Return true if the vector @var{A} is sorted according to @var{mode}, which
may be either @qcode{"ascend"}, @qcode{"descend"}, or @qcode{"either"}.

By default, @var{mode} is @qcode{"ascend"}.  NaNs are treated in the same
manner as @code{sort}.

If the optional argument @qcode{"rows"} is supplied, check whether the matrix
is sorted by rows as output by the function @code{sortrows} (with no options).

This function does not support sparse matrices.
@seealso{sort, sortrows}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    print_usage ();

  bool by_rows = false;

  sortmode smode = ASCENDING;

  if (nargin > 1)
    {
      if (nargin == 3)
        smode = get_sort_mode_option (args(2));

      std::string tmp = args(1).xstring_value ("issorted: second argument must be a string");
      if (tmp == "rows")
        by_rows = true;
      else
        smode = get_sort_mode_option (args(1));
    }

  octave_value retval;

  octave_value arg = args(0);

  if (arg.isempty ())
    retval = true;
  else if (by_rows)
    {
      if (arg.issparse ())
        error ("issorted: sparse matrices not yet supported");

      if (arg.ndims () != 2)
        error ("issorted: A must be a 2-D object");

      retval = arg.is_sorted_rows (smode) != UNSORTED;
    }
  else
    {
      if (! arg.dims ().isvector ())
        error ("issorted: needs a vector");

      retval = args(0).issorted (smode) != UNSORTED;
    }

  return retval;
}

/*
%!shared sm, um, sv, uv
%! sm = [1, 2; 3, 4];
%! um = [3, 1; 2, 4];
%! sv = [1, 2, 3, 4];
%! uv = [2, 1, 4, 3];

%!assert (issorted (sm, "rows"))
%!assert (! issorted (um, "rows"))
%!assert (issorted (sv))
%!assert (! issorted (uv))
%!assert (issorted (sv'))
%!assert (! issorted (uv'))
%!assert (issorted (sm, "rows", "ascend"))
%!assert (! issorted (um, "rows", "ascend"))
%!assert (issorted (sv, "ascend"))
%!assert (! issorted (uv, "ascend"))
%!assert (issorted (sv', "ascend"))
%!assert (! issorted (uv', "ascend"))
%!assert (! issorted (sm, "rows", "descend"))
%!assert (issorted (flipud (sm), "rows", "descend"))
%!assert (! issorted (sv, "descend"))
%!assert (issorted (fliplr (sv), "descend"))
%!assert (! issorted (sv', "descend"))
%!assert (issorted (fliplr (sv)', "descend"))
%!assert (! issorted (um, "rows", "either"))
%!assert (! issorted (uv, "either"))
%!assert (issorted (sm, "rows", "either"))
%!assert (issorted (flipud (sm), "rows", "either"))
%!assert (issorted (sv, "either"))
%!assert (issorted (fliplr (sv), "either"))
%!assert (issorted (sv', "either"))
%!assert (issorted (fliplr (sv)', "either"))

%!assert (issorted ([]))
%!assert (issorted ([], "rows"))
%!assert (issorted ([], "ascend"))
%!assert (issorted ([], "rows", "ascend"))
%!assert (issorted ([], "descend"))
%!assert (issorted ([], "rows", "descend"))
%!assert (issorted ({}))
%!assert (issorted ({}, "rows"))
%!assert (issorted ({}, "ascend"))
%!assert (issorted ({}, "rows", "ascend"))
%!assert (issorted ({}, "descend"))
%!assert (issorted ({}, "rows", "descend"))
%!assert (issorted (""))
%!assert (issorted ("", "rows"))
%!assert (issorted ("", "ascend"))
%!assert (issorted ("", "rows", "ascend"))
%!assert (issorted ("", "descend"))
%!assert (issorted ("", "rows", "descend"))

## Test input validation
%!error issorted ()
%!error issorted (1,2,3,4)
%!error <second argument must be a string> issorted (1, 2)
%!error <second argument must be a string> issorted (1, {"rows"})
%!error <sparse matrices not yet supported> issorted (sparse ([1 2 3]), "rows")
%!error <A must be a 2-D object> issorted (rand (2,2,2), "rows")
%!error <needs a vector> issorted (ones (2,2))
*/

DEFUN (nth_element, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{nel} =} nth_element (@var{x}, @var{n})
@deftypefnx {} {@var{nel} =} nth_element (@var{x}, @var{n}, @var{dim})
Select the n-th smallest element of a vector, using the ordering defined by
@code{sort}.

The result is equivalent to @code{sort(@var{x})(@var{n})}.

@var{n} can also be a contiguous range, either ascending @code{l:u}
or descending @code{u:-1:l}, in which case a range of elements is returned.

If @var{x} is an array, @code{nth_element} operates along the dimension
defined by @var{dim}, or the first non-singleton dimension if @var{dim} is
not given.

Programming Note: nth_element encapsulates the C++ standard library
algorithms nth_element and partial_sort.  On average, the complexity of the
operation is O(M*log(K)), where @w{@code{M = size (@var{x}, @var{dim})}} and
@w{@code{K = length (@var{n})}}.  This function is intended for cases where
the ratio K/M is small; otherwise, it may be better to use @code{sort}.
@seealso{sort, min, max}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    print_usage ();

  int dim = -1;
  if (nargin == 3)
    {
      dim = args(2).int_value (true) - 1;
      if (dim < 0)
        error ("nth_element: DIM must be a valid dimension");
    }

  octave_value argx = args(0);
  if (dim < 0)
    dim = argx.dims ().first_non_singleton ();

  octave_value retval;

  try
    {
      idx_vector n = args(1).index_vector ();

      switch (argx.builtin_type ())
        {
        case btyp_double:
          retval = argx.array_value ().nth_element (n, dim);
          break;
        case btyp_float:
          retval = argx.float_array_value ().nth_element (n, dim);
          break;
        case btyp_complex:
          retval = argx.complex_array_value ().nth_element (n, dim);
          break;
        case btyp_float_complex:
          retval = argx.float_complex_array_value ().nth_element (n, dim);
          break;

#define MAKE_INT_BRANCH(X)                                              \
          case btyp_ ## X:                                              \
            retval = argx.X ## _array_value ().nth_element (n, dim);    \
            break;

          MAKE_INT_BRANCH (int8);
          MAKE_INT_BRANCH (int16);
          MAKE_INT_BRANCH (int32);
          MAKE_INT_BRANCH (int64);
          MAKE_INT_BRANCH (uint8);
          MAKE_INT_BRANCH (uint16);
          MAKE_INT_BRANCH (uint32);
          MAKE_INT_BRANCH (uint64);
          MAKE_INT_BRANCH (bool);

#undef MAKE_INT_BRANCH

        default:
          if (argx.iscellstr ())
            retval = argx.cellstr_value ().nth_element (n, dim);
          else
            err_wrong_type_arg ("nth_element", argx);
        }
    }
  catch (const index_exception& ie)
    {
      error ("nth_element: invalid index %s", ie.what ());
    }

  return retval;
}

/*
%!assert (nth_element ([1:10], 1), 1)
%!assert (nth_element ([1:10], 10), 10)
%!assert (nth_element ([1:10], 1:3), [1 2 3])
%!assert (nth_element ([1:10], 1:10), [1:10])

%!assert <*51329> (nth_element ([1:10], [1:10]), [1:10])

%!error nth_element ()
%!error nth_element (1)
%!error nth_element (1, 1.5)
%!error nth_element (1, 2, 3, 4)
%!error nth_element ("abcd", 3)
*/

template <typename NDT>
static NDT
do_accumarray_sum (const idx_vector& idx, const NDT& vals,
                   octave_idx_type n = -1)
{
  typedef typename NDT::element_type T;
  if (n < 0)
    n = idx.extent (0);
  else if (idx.extent (n) > n)
    error ("accumarray: index out of range");

  NDT retval (dim_vector (n, 1), T ());

  if (vals.numel () == 1)
    retval.idx_add (idx, vals (0));
  else if (vals.numel () == idx.length (n))
    retval.idx_add (idx, vals);
  else
    error ("accumarray: dimensions mismatch");

  return retval;
}

DEFUN (__accumarray_sum__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __accumarray_sum__ (@var{idx}, @var{vals}, @var{n})
Undocumented internal function.
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    print_usage ();

  if (! args(0).isnumeric ())
    error ("__accumarray_sum__: first argument must be numeric");

  octave_value retval;

  try
    {
      idx_vector idx = args(0).index_vector ();
      octave_idx_type n = -1;
      if (nargin == 3)
        n = args(2).idx_type_value (true);

      octave_value vals = args(1);

      if (vals.is_range ())
        {
          range<double> r = vals.range_value ();
          if (r.increment () == 0)
            vals = r.base ();
        }

      if (vals.is_single_type ())
        {
          if (vals.iscomplex ())
            retval = do_accumarray_sum (idx,
                                        vals.float_complex_array_value (),
                                        n);
          else
            retval = do_accumarray_sum (idx, vals.float_array_value (), n);
        }
      else if (vals.isnumeric () || vals.islogical ())
        {
          if (vals.iscomplex ())
            retval = do_accumarray_sum (idx,
                                        vals.complex_array_value (),
                                        n);
          else
            retval = do_accumarray_sum (idx, vals.array_value (), n);
        }
      else
        err_wrong_type_arg ("accumarray", vals);
    }
  catch (const index_exception& ie)
    {
      error ("__accumarray_sum__: invalid index %s", ie.what ());
    }

  return retval;
}

template <typename NDT>
static NDT
do_accumarray_minmax (const idx_vector& idx, const NDT& vals,
                      octave_idx_type n, bool ismin,
                      const typename NDT::element_type& zero_val)
{
  typedef typename NDT::element_type T;
  if (n < 0)
    n = idx.extent (0);
  else if (idx.extent (n) > n)
    error ("accumarray: index out of range");

  NDT retval (dim_vector (n, 1), zero_val);

  // Pick minimizer or maximizer.
  void (MArray<T>::*op) (const idx_vector&, const MArray<T>&)
    = ismin ? (&MArray<T>::idx_min) : (&MArray<T>::idx_max);

  octave_idx_type l = idx.length (n);
  if (vals.numel () == 1)
    (retval.*op) (idx, NDT (dim_vector (l, 1), vals(0)));
  else if (vals.numel () == l)
    (retval.*op) (idx, vals);
  else
    error ("accumarray: dimensions mismatch");

  return retval;
}

static octave_value_list
do_accumarray_minmax_fcn (const octave_value_list& args,
                          bool ismin)
{
  int nargin = args.length ();

  if (nargin < 3 || nargin > 4)
    print_usage ();

  if (! args(0).isnumeric ())
    error ("accumarray: first argument must be numeric");

  octave_value retval;

  try
    {
      idx_vector idx = args(0).index_vector ();
      octave_idx_type n = -1;
      if (nargin == 4)
        n = args(3).idx_type_value (true);

      octave_value vals = args(1);
      octave_value zero = args(2);

      switch (vals.builtin_type ())
        {
        case btyp_double:
          retval = do_accumarray_minmax (idx, vals.array_value (), n, ismin,
                                         zero.double_value ());
          break;

        case btyp_float:
          retval = do_accumarray_minmax (idx, vals.float_array_value (), n,
                                         ismin, zero.float_value ());
          break;

        case btyp_complex:
          retval = do_accumarray_minmax (idx, vals.complex_array_value (),
                                         n, ismin, zero.complex_value ());
          break;

        case btyp_float_complex:
          retval = do_accumarray_minmax (idx,
                                         vals.float_complex_array_value (),
                                         n, ismin,
                                         zero.float_complex_value ());
          break;

#define MAKE_INT_BRANCH(X)                                              \
          case btyp_ ## X:                                              \
            retval = do_accumarray_minmax (idx, vals.X ## _array_value (), \
                                           n, ismin, zero.X ## _scalar_value ()); \
            break;

          MAKE_INT_BRANCH (int8);
          MAKE_INT_BRANCH (int16);
          MAKE_INT_BRANCH (int32);
          MAKE_INT_BRANCH (int64);
          MAKE_INT_BRANCH (uint8);
          MAKE_INT_BRANCH (uint16);
          MAKE_INT_BRANCH (uint32);
          MAKE_INT_BRANCH (uint64);

#undef MAKE_INT_BRANCH

        case btyp_bool:
          retval = do_accumarray_minmax (idx, vals.array_value (), n, ismin,
                                         zero.bool_value ());
          break;

        default:
          err_wrong_type_arg ("accumarray", vals);
        }
    }
  catch (const index_exception& ie)
    {
      error ("do_accumarray_minmax_fcn: invalid index %s", ie.what ());
    }

  return retval;
}

DEFUN (__accumarray_min__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __accumarray_min__ (@var{idx}, @var{vals}, @var{zero}, @var{n})
Undocumented internal function.
@end deftypefn */)
{
  return do_accumarray_minmax_fcn (args, true);
}

DEFUN (__accumarray_max__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __accumarray_max__ (@var{idx}, @var{vals}, @var{zero}, @var{n})
Undocumented internal function.
@end deftypefn */)
{
  return do_accumarray_minmax_fcn (args, false);
}

template <typename NDT>
static NDT
do_accumdim_sum (const idx_vector& idx, const NDT& vals,
                 int dim = -1, octave_idx_type n = -1)
{
  typedef typename NDT::element_type T;
  if (n < 0)
    n = idx.extent (0);
  else if (idx.extent (n) > n)
    error ("accumdim: index out of range");

  dim_vector vals_dim = vals.dims ();
  dim_vector rdv = vals_dim;

  if (dim < 0)
    dim = vals.dims ().first_non_singleton ();
  else if (dim >= rdv.ndims ())
    rdv.resize (dim+1, 1);

  rdv(dim) = n;

  NDT retval (rdv, T ());

  if (idx.length () != vals_dim(dim))
    error ("accumdim: dimension mismatch");

  retval.idx_add_nd (idx, vals, dim);

  return retval;
}

DEFUN (__accumdim_sum__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __accumdim_sum__ (@var{idx}, @var{vals}, @var{dim}, @var{n})
Undocumented internal function.
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2 || nargin > 4)
    print_usage ();

  if (! args(0).isnumeric ())
    error ("__accumdim_sum__: first argument must be numeric");

  octave_value retval;

  try
    {
      idx_vector idx = args(0).index_vector ();
      int dim = -1;
      if (nargin >= 3)
        dim = args(2).int_value () - 1;

      octave_idx_type n = -1;
      if (nargin == 4)
        n = args(3).idx_type_value (true);

      octave_value vals = args(1);

      if (vals.is_single_type ())
        {
          if (vals.iscomplex ())
            retval = do_accumdim_sum (idx,
                                      vals.float_complex_array_value (),
                                      dim, n);
          else
            retval = do_accumdim_sum (idx, vals.float_array_value (),
                                      dim, n);
        }
      else if (vals.isnumeric () || vals.islogical ())
        {
          if (vals.iscomplex ())
            retval = do_accumdim_sum (idx, vals.complex_array_value (),
                                      dim, n);
          else
            retval = do_accumdim_sum (idx, vals.array_value (), dim, n);
        }
      else
        err_wrong_type_arg ("accumdim", vals);
    }
  catch (const index_exception& ie)
    {
      error ("__accumdim_sum__: invalid index %s", ie.what ());
    }

  return retval;
}

template <typename NDT>
static NDT
do_merge (const Array<bool>& mask,
          const NDT& tval, const NDT& fval)
{
  typedef typename NDT::element_type T;
  dim_vector dv = mask.dims ();
  NDT retval (dv);

  bool tscl = tval.numel () == 1;
  bool fscl = fval.numel () == 1;

  if ((! tscl && tval.dims () != dv) || (! fscl && fval.dims () != dv))
    error ("merge: MASK, TVAL, and FVAL dimensions must match");

  T *rv = retval.fortran_vec ();
  octave_idx_type n = retval.numel ();

  const T *tv = tval.data ();
  const T *fv = fval.data ();
  const bool *mv = mask.data ();

  if (tscl)
    {
      if (fscl)
        {
          T ts = tv[0];
          T fs = fv[0];
          for (octave_idx_type i = 0; i < n; i++)
            rv[i] = (mv[i] ? ts : fs);
        }
      else
        {
          T ts = tv[0];
          for (octave_idx_type i = 0; i < n; i++)
            rv[i] = (mv[i] ? ts : fv[i]);
        }
    }
  else
    {
      if (fscl)
        {
          T fs = fv[0];
          for (octave_idx_type i = 0; i < n; i++)
            rv[i] = (mv[i] ? tv[i] : fs);
        }
      else
        {
          for (octave_idx_type i = 0; i < n; i++)
            rv[i] = (mv[i] ? tv[i] : fv[i]);
        }
    }

  return retval;
}

#define MAKE_INT_BRANCH(INTX)                                           \
  else if (tval.is_ ## INTX ## _type () && fval.is_ ## INTX ## _type ()) \
    {                                                                   \
      retval = do_merge (mask,                                          \
                         tval.INTX ## _array_value (),                  \
                         fval.INTX ## _array_value ());                 \
    }

DEFUN (merge, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{M} =} merge (@var{mask}, @var{tval}, @var{fval})
@deftypefnx {} {@var{M} =} ifelse (@var{mask}, @var{tval}, @var{fval})
Merge elements of @var{true_val} and @var{false_val}, depending on the
value of @var{mask}.

If @var{mask} is a logical scalar, the other two arguments can be arbitrary
values.  Otherwise, @var{mask} must be a logical array, and @var{tval},
@var{fval} should be arrays of matching class, or cell arrays.  In the
scalar mask case, @var{tval} is returned if @var{mask} is true, otherwise
@var{fval} is returned.

In the array mask case, both @var{tval} and @var{fval} must be either
scalars or arrays with dimensions equal to @var{mask}.  The result is
constructed as follows:

@example
@group
result(mask) = tval(mask);
result(! mask) = fval(! mask);
@end group
@end example

@var{mask} can also be arbitrary numeric type, in which case it is first
converted to logical.
@seealso{logical, diff}
@end deftypefn */)
{
  if (args.length () != 3)
    print_usage ();

  if (! (args(0).islogical () || args(0).isnumeric ()))
    error ("merge: first argument must be logical or numeric");

  octave_value retval;

  octave_value mask_val = args(0);

  if (mask_val.is_scalar_type ())
    retval = (mask_val.is_true () ? args(1) : args(2));
  else
    {
      boolNDArray mask = mask_val.bool_array_value ();

      octave_value tval = args(1);
      octave_value fval = args(2);

      if (tval.is_double_type () && fval.is_double_type ())
        {
          if (tval.iscomplex () || fval.iscomplex ())
            retval = do_merge (mask,
                               tval.complex_array_value (),
                               fval.complex_array_value ());
          else
            retval = do_merge (mask,
                               tval.array_value (),
                               fval.array_value ());
        }
      else if (tval.is_single_type () && fval.is_single_type ())
        {
          if (tval.iscomplex () || fval.iscomplex ())
            retval = do_merge (mask,
                               tval.float_complex_array_value (),
                               fval.float_complex_array_value ());
          else
            retval = do_merge (mask,
                               tval.float_array_value (),
                               fval.float_array_value ());
        }
      else if (tval.is_string () && fval.is_string ())
        {
          bool sq_string = tval.is_sq_string () || fval.is_sq_string ();
          retval = octave_value (do_merge (mask,
                                           tval.char_array_value (),
                                           fval.char_array_value ()),
                                 sq_string ? '\'' : '"');
        }
      else if (tval.iscell () && fval.iscell ())
        {
          retval = do_merge (mask,
                             tval.cell_value (),
                             fval.cell_value ());
        }

      MAKE_INT_BRANCH (int8)
      MAKE_INT_BRANCH (int16)
      MAKE_INT_BRANCH (int32)
      MAKE_INT_BRANCH (int64)
      MAKE_INT_BRANCH (uint8)
      MAKE_INT_BRANCH (uint16)
      MAKE_INT_BRANCH (uint32)
      MAKE_INT_BRANCH (uint64)

      else
        error ("merge: cannot merge %s with %s with array mask",
               tval.class_name ().c_str (),
               fval.class_name ().c_str ());
    }

  return retval;
}

DEFALIAS (ifelse, merge);

#undef MAKE_INT_BRANCH

template <typename SparseT>
static SparseT
do_sparse_diff (const SparseT& array, octave_idx_type order,
                int dim)
{
  SparseT retval = array;
  if (dim == 1)
    {
      octave_idx_type k = retval.columns ();
      while (order > 0 && k > 0)
        {
          idx_vector col1 (':'), col2 (':'), sl1 (1, k), sl2 (0, k-1);
          retval = SparseT (retval.index (col1, sl1))
                   - SparseT (retval.index (col2, sl2));
          error_unless (retval.columns () == k-1);
          order--;
          k--;
        }
    }
  else
    {
      octave_idx_type k = retval.rows ();
      while (order > 0 && k > 0)
        {
          idx_vector col1 (':'), col2 (':'), sl1 (1, k), sl2 (0, k-1);
          retval = SparseT (retval.index (sl1, col1))
                   - SparseT (retval.index (sl2, col2));
          error_unless (retval.rows () == k-1);
          order--;
          k--;
        }
    }

  return retval;
}

static octave_value
do_diff (const octave_value& array, octave_idx_type order,
         int dim = -1)
{
  octave_value retval;

  const dim_vector& dv = array.dims ();
  if (dim == -1)
    {
      dim = array.dims ().first_non_singleton ();

      // Bother Matlab.  This behavior is really wicked.
      if (dv(dim) <= order)
        {
          if (dv(dim) == 1)
            retval = array.resize (dim_vector (0, 0));
          else
            {
              retval = array;
              while (order > 0)
                {
                  if (dim == dv.ndims ())
                    {
                      retval = do_diff (array, order, dim - 1);
                      order = 0;
                    }
                  else if (dv(dim) == 1)
                    dim++;
                  else
                    {
                      retval = do_diff (array, dv(dim) - 1, dim);
                      order -= dv(dim) - 1;
                      dim++;
                    }
                }
            }

          return retval;
        }
    }

  if (array.isinteger ())
    {
      if (array.is_int8_type ())
        retval = array.int8_array_value ().diff (order, dim);
      else if (array.is_int16_type ())
        retval = array.int16_array_value ().diff (order, dim);
      else if (array.is_int32_type ())
        retval = array.int32_array_value ().diff (order, dim);
      else if (array.is_int64_type ())
        retval = array.int64_array_value ().diff (order, dim);
      else if (array.is_uint8_type ())
        retval = array.uint8_array_value ().diff (order, dim);
      else if (array.is_uint16_type ())
        retval = array.uint16_array_value ().diff (order, dim);
      else if (array.is_uint32_type ())
        retval = array.uint32_array_value ().diff (order, dim);
      else if (array.is_uint64_type ())
        retval = array.uint64_array_value ().diff (order, dim);
      else
        panic_impossible ();
    }
  else if (array.issparse ())
    {
      if (array.iscomplex ())
        retval = do_sparse_diff (array.sparse_complex_matrix_value (),
                                 order, dim);
      else
        retval = do_sparse_diff (array.sparse_matrix_value (), order, dim);
    }
  else if (array.is_single_type ())
    {
      if (array.iscomplex ())
        retval = array.float_complex_array_value ().diff (order, dim);
      else
        retval = array.float_array_value ().diff (order, dim);
    }
  else
    {
      if (array.iscomplex ())
        retval = array.complex_array_value ().diff (order, dim);
      else
        retval = array.array_value ().diff (order, dim);
    }

  return retval;
}

DEFUN (diff, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{y} =} diff (@var{x})
@deftypefnx {} {@var{y} =} diff (@var{x}, @var{k})
@deftypefnx {} {@var{y} =} diff (@var{x}, @var{k}, @var{dim})
If @var{x} is a vector of length @math{n}, @w{@code{diff (@var{x})}} is the
vector of first differences
@tex
 $x_2 - x_1, \ldots{}, x_n - x_{n-1}$.
@end tex
@ifnottex
 @var{x}(2) - @var{x}(1), @dots{}, @var{x}(n) - @var{x}(n-1).
@end ifnottex

If @var{x} is a matrix, @w{@code{diff (@var{x})}} is the matrix of column
differences along the first non-singleton dimension.

The second argument is optional.  If supplied,
@w{@code{diff (@var{x}, @var{k})}}, where @var{k} is a non-negative integer,
returns the @var{k}-th differences.  It is possible that @var{k} is larger
than the first non-singleton dimension of the matrix.  In this case,
@code{diff} continues to take the differences along the next
non-singleton dimension.

The dimension along which to take the difference can be explicitly
stated with the optional variable @var{dim}.  In this case the
@var{k}-th order differences are calculated along this dimension.
In the case where @var{k} exceeds @w{@code{size (@var{x}, @var{dim})}}
an empty matrix is returned.
@seealso{sort, merge}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    print_usage ();

  if (! (args(0).isnumeric () || args(0).islogical ()))
    error ("diff: X must be numeric or logical");

  int dim = -1;
  octave_idx_type order = 1;
  if (nargin > 1)
    {
      if (args(1).is_scalar_type ())
        order = args(1).idx_type_value (true, false);
      else if (! args(1).is_zero_by_zero ())
        error ("diff: order K must be a scalar or []");
      if (order < 0)
        error ("diff: order K must be non-negative");
    }

  if (nargin > 2)
    {
      dim = args(2).int_value (true, false);
      if (dim < 1 || dim > args(0).ndims ())
        error ("diff: DIM must be a valid dimension");

      dim -= 1;
    }

  return do_diff (args(0), order, dim);
}

/*
%!assert (diff ([1, 2, 3, 4]), [1, 1, 1])
%!assert (diff ([1, 3, 7, 19], 2), [2, 8])
%!assert (diff ([1, 2; 5, 4; 8, 7; 9, 6; 3, 1]), [4, 2; 3, 3; 1, -1; -6, -5])
%!assert (diff ([1, 2; 5, 4; 8, 7; 9, 6; 3, 1], 3), [-1, -5; -5, 0])
%!assert (isempty (diff (1)))

%!error diff ()
%!error diff (1, 2, 3, 4)
%!error diff ("foo")
%!error diff ([1, 2; 3, 4], -1)
*/

template <typename T>
static Array<T>
do_repelems (const Array<T>& src, const Array<octave_idx_type>& rep)
{
  Array<T> retval;

  if (rep.ndims () != 2 || rep.rows () != 2)
    error ("repelems: R must be a 2-row, N-column matrix of integers");

  octave_idx_type n = rep.columns ();
  octave_idx_type l = 0;
  for (octave_idx_type i = 0; i < n; i++)
    {
      octave_idx_type k = rep(1, i);
      if (k < 0)
        error ("repelems: second row must contain non-negative numbers");

      l += k;
    }

  retval.clear (1, l);
  T *dest = retval.fortran_vec ();
  l = 0;
  for (octave_idx_type i = 0; i < n; i++)
    {
      octave_idx_type k = rep(1, i);
      std::fill_n (dest, k, src.checkelem (rep(0, i) - 1));
      dest += k;
    }

  return retval;
}

DEFUN (repelems, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{y} =} repelems (@var{x}, @var{r})
Construct a vector of repeated elements from @var{x}.

@var{r} is a 2x@var{N} integer matrix specifying which elements to repeat
and how often to repeat each element.  Entries in the first row,
@var{r}(1,j), select an element to repeat.  The corresponding entry in the
second row, @var{r}(2,j), specifies the repeat count.  If @var{x} is a
matrix then the columns of @var{x} are imagined to be stacked on top of
each other for purposes of the selection index.  A row vector is always
returned.

Conceptually the result is calculated as follows:

@example
@group
y = [];
for i = 1:columns (@var{r})
  y = [y, @var{x}(@var{r}(1,i)*ones(1, @var{r}(2,i)))];
endfor
@end group
@end example
@seealso{repmat, cat}
@end deftypefn */)
{
  if (args.length () != 2)
    print_usage ();

  octave_value retval;

  const Matrix rm = args(1).matrix_value ();

  if (rm.rows () != 2 || rm.ndims () != 2)
    error ("repelems: R must be a matrix with two rows");

  octave_value x = args(0);

  Array<octave_idx_type> r (rm.dims ());

  for (octave_idx_type i = 0; i < rm.numel (); i++)
    {
      octave_idx_type rx = rm(i);
      if (static_cast<double> (rx) != rm(i))
        error ("repelems: R must be a matrix of integers");

      r.xelem (i) = rx;
    }

  switch (x.builtin_type ())
    {
#define BTYP_BRANCH(X, EX)                              \
      case btyp_ ## X:                                  \
        retval = do_repelems (x.EX ## _value (), r);    \
        break;

      BTYP_BRANCH (double, array);
      BTYP_BRANCH (float, float_array);
      BTYP_BRANCH (complex, complex_array);
      BTYP_BRANCH (float_complex, float_complex_array);
      BTYP_BRANCH (bool, bool_array);
      BTYP_BRANCH (char, char_array);

      BTYP_BRANCH (int8,  int8_array);
      BTYP_BRANCH (int16, int16_array);
      BTYP_BRANCH (int32, int32_array);
      BTYP_BRANCH (int64, int64_array);
      BTYP_BRANCH (uint8,  uint8_array);
      BTYP_BRANCH (uint16, uint16_array);
      BTYP_BRANCH (uint32, uint32_array);
      BTYP_BRANCH (uint64, uint64_array);

      BTYP_BRANCH (cell, cell);
      //BTYP_BRANCH (struct, map);//FIXME

#undef BTYP_BRANCH

    default:
      err_wrong_type_arg ("repelems", x);
    }

  return retval;
}

DEFUN (base64_encode, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{s} =} base64_encode (@var{x})
Encode a double matrix or array @var{x} into the base64 format string
@var{s}.

@seealso{base64_decode, matlab.net.base64decode, matlab.net.base64encode}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  if (! args(0).isnumeric ())
    error ("base64_encode: encoding is supported only for numeric arrays");

  if (args(0).iscomplex () || args(0).issparse ())
    error ("base64_encode: encoding complex or sparse data is not supported");

  octave_value_list retval;

  if (args(0).isinteger ())
    {
#define MAKE_INT_BRANCH(X)                                              \
      if (args(0).is_ ## X ## _type ())                                 \
        {                                                               \
          const X##NDArray in = args(0).  X## _array_value ();          \
          std::size_t inlen = in.numel () * sizeof (X## _t) / sizeof (char); \
          const char *inc = reinterpret_cast<const char *> (in.data ()); \
          char *out;                                                    \
          if (base64_encode (inc, inlen, &out))                         \
            {                                                           \
              retval(0) = octave_value (out);                           \
              ::free (out);                                             \
            }                                                           \
        }

      MAKE_INT_BRANCH(int8)
      else MAKE_INT_BRANCH(int16)
        else MAKE_INT_BRANCH(int32)
          else MAKE_INT_BRANCH(int64)
            else MAKE_INT_BRANCH(uint8)
              else MAKE_INT_BRANCH(uint16)
                else MAKE_INT_BRANCH(uint32)
                  else MAKE_INT_BRANCH(uint64)

#undef MAKE_INT_BRANCH

                    else
                      panic_impossible ();
    }
  else if (args(0).is_single_type ())
    {
      const Array<float> in = args(0).float_array_value ();
      std::size_t inlen;
      inlen = in.numel () * sizeof (float) / sizeof (char);
      const char *inc;
      inc = reinterpret_cast<const char *> (in.data ());
      char *out;
      if (base64_encode (inc, inlen, &out))
        {
          retval(0) = octave_value (out);
          ::free (out);
        }
    }
  else  // double_type
    {
      const Array<double> in = args(0).array_value ();
      std::size_t inlen;
      inlen = in.numel () * sizeof (double) / sizeof (char);
      const char *inc;
      inc = reinterpret_cast<const char *> (in.data ());
      char *out;
      if (base64_encode (inc, inlen, &out))
        {
          retval(0) = octave_value (out);
          ::free (out);
        }
    }

  return retval;
}

/*
%!test
%! ## FIXME: better test for endianness?
%! if (bitunpack (uint16 (1))(1) == 1)
%!   expected = "2w9JQA==";
%! else
%!   expected = "QEkP2w==";
%! endif
%! assert (base64_encode (single (pi)), expected);

%!assert (base64_encode (uint8 ([0 0 0])), "AAAA")
%!assert (base64_encode (uint16 ([0 0 0])), "AAAAAAAA")
%!assert (base64_encode (uint32 ([0 0 0])), "AAAAAAAAAAAAAAAA")
%!assert (base64_encode (uint64 ([0 0 0])), "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
%!assert (base64_encode (uint8 ([255 255 255])), "////")

%!error base64_encode ()
%!error base64_encode (1,2)
%!error base64_encode ("A string")
%!error base64_encode ({"A cell array"})
%!error base64_encode (struct ())
*/

DEFUN (base64_decode, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{x} =} base64_decode (@var{s})
@deftypefnx {} {@var{x} =} base64_decode (@var{s}, @var{dims})
Decode the double matrix or array @var{x} from the base64 encoded string
@var{s}.

The optional input parameter @var{dims} should be a vector containing the
dimensions of the decoded array.
@seealso{base64_encode, matlab.net.base64decode, matlab.net.base64encode}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  std::string str = args(0).string_value ();

  Array<double> retval = base64_decode (str);

  if (nargin == 2)
    {
      dim_vector dims;

      const Array<octave_idx_type> size
        = args(1).octave_idx_type_vector_value ();

      dims = dim_vector::alloc (size.numel ());
      for (octave_idx_type i = 0; i < size.numel (); i++)
        dims(i) = size(i);

      retval = retval.reshape (dims);
    }

  return ovl (retval);
}

/*
%!assert (base64_decode (base64_encode (pi)), pi)
%!
%!test
%! in   = randn (10);
%! outv = base64_decode (base64_encode (in));
%! outm = base64_decode (base64_encode (in), size (in));
%! assert (outv, in(:).');
%! assert (outm, in);

%!error base64_decode ()
%!error base64_decode (1,2,3)
%!error base64_decode (1, "this is not a valid set of dimensions")
%!error <input was not valid base64> base64_decode (1)
%!error <input was not valid base64> base64_decode ("AQ=")
%!error <incorrect input size> base64_decode ("AQ==")
*/

DEFUN (__base64_decode_bytes__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{x} =} base64_decode_bytes (@var{s})
@deftypefnx {} {@var{x} =} base64_decode_bytes (@var{s}, @var{dims})
Decode the uint8 matrix or array @var{x} from the base64 encoded string
@var{s}.

The optional input parameter @var{dims} should be a vector containing the
dimensions of the decoded array.
@seealso{base64_decode}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  std::string str = args(0).string_value ();

  intNDArray<octave_uint8> retval = base64_decode_bytes (str);

  if (nargin == 2)
    {
      dim_vector dims;

      const Array<octave_idx_type> size
        = args(1).octave_idx_type_vector_value ();

      dims = dim_vector::alloc (size.numel ());
      for (octave_idx_type i = 0; i < size.numel (); i++)
        dims(i) = size(i);

      retval = retval.reshape (dims);
    }

  return ovl (retval);
}

/*
%!assert (__base64_decode_bytes__ (base64_encode (uint8 (1))), uint8 (1))

%!test
%! in   = uint8 (rand (10)*255);
%! outv = __base64_decode_bytes__ (base64_encode (in));
%! outm = __base64_decode_bytes__ (base64_encode (in), size (in));
%! assert (outv, in(:).');
%! assert (outm, in);

%!error __base64_decode_bytes__ ()
%!error __base64_decode_bytes__ (1,2,3)
%!error __base64_decode_bytes__ (1, "this is not a valid set of dimensions")
%!error <input was not valid base64> __base64_decode_bytes__ (1)
*/

OCTAVE_END_NAMESPACE(octave)
