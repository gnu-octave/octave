////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2008-2023 The Octave Project Developers
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

#include <cctype>
#include <functional>
#include <algorithm>

#include "dNDArray.h"
#include "CNDArray.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"
#include "ov.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static
bool
contains_char (const std::string& str, char c)
{
  return (str.find (c) != std::string::npos
          || str.find (std::toupper (c)) != std::string::npos);
}

template <typename T>
inline sortmode
get_sort_mode (const Array<T>& array,
               typename octave_sort<T>::compare_fcn_type desc_comp
               = octave_sort<T>::descending_compare)
{
  octave_idx_type n = array.numel ();
  if (n > 1 && desc_comp (array (0), array (n-1)))
    return DESCENDING;
  else
    return ASCENDING;
}

// FIXME: perhaps there should be octave_value::lookup?
// The question is, how should it behave w.r.t. the second argument's type.
// We'd need a dispatch on two arguments.  Hmmm...

#define INT_ARRAY_LOOKUP(TYPE)                                  \
  (table.is_ ## TYPE ## _type () && y.is_ ## TYPE ## _type ())  \
    retval = do_numeric_lookup (table.TYPE ## _array_value (),  \
                                y.TYPE ## _array_value (),      \
                                left_inf, right_inf,            \
                                match_idx, match_bool);
template <typename ArrayT>
static octave_value
do_numeric_lookup (const ArrayT& array, const ArrayT& values,
                   bool left_inf, bool right_inf,
                   bool match_idx, bool match_bool)
{
  octave_value retval;

  Array<octave_idx_type> idx = array.lookup (values);
  octave_idx_type n = array.numel ();
  octave_idx_type nval = values.numel ();

  // Post-process.
  if (match_bool)
    {
      boolNDArray match (idx.dims ());
      for (octave_idx_type i = 0; i < nval; i++)
        {
          octave_idx_type j = idx.xelem (i);
          match.xelem (i) = j != 0 && values(i) == array(j-1);
        }

      retval = match;
    }
  else if (match_idx || left_inf || right_inf)
    {
      if (match_idx)
        {
          NDArray ridx (idx.dims ());

          for (octave_idx_type i = 0; i < nval; i++)
            {
              octave_idx_type j = idx.xelem (i);
              ridx.xelem (i) = (j != 0 && values(i) == array(j-1)) ? j : 0;
            }

          retval = ridx;
        }
      else if (left_inf && right_inf)
        {
          // Results in valid indices.  Optimize using lazy index.
          octave_idx_type zero = 0;
          for (octave_idx_type i = 0; i < nval; i++)
            {
              octave_idx_type j = idx.xelem (i) - 1;
              idx.xelem (i) = std::max (zero, std::min (j, n-2));
            }

          retval = idx_vector (idx);
        }
      else if (left_inf)
        {
          // Results in valid indices.  Optimize using lazy index.
          octave_idx_type zero = 0;
          for (octave_idx_type i = 0; i < nval; i++)
            {
              octave_idx_type j = idx.xelem (i) - 1;
              idx.xelem (i) = std::max (zero, j);
            }

          retval = idx_vector (idx);
        }
      else if (right_inf)
        {
          NDArray ridx (idx.dims ());

          for (octave_idx_type i = 0; i < nval; i++)
            {
              octave_idx_type j = idx.xelem (i);
              ridx.xelem (i) = std::min (j, n-1);
            }

          retval = ridx;
        }
    }
  else
    retval = idx;

  return retval;
}

DEFUN (lookup, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{idx} =} lookup (@var{table}, @var{y})
@deftypefnx {} {@var{idx} =} lookup (@var{table}, @var{y}, @var{opt})
Lookup values in a @strong{sorted} table.

This function is usually used as a prelude to interpolation.

If table is increasing, of length N and @code{idx = lookup (table, y)}, then
@code{table(idx(i)) <= y(i) < table(idx(i+1))} for all @code{y(i)} within the
table.  If @code{y(i) < table(1)} then @code{idx(i)} is 0.  If
@code{y(i) >= table(end)} or @code{isnan (y(i))} then @code{idx(i)} is N.

If the table is decreasing, then the tests are reversed.  For non-strictly
monotonic tables, empty intervals are always skipped.  The result is undefined
if @var{table} is not monotonic, or if @var{table} contains a NaN.

The complexity of the lookup is O(M*log(N)) where M is the size of @var{y}.
In the special case when @var{y} is also sorted, the complexity is
O(min (M*log(N), M+N)).

@var{table} and @var{y} can also be cell arrays of strings (or @var{y} can be a
single string).  In this case, string lookup is performed using lexicographical
comparison.

If @var{opts} is specified, it must be a string with letters indicating
additional options.

@table @code
@item m
Match.  @code{table(idx(i)) == y(i)} if @code{y(i)} occurs in table;
otherwise, @code{idx(i)} is zero.

@item b
Boolean.  @code{idx(i)} is a logical 1 or 0, indicating whether @code{y(i)}
is contained in table or not.

@item l
Left.  For numeric lookups the leftmost subinterval shall be extended to
minus infinity (i.e., all indices at least 1).

@item r
Right.  For numeric lookups the rightmost subinterval shall be extended to
infinity (i.e., all indices at most N-1).
@end table

@strong{Note}: If @var{table} is not sorted the results from @code{lookup}
will be unpredictable.
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    print_usage ();

  octave_value table = args(0);
  octave_value y = args(1);
  if (table.ndims () > 2 || (table.columns () > 1 && table.rows () > 1))
    warning ("lookup: table is not a vector");

  octave_value retval;

  bool num_case = ((table.isnumeric () && y.isnumeric ())
                   || (table.is_char_matrix () && y.is_char_matrix ()));
  bool str_case = table.iscellstr () && (y.is_string () || y.iscellstr ());
  bool left_inf = false;
  bool right_inf = false;
  bool match_idx = false;
  bool match_bool = false;

  if (nargin == 3)
    {
      std::string opt = args(2).xstring_value ("lookup: OPT must be a string");
      left_inf = contains_char (opt, 'l');
      right_inf = contains_char (opt, 'r');
      match_idx = contains_char (opt, 'm');
      match_bool = contains_char (opt, 'b');
      if (opt.find_first_not_of ("lrmb") != std::string::npos)
        error ("lookup: unrecognized option: %c",
               opt[opt.find_first_not_of ("lrmb")]);
    }

  if ((match_idx || match_bool) && (left_inf || right_inf))
    error ("lookup: m, b cannot be specified with l or r");
  else if (match_idx && match_bool)
    error ("lookup: only one of m or b can be specified");
  else if (str_case && (left_inf || right_inf))
    error ("lookup: l, r are not recognized for string lookups");

  if (num_case)
    {
      // In the case of a complex array, absolute values will be used for
      // compatibility (though it's not too meaningful).
      if (table.iscomplex ())
        table = table.abs ();

      if (y.iscomplex ())
        y = y.abs ();

      Array<octave_idx_type> idx;

      // PS: I learned this from data.cc
      if INT_ARRAY_LOOKUP (int8)
        else if INT_ARRAY_LOOKUP (int16)
          else if INT_ARRAY_LOOKUP (int32)
            else if INT_ARRAY_LOOKUP (int64)
              else if INT_ARRAY_LOOKUP (uint8)
                else if INT_ARRAY_LOOKUP (uint16)
                  else if INT_ARRAY_LOOKUP (uint32)
                    else if INT_ARRAY_LOOKUP (uint64)
                      else if (table.is_char_matrix () && y.is_char_matrix ())
                        retval = do_numeric_lookup (table.char_array_value (),
                                                    y.char_array_value (),
                                                    left_inf, right_inf,
                                                    match_idx, match_bool);
                      else if (table.is_single_type () || y.is_single_type ())
                        retval = do_numeric_lookup (table.float_array_value (),
                                                    y.float_array_value (),
                                                    left_inf, right_inf,
                                                    match_idx, match_bool);
                      else
                        retval = do_numeric_lookup (table.array_value (),
                                                    y.array_value (),
                                                    left_inf, right_inf,
                                                    match_idx, match_bool);
    }
  else if (str_case)
    {
      Array<std::string> str_table = table.cellstr_value ();
      Array<std::string> str_y (dim_vector (1, 1));

      if (y.iscellstr ())
        str_y = y.cellstr_value ();
      else
        str_y(0) = y.string_value ();

      Array<octave_idx_type> idx = str_table.lookup (str_y);
      octave_idx_type nval = str_y.numel ();

      // Post-process.
      if (match_bool)
        {
          boolNDArray match (idx.dims ());
          for (octave_idx_type i = 0; i < nval; i++)
            {
              octave_idx_type j = idx.xelem (i);
              match.xelem (i) = j != 0 && str_y(i) == str_table(j-1);
            }

          retval = match;
        }
      else if (match_idx)
        {
          NDArray ridx (idx.dims ());
          for (octave_idx_type i = 0; i < nval; i++)
            {
              octave_idx_type j = idx.xelem (i);
              ridx.xelem (i) = (j != 0 && str_y(i) == str_table(j-1) ? j : 0);
            }

          retval = ridx;
        }
      else
        retval = idx;
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (lookup (1:3, 0.5), 0)     # value before table
%!assert (lookup (1:3, 3.5), 3)     # value after table error
%!assert (lookup (1:3, 1.5), 1)     # value within table error
%!assert (lookup (1:3, [3,2,1]), [3,2,1])
%!assert (lookup ([1:4]', [1.2, 3.5]'), [1, 3]')
%!assert (lookup ([1:4], [1.2, 3.5]'), [1, 3]')
%!assert (lookup ([1:4]', [1.2, 3.5]), [1, 3])
%!assert (lookup ([1:4], [1.2, 3.5]), [1, 3])
%!assert (lookup (1:3, [3, 2, 1]), [3, 2, 1])
%!assert (lookup ([3:-1:1], [3.5, 3, 1.2, 2.5, 2.5]), [0, 1, 2, 1, 1])
%!assert (isempty (lookup ([1:3], [])))
%!assert (isempty (lookup ([1:3]', [])))
%!assert (lookup (1:3, [1, 2; 3, 0.5]), [1, 2; 3, 0])
%!assert (lookup (1:4, [1, 1.2; 3, 2.5], "m"), [1, 0; 3, 0])
%!assert (lookup (4:-1:1, [1, 1.2; 3, 2.5], "m"), [4, 0; 2, 0])
%!assert (lookup (1:4, [1, 1.2; 3, 2.5], "b"), logical ([1, 0; 3, 0]))
%!assert (lookup (4:-1:1, [1, 1.2; 3, 2.5], "b"), logical ([4, 0; 2, 0]))
%!
%!assert (lookup ({"apple","lemon","orange"}, {"banana","kiwi"; "ananas","mango"}), [1,1;0,2])
%!assert (lookup ({"apple","lemon","orange"}, "potato"), 3)
%!assert (lookup ({"orange","lemon","apple"}, "potato"), 0)
*/

OCTAVE_END_NAMESPACE(octave)
