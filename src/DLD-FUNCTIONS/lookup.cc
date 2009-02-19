/*

Copyright (C) 2008 VZLU Prague a.s., Czech Republic

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

// Author: Jaroslav Hajek <highegg@gmail.com>

#include <cctype>
#include <functional>
#include <algorithm>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "dNDArray.h"
#include "CNDArray.h"

#include "Cell.h"
#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "ov.h"

static
bool
contains_char (const std::string& str, char c)
{
  return (str.find (c) != std::string::npos 
	  || str.find (std::toupper (c)) != std::string::npos);
}

// case-insensitive character comparison functors
struct icmp_char_lt : public std::binary_function<char, char, bool>
{
  bool operator () (char x, char y) const
    { return std::toupper (x) < std::toupper (y); }
};

struct icmp_char_gt : public std::binary_function<char, char, bool>
{
  bool operator () (char x, char y) const
    { return std::toupper (x) > std::toupper (y); }
};

// FIXME: maybe these should go elsewhere?
// case-insensitive ascending comparator
static bool
stri_comp_lt (const std::string& a, const std::string& b)
{
  return std::lexicographical_compare (a.begin (), a.end (), 
                                       b.begin (), b.end (),
                                       icmp_char_lt());
}

// case-insensitive descending comparator
static bool
stri_comp_gt (const std::string& a, const std::string& b)
{
  return std::lexicographical_compare (a.begin (), a.end (), 
                                       b.begin (), b.end (),
                                       icmp_char_gt());
}

template <class T>
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
// We'd need a dispatch on two arguments. Hmmm...

#define INT_ARRAY_LOOKUP(TYPE) \
  (table.is_ ## TYPE ## _type () && y.is_ ## TYPE ## _type ()) \
    idx = table.TYPE ## _array_value ().lookup (y.TYPE ## _array_value (), \
                                                UNSORTED, left_inf, right_inf);

DEFUN_DLD (lookup, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{idx} =} lookup (@var{table}, @var{y}, @var{opt})\n\
Lookup values in a sorted table.  Usually used as a prelude to\n\
interpolation.\n\
\n\
If table is strictly increasing and @code{idx = lookup (table, y)}, then\n\
@code{table(idx(i)) <= y(i) < table(idx(i+1))} for all @code{y(i)}\n\
within the table.  If @code{y(i) < table (1)} then\n\
@code{idx(i)} is 0. If @code{y(i) >= table(end)} then\n\
@code{idx(i)} is @code{table(n)}.\n\
\n\
If the table is strictly decreasing, then the tests are reversed.\n\
There are no guarantees for tables which are non-monotonic or are not\n\
strictly monotonic.\n\
\n\
The algorithm used by lookup is standard binary search, with optimizations\n\
to speed up the case of partially ordered arrays (dense downsampling).\n\
In particular, looking up a single entry is of logarithmic complexity\n\
(unless a conversion occurs due to non-numeric or unequal types).\n\
\n\
@var{table} and @var{y} can also be cell arrays of strings\n\
(or @var{y} can be a single string). In this case, string lookup\n\
is performed using lexicographical comparison.\n\
\n\
If @var{opts} is specified, it shall be a string with letters indicating\n\
additional options.\n\
For numeric lookup, 'l' in @var{opts} indicates that\n\
the leftmost subinterval shall be extended to infinity (i.e. all indices\n\
at least 1), and 'r' indicates that the rightmost subinterval shall be\n\
extended to infinity (i.e. all indices at most n-1).\n\
\n\
For string lookup, 'i' indicates case-insensitive comparison.\n\
@end deftypefn") 
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 2 || nargin > 3 || (nargin == 3 && ! args(2).is_string ()))
    {
      print_usage ();
      return retval;
    }

  octave_value table = args(0), y = args(1);
  if (table.ndims () > 2 || (table.columns () > 1 && table.rows () > 1))
    warning ("lookup: table is not a vector");

  bool num_case = table.is_numeric_type () && y.is_numeric_type ();
  bool str_case = table.is_cellstr () && (y.is_string () || y.is_cellstr ());

  if (num_case) 
    {
      bool left_inf = false;
      bool right_inf = false;

      if (nargin == 3)
        {
          std::string opt = args(2).string_value ();
          left_inf = contains_char (opt, 'l');
          right_inf = contains_char (opt, 'r');
        }

      // In the case of a complex array, absolute values will be used for compatibility
      // (though it's not too meaningful).
      
      if (table.is_complex_type ())
        table = table.abs ();

      if (y.is_complex_type ())
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
      else if (table.is_single_type () || y.is_single_type ())
        idx = table.float_array_value ().lookup (y.float_array_value (), 
                                                 UNSORTED, left_inf, right_inf);
      else
        idx = table.array_value ().lookup (y.array_value (), 
                                           UNSORTED, left_inf, right_inf);

      retval(0) = NDArray (idx);

    }
  else if (str_case)
    {
      Array<std::string> str_table = table.cellstr_value ();
      
      // Here we'll use octave_sort directly to avoid converting the array
      // for case-insensitive comparison.

      bool icase = false;

      // check for case-insensitive option
      if (nargin == 3)
        {
          std::string opt = args(2).string_value ();
          icase = contains_char (opt, 'i');
        }

      sortmode mode = (icase ? get_sort_mode (str_table, stri_comp_gt)
                       : get_sort_mode (str_table));

      bool (*str_comp) (const std::string&, const std::string&);

      // pick the correct comparator
      if (mode == DESCENDING)
        str_comp = icase ? stri_comp_gt : octave_sort<std::string>::descending_compare;
      else
        str_comp = icase ? stri_comp_lt : octave_sort<std::string>::ascending_compare;

      octave_sort<std::string> lsort (str_comp);
      if (y.is_cellstr ())
        {
          Array<std::string> str_y = y.cellstr_value ();

          Array<octave_idx_type> idx (str_y.dims ());

          lsort.lookup (str_table.data (), str_table.nelem (), str_y.data (),
                        str_y.nelem (), idx.fortran_vec ());

          retval(0) = NDArray (idx);
        }
      else if (y.is_string ())
        {
          std::string str_y = y.string_value ();

          octave_idx_type idx;

          lsort.lookup (str_table.data (), str_table.nelem (), &str_y,
                        1, &idx);

          retval(0) = idx;
        }
    }
  else
    print_usage ();

  return retval;

}  

/*
%!assert (real(lookup(1:3, 0.5)), 0)     # value before table
%!assert (real(lookup(1:3, 3.5)), 3)     # value after table error
%!assert (real(lookup(1:3, 1.5)), 1)     # value within table error
%!assert (real(lookup(1:3, [3,2,1])), [3,2,1])
%!assert (real(lookup([1:4]', [1.2, 3.5]')), [1, 3]');
%!assert (real(lookup([1:4], [1.2, 3.5]')), [1, 3]');
%!assert (real(lookup([1:4]', [1.2, 3.5])), [1, 3]);
%!assert (real(lookup([1:4], [1.2, 3.5])), [1, 3]);
%!assert (real(lookup(1:3, [3, 2, 1])), [3, 2, 1]);
%!assert (real(lookup([3:-1:1], [3.5, 3, 1.2, 2.5, 2.5])), [0, 1, 2, 1, 1])
%!assert (isempty(lookup([1:3], [])))
%!assert (isempty(lookup([1:3]', [])))
%!assert (real(lookup(1:3, [1, 2; 3, 0.5])), [1, 2; 3, 0]);
%!
%!assert (real(lookup({"apple","lemon","orange"}, {"banana","kiwi"; "ananas","mango"})), [1,1;0,2])
%!assert (real(lookup({"apple","lemon","orange"}, "potato")), 3)
%!assert (real(lookup({"orange","lemon","apple"}, "potato")), 0)
*/

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
