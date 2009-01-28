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
#include "oct-lookup.h"

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

// normal ascending comparator
static bool
ov_str_less (const octave_value& a, const octave_value& b)
{
  return a.string_value () < b.string_value ();
}

// normal descending comparator
static bool
ov_str_greater (const octave_value& a, const octave_value& b)
{
  return a.string_value () > b.string_value ();
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

// case-insensitive ascending comparator
static bool
ov_stri_less (const octave_value& a, const octave_value& b)
{
  std::string as = a.string_value ();
  std::string bs = b.string_value ();

  return std::lexicographical_compare (as.begin (), as.end (), 
                                       bs.begin (), bs.end (),
                                       icmp_char_lt());
}

// case-insensitive descending comparator
static bool
ov_stri_greater (const octave_value& a, const octave_value& b)
{
  std::string as = a.string_value ();
  std::string bs = b.string_value ();

  return std::lexicographical_compare (as.begin (), as.end (), 
                                       bs.begin (), bs.end (),
                                       icmp_char_gt());
}

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
In particular, looking up a single entry is of binary complexity.\n\
\n\
@var{table} and @var{y} can also be a cell array of strings\n\
(or @var{y} can be a single string). In this case, string lookup\n\
is performed using lexicographical comparison.\n\
If @var{opts} is specified, it shall be a string with letters indicating\n\
additional options.\n\
\n\
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

  octave_value argtable = args(0), argy = args(1);
  if (argtable.ndims () > 2 || (argtable.columns () > 1 && argtable.rows () > 1))
    warning ("lookup: table is not a vector");

  bool num_case = argtable.is_numeric_type () && argy.is_numeric_type ();
  bool str_case = argtable.is_cell () && (argy.is_cell () || argy.is_string ());

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

      // in the case of a complex array, absolute values will be used for compatibility
      // (though it's not too meaningful).
      ArrayN<octave_idx_type> idx;

      if (argtable.is_single_type () || argy.is_single_type ())
	{
	  FloatNDArray table = (argtable.is_complex_type ()) 
	    ? argtable.float_complex_array_value ().abs ()
	    : argtable.float_array_value ();

	  FloatNDArray y = (argy.is_complex_type ()) 
	    ? argy.float_complex_array_value ().abs ()
	    : argy.float_array_value ();

	  idx = ArrayN<octave_idx_type> (y.dims ());

	  // determine whether the array is descending. 
	  bool desc = is_descending (table.data (), table.length ());
	  octave_idx_type offset = left_inf ? 1 : 0;
	  octave_idx_type size = table.length () - offset - (right_inf ? 1 : 0);
	  if (size < 0) 
	    size = 0;

	  if (desc)
	    seq_lookup (table.data (), offset, size, 
			y.data (), y.length (), idx.fortran_vec (),
			std::greater<float> ());
	  else
	    seq_lookup (table.data (), offset, size, 
			y.data (), y.length (), idx.fortran_vec (),
			std::less<float> ());
	}
      else
	{
	  NDArray table = (argtable.is_complex_type ()) 
	    ? argtable.complex_array_value ().abs ()
	    : argtable.array_value ();

	  NDArray y = (argy.is_complex_type ()) 
	    ? argy.complex_array_value ().abs ()
	    : argy.array_value ();

	  idx = ArrayN<octave_idx_type> (y.dims ());

	  // determine whether the array is descending. 
	  bool desc = is_descending (table.data (), table.length ());
	  octave_idx_type offset = left_inf ? 1 : 0;
	  octave_idx_type size = table.length () - offset - (right_inf ? 1 : 0);
	  if (size < 0) 
	    size = 0;

	  if (desc)
	    seq_lookup (table.data (), offset, size, 
			y.data (), y.length (), idx.fortran_vec (),
			std::greater<double> ());
	  else
	    seq_lookup (table.data (), offset, size, 
			y.data (), y.length (), idx.fortran_vec (),
			std::less<double> ());
	}

      retval(0) = NDArray (idx);
    }
  else if (str_case)
    {
      Cell table = argtable.cell_value ();
      
      bool (*ov_str_comp) (const octave_value&, const octave_value&);

      bool icase = false;

      // check for case-insensitive option
      if (nargin == 3)
        {
          std::string opt = args(2).string_value ();
          icase = contains_char (opt, 'i');
        }

      // pick the correct comparator
      if (icase)
        {
          if (is_descending (table.data (), table.length (), ov_stri_less))
            ov_str_comp = ov_stri_greater;
          else
            ov_str_comp = ov_stri_less;
        }
      else
        {
          if (is_descending (table.data (), table.length (), ov_str_less))
            ov_str_comp = ov_str_greater;
          else
            ov_str_comp = ov_str_less;
        }


      // query just the first cell to verify it's a string
      if (table(0).is_string ())
        {
          if (argy.is_cell ())
            {
              Cell y = argy.cell_value ();
              ArrayN<octave_idx_type> idx (y.dims ());



              for (int i = 0; i < y.numel (); i++)
                  idx(i) = bin_lookup (table.data (), table.length (), y(i), 
                                       std::ptr_fun (ov_str_comp));

              retval(0) = NDArray (idx);
            }
          else
            {
              octave_idx_type idx;

              idx = bin_lookup (table.data (), table.length (), argy, 
                                std::ptr_fun (ov_str_comp));

              retval(0) = static_cast<double> (idx);
            }
        }
      else
        error("lookup: table is not a cell array of strings.");
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
