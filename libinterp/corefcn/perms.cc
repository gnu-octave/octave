////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023-2024 The Octave Project Developers
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

#include <algorithm>
#include <numeric>

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE (octave)

static inline double
Factorial (octave_idx_type n)
{
  double ret = 1;
  for (octave_idx_type i = 2; i <= n; i++)
    ret *= i;
  return ret;
}

//
// Use C++ template to cater for the different octave array classes.
//

template <typename T>
static inline Array<T>
GetPerms (const Array<T>& ar_in, bool uniq_v = false)
{
  octave_idx_type m = ar_in.numel ();
  double nr;  // number of rows of resulting array

  // Setup index vector filled from 0..m-1
  OCTAVE_LOCAL_BUFFER (octave_idx_type, myvidx, m);
  std::iota (&myvidx[0], &myvidx[m], 0);

  const T *Ar = ar_in.data ();

  if (uniq_v)
    {
      // Mutual Comparison is used to detect duplicated values.
      // Using sort would be possible for numerical values and be of
      // O(n*log (n)) complexity instead of O(n*(n -1) / 2).  But sort
      // is not supported for the octave_value container (structs/cells).
      // Moreover, sort requires overhead for creating, filling, and sorting
      // the intermediate array which would need to be post-processed.
      // In practice, and because n must be very small, mutual comparison is
      // typically faster and consumes less memory.

      octave_idx_type N_el = 1;
      double denom = 1.0;
      // Number of unique permutations is n! / (n_el1! * n_el2! * ...)
      for (octave_idx_type i = 0; i < m - 1; i++)
        {
          for (octave_idx_type j = i + 1; j < m; j++)
            {
              bool isequal;
              if constexpr (std::is_same<T, octave_value>::value)
                // operator '==' is not supported for octave_value objects
                isequal = Ar[i].is_equal (Ar[j]);
              else
                isequal = (Ar[i] == Ar[j]);

              if (myvidx[j] > myvidx[i] && isequal)
                {
                  myvidx[j] = myvidx[i];  // not yet processed...
                  N_el++;
                }
              else
                {
                  denom *= Factorial (N_el);
                  N_el = 1;
                }
            }
        }
      denom *= Factorial (N_el);
      nr = Factorial (m) / denom;
    }
  else
    nr = Factorial (m);

  // Sort vector indices for inverse lexicographic order later.
  std::sort (myvidx, myvidx + m, std::greater<octave_idx_type> ());

  // Set up result array
  octave_idx_type n = static_cast<octave_idx_type> (nr);
  Array<T> res (dim_vector (n, m));
  T *Res = res.rwdata ();

  // Do the actual job
  octave_idx_type i = 0;
  do
    {
      for (octave_idx_type j = 0; j < m; j++)
        Res[i + j * n] = Ar[myvidx[j]];
      i++;
    }
  while (std::next_permutation (myvidx, myvidx + m, std::greater<int> ()));

  return res;
}

DEFUN (perms, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{P} =} perms (@var{v})
@deftypefnx {} {@var{P} =} perms (@var{v}, "unique")
Generate all permutations of vector @var{v} with one row per permutation.

Results are returned in reverse lexicographic order if @var{v} is in ascending
order.  If @var{v} is in a different permutation, then the result is permuted
that way too.  Consequently, an input in descending order yields a result in
normal lexicographic order.  The result has size
@code{factorial (@var{n}) * @var{n}}, where @var{n} is the length of @var{v}.
Any repeated elements are included in the output.

If the optional argument @qcode{"unique"} is given then only unique
permutations are returned, using less memory and taking less time than calling
@code{unique (perms (@var{v}), "rows")}.

Example 1

@example
@group
perms ([1, 2, 3])
@result{}
3   2   1
3   1   2
2   3   1
2   1   3
1   3   2
1   2   3
@end group
@end example

Example 2

@example
@group
perms ([1, 1, 2, 2], "unique")
@result{}
2   2   1   1
2   1   2   1
2   1   1   2
1   2   2   1
1   2   1   2
1   1   2   2
@end group
@end example

Programming Note: If the @qcode{"unique"} option is not used, the length of
@var{v} should be no more than 10-12 to limit memory consumption.  Even with
@qcode{"unique"}, there should be no more than 10-12 unique elements in
@var{v}.
@seealso{permute, randperm, nchoosek}

@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  octave_value retval;

  // Parameter check "unique"
  bool uniq_v = false;
  if (nargin == 2)
    {
      const charMatrix opt = args (1).char_matrix_value ();
      const char *str = opt.data ();
      if (std::string (str, opt.cols ()) != "unique")
        {
          error ("perms: option must be the string \"unique\".");
        }
      uniq_v = true;
    }

  if (! (args (0).is_matrix_type () || args (0).is_range ()
         || args (0).iscell () || args (0).is_scalar_type ()
         || args (0).isstruct ()))
    {
      error ("perms: V must be a matrix, range, cell array, "
             "struct, or a scalar.");
    }

  std::string clname = args (0).class_name ();

  // Execute main permutation code for the different classes
  if (clname == "double")
    retval = GetPerms<double> (args (0).array_value (), uniq_v);
  else if (clname == "single")
    retval = GetPerms<float> (args (0).float_array_value (), uniq_v);
  else if (clname == "logical")
    retval = GetPerms<bool> (args (0).bool_array_value (), uniq_v);
  else if (clname == "char")
    retval = GetPerms<char> (args (0).char_array_value (), uniq_v);
  else if (clname == "int8")
    retval = GetPerms<octave_int8> (args (0).int8_array_value (), uniq_v);
  else if (clname == "int16")
    retval = GetPerms<octave_int16> (args (0).int16_array_value (), uniq_v);
  else if (clname == "int32")
    retval = GetPerms<octave_int32> (args (0).int32_array_value (), uniq_v);
  else if (clname == "int64")
    retval = GetPerms<octave_int64> (args (0).int64_array_value (), uniq_v);
  else if (clname == "uint8")
    retval = GetPerms<octave_uint8> (args (0).uint8_array_value (), uniq_v);
  else if (clname == "uint16")
    retval = GetPerms<octave_uint16> (args (0).uint16_array_value (), uniq_v);
  else if (clname == "uint32")
    retval = GetPerms<octave_uint32> (args (0).uint32_array_value (), uniq_v);
  else if (clname == "uint64")
    retval = GetPerms<octave_uint64> (args (0).uint64_array_value (), uniq_v);
  else if (clname == "cell")
    retval = GetPerms<octave_value> (args (0).cell_value (), uniq_v);
  else if (clname == "struct")
    {
      const octave_map map_in (args (0).map_value ());
      string_vector fn = map_in.fieldnames ();
      if (fn.numel () == 0 && map_in.numel () != 0)
        {
          octave_scalar_map out;
          retval = out;
        }
      else
        {
          octave_map out;
          if (fn.numel () == 0)
            {
              out = octave_map (dim_vector (1, 0));
            }
          else
            {
              for (octave_idx_type i = 0; i < fn.numel (); i++)
                {
                  out.assign (fn (i), GetPerms<octave_value>
                                      (map_in.contents (fn (i)), uniq_v));
                }
            }
          retval = out;
        }
    }
  else  // none of the above class criteria were met
    {
      warning ("perms: unable to permute for class %s", clname.c_str ());
      // retval stays empty
    }
  return ovl (retval);
}

/*

%!assert (rows (perms (1:6)), factorial (6))
%!assert (perms (pi), pi)
%!assert (perms ([e, pi]), [pi, e; e, pi])
%!assert (perms ([pi, e]), [e, pi; pi, e])
%!assert (perms ([1 2 3]), [3 2 1; 3 1 2; 2 3 1; 2 1 3; 1 3 2; 1 2 3])
%!assert (sortrows (perms (1:5)), sortrows (perms ([2 5 4 1 3]')))
%!assert (perms ("abc"), char ("cba", "cab", "bca", "bac", "acb", "abc"))
%!assert (sortrows (perms ("fobar")), unique (perms ("fobar"), "rows"))
%!assert (unique (perms (1:5)(:))', 1:5)
%!assert (perms (int8 (1:4)), int8 (perms (1:4)))

%!assert (sortrows (perms ("abb", "unique")), ["abb"; "bab"; "bba"])
%!assert (size (perms ([1 1 1 1 2 2 2 3 3], "unique")), [1260 9])
%!assert (size (perms (int8([1 1 1 1 1 2 2 2 2 3 3 3]), "unique")), [27720 12])

## Should work for any array type, such as cells and structs,
## and not only for numeric data.

%!assert <*52431> (perms ({1}), {1})
%!assert <*52431> (perms ({0.1, "foo"}), {"foo", 0.1; 0.1, "foo"})
%!assert <*52431> (perms ({"foo", 0.1}), {0.1, "foo"; "foo", 0.1})
%!assert <*52431> (perms ({"foo"; 0.1}), {0.1, "foo"; "foo", 0.1})
%!assert <*52431> (perms ({0.1; "foo"}), {"foo", 0.1; 0.1, "foo"})
%!assert <*52431> (perms ({"foo", "bar"}), {"bar", "foo"; "foo", "bar"})
%!assert <*52431> (perms ({"bar", "foo"}), {"foo", "bar"; "bar", "foo"})
%!
%!assert <*52431> (perms (struct ()), struct ())
%!assert <*52431> (perms (struct ("foo", {1, 2})),
%!                struct ("foo", {2, 1; 1, 2}))
%!assert <*52431> (perms (struct ("foo", {1, 2}, "bar", {3, 4})),
%!                struct ("foo", {2, 1; 1, 2}, "bar", {4, 3; 3, 4}))

## Also sort logical input with order dependent on the input order and
## not their values.

%!assert <*52431> (perms (logical ([1 0])), logical ([0 1;, 1 0]))
%!assert <*52431> (perms (logical ([0 1])), logical ([1 0; 0 1]))
%!assert <*52431> (perms (logical ([0 1 0])),
%!                logical ([0 1 0; 0 0 1; 1 0 0; 1 0 0; 0 0 1; 0 1 0]))
%!assert <*52431> (perms (logical ([0 1 1])),
%!                logical ([1 1 0; 1 0 1; 1 1 0; 1 0 1; 0 1 1; 0 1 1]))

%!assert <*52432> (perms ([]), reshape ([], 1, 0))
%!assert <*52432> (perms (single ([])), reshape (single ([]), 1, 0))
%!assert <*52432> (perms (int8 ([])), reshape (int8 ([]), 1, 0))
%!assert <*52432> (perms ({}), cell (1, 0))

%!test <*52432>
%! s = struct ();
%! s(1) = [];
%! assert (perms (reshape (s, 0, 0)), reshape (s, 1, 0));
%! assert (perms (reshape (s, 0, 1)), reshape (s, 1, 0));

## test if "unique" works also for cell arrays
%!assert <*63965> (perms ({"foo"; "foo"}, "unique"), {"foo", "foo"})
%!assert <*63965> (perms ({"foo", "foo", "bar"}, "unique"), ...
%!                        {"bar", "foo", "foo";
%!                         "foo", "bar", "foo";
%!                         "foo", "foo", "bar"})

## Test input validation
%!error <Invalid call> perms ()
%!error <option must be the string "unique"> perms (1:5, "foobar")
%!error <option must be the string "unique"> perms (1:5, {"foo"})

*/

OCTAVE_END_NAMESPACE (octave)
