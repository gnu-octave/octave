////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2004-2023 The Octave Project Developers
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
#include "Array.h"
#include "Sparse.h"
#include "mx-base.h"

#include "ov.h"
#include "Cell.h"

#include "defun.h"
#include "error.h"
#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// The bulk of the work.
template <typename T>
static Array<T>
do_tril (const Array<T>& a, octave_idx_type k, bool pack)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.columns ();
  const T *avec = a.data ();
  octave_idx_type zero = 0;

  if (pack)
    {
      octave_idx_type j1 = std::min (std::max (zero, k), nc);
      octave_idx_type j2 = std::min (std::max (zero, nr + k), nc);
      octave_idx_type n = j1 * nr + ((j2 - j1) * (nr-(j1-k) + nr-(j2-1-k))) / 2;
      Array<T> r (dim_vector (n, 1));
      T *rvec = r.fortran_vec ();
      for (octave_idx_type j = 0; j < nc; j++)
        {
          octave_idx_type ii = std::min (std::max (zero, j - k), nr);
          rvec = std::copy (avec + ii, avec + nr, rvec);
          avec += nr;
        }

      return r;
    }
  else
    {
      Array<T> r (a.dims ());
      T *rvec = r.fortran_vec ();
      for (octave_idx_type j = 0; j < nc; j++)
        {
          octave_idx_type ii = std::min (std::max (zero, j - k), nr);
          std::fill (rvec, rvec + ii, T ());
          std::copy (avec + ii, avec + nr, rvec + ii);
          avec += nr;
          rvec += nr;
        }

      return r;
    }
}

template <typename T>
static Array<T>
do_triu (const Array<T>& a, octave_idx_type k, bool pack)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.columns ();
  const T *avec = a.data ();
  octave_idx_type zero = 0;

  if (pack)
    {
      octave_idx_type j1 = std::min (std::max (zero, k), nc);
      octave_idx_type j2 = std::min (std::max (zero, nr + k), nc);
      octave_idx_type n
        = ((j2 - j1) * ((j1+1-k) + (j2-k))) / 2 + (nc - j2) * nr;
      Array<T> r (dim_vector (n, 1));
      T *rvec = r.fortran_vec ();
      for (octave_idx_type j = 0; j < nc; j++)
        {
          octave_idx_type ii = std::min (std::max (zero, j + 1 - k), nr);
          rvec = std::copy (avec, avec + ii, rvec);
          avec += nr;
        }

      return r;
    }
  else
    {
      Array<T> r (a.dims ());
      T *rvec = r.fortran_vec ();
      for (octave_idx_type j = 0; j < nc; j++)
        {
          octave_idx_type ii = std::min (std::max (zero, j + 1 - k), nr);
          std::copy (avec, avec + ii, rvec);
          std::fill (rvec + ii, rvec + nr, T ());
          avec += nr;
          rvec += nr;
        }

      return r;
    }
}

// These two are by David Bateman.
// FIXME: optimizations possible. "pack" support missing.

template <typename T>
static Sparse<T>
do_tril (const Sparse<T>& a, octave_idx_type k, bool pack)
{
  if (pack) // FIXME
    error (R"(tril: "pack" not implemented for sparse matrices)");

  Sparse<T> m = a;
  octave_idx_type nc = m.cols ();

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = m.cidx (j); i < m.cidx (j+1); i++)
      if (m.ridx (i) < j-k)
        m.data(i) = 0.;

  m.maybe_compress (true);

  return m;
}

template <typename T>
static Sparse<T>
do_triu (const Sparse<T>& a, octave_idx_type k, bool pack)
{
  if (pack) // FIXME
    error (R"(triu: "pack" not implemented for sparse matrices)");

  Sparse<T> m = a;
  octave_idx_type nc = m.cols ();

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = m.cidx (j); i < m.cidx (j+1); i++)
      if (m.ridx (i) > j-k)
        m.data(i) = 0.;

  m.maybe_compress (true);
  return m;
}

// Convenience dispatchers.
template <typename T>
static Array<T>
do_trilu (const Array<T>& a, octave_idx_type k, bool lower, bool pack)
{
  return lower ? do_tril (a, k, pack) : do_triu (a, k, pack);
}

template <typename T>
static Sparse<T>
do_trilu (const Sparse<T>& a, octave_idx_type k, bool lower, bool pack)
{
  return lower ? do_tril (a, k, pack) : do_triu (a, k, pack);
}

static octave_value
do_trilu (const std::string& name,
          const octave_value_list& args)
{
  bool lower = (name == "tril");

  int nargin = args.length ();
  bool pack = false;

  if (nargin >= 2 && args(nargin-1).is_string ())
    {
      pack = (args(nargin-1).string_value () == "pack");
      nargin--;
    }

  if (nargin < 1 || nargin > 2)
    print_usage ();

  octave_idx_type k = 0;
  if (nargin == 2)
    k = args(1).idx_type_value (true);

  octave_value arg = args(0);

  dim_vector dims = arg.dims ();
  if (dims.ndims () != 2)
    error ("%s: need a 2-D matrix", name.c_str ());
  else if (k < -dims(0))
    k = -dims(0);
  else if (k > dims(1))
    k = dims(1);

  octave_value retval;

  switch (arg.builtin_type ())
    {
    case btyp_double:
      if (arg.issparse ())
        retval = do_trilu (arg.sparse_matrix_value (), k, lower, pack);
      else
        retval = do_trilu (arg.array_value (), k, lower, pack);
      break;

    case btyp_complex:
      if (arg.issparse ())
        retval = do_trilu (arg.sparse_complex_matrix_value (), k, lower,
                           pack);
      else
        retval = do_trilu (arg.complex_array_value (), k, lower, pack);
      break;

    case btyp_bool:
      if (arg.issparse ())
        retval = do_trilu (arg.sparse_bool_matrix_value (), k, lower,
                           pack);
      else
        retval = do_trilu (arg.bool_array_value (), k, lower, pack);
      break;

#define ARRAYCASE(TYP)                                                  \
      case btyp_ ## TYP:                                                \
        retval = do_trilu (arg.TYP ## _array_value (), k, lower, pack); \
        break

      ARRAYCASE (float);
      ARRAYCASE (float_complex);
      ARRAYCASE (int8);
      ARRAYCASE (int16);
      ARRAYCASE (int32);
      ARRAYCASE (int64);
      ARRAYCASE (uint8);
      ARRAYCASE (uint16);
      ARRAYCASE (uint32);
      ARRAYCASE (uint64);
      ARRAYCASE (char);

#undef ARRAYCASE

    default:
      {
        // Generic code that works on octave-values, that is slow
        // but will also work on arbitrary user types
        if (pack) // FIXME
          error (R"(%s: "pack" not implemented for class %s)",
                 name.c_str (), arg.class_name ().c_str ());

        octave_value tmp = arg;
        if (arg.isempty ())
          return arg;

        octave_idx_type nr = dims(0);
        octave_idx_type nc = dims(1);

        // The sole purpose of this code is to force the correct matrix size.
        // This would not be necessary if the octave_value resize function
        // allowed a fill_value.  It also allows odd attributes in some user
        // types to be handled.  With a fill_value, it should be replaced with
        //
        // octave_value_list ov_idx;
        // tmp = tmp.resize(dim_vector (0,0)).resize (dims, fill_value);

        octave_value_list ov_idx;
        std::list<octave_value_list> idx_tmp;
        ov_idx(1) = static_cast<double> (nc+1);
        ov_idx(0) = range<double> (1, nr);
        idx_tmp.push_back (ov_idx);
        ov_idx(1) = static_cast<double> (nc);
        tmp = tmp.resize (dim_vector (0, 0));
        tmp = tmp.subsasgn ("(", idx_tmp, arg.index_op (ov_idx));
        tmp = tmp.resize (dims);

        octave_idx_type one = 1;

        if (lower)
          {
            octave_idx_type st = std::min (nc, nr + k);

            for (octave_idx_type j = 1; j <= st; j++)
              {
                octave_idx_type nr_limit = std::max (one, j - k);
                ov_idx(1) = static_cast<double> (j);
                ov_idx(0) = range<double> (nr_limit, nr);
                std::list<octave_value_list> idx;
                idx.push_back (ov_idx);

                tmp = tmp.subsasgn ("(", idx, arg.index_op (ov_idx));
              }
          }
        else
          {
            octave_idx_type st = std::max (k + 1, one);

            for (octave_idx_type j = st; j <= nc; j++)
              {
                octave_idx_type nr_limit = std::min (nr, j - k);
                ov_idx(1) = static_cast<double> (j);
                ov_idx(0) = range<double> (1, nr_limit);
                std::list<octave_value_list> idx;
                idx.push_back (ov_idx);

                tmp = tmp.subsasgn ("(", idx, arg.index_op (ov_idx));
              }
          }

        retval = tmp;
      }
    }

  return retval;
}

DEFUN (tril, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{A_LO} =} tril (@var{A})
@deftypefnx {} {@var{A_LO} =} tril (@var{A}, @var{k})
@deftypefnx {} {@var{A_LO} =} tril (@var{A}, @var{k}, @var{pack})
Return a new matrix formed by extracting the lower triangular part of the
matrix @var{A}, and setting all other elements to zero.

The optional second argument specifies how many diagonals above or below the
main diagonal should also be set to zero.  The default value of @var{k} is
zero which includes the main diagonal as part of the result.  If the value of
@var{k} is a nonzero integer then the selection of elements starts at an offset
of @var{k} diagonals above the main diagonal for positive @var{k} or below the
main diagonal for negative @var{k}.  The absolute value of @var{k} may not be
greater than the number of subdiagonals or superdiagonals.

Example 1 : exclude main diagonal

@example
@group
tril (ones (3), -1)
     @result{}  0  0  0
         1  0  0
         1  1  0
@end group
@end example

@noindent

Example 2 : include first superdiagonal

@example
@group
tril (ones (3), 1)
     @result{}  1  1  0
         1  1  1
         1  1  1
@end group
@end example

If the optional third argument @qcode{"pack"} is given then the extracted
elements are not inserted into a matrix, but instead stacked column-wise one
above another, and returned as a column vector.
@seealso{triu, istril, diag}
@end deftypefn */)
{
  return do_trilu ("tril", args);
}

DEFUN (triu, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{A_UP} =} triu (@var{A})
@deftypefnx {} {@var{A_UP} =} triu (@var{A}, @var{k})
@deftypefnx {} {@var{A_UP} =} triu (@var{A}, @var{k}, @var{pack})
Return a new matrix formed by extracting the upper triangular part of the
matrix @var{A}, and setting all other elements to zero.

The optional second argument specifies how many diagonals above or below the
main diagonal should also be set to zero.  The default value of @var{k} is
zero which includes the main diagonal as part of the result.  If the value of
@var{k} is a nonzero integer then the selection of elements starts at an offset
of @var{k} diagonals above the main diagonal for positive @var{k} or below the
main diagonal for negative @var{k}.  The absolute value of @var{k} may not be
greater than the number of subdiagonals or superdiagonals.

Example 1 : exclude main diagonal

@example
@group
triu (ones (3), 1)
     @result{}  0  1  1
         0  0  1
         0  0  0
@end group
@end example

@noindent

Example 2 : include first subdiagonal

@example
@group
triu (ones (3), -1)
     @result{}  1  1  1
         1  1  1
         0  1  1
@end group
@end example

If the optional third argument @qcode{"pack"} is given then the extracted
elements are not inserted into a matrix, but instead stacked column-wise one
above another, and returned as a column vector.
@seealso{tril, istriu, diag}
@end deftypefn */)
{
  return do_trilu ("triu", args);
}

/*
%!shared a, l2, l1, l0, lm1, lm2, lm3, lm4
%! a = [1, 2, 3; 4, 5, 6; 7, 8, 9; 10, 11, 12];
%!
%! l2 = [1, 2, 3; 4, 5, 6; 7, 8, 9; 10, 11, 12];
%! l1 = [1, 2, 0; 4, 5, 6; 7, 8, 9; 10, 11, 12];
%! l0 = [1, 0, 0; 4, 5, 0; 7, 8, 9; 10, 11, 12];
%! lm1 = [0, 0, 0; 4, 0, 0; 7, 8, 0; 10, 11, 12];
%! lm2 = [0, 0, 0; 0, 0, 0; 7, 0, 0; 10, 11, 0];
%! lm3 = [0, 0, 0; 0, 0, 0; 0, 0, 0; 10, 0, 0];
%! lm4 = [0, 0, 0; 0, 0, 0; 0, 0, 0; 0, 0, 0];
%!
%!assert (tril (a, 3), l2)
%!assert (tril (a, 2), l2)
%!assert (tril (a, 1), l1)
%!assert (tril (a, 0), l0)
%!assert (tril (a), l0)
%!assert (tril (a, -1), lm1)
%!assert (tril (a, -2), lm2)
%!assert (tril (a, -3), lm3)
%!assert (tril (a, -4), lm4)
%!assert (tril (a, -5), lm4)

%!shared a, u3, u2, u1, u0, um1, um2, um3
%!
%! a = [1, 2, 3; 4, 5, 6; 7, 8, 9; 10, 11, 12];
%!
%! u3 = [0, 0, 0; 0, 0, 0; 0, 0, 0; 0, 0, 0];
%! u2 = [0, 0, 3; 0, 0, 0; 0, 0, 0; 0, 0, 0];
%! u1 = [0, 2, 3; 0, 0, 6; 0, 0, 0; 0, 0, 0];
%! u0 = [1, 2, 3; 0, 5, 6; 0, 0, 9; 0, 0, 0];
%! um1 = [1, 2, 3; 4, 5, 6; 0, 8, 9; 0, 0, 12];
%! um2 = [1, 2, 3; 4, 5, 6; 7, 8, 9; 0, 11, 12];
%! um3 = [1, 2, 3; 4, 5, 6; 7, 8, 9; 10, 11, 12];
%!
%!assert (triu (a, 4), u3)
%!assert (triu (a, 3), u3)
%!assert (triu (a, 2), u2)
%!assert (triu (a, 1), u1)
%!assert (triu (a, 0), u0)
%!assert (triu (a), u0)
%!assert (triu (a, -1), um1)
%!assert (triu (a, -2), um2)
%!assert (triu (a, -3), um3)
%!assert (triu (a, -4), um3)

%!error tril ()
%!error triu ()
*/

OCTAVE_END_NAMESPACE(octave)
