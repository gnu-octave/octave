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

#include "quit.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Find at most N_TO_FIND nonzero elements in NDA.  Search forward if
// DIRECTION is 1, backward if it is -1.  NARGOUT is the number of
// output arguments.  If N_TO_FIND is -1, find all nonzero elements.

template <typename T>
octave_value_list
find_nonzero_elem_idx (const Array<T>& nda, int nargout,
                       octave_idx_type n_to_find, int direction)
{
  octave_value_list retval ((nargout == 0 ? 1 : nargout), Matrix ());

  Array<octave_idx_type> idx;
  if (n_to_find >= 0)
    idx = nda.find (n_to_find, direction == -1);
  else
    idx = nda.find ();

  // The maximum element is always at the end.
  octave_idx_type iext = (idx.isempty () ? 0
                          : idx.xelem (idx.numel () - 1) + 1);

  switch (nargout)
    {
    default:
    case 3:
      retval(2) = Array<T> (nda.index (idx_vector (idx)));
      OCTAVE_FALLTHROUGH;

    case 2:
      {
        Array<octave_idx_type> jdx (idx.dims ());
        octave_idx_type n = idx.numel ();
        octave_idx_type nr = nda.rows ();
        for (octave_idx_type i = 0; i < n; i++)
          {
            jdx.xelem (i) = idx.xelem (i) / nr;
            idx.xelem (i) %= nr;
          }
        iext = -1;
        retval(1) = idx_vector (jdx, -1);
      }
      OCTAVE_FALLTHROUGH;

    case 1:
    case 0:
      retval(0) = idx_vector (idx, iext);
      break;
    }

  return retval;
}

template <typename T>
octave_value_list
find_nonzero_elem_idx (const Sparse<T>& v, int nargout,
                       octave_idx_type n_to_find, int direction)
{
  nargout = std::min (nargout, 5);
  octave_value_list retval ((nargout == 0 ? 1 : nargout), Matrix ());

  octave_idx_type nr = v.rows ();
  octave_idx_type nc = v.cols ();
  octave_idx_type nz = v.nnz ();

  // Search in the default range.
  octave_idx_type start_nc = -1;
  octave_idx_type end_nc = -1;
  octave_idx_type count;

  // Search for the range to search
  if (n_to_find < 0)
    {
      start_nc = 0;
      end_nc = nc;
      n_to_find = nz;
    }
  else if (direction > 0)
    {
      for (octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          if (v.cidx (j) == 0 && v.cidx (j+1) != 0)
            start_nc = j;
          if (v.cidx (j+1) >= n_to_find)
            {
              end_nc = j + 1;
              break;
            }
        }
    }
  else
    {
      for (octave_idx_type j = nc; j > 0; j--)
        {
          octave_quit ();

          if (v.cidx (j) == nz && v.cidx (j-1) != nz)
            end_nc = j;
          if (nz - v.cidx (j-1) >= n_to_find)
            {
              start_nc = j - 1;
              break;
            }
        }
    }

  count = (n_to_find > v.cidx (end_nc) - v.cidx (start_nc) ?
           v.cidx (end_nc) - v.cidx (start_nc) : n_to_find);

  octave_idx_type result_nr;
  octave_idx_type result_nc;

  // Default case is to return a column vector, however, if the original
  // argument was a row vector, then force return of a row vector.
  if (nr == 1)
    {
      result_nr = 1;
      result_nc = count;
    }
  else
    {
      result_nr = count;
      result_nc = 1;
    }

  Matrix idx (result_nr, result_nc);

  Matrix i_idx (result_nr, result_nc);
  Matrix j_idx (result_nr, result_nc);

  Array<T> val (dim_vector (result_nr, result_nc));

  if (count > 0)
    {
      // Search for elements to return.  Only search the region where there
      // are elements to be found using the count that we want to find.
      for (octave_idx_type j = start_nc, cx = 0; j < end_nc; j++)
        for (octave_idx_type i = v.cidx (j); i < v.cidx (j+1); i++)
          {
            octave_quit ();

            if (direction < 0 && i < nz - count)
              continue;
            i_idx(cx) = static_cast<double> (v.ridx (i) + 1);
            j_idx(cx) = static_cast<double> (j + 1);
            idx(cx) = j * nr + v.ridx (i) + 1;
            val(cx) = v.data(i);
            cx++;
            if (cx == count)
              break;
          }
    }
  else
    {
      // No items found.  Fixup return dimensions for Matlab compatibility.
      // The behavior to match is documented in Array.cc (Array<T>::find).
      if ((nr == 0 && nc == 0) || (nr == 1 && nc == 1))
        {
          idx.resize (0, 0);

          i_idx.resize (0, 0);
          j_idx.resize (0, 0);

          val.resize (dim_vector (0, 0));
        }
    }

  switch (nargout)
    {
    case 0:
    case 1:
      retval(0) = idx;
      break;

    case 5:
      retval(4) = nc;
      OCTAVE_FALLTHROUGH;

    case 4:
      retval(3) = nr;
      OCTAVE_FALLTHROUGH;

    case 3:
      retval(2) = val;
      OCTAVE_FALLTHROUGH;

    case 2:
      retval(1) = j_idx;
      retval(0) = i_idx;
    }

  return retval;
}

octave_value_list
find_nonzero_elem_idx (const PermMatrix& v, int nargout,
                       octave_idx_type n_to_find, int direction)
{
  // There are far fewer special cases to handle for a PermMatrix.
  nargout = std::min (nargout, 5);
  octave_value_list retval ((nargout == 0 ? 1 : nargout), Matrix ());

  octave_idx_type nr = v.rows ();
  octave_idx_type nc = v.cols ();
  octave_idx_type start_nc, count;

  // Determine the range to search.
  if (n_to_find < 0 || n_to_find >= nc)
    {
      start_nc = 0;
      count = nc;
    }
  else if (direction > 0)
    {
      start_nc = 0;
      count = n_to_find;
    }
  else
    {
      start_nc = nc - n_to_find;
      count = n_to_find;
    }

  Matrix idx (count, 1);
  Matrix i_idx (count, 1);
  Matrix j_idx (count, 1);
  // Every value is 1.
  Array<double> val (dim_vector (count, 1), 1.0);

  if (count > 0)
    {
      const Array<octave_idx_type>& p = v.col_perm_vec ();
      for (octave_idx_type k = 0; k < count; k++)
        {
          octave_quit ();

          const octave_idx_type j = start_nc + k;
          const octave_idx_type i = p(j);
          i_idx(k) = static_cast<double> (1+i);
          j_idx(k) = static_cast<double> (1+j);
          idx(k) = j * nc + i + 1;
        }
    }
  else
    {
      // FIXME: Is this case even possible?  A scalar permutation matrix seems
      // to devolve to a scalar full matrix, at least from the Octave command
      // line.  Perhaps this function could be called internally from C++ with
      // such a matrix.
      // No items found.  Fixup return dimensions for Matlab compatibility.
      // The behavior to match is documented in Array.cc (Array<T>::find).
      if ((nr == 0 && nc == 0) || (nr == 1 && nc == 1))
        {
          idx.resize (0, 0);

          i_idx.resize (0, 0);
          j_idx.resize (0, 0);

          val.resize (dim_vector (0, 0));
        }
    }

  switch (nargout)
    {
    case 0:
    case 1:
      retval(0) = idx;
      break;

    case 5:
      retval(4) = nc;
      OCTAVE_FALLTHROUGH;

    case 4:
      retval(3) = nc;
      OCTAVE_FALLTHROUGH;

    case 3:
      retval(2) = val;
      OCTAVE_FALLTHROUGH;

    case 2:
      retval(1) = j_idx;
      retval(0) = i_idx;
    }

  return retval;
}

DEFUN (find, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{idx} =} find (@var{x})
@deftypefnx {} {@var{idx} =} find (@var{x}, @var{n})
@deftypefnx {} {@var{idx} =} find (@var{x}, @var{n}, @var{direction})
@deftypefnx {} {[i, j] =} find (@dots{})
@deftypefnx {} {[i, j, v] =} find (@dots{})
Return a vector of indices of nonzero elements of a matrix, as a row if
@var{x} is a row vector or as a column otherwise.

To obtain a single index for each matrix element, Octave pretends that the
columns of a matrix form one long vector (like Fortran arrays are stored).
For example:

@example
@group
find (eye (2))
  @result{} [ 1; 4 ]
@end group
@end example

If two inputs are given, @var{n} indicates the maximum number of elements to
find from the beginning of the matrix or vector.

If three inputs are given, @var{direction} should be one of
@qcode{"first"} or @qcode{"last"}, requesting only the first or last
@var{n} indices, respectively.  However, the indices are always returned in
ascending order.

If two outputs are requested, @code{find} returns the row and column
indices of nonzero elements of a matrix.  For example:

@example
@group
[i, j] = find (2 * eye (2))
    @result{} i = [ 1; 2 ]
    @result{} j = [ 1; 2 ]
@end group
@end example

If three outputs are requested, @code{find} also returns a vector
containing the nonzero values.  For example:

@example
@group
[i, j, v] = find (3 * eye (2))
       @result{} i = [ 1; 2 ]
       @result{} j = [ 1; 2 ]
       @result{} v = [ 3; 3 ]
@end group
@end example

If @var{x} is a multi-dimensional array of size m x n x p x @dots{}, @var{j}
contains the column locations as if @var{x} was flattened into a
two-dimensional matrix of size m x (n + p + @dots{}).

Note that this function is particularly useful for sparse matrices, as
it extracts the nonzero elements as vectors, which can then be used to
create the original matrix.  For example:

@example
@group
sz = size (a);
[i, j, v] = find (a);
b = sparse (i, j, v, sz(1), sz(2));
@end group
@end example
@seealso{nonzeros}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    print_usage ();

  // Setup the default options.
  octave_idx_type n_to_find = -1;
  if (nargin > 1)
    {
      double val = args(1).xscalar_value ("find: N must be an integer");

      if (val < 0 || (! math::isinf (val)
                      && val != math::fix (val)))
        error ("find: N must be a non-negative integer");
      else if (! math::isinf (val))
        n_to_find = val;
    }

  // Direction to do the searching (1 == forward, -1 == reverse).
  int direction = 1;
  if (nargin > 2)
    {
      std::string s_arg = args(2).string_value ();

      if (s_arg == "first")
        direction = 1;
      else if (s_arg == "last")
        direction = -1;
      else
        error (R"(find: DIRECTION must be "first" or "last")");
    }

  octave_value_list retval;

  octave_value arg = args(0);

  if (arg.islogical ())
    {
      if (arg.issparse ())
        {
          SparseBoolMatrix v = arg.sparse_bool_matrix_value ();

          retval = find_nonzero_elem_idx (v, nargout, n_to_find, direction);
        }
      else if (nargout <= 1 && n_to_find == -1 && direction == 1)
        {
          // This case is equivalent to extracting indices from a logical
          // matrix.  Try to reuse the possibly cached index vector.

          // No need to catch index_exception, since arg is bool.
          // Out-of-range errors have already set pos, and will be
          // caught later.

          octave_value result = arg.index_vector ().unmask ();

          dim_vector dv = result.dims ();

          retval(0) = (dv.all_zero () || dv.isvector ()
                       ? result : result.reshape (dv.as_column ()));
        }
      else
        {
          boolNDArray v = arg.bool_array_value ();

          retval = find_nonzero_elem_idx (v, nargout, n_to_find, direction);
        }
    }
  else if (arg.isinteger ())
    {
#define DO_INT_BRANCH(INTT)                                             \
      else if (arg.is_ ## INTT ## _type ())                             \
        {                                                               \
          INTT ## NDArray v = arg.INTT ## _array_value ();              \
                                                                        \
          retval = find_nonzero_elem_idx (v, nargout, n_to_find, direction); \
        }

      if (false)
        ;
      DO_INT_BRANCH (int8)
      DO_INT_BRANCH (int16)
      DO_INT_BRANCH (int32)
      DO_INT_BRANCH (int64)
      DO_INT_BRANCH (uint8)
      DO_INT_BRANCH (uint16)
      DO_INT_BRANCH (uint32)
      DO_INT_BRANCH (uint64)
      else
        panic_impossible ();
    }
  else if (arg.issparse ())
    {
      if (arg.isreal ())
        {
          SparseMatrix v = arg.sparse_matrix_value ();

          retval = find_nonzero_elem_idx (v, nargout, n_to_find, direction);
        }
      else if (arg.iscomplex ())
        {
          SparseComplexMatrix v = arg.sparse_complex_matrix_value ();

          retval = find_nonzero_elem_idx (v, nargout, n_to_find, direction);
        }
      else
        err_wrong_type_arg ("find", arg);
    }
  else if (arg.is_perm_matrix ())
    {
      PermMatrix P = arg.perm_matrix_value ();

      retval = find_nonzero_elem_idx (P, nargout, n_to_find, direction);
    }
  else if (arg.is_string ())
    {
      charNDArray chnda = arg.char_array_value ();

      retval = find_nonzero_elem_idx (chnda, nargout, n_to_find, direction);
    }
  else if (arg.is_single_type ())
    {
      if (arg.isreal ())
        {
          FloatNDArray nda = arg.float_array_value ();

          retval = find_nonzero_elem_idx (nda, nargout, n_to_find, direction);
        }
      else if (arg.iscomplex ())
        {
          FloatComplexNDArray cnda = arg.float_complex_array_value ();

          retval = find_nonzero_elem_idx (cnda, nargout, n_to_find, direction);
        }
    }
  else if (arg.isreal ())
    {
      NDArray nda = arg.array_value ();

      retval = find_nonzero_elem_idx (nda, nargout, n_to_find, direction);
    }
  else if (arg.iscomplex ())
    {
      ComplexNDArray cnda = arg.complex_array_value ();

      retval = find_nonzero_elem_idx (cnda, nargout, n_to_find, direction);
    }
  else
    err_wrong_type_arg ("find", arg);

  return retval;
}

/*
%!assert (find (char ([0, 97])), 2)
%!assert (find ([1, 0, 1, 0, 1]), [1, 3, 5])
%!assert (find ([1; 0; 3; 0; 1]), [1; 3; 5])
%!assert (find ([0, 0, 2; 0, 3, 0; -1, 0, 0]), [3; 5; 7])

%!assert <*53603> (find (ones (1,1,2) > 0), [1;2])
%!assert <*53603> (find (ones (1,1,1,3) > 0), [1;2;3])

%!test
%! [i, j, v] = find ([0, 0, 2; 0, 3, 0; -1, 0, 0]);
%!
%! assert (i, [3; 2; 1]);
%! assert (j, [1; 2; 3]);
%! assert (v, [-1; 3; 2]);

%!assert (find (single ([1, 0, 1, 0, 1])), [1, 3, 5])
%!assert (find (single ([1; 0; 3; 0; 1])), [1; 3; 5])
%!assert (find (single ([0, 0, 2; 0, 3, 0; -1, 0, 0])), [3; 5; 7])

%!test
%! [i, j, v] = find (single ([0, 0, 2; 0, 3, 0; -1, 0, 0]));
%!
%! assert (i, [3; 2; 1]);
%! assert (j, [1; 2; 3]);
%! assert (v, single ([-1; 3; 2]));

%!test
%! pcol = [5 1 4 3 2];
%! P = eye (5) (:, pcol);
%! [i, j, v] = find (P);
%! [ifull, jfull, vfull] = find (full (P));
%! assert (i, ifull);
%! assert (j, jfull);
%! assert (all (v == 1));

%!test
%! prow = [5 1 4 3 2];
%! P = eye (5) (prow, :);
%! [i, j, v] = find (P);
%! [ifull, jfull, vfull] = find (full (P));
%! assert (i, ifull);
%! assert (j, jfull);
%! assert (all (v == 1));

%!test <*61986>
%! P = cat (3, eye(3), eye(3));
%! loc = find (P);
%! [i, j, v] = find(P);
%! assert (loc, [1, 5, 9, 10, 14, 18]');
%! assert (i, [1, 2, 3, 1, 2, 3]');
%! assert (j, [1, 2, 3, 4, 5, 6]');
%! assert (v, [1, 1, 1, 1, 1, 1]');

%!assert <*53655> (find (false), zeros (0, 0))
%!assert <*53655> (find ([false, false]), zeros (1, 0))
%!assert <*53655> (find ([false; false]), zeros (0, 1))
%!assert <*53655> (find ([false, false; false, false]), zeros (0, 1))

%!assert (find ([2 0 1 0 5 0], 1), 1)
%!assert (find ([2 0 1 0 5 0], 2, "last"), [3, 5])

%!assert (find ([2 0 1 0 5 0], Inf), [1, 3, 5])
%!assert (find ([2 0 1 0 5 0], Inf, "last"), [1, 3, 5])

%!error find ()
*/

OCTAVE_END_NAMESPACE(octave)
