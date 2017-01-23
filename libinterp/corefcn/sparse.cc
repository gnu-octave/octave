/*

Copyright (C) 2004-2016 David Bateman
Copyright (C) 1998-2004 Andy Adler
Copyright (C) 2010 VZLU Prague

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstdlib>
#include <string>

#include "variables.h"
#include "utils.h"
#include "pager.h"
#include "defun.h"
#include "errwarn.h"
#include "quit.h"
#include "unwind-prot.h"

#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"
#include "ov-bool-sparse.h"

DEFUN (issparse, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} issparse (@var{x})
Return true if @var{x} is a sparse matrix.
@seealso{ismatrix}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  return ovl (args(0).is_sparse_type ());
}

DEFUN (sparse, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{s} =} sparse (@var{a})
@deftypefnx {} {@var{s} =} sparse (@var{i}, @var{j}, @var{sv}, @var{m}, @var{n})
@deftypefnx {} {@var{s} =} sparse (@var{i}, @var{j}, @var{sv})
@deftypefnx {} {@var{s} =} sparse (@var{m}, @var{n})
@deftypefnx {} {@var{s} =} sparse (@var{i}, @var{j}, @var{s}, @var{m}, @var{n}, "unique")
@deftypefnx {} {@var{s} =} sparse (@var{i}, @var{j}, @var{sv}, @var{m}, @var{n}, @var{nzmax})
Create a sparse matrix from a full matrix, or row, column, value triplets.

If @var{a} is a full matrix, convert it to a sparse matrix representation,
removing all zero values in the process.

Given the integer index vectors @var{i} and @var{j}, and a 1-by-@code{nnz}
vector of real or complex values @var{sv}, construct the sparse matrix
@code{S(@var{i}(@var{k}),@var{j}(@var{k})) = @var{sv}(@var{k})} with overall
dimensions @var{m} and @var{n}.  If any of @var{sv}, @var{i} or @var{j} are
scalars, they are expanded to have a common size.

If @var{m} or @var{n} are not specified their values are derived from the
maximum index in the vectors @var{i} and @var{j} as given by
@code{@var{m} = max (@var{i})}, @code{@var{n} = max (@var{j})}.

@strong{Note}: if multiple values are specified with the same @var{i},
@var{j} indices, the corresponding value in @var{s} will be the sum of the
values at the repeated location.  See @code{accumarray} for an example of
how to produce different behavior, such as taking the minimum instead.

If the option @qcode{"unique"} is given, and more than one value is
specified at the same @var{i}, @var{j} indices, then the last specified
value will be used.

@code{sparse (@var{m}, @var{n})} will create an empty @var{m}x@var{n} sparse
matrix and is equivalent to @code{sparse ([], [], [], @var{m}, @var{n})}

The argument @code{nzmax} is ignored but accepted for compatibility with
@sc{matlab}.

Example 1 (sum at repeated indices):

@example
@group
@var{i} = [1 1 2]; @var{j} = [1 1 2]; @var{sv} = [3 4 5];
sparse (@var{i}, @var{j}, @var{sv}, 3, 4)
@result{}
Compressed Column Sparse (rows = 3, cols = 4, nnz = 2 [17%])

  (1, 1) ->  7
  (2, 2) ->  5
@end group
@end example

Example 2 ("unique" option):

@example
@group
@var{i} = [1 1 2]; @var{j} = [1 1 2]; @var{sv} = [3 4 5];
sparse (@var{i}, @var{j}, @var{sv}, 3, 4, "unique")
@result{}
Compressed Column Sparse (rows = 3, cols = 4, nnz = 2 [17%])

  (1, 1) ->  4
  (2, 2) ->  5
@end group
@end example
@seealso{full, accumarray, spalloc, spdiags, speye, spones, sprand, sprandn, sprandsym, spconvert, spfun}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin == 0 || nargin > 6)
    print_usage ();

  octave_value retval;

  // Temporarily disable sparse_auto_mutate if set (it's obsolete anyway).
  octave::unwind_protect frame;
  frame.protect_var (Vsparse_auto_mutate);
  Vsparse_auto_mutate = false;

  if (nargin == 1)
    {
      octave_value arg = args(0);
      if (arg.is_bool_type ())
        retval = arg.sparse_bool_matrix_value ();
      else if (arg.is_complex_type ())
        retval = arg.sparse_complex_matrix_value ();
      else if (arg.is_numeric_type ())
        retval = arg.sparse_matrix_value ();
      else
        err_wrong_type_arg ("sparse", arg);
    }
  else if (nargin == 2)
    {
      octave_idx_type m = 0;
      octave_idx_type n = 0;

      get_dimensions (args(0), args(1), "sparse", m, n);

      if (m >= 0 && n >= 0)
        retval = SparseMatrix (m, n);
      else
        error ("sparse: dimensions must be non-negative");
    }
  else if (nargin >= 3)
    {
      bool summation = true;
      if (nargin > 3 && args(nargin-1).is_string ())
        {
          std::string opt = args(nargin-1).string_value ();
          if (opt == "unique")
            summation = false;
          else if (opt == "sum" || opt == "summation")
            summation = true;
          else
            error ("sparse: invalid option: %s", opt.c_str ());

          nargin -= 1;
        }

      octave_idx_type m, n, nzmax;
      m = n = nzmax = -1;
      if (nargin == 6)
        {
          nzmax = args(5).idx_type_value ();
          nargin--;
        }

      if (nargin == 5)
        {
          get_dimensions (args(3), args(4), "sparse", m, n);

          if (m < 0 || n < 0)
            error ("sparse: dimensions must be non-negative");
        }

      int k = 0;    // index we're checking when index_vector throws
      try
        {
          idx_vector i = args(0).index_vector ();
          k = 1;
          idx_vector j = args(1).index_vector ();

          if (args(2).is_bool_type ())
            retval = SparseBoolMatrix (args(2).bool_array_value (), i,j,
                                       m, n, summation, nzmax);
          else if (args(2).is_complex_type ())
            retval = SparseComplexMatrix (args(2).complex_array_value(),
                                          i, j, m, n, summation, nzmax);
          else if (args(2).is_numeric_type ())
            retval = SparseMatrix (args(2).array_value (), i, j,
                                   m, n, summation, nzmax);
          else
            err_wrong_type_arg ("sparse", args(2));
        }
      catch (octave::index_exception& e)
        {
          // Rethrow to allow more info to be reported later.
          e.set_pos_if_unset (2, k+1);
          throw;
        }
    }

  return retval;
}

DEFUN (spalloc, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{s} =} spalloc (@var{m}, @var{n}, @var{nz})
Create an @var{m}-by-@var{n} sparse matrix with pre-allocated space for at
most @var{nz} nonzero elements.

This is useful for building a matrix incrementally by a sequence of indexed
assignments.  Subsequent indexed assignments after @code{spalloc} will reuse
the pre-allocated memory, provided they are of one of the simple forms

@itemize
@item @code{@var{s}(I:J) = @var{x}}

@item @code{@var{s}(:,I:J) = @var{x}}

@item @code{@var{s}(K:L,I:J) = @var{x}}
@end itemize

@b{and} that the following conditions are met:

@itemize
@item the assignment does not decrease nnz (@var{S}).

@item after the assignment, nnz (@var{S}) does not exceed @var{nz}.

@item no index is out of bounds.
@end itemize

Partial movement of data may still occur, but in general the assignment will
be more memory and time efficient under these circumstances.  In particular,
it is possible to efficiently build a pre-allocated sparse matrix from a
contiguous block of columns.

The amount of pre-allocated memory for a given matrix may be queried using
the function @code{nzmax}.
@seealso{nzmax, sparse}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    print_usage ();

  octave_idx_type m = args(0).idx_type_value ();
  octave_idx_type n = args(1).idx_type_value ();

  octave_idx_type nz = 0;
  if (nargin == 3)
    nz = args(2).idx_type_value ();

  if (m >= 0 && n >= 0 && nz >= 0)
    return ovl (SparseMatrix (dim_vector (m, n), nz));
  else
    error ("spalloc: M,N,NZ must be non-negative");
}
