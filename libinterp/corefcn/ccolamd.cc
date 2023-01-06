////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2005-2023 The Octave Project Developers
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

// This is the octave interface to ccolamd, which bore the copyright given
// in the help of the functions.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstdlib>

#include "CSparse.h"
#include "Sparse.h"
#include "dNDArray.h"
#include "oct-locbuf.h"
#include "oct-sparse.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ov.h"
#include "pager.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (ccolamd, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{p} =} ccolamd (@var{S})
@deftypefnx {} {@var{p} =} ccolamd (@var{S}, @var{knobs})
@deftypefnx {} {@var{p} =} ccolamd (@var{S}, @var{knobs}, @var{cmember})
@deftypefnx {} {[@var{p}, @var{stats}] =} ccolamd (@dots{})

Constrained column approximate minimum degree permutation.

@code{@var{p} = ccolamd (@var{S})} returns the column approximate minimum
degree permutation vector for the sparse matrix @var{S}.  For a
non-symmetric matrix @var{S}, @code{@var{S}(:, @var{p})} tends to have
sparser LU@tie{}factors than @var{S}.
@code{chol (@var{S}(:, @var{p})' * @var{S}(:, @var{p}))} also tends to be
sparser than @code{chol (@var{S}' * @var{S})}.
@code{@var{p} = ccolamd (@var{S}, 1)} optimizes the ordering for
@code{lu (@var{S}(:, @var{p}))}.  The ordering is followed by a column
elimination tree post-ordering.

@var{knobs} is an optional 1-element to 5-element input vector, with a
default value of @code{[0 10 10 1 0]} if not present or empty.  Entries not
present are set to their defaults.

@table @code
@item @var{knobs}(1)
if nonzero, the ordering is optimized for @code{lu (S(:, p))}.  It will be a
poor ordering for @code{chol (@var{S}(:, @var{p})' * @var{S}(:, @var{p}))}.
This is the most important knob for ccolamd.

@item @var{knobs}(2)
if @var{S} is m-by-n, rows with more than
@code{max (16, @var{knobs}(2) * sqrt (n))} entries are ignored.

@item @var{knobs}(3)
columns with more than
@code{max (16, @var{knobs}(3) * sqrt (min (@var{m}, @var{n})))} entries are
ignored and ordered last in the output permutation
(subject to the cmember constraints).

@item @var{knobs}(4)
if nonzero, aggressive absorption is performed.

@item @var{knobs}(5)
if nonzero, statistics and knobs are printed.

@end table

@var{cmember} is an optional vector of length @math{n}.  It defines the
constraints on the column ordering.  If @code{@var{cmember}(j) = @var{c}},
then column @var{j} is in constraint set @var{c} (@var{c} must be in the
range 1 to n).  In the output permutation @var{p}, all columns in set 1
appear first, followed by all columns in set 2, and so on.
@code{@var{cmember} = ones (1,n)} if not present or empty.
@code{ccolamd (@var{S}, [], 1 : n)} returns @code{1 : n}

@code{@var{p} = ccolamd (@var{S})} is about the same as
@code{@var{p} = colamd (@var{S})}.  @var{knobs} and its default values
differ.  @code{colamd} always does aggressive absorption, and it finds an
ordering suitable for both @code{lu (@var{S}(:, @var{p}))} and @code{chol
(@var{S}(:, @var{p})' * @var{S}(:, @var{p}))}; it cannot optimize its
ordering for @code{lu (@var{S}(:, @var{p}))} to the extent that
@code{ccolamd (@var{S}, 1)} can.

@var{stats} is an optional 20-element output vector that provides data
about the ordering and the validity of the input matrix @var{S}.  Ordering
statistics are in @code{@var{stats}(1 : 3)}.  @code{@var{stats}(1)} and
@code{@var{stats}(2)} are the number of dense or empty rows and columns
ignored by @sc{ccolamd} and @code{@var{stats}(3)} is the number of garbage
collections performed on the internal data structure used by @sc{ccolamd}
(roughly of size @code{2.2 * nnz (@var{S}) + 4 * @var{m} + 7 * @var{n}}
integers).

@code{@var{stats}(4 : 7)} provide information if CCOLAMD was able to
continue.  The matrix is OK if @code{@var{stats}(4)} is zero, or 1 if
invalid.  @code{@var{stats}(5)} is the rightmost column index that is
unsorted or contains duplicate entries, or zero if no such column exists.
@code{@var{stats}(6)} is the last seen duplicate or out-of-order row
index in the column index given by @code{@var{stats}(5)}, or zero if no
such row index exists.  @code{@var{stats}(7)} is the number of duplicate
or out-of-order row indices.  @code{@var{stats}(8 : 20)} is always zero in
the current version of @sc{ccolamd} (reserved for future use).

The authors of the code itself are @nospell{S. Larimore, T. Davis} and
@nospell{S. Rajamanickam} in collaboration with @nospell{J. Bilbert and E. Ng}.
Supported by the National Science Foundation
@nospell{(DMS-9504974, DMS-9803599, CCR-0203270)}, and a grant from
@nospell{Sandia} National Lab.
See @url{http://faculty.cse.tamu.edu/davis/suitesparse.html} for ccolamd,
csymamd, amd, colamd, symamd, and other related orderings.
@seealso{colamd, csymamd}
@end deftypefn */)
{
#if defined (HAVE_CCOLAMD)

  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    print_usage ();

  octave_value_list retval (nargout == 2 ? 2 : 1);
  int spumoni = 0;

  // Get knobs
  static_assert (CCOLAMD_KNOBS <= 40,
                 "ccolamd: # of CCOLAMD_KNOBS exceeded.  Please report this to bugs.octave.org");
  double knob_storage[CCOLAMD_KNOBS];
  double *knobs = &knob_storage[0];
  CCOLAMD_NAME (_set_defaults) (knobs);

  // Check for user-passed knobs
  if (nargin > 1)
    {
      NDArray User_knobs = args(1).array_value ();
      int nel_User_knobs = User_knobs.numel ();

      if (nel_User_knobs > 0)
        knobs[CCOLAMD_LU] = (User_knobs(0) != 0);
      if (nel_User_knobs > 1)
        knobs[CCOLAMD_DENSE_ROW] = User_knobs(1);
      if (nel_User_knobs > 2)
        knobs[CCOLAMD_DENSE_COL] = User_knobs(2);
      if (nel_User_knobs > 3)
        knobs[CCOLAMD_AGGRESSIVE] = (User_knobs(3) != 0);
      if (nel_User_knobs > 4)
        spumoni = (User_knobs(4) != 0);

      // print knob settings if spumoni is set
      if (spumoni)
        {
          octave_stdout << "\nccolamd version " << CCOLAMD_MAIN_VERSION << '.'
                        <<  CCOLAMD_SUB_VERSION << ", " << CCOLAMD_DATE
                        << ":\nknobs(1): " << User_knobs(0) << ", order for ";
          if (knobs[CCOLAMD_LU] != 0)
            octave_stdout << "lu (A)\n";
          else
            octave_stdout << "chol (A'*A)\n";

          if (knobs[CCOLAMD_DENSE_ROW] >= 0)
            octave_stdout << "knobs(2): " << User_knobs(1)
                          << ", rows with > max (16,"
                          << knobs[CCOLAMD_DENSE_ROW]
                          << "* sqrt (columns(A)))"
                          << " entries removed\n";
          else
            octave_stdout << "knobs(2): " << User_knobs(1)
                          << ", no dense rows removed\n";

          if (knobs[CCOLAMD_DENSE_COL] >= 0)
            octave_stdout << "knobs(3): " << User_knobs(2)
                          << ", cols with > max (16,"
                          << knobs[CCOLAMD_DENSE_COL] << "* sqrt (size(A)))"
                          << " entries removed\n";
          else
            octave_stdout << "knobs(3): " << User_knobs(2)
                          << ", no dense columns removed\n";

          if (knobs[CCOLAMD_AGGRESSIVE] != 0)
            octave_stdout << "knobs(4): " << User_knobs(3)
                          << ", aggressive absorption: yes";
          else
            octave_stdout << "knobs(4): " << User_knobs(3)
                          << ", aggressive absorption: no";

          octave_stdout << "knobs(5): " << User_knobs(4)
                        << ", statistics and knobs printed\n";
        }
    }

  octave_idx_type n_row, n_col, nnz;
  octave_idx_type *ridx, *cidx;
  SparseComplexMatrix scm;
  SparseMatrix sm;

  if (args(0).issparse ())
    {
      if (args(0).iscomplex ())
        {
          scm = args(0).sparse_complex_matrix_value ();
          n_row = scm.rows ();
          n_col = scm.cols ();
          nnz = scm.nnz ();
          ridx = scm.xridx ();
          cidx = scm.xcidx ();
        }
      else
        {
          sm = args(0).sparse_matrix_value ();

          n_row = sm.rows ();
          n_col = sm.cols ();
          nnz = sm.nnz ();
          ridx = sm.xridx ();
          cidx = sm.xcidx ();
        }
    }
  else
    {
      if (args(0).iscomplex ())
        sm = SparseMatrix (real (args(0).complex_matrix_value ()));
      else
        sm = SparseMatrix (args(0).matrix_value ());

      n_row = sm.rows ();
      n_col = sm.cols ();
      nnz = sm.nnz ();
      ridx = sm.xridx ();
      cidx = sm.xcidx ();
    }

  // Allocate workspace for ccolamd
  OCTAVE_LOCAL_BUFFER (suitesparse_integer, p, n_col+1);
  for (octave_idx_type i = 0; i < n_col+1; i++)
    p[i] = cidx[i];

  octave_idx_type Alen = CCOLAMD_NAME (_recommended) (nnz, n_row, n_col);
  OCTAVE_LOCAL_BUFFER (suitesparse_integer, A, Alen);
  for (octave_idx_type i = 0; i < nnz; i++)
    A[i] = ridx[i];

  static_assert (CCOLAMD_STATS <= 40,
                 "ccolamd: # of CCOLAMD_STATS exceeded.  Please report this to bugs.octave.org");
  suitesparse_integer stats_storage[CCOLAMD_STATS];
  suitesparse_integer *stats = &stats_storage[0];

  if (nargin > 2)
    {
      NDArray in_cmember = args(2).array_value ();
      octave_idx_type cslen = in_cmember.numel ();
      OCTAVE_LOCAL_BUFFER (suitesparse_integer, cmember, cslen);
      for (octave_idx_type i = 0; i < cslen; i++)
        // convert cmember from 1-based to 0-based
        cmember[i] = static_cast<suitesparse_integer>(in_cmember(i) - 1);

      if (cslen != n_col)
        error ("ccolamd: CMEMBER must be of length equal to #cols of A");

      // Order the columns (destroys A)
      if (! CCOLAMD_NAME () (n_row, n_col, Alen, A, p, knobs, stats, cmember))
        {
          CCOLAMD_NAME (_report) (stats);

          error ("ccolamd: internal error!");
        }
    }
  else
    {
      // Order the columns (destroys A)
      if (! CCOLAMD_NAME () (n_row, n_col, Alen, A, p, knobs, stats, nullptr))
        {
          CCOLAMD_NAME (_report) (stats);

          error ("ccolamd: internal error!");
        }
    }

  // return the permutation vector
  NDArray out_perm (dim_vector (1, n_col));
  for (octave_idx_type i = 0; i < n_col; i++)
    out_perm(i) = p[i] + 1;

  retval(0) = out_perm;

  // print stats if spumoni > 0
  if (spumoni > 0)
    CCOLAMD_NAME (_report) (stats);

  // Return the stats vector
  if (nargout == 2)
    {
      NDArray out_stats (dim_vector (1, CCOLAMD_STATS));
      for (octave_idx_type i = 0 ; i < CCOLAMD_STATS ; i++)
        out_stats(i) = stats[i];
      retval(1) = out_stats;

      // fix stats (5) and (6), for 1-based information on
      // jumbled matrix.  note that this correction doesn't
      // occur if symamd returns FALSE
      out_stats(CCOLAMD_INFO1)++;
      out_stats(CCOLAMD_INFO2)++;
    }

  return retval;

#else

  octave_unused_parameter (args);
  octave_unused_parameter (nargout);

  err_disabled_feature ("ccolamd", "CCOLAMD");

#endif
}

DEFUN (csymamd, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{p} =} csymamd (@var{S})
@deftypefnx {} {@var{p} =} csymamd (@var{S}, @var{knobs})
@deftypefnx {} {@var{p} =} csymamd (@var{S}, @var{knobs}, @var{cmember})
@deftypefnx {} {[@var{p}, @var{stats}] =} csymamd (@dots{})

For a symmetric positive definite matrix @var{S}, return the permutation
vector @var{p} such that @code{@var{S}(@var{p},@var{p})} tends to have a
sparser Cholesky@tie{}factor than @var{S}.

Sometimes @code{csymamd} works well for symmetric indefinite matrices too.
The matrix @var{S} is assumed to be symmetric; only the strictly lower
triangular part is referenced.  @var{S} must be square.  The ordering is
followed by an elimination tree post-ordering.

@var{knobs} is an optional 1-element to 3-element input vector, with a
default value of @code{[10 1 0]}.  Entries not present are set to their
defaults.

@table @code
@item @var{knobs}(1)
If @var{S} is n-by-n, then rows and columns with more than
@code{max(16,@var{knobs}(1)*sqrt(n))} entries are ignored, and ordered
last in the output permutation (subject to the cmember constraints).

@item @var{knobs}(2)
If nonzero, aggressive absorption is performed.

@item @var{knobs}(3)
If nonzero, statistics and knobs are printed.

@end table

@var{cmember} is an optional vector of length n.  It defines the constraints
on the ordering.  If @code{@var{cmember}(j) = @var{S}}, then row/column j is
in constraint set @var{c} (@var{c} must be in the range 1 to n).  In the
output permutation @var{p}, rows/columns in set 1 appear first, followed
by all rows/columns in set 2, and so on.  @code{@var{cmember} = ones (1,n)}
if not present or empty.  @code{csymamd (@var{S},[],1:n)} returns
@code{1:n}.

@code{@var{p} = csymamd (@var{S})} is about the same as
@code{@var{p} = symamd (@var{S})}.  @var{knobs} and its default values
differ.

@code{@var{stats}(4:7)} provide information if CCOLAMD was able to
continue.  The matrix is OK if @code{@var{stats}(4)} is zero, or 1 if
invalid.  @code{@var{stats}(5)} is the rightmost column index that is
unsorted or contains duplicate entries, or zero if no such column exists.
@code{@var{stats}(6)} is the last seen duplicate or out-of-order row
index in the column index given by @code{@var{stats}(5)}, or zero if no
such row index exists.  @code{@var{stats}(7)} is the number of duplicate
or out-of-order row indices.  @code{@var{stats}(8:20)} is always zero in
the current version of @sc{ccolamd} (reserved for future use).

The authors of the code itself are @nospell{S. Larimore, T. Davis} and
@nospell{S. Rajamanickam} in collaboration with @nospell{J. Bilbert and E. Ng}.
Supported by the National Science Foundation
@nospell{(DMS-9504974, DMS-9803599, CCR-0203270)}, and a grant from
@nospell{Sandia} National Lab.
See @url{http://faculty.cse.tamu.edu/davis/suitesparse.html} for ccolamd,
colamd, csymamd, amd, colamd, symamd, and other related orderings.
@seealso{symamd, ccolamd}
@end deftypefn */)
{
#if defined (HAVE_CCOLAMD)

  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    print_usage ();

  octave_value_list retval (nargout == 2 ? 2 : 1);
  int spumoni = 0;

  // Get knobs
  static_assert (CCOLAMD_KNOBS <= 40,
                 "csymamd: # of CCOLAMD_KNOBS exceeded.  Please report this to bugs.octave.org");
  double knob_storage[CCOLAMD_KNOBS];
  double *knobs = &knob_storage[0];
  CCOLAMD_NAME (_set_defaults) (knobs);

  // Check for user-passed knobs
  if (nargin > 1)
    {
      NDArray User_knobs = args(1).array_value ();
      int nel_User_knobs = User_knobs.numel ();

      if (nel_User_knobs > 0)
        knobs[CCOLAMD_DENSE_ROW] = User_knobs(0);
      if (nel_User_knobs > 1)
        knobs[CCOLAMD_AGGRESSIVE] = User_knobs(1);
      if (nel_User_knobs > 2)
        spumoni = static_cast<int> (User_knobs(2));

      // print knob settings if spumoni is set
      if (spumoni)
        {
          octave_stdout << "\ncsymamd version " << CCOLAMD_MAIN_VERSION
                        << '.' << CCOLAMD_SUB_VERSION
                        << ", " << CCOLAMD_DATE << "\n";

          if (knobs[CCOLAMD_DENSE_ROW] >= 0)
            octave_stdout << "knobs(1): " << User_knobs(0)
                          << ", rows/cols with > max (16,"
                          << knobs[CCOLAMD_DENSE_ROW]
                          << "* sqrt (columns(A)))"
                          << " entries removed\n";
          else
            octave_stdout << "knobs(1): " << User_knobs(0)
                          << ", no dense rows/cols removed\n";

          if (knobs[CCOLAMD_AGGRESSIVE] != 0)
            octave_stdout << "knobs(2): " << User_knobs(1)
                          << ", aggressive absorption: yes";
          else
            octave_stdout << "knobs(2): " << User_knobs(1)
                          << ", aggressive absorption: no";

          octave_stdout << "knobs(3): " << User_knobs(2)
                        << ", statistics and knobs printed\n";
        }
    }

  octave_idx_type n_row, n_col;
  octave_idx_type *ridx, *cidx;
  SparseMatrix sm;
  SparseComplexMatrix scm;

  if (args(0).issparse ())
    {
      if (args(0).iscomplex ())
        {
          scm = args(0).sparse_complex_matrix_value ();
          n_row = scm.rows ();
          n_col = scm.cols ();
          ridx = scm.xridx ();
          cidx = scm.xcidx ();
        }
      else
        {
          sm = args(0).sparse_matrix_value ();
          n_row = sm.rows ();
          n_col = sm.cols ();
          ridx = sm.xridx ();
          cidx = sm.xcidx ();
        }
    }
  else
    {
      if (args(0).iscomplex ())
        sm = SparseMatrix (real (args(0).complex_matrix_value ()));
      else
        sm = SparseMatrix (args(0).matrix_value ());

      n_row = sm.rows ();
      n_col = sm.cols ();
      ridx = sm.xridx ();
      cidx = sm.xcidx ();
    }

  if (n_row != n_col)
    err_square_matrix_required ("csymamd", "S");

  // Allocate workspace for symamd
  OCTAVE_LOCAL_BUFFER (suitesparse_integer, perm, n_col+1);
  static_assert (CCOLAMD_STATS <= 40,
                 "csymamd: # of CCOLAMD_STATS exceeded.  Please report this to bugs.octave.org");
  suitesparse_integer stats_storage[CCOLAMD_STATS];
  suitesparse_integer *stats = &stats_storage[0];

  if (nargin > 2)
    {
      NDArray in_cmember = args(2).array_value ();
      octave_idx_type cslen = in_cmember.numel ();
      OCTAVE_LOCAL_BUFFER (suitesparse_integer, cmember, cslen);
      for (octave_idx_type i = 0; i < cslen; i++)
        // convert cmember from 1-based to 0-based
        cmember[i] = static_cast<octave_idx_type> (in_cmember(i) - 1);

      if (cslen != n_col)
        error ("csymamd: CMEMBER must be of length equal to #cols of A");

      if (! CSYMAMD_NAME () (n_col,
                             to_suitesparse_intptr (ridx),
                             to_suitesparse_intptr (cidx),
                             perm, knobs, stats, &calloc, &free, cmember, -1))
        {
          CSYMAMD_NAME (_report)(stats);

          error ("csymamd: internal error!");
        }
    }
  else
    {
      if (! CSYMAMD_NAME () (n_col,
                             to_suitesparse_intptr (ridx),
                             to_suitesparse_intptr (cidx),
                             perm, knobs, stats, &calloc, &free, nullptr, -1))
        {
          CSYMAMD_NAME (_report)(stats);

          error ("csymamd: internal error!");
        }
    }

  // return the permutation vector
  NDArray out_perm (dim_vector (1, n_col));
  for (octave_idx_type i = 0; i < n_col; i++)
    out_perm(i) = perm[i] + 1;

  retval(0) = out_perm;

  // print stats if spumoni > 0
  if (spumoni > 0)
    CSYMAMD_NAME (_report)(stats);

  // Return the stats vector
  if (nargout == 2)
    {
      NDArray out_stats (dim_vector (1, CCOLAMD_STATS));
      for (octave_idx_type i = 0 ; i < CCOLAMD_STATS ; i++)
        out_stats(i) = stats[i];
      retval(1) = out_stats;

      // fix stats (5) and (6), for 1-based information on
      // jumbled matrix.  note that this correction doesn't
      // occur if symamd returns FALSE
      out_stats(CCOLAMD_INFO1)++;
      out_stats(CCOLAMD_INFO2)++;
    }

  return retval;

#else

  octave_unused_parameter (args);
  octave_unused_parameter (nargout);

  err_disabled_feature ("csymamd", "CCOLAMD");

#endif
}

OCTAVE_END_NAMESPACE(octave)
