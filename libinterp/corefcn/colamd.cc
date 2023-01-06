////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1998-2023 The Octave Project Developers
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

// This is the octave interface to colamd, which bore the copyright given
// in the help of the functions.

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstdlib>

#include <string>

#include "CSparse.h"
#include "dNDArray.h"
#include "dSparse.h"
#include "oct-locbuf.h"
#include "oct-sparse.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"
#include "pager.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// The symmetric column elimination tree code take from the Davis LDL code.
// Copyright given elsewhere in this file.
static void
symetree (const octave_idx_type *ridx, const octave_idx_type *cidx,
          octave_idx_type *Parent, octave_idx_type *P, octave_idx_type n)
{
  OCTAVE_LOCAL_BUFFER (octave_idx_type, Flag, n);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, Pinv, (P ? n : 0));
  if (P)
    // If P is present then compute Pinv, the inverse of P
    for (octave_idx_type k = 0 ; k < n ; k++)
      Pinv[P[k]] = k;

  for (octave_idx_type k = 0 ; k < n ; k++)
    {
      // L(k,:) pattern: all nodes reachable in etree from nz in A(0:k-1,k)
      Parent[k] = n ;                // parent of k is not yet known
      Flag[k] = k ;                  // mark node k as visited
      octave_idx_type kk = (P ? P[k] : k); // kth original, or permuted, column
      octave_idx_type p2 = cidx[kk+1];
      for (octave_idx_type p = cidx[kk] ; p < p2 ; p++)
        {
          // A (i,k) is nonzero (original or permuted A)
          octave_idx_type i = (P ? Pinv[ridx[p]] : ridx[p]);
          if (i < k)
            {
              // follow path from i to root of etree, stop at flagged node
              for ( ; Flag[i] != k ; i = Parent[i])
                {
                  // find parent of i if not yet determined
                  if (Parent[i] == n)
                    Parent[i] = k;
                  Flag[i] = k ;        // mark i as visited
                }
            }
        }
    }
}

// The elimination tree post-ordering code below is taken from SuperLU
static inline octave_idx_type
make_set (octave_idx_type i, octave_idx_type *pp)
{
  pp[i] = i;
  return i;
}

static inline octave_idx_type
link (octave_idx_type s, octave_idx_type t, octave_idx_type *pp)
{
  pp[s] = t;
  return t;
}

static inline octave_idx_type
find (octave_idx_type i, octave_idx_type *pp)
{
  octave_idx_type p = pp[i];
  octave_idx_type gp = pp[p];

  while (gp != p)
    {
      pp[i] = gp;
      i = gp;
      p = pp[i];
      gp = pp[p];
    }

  return p;
}

static octave_idx_type
etdfs (octave_idx_type v, octave_idx_type *first_kid,
       octave_idx_type *next_kid, octave_idx_type *post,
       octave_idx_type postnum)
{
  for (octave_idx_type w = first_kid[v]; w != -1; w = next_kid[w])
    postnum = etdfs (w, first_kid, next_kid, post, postnum);

  post[postnum++] = v;

  return postnum;
}

static void
tree_postorder (octave_idx_type n, octave_idx_type *parent,
                octave_idx_type *post)
{
  // Allocate storage for working arrays and results
  OCTAVE_LOCAL_BUFFER (octave_idx_type, first_kid, n+1);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, next_kid, n+1);

  // Set up structure describing children
  for (octave_idx_type v = 0; v <= n; first_kid[v++] = -1)
    ; // do nothing

  for (octave_idx_type v = n-1; v >= 0; v--)
    {
      octave_idx_type dad = parent[v];
      next_kid[v] = first_kid[dad];
      first_kid[dad] = v;
    }

  // Depth-first search from dummy root vertex #n
  etdfs (n, first_kid, next_kid, post, 0);
}

static void
coletree (const octave_idx_type *ridx, const octave_idx_type *colbeg,
          octave_idx_type *colend, octave_idx_type *parent,
          octave_idx_type nr, octave_idx_type nc)
{
  OCTAVE_LOCAL_BUFFER (octave_idx_type, root, nc);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, pp, nc);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, firstcol, nr);

  // Compute firstcol[row] = first nonzero column in row
  for (octave_idx_type row = 0; row < nr; firstcol[row++] = nc)
    ; // do nothing

  for (octave_idx_type col = 0; col < nc; col++)
    for (octave_idx_type p = colbeg[col]; p < colend[col]; p++)
      {
        octave_idx_type row = ridx[p];
        if (firstcol[row] > col)
          firstcol[row] = col;
      }

  // Compute etree by Liu's algorithm for symmetric matrices,
  // except use (firstcol[r],c) in place of an edge (r,c) of A.
  // Thus each row clique in A'*A is replaced by a star
  // centered at its first vertex, which has the same fill.
  for (octave_idx_type col = 0; col < nc; col++)
    {
      octave_idx_type cset = make_set (col, pp);
      root[cset] = col;
      parent[col] = nc;
      for (octave_idx_type p = colbeg[col]; p < colend[col]; p++)
        {
          octave_idx_type row = firstcol[ridx[p]];
          if (row >= col)
            continue;
          octave_idx_type rset = find (row, pp);
          octave_idx_type rroot = root[rset];
          if (rroot != col)
            {
              parent[rroot] = col;
              cset = link (cset, rset, pp);
              root[cset] = col;
            }
        }
    }
}

DEFUN (colamd, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{p} =} colamd (@var{S})
@deftypefnx {} {@var{p} =} colamd (@var{S}, @var{knobs})
@deftypefnx {} {[@var{p}, @var{stats}] =} colamd (@var{S})
@deftypefnx {} {[@var{p}, @var{stats}] =} colamd (@var{S}, @var{knobs})

Compute the column approximate minimum degree permutation.

@code{@var{p} = colamd (@var{S})} returns the column approximate minimum
degree permutation vector for the sparse matrix @var{S}.  For a
non-symmetric matrix @var{S}, @code{@var{S}(:,@var{p})} tends to have
sparser LU@tie{}factors than @var{S}.  The Cholesky@tie{}factorization of
@code{@var{S}(:,@var{p})' * @var{S}(:,@var{p})} also tends to be sparser
than that of @code{@var{S}' * @var{S}}.

@var{knobs} is an optional one- to three-element input vector.  If @var{S}
is m-by-n, then rows with more than @code{max(16,@var{knobs}(1)*sqrt(n))}
entries are ignored.  Columns with more than
@code{max (16,@var{knobs}(2)*sqrt(min(m,n)))} entries are removed prior to
ordering, and ordered last in the output permutation @var{p}.  Only
completely dense rows or columns are removed if @code{@var{knobs}(1)} and
@code{@var{knobs}(2)} are < 0, respectively.  If @code{@var{knobs}(3)} is
nonzero, @var{stats} and @var{knobs} are printed.  The default is
@code{@var{knobs} = [10 10 0]}.  Note that @var{knobs} differs from earlier
versions of colamd.

@var{stats} is an optional 20-element output vector that provides data
about the ordering and the validity of the input matrix @var{S}.  Ordering
statistics are in @code{@var{stats}(1:3)}.  @code{@var{stats}(1)} and
@code{@var{stats}(2)} are the number of dense or empty rows and columns
ignored by @sc{colamd} and @code{@var{stats}(3)} is the number of garbage
collections performed on the internal data structure used by @sc{colamd}
(roughly of size @code{2.2 * nnz(@var{S}) + 4 * @var{m} + 7 * @var{n}}
integers).

Octave built-in functions are intended to generate valid sparse matrices,
with no duplicate entries, with ascending row indices of the nonzeros
in each column, with a non-negative number of entries in each column (!)
and so on.  If a matrix is invalid, then @sc{colamd} may or may not be able
to continue.  If there are duplicate entries (a row index appears two or
more times in the same column) or if the row indices in a column are out
of order, then @sc{colamd} can correct these errors by ignoring the
duplicate entries and sorting each column of its internal copy of the
matrix @var{S} (the input matrix @var{S} is not repaired, however).  If a
matrix is invalid in other ways then @sc{colamd} cannot continue, an error
message is printed, and no output arguments (@var{p} or @var{stats}) are
returned.
@sc{colamd} is thus a simple way to check a sparse matrix to see if it's
valid.

@code{@var{stats}(4:7)} provide information if @sc{colamd} was able to
continue.  The matrix is OK if @code{@var{stats}(4)} is zero, or 1 if
invalid.  @code{@var{stats}(5)} is the rightmost column index that is
unsorted or contains duplicate entries, or zero if no such column exists.
@code{@var{stats}(6)} is the last seen duplicate or out-of-order row
index in the column index given by @code{@var{stats}(5)}, or zero if no
such row index exists.  @code{@var{stats}(7)} is the number of duplicate
or out-of-order row indices.  @code{@var{stats}(8:20)} is always zero in
the current version of @sc{colamd} (reserved for future use).

The ordering is followed by a column elimination tree post-ordering.

The authors of the code itself are @nospell{Stefan I. Larimore} and
@nospell{Timothy A. Davis}.  The algorithm was developed in collaboration with
@nospell{John Gilbert}, Xerox PARC, and @nospell{Esmond Ng}, Oak Ridge National
Laboratory.  (see @url{http://faculty.cse.tamu.edu/davis/suitesparse.html})
@seealso{colperm, symamd, ccolamd}
@end deftypefn */)
{
#if defined (HAVE_COLAMD)

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  octave_value_list retval (nargout == 2 ? 2 : 1);
  int spumoni = 0;

  // Get knobs
  static_assert (COLAMD_KNOBS <= 40,
                 "colamd: # of COLAMD_KNOBS exceeded.  Please report this to bugs.octave.org");
  double knob_storage[COLAMD_KNOBS];
  double *knobs = &knob_storage[0];
  COLAMD_NAME (_set_defaults) (knobs);

  // Check for user-passed knobs
  if (nargin == 2)
    {
      NDArray User_knobs = args(1).array_value ();
      int nel_User_knobs = User_knobs.numel ();

      if (nel_User_knobs > 0)
        knobs[COLAMD_DENSE_ROW] = User_knobs(0);
      if (nel_User_knobs > 1)
        knobs[COLAMD_DENSE_COL] = User_knobs(1);
      if (nel_User_knobs > 2)
        spumoni = static_cast<int> (User_knobs(2));

      // print knob settings if spumoni is set
      if (spumoni)
        {

          octave_stdout << "\ncolamd version " << COLAMD_MAIN_VERSION
                        << '.' <<  COLAMD_SUB_VERSION
                        << ", " << COLAMD_DATE << ":\n";

          if (knobs[COLAMD_DENSE_ROW] >= 0)
            octave_stdout << "knobs(1): " << User_knobs (0)
                          << ", rows with > max (16,"
                          << knobs[COLAMD_DENSE_ROW] << "* sqrt (columns(A)))"
                          << " entries removed\n";
          else
            octave_stdout << "knobs(1): " << User_knobs (0)
                          << ", only completely dense rows removed\n";

          if (knobs[COLAMD_DENSE_COL] >= 0)
            octave_stdout << "knobs(2): " << User_knobs (1)
                          << ", cols with > max (16,"
                          << knobs[COLAMD_DENSE_COL] << "* sqrt (size(A)))"
                          << " entries removed\n";
          else
            octave_stdout << "knobs(2): " << User_knobs (1)
                          << ", only completely dense columns removed\n";

          octave_stdout << "knobs(3): " << User_knobs (2)
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

  // Allocate workspace for colamd
  OCTAVE_LOCAL_BUFFER (suitesparse_integer, p, n_col+1);
  for (octave_idx_type i = 0; i < n_col+1; i++)
    p[i] = cidx[i];

  octave_idx_type Alen = COLAMD_NAME (_recommended) (nnz, n_row, n_col);
  OCTAVE_LOCAL_BUFFER (suitesparse_integer, A, Alen);
  for (octave_idx_type i = 0; i < nnz; i++)
    A[i] = ridx[i];

  // Order the columns (destroys A)
  static_assert (COLAMD_STATS <= 40,
                 "colamd: # of COLAMD_STATS exceeded.  Please report this to bugs.octave.org");
  suitesparse_integer stats_storage[COLAMD_STATS];
  suitesparse_integer *stats = &stats_storage[0];
  if (! COLAMD_NAME () (n_row, n_col, Alen, A, p, knobs, stats))
    {
      COLAMD_NAME (_report)(stats);

      error ("colamd: internal error!");
    }

  // column elimination tree post-ordering (reuse variables)
  OCTAVE_LOCAL_BUFFER (octave_idx_type, colbeg, n_col + 1);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, colend, n_col + 1);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, etree, n_col + 1);

  for (octave_idx_type i = 0; i < n_col; i++)
    {
      colbeg[i] = cidx[p[i]];
      colend[i] = cidx[p[i]+1];
    }

  coletree (ridx, colbeg, colend, etree, n_row, n_col);

  // Calculate the tree post-ordering
  tree_postorder (n_col, etree, colbeg);

  // return the permutation vector
  NDArray out_perm (dim_vector (1, n_col));
  for (octave_idx_type i = 0; i < n_col; i++)
    out_perm(i) = p[colbeg[i]] + 1;

  retval(0) = out_perm;

  // print stats if spumoni > 0
  if (spumoni > 0)
    COLAMD_NAME (_report)(stats);

  // Return the stats vector
  if (nargout == 2)
    {
      NDArray out_stats (dim_vector (1, COLAMD_STATS));
      for (octave_idx_type i = 0 ; i < COLAMD_STATS ; i++)
        out_stats(i) = stats[i];
      retval(1) = out_stats;

      // fix stats (5) and (6), for 1-based information on
      // jumbled matrix.  note that this correction doesn't
      // occur if symamd returns FALSE
      out_stats(COLAMD_INFO1)++;
      out_stats(COLAMD_INFO2)++;
    }

  return retval;

#else

  octave_unused_parameter (args);
  octave_unused_parameter (nargout);

  err_disabled_feature ("colamd", "COLAMD");

#endif
}

DEFUN (symamd, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{p} =} symamd (@var{S})
@deftypefnx {} {@var{p} =} symamd (@var{S}, @var{knobs})
@deftypefnx {} {[@var{p}, @var{stats}] =} symamd (@var{S})
@deftypefnx {} {[@var{p}, @var{stats}] =} symamd (@var{S}, @var{knobs})

For a symmetric positive definite matrix @var{S}, returns the permutation
vector p such that @code{@var{S}(@var{p}, @var{p})} tends to have a
sparser Cholesky@tie{}factor than @var{S}.

Sometimes @code{symamd} works well for symmetric indefinite matrices too.
The matrix @var{S} is assumed to be symmetric; only the strictly lower
triangular part is referenced.  @var{S} must be square.

@var{knobs} is an optional one- to two-element input vector.  If @var{S} is
n-by-n, then rows and columns with more than
@code{max (16,@var{knobs}(1)*sqrt(n))} entries are removed prior to
ordering, and ordered last in the output permutation @var{p}.  No
rows/columns are removed if @code{@var{knobs}(1) < 0}.  If
@code{@var{knobs}(2)} is nonzero, @var{stats} and @var{knobs} are
printed.  The default is @code{@var{knobs} = [10 0]}.  Note that
@var{knobs} differs from earlier versions of @code{symamd}.

@var{stats} is an optional 20-element output vector that provides data
about the ordering and the validity of the input matrix @var{S}.  Ordering
statistics are in @code{@var{stats}(1:3)}.
@code{@var{stats}(1) = @var{stats}(2)} is the number of dense or empty rows
and columns ignored by SYMAMD and @code{@var{stats}(3)} is the number of
garbage collections performed on the internal data structure used by SYMAMD
(roughly of size @code{8.4 * nnz (tril (@var{S}, -1)) + 9 * @var{n}}
integers).

Octave built-in functions are intended to generate valid sparse matrices,
with no duplicate entries, with ascending row indices of the nonzeros
in each column, with a non-negative number of entries in each column (!)
and so on.  If a matrix is invalid, then SYMAMD may or may not be able
to continue.  If there are duplicate entries (a row index appears two or
more times in the same column) or if the row indices in a column are out
of order, then SYMAMD can correct these errors by ignoring the duplicate
entries and sorting each column of its internal copy of the matrix S (the
input matrix S is not repaired, however).  If a matrix is invalid in
other ways then SYMAMD cannot continue, an error message is printed, and
no output arguments (@var{p} or @var{stats}) are returned.  SYMAMD is
thus a simple way to check a sparse matrix to see if it's valid.

@code{@var{stats}(4:7)} provide information if SYMAMD was able to
continue.  The matrix is OK if @code{@var{stats} (4)} is zero, or 1
if invalid.  @code{@var{stats}(5)} is the rightmost column index that
is unsorted or contains duplicate entries, or zero if no such column
exists.  @code{@var{stats}(6)} is the last seen duplicate or out-of-order
row index in the column index given by @code{@var{stats}(5)}, or zero
if no such row index exists.  @code{@var{stats}(7)} is the number of
duplicate or out-of-order row indices.  @code{@var{stats}(8:20)} is
always zero in the current version of SYMAMD (reserved for future use).

The ordering is followed by a column elimination tree post-ordering.

The authors of the code itself are @nospell{Stefan I. Larimore} and
@nospell{Timothy A. Davis}.  The algorithm was developed in collaboration with
@nospell{John Gilbert}, Xerox PARC, and @nospell{Esmond Ng}, Oak Ridge National
Laboratory.  (see @url{http://faculty.cse.tamu.edu/davis/suitesparse.html})
@seealso{colperm, colamd}
@end deftypefn */)
{
#if defined (HAVE_COLAMD)

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  octave_value_list retval (nargin == 2 ? 2 : 1);
  int spumoni = 0;

  // Get knobs
  static_assert (COLAMD_KNOBS <= 40,
                 "symamd: # of COLAMD_KNOBS exceeded.  Please report this to bugs.octave.org");
  double knob_storage[COLAMD_KNOBS];
  double *knobs = &knob_storage[0];
  COLAMD_NAME (_set_defaults) (knobs);

  // Check for user-passed knobs
  if (nargin == 2)
    {
      NDArray User_knobs = args(1).array_value ();
      int nel_User_knobs = User_knobs.numel ();

      if (nel_User_knobs > 0)
        knobs[COLAMD_DENSE_ROW] = User_knobs(COLAMD_DENSE_ROW);
      if (nel_User_knobs > 1)
        spumoni = static_cast<int> (User_knobs (1));
    }

  // print knob settings if spumoni is set
  if (spumoni > 0)
    octave_stdout << "symamd: dense row/col fraction: "
                  << knobs[COLAMD_DENSE_ROW] << std::endl;

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
    err_square_matrix_required ("symamd", "S");

  // Allocate workspace for symamd
  OCTAVE_LOCAL_BUFFER (octave_idx_type, perm, n_col+1);
  static_assert (COLAMD_STATS <= 40,
                 "symamd: # of COLAMD_STATS exceeded.  Please report this to bugs.octave.org");
  suitesparse_integer stats_storage[COLAMD_STATS];
  suitesparse_integer *stats = &stats_storage[0];
  if (! SYMAMD_NAME () (n_col, to_suitesparse_intptr (ridx),
                        to_suitesparse_intptr (cidx),
                        to_suitesparse_intptr (perm),
                        knobs, stats, &calloc, &free))
    {
      SYMAMD_NAME (_report)(stats);

      error ("symamd: internal error!");
    }

  // column elimination tree post-ordering
  OCTAVE_LOCAL_BUFFER (octave_idx_type, etree, n_col + 1);
  symetree (ridx, cidx, etree, perm, n_col);

  // Calculate the tree post-ordering
  OCTAVE_LOCAL_BUFFER (octave_idx_type, post, n_col + 1);
  tree_postorder (n_col, etree, post);

  // return the permutation vector
  NDArray out_perm (dim_vector (1, n_col));
  for (octave_idx_type i = 0; i < n_col; i++)
    out_perm(i) = perm[post[i]] + 1;

  retval(0) = out_perm;

  // print stats if spumoni > 0
  if (spumoni > 0)
    SYMAMD_NAME (_report)(stats);

  // Return the stats vector
  if (nargout == 2)
    {
      NDArray out_stats (dim_vector (1, COLAMD_STATS));
      for (octave_idx_type i = 0 ; i < COLAMD_STATS ; i++)
        out_stats(i) = stats[i];
      retval(1) = out_stats;

      // fix stats (5) and (6), for 1-based information on
      // jumbled matrix.  note that this correction doesn't
      // occur if symamd returns FALSE
      out_stats(COLAMD_INFO1)++;
      out_stats(COLAMD_INFO2)++;
    }

  return retval;

#else

  octave_unused_parameter (args);
  octave_unused_parameter (nargout);

  err_disabled_feature ("symamd", "COLAMD");

#endif
}

DEFUN (etree, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{p} =} etree (@var{S})
@deftypefnx {} {@var{p} =} etree (@var{S}, @var{typ})
@deftypefnx {} {[@var{p}, @var{q}] =} etree (@var{S}, @var{typ})

Return the elimination tree for the matrix @var{S}.

By default @var{S} is assumed to be symmetric and the symmetric elimination
tree is returned.  The argument @var{typ} controls whether a symmetric or
column elimination tree is returned.  Valid values of @var{typ} are
@qcode{"sym"} or @qcode{"col"}, for symmetric or column elimination tree
respectively.

Called with a second argument, @code{etree} also returns the postorder
permutations on the tree.
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  octave_value_list retval (nargout == 2 ? 2 : 1);

  octave_idx_type n_row = 0;
  octave_idx_type n_col = 0;
  octave_idx_type *ridx = nullptr;
  octave_idx_type *cidx = nullptr;

  SparseComplexMatrix scm;
  SparseBoolMatrix sbm;
  SparseMatrix sm;

  if (args(0).iscomplex ())
    {
      scm = args(0).sparse_complex_matrix_value ();

      n_row = scm.rows ();
      n_col = scm.cols ();
      ridx = scm.xridx ();
      cidx = scm.xcidx ();
    }
  else if (args(0).islogical ())
    {
      sbm = args(0).sparse_bool_matrix_value ();

      n_row = sbm.rows ();
      n_col = sbm.cols ();
      ridx = sbm.xridx ();
      cidx = sbm.xcidx ();
    }
  else
    {
      sm = args(0).sparse_matrix_value ();

      n_row = sm.rows ();
      n_col = sm.cols ();
      ridx = sm.xridx ();
      cidx = sm.xcidx ();
    }

  bool is_sym = true;

  if (nargin == 2)
    {
      std::string str = args(1).xstring_value ("etree: TYP must be a string");
      if (str.find ('C') == 0 || str.find ('c') == 0)
        is_sym = false;
    }

  // column elimination tree post-ordering (reuse variables)
  OCTAVE_LOCAL_BUFFER (octave_idx_type, etree, n_col + 1);

  if (is_sym)
    {
      if (n_row != n_col)
        error ("etree: S is marked as symmetric, but is not square");

      symetree (ridx, cidx, etree, nullptr, n_col);
    }
  else
    {
      OCTAVE_LOCAL_BUFFER (octave_idx_type, colbeg, n_col);
      OCTAVE_LOCAL_BUFFER (octave_idx_type, colend, n_col);

      for (octave_idx_type i = 0; i < n_col; i++)
        {
          colbeg[i] = cidx[i];
          colend[i] = cidx[i+1];
        }

      coletree (ridx, colbeg, colend, etree, n_row, n_col);
    }

  NDArray tree (dim_vector (1, n_col));
  for (octave_idx_type i = 0; i < n_col; i++)
    // We flag a root with n_col while Matlab does it with zero
    // Convert for Matlab-compatible output
    if (etree[i] == n_col)
      tree(i) = 0;
    else
      tree(i) = etree[i] + 1;

  retval(0) = tree;

  if (nargout == 2)
    {
      // Calculate the tree post-ordering
      OCTAVE_LOCAL_BUFFER (octave_idx_type, post, n_col + 1);
      tree_postorder (n_col, etree, post);

      NDArray postorder (dim_vector (1, n_col));
      for (octave_idx_type i = 0; i < n_col; i++)
        postorder(i) = post[i] + 1;

      retval(1) = postorder;
    }

  return retval;
}

/*
%!assert (etree (sparse ([1,2], [1,2], [1,1], 2, 2)), [0, 0])
%!assert (etree (sparse ([1,2], [1,2], [true, true], 2, 2)), [0, 0])
%!assert (etree (sparse ([1,2], [1,2], [i,i], 2, 2)), [0, 0])
%!assert (etree (gallery ("poisson", 16)), [2:256, 0])

%!error <Invalid call> etree ()
%!error <Invalid call> etree (1, 2, 3)
%!error <TYP must be a string> etree (speye (2), 3)
%!error <is not square> etree (sprand (2, 4, .25))
*/

OCTAVE_END_NAMESPACE(octave)
