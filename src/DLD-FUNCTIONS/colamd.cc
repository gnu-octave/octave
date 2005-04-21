/*

Copyright (C) 2004 David Bateman
Copyright (C) 1998-2004 Andy Adler

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

// This is the octave interface to colamd, which bore the copyright given
// in the help of the functions.

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>

#include <string>
#include <vector>

#include "ov.h"
#include "defun-dld.h"
#include "pager.h"
#include "ov-re-mat.h"

#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

#if SIZEOF_INT == SIZEOF_OCTAVE_IDX_TYPE

// External COLAMD functions in C
extern "C" {
#include "COLAMD/colamd.h"
}

// The symmetric column elimination tree code take from the Davis LDL code. 
// Copyright given elsewhere in this file.
static 
void symetree (const int *ridx, const int *cidx, int *Parent, int *P, int n)
{
  OCTAVE_LOCAL_BUFFER (int, Flag, n);
  OCTAVE_LOCAL_BUFFER (int, Pinv, (P ? n : 0));
  if (P)
    // If P is present then compute Pinv, the inverse of P
    for (int k = 0 ; k < n ; k++)
      Pinv [P [k]] = k ;

  for (int k = 0 ; k < n ; k++)
    {
      // L(k,:) pattern: all nodes reachable in etree from nz in A(0:k-1,k)
      Parent [k] = n ;	              // parent of k is not yet known 
      Flag [k] = k ;		      // mark node k as visited 
      int kk = (P) ? (P [k]) : (k) ;  // kth original, or permuted, column
      int p2 = cidx [kk+1] ;
      for (int p = cidx [kk] ; p < p2 ; p++)
	{
	  // A (i,k) is nonzero (original or permuted A)
	  int i = (Pinv) ? (Pinv [ridx [p]]) : (ridx [p]) ;
	  if (i < k)
	    {
	      // follow path from i to root of etree, stop at flagged node 
	      for ( ; Flag [i] != k ; i = Parent [i])
		{
		  // find parent of i if not yet determined
		  if (Parent [i] == n)
		    Parent [i] = k ;
		  Flag [i] = k ;	// mark i as visited
		}
	    }
	}
    }
}

// The elimination tree post-ordering code below is taken from SuperLU
static inline
int make_set (int i, int *pp)
{
  pp[i] = i;
  return i;
}

static inline
int link (int s, int t, int *pp)
{
  pp[s] = t;
  return t;
}

static inline
int find (int i, int *pp)
{
  register int p, gp;
    
  p = pp[i];
  gp = pp[p];
  while (gp != p) {
    pp[i] = gp;
    i = gp;
    p = pp[i];
    gp = pp[p];
  }
  return (p);
}

static
int etdfs (int v, int *first_kid, int *next_kid, int *post, int postnum)
{
  for (int w = first_kid[v]; w != -1; w = next_kid[w]) {
    postnum = etdfs (w, first_kid, next_kid, post, postnum);
  }
  post[postnum++] = v;

  return postnum;
}

static
void TreePostorder(int n, int *parent, int *post)
{
  // Allocate storage for working arrays and results
  OCTAVE_LOCAL_BUFFER (int, first_kid, n+1);
  OCTAVE_LOCAL_BUFFER (int, next_kid, n+1);

  // Set up structure describing children
  for (int v = 0; v <= n; first_kid[v++] = -1);
  for (int v = n-1; v >= 0; v--) 
    {
      int dad = parent[v];
      next_kid[v] = first_kid[dad];
      first_kid[dad] = v;
    }

  // Depth-first search from dummy root vertex #n
  etdfs (n, first_kid, next_kid, post, 0);
}

static 
void coletree (const int *ridx, const int *colbeg, int *colend, 
	       int *parent, int nr, int nc)
{
  OCTAVE_LOCAL_BUFFER (int, root, nc);
  OCTAVE_LOCAL_BUFFER (int, pp, nc);
  OCTAVE_LOCAL_BUFFER (int, firstcol, nr);

  // Compute firstcol[row] = first nonzero column in row
  for (int row = 0; row < nr; firstcol[row++] = nc);
  for (int col = 0; col < nc; col++) 
    for (int p = colbeg[col]; p < colend[col]; p++) 
      {
	int row = ridx[p];
	if (firstcol[row] > col)
	  firstcol[row] = col;
      }

  // Compute etree by Liu's algorithm for symmetric matrices,
  // except use (firstcol[r],c) in place of an edge (r,c) of A.
  // Thus each row clique in A'*A is replaced by a star
  // centered at its first vertex, which has the same fill.
  for (int col = 0; col < nc; col++) 
    {
      int cset = make_set (col, pp);
      root[cset] = col;
      parent[col] = nc; 
      for (int p = colbeg[col]; p < colend[col]; p++) 
	{
	  int row = firstcol[ridx[p]];
	  if (row >= col) 
	    continue;
	  int rset = find (row, pp);
	  int rroot = root[rset];
	  if (rroot != col) 
	    {
	      parent[rroot] = col;
	      cset = link (cset, rset, pp);
	      root[cset] = col;
	    }
	}
    }
}

#endif

DEFUN_DLD (colamd, args, nargout,
    "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{p} =} colamd (@var{s})\n\
@deftypefnx {Loadable Function} {@var{p} =} colamd (@var{s}, @var{knobs})\n\
@deftypefnx {Loadable Function} {[@var{p}, @var{stats}] =} colamd (@var{s})\n\
@deftypefnx {Loadable Function} {[@var{p}, @var{stats}] =} colamd (@var{s}, @var{knobs})\n\
\n\
Column approximate minimum degree permutation. @code{@var{p} = colamd\n\
(@var{s})} returns the column approximate minimum degree permutation\n\
vector for the sparse matrix @var{s}. For a non-symmetric matrix @var{s},\n\
@code{@var{s} (:,@var{p})} tends to have sparser LU factors than @var{s}.\n\
The Cholesky factorization of @code{@var{s} (:,@var{p})' * @var{s}\n\
(:,@var{p})} also tends to be sparser than that of @code{@var{s}' *\n\
@var{s}}.\n\
\n\
@var{knobs} is an optional two-element input vector. If @var{s} is\n\
m-by-n, then rows with more than @code{(@var{knobs} (1)) * @var{n}}\n\
entries are ignored.  Columns with more than @code{(@var{knobs} (2)) *\n\
@var{m}} entries are removed prior to ordering, and ordered last in the\n\
output permutation @var{p}. If the knobs parameter is not present, then\n\
0.5 is used instead, for both @code{@var{knobs} (1)} and\n\
@code{@var{knobs} (2)}. @code{@var{knobs} (3)} controls the printing of\n\
statistics and error messages.\n\
\n\
@var{stats} is an optional 20-element output vector that provides data\n\
about the ordering and the validity of the input matrix @var{s}. Ordering\n\
statistics are in @code{@var{stats} (1:3)}. @code{@var{stats} (1)} and\n\
@code{@var{stats} (2)} are the number of dense or empty rows and columns\n\
ignored by COLAMD and @code{@var{stats} (3)} is the number of garbage\n\
collections performed on the internal data structure used by COLAMD\n\
(roughly of size @code{2.2 * nnz(@var{s}) + 4 * @var{m} + 7 * @var{n}}\n\
integers).\n\
\n\
Octave built-in functions are intended to generate valid sparse matrices,\n\
with no duplicate entries, with ascending row indices of the nonzeros\n\
in each column, with a non-negative number of entries in each column (!)\n\
and so on.  If a matrix is invalid, then COLAMD may or may not be able\n\
to continue.  If there are duplicate entries (a row index appears two or\n\
more times in the same column) or if the row indices in a column are out\n\
of order, then COLAMD can correct these errors by ignoring the duplicate\n\
entries and sorting each column of its internal copy of the matrix\n\
@var{s} (the input matrix @var{s} is not repaired, however). If a matrix\n\
is invalid in other ways then COLAMD cannot continue, an error message is\n\
printed, and no output arguments (@var{p} or @var{stats}) are returned.\n\
COLAMD is thus a simple way to check a sparse matrix to see if it's\n\
valid.\n\
\n\
@code{@var{stats} (4:7)} provide information if COLAMD was able to\n\
continue. The matrix is OK if @code{@var{stats} (4)} is zero, or 1 if\n\
invalid. @code{@var{stats} (5)} is the rightmost column index that is\n\
unsorted or contains duplicate entries, or zero if no such column exists.\n\
@code{@var{stats} (6)} is the last seen duplicate or out-of-order row\n\
index in the column index given by @code{@var{stats} (5)}, or zero if no\n\
such row index exists. @code{@var{stats} (7)} is the number of duplicate\n\
or out-of-order row indices. @code{@var{stats} (8:20)} is always zero in\n\
the current version of COLAMD (reserved for future use).\n\
\n\
The ordering is followed by a column elimination tree post-ordering.\n\
\n\
The authors of the code itself are Stefan I. Larimore and Timothy A.\n\
Davis (davis@@cise.ufl.edu), University of Florida.  The algorithm was\n\
developed in collaboration with John Gilbert, Xerox PARC, and Esmond\n\
Ng, Oak Ridge National Laboratory. (see\n\
@url{http://www.cise.ufl.edu/research/sparse/colamd})\n\
@end deftypefn\n\
@seealso{colperm, symamd}")
{
  octave_value_list retval;

#if SIZEOF_INT == SIZEOF_OCTAVE_IDX_TYPE

  int nargin = args.length ();
  int spumoni = 0;
 
  if (nargout < 0 || nargout > 2 || nargin < 0 || nargin > 2)
    usage ("colamd: incorrect number of input and/or output arguments");
  else
    {
      // Get knobs
      OCTAVE_LOCAL_BUFFER (double, knobs, COLAMD_KNOBS);
      colamd_set_defaults (knobs);

      // Check for user-passed knobs
      if (nargin == 2)
	{
	  NDArray User_knobs = args(1).array_value ();
	  int nel_User_knobs = User_knobs.length ();
	  
	  if (nel_User_knobs > 0) 
	    knobs [COLAMD_DENSE_ROW] = User_knobs (COLAMD_DENSE_ROW);
	  if (nel_User_knobs > 1) 
	    knobs [COLAMD_DENSE_COL] = User_knobs (COLAMD_DENSE_COL) ;
	  if (nel_User_knobs > 2) 
	    spumoni = (int) User_knobs (2);
	}

      // print knob settings if spumoni is set
      if (spumoni > 0)
	{
	  octave_stdout << "colamd: dense row fraction: " 
			<< knobs [COLAMD_DENSE_ROW] << std::endl;
	  octave_stdout << "colamd: dense col fraction: " 
			<< knobs [COLAMD_DENSE_COL] << std::endl;
	}
      
      int n_row, n_col, nnz;
      int *ridx, *cidx;
      SparseComplexMatrix scm;
      SparseMatrix sm;

      if (args(0).class_name () == "sparse")
	{
	  if (args(0).is_complex_type ())
	    {
	      scm = args(0). sparse_complex_matrix_value ();
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
	  if (args(0).is_complex_type ())
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
      OCTAVE_LOCAL_BUFFER (int, p, n_col+1);
      for (int i = 0; i < n_col+1; i++)
	p[i] = cidx [i];

      int Alen = colamd_recommended (nnz, n_row, n_col);
      OCTAVE_LOCAL_BUFFER (int, A, Alen);
      for (int i = 0; i < nnz; i++)
	A[i] = ridx [i];

      // Order the columns (destroys A)
      OCTAVE_LOCAL_BUFFER (int, stats, COLAMD_STATS);
      if (!colamd (n_row, n_col, Alen, A, p, knobs, stats))
	{
	  colamd_report (stats) ;
	  error ("colamd: internal error!");
	  return retval;
	}

      // column elimination tree post-ordering (reuse variables)
      OCTAVE_LOCAL_BUFFER (int, colbeg, n_col + 1);
      OCTAVE_LOCAL_BUFFER (int, colend, n_col + 1);
      OCTAVE_LOCAL_BUFFER (int, etree, n_col + 1);

      for (int i = 0; i < n_col; i++)
	{
	  colbeg[i] = cidx[p[i]];
	  colend[i] = cidx[p[i]+1];
	}

      coletree (ridx, colbeg, colend, etree, n_row, n_col);

      // Calculate the tree post-ordering
      TreePostorder (n_col, etree, colbeg);

      // return the permutation vector
      NDArray out_perm (dim_vector (1, n_col));
      for (int i = 0; i < n_col; i++)
	out_perm(i) = p [colbeg [i]] + 1;

      retval (0) = out_perm;

      // print stats if spumoni > 0
      if (spumoni > 0)
	colamd_report (stats) ;

      // Return the stats vector
      if (nargout == 2)
	{
	  NDArray out_stats (dim_vector (1, COLAMD_STATS));
	  for (int i = 0 ; i < COLAMD_STATS ; i++)
	    out_stats (i) = stats [i] ;
	  retval(1) = out_stats;

	  // fix stats (5) and (6), for 1-based information on 
	  // jumbled matrix.  note that this correction doesn't 
	  // occur if symamd returns FALSE
	  out_stats (COLAMD_INFO1) ++ ; 
	  out_stats (COLAMD_INFO2) ++ ; 
	}
    }

#else

  error ("colamd: not available in this version of Octave");

#endif

  return retval;
}

DEFUN_DLD (symamd, args, nargout,
    "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{p} =} symamd (@var{s})\n\
@deftypefnx {Loadable Function} {@var{p} =} symamd (@var{s}, @var{knobs})\n\
@deftypefnx {Loadable Function} {[@var{p}, @var{stats}] =} symamd (@var{s})\n\
@deftypefnx {Loadable Function} {[@var{p}, @var{stats}] =} symamd (@var{s}, @var{knobs})\n\
\n\
For a symmetric positive definite matrix @var{s}, returns the permutation\n\
vector p such that @code{@var{s} (@var{p}, @var{p})} tends to have a\n\
sparser Cholesky factor than @var{s}. Sometimes SYMAMD works well for\n\
symmetric indefinite matrices too. The matrix @var{s} is assumed to be\n\
symmetric; only the strictly lower triangular part is referenced. @var{s}\n\
must be square.\n\
\n\
@var{knobs} is an optional input argument. If @var{s} is n-by-n, then\n\
rows and columns with more than @code{@var{knobs} (1) * @var{n}} entries\n\
are removed prior to ordering, and ordered last in the output permutation\n\
@var{p}. If the @var{knobs} parameter is not present, then the default of\n\
0.5 is used instead. @code{@var{knobs} (2)} controls the printing of\n\
statistics and error messages.\n\
\n\
@var{stats} is an optional 20-element output vector that provides data\n\
about the ordering and the validity of the input matrix @var{s}. Ordering\n\
statistics are in @code{@var{stats} (1:3)}. @code{@var{stats} (1) =\n\
@var{stats} (2)} is the number of dense or empty rows and columns\n\
ignored by SYMAMD and @code{@var{stats} (3)} is the number of garbage\n\
collections performed on the internal data structure used by SYMAMD\n\
(roughly of size @code{8.4 * nnz (tril (@var{s}, -1)) + 9 * @var{n}}\n\
integers).\n\
\n\
Octave built-in functions are intended to generate valid sparse matrices,\n\
with no duplicate entries, with ascending row indices of the nonzeros\n\
in each column, with a non-negative number of entries in each column (!)\n\
and so on.  If a matrix is invalid, then SYMAMD may or may not be able\n\
to continue.  If there are duplicate entries (a row index appears two or\n\
more times in the same column) or if the row indices in a column are out\n\
of order, then SYMAMD can correct these errors by ignoring the duplicate\n\
entries and sorting each column of its internal copy of the matrix S (the\n\
input matrix S is not repaired, however).  If a matrix is invalid in\n\
other ways then SYMAMD cannot continue, an error message is printed, and\n\
no output arguments (@var{p} or @var{stats}) are returned.  SYMAMD is\n\
thus a simple way to check a sparse matrix to see if it's valid.\n\
\n\
@code{@var{stats} (4:7)} provide information if SYMAMD was able to\n\
continue. The matrix is OK if @code{@var{stats} (4)} is zero, or 1\n\
if invalid. @code{@var{stats} (5)} is the rightmost column index that\n\
is unsorted or contains duplicate entries, or zero if no such column\n\
exists. @code{@var{stats} (6)} is the last seen duplicate or out-of-order\n\
row index in the column index given by @code{@var{stats} (5)}, or zero\n\
if no such row index exists. @code{@var{stats} (7)} is the number of\n\
duplicate or out-of-order row indices. @code{@var{stats} (8:20)} is\n\
always zero in the current version of SYMAMD (reserved for future use).\n\
\n\
The ordering is followed by a column elimination tree post-ordering.\n\
\n\
\n\
The authors of the code itself are Stefan I. Larimore and Timothy A.\n\
Davis (davis@@cise.ufl.edu), University of Florida.  The algorithm was\n\
developed in collaboration with John Gilbert, Xerox PARC, and Esmond\n\
Ng, Oak Ridge National Laboratory. (see\n\
@url{http://www.cise.ufl.edu/research/sparse/colamd})\n\
@end deftypefn\n\
@seealso{colperm, colamd}")
{
  octave_value_list retval;

#if SIZEOF_INT == SIZEOF_OCTAVE_IDX_TYPE

  int nargin = args.length ();
  int spumoni = 0;
 
  if (nargout < 0 || nargout > 2 || nargin < 0 || nargin > 2)
    usage ("symamd: incorrect number of input and/or output arguments");
  else
    {
      // Get knobs
      OCTAVE_LOCAL_BUFFER (double, knobs, COLAMD_KNOBS);
      colamd_set_defaults (knobs);

      // Check for user-passed knobs
      if (nargin == 2)
	{
	  NDArray User_knobs = args(1).array_value ();
	  int nel_User_knobs = User_knobs.length ();
	  
	  if (nel_User_knobs > 0) 
	    knobs [COLAMD_DENSE_ROW] = User_knobs (COLAMD_DENSE_ROW);
	  if (nel_User_knobs > 1) 
	    spumoni = (int) User_knobs (1);
	}

      // print knob settings if spumoni is set
      if (spumoni > 0)
	octave_stdout << "symamd: dense row/col fraction: " 
		      << knobs [COLAMD_DENSE_ROW] << std::endl;
      
      int n_row, n_col, nnz;
      int *ridx, *cidx;
      SparseMatrix sm;
      SparseComplexMatrix scm;

      if (args(0).class_name () == "sparse")
	{
	  if (args(0).is_complex_type ())
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
	  if (args(0).is_complex_type ())
	    sm = SparseMatrix (real (args(0).complex_matrix_value ()));
	  else
	    sm = SparseMatrix (args(0).matrix_value ());
	  
	  n_row = sm.rows ();
	  n_col = sm.cols ();
	  nnz = sm.nnz ();
	  ridx = sm.xridx ();
	  cidx = sm.xcidx ();
	}

      if (n_row != n_col)
	{
	  error ("symamd: matrix must be square");
	  return retval;
	}

      // Allocate workspace for symamd
      OCTAVE_LOCAL_BUFFER (int, perm, n_col+1);
      OCTAVE_LOCAL_BUFFER (int, stats, COLAMD_STATS);
      if (!symamd (n_col, ridx, cidx, perm, knobs, stats, &calloc, &free))
	{
	  symamd_report (stats) ;
	  error ("symamd: internal error!") ;
	  return retval;
	}

      // column elimination tree post-ordering
      OCTAVE_LOCAL_BUFFER (int, etree, n_col + 1);
      symetree (ridx, cidx, etree, perm, n_col);

      // Calculate the tree post-ordering
      OCTAVE_LOCAL_BUFFER (int, post, n_col + 1);
      TreePostorder (n_col, etree, post);

      // return the permutation vector
      NDArray out_perm (dim_vector (1, n_col));
      for (int i = 0; i < n_col; i++)
	out_perm(i) = perm [post [i]] + 1;

      retval (0) = out_perm;

      // print stats if spumoni > 0
      if (spumoni > 0)
	symamd_report (stats) ;

      // Return the stats vector
      if (nargout == 2)
	{
	  NDArray out_stats (dim_vector (1, COLAMD_STATS));
	  for (int i = 0 ; i < COLAMD_STATS ; i++)
	    out_stats (i) = stats [i] ;
	  retval(1) = out_stats;

	  // fix stats (5) and (6), for 1-based information on 
	  // jumbled matrix.  note that this correction doesn't 
	  // occur if symamd returns FALSE
	  out_stats (COLAMD_INFO1) ++ ; 
	  out_stats (COLAMD_INFO2) ++ ; 
	}
    }

#else

  error ("symamd: not available in this version of Octave");

#endif

  return retval;
}

DEFUN_DLD (etree, args, nargout,
    "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{p} =} etree (@var{s})\n\
@deftypefnx {Loadable Function} {@var{p} =} etree (@var{s}, @var{typ})\n\
@deftypefnx {Loadable Function} {[@var{p}, @var{q}] =} etree (@var{s}, @var{typ})\n\
\n\
Returns the elimination tree for the matrix @var{s}. By default @var{s}\n\
is assumed to be symmetric and the symmetric elimination tree is\n\
returned. The argument @var{typ} controls whether a symmetric or\n\
column elimination tree is returned. Valid values of @var{typ} are\n\
'sym' or 'col', for symmetric or column elimination tree respectively\n\
\n\
Called with a second argument, @dfn{etree} also returns the postorder\n\
permutations on the tree.\n\
@end deftypefn")
{
  octave_value_list retval;

#if SIZEOF_INT == SIZEOF_OCTAVE_IDX_TYPE

  int nargin = args.length ();

  if (nargout < 0 || nargout > 2 || nargin < 0 || nargin > 2)
    usage ("etree: incorrect number of input and/or output arguments");
  else
    {
      int n_row, n_col, nnz;
      int *ridx, *cidx;
      bool is_sym = true;
      SparseMatrix sm;
      SparseComplexMatrix scm;

      if (args(0).class_name () == "sparse")
	{
	  if (args(0).is_complex_type ())
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
	  error ("etree: must be called with a sparse matrix");
	  return retval;
	}

      if (nargin == 2)
	if (args(1).is_string ())
	  {
	    std::string str = args(1).string_value ();
	    if (str.find("C") == 0 || str.find("c") == 0)
	      is_sym = false;
	  }
	else
	  {
	    error ("etree: second argument must be a string");
	    return retval;
	  }

      // column elimination tree post-ordering (reuse variables)
      OCTAVE_LOCAL_BUFFER (int, etree, n_col + 1);
      

      if (is_sym)
	{
	  if (n_row != n_col)
	    {
	      error ("etree: matrix is marked as symmetric, but not square");
	      return retval;
	    }
	  symetree (ridx, cidx, etree, NULL, n_col);
	}
      else
	{
	  OCTAVE_LOCAL_BUFFER (int, colbeg, n_col);
	  OCTAVE_LOCAL_BUFFER (int, colend, n_col);

	  for (int i = 0; i < n_col; i++)
	    {
	      colbeg[i] = cidx[i];
	      colend[i] = cidx[i+1];
	    }

	  coletree (ridx, colbeg, colend, etree, n_row, n_col);
	}

      NDArray tree (dim_vector (1, n_col));
      for (int i = 0; i < n_col; i++)
	// We flag a root with n_col while Matlab does it with zero
	// Convert for matlab compatiable output
	if (etree[i] == n_col)
	  tree (i) = 0;
	else
	  tree (i) = etree[i] + 1;

      retval (0) = tree;

      if (nargout == 2)
	{
	  // Calculate the tree post-ordering
	  OCTAVE_LOCAL_BUFFER (int, post, n_col + 1);
	  TreePostorder (n_col, etree, post);

	  NDArray postorder (dim_vector (1, n_col));
	  for (int i = 0; i < n_col; i++)
	    postorder (i) = post[i] + 1;

	  retval (1) = postorder;
	}
    }

#else

  error ("etree: not available in this version of Octave");

#endif

  return retval;
}

DEFUN_DLD (symbfact, args, nargout,
    "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{count}, @var{h}, @var{parent}, @var{post}, @var{r}]} = symbfact (@var{s}, @var{typ})\n\
\n\
Performs a symbolic factorization analysis on the sparse matrix @var{s}.\n\
@end deftypefn")
{
  error ("symbfact: not implemented yet");
  return octave_value ();
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
