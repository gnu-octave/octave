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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cmath>

#include <algorithm>
#include <string>

#include "CSparse.h"
#include "boolSparse.h"
#include "dColVector.h"
#include "dSparse.h"
#include "oct-locbuf.h"
#include "oct-sparse.h"
#include "oct-spparms.h"
#include "sparse-util.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"
#include "parse.h"
#include "utils.h"

OCTAVE_BEGIN_NAMESPACE(octave)

DEFUN (symbfact, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {[@var{count}, @var{h}, @var{parent}, @var{post}, @var{R}] =} symbfact (@var{S})
@deftypefnx {} {[@dots{}] =} symbfact (@var{S}, @var{typ})
@deftypefnx {} {[@dots{}] =} symbfact (@var{S}, @var{typ}, @var{mode})

Perform a symbolic factorization analysis of the sparse matrix @var{S}.

The input variables are

@table @var
@item S
@var{S} is a real or complex sparse matrix.

@item typ
Is the type of the factorization and can be one of

@table @asis
@item @qcode{"sym"} (default)
Factorize @var{S}.  Assumes @var{S} is symmetric and uses the upper
triangular portion of the matrix.

@item @qcode{"col"}
Factorize @tcode{@var{S}' * @var{S}}.

@item @qcode{"row"}
Factorize @tcode{@var{S} * @var{S}'}.

@item @qcode{"lo"}
Factorize @tcode{@var{S}'}.  Assumes @var{S} is symmetric and uses the lower
triangular portion of the matrix.
@end table

@item mode
When @var{mode} is unspecified return the Cholesky@tie{}factorization for
@var{R}.  If @var{mode} is @qcode{"lower"} or @qcode{"L"} then return
the conjugate transpose @tcode{@var{R}'} which is a lower triangular factor.
The conjugate transpose version is faster and uses less memory, but still
returns the same values for all other outputs: @var{count}, @var{h},
@var{parent}, and @var{post}.
@end table

The output variables are:

@table @var
@item count
The row counts of the Cholesky@tie{}factorization as determined by
@var{typ}.  The computational difficulty of performing the true
factorization using @code{chol} is @code{sum (@var{count} .^ 2)}.

@item h
The height of the elimination tree.

@item parent
The elimination tree itself.

@item post
A sparse boolean matrix whose structure is that of the
Cholesky@tie{}factorization as determined by @var{typ}.
@end table
@seealso{chol, etree, treelayout}
@end deftypefn */)
{
#if defined (HAVE_CHOLMOD)

  int nargin = args.length ();

  if (nargin < 1 || nargin > 3)
    print_usage ();

  octave_value_list retval;

  double dummy;
  cholmod_sparse Astore;
  cholmod_sparse *A = &Astore;
  A->packed = true;
  A->sorted = true;
  A->nz = nullptr;
#if defined (OCTAVE_ENABLE_64)
  A->itype = CHOLMOD_LONG;
#else
  A->itype = CHOLMOD_INT;
#endif
  A->dtype = CHOLMOD_DOUBLE;
  A->stype = 1;
  A->x = &dummy;

  SparseMatrix sm;
  SparseComplexMatrix scm;

  if (args(0).isreal ())
    {
      sm = args(0).sparse_matrix_value ();
      A->nrow = sm.rows ();
      A->ncol = sm.cols ();
      A->p = sm.cidx ();
      A->i = sm.ridx ();
      A->nzmax = sm.nnz ();
      A->xtype = CHOLMOD_REAL;

      if (A->nrow > 0 && A->ncol > 0)
        A->x = sm.data ();
    }
  else if (args(0).iscomplex ())
    {
      scm = args(0).sparse_complex_matrix_value ();
      A->nrow = scm.rows ();
      A->ncol = scm.cols ();
      A->p = scm.cidx ();
      A->i = scm.ridx ();
      A->nzmax = scm.nnz ();
      A->xtype = CHOLMOD_COMPLEX;

      if (A->nrow > 0 && A->ncol > 0)
        A->x = scm.data ();
    }
  else
    err_wrong_type_arg ("symbfact", args(0));

  bool coletree = false;
  octave_idx_type n = A->nrow;

  if (nargin > 1)
    {
      std::string str = args(1).xstring_value ("TYP must be a string");
      // FIXME: The input validation could be improved to use strncmp
      char ch;
      ch = tolower (str[0]);
      if (ch == 'r')          // 'row'
        A->stype = 0;
      else if (ch == 'c')     // 'col'
        {
          n = A->ncol;
          coletree = true;
          A->stype = 0;
        }
      else if (ch == 's')     // 'sym' (default)
        A->stype = 1;
      else if (ch == 'l')     // 'lo'
        A->stype = -1;
      else
        error (R"(symbfact: unrecognized TYP "%s")", str.c_str ());
    }

  if (nargin == 3)
    {
      std::string str = args(2).xstring_value ("MODE must be a string");
      // FIXME: The input validation could be improved to use strncmp
      char ch;
      ch = toupper (str[0]);
      if (ch != 'L')
        error (R"(symbfact: unrecognized MODE "%s")", str.c_str ());
    }

  if (A->stype && A->nrow != A->ncol)
    err_square_matrix_required ("symbfact", "S");

  OCTAVE_LOCAL_BUFFER (suitesparse_integer, Parent, n);
  OCTAVE_LOCAL_BUFFER (suitesparse_integer, Post, n);
  OCTAVE_LOCAL_BUFFER (suitesparse_integer, ColCount, n);
  OCTAVE_LOCAL_BUFFER (suitesparse_integer, First, n);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, Level, n);

  cholmod_common Common;
  cholmod_common *cm = &Common;
  CHOLMOD_NAME(start) (cm);

  double spu = sparse_params::get_key ("spumoni");
  if (spu == 0.0)
    {
      cm->print = -1;
      SUITESPARSE_ASSIGN_FPTR (printf_func, cm->print_function, nullptr);
    }
  else
    {
      cm->print = static_cast<int> (spu) + 2;
      SUITESPARSE_ASSIGN_FPTR (printf_func, cm->print_function, &SparseCholPrint);
    }

  cm->error_handler = &SparseCholError;
  SUITESPARSE_ASSIGN_FPTR2 (divcomplex_func, cm->complex_divide, divcomplex);
  SUITESPARSE_ASSIGN_FPTR2 (hypot_func, cm->hypotenuse, hypot);

  cholmod_sparse *F = CHOLMOD_NAME(transpose) (A, 0, cm);
  cholmod_sparse *Aup, *Alo;

  if (A->stype == 1 || coletree)
    {
      Aup = A;
      Alo = F;
    }
  else
    {
      Aup = F;
      Alo = A;
    }

  CHOLMOD_NAME(etree) (Aup, Parent, cm);

  ColumnVector tmp (n);    // Declaration must precede any goto cleanup.
  std::string err_msg;

  if (cm->status < CHOLMOD_OK)
    {
      err_msg = "symbfact: matrix corrupted";
      goto cleanup;
    }

  if (CHOLMOD_NAME(postorder) (Parent, n, nullptr, Post, cm) != n)
    {
      err_msg = "symbfact: postorder failed";
      goto cleanup;
    }

  CHOLMOD_NAME(rowcolcounts) (Alo, nullptr, 0, Parent, Post, nullptr, ColCount,
                              First, to_suitesparse_intptr (Level), cm);

  if (cm->status < CHOLMOD_OK)
    {
      err_msg = "symbfact: matrix corrupted";
      goto cleanup;
    }

  if (nargout > 4)
    {
      cholmod_sparse *A1, *A2;

      if (A->stype == 1)
        {
          A1 = A;
          A2 = nullptr;
        }
      else if (A->stype == -1)
        {
          A1 = F;
          A2 = nullptr;
        }
      else if (coletree)
        {
          A1 = F;
          A2 = A;
        }
      else
        {
          A1 = A;
          A2 = F;
        }

      // count the total number of entries in L
      octave_idx_type lnz = 0;
      for (octave_idx_type j = 0 ; j < n ; j++)
        lnz += ColCount[j];

      // allocate the output matrix L (pattern-only)
      SparseBoolMatrix L (dim_vector (n, n), lnz);

      // initialize column pointers
      lnz = 0;
      for (octave_idx_type j = 0 ; j < n ; j++)
        {
          L.xcidx(j) = lnz;
          lnz += ColCount[j];
        }
      L.xcidx(n) = lnz;

      // create a copy of the column pointers
      suitesparse_integer *W = First;
      for (octave_idx_type j = 0 ; j < n ; j++)
        W[j] = L.xcidx (j);

      // get workspace for computing one row of L
      cholmod_sparse *R
        = CHOLMOD_NAME(allocate_sparse) (n, 1, n, false, true,
                                         0, CHOLMOD_PATTERN, cm);
      octave_idx_type *Rp = static_cast<octave_idx_type *> (R->p);
      octave_idx_type *Ri = static_cast<octave_idx_type *> (R->i);

      // compute L one row at a time
      for (octave_idx_type k = 0 ; k < n ; k++)
        {
          // get the kth row of L and store in the columns of L
          CHOLMOD_NAME(row_subtree) (A1, A2, k, Parent, R, cm);
          for (octave_idx_type p = 0 ; p < Rp[1] ; p++)
            L.xridx (W[Ri[p]]++) = k;

          // add the diagonal entry
          L.xridx (W[k]++) = k;
        }

      // free workspace
      CHOLMOD_NAME(free_sparse) (&R, cm);

      // fill L with one's
      std::fill_n (L.xdata (), lnz, true);

      // transpose L to get R, or leave as is
      if (nargin < 3)
        L = L.transpose ();

      retval(4) = L;
    }

  if (nargout > 3)
    {
      for (octave_idx_type i = 0; i < n; i++)
        tmp(i) = Post[i] + 1;
      retval(3) = tmp;
    }

  if (nargout > 2)
    {
      for (octave_idx_type i = 0; i < n; i++)
        tmp(i) = Parent[i] + 1;
      retval(2) = tmp;
    }

  if (nargout > 1)
    {
      // compute the elimination tree height
      octave_idx_type height = 0;
      for (int i = 0 ; i < n ; i++)
        height = std::max (height, Level[i]);
      height++;
      retval(1) = static_cast<double> (height);
    }

  for (octave_idx_type i = 0; i < n; i++)
    tmp(i) = ColCount[i];
  retval(0) = tmp;

cleanup:
  CHOLMOD_NAME(free_sparse) (&F, cm);
  CHOLMOD_NAME(finish) (cm);

  if (! err_msg.empty ())
    error ("%s", err_msg.c_str ());

  return retval;

#else

  octave_unused_parameter (args);
  octave_unused_parameter (nargout);

  err_disabled_feature ("symbfact", "CHOLMOD");

#endif
}

/*
%!testif HAVE_CHOLMOD
%! A = sparse (magic (3));
%! [count, h, parent, post, r] = symbfact (A);
%! assert (count, [3; 2; 1]);
%! assert (h, 3);
%! assert (parent, [2; 3; 0]);
%! assert (r, sparse (triu (true (3))));

%!testif HAVE_CHOLMOD
%! ## Test MODE "lower"
%! A = sparse (magic (3));
%! [~, ~, ~, ~, l] = symbfact (A, "sym", "lower");
%! assert (l, sparse (tril (true (3))));

%!testif HAVE_CHOLMOD <*42587>
%! ## singular matrix
%! A = sparse ([1 0 8;0 1 8;8 8 1]);
%! [count, h, parent, post, r] = symbfact (A);

## Test input validation
%!testif HAVE_CHOLMOD
%! fail ("symbfact ()");
%! fail ("symbfact (1,2,3,4)");
%! fail ("symbfact ({1})", "wrong type argument 'cell'");
%! fail ("symbfact (sparse (1), {1})", "TYP must be a string");
%! fail ("symbfact (sparse (1), 'foobar')", 'unrecognized TYP "foobar"');
%! fail ("symbfact (sparse (1), 'sym', {'L'})", "MODE must be a string");
%! fail ('symbfact (sparse (1), "sym", "foobar")',
%!       'unrecognized MODE "foobar"');
%! fail ("symbfact (sparse ([1, 2; 3, 4; 5, 6]))", "S must be a square matrix");

*/

OCTAVE_END_NAMESPACE(octave)
