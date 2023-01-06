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

#include "CSparse.h"
#include "dRowVector.h"
#include "dSparse.h"
#include "oct-sparse.h"

#include "defun.h"
#include "errwarn.h"
#include "ov.h"
#include "ovl.h"
#include "utils.h"

OCTAVE_BEGIN_NAMESPACE(octave)

#if defined (OCTAVE_ENABLE_64)
#  define CXSPARSE_NAME(name) cs_dl ## name
#else
#  define CXSPARSE_NAME(name) cs_di ## name
#endif

#if defined (HAVE_CXSPARSE)

static RowVector
put_int (suitesparse_integer *p, octave_idx_type n)
{
  RowVector ret (n);
  for (octave_idx_type i = 0; i < n; i++)
    ret.xelem (i) = p[i] + 1;
  return ret;
}

static octave_value_list
dmperm_internal (bool rank, const octave_value arg, int nargout)
{
  octave_value_list retval;
  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();
  SparseMatrix m;
  SparseComplexMatrix cm;
  CXSPARSE_NAME () csm;
  csm.m = nr;
  csm.n = nc;
  csm.x = nullptr;
  csm.nz = -1;

  if (arg.isreal ())
    {
      m = arg.sparse_matrix_value ();
      csm.nzmax = m.nnz ();
      csm.p = to_suitesparse_intptr (m.xcidx ());
      csm.i = to_suitesparse_intptr (m.xridx ());
    }
  else
    {
      cm = arg.sparse_complex_matrix_value ();
      csm.nzmax = cm.nnz ();
      csm.p = to_suitesparse_intptr (cm.xcidx ());
      csm.i = to_suitesparse_intptr (cm.xridx ());
    }

  if (nargout <= 1 || rank)
    {
      suitesparse_integer *jmatch = CXSPARSE_NAME (_maxtrans) (&csm, 0);
      if (rank)
        {
          octave_idx_type r = 0;
          for (octave_idx_type i = 0; i < nc; i++)
            if (jmatch[nr+i] >= 0)
              r++;
          retval(0) = static_cast<double> (r);
        }
      else
        retval(0) = put_int (jmatch + nr, nc);
      CXSPARSE_NAME (_free) (jmatch);
    }
  else
    {
      CXSPARSE_NAME (d) *dm = CXSPARSE_NAME(_dmperm) (&csm, 0);

      retval = ovl (put_int (dm->p, nr), put_int (dm->q, nc),
                    put_int (dm->r, dm->nb+1), put_int (dm->s, dm->nb+1),
                    put_int (dm->cc, 5), put_int (dm->rr, 5));

      CXSPARSE_NAME (_dfree) (dm);
    }

  return retval;
}

#endif

// NOTE: the docstring for dmperm is adapted from the text found in the
// file cs_dmperm.m that is distributed with the CSparse portion of the
// SuiteSparse library, version 5.6.0.  CSparse is distributed under the
// terms of the LGPL v2.1 or any later version.

DEFUN (dmperm, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{p} =} dmperm (@var{A})
@deftypefnx {} {[@var{p}, @var{q}, @var{r}, @var{s}, @var{cc}, @var{rr}] =} dmperm (@var{A})

@cindex @nospell{Dulmage-Mendelsohn} decomposition
Perform a @nospell{Dulmage-Mendelsohn} permutation of the sparse matrix
@var{A}.

With a single output argument @code{dmperm}, return a maximum matching @var{p}
such that @code{p(j) = i} if column @var{j} is matched to row @var{i}, or 0 if
column @var{j} is unmatched.  If @var{A} is square and full structural rank,
@var{p} is a row permutation and @code{A(p,:)} has a zero-free diagonal.  The
structural rank of @var{A} is @code{sprank(A) = sum(p>0)}.

Called with two or more output arguments, return the
@nospell{Dulmage-Mendelsohn} decomposition of @var{A}.  @var{p} and @var{q} are
permutation vectors.  @var{cc} and @var{rr} are vectors of length 5.
@code{c = A(p,q)} is split into a 4-by-4 set of coarse blocks:

@example
@group
   A11 A12 A13 A14
    0  0   A23 A24
    0  0    0  A34
    0  0    0  A44
@end group
@end example

@noindent
where @code{A12}, @code{A23}, and @code{A34} are square with zero-free
diagonals.  The columns of @code{A11} are the unmatched columns, and the rows
of @code{A44} are the unmatched rows.  Any of these blocks can be empty.  In
the "coarse" decomposition, the (i,j)-th block is
@code{C(rr(i):rr(i+1)-1,cc(j):cc(j+1)-1)}.  In terms of a linear system,
@code{[A11 A12]} is the underdetermined part of the system (it is always
rectangular and with more columns and rows, or 0-by-0), @code{A23} is the
well-determined part of the system (it is always square), and
@code{[A34 ; A44]} is the over-determined part of the system (it is always
rectangular with more rows than columns, or 0-by-0).

The structural rank of @var{A} is @code{sprank (A) = rr(4)-1}, which is an
upper bound on the numerical rank of @var{A}.
@code{sprank(A) = rank(full(sprand(A)))} with probability 1 in exact
arithmetic.

The @code{A23} submatrix is further subdivided into block upper triangular form
via the "fine" decomposition (the strongly-connected components of @code{A23}).
If @var{A} is square and structurally non-singular, @code{A23} is the entire
matrix.

@code{C(r(i):r(i+1)-1,s(j):s(j+1)-1)} is the (i,j)-th block of the fine
decomposition.  The (1,1) block is the rectangular block @code{[A11 A12]},
unless this block is 0-by-0.  The (b,b) block is the rectangular block
@code{[A34 ; A44]}, unless this block is 0-by-0, where @code{b = length(r)-1}.
All other blocks of the form @code{C(r(i):r(i+1)-1,s(i):s(i+1)-1)} are diagonal
blocks of @code{A23}, and are square with a zero-free diagonal.

The method used is described in: @nospell{A. Pothen & C.-J. Fan.}
@cite{Computing the Block Triangular Form of a Sparse Matrix}.
@nospell{ACM} Trans.@: Math.@: Software, 16(4):303--324, 1990.
@seealso{colamd, ccolamd}
@end deftypefn */)
{
#if defined (HAVE_CXSPARSE)

  if (args.length () != 1)
    print_usage ();

  return dmperm_internal (false, args(0), nargout);

#else

  octave_unused_parameter (args);
  octave_unused_parameter (nargout);

  err_disabled_feature ("dmperm", "CXSparse");

#endif
}

/*
%!testif HAVE_CXSPARSE
%! n = 20;
%! a = speye (n,n);
%! a = a(randperm (n),:);
%! assert (a(dmperm (a),:), speye (n));

%!testif HAVE_CXSPARSE
%! n = 20;
%! d = 0.2;
%! a = tril (sprandn (n,n,d), -1) + speye (n,n);
%! a = a(randperm (n), randperm (n));
%! [p,q,r,s] = dmperm (a);
%! assert (tril (a(p,q), -1), sparse (n, n));
*/

DEFUN (sprank, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{p} =} sprank (@var{S})
@cindex structural rank

Calculate the structural rank of the sparse matrix @var{S}.

Note that only the structure of the matrix is used in this calculation based
on a @nospell{Dulmage-Mendelsohn} permutation to block triangular form.  As
such the numerical rank of the matrix @var{S} is bounded by
@code{sprank (@var{S}) >= rank (@var{S})}.  Ignoring floating point errors
@code{sprank (@var{S}) == rank (@var{S})}.
@seealso{dmperm}
@end deftypefn */)
{
#if defined (HAVE_CXSPARSE)

  if (args.length () != 1)
    print_usage ();

  return dmperm_internal (true, args(0), nargout);

#else

  octave_unused_parameter (args);
  octave_unused_parameter (nargout);

  err_disabled_feature ("sprank", "CXSparse");

#endif
}

/*
%!testif HAVE_CXSPARSE
%! assert (sprank (speye (20)), 20);
%!testif HAVE_CXSPARSE
%! assert (sprank ([1,0,2,0;2,0,4,0]), 2);

%!error sprank (1,2)
*/

OCTAVE_END_NAMESPACE(octave)
