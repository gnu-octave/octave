/*

Copyright (C) 2005-2018 David Bateman
Copyright (C) 1998-2005 Andy Adler

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "CSparse.h"
#include "dRowVector.h"
#include "dSparse.h"
#include "oct-sparse.h"

#include "defun-dld.h"
#include "errwarn.h"
#include "ov.h"
#include "ovl.h"
#include "utils.h"

#if defined (OCTAVE_ENABLE_64)
#  define CXSPARSE_NAME(name) cs_dl ## name
#else
#  define CXSPARSE_NAME(name) cs_di ## name
#endif

#if defined (HAVE_CXSPARSE)

static RowVector
put_int (octave::suitesparse_integer *p, octave_idx_type n)
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
      csm.p = octave::to_suitesparse_intptr (m.xcidx ());
      csm.i = octave::to_suitesparse_intptr (m.xridx ());
    }
  else
    {
      cm = arg.sparse_complex_matrix_value ();
      csm.nzmax = cm.nnz ();
      csm.p = octave::to_suitesparse_intptr (cm.xcidx ());
      csm.i = octave::to_suitesparse_intptr (cm.xridx ());
    }

  if (nargout <= 1 || rank)
    {
      octave::suitesparse_integer *jmatch = CXSPARSE_NAME (_maxtrans) (&csm, 0);
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

      //retval(5) = put_int (dm->rr, 5);
      //retval(4) = put_int (dm->cc, 5);
      retval = ovl (put_int (dm->p, nr), put_int (dm->q, nc),
                    put_int (dm->r, dm->nb+1), put_int (dm->s, dm->nb+1));

      CXSPARSE_NAME (_dfree) (dm);
    }

  return retval;
}

#endif

DEFUN_DLD (dmperm, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{p} =} dmperm (@var{S})
@deftypefnx {} {[@var{p}, @var{q}, @var{r}, @var{S}] =} dmperm (@var{S})

@cindex @nospell{Dulmage-Mendelsohn} decomposition
Perform a @nospell{Dulmage-Mendelsohn} permutation of the sparse matrix
@var{S}.

With a single output argument @code{dmperm} performs the row permutations
@var{p} such that @code{@var{S}(@var{p},:)} has no zero elements on the
diagonal.

Called with two or more output arguments, returns the row and column
permutations, such that @code{@var{S}(@var{p}, @var{q})} is in block
triangular form.  The values of @var{r} and @var{S} define the boundaries
of the blocks.  If @var{S} is square then @code{@var{r} == @var{S}}.

The method used is described in: @nospell{A. Pothen & C.-J. Fan.}
@cite{Computing the Block Triangular Form of a Sparse Matrix}.
@nospell{ACM} Trans. Math. Software, 16(4):303-324, 1990.
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

DEFUN_DLD (sprank, args, nargout,
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
