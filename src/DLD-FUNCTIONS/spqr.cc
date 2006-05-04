/*

Copyright (C) 2005 David Bateman
Copyright (C) 1998-2005 Andy Adler

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

#include "oct-sparse.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"
#include "SparseQR.h"
#include "SparseCmplxQR.h"

#ifdef IDX_TYPE_LONG
#define CXSPARSE_NAME(name) cs_dl ## name
#else
#define CXSPARSE_NAME(name) cs_di ## name
#endif

// PKG_ADD: dispatch ("qr", "spqr", "sparse matrix");
// PKG_ADD: dispatch ("qr", "spqr", "sparse complex matrix");
// PKG_ADD: dispatch ("qr", "spqr", "sparse bool matrix");
DEFUN_DLD (spqr, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{r} =} spqr (@var{a})\n\
@deftypefnx {Loadable Function} {@var{r} =} spqr (@var{a},0)\n\
@deftypefnx {Loadable Function} {[@var{c}, @var{r}] =} spqr (@var{a},@var{b})\n\
@deftypefnx {Loadable Function} {[@var{c}, @var{r}] =} spqr (@var{a},@var{b},0)\n\
@cindex QR factorization\n\
Compute the sparse QR factorization of @var{a}, using @sc{CSparse}.\n\
As the matrix @var{Q} is in general a full matrix, this function returns\n\
the @var{Q}-less factorization @var{r} of @var{a}, such that\n\
@code{@var{r} = chol (@var{a}' * @var{a})}.\n\
\n\
If the final argument is the scalar @code{0} and the number of rows is\n\
larger than the number of columns, then an economy factorization is\n\
returned. That is @var{r} will have only @code{size (@var{a},1)} rows.\n\
\n\
If an additional matrix @var{b} is supplied, then @code{spqr} returns\n\
@var{c}, where @code{@var{c} = @var{q}' * @var{b}}. This allows the\n\
least squares approximation of @code{@var{a} \\ @var{b}} to be calculated\n\
as\n\
\n\
@example\n\
[@var{c},@var{r}] = spqr (@var{a},@var{b})\n\
@var{x} = @var{r} \\ @var{c}\n\
@end example\n\
@seealso{spchol, qr}\n\
@end deftypefn")
{
  int nargin = args.length ();
  octave_value_list retval;
  bool economy = false;
  bool is_cmplx = false;
  bool have_b = false;

  if (nargin < 1 || nargin > 3)
    print_usage ("spqr");
  else
    {
      if (args(0).is_complex_type ())
	is_cmplx = true;
      if (nargin > 1)
	{
	  have_b = true;
	  if (args(nargin-1).is_scalar_type ())
	    {
	      int val = args(nargin-1).int_value ();
	      if (val == 0)
		{
		  economy = true;
		  have_b = (nargin > 2);
		}
	    }
	  if (have_b && args(1).is_complex_type ())
	    is_cmplx = true;
	}
	
      if (!error_state)
	{
	  if (have_b && nargout < 2)
	    error ("spqr: incorrect number of output arguments");
	  else if (is_cmplx)
	    {
	      SparseComplexQR q (args(0).sparse_complex_matrix_value ());
	      if (!error_state)
		{
		  if (have_b)
		    {
		      retval(1) = q.R (economy);
		      retval(0) = q.C (args(1).complex_matrix_value ());
		      if (args(0).rows() < args(0).columns())
			warning ("spqr: non minimum norm solution for under-determined problem");
		    }
		  else
		    retval(0) = q.R (economy);
		}
	    }
	  else
	    {
	      SparseQR q (args(0).sparse_matrix_value ());
	      if (!error_state)
		{
		  if (have_b)
		    {
		      retval(1) = q.R (economy);
		      retval(0) = q.C (args(1).matrix_value ());
		      if (args(0).rows() < args(0).columns())
			warning ("spqr: non minimum norm solution for under-determined problem");
		    }
		  else
		    retval(0) = q.R (economy);
		}
	    }
	}
    }
  return retval;
}

/*

The deactivated tests below can't be tested till rectangular back-subs is
implemented for sparse matrices.

%!test
%! n = 20; d= 0.2;
%! a = sprandn(n,n,d)+speye(n,n);
%! r = spqr(a);
%! assert(r'*r,a'*a,1e-10)

%!test
%! n = 20; d= 0.2;
%! a = sprandn(n,n,d)+speye(n,n);
%! q = symamd(a);
%! a = a(q,q);
%! r = spqr(a);
%! assert(r'*r,a'*a,1e-10)

%!test
%! n = 20; d= 0.2;
%! a = sprandn(n,n,d)+speye(n,n);
%! [c,r] = spqr(a,ones(n,1));
%! assert (r\c,full(a)\ones(n,1),10e-10)

%!test
%! n = 20; d= 0.2;
%! a = sprandn(n,n,d)+speye(n,n);
%! b = randn(n,2);
%! [c,r] = spqr(a,b);
%! assert (r\c,full(a)\b,10e-10)

%% Test under-determined systems!!
%!#test
%! n = 20; d= 0.2;
%! a = sprandn(n,n+1,d)+speye(n,n+1);
%! b = randn(n,2);
%! [c,r] = spqr(a,b);
%! assert (r\c,full(a)\b,10e-10)

%!test
%! n = 20; d= 0.2;
%! a = 1i*sprandn(n,n,d)+speye(n,n);
%! r = spqr(a);
%! assert(r'*r,a'*a,1e-10)

%!test
%! n = 20; d= 0.2;
%! a = 1i*sprandn(n,n,d)+speye(n,n);
%! q = symamd(a);
%! a = a(q,q);
%! r = spqr(a);
%! assert(r'*r,a'*a,1e-10)

%!test
%! n = 20; d= 0.2;
%! a = 1i*sprandn(n,n,d)+speye(n,n);
%! [c,r] = spqr(a,ones(n,1));
%! assert (r\c,full(a)\ones(n,1),10e-10)

%!test
%! n = 20; d= 0.2;
%! a = 1i*sprandn(n,n,d)+speye(n,n);
%! b = randn(n,2);
%! [c,r] = spqr(a,b);
%! assert (r\c,full(a)\b,10e-10)

%% Test under-determined systems!!
%!#test
%! n = 20; d= 0.2;
%! a = 1i*sprandn(n,n+1,d)+speye(n,n+1);
%! b = randn(n,2);
%! [c,r] = spqr(a,b);
%! assert (r\c,full(a)\b,10e-10)

%!error spqr(sprandn(10,10,0.2),ones(10,1));

*/

static RowVector
put_int (octave_idx_type *p, octave_idx_type n)
{
  RowVector ret (n);
  for (octave_idx_type i = 0; i < n; i++)
    ret.xelem(i) = p[i] + 1;
  return ret;
}

DEFUN_DLD (dmperm, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{p} =} dmperm (@var{s})\n\
@deftypefnx {Loadable Function} {[@var{p}, @var{q}, @var{r}, @var{s}] =} dmperm (@var{s})\n\
\n\
@cindex Dulmage-Mendelsohn decomposition\n\
Perform a Deulmage-Mendelsohn permutation on the sparse matrix @var{s}.\n\
With a single output argument @dfn{dmperm} performs the row permutations\n\
@var{p} such that @code{@var{s} (@var{p},:)} has no zero elements on the\n\
diagonal.\n\
\n\
Called with two or more output arguments, returns the row and column\n\
permutations, such that @code{@var{s} (@var{p}, @var{q})} is in block\n\
triangular form. The values of @var{r} and @var{s} define the boundaries\n\
of the blocks. If @var{s} is square then @code{@var{r} == @var{s}}.\n\
\n\
The method used is described in: A. Pothen & C.-J. Fan. Computing the block\n\
triangular form of a sparse matrix. ACM Trans. Math. Software,\n\
16(4):303-324, 1990.\n\
@seealso{colamd, ccolamd}\n\
@end deftypefn")
{
  int nargin = args.length();
  octave_value_list retval;
  
#if HAVE_CXSPARSE
  if (nargin != 1)
    {
      print_usage ("dmperm");
      return retval;
    }

  octave_value arg = args(0);
  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();
  SparseMatrix m;
  SparseComplexMatrix cm;
  CXSPARSE_NAME () csm;
  csm.m = nr;
  csm.n = nc;
  csm.x = NULL;
  csm.nz = -1;

  if (arg.is_real_type ())
    {
      m = arg.sparse_matrix_value ();
      csm.nzmax = m.nnz();
      csm.p = m.xcidx ();
      csm.i = m.xridx ();
    }
  else
    {
      cm = arg.sparse_complex_matrix_value ();
      csm.nzmax = cm.nnz();
      csm.p = cm.xcidx ();
      csm.i = cm.xridx ();
    }

  if (!error_state)
    {
      if (nargout <= 1)
	{
#if defined(CS_VER) && (CS_VER >= 2)
	  octave_idx_type *jmatch = CXSPARSE_NAME (_maxtrans) (&csm, 0);
#else
	  octave_idx_type *jmatch = CXSPARSE_NAME (_maxtrans) (&csm);
#endif
	  retval(0) = put_int (jmatch + nr, nc);
	  CXSPARSE_NAME (_free) (jmatch);
	}
      else
	{
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_NAME (d) *dm = CXSPARSE_NAME(_dmperm) (&csm, 0);
#else
	  CXSPARSE_NAME (d) *dm = CXSPARSE_NAME(_dmperm) (&csm);
#endif
	  //retval(5) = put_int (dm->rr, 5);
	  //retval(4) = put_int (dm->cc, 5);
#if defined(CS_VER) && (CS_VER >= 2)
	  retval(3) = put_int (dm->s, dm->nb+1);
	  retval(2) = put_int (dm->r, dm->nb+1);
	  retval(1) = put_int (dm->q, nc);
	  retval(0) = put_int (dm->p, nr);
#else
	  retval(3) = put_int (dm->S, dm->nb+1);
	  retval(2) = put_int (dm->R, dm->nb+1);
	  retval(1) = put_int (dm->Q, nc);
	  retval(0) = put_int (dm->P, nr);
#endif
	  CXSPARSE_NAME (_dfree) (dm);
	}
    }
#else
  error ("dmperm: not available in this version of Octave");
#endif

  return retval;
}

/*

%!test
%! n=20;
%! a=speye(n,n);a=a(randperm(n),:);
%! assert(a(dmperm(a),:),speye(n))

%!test
%! n=20;
%! d=0.2;
%! a=tril(sprandn(n,n,d),-1)+speye(n,n);
%! a=a(randperm(n),randperm(n));
%! [p,q,r,s]=dmperm(a);
%! assert(tril(a(p,q),-1),sparse(n,n))

*/

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
