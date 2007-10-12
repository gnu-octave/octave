/*

Copyright (C) 2005 David Bateman
Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005 Andy Adler

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

#include "SparseCmplxCHOL.h"
#include "SparsedbleCHOL.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"
#include "oct-spparms.h"
#include "sparse-util.h"

static octave_value_list
sparse_chol (const octave_value_list& args, const int nargout, 
	     const std::string& name, const bool LLt)
{
  octave_value_list retval;
  int nargin = args.length ();

  if (nargin != 1 || nargout > 3)
    {
      print_usage ();
      return retval;
    }

  octave_value arg = args(0);
    
  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();
  bool natural = (nargout != 3);

  int arg_is_empty = empty_arg (name.c_str(), nr, nc);

  if (arg_is_empty < 0)
    return retval;
  if (arg_is_empty > 0)
    return octave_value (Matrix ());

  if (arg.is_real_type ())
    {
      SparseMatrix m = arg.sparse_matrix_value ();

      if (! error_state)
	{
	  octave_idx_type info;
	  SparseCHOL fact (m, info, natural);
	  if (nargout == 3)
	    retval(2) = fact.Q();

	  if (nargout > 1 || info == 0)
	    {
	      retval(1) = fact.P();
	      if (LLt)
		retval(0) = fact.L();
	      else
		retval(0) = fact.R();
	    }
	  else
	    error ("%s: matrix not positive definite", name.c_str());
	}
    }
  else if (arg.is_complex_type ())
    {
      SparseComplexMatrix m = arg.sparse_complex_matrix_value ();

      if (! error_state)
	{
	  octave_idx_type info;
	  SparseComplexCHOL fact (m, info, natural);

	  if (nargout == 3)
	    retval(2) = fact.Q();
	  
	  if (nargout > 1 || info == 0)
	    {
	      retval(1) = fact.P();
	      if (LLt)
		retval(0) = fact.L();
	      else
		retval(0) = fact.R();
	    }
	  else
	    error ("%s: matrix not positive definite", name.c_str());
	}
    }
  else
    gripe_wrong_type_arg (name.c_str(), arg);

  return retval;
}

// PKG_ADD: dispatch ("chol", "spchol", "sparse matrix");
// PKG_ADD: dispatch ("chol", "spchol", "sparse complex matrix");
// PKG_ADD: dispatch ("chol", "spchol", "sparse bool matrix");
DEFUN_DLD (spchol, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{r} =} spchol (@var{a})\n\
@deftypefnx {Loadable Function} {[@var{r}, @var{p}] =} spchol (@var{a})\n\
@deftypefnx {Loadable Function} {[@var{r}, @var{p}, @var{q}] =} spchol (@var{a})\n\
@cindex Cholesky factorization\n\
Compute the Cholesky factor, @var{r}, of the symmetric positive definite\n\
sparse matrix @var{a}, where\n\
@iftex\n\
@tex\n\
$ R^T R = A $.\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
\n\
@example\n\
r' * r = a.\n\
@end example\n\
@end ifinfo\n\
\n\
If called with 2 or more outputs @var{p} is the 0 when @var{r} is positive\n\
definite and @var{p} is a positive integer otherwise.\n\
\n\
If called with 3 outputs then a sparsity preserving row/column permutation\n\
is applied to @var{a} prior to the factorization. That is @var{r}\n\
is the factorization of @code{@var{a}(@var{q},@var{q})} such that\n\
@iftex\n\
@tex\n\
$ R^T R = Q A Q^T$.\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
\n\
@example\n\
r' * r = q * a * q'.\n\
@end example\n\
@end ifinfo\n\
\n\
Note that @code{splchol} factorization is faster and uses less memory.\n\
@seealso{spcholinv, spchol2inv, splchol}\n\
@end deftypefn")
{
  return sparse_chol (args, nargout, "spchol", false);
}

// PKG_ADD: dispatch ("lchol", "splchol", "sparse matrix");
// PKG_ADD: dispatch ("lchol", "splchol", "sparse complex matrix");
// PKG_ADD: dispatch ("lchol", "splchol", "sparse bool matrix");
DEFUN_DLD (splchol, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{l} =} splchol (@var{a})\n\
@deftypefnx {Loadable Function} {[@var{l}, @var{p}] =} splchol (@var{a})\n\
@deftypefnx {Loadable Function} {[@var{l}, @var{p}, @var{q}] =} splchol (@var{a})\n\
@cindex Cholesky factorization\n\
Compute the Cholesky factor, @var{l}, of the symmetric positive definite\n\
sparse matrix @var{a}, where\n\
@iftex\n\
@tex\n\
$ L L^T = A $.\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
\n\
@example\n\
l * l' = a.\n\
@end example\n\
@end ifinfo\n\
\n\
If called with 2 or more outputs @var{p} is the 0 when @var{l} is positive\n\
definite and @var{l} is a positive integer otherwise.\n\
\n\
If called with 3 outputs that a sparsity preserving row/column permutation\n\
is applied to @var{a} prior to the factorization. That is @var{l}\n\
is the factorization of @code{@var{a}(@var{q},@var{q})} such that\n\
@iftex\n\
@tex\n\
$ L R^T = A (Q, Q)$.\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
\n\
@example\n\
r * r' = a (q, q).\n\
@end example\n\
@end ifinfo\n\
\n\
Note that @code{splchol} factorization is faster and uses less memory\n\
than @code{spchol}. @code{splchol(@var{a})} is equivalent to\n\
@code{spchol(@var{a})'}.\n\
@seealso{spcholinv, spchol2inv, splchol}\n\
@end deftypefn")
{
  return sparse_chol (args, nargout, "splchol", true);
}

// PKG_ADD: dispatch ("cholinv", "spcholinv", "sparse matrix");
// PKG_ADD: dispatch ("cholinv", "spcholinv", "sparse complex matrix");
// PKG_ADD: dispatch ("cholinv", "spcholinv", "sparse bool matrix");
DEFUN_DLD (spcholinv, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} spcholinv (@var{a})\n\
Use the Cholesky factorization to compute the inverse of the\n\
sparse symmetric positive definite matrix @var{a}.\n\
@seealso{spchol, spchol2inv}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_value arg = args(0);
    
      octave_idx_type nr = arg.rows ();
      octave_idx_type nc = arg.columns ();

      if (nr == 0 || nc == 0)
	retval = Matrix ();
      else
	{
	  if (arg.is_real_type ())
	    {
	      SparseMatrix m = arg.sparse_matrix_value ();

	      if (! error_state)
		{
		  octave_idx_type info;
		  SparseCHOL chol (m, info);
		  if (info == 0)
		    retval = chol.inverse ();
		  else
		    error ("spcholinv: matrix not positive definite");
		}
	    }
	  else if (arg.is_complex_type ())
	    {
	      SparseComplexMatrix m = arg.sparse_complex_matrix_value ();

	      if (! error_state)
		{
		  octave_idx_type info;
		  SparseComplexCHOL chol (m, info);
		  if (info == 0)
		    retval = chol.inverse ();
		  else
		    error ("spcholinv: matrix not positive definite");
		}
	    }
	  else
	    gripe_wrong_type_arg ("spcholinv", arg);
	}
    }
  else
    print_usage ();

  return retval;
}

// PKG_ADD: dispatch ("chol2inv", "spchol2inv", "sparse matrix");
// PKG_ADD: dispatch ("chol2inv", "spchol2inv", "sparse complex matrix");
// PKG_ADD: dispatch ("chol2inv", "spchol2inv", "sparse bool matrix");
DEFUN_DLD (spchol2inv, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} spchol2inv (@var{u})\n\
Invert a sparse symmetric, positive definite square matrix from its\n\
Cholesky decomposition, @var{u}.  Note that @var{u} should be an\n\
upper-triangular matrix with positive diagonal elements.\n\
@code{chol2inv (@var{u})} provides @code{inv (@var{u}'*@var{u})} but\n\
it is much faster than using @code{inv}.\n\
@seealso{spchol, spcholinv}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_value arg = args(0);
    
      octave_idx_type nr = arg.rows ();
      octave_idx_type nc = arg.columns ();

      if (nr == 0 || nc == 0)
	retval = Matrix ();
      else
	{
	  if (arg.is_real_type ())
	    {
	      SparseMatrix r = arg.sparse_matrix_value ();

	      if (! error_state)
		retval = chol2inv (r);
	    }
	  else if (arg.is_complex_type ())
	    {
	      SparseComplexMatrix r = arg.sparse_complex_matrix_value ();

	      if (! error_state)
		retval = chol2inv (r);
	    }
	  else
	    gripe_wrong_type_arg ("spchol2inv", arg);
	}
    }
  else
    print_usage ();

  return retval;
}

DEFUN_DLD (symbfact, args, nargout,
    "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{count}, @var{h}, @var{parent}, @var{post}, @var{r}] =} symbfact (@var{s}, @var{typ}, @var{mode})\n\
\n\
Performs a symbolic factorization analysis on the sparse matrix @var{s}.\n\
Where\n\
\n\
@table @asis\n\
@item @var{s}\n\
@var{s} is a complex or real sparse matrix.\n\
\n\
@item @var{typ}\n\
Is the type of the factorization and can be one of\n\
\n\
@table @code\n\
@item sym\n\
Factorize @var{s}. This is the default.\n\
\n\
@item col\n\
Factorize @code{@var{s}' * @var{s}}.\n\
@item row\n\
Factorize @code{@var{s} * @var{s}'}.\n\
@item lo\n\
Factorize @code{@var{s}'}\n\
@end table\n\
\n\
@item @var{mode}\n\
The default is to return the Cholesky factorization for @var{r}, and if\n\
@var{mode} is 'L', the conjugate transpose of the Cholesky factorization\n\
is returned. The conjugate transpose version is faster and uses less\n\
memory, but returns the same values for @var{count}, @var{h}, @var{parent}\n\
and @var{post} outputs.\n\
@end table\n\
\n\
The output variables are\n\
\n\
@table @asis\n\
@item @var{count}\n\
The row counts of the Cholesky factorization as determined by @var{typ}.\n\
\n\
@item @var{h}\n\
The height of the elimination tree.\n\
\n\
@item @var{parent}\n\
The elimination tree itself.\n\
\n\
@item @var{post}\n\
A sparse boolean matrix whose structure is that of the Cholesky\n\
factorization as determined by @var{typ}.\n\
@end table\n\
@end deftypefn")
{
  octave_value_list retval;
  int nargin = args.length ();

  if (nargin < 1  || nargin > 3 || nargout > 5)
    {
      print_usage ();
      return retval;
    }

#ifdef HAVE_CHOLMOD

  cholmod_common Common;
  cholmod_common *cm = &Common;
  CHOLMOD_NAME(start) (cm);

  double spu = octave_sparse_params::get_key ("spumoni");
  if (spu == 0.)
    {
      cm->print = -1;
      cm->print_function = NULL;
    }
  else
    {
      cm->print = static_cast<int> (spu) + 2;
      cm->print_function =&SparseCholPrint;
    }

  cm->error_handler = &SparseCholError;
  cm->complex_divide = CHOLMOD_NAME(divcomplex);
  cm->hypotenuse = CHOLMOD_NAME(hypot);

#ifdef HAVE_METIS
  // METIS 4.0.1 uses malloc and free, and will terminate if it runs
  // out of memory.  Use CHOLMOD's memory guard for METIS, which
  // allocates a huge block of memory (and then immediately frees it)
  // before calling METIS.
  cm->metis_memory = 2.0;

#if defined(METIS_VERSION)
#if (METIS_VERSION >= METIS_VER(4,0,2))
  // METIS 4.0.2 uses function pointers for malloc and free.
  METIS_malloc = cm->malloc_memory;
  METIS_free   = cm->free_memory;
  // Turn off METIS memory guard.
  cm->metis_memory   = 0.0;
#endif
#endif
#endif
  
  double dummy;
  cholmod_sparse Astore;
  cholmod_sparse *A = &Astore;
  A->packed = true;
  A->sorted = true;
  A->nz = NULL;
#ifdef IDX_TYPE_LONG
  A->itype = CHOLMOD_LONG;
#else
  A->itype = CHOLMOD_INT;
#endif
  A->dtype = CHOLMOD_DOUBLE;
  A->stype = 1;
  A->x = &dummy;

  if (args(0).is_real_type ())
    {
      const SparseMatrix a = args(0).sparse_matrix_value();
      A->nrow = a.rows();
      A->ncol = a.cols();
      A->p = a.cidx();
      A->i = a.ridx();
      A->nzmax = a.nnz();
      A->xtype = CHOLMOD_REAL;

      if (a.rows() > 0 && a.cols() > 0)
	A->x = a.data();
    }
  else if (args(0).is_complex_type ())
    {
      const SparseComplexMatrix a = args(0).sparse_complex_matrix_value();
      A->nrow = a.rows();
      A->ncol = a.cols();
      A->p = a.cidx();
      A->i = a.ridx();
      A->nzmax = a.nnz();
      A->xtype = CHOLMOD_COMPLEX;

      if (a.rows() > 0 && a.cols() > 0)
	A->x = a.data();
    }
  else
    gripe_wrong_type_arg ("symbfact", arg(0));

  octave_idx_type coletree = false;
  octave_idx_type n = A->nrow;

  if (nargin > 1)
    {
      char ch;
      std::string str = args(1).string_value();
      ch = tolower (str.c_str()[0]);
      if (ch == 'r')
	A->stype = 0;
      else if (ch == 'c')
	{
	  n = A->ncol;
	  coletree = true;
	  A->stype = 0;
	}
      else if (ch == 's')
	A->stype = 1;
      else if (ch == 's')
	A->stype = -1;
      else
	error ("Unrecognized typ in symbolic factorization");
    }

  if (A->stype && A->nrow != A->ncol)
    error ("Matrix must be square");

  if (!error_state)
    {
      OCTAVE_LOCAL_BUFFER (octave_idx_type, Parent, n);
      OCTAVE_LOCAL_BUFFER (octave_idx_type, Post, n);
      OCTAVE_LOCAL_BUFFER (octave_idx_type, ColCount, n);
      OCTAVE_LOCAL_BUFFER (octave_idx_type, First, n);
      OCTAVE_LOCAL_BUFFER (octave_idx_type, Level, n);

      cholmod_sparse *F = CHOLMOD_NAME(transpose) (A, 0, cm);
      cholmod_sparse *Aup, *Alo;

      if (A->stype == 1 || coletree)
	{
	  Aup = A ;
	  Alo = F ;
	}
      else
	{
	  Aup = F ;
	  Alo = A ;
	}

      CHOLMOD_NAME(etree) (Aup, Parent, cm);

      if (cm->status < CHOLMOD_OK)
	{
	  error("matrix corrupted");
	  goto symbfact_error;
	}

      if (CHOLMOD_NAME(postorder) (Parent, n, NULL, Post, cm) != n)
	{
	  error("postorder failed");
	  goto symbfact_error;
	}

      CHOLMOD_NAME(rowcolcounts) (Alo, NULL, 0, Parent, Post, NULL,
				  ColCount, First, Level, cm);

      if (cm->status < CHOLMOD_OK)
	{
	  error("matrix corrupted");
	  goto symbfact_error;
	}

      if (nargout > 4)
	{
	  cholmod_sparse *A1, *A2;

	  if (A->stype == 1)
	    {
	      A1 = A;
	      A2 = NULL;
	    }
	  else if (A->stype == -1)
	    {
	      A1 = F;
	      A2 = NULL;
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
	  octave_idx_type lnz = 0 ;
	  for (octave_idx_type j = 0 ; j < n ; j++)
	    lnz += ColCount [j] ;
	

	  // allocate the output matrix L (pattern-only)
	  SparseBoolMatrix L (n, n, lnz);

	  // initialize column pointers
	  lnz = 0;
	  for (octave_idx_type j = 0 ; j < n ; j++)
	    {
	      L.xcidx(j) = lnz;
	      lnz += ColCount [j];
	    }
	  L.xcidx(n) = lnz;


	  /* create a copy of the column pointers */
	  octave_idx_type *W = First;
	  for (octave_idx_type j = 0 ; j < n ; j++)
	    W [j] = L.xcidx(j);

	  // get workspace for computing one row of L
	  cholmod_sparse *R = cholmod_allocate_sparse (n, 1, n, false, true, 
						       0, CHOLMOD_PATTERN, cm);
	  octave_idx_type *Rp = static_cast<octave_idx_type *>(R->p);
	  octave_idx_type *Ri = static_cast<octave_idx_type *>(R->i);

	  // compute L one row at a time
	  for (octave_idx_type k = 0 ; k < n ; k++)
	    {
	      // get the kth row of L and store in the columns of L
	      CHOLMOD_NAME (row_subtree) (A1, A2, k, Parent, R, cm) ;
	      for (octave_idx_type p = 0 ; p < Rp [1] ; p++)
		L.xridx (W [Ri [p]]++) = k ;

	      // add the diagonal entry
	      L.xridx (W [k]++) = k ;
	    }

	  // free workspace
	  cholmod_free_sparse (&R, cm) ;


	  // transpose L to get R, or leave as is
	  if (nargin < 3)
	    L = L.transpose ();

	  // fill numerical values of L with one's
	  for (octave_idx_type p = 0 ; p < lnz ; p++)
	    L.xdata(p) = true;

	  retval(4) = L;
	}

      ColumnVector tmp (n);
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
	  /* compute the elimination tree height */
	  octave_idx_type height = 0 ;
	  for (int i = 0 ; i < n ; i++)
	    height = (height > Level[i] ? height : Level[i]);
	  height++ ;
	  retval(1) = static_cast<double> (height);
	}

      for (octave_idx_type i = 0; i < n; i++)
	tmp(i) = ColCount[i];
      retval(0) = tmp;
    }

 symbfact_error:
#else
  error ("symbfact: not available in this version of Octave");
#endif

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

