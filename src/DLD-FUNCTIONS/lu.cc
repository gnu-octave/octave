/*

Copyright (C) 1996, 1997, 1999, 2000, 2003, 2005, 2006, 2007, 2008, 2009
              John W. Eaton

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

#include "CmplxLU.h"
#include "dbleLU.h"
#include "fCmplxLU.h"
#include "floatLU.h"
#include "SparseCmplxLU.h"
#include "SparsedbleLU.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

template <class MT>
static octave_value
maybe_set_triangular (const MT& m, MatrixType::matrix_type t = MatrixType::Upper)
{
  typedef typename MT::element_type T;
  octave_value retval;
  octave_idx_type r = m.rows (), c = m.columns ();
  if (r == c)
    {
      const T zero = T();
      octave_idx_type i = 0;
      for (;i != r && m(i,i) != zero; i++) ;
      if (i == r)
        retval = octave_value (m, MatrixType (t));
      else
        retval = m;
    }
  else
    retval = m;

  return retval;
}

DEFUN_DLD (lu, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{l}, @var{u}, @var{p}] =} lu (@var{a})\n\
@deftypefnx {Loadable Function} {[@var{l}, @var{u}, @var{p}, @var{q}] =} lu (@var{s})\n\
@deftypefnx {Loadable Function} {[@var{l}, @var{u}, @var{p}, @var{q}, @var{r}] =} lu (@var{s})\n\
@deftypefnx {Loadable Function} {[@dots{}] =} lu (@var{s}, @var{thres})\n\
@deftypefnx {Loadable Function} {@var{y} =} lu (@dots{})\n\
@deftypefnx {Loadable Function} {[@dots{}] =} lu (@dots{}, 'vector')\n\
@cindex LU decomposition\n\
Compute the LU decomposition of @var{a}.  If @var{a} is full subroutines from\n\
@sc{Lapack} are used and if @var{a} is sparse then UMFPACK is used.  The\n\
result is returned in a permuted form, according to the optional return\n\
value @var{p}.  For example, given the matrix @code{a = [1, 2; 3, 4]},\n\
\n\
@example\n\
[l, u, p] = lu (a)\n\
@end example\n\
\n\
@noindent\n\
returns\n\
\n\
@example\n\
@group\n\
l =\n\
\n\
  1.00000  0.00000\n\
  0.33333  1.00000\n\
\n\
u =\n\
\n\
  3.00000  4.00000\n\
  0.00000  0.66667\n\
\n\
p =\n\
\n\
  0  1\n\
  1  0\n\
@end group\n\
@end example\n\
\n\
The matrix is not required to be square.\n\
\n\
Called with two or three output arguments and a spare input matrix,\n\
then @dfn{lu} does not attempt to perform sparsity preserving column\n\
permutations.  Called with a fourth output argument, the sparsity\n\
preserving column transformation @var{Q} is returned, such that\n\
@code{@var{p} * @var{a} * @var{q} = @var{l} * @var{u}}.\n\
\n\
Called with a fifth output argument and a sparse input matrix, then\n\
@dfn{lu} attempts to use a scaling factor @var{r} on the input matrix\n\
such that @code{@var{p} * (@var{r} \\ @var{a}) * @var{q} = @var{l} * @var{u}}.\n\
This typically leads to a sparser and more stable factorsation.\n\
\n\
An additional input argument @var{thres}, that defines the pivoting\n\
threshold can be given.  @var{thres} can be a scalar, in which case\n\
it defines UMFPACK pivoting tolerance for both symmetric and unsymmetric\n\
cases.  If @var{thres} is a two element vector, then the first element\n\
defines the pivoting tolerance for the unsymmetric UMFPACK pivoting\n\
strategy and the second the symmetric strategy.  By default, the values\n\
defined by @code{spparms} are used and are by default @code{[0.1, 0.001]}.\n\
\n\
Given the string argument 'vector', @dfn{lu} returns the values of @var{p}\n\
@var{q} as vector values, such that for full matrix, @code{@var{a}\n\
(@var{p},:) = @var{l} * @var{u}}, and @code{@var{r}(@var{p},:) * @var{a}\n\
(:, @var{q}) = @var{l} * @var{u}}.\n\
\n\
With two output arguments, returns the permuted forms of the upper and\n\
lower triangular matrices, such that @code{@var{a} = @var{l} * @var{u}}.\n\
With one output argument @var{y}, then the matrix returned by the @sc{Lapack}\n\
routines is returned.  If the input matrix is sparse then the matrix @var{l}\n\
is embedded into @var{u} to give a return value similar to the full case.\n\
For both full and sparse matrices, @dfn{lu} looses the permutation\n\
information.\n\
@end deftypefn")
{
  octave_value_list retval;
  int nargin = args.length ();
  bool issparse = (nargin > 0 && args(0).is_sparse_type ());
  bool scale = (nargout  == 5);

  if (nargin < 1 || (issparse && (nargin > 3 || nargout > 5)) 
      || (!issparse && (nargin > 2 || nargout > 3)))
    {
      print_usage ();
      return retval;
    }

  bool vecout = false;
  Matrix thres;

  int n = 1;
  while (n < nargin && ! error_state)
    {
      if (args (n).is_string ())
	{
	  std::string tmp = args(n++).string_value ();

	  if (! error_state )
	    {
	      if (tmp.compare ("vector") == 0)
		vecout = true;
	      else
		error ("lu: unrecognized string argument");
	    }
	}
      else
	{
	  Matrix tmp = args(n++).matrix_value ();

	  if (! error_state )
	    {
	      if (!issparse)
		error ("lu: can not define pivoting threshold for full matrices");
	      else if (tmp.nelem () == 1)
		{
		  thres.resize(1,2);
		  thres(0) = tmp(0);
		  thres(1) = tmp(0);
		}
	      else if (tmp.nelem () == 2)
		thres = tmp;
	      else
		error ("lu: expecting 2 element vector for thres");
	    }
	}
    }

  octave_value arg = args(0);

  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();

  int arg_is_empty = empty_arg ("lu", nr, nc);

  if (issparse)
    {
      if (arg_is_empty < 0)
	return retval;
      else if (arg_is_empty > 0)
	return octave_value_list (5, SparseMatrix ());

      ColumnVector Qinit;
      if (nargout < 4)
	{
	  Qinit.resize (nc);
	  for (octave_idx_type i = 0; i < nc; i++)
	    Qinit (i) = i;
	}

      if (arg.is_real_type ())
	{
	  SparseMatrix m = arg.sparse_matrix_value ();

	  switch (nargout)
	    {
	    case 0:
	    case 1:
	    case 2:
	      {
		SparseLU fact (m, Qinit, thres, false, true);

		if (nargout < 2)
		  retval (0) = fact.Y ();
		else
		  {
		    PermMatrix P = fact.Pr_mat ();
		    SparseMatrix L = P.transpose () * fact.L ();
		    retval(1) = octave_value (fact.U (), 
					      MatrixType (MatrixType::Upper));

		    retval(0) = octave_value (L, 
			MatrixType (MatrixType::Permuted_Lower, 
				    nr, fact.row_perm ()));
		  }
	      }
	      break;

	    case 3:
	      {
		SparseLU fact (m, Qinit, thres, false, true);

		if (vecout)
		  retval (2) = fact.Pr_vec ();
		else
		  retval(2) = fact.Pr_mat ();

		retval(1) = octave_value (fact.U (), 
					  MatrixType (MatrixType::Upper));
		retval(0) = octave_value (fact.L (), 
					  MatrixType (MatrixType::Lower));
	      }
	      break;

	    case 4:
	    default:
	      {
		SparseLU fact (m, thres, scale);

		if (scale)
		  retval(4) = fact.R ();

		if (vecout)
		  {
		    retval(3) = fact.Pc_vec ();
		    retval(2) = fact.Pr_vec ();
		  }
		else
		  {
		    retval(3) = fact.Pc_mat ();
		    retval(2) = fact.Pr_mat ();
		  }
		retval(1) = octave_value (fact.U (), 
					  MatrixType (MatrixType::Upper));
		retval(0) = octave_value (fact.L (), 
					  MatrixType (MatrixType::Lower));
	      }
	      break;
	    }
	}
      else if (arg.is_complex_type ())
	{
	  SparseComplexMatrix m = arg.sparse_complex_matrix_value ();

	  switch (nargout)
	    {
	    case 0:
	    case 1:
	    case 2:
	      {
		SparseComplexLU fact (m, Qinit, thres, false, true);

		if (nargout < 2)
		  retval (0) = fact.Y ();
		else
		  {
		    PermMatrix P = fact.Pr_mat ();
		    SparseComplexMatrix L = P.transpose () * fact.L ();
		    retval(1) = octave_value (fact.U (), 
					      MatrixType (MatrixType::Upper));

		    retval(0) = octave_value (L, 
			MatrixType (MatrixType::Permuted_Lower, 
				    nr, fact.row_perm ()));
		  }
	      }
	      break;

	    case 3:
	      {
		SparseComplexLU fact (m, Qinit, thres, false, true);

		if (vecout)
		  retval (2) = fact.Pr_vec ();
		else
		  retval(2) = fact.Pr_mat ();

		retval(1) = octave_value (fact.U (), 
					  MatrixType (MatrixType::Upper));
		retval(0) = octave_value (fact.L (), 
					  MatrixType (MatrixType::Lower));
	      }
	      break;

	    case 4:
	    default:
	      {
		SparseComplexLU fact (m, thres, scale);

		if (scale)
		  retval(4) = fact.R ();

		if (vecout)
		  {
		    retval(3) = fact.Pc_vec ();
		    retval(2) = fact.Pr_vec ();
		  }
		else
		  {
		    retval(3) = fact.Pc_mat ();
		    retval(2) = fact.Pr_mat ();
		  }
		retval(1) = octave_value (fact.U (), 
					  MatrixType (MatrixType::Upper));
		retval(0) = octave_value (fact.L (), 
					  MatrixType (MatrixType::Lower));
	      }
	      break;
	    }
	}
      else
	gripe_wrong_type_arg ("lu", arg);
    }
  else
    {
      if (arg_is_empty < 0)
	return retval;
      else if (arg_is_empty > 0)
	return octave_value_list (3, Matrix ());

      if (arg.is_real_type ())
	{
	  if (arg.is_single_type ())
	    {
	      FloatMatrix m = arg.float_matrix_value ();

	      if (! error_state)
		{
		  FloatLU fact (m);

		  switch (nargout)
		    {
		    case 0:
		    case 1:
		      retval(0) = fact.Y ();
		      break;

		    case 2:
		      {
			PermMatrix P = fact.P ();
			FloatMatrix L = P.transpose () * fact.L ();
			retval(1) = maybe_set_triangular (fact.U (), MatrixType::Upper);
			retval(0) = L;
		      }
		      break;

		    case 3:
		    default:
		      {
			if (vecout)
			  retval(2) = fact.P_vec ();
			else
			  retval(2) = fact.P ();
			retval(1) = maybe_set_triangular (fact.U (), MatrixType::Upper);
			retval(0) = maybe_set_triangular (fact.L (), MatrixType::Lower);
		      }
		      break;
		    }
		}
	    }
	  else
	    {
	      Matrix m = arg.matrix_value ();

	      if (! error_state)
		{
		  LU fact (m);

		  switch (nargout)
		    {
		    case 0:
		    case 1:
		      retval(0) = fact.Y ();
		      break;

		    case 2:
		      {
			PermMatrix P = fact.P ();
			Matrix L = P.transpose () * fact.L ();
			retval(1) = maybe_set_triangular (fact.U (), MatrixType::Upper);
			retval(0) = L;
		      }
		      break;

		    case 3:
		    default:
		      {
			if (vecout)
			  retval(2) = fact.P_vec ();
			else
			  retval(2) = fact.P ();
			retval(1) = maybe_set_triangular (fact.U (), MatrixType::Upper);
			retval(0) = maybe_set_triangular (fact.L (), MatrixType::Lower);
		      }
		      break;
		    }
		}
	    }
	}
      else if (arg.is_complex_type ())
	{
	  if (arg.is_single_type ())
	    {
	      FloatComplexMatrix m = arg.float_complex_matrix_value ();

	      if (! error_state)
		{
		  FloatComplexLU fact (m);

		  switch (nargout)
		    {
		    case 0:
		    case 1:
		      retval(0) = fact.Y ();
		      break;

		    case 2:
		      {
			PermMatrix P = fact.P ();
			FloatComplexMatrix L = P.transpose () * fact.L ();
			retval(1) = maybe_set_triangular (fact.U (), MatrixType::Upper);
			retval(0) = L;
		      }
		      break;

		    case 3:
		    default:
		      {
			if (vecout)
			  retval(2) = fact.P_vec ();
			else
			  retval(2) = fact.P ();
			retval(1) = maybe_set_triangular (fact.U (), MatrixType::Upper);
			retval(0) = maybe_set_triangular (fact.L (), MatrixType::Lower);
		      }
		      break;
		    }
		}
	    }
	  else
	    {
	      ComplexMatrix m = arg.complex_matrix_value ();

	      if (! error_state)
		{
		  ComplexLU fact (m);

		  switch (nargout)
		    {
		    case 0:
		    case 1:
		      retval(0) = fact.Y ();
		      break;

		    case 2:
		      {
			PermMatrix P = fact.P ();
			ComplexMatrix L = P.transpose () * fact.L ();
			retval(1) = maybe_set_triangular (fact.U (), MatrixType::Upper);
			retval(0) = L;
		      }
		      break;

		    case 3:
		    default:
		      {
			if (vecout)
			  retval(2) = fact.P_vec ();
			else
			  retval(2) = fact.P ();
			retval(1) = maybe_set_triangular (fact.U (), MatrixType::Upper);
			retval(0) = maybe_set_triangular (fact.L (), MatrixType::Lower);
		      }
		      break;
		    }
		}
	    }
	}
      else
	gripe_wrong_type_arg ("lu", arg);
    }

  return retval;
}

/*

%!assert(lu ([1, 2; 3, 4]), [3, 4; 1/3, 2/3], eps);

%!test
%! [l, u] = lu ([1, 2; 3, 4]);
%! assert(l, [1/3, 1; 1, 0], sqrt (eps));
%! assert(u, [3, 4; 0, 2/3], sqrt (eps));

%!test
%! [l, u, p] = lu ([1, 2; 3, 4]);
%! assert(l, [1, 0; 1/3, 1], sqrt (eps));
%! assert(u, [3, 4; 0, 2/3], sqrt (eps));
%! assert(p(:,:), [0, 1; 1, 0], sqrt (eps));

%!test
%! [l, u, p] = lu ([1, 2; 3, 4],'vector');
%! assert(l, [1, 0; 1/3, 1], sqrt (eps));
%! assert(u, [3, 4; 0, 2/3], sqrt (eps));
%! assert(p, [2;1], sqrt (eps));

%!test
%! [l u p] = lu ([1, 2; 3, 4; 5, 6]);
%! assert(l, [1, 0; 1/5, 1; 3/5, 1/2], sqrt (eps));
%! assert(u, [5, 6; 0, 4/5], sqrt (eps));
%! assert(p(:,:), [0, 0, 1; 1, 0, 0; 0 1 0], sqrt (eps));

%!assert(lu (single([1, 2; 3, 4])), single([3, 4; 1/3, 2/3]), eps('single'));

%!test
%! [l, u] = lu (single([1, 2; 3, 4]));
%! assert(l, single([1/3, 1; 1, 0]), sqrt (eps('single')));
%! assert(u, single([3, 4; 0, 2/3]), sqrt (eps('single')));

%!test
%! [l, u, p] = lu (single([1, 2; 3, 4]));
%! assert(l, single([1, 0; 1/3, 1]), sqrt (eps('single')));
%! assert(u, single([3, 4; 0, 2/3]), sqrt (eps('single')));
%! assert(p(:,:), single([0, 1; 1, 0]), sqrt (eps('single')));

%!test
%! [l, u, p] = lu (single([1, 2; 3, 4]),'vector');
%! assert(l, single([1, 0; 1/3, 1]), sqrt (eps('single')));
%! assert(u, single([3, 4; 0, 2/3]), sqrt (eps('single')));
%! assert(p, single([2;1]), sqrt (eps('single')));

%!test
%! [l u p] = lu (single([1, 2; 3, 4; 5, 6]));
%! assert(l, single([1, 0; 1/5, 1; 3/5, 1/2]), sqrt (eps('single')));
%! assert(u, single([5, 6; 0, 4/5]), sqrt (eps('single')));
%! assert(p(:,:), single([0, 0, 1; 1, 0, 0; 0 1 0]), sqrt (eps('single')));

%!error <Invalid call to lu.*> lu ();
%!error lu ([1, 2; 3, 4], 2);

 */

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
