/*

Copyright (C) 2005 David Bateman

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"
#include "oct-map.h"

#include "SparseCmplxLU.h"
#include "SparsedbleLU.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

DEFUN_DLD (luinc, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{l}, @var{u}, @var{p}, @var{q}] =} luinc (@var{a}, '0')\n\
@deftypefnx {Loadable Function} {[@var{l}, @var{u}, @var{p}, @var{q}] =} luinc (@var{a}, @var{droptol})\n\
@deftypefnx {Loadable Function} {[@var{l}, @var{u}, @var{p}, @var{q}] =} luinc (@var{a}, @var{opts})\n\
@cindex LU decomposition\n\
Produce the incomplete LU factorization of the sparse matrix @var{a}.\n\
Two types of incomplete factorization are possible, and the type\n\
is determined by the second argument to @dfn{luinc}.\n\
\n\
Called with a second argument of '0', the zero-level incomplete\n\
LU factorization is produced. This creates a factorization of @var{a}\n\
where the position of the non-zero arguments correspond to the same\n\
positions as in the matrix @var{a}.\n\
\n\
Alternatively, the fill-in of the incomplete LU factorization can\n\
be controlled through the variable @var{droptol} or the structure\n\
@var{opts}. The UMFPACK multifrontal factorization code by Tim A.\n\
Davis is used for the incomplete LU factorication, (availability\n\
@url{http://www.cise.ufl.edu/research/sparse/umfpack/})\n\
\n\
@var{droptol} determines the values below which the values in the LU\n\
factorization are dropped and replaced by zero. It must be a positive\n\
scalar, and any values in the factorization whose absolute value are\n\
less than this value are dropped, expect if leaving them increase the\n\
sparsity of the matrix. Setting @var{droptol} to zero results in a\n\
complete LU factorization which is the default.\n\
\n\
@var{opts} is a structure containing one or more of the fields\n\
\n\
@table @code\n\
@item droptol\n\
The drop tolerance as above. If @var{opts} only contains @code{droptol}\n\
then this is equivalent to using the variable @var{droptol}.\n\
\n\
@item milu\n\
A logical variable flagging whether to use the modified incomplete LU\n\
factorization. In the case that @code{milu} is true, the dropped values\n\
are subtract from the diagonal of the matrix U of the factorization.\n\
The default is @code{false}.\n\
\n\
@item udiag\n\
A logical variable that flags whether zero elements on the diagonal of U\n\
should be replaced with @var{droptol} to attempt to avoid singular\n\
factors. The default is @code{false}.\n\
\n\
@item thresh\n\
Defines the pivot threshold in the interval [0,1]. Values outside that\n\
range are ignored.\n\
@end table\n\
\n\
All other fields in @var{opts} are ignored. The outputs from @dfn{luinc}\n\
are the same as for @dfn{lu}.\n\
@end deftypefn\n\
@seealso{sparse, lu, cholinc}")
{
  int nargin = args.length ();
  octave_value_list retval;

  if (nargin == 0)
    print_usage ("luinc");
  else if (nargin != 2)
    error ("luinc: incorrect number of arguments");
  else
    {
      bool zero_level = false;
      bool milu = false;
      bool udiag = false;
      bool thresh = -1;
      double droptol = -1.;

      if (args(1).is_string ())
	{
	  if (args(1).string_value () == "0")
	    zero_level = true;
	  else
	    error ("luinc: unrecognized string argument");
	}
      else if (args(1).is_map ())
	{
	  Octave_map map = args(1).map_value ();

	  if (map.contains ("droptol"))
	    droptol = map.contents ("droptol")(0).double_value ();

	  if (map.contains ("milu"))
	    {
	      double tmp = map.contents ("milu")(0).double_value ();

	      milu = (tmp == 0. ? false : true);
	    }

	  if (map.contains ("udiag"))
	    {
	      double tmp = map.contents ("udiag")(0).double_value ();

	      udiag = (tmp == 0. ? false : true);
	    }

	  if (map.contains ("thresh"))
	    thresh = map.contents ("thresh")(0).double_value ();
	}
      else
	droptol = args(1).double_value ();

      // XXX FIXME XXX Add code for zero-level factorization
      if (zero_level)
	error ("luinc: zero-level factorization not implemented");

      if (!error_state)
	{
	  if (args(0).class_name () == "sparse") 
	    {
	      SparseMatrix sm = args(0).sparse_matrix_value ();
	      octave_idx_type sm_nc = sm.cols ();
	      ColumnVector Qinit (sm_nc);

	      for (octave_idx_type i = 0; i < sm_nc; i++)
		Qinit (i) = i;

	      if (! error_state)
		{
		  switch (nargout)
		    {
		    case 0:
		    case 1:
		    case 2:
		      {
			SparseLU fact (sm, Qinit, thresh, true, droptol,
				       milu, udiag);

			SparseMatrix P = fact.Pr ();
			SparseMatrix L = P.transpose () * fact.L ();
			retval(1) = fact.U ();
			retval(0) = L;
		      }
		      break;

		    case 3:
		      {
			SparseLU fact (sm, Qinit, thresh, true, droptol,
				       milu, udiag);

			retval(2) = fact.Pr ();
			retval(1) = fact.U ();
			retval(0) = fact.L ();
		      }
		      break;

		    case 4:
		    default:
		      {
			SparseLU fact (sm, Qinit, thresh, false, droptol,
				       milu, udiag);

			retval(3) = fact.Pc ();
			retval(2) = fact.Pr ();
			retval(1) = fact.U ();
			retval(0) = fact.L ();
		      }
		      break;
		    }
		}
	    }
	  else if (args(0).class_name () == "sparse complex") 
	    {
	      SparseComplexMatrix sm = 
		args(0).sparse_complex_matrix_value ();
	      octave_idx_type sm_nc = sm.cols ();
	      ColumnVector Qinit (sm_nc);

	      for (octave_idx_type i = 0; i < sm_nc; i++)
		Qinit (i) = i;

	      if (! error_state)
		{
		  switch (nargout)
		    {
		    case 0:
		    case 1:
		    case 2:
		      {
			SparseComplexLU fact (sm, Qinit, thresh, true, 
					      droptol, milu, udiag);

			SparseMatrix P = fact.Pr ();
			SparseComplexMatrix L = P.transpose () * fact.L ();
			retval(1) = fact.U ();
			retval(0) = L;
		      }
		      break;

		    case 3:
		      {
			SparseComplexLU fact (sm, Qinit, thresh, true,
					      droptol, milu, udiag);

			retval(2) = fact.Pr ();
			retval(1) = fact.U ();
			retval(0) = fact.L ();
		      }
		      break;

		    case 4:
		    default:
		      {
			SparseComplexLU fact (sm, Qinit, thresh, false,
					      droptol, milu, udiag);

			retval(3) = fact.Pc ();
			retval(2) = fact.Pr ();
			retval(1) = fact.U ();
			retval(0) = fact.L ();
		      }
		      break;
		    }
		}
	    }
	  else
	    error ("luinc: first argument must be sparse");
	}
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

/* 
 LUINC  Sparse Incomplete LU factorization.
    LUINC produces two different kinds of incomplete LU factorizations -- the
    drop tolerance and the 0 level of fill-in factorizations.  These factors
    may be useful as preconditioners for a system of linear equations being
    solved by an iterative method such as BICG (BiConjugate Gradients).
 
    LUINC(X,DROPTOL) performs the incomplete LU factorization of
    X with drop tolerance DROPTOL.
 
    LUINC(X,OPTS) allows additional options to the incomplete LU
    factorization.  OPTS is a structure with up to four fields:
        droptol --- the drop tolerance of incomplete LU
        milu    --- modified incomplete LU
        udiag   --- replace zeros on the diagonal of U
        thresh  --- the pivot threshold (see also LU)
 
    Only the fields of interest need to be set.
 
    droptol is a non-negative scalar used as the drop
    tolerance for the incomplete LU factorization.  This factorization
    is computed in the same (column-oriented) manner as the
    LU factorization except after each column of L and U has
    been calculated, all entries in that column which are smaller
    in magnitude than the local drop tolerance, which is 
    droptol * NORM of the column of X, are "dropped" from L or U.
    The only exception to this dropping rule is the diagonal of the
    upper triangular factor U which remains even if it is too small.
    Note that entries of the lower triangular factor L are tested
    before being scaled by the pivot.  Setting droptol = 0
    produces the complete LU factorization, which is the default.
 
    milu stands for modified incomplete LU factorization.  Its
    value is either 0 (unmodified, the default) or 1 (modified).
    Instead of discarding those entries from the newly-formed
    column of the factors, they are subtracted from the diagonal
    of the upper triangular factor, U.
 
    udiag is either 0 or 1.  If it is 1, any zero diagonal entries
    of the upper triangular factor U are replaced by the local drop
    tolerance in an attempt to avoid a singular factor.  The default
    is 0.
 
    thresh is a pivot threshold in [0,1].  Pivoting occurs
    when the diagonal entry in a column has magnitude less
    than thresh times the magnitude of any sub-diagonal entry in
    that column.  thresh = 0 forces diagonal pivoting.  thresh = 1 is
    the default.
 
    Example:
 
       load west0479
       A = west0479;
       nnz(A)
       nnz(lu(A))
       nnz(luinc(A,1e-6))
 
       This shows that A has 1887 nonzeros, its complete LU factorization
       has 16777 nonzeros, and its incomplete LU factorization with a
       drop tolerance of 1e-6 has 10311 nonzeros.
 
 
    [L,U,P] = LUINC(X,'0') produces the incomplete LU factors of a sparse
    matrix with 0 level of fill-in (i.e. no fill-in).  L is unit lower
    trianglar, U is upper triangular and P is a permutation matrix.  U has the
    same sparsity pattern as triu(P*X).  L has the same sparsity pattern as
    tril(P*X), except for 1's on the diagonal of L where P*X may be zero.  Both
    L and U may have a zero because of cancellation where P*X is nonzero.  L*U
    differs from P*X only outside of the sparsity pattern of P*X.
 
    [L,U] = LUINC(X,'0') produces upper triangular U and L is a permutation of
    unit lower triangular matrix.  Thus no comparison can be made between the
    sparsity patterns of L,U and X, although nnz(L) + nnz(U) = nnz(X) + n.  L*U
    differs from X only outside of its sparsity pattern.
 
    LU = LUINC(X,'0') returns "L+U-I", where L is unit lower triangular, U is
    upper triangular and the permutation information is lost.
 
    Example:
 
       load west0479
       A = west0479;
       [L,U,P] = luinc(A,'0');
       isequal(spones(U),spones(triu(P*A)))
       spones(L) ~= spones(tril(P*A))
       D = (L*U) .* spones(P*A) - P*A
 
       spones(L) differs from spones(tril(P*A)) at some positions on the
       diagonal and at one position in L where cancellation zeroed out a
       nonzero element of P*A.  The entries of D are of the order of eps.
 
    LUINC works only for sparse matrices.
 
    See also LU, CHOLINC, BICG.

*/
