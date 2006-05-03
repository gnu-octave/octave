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

#include "SparseCmplxLU.h"
#include "SparsedbleLU.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

// PKG_ADD: dispatch ("lu", "splu", "sparse matrix");
// PKG_ADD: dispatch ("lu", "splu", "sparse complex matrix");
// PKG_ADD: dispatch ("lu", "splu", "sparse bool matrix");
DEFUN_DLD (splu, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{l}, @var{u}] =} splu (@var{a})\n\
@deftypefnx {Loadable Function} {[@var{l}, @var{u}, @var{P}] =} splu (@var{a})\n\
@deftypefnx {Loadable Function} {[@var{l}, @var{u}, @var{P}, @var{Q}] =} splu (@var{a})\n\
@deftypefnx {Loadable Function} {[@var{l}, @var{u}, @var{P}, @var{Q}] =} splu (@dots{}, @var{thres})\n\
@deftypefnx {Loadable Function} {[@var{l}, @var{u}, @var{P}] =} splu (@dots{}, @var{Q})\n\
@cindex LU decomposition\n\
Compute the LU decomposition of the sparse matrix @var{a}, using\n\
subroutines from UMFPACK.  The result is returned in a permuted\n\
form, according to the optional return values @var{P} and @var{Q}.\n\
\n\
Called with two or three output arguments and a single input argument,\n\
@dfn{splu} is a replacement for @dfn{lu}, and therefore the sparsity\n\
preserving column permutations @var{Q} are not performed. Called with\n\
a fourth output argument, the sparsity preserving column transformation\n\
@var{Q} is returned, such that @code{@var{P} * @var{a} * @var{Q} =\n\
@var{l} * @var{u}}.\n\
\n\
An additional input argument @var{thres}, that defines the pivoting\n\
threshold can be given. Alternatively, the desired sparsity preserving\n\
column permutations @var{Q} can be passed. Note that @var{Q} is assumed\n\
to be fixed if three are fewer than four output arguments. Otherwise,\n\
the updated column permutations are returned as the fourth argument.\n\
\n\
With two output arguments, returns the permuted forms of the upper and\n\
lower triangular matrices, such that @code{@var{a} = @var{l} * @var{u}}.\n\
With two or three output arguments, if a user-defined @var{Q} is given,\n\
then @code{@var{u} * @var{Q}'} is returned. The matrix is not required to\n\
be square.\n\
@seealso{sparse, spinv, colamd, symamd}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 3 || nargout > 4)
    {
      print_usage ("splu");
      return retval;
    }

  octave_value arg = args(0);

  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();

  int arg_is_empty = empty_arg ("splu", nr, nc);

  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return octave_value_list (3, SparseMatrix ());

  ColumnVector Qinit;
  bool have_Qinit = false;
  double thres = -1.;

  for (int k = 1; k < nargin; k++)
    {
      if (args(k).is_sparse_type ())
	{
	  SparseMatrix tmp = args (k).sparse_matrix_value ();
	  
	  if (error_state)
	    {
	      error ("splu: Not a valid permutation/threshold");
	      return retval;
	    }

	  dim_vector dv = tmp.dims ();

	  if (dv(0) == 1 && dv(1) == 1)
	    thres = tmp (0);
	  else if (dv(0) == 1 || dv(1) == 1)
	    {
	      octave_idx_type nel = tmp.numel ();
	      Qinit.resize (nel);
	      for (octave_idx_type i = 0; i < nel; i++)
		Qinit (i) = tmp (i) - 1;
	      have_Qinit = true;
	    }
	  else
	    {
	      octave_idx_type t_nc = tmp.cols ();
	      
	      if (tmp.nzmax () != t_nc)
		error ("splu: Not a valid permutation matrix");
	      else
		{
		  for (octave_idx_type i = 0; i < t_nc + 1; i++)
		    if (tmp.cidx(i) != i)
		      {
			error ("splu: Not a valid permutation matrix");
			break;
		      }
		}
		  
	      if (!error_state)
		{
		  for (octave_idx_type i = 0; i < t_nc; i++)
		    if (tmp.data (i) != 1.)
		      {
			error ("splu: Not a valid permutation matrix");
			break;
		      }
		    else
		      Qinit (i) = tmp.ridx (i) - 1; 
		}
	      
	      if (! error_state)
		have_Qinit = true;
	    }
	}
      else
	{
	  NDArray tmp = args(k).array_value ();

	  if (error_state)
	    return retval;

	  dim_vector dv = tmp.dims ();
	  if (dv.length () > 2)
	    {
	      error ("splu: second argument must be a vector/matrix or a scalar");
	    }
	  else if (dv(0) == 1 && dv(1) == 1)
	    thres = tmp (0);
	  else if (dv(0) == 1 || dv(1) == 1)
	    {
	      octave_idx_type nel = tmp.numel ();
	      Qinit.resize (nel);
	      for (octave_idx_type i = 0; i < nel; i++)
		Qinit (i) = tmp (i) - 1;
	      have_Qinit = true;
	    }
	  else
	    {
	      SparseMatrix tmp2 (tmp);

	      octave_idx_type t_nc = tmp2.cols ();
	      
	      if (tmp2.nzmax () != t_nc)
		error ("splu: Not a valid permutation matrix");
	      else
		{
		  for (octave_idx_type i = 0; i < t_nc + 1; i++)
		    if (tmp2.cidx(i) != i)
		      {
			error ("splu: Not a valid permutation matrix");
			break;
		      }
		}
		  
	      if (!error_state)
		{
		  for (octave_idx_type i = 0; i < t_nc; i++)
		    if (tmp2.data (i) != 1.)
		      {
			error ("splu: Not a valid permutation matrix");
			break;
		      }
		    else
		      Qinit (i) = tmp2.ridx (i) - 1; 
		}
	      
	      if (! error_state)
		have_Qinit = true;
	    }
	}
    }

  if (error_state)
    return retval;

  if (arg.is_real_type ())
    {
      SparseMatrix m = arg.sparse_matrix_value ();

      if (nargout < 4 && ! have_Qinit)
	{
	  octave_idx_type m_nc = m.cols ();
	  Qinit.resize (m_nc);
	  for (octave_idx_type i = 0; i < m_nc; i++)
	    Qinit (i) = i;
	}

      if (! error_state)
	{
	  switch (nargout)
	    {
	    case 0:
	    case 1:
	    case 2:
	      {
		SparseLU fact (m, Qinit, thres, true);

		SparseMatrix P = fact.Pr ();
		SparseMatrix L = P.transpose () * fact.L ();
		if (have_Qinit)
		  retval(1) = octave_value (fact.U () * fact.Pc ().transpose (),
		    MatrixType (MatrixType::Permuted_Upper, nc, fact.col_perm ()));
		else
		  retval(1) = octave_value (fact.U (), 
					    MatrixType (MatrixType::Upper));

		retval(0) = octave_value (L,
		  MatrixType (MatrixType::Permuted_Lower, nr, fact.row_perm ()));
	      }
	      break;

	    case 3:
	      {
		SparseLU fact (m, Qinit, thres, true);

		retval(2) = fact.Pr ();
		if (have_Qinit)
		  retval(1) = octave_value (fact.U () * fact.Pc ().transpose (),
		    MatrixType (MatrixType::Permuted_Upper, nc, fact.col_perm ()));
		else
		  retval(1) = octave_value (fact.U (), 
					    MatrixType (MatrixType::Upper));

		retval(0) = octave_value (fact.L (), 
					  MatrixType (MatrixType::Lower));
	      }
	      break;

	    case 4:
	    default:
	      {
		if (have_Qinit)
		  {
		    SparseLU fact (m, Qinit, thres, false);

		    retval(3) = fact.Pc ();
		    retval(2) = fact.Pr ();
		    retval(1) = octave_value (fact.U (), 
					      MatrixType (MatrixType::Upper));
		    retval(0) = octave_value (fact.L (), 
					      MatrixType (MatrixType::Lower));
		  }
		else
		  {
		    SparseLU fact (m, thres);

		    retval(3) = fact.Pc ();
		    retval(2) = fact.Pr ();
		    retval(1) = octave_value (fact.U (), 
					      MatrixType (MatrixType::Upper));
		    retval(0) = octave_value (fact.L (), 
					      MatrixType (MatrixType::Lower));
		  }
	      }
	      break;
	    }
	}
    }
  else if (arg.is_complex_type ())
    {
      SparseComplexMatrix m = arg.sparse_complex_matrix_value ();

      if (nargout < 4 && ! have_Qinit)
	{
	  octave_idx_type m_nc = m.cols ();
	  Qinit.resize (m_nc);
	  for (octave_idx_type i = 0; i < m_nc; i++)
	    Qinit (i) = i;
	}

      if (! error_state)
	{
	  switch (nargout)
	    {
	    case 0:
	    case 1:
	    case 2:
	      {
		SparseComplexLU fact (m, Qinit, thres, true);

		SparseMatrix P = fact.Pr ();
		SparseComplexMatrix L = P.transpose () * fact.L ();

		if (have_Qinit)
		  retval(1) = octave_value (fact.U () * fact.Pc ().transpose (),
		    MatrixType (MatrixType::Permuted_Upper, nc, fact.col_perm ()));
		else
		  retval(1) = octave_value (fact.U (), 
					    MatrixType (MatrixType::Upper));

		retval(0) = octave_value (L,
		  MatrixType (MatrixType::Permuted_Lower, nr, fact.row_perm ()));
	      }
	      break;

	    case 3:
	      {
		SparseComplexLU fact (m, Qinit, thres, true);

		retval(2) = fact.Pr ();
		if (have_Qinit)
		  retval(1) = octave_value (fact.U () * fact.Pc ().transpose (),
		    MatrixType (MatrixType::Permuted_Upper, nc, fact.col_perm ()));
		else
		  retval(1) = octave_value (fact.U (), 
					    MatrixType (MatrixType::Upper));

		retval(0) = octave_value (fact.L (), 
					  MatrixType (MatrixType::Lower));
	      }
	      break;

	    case 4:
	    default:
	      {
		if (have_Qinit)
		  {
		    SparseComplexLU fact (m, Qinit, thres, false);
		    
		    retval(3) = fact.Pc ();
		    retval(2) = fact.Pr ();
		    retval(1) = octave_value (fact.U (), 
					      MatrixType (MatrixType::Upper));
		    retval(0) = octave_value (fact.L (), 
					      MatrixType (MatrixType::Lower));
		  }
		else
		  {
		    SparseComplexLU fact (m, thres);

		    retval(3) = fact.Pc ();
		    retval(2) = fact.Pr ();
		    retval(1) = octave_value (fact.U (), 
					      MatrixType (MatrixType::Upper));
		    retval(0) = octave_value (fact.L (), 
					      MatrixType (MatrixType::Lower));
		  }
	      }
	      break;
	    }
	}
    }
  else
    {
      gripe_wrong_type_arg ("splu", arg);
    }

  return retval;
}

// PKG_ADD: dispatch ("inv", "spinv", "sparse matrix");
// PKG_ADD: dispatch ("inv", "spinv", "sparse complex matrix");
// PKG_ADD: dispatch ("inv", "spinv", "sparse bool matrix");
// PKG_ADD: dispatch ("inverse", "spinv", "sparse matrix");
// PKG_ADD: dispatch ("inverse", "spinv", "sparse complex matrix");
// PKG_ADD: dispatch ("inverse", "spinv", "sparse bool matrix");
DEFUN_DLD (spinv, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{x}, @var{rcond}] = } spinv (@var{a}, @var{Q})\n\
Compute the inverse of the sparse square matrix @var{a}.  Return an estimate\n\
of the reciprocal condition number if requested, otherwise warn of an\n\
ill-conditioned matrix if the reciprocal condition number is small.\n\
This function takes advantage of the sparsity of the matrix to accelerate\n\
the calculation of the inverse.\n\
\n\
In general @var{x} will be a full matrix, and so if possible forming the\n\
inverse of a sparse matrix should be avoided. It is significantly more\n\
accurate and faster to do @code{@var{y} = @var{a} \\ @var{b}}, rather\n\
than @code{@var{y} = spinv (@var{a}) * @var{b}}.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin != 1)
    {
      print_usage ("inv");
      return retval;
    }

  octave_value arg = args(0);

  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();

  int arg_is_empty = empty_arg ("spinverse", nr, nc);

  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return octave_value (Matrix ());

  if (nr != nc)
    {
      gripe_square_matrix_required ("spinverse");
      return retval;
    }

  if (arg.is_real_type ())
    {
      
      SparseMatrix m = arg.sparse_matrix_value ();      

      if (! error_state)
	{
	  MatrixType mattyp = args(0).matrix_type ();

	  octave_idx_type info;
	  double rcond = 0.0;
	  SparseMatrix result = m.inverse (mattyp, info, rcond, 1);

	  args(0).matrix_type (mattyp);

	  if (nargout > 1)
	    retval(1) = rcond;

	  retval(0) = result;

	  volatile double xrcond = rcond;
	  xrcond += 1.0;
	  if (nargout < 2 && (info == -1 || xrcond == 1.0))
	    warning ("spinverse: matrix singular to machine precision,\
 rcond = %g", rcond);
	}
    }
  else if (arg.is_complex_type ())
    {
      SparseComplexMatrix m = arg.sparse_complex_matrix_value ();

      if (! error_state)
	{
	  MatrixType mattyp = args(0).matrix_type ();

	  octave_idx_type info;
	  double rcond = 0.0;

	  SparseComplexMatrix result = m.inverse (mattyp, info, rcond, 1);

	  args(0).matrix_type (mattyp);

	  if (nargout > 1)
	    retval(1) = rcond;

	  retval(0) = result;

	  volatile double xrcond = rcond;
	  xrcond += 1.0;
	  if (nargout < 2 && (info == -1 || xrcond == 1.0))
	    warning ("spinverse: matrix singular to machine precision,\
 rcond = %g", rcond);
	}
    }
  else
    gripe_wrong_type_arg ("spinverse", arg);

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
