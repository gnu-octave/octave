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

// PKG_ADD: dispatch ("lu", "splu", "sparse matrix")
// PKG_ADD: dispatch ("lu", "splu", "sparse complex matrix")
// PKG_ADD: dispatch ("lu", "splu", "sparse bool matrix")
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
@end deftypefn\n\
@seealso{sparse, spinv, colamd, symamd}")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 3 || nargout > 4)
    {
      print_usage ("splu");
      return retval;
    }

  octave_value arg = args(0);

  int nr = arg.rows ();
  int nc = arg.columns ();

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
      if (args(k).class_name () == "sparse") 
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
	      int nel = tmp.numel ();
	      Qinit.resize (nel);
	      for (int i = 0; i < nel; i++)
		Qinit (i) = tmp (i) - 1;
	      have_Qinit = true;
	    }
	  else
	    {
	      int t_nc = tmp.cols ();
	      
	      if (tmp.nnz () != t_nc)
		error ("splu: Not a valid permutation matrix");
	      else
		{
		  for (int i = 0; i < t_nc + 1; i++)
		    if (tmp.cidx(i) != i)
		      {
			error ("splu: Not a valid permutation matrix");
			break;
		      }
		}
		  
	      if (!error_state)
		{
		  for (int i = 0; i < t_nc; i++)
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
	      int nel = tmp.numel ();
	      Qinit.resize (nel);
	      for (int i = 0; i < nel; i++)
		Qinit (i) = tmp (i) - 1;
	      have_Qinit = true;
	    }
	  else
	    {
	      SparseMatrix tmp2 (tmp);

	      int t_nc = tmp2.cols ();
	      
	      if (tmp2.nnz () != t_nc)
		error ("splu: Not a valid permutation matrix");
	      else
		{
		  for (int i = 0; i < t_nc + 1; i++)
		    if (tmp2.cidx(i) != i)
		      {
			error ("splu: Not a valid permutation matrix");
			break;
		      }
		}
		  
	      if (!error_state)
		{
		  for (int i = 0; i < t_nc; i++)
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
	  int m_nc = m.cols ();
	  Qinit.resize (m_nc);
	  for (int i = 0; i < m_nc; i++)
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
		  retval(1) = fact.U () * fact.Pc ().transpose ();
		else
		  retval(1) = fact.U ();

		retval(0) = L;
	      }
	      break;

	    case 3:
	      {
		SparseLU fact (m, Qinit, thres, true);

		retval(2) = fact.Pr ();
		if (have_Qinit)
		  retval(1) = fact.U () * fact.Pc ().transpose ();
		else
		  retval(1) = fact.U ();
		retval(0) = fact.L ();
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
		    retval(1) = fact.U ();
		    retval(0) = fact.L ();
		  }
		else
		  {
		    SparseLU fact (m, thres);

		    retval(3) = fact.Pc ();
		    retval(2) = fact.Pr ();
		    retval(1) = fact.U ();
		    retval(0) = fact.L ();
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
	  int m_nc = m.cols ();
	  Qinit.resize (m_nc);
	  for (int i = 0; i < m_nc; i++)
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
		  retval(1) = fact.U () * fact.Pc ().transpose ();
		else
		  retval(1) = fact.U ();
		retval(0) = L;
	      }
	      break;

	    case 3:
	      {
		SparseComplexLU fact (m, Qinit, thres, true);

		retval(2) = fact.Pr ();
		if (have_Qinit)
		  retval(1) = fact.U () * fact.Pc ().transpose ();
		else
		  retval(1) = fact.U ();
		retval(0) = fact.L ();
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
		    retval(1) = fact.U ();
		    retval(0) = fact.L ();
		  }
		else
		  {
		    SparseComplexLU fact (m, thres);

		    retval(3) = fact.Pc ();
		    retval(2) = fact.Pr ();
		    retval(1) = fact.U ();
		    retval(0) = fact.L ();
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

// PKG_ADD: dispatch ("inv", "spinv", "sparse matrix")
// PKG_ADD: dispatch ("inv", "spinv", "sparse complex matrix")
// PKG_ADD: dispatch ("inv", "spinv", "sparse bool matrix")
// PKG_ADD: dispatch ("inverse", "spinv", "sparse matrix")
// PKG_ADD: dispatch ("inverse", "spinv", "sparse complex matrix")
// PKG_ADD: dispatch ("inverse", "spinv", "sparse bool matrix")
DEFUN_DLD (spinv, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{x}, @var{rcond}] = } spinv (@var{a}, @var{Q})\n\
@deftypefnx {Loadable Function} {[@var{x}, @var{rcond}, @var{Q}] = } spinv (@var{a}, @var{Q})\n\
Compute the inverse of the square matrix @var{a}.  Return an estimate\n\
of the reciprocal condition number if requested, otherwise warn of an\n\
ill-conditioned matrix if the reciprocal condition number is small.\n\
\n\
An optional second input argument @var{Q} is the optional pre-ordering of\n\
the matrix, such that @code{@var{x} = inv (@var{a} (:, @var{Q}))}. @var{Q}\n\
can equally be a matrix, in which case @code{@var{x} = inv (@var{a} *\n\
@var{Q}))}.\n\
\n\
If a third output argument is given then the permuations to achieve a sparse\n\
inverse are returned. It is not required that the return column permutations\n\
@var{Q} and the same as the user supplied permutations\n\
@end deftypefn")
{
  error ("spinv: not implemented yet");
  return octave_value ();
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
