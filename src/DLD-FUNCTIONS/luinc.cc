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
