/*

Copyright (C) 2004 David Bateman

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

In addition to the terms of the GPL, you are permitted to link
this program with any Open Source program, as defined by the
Open Source Initiative (www.opensource.org)

*/

/*

This is the Octave interface to the UMFPACK AMD code, which bore the following
copyright

 AMD Version 1.1 (Jan. 21, 2004), Copyright (c) 2004 by Timothy A. Davis,
 Patrick R. Amestoy, and Iain S. Duff.  See ../README for License.
 email: davis@cise.ufl.edu    CISE Department, Univ. of Florida.
 web: http://www.cise.ufl.edu/research/sparse/amd
 --------------------------------------------------------------------------

    Acknowledgements: This work was supported by the National Science
       Foundation, under grants ASC-9111263, DMS-9223088, and CCR-0203270.

*/

#include <cstdlib>
#include <string>

#include <octave/config.h>
#include <octave/ov.h>
#include <octave/defun-dld.h>
#include <octave/pager.h>
#include <octave/ov-re-mat.h>

#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

// External AMD functions in C
extern "C" {
#include "amd.h"
}

DEFUN_DLD (amd, args, nargout,
    "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{p} =} amd (@var{s})\n\
@deftypefnx {Loadable Function} {@var{Control} =} amd ()\n\
@deftypefnx {Loadable Function} {[@var{p}, @var{info}] =} amd (@var{s})\n\
\n\
AMD Approximate minimum degree permutation. Returns the approximate\n\
minimum degree permutation vector for the sparse matrix\n\
@code{@var{c} = @var{S} + @var{S}'}. The Cholesky factorization of\n\
@code{@var{c} (@var{p}, @var{p})}, or @code{@var{s} (@var{p}, @var{p})},\n\
tends to be sparser than that of @var{c} or @var{s}.\n\
@var{s} must be square. If @var{s} is full, @code{amd (@var{S})} is\n\
equivalent to  @code{amd (sparse (@var{s}))}.\n\
\n\
@table @asis\n\
@item @var{Control} (1)\n\
If S is n-by-n, then rows/columns with more than\n\
@code{@dfn{max} (16, (@var{Control} (1)) * @dfn{sqrt} (@var{n}))} entries\n\
in @code{@var{s} + @var{S}'} are considered @emph{dense}, and ignored during\n\
ordering.  They are placed last in the output permutation.  The default is\n\
10.0 if @var{Control} is not present.\n\
@item @var{Control} (2)\n\
If nonzero, then aggressive absorption is performed. This is the default if\n\
@var{Control} is not present.\n\
@item @var{Control} (3)\n\
If nonzero, print statistics about the ordering.\n\
@end table\n\
\n\
@table @asis\n\
@item @var{Info} (1)\n\
status (0: ok, -1: out of memory, -2: matrix invalid)\n\
@item @var{Info} (2)\n\
@code{@var{n} = size (@var{a}, 1)}\n\
@item @var{Info} (3)\n\
@code{nnz (A)}\n\
@item @var{Info} (4)\n\
The symmetry of the matrix @var{s} (0.0 means purely unsymmetric, 1.0 means\n\
purely symmetric).  Computed as: @code{@var{b} = tril (@var{s}, -1) +\n\
triu (@var{s}, 1); @var{symmetry} = nnz (@var{b} & @var{b}')\n\
/ nnz (@var{b});}\n\
@item @var{Info} (5)\n\
@code{nnz (diag (@var{s}))}\n\
@item @var{Info} (6)\n\
@dfn{nnz} in @code{@var{s} + @var{s}'}, excluding the diagonal\n\
(= @code{nnz (@var{b} + @var{b}')})\n\
@item @var{Info} (7)\n\
Number of @emph{dense} rows/columns in @code{@var{s} + @var{s}'}\n\
@item @var{Info} (8)\n\
The amount of memory used by AMD, in bytes\n\
@item @var{Info} (9)\n\
The number of memory compactions performed by AMD\n\
@end table\n\
\n\
The following statistics are slight upper bounds because of the\n\
approximate degree in AMD. The bounds are looser if @emph{dense}\n\
rows/columns are ignored during ordering @code{(@var{Info} (7) > 0)}.\n\
The statistics are for a subsequent factorization of the matrix\n\
@code{@var{c} (@var{p},@var{p})}.  The LU factorization statistics assume\n \
no pivoting.\n\
\n\
@table @asis\n\
@item @var{Info} (10)\n\
The number of nonzeros in L, excluding the diagonal\n\
@item @var{Info} (11)\n\
The number of divide operations for LL', LDL', or LU\n\
@item @var{Info (12)}\n\
The number of multiply-subtract pairs for LL' or LDL'\n\
@item @var{Info} (13)\n\
The number of multiply-subtract pairs for LU\n\
@item @var{Info} (14)\n\
The max number of nonzeros in any column of L (incl. diagonal)\n\
@item @var{Info} (15:20)\n\
unused, reserved for future use\n\
@end table\n\
\n\
An assembly tree post-ordering is performed, which is typically the same\n\
as an elimination tree post-ordering.  It is not always identical because\n\
of the approximate degree update used, and because @emph{dense} rows/columns\n\
do not take part in the post-order.  It well-suited for a subsequent\n\
@dfn{chol}, however.  If you require a precise elimination tree\n\
post-ordering, then do:\n\
\n\
@group\n\
   @var{p} = @dfn{amd} (@var{s});\n\
   % skip this if S already symmetric\n\
   @var{c} = spones (@var{s}) + spones (@var{s}');\n\
   [@var{ignore}, @var{q}] = sparsfun ('symetree', @var{c} (@var{p}, @var{p}));\n\
   @var{p} = @var{p} (@var{q});\n\
@end group\n\
\n\
AMD Version 1.1 (Jan. 21, 2004), Copyright @copyright{} 2004 by\n\
Timothy A. Davis, Patrick R. Amestoy, and Iain S. Duff.\n\
\n\
email: davis@@cise.ufl.edu  (CISE Department, Univ. of Florida).\n\
\n\
web: http://www.cise.ufl.edu/research/sparse/amd\n\
\n\
Acknowledgements: This work was supported by the National Science\n\
Foundation, under grants ASC-9111263, DMS-9223088, and CCR-0203270.\n\
@end deftypefn")
{
  int nargin = args.length (); 
  octave_value_list retval; 
  int spumoni = 0;

  if (nargin > 2 || nargout > 2)
    usage ("p = amd (A) or [p, Info] = amd (A, Control)");
  else if (nargin == 0)
    {
      // Get the default control parameter, and return
      NDArray control (dim_vector (AMD_CONTROL, 1));
      double *control_ptr = control.fortran_vec ();
      amd_defaults (control_ptr);
      if (nargout == 0)
	amd_control (control_ptr);
      else
	retval(0) = control;
    }
  else
    {
      NDArray control (dim_vector (AMD_CONTROL, 1));
      double *control_ptr = control.fortran_vec ();
      amd_defaults (control_ptr);

      if (nargin > 1)
	{
	  NDArray control_in = args(1).array_value();

	  if (error_state)
	    {	  
	      error ("amd: could not read control vector");
	      return retval;
	    }

	  dim_vector dv = control_in.dims ();
	  if (dv.length() > 2 || (dv(0) != 1 && dv(1) != 1))
	    {
	      error ("amd: control vector isn't a vector");
	      return retval;
	    }
	  
	  int nc = dv.numel ();
	  control (AMD_DENSE) = (nc > 0 ? control_in (AMD_DENSE) :
				 AMD_DEFAULT_DENSE);
	  control (AMD_AGGRESSIVE) = (nc > 1 ? control_in (AMD_AGGRESSIVE) :
				      AMD_DEFAULT_AGGRESSIVE);
	  spumoni = (nc > 2 ? (control_in (2) != 0) : 0);
	}

      if (spumoni > 0)
	amd_control (control_ptr);

      int *Ap, *Ai;
      int n, m, nz;

      // These are here only so that the C++ destructors don't prematurally
      // remove the underlying data we are interested in
      SparseMatrix sm;
      SparseComplexMatrix scm;
      Matrix mm;
      ComplexMatrix cm;

      if (args(0).class_name () != "sparse" && spumoni > 0)
	octave_stdout << "    input matrix A is full (sparse copy"
		      << " of A will be created)" << std::endl;

      if (args(0).is_complex_type ())
	{
	  scm = args(0).sparse_complex_matrix_value ();
	  Ai = scm.ridx ();
	  Ap = scm.cidx ();
	  m = scm.rows ();
	  n = scm.cols ();
	  nz = scm.nnz ();
	}
      else
	{
	  sm = args(0).sparse_matrix_value ();
	  Ai = sm.ridx ();
	  Ap = sm.cidx ();
	  m = sm.rows ();
	  n = sm.cols ();
	  nz = sm.nnz ();
	}

      if (spumoni > 0)
	octave_stdout << "    input matrix A is " << m << "-by-" << n 
		      << std::endl;

      if (m != n)
	{
	  error ("amd: A must be square");
	  return retval;
	}

      if (spumoni > 0)
	octave_stdout << "    input matrix A has " << nz << 
	  " nonzero entries" << std::endl;

      // allocate workspace for output permutation 
      Array<int> P(n+1);
      int *P_ptr = P.fortran_vec ();
      NDArray info (dim_vector (AMD_INFO, 1));
      double *info_ptr = info.fortran_vec ();
      int result;

      // order the matrix
      result = amd_order (n, Ap, Ai, P_ptr, control_ptr, info_ptr);

      // print results (including return value)
      if (spumoni > 0)
	amd_info (info_ptr);

      // check error conditions
      if (result == AMD_OUT_OF_MEMORY)
	error ("amd: out of memory");
      else if (result == AMD_INVALID)
	error ("amd: input matrix A is corrupted");
      else
	{
	  // copy the outputs to Octave

	  // output permutation, P
	  NDArray perm (dim_vector (1, n));
	  for (int i = 0; i < n; i++)
	    perm (i) = double (P(i) + 1);  // 1-based indexing for Octave

	  retval (0) = perm;

	  // Info
	  if (nargout > 1)
	    retval (1) = info;
	}
    }
  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
