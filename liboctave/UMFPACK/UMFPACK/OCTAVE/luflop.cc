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

This is the Octave interface to the UMFPACK code, which bore the following
copyright

  UMFPACK Version 4.3 (Jan. 16, 2004), Copyright (c) 2004 by Timothy A.
  Davis.  All Rights Reserved.  See ../README for License.
  email: davis@cise.ufl.edu    CISE Department, Univ. of Florida.
  web: http://www.cise.ufl.edu/research/sparse/umfpack

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

DEFUN_DLD (luflop, args, nargout,
    "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{f} =} luflup (@var{l}, @var{u})\n\
\n\
Given an LU factorization, compute how many flops took to compute it. This\n\
is the same as (assuming U has a zero-free diagonal):\n\
\n\
@example\n\
@group\n\
  Lnz = full (sum (spones (L))) - 1 ;\n\
  Unz = full (sum (spones (U')))' - 1 ;\n\
  f = 2*Lnz*Unz + sum (Lnz) ;\n\
@end group\n\
@end example\n\
\n\
except that no extra workspace is allocated for spones (L) and spones (U).\n\
L and U must be sparse.\n\
\n\
Note: the above expression has a subtle undercount when exact numerical\n\
cancelation occurs.  Try [L,U,P] = lu (sparse (ones (10))) and then\n\
luflop (L,U).\n\
@end deftypefn")
{
  int nargin = args.length ();
  octave_value retval;

  // These are here only so that the C++ destructors don't prematurally
  // remove the underlying data we are interested in
  SparseMatrix Lmatrix, Umatrix;
  SparseComplexMatrix CLmatrix, CUmatrix;
  int *Lp, *Li, *Up, *Ui;
  int Lm, Ln, Um, Un;

  if (nargin != 2)
    {
      usage ("f = luflop (L, U)");
      return retval;
    }

  if (args(0).class_name () == "sparse")
    {
      if (args(0).is_complex_type ())
	{
	  CLmatrix = 
	    (((const octave_sparse_complex_matrix&) args(0).get_rep ())
	     .sparse_complex_matrix_value ());
	  Lp = CLmatrix.cidx ();
	  Li = CLmatrix.ridx ();
	  Lm = CLmatrix.rows ();
	  Ln = CLmatrix.cols ();
	}
      else
	{
	  Lmatrix = (((const octave_sparse_matrix&) args(0).get_rep ())
	     .sparse_matrix_value ());
	  Lp = Lmatrix.cidx ();
	  Li = Lmatrix.ridx ();
	  Lm = Lmatrix.rows ();
	  Ln = Lmatrix.cols ();
	}
    }
  else
    {
      if (args(0).is_complex_type ())
	{
	  CLmatrix =  SparseComplexMatrix (args(0).complex_matrix_value ());
	  Lp = CLmatrix.cidx ();
	  Li = CLmatrix.ridx ();
	  Lm = CLmatrix.rows ();
	  Ln = CLmatrix.cols ();
	}
      else
	{
	  Lmatrix = SparseMatrix (args(0).matrix_value ());
	  Lp = Lmatrix.cidx ();
	  Li = Lmatrix.ridx ();
	  Lm = Lmatrix.rows ();
	  Ln = Lmatrix.cols ();
	}
    }


  if (args(0).class_name () == "sparse")
    {
      if (args(1).is_complex_type ())
	{
	  CUmatrix = 
	    (((const octave_sparse_complex_matrix&) args(1).get_rep ())
	     .sparse_complex_matrix_value ());
	  Up = CUmatrix.cidx ();
	  Ui = CUmatrix.ridx ();
	  Um = CUmatrix.rows ();
	  Un = CUmatrix.cols ();
	}
      else
	{
	  Umatrix = 
	    (((const octave_sparse_matrix&) args(1).get_rep ())
	     .sparse_matrix_value ());
	  Up = Umatrix.cidx ();
	  Ui = Umatrix.ridx ();
	  Um = Umatrix.rows ();
	  Un = Umatrix.cols ();
	}
    }
  else
    {
      if (args(1).is_complex_type ())
	{
	  CUmatrix = SparseComplexMatrix (args(1).complex_matrix_value ());
	  Up = CUmatrix.cidx ();
	  Ui = CUmatrix.ridx ();
	  Um = CUmatrix.rows ();
	  Un = CUmatrix.cols ();
	}
      else
	{
	  Umatrix = SparseMatrix (args(1).matrix_value ());
	  Up = Umatrix.cidx ();
	  Ui = Umatrix.ridx ();
	  Um = Umatrix.rows ();
	  Un = Umatrix.cols ();
	}
    }


  if (error_state)
    return retval;

      
  int n = Lm;
      
  if (n != Ln || n != Um || n != Un)
    error ("luflop: L and U must be square");
  else
    {
	
      OCTAVE_LOCAL_BUFFER (int, Unz, n);

      // count the nonzeros in each row of U
      for (int row = 0 ; row < n ; row++)
	Unz [row] = 0 ;

      for (int col = 0 ; col < n ; col++)
	{
	  for (int p = Up [col] ; p < Up [col+1] ; p++)
	    {
	      int row = Ui [p] ;
	      Unz [row]++ ;
	    }
	}

      // count the flops
      double flop_count = 0.0 ;
      for (int k = 0 ; k < n ; k++)
	{
	  /* off-diagonal nonzeros in column k of L: */
	  int Lnz_k = Lp [k+1] - Lp [k] - 1 ;
	  int Unz_k = Unz [k] - 1 ;
	  flop_count += (2 * Lnz_k * Unz_k) + Lnz_k ;
	}

      // return the result
      retval = flop_count;
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
