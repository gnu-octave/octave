/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

// Written by A. S. Hodel <scotte@eng.auburn.edu>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>

#include "CmplxAEPBAL.h"
#include "CmplxAEPBAL.h"
#include "dbleAEPBAL.h"
#include "dbleAEPBAL.h"

#include "defun-dld.h"
#include "error.h"
#include "f77-fcn.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

extern "C"
{
  int F77_FCN( dggbal, DGGBAL) (const char* JOB,  const int& N,
        double* A, const int& LDA, double* B, const int& LDB,
        int& ILO, int & IHI, double* LSCALE,
        double* RSCALE, double* WORK, int& INFO, long );

  int F77_FCN( dggbak, DGGBAK) (const char* JOB, const char* SIDE,
        const int& N, const int& ILO, const int& IHI,
        double* LSCALE, double* RSCALE, int& M,
        double* V, const int& LDV, int& INFO, long, long);

  int F77_FCN( zggbal, ZGGBAL) ( const char* JOB,  const int& N,
        Complex* A, const int& LDA, Complex* B, const int& LDB,
        int& ILO, int & IHI, double* LSCALE,
        double* RSCALE, double* WORK, int& INFO, long );
}

DEFUN_DLD (balance, args, nargout,
  "AA = balance (A [, OPT]) or [[DD,] AA] =  balance (A [, OPT])\n\
\n\
generalized eigenvalue problem:\n\
\n\
  [cc, dd, aa, bb] = balance (a, b [, opt])\n\
\n\
where OPT is an optional single character argument as follows: \n\
\n\
  N: no balancing; arguments copied, transformation(s) set to identity\n\
  P: permute argument(s) to isolate eigenvalues where possible\n\
  S: scale to improve accuracy of computed eigenvalues\n\
  B: (default) permute and scale, in that order.  Rows/columns\n\
     of a (and b) that are isolated by permutation are not scaled\n\
\n\
[DD, AA] = balance (A, OPT) returns aa = dd*a*dd,\n\
\n\
[CC, DD, AA, BB] = balance (A, B, OPT) returns AA (BB) = CC*A*DD (CC*B*DD)")
{

  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 3 || nargout < 0 || nargout > 4)
    {
      print_usage ("balance");
      return retval;
    }

  // determine if it's AEP or GEP
  int AEPcase = (nargin == 1 ? 1 : args(1).is_string() );
  string bal_job;

  // problem dimension
  int nn = args(0).rows ();

  int arg_is_empty = empty_arg ("balance", nn, args(0).columns());

  if (arg_is_empty < 0)
    return retval;
  if (arg_is_empty > 0)
    return octave_value_list (2, Matrix ());

  if (nn != args(0).columns())
  {
    gripe_square_matrix_required ("balance");
    return retval;
  }

  // Extract argument 1 parameter for both AEP and GEP.
  Matrix aa;
  ComplexMatrix caa;
  if (args(0).is_complex_type ()) caa = args(0).complex_matrix_value ();
  else                            aa = args(0).matrix_value ();
  if (error_state)                return retval;

  // Treat AEP/GEP cases.
  if(AEPcase)
  {   
    // Algebraic eigenvalue problem.
    if(nargin == 1)
      bal_job = "B";
    else if(args(1).is_string())
      bal_job = args(1).string_value();
    // the next line should never execute, but better safe than sorry.
    else error("balance: AEP argument 2 must be a string");

    // balance the AEP
    if (args(0).is_complex_type ())
    {
      ComplexAEPBALANCE result (caa, bal_job);

      if (nargout == 0 || nargout == 1)
        retval(0) = result.balanced_matrix ();
      else
      {
        retval(1) = result.balanced_matrix ();
        retval(0) = result.balancing_matrix ();
      }
    }
    else
    {
      AEPBALANCE result (aa, bal_job);

      if (nargout == 0 || nargout == 1)
        retval(0) = result.balanced_matrix ();
      else
      {
        retval(1) = result.balanced_matrix ();
        retval(0) = result.balancing_matrix ();
       }
    }
  }
  //
  // end of AEP case, now do GEP case
  else
  {
    // Generalized eigenvalue problem.
    if(nargin == 2)
      bal_job = "B";
    else if(args(2).is_string())
      bal_job = args(2).string_value();
    else error("balance: GEP argument 3 must be a string");

    if( (nn != args(1).columns()) || (nn != args(1).rows() ) )
    {
      gripe_nonconformant ();
      return retval;
    }
    Matrix bb;
    ComplexMatrix cbb;
    if (args(1).is_complex_type ()) cbb = args(1).complex_matrix_value ();
    else                            bb = args(1).matrix_value ();
    if (error_state) return retval;

    //
    // Both matrices loaded, now let's check what kind of arithmetic:
    // first, declare variables used in both the real and complex case
    int ilo, ihi, info;
    RowVector lscale(nn), rscale(nn), work(6*nn);
    char job = bal_job[0];
    static int complex_case = (args(0).is_complex_type() 
                       || args(1).is_complex_type());

    // now balance
    if (complex_case)
    {
      if (args(0).is_real_type ()) caa = aa;
      if (args(1).is_real_type ()) cbb = bb;
  
      F77_XFCN( zggbal, ZGGBAL, ( &job, nn, caa.fortran_vec(), nn,
          cbb.fortran_vec(), nn, ilo, ihi, lscale.fortran_vec(),
          rscale.fortran_vec(), work.fortran_vec(), info, 1L));
    }
    else          // real matrices case
    {
      F77_XFCN( dggbal, DGGBAL, (&job,  nn, aa.fortran_vec(),
          nn, bb.fortran_vec() , nn, ilo, ihi, lscale.fortran_vec(),
          rscale.fortran_vec(), work.fortran_vec(), info , 1L));
      
      if(f77_exception_encountered)
        (*current_liboctave_error_handler) 
         ("unrecoverable error in balance GEP");
    }
      
    // Since we just want the balancing matrices, we can use dggbal
    // for both the real and complex cases;
    Matrix Pl(nn,nn), Pr(nn,nn);
    for(int ii=0; ii < nn ; ii++)
      for( int jj=0; jj < nn ; jj++)
        Pl(ii,jj) = Pr(ii,jj) = (ii == jj ? 1.0 : 0.0);
  
    // left first
    F77_XFCN( dggbak, DGGBAK, (&job, "L",
          nn, ilo, ihi, lscale.fortran_vec(),
          rscale.fortran_vec(), nn, Pl.fortran_vec(),
          nn, info, 1L, 1L));
      
    if(f77_exception_encountered)
      (*current_liboctave_error_handler) 
        ("unrecoverable error in balance GEP(L)");
      
    // then right
    F77_XFCN(dggbak, DGGBAK, (&job, "R",
          nn, ilo, ihi, lscale.fortran_vec(),
          rscale.fortran_vec(), nn, Pr.fortran_vec(),
          nn, info, 1L, 1L));
    if(f77_exception_encountered)
      (*current_liboctave_error_handler) 
        ("unrecoverable error in balance GEP(R)");

    switch (nargout)
    {
    case 0:
    case 1:
      warning ("balance: used GEP, should have two output arguments");
      if(complex_case)
        retval(0) = caa;
      else
        retval(0) = aa;
      break;

    case 2:
      if(complex_case)
      {
        retval(1) = cbb;
        retval(0) = caa;
      }
      else
      {
        retval(1) = bb;
        retval(0) = aa;
      }
      break;

    case 4:
      if(complex_case)
      {
        retval(3) = cbb;
        retval(2) = caa;
      }
      else
      {
        retval(3) = bb;
        retval(2) = aa;
      }
      retval(1) = Pr;
      retval(0) = Pl.transpose();  // so that aa_bal = cc*aa*dd, etc.
      break;

    default:
      error ("balance: invalid number of output arguments");
      break;
    }
  }
  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
