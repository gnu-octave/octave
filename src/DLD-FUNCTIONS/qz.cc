/*

Copyright (C) 1998 A. S. Hodel

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

// Generalized eigenvalue balancing via LAPACK
// Written by A. S. Hodel <scotte@eng.auburn.edu>

#undef DEBUG
#undef DEBUG_SORT
#undef DEBUG_EIG

#include "config.h"

#include <cfloat>
#include <iostream.h>
#include <iomanip.h>
#include <math.h>
#include <string.h>
#include <strstream.h>

#include "CmplxQRP.h"
#include "dbleQR.h"
#include "defun-dld.h"
#include "error.h"
#include "f77-fcn.h"
#include "gripes.h"
#include "oct-obj.h"
#include "oct-map.h"
#include "ov.h"
#include "pager.h"
#if defined (DEBUG) || defined (DEBUG_SORT)
#include "pr-output.h"
#endif
#include "symtab.h"
#include "utils.h"
#include "variables.h"

typedef int (*sort_function) (const int& LSIZE, const double& ALPHA,
			      const double& BETA, const double& S,
			      const double& P);

extern "C"
{
  int F77_FCN (dggbal, DGGBAL) (const char* JOB, const int& N,
				double* A, const int& LDA, double* B,
				const int& LDB, int& ILO, int& IHI,
				double* LSCALE, double* RSCALE,
				double* WORK, int& INFO, long);

  int F77_FCN (dggbak, DGGBAK) (const char* JOB, const char* SIDE,
				const int& N, const int& ILO,
				const int& IHI, double* LSCALE,
				double* RSCALE, int& M, double* V,
				const int& LDV, int& INFO, long, long);

  int F77_FCN (dgghrd, DGGHRD) (const char* COMPQ, const char* COMPZ,
				const int& N, const int& ILO,
				const int& IHI, double* A,
				const int& LDA, double* B,
				const int& LDB, double* Q,
				const int& LDQ, double* Z,
				const int& LDZ, int& INFO, long, long);

  int F77_FCN (dhgeqz, DHGEQZ) (const char* JOB, const char* COMPQ,
				const char* COMPZ, const int& N,
				const int& ILO, const int& IHI,
				double* A, const int& LDA, double* B,
				const int& LDB, double* ALPHAR,
				double* ALPHAI, double* BETA, double* Q,
				const int& LDQ, double* Z,
				const int& LDZ, double* WORK,
				const int& LWORK, int& INFO,
				long, long, long);

  int F77_FCN (dlag2, DLAG2) (double* A, const int& LDA, double* B,
			      const int& LDB, const double& SAFMIN,
			      double& SCALE1, double& SCALE2,
			      double& WR1, double& WR2, double& WI);

  // Van Dooren's code (netlib.org: toms/590) for reordering
  // GEP.  Only processes Z, not Q.
  int F77_FCN (dsubsp, DSUBSP) (const int& NMAX, const int& N, double* A,
				double* B, double* Z, sort_function,
				const double& EPS, int& NDIM, int& FAIL,
				int* IND);

  // documentation for DTGEVC incorrectly states that VR, VL are
  // complex*16; they are declared in DTGEVC as double precision
  // (probably a cut and paste problem fro ZTGEVC)
  int F77_FCN (dtgevc, DTGEVC) (const char* SIDE, const char* HOWMNY,
				int* SELECT, const int& N, double* A,
				const int& LDA, double* B,
				const int& LDB, double* VL,
				const int& LDVL, double* VR,
				const int& LDVR, const int& MM,
				int& M, double* WORK, int& INFO,
				long, long);

  int F77_FCN (xdlamch, XDLAMCH) (const char* cmach, double& retval, long);

  int F77_FCN (xdlange, XDLANGE) (const char*, const int&,
                                  const int&, const double*,
                                  const int&, double*, double&);
}

// fcrhp, fin, fout, folhp:
// routines for ordering of generalized eigenvalues
// return 1 if  test is passed, 0 otherwise
//    fin: |lambda| < 1
//    fout: |lambda| >= 1
//    fcrhp: real(lambda) >= 0
//    folhp: real(lambda) < 0

static int
fcrhp (const int& lsize, const double& alpha,
       const double& beta, const double& s, const double&)
{
  if (lsize == 1)
    return (alpha*beta >= 0 ? 1 : -1);
  else
    return (s >= 0 ? 1 : -1);
}

static int
fin (const int& lsize, const double& alpha,
     const double& beta, const double&, const double& p)
{
  int retval;

  if (lsize == 1)
    retval = (fabs (alpha) < fabs (beta) ? 1 : -1);
  else
    retval = (fabs (p) < 1 ? 1 : -1);

#ifdef DEBUG
  cout << "qz: fin: retval=" << retval << endl;
#endif

  return retval;
}

static int
folhp (const int& lsize, const double& alpha,
       const double& beta, const double& s, const double&)
{
  if (lsize == 1)
    return (alpha*beta < 0 ? 1 : -1);
  else
    return (s < 0 ? 1 : -1);
}

static int
fout (const int& lsize, const double& alpha,
      const double& beta, const double&, const double& p)
{
  if (lsize == 1)
    return (fabs (alpha) >= fabs (beta) ? 1 : -1);
  else
    return (fabs (p) >= 1 ? 1 : -1);
}

DEFUN_DLD (qz, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{lambda} =} qz (@var{a}, @var{b})\n\
Generalized eigenvalue problem @math{A x = s B x},\n\
@var{QZ} decomposition.  Three ways to call:\n\
@enumerate\n\
@item @code{lambda = qz(A,B)}\n\
\n\
Computes the generalized eigenvalues @var{lambda} of @math{(A - sB)}.\n\
\n\
@item @code{[AA, BB, Q, Z @{, V, W, lambda@}] = qz (A, B)}\n\
\n\
Computes qz decomposition, generalized eigenvectors, and \n\
        generalized eigenvalues of @math{(A - sB)}\n\
@example\n\
@group\n\
        A V = B V diag(lambda)\n\
        W' A = diag(lambda) W' B\n\
        AA = Q'*A*Z, BB = Q'*B*Z  with Q, Z orthogonal (unitary)= I\n\
@end group\n\
@end example\n\
\n\
@item @code{[AA,BB,Z@{,lambda@}] = qz(A,B,opt)}\n\
\n\
As in form [2], but allows ordering of generalized eigenpairs\n\
        for (e.g.) solution of discrete time algebraic Riccati equations.\n\
        Form 3 is not available for complex matrices and does not compute\n\
        the generalized eigenvectors V, W, nor the orthogonal matrix Q.\n\
@table @var\n\
@item opt\n\
 for ordering eigenvalues of the GEP pencil.  The leading  block\n\
             of the revised pencil contains all eigenvalues that satisfy:\n\
@table @code\n\
@item \"N\"\n\
 = unordered (default) \n\
\n\
@item \"S\"\n\
= small: leading block has all |lambda| <=1 \n\
\n\
@item \"B\"\n\
 = big: leading block has all |lambda >= 1 \n\
\n\
@item \"-\"\n\
 = negative real part: leading  block has all eigenvalues\n\
                  in the open left half-plant\n\
\n\
@item \"+\"\n\
 = nonnegative real part:  leading block has all eigenvalues\n\
                  in the closed right half-plane\n\
@end  table\n\
@end table\n\
@end enumerate\n\
\n\
Note: qz performs permutation balancing, but not scaling (see balance).\n\
      Order of output arguments was selected for compatibility with MATLAB\n\
\n\
See also: balance, dare, eig, schur\n\
@end deftypefn")
{
  octave_value_list retval;
  int nargin = args.length ();

#ifdef DEBUG
  cout << "qz: nargin = " << nargin << ", nargout = " << nargout << endl;
#endif

  if (nargin < 2 || nargin > 3 || nargout > 7)
    {
      print_usage ("qz");
      return retval;
    }
  else if (nargin == 3 && (nargout < 3 || nargout > 4))
    {
      error ("qz: invalid number of output arguments for form [3] call");
      return retval;
    }

#ifdef DEBUG
  cout << "qz: determine ordering option" << endl;
#endif

  // Determine ordering option
  string ord_job;
  static double safmin;

  if (nargin == 2)
    ord_job = "N";
  else if (!args(2).is_string ())
    {
      error ("qz: argument 3 must be a string");
      return retval;
    }
  else
    {
      ord_job = args(2).string_value ();

      if (ord_job[0] != 'N'
	  && ord_job[0] != 'S'
	  && ord_job[0] != 'B'
	  && ord_job[0] != '+'
	  && ord_job[0] != '-')
	{
	  error ("qz: invalid order option");
	  return retval;
	}

      // overflow constant required by dlag2
      F77_FCN (xdlamch, XDLAMCH) ("S", safmin, 1L);

#ifdef DEBUG_EIG
      cout << "qz: initial value of safmin=" << setiosflags (ios::scientific)
	   << safmin << endl;
#endif

      // some machines (e.g., DEC alpha) get safmin = 0;
      // for these, use eps instead to avoid problems in dlag2
      if (safmin == 0)
	{
#ifdef DEBUG_EIG
	  cout << "qz: DANGER WILL ROBINSON: safmin is 0!" << endl;
#endif

	  F77_FCN (xdlamch, XDLAMCH) ("E", safmin, 1L);

#ifdef DEBUG_EIG
	  cout << "qz: safmin set to " << setiosflags (ios::scientific)
	       << safmin << endl;
#endif
	}
    }

#ifdef DEBUG
  cout << "qz: check argument 1" << endl;
#endif

  // Argument 1: check if it's o.k. dimensioned
  int nn = args(0).rows ();

#ifdef DEBUG
  cout << "argument 1 dimensions: (" << nn << "," << args(0).columns () << ")"
       << endl;
#endif

  int arg_is_empty = empty_arg ("qz", nn, args(0).columns ());

  if (arg_is_empty < 0)
    {
      gripe_empty_arg ("qz: parameter 1", 0);
      return retval;
    }
  else if (arg_is_empty > 0)
    {
      gripe_empty_arg ("qz: parameter 1; continuing", 0);
      return octave_value_list (2, Matrix ());
    }
  else if (args(0).columns () != nn)
    {
      gripe_square_matrix_required ("qz");
      return retval;
    }

  // Argument 1: dimensions look good; get the value
  Matrix aa;
  ComplexMatrix caa;

  if (args(0).is_complex_type ())
    caa = args(0).complex_matrix_value ();
  else
    aa = args(0).matrix_value ();

  if (error_state)
    return retval;

#ifdef DEBUG
  cout << "qz: check argument 2" << endl;
#endif

  // Extract argument 2 (bb, or cbb if complex)
  if ((nn != args(1).columns ()) || (nn != args(1).rows ()))
    {
      gripe_nonconformant ();
      return retval;
    }

  Matrix bb;
  ComplexMatrix cbb;

  if (args(1).is_complex_type ())
    cbb = args(1).complex_matrix_value ();
  else
    bb = args(1).matrix_value ();

  if (error_state)
    return retval;

  // Both matrices loaded, now let's check what kind of arithmetic:
  //declared static to avoid compiler warnings about long jumps, vforks.

  static int complex_case
    = (args(0).is_complex_type () || args(1).is_complex_type ());

  if (nargin == 3 && complex_case)
    {
      error ("qz: cannot re-order complex qz decomposition.");
      return retval;
    }

  // first, declare variables used in both the real and complex case
  Matrix QQ(nn,nn), ZZ(nn,nn), VR(nn,nn), VL(nn,nn);
  RowVector alphar(nn), alphai(nn), betar(nn);

  ComplexMatrix CQ(nn,nn), CZ(nn,nn), CVR(nn,nn), CVL(nn,nn);
  int ilo, ihi, info;
  char compq = (nargout >= 3 ? 'V' : 'N');
  char compz = (nargout >= 4 ? 'V' : 'N');

  // initialize Q, Z to identity if we need either of them
  if (compq == 'V' || compz == 'V')
    for (int ii = 0; ii < nn; ii++)
      for (int jj = 0; jj < nn; jj++)
        QQ(ii,jj) = ZZ(ii,jj) = (ii == jj ? 1.0 : 0.0);

  // always perform permutation balancing
  char bal_job = 'P';
  RowVector lscale(nn), rscale(nn), work(6*nn);

  if (complex_case)
    {
      error ("Complex case not implemented yet");
      return retval;
    }
  else
    {
#ifdef DEBUG
      if (compq == 'V')
	cout << "qz: performing balancing; QQ=" << endl << QQ << endl;
#endif

      F77_XFCN (dggbal, DGGBAL,
		(&bal_job,  nn, aa.fortran_vec(), nn, bb.fortran_vec(),
		 nn, ilo, ihi, lscale.fortran_vec(),
		 rscale.fortran_vec(), work.fortran_vec(), info, 1L));

      if (f77_exception_encountered)
	{
	  error ("unrecoverable error in qz (bal)");
	  return retval;
	}
    }

  // Since we just want the balancing matrices, we can use dggbal
  // for both the real and complex cases;
  // left first

  if (compq == 'V')
    {
      F77_XFCN (dggbak, DGGBAK,
		(&bal_job, "L", nn, ilo, ihi, lscale.fortran_vec(),
		 rscale.fortran_vec(), nn, QQ.fortran_vec(),
		 nn, info, 1L, 1L));

#ifdef DEBUG
      if (compq == 'V')
	cout << "qz: balancing done; QQ=" << endl << QQ << endl;
#endif

    if (f77_exception_encountered)
      {
	error ("unrecoverable error in qz (bal-L)");
	return retval;
      }
  }

  // then right
  if (compz == 'V')
    {
      F77_XFCN (dggbak, DGGBAK, (&bal_job, "R",
				 nn, ilo, ihi, lscale.fortran_vec(),
				 rscale.fortran_vec(), nn, ZZ.fortran_vec(),
				 nn, info, 1L, 1L));

#ifdef DEBUG
      if (compz == 'V')
	cout << "qz: balancing done; ZZ=" << endl << ZZ << endl;
#endif

      if (f77_exception_encountered)
	{
	  error ("unrecoverable error in qz (bal-R)");
	  return retval;
	}
    }

  static char qz_job;
  qz_job = (nargout < 2 ? 'E' : 'S');	

  if (complex_case)
    {
      // complex case
      if (args(0).is_real_type ())
	caa = aa;

      if (args(1).is_real_type ())
	cbb = bb;

      if (compq == 'V')
	CQ = QQ;

      if (compz == 'V')
	CZ = ZZ;

      error ("complex case not done yet");
      return retval;
    }
  else  	// real matrices case
    {
#ifdef DEBUG
      cout << "qz: peforming qr decomposition of bb" << endl;
#endif

      // compute the QR factorization of bb
      QR bqr (bb);

#ifdef DEBUG
      cout << "qz: qr (bb) done; now peforming qz decomposition" << endl;
#endif

      bb = bqr.R ();

#ifdef DEBUG
      cout << "qz: extracted bb" << endl;
#endif

      aa = (bqr.Q ()).transpose ()*aa;

#ifdef DEBUG
      cout << "qz: updated aa " << endl;
      cout << "bqr.Q () = " << endl << bqr.Q () << endl;

      if (compq == 'V')
	cout << "QQ =" << QQ << endl;
#endif

      if (compq == 'V')
	QQ = QQ*bqr.Q ();

#ifdef DEBUG
      cout << "qz: precursors done..." << endl;
#endif

#ifdef DEBUG
      cout << "qz: compq = " << compq << ", compz = " << compz << endl;
#endif

      // reduce  to generalized hessenberg form
      F77_XFCN (dgghrd, DGGHRD,
		(&compq, &compz, nn, ilo, ihi, aa.fortran_vec(),
		 nn, bb.fortran_vec(), nn, QQ.fortran_vec(), nn,
		 ZZ.fortran_vec(), nn, info, 1L, 1L));

      if (f77_exception_encountered)
	{
	  error ("unrecoverable error in qz (dgghrd)");
	  return retval;
	}

      // check if just computing generalized eigenvalues or if we're
      // actually computing the decomposition

      // reduce to generalized Schur form
      F77_XFCN (dhgeqz, DHGEQZ,
		(&qz_job, &compq, &compz, nn, ilo, ihi,
		 aa.fortran_vec(), nn, bb.fortran_vec(), nn,
		 alphar.fortran_vec(), alphai.fortran_vec(),
		 betar.fortran_vec(), QQ.fortran_vec(), nn,
		 ZZ.fortran_vec(), nn, work.fortran_vec(), nn, info,
		 1L, 1L, 1L));

      if (f77_exception_encountered)
	{
	  error ("unrecoverable error in qz (dhgeqz)");
	  return retval;
	}
    }

  // order the QZ decomposition?
  if (ord_job[0] != 'N')
    {
      if (complex_case)
	{
	  // probably not needed, but better be safe
	  error ("qz: cannot re-order complex qz decomposition.");
	  return retval;
	}
      else
	{
#ifdef DEBUG_SORT
	  cout << "qz: ordering eigenvalues: ord_job = " << ord_job[0] << endl;
#endif

	  // declared static to avoid vfork/long jump compiler complaints
	  static sort_function sort_test;
	  sort_test = NULL;

	  switch (ord_job[0])
	    {
	    case 'S':
	      sort_test = &fin;
	      break;

	    case 'B':
	      sort_test = &fout;
	      break;

	    case '+':
	      sort_test = &fcrhp;
	      break;

	    case '-':
	      sort_test = &folhp;
	      break;

	    default:
	      // invalid order option (should never happen, since we
	      // checked the options at the top).
	      panic_impossible ();
	      break;
      }

	  int ndim, fail, ind[nn];
	  double inf_norm;

	  F77_XFCN (xdlange, XDLANGE,
		    ("I", nn, nn, aa.fortran_vec (), nn,
		     work.fortran_vec (), inf_norm));

	  double eps = DBL_EPSILON*inf_norm*nn;

#ifdef DEBUG_SORT
	  cout << "qz: calling dsubsp: aa=" << endl;
	  octave_print_internal (cout, aa, 0);
	  cout << endl << "bb="  << endl;
	  octave_print_internal (cout, bb, 0);
	  if (compz == 'V')
	    {
	      cout << endl << "ZZ="  << endl;
	      octave_print_internal (cout, ZZ, 0);
	    }
	  cout << endl;
	  cout << "alphar = " << endl;
	  octave_print_internal (cout, (Matrix) alphar, 0);
	  cout << endl << "alphai = " << endl;
	  octave_print_internal (cout, (Matrix) alphai, 0);
	  cout << endl << "beta = " << endl;
	  octave_print_internal (cout, (Matrix) betar, 0);
	  cout << endl;
#endif

	  F77_XFCN (dsubsp, DSUBSP,
		    (nn, nn, aa.fortran_vec(), bb.fortran_vec(),
		     ZZ.fortran_vec(), sort_test, eps, ndim, fail, ind));

#ifdef DEBUG
	  cout << "qz: back from dsubsp: aa=" << endl;
	  octave_print_internal (cout, aa, 0);
	  cout << endl << "bb="  << endl;
	  octave_print_internal (cout, bb, 0);
	  if (compz == 'V')
	    {
	      cout << endl << "ZZ="  << endl;
	      octave_print_internal (cout, ZZ, 0);
	    }
	  cout << endl;
#endif

	  // manually update alphar, alphai, betar
	  static int jj;

	  jj=0;
	  while (jj < nn)
	    {
#ifdef DEBUG_EIG
	      cout << "computing gen eig #" << jj << endl;
#endif

	      static int zcnt;	// number of zeros in this block

	      if (jj == (nn-1))
		zcnt = 1;
	      else if (aa(jj+1,jj) == 0)
		zcnt = 1;
	      else zcnt = 2;

	      if (zcnt == 1)  // real zero
		{
#ifdef DEBUG_EIG
		  cout << "  single gen eig:" << endl;
		  cout << "  alphar(" << jj << ") = " << aa(jj,jj) << endl;
		  cout << "  betar( " << jj << ") = " << bb(jj,jj) << endl;
		  cout << "  alphai(" << jj << ") = 0" << endl;
#endif

		  alphar(jj) = aa(jj,jj);
		  alphai(jj) = 0;
		  betar(jj) = bb(jj,jj);
		}
	      else
		{
		  // complex conjugate pair
#ifdef DEBUG_EIG
		  cout << "qz: calling dlag2:" << endl;
		  cout << "safmin="
		       << setiosflags (ios::scientific) << safmin << endl;

		  for (int idr = jj; idr <= jj+1; idr++)
		    {
		      for (int idc = jj; idc <= jj+1; idc++)
			{
			  cout << "aa(" << idr << "," << idc << ")="
			       << aa(idr,idc) << endl;
			  cout << "bb(" << idr << "," << idc << ")="
			       << bb(idr,idc) << endl;
			}
		    }
#endif

		  double scale1, scale2, wr1, wr2, wi;
		  F77_XFCN (dlag2, DLAG2,
			    (&aa(jj,jj), nn, &bb(jj,jj), nn, safmin,
			     scale1, scale2, wr1, wr2, wi));

#ifdef DEBUG_EIG
		  cout << "dlag2 returns: scale1=" << scale1
		       << "\tscale2=" << scale2 << endl
		       << "\twr1=" << wr1 << "\twr2=" << wr2
		       << "\twi=" << wi << endl;
#endif

		  // just to be safe, check if it's a real pair
		  if (wi == 0)
		    {
		      alphar(jj) = wr1;
		      alphai(jj) = 0;
		      betar(jj) = scale1;
		      alphar(jj+1) = wr2;
		      alphai(jj+1) = 0;
		      betar(jj+1) = scale2;
		    }
		  else
		    {
		      alphar(jj) = alphar(jj+1)=wr1;
		      alphai(jj) = -(alphai(jj+1) = wi);
		      betar(jj)  = betar(jj+1) = scale1;
		    }
		}

	      // advance past this block
	      jj += zcnt;
	    }

#ifdef DEBUG_SORT
	  cout << "qz: back from dsubsp: aa=" << endl;
	  octave_print_internal (cout, aa, 0);
	  cout << endl << "bb="  << endl;
	  octave_print_internal (cout, bb, 0);

	  if (compz == 'V')
	    {
	      cout << endl << "ZZ="  << endl;
	      octave_print_internal (cout, ZZ, 0);
	    }
	  cout << endl << "qz: ndim=" << ndim << endl
	       << "fail=" << fail << endl;
	  cout << "alphar = " << endl;
	  octave_print_internal (cout, (Matrix) alphar, 0);
	  cout << endl << "alphai = " << endl;
	  octave_print_internal (cout, (Matrix) alphai, 0);
	  cout << endl << "beta = " << endl;
	  octave_print_internal (cout, (Matrix) betar, 0);
	  cout << endl;
#endif
	}
    }

  // compute  generalized eigenvalues?
  ComplexColumnVector gev;

  if (nargout < 2 || nargout == 7 || (nargin == 3 && nargout == 4))
    {
      if (complex_case)
	{
	  error ("complex case not yet implemented");
	  return retval;
	}
      else
	{
#ifdef DEBUG
	  cout << "qz: computing generalized eigenvalues" << endl;
#endif

	  // return finite generalized eigenvalues
	  int cnt = 0;

	  for (int ii = 0; ii < nn; ii++)
	    if (betar(ii) != 0)
	      cnt++;

	  ComplexColumnVector tmp(cnt);

	  for (int ii = 0; ii < nn; ii++)
	    if (betar(ii) != 0)
	      tmp(ii) = Complex(alphar(ii), alphai(ii))/betar(ii);
	  gev = tmp;
	}
    }

  // right, left eigenvector matrices
  if (nargout >= 5)
    {
      char side = (nargout == 5 ? 'R' : 'B');	// which side to compute?
      char howmny = 'B';  // compute all of them and backtransform
      int *select = NULL; // dummy pointer; select is not used.
      int m;

      if (complex_case)
	{
	  error ("complex type not yet implemented");
	  return retval;
	}
      else
	{
#ifdef DEBUG
	  cout << "qz: computing  generalized eigenvectors" << endl;
#endif

	  VL = QQ;
	  VR = ZZ;

	  F77_XFCN (dtgevc, DTGEVC,
		    (&side, &howmny, select, nn, aa.fortran_vec(),
		     nn, bb.fortran_vec(), nn, VL.fortran_vec(), nn,
		     VR.fortran_vec(), nn, nn, m, work.fortran_vec(),
		     info, 1L, 1L));

	  if (f77_exception_encountered)
	    {
	      error ("unrecoverable error in qz (dtgevc)");
	      return retval;
	    }

	  // now construct the complex form of VV, WW
	  int jj = 0;

	  while (jj < nn)
	    {
	      // see if real or complex eigenvalue
	      int cinc = 2;	// column increment; assume complex eigenvalue

	      if (jj == (nn-1))
		cinc = 1;	// single column
	      else if (aa(jj+1,jj) == 0)
		cinc = 1;

	      // now copy the eigenvector (s) to CVR, CVL
	      if (cinc == 1)
		{
		  for (int ii = 0; ii < nn; ii++)
		    CVR(ii,jj) = VR(ii,jj);

		  if (side == 'B')
		    for (int ii = 0; ii < nn; ii++)
		      CVL(ii,jj) = VL(ii,jj);
		}
	      else
		{
		  // double column; complex vector

		  for (int ii = 0; ii < nn; ii++)
		    {
		      CVR(ii,jj) = Complex (VR(ii,jj), VR(ii,jj+1));
		      CVR(ii,jj+1) = Complex (VR(ii,jj), -VR(ii,jj+1));
		    }

		  if (side == 'B')
		    for (int ii = 0; ii < nn; ii++)
		      {
			CVL(ii,jj) = Complex (VL(ii,jj), VL(ii,jj+1));
			CVL(ii,jj+1) = Complex (VL(ii,jj), -VL(ii,jj+1));
		      }
		}

	      // advance to next eigenvectors (if any)
	      jj += cinc;
	    }
	}
    }

  switch (nargout)
    {
    case 7:
      retval(6) = gev;

    case 6:	// return eigenvectors
      retval(5) = CVL;

    case 5:	// return eigenvectors
      retval(4) = CVR;

    case 4:
      if (nargin == 3)
	{
#ifdef DEBUG
	  cout << "qz: sort: retval(3) = gev = " << endl;
	  octave_print_internal (cout, gev);
	  cout << endl;
#endif
	  retval(3) = gev;
	}
      else
	retval(3) = ZZ;

    case 3:
      if (nargin == 3)
	retval(2) = ZZ;
      else
	retval(2) = QQ;

    case 2:
#ifdef DEBUG
      cout << "qz: retval (1) = bb = " << endl;
      octave_print_internal (cout, bb, 0);
      cout << endl << "qz: retval(0) = aa = " <<endl;
      octave_print_internal (cout, aa, 0);
      cout << endl;
#endif
      retval(1) = bb;
      retval(0) = aa;
      break;

    case 1:
    case 0:
#ifdef DEBUG
      cout << "qz: retval(0) = gev = " << gev << endl;
#endif
      retval(0) = gev;
      break;

    default:
      error ("qz: too many return arguments.");
      break;
  }

#ifdef DEBUG
  cout << "qz: exiting (at long last)" << endl;
#endif

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
