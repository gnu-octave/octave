// Extra Matrix manipulations.                           -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <iostream.h>
#include <float.h>

#include "Matrix.h"
#include "mx-inlines.cc"
#include "lo-error.h"
#include "f77-uscore.h"

// Fortran functions we call.

extern "C"
{
  int F77_FCN (dgesv) (const int*, const int*, double*, const int*,
		       int*, double*, const int*, int*);

  int F77_FCN (dgeqrf) (const int*, const int*, double*, const int*,
			double*, double*, const int*, int*);

  int F77_FCN (dorgqr) (const int*, const int*, const int*, double*,
			const int*, double*, double*, const int*, int*);

  int F77_FCN (dgeev) (const char*, const char*, const int*, double*,
		       const int*, double*, double*, double*,
		       const int*, double*, const int*, double*,
		       const int*, int*, long, long);

  int F77_FCN (dgeesx) (const char*, const char*,
			int (*)(double*, double*), const char*,
			const int*, double*, const int*, int*, double*,
			double*, double*, const int*, double*, double*, 
			double*, const int*, int*, const int*, int*,
			int*, long, long);

  int F77_FCN (dgebal) (const char*, const int*, double*,
                        const int*, int*, int*, double*,
                        int*, long, long);

  int F77_FCN (dgebak) (const char*, const char*, const int*, const int*,
			const int*, double*, const int*, double*, const int*,
			int*, long, long);

  int F77_FCN (dgehrd) (const int*, const int*, const int*,
                        double*, const int*, double*, double*,
                        const int*, int*, long, long);

  int F77_FCN (dorghr) (const int*, const int*, const int*,
                        double*, const int*, double*, double*,
                        const int*, int*, long, long);

  int F77_FCN (dgesvd) (const char*, const char*, const int*,
			const int*, double*, const int*, double*,
			double*, const int*, double*, const int*,
			double*, const int*, int*, long, long);

  int F77_FCN (dpotrf) (const char*, const int*, double*, const int*,
			int*, long);

//
// fortran functions for generalized eigenvalue problems
//
  int F77_FCN (reduce) (const int*, const int*, double*,
	   	        const int*, double*,
			int*, int*, double*, double*);

  int F77_FCN (scaleg) (const int*, const int*, double*,
	   	        const int*, double*,
			const int*, const int*, double*, double*, double*);

  int F77_FCN (gradeq) (const int*, const int*, double*,
	   	        const int*, double*,
			int*, int*, double*, double*);

/*
 * f2c translates complex*16 as
 *
 *   typedef struct { doublereal re, im; } doublecomplex;
 *
 * and Complex.h from libg++ uses
 *
 *   protected:
 *     double re;
 *     double im;
 *
 * as the only data members, so this should work (fingers crossed that
 * things don't change).
 */

  int F77_FCN (zgesv) (const int*, const int*, Complex*, const int*,
		       int*, Complex*, const int*, int*);

  int F77_FCN (zgeqrf) (const int*, const int*, Complex*, const int*,
			Complex*, Complex*, const int*, int*);

  int F77_FCN (zgeesx) (const char*, const char*, int (*)(Complex*),
			const char*, const int*, Complex*, const int*,
			int*, Complex*, Complex*, const int*, double*,
			double*, Complex*, const int*, double*, int*,
			int*, long, long);

  int F77_FCN (zgebal) (const char*, const int*, Complex*, const int*,
                        int*, int*, double*, int*, long, long);
 
  int F77_FCN (zgebak) (const char*, const char*, const int*, const int*,
			const int*, double*, const int*, Complex*, 
			const int*, int*, long, long);

  int F77_FCN (zgehrd) (const int*, const int*, const int*, Complex*,
                        const int*, Complex*, Complex*, const int*,
                        int*, long, long);
 
  int F77_FCN (zunghr) (const int*, const int*, const int*, Complex*,
                        const int*, Complex*, Complex*, const int*,
                        int*, long, long);

  int F77_FCN (zungqr) (const int*, const int*, const int*, Complex*,
			const int*, Complex*, Complex*, const int*, int*);

  int F77_FCN (zgeev) (const char*, const char*, const int*, Complex*,
		       const int*, Complex*, Complex*, const int*,
		       Complex*, const int*, Complex*, const int*,
		       double*, int*, long, long);

  int F77_FCN (zgesvd) (const char*, const char*, const int*,
			const int*, Complex*, const int*, double*,
			Complex*, const int*, Complex*, const int*,
			Complex*, const int*, double*, int*, long, long);

  int F77_FCN (zpotrf) (const char*, const int*, Complex*, const int*,
			int*, long);
}

/*
 * AEPBALANCE operations
 */

int
AEPBALANCE::init (const Matrix& a, const char *balance_job)
{
  int a_nc = a.cols ();
  if (a.rows () != a_nc)
    {
      (*current_liboctave_error_handler) ("AEPBALANCE requires square matrix");
      return -1;
    }

  int n = a_nc;

// Parameters for balance call.

  int info;
  int ilo;
  int ihi;
  double *scale = new double [n];

// Copy matrix into local structure.

  balanced_mat = a;

  F77_FCN (dgebal) (balance_job, &n, balanced_mat.fortran_vec (), 
		    &n, &ilo, &ihi, scale, &info, 1L, 1L);

// Initialize balancing matrix to identity.

  balancing_mat = Matrix (n, n, 0.0);
  for (int i = 0; i < n; i++)
    balancing_mat.elem (i ,i) = 1.0;

  F77_FCN (dgebak) (balance_job, "R", &n, &ilo, &ihi, scale, &n, 
		    balancing_mat.fortran_vec (), &n, &info, 1L, 1L);

  delete [] scale;

  return info;
}

int
ComplexAEPBALANCE::init (const ComplexMatrix& a, const char *balance_job)
{

  int n = a.cols ();

// Parameters for balance call.

  int info;
  int ilo;
  int ihi;
  double *scale = new double [n];

// Copy matrix into local structure.

  balanced_mat = a;

  F77_FCN (zgebal) (balance_job, &n, balanced_mat.fortran_vec (),
		    &n, &ilo, &ihi, scale, &info, 1L, 1L);

// Initialize balancing matrix to identity.

  balancing_mat = Matrix (n, n, 0.0);
  for (int i = 0; i < n; i++)
    balancing_mat (i, i) = 1.0;

  F77_FCN (zgebak) (balance_job, "R", &n, &ilo, &ihi, scale, &n, 
		    balancing_mat.fortran_vec (), &n, &info, 1L, 1L);

  delete [] scale;

  return info;
}

/*
 * GEPBALANCE operations
 */

int
GEPBALANCE::init (const Matrix& a, const Matrix& b, const char *balance_job)
{
  int a_nr = a.rows ();
  int a_nc = a.cols ();
  int b_nr = b.rows ();
  if (a_nr != a_nc || a_nr != b_nr || b_nr != b.cols ())
    {
      (*current_liboctave_error_handler)
	("GEPBALANCE requires square matrices of the same size");
      return -1;
    }

  int n = a_nc;

// Parameters for balance call.

  int info;
  int ilo;
  int ihi;
  double *cscale = new double [n];
  double *cperm = new double [n];
  Matrix wk (n, 6, 0.0);

// Back out the permutations:
//
// cscale contains the exponents of the column scaling factors in its 
// ilo through ihi locations and the reducing column permutations in 
// its first ilo-1 and its ihi+1 through n locations.
//
// cperm contains the column permutations applied in grading the a and b 
// submatrices in its ilo through ihi locations.
//
// wk contains the exponents of the row scaling factors in its ilo 
// through ihi locations, the reducing row permutations in its first 
// ilo-1 and its ihi+1 through n locations, and the row permutations
// applied in grading the a and b submatrices in its n+ilo through 
// n+ihi locations.
  
// Copy matrices into local structure.

  balanced_a_mat = a;
  balanced_b_mat = b;

// Initialize balancing matrices to identity.

  left_balancing_mat = Matrix (n, n, 0.0);
  for (int i = 0; i < n; i++)
    left_balancing_mat (i, i) = 1.0;

  right_balancing_mat = left_balancing_mat;

// Check for permutation option.

  if (*balance_job == 'P' || *balance_job == 'B')
    {
      F77_FCN (reduce) (&n, &n, balanced_a_mat.fortran_vec (),
			&n, balanced_b_mat.fortran_vec (), &ilo, &ihi,
			cscale, wk.fortran_vec ());
    }
  else
    {

// Set up for scaling later.

      ilo = 1;
      ihi = n;
    }

// Check for scaling option.

  if ((*balance_job == 'S' || *balance_job == 'B') && ilo != ihi)
    {
      F77_FCN (scaleg) (&n, &n, balanced_a_mat.fortran_vec (), 
			&n, balanced_b_mat.fortran_vec (), &ilo, &ihi,
			cscale, cperm, wk.fortran_vec ());
    }
  else
    {

// Set scaling data to 0's.

      for (int tmp = ilo-1; tmp < ihi; tmp++)
	{
	  cscale[tmp] = 0.0;
	  wk.elem (tmp, 0) = 0.0;
	}
    }

// Scaleg returns exponents, not values, so...

  for (int tmp = ilo-1; tmp < ihi; tmp++)
    {
      cscale[tmp] = pow (2.0, cscale[tmp]);
      wk.elem (tmp, 0) = pow (2.0, -wk.elem (tmp, 0));
    }

// Column permutations/scaling.

  F77_FCN (dgebak) (balance_job, "R", &n, &ilo, &ihi, cscale, &n, 
		    right_balancing_mat.fortran_vec (), &n, &info, 1L,
		    1L);
    
// Row permutations/scaling.

  F77_FCN (dgebak) (balance_job, "L", &n, &ilo, &ihi, &wk.elem (0, 0), &n, 
		    left_balancing_mat.fortran_vec (), &n, &info, 1L, 1L);

// XXX FIXME XXX --- these four lines need to be added and debugged.
// GEPBALANCE::init will work without them, though, so here they are.

#if 0
  if ((*balance_job == 'P' || *balance_job == 'B') && ilo != ihi)
    {
      F77_FCN (gradeq) (&n, &n, balanced_a_mat.fortran_vec (),
			&n, balanced_b_mat.fortran_vec (), &ilo, &ihi,
			cperm, &wk.elem (0, 1));
    }
#endif

// Transpose for aa = cc*a*dd convention...
  left_balancing_mat = left_balancing_mat.transpose ();

  delete [] cscale;
  delete [] cperm;

  return info;
}

/*
 * CHOL stuff
 */

int
CHOL::init (const Matrix& a)
{
  int a_nr = a.rows ();
  int a_nc = a.cols ();
  if (a_nr != a_nc)
    {
      (*current_liboctave_error_handler) ("CHOL requires square matrix");
      return -1;
    }

  char uplo = 'U';

  int n = a_nc;
  int info;

  double *h = dup (a.data (), a.length ());

  F77_FCN (dpotrf) (&uplo, &n, h, &n, &info, 1L);

  chol_mat = Matrix (h, n, n);

// If someone thinks of a more graceful way of doing this (or faster for
// that matter :-)), please let me know!

  if (n > 1)
    for (int j = 0; j < a_nc; j++)
      for (int i = j+1; i < a_nr; i++)
        chol_mat.elem (i, j) = 0.0;

  return info;
}

int
ComplexCHOL::init (const ComplexMatrix& a)
{
  int a_nr = a.rows ();
  int a_nc = a.cols ();
   if (a_nr != a_nc)
     {
       (*current_liboctave_error_handler)
	 ("ComplexCHOL requires square matrix");
       return -1;
     }

   char uplo = 'U';

   int n = a_nc;
   int info;

   Complex *h = dup (a.data (), a.length ());

   F77_FCN (zpotrf) (&uplo, &n, h, &n, &info, 1L);

   chol_mat = ComplexMatrix (h, n, n);

// If someone thinks of a more graceful way of doing this (or faster for
// that matter :-)), please let me know!

  if (n > 1)
    for (int j = 0; j < a_nc; j++)
      for (int i = j+1; i < a_nr; i++)
        chol_mat.elem (i, j) = 0.0;

   return info;
}

/*
 * HESS stuff
 */

int
HESS::init (const Matrix& a)
{
  int a_nr = a.rows ();
  int a_nc = a.cols ();
  if (a_nr != a_nc)
    {
      (*current_liboctave_error_handler) ("HESS requires square matrix");
      return -1;
    }

  char jobbal = 'N';
  char side = 'R';

  int n = a_nc;
  int lwork = 32 * n;
  int info;
  int ilo;
  int ihi;

  double *h = dup (a.data (), a.length ());

  double *tau = new double [n+1];
  double *scale = new double [n];
  double *z = new double [n*n];
  double *work = new double [lwork];

  F77_FCN (dgebal) (&jobbal, &n, h, &n, &ilo, &ihi, scale, &info,
		    1L, 1L);

  F77_FCN (dgehrd) (&n, &ilo, &ihi, h, &n, tau, work, &lwork, &info,
		    1L, 1L);

  copy (z, h, n*n);

  F77_FCN (dorghr) (&n, &ilo, &ihi, z, &n, tau, work, &lwork, &info,
		    1L, 1L);

  F77_FCN (dgebak) (&jobbal, &side, &n, &ilo, &ihi, scale, &n, z, &n, 
		    &info, 1L, 1L);

// We need to clear out all of the area below the sub-diagonal which was used
// to store the unitary matrix.

  hess_mat = Matrix (h, n, n);
  unitary_hess_mat = Matrix (z, n, n);

// If someone thinks of a more graceful way of doing this (or faster for 
// that matter :-)), please let me know! 

  if (n > 2)
    for (int j = 0; j < a_nc; j++)
      for (int i = j+2; i < a_nr; i++)
        hess_mat.elem (i, j) = 0;

  delete [] tau;
  delete [] work;
  delete [] scale;

  return info;
}

int
ComplexHESS::init (const ComplexMatrix& a)
{
  int a_nr = a.rows ();
  int a_nc = a.cols ();
   if (a_nr != a_nc)
     {
       (*current_liboctave_error_handler)
	 ("ComplexHESS requires square matrix");
       return -1;
     }

   char job = 'N';
   char side = 'R';

   int n = a_nc;
   int lwork = 32 * n;
   int info;
   int ilo;
   int ihi;

   Complex *h = dup (a.data (), a.length ());

   double *scale = new double [n];
   Complex *tau = new Complex [n-1];
   Complex *work = new Complex [lwork];
   Complex *z = new Complex [n*n];

   F77_FCN (zgebal) (&job, &n, h, &n, &ilo, &ihi, scale, &info, 1L, 1L);

   F77_FCN (zgehrd) (&n, &ilo, &ihi, h, &n, tau, work, &lwork, &info, 1L,
		     1L);

   copy (z, h, n*n);

   F77_FCN (zunghr) (&n, &ilo, &ihi, z, &n, tau, work, &lwork, &info, 1L,
		     1L);

   F77_FCN (zgebak) (&job, &side, &n, &ilo, &ihi, scale, &n, z, &n, &info,
		     1L, 1L); 

   hess_mat = ComplexMatrix (h,n,n);
   unitary_hess_mat = ComplexMatrix (z,n,n);

// If someone thinks of a more graceful way of doing this (or faster for
// that matter :-)), please let me know!

   if (n > 2)
     for (int j = 0; j < a_nc; j++)
       for (int i = j+2; i < a_nr; i++)
         hess_mat.elem (i, j) = 0;

   delete [] work;
   delete [] tau;
   delete [] scale;

   return info;
}

/*
 * SCHUR stuff
 */

static int
select_ana (double *a, double *b)
{
   return (*a < 0.0);
}

static int
select_dig (double *a, double *b)
{
  return (hypot (*a, *b) < 1.0);
}

int
SCHUR::init (const Matrix& a, const char *ord)
{
  int a_nr = a.rows ();
  int a_nc = a.cols ();
  if (a_nr != a_nc)
    {
      (*current_liboctave_error_handler) ("SCHUR requires square matrix");
      return -1;
    }

  char jobvs = 'V';
  char sort;

  if (*ord == 'A' || *ord == 'D' || *ord == 'a' || *ord == 'd')
    sort = 'S';
  else
    sort = 'N';

  char sense = 'N';

  int n = a_nc;
  int lwork = 8 * n;
  int liwork = 1;
  int info;
  int sdim;
  double rconde;
  double rcondv;

  double *s = dup (a.data (), a.length ());

  double *wr = new double [n];
  double *wi = new double [n];
  double *q = new double [n*n];
  double *work = new double [lwork];

// These are not referenced for the non-ordered Schur routine.

  int *iwork = (int *) NULL;
  int *bwork = (int *) NULL;
  if (*ord == 'A' || *ord == 'D' || *ord == 'a' || *ord == 'd')
    {
      iwork = new int [liwork];
      bwork = new int [n];
    }

  if (*ord == 'A' || *ord == 'a')
    {
      F77_FCN (dgeesx) (&jobvs, &sort, select_ana, &sense, &n, s, &n,
			&sdim, wr, wi, q, &n, &rconde, &rcondv, work,
			&lwork, iwork, &liwork, bwork, &info, 1L, 1L);
    }
  else if (*ord == 'D' || *ord == 'd')
    {
      F77_FCN (dgeesx) (&jobvs, &sort, select_dig, &sense, &n, s, &n,
			&sdim, wr, wi, q, &n, &rconde, &rcondv, work,
			&lwork, iwork, &liwork, bwork, &info, 1L, 1L);
      
    }
  else
    {
      F77_FCN (dgeesx) (&jobvs, &sort, (void *) 0, &sense, &n, s,
			&n, &sdim, wr, wi, q, &n, &rconde, &rcondv,
			work, &lwork, iwork, &liwork, bwork, &info,
			1L, 1L);
    }

  schur_mat = Matrix (s, n, n);
  unitary_mat = Matrix (q, n, n);

  delete [] wr;
  delete [] wi;
  delete [] work;
  delete [] iwork;
  delete [] bwork;

  return info;
}

static int
complex_select_ana (Complex *a)
{
  return a->real () < 0.0;
}

static int
complex_select_dig (Complex *a)
{
  return (abs (*a) < 1.0);
}

int
ComplexSCHUR::init (const ComplexMatrix& a, const char *ord)
{
  int a_nr = a.rows ();
  int a_nc = a.cols ();
  if (a_nr != a_nc)
    {
      (*current_liboctave_error_handler)
	("ComplexSCHUR requires square matrix");
      return -1;
    }

  char jobvs = 'V';
  char sort;
  if (*ord == 'A' || *ord == 'D' || *ord == 'a' || *ord == 'd')
     sort = 'S';
   else
     sort = 'N';

  char sense = 'N';

  int n = a_nc;
  int lwork = 8 * n;
  int info;
  int sdim;
  double rconde;
  double rcondv;

  double *rwork = new double [n];

// bwork is not referenced for non-ordered Schur.

  int *bwork = (int *) NULL;
  if (*ord == 'A' || *ord == 'D' || *ord == 'a' || *ord == 'd')
    bwork = new int [n];

  Complex *s = dup (a.data (), a.length ());

  Complex *work = new Complex [lwork];
  Complex *q = new Complex [n*n];
  Complex *w = new Complex [n];

  if (*ord == 'A' || *ord == 'a')
    {
      F77_FCN (zgeesx) (&jobvs, &sort, complex_select_ana, &sense,
			&n, s, &n, &sdim, w, q, &n, &rconde, &rcondv,
			work, &lwork, rwork, bwork, &info, 1L, 1L);
    }
  else if (*ord == 'D' || *ord == 'd')
    {
      F77_FCN (zgeesx) (&jobvs, &sort, complex_select_dig, &sense,
			&n, s, &n, &sdim, w, q, &n, &rconde, &rcondv,
			work, &lwork, rwork, bwork, &info, 1L, 1L);
    }
  else
    {
      F77_FCN (zgeesx) (&jobvs, &sort, (void *) 0, &sense, &n, s,
			&n, &sdim, w, q, &n, &rconde, &rcondv, work,
			&lwork, rwork, bwork, &info, 1L, 1L);
    }

  schur_mat = ComplexMatrix (s,n,n);
  unitary_mat = ComplexMatrix (q,n,n);

  delete [] w;
  delete [] work;
  delete [] rwork;
  delete [] bwork;

  return info;
}

ostream&
operator << (ostream& os, const SCHUR& a)
{
  os << a.schur_matrix () << "\n";
  os << a.unitary_matrix () << "\n";

  return os;
}

/*
 * SVD stuff
 */

int
SVD::init (const Matrix& a)
{
  int info;

  int m = a.rows ();
  int n = a.cols ();

  char jobu = 'A';
  char jobv = 'A';

  double *tmp_data = dup (a.data (), a.length ());

  int min_mn = m < n ? m : n;
  int max_mn = m > n ? m : n;

  double *u = new double[m*m];
  double *s_vec  = new double[min_mn];
  double *vt = new double[n*n];

  int tmp1 = 3*min_mn + max_mn;
  int tmp2 = 5*min_mn - 4;
  int lwork = tmp1 > tmp2 ? tmp1 : tmp2;
  double *work = new double[lwork];

  F77_FCN (dgesvd) (&jobu, &jobv, &m, &n, tmp_data, &m, s_vec, u, &m,
		    vt, &n, work, &lwork, &info, 1L, 1L);

  left_sm = Matrix (u, m, m);
  sigma = DiagMatrix (s_vec, m, n);
  Matrix vt_m (vt, n, n);
  right_sm = Matrix (vt_m.transpose ());

  delete [] tmp_data;
  delete [] work;

  return info;
}

ostream&
operator << (ostream& os, const SVD& a)
{
  os << a.left_singular_matrix () << "\n";
  os << a.singular_values () << "\n";
  os << a.right_singular_matrix () << "\n";

  return os;
}

int
ComplexSVD::init (const ComplexMatrix& a)
{
  int info;

  int m = a.rows ();
  int n = a.cols ();

  char jobu = 'A';
  char jobv = 'A';

  Complex *tmp_data = dup (a.data (), a.length ());

  int min_mn = m < n ? m : n;
  int max_mn = m > n ? m : n;

  Complex *u = new Complex[m*m];
  double *s_vec  = new double[min_mn];
  Complex *vt = new Complex[n*n];

  int lwork = 2*min_mn + max_mn;
  Complex *work = new Complex[lwork];

  int lrwork = 5*max_mn;
  double *rwork = new double[lrwork];

  F77_FCN (zgesvd) (&jobu, &jobv, &m, &n, tmp_data, &m, s_vec, u, &m,
		    vt, &n, work, &lwork, rwork, &info, 1L, 1L);

  left_sm = ComplexMatrix (u, m, m);
  sigma = DiagMatrix (s_vec, m, n);
  ComplexMatrix vt_m (vt, n, n);
  right_sm = ComplexMatrix (vt_m.hermitian ());

  delete [] tmp_data;
  delete [] work;

  return info;
}

/*
 * DET stuff.
 */

int
DET::value_will_overflow (void) const
{
  return det[2] + 1 > log10 (DBL_MAX) ? 1 : 0;
}

int
DET::value_will_underflow (void) const
{
  return det[2] - 1 < log10 (DBL_MIN) ? 1 : 0;
}

double
DET::coefficient (void) const
{
  return det[0];
}

int
DET::exponent (void) const
{
  return (int) det[1];
}

double
DET::value (void) const
{
  return det[0] * pow (10.0, det[1]);
}

int
ComplexDET::value_will_overflow (void) const
{
  return det[2].real () + 1 > log10 (DBL_MAX) ? 1 : 0;
}

int
ComplexDET::value_will_underflow (void) const
{
  return det[2].real () - 1 < log10 (DBL_MIN) ? 1 : 0;
}

Complex
ComplexDET::coefficient (void) const
{
  return det[0];
}

int
ComplexDET::exponent (void) const
{
  return (int) (det[1].real ());
}

Complex
ComplexDET::value (void) const
{
  return det[0] * pow (10.0, det[1].real ());
}

/*
 * EIG stuff.
 */

int
EIG::init (const Matrix& a)
{
  int a_nr = a.rows ();
  if (a_nr != a.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  int n = a_nr;

  int info;

  char jobvl = 'N';
  char jobvr = 'V';

  double *tmp_data = dup (a.data (), a.length ());
  double *wr = new double[n];
  double *wi = new double[n];
  Matrix vr (n, n);
  double *pvr = vr.fortran_vec ();
  int lwork = 8*n;
  double *work = new double[lwork];

  double dummy;
  int idummy = 1;

  F77_FCN (dgeev) (&jobvl, &jobvr, &n, tmp_data, &n, wr, wi, &dummy,
		   &idummy, pvr, &n, work, &lwork, &info, 1L, 1L);

  lambda.resize (n);
  v.resize (n, n);

  for (int j = 0; j < n; j++)
    {
      if (wi[j] == 0.0)
	{
	  lambda.elem (j) = Complex (wr[j]);
	  for (int i = 0; i < n; i++)
	    v.elem (i, j) = vr.elem (i, j);
	}
      else
	{
	  if (j+1 >= n)
	    {
	      (*current_liboctave_error_handler) ("EIG: internal error");
	      return -1;
	    }

	  for (int i = 0; i < n; i++)
	    {
	      lambda.elem (j) = Complex (wr[j], wi[j]);
	      lambda.elem (j+1) = Complex (wr[j+1], wi[j+1]);
	      double real_part = vr.elem (i, j);
	      double imag_part = vr.elem (i, j+1);
	      v.elem (i, j) = Complex (real_part, imag_part);
	      v.elem (i, j+1) = Complex (real_part, -imag_part);
	    }
	  j++;
	}
    }

  delete [] tmp_data;
  delete [] wr;
  delete [] wi;
  delete [] work;

  return info;
}

int
EIG::init (const ComplexMatrix& a)
{
  int a_nr = a.rows ();
  if (a_nr != a.cols ())
    {
      (*current_liboctave_error_handler) ("EIG requires square matrix");
      return -1;
    }

  int n = a_nr;

  int info;

  char jobvl = 'N';
  char jobvr = 'V';

  lambda.resize (n);
  v.resize (n, n);

  Complex *pw = lambda.fortran_vec ();
  Complex *pvr = v.fortran_vec ();

  Complex *tmp_data = dup (a.data (), a.length ());

  int lwork = 8*n;
  Complex *work = new Complex[lwork];
  double *rwork = new double[4*n];

  Complex dummy;
  int idummy = 1;

  F77_FCN (zgeev) (&jobvl, &jobvr, &n, tmp_data, &n, pw, &dummy,
		   &idummy, pvr, &n, work, &lwork, rwork, &info, 1L,
		   1L);

  delete [] tmp_data;
  delete [] work;
  delete [] rwork;

  return info;
}

/*
 * LU stuff.
 */

LU::LU (const Matrix& a)
{
  int a_nr = a.rows ();
  int a_nc = a.cols ();
  if (a_nr == 0 || a_nc == 0 || a_nr != a_nc)
    {
      (*current_liboctave_error_handler) ("LU requires square matrix");
      return;
    }

  int n = a_nr;

  int *ipvt = new int [n];
  int *pvt = new int [n];
  double *tmp_data = dup (a.data (), a.length ());
  int info = 0;
  int zero = 0;
  double b;

  F77_FCN (dgesv) (&n, &zero, tmp_data, &n, ipvt, &b, &n, &info);

  Matrix A_fact (tmp_data, n, n);

  int i;

  for (i = 0; i < n; i++)
    {
      ipvt[i] -= 1;
      pvt[i] = i;
    }

  for (i = 0; i < n - 1; i++)
    {
      int k = ipvt[i];
      if (k != i)
	{
	  int tmp = pvt[k];
	  pvt[k] = pvt[i];
	  pvt[i] = tmp;
	}
    }

  l.resize (n, n, 0.0);
  u.resize (n, n, 0.0);
  p.resize (n, n, 0.0);

  for (i = 0; i < n; i++)
    {
      p.elem (i, pvt[i]) = 1.0;

      int j;

      l.elem (i, i) = 1.0;
      for (j = 0; j < i; j++)
	l.elem (i, j) = A_fact.elem (i, j);

      for (j = i; j < n; j++)
	u.elem (i, j) = A_fact.elem (i, j);
    }

  delete [] ipvt;
  delete [] pvt;
}

ComplexLU::ComplexLU (const ComplexMatrix& a)
{
  int a_nr = a.rows ();
  int a_nc = a.cols ();
  if (a_nr == 0 || a_nc == 0 || a_nr != a_nc)
    {
      (*current_liboctave_error_handler) ("ComplexLU requires square matrix");
      return;
    }

  int n = a_nr;

  int *ipvt = new int [n];
  int *pvt = new int [n];
  Complex *tmp_data = dup (a.data (), a.length ());
  int info = 0;
  int zero = 0;
  Complex b;

  F77_FCN (zgesv) (&n, &zero, tmp_data, &n, ipvt, &b, &n, &info);

  ComplexMatrix A_fact (tmp_data, n, n);

  int i;

  for (i = 0; i < n; i++)
    {
      ipvt[i] -= 1;
      pvt[i] = i;
    }

  for (i = 0; i < n - 1; i++)
    {
      int k = ipvt[i];
      if (k != i)
	{
	  int tmp = pvt[k];
	  pvt[k] = pvt[i];
	  pvt[i] = tmp;
	}
    }

  l.resize (n, n, 0.0);
  u.resize (n, n, 0.0);
  p.resize (n, n, 0.0);

  for (i = 0; i < n; i++)
    {
      p.elem (i, pvt[i]) = 1.0;

      int j;

      l.elem (i, i) = 1.0;
      for (j = 0; j < i; j++)
	l.elem (i, j) = A_fact.elem (i, j);

      for (j = i; j < n; j++)
	u.elem (i, j) = A_fact.elem (i, j);
    }

  delete [] ipvt;
  delete [] pvt;
}

/*
 * QR stuff.
 */

QR::QR (const Matrix& a)
{
  int m = a.rows ();
  int n = a.cols ();

  if (m == 0 || n == 0)
    {
      (*current_liboctave_error_handler) ("QR must have non-empty matrix");
      return;
    }

  double *tmp_data;
  int min_mn = m < n ? m : n;
  double *tau = new double[min_mn];
  int lwork = 32*n;
  double *work = new double[lwork];
  int info = 0;

  if (m > n)
    {
      tmp_data = new double [m*m];
      copy (tmp_data, a.data (), a.length ());
    }
  else
    tmp_data = dup (a.data (), a.length ());

  F77_FCN (dgeqrf) (&m, &n, tmp_data, &m, tau, work, &lwork, &info);

  delete [] work;

  r.resize (m, n, 0.0);
  for (int j = 0; j < n; j++)
    {
      int limit = j < min_mn-1 ? j : min_mn-1;
      for (int i = 0; i <= limit; i++)
	r.elem (i, j) = tmp_data[m*j+i];
    }

  lwork = 32*m;
  work = new double[lwork];

  F77_FCN (dorgqr) (&m, &m, &min_mn, tmp_data, &m, tau, work, &lwork, &info);

  q = Matrix (tmp_data, m, m);

  delete [] tau;
  delete [] work;
}

ComplexQR::ComplexQR (const ComplexMatrix& a)
{
  int m = a.rows ();
  int n = a.cols ();

  if (m == 0 || n == 0)
    {
      (*current_liboctave_error_handler)
	("ComplexQR must have non-empty matrix");
      return;
    }

  Complex *tmp_data;
  int min_mn = m < n ? m : n;
  Complex *tau = new Complex[min_mn];
  int lwork = 32*n;
  Complex *work = new Complex[lwork];
  int info = 0;

  if (m > n)
    {
      tmp_data = new Complex [m*m];
      copy (tmp_data, a.data (), a.length ());
    }
  else
    tmp_data = dup (a.data (), a.length ());

  F77_FCN (zgeqrf) (&m, &n, tmp_data, &m, tau, work, &lwork, &info);

  delete [] work;

  r.resize (m, n, 0.0);
  for (int j = 0; j < n; j++)
    {
      int limit = j < min_mn-1 ? j : min_mn-1;
      for (int i = 0; i <= limit; i++)
	r.elem (i, j) = tmp_data[m*j+i];
    }

  lwork = 32*m;
  work = new Complex[lwork];

  F77_FCN (zungqr) (&m, &m, &min_mn, tmp_data, &m, tau, work, &lwork, &info);

  q = ComplexMatrix (tmp_data, m, m);

  delete [] tau;
  delete [] work;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
