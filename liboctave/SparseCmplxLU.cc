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

#include <vector>

#include "lo-error.h"

#include "SparseCmplxLU.h"
#include "oct-spparms.h"

// Instantiate the base LU class for the types we need.

#include "sparse-base-lu.h"
#include "sparse-base-lu.cc"

template class sparse_base_lu <SparseComplexMatrix, Complex, SparseMatrix, double>;

#ifdef HAVE_UMFPACK
// Include the UMFPACK functions
extern "C" {
#include <umfpack/umfpack.h>
}
#endif

SparseComplexLU::SparseComplexLU (const SparseComplexMatrix& a, 
				  double piv_thres)
{
#ifdef HAVE_UMFPACK
  int nr = a.rows ();
  int nc = a.cols ();

  // Setup the control parameters
  Matrix Control (UMFPACK_CONTROL, 1);
  double *control = Control.fortran_vec ();
  umfpack_zi_defaults (control);

  double tmp = Voctave_sparse_controls.get_key ("spumoni");
  if (!xisnan (tmp))
    Control (UMFPACK_PRL) = tmp;
  if (piv_thres >= 0.)
    {
      piv_thres = (piv_thres > 1. ? 1. : piv_thres);
      Control (UMFPACK_SYM_PIVOT_TOLERANCE) = piv_thres;
      Control (UMFPACK_PIVOT_TOLERANCE) = piv_thres;
    }
  else
    {
      tmp = Voctave_sparse_controls.get_key ("piv_tol");
      if (!xisnan (tmp))
	{
	  Control (UMFPACK_SYM_PIVOT_TOLERANCE) = tmp;
      Control (UMFPACK_PIVOT_TOLERANCE) = tmp;
	}
    }

  // Set whether we are allowed to modify Q or not
  tmp = Voctave_sparse_controls.get_key ("autoamd");
  if (!xisnan (tmp))
    Control (UMFPACK_FIXQ) = tmp;

  // Turn-off UMFPACK scaling for LU 
  Control (UMFPACK_SCALE) = UMFPACK_SCALE_NONE;

  umfpack_zi_report_control (control);

  const int *Ap = a.cidx ();
  const int *Ai = a.ridx ();
  const Complex *Ax = a.data ();

  umfpack_zi_report_matrix (nr, nc, Ap, Ai, X_CAST (const double *, Ax), 
			    NULL, 1, control);

  void *Symbolic;
  Matrix Info (1, UMFPACK_INFO);
  double *info = Info.fortran_vec ();
  int status = umfpack_zi_qsymbolic (nr, nc, Ap, Ai, 
				     X_CAST (const double *, Ax), NULL, NULL,
				     &Symbolic, control, info);

  if (status < 0)
    {
      (*current_liboctave_error_handler) 
	    ("SparseComplexLU::SparseComplexLU symbolic factorization failed");

      umfpack_zi_report_status (control, status);
      umfpack_zi_report_info (control, info);

      umfpack_zi_free_symbolic (&Symbolic) ;
    }
  else
    {
      umfpack_zi_report_symbolic (Symbolic, control);

      void *Numeric;
      status = umfpack_zi_numeric (Ap, Ai, X_CAST (const double *, Ax), NULL,
				   Symbolic, &Numeric, control, info) ;
      umfpack_zi_free_symbolic (&Symbolic) ;

      cond = Info (UMFPACK_RCOND);

      if (status < 0)
	{
	  (*current_liboctave_error_handler) 
	    ("SparseComplexLU::SparseComplexLU numeric factorization failed");

	  umfpack_zi_report_status (control, status);
	  umfpack_zi_report_info (control, info);

	  umfpack_zi_free_numeric (&Numeric);
	}
      else
	{
	  umfpack_zi_report_numeric (Numeric, control);

	  int lnz, unz, ignore1, ignore2, ignore3;
	  status = umfpack_zi_get_lunz (&lnz, &unz, &ignore1, &ignore2,
					&ignore3, Numeric) ;
	  
	  if (status < 0)
	    {
	      (*current_liboctave_error_handler) 
		("SparseComplexLU::SparseComplexLU extracting LU factors failed");

	      umfpack_zi_report_status (control, status);
	      umfpack_zi_report_info (control, info);

	      umfpack_zi_free_numeric (&Numeric);
	    }
	  else
	    {
	      int n_inner = (nr < nc ? nr : nc);

	      if (lnz < 1)
		Lfact = SparseComplexMatrix (n_inner, nr, 1);
	      else
		Lfact = SparseComplexMatrix (n_inner, nr, lnz);

	      int *Ltp = Lfact.cidx ();
	      int *Ltj = Lfact.ridx ();
	      Complex *Ltx = Lfact.data ();

	      if (unz < 1)
		Ufact = SparseComplexMatrix (n_inner, nc, 1);
	      else
		Ufact = SparseComplexMatrix (n_inner, nc, unz);

	      int *Up = Ufact.cidx ();
	      int *Uj = Ufact.ridx ();
	      Complex *Ux = Ufact.data ();
	      
	      P.resize (nr);
	      int *p = P.fortran_vec ();

	      Q.resize (nc);
	      int *q = Q.fortran_vec ();

	      int do_recip;
	      status = umfpack_zi_get_numeric (Ltp, Ltj, X_CAST (double *, Ltx),
					       NULL, Up, Uj,
					       X_CAST (double *, Ux), NULL, p, 
					       q, NULL, NULL, &do_recip,
					       NULL, Numeric) ;

	      umfpack_zi_free_numeric (&Numeric) ;

	      if (status < 0 || do_recip)
		{
		  (*current_liboctave_error_handler) 
		    ("SparseComplexLU::SparseComplexLU extracting LU factors failed");

		  umfpack_zi_report_status (control, status);
		}
	      else
		{
		  Lfact = Lfact.transpose ();

		  umfpack_zi_report_matrix (nr, n_inner, Lfact.cidx (), 
					    Lfact.ridx (), 
					    X_CAST (double *, Lfact.data()), 
					    NULL, 1, control);

		  umfpack_zi_report_matrix (n_inner, nc, Ufact.cidx (), 
					    Ufact.ridx (), 
					    X_CAST (double *, Ufact.data()), 
					    NULL, 1, control);
		  umfpack_zi_report_perm (nr, p, control);
		  umfpack_zi_report_perm (nc, q, control);
		}

	      umfpack_zi_report_info (control, info);
	    }
	}
    }
#else
  (*current_liboctave_error_handler) ("UMFPACK not installed");
#endif
}

SparseComplexLU::SparseComplexLU (const SparseComplexMatrix& a, 
				  const ColumnVector& Qinit, 
				  double piv_thres, bool FixedQ)
{
#ifdef HAVE_UMFPACK
  int nr = a.rows ();
  int nc = a.cols ();

  // Setup the control parameters
  Matrix Control (UMFPACK_CONTROL, 1);
  double *control = Control.fortran_vec ();
  umfpack_zi_defaults (control);

  double tmp = Voctave_sparse_controls.get_key ("spumoni");
  if (!xisnan (tmp))
    Control (UMFPACK_PRL) = tmp;
  if (piv_thres >= 0.)
    {
      piv_thres = (piv_thres > 1. ? 1. : piv_thres);
      Control (UMFPACK_SYM_PIVOT_TOLERANCE) = piv_thres;
      Control (UMFPACK_PIVOT_TOLERANCE) = piv_thres;
    }
  else
    {
      tmp = Voctave_sparse_controls.get_key ("piv_tol");
      if (!xisnan (tmp))
	{
	  Control (UMFPACK_SYM_PIVOT_TOLERANCE) = tmp;
	  Control (UMFPACK_PIVOT_TOLERANCE) = tmp;
	}
    }

  // Set whether we are allowed to modify Q or not
  if (FixedQ)
    Control (UMFPACK_FIXQ) = 1.0;
  else
    {
      tmp = Voctave_sparse_controls.get_key ("autoamd");
      if (!xisnan (tmp))
	Control (UMFPACK_FIXQ) = tmp;
    }

  // Turn-off UMFPACK scaling for LU 
  Control (UMFPACK_SCALE) = UMFPACK_SCALE_NONE;

  umfpack_zi_report_control (control);

  const int *Ap = a.cidx ();
  const int *Ai = a.ridx ();
  const Complex *Ax = a.data ();

  umfpack_zi_report_matrix (nr, nc, Ap, Ai, X_CAST (const double *, Ax), NULL,
			    1, control);

  void *Symbolic;
  Matrix Info (1, UMFPACK_INFO);
  double *info = Info.fortran_vec ();
  int status;

  // Null loop so that qinit is imediately deallocated when not needed
  do {
    OCTAVE_LOCAL_BUFFER (int, qinit, nc);

    for (int i = 0; i < nc; i++)
      qinit [i] = static_cast<int> (Qinit (i));

    status = umfpack_zi_qsymbolic (nr, nc, Ap, Ai, X_CAST (const double *, Ax),
				   NULL, qinit, &Symbolic, control, info);
  } while (0);

  if (status < 0)
    {
      (*current_liboctave_error_handler) 
	    ("SparseComplexLU::SparseComplexLU symbolic factorization failed");

      umfpack_zi_report_status (control, status);
      umfpack_zi_report_info (control, info);

      umfpack_zi_free_symbolic (&Symbolic) ;
    }
  else
    {
      umfpack_zi_report_symbolic (Symbolic, control);

      void *Numeric;
      status = umfpack_zi_numeric (Ap, Ai, X_CAST (const double *, Ax), NULL,
				   Symbolic, &Numeric, control, info) ;
      umfpack_zi_free_symbolic (&Symbolic) ;

      cond = Info (UMFPACK_RCOND);

      if (status < 0)
	{
	  (*current_liboctave_error_handler) 
	    ("SparseComplexLU::SparseComplexLU numeric factorization failed");

	  umfpack_zi_report_status (control, status);
	  umfpack_zi_report_info (control, info);

	  umfpack_zi_free_numeric (&Numeric);
	}
      else
	{
	  umfpack_zi_report_numeric (Numeric, control);

	  int lnz, unz, ignore1, ignore2, ignore3;
	  status = umfpack_zi_get_lunz (&lnz, &unz, &ignore1, &ignore2,
					&ignore3, Numeric) ;
	  
	  if (status < 0)
	    {
	      (*current_liboctave_error_handler) 
		("SparseComplexLU::SparseComplexLU extracting LU factors failed");

	      umfpack_zi_report_status (control, status);
	      umfpack_zi_report_info (control, info);

	      umfpack_zi_free_numeric (&Numeric);
	    }
	  else
	    {
	      int n_inner = (nr < nc ? nr : nc);

	      if (lnz < 1)
		Lfact = SparseComplexMatrix (n_inner, nr, 1);
	      else
		Lfact = SparseComplexMatrix (n_inner, nr, lnz);

	      int *Ltp = Lfact.cidx ();
	      int *Ltj = Lfact.ridx ();
	      Complex *Ltx = Lfact.data ();

	      if (unz < 1)
		Ufact = SparseComplexMatrix (n_inner, nc, 1);
	      else
		Ufact = SparseComplexMatrix (n_inner, nc, unz);

	      int *Up = Ufact.cidx ();
	      int *Uj = Ufact.ridx ();
	      Complex *Ux = Ufact.data ();
	      
	      P.resize (nr);
	      int *p = P.fortran_vec ();

	      Q.resize (nc);
	      int *q = Q.fortran_vec ();

	      int do_recip;
	      status = umfpack_zi_get_numeric (Ltp, Ltj, X_CAST (double *, Ltx),
					       NULL, Up, Uj,
					       X_CAST (double *, Ux), NULL, p, 
					       q, NULL, NULL, &do_recip,
					       NULL, Numeric) ;

	      umfpack_zi_free_numeric (&Numeric) ;

	      if (status < 0 || do_recip)
		{
		  (*current_liboctave_error_handler) 
		    ("SparseComplexLU::SparseComplexLU extracting LU factors failed");

		  umfpack_zi_report_status (control, status);
		}
	      else
		{
		  Lfact = Lfact.transpose ();

		  umfpack_zi_report_matrix (nr, n_inner, Lfact.cidx (), 
					    Lfact.ridx (), 
					    X_CAST (double *, Lfact.data()), 
					    NULL, 1, control);

		  umfpack_zi_report_matrix (n_inner, nc, Ufact.cidx (), 
					    Ufact.ridx (), 
					    X_CAST (double *, Ufact.data()), 
					    NULL, 1, control);
		  umfpack_zi_report_perm (nr, p, control);
		  umfpack_zi_report_perm (nc, q, control);
		}

	      umfpack_zi_report_info (control, info);
	    }
	}
    }
#else
  (*current_liboctave_error_handler) ("UMFPACK not installed");
#endif
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

