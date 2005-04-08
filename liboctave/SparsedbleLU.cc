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

#include "SparsedbleLU.h"
#include "oct-spparms.h"

// Instantiate the base LU class for the types we need.

#include "sparse-base-lu.h"
#include "sparse-base-lu.cc"

template class sparse_base_lu <SparseMatrix, double, SparseMatrix, double>;

#ifdef HAVE_UMFPACK
// Include the UMFPACK functions
extern "C" {
#include <umfpack/umfpack.h>
}
#endif

SparseLU::SparseLU (const SparseMatrix& a, double piv_thres)
{
#ifdef HAVE_UMFPACK
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  // Setup the control parameters
  Matrix Control (UMFPACK_CONTROL, 1);
  double *control = Control.fortran_vec ();
  umfpack_di_defaults (control);

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

  umfpack_di_report_control (control);

  const octave_idx_type *Ap = a.cidx ();
  const octave_idx_type *Ai = a.ridx ();
  const double *Ax = a.data ();

  umfpack_di_report_matrix (nr, nc, Ap, Ai, Ax, 1, control);

  void *Symbolic;
  Matrix Info (1, UMFPACK_INFO);
  double *info = Info.fortran_vec ();
  int status = umfpack_di_qsymbolic (nr, nc, Ap, Ai, Ax, NULL,
				     &Symbolic, control, info);

  if (status < 0)
    {
      (*current_liboctave_error_handler) 
	    ("SparseLU::SparseLU symbolic factorization failed");

      umfpack_di_report_status (control, status);
      umfpack_di_report_info (control, info);

      umfpack_di_free_symbolic (&Symbolic) ;
    }
  else
    {
      umfpack_di_report_symbolic (Symbolic, control);

      void *Numeric;
      status = umfpack_di_numeric (Ap, Ai, Ax, Symbolic, &Numeric,
				   control, info) ;
      umfpack_di_free_symbolic (&Symbolic) ;

      cond = Info (UMFPACK_RCOND);

      if (status < 0)
	{
	  (*current_liboctave_error_handler) 
	    ("SparseLU::SparseLU numeric factorization failed");

	  umfpack_di_report_status (control, status);
	  umfpack_di_report_info (control, info);

	  umfpack_di_free_numeric (&Numeric);
	}
      else
	{
	  umfpack_di_report_numeric (Numeric, control);

	  int lnz, unz, ignore1, ignore2, ignore3;
	  status = umfpack_di_get_lunz (&lnz, &unz, &ignore1, &ignore2,
					&ignore3, Numeric) ;
	  
	  if (status < 0)
	    {
	      (*current_liboctave_error_handler) 
		("SparseLU::SparseLU extracting LU factors failed");

	      umfpack_di_report_status (control, status);
	      umfpack_di_report_info (control, info);

	      umfpack_di_free_numeric (&Numeric);
	    }
	  else
	    {
	      int n_inner = (nr < nc ? nr : nc);

	      if (lnz < 1)
		Lfact = SparseMatrix (static_cast<octave_idx_type> (n_inner), nr,
				      static_cast<octave_idx_type> (1));
	      else
		Lfact = SparseMatrix (static_cast<octave_idx_type> (n_inner), nr,
				      static_cast<octave_idx_type> (lnz));

	      octave_idx_type *Ltp = Lfact.cidx ();
	      octave_idx_type *Ltj = Lfact.ridx ();
	      double *Ltx = Lfact.data ();

	      if (unz < 1)
		Ufact = SparseMatrix (static_cast<octave_idx_type> (n_inner), nc,
				      static_cast<octave_idx_type> (1));
	      else
		Ufact = SparseMatrix (static_cast<octave_idx_type> (n_inner), nc,
				      static_cast<octave_idx_type> (unz));

	      octave_idx_type *Up = Ufact.cidx ();
	      octave_idx_type *Uj = Ufact.ridx ();
	      double *Ux = Ufact.data ();

	      P.resize (nr);
	      int *p = P.fortran_vec ();

	      Q.resize (nc);
	      int *q = Q.fortran_vec ();

	      int do_recip;
	      status = umfpack_di_get_numeric (Ltp, Ltj, Ltx, Up, Uj,
					       Ux, p, q, (double *) NULL,
					       &do_recip, (double *) NULL, 
					       Numeric) ;

	      umfpack_di_free_numeric (&Numeric) ;

	      if (status < 0 || do_recip)
		{
		  (*current_liboctave_error_handler) 
		    ("SparseLU::SparseLU extracting LU factors failed");

		  umfpack_di_report_status (control, status);
		}
	      else
		{
		  Lfact = Lfact.transpose ();

		  umfpack_di_report_matrix (nr, n_inner, Lfact.cidx (), 
					    Lfact.ridx (), Lfact.data (),
					    1, control);
		  umfpack_di_report_matrix (n_inner, nc, Ufact.cidx (), 
					    Ufact.ridx (), Ufact.data (),
					    1, control);
		  umfpack_di_report_perm (nr, p, control);
		  umfpack_di_report_perm (nc, q, control);
		}

	      umfpack_di_report_info (control, info);
	    }
	}
    }
#else
  (*current_liboctave_error_handler) ("UMFPACK not installed");
#endif
}

SparseLU::SparseLU (const SparseMatrix& a, const ColumnVector& Qinit,
		    double piv_thres, bool FixedQ)
{
#ifdef HAVE_UMFPACK
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  // Setup the control parameters
  Matrix Control (UMFPACK_CONTROL, 1);
  double *control = Control.fortran_vec ();
  umfpack_di_defaults (control);

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

  umfpack_di_report_control (control);

  const octave_idx_type *Ap = a.cidx ();
  const octave_idx_type *Ai = a.ridx ();
  const double *Ax = a.data ();

  umfpack_di_report_matrix (nr, nc, Ap, Ai, Ax, 1, control);

  void *Symbolic;
  Matrix Info (1, UMFPACK_INFO);
  double *info = Info.fortran_vec ();
  int status;

  // Null loop so that qinit is imediately deallocated when not needed
  do {
    OCTAVE_LOCAL_BUFFER (int, qinit, nc);

    for (int i = 0; i < nc; i++)
      qinit [i] = static_cast<int> (Qinit (i));

    status = umfpack_di_qsymbolic (nr, nc, Ap, Ai, Ax, qinit,
				   &Symbolic, control, info);
  } while (0);

  if (status < 0)
    {
      (*current_liboctave_error_handler) 
	    ("SparseLU::SparseLU symbolic factorization failed");

      umfpack_di_report_status (control, status);
      umfpack_di_report_info (control, info);

      umfpack_di_free_symbolic (&Symbolic) ;
    }
  else
    {
      umfpack_di_report_symbolic (Symbolic, control);

      void *Numeric;
      status = umfpack_di_numeric (Ap, Ai, Ax, Symbolic, &Numeric,
				   control, info) ;
      umfpack_di_free_symbolic (&Symbolic) ;

      cond = Info (UMFPACK_RCOND);

      if (status < 0)
	{
	  (*current_liboctave_error_handler) 
	    ("SparseLU::SparseLU numeric factorization failed");

	  umfpack_di_report_status (control, status);
	  umfpack_di_report_info (control, info);

	  umfpack_di_free_numeric (&Numeric);
	}
      else
	{
	  umfpack_di_report_numeric (Numeric, control);

	  int lnz, unz, ignore1, ignore2, ignore3;
	  status = umfpack_di_get_lunz (&lnz, &unz, &ignore1, &ignore2,
					&ignore3, Numeric) ;
	  
	  if (status < 0)
	    {
	      (*current_liboctave_error_handler) 
		("SparseLU::SparseLU extracting LU factors failed");

	      umfpack_di_report_status (control, status);
	      umfpack_di_report_info (control, info);

	      umfpack_di_free_numeric (&Numeric);
	    }
	  else
	    {
	      int n_inner = (nr < nc ? nr : nc);

	      if (lnz < 1)
		Lfact = SparseMatrix (static_cast<octave_idx_type> (n_inner), nr,
				      static_cast<octave_idx_type> (1));
	      else
		Lfact = SparseMatrix (static_cast<octave_idx_type> (n_inner), nr,
				      static_cast<octave_idx_type> (lnz));

	      octave_idx_type *Ltp = Lfact.cidx ();
	      octave_idx_type *Ltj = Lfact.ridx ();
	      double *Ltx = Lfact.data ();

	      if (unz < 1)
		Ufact = SparseMatrix (static_cast<octave_idx_type> (n_inner), nc,
				      static_cast<octave_idx_type> (1));
	      else
		Ufact = SparseMatrix (static_cast<octave_idx_type> (n_inner), nc,
				      static_cast<octave_idx_type> (unz));

	      octave_idx_type *Up = Ufact.cidx ();
	      octave_idx_type *Uj = Ufact.ridx ();
	      double *Ux = Ufact.data ();

	      P.resize (nr);
	      int *p = P.fortran_vec ();

	      Q.resize (nc);
	      int *q = Q.fortran_vec ();

	      int do_recip;
	      status = umfpack_di_get_numeric (Ltp, Ltj, Ltx, Up, Uj,
					       Ux, p, q, (double *) NULL,
					       &do_recip, (double *) NULL, 
					       Numeric) ;

	      umfpack_di_free_numeric (&Numeric) ;

	      if (status < 0 || do_recip)
		{
		  (*current_liboctave_error_handler) 
		    ("SparseLU::SparseLU extracting LU factors failed");

		  umfpack_di_report_status (control, status);
		}
	      else
		{
		  Lfact = Lfact.transpose ();
		  umfpack_di_report_matrix (nr, n_inner, Lfact.cidx (), 
					    Lfact.ridx (), Lfact.data (),
					    1, control);
		  umfpack_di_report_matrix (n_inner, nc, Ufact.cidx (), 
					    Ufact.ridx (), Ufact.data (),
					    1, control);
		  umfpack_di_report_perm (nr, p, control);
		  umfpack_di_report_perm (nc, q, control);
		}

	      umfpack_di_report_info (control, info);
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

