/*

Copyright (C) 2016 John W. Eaton
Copyright (C) 2004-2015 David Bateman
Copyright (C) 1998-2004 Andy Adler

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "CSparse.h"
#include "PermMatrix.h"
#include "dSparse.h"
#include "lo-error.h"
#include "oct-locbuf.h"
#include "oct-sparse.h"
#include "oct-spparms.h"
#include "sparse-lu.h"

// Wrappers for SuiteSparse (formerly UMFPACK) functions that have
// different names depending on the sparse matrix data type.
//
// All of these functions must be specialized to forward to the correct
// SuiteSparse functions.

template <typename T>
void
umfpack_defaults (double *Control);

template <typename T>
void
umfpack_free_numeric (void **Numeric);

template <typename T>
void
umfpack_free_symbolic (void **Symbolic);

template <typename T>
octave_idx_type
umfpack_get_lunz (octave_idx_type *lnz, octave_idx_type *unz, void *Numeric);

template <typename T>
octave_idx_type
umfpack_get_numeric (octave_idx_type *Lp, octave_idx_type *Lj,
                     T *Lx, // Or Lz_packed
                     octave_idx_type *Up, octave_idx_type *Ui,
                     T *Ux, // Or Uz_packed
                     octave_idx_type *p, octave_idx_type *q,
                     double *Dz_packed, octave_idx_type *do_recip,
                     double *Rs, void *Numeric);

template <typename T>
octave_idx_type
umfpack_numeric (const octave_idx_type *Ap, const octave_idx_type *Ai,
                 const T *Ax, // Or Az_packed
                 void *Symbolic, void **Numeric,
                 const double *Control, double *Info);

template <typename T>
octave_idx_type
umfpack_qsymbolic (octave_idx_type n_row, octave_idx_type n_col,
                   const octave_idx_type *Ap, const octave_idx_type *Ai,
                   const T *Ax, // Or Az_packed
                   const octave_idx_type *Qinit, void **Symbolic,
                   const double *Control, double *Info);

template <typename T>
void
umfpack_report_control (const double *Control);

template <typename T>
void
umfpack_report_info (const double *Control, const double *Info);

template <typename T>
void
umfpack_report_matrix (octave_idx_type n_row, octave_idx_type n_col,
                       const octave_idx_type *Ap, const octave_idx_type *Ai,
                       const T *Ax, // Or Az_packed
                       octave_idx_type col_form, const double *Control);

template <typename T>
void
umfpack_report_numeric (void *Numeric, const double *Control);

template <typename T>
void
umfpack_report_perm (octave_idx_type np, const octave_idx_type *Perm,
                     const double *Control);

template <typename T>
void
umfpack_report_status (double *Control, octave_idx_type status);

template <typename T>
void
umfpack_report_symbolic (void *Symbolic, const double *Control);

// SparseMatrix Specialization.

template <>
inline void
umfpack_defaults<double> (double *Control)
{
  UMFPACK_DNAME (defaults) (Control);
}

template <>
inline void
umfpack_free_numeric<double> (void **Numeric)
{
  UMFPACK_DNAME (free_numeric) (Numeric);
}

template <>
inline void
umfpack_free_symbolic<double> (void **Symbolic)
{
  UMFPACK_DNAME (free_symbolic) (Symbolic);
}

template <>
inline octave_idx_type
umfpack_get_lunz<double>
  (octave_idx_type *lnz, octave_idx_type *unz, void *Numeric)
{
  octave_idx_type ignore1, ignore2, ignore3;

  return UMFPACK_DNAME (get_lunz) (lnz, unz, &ignore1, &ignore2,
                                   &ignore3, Numeric);
}

template <>
inline octave_idx_type
umfpack_get_numeric<double>
  (octave_idx_type *Lp, octave_idx_type *Lj, double *Lx,
   octave_idx_type *Up, octave_idx_type *Ui, double *Ux,
   octave_idx_type *p, octave_idx_type *q, double *Dx,
   octave_idx_type *do_recip, double *Rs, void *Numeric)
{
  return UMFPACK_DNAME (get_numeric) (Lp, Lj, Lx, Up, Ui, Ux, p, q, Dx,
                                      do_recip, Rs, Numeric);
}

template <>
inline octave_idx_type
umfpack_numeric<double>
  (const octave_idx_type *Ap, const octave_idx_type *Ai,
   const double *Ax, void *Symbolic, void **Numeric,
   const double *Control, double *Info)
{
  return UMFPACK_DNAME (numeric) (Ap, Ai, Ax, Symbolic, Numeric, Control,
                                  Info);
}

template <>
inline octave_idx_type
umfpack_qsymbolic<double>
  (octave_idx_type n_row, octave_idx_type n_col, const octave_idx_type *Ap,
   const octave_idx_type *Ai, const double *Ax,
   const octave_idx_type *Qinit, void **Symbolic,
   const double *Control, double *Info)
{
  return UMFPACK_DNAME (qsymbolic) (n_row, n_col, Ap, Ai, Ax, Qinit,
                                    Symbolic, Control, Info);
}

template <>
inline void
umfpack_report_control<double> (const double *Control)
{
  UMFPACK_DNAME (report_control) (Control);
}

template <>
inline void
umfpack_report_info<double> (const double *Control, const double *Info)
{
  UMFPACK_DNAME (report_info) (Control, Info);
}

template <>
inline void
umfpack_report_matrix<double>
  (octave_idx_type n_row, octave_idx_type n_col, const octave_idx_type *Ap,
   const octave_idx_type *Ai, const double *Ax, octave_idx_type col_form,
   const double *Control)
{
  UMFPACK_DNAME (report_matrix) (n_row, n_col, Ap, Ai, Ax, col_form, Control);
}

template <>
inline void
umfpack_report_numeric<double> (void *Numeric, const double *Control)
{
  UMFPACK_DNAME (report_numeric) (Numeric, Control);
}

template <>
inline void
umfpack_report_perm<double>
  (octave_idx_type np, const octave_idx_type *Perm, const double *Control)
{
  UMFPACK_DNAME (report_perm) (np, Perm, Control);
}

template <>
inline void
umfpack_report_status<double> (double *Control, octave_idx_type status)
{
  UMFPACK_DNAME (report_status) (Control, status);
}

template <>
inline void
umfpack_report_symbolic<double> (void *Symbolic, const double *Control)
{
  UMFPACK_DNAME (report_symbolic) (Symbolic, Control);
}

// SparseComplexMatrix specialization.

template <>
inline void
umfpack_defaults<Complex> (double *Control)
{
  UMFPACK_ZNAME (defaults) (Control);
}

template <>
inline void
umfpack_free_numeric<Complex> (void **Numeric)
{
  UMFPACK_ZNAME (free_numeric) (Numeric);
}

template <>
inline void
umfpack_free_symbolic<Complex> (void **Symbolic)
{
  UMFPACK_ZNAME (free_symbolic) (Symbolic);
}

template <>
inline octave_idx_type
umfpack_get_lunz<Complex>
  (octave_idx_type *lnz, octave_idx_type *unz, void *Numeric)
{
  octave_idx_type ignore1, ignore2, ignore3;

  return UMFPACK_ZNAME (get_lunz) (lnz, unz, &ignore1, &ignore2,
                                   &ignore3, Numeric);
}

template <>
inline octave_idx_type
umfpack_get_numeric<Complex>
  (octave_idx_type *Lp, octave_idx_type *Lj, Complex *Lz,
   octave_idx_type *Up, octave_idx_type *Ui, Complex *Uz,
   octave_idx_type *p, octave_idx_type *q, double *Dz,
   octave_idx_type *do_recip, double *Rs, void *Numeric)
{
  return UMFPACK_ZNAME (get_numeric) (Lp, Lj,
                                      reinterpret_cast<double *> (Lz),
                                      0, Up, Ui,
                                      reinterpret_cast<double *> (Uz),
                                      0, p, q,
                                      reinterpret_cast<double *> (Dz),
                                      0, do_recip, Rs, Numeric);
}

template <>
inline octave_idx_type
umfpack_numeric<Complex>
  (const octave_idx_type *Ap, const octave_idx_type *Ai,
   const Complex *Az, void *Symbolic, void **Numeric,
   const double *Control, double *Info)
{
  return UMFPACK_ZNAME (numeric) (Ap, Ai,
                                  reinterpret_cast<const double *> (Az),
                                  0, Symbolic, Numeric, Control, Info);
}


template <>
inline octave_idx_type
umfpack_qsymbolic<Complex>
  (octave_idx_type n_row, octave_idx_type n_col,
   const octave_idx_type *Ap, const octave_idx_type *Ai,
   const Complex *Az, const octave_idx_type *Qinit,
   void **Symbolic, const double *Control, double *Info)
{
  return UMFPACK_ZNAME (qsymbolic) (n_row, n_col, Ap, Ai,
                                    reinterpret_cast<const double *> (Az),
                                    0, Qinit, Symbolic, Control, Info);
}


template <>
inline void
umfpack_report_control<Complex> (const double *Control)
{
  UMFPACK_ZNAME (report_control) (Control);
}

template <>
inline void
umfpack_report_info<Complex> (const double *Control, const double *Info)
{
  UMFPACK_ZNAME (report_info) (Control, Info);
}

template <>
inline void
umfpack_report_matrix<Complex>
  (octave_idx_type n_row, octave_idx_type n_col,
   const octave_idx_type *Ap, const octave_idx_type *Ai,
   const Complex *Az, octave_idx_type col_form, const double *Control)
{
  UMFPACK_ZNAME (report_matrix) (n_row, n_col, Ap, Ai,
                                 reinterpret_cast<const double *> (Az),
                                 0, col_form, Control);
}

template <>
inline void
umfpack_report_numeric<Complex> (void *Numeric, const double *Control)
{
  UMFPACK_ZNAME (report_numeric) (Numeric, Control);
}

template <>
inline void
umfpack_report_perm<Complex>
  (octave_idx_type np, const octave_idx_type *Perm, const double *Control)
{
  UMFPACK_ZNAME (report_perm) (np, Perm, Control);
}

template <>
inline void
umfpack_report_status<Complex> (double *Control, octave_idx_type status)
{
  UMFPACK_ZNAME (report_status) (Control, status);
}

template <>
inline void
umfpack_report_symbolic <Complex> (void *Symbolic, const double *Control)
{
  UMFPACK_ZNAME (report_symbolic) (Symbolic, Control);
}

template <typename lu_type>
sparse_lu<lu_type>::sparse_lu (const lu_type& a, const Matrix& piv_thres,
                               bool scale)
  : Lfact (), Ufact (), Rfact (), cond (0), P (), Q ()
{
#ifdef HAVE_UMFPACK
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  // Setup the control parameters
  Matrix Control (UMFPACK_CONTROL, 1);
  double *control = Control.fortran_vec ();
  umfpack_defaults<lu_elt_type> (control);

  double tmp = octave_sparse_params::get_key ("spumoni");
  if (! xisnan (tmp))
    Control (UMFPACK_PRL) = tmp;

  if (piv_thres.numel () == 2)
    {
      tmp = (piv_thres (0) > 1. ? 1. : piv_thres (0));
      if (! xisnan (tmp))
        Control (UMFPACK_PIVOT_TOLERANCE) = tmp;

      tmp = (piv_thres (1) > 1. ? 1. : piv_thres (1));
      if (! xisnan (tmp))
        Control (UMFPACK_SYM_PIVOT_TOLERANCE) = tmp;
    }
  else
    {
      tmp = octave_sparse_params::get_key ("piv_tol");
      if (! xisnan (tmp))
        Control (UMFPACK_PIVOT_TOLERANCE) = tmp;

      tmp = octave_sparse_params::get_key ("sym_tol");
      if (! xisnan (tmp))
        Control (UMFPACK_SYM_PIVOT_TOLERANCE) = tmp;
    }

  // Set whether we are allowed to modify Q or not
  tmp = octave_sparse_params::get_key ("autoamd");
  if (! xisnan (tmp))
    Control (UMFPACK_FIXQ) = tmp;

  // Turn-off UMFPACK scaling for LU
  if (scale)
    Control (UMFPACK_SCALE) = UMFPACK_SCALE_SUM;
  else
    Control (UMFPACK_SCALE) = UMFPACK_SCALE_NONE;

  umfpack_report_control<lu_elt_type> (control);

  const octave_idx_type *Ap = a.cidx ();
  const octave_idx_type *Ai = a.ridx ();
  const lu_elt_type *Ax = a.data ();

  umfpack_report_matrix<lu_elt_type> (nr, nc, Ap, Ai, Ax, static_cast<octave_idx_type> (1), control);

  void *Symbolic;
  Matrix Info (1, UMFPACK_INFO);
  double *info = Info.fortran_vec ();
  int status = umfpack_qsymbolic<lu_elt_type> (nr, nc, Ap, Ai, Ax, 0, &Symbolic, control, info);

  if (status < 0)
    {
      umfpack_report_status<lu_elt_type> (control, status);
      umfpack_report_info<lu_elt_type> (control, info);

      umfpack_free_symbolic<lu_elt_type> (&Symbolic);

      (*current_liboctave_error_handler)
        ("sparse_lu: symbolic factorization failed");
    }
  else
    {
      umfpack_report_symbolic<lu_elt_type> (Symbolic, control);

      void *Numeric;
      status = umfpack_numeric<lu_elt_type> (Ap, Ai, Ax, Symbolic, &Numeric, control, info);
      umfpack_free_symbolic<lu_elt_type> (&Symbolic);

      cond = Info (UMFPACK_RCOND);

      if (status < 0)
        {
          umfpack_report_status<lu_elt_type> (control, status);
          umfpack_report_info<lu_elt_type> (control, info);

          umfpack_free_numeric<lu_elt_type> (&Numeric);

          (*current_liboctave_error_handler)
            ("sparse_lu: numeric factorization failed");
        }
      else
        {
          umfpack_report_numeric<lu_elt_type> (Numeric, control);

          octave_idx_type lnz, unz;
          status = umfpack_get_lunz<lu_elt_type> (&lnz, &unz, Numeric);

          if (status < 0)
            {
              umfpack_report_status<lu_elt_type> (control, status);
              umfpack_report_info<lu_elt_type> (control, info);

              umfpack_free_numeric<lu_elt_type> (&Numeric);

              (*current_liboctave_error_handler)
                ("sparse_lu: extracting LU factors failed");
            }
          else
            {
              octave_idx_type n_inner = (nr < nc ? nr : nc);

              if (lnz < 1)
                Lfact = lu_type (n_inner, nr, static_cast<octave_idx_type> (1));
              else
                Lfact = lu_type (n_inner, nr, lnz);

              octave_idx_type *Ltp = Lfact.cidx ();
              octave_idx_type *Ltj = Lfact.ridx ();
              lu_elt_type *Ltx = Lfact.data ();

              if (unz < 1)
                Ufact = lu_type (n_inner, nc, static_cast<octave_idx_type> (1));
              else
                Ufact = lu_type (n_inner, nc, unz);

              octave_idx_type *Up = Ufact.cidx ();
              octave_idx_type *Uj = Ufact.ridx ();
              lu_elt_type *Ux = Ufact.data ();

              Rfact = SparseMatrix (nr, nr, nr);
              for (octave_idx_type i = 0; i < nr; i++)
                {
                  Rfact.xridx (i) = i;
                  Rfact.xcidx (i) = i;
                }
              Rfact.xcidx (nr) = nr;
              double *Rx = Rfact.data ();

              P.resize (dim_vector (nr, 1));
              octave_idx_type *p = P.fortran_vec ();

              Q.resize (dim_vector (nc, 1));
              octave_idx_type *q = Q.fortran_vec ();

              octave_idx_type do_recip;
              status = umfpack_get_numeric<lu_elt_type> (Ltp, Ltj, Ltx, Up, Uj, Ux, p, q, 0, &do_recip, Rx, Numeric);

              umfpack_free_numeric<lu_elt_type> (&Numeric);

              if (status < 0)
                {
                  umfpack_report_status<lu_elt_type> (control, status);

                  (*current_liboctave_error_handler)
                    ("sparse_lu: extracting LU factors failed");
                }
              else
                {
                  Lfact = Lfact.transpose ();

                  if (do_recip)
                    for (octave_idx_type i = 0; i < nr; i++)
                      Rx[i] = 1.0 / Rx[i];

                  umfpack_report_matrix<lu_elt_type> (nr, n_inner, Lfact.cidx (), Lfact.ridx (), Lfact.data (), static_cast<octave_idx_type> (1), control);
                  umfpack_report_matrix<lu_elt_type> (n_inner, nc, Ufact.cidx (), Ufact.ridx (), Ufact.data (), static_cast<octave_idx_type> (1), control);
                  umfpack_report_perm<lu_elt_type> (nr, p, control);
                  umfpack_report_perm<lu_elt_type> (nc, q, control);
                }

              umfpack_report_info<lu_elt_type> (control, info);
            }
        }
    }

#else

  (*current_liboctave_error_handler)
    ("support for UMFPACK was unavailable or disabled when liboctave was built");

#endif
}

template <typename lu_type>
sparse_lu<lu_type>::sparse_lu (const lu_type& a,
                               const ColumnVector& Qinit,
                               const Matrix& piv_thres, bool scale,
                               bool FixedQ, double droptol,
                               bool milu, bool udiag)
  : Lfact (), Ufact (), Rfact (), cond (0), P (), Q ()
{
#ifdef HAVE_UMFPACK

  if (milu)
    (*current_liboctave_error_handler)
      ("Modified incomplete LU not implemented");

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  // Setup the control parameters
  Matrix Control (UMFPACK_CONTROL, 1);
  double *control = Control.fortran_vec ();
  umfpack_defaults<lu_elt_type> (control);

  double tmp = octave_sparse_params::get_key ("spumoni");
  if (! xisnan (tmp))
    Control (UMFPACK_PRL) = tmp;

  if (piv_thres.numel () == 2)
    {
      tmp = (piv_thres (0) > 1. ? 1. : piv_thres (0));
      if (! xisnan (tmp))
        Control (UMFPACK_PIVOT_TOLERANCE) = tmp;
      tmp = (piv_thres (1) > 1. ? 1. : piv_thres (1));
      if (! xisnan (tmp))
        Control (UMFPACK_SYM_PIVOT_TOLERANCE) = tmp;
    }
  else
    {
      tmp = octave_sparse_params::get_key ("piv_tol");
      if (! xisnan (tmp))
        Control (UMFPACK_PIVOT_TOLERANCE) = tmp;

      tmp = octave_sparse_params::get_key ("sym_tol");
      if (! xisnan (tmp))
        Control (UMFPACK_SYM_PIVOT_TOLERANCE) = tmp;
    }

  if (droptol >= 0.)
    Control (UMFPACK_DROPTOL) = droptol;

  // Set whether we are allowed to modify Q or not
  if (FixedQ)
    Control (UMFPACK_FIXQ) = 1.0;
  else
    {
      tmp = octave_sparse_params::get_key ("autoamd");
      if (! xisnan (tmp))
        Control (UMFPACK_FIXQ) = tmp;
    }

  // Turn-off UMFPACK scaling for LU
  if (scale)
    Control (UMFPACK_SCALE) = UMFPACK_SCALE_SUM;
  else
    Control (UMFPACK_SCALE) = UMFPACK_SCALE_NONE;

  umfpack_report_control<lu_elt_type> (control);

  const octave_idx_type *Ap = a.cidx ();
  const octave_idx_type *Ai = a.ridx ();
  const lu_elt_type *Ax = a.data ();

  umfpack_report_matrix<lu_elt_type> (nr, nc, Ap, Ai, Ax, static_cast<octave_idx_type> (1), control);

  void *Symbolic;
  Matrix Info (1, UMFPACK_INFO);
  double *info = Info.fortran_vec ();
  int status;

  // Null loop so that qinit is imediately deallocated when not needed
  do
    {
      OCTAVE_LOCAL_BUFFER (octave_idx_type, qinit, nc);

      for (octave_idx_type i = 0; i < nc; i++)
        qinit[i] = static_cast<octave_idx_type> (Qinit (i));

      status = umfpack_qsymbolic<lu_elt_type> (nr, nc, Ap, Ai, Ax, qinit, &Symbolic, control, info);
    }
  while (0);

  if (status < 0)
    {
      umfpack_report_status<lu_elt_type> (control, status);
      umfpack_report_info<lu_elt_type> (control, info);

      umfpack_free_symbolic<lu_elt_type> (&Symbolic);

      (*current_liboctave_error_handler)
        ("sparse_lu: symbolic factorization failed");
    }
  else
    {
      umfpack_report_symbolic<lu_elt_type> (Symbolic, control);

      void *Numeric;
      status = umfpack_numeric<lu_elt_type> (Ap, Ai, Ax, Symbolic, &Numeric, control, info);
      umfpack_free_symbolic<lu_elt_type> (&Symbolic);

      cond = Info (UMFPACK_RCOND);

      if (status < 0)
        {
          umfpack_report_status<lu_elt_type> (control, status);
          umfpack_report_info<lu_elt_type> (control, info);

          umfpack_free_numeric<lu_elt_type> (&Numeric);

          (*current_liboctave_error_handler)
            ("sparse_lu: numeric factorization failed");
        }
      else
        {
          umfpack_report_numeric<lu_elt_type> (Numeric, control);

          octave_idx_type lnz, unz;
          status = umfpack_get_lunz<lu_elt_type> (&lnz, &unz, Numeric);

          if (status < 0)
            {
              umfpack_report_status<lu_elt_type> (control, status);
              umfpack_report_info<lu_elt_type> (control, info);

              umfpack_free_numeric<lu_elt_type> (&Numeric);

              (*current_liboctave_error_handler)
                ("sparse_lu: extracting LU factors failed");
            }
          else
            {
              octave_idx_type n_inner = (nr < nc ? nr : nc);

              if (lnz < 1)
                Lfact = lu_type (n_inner, nr, static_cast<octave_idx_type> (1));
              else
                Lfact = lu_type (n_inner, nr, lnz);

              octave_idx_type *Ltp = Lfact.cidx ();
              octave_idx_type *Ltj = Lfact.ridx ();
              lu_elt_type *Ltx = Lfact.data ();

              if (unz < 1)
                Ufact = lu_type (n_inner, nc, static_cast<octave_idx_type> (1));
              else
                Ufact = lu_type (n_inner, nc, unz);

              octave_idx_type *Up = Ufact.cidx ();
              octave_idx_type *Uj = Ufact.ridx ();
              lu_elt_type *Ux = Ufact.data ();

              Rfact = SparseMatrix (nr, nr, nr);
              for (octave_idx_type i = 0; i < nr; i++)
                {
                  Rfact.xridx (i) = i;
                  Rfact.xcidx (i) = i;
                }
              Rfact.xcidx (nr) = nr;
              double *Rx = Rfact.data ();

              P.resize (dim_vector (nr, 1));
              octave_idx_type *p = P.fortran_vec ();

              Q.resize (dim_vector (nc, 1));
              octave_idx_type *q = Q.fortran_vec ();

              octave_idx_type do_recip;
              status = umfpack_get_numeric<lu_elt_type> (Ltp, Ltj, Ltx, Up, Uj, Ux, p, q, 0, &do_recip, Rx, Numeric);

              umfpack_free_numeric<lu_elt_type> (&Numeric);

              if (status < 0)
                {
                  umfpack_report_status<lu_elt_type> (control, status);

                  (*current_liboctave_error_handler)
                    ("sparse_lu: extracting LU factors failed");
                }
              else
                {
                  Lfact = Lfact.transpose ();

                  if (do_recip)
                    for (octave_idx_type i = 0; i < nr; i++)
                      Rx[i] = 1.0 / Rx[i];

                  umfpack_report_matrix<lu_elt_type> (nr, n_inner, Lfact.cidx (), Lfact.ridx (), Lfact.data (), static_cast<octave_idx_type> (1), control);
                  umfpack_report_matrix<lu_elt_type> (n_inner, nc, Ufact.cidx (), Ufact.ridx (), Ufact.data (), static_cast<octave_idx_type> (1), control);
                  umfpack_report_perm<lu_elt_type> (nr, p, control);
                  umfpack_report_perm<lu_elt_type> (nc, q, control);
                }

              umfpack_report_info<lu_elt_type> (control, info);
            }
        }
    }

  if (udiag)
    (*current_liboctave_error_handler)
      ("Option udiag of incomplete LU not implemented");

#else

  (*current_liboctave_error_handler)
    ("support for UMFPACK was unavailable or disabled when liboctave was built");

#endif
}

template <typename lu_type>
lu_type
sparse_lu<lu_type>::Y (void) const
{
  octave_idx_type nr = Lfact.rows ();
  octave_idx_type nz = Lfact.cols ();
  octave_idx_type nc = Ufact.cols ();

  lu_type Yout (nr, nc, Lfact.nnz () + Ufact.nnz () - (nr<nz?nr:nz));
  octave_idx_type ii = 0;
  Yout.xcidx (0) = 0;

  for (octave_idx_type j = 0; j < nc; j++)
    {
      for (octave_idx_type i = Ufact.cidx (j); i < Ufact.cidx (j + 1); i++)
        {
          Yout.xridx (ii) = Ufact.ridx (i);
          Yout.xdata (ii++) = Ufact.data (i);
        }

      if (j < nz)
        {
          // Note the +1 skips the 1.0 on the diagonal
          for (octave_idx_type i = Lfact.cidx (j) + 1;
               i < Lfact.cidx (j +1); i++)
            {
              Yout.xridx (ii) = Lfact.ridx (i);
              Yout.xdata (ii++) = Lfact.data (i);
            }
        }

      Yout.xcidx (j + 1) = ii;
    }

  return Yout;
}

template <typename lu_type>
SparseMatrix
sparse_lu<lu_type>::Pr (void) const
{
  octave_idx_type nr = Lfact.rows ();

  SparseMatrix Pout (nr, nr, nr);

  for (octave_idx_type i = 0; i < nr; i++)
    {
      Pout.cidx (i) = i;
      Pout.ridx (P (i)) = i;
      Pout.data (i) = 1;
    }

  Pout.cidx (nr) = nr;

  return Pout;
}

template <typename lu_type>
ColumnVector
sparse_lu<lu_type>::Pr_vec (void) const
{
  octave_idx_type nr = Lfact.rows ();

  ColumnVector Pout (nr);

  for (octave_idx_type i = 0; i < nr; i++)
    Pout.xelem (i) = static_cast<double> (P(i) + 1);

  return Pout;
}

template <typename lu_type>
PermMatrix
sparse_lu<lu_type>::Pr_mat (void) const
{
  return PermMatrix (P, false);
}

template <typename lu_type>
SparseMatrix
sparse_lu<lu_type>::Pc (void) const
{
  octave_idx_type nc = Ufact.cols ();

  SparseMatrix Pout (nc, nc, nc);

  for (octave_idx_type i = 0; i < nc; i++)
    {
      Pout.cidx (i) = i;
      Pout.ridx (i) = Q (i);
      Pout.data (i) = 1;
    }

  Pout.cidx (nc) = nc;

  return Pout;
}

template <typename lu_type>
ColumnVector
sparse_lu<lu_type>::Pc_vec (void) const
{
  octave_idx_type nc = Ufact.cols ();

  ColumnVector Pout (nc);

  for (octave_idx_type i = 0; i < nc; i++)
    Pout.xelem (i) = static_cast<double> (Q(i) + 1);

  return Pout;
}

template <typename lu_type>
PermMatrix
sparse_lu<lu_type>::Pc_mat (void) const
{
  return PermMatrix (Q, true);
}
