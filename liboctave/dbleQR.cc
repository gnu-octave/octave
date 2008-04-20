/*

Copyright (C) 1994, 1995, 1996, 1997, 2002, 2003, 2004, 2005, 2007
              John W. Eaton

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

#include "dbleQR.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "Range.h"
#include "idx-vector.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (dgeqrf, DGEQRF) (const octave_idx_type&, const octave_idx_type&, double*, const octave_idx_type&,
			     double*, double*, const octave_idx_type&, octave_idx_type&); 

  F77_RET_T
  F77_FUNC (dorgqr, DORGQR) (const octave_idx_type&, const octave_idx_type&, const octave_idx_type&, double*,
			     const octave_idx_type&, double*, double*, const octave_idx_type&, octave_idx_type&);

  // these come from qrupdate

  F77_RET_T
  F77_FUNC (dqr1up, DQR1UP) (const octave_idx_type&, const octave_idx_type&, const octave_idx_type&, 
                             double*, double*, const double*, const double*);

  F77_RET_T
  F77_FUNC (dqrinc, DQRINC) (const octave_idx_type&, const octave_idx_type&, const octave_idx_type&, 
                             double*, const double*, double*, const octave_idx_type&, const double*);

  F77_RET_T
  F77_FUNC (dqrdec, DQRDEC) (const octave_idx_type&, const octave_idx_type&, const octave_idx_type&, 
                             double*, const double*, double*, const octave_idx_type&);

  F77_RET_T
  F77_FUNC (dqrinr, DQRINR) (const octave_idx_type&, const octave_idx_type&, 
                             const double*, double*, const double*, double*, 
                             const octave_idx_type&, const double*);

  F77_RET_T
  F77_FUNC (dqrder, DQRDER) (const octave_idx_type&, const octave_idx_type&, 
                             const double*, double*, const double*, double *, 
                             const octave_idx_type&);

  F77_RET_T
  F77_FUNC (dqrshc, DQRSHC) (const octave_idx_type&, const octave_idx_type&, const octave_idx_type&,
                             double*, double*, const octave_idx_type&, const octave_idx_type&);
}

QR::QR (const Matrix& a, QR::type qr_type)
  : q (), r ()
{
  init (a, qr_type);
}

void
QR::init (const Matrix& a, QR::type qr_type)
{
  octave_idx_type m = a.rows ();
  octave_idx_type n = a.cols ();

  if (m == 0 || n == 0)
    {
      (*current_liboctave_error_handler) ("QR must have non-empty matrix");
      return;
    }

  octave_idx_type min_mn = m < n ? m : n;
  Array<double> tau (min_mn);
  double *ptau = tau.fortran_vec ();

  octave_idx_type lwork = 32*n;
  Array<double> work (lwork);
  double *pwork = work.fortran_vec ();

  octave_idx_type info = 0;

  Matrix A_fact = a;
  if (m > n && qr_type != QR::economy)
      A_fact.resize (m, m, 0.0);

  double *tmp_data = A_fact.fortran_vec ();

  F77_XFCN (dgeqrf, DGEQRF, (m, n, tmp_data, m, ptau, pwork, lwork, info));

  if (qr_type == QR::raw)
    {
      for (octave_idx_type j = 0; j < min_mn; j++)
	{
	  octave_idx_type limit = j < min_mn - 1 ? j : min_mn - 1;
	  for (octave_idx_type i = limit + 1; i < m; i++)
	    A_fact.elem (i, j) *= tau.elem (j);
	}

      r = A_fact;

      if (m > n)
	r.resize (m, n);
    }
  else
    {
      octave_idx_type n2 = (qr_type == QR::economy) ? min_mn : m;

      if (qr_type == QR::economy && m > n)
	r.resize (n, n, 0.0);
      else
	r.resize (m, n, 0.0);

      for (octave_idx_type j = 0; j < n; j++)
	{
	  octave_idx_type limit = j < min_mn-1 ? j : min_mn-1;
	  for (octave_idx_type i = 0; i <= limit; i++)
	    r.elem (i, j) = tmp_data[m*j+i];
	}

      lwork = 32 * n2;
      work.resize (lwork);
      double *pwork2 = work.fortran_vec ();

      F77_XFCN (dorgqr, DORGQR, (m, n2, min_mn, tmp_data, m, ptau,
				 pwork2, lwork, info));

      q = A_fact;
      q.resize (m, n2);
    }
}

QR::QR (const Matrix& q, const Matrix& r)
{
  if (q.columns () != r.rows ()) 
    {
      (*current_liboctave_error_handler) ("QR dimensions mismatch");
      return;
    }

  this->q = q;
  this->r = r;
}

void
QR::update (const Matrix& u, const Matrix& v)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  if (u.length () == m && v.length () == n)
    F77_XFCN (dqr1up, DQR1UP, (m, n, k, q.fortran_vec (), r.fortran_vec (), 
			       u.data (), v.data ()));
  else
    (*current_liboctave_error_handler) ("QR update dimensions mismatch");
}

void
QR::insert_col (const Matrix& u, octave_idx_type j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  if (u.length () != m)
    (*current_liboctave_error_handler) ("QR insert dimensions mismatch");
  else if (j < 0 || j > n) 
    (*current_liboctave_error_handler) ("QR insert index out of range");
  else
    {
      Matrix r1 (m, n+1);

      F77_XFCN (dqrinc, DQRINC, (m, n, k, q.fortran_vec (), r.data (),
				 r1.fortran_vec (), j+1, u.data ()));

      r = r1;
    }
}

void
QR::delete_col (octave_idx_type j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type k = r.rows ();
  octave_idx_type n = r.columns ();

  if (k < m && k < n) 
    (*current_liboctave_error_handler) ("QR delete dimensions mismatch");
  else if (j < 0 || j > n-1) 
    (*current_liboctave_error_handler) ("QR delete index out of range");
  else
    {
      Matrix r1 (k, n-1);

      F77_XFCN (dqrdec, DQRDEC, (m, n, k, q.fortran_vec (), r.data (),
				 r1.fortran_vec (), j+1));

      r = r1;
    }
}

void
QR::insert_row (const Matrix& u, octave_idx_type j)
{
  octave_idx_type m = r.rows ();
  octave_idx_type n = r.columns ();

  if (! q.is_square () || u.length () != n)
    (*current_liboctave_error_handler) ("QR insert dimensions mismatch");
  else if (j < 0 || j > m) 
    (*current_liboctave_error_handler) ("QR insert index out of range");
  else
    {
      Matrix q1 (m+1, m+1);
      Matrix r1 (m+1, n);

      F77_XFCN (dqrinr, DQRINR, (m, n, q.data (), q1.fortran_vec (), 
				 r.data (), r1.fortran_vec (), j+1, u.data ()));

      q = q1;
      r = r1;
    }
}

void
QR::delete_row (octave_idx_type j)
{
  octave_idx_type m = r.rows ();
  octave_idx_type n = r.columns ();

  if (! q.is_square ())
    (*current_liboctave_error_handler) ("QR delete dimensions mismatch");
  else if (j < 0 || j > m-1) 
    (*current_liboctave_error_handler) ("QR delete index out of range");
  else
    {
      Matrix q1 (m-1, m-1);
      Matrix r1 (m-1, n);

      F77_XFCN (dqrder, DQRDER, (m, n, q.data (), q1.fortran_vec (), 
				 r.data (), r1.fortran_vec (), j+1 ));

      q = q1;
      r = r1;
    }
}

void
QR::shift_cols (octave_idx_type i, octave_idx_type j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type k = r.rows ();
  octave_idx_type n = r.columns ();

  if (i < 0 || i > n-1 || j < 0 || j > n-1) 
    (*current_liboctave_error_handler) ("QR shift index out of range");
  else
    F77_XFCN (dqrshc, DQRSHC, (m, n, k, q.fortran_vec (), r.fortran_vec (), i+1, j+1));
}

void
QR::economize (void)
{
  octave_idx_type r_nc = r.columns ();

  if (r.rows () > r_nc)
    {
      q.resize (q.rows (), r_nc);
      r.resize (r_nc, r_nc);
    }
}


/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
