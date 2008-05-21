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

#include "floatQR.h"
#include "f77-fcn.h"
#include "lo-error.h"
#include "Range.h"
#include "idx-vector.h"

extern "C"
{
  F77_RET_T
  F77_FUNC (sgeqrf, SGEQRF) (const octave_idx_type&, const octave_idx_type&, float*, const octave_idx_type&,
			     float*, float*, const octave_idx_type&, octave_idx_type&); 

  F77_RET_T
  F77_FUNC (sorgqr, SORGQR) (const octave_idx_type&, const octave_idx_type&, const octave_idx_type&, float*,
			     const octave_idx_type&, float*, float*, const octave_idx_type&, octave_idx_type&);

  // these come from qrupdate

  F77_RET_T
  F77_FUNC (sqr1up, SQR1UP) (const octave_idx_type&, const octave_idx_type&, const octave_idx_type&, 
                             float*, float*, const float*, const float*);

  F77_RET_T
  F77_FUNC (sqrinc, SQRINC) (const octave_idx_type&, const octave_idx_type&, const octave_idx_type&, 
                             float*, const float*, float*, const octave_idx_type&, const float*);

  F77_RET_T
  F77_FUNC (sqrdec, SQRDEC) (const octave_idx_type&, const octave_idx_type&, const octave_idx_type&, 
                             float*, const float*, float*, const octave_idx_type&);

  F77_RET_T
  F77_FUNC (sqrinr, SQRINR) (const octave_idx_type&, const octave_idx_type&, 
                             const float*, float*, const float*, float*, 
                             const octave_idx_type&, const float*);

  F77_RET_T
  F77_FUNC (sqrder, SQRDER) (const octave_idx_type&, const octave_idx_type&, 
                             const float*, float*, const float*, float *, 
                             const octave_idx_type&);

  F77_RET_T
  F77_FUNC (sqrshc, SQRSHC) (const octave_idx_type&, const octave_idx_type&, const octave_idx_type&,
                             float*, float*, const octave_idx_type&, const octave_idx_type&);
}

FloatQR::FloatQR (const FloatMatrix& a, QR::type qr_type)
  : q (), r ()
{
  init (a, qr_type);
}

void
FloatQR::init (const FloatMatrix& a, QR::type qr_type)
{
  octave_idx_type m = a.rows ();
  octave_idx_type n = a.cols ();

  if (m == 0 || n == 0)
    {
      (*current_liboctave_error_handler) ("QR must have non-empty matrix");
      return;
    }

  octave_idx_type min_mn = m < n ? m : n;
  Array<float> tau (min_mn);
  float *ptau = tau.fortran_vec ();

  octave_idx_type lwork = 32*n;
  Array<float> work (lwork);
  float *pwork = work.fortran_vec ();

  octave_idx_type info = 0;

  FloatMatrix A_fact = a;
  if (m > n && qr_type != QR::economy)
      A_fact.resize (m, m, 0.0);

  float *tmp_data = A_fact.fortran_vec ();

  F77_XFCN (sgeqrf, SGEQRF, (m, n, tmp_data, m, ptau, pwork, lwork, info));

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
      float *pwork2 = work.fortran_vec ();

      F77_XFCN (sorgqr, SORGQR, (m, n2, min_mn, tmp_data, m, ptau,
				 pwork2, lwork, info));

      q = A_fact;
      q.resize (m, n2);
    }
}

FloatQR::FloatQR (const FloatMatrix& q, const FloatMatrix& r)
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
FloatQR::update (const FloatMatrix& u, const FloatMatrix& v)
{
  octave_idx_type m = q.rows ();
  octave_idx_type n = r.columns ();
  octave_idx_type k = q.columns ();

  if (u.length () == m && v.length () == n)
    F77_XFCN (sqr1up, SQR1UP, (m, n, k, q.fortran_vec (), r.fortran_vec (), 
			       u.data (), v.data ()));
  else
    (*current_liboctave_error_handler) ("QR update dimensions mismatch");
}

void
FloatQR::insert_col (const FloatMatrix& u, octave_idx_type j)
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
      FloatMatrix r1 (m, n+1);

      F77_XFCN (sqrinc, SQRINC, (m, n, k, q.fortran_vec (), r.data (),
				 r1.fortran_vec (), j+1, u.data ()));

      r = r1;
    }
}

void
FloatQR::delete_col (octave_idx_type j)
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
      FloatMatrix r1 (k, n-1);

      F77_XFCN (sqrdec, SQRDEC, (m, n, k, q.fortran_vec (), r.data (),
				 r1.fortran_vec (), j+1));

      r = r1;
    }
}

void
FloatQR::insert_row (const FloatMatrix& u, octave_idx_type j)
{
  octave_idx_type m = r.rows ();
  octave_idx_type n = r.columns ();

  if (! q.is_square () || u.length () != n)
    (*current_liboctave_error_handler) ("QR insert dimensions mismatch");
  else if (j < 0 || j > m) 
    (*current_liboctave_error_handler) ("QR insert index out of range");
  else
    {
      FloatMatrix q1 (m+1, m+1);
      FloatMatrix r1 (m+1, n);

      F77_XFCN (sqrinr, SQRINR, (m, n, q.data (), q1.fortran_vec (), 
				 r.data (), r1.fortran_vec (), j+1, u.data ()));

      q = q1;
      r = r1;
    }
}

void
FloatQR::delete_row (octave_idx_type j)
{
  octave_idx_type m = r.rows ();
  octave_idx_type n = r.columns ();

  if (! q.is_square ())
    (*current_liboctave_error_handler) ("QR delete dimensions mismatch");
  else if (j < 0 || j > m-1) 
    (*current_liboctave_error_handler) ("QR delete index out of range");
  else
    {
      FloatMatrix q1 (m-1, m-1);
      FloatMatrix r1 (m-1, n);

      F77_XFCN (sqrder, SQRDER, (m, n, q.data (), q1.fortran_vec (), 
				 r.data (), r1.fortran_vec (), j+1 ));

      q = q1;
      r = r1;
    }
}

void
FloatQR::shift_cols (octave_idx_type i, octave_idx_type j)
{
  octave_idx_type m = q.rows ();
  octave_idx_type k = r.rows ();
  octave_idx_type n = r.columns ();

  if (i < 0 || i > n-1 || j < 0 || j > n-1) 
    (*current_liboctave_error_handler) ("QR shift index out of range");
  else
    F77_XFCN (sqrshc, SQRSHC, (m, n, k, q.fortran_vec (), r.fortran_vec (), i+1, j+1));
}

void
FloatQR::economize (void)
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
