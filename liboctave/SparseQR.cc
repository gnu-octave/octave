/*

Copyright (C) 2005 David Bateman

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <vector>

#include "lo-error.h"
#include "SparseQR.h"

SparseQR::SparseQR_rep::SparseQR_rep (const SparseMatrix& a, int order)
{
#ifdef HAVE_CXSPARSE
  CXSPARSE_DNAME () A;
  A.nzmax = a.nzmax ();
  A.m = a.rows ();
  A.n = a.cols ();
  nrows = A.m;
  // Cast away const on A, with full knowledge that CSparse won't touch it
  // Prevents the methods below making a copy of the data.
  A.p = const_cast<octave_idx_type *>(a.cidx ());
  A.i = const_cast<octave_idx_type *>(a.ridx ());
  A.x = const_cast<double *>(a.data ());
  A.nz = -1;
  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
#if defined(CS_VER) && (CS_VER >= 2)
  S = CXSPARSE_DNAME (_sqr) (order, &A, 1);
#else
  S = CXSPARSE_DNAME (_sqr) (&A, order - 1, 1);
#endif

  N = CXSPARSE_DNAME (_qr) (&A, S);
  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
  if (!N)
    (*current_liboctave_error_handler)
      ("SparseQR: sparse matrix QR factorization filled");
  count = 1;
#else
  (*current_liboctave_error_handler)
    ("SparseQR: sparse matrix QR factorization not implemented");
#endif
}

SparseQR::SparseQR_rep::~SparseQR_rep (void)
{
#ifdef HAVE_CXSPARSE
  CXSPARSE_DNAME (_sfree) (S);
  CXSPARSE_DNAME (_nfree) (N);
#endif
}

SparseMatrix 
SparseQR::SparseQR_rep::V (void) const
{
#ifdef HAVE_CXSPARSE
  // Drop zeros from V and sort
  // FIXME Is the double transpose to sort necessary?
  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
  CXSPARSE_DNAME (_dropzeros) (N->L);
  CXSPARSE_DNAME () *D = CXSPARSE_DNAME (_transpose) (N->L, 1);
  CXSPARSE_DNAME (_spfree) (N->L);
  N->L = CXSPARSE_DNAME (_transpose) (D, 1);
  CXSPARSE_DNAME (_spfree) (D);
  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

  octave_idx_type nc = N->L->n;
  octave_idx_type nz = N->L->nzmax;
  SparseMatrix ret (N->L->m, nc, nz);
  for (octave_idx_type j = 0; j < nc+1; j++)
    ret.xcidx (j) = N->L->p[j];
  for (octave_idx_type j = 0; j < nz; j++)
    {
      ret.xridx (j) = N->L->i[j];
      ret.xdata (j) = N->L->x[j];
    }
  return ret;
#else
  return SparseMatrix ();
#endif
}

ColumnVector 
SparseQR::SparseQR_rep::Pinv (void) const
{
#ifdef HAVE_CXSPARSE
  ColumnVector ret(N->L->m);
  for (octave_idx_type i = 0; i < N->L->m; i++)
#if defined(CS_VER) && (CS_VER >= 2)
    ret.xelem(i) = S->pinv[i];
#else
    ret.xelem(i) = S->Pinv[i];
#endif
  return ret;
#else
  return ColumnVector ();
#endif
}

ColumnVector 
SparseQR::SparseQR_rep::P (void) const
{
#ifdef HAVE_CXSPARSE
  ColumnVector ret(N->L->m);
  for (octave_idx_type i = 0; i < N->L->m; i++)
#if defined(CS_VER) && (CS_VER >= 2)
    ret.xelem(S->pinv[i]) = i;
#else
    ret.xelem(S->Pinv[i]) = i;
#endif
  return ret;
#else
  return ColumnVector ();
#endif
}

SparseMatrix 
SparseQR::SparseQR_rep::R (const bool econ) const
{
#ifdef HAVE_CXSPARSE
  // Drop zeros from R and sort
  // FIXME Is the double transpose to sort necessary?
  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
  CXSPARSE_DNAME (_dropzeros) (N->U);
  CXSPARSE_DNAME () *D = CXSPARSE_DNAME (_transpose) (N->U, 1);
  CXSPARSE_DNAME (_spfree) (N->U);
  N->U = CXSPARSE_DNAME (_transpose) (D, 1);
  CXSPARSE_DNAME (_spfree) (D);
  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

  octave_idx_type nc = N->U->n;
  octave_idx_type nz = N->U->nzmax;

  SparseMatrix ret ((econ ? (nc > nrows ? nrows : nc) : nrows), nc, nz);

  for (octave_idx_type j = 0; j < nc+1; j++)
    ret.xcidx (j) = N->U->p[j];
  for (octave_idx_type j = 0; j < nz; j++)
    {
      ret.xridx (j) = N->U->i[j];
      ret.xdata (j) = N->U->x[j];
    }
  return ret;
#else
  return SparseMatrix ();
#endif
}

Matrix
SparseQR::SparseQR_rep::C (const Matrix &b) const
{
#ifdef HAVE_CXSPARSE
  octave_idx_type b_nr = b.rows();
  octave_idx_type b_nc = b.cols();
  octave_idx_type nc = N->L->n;
  octave_idx_type nr = nrows;
  const double *bvec = b.fortran_vec();
  Matrix ret(b_nr,b_nc);
  double *vec = ret.fortran_vec();
  if (nr < 1 || nc < 1 || nr != b_nr)
    (*current_liboctave_error_handler) ("matrix dimension mismatch");
  else
    {
      OCTAVE_LOCAL_BUFFER (double, buf, S->m2);
      for (volatile octave_idx_type j = 0, idx = 0; j < b_nc; j++, idx+=b_nr)
	{
	  OCTAVE_QUIT;
	  for (octave_idx_type i = nr; i < S->m2; i++)
	    buf[i] = 0.;
	  volatile octave_idx_type nm = (nr < nc ? nr : nc);
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_ipvec) (S->pinv, bvec + idx, buf, b_nr);
#else
	  CXSPARSE_DNAME (_ipvec) (b_nr, S->Pinv, bvec + idx, buf);
#endif
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

	  for (volatile octave_idx_type i = 0; i < nm; i++)
	    {
	      OCTAVE_QUIT;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CXSPARSE_DNAME (_happly) (N->L, i, N->B[i], buf);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
	  for (octave_idx_type i = 0; i < b_nr; i++)
	    vec[i+idx] = buf[i];
	}
    }
  return ret;
#else
  return Matrix ();
#endif
}

Matrix
qrsolve(const SparseMatrix&a, const Matrix &b, octave_idx_type& info)
{
#ifdef HAVE_CXSPARSE
  octave_idx_type nr = a.rows();
  octave_idx_type nc = a.cols();
  octave_idx_type b_nc = b.cols();
  octave_idx_type b_nr = b.rows();
  const double *bvec = b.fortran_vec();
  Matrix x;
  info = 0;

  if (nr < 1 || nc < 1 || nr != b_nr)
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch in solution of minimum norm problem");
  else if (nr >= nc)
    {
      SparseQR q (a, 3);
      if (! q.ok ()) 
	{
	  info = -1;
	  return Matrix();
	}
      x.resize(nc, b_nc);
      double *vec = x.fortran_vec();
      OCTAVE_LOCAL_BUFFER (double, buf, q.S()->m2);
      for (volatile octave_idx_type i = 0, idx = 0, bidx = 0; i < b_nc; 
	   i++, idx+=nc, bidx+=b_nr)
	{
	  OCTAVE_QUIT;
	  for (octave_idx_type j = nr; j < q.S()->m2; j++)
	    buf[j] = 0.;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_ipvec) (q.S()->pinv, bvec + bidx, buf, nr);
#else
	  CXSPARSE_DNAME (_ipvec) (nr, q.S()->Pinv, bvec + bidx, buf);
#endif
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (volatile octave_idx_type j = 0; j < nc; j++)
	    {
	      OCTAVE_QUIT;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CXSPARSE_DNAME (_happly) (q.N()->L, j, q.N()->B[j], buf);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_DNAME (_usolve) (q.N()->U, buf);
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_ipvec) (q.S()->q, buf, vec + idx, nc);
#else
	  CXSPARSE_DNAME (_ipvec) (nc, q.S()->Q, buf, vec + idx);
#endif
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	}
    }
  else
    {
      SparseMatrix at = a.hermitian();
      SparseQR q (at, 3);
      if (! q.ok ())
	{
	  info = -1;
	  return Matrix();
	}
      x.resize(nc, b_nc);
      double *vec = x.fortran_vec();
      volatile octave_idx_type nbuf = (nc > q.S()->m2 ? nc : q.S()->m2);
      OCTAVE_LOCAL_BUFFER (double, buf, nbuf);
      for (volatile octave_idx_type i = 0, idx = 0, bidx = 0; i < b_nc; 
	   i++, idx+=nc, bidx+=b_nr)
	{
	  OCTAVE_QUIT;
	  for (octave_idx_type j = nr; j < nbuf; j++)
	    buf[j] = 0.;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_pvec) (q.S()->q, bvec + bidx, buf, nr);
#else
	  CXSPARSE_DNAME (_pvec) (nr, q.S()->Q, bvec + bidx, buf);
#endif
	  CXSPARSE_DNAME (_utsolve) (q.N()->U, buf);
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (volatile octave_idx_type j = nr-1; j >= 0; j--)
	    {
	      OCTAVE_QUIT;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CXSPARSE_DNAME (_happly) (q.N()->L, j, q.N()->B[j], buf);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_pvec) (q.S()->pinv, buf, vec + idx, nc);
#else
	  CXSPARSE_DNAME (_pvec) (nc, q.S()->Pinv, buf, vec + idx);
#endif
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	}
    }

  return x;
#else
  return Matrix ();
#endif
}

SparseMatrix
qrsolve(const SparseMatrix&a, const SparseMatrix &b, octave_idx_type &info)
{
#ifdef HAVE_CXSPARSE
  octave_idx_type nr = a.rows();
  octave_idx_type nc = a.cols();
  octave_idx_type b_nr = b.rows();
  octave_idx_type b_nc = b.cols();
  SparseMatrix x;
  volatile octave_idx_type ii, x_nz;
  info = 0;

  if (nr < 1 || nc < 1 || nr != b_nr)
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch in solution of minimum norm problem");
  else if (nr >= nc)
    {
      SparseQR q (a, 3);
      if (! q.ok ()) 
	{
	  info = -1;
	  return SparseMatrix();
	}
      x = SparseMatrix (nc, b_nc, b.nzmax());
      x.xcidx(0) = 0;
      x_nz = b.nzmax();
      ii = 0;
      OCTAVE_LOCAL_BUFFER (double, Xx, (b_nr > nc ? b_nr : nc));
      OCTAVE_LOCAL_BUFFER (double, buf, q.S()->m2);
      for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
	{
	  OCTAVE_QUIT;
	  for (octave_idx_type j = 0; j < b_nr; j++)
	    Xx[j] = b.xelem(j,i);
	  for (octave_idx_type j = nr; j < q.S()->m2; j++)
	    buf[j] = 0.;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_ipvec) (q.S()->pinv, Xx, buf, nr);
#else
	  CXSPARSE_DNAME (_ipvec) (nr, q.S()->Pinv, Xx, buf);
#endif
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (volatile octave_idx_type j = 0; j < nc; j++)
	    {
	      OCTAVE_QUIT;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CXSPARSE_DNAME (_happly) (q.N()->L, j, q.N()->B[j], buf);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_DNAME (_usolve) (q.N()->U, buf);
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_ipvec) (q.S()->q, buf, Xx, nc);
#else
	  CXSPARSE_DNAME (_ipvec) (nc, q.S()->Q, buf, Xx);
#endif
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

	  for (octave_idx_type j = 0; j < nc; j++)
	    {
	      double tmp = Xx[j];
	      if (tmp != 0.0)
		{
		  if (ii == x_nz)
		    {
		      // Resize the sparse matrix
		      octave_idx_type sz = x_nz * (b_nc - i) / b_nc;
		      sz = (sz > 10 ? sz : 10) + x_nz;
		      x.change_capacity (sz);
		      x_nz = sz;
		    }
		  x.xdata(ii) = tmp;
		  x.xridx(ii++) = j;
		}
	    }
	  x.xcidx(i+1) = ii;
	}
    }
  else
    {
      SparseMatrix at = a.hermitian();
      SparseQR q (at, 3);
      if (! q.ok ())
	{
	  info = -1;
	  return SparseMatrix();
	}
      x = SparseMatrix (nc, b_nc, b.nzmax());
      x.xcidx(0) = 0;
      x_nz = b.nzmax();
      ii = 0;
      volatile octave_idx_type nbuf = (nc > q.S()->m2 ? nc : q.S()->m2);
      OCTAVE_LOCAL_BUFFER (double, Xx, (b_nr > nc ? b_nr : nc));
      OCTAVE_LOCAL_BUFFER (double, buf, nbuf);
      for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
	{
	  OCTAVE_QUIT;
	  for (octave_idx_type j = 0; j < b_nr; j++)
	    Xx[j] = b.xelem(j,i);
	  for (octave_idx_type j = nr; j < nbuf; j++)
	    buf[j] = 0.;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_pvec) (q.S()->q, Xx, buf, nr);
#else
	  CXSPARSE_DNAME (_pvec) (nr, q.S()->Q, Xx, buf);
#endif
	  CXSPARSE_DNAME (_utsolve) (q.N()->U, buf);
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (volatile octave_idx_type j = nr-1; j >= 0; j--)
	    {
	      OCTAVE_QUIT;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CXSPARSE_DNAME (_happly) (q.N()->L, j, q.N()->B[j], buf);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_pvec) (q.S()->pinv, buf, Xx, nc);
#else
	  CXSPARSE_DNAME (_pvec) (nc, q.S()->Pinv, buf, Xx);
#endif
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

	  for (octave_idx_type j = 0; j < nc; j++)
	    {
	      double tmp = Xx[j];
	      if (tmp != 0.0)
		{
		  if (ii == x_nz)
		    {
		      // Resize the sparse matrix
		      octave_idx_type sz = x_nz * (b_nc - i) / b_nc;
		      sz = (sz > 10 ? sz : 10) + x_nz;
		      x.change_capacity (sz);
		      x_nz = sz;
		    }
		  x.xdata(ii) = tmp;
		  x.xridx(ii++) = j;
		}
	    }
	  x.xcidx(i+1) = ii;
	}
    }

  x.maybe_compress ();
  return x;
#else
  return SparseMatrix ();
#endif
}

ComplexMatrix
qrsolve(const SparseMatrix&a, const ComplexMatrix &b, octave_idx_type &info)
{
#ifdef HAVE_CXSPARSE
  octave_idx_type nr = a.rows();
  octave_idx_type nc = a.cols();
  octave_idx_type b_nc = b.cols();
  octave_idx_type b_nr = b.rows();
  ComplexMatrix x;
  info = 0;

  if (nr < 1 || nc < 1 || nr != b_nr)
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch in solution of minimum norm problem");
  else if (nr >= nc)
    {
      SparseQR q (a, 3);
      if (! q.ok ())
	{
	  info = -1;
	  return ComplexMatrix();
	}
      x.resize(nc, b_nc);
      Complex *vec = x.fortran_vec();
      OCTAVE_LOCAL_BUFFER (double, Xx, (b_nr > nc ? b_nr : nc));
      OCTAVE_LOCAL_BUFFER (double, Xz, (b_nr > nc ? b_nr : nc));
      OCTAVE_LOCAL_BUFFER (double, buf, q.S()->m2);
      for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
	{
	  OCTAVE_QUIT;
	  for (octave_idx_type j = 0; j < b_nr; j++)
	    {
	      Complex c = b.xelem (j,i);
	      Xx[j] = std::real (c);
	      Xz[j] = std::imag (c);
	    }
	  for (octave_idx_type j = nr; j < q.S()->m2; j++)
	    buf[j] = 0.;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_ipvec) (q.S()->pinv, Xx, buf, nr);
#else
	  CXSPARSE_DNAME (_ipvec) (nr, q.S()->Pinv, Xx, buf);
#endif
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (volatile octave_idx_type j = 0; j < nc; j++)
	    {
	      OCTAVE_QUIT;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CXSPARSE_DNAME (_happly) (q.N()->L, j, q.N()->B[j], buf);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_DNAME (_usolve) (q.N()->U, buf);
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_ipvec) (q.S()->q, buf, Xx, nc);
#else
	  CXSPARSE_DNAME (_ipvec) (nc, q.S()->Q, buf, Xx);
#endif
	  for (octave_idx_type j = nr; j < q.S()->m2; j++)
	    buf[j] = 0.;
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_ipvec) (q.S()->pinv, Xz, buf, nr);
#else
	  CXSPARSE_DNAME (_ipvec) (nr, q.S()->Pinv, Xz, buf);
#endif
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (volatile octave_idx_type j = 0; j < nc; j++)
	    {
	      OCTAVE_QUIT;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CXSPARSE_DNAME (_happly) (q.N()->L, j, q.N()->B[j], buf);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_DNAME (_usolve) (q.N()->U, buf);
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_ipvec) (q.S()->q, buf, Xz, nc);
#else
	  CXSPARSE_DNAME (_ipvec) (nc, q.S()->Q, buf, Xz);
#endif
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (octave_idx_type j = 0; j < nc; j++)
	    vec[j+idx] = Complex (Xx[j], Xz[j]);
	}
    }
  else
    {
      SparseMatrix at = a.hermitian();
      SparseQR q (at, 3);
      if (! q.ok ())
	{
	  info = -1;
	  return ComplexMatrix();
	}
      x.resize(nc, b_nc);
      Complex *vec = x.fortran_vec();
      volatile octave_idx_type nbuf = (nc > q.S()->m2 ? nc : q.S()->m2);
      OCTAVE_LOCAL_BUFFER (double, Xx, (b_nr > nc ? b_nr : nc));
      OCTAVE_LOCAL_BUFFER (double, Xz, (b_nr > nc ? b_nr : nc));
      OCTAVE_LOCAL_BUFFER (double, buf, nbuf);
      for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
	{
	  OCTAVE_QUIT;
	  for (octave_idx_type j = 0; j < b_nr; j++)
	    {
	      Complex c = b.xelem (j,i);
	      Xx[j] = std::real (c);
	      Xz[j] = std::imag (c);
	    }
	  for (octave_idx_type j = nr; j < nbuf; j++)
	    buf[j] = 0.;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_pvec) (q.S()->q, Xx, buf, nr);
#else
	  CXSPARSE_DNAME (_pvec) (nr, q.S()->Q, Xx, buf);
#endif
	  CXSPARSE_DNAME (_utsolve) (q.N()->U, buf);
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (volatile octave_idx_type j = nr-1; j >= 0; j--)
	    {
	      OCTAVE_QUIT;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CXSPARSE_DNAME (_happly) (q.N()->L, j, q.N()->B[j], buf);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_pvec) (q.S()->pinv, buf, Xx, nc);
#else
	  CXSPARSE_DNAME (_pvec) (nc, q.S()->Pinv, buf, Xx);
#endif
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (octave_idx_type j = nr; j < nbuf; j++)
	    buf[j] = 0.;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_pvec) (q.S()->q, Xz, buf, nr);
#else
	  CXSPARSE_DNAME (_pvec) (nr, q.S()->Q, Xz, buf);
#endif
	  CXSPARSE_DNAME (_utsolve) (q.N()->U, buf);
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (volatile octave_idx_type j = nr-1; j >= 0; j--)
	    {
	      OCTAVE_QUIT;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CXSPARSE_DNAME (_happly) (q.N()->L, j, q.N()->B[j], buf);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_pvec) (q.S()->pinv, buf, Xz, nc);
#else
	  CXSPARSE_DNAME (_pvec) (nc, q.S()->Pinv, buf, Xz);
#endif
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (octave_idx_type j = 0; j < nc; j++)
	    vec[j+idx] = Complex (Xx[j], Xz[j]);
	}
    }

  return x;
#else
  return ComplexMatrix ();
#endif
}

SparseComplexMatrix
qrsolve(const SparseMatrix&a, const SparseComplexMatrix &b, octave_idx_type &info)
{
#ifdef HAVE_CXSPARSE
  octave_idx_type nr = a.rows();
  octave_idx_type nc = a.cols();
  octave_idx_type b_nr = b.rows();
  octave_idx_type b_nc = b.cols();
  SparseComplexMatrix x;
  volatile octave_idx_type ii, x_nz;
  info = 0;

  if (nr < 1 || nc < 1 || nr != b_nr)
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch in solution of minimum norm problem");
  else if (nr >= nc)
    {
      SparseQR q (a, 3);
      if (! q.ok ()) 
	{
	  info = -1;
	  return SparseComplexMatrix();
	}
      x = SparseComplexMatrix (nc, b_nc, b.nzmax());
      x.xcidx(0) = 0;
      x_nz = b.nzmax();
      ii = 0;
      OCTAVE_LOCAL_BUFFER (double, Xx, (b_nr > nc ? b_nr : nc));
      OCTAVE_LOCAL_BUFFER (double, Xz, (b_nr > nc ? b_nr : nc));
      OCTAVE_LOCAL_BUFFER (double, buf, q.S()->m2);
      for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
	{
	  OCTAVE_QUIT;
	  for (octave_idx_type j = 0; j < b_nr; j++)
	    {
	      Complex c = b.xelem (j,i);
	      Xx[j] = std::real (c);
	      Xz[j] = std::imag (c);
	    }
	  for (octave_idx_type j = nr; j < q.S()->m2; j++)
	    buf[j] = 0.;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_ipvec) (q.S()->pinv, Xx, buf, nr);
#else
	  CXSPARSE_DNAME (_ipvec) (nr, q.S()->Pinv, Xx, buf);
#endif
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (volatile octave_idx_type j = 0; j < nc; j++)
	    {
	      OCTAVE_QUIT;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CXSPARSE_DNAME (_happly) (q.N()->L, j, q.N()->B[j], buf);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_DNAME (_usolve) (q.N()->U, buf);
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_ipvec) (q.S()->q, buf, Xx, nc);
#else
	  CXSPARSE_DNAME (_ipvec) (nc, q.S()->Q, buf, Xx);
#endif
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (octave_idx_type j = nr; j < q.S()->m2; j++)
	    buf[j] = 0.;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_ipvec) (q.S()->pinv, Xz, buf, nr);
#else
	  CXSPARSE_DNAME (_ipvec) (nr, q.S()->Pinv, Xz, buf);
#endif
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (volatile octave_idx_type j = 0; j < nc; j++)
	    {
	      OCTAVE_QUIT;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CXSPARSE_DNAME (_happly) (q.N()->L, j, q.N()->B[j], buf);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_DNAME (_usolve) (q.N()->U, buf);
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_ipvec) (q.S()->q, buf, Xz, nc);
#else
	  CXSPARSE_DNAME (_ipvec) (nc, q.S()->Q, buf, Xz);
#endif
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

	  for (octave_idx_type j = 0; j < nc; j++)
	    {
	      Complex tmp = Complex (Xx[j], Xz[j]);
	      if (tmp != 0.0)
		{
		  if (ii == x_nz)
		    {
		      // Resize the sparse matrix
		      octave_idx_type sz = x_nz * (b_nc - i) / b_nc;
		      sz = (sz > 10 ? sz : 10) + x_nz;
		      x.change_capacity (sz);
		      x_nz = sz;
		    }
		  x.xdata(ii) = tmp;
		  x.xridx(ii++) = j;
		}
	    }
	  x.xcidx(i+1) = ii;
	}
    }
  else
    {
      SparseMatrix at = a.hermitian();
      SparseQR q (at, 3);
      if (! q.ok ())
	{
	  info = -1;
	  return SparseComplexMatrix();
	}
      x = SparseComplexMatrix (nc, b_nc, b.nzmax());
      x.xcidx(0) = 0;
      x_nz = b.nzmax();
      ii = 0;
      volatile octave_idx_type nbuf = (nc > q.S()->m2 ? nc : q.S()->m2);
      OCTAVE_LOCAL_BUFFER (double, Xx, (b_nr > nc ? b_nr : nc));
      OCTAVE_LOCAL_BUFFER (double, Xz, (b_nr > nc ? b_nr : nc));
      OCTAVE_LOCAL_BUFFER (double, buf, nbuf);
      for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
	{
	  OCTAVE_QUIT;
	  for (octave_idx_type j = 0; j < b_nr; j++)
	    {
	      Complex c = b.xelem (j,i);
	      Xx[j] = std::real (c);
	      Xz[j] = std::imag (c);
	    }
	  for (octave_idx_type j = nr; j < nbuf; j++)
	    buf[j] = 0.;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_pvec) (q.S()->q, Xx, buf, nr);
#else
	  CXSPARSE_DNAME (_pvec) (nr, q.S()->Q, Xx, buf);
#endif
	  CXSPARSE_DNAME (_utsolve) (q.N()->U, buf);
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (volatile octave_idx_type j = nr-1; j >= 0; j--)
	    {
	      OCTAVE_QUIT;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CXSPARSE_DNAME (_happly) (q.N()->L, j, q.N()->B[j], buf);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_pvec) (q.S()->pinv, buf, Xx, nc);
#else
	  CXSPARSE_DNAME (_pvec) (nc, q.S()->Pinv, buf, Xx);
#endif
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (octave_idx_type j = nr; j < nbuf; j++)
	    buf[j] = 0.;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_pvec) (q.S()->q, Xz, buf, nr);
#else
	  CXSPARSE_DNAME (_pvec) (nr, q.S()->Q, Xz, buf);
#endif
	  CXSPARSE_DNAME (_utsolve) (q.N()->U, buf);
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (volatile octave_idx_type j = nr-1; j >= 0; j--)
	    {
	      OCTAVE_QUIT;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CXSPARSE_DNAME (_happly) (q.N()->L, j, q.N()->B[j], buf);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
#if defined(CS_VER) && (CS_VER >= 2)
	  CXSPARSE_DNAME (_pvec) (q.S()->pinv, buf, Xz, nc);
#else
	  CXSPARSE_DNAME (_pvec) (nc, q.S()->Pinv, buf, Xz);
#endif
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

	  for (octave_idx_type j = 0; j < nc; j++)
	    {
	      Complex tmp = Complex (Xx[j], Xz[j]);
	      if (tmp != 0.0)
		{
		  if (ii == x_nz)
		    {
		      // Resize the sparse matrix
		      octave_idx_type sz = x_nz * (b_nc - i) / b_nc;
		      sz = (sz > 10 ? sz : 10) + x_nz;
		      x.change_capacity (sz);
		      x_nz = sz;
		    }
		  x.xdata(ii) = tmp;
		  x.xridx(ii++) = j;
		}
	    }
	  x.xcidx(i+1) = ii;
	}
    }

  x.maybe_compress ();
  return x;
#else
  return SparseComplexMatrix ();
#endif
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
