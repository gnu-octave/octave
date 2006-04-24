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
#include "SparseCmplxQR.h"

// Why did g++ 4.x stl_vector.h make
//   OCTAVE_LOCAL_BUFFER (double _Complex, buf, n)
// an error ?
#define OCTAVE_C99_COMPLEX(buf, n) \
  OCTAVE_LOCAL_BUFFER (double, buf ## tmp, (2 * (n))); \
  double _Complex *buf = reinterpret_cast<double _Complex *> (buf ## tmp);

#define OCTAVE_C99_ZERO (0. + 0.iF);

SparseComplexQR::SparseComplexQR_rep::SparseComplexQR_rep 
(const SparseComplexMatrix& a, int order)
{
#ifdef HAVE_CXSPARSE
  CXSPARSE_ZNAME () A;
  A.nzmax = a.nnz ();
  A.m = a.rows ();
  A.n = a.cols ();
  nrows = A.m;
  // Cast away const on A, with full knowledge that CSparse won't touch it
  // Prevents the methods below making a copy of the data.
  A.p = const_cast<octave_idx_type *>(a.cidx ());
  A.i = const_cast<octave_idx_type *>(a.ridx ());
  A.x = const_cast<double _Complex *>(reinterpret_cast<const double _Complex *> 
				      (a.data ()));
  A.nz = -1;
  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
  S = CXSPARSE_ZNAME (_sqr) (&A, order, 1);
  N = CXSPARSE_ZNAME (_qr) (&A, S);
  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
  if (!N)
    (*current_liboctave_error_handler)
      ("SparseComplexQR: sparse matrix QR factorization filled");
  count = 1;
#else
  (*current_liboctave_error_handler)
    ("SparseComplexQR: sparse matrix QR factorization not implemented");
#endif
}

SparseComplexQR::SparseComplexQR_rep::~SparseComplexQR_rep (void)
{
#ifdef HAVE_CXSPARSE
  CXSPARSE_ZNAME (_sfree) (S);
  CXSPARSE_ZNAME (_nfree) (N);
#endif
}

SparseComplexMatrix 
SparseComplexQR::SparseComplexQR_rep::V (void) const
{
#ifdef HAVE_CXSPARSE
  // Drop zeros from V and sort
  // FIXME Is the double transpose to sort necessary?
  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
  CXSPARSE_ZNAME (_dropzeros) (N->L);
  CXSPARSE_ZNAME () *D = CXSPARSE_ZNAME (_transpose) (N->L, 1);
  CXSPARSE_ZNAME (_spfree) (N->L);
  N->L = CXSPARSE_ZNAME (_transpose) (D, 1);
  CXSPARSE_ZNAME (_spfree) (D);
  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

  octave_idx_type nc = N->L->n;
  octave_idx_type nz = N->L->nzmax;
  SparseComplexMatrix ret (N->L->m, nc, nz);
  for (octave_idx_type j = 0; j < nc+1; j++)
    ret.xcidx (j) = N->L->p[j];
  for (octave_idx_type j = 0; j < nz; j++)
    {
      ret.xridx (j) = N->L->i[j];
      ret.xdata (j) = reinterpret_cast<Complex *>(N->L->x)[j];
    }
  return ret;
#else
  return SparseComplexMatrix ();
#endif
}

ColumnVector 
SparseComplexQR::SparseComplexQR_rep::Pinv (void) const
{
#ifdef HAVE_CXSPARSE
  ColumnVector ret(N->L->m);
  for (octave_idx_type i = 0; i < N->L->m; i++)
    ret.xelem(i) = S->Pinv[i];
  return ret;
#else
  return ColumnVector ();
#endif
}

ColumnVector 
SparseComplexQR::SparseComplexQR_rep::P (void) const
{
#ifdef HAVE_CXSPARSE
  ColumnVector ret(N->L->m);
  for (octave_idx_type i = 0; i < N->L->m; i++)
    ret.xelem(S->Pinv[i]) = i;
  return ret;
#else
  return ColumnVector ();
#endif
}

SparseComplexMatrix 
SparseComplexQR::SparseComplexQR_rep::R (const bool econ) const
{
#ifdef HAVE_CXSPARSE
  // Drop zeros from R and sort
  // FIXME Is the double transpose to sort necessary?
  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
  CXSPARSE_ZNAME (_dropzeros) (N->U);
  CXSPARSE_ZNAME () *D = CXSPARSE_ZNAME (_transpose) (N->U, 1);
  CXSPARSE_ZNAME (_spfree) (N->U);
  N->U = CXSPARSE_ZNAME (_transpose) (D, 1);
  CXSPARSE_ZNAME (_spfree) (D);
  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

  octave_idx_type nc = N->U->n;
  octave_idx_type nz = N->U->nzmax;
  SparseComplexMatrix ret ((econ ? (nc > nrows ? nrows : nc) : nrows), nc, nz);
  for (octave_idx_type j = 0; j < nc+1; j++)
    ret.xcidx (j) = N->U->p[j];
  for (octave_idx_type j = 0; j < nz; j++)
    {
      ret.xridx (j) = N->U->i[j];
      ret.xdata (j) = reinterpret_cast<Complex *>(N->U->x)[j];
    }
  return ret;
#else
  return SparseComplexMatrix ();
#endif
}

ComplexMatrix
SparseComplexQR::SparseComplexQR_rep::C (const ComplexMatrix &b) const
{
#ifdef HAVE_CXSPARSE
  octave_idx_type b_nr = b.rows();
  octave_idx_type b_nc = b.cols();
  octave_idx_type nc = N->L->n;
  octave_idx_type nr = nrows;
  const double _Complex *bvec = 
    reinterpret_cast<const double _Complex *>(b.fortran_vec());
  ComplexMatrix ret(b_nr,b_nc);
  Complex *vec = ret.fortran_vec();
  if (nr < 1 || nc < 1 || nr != b_nr)
    (*current_liboctave_error_handler) ("matrix dimension mismatch");
  else
    {
      OCTAVE_LOCAL_BUFFER (Complex, buf, S->m2);
      for (volatile octave_idx_type j = 0, idx = 0; j < b_nc; j++, idx+=b_nr)
	{
	  OCTAVE_QUIT;
	  volatile octave_idx_type nm = (nr < nc ? nr : nc);
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_ZNAME (_ipvec) (b_nr, S->Pinv, bvec + idx,
				     reinterpret_cast<double _Complex *>(buf));
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (volatile octave_idx_type i = 0; i < nm; i++)
	    {
	      OCTAVE_QUIT;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CXSPARSE_ZNAME (_happly) 
		(N->L, i, N->B[i], reinterpret_cast<double _Complex *>(buf));
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
	  for (octave_idx_type i = 0; i < b_nr; i++)
	    vec[i+idx] = buf[i];
	}
    }
  return ret;
#else
  return ComplexMatrix ();
#endif
}

ComplexMatrix
qrsolve(const SparseComplexMatrix&a, const Matrix &b, octave_idx_type &info)
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
      SparseComplexQR q (a, 2);
      if (! q.ok ())
	{
	  info = -1;
	  return ComplexMatrix();
	}
      x.resize(nc, b_nc);
      double _Complex *vec = reinterpret_cast<double _Complex *>
	(x.fortran_vec());
      OCTAVE_C99_COMPLEX (buf, q.S()->m2);
      OCTAVE_LOCAL_BUFFER (Complex, Xx, b_nr);
      for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
	{
	  OCTAVE_QUIT;
	  for (octave_idx_type j = 0; j < b_nr; j++)
	    Xx[j] = b.xelem(j,i);
	  for (octave_idx_type j = nr; j < q.S()->m2; j++)
	    buf[j] = OCTAVE_C99_ZERO;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_ZNAME (_ipvec) 
	    (nr, q.S()->Pinv, reinterpret_cast<double _Complex *>(Xx), buf);
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (volatile octave_idx_type j = 0; j < nc; j++)
	    {
	      OCTAVE_QUIT;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CXSPARSE_ZNAME (_happly) (q.N()->L, j, q.N()->B[j], buf);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_ZNAME (_usolve) (q.N()->U, buf);
	  CXSPARSE_ZNAME (_ipvec) (nc, q.S()->Q, buf, vec + idx);
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	}
    }
  else
    {
      SparseComplexMatrix at = a.hermitian();
      SparseComplexQR q (at, 2);
      if (! q.ok ())
	{
	  info = -1;
	  return ComplexMatrix();
	}
      x.resize(nc, b_nc);
      double _Complex *vec = reinterpret_cast<double _Complex *>
	(x.fortran_vec());
      volatile octave_idx_type nbuf = (nc > q.S()->m2 ? nc : q.S()->m2);
      OCTAVE_C99_COMPLEX (buf, nbuf);
      OCTAVE_LOCAL_BUFFER (Complex, Xx, b_nr);
      OCTAVE_LOCAL_BUFFER (Complex, B, nr);
      for (octave_idx_type i = 0; i < nr; i++)
	B[i] = conj (reinterpret_cast<Complex *>(q.N()->B) [i]);
      for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
	{
	  OCTAVE_QUIT;
	  for (octave_idx_type j = 0; j < b_nr; j++)
	    Xx[j] = b.xelem(j,i);
	  for (octave_idx_type j = nr; j < nbuf; j++)
	    buf[j] = OCTAVE_C99_ZERO;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_ZNAME (_pvec)
	    (nr, q.S()->Q, reinterpret_cast<double _Complex *>(Xx), buf);
	  CXSPARSE_ZNAME (_utsolve) (q.N()->U, buf);
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (volatile octave_idx_type j = nr-1; j >= 0; j--)
	    {
	      OCTAVE_QUIT;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

	      CXSPARSE_ZNAME (_happly) 
		(q.N()->L, j, reinterpret_cast<double _Complex *>(B)[j], buf);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_ZNAME (_pvec) (nc, q.S()->Pinv, buf, vec + idx);
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	}
    }

  return x;
#else
  return ComplexMatrix ();
#endif
}

SparseComplexMatrix
qrsolve(const SparseComplexMatrix&a, const SparseMatrix &b, octave_idx_type &info)
{
#ifdef HAVE_CXSPARSE
  octave_idx_type nr = a.rows();
  octave_idx_type nc = a.cols();
  octave_idx_type b_nc = b.cols();
  octave_idx_type b_nr = b.rows();
  SparseComplexMatrix x;
  volatile octave_idx_type ii, x_nz;
  info = 0;

  if (nr < 1 || nc < 1 || nr != b_nr)
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch in solution of minimum norm problem");
  else if (nr >= nc)
    {
      SparseComplexQR q (a, 2);
      if (! q.ok ())
	{
	  info = -1;
	  return SparseComplexMatrix();
	}
      x = SparseComplexMatrix (nc, b_nc, b.nzmax());
      x.xcidx(0) = 0;
      x_nz = b.nzmax();
      ii = 0;
      OCTAVE_LOCAL_BUFFER (Complex, Xx, (b_nr > nc ? b_nr : nc));
      OCTAVE_C99_COMPLEX (buf, q.S()->m2);
      for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
	{
	  OCTAVE_QUIT;
	  for (octave_idx_type j = 0; j < b_nr; j++)
	    Xx[j] = b.xelem(j,i);
	  for (octave_idx_type j = nr; j < q.S()->m2; j++)
	    buf[j] = OCTAVE_C99_ZERO;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_ZNAME (_ipvec) 
	    (nr, q.S()->Pinv, reinterpret_cast<double _Complex *>(Xx), buf);
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (volatile octave_idx_type j = 0; j < nc; j++)
	    {
	      OCTAVE_QUIT;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CXSPARSE_ZNAME (_happly) (q.N()->L, j, q.N()->B[j], buf);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_ZNAME (_usolve) (q.N()->U, buf);
	  CXSPARSE_ZNAME (_ipvec) (nc, q.S()->Q, buf, 
				     reinterpret_cast<double _Complex *>(Xx));
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

	  for (octave_idx_type j = 0; j < nc; j++)
	    {
	      Complex tmp = Xx[j];
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
      SparseComplexMatrix at = a.hermitian();
      SparseComplexQR q (at, 2);
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
      OCTAVE_LOCAL_BUFFER (Complex, Xx, (b_nr > nc ? b_nr : nc));
      OCTAVE_C99_COMPLEX (buf, nbuf);
      OCTAVE_LOCAL_BUFFER (Complex, B, nr);
      for (octave_idx_type i = 0; i < nr; i++)
	B[i] = conj (reinterpret_cast<Complex *>(q.N()->B) [i]);
      for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
	{
	  OCTAVE_QUIT;
	  for (octave_idx_type j = 0; j < b_nr; j++)
	    Xx[j] = b.xelem(j,i);
	  for (octave_idx_type j = nr; j < nbuf; j++)
	    buf[j] = OCTAVE_C99_ZERO;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_ZNAME (_pvec)
	    (nr, q.S()->Q, reinterpret_cast<double _Complex *>(Xx), buf);
	  CXSPARSE_ZNAME (_utsolve) (q.N()->U, buf);
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (volatile octave_idx_type j = nr-1; j >= 0; j--)
	    {
	      OCTAVE_QUIT;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CXSPARSE_ZNAME (_happly) 
		(q.N()->L, j, reinterpret_cast<double _Complex *>(B)[j], buf);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_ZNAME (_pvec) (nc, q.S()->Pinv, buf, 
				     reinterpret_cast<double _Complex *>(Xx));
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

	  for (octave_idx_type j = 0; j < nc; j++)
	    {
	      Complex tmp = Xx[j];
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

ComplexMatrix
qrsolve(const SparseComplexMatrix&a, const ComplexMatrix &b, octave_idx_type &info)
{
#ifdef HAVE_CXSPARSE
  octave_idx_type nr = a.rows();
  octave_idx_type nc = a.cols();
  octave_idx_type b_nc = b.cols();
  octave_idx_type b_nr = b.rows();
  const double _Complex *bvec = 
    reinterpret_cast<const double _Complex *>(b.fortran_vec());
  ComplexMatrix x;
  info = 0;

  if (nr < 1 || nc < 1 || nr != b_nr)
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch in solution of minimum norm problem");
  else if (nr >= nc)
    {
      SparseComplexQR q (a, 2);
      if (! q.ok ())
	{
	  info = -1;
	  return ComplexMatrix();
	}
      x.resize(nc, b_nc);
      double _Complex *vec = reinterpret_cast<double _Complex *>
	(x.fortran_vec());
      OCTAVE_C99_COMPLEX (buf, q.S()->m2);
      for (volatile octave_idx_type i = 0, idx = 0, bidx = 0; i < b_nc; 
	   i++, idx+=nc, bidx+=b_nr)
	{
	  OCTAVE_QUIT;
	  for (octave_idx_type j = nr; j < q.S()->m2; j++)
	    buf[j] = OCTAVE_C99_ZERO;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_ZNAME (_ipvec) (nr, q.S()->Pinv, bvec + bidx, buf);
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (volatile octave_idx_type j = 0; j < nc; j++)
	    {
	      OCTAVE_QUIT;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CXSPARSE_ZNAME (_happly) (q.N()->L, j, q.N()->B[j], buf);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_ZNAME (_usolve) (q.N()->U, buf);
	  CXSPARSE_ZNAME (_ipvec) (nc, q.S()->Q, buf, vec + idx);
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	}
    }
  else
    {
      SparseComplexMatrix at = a.hermitian();
      SparseComplexQR q (at, 2);
      if (! q.ok ())
	{
	  info = -1;
	  return ComplexMatrix();
	}
      x.resize(nc, b_nc);
      double _Complex *vec = reinterpret_cast<double _Complex *>
	(x.fortran_vec());
      volatile octave_idx_type nbuf = (nc > q.S()->m2 ? nc : q.S()->m2);
      OCTAVE_C99_COMPLEX (buf, nbuf);
      OCTAVE_LOCAL_BUFFER (Complex, B, nr);
      for (octave_idx_type i = 0; i < nr; i++)
	B[i] = conj (reinterpret_cast<Complex *>(q.N()->B) [i]);
      for (volatile octave_idx_type i = 0, idx = 0, bidx = 0; i < b_nc; 
	   i++, idx+=nc, bidx+=b_nr)
	{
	  OCTAVE_QUIT;
	  for (octave_idx_type j = nr; j < nbuf; j++)
	    buf[j] = OCTAVE_C99_ZERO;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_ZNAME (_pvec) (nr, q.S()->Q, bvec + bidx, buf);
	  CXSPARSE_ZNAME (_utsolve) (q.N()->U, buf);
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (volatile octave_idx_type j = nr-1; j >= 0; j--)
	    {
	      OCTAVE_QUIT;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CXSPARSE_ZNAME (_happly) 
		(q.N()->L, j, reinterpret_cast<double _Complex *>(B)[j], buf);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_ZNAME (_pvec) (nc, q.S()->Pinv, buf, vec + idx);
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	}
    }

  return x;
#else
  return ComplexMatrix ();
#endif
}

SparseComplexMatrix
qrsolve(const SparseComplexMatrix&a, const SparseComplexMatrix &b, octave_idx_type &info)
{
#ifdef HAVE_CXSPARSE
  octave_idx_type nr = a.rows();
  octave_idx_type nc = a.cols();
  octave_idx_type b_nc = b.cols();
  octave_idx_type b_nr = b.rows();
  SparseComplexMatrix x;
  volatile octave_idx_type ii, x_nz;
  info = 0;

  if (nr < 1 || nc < 1 || nr != b_nr)
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch in solution of minimum norm problem");
  else if (nr >= nc)
    {
      SparseComplexQR q (a, 2);
      if (! q.ok ())
	{
	  info = -1;
	  return SparseComplexMatrix();
	}
      x = SparseComplexMatrix (nc, b_nc, b.nzmax());
      x.xcidx(0) = 0;
      x_nz = b.nzmax();
      ii = 0;
      OCTAVE_LOCAL_BUFFER (Complex, Xx, (b_nr > nc ? b_nr : nc));
      OCTAVE_C99_COMPLEX (buf, q.S()->m2);
      for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
	{
	  OCTAVE_QUIT;
	  for (octave_idx_type j = 0; j < b_nr; j++)
	    Xx[j] = b.xelem(j,i);
	  for (octave_idx_type j = nr; j < q.S()->m2; j++)
	    buf[j] = OCTAVE_C99_ZERO;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_ZNAME (_ipvec) 
	    (nr, q.S()->Pinv, reinterpret_cast<double _Complex *>(Xx), buf);
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (volatile octave_idx_type j = 0; j < nc; j++)
	    {
	      OCTAVE_QUIT;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CXSPARSE_ZNAME (_happly) (q.N()->L, j, q.N()->B[j], buf);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_ZNAME (_usolve) (q.N()->U, buf);
	  CXSPARSE_ZNAME (_ipvec) (nc, q.S()->Q, buf, 
				     reinterpret_cast<double _Complex *>(Xx));
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

	  for (octave_idx_type j = 0; j < nc; j++)
	    {
	      Complex tmp = Xx[j];
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
      SparseComplexMatrix at = a.hermitian();
      SparseComplexQR q (at, 2);
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
      OCTAVE_LOCAL_BUFFER (Complex, Xx, (b_nr > nc ? b_nr : nc));
      OCTAVE_C99_COMPLEX (buf, nbuf);
      OCTAVE_LOCAL_BUFFER (Complex, B, nr);
      for (octave_idx_type i = 0; i < nr; i++)
	B[i] = conj (reinterpret_cast<Complex *>(q.N()->B) [i]);
      for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
	{
	  OCTAVE_QUIT;
	  for (octave_idx_type j = 0; j < b_nr; j++)
	    Xx[j] = b.xelem(j,i);
	  for (octave_idx_type j = nr; j < nbuf; j++)
	    buf[j] = OCTAVE_C99_ZERO;
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_ZNAME (_pvec)
	    (nr, q.S()->Q, reinterpret_cast<double _Complex *>(Xx), buf);
	  CXSPARSE_ZNAME (_utsolve) (q.N()->U, buf);
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  for (volatile octave_idx_type j = nr-1; j >= 0; j--)
	    {
	      OCTAVE_QUIT;
	      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	      CXSPARSE_ZNAME (_happly) 
		(q.N()->L, j, reinterpret_cast<double _Complex *>(B)[j], buf);
	      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	    }
	  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
	  CXSPARSE_ZNAME (_pvec) (nc, q.S()->Pinv, buf, 
				     reinterpret_cast<double _Complex *>(Xx));
	  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

	  for (octave_idx_type j = 0; j < nc; j++)
	    {
	      Complex tmp = Xx[j];
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
