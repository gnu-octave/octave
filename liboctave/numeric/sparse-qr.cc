/*

Copyright (C) 2016 John W. Eaton
Copyright (C) 2005-2015 David Bateman

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
#  include <config.h>
#endif

#include "lo-error.h"
#include "oct-locbuf.h"
#include "oct-sparse.h"
#include "sparse-qr.h"

template <typename SPARSE_T>
class
cxsparse_types
{
};

template <>
class
cxsparse_types<SparseMatrix>
{
public:
#if defined (HAVE_CXSPARSE)
  typedef CXSPARSE_DNAME (s) symbolic_type;
  typedef CXSPARSE_DNAME (n) numeric_type;
#else
  typedef void symbolic_type;
  typedef void numeric_type;
#endif
};

template <>
class
cxsparse_types<SparseComplexMatrix>
{
public:
#if defined (HAVE_CXSPARSE)
  typedef CXSPARSE_ZNAME (s) symbolic_type;
  typedef CXSPARSE_ZNAME (n) numeric_type;
#else
  typedef void symbolic_type;
  typedef void numeric_type;
#endif
};

template <typename SPARSE_T>
class sparse_qr<SPARSE_T>::sparse_qr_rep
{
public:

  sparse_qr_rep (const SPARSE_T& a, int order);

  ~sparse_qr_rep (void);

  bool ok (void) const
  {
#if defined (HAVE_CXSPARSE)
    return (N && S);
#else
    return false;
#endif
  }

  SPARSE_T V (void) const;

  ColumnVector Pinv (void) const;

  ColumnVector P (void) const;

  SPARSE_T R (bool econ) const;

  typename SPARSE_T::dense_matrix_type
  C (const typename SPARSE_T::dense_matrix_type& b) const;

  typename SPARSE_T::dense_matrix_type
  Q (void) const;

  octave_refcount<int> count;

  octave_idx_type nrows;
  octave_idx_type ncols;

  typename cxsparse_types<SPARSE_T>::symbolic_type *S;
  typename cxsparse_types<SPARSE_T>::numeric_type *N;

  template <typename RHS_T, typename RET_T>
  RET_T
  tall_solve (const RHS_T& b, octave_idx_type& info) const;

  template <typename RHS_T, typename RET_T>
  RET_T
  wide_solve (const RHS_T& b, octave_idx_type& info) const;

private:

  // No copying!

  sparse_qr_rep (const sparse_qr_rep&);

  sparse_qr_rep& operator = (const sparse_qr_rep&);
};

template <typename SPARSE_T>
ColumnVector
sparse_qr<SPARSE_T>::sparse_qr_rep::Pinv (void) const
{
#if defined (HAVE_CXSPARSE)

  ColumnVector ret (N->L->m);

  for (octave_idx_type i = 0; i < N->L->m; i++)
    ret.xelem (i) = S->pinv[i];

  return ret;

#else

  return ColumnVector ();

#endif
}

template <typename SPARSE_T>
ColumnVector
sparse_qr<SPARSE_T>::sparse_qr_rep::P (void) const
{
#if defined (HAVE_CXSPARSE)

  ColumnVector ret (N->L->m);

  for (octave_idx_type i = 0; i < N->L->m; i++)
    ret.xelem (S->pinv[i]) = i;

  return ret;

#else

  return ColumnVector ();

#endif
}

// Specializations.

// Real-valued matrices.

template <>
sparse_qr<SparseMatrix>::sparse_qr_rep::sparse_qr_rep
  (const SparseMatrix& a, int order)
    : count (1), nrows (a.rows ()), ncols (a.columns ())
#if defined (HAVE_CXSPARSE)
    , S (0), N (0)
#endif
{
#if defined (HAVE_CXSPARSE)

  CXSPARSE_DNAME () A;

  A.nzmax = a.nnz ();
  A.m = nrows;
  A.n = ncols;
  // Cast away const on A, with full knowledge that CSparse won't touch it
  // Prevents the methods below making a copy of the data.
  A.p = const_cast<octave_idx_type *>(a.cidx ());
  A.i = const_cast<octave_idx_type *>(a.ridx ());
  A.x = const_cast<double *>(a.data ());
  A.nz = -1;

  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
  S = CXSPARSE_DNAME (_sqr) (order, &A, 1);
  N = CXSPARSE_DNAME (_qr) (&A, S);
  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

  if (! N)
    (*current_liboctave_error_handler)
      ("sparse_qr: sparse matrix QR factorization filled");

  count = 1;

#else

  (*current_liboctave_error_handler)
    ("sparse_qr: support for CXSparse was unavailable or disabled when liboctave was built");

#endif
}

template <>
sparse_qr<SparseMatrix>::sparse_qr_rep::~sparse_qr_rep (void)
{
#if defined (HAVE_CXSPARSE)
  CXSPARSE_DNAME (_sfree) (S);
  CXSPARSE_DNAME (_nfree) (N);
#endif
}

template <>
SparseMatrix
sparse_qr<SparseMatrix>::sparse_qr_rep::V (void) const
{
#if defined (HAVE_CXSPARSE)

  // Drop zeros from V and sort
  // FIXME: Is the double transpose to sort necessary?

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

template <>
SparseMatrix
sparse_qr<SparseMatrix>::sparse_qr_rep::R (bool econ) const
{
#if defined (HAVE_CXSPARSE)

  // Drop zeros from R and sort
  // FIXME: Is the double transpose to sort necessary?

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

template <>
Matrix
sparse_qr<SparseMatrix>::sparse_qr_rep::C (const Matrix& b) const
{
#if defined (HAVE_CXSPARSE)

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  octave_idx_type nc = N->L->n;
  octave_idx_type nr = nrows;

  const double *bvec = b.fortran_vec ();

  Matrix ret (b_nr, b_nc);
  double *vec = ret.fortran_vec ();

  if (nr < 0 || nc < 0 || nr != b_nr)
    (*current_liboctave_error_handler) ("matrix dimension mismatch");

  if (nr == 0 || nc == 0 || b_nc == 0)
    ret = Matrix (nc, b_nc, 0.0);
  else
    {
      OCTAVE_LOCAL_BUFFER (double, buf, S->m2);

      for (volatile octave_idx_type j = 0, idx = 0; j < b_nc; j++, idx+=b_nr)
        {
          octave_quit ();

          for (octave_idx_type i = nr; i < S->m2; i++)
            buf[i] = 0.;

          volatile octave_idx_type nm = (nr < nc ? nr : nc);

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_DNAME (_ipvec) (S->pinv, bvec + idx, buf, b_nr);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

          for (volatile octave_idx_type i = 0; i < nm; i++)
            {
              octave_quit ();

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

template <>
Matrix
sparse_qr<SparseMatrix>::sparse_qr_rep::Q (void) const
{
#if defined (HAVE_CXSPARSE)
  octave_idx_type nc = N->L->n;
  octave_idx_type nr = nrows;
  Matrix ret (nr, nr);
  double *vec = ret.fortran_vec ();

  if (nr < 0 || nc < 0)
    (*current_liboctave_error_handler) ("matrix dimension mismatch");

  if (nr == 0 || nc == 0)
    ret = Matrix (nc, nr, 0.0);
  else
    {
      OCTAVE_LOCAL_BUFFER (double, bvec, nr + 1);

      for (octave_idx_type i = 0; i < nr; i++)
        bvec[i] = 0.;

      OCTAVE_LOCAL_BUFFER (double, buf, S->m2);

      for (volatile octave_idx_type j = 0, idx = 0; j < nr; j++, idx+=nr)
        {
          octave_quit ();

          bvec[j] = 1.0;
          for (octave_idx_type i = nr; i < S->m2; i++)
            buf[i] = 0.;

          volatile octave_idx_type nm = (nr < nc ? nr : nc);

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_DNAME (_ipvec) (S->pinv, bvec, buf, nr);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

          for (volatile octave_idx_type i = 0; i < nm; i++)
            {
              octave_quit ();

              BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
              CXSPARSE_DNAME (_happly) (N->L, i, N->B[i], buf);
              END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
            }

          for (octave_idx_type i = 0; i < nr; i++)
            vec[i+idx] = buf[i];

          bvec[j] = 0.0;
        }
    }

  return ret.transpose ();

#else

  return Matrix ();

#endif
}

template <>
template <>
Matrix
sparse_qr<SparseMatrix>::sparse_qr_rep::tall_solve<MArray<double>, Matrix>
  (const MArray<double>& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  octave_idx_type nr = nrows;
  octave_idx_type nc = ncols;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  const double *bvec = b.data ();

  Matrix x (nc, b_nc);
  double *vec = x.fortran_vec ();

  OCTAVE_LOCAL_BUFFER (double, buf, S->m2);

  for (volatile octave_idx_type i = 0, idx = 0, bidx = 0; i < b_nc;
       i++, idx+=nc, bidx+=b_nr)
    {
      octave_quit ();

      for (octave_idx_type j = nr; j < S->m2; j++)
        buf[j] = 0.;

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_ipvec) (S->pinv, bvec + bidx, buf, nr);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (volatile octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
        }

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_usolve) (N->U, buf);
      CXSPARSE_DNAME (_ipvec) (S->q, buf, vec + idx, nc);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
    }

  info = 0;

  return x;

#else

  return Matrix ();

#endif
}

template <>
template <>
Matrix
sparse_qr<SparseMatrix>::sparse_qr_rep::wide_solve<MArray<double>, Matrix>
  (const MArray<double>& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  // These are swapped because the original matrix was transposed in
  // sparse_qr<SparseMatrix>::solve.

  octave_idx_type nr = ncols;
  octave_idx_type nc = nrows;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  const double *bvec = b.data ();

  Matrix x (nc, b_nc);
  double *vec = x.fortran_vec ();

  volatile octave_idx_type nbuf = (nc > S->m2 ? nc : S->m2);

  OCTAVE_LOCAL_BUFFER (double, buf, nbuf);

  for (volatile octave_idx_type i = 0, idx = 0, bidx = 0; i < b_nc;
       i++, idx+=nc, bidx+=b_nr)
    {
      octave_quit ();

      for (octave_idx_type j = nr; j < nbuf; j++)
        buf[j] = 0.;

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_pvec) (S->q, bvec + bidx, buf, nr);
      CXSPARSE_DNAME (_utsolve) (N->U, buf);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (volatile octave_idx_type j = nr-1; j >= 0; j--)
        {
          octave_quit ();

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
        }

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_pvec) (S->pinv, buf, vec + idx, nc);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
    }

  info = 0;

  return x;

#else

  return Matrix ();

#endif
}

template <>
template <>
SparseMatrix
sparse_qr<SparseMatrix>::sparse_qr_rep::tall_solve<SparseMatrix, SparseMatrix>
  (const SparseMatrix& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  octave_idx_type nr = nrows;
  octave_idx_type nc = ncols;

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  SparseMatrix x (nc, b_nc, b.nnz ());
  x.xcidx (0) = 0;

  volatile octave_idx_type x_nz = b.nnz ();
  volatile octave_idx_type ii = 0;

  OCTAVE_LOCAL_BUFFER (double, Xx, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (double, buf, S->m2);

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        Xx[j] = b.xelem (j,i);

      for (octave_idx_type j = nr; j < S->m2; j++)
        buf[j] = 0.;

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_ipvec) (S->pinv, Xx, buf, nr);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (volatile octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
        }

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_usolve) (N->U, buf);
      CXSPARSE_DNAME (_ipvec) (S->q, buf, Xx, nc);
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

              x.xdata (ii) = tmp;
              x.xridx (ii++) = j;
            }
        }

      x.xcidx (i+1) = ii;
    }

  info = 0;

  return x;

#else

  return SparseMatrix ();

#endif
}

template <>
template <>
SparseMatrix
sparse_qr<SparseMatrix>::sparse_qr_rep::wide_solve<SparseMatrix, SparseMatrix>
  (const SparseMatrix& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  // These are swapped because the original matrix was transposed in
  // sparse_qr<SparseMatrix>::solve.

  octave_idx_type nr = ncols;
  octave_idx_type nc = nrows;

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  SparseMatrix x (nc, b_nc, b.nnz ());
  x.xcidx (0) = 0;

  volatile octave_idx_type x_nz = b.nnz ();
  volatile octave_idx_type ii = 0;
  volatile octave_idx_type nbuf = (nc > S->m2 ? nc : S->m2);

  OCTAVE_LOCAL_BUFFER (double, Xx, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (double, buf, nbuf);

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        Xx[j] = b.xelem (j,i);

      for (octave_idx_type j = nr; j < nbuf; j++)
        buf[j] = 0.;

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_pvec) (S->q, Xx, buf, nr);
      CXSPARSE_DNAME (_utsolve) (N->U, buf);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (volatile octave_idx_type j = nr-1; j >= 0; j--)
        {
          octave_quit ();

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
        }

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_pvec) (S->pinv, buf, Xx, nc);
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

              x.xdata (ii) = tmp;
              x.xridx (ii++) = j;
            }
        }

      x.xcidx (i+1) = ii;
    }

  info = 0;

  x.maybe_compress ();

  return x;

#else

  return SparseMatrix ();

#endif
}

template <>
template <>
ComplexMatrix
sparse_qr<SparseMatrix>::sparse_qr_rep::tall_solve<MArray<Complex>, ComplexMatrix>
  (const MArray<Complex>& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  octave_idx_type nr = nrows;
  octave_idx_type nc = ncols;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  ComplexMatrix x (nc, b_nc);
  Complex *vec = x.fortran_vec ();

  OCTAVE_LOCAL_BUFFER (double, Xx, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (double, Xz, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (double, buf, S->m2);

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        {
          Complex c = b.xelem (j,i);
          Xx[j] = std::real (c);
          Xz[j] = std::imag (c);
        }

      for (octave_idx_type j = nr; j < S->m2; j++)
        buf[j] = 0.;

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_ipvec) (S->pinv, Xx, buf, nr);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (volatile octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
        }

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_usolve) (N->U, buf);
      CXSPARSE_DNAME (_ipvec) (S->q, buf, Xx, nc);

      for (octave_idx_type j = nr; j < S->m2; j++)
        buf[j] = 0.;

      CXSPARSE_DNAME (_ipvec) (S->pinv, Xz, buf, nr);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (volatile octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
        }

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_usolve) (N->U, buf);
      CXSPARSE_DNAME (_ipvec) (S->q, buf, Xz, nc);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (octave_idx_type j = 0; j < nc; j++)
        vec[j+idx] = Complex (Xx[j], Xz[j]);
    }

  info = 0;

  return x;

#else

  return ComplexMatrix ();

#endif
}

template <>
template <>
ComplexMatrix
sparse_qr<SparseMatrix>::sparse_qr_rep::wide_solve<MArray<Complex>, ComplexMatrix>
  (const MArray<Complex>& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  // These are swapped because the original matrix was transposed in
  // sparse_qr<SparseMatrix>::solve.

  octave_idx_type nr = ncols;
  octave_idx_type nc = nrows;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  ComplexMatrix x (nc, b_nc);
  Complex *vec = x.fortran_vec ();

  volatile octave_idx_type nbuf = (nc > S->m2 ? nc : S->m2);

  OCTAVE_LOCAL_BUFFER (double, Xx, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (double, Xz, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (double, buf, nbuf);

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        {
          Complex c = b.xelem (j,i);
          Xx[j] = std::real (c);
          Xz[j] = std::imag (c);
        }

      for (octave_idx_type j = nr; j < nbuf; j++)
        buf[j] = 0.;

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_pvec) (S->q, Xx, buf, nr);
      CXSPARSE_DNAME (_utsolve) (N->U, buf);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (volatile octave_idx_type j = nr-1; j >= 0; j--)
        {
          octave_quit ();

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
        }

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_pvec) (S->pinv, buf, Xx, nc);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (octave_idx_type j = nr; j < nbuf; j++)
        buf[j] = 0.;

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_pvec) (S->q, Xz, buf, nr);
      CXSPARSE_DNAME (_utsolve) (N->U, buf);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (volatile octave_idx_type j = nr-1; j >= 0; j--)
        {
          octave_quit ();

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
        }

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_pvec) (S->pinv, buf, Xz, nc);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (octave_idx_type j = 0; j < nc; j++)
        vec[j+idx] = Complex (Xx[j], Xz[j]);
    }

  info = 0;

  return x;

#else

  return ComplexMatrix ();

#endif
}

// Complex-valued matrices.

template <>
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::sparse_qr_rep
  (const SparseComplexMatrix& a, int order)
    : count (1), nrows (a.rows ()), ncols (a.columns ())
#if defined (HAVE_CXSPARSE)
    , S (0), N (0)
#endif
{
#if defined (HAVE_CXSPARSE)

  CXSPARSE_ZNAME () A;

  A.nzmax = a.nnz ();
  A.m = nrows;
  A.n = ncols;
  // Cast away const on A, with full knowledge that CSparse won't touch it
  // Prevents the methods below making a copy of the data.
  A.p = const_cast<octave_idx_type *>(a.cidx ());
  A.i = const_cast<octave_idx_type *>(a.ridx ());
  A.x = const_cast<cs_complex_t *>(reinterpret_cast<const cs_complex_t *> (a.data ()));
  A.nz = -1;

  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
  S = CXSPARSE_ZNAME (_sqr) (order, &A, 1);
  N = CXSPARSE_ZNAME (_qr) (&A, S);
  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

  if (! N)
    (*current_liboctave_error_handler)
      ("sparse_qr: sparse matrix QR factorization filled");

  count = 1;

#else

  (*current_liboctave_error_handler)
    ("sparse_qr: support for CXSparse was unavailable or disabled when liboctave was built");

#endif
}

template <>
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::~sparse_qr_rep (void)
{
#if defined (HAVE_CXSPARSE)
  CXSPARSE_ZNAME (_sfree) (S);
  CXSPARSE_ZNAME (_nfree) (N);
#endif
}

template <>
SparseComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::V (void) const
{
#if defined (HAVE_CXSPARSE)
  // Drop zeros from V and sort
  // FIXME: Is the double transpose to sort necessary?

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

template <>
SparseComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::R (bool econ) const
{
#if defined (HAVE_CXSPARSE)
  // Drop zeros from R and sort
  // FIXME: Is the double transpose to sort necessary?

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

template <>
ComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::C (const ComplexMatrix& b) const
{
#if defined (HAVE_CXSPARSE)
  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();
  octave_idx_type nc = N->L->n;
  octave_idx_type nr = nrows;
  const cs_complex_t *bvec = reinterpret_cast<const cs_complex_t *>(b.fortran_vec ());
  ComplexMatrix ret (b_nr, b_nc);
  Complex *vec = ret.fortran_vec ();

  if (nr < 0 || nc < 0 || nr != b_nr)
    (*current_liboctave_error_handler) ("matrix dimension mismatch");


  if (nr == 0 || nc == 0 || b_nc == 0)
    ret = ComplexMatrix (nc, b_nc, Complex (0.0, 0.0));
  else
    {
      OCTAVE_LOCAL_BUFFER (Complex, buf, S->m2);

      for (volatile octave_idx_type j = 0, idx = 0; j < b_nc; j++, idx+=b_nr)
        {
          octave_quit ();

          volatile octave_idx_type nm = (nr < nc ? nr : nc);

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_ZNAME (_ipvec) (S->pinv, bvec + idx, reinterpret_cast<cs_complex_t *>(buf), b_nr);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

          for (volatile octave_idx_type i = 0; i < nm; i++)
            {
              octave_quit ();

              BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
              CXSPARSE_ZNAME (_happly) (N->L, i, N->B[i], reinterpret_cast<cs_complex_t *>(buf));
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

template <>
ComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::Q (void) const
{
#if defined (HAVE_CXSPARSE)
  octave_idx_type nc = N->L->n;
  octave_idx_type nr = nrows;
  ComplexMatrix ret (nr, nr);
  Complex *vec = ret.fortran_vec ();

  if (nr < 0 || nc < 0)
    (*current_liboctave_error_handler) ("matrix dimension mismatch");

  if (nr == 0 || nc == 0)
    ret = ComplexMatrix (nc, nr, Complex (0.0, 0.0));
  else
    {
      OCTAVE_LOCAL_BUFFER (cs_complex_t, bvec, nr);

      for (octave_idx_type i = 0; i < nr; i++)
        bvec[i] = cs_complex_t (0.0, 0.0);

      OCTAVE_LOCAL_BUFFER (Complex, buf, S->m2);

      for (volatile octave_idx_type j = 0, idx = 0; j < nr; j++, idx+=nr)
        {
          octave_quit ();

          bvec[j] = cs_complex_t (1.0, 0.0);

          volatile octave_idx_type nm = (nr < nc ? nr : nc);

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_ZNAME (_ipvec) (S->pinv, bvec, reinterpret_cast<cs_complex_t *>(buf), nr);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

          for (volatile octave_idx_type i = 0; i < nm; i++)
            {
              octave_quit ();

              BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
              CXSPARSE_ZNAME (_happly) (N->L, i, N->B[i], reinterpret_cast<cs_complex_t *>(buf));
              END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
            }

          for (octave_idx_type i = 0; i < nr; i++)
            vec[i+idx] = buf[i];

          bvec[j] = cs_complex_t (0.0, 0.0);
        }
    }

  return ret.hermitian ();

#else

  return ComplexMatrix ();

#endif
}

template <>
template <>
SparseComplexMatrix
sparse_qr<SparseMatrix>::sparse_qr_rep::tall_solve<SparseComplexMatrix, SparseComplexMatrix>
  (const SparseComplexMatrix& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  octave_idx_type nr = nrows;
  octave_idx_type nc = ncols;

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  SparseComplexMatrix x (nc, b_nc, b.nnz ());
  x.xcidx (0) = 0;

  volatile octave_idx_type x_nz = b.nnz ();
  volatile octave_idx_type ii = 0;

  OCTAVE_LOCAL_BUFFER (double, Xx, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (double, Xz, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (double, buf, S->m2);

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        {
          Complex c = b.xelem (j,i);
          Xx[j] = std::real (c);
          Xz[j] = std::imag (c);
        }

      for (octave_idx_type j = nr; j < S->m2; j++)
        buf[j] = 0.;

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_ipvec) (S->pinv, Xx, buf, nr);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (volatile octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
        }

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_usolve) (N->U, buf);
      CXSPARSE_DNAME (_ipvec) (S->q, buf, Xx, nc);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (octave_idx_type j = nr; j < S->m2; j++)
        buf[j] = 0.;

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_ipvec) (S->pinv, Xz, buf, nr);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (volatile octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
        }

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_usolve) (N->U, buf);
      CXSPARSE_DNAME (_ipvec) (S->q, buf, Xz, nc);
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

              x.xdata (ii) = tmp;
              x.xridx (ii++) = j;
            }
        }

      x.xcidx (i+1) = ii;
    }

  info = 0;

  return x;

#else

  return SparseComplexMatrix ();

#endif
}

template <>
template <>
SparseComplexMatrix
sparse_qr<SparseMatrix>::sparse_qr_rep::wide_solve<SparseComplexMatrix, SparseComplexMatrix>
  (const SparseComplexMatrix& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  // These are swapped because the original matrix was transposed in
  // sparse_qr<SparseMatrix>::solve.

  octave_idx_type nr = ncols;
  octave_idx_type nc = nrows;

  octave_idx_type b_nr = b.rows ();
  octave_idx_type b_nc = b.cols ();

  SparseComplexMatrix x (nc, b_nc, b.nnz ());
  x.xcidx (0) = 0;

  volatile octave_idx_type x_nz = b.nnz ();
  volatile octave_idx_type ii = 0;
  volatile octave_idx_type nbuf = (nc > S->m2 ? nc : S->m2);

  OCTAVE_LOCAL_BUFFER (double, Xx, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (double, Xz, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (double, buf, nbuf);

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        {
          Complex c = b.xelem (j,i);
          Xx[j] = std::real (c);
          Xz[j] = std::imag (c);
        }

      for (octave_idx_type j = nr; j < nbuf; j++)
        buf[j] = 0.;

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_pvec) (S->q, Xx, buf, nr);
      CXSPARSE_DNAME (_utsolve) (N->U, buf);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (volatile octave_idx_type j = nr-1; j >= 0; j--)
        {
          octave_quit ();

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
        }

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_pvec) (S->pinv, buf, Xx, nc);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (octave_idx_type j = nr; j < nbuf; j++)
        buf[j] = 0.;

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_pvec) (S->q, Xz, buf, nr);
      CXSPARSE_DNAME (_utsolve) (N->U, buf);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (volatile octave_idx_type j = nr-1; j >= 0; j--)
        {
          octave_quit ();

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_DNAME (_happly) (N->L, j, N->B[j], buf);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
        }

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_DNAME (_pvec) (S->pinv, buf, Xz, nc);
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

              x.xdata (ii) = tmp;
              x.xridx (ii++) = j;
            }
        }

      x.xcidx (i+1) = ii;
    }

  info = 0;

  x.maybe_compress ();

  return x;

#else

  return SparseComplexMatrix ();

#endif
}

template <>
template <>
ComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::tall_solve<MArray<double>, ComplexMatrix>
  (const MArray<double>& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  octave_idx_type nr = nrows;
  octave_idx_type nc = ncols;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  ComplexMatrix x (nc, b_nc);
  cs_complex_t *vec = reinterpret_cast<cs_complex_t *> (x.fortran_vec ());

  OCTAVE_LOCAL_BUFFER (cs_complex_t, buf, S->m2);
  OCTAVE_LOCAL_BUFFER (Complex, Xx, b_nr);

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        Xx[j] = b.xelem (j,i);

      for (octave_idx_type j = nr; j < S->m2; j++)
        buf[j] = cs_complex_t (0.0, 0.0);

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_ZNAME (_ipvec) (S->pinv, reinterpret_cast<cs_complex_t *>(Xx), buf, nr);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (volatile octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_ZNAME (_happly) (N->L, j, N->B[j], buf);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
        }

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_ZNAME (_usolve) (N->U, buf);
      CXSPARSE_ZNAME (_ipvec) (S->q, buf, vec + idx, nc);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
    }

  info = 0;

  return x;

#else

  return ComplexMatrix ();

#endif
}

template <>
template <>
ComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::wide_solve<MArray<double>, ComplexMatrix>
  (const MArray<double>& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  // These are swapped because the original matrix was transposed in
  // sparse_qr<SparseComplexMatrix>::solve.

  octave_idx_type nr = ncols;
  octave_idx_type nc = nrows;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  ComplexMatrix x (nc, b_nc);
  cs_complex_t *vec = reinterpret_cast<cs_complex_t *> (x.fortran_vec ());

  volatile octave_idx_type nbuf = (nc > S->m2 ? nc : S->m2);

  OCTAVE_LOCAL_BUFFER (cs_complex_t, buf, nbuf);
  OCTAVE_LOCAL_BUFFER (Complex, Xx, b_nr);
  OCTAVE_LOCAL_BUFFER (double, B, nr);

  for (octave_idx_type i = 0; i < nr; i++)
    B[i] = N->B[i];

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        Xx[j] = b.xelem (j,i);

      for (octave_idx_type j = nr; j < nbuf; j++)
        buf[j] = cs_complex_t (0.0, 0.0);

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_ZNAME (_pvec) (S->q, reinterpret_cast<cs_complex_t *>(Xx), buf, nr);
      CXSPARSE_ZNAME (_utsolve) (N->U, buf);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (volatile octave_idx_type j = nr-1; j >= 0; j--)
        {
          octave_quit ();

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_ZNAME (_happly) (N->L, j, B[j], buf);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
        }

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_ZNAME (_pvec) (S->pinv, buf, vec + idx, nc);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
    }

  info = 0;

  return x;

#else

  return ComplexMatrix ();

#endif
}

template <>
template <>
SparseComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::tall_solve<SparseMatrix, SparseComplexMatrix>
  (const SparseMatrix& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  octave_idx_type nr = nrows;
  octave_idx_type nc = ncols;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  SparseComplexMatrix x (nc, b_nc, b.nnz ());
  x.xcidx (0) = 0;

  volatile octave_idx_type x_nz = b.nnz ();
  volatile octave_idx_type ii = 0;

  OCTAVE_LOCAL_BUFFER (Complex, Xx, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (cs_complex_t, buf, S->m2);

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        Xx[j] = b.xelem (j,i);

      for (octave_idx_type j = nr; j < S->m2; j++)
        buf[j] = cs_complex_t (0.0, 0.0);

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_ZNAME (_ipvec) (S->pinv, reinterpret_cast<cs_complex_t *>(Xx), buf, nr);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (volatile octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_ZNAME (_happly) (N->L, j, N->B[j], buf);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
        }

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_ZNAME (_usolve) (N->U, buf);
      CXSPARSE_ZNAME (_ipvec) (S->q, buf, reinterpret_cast<cs_complex_t *>(Xx), nc);
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

              x.xdata (ii) = tmp;
              x.xridx (ii++) = j;
            }
        }

      x.xcidx (i+1) = ii;
    }

  info = 0;

  x.maybe_compress ();

  return x;

#else

  return SparseComplexMatrix ();

#endif
}

template <>
template <>
SparseComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::wide_solve<SparseMatrix, SparseComplexMatrix>
  (const SparseMatrix& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  // These are swapped because the original matrix was transposed in
  // sparse_qr<SparseComplexMatrix>::solve.

  octave_idx_type nr = ncols;
  octave_idx_type nc = nrows;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  SparseComplexMatrix x (nc, b_nc, b.nnz ());
  x.xcidx (0) = 0;

  volatile octave_idx_type x_nz = b.nnz ();
  volatile octave_idx_type ii = 0;
  volatile octave_idx_type nbuf = (nc > S->m2 ? nc : S->m2);

  OCTAVE_LOCAL_BUFFER (Complex, Xx, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (cs_complex_t, buf, nbuf);
  OCTAVE_LOCAL_BUFFER (double, B, nr);

  for (octave_idx_type i = 0; i < nr; i++)
    B[i] = N->B[i];

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        Xx[j] = b.xelem (j,i);

      for (octave_idx_type j = nr; j < nbuf; j++)
        buf[j] = cs_complex_t (0.0, 0.0);

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_ZNAME (_pvec) (S->q, reinterpret_cast<cs_complex_t *>(Xx), buf, nr);
      CXSPARSE_ZNAME (_utsolve) (N->U, buf);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (volatile octave_idx_type j = nr-1; j >= 0; j--)
        {
          octave_quit ();

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_ZNAME (_happly) (N->L, j, B[j], buf);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
        }

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_ZNAME (_pvec) (S->pinv, buf, reinterpret_cast<cs_complex_t *>(Xx), nc);
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

              x.xdata (ii) = tmp;
              x.xridx (ii++) = j;
            }
        }

      x.xcidx (i+1) = ii;
    }

  info = 0;

  x.maybe_compress ();

  return x;

#else

  return SparseComplexMatrix ();

#endif
}

template <>
template <>
ComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::tall_solve<MArray<Complex>, ComplexMatrix>
  (const MArray<Complex>& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  octave_idx_type nr = nrows;
  octave_idx_type nc = ncols;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  const cs_complex_t *bvec = reinterpret_cast<const cs_complex_t *>(b.fortran_vec ());

  ComplexMatrix x (nc, b_nc);
  cs_complex_t *vec = reinterpret_cast<cs_complex_t *>
    (x.fortran_vec ());

  OCTAVE_LOCAL_BUFFER (cs_complex_t, buf, S->m2);

  for (volatile octave_idx_type i = 0, idx = 0, bidx = 0; i < b_nc;
       i++, idx+=nc, bidx+=b_nr)
    {
      octave_quit ();

      for (octave_idx_type j = nr; j < S->m2; j++)
        buf[j] = cs_complex_t (0.0, 0.0);

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_ZNAME (_ipvec) (S->pinv, bvec + bidx, buf, nr);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (volatile octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_ZNAME (_happly) (N->L, j, N->B[j], buf);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
        }

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_ZNAME (_usolve) (N->U, buf);
      CXSPARSE_ZNAME (_ipvec) (S->q, buf, vec + idx, nc);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
    }

  info = 0;

  return x;

#else

  return ComplexMatrix ();

#endif
}

template <>
template <>
ComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::wide_solve<MArray<Complex>, ComplexMatrix>
  (const MArray<Complex>& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  // These are swapped because the original matrix was transposed in
  // sparse_qr<SparseComplexMatrix>::solve.

  octave_idx_type nr = ncols;
  octave_idx_type nc = nrows;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  const cs_complex_t *bvec = reinterpret_cast<const cs_complex_t *>(b.fortran_vec ());

  ComplexMatrix x (nc, b_nc);
  cs_complex_t *vec = reinterpret_cast<cs_complex_t *> (x.fortran_vec ());

  volatile octave_idx_type nbuf = (nc > S->m2 ? nc : S->m2);

  OCTAVE_LOCAL_BUFFER (cs_complex_t, buf, nbuf);
  OCTAVE_LOCAL_BUFFER (double, B, nr);

  for (octave_idx_type i = 0; i < nr; i++)
    B[i] = N->B[i];

  for (volatile octave_idx_type i = 0, idx = 0, bidx = 0; i < b_nc;
       i++, idx+=nc, bidx+=b_nr)
    {
      octave_quit ();

      for (octave_idx_type j = nr; j < nbuf; j++)
        buf[j] = cs_complex_t (0.0, 0.0);

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_ZNAME (_pvec) (S->q, bvec + bidx, buf, nr);
      CXSPARSE_ZNAME (_utsolve) (N->U, buf);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (volatile octave_idx_type j = nr-1; j >= 0; j--)
        {
          octave_quit ();

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_ZNAME (_happly) (N->L, j, B[j], buf);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
        }

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_ZNAME (_pvec) (S->pinv, buf, vec + idx, nc);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
    }

  info = 0;

  return x;

#else

  return ComplexMatrix ();

#endif
}

template <>
template <>
SparseComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::tall_solve<SparseComplexMatrix, SparseComplexMatrix>
  (const SparseComplexMatrix& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  octave_idx_type nr = nrows;
  octave_idx_type nc = ncols;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  SparseComplexMatrix x (nc, b_nc, b.nnz ());
  x.xcidx (0) = 0;

  volatile octave_idx_type x_nz = b.nnz ();
  volatile octave_idx_type ii = 0;

  OCTAVE_LOCAL_BUFFER (Complex, Xx, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (cs_complex_t, buf, S->m2);

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        Xx[j] = b.xelem (j,i);

      for (octave_idx_type j = nr; j < S->m2; j++)
        buf[j] = cs_complex_t (0.0, 0.0);

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_ZNAME (_ipvec) (S->pinv, reinterpret_cast<cs_complex_t *>(Xx), buf, nr);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (volatile octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_ZNAME (_happly) (N->L, j, N->B[j], buf);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
        }

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_ZNAME (_usolve) (N->U, buf);
      CXSPARSE_ZNAME (_ipvec) (S->q, buf, reinterpret_cast<cs_complex_t *>(Xx), nc);
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

              x.xdata (ii) = tmp;
              x.xridx (ii++) = j;
            }
        }

      x.xcidx (i+1) = ii;
    }

  info = 0;

  x.maybe_compress ();

  return x;

#else

  return SparseComplexMatrix ();

#endif
}

template <>
template <>
SparseComplexMatrix
sparse_qr<SparseComplexMatrix>::sparse_qr_rep::wide_solve<SparseComplexMatrix, SparseComplexMatrix>
  (const SparseComplexMatrix& b, octave_idx_type& info) const
{
  info = -1;

#if defined (HAVE_CXSPARSE)

  // These are swapped because the original matrix was transposed in
  // sparse_qr<SparseComplexMatrix>::solve.

  octave_idx_type nr = ncols;
  octave_idx_type nc = nrows;

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  SparseComplexMatrix x (nc, b_nc, b.nnz ());
  x.xcidx (0) = 0;

  volatile octave_idx_type x_nz = b.nnz ();
  volatile octave_idx_type ii = 0;
  volatile octave_idx_type nbuf = (nc > S->m2 ? nc : S->m2);

  OCTAVE_LOCAL_BUFFER (Complex, Xx, (b_nr > nc ? b_nr : nc));
  OCTAVE_LOCAL_BUFFER (cs_complex_t, buf, nbuf);
  OCTAVE_LOCAL_BUFFER (double, B, nr);

  for (octave_idx_type i = 0; i < nr; i++)
    B[i] = N->B[i];

  for (volatile octave_idx_type i = 0, idx = 0; i < b_nc; i++, idx+=nc)
    {
      octave_quit ();

      for (octave_idx_type j = 0; j < b_nr; j++)
        Xx[j] = b.xelem (j,i);

      for (octave_idx_type j = nr; j < nbuf; j++)
        buf[j] = cs_complex_t (0.0, 0.0);

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_ZNAME (_pvec) (S->q, reinterpret_cast<cs_complex_t *>(Xx), buf, nr);
      CXSPARSE_ZNAME (_utsolve) (N->U, buf);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

      for (volatile octave_idx_type j = nr-1; j >= 0; j--)
        {
          octave_quit ();

          BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
          CXSPARSE_ZNAME (_happly) (N->L, j, B[j], buf);
          END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
        }

      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CXSPARSE_ZNAME (_pvec) (S->pinv, buf, reinterpret_cast<cs_complex_t *>(Xx), nc);
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

              x.xdata (ii) = tmp;
              x.xridx (ii++) = j;
            }
        }

      x.xcidx (i+1) = ii;
    }

  info = 0;

  x.maybe_compress ();

  return x;

#else

  return SparseComplexMatrix ();

#endif
}

template <typename SPARSE_T>
sparse_qr<SPARSE_T>::sparse_qr (void)
  : rep (new sparse_qr_rep (SPARSE_T (), 0))
{ }

template <typename SPARSE_T>
sparse_qr<SPARSE_T>::sparse_qr (const SPARSE_T& a, int order)
  : rep (new sparse_qr_rep (a, order))
{ }

template <typename SPARSE_T>
sparse_qr<SPARSE_T>::sparse_qr (const sparse_qr<SPARSE_T>& a)
  : rep (a.rep)
{
  rep->count++;
}

template <typename SPARSE_T>
sparse_qr<SPARSE_T>::~sparse_qr (void)
{
  if (--rep->count == 0)
    delete rep;
}

template <typename SPARSE_T>
sparse_qr<SPARSE_T>&
sparse_qr<SPARSE_T>::operator = (const sparse_qr<SPARSE_T>& a)
{
  if (this != &a)
    {
      if (--rep->count == 0)
        delete rep;

      rep = a.rep;
      rep->count++;
    }

  return *this;
}

template <typename SPARSE_T>
bool
sparse_qr<SPARSE_T>::ok (void) const
{
  return rep->ok ();
}

template <typename SPARSE_T>
SPARSE_T
sparse_qr<SPARSE_T>::V (void) const
{
  return rep->V ();
}

template <typename SPARSE_T>
ColumnVector
sparse_qr<SPARSE_T>::Pinv (void) const
{
  return rep->P ();
}

template <typename SPARSE_T>
ColumnVector
sparse_qr<SPARSE_T>::P (void) const
{
  return rep->P ();
}

template <typename SPARSE_T>
SPARSE_T
sparse_qr<SPARSE_T>::R (bool econ) const
{
  return rep->R (econ);
}

template <typename SPARSE_T>
typename SPARSE_T::dense_matrix_type
sparse_qr<SPARSE_T>::C (const typename SPARSE_T::dense_matrix_type& b) const
{
  return rep->C (b);
}

template <typename SPARSE_T>
typename SPARSE_T::dense_matrix_type
sparse_qr<SPARSE_T>::Q (void) const
{
  return rep->Q ();
}

// FIXME: Why is the "order" of the QR calculation as used in the
// CXSparse function sqr 3 for real matrices and 2 for complex?  These
// values seem to be required but there was no explanation in David
// Bateman's original code.

template <typename SPARSE_T>
class
cxsparse_defaults
{
public:
  enum { order = -1 };
};

template <>
class
cxsparse_defaults<SparseMatrix>
{
public:
  enum { order = 3 };
};

template <>
class
cxsparse_defaults<SparseComplexMatrix>
{
public:
  enum { order = 2 };
};

template <typename SPARSE_T>
template <typename RHS_T, typename RET_T>
RET_T
sparse_qr<SPARSE_T>::solve (const SPARSE_T& a, const RHS_T& b,
                            octave_idx_type& info)
{
  info = -1;

  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  octave_idx_type b_nc = b.cols ();
  octave_idx_type b_nr = b.rows ();

  int order = cxsparse_defaults<SPARSE_T>::order;

  if (nr < 0 || nc < 0 || nr != b_nr)
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch in solution of minimum norm problem");

  if (nr == 0 || nc == 0 || b_nc == 0)
    {
      info = 0;

      return RET_T (nc, b_nc, 0.0);
    }
  else if (nr >= nc)
    {
      sparse_qr<SPARSE_T> q (a, order);

      return q.ok () ? q.tall_solve<RHS_T, RET_T> (b, info) : RET_T ();
    }
  else
    {
      sparse_qr<SPARSE_T> q (a.hermitian (), order);

      return q.ok () ? q.wide_solve<RHS_T, RET_T> (b, info) : RET_T ();
    }
}

template <typename SPARSE_T>
template <typename RHS_T, typename RET_T>
RET_T
sparse_qr<SPARSE_T>::tall_solve (const RHS_T& b, octave_idx_type& info) const
{
  return rep->tall_solve<RHS_T, RET_T> (b, info);
}

template <typename SPARSE_T>
template <typename RHS_T, typename RET_T>
RET_T
sparse_qr<SPARSE_T>::wide_solve (const RHS_T& b, octave_idx_type& info) const
{
  return rep->wide_solve<RHS_T, RET_T> (b, info);
}

Matrix
qrsolve (const SparseMatrix& a, const MArray<double>& b,
         octave_idx_type& info)
{
  return sparse_qr<SparseMatrix>::solve<MArray<double>, Matrix> (a, b, info);
}

SparseMatrix
qrsolve (const SparseMatrix& a, const SparseMatrix& b,
         octave_idx_type& info)
{
  return sparse_qr<SparseMatrix>::solve<SparseMatrix, SparseMatrix> (a, b, info);
}

ComplexMatrix
qrsolve (const SparseMatrix& a, const MArray<Complex>& b,
         octave_idx_type& info)
{
  return sparse_qr<SparseMatrix>::solve<MArray<Complex>, ComplexMatrix> (a, b, info);
}

SparseComplexMatrix
qrsolve (const SparseMatrix& a, const SparseComplexMatrix& b,
         octave_idx_type& info)
{
  return sparse_qr<SparseMatrix>::solve<SparseComplexMatrix, SparseComplexMatrix> (a, b, info);
}

ComplexMatrix
qrsolve (const SparseComplexMatrix& a, const MArray<double>& b,
         octave_idx_type& info)
{
  return sparse_qr<SparseComplexMatrix>::solve<MArray<double>, ComplexMatrix> (a, b, info);
}

SparseComplexMatrix
qrsolve (const SparseComplexMatrix& a, const SparseMatrix& b,
         octave_idx_type& info)
{
  return sparse_qr<SparseComplexMatrix>::solve<SparseMatrix, SparseComplexMatrix> (a, b, info);
}

ComplexMatrix
qrsolve (const SparseComplexMatrix& a, const MArray<Complex>& b,
         octave_idx_type& info)
{
  return sparse_qr<SparseComplexMatrix>::solve<MArray<Complex>, ComplexMatrix> (a, b, info);
}

SparseComplexMatrix
qrsolve (const SparseComplexMatrix& a, const SparseComplexMatrix& b,
         octave_idx_type& info)
{
  return sparse_qr<SparseComplexMatrix>::solve<SparseComplexMatrix, SparseComplexMatrix> (a, b, info);
}

// Instantiations we need.

template class sparse_qr<SparseMatrix>;

template class sparse_qr<SparseComplexMatrix>;
