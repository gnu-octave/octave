////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1994-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_fCMatrix_h)
#define octave_fCMatrix_h 1

#include "octave-config.h"

#include "DET.h"
#include "MArray.h"
#include "MDiagArray2.h"
#include "MatrixType.h"
#include "fCNDArray.h"
#include "mx-defs.h"
#include "mx-op-decl.h"
#include "oct-cmplx.h"

class
OCTAVE_API
FloatComplexMatrix : public FloatComplexNDArray
{
public:

  typedef FloatComplexColumnVector column_vector_type;
  typedef FloatComplexRowVector row_vector_type;

  typedef FloatColumnVector real_column_vector_type;
  typedef FloatRowVector real_row_vector_type;

  typedef FloatMatrix real_matrix_type;
  typedef FloatComplexMatrix complex_matrix_type;

  typedef FloatDiagMatrix real_diag_matrix_type;
  typedef FloatComplexDiagMatrix complex_diag_matrix_type;

  typedef float real_elt_type;
  typedef FloatComplex complex_elt_type;

  typedef void (*solve_singularity_handler) (float rcon);

  FloatComplexMatrix (void) = default;

  FloatComplexMatrix (const FloatComplexMatrix& a) = default;

  FloatComplexMatrix& operator = (const FloatComplexMatrix& a) = default;

  ~FloatComplexMatrix (void) = default;

  FloatComplexMatrix (octave_idx_type r, octave_idx_type c)
    : FloatComplexNDArray (dim_vector (r, c)) { }

  FloatComplexMatrix (octave_idx_type r, octave_idx_type c,
                      const FloatComplex& val)
    : FloatComplexNDArray (dim_vector (r, c), val) { }

  FloatComplexMatrix (const dim_vector& dv)
    : FloatComplexNDArray (dv.redim (2)) { }

  FloatComplexMatrix (const dim_vector& dv, const FloatComplex& val)
    : FloatComplexNDArray (dv.redim (2), val) { }

  template <typename U>
  FloatComplexMatrix (const MArray<U>& a)
    : FloatComplexNDArray (a.as_matrix ()) { }

  template <typename U>
  FloatComplexMatrix (const Array<U>& a)
    : FloatComplexNDArray (a.as_matrix ()) { }

  explicit OCTAVE_API FloatComplexMatrix (const FloatMatrix& a);

  explicit OCTAVE_API FloatComplexMatrix (const FloatRowVector& rv);

  explicit OCTAVE_API FloatComplexMatrix (const FloatColumnVector& cv);

  explicit OCTAVE_API FloatComplexMatrix (const FloatDiagMatrix& a);

  explicit OCTAVE_API FloatComplexMatrix (const MDiagArray2<float>& a);

  explicit OCTAVE_API FloatComplexMatrix (const DiagArray2<float>& a);

  explicit OCTAVE_API FloatComplexMatrix (const FloatComplexRowVector& rv);

  explicit OCTAVE_API FloatComplexMatrix (const FloatComplexColumnVector& cv);

  explicit OCTAVE_API FloatComplexMatrix (const FloatComplexDiagMatrix& a);

  explicit OCTAVE_API FloatComplexMatrix (const MDiagArray2<FloatComplex>& a);

  explicit OCTAVE_API FloatComplexMatrix (const DiagArray2<FloatComplex>& a);

  explicit OCTAVE_API FloatComplexMatrix (const boolMatrix& a);

  explicit OCTAVE_API FloatComplexMatrix (const charMatrix& a);

  OCTAVE_API FloatComplexMatrix (const FloatMatrix& re, const FloatMatrix& im);

  OCTAVE_API bool operator == (const FloatComplexMatrix& a) const;
  OCTAVE_API bool operator != (const FloatComplexMatrix& a) const;

  OCTAVE_API bool ishermitian (void) const;

  // destructive insert/delete/reorder operations

  OCTAVE_API FloatComplexMatrix&
  insert (const FloatMatrix& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API FloatComplexMatrix&
  insert (const FloatRowVector& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API FloatComplexMatrix&
  insert (const FloatColumnVector& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API FloatComplexMatrix&
  insert (const FloatDiagMatrix& a, octave_idx_type r, octave_idx_type c);

  OCTAVE_API FloatComplexMatrix&
  insert (const FloatComplexMatrix& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API FloatComplexMatrix&
  insert (const FloatComplexRowVector& a, octave_idx_type r,
          octave_idx_type c);
  OCTAVE_API FloatComplexMatrix&
  insert (const FloatComplexColumnVector& a, octave_idx_type r,
          octave_idx_type c);
  OCTAVE_API FloatComplexMatrix&
  insert (const FloatComplexDiagMatrix& a, octave_idx_type r,
          octave_idx_type c);

  OCTAVE_API FloatComplexMatrix& fill (float val);
  OCTAVE_API FloatComplexMatrix& fill (const FloatComplex& val);
  OCTAVE_API FloatComplexMatrix&
  fill (float val, octave_idx_type r1, octave_idx_type c1,
        octave_idx_type r2, octave_idx_type c2);
  OCTAVE_API FloatComplexMatrix&
  fill (const FloatComplex& val, octave_idx_type r1, octave_idx_type c1,
        octave_idx_type r2, octave_idx_type c2);

  OCTAVE_API FloatComplexMatrix append (const FloatMatrix& a) const;
  OCTAVE_API FloatComplexMatrix append (const FloatRowVector& a) const;
  OCTAVE_API FloatComplexMatrix append (const FloatColumnVector& a) const;
  OCTAVE_API FloatComplexMatrix append (const FloatDiagMatrix& a) const;

  OCTAVE_API FloatComplexMatrix append (const FloatComplexMatrix& a) const;
  OCTAVE_API FloatComplexMatrix append (const FloatComplexRowVector& a) const;
  OCTAVE_API FloatComplexMatrix
  append (const FloatComplexColumnVector& a) const;
  OCTAVE_API FloatComplexMatrix append (const FloatComplexDiagMatrix& a) const;

  OCTAVE_API FloatComplexMatrix stack (const FloatMatrix& a) const;
  OCTAVE_API FloatComplexMatrix stack (const FloatRowVector& a) const;
  OCTAVE_API FloatComplexMatrix stack (const FloatColumnVector& a) const;
  OCTAVE_API FloatComplexMatrix stack (const FloatDiagMatrix& a) const;

  OCTAVE_API FloatComplexMatrix stack (const FloatComplexMatrix& a) const;
  OCTAVE_API FloatComplexMatrix stack (const FloatComplexRowVector& a) const;
  OCTAVE_API FloatComplexMatrix stack (const FloatComplexColumnVector& a) const;
  OCTAVE_API FloatComplexMatrix stack (const FloatComplexDiagMatrix& a) const;

  FloatComplexMatrix hermitian (void) const
  { return MArray<FloatComplex>::hermitian (std::conj); }
  FloatComplexMatrix transpose (void) const
  { return MArray<FloatComplex>::transpose (); }

  friend OCTAVE_API FloatComplexMatrix conj (const FloatComplexMatrix& a);

  // resize is the destructive equivalent for this one

  OCTAVE_API FloatComplexMatrix
  extract (octave_idx_type r1, octave_idx_type c1,
           octave_idx_type r2, octave_idx_type c2) const;

  OCTAVE_API FloatComplexMatrix
  extract_n (octave_idx_type r1, octave_idx_type c1,
             octave_idx_type nr, octave_idx_type nc) const;

  // extract row or column i.

  OCTAVE_API FloatComplexRowVector row (octave_idx_type i) const;

  OCTAVE_API FloatComplexColumnVector column (octave_idx_type i) const;

  void resize (octave_idx_type nr, octave_idx_type nc,
               const FloatComplex& rfv = FloatComplex (0))
  {
    MArray<FloatComplex>::resize (dim_vector (nr, nc), rfv);
  }

private:
  FloatComplexMatrix tinverse (MatrixType& mattype, octave_idx_type& info,
                               float& rcon, bool force, bool calc_cond) const;

  FloatComplexMatrix finverse (MatrixType& mattype, octave_idx_type& info,
                               float& rcon, bool force, bool calc_cond) const;

public:
  OCTAVE_API FloatComplexMatrix inverse (void) const;
  OCTAVE_API FloatComplexMatrix inverse (octave_idx_type& info) const;
  OCTAVE_API FloatComplexMatrix
  inverse (octave_idx_type& info, float& rcon, bool force = false,
           bool calc_cond = true) const;

  OCTAVE_API FloatComplexMatrix inverse (MatrixType& mattype) const;
  OCTAVE_API FloatComplexMatrix
  inverse (MatrixType& mattype, octave_idx_type& info) const;
  OCTAVE_API FloatComplexMatrix
  inverse (MatrixType& mattype, octave_idx_type& info, float& rcon,
           bool force = false, bool calc_cond = true) const;

  OCTAVE_API FloatComplexMatrix pseudo_inverse (float tol = 0.0) const;

  OCTAVE_API FloatComplexMatrix fourier (void) const;
  OCTAVE_API FloatComplexMatrix ifourier (void) const;

  OCTAVE_API FloatComplexMatrix fourier2d (void) const;
  OCTAVE_API FloatComplexMatrix ifourier2d (void) const;

  OCTAVE_API FloatComplexDET determinant (void) const;
  OCTAVE_API FloatComplexDET determinant (octave_idx_type& info) const;
  OCTAVE_API FloatComplexDET
  determinant (octave_idx_type& info, float& rcon,
               bool calc_cond = true) const;
  OCTAVE_API FloatComplexDET
  determinant (MatrixType& mattype, octave_idx_type& info,
               float& rcon, bool calc_cond = true) const;

  OCTAVE_API float rcond (void) const;
  OCTAVE_API float rcond (MatrixType& mattype) const;

private:
  // Upper triangular matrix solvers
  FloatComplexMatrix utsolve (MatrixType& mattype, const FloatComplexMatrix& b,
                              octave_idx_type& info, float& rcon,
                              solve_singularity_handler sing_handler,
                              bool calc_cond = false,
                              blas_trans_type transt = blas_no_trans) const;

  // Lower triangular matrix solvers
  FloatComplexMatrix ltsolve (MatrixType& mattype, const FloatComplexMatrix& b,
                              octave_idx_type& info, float& rcon,
                              solve_singularity_handler sing_handler,
                              bool calc_cond = false,
                              blas_trans_type transt = blas_no_trans) const;

  // Full matrix solvers (umfpack/cholesky)
  FloatComplexMatrix fsolve (MatrixType& mattype, const FloatComplexMatrix& b,
                             octave_idx_type& info, float& rcon,
                             solve_singularity_handler sing_handler,
                             bool calc_cond = false) const;

public:
  // Generic interface to solver with no probing of type
  OCTAVE_API FloatComplexMatrix
  solve (MatrixType& mattype, const FloatMatrix& b) const;
  OCTAVE_API FloatComplexMatrix
  solve (MatrixType& mattype, const FloatMatrix& b,
         octave_idx_type& info) const;
  OCTAVE_API FloatComplexMatrix
  solve (MatrixType& mattype, const FloatMatrix& b, octave_idx_type& info,
         float& rcon) const;
  OCTAVE_API FloatComplexMatrix
  solve (MatrixType& mattype, const FloatMatrix& b, octave_idx_type& info,
         float& rcon, solve_singularity_handler sing_handler,
         bool singular_fallback = true,
         blas_trans_type transt = blas_no_trans) const;

  OCTAVE_API FloatComplexMatrix
  solve (MatrixType& mattype, const FloatComplexMatrix& b) const;
  OCTAVE_API FloatComplexMatrix
  solve (MatrixType& mattype, const FloatComplexMatrix& b,
         octave_idx_type& info) const;
  OCTAVE_API FloatComplexMatrix
  solve (MatrixType& mattype, const FloatComplexMatrix& b,
         octave_idx_type& info, float& rcon) const;
  OCTAVE_API FloatComplexMatrix
  solve (MatrixType& mattype, const FloatComplexMatrix& b,
         octave_idx_type& info, float& rcon,
         solve_singularity_handler sing_handler,
         bool singular_fallback = true,
         blas_trans_type transt = blas_no_trans) const;

  OCTAVE_API FloatComplexColumnVector
  solve (MatrixType& mattype, const FloatColumnVector& b) const;
  OCTAVE_API FloatComplexColumnVector
  solve (MatrixType& mattype, const FloatColumnVector& b,
         octave_idx_type& info) const;
  OCTAVE_API FloatComplexColumnVector
  solve (MatrixType& mattype, const FloatColumnVector& b,
         octave_idx_type& info, float& rcon) const;
  OCTAVE_API FloatComplexColumnVector
  solve (MatrixType& mattype, const FloatColumnVector& b,
         octave_idx_type& info, float& rcon,
         solve_singularity_handler sing_handler,
         blas_trans_type transt = blas_no_trans) const;

  OCTAVE_API FloatComplexColumnVector
  solve (MatrixType& mattype, const FloatComplexColumnVector& b) const;
  OCTAVE_API FloatComplexColumnVector
  solve (MatrixType& mattype, const FloatComplexColumnVector& b,
         octave_idx_type& info) const;
  OCTAVE_API FloatComplexColumnVector
  solve (MatrixType& mattype, const FloatComplexColumnVector& b,
         octave_idx_type& info, float& rcon) const;
  OCTAVE_API FloatComplexColumnVector
  solve (MatrixType& mattype, const FloatComplexColumnVector& b,
         octave_idx_type& info, float& rcon,
         solve_singularity_handler sing_handler,
         blas_trans_type transt = blas_no_trans) const;

  // Generic interface to solver with probing of type
  OCTAVE_API FloatComplexMatrix solve (const FloatMatrix& b) const;
  OCTAVE_API FloatComplexMatrix
  solve (const FloatMatrix& b, octave_idx_type& info) const;
  OCTAVE_API FloatComplexMatrix
  solve (const FloatMatrix& b, octave_idx_type& info, float& rcon) const;
  OCTAVE_API FloatComplexMatrix
  solve (const FloatMatrix& b, octave_idx_type& info, float& rcon,
         solve_singularity_handler sing_handler,
         blas_trans_type transt = blas_no_trans) const;

  OCTAVE_API FloatComplexMatrix solve (const FloatComplexMatrix& b) const;
  OCTAVE_API FloatComplexMatrix
  solve (const FloatComplexMatrix& b, octave_idx_type& info) const;
  OCTAVE_API FloatComplexMatrix
  solve (const FloatComplexMatrix& b, octave_idx_type& info,
         float& rcon) const;
  OCTAVE_API FloatComplexMatrix
  solve (const FloatComplexMatrix& b, octave_idx_type& info, float& rcon,
         solve_singularity_handler sing_handler,
         blas_trans_type transt = blas_no_trans) const;

  OCTAVE_API FloatComplexColumnVector solve (const FloatColumnVector& b) const;
  OCTAVE_API FloatComplexColumnVector
  solve (const FloatColumnVector& b, octave_idx_type& info) const;
  OCTAVE_API FloatComplexColumnVector
  solve (const FloatColumnVector& b, octave_idx_type& info,
         float& rcon) const;
  OCTAVE_API FloatComplexColumnVector
  solve (const FloatColumnVector& b, octave_idx_type& info, float& rcon,
         solve_singularity_handler sing_handler,
         blas_trans_type transt = blas_no_trans) const;

  OCTAVE_API FloatComplexColumnVector
  solve (const FloatComplexColumnVector& b) const;
  OCTAVE_API FloatComplexColumnVector
  solve (const FloatComplexColumnVector& b, octave_idx_type& info) const;
  OCTAVE_API FloatComplexColumnVector
  solve (const FloatComplexColumnVector& b, octave_idx_type& info,
         float& rcon) const;
  OCTAVE_API FloatComplexColumnVector
  solve (const FloatComplexColumnVector& b, octave_idx_type& info, float& rcon,
         solve_singularity_handler sing_handler,
         blas_trans_type transt = blas_no_trans) const;

  OCTAVE_API FloatComplexMatrix lssolve (const FloatMatrix& b) const;
  OCTAVE_API FloatComplexMatrix
  lssolve (const FloatMatrix& b, octave_idx_type& info) const;
  OCTAVE_API FloatComplexMatrix
  lssolve (const FloatMatrix& b, octave_idx_type& info, octave_idx_type& rank) const;
  OCTAVE_API FloatComplexMatrix
  lssolve (const FloatMatrix& b, octave_idx_type& info, octave_idx_type& rank,
           float& rcon) const;

  OCTAVE_API FloatComplexMatrix lssolve (const FloatComplexMatrix& b) const;
  OCTAVE_API FloatComplexMatrix
  lssolve (const FloatComplexMatrix& b, octave_idx_type& info) const;
  OCTAVE_API FloatComplexMatrix
  lssolve (const FloatComplexMatrix& b, octave_idx_type& info,
           octave_idx_type& rank) const;
  OCTAVE_API FloatComplexMatrix
  lssolve (const FloatComplexMatrix& b, octave_idx_type& info,
           octave_idx_type& rank, float& rcon) const;

  OCTAVE_API FloatComplexColumnVector
  lssolve (const FloatColumnVector& b) const;
  OCTAVE_API FloatComplexColumnVector
  lssolve (const FloatColumnVector& b, octave_idx_type& info) const;
  OCTAVE_API FloatComplexColumnVector
  lssolve (const FloatColumnVector& b, octave_idx_type& info,
           octave_idx_type& rank) const;
  OCTAVE_API FloatComplexColumnVector
  lssolve (const FloatColumnVector& b, octave_idx_type& info,
           octave_idx_type& rank, float& rcon) const;

  OCTAVE_API FloatComplexColumnVector
  lssolve (const FloatComplexColumnVector& b) const;
  OCTAVE_API FloatComplexColumnVector
  lssolve (const FloatComplexColumnVector& b, octave_idx_type& info) const;
  OCTAVE_API FloatComplexColumnVector
  lssolve (const FloatComplexColumnVector& b, octave_idx_type& info,
           octave_idx_type& rank) const;
  OCTAVE_API FloatComplexColumnVector
  lssolve (const FloatComplexColumnVector& b, octave_idx_type& info,
           octave_idx_type& rank, float& rcon) const;

  // matrix by diagonal matrix -> matrix operations

  OCTAVE_API FloatComplexMatrix& operator += (const FloatDiagMatrix& a);
  OCTAVE_API FloatComplexMatrix& operator -= (const FloatDiagMatrix& a);

  OCTAVE_API FloatComplexMatrix& operator += (const FloatComplexDiagMatrix& a);
  OCTAVE_API FloatComplexMatrix& operator -= (const FloatComplexDiagMatrix& a);

  // matrix by matrix -> matrix operations

  OCTAVE_API FloatComplexMatrix& operator += (const FloatMatrix& a);
  OCTAVE_API FloatComplexMatrix& operator -= (const FloatMatrix& a);

  // unary operations

  OCTAVE_API boolMatrix operator ! (void) const;

  // other operations

  OCTAVE_API boolMatrix all (int dim = -1) const;
  OCTAVE_API boolMatrix any (int dim = -1) const;

  OCTAVE_API FloatComplexMatrix cumprod (int dim = -1) const;
  OCTAVE_API FloatComplexMatrix cumsum (int dim = -1) const;
  OCTAVE_API FloatComplexMatrix prod (int dim = -1) const;
  OCTAVE_API FloatComplexMatrix sum (int dim = -1) const;
  OCTAVE_API FloatComplexMatrix sumsq (int dim = -1) const;
  OCTAVE_API FloatMatrix abs (void) const;

  OCTAVE_API FloatComplexMatrix diag (octave_idx_type k = 0) const;

  OCTAVE_API FloatComplexDiagMatrix
  diag (octave_idx_type m, octave_idx_type n) const;

  OCTAVE_API bool row_is_real_only (octave_idx_type) const;
  OCTAVE_API bool column_is_real_only (octave_idx_type) const;

  OCTAVE_API FloatComplexColumnVector row_min (void) const;
  OCTAVE_API FloatComplexColumnVector row_max (void) const;

  OCTAVE_API FloatComplexColumnVector
  row_min (Array<octave_idx_type>& index) const;
  OCTAVE_API FloatComplexColumnVector
  row_max (Array<octave_idx_type>& index) const;

  OCTAVE_API FloatComplexRowVector column_min (void) const;
  OCTAVE_API FloatComplexRowVector column_max (void) const;

  OCTAVE_API FloatComplexRowVector
  column_min (Array<octave_idx_type>& index) const;
  OCTAVE_API FloatComplexRowVector
  column_max (Array<octave_idx_type>& index) const;

  // i/o

  friend OCTAVE_API std::ostream&
  operator << (std::ostream& os, const FloatComplexMatrix& a);
  friend OCTAVE_API std::istream&
  operator >> (std::istream& is, FloatComplexMatrix& a);
};

extern OCTAVE_API FloatComplexMatrix conj (const FloatComplexMatrix& a);

// column vector by row vector -> matrix operations

extern OCTAVE_API FloatComplexMatrix
operator * (const FloatColumnVector& a, const FloatComplexRowVector& b);

extern OCTAVE_API FloatComplexMatrix
operator * (const FloatComplexColumnVector& a, const FloatRowVector& b);

extern OCTAVE_API FloatComplexMatrix
operator * (const FloatComplexColumnVector& a, const FloatComplexRowVector& b);

extern OCTAVE_API FloatComplexMatrix
Givens (const FloatComplex&, const FloatComplex&);

extern OCTAVE_API FloatComplexMatrix
Sylvester (const FloatComplexMatrix&, const FloatComplexMatrix&,
           const FloatComplexMatrix&);

extern OCTAVE_API FloatComplexMatrix
xgemm (const FloatComplexMatrix& a, const FloatComplexMatrix& b,
       blas_trans_type transa = blas_no_trans,
       blas_trans_type transb = blas_no_trans);

extern OCTAVE_API FloatComplexMatrix operator * (const FloatMatrix&,
                                                 const FloatComplexMatrix&);
extern OCTAVE_API FloatComplexMatrix operator * (const FloatComplexMatrix&,
                                                 const FloatMatrix&);
extern OCTAVE_API FloatComplexMatrix operator * (const FloatComplexMatrix&,
                                                 const FloatComplexMatrix&);

extern OCTAVE_API FloatComplexMatrix min (const FloatComplex& c,
                                          const FloatComplexMatrix& m);
extern OCTAVE_API FloatComplexMatrix min (const FloatComplexMatrix& m,
                                          const FloatComplex& c);
extern OCTAVE_API FloatComplexMatrix min (const FloatComplexMatrix& a,
                                          const FloatComplexMatrix& b);

extern OCTAVE_API FloatComplexMatrix max (const FloatComplex& c,
                                          const FloatComplexMatrix& m);
extern OCTAVE_API FloatComplexMatrix max (const FloatComplexMatrix& m,
                                          const FloatComplex& c);
extern OCTAVE_API FloatComplexMatrix max (const FloatComplexMatrix& a,
                                          const FloatComplexMatrix& b);

extern OCTAVE_API FloatComplexMatrix
linspace (const FloatComplexColumnVector& x1,
          const FloatComplexColumnVector& x2,
          octave_idx_type n);

MS_CMP_OP_DECLS (FloatComplexMatrix, FloatComplex, OCTAVE_API)
MS_BOOL_OP_DECLS (FloatComplexMatrix, FloatComplex, OCTAVE_API)

SM_CMP_OP_DECLS (FloatComplex, FloatComplexMatrix, OCTAVE_API)
SM_BOOL_OP_DECLS (FloatComplex, FloatComplexMatrix, OCTAVE_API)

MM_CMP_OP_DECLS (FloatComplexMatrix, FloatComplexMatrix, OCTAVE_API)
MM_BOOL_OP_DECLS (FloatComplexMatrix, FloatComplexMatrix, OCTAVE_API)

MARRAY_FORWARD_DEFS (MArray, FloatComplexMatrix, FloatComplex)

#endif
