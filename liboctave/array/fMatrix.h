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

#if ! defined (octave_fMatrix_h)
#define octave_fMatrix_h 1

#include "octave-config.h"

#include "DET.h"
#include "MArray.h"
#include "MDiagArray2.h"
#include "MatrixType.h"
#include "fNDArray.h"
#include "mx-defs.h"
#include "mx-op-decl.h"

class
OCTAVE_API
FloatMatrix : public FloatNDArray
{
public:

  typedef FloatColumnVector column_vector_type;
  typedef FloatRowVector row_vector_type;

  typedef FloatColumnVector real_column_vector_type;
  typedef FloatRowVector real_row_vector_type;

  typedef FloatMatrix real_matrix_type;
  typedef FloatComplexMatrix complex_matrix_type;

  typedef FloatDiagMatrix real_diag_matrix_type;
  typedef FloatComplexDiagMatrix complex_diag_matrix_type;

  typedef float real_elt_type;
  typedef FloatComplex complex_elt_type;

  typedef void (*solve_singularity_handler) (float rcon);

  FloatMatrix (void) = default;

  FloatMatrix (const FloatMatrix& a) = default;

  FloatMatrix& operator = (const FloatMatrix& a) = default;

  ~FloatMatrix (void) = default;

  FloatMatrix (octave_idx_type r, octave_idx_type c)
    : FloatNDArray (dim_vector (r, c)) { }

  FloatMatrix (octave_idx_type r, octave_idx_type c, float val)
    : FloatNDArray (dim_vector (r, c), val) { }

  FloatMatrix (const dim_vector& dv) : FloatNDArray (dv.redim (2)) { }

  FloatMatrix (const dim_vector& dv, float val)
    : FloatNDArray (dv.redim (2), val) { }

  template <typename U>
  FloatMatrix (const MArray<U>& a) : FloatNDArray (a.as_matrix ()) { }

  template <typename U>
  FloatMatrix (const Array<U>& a) : FloatNDArray (a.as_matrix ()) { }

  explicit OCTAVE_API FloatMatrix (const FloatRowVector& rv);

  explicit OCTAVE_API FloatMatrix (const FloatColumnVector& cv);

  explicit OCTAVE_API FloatMatrix (const FloatDiagMatrix& a);

  explicit OCTAVE_API FloatMatrix (const MDiagArray2<float>& a);

  explicit OCTAVE_API FloatMatrix (const DiagArray2<float>& a);

  explicit OCTAVE_API FloatMatrix (const PermMatrix& a);

  explicit OCTAVE_API FloatMatrix (const boolMatrix& a);

  explicit OCTAVE_API FloatMatrix (const charMatrix& a);

  OCTAVE_API bool operator == (const FloatMatrix& a) const;
  OCTAVE_API bool operator != (const FloatMatrix& a) const;

  OCTAVE_API bool issymmetric (void) const;

  // destructive insert/delete/reorder operations

  OCTAVE_API FloatMatrix&
  insert (const FloatMatrix& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API FloatMatrix&
  insert (const FloatRowVector& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API FloatMatrix&
  insert (const FloatColumnVector& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API FloatMatrix&
  insert (const FloatDiagMatrix& a, octave_idx_type r, octave_idx_type c);

  OCTAVE_API FloatMatrix& fill (float val);
  OCTAVE_API FloatMatrix&
  fill (float val, octave_idx_type r1, octave_idx_type c1,
        octave_idx_type r2, octave_idx_type c2);

  OCTAVE_API FloatMatrix append (const FloatMatrix& a) const;
  OCTAVE_API FloatMatrix append (const FloatRowVector& a) const;
  OCTAVE_API FloatMatrix append (const FloatColumnVector& a) const;
  OCTAVE_API FloatMatrix append (const FloatDiagMatrix& a) const;

  OCTAVE_API FloatMatrix stack (const FloatMatrix& a) const;
  OCTAVE_API FloatMatrix stack (const FloatRowVector& a) const;
  OCTAVE_API FloatMatrix stack (const FloatColumnVector& a) const;
  OCTAVE_API FloatMatrix stack (const FloatDiagMatrix& a) const;

  friend OCTAVE_API FloatMatrix real (const FloatComplexMatrix& a);
  friend OCTAVE_API FloatMatrix imag (const FloatComplexMatrix& a);

  friend class FloatComplexMatrix;

  FloatMatrix hermitian (void) const { return MArray<float>::transpose (); }
  FloatMatrix transpose (void) const { return MArray<float>::transpose (); }

  // resize is the destructive equivalent for this one

  OCTAVE_API FloatMatrix
  extract (octave_idx_type r1, octave_idx_type c1,
           octave_idx_type r2, octave_idx_type c2) const;

  OCTAVE_API FloatMatrix
  extract_n (octave_idx_type r1, octave_idx_type c1,
             octave_idx_type nr, octave_idx_type nc) const;

  // extract row or column i.

  OCTAVE_API FloatRowVector row (octave_idx_type i) const;

  OCTAVE_API FloatColumnVector column (octave_idx_type i) const;

  void resize (octave_idx_type nr, octave_idx_type nc, float rfv = 0)
  {
    MArray<float>::resize (dim_vector (nr, nc), rfv);
  }

private:
  FloatMatrix tinverse (MatrixType& mattype, octave_idx_type& info,
                        float& rcon, bool force, bool calc_cond) const;

  FloatMatrix finverse (MatrixType& mattype, octave_idx_type& info,
                        float& rcon, bool force, bool calc_cond) const;

public:
  OCTAVE_API FloatMatrix inverse (void) const;
  OCTAVE_API FloatMatrix inverse (octave_idx_type& info) const;
  OCTAVE_API FloatMatrix
  inverse (octave_idx_type& info, float& rcon, bool force = false,
           bool calc_cond = true) const;

  OCTAVE_API FloatMatrix inverse (MatrixType& mattype) const;
  OCTAVE_API FloatMatrix
  inverse (MatrixType& mattype, octave_idx_type& info) const;
  OCTAVE_API FloatMatrix
  inverse (MatrixType& mattype, octave_idx_type& info, float& rcon,
           bool force = false, bool calc_cond = true) const;

  OCTAVE_API FloatMatrix pseudo_inverse (float tol = 0.0) const;

  OCTAVE_API FloatComplexMatrix fourier (void) const;
  OCTAVE_API FloatComplexMatrix ifourier (void) const;

  OCTAVE_API FloatComplexMatrix fourier2d (void) const;
  OCTAVE_API FloatComplexMatrix ifourier2d (void) const;

  OCTAVE_API FloatDET determinant (void) const;
  OCTAVE_API FloatDET determinant (octave_idx_type& info) const;
  OCTAVE_API FloatDET
  determinant (octave_idx_type& info, float& rcon,
               bool calc_cond = true) const;
  OCTAVE_API FloatDET
  determinant (MatrixType& mattype, octave_idx_type& info, float& rcon,
               bool calc_cond = true) const;

  OCTAVE_API float rcond (void) const;
  OCTAVE_API float rcond (MatrixType& mattype) const;

private:
  // Upper triangular matrix solvers
  FloatMatrix utsolve (MatrixType& mattype, const FloatMatrix& b,
                       octave_idx_type& info,
                       float& rcon, solve_singularity_handler sing_handler,
                       bool calc_cond = false,
                       blas_trans_type transt = blas_no_trans) const;

  // Lower triangular matrix solvers
  FloatMatrix ltsolve (MatrixType& mattype, const FloatMatrix& b,
                       octave_idx_type& info,
                       float& rcon, solve_singularity_handler sing_handler,
                       bool calc_cond = false,
                       blas_trans_type transt = blas_no_trans) const;

  // Full matrix solvers (lu/cholesky)
  FloatMatrix fsolve (MatrixType& mattype, const FloatMatrix& b,
                      octave_idx_type& info,
                      float& rcon, solve_singularity_handler sing_handler,
                      bool calc_cond = false) const;

public:
  // Generic interface to solver with no probing of type
  OCTAVE_API FloatMatrix
  solve (MatrixType& mattype, const FloatMatrix& b) const;
  OCTAVE_API FloatMatrix
  solve (MatrixType& mattype, const FloatMatrix& b,
         octave_idx_type& info) const;
  OCTAVE_API FloatMatrix
  solve (MatrixType& mattype, const FloatMatrix& b, octave_idx_type& info,
         float& rcon) const;
  OCTAVE_API FloatMatrix
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
         solve_singularity_handler sing_handler, bool singular_fallback = true,
         blas_trans_type transt = blas_no_trans) const;

  OCTAVE_API FloatColumnVector
  solve (MatrixType& mattype, const FloatColumnVector& b) const;
  OCTAVE_API FloatColumnVector
  solve (MatrixType& mattype, const FloatColumnVector& b,
         octave_idx_type& info) const;
  OCTAVE_API FloatColumnVector
  solve (MatrixType& mattype, const FloatColumnVector& b,
         octave_idx_type& info, float& rcon) const;
  OCTAVE_API FloatColumnVector
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
  OCTAVE_API FloatMatrix solve (const FloatMatrix& b) const;
  OCTAVE_API FloatMatrix
  solve (const FloatMatrix& b, octave_idx_type& info) const;
  OCTAVE_API FloatMatrix
  solve (const FloatMatrix& b, octave_idx_type& info, float& rcon) const;
  OCTAVE_API FloatMatrix
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
  solve (const FloatComplexMatrix& b, octave_idx_type& info,
         float& rcon, solve_singularity_handler sing_handler,
         blas_trans_type transt = blas_no_trans) const;

  OCTAVE_API FloatColumnVector solve (const FloatColumnVector& b) const;
  OCTAVE_API FloatColumnVector
  solve (const FloatColumnVector& b, octave_idx_type& info) const;
  OCTAVE_API FloatColumnVector
  solve (const FloatColumnVector& b, octave_idx_type& info, float& rcon) const;
  OCTAVE_API FloatColumnVector
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

  // Singular solvers
  OCTAVE_API FloatMatrix lssolve (const FloatMatrix& b) const;
  OCTAVE_API FloatMatrix
  lssolve (const FloatMatrix& b, octave_idx_type& info) const;
  OCTAVE_API FloatMatrix
  lssolve (const FloatMatrix& b, octave_idx_type& info,
           octave_idx_type& rank) const;
  OCTAVE_API FloatMatrix
  lssolve (const FloatMatrix& b, octave_idx_type& info,
           octave_idx_type& rank, float& rcon) const;

  OCTAVE_API FloatComplexMatrix lssolve (const FloatComplexMatrix& b) const;
  OCTAVE_API FloatComplexMatrix
  lssolve (const FloatComplexMatrix& b, octave_idx_type& info) const;
  OCTAVE_API FloatComplexMatrix
  lssolve (const FloatComplexMatrix& b, octave_idx_type& info,
           octave_idx_type& rank) const;
  OCTAVE_API FloatComplexMatrix
  lssolve (const FloatComplexMatrix& b, octave_idx_type& info,
           octave_idx_type& rank, float& rcon) const;

  OCTAVE_API FloatColumnVector lssolve (const FloatColumnVector& b) const;
  OCTAVE_API FloatColumnVector
  lssolve (const FloatColumnVector& b, octave_idx_type& info) const;
  OCTAVE_API FloatColumnVector
  lssolve (const FloatColumnVector& b, octave_idx_type& info,
           octave_idx_type& rank) const;
  OCTAVE_API FloatColumnVector
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

  OCTAVE_API FloatMatrix& operator += (const FloatDiagMatrix& a);
  OCTAVE_API FloatMatrix& operator -= (const FloatDiagMatrix& a);

  OCTAVE_API FloatMatrix cumprod (int dim = -1) const;
  OCTAVE_API FloatMatrix cumsum (int dim = -1) const;
  OCTAVE_API FloatMatrix prod (int dim = -1) const;
  OCTAVE_API FloatMatrix sum (int dim = -1) const;
  OCTAVE_API FloatMatrix sumsq (int dim = -1) const;
  OCTAVE_API FloatMatrix abs (void) const;

  OCTAVE_API FloatMatrix diag (octave_idx_type k = 0) const;

  OCTAVE_API FloatDiagMatrix diag (octave_idx_type m, octave_idx_type n) const;

  OCTAVE_API FloatColumnVector row_min (void) const;
  OCTAVE_API FloatColumnVector row_max (void) const;

  OCTAVE_API FloatColumnVector row_min (Array<octave_idx_type>& index) const;
  OCTAVE_API FloatColumnVector row_max (Array<octave_idx_type>& index) const;

  OCTAVE_API FloatRowVector column_min (void) const;
  OCTAVE_API FloatRowVector column_max (void) const;

  OCTAVE_API FloatRowVector column_min (Array<octave_idx_type>& index) const;
  OCTAVE_API FloatRowVector column_max (Array<octave_idx_type>& index) const;

  // i/o

  friend OCTAVE_API std::ostream&
  operator << (std::ostream& os, const FloatMatrix& a);
  friend OCTAVE_API std::istream&
  operator >> (std::istream& is, FloatMatrix& a);
};

// Publish externally used friend functions.

extern OCTAVE_API FloatMatrix real (const FloatComplexMatrix& a);
extern OCTAVE_API FloatMatrix imag (const FloatComplexMatrix& a);

// column vector by row vector -> matrix operations

extern OCTAVE_API FloatMatrix operator * (const FloatColumnVector& a,
                                          const FloatRowVector& b);

// Other functions.

extern OCTAVE_API FloatMatrix Givens (float, float);

extern OCTAVE_API FloatMatrix Sylvester (const FloatMatrix&, const FloatMatrix&,
                                         const FloatMatrix&);

extern OCTAVE_API FloatMatrix xgemm (const FloatMatrix& a, const FloatMatrix& b,
                                     blas_trans_type transa = blas_no_trans,
                                     blas_trans_type transb = blas_no_trans);

extern OCTAVE_API FloatMatrix operator * (const FloatMatrix& a,
                                          const FloatMatrix& b);

extern OCTAVE_API FloatMatrix min (float d, const FloatMatrix& m);
extern OCTAVE_API FloatMatrix min (const FloatMatrix& m, float d);
extern OCTAVE_API FloatMatrix min (const FloatMatrix& a, const FloatMatrix& b);

extern OCTAVE_API FloatMatrix max (float d, const FloatMatrix& m);
extern OCTAVE_API FloatMatrix max (const FloatMatrix& m, float d);
extern OCTAVE_API FloatMatrix max (const FloatMatrix& a, const FloatMatrix& b);

extern OCTAVE_API FloatMatrix linspace (const FloatColumnVector& x1,
                                        const FloatColumnVector& x2,
                                        octave_idx_type n);

MS_CMP_OP_DECLS (FloatMatrix, float, OCTAVE_API)
MS_BOOL_OP_DECLS (FloatMatrix, float, OCTAVE_API)

SM_CMP_OP_DECLS (float, FloatMatrix, OCTAVE_API)
SM_BOOL_OP_DECLS (float, FloatMatrix, OCTAVE_API)

MM_CMP_OP_DECLS (FloatMatrix, FloatMatrix, OCTAVE_API)
MM_BOOL_OP_DECLS (FloatMatrix, FloatMatrix, OCTAVE_API)

MARRAY_FORWARD_DEFS (MArray, FloatMatrix, float)

template <typename T>
void read_int (std::istream& is, bool swap_bytes, T& val);

#endif
