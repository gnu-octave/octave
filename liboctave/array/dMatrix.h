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

#if ! defined (octave_dMatrix_h)
#define octave_dMatrix_h 1

#include "octave-config.h"

#include "DET.h"
#include "MArray.h"
#include "MDiagArray2.h"
#include "MatrixType.h"
#include "dNDArray.h"
#include "mx-defs.h"
#include "mx-op-decl.h"

class
OCTAVE_API
Matrix : public NDArray
{
public:

  typedef ColumnVector column_vector_type;
  typedef RowVector row_vector_type;

  typedef ColumnVector real_column_vector_type;
  typedef RowVector real_row_vector_type;

  typedef Matrix real_matrix_type;
  typedef ComplexMatrix complex_matrix_type;

  typedef DiagMatrix real_diag_matrix_type;
  typedef ComplexDiagMatrix complex_diag_matrix_type;

  typedef double real_elt_type;
  typedef Complex complex_elt_type;

  typedef void (*solve_singularity_handler) (double rcon);

  Matrix (void) = default;

  Matrix (const Matrix& a) = default;

  Matrix& operator = (const Matrix& a) = default;

  ~Matrix (void) = default;

  Matrix (octave_idx_type r, octave_idx_type c)
    : NDArray (dim_vector (r, c)) { }

  Matrix (octave_idx_type r, octave_idx_type c, double val)
    : NDArray (dim_vector (r, c), val) { }

  Matrix (const dim_vector& dv) : NDArray (dv.redim (2)) { }

  Matrix (const dim_vector& dv, double val)
    : NDArray (dv.redim (2), val) { }

  template <typename U>
  Matrix (const MArray<U>& a) : NDArray (a.as_matrix ()) { }

  template <typename U>
  Matrix (const Array<U>& a) : NDArray (a.as_matrix ()) { }

  explicit OCTAVE_API Matrix (const RowVector& rv);

  explicit OCTAVE_API Matrix (const ColumnVector& cv);

  explicit OCTAVE_API Matrix (const DiagMatrix& a);

  explicit OCTAVE_API Matrix (const MDiagArray2<double>& a);

  explicit OCTAVE_API Matrix (const DiagArray2<double>& a);

  explicit OCTAVE_API Matrix (const PermMatrix& a);

  explicit OCTAVE_API Matrix (const boolMatrix& a);

  explicit Matrix (const charMatrix& a);

  OCTAVE_API bool operator == (const Matrix& a) const;
  OCTAVE_API bool operator != (const Matrix& a) const;

  OCTAVE_API bool issymmetric (void) const;

  // destructive insert/delete/reorder operations

  OCTAVE_API Matrix&
  insert (const Matrix& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API Matrix&
  insert (const RowVector& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API Matrix&
  insert (const ColumnVector& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API Matrix&
  insert (const DiagMatrix& a, octave_idx_type r, octave_idx_type c);

  OCTAVE_API Matrix& fill (double val);
  OCTAVE_API Matrix&
  fill (double val, octave_idx_type r1, octave_idx_type c1,
        octave_idx_type r2, octave_idx_type c2);

  OCTAVE_API Matrix append (const Matrix& a) const;
  OCTAVE_API Matrix append (const RowVector& a) const;
  OCTAVE_API Matrix append (const ColumnVector& a) const;
  OCTAVE_API Matrix append (const DiagMatrix& a) const;

  OCTAVE_API Matrix stack (const Matrix& a) const;
  OCTAVE_API Matrix stack (const RowVector& a) const;
  OCTAVE_API Matrix stack (const ColumnVector& a) const;
  OCTAVE_API Matrix stack (const DiagMatrix& a) const;

  friend OCTAVE_API Matrix real (const ComplexMatrix& a);
  friend OCTAVE_API Matrix imag (const ComplexMatrix& a);

  friend class ComplexMatrix;

  Matrix hermitian (void) const { return MArray<double>::transpose (); }
  Matrix transpose (void) const { return MArray<double>::transpose (); }

  // resize is the destructive equivalent for this one

  OCTAVE_API Matrix
  extract (octave_idx_type r1, octave_idx_type c1,
           octave_idx_type r2, octave_idx_type c2) const;

  OCTAVE_API Matrix
  extract_n (octave_idx_type r1, octave_idx_type c1,
             octave_idx_type nr, octave_idx_type nc) const;

  // extract row or column i.

  OCTAVE_API RowVector row (octave_idx_type i) const;

  OCTAVE_API ColumnVector column (octave_idx_type i) const;

  void resize (octave_idx_type nr, octave_idx_type nc, double rfv = 0)
  {
    MArray<double>::resize (dim_vector (nr, nc), rfv);
  }

private:
  Matrix tinverse (MatrixType& mattype, octave_idx_type& info, double& rcon,
                   bool force, bool calc_cond) const;

  Matrix finverse (MatrixType& mattype, octave_idx_type& info, double& rcon,
                   bool force, bool calc_cond) const;

public:
  OCTAVE_API Matrix inverse (void) const;
  OCTAVE_API Matrix inverse (octave_idx_type& info) const;
  OCTAVE_API Matrix
  inverse (octave_idx_type& info, double& rcon, bool force = false,
           bool calc_cond = true) const;

  OCTAVE_API Matrix inverse (MatrixType& mattype) const;
  OCTAVE_API Matrix inverse (MatrixType& mattype, octave_idx_type& info) const;
  OCTAVE_API Matrix
  inverse (MatrixType& mattype, octave_idx_type& info, double& rcon,
           bool force = false, bool calc_cond = true) const;

  OCTAVE_API Matrix pseudo_inverse (double tol = 0.0) const;

  OCTAVE_API ComplexMatrix fourier (void) const;
  OCTAVE_API ComplexMatrix ifourier (void) const;

  OCTAVE_API ComplexMatrix fourier2d (void) const;
  OCTAVE_API ComplexMatrix ifourier2d (void) const;

  OCTAVE_API DET determinant (void) const;
  OCTAVE_API DET determinant (octave_idx_type& info) const;
  OCTAVE_API DET
  determinant (octave_idx_type& info, double& rcon,
               bool calc_cond = true) const;
  OCTAVE_API DET
  determinant (MatrixType& mattype, octave_idx_type& info,
               double& rcon, bool calc_cond = true) const;

  OCTAVE_API double rcond (void) const;
  OCTAVE_API double rcond (MatrixType& mattype) const;

private:
  // Upper triangular matrix solvers
  Matrix utsolve (MatrixType& mattype, const Matrix& b, octave_idx_type& info,
                  double& rcon, solve_singularity_handler sing_handler,
                  bool calc_cond = false,
                  blas_trans_type transt = blas_no_trans) const;

  // Lower triangular matrix solvers
  Matrix ltsolve (MatrixType& mattype, const Matrix& b, octave_idx_type& info,
                  double& rcon, solve_singularity_handler sing_handler,
                  bool calc_cond = false,
                  blas_trans_type transt = blas_no_trans) const;

  // Full matrix solvers (lu/cholesky)
  Matrix fsolve (MatrixType& mattype, const Matrix& b, octave_idx_type& info,
                 double& rcon, solve_singularity_handler sing_handler,
                 bool calc_cond = false) const;

public:
  // Generic interface to solver with no probing of type
  OCTAVE_API Matrix solve (MatrixType& mattype, const Matrix& b) const;
  OCTAVE_API Matrix
  solve (MatrixType& mattype, const Matrix& b, octave_idx_type& info) const;
  OCTAVE_API Matrix
  solve (MatrixType& mattype, const Matrix& b, octave_idx_type& info,
         double& rcon) const;
  OCTAVE_API Matrix
  solve (MatrixType& mattype, const Matrix& b, octave_idx_type& info,
         double& rcon, solve_singularity_handler sing_handler,
         bool singular_fallback = true,
         blas_trans_type transt = blas_no_trans) const;

  OCTAVE_API ComplexMatrix
  solve (MatrixType& mattype, const ComplexMatrix& b) const;
  OCTAVE_API ComplexMatrix
  solve (MatrixType& mattype, const ComplexMatrix& b,
         octave_idx_type& info) const;
  OCTAVE_API ComplexMatrix
  solve (MatrixType& mattype, const ComplexMatrix& b,
         octave_idx_type& info, double& rcon) const;
  OCTAVE_API ComplexMatrix
  solve (MatrixType& mattype, const ComplexMatrix& b,
         octave_idx_type& info, double& rcon,
         solve_singularity_handler sing_handler,
         bool singular_fallback = true,
         blas_trans_type transt = blas_no_trans) const;

  OCTAVE_API ColumnVector
  solve (MatrixType& mattype, const ColumnVector& b) const;
  OCTAVE_API ColumnVector
  solve (MatrixType& mattype, const ColumnVector& b,
         octave_idx_type& info) const;
  OCTAVE_API ColumnVector
  solve (MatrixType& mattype, const ColumnVector& b,
         octave_idx_type& info, double& rcon) const;
  OCTAVE_API ColumnVector
  solve (MatrixType& mattype, const ColumnVector& b, octave_idx_type& info,
         double& rcon, solve_singularity_handler sing_handler,
         blas_trans_type transt = blas_no_trans) const;

  OCTAVE_API ComplexColumnVector
  solve (MatrixType& mattype, const ComplexColumnVector& b) const;
  OCTAVE_API ComplexColumnVector
  solve (MatrixType& mattype, const ComplexColumnVector& b,
         octave_idx_type& info) const;
  OCTAVE_API ComplexColumnVector
  solve (MatrixType& mattype, const ComplexColumnVector& b,
         octave_idx_type& info, double& rcon) const;
  OCTAVE_API ComplexColumnVector
  solve (MatrixType& mattype, const ComplexColumnVector& b,
         octave_idx_type& info, double& rcon,
         solve_singularity_handler sing_handler,
         blas_trans_type transt = blas_no_trans) const;

  // Generic interface to solver with probing of type
  OCTAVE_API Matrix solve (const Matrix& b) const;
  OCTAVE_API Matrix solve (const Matrix& b, octave_idx_type& info) const;
  OCTAVE_API Matrix
  solve (const Matrix& b, octave_idx_type& info, double& rcon) const;
  OCTAVE_API Matrix
  solve (const Matrix& b, octave_idx_type& info, double& rcon,
         solve_singularity_handler sing_handler,
         blas_trans_type transt = blas_no_trans) const;

  OCTAVE_API ComplexMatrix solve (const ComplexMatrix& b) const;
  OCTAVE_API ComplexMatrix
  solve (const ComplexMatrix& b, octave_idx_type& info) const;
  OCTAVE_API ComplexMatrix
  solve (const ComplexMatrix& b, octave_idx_type& info,
         double& rcon) const;
  OCTAVE_API ComplexMatrix
  solve (const ComplexMatrix& b, octave_idx_type& info, double& rcon,
         solve_singularity_handler sing_handler,
         blas_trans_type transt = blas_no_trans) const;

  OCTAVE_API ColumnVector solve (const ColumnVector& b) const;
  OCTAVE_API ColumnVector
  solve (const ColumnVector& b, octave_idx_type& info) const;
  OCTAVE_API ColumnVector
  solve (const ColumnVector& b, octave_idx_type& info, double& rcon) const;
  OCTAVE_API ColumnVector
  solve (const ColumnVector& b, octave_idx_type& info, double& rcon,
         solve_singularity_handler sing_handler,
         blas_trans_type transt = blas_no_trans) const;

  OCTAVE_API ComplexColumnVector solve (const ComplexColumnVector& b) const;
  OCTAVE_API ComplexColumnVector
  solve (const ComplexColumnVector& b, octave_idx_type& info) const;
  OCTAVE_API ComplexColumnVector
  solve (const ComplexColumnVector& b, octave_idx_type& info,
         double& rcon) const;
  OCTAVE_API ComplexColumnVector
  solve (const ComplexColumnVector& b, octave_idx_type& info, double& rcon,
         solve_singularity_handler sing_handler,
         blas_trans_type transt = blas_no_trans) const;

  // Singular solvers
  OCTAVE_API Matrix lssolve (const Matrix& b) const;
  OCTAVE_API Matrix lssolve (const Matrix& b, octave_idx_type& info) const;
  OCTAVE_API Matrix
  lssolve (const Matrix& b, octave_idx_type& info,
           octave_idx_type& rank) const;
  OCTAVE_API Matrix
  lssolve (const Matrix& b, octave_idx_type& info, octave_idx_type& rank,
           double& rcon) const;

  OCTAVE_API ComplexMatrix lssolve (const ComplexMatrix& b) const;
  OCTAVE_API ComplexMatrix
  lssolve (const ComplexMatrix& b, octave_idx_type& info) const;
  OCTAVE_API ComplexMatrix
  lssolve (const ComplexMatrix& b, octave_idx_type& info,
           octave_idx_type& rank) const;
  OCTAVE_API ComplexMatrix
  lssolve (const ComplexMatrix& b, octave_idx_type& info,
           octave_idx_type& rank, double& rcon) const;

  OCTAVE_API ColumnVector lssolve (const ColumnVector& b) const;
  OCTAVE_API ColumnVector
  lssolve (const ColumnVector& b, octave_idx_type& info) const;
  OCTAVE_API ColumnVector
  lssolve (const ColumnVector& b, octave_idx_type& info,
           octave_idx_type& rank) const;
  OCTAVE_API ColumnVector
  lssolve (const ColumnVector& b, octave_idx_type& info,
           octave_idx_type& rank, double& rcon) const;

  OCTAVE_API ComplexColumnVector lssolve (const ComplexColumnVector& b) const;
  OCTAVE_API ComplexColumnVector
  lssolve (const ComplexColumnVector& b, octave_idx_type& info) const;
  OCTAVE_API ComplexColumnVector
  lssolve (const ComplexColumnVector& b, octave_idx_type& info,
           octave_idx_type& rank) const;
  OCTAVE_API ComplexColumnVector
  lssolve (const ComplexColumnVector& b, octave_idx_type& info,
           octave_idx_type& rank, double& rcon) const;

  OCTAVE_API Matrix& operator += (const DiagMatrix& a);
  OCTAVE_API Matrix& operator -= (const DiagMatrix& a);

  // unary operations

  // other operations

  OCTAVE_API boolMatrix all (int dim = -1) const;
  OCTAVE_API boolMatrix any (int dim = -1) const;

  OCTAVE_API Matrix cumprod (int dim = -1) const;
  OCTAVE_API Matrix cumsum (int dim = -1) const;
  OCTAVE_API Matrix prod (int dim = -1) const;
  OCTAVE_API Matrix sum (int dim = -1) const;
  OCTAVE_API Matrix sumsq (int dim = -1) const;
  OCTAVE_API Matrix abs (void) const;

  OCTAVE_API Matrix diag (octave_idx_type k = 0) const;

  OCTAVE_API DiagMatrix diag (octave_idx_type m, octave_idx_type n) const;

  OCTAVE_API ColumnVector row_min (void) const;
  OCTAVE_API ColumnVector row_max (void) const;

  OCTAVE_API ColumnVector row_min (Array<octave_idx_type>& index) const;
  OCTAVE_API ColumnVector row_max (Array<octave_idx_type>& index) const;

  OCTAVE_API RowVector column_min (void) const;
  OCTAVE_API RowVector column_max (void) const;

  OCTAVE_API RowVector column_min (Array<octave_idx_type>& index) const;
  OCTAVE_API RowVector column_max (Array<octave_idx_type>& index) const;

  // i/o

  friend OCTAVE_API std::ostream&
  operator << (std::ostream& os, const Matrix& a);
  friend OCTAVE_API std::istream&
  operator >> (std::istream& is, Matrix& a);
};

// Publish externally used friend functions.

extern OCTAVE_API Matrix real (const ComplexMatrix& a);
extern OCTAVE_API Matrix imag (const ComplexMatrix& a);

// column vector by row vector -> matrix operations

extern OCTAVE_API Matrix operator * (const ColumnVector& a,
                                     const RowVector& b);

// Other functions.

extern OCTAVE_API Matrix Givens (double, double);

extern OCTAVE_API Matrix Sylvester (const Matrix&, const Matrix&,
                                    const Matrix&);

extern OCTAVE_API Matrix xgemm (const Matrix& a, const Matrix& b,
                                blas_trans_type transa = blas_no_trans,
                                blas_trans_type transb = blas_no_trans);

extern OCTAVE_API Matrix operator * (const Matrix& a, const Matrix& b);

extern OCTAVE_API Matrix min (double d, const Matrix& m);
extern OCTAVE_API Matrix min (const Matrix& m, double d);
extern OCTAVE_API Matrix min (const Matrix& a, const Matrix& b);

extern OCTAVE_API Matrix max (double d, const Matrix& m);
extern OCTAVE_API Matrix max (const Matrix& m, double d);
extern OCTAVE_API Matrix max (const Matrix& a, const Matrix& b);

extern OCTAVE_API Matrix linspace (const ColumnVector& x1,
                                   const ColumnVector& x2,
                                   octave_idx_type n);

MS_CMP_OP_DECLS (Matrix, double, OCTAVE_API)
MS_BOOL_OP_DECLS (Matrix, double, OCTAVE_API)

SM_CMP_OP_DECLS (double, Matrix, OCTAVE_API)
SM_BOOL_OP_DECLS (double, Matrix, OCTAVE_API)

MM_CMP_OP_DECLS (Matrix, Matrix, OCTAVE_API)
MM_BOOL_OP_DECLS (Matrix, Matrix, OCTAVE_API)

MARRAY_FORWARD_DEFS (MArray, Matrix, double)

template <typename T>
void read_int (std::istream& is, bool swap_bytes, T& val);

#endif
