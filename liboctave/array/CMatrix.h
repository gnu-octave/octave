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

#if ! defined (octave_CMatrix_h)
#define octave_CMatrix_h 1

#include "octave-config.h"

#include "CNDArray.h"
#include "DET.h"
#include "MArray.h"
#include "MDiagArray2.h"
#include "MatrixType.h"
#include "mx-defs.h"
#include "mx-op-decl.h"
#include "oct-cmplx.h"

class
OCTAVE_API
ComplexMatrix : public ComplexNDArray
{
public:

  typedef ComplexColumnVector column_vector_type;
  typedef ComplexRowVector row_vector_type;

  typedef ColumnVector real_column_vector_type;
  typedef RowVector real_row_vector_type;

  typedef Matrix real_matrix_type;
  typedef ComplexMatrix complex_matrix_type;

  typedef DiagMatrix real_diag_matrix_type;
  typedef ComplexDiagMatrix complex_diag_matrix_type;

  typedef double real_elt_type;
  typedef Complex complex_elt_type;

  typedef void (*solve_singularity_handler) (double rcon);

  ComplexMatrix (void) = default;

  ComplexMatrix (const ComplexMatrix& a) = default;

  ComplexMatrix& operator = (const ComplexMatrix& a) = default;

  ~ComplexMatrix (void) = default;

  ComplexMatrix (octave_idx_type r, octave_idx_type c)
    : ComplexNDArray (dim_vector (r, c)) { }

  ComplexMatrix (octave_idx_type r, octave_idx_type c, const Complex& val)
    : ComplexNDArray (dim_vector (r, c), val) { }

  ComplexMatrix (const dim_vector& dv) : ComplexNDArray (dv.redim (2)) { }

  ComplexMatrix (const dim_vector& dv, const Complex& val)
    : ComplexNDArray (dv.redim (2), val) { }

  template <typename U>
  ComplexMatrix (const MArray<U>& a) : ComplexNDArray (a.as_matrix ()) { }

  template <typename U>
  ComplexMatrix (const Array<U>& a) : ComplexNDArray (a.as_matrix ()) { }

  OCTAVE_API ComplexMatrix (const Matrix& re, const Matrix& im);

  explicit OCTAVE_API ComplexMatrix (const Matrix& a);

  explicit OCTAVE_API ComplexMatrix (const RowVector& rv);

  explicit OCTAVE_API ComplexMatrix (const ColumnVector& cv);

  explicit OCTAVE_API ComplexMatrix (const DiagMatrix& a);

  explicit OCTAVE_API ComplexMatrix (const MDiagArray2<double>& a);

  explicit OCTAVE_API ComplexMatrix (const DiagArray2<double>& a);

  explicit OCTAVE_API ComplexMatrix (const ComplexRowVector& rv);

  explicit OCTAVE_API ComplexMatrix (const ComplexColumnVector& cv);

  explicit OCTAVE_API ComplexMatrix (const ComplexDiagMatrix& a);

  explicit OCTAVE_API ComplexMatrix (const MDiagArray2<Complex>& a);

  explicit OCTAVE_API ComplexMatrix (const DiagArray2<Complex>& a);

  explicit OCTAVE_API ComplexMatrix (const boolMatrix& a);

  explicit OCTAVE_API ComplexMatrix (const charMatrix& a);

  OCTAVE_API bool operator == (const ComplexMatrix& a) const;
  OCTAVE_API bool operator != (const ComplexMatrix& a) const;

  OCTAVE_API bool ishermitian (void) const;

  // destructive insert/delete/reorder operations

  OCTAVE_API ComplexMatrix&
  insert (const Matrix& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API ComplexMatrix&
  insert (const RowVector& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API ComplexMatrix&
  insert (const ColumnVector& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API ComplexMatrix&
  insert (const DiagMatrix& a, octave_idx_type r, octave_idx_type c);

  OCTAVE_API ComplexMatrix&
  insert (const ComplexMatrix& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API ComplexMatrix&
  insert (const ComplexRowVector& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API ComplexMatrix&
  insert (const ComplexColumnVector& a, octave_idx_type r, octave_idx_type c);
  OCTAVE_API ComplexMatrix&
  insert (const ComplexDiagMatrix& a, octave_idx_type r, octave_idx_type c);

  OCTAVE_API ComplexMatrix& fill (double val);
  OCTAVE_API ComplexMatrix& fill (const Complex& val);
  OCTAVE_API ComplexMatrix&
  fill (double val, octave_idx_type r1, octave_idx_type c1,
        octave_idx_type r2, octave_idx_type c2);
  OCTAVE_API ComplexMatrix&
  fill (const Complex& val, octave_idx_type r1, octave_idx_type c1,
        octave_idx_type r2, octave_idx_type c2);

  OCTAVE_API ComplexMatrix append (const Matrix& a) const;
  OCTAVE_API ComplexMatrix append (const RowVector& a) const;
  OCTAVE_API ComplexMatrix append (const ColumnVector& a) const;
  OCTAVE_API ComplexMatrix append (const DiagMatrix& a) const;

  OCTAVE_API ComplexMatrix append (const ComplexMatrix& a) const;
  OCTAVE_API ComplexMatrix append (const ComplexRowVector& a) const;
  OCTAVE_API ComplexMatrix append (const ComplexColumnVector& a) const;
  OCTAVE_API ComplexMatrix append (const ComplexDiagMatrix& a) const;

  OCTAVE_API ComplexMatrix stack (const Matrix& a) const;
  OCTAVE_API ComplexMatrix stack (const RowVector& a) const;
  OCTAVE_API ComplexMatrix stack (const ColumnVector& a) const;
  OCTAVE_API ComplexMatrix stack (const DiagMatrix& a) const;

  OCTAVE_API ComplexMatrix stack (const ComplexMatrix& a) const;
  OCTAVE_API ComplexMatrix stack (const ComplexRowVector& a) const;
  OCTAVE_API ComplexMatrix stack (const ComplexColumnVector& a) const;
  OCTAVE_API ComplexMatrix stack (const ComplexDiagMatrix& a) const;

  ComplexMatrix hermitian (void) const
  { return MArray<Complex>::hermitian (std::conj); }
  ComplexMatrix transpose (void) const
  { return MArray<Complex>::transpose (); }

  friend OCTAVE_API ComplexMatrix conj (const ComplexMatrix& a);

  // resize is the destructive equivalent for this one

  OCTAVE_API ComplexMatrix
  extract (octave_idx_type r1, octave_idx_type c1,
           octave_idx_type r2, octave_idx_type c2) const;

  OCTAVE_API ComplexMatrix
  extract_n (octave_idx_type r1, octave_idx_type c1,
             octave_idx_type nr, octave_idx_type nc) const;

  // extract row or column i.

  OCTAVE_API ComplexRowVector row (octave_idx_type i) const;

  OCTAVE_API ComplexColumnVector column (octave_idx_type i) const;

  void resize (octave_idx_type nr, octave_idx_type nc,
               const Complex& rfv = Complex (0))
  {
    MArray<Complex>::resize (dim_vector (nr, nc), rfv);
  }

private:
  ComplexMatrix tinverse (MatrixType& mattype, octave_idx_type& info,
                          double& rcon, bool force, bool calc_cond) const;

  ComplexMatrix finverse (MatrixType& mattype, octave_idx_type& info,
                          double& rcon, bool force, bool calc_cond) const;

public:
  OCTAVE_API ComplexMatrix inverse (void) const;
  OCTAVE_API ComplexMatrix inverse (octave_idx_type& info) const;
  OCTAVE_API ComplexMatrix
  inverse (octave_idx_type& info, double& rcon,
           bool force = false, bool calc_cond = true) const;

  OCTAVE_API ComplexMatrix inverse (MatrixType& mattype) const;
  OCTAVE_API ComplexMatrix
  inverse (MatrixType& mattype, octave_idx_type& info) const;
  OCTAVE_API ComplexMatrix
  inverse (MatrixType& mattype, octave_idx_type& info, double& rcon,
           bool force = false, bool calc_cond = true) const;

  OCTAVE_API ComplexMatrix pseudo_inverse (double tol = 0.0) const;

  OCTAVE_API ComplexMatrix fourier (void) const;
  OCTAVE_API ComplexMatrix ifourier (void) const;

  OCTAVE_API ComplexMatrix fourier2d (void) const;
  OCTAVE_API ComplexMatrix ifourier2d (void) const;

  OCTAVE_API ComplexDET determinant (void) const;
  OCTAVE_API ComplexDET determinant (octave_idx_type& info) const;
  OCTAVE_API ComplexDET
  determinant (octave_idx_type& info, double& rcon,
               bool calc_cond = true) const;
  OCTAVE_API ComplexDET
  determinant (MatrixType& mattype, octave_idx_type& info, double& rcon,
               bool calc_cond = true) const;

  OCTAVE_API double rcond (void) const;
  OCTAVE_API double rcond (MatrixType& mattype) const;

private:
  // Upper triangular matrix solvers
  ComplexMatrix utsolve (MatrixType& mattype, const ComplexMatrix& b,
                         octave_idx_type& info, double& rcon,
                         solve_singularity_handler sing_handler,
                         bool calc_cond = false,
                         blas_trans_type transt = blas_no_trans) const;

  // Lower triangular matrix solvers
  ComplexMatrix ltsolve (MatrixType& mattype, const ComplexMatrix& b,
                         octave_idx_type& info, double& rcon,
                         solve_singularity_handler sing_handler,
                         bool calc_cond = false,
                         blas_trans_type transt = blas_no_trans) const;

  // Full matrix solvers (umfpack/cholesky)
  ComplexMatrix fsolve (MatrixType& mattype, const ComplexMatrix& b,
                        octave_idx_type& info, double& rcon,
                        solve_singularity_handler sing_handler,
                        bool calc_cond = false) const;

public:
  // Generic interface to solver with no probing of type
  OCTAVE_API ComplexMatrix solve (MatrixType& mattype, const Matrix& b) const;
  OCTAVE_API ComplexMatrix
  solve (MatrixType& mattype, const Matrix& b, octave_idx_type& info) const;
  OCTAVE_API ComplexMatrix
  solve (MatrixType& mattype, const Matrix& b, octave_idx_type& info,
         double& rcon) const;
  OCTAVE_API ComplexMatrix
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
  solve (MatrixType& mattype, const ComplexMatrix& b, octave_idx_type& info,
         double& rcon) const;
  OCTAVE_API ComplexMatrix
  solve (MatrixType& mattype, const ComplexMatrix& b, octave_idx_type& info,
         double& rcon, solve_singularity_handler sing_handler,
         bool singular_fallback = true,
         blas_trans_type transt = blas_no_trans) const;

  OCTAVE_API ComplexColumnVector
  solve (MatrixType& mattype, const ColumnVector& b) const;
  OCTAVE_API ComplexColumnVector
  solve (MatrixType& mattype, const ColumnVector& b,
         octave_idx_type& info) const;
  OCTAVE_API ComplexColumnVector
  solve (MatrixType& mattype, const ColumnVector& b,
         octave_idx_type& info, double& rcon) const;
  OCTAVE_API ComplexColumnVector
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
  OCTAVE_API ComplexMatrix solve (const Matrix& b) const;
  OCTAVE_API ComplexMatrix
  solve (const Matrix& b, octave_idx_type& info) const;
  OCTAVE_API ComplexMatrix
  solve (const Matrix& b, octave_idx_type& info, double& rcon) const;
  OCTAVE_API ComplexMatrix
  solve (const Matrix& b, octave_idx_type& info, double& rcon,
         solve_singularity_handler sing_handler,
         blas_trans_type transt = blas_no_trans) const;

  OCTAVE_API ComplexMatrix solve (const ComplexMatrix& b) const;
  OCTAVE_API ComplexMatrix
  solve (const ComplexMatrix& b, octave_idx_type& info) const;
  OCTAVE_API ComplexMatrix
  solve (const ComplexMatrix& b, octave_idx_type& info, double& rcon) const;
  OCTAVE_API ComplexMatrix
  solve (const ComplexMatrix& b, octave_idx_type& info, double& rcon,
         solve_singularity_handler sing_handler,
         blas_trans_type transt = blas_no_trans) const;

  OCTAVE_API ComplexColumnVector solve (const ColumnVector& b) const;
  OCTAVE_API ComplexColumnVector
  solve (const ColumnVector& b, octave_idx_type& info) const;
  OCTAVE_API ComplexColumnVector
  solve (const ColumnVector& b, octave_idx_type& info, double& rcon) const;
  OCTAVE_API ComplexColumnVector
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

  OCTAVE_API ComplexMatrix lssolve (const Matrix& b) const;
  OCTAVE_API ComplexMatrix
  lssolve (const Matrix& b, octave_idx_type& info) const;
  OCTAVE_API ComplexMatrix
  lssolve (const Matrix& b, octave_idx_type& info,
           octave_idx_type& rank) const;
  OCTAVE_API ComplexMatrix
  lssolve (const Matrix& b, octave_idx_type& info,
           octave_idx_type& rank, double& rcon) const;

  OCTAVE_API ComplexMatrix lssolve (const ComplexMatrix& b) const;
  OCTAVE_API ComplexMatrix
  lssolve (const ComplexMatrix& b, octave_idx_type& info) const;
  OCTAVE_API ComplexMatrix
  lssolve (const ComplexMatrix& b, octave_idx_type& info,
           octave_idx_type& rank) const;
  OCTAVE_API ComplexMatrix
  lssolve (const ComplexMatrix& b, octave_idx_type& info,
           octave_idx_type& rank, double& rcon) const;

  OCTAVE_API ComplexColumnVector lssolve (const ColumnVector& b) const;
  OCTAVE_API ComplexColumnVector
  lssolve (const ColumnVector& b, octave_idx_type& info) const;
  OCTAVE_API ComplexColumnVector
  lssolve (const ColumnVector& b, octave_idx_type& info,
           octave_idx_type& rank) const;
  OCTAVE_API ComplexColumnVector
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

  // matrix by diagonal matrix -> matrix operations

  OCTAVE_API ComplexMatrix& operator += (const DiagMatrix& a);
  OCTAVE_API ComplexMatrix& operator -= (const DiagMatrix& a);

  OCTAVE_API ComplexMatrix& operator += (const ComplexDiagMatrix& a);
  OCTAVE_API ComplexMatrix& operator -= (const ComplexDiagMatrix& a);

  // matrix by matrix -> matrix operations

  OCTAVE_API ComplexMatrix& operator += (const Matrix& a);
  OCTAVE_API ComplexMatrix& operator -= (const Matrix& a);

  // other operations

  OCTAVE_API boolMatrix all (int dim = -1) const;
  OCTAVE_API boolMatrix any (int dim = -1) const;

  OCTAVE_API ComplexMatrix cumprod (int dim = -1) const;
  OCTAVE_API ComplexMatrix cumsum (int dim = -1) const;
  OCTAVE_API ComplexMatrix prod (int dim = -1) const;
  OCTAVE_API ComplexMatrix sum (int dim = -1) const;
  OCTAVE_API ComplexMatrix sumsq (int dim = -1) const;
  OCTAVE_API Matrix abs (void) const;

  OCTAVE_API ComplexMatrix diag (octave_idx_type k = 0) const;

  OCTAVE_API ComplexDiagMatrix
  diag (octave_idx_type m, octave_idx_type n) const;

  OCTAVE_API bool row_is_real_only (octave_idx_type) const;
  OCTAVE_API bool column_is_real_only (octave_idx_type) const;

  OCTAVE_API ComplexColumnVector row_min (void) const;
  OCTAVE_API ComplexColumnVector row_max (void) const;

  OCTAVE_API ComplexColumnVector row_min (Array<octave_idx_type>& index) const;
  OCTAVE_API ComplexColumnVector row_max (Array<octave_idx_type>& index) const;

  OCTAVE_API ComplexRowVector column_min (void) const;
  OCTAVE_API ComplexRowVector column_max (void) const;

  OCTAVE_API ComplexRowVector column_min (Array<octave_idx_type>& index) const;
  OCTAVE_API ComplexRowVector column_max (Array<octave_idx_type>& index) const;

  // i/o

  friend OCTAVE_API std::ostream&
  operator << (std::ostream& os, const ComplexMatrix& a);
  friend OCTAVE_API std::istream&
  operator >> (std::istream& is, ComplexMatrix& a);
};

extern OCTAVE_API ComplexMatrix conj (const ComplexMatrix& a);

// column vector by row vector -> matrix operations

extern OCTAVE_API ComplexMatrix
operator * (const ColumnVector& a, const ComplexRowVector& b);

extern OCTAVE_API ComplexMatrix
operator * (const ComplexColumnVector& a, const RowVector& b);

extern OCTAVE_API ComplexMatrix
operator * (const ComplexColumnVector& a, const ComplexRowVector& b);

extern OCTAVE_API ComplexMatrix
Givens (const Complex&, const Complex&);

extern OCTAVE_API ComplexMatrix
Sylvester (const ComplexMatrix&, const ComplexMatrix&, const ComplexMatrix&);

extern OCTAVE_API ComplexMatrix
xgemm (const ComplexMatrix& a, const ComplexMatrix& b,
       blas_trans_type transa = blas_no_trans,
       blas_trans_type transb = blas_no_trans);

extern OCTAVE_API ComplexMatrix operator * (const Matrix&,
                                            const ComplexMatrix&);
extern OCTAVE_API ComplexMatrix operator * (const ComplexMatrix&,
                                            const Matrix&);
extern OCTAVE_API ComplexMatrix operator * (const ComplexMatrix&,
                                            const ComplexMatrix&);

extern OCTAVE_API ComplexMatrix min (const Complex& c, const ComplexMatrix& m);
extern OCTAVE_API ComplexMatrix min (const ComplexMatrix& m, const Complex& c);
extern OCTAVE_API ComplexMatrix min (const ComplexMatrix& a,
                                     const ComplexMatrix& b);

extern OCTAVE_API ComplexMatrix max (const Complex& c, const ComplexMatrix& m);
extern OCTAVE_API ComplexMatrix max (const ComplexMatrix& m, const Complex& c);
extern OCTAVE_API ComplexMatrix max (const ComplexMatrix& a,
                                     const ComplexMatrix& b);

extern OCTAVE_API ComplexMatrix linspace (const ComplexColumnVector& x1,
                                          const ComplexColumnVector& x2,
                                          octave_idx_type n);

MS_CMP_OP_DECLS (ComplexMatrix, Complex, OCTAVE_API)
MS_BOOL_OP_DECLS (ComplexMatrix, Complex, OCTAVE_API)

SM_CMP_OP_DECLS (Complex, ComplexMatrix, OCTAVE_API)
SM_BOOL_OP_DECLS (Complex, ComplexMatrix, OCTAVE_API)

MM_CMP_OP_DECLS (ComplexMatrix, ComplexMatrix, OCTAVE_API)
MM_BOOL_OP_DECLS (ComplexMatrix, ComplexMatrix, OCTAVE_API)

MARRAY_FORWARD_DEFS (MArray, ComplexMatrix, Complex)

#endif
