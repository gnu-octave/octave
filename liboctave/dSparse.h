/*

Copyright (C) 2004 David Bateman
Copyright (C) 1998-2004 Andy Adler

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

#if !defined (octave_dSparse_h)
#define octave_dSparse_h 1

#include "dMatrix.h"
#include "dNDArray.h"
#include "CMatrix.h"
#include "dColVector.h"
#include "CColVector.h"

#include "dbleDET.h"
#include "MSparse.h"
#include "MSparse-defs.h"
#include "Sparse-op-defs.h"
#include "SparseType.h"

class SparseComplexMatrix;
class SparseBoolMatrix;

class
SparseMatrix : public MSparse<double>
{
 public:

  typedef void (*solve_singularity_handler) (double rcond);

  SparseMatrix (void) : MSparse<double> () { }

  SparseMatrix (octave_idx_type r, octave_idx_type c) : MSparse<double> (r, c) { }

  explicit SparseMatrix (octave_idx_type r, octave_idx_type c, double val) 
    : MSparse<double> (r, c, val) { }

  SparseMatrix (const SparseMatrix& a) : MSparse<double> (a) { }

  SparseMatrix (const SparseMatrix& a, const dim_vector& dv) 
    : MSparse<double> (a, dv) { }

  SparseMatrix (const MSparse<double>& a) : MSparse<double> (a) { }

  explicit SparseMatrix (const SparseBoolMatrix& a);

  explicit SparseMatrix (const Matrix& a) : MSparse<double> (a) { }

  explicit SparseMatrix (const NDArray& a) : MSparse<double> (a) { }

  explicit SparseMatrix (const Array<double> a, const Array<octave_idx_type>& r, 
			 const Array<octave_idx_type>& c, octave_idx_type nr = -1, 
			 octave_idx_type nc = -1, bool sum_terms = true)
    : MSparse<double> (a, r, c, nr, nc, sum_terms) { }

  explicit SparseMatrix (const Array<double> a, const Array<double>& r, 
			 const Array<double>& c, octave_idx_type nr = -1, 
			 octave_idx_type nc = -1, bool sum_terms = true)
    : MSparse<double> (a, r, c, nr, nc, sum_terms) { }

  SparseMatrix (octave_idx_type r, octave_idx_type c, octave_idx_type num_nz) : MSparse<double> (r, c, num_nz) { }

  SparseMatrix& operator = (const SparseMatrix& a)
    {
      MSparse<double>::operator = (a);
      return *this;
    }

  bool operator == (const SparseMatrix& a) const;
  bool operator != (const SparseMatrix& a) const;

  bool is_symmetric (void) const;

  SparseMatrix max (int dim = 0) const;
  SparseMatrix max (Array2<octave_idx_type>& index, int dim = 0) const;
  SparseMatrix min (int dim = 0) const;
  SparseMatrix min (Array2<octave_idx_type>& index, int dim = 0) const;
  
  // destructive insert/delete/reorder operations

  SparseMatrix& insert (const SparseMatrix& a, octave_idx_type r, octave_idx_type c);

  SparseMatrix concat (const SparseMatrix& rb, const Array<octave_idx_type>& ra_idx);
  SparseComplexMatrix concat (const SparseComplexMatrix& rb,
			      const Array<octave_idx_type>& ra_idx);

  friend SparseMatrix real (const SparseComplexMatrix& a);
  friend SparseMatrix imag (const SparseComplexMatrix& a);

  friend SparseMatrix atan2 (const double& x, const SparseMatrix& y);
  friend SparseMatrix atan2 (const SparseMatrix& x, const double& y);
  friend SparseMatrix atan2 (const SparseMatrix& x, const SparseMatrix& y);

  SparseMatrix transpose (void) const 
    { 
      return MSparse<double>::transpose (); 
    }

  SparseMatrix inverse (void) const;
  SparseMatrix inverse (octave_idx_type& info) const;
  SparseMatrix inverse (octave_idx_type& info, double& rcond, int force = 0, 
		        int calc_cond = 1) const;

  DET determinant (void) const;
  DET determinant (octave_idx_type& info) const;
  DET determinant (octave_idx_type& info, double& rcond, int calc_cond = 1) const;

private:
  // Diagonal matrix solvers
  Matrix dsolve (SparseType &typ, const Matrix& b, octave_idx_type& info, double& rcond,
		solve_singularity_handler sing_handler) const;

  ComplexMatrix dsolve (SparseType &typ, const ComplexMatrix& b, octave_idx_type& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseMatrix dsolve (SparseType &typ, const SparseMatrix& b, octave_idx_type& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseComplexMatrix dsolve (SparseType &typ, const SparseComplexMatrix& b,
		octave_idx_type& info, double& rcond, 
		solve_singularity_handler sing_handler) const;

  // Upper triangular matrix solvers
  Matrix utsolve (SparseType &typ, const Matrix& b, octave_idx_type& info, double& rcond,
		solve_singularity_handler sing_handler) const;

  ComplexMatrix utsolve (SparseType &typ, const ComplexMatrix& b, octave_idx_type& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseMatrix utsolve (SparseType &typ, const SparseMatrix& b, octave_idx_type& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseComplexMatrix utsolve (SparseType &typ, const SparseComplexMatrix& b,
		octave_idx_type& info, double& rcond, 
		solve_singularity_handler sing_handler) const;

  // Lower triangular matrix solvers
  Matrix ltsolve (SparseType &typ, const Matrix& b, octave_idx_type& info, double& rcond,
		solve_singularity_handler sing_handler) const;

  ComplexMatrix ltsolve (SparseType &typ, const ComplexMatrix& b, octave_idx_type& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseMatrix ltsolve (SparseType &typ, const SparseMatrix& b, octave_idx_type& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseComplexMatrix ltsolve (SparseType &typ, const SparseComplexMatrix& b,
		octave_idx_type& info, double& rcond, 
		solve_singularity_handler sing_handler) const;

  // Tridiagonal matrix solvers
  Matrix trisolve (SparseType &typ, const Matrix& b, octave_idx_type& info, double& rcond,
		solve_singularity_handler sing_handler) const;

  ComplexMatrix trisolve (SparseType &typ, const ComplexMatrix& b, octave_idx_type& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseMatrix trisolve (SparseType &typ, const SparseMatrix& b, octave_idx_type& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseComplexMatrix trisolve (SparseType &typ, const SparseComplexMatrix& b,
		octave_idx_type& info, double& rcond, 
		solve_singularity_handler sing_handler) const;

  // Banded matrix solvers (umfpack/cholesky)
  Matrix bsolve (SparseType &typ, const Matrix& b, octave_idx_type& info, double& rcond,
		solve_singularity_handler sing_handler) const;

  ComplexMatrix bsolve (SparseType &typ, const ComplexMatrix& b, octave_idx_type& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseMatrix bsolve (SparseType &typ, const SparseMatrix& b, octave_idx_type& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseComplexMatrix bsolve (SparseType &typ, const SparseComplexMatrix& b,
		octave_idx_type& info, double& rcond, 
		solve_singularity_handler sing_handler) const;

  // Full matrix solvers (umfpack/cholesky)
  void * factorize (octave_idx_type& err, double &rcond, Matrix &Control, Matrix &Info,
		    solve_singularity_handler sing_handler) const;

  Matrix fsolve (SparseType &typ, const Matrix& b, octave_idx_type& info, double& rcond,
		solve_singularity_handler sing_handler) const;

  ComplexMatrix fsolve (SparseType &typ, const ComplexMatrix& b, octave_idx_type& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseMatrix fsolve (SparseType &typ, const SparseMatrix& b, octave_idx_type& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseComplexMatrix fsolve (SparseType &typ, const SparseComplexMatrix& b,
		octave_idx_type& info, double& rcond,
		solve_singularity_handler sing_handler) const;

public:
  // Generic interface to solver with no probing of type
  Matrix solve (SparseType &typ, const Matrix& b) const;
  Matrix solve (SparseType &typ, const Matrix& b, octave_idx_type& info) const;
  Matrix solve (SparseType &typ, const Matrix& b, octave_idx_type& info, 
		double& rcond) const;
  Matrix solve (SparseType &typ, const Matrix& b, octave_idx_type& info, double& rcond,
		solve_singularity_handler sing_handler) const;

  ComplexMatrix solve (SparseType &typ, const ComplexMatrix& b) const;
  ComplexMatrix solve (SparseType &typ, const ComplexMatrix& b, 
		       octave_idx_type& info) const;
  ComplexMatrix solve (SparseType &typ, const ComplexMatrix& b, octave_idx_type& info, 
		       double& rcond) const;
  ComplexMatrix solve (SparseType &typ, const ComplexMatrix& b, octave_idx_type& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseMatrix solve (SparseType &typ, const SparseMatrix& b) const;
  SparseMatrix solve (SparseType &typ, const SparseMatrix& b, 
		      octave_idx_type& info) const;
  SparseMatrix solve (SparseType &typ, const SparseMatrix& b, octave_idx_type& info, 
		      double& rcond) const;
  SparseMatrix solve (SparseType &typ, const SparseMatrix& b, octave_idx_type& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseComplexMatrix solve (SparseType &typ, 
			     const SparseComplexMatrix& b) const;
  SparseComplexMatrix solve (SparseType &typ, const SparseComplexMatrix& b, 
			     octave_idx_type& info) const;
  SparseComplexMatrix solve (SparseType &typ, const SparseComplexMatrix& b, 
			     octave_idx_type& info, double& rcond) const;
  SparseComplexMatrix solve (SparseType &typ, const SparseComplexMatrix& b, octave_idx_type& info, 
	       double& rcond, solve_singularity_handler sing_handler) const;

  ColumnVector solve (SparseType &typ, const ColumnVector& b) const;
  ColumnVector solve (SparseType &typ, const ColumnVector& b, 
		      octave_idx_type& info) const;
  ColumnVector solve (SparseType &typ, const ColumnVector& b, 
		      octave_idx_type& info, double& rcond) const;
  ColumnVector solve (SparseType &typ, const ColumnVector& b, octave_idx_type& info,
		double& rcond, solve_singularity_handler sing_handler) const;

  ComplexColumnVector solve (SparseType &typ, 
			     const ComplexColumnVector& b) const;
  ComplexColumnVector solve (SparseType &typ, 
			     const ComplexColumnVector& b, octave_idx_type& info) const;
  ComplexColumnVector solve (SparseType &typ, const ComplexColumnVector& b,
			     octave_idx_type& info, double& rcond) const;
  ComplexColumnVector solve (SparseType &typ, const ComplexColumnVector& b,
			     octave_idx_type& info, double& rcond,
			     solve_singularity_handler sing_handler) const;

  // Generic interface to solver with probing of type
  Matrix solve (const Matrix& b) const;
  Matrix solve (const Matrix& b, octave_idx_type& info) const;
  Matrix solve (const Matrix& b, octave_idx_type& info, double& rcond) const;
  Matrix solve (const Matrix& b, octave_idx_type& info, double& rcond,
		solve_singularity_handler sing_handler) const;

  ComplexMatrix solve (const ComplexMatrix& b) const;
  ComplexMatrix solve (const ComplexMatrix& b, octave_idx_type& info) const;
  ComplexMatrix solve (const ComplexMatrix& b, octave_idx_type& info, 
		       double& rcond) const;
  ComplexMatrix solve (const ComplexMatrix& b, octave_idx_type& info, double& rcond,
		       solve_singularity_handler sing_handler) const;

  SparseMatrix solve (const SparseMatrix& b) const;
  SparseMatrix solve (const SparseMatrix& b, octave_idx_type& info) const;
  SparseMatrix solve (const SparseMatrix& b, octave_idx_type& info, 
		      double& rcond) const;
  SparseMatrix solve (const SparseMatrix& b, octave_idx_type& info, double& rcond,
		solve_singularity_handler sing_handler) const;

  SparseComplexMatrix solve (const SparseComplexMatrix& b) const;
  SparseComplexMatrix solve (const SparseComplexMatrix& b, octave_idx_type& info) const;
  SparseComplexMatrix solve (const SparseComplexMatrix& b, octave_idx_type& info, 
			     double& rcond) const;
  SparseComplexMatrix solve (const SparseComplexMatrix& b, octave_idx_type& info, 
			     double& rcond,
			     solve_singularity_handler sing_handler) const;

  ColumnVector solve (const ColumnVector& b) const;
  ColumnVector solve (const ColumnVector& b, octave_idx_type& info) const;
  ColumnVector solve (const ColumnVector& b, octave_idx_type& info, double& rcond) const;
  ColumnVector solve (const ColumnVector& b, octave_idx_type& info, double& rcond,
		      solve_singularity_handler sing_handler) const;

  ComplexColumnVector solve (const ComplexColumnVector& b) const;
  ComplexColumnVector solve (const ComplexColumnVector& b, octave_idx_type& info) const;
  ComplexColumnVector solve (const ComplexColumnVector& b, octave_idx_type& info,
			     double& rcond) const;
  ComplexColumnVector solve (const ComplexColumnVector& b, octave_idx_type& info,
			     double& rcond,
			     solve_singularity_handler sing_handler) const;

  // Minimum-norm solvers
  Matrix lssolve (const Matrix& b) const;
  Matrix lssolve (const Matrix& b, octave_idx_type& info) const;
  Matrix lssolve (const Matrix& b, octave_idx_type& info, octave_idx_type& rank) const;

  ComplexMatrix lssolve (const ComplexMatrix& b) const;
  ComplexMatrix lssolve (const ComplexMatrix& b, octave_idx_type& info) const;
  ComplexMatrix lssolve (const ComplexMatrix& b, octave_idx_type& info,
			 octave_idx_type& rank) const;

  SparseMatrix lssolve (const SparseMatrix& b) const;
  SparseMatrix lssolve (const SparseMatrix& b, octave_idx_type& info) const;
  SparseMatrix lssolve (const SparseMatrix& b, octave_idx_type& info, octave_idx_type& rank) const;

  SparseComplexMatrix lssolve (const SparseComplexMatrix& b) const;
  SparseComplexMatrix lssolve (const SparseComplexMatrix& b, 
			       octave_idx_type& info) const;
  SparseComplexMatrix lssolve (const SparseComplexMatrix& b, octave_idx_type& info,
			       octave_idx_type& rank) const;

  ColumnVector lssolve (const ColumnVector& b) const;
  ColumnVector lssolve (const ColumnVector& b, octave_idx_type& info) const;
  ColumnVector lssolve (const ColumnVector& b, octave_idx_type& info, octave_idx_type& rank) const;

  ComplexColumnVector lssolve (const ComplexColumnVector& b) const;
  ComplexColumnVector lssolve (const ComplexColumnVector& b, octave_idx_type& info) const;
  ComplexColumnVector lssolve (const ComplexColumnVector& b, octave_idx_type& info,
			       octave_idx_type& rank) const;

  // other operations
  SparseMatrix map (d_d_Mapper f) const;
  SparseBoolMatrix map (b_d_Mapper f) const;

  SparseMatrix& apply (d_d_Mapper f);

  bool any_element_is_negative (bool = false) const;
  bool any_element_is_inf_or_nan (void) const;
  bool all_elements_are_int_or_inf_or_nan (void) const;
  bool all_integers (double& max_val, double& min_val) const;
  bool too_large_for_float (void) const;
 
  SparseBoolMatrix operator ! (void) const;

  SparseBoolMatrix all (int dim = -1) const;
  SparseBoolMatrix any (int dim = -1) const;

  SparseMatrix cumprod (int dim = -1) const;
  SparseMatrix cumsum (int dim = -1) const;
  SparseMatrix prod (int dim = -1) const;
  SparseMatrix sum (int dim = -1) const;
  SparseMatrix sumsq (int dim = -1) const;
  SparseMatrix abs (void) const;

  SparseMatrix diag (octave_idx_type k = 0) const;

  Matrix matrix_value (void) const;

  SparseMatrix squeeze (void) const;

  SparseMatrix index (idx_vector& i, int resize_ok) const;

  SparseMatrix index (idx_vector& i, idx_vector& j, int resize_ok) const;
  
  SparseMatrix index (Array<idx_vector>& ra_idx, int resize_ok) const;

  SparseMatrix reshape (const dim_vector& new_dims) const;

  SparseMatrix permute (const Array<octave_idx_type>& vec, bool inv = false) const;

  SparseMatrix ipermute (const Array<octave_idx_type>& vec) const;

  // i/o

  friend std::ostream& operator << (std::ostream& os, const SparseMatrix& a);
  friend std::istream& operator >> (std::istream& is, SparseMatrix& a);
};

extern SparseMatrix operator * (const SparseMatrix& a, 
				const SparseMatrix& b);
extern Matrix operator * (const Matrix& a, 
				const SparseMatrix& b);
extern Matrix operator * (const SparseMatrix& a, 
				const Matrix& b);

extern SparseMatrix min (double d, const SparseMatrix& m);
extern SparseMatrix min (const SparseMatrix& m, double d);
extern SparseMatrix min (const SparseMatrix& a, const SparseMatrix& b);

extern SparseMatrix max (double d, const SparseMatrix& m);
extern SparseMatrix max (const SparseMatrix& m, double d);
extern SparseMatrix max (const SparseMatrix& a, const SparseMatrix& b);

SPARSE_SMS_CMP_OP_DECLS (SparseMatrix, double)
SPARSE_SMS_BOOL_OP_DECLS (SparseMatrix, double)

SPARSE_SSM_CMP_OP_DECLS (double, SparseMatrix)
SPARSE_SSM_BOOL_OP_DECLS (double, SparseMatrix)

SPARSE_SMSM_CMP_OP_DECLS (SparseMatrix, SparseMatrix)
SPARSE_SMSM_BOOL_OP_DECLS (SparseMatrix, SparseMatrix)

SPARSE_FORWARD_DEFS (MSparse, SparseMatrix, Matrix, double)

#ifdef IDX_TYPE_LONG
#define UMFPACK_DNAME(name) umfpack_dl_ ## name
#else
#define UMFPACK_DNAME(name) umfpack_di_ ## name
#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
