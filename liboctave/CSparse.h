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
along with this program; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_CSparse_h)
#define octave_CSparse_h 1

#include "dMatrix.h"
#include "dNDArray.h"
#include "CNDArray.h"
#include "dColVector.h"
#include "CColVector.h"
#include "oct-cmplx.h"

#include "CmplxDET.h"
#include "MSparse.h"
#include "MSparse-defs.h"
#include "Sparse-op-defs.h"
#include "SparseType.h"

class SparseMatrix;
class SparseBoolMatrix;

class
SparseComplexMatrix : public MSparse<Complex>
{
public:
  
  typedef void (*solve_singularity_handler) (double rcond);

  SparseComplexMatrix (void) : MSparse<Complex> () { }

  SparseComplexMatrix (int r, int c) : MSparse<Complex> (r, c) { }

  explicit SparseComplexMatrix (int r, int c, Complex val) 
    : MSparse<Complex> (r, c, val) { }

  SparseComplexMatrix (int r, int c, double val) 
    : MSparse<Complex> (r, c, Complex (val)) { }

  SparseComplexMatrix (const SparseComplexMatrix& a) 
    : MSparse<Complex> (a) { }

  SparseComplexMatrix (const SparseComplexMatrix& a, const dim_vector& dv) 
    : MSparse<Complex> (a, dv) { }

  SparseComplexMatrix (const MSparse<Complex>& a) : MSparse<Complex> (a) { }

  explicit SparseComplexMatrix (const ComplexMatrix& a) 
    : MSparse<Complex> (a) { }

  explicit SparseComplexMatrix (const ComplexNDArray& a) 
    : MSparse<Complex> (a) { }

  explicit SparseComplexMatrix (const Array<Complex> a, const Array<int>& r, 
			 const Array<int>& c, int nr = -1, 
			 int nc = -1, bool sum_terms = true)
    : MSparse<Complex> (a, r, c, nr, nc, sum_terms) { }

  explicit SparseComplexMatrix (const Array<Complex> a, 
				const Array<double>& r, 
				const Array<double>& c, int nr = -1, 
				int nc = -1, bool sum_terms = true)
    : MSparse<Complex> (a, r, c, nr, nc, sum_terms) { }

  explicit SparseComplexMatrix (const SparseMatrix& a);

  explicit SparseComplexMatrix (const SparseBoolMatrix& a);

  SparseComplexMatrix (int r, int c, int num_nz) 
    : MSparse<Complex> (r, c, num_nz) { }

  SparseComplexMatrix& operator = (const SparseComplexMatrix& a)
    {
      MSparse<Complex>::operator = (a);
      return *this;
    }

  bool operator == (const SparseComplexMatrix& a) const;
  bool operator != (const SparseComplexMatrix& a) const;

  bool is_hermitian (void) const;

  SparseComplexMatrix max (int dim = 0) const;
  SparseComplexMatrix max (Array2<int>& index, int dim = 0) const;
  SparseComplexMatrix min (int dim = 0) const;
  SparseComplexMatrix min (Array2<int>& index, int dim = 0) const;

  SparseComplexMatrix& insert (const SparseComplexMatrix& a, int r, int c);
  SparseComplexMatrix& insert (const SparseMatrix& a, int r, int c);

  SparseComplexMatrix concat (const SparseComplexMatrix& rb,
			      const Array<int>& ra_idx);
  SparseComplexMatrix concat (const SparseMatrix& rb,
			      const Array<int>& ra_idx);

  ComplexMatrix matrix_value (void) const;

  SparseComplexMatrix hermitian (void) const;  // complex conjugate transpose
  SparseComplexMatrix transpose (void) const
    { return MSparse<Complex>::transpose (); }

  friend SparseComplexMatrix conj (const SparseComplexMatrix& a);

  SparseComplexMatrix inverse (void) const;
  SparseComplexMatrix inverse (int& info) const;
  SparseComplexMatrix inverse (int& info, double& rcond, int force = 0,
			       int calc_cond = 1) const;

  ComplexDET determinant (void) const;
  ComplexDET determinant (int& info) const;
  ComplexDET determinant (int& info, double& rcond, 
				int calc_cond = 1) const;

private:
  // Diagonal matrix solvers
  ComplexMatrix dsolve (SparseType &typ, const Matrix& b, int& info, 
	        double& rcond, solve_singularity_handler sing_handler) const;

  ComplexMatrix dsolve (SparseType &typ, const ComplexMatrix& b, int& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseComplexMatrix dsolve (SparseType &typ, const SparseMatrix& b, int& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseComplexMatrix dsolve (SparseType &typ, const SparseComplexMatrix& b,
		int& info, double& rcond, 
		solve_singularity_handler sing_handler) const;

  // Upper triangular matrix solvers
  ComplexMatrix utsolve (SparseType &typ, const Matrix& b, int& info,
		double& rcond, solve_singularity_handler sing_handler) const;

  ComplexMatrix utsolve (SparseType &typ, const ComplexMatrix& b, int& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseComplexMatrix utsolve (SparseType &typ, const SparseMatrix& b, int& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseComplexMatrix utsolve (SparseType &typ, const SparseComplexMatrix& b, 
		int& info, double& rcond, 
		solve_singularity_handler sing_handler) const;

  // Lower triangular matrix solvers
  ComplexMatrix ltsolve (SparseType &typ, const Matrix& b, int& info, 
	       double& rcond, solve_singularity_handler sing_handler) const;

  ComplexMatrix ltsolve (SparseType &typ, const ComplexMatrix& b, int& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseComplexMatrix ltsolve (SparseType &typ, const SparseMatrix& b, int& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseComplexMatrix ltsolve (SparseType &typ, const SparseComplexMatrix& b, 
		int& info, double& rcond, 
		solve_singularity_handler sing_handler) const;

  // Tridiagonal matrix solvers
  ComplexMatrix trisolve (SparseType &typ, const Matrix& b, int& info, 
	       double& rcond, solve_singularity_handler sing_handler) const;

  ComplexMatrix trisolve (SparseType &typ, const ComplexMatrix& b, int& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseComplexMatrix trisolve (SparseType &typ, const SparseMatrix& b, int& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseComplexMatrix trisolve (SparseType &typ, const SparseComplexMatrix& b,
		int& info, double& rcond, 
		solve_singularity_handler sing_handler) const;

  // Banded matrix solvers (umfpack/cholesky)
  ComplexMatrix bsolve (SparseType &typ, const Matrix& b, int& info,
		double& rcond, solve_singularity_handler sing_handler) const;

  ComplexMatrix bsolve (SparseType &typ, const ComplexMatrix& b, int& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseComplexMatrix bsolve (SparseType &typ, const SparseMatrix& b, int& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseComplexMatrix bsolve (SparseType &typ, const SparseComplexMatrix& b,
		int& info, double& rcond,
		solve_singularity_handler sing_handler) const;

  // Full matrix solvers (umfpack/cholesky)
  void * factorize (int& err, double &rcond, Matrix &Control, Matrix &Info,
		    solve_singularity_handler sing_handler) const;

  ComplexMatrix fsolve (SparseType &typ, const Matrix& b, int& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  ComplexMatrix fsolve (SparseType &typ, const ComplexMatrix& b, int& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseComplexMatrix fsolve (SparseType &typ, const SparseMatrix& b, int& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseComplexMatrix fsolve (SparseType &typ, const SparseComplexMatrix& b,
		int& info, double& rcond, 
		solve_singularity_handler sing_handler) const;

public:
  // Generic interface to solver with no probing of type
  ComplexMatrix solve (SparseType &typ, const Matrix& b) const;
  ComplexMatrix solve (SparseType &typ, const Matrix& b, int& info) const;
  ComplexMatrix solve (SparseType &typ, const Matrix& b, int& info, 
		double& rcond) const;
  ComplexMatrix solve (SparseType &typ, const Matrix& b, int& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  ComplexMatrix solve (SparseType &typ, const ComplexMatrix& b) const;
  ComplexMatrix solve (SparseType &typ, const ComplexMatrix& b, 
		       int& info) const;
  ComplexMatrix solve (SparseType &typ, const ComplexMatrix& b, int& info, 
		       double& rcond) const;
  ComplexMatrix solve (SparseType &typ, const ComplexMatrix& b, int& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseComplexMatrix solve (SparseType &typ, const SparseMatrix& b) const;
  SparseComplexMatrix solve (SparseType &typ, const SparseMatrix& b, 
		      int& info) const;
  SparseComplexMatrix solve (SparseType &typ, const SparseMatrix& b, int& info, 
		      double& rcond) const;
  SparseComplexMatrix solve (SparseType &typ, const SparseMatrix& b, int& info, 
		double& rcond, solve_singularity_handler sing_handler) const;

  SparseComplexMatrix solve (SparseType &typ, 
			     const SparseComplexMatrix& b) const;
  SparseComplexMatrix solve (SparseType &typ, const SparseComplexMatrix& b, 
			     int& info) const;
  SparseComplexMatrix solve (SparseType &typ, const SparseComplexMatrix& b, 
			     int& info, double& rcond) const;
  SparseComplexMatrix solve (SparseType &typ, const SparseComplexMatrix& b, int& info, 
	       double& rcond, solve_singularity_handler sing_handler) const;

  ComplexColumnVector solve (SparseType &typ, const ColumnVector& b) const;
  ComplexColumnVector solve (SparseType &typ, const ColumnVector& b, 
		      int& info) const;
  ComplexColumnVector solve (SparseType &typ, const ColumnVector& b, 
		      int& info, double& rcond) const;
  ComplexColumnVector solve (SparseType &typ, const ColumnVector& b, int& info,
		double& rcond, solve_singularity_handler sing_handler) const;

  ComplexColumnVector solve (SparseType &typ, 
			     const ComplexColumnVector& b) const;
  ComplexColumnVector solve (SparseType &typ, 
			     const ComplexColumnVector& b, int& info) const;
  ComplexColumnVector solve (SparseType &typ, const ComplexColumnVector& b,
			     int& info, double& rcond) const;
  ComplexColumnVector solve (SparseType &typ, const ComplexColumnVector& b,
			     int& info, double& rcond,
			     solve_singularity_handler sing_handler) const;

  // Generic interface to solver with probing of type
  ComplexMatrix solve (const Matrix& b) const;
  ComplexMatrix solve (const Matrix& b, int& info) const;
  ComplexMatrix solve (const Matrix& b, int& info, double& rcond) const;
  ComplexMatrix solve (const Matrix& b, int& info, double& rcond, 
		       solve_singularity_handler sing_handler) const;

  ComplexMatrix solve (const ComplexMatrix& b) const;
  ComplexMatrix solve (const ComplexMatrix& b, int& info) const;
  ComplexMatrix solve (const ComplexMatrix& b, int& info, 
		       double& rcond) const;
  ComplexMatrix solve (const ComplexMatrix& b, int& info, double& rcond,
		       solve_singularity_handler sing_handler) const;

  SparseComplexMatrix solve (const SparseMatrix& b) const;
  SparseComplexMatrix solve (const SparseMatrix& b, int& info) const;
  SparseComplexMatrix solve (const SparseMatrix& b, int& info, 
			     double& rcond) const;
  SparseComplexMatrix solve (const SparseMatrix& b, int& info, 
			     double& rcond, 
		       solve_singularity_handler sing_handler) const;

  SparseComplexMatrix solve (const SparseComplexMatrix& b) const;
  SparseComplexMatrix solve (const SparseComplexMatrix& b, int& info) const;
  SparseComplexMatrix solve (const SparseComplexMatrix& b, int& info, 
			     double& rcond) const;
  SparseComplexMatrix solve (const SparseComplexMatrix& b, int& info, 
			     double& rcond,
			     solve_singularity_handler sing_handler) const;

  ComplexColumnVector solve (const ColumnVector& b) const;
  ComplexColumnVector solve (const ColumnVector& b, int& info) const;
  ComplexColumnVector solve (const ColumnVector& b, int& info,
			     double& rcond) const;
  ComplexColumnVector solve (const ColumnVector& b, int& info, double& rcond,
			     solve_singularity_handler sing_handler) const;

  ComplexColumnVector solve (const ComplexColumnVector& b) const;
  ComplexColumnVector solve (const ComplexColumnVector& b, int& info) const;
  ComplexColumnVector solve (const ComplexColumnVector& b, int& info,
			     double& rcond) const;
  ComplexColumnVector solve (const ComplexColumnVector& b, int& info,
			     double& rcond,
			     solve_singularity_handler sing_handler) const;

  ComplexMatrix lssolve (const Matrix& b) const;
  ComplexMatrix lssolve (const Matrix& b, int& info) const;
  ComplexMatrix lssolve (const Matrix& b, int& info, int& rank) const;

  ComplexMatrix lssolve (const ComplexMatrix& b) const;
  ComplexMatrix lssolve (const ComplexMatrix& b, int& info) const;
  ComplexMatrix lssolve (const ComplexMatrix& b, int& info,
			 int& rank) const;

  SparseComplexMatrix lssolve (const SparseMatrix& b) const;
  SparseComplexMatrix lssolve (const SparseMatrix& b, int& info) const;
  SparseComplexMatrix lssolve (const SparseMatrix& b, int& info, 
			       int& rank) const;

  SparseComplexMatrix lssolve (const SparseComplexMatrix& b) const;
  SparseComplexMatrix lssolve (const SparseComplexMatrix& b, 
			       int& info) const;
  SparseComplexMatrix lssolve (const SparseComplexMatrix& b, int& info,
			       int& rank) const;

  ComplexColumnVector lssolve (const ColumnVector& b) const;
  ComplexColumnVector lssolve (const ColumnVector& b, int& info) const;
  ComplexColumnVector lssolve (const ColumnVector& b, int& info,
			       int& rank) const;

  ComplexColumnVector lssolve (const ComplexColumnVector& b) const;
  ComplexColumnVector lssolve (const ComplexColumnVector& b, int& info) const;
  ComplexColumnVector lssolve (const ComplexColumnVector& b, int& info,
			       int& rank) const;

  SparseComplexMatrix squeeze (void) const;

  SparseComplexMatrix index (idx_vector& i, int resize_ok) const;

  SparseComplexMatrix index (idx_vector& i, idx_vector& j, int resize_ok) const;
  
  SparseComplexMatrix index (Array<idx_vector>& ra_idx, int resize_ok) const;

  SparseComplexMatrix reshape (const dim_vector& new_dims) const;

  SparseComplexMatrix permute (const Array<int>& vec, bool inv = false) const;

  SparseComplexMatrix ipermute (const Array<int>& vec) const;

  SparseComplexMatrix map (c_c_Mapper f) const;
  SparseMatrix map (d_c_Mapper f) const;
  SparseBoolMatrix map (b_c_Mapper f) const;

  SparseComplexMatrix& apply (c_c_Mapper f);

  bool any_element_is_inf_or_nan (void) const;
  bool all_elements_are_real (void) const;
  bool all_integers (double& max_val, double& min_val) const;
  bool too_large_for_float (void) const;

  SparseBoolMatrix operator ! (void) const;

  SparseBoolMatrix all (int dim = -1) const;
  SparseBoolMatrix any (int dim = -1) const;

  SparseComplexMatrix cumprod (int dim = -1) const;
  SparseComplexMatrix cumsum (int dim = -1) const;
  SparseComplexMatrix prod (int dim = -1) const;
  SparseComplexMatrix sum (int dim = -1) const;
  SparseComplexMatrix sumsq (int dim = -1) const;
  SparseMatrix abs (void) const;

  SparseComplexMatrix diag (int k = 0) const;

  // i/o
  friend std::ostream& operator << (std::ostream& os, 
				    const SparseComplexMatrix& a);
  friend std::istream& operator >> (std::istream& is, 
				    SparseComplexMatrix& a);
};

extern SparseComplexMatrix operator * (const SparseMatrix&,        
				       const SparseComplexMatrix&);
extern SparseComplexMatrix operator * (const SparseComplexMatrix&, 
				       const SparseMatrix&);
extern SparseComplexMatrix operator * (const SparseComplexMatrix&, 
				       const SparseComplexMatrix&);

extern SparseComplexMatrix min (const Complex& c, 
				const SparseComplexMatrix& m);
extern SparseComplexMatrix min (const SparseComplexMatrix& m, 
				const Complex& c);
extern SparseComplexMatrix min (const SparseComplexMatrix& a, 
				const SparseComplexMatrix& b);

extern SparseComplexMatrix max (const Complex& c, 
				const SparseComplexMatrix& m);
extern SparseComplexMatrix max (const SparseComplexMatrix& m, 
				const Complex& c);
extern SparseComplexMatrix max (const SparseComplexMatrix& a, 
				const SparseComplexMatrix& b);

SPARSE_SMS_CMP_OP_DECLS (SparseComplexMatrix, Complex)
SPARSE_SMS_BOOL_OP_DECLS (SparseComplexMatrix, Complex)

SPARSE_SSM_CMP_OP_DECLS (Complex, SparseComplexMatrix)
SPARSE_SSM_BOOL_OP_DECLS (Complex, SparseComplexMatrix)

SPARSE_SMSM_CMP_OP_DECLS (SparseComplexMatrix, SparseComplexMatrix)
SPARSE_SMSM_BOOL_OP_DECLS (SparseComplexMatrix, SparseComplexMatrix)

SPARSE_FORWARD_DEFS (MSparse, SparseComplexMatrix, ComplexMatrix, Complex)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
