// Copyright (C) 2016-2022 The Octave Project Developers
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

/* -----------------------------------------------------------------
 * generating custom SUNDIALS implementations
 * to be able to allow Octave's native classes and 
 * solvers to be utilized by the SUNDIALS IDA Class
 * ----------------------------------------------------------------- */

#ifndef __CONFIG_H_INCLUDE_GUARD
#define __CONFIG_H_INCLUDE_GUARD
#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif
#endif

#include <stdio.h>
#include <stdlib.h>
#include <iostream>

#include <oct.h>
#include <builtin-defun-decls.h>
#include <data.h>
#include <ov.h>
#include <ovl.h>
#include <dColVector.h>
#include <dSparse.h>
#include <dMatrix.h>

#include "oct-locbuf.h"
#include "quit.h"

/* FIXME : This flag currently checks for a 
  lot of Sundials features, such as nvecserial,
  etc, which might not be needed for just these files.
  Maybe a simpler configure flag should be made for 
  this. */

#if defined (HAVE_SUNDIALS)

#   include <sundials/sundials_math.h>
#   include <sundials/sundials_nvector.h>
#   include <sundials/sundials_config.h>
#   include <sundials/sundials_linearsolver.h>
#   include <sundials/sundials_matrix.h>

#endif

/* -----------------------------------------------------------------
 * Useful mathematical macros for computation
 * ----------------------------------------------------------------- */
#define ZERO RCONST(0.0)
#define HALF RCONST(0.5)
#define ONE  RCONST(1.0)
#define ONEPT5 RCONST(1.5)
#define TWO       RCONST(2.0)
#define TWOTHIRDS RCONST(0.666666666666666666666666666666667)

/* -----------------------------------------------------------------
 * SUNContext : Since SUNDIALS 6.0.0, all SUNDIALS objects
 * (vectors, linear and nonlinear solvers, matrices, etc.)
 * hold a reference to a common simulation context object 
 * defined by the SUNContext class. The macro ensures these 
 * declarations maintain backward compatibility with versions
 * before 6.0.0 as well.
 * ----------------------------------------------------------------- */
#  if (HAVE_SUNDIALS_SUNCONTEXT)
#    define SP_ARG_SUNCONTEXT SUNContext m_sunContext
#    define ARG_SUNCONTEXT , SUNContext m_sunContext
#    define OCTAVE_SUNCONTEXT , m_sunContext
#    define SP_OCTAVE_SUNCONTEXT  m_sunContext
#  else
#    define ARG_SUNCONTEXT 
#    define OCTAVE_SUNCONTEXT
#    define SP_ARG_SUNCONTEXT 
#    define SP_OCTAVE_SUNCONTEXT
#  endif

#ifdef __cplusplus  /* wrapper to enable C++ usage */
extern "C" {
#endif

/*
 * -----------------------------------------------------------------
 * Macros for access to octave implementation of N_Vector 
 * -----------------------------------------------------------------
 */

#define NV_CONTENT_C(v)  ( (ColumnVector *)(v->content) )
#define NV_LENGTH_C(v)   ( NV_CONTENT_C(v)->numel() )
#define NV_DATA_C(v)     ( NV_CONTENT_C(v)->fortran_vec() )
#define NV_Ith_C(v,i)     ( NV_DATA_C(v)[i])

/* -----------------------------------------------------------------
 * NVECTOR API functions exported for Octave's custom implementation
 * ----------------------------------------------------------------- */

SUNDIALS_EXPORT N_Vector N_VNew_Octave(int vec_length ARG_SUNCONTEXT );
SUNDIALS_EXPORT N_Vector N_VNewEmpty_Octave ( SP_ARG_SUNCONTEXT );
SUNDIALS_EXPORT N_Vector N_VMake_Octave(const ColumnVector& v ARG_SUNCONTEXT );

SUNDIALS_EXPORT sunindextype N_VGetLength_Octave(N_Vector v);
SUNDIALS_EXPORT void N_VPrint_Octave(N_Vector v);
SUNDIALS_EXPORT N_Vector_ID N_VGetVectorID_Octave(N_Vector v);
SUNDIALS_EXPORT N_Vector N_VCloneEmpty_Octave(N_Vector w);
SUNDIALS_EXPORT N_Vector N_VClone_Octave(N_Vector w);
SUNDIALS_EXPORT void N_VDestroy_Octave(N_Vector v);
SUNDIALS_EXPORT void N_VSpace_Octave(N_Vector v, sunindextype *lrw, sunindextype *liw);
SUNDIALS_EXPORT double *N_VGetArrayPointer_Octave(N_Vector v);
SUNDIALS_EXPORT void N_VSetArrayPointer_Octave(realtype *v_data, N_Vector v);

/* standard vector operations */
SUNDIALS_EXPORT void N_VLinearSum_Octave(double a, N_Vector x, double b, N_Vector y, N_Vector z);
SUNDIALS_EXPORT void N_VConst_Octave(double c, N_Vector z);
SUNDIALS_EXPORT void N_VProd_Octave(N_Vector x, N_Vector y, N_Vector z);
SUNDIALS_EXPORT void N_VDiv_Octave(N_Vector x, N_Vector y, N_Vector z);
SUNDIALS_EXPORT void N_VScale_Octave(double c, N_Vector x, N_Vector z);
SUNDIALS_EXPORT void N_VAbs_Octave(N_Vector x, N_Vector z);
SUNDIALS_EXPORT void N_VInv_Octave(N_Vector x, N_Vector z);
SUNDIALS_EXPORT void N_VAddConst_Octave(N_Vector x, double b, N_Vector z);
SUNDIALS_EXPORT double N_VDotProd_Octave(N_Vector x, N_Vector y);
SUNDIALS_EXPORT double N_VMaxNorm_Octave(N_Vector x);
SUNDIALS_EXPORT double N_VWrmsNorm_Octave(N_Vector x, N_Vector w);
SUNDIALS_EXPORT double N_VWrmsNormMask_Octave(N_Vector x, N_Vector w, N_Vector id);
SUNDIALS_EXPORT double N_VMin_Octave(N_Vector x);
SUNDIALS_EXPORT void N_VCompare_Octave(double c, N_Vector x, N_Vector z);
SUNDIALS_EXPORT booleantype N_VInvTest_Octave(N_Vector x, N_Vector z);
SUNDIALS_EXPORT booleantype N_VConstrMask_Octave(N_Vector c, N_Vector x, N_Vector m);
SUNDIALS_EXPORT double N_VMinQuotient_Octave(N_Vector num, N_Vector denom);

// /* fused vector operations */
SUNDIALS_EXPORT int N_VScaleAddMulti_Octave(int nvec, double* a, N_Vector x,
                                            N_Vector* Y, N_Vector* Z);

/* vector array operations */
SUNDIALS_EXPORT int N_VLinearSumVectorArray_Octave(int nvec, 
                                                   double a, N_Vector* X,
                                                   double b, N_Vector* Y,
                                                   N_Vector* Z);
SUNDIALS_EXPORT int N_VScaleVectorArray_Octave(int nvec, double* c,
                                               N_Vector* X, N_Vector* Z);

/* OPTIONAL local reduction kernels (no parallel communication) */
SUNDIALS_EXPORT double N_VWSqrSumLocal_Octave(N_Vector x, N_Vector w);
SUNDIALS_EXPORT double N_VWSqrSumMaskLocal_Octave(N_Vector x, N_Vector w, N_Vector id);

/*
 * -----------------------------------------------------------------
 * Enable / disable fused vector operations
 * -----------------------------------------------------------------
 */
SUNDIALS_EXPORT int N_VEnableFusedOps_Octave(N_Vector v, booleantype tf);


/* ------------------------------------------------------
 * Macros for access to Octave implementation of SUNMATRIX
 * ----------------------------------------------------- */
#define CSC_MAT 0  /* Matrix Type Definition */
#define SM_CONTENT_O(A)     ( (SparseMatrix *)(A->content) )
#define SM_ROWS_O(A)        ( SM_CONTENT_O(A)->rows() )
#define SM_COLS_O(A)        ( SM_CONTENT_O(A)->cols() )
#define SM_COLUMNS_O(A)     ( SM_CONTENT_O(A)->columns() )
#define SM_NNZ_O(A)         ( SM_CONTENT_O(A)->nnz() )
/* directly setting =N as Octave only supports CSC Representation */
#define SM_NP_O(A)          ( SM_CONTENT_O(A)->columns() ) 
#define SM_SPARSETYPE_O(A)  ( CSC_MAT ) 
#define SM_DATA_O(A)        ( SM_CONTENT_O(A)->data() )
#define SM_INDEXVALS_O(A)   ( SM_CONTENT_O(A)->ridx() )
#define SM_INDEXPTRS_O(A)   ( SM_CONTENT_O(A)->cidx() )

/* ----------------------------------------
 * SUNMATRIX API functions exported for Octave's
 * custom implementation
 * ---------------------------------------- */

SUNDIALS_EXPORT SUNMatrix OCTSparseMatrix(sunindextype M, sunindextype N,
                                          sunindextype NNZ ARG_SUNCONTEXT );
SUNDIALS_EXPORT void OCTSparseMatrix_Print(SUNMatrix A);

SUNDIALS_EXPORT sunindextype OCTSparseMatrix_Rows(SUNMatrix A);
SUNDIALS_EXPORT sunindextype OCTSparseMatrix_Columns(SUNMatrix A);
SUNDIALS_EXPORT sunindextype OCTSparseMatrix_NNZ(SUNMatrix A);
SUNDIALS_EXPORT sunindextype OCTSparseMatrix_NP(SUNMatrix A);
SUNDIALS_EXPORT int OCTSparseMatrix_SparseType(SUNMatrix A);
SUNDIALS_EXPORT realtype* OCTSparseMatrix_Data(SUNMatrix A);
SUNDIALS_EXPORT sunindextype* OCTSparseMatrix_IndexValues(SUNMatrix A);
SUNDIALS_EXPORT sunindextype* OCTSparseMatrix_IndexPointers(SUNMatrix A);

SUNDIALS_EXPORT SUNMatrix_ID OCTMatGetID_Sparse(SUNMatrix A);
SUNDIALS_EXPORT void OCTMatDestroy_Sparse (SUNMatrix A);
SUNDIALS_EXPORT int OCTMatZero_Sparse (SUNMatrix A);

/*
 * ----------------------------------------------------------------------------
 * Octave implementation of SUNLinearSolver
 * ----------------------------------------------------------------------------
 */
struct _OCTLinearSolverContent_GEN {
  int              last_flag;
  int              first_factorize;
};

/* --------------------------------------------------
 * Declarations of Octave's custom implementation of
 * a Matrix Linear Solver
 * ------------------------------------------------- */
typedef struct _OCTLinearSolverContent_GEN *OCTLinearSolverContent_GEN;

/* -------------------------------------
 * SUNLINEARSOLVER API functions exported for Octave's
 * custom implementation
 * -----------------------------------*/

SUNDIALS_EXPORT SUNLinearSolver OCTLinSol_Gen(N_Vector y, SUNMatrix A ARG_SUNCONTEXT );
SUNDIALS_EXPORT SUNLinearSolver_Type OCTLinSolGetType_Gen(SUNLinearSolver S);
SUNDIALS_EXPORT SUNLinearSolver_ID OCTLinSolGetID_Gen(SUNLinearSolver S);
SUNDIALS_EXPORT int OCTLinSolSolve_Gen(SUNLinearSolver S, SUNMatrix A,
                                       N_Vector x, N_Vector b, realtype tol);
SUNDIALS_EXPORT int OCTLinSolFree_Gen(SUNLinearSolver S);


#ifdef __cplusplus
}
#endif