////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2016-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "dColVector.h"
#include "dMatrix.h"
#include "dSparse.h"
#include "f77-fcn.h"
#include "lo-utils.h"

#include "Cell.h"
#include "defun-dld.h"
#include "error.h"
#include "errwarn.h"
#include "oct-map.h"
#include "ov.h"
#include "ovl.h"
#include "pager.h"
#include "parse.h"

#if defined (HAVE_SUNDIALS)

#  if defined (HAVE_NVECTOR_NVECTOR_SERIAL_H)
#    include <nvector/nvector_serial.h>
#  endif

#  if defined (HAVE_IDA_IDA_H)
#    include <ida/ida.h>
#  elif defined (HAVE_IDA_H)
#    include <ida.h>
#  endif
#  if defined (HAVE_IDA_IDA_DIRECT_H)
#    include <ida/ida_direct.h>
#  elif defined (HAVE_IDA_DIRECT_H)
#    include <ida_direct.h>
#  endif

#  if defined (HAVE_SUNLINSOL_SUNLINSOL_DENSE_H)
#    include <sunlinsol/sunlinsol_dense.h>
#  endif

#  if defined (HAVE_SUNLINSOL_SUNLINSOL_KLU_H)
#    if defined (HAVE_KLU_H)
#      include <klu.h>
#    endif
#    if defined (HAVE_KLU_KLU_H)
#      include <klu/klu.h>
#    endif
#    if defined (HAVE_SUITESPARSE_KLU_H)
#      include <suitesparse/klu.h>
#    endif
#    if defined (HAVE_UFPARSE_KLU_H)
#      include <ufsparse/klu.h>
#    endif
#    include <sunlinsol/sunlinsol_klu.h>
#  endif

#endif

OCTAVE_BEGIN_NAMESPACE(octave)

#if defined (HAVE_SUNDIALS)

#  if ! defined (HAVE_IDASETJACFN) && defined (HAVE_IDADLSSETJACFN)
static inline int
IDASetJacFn (void *ida_mem, IDADlsJacFn jac)
{
  return IDADlsSetJacFn (ida_mem, jac);
}
#  endif

#  if ! defined (HAVE_IDASETLINEARSOLVER) && defined (HAVE_IDADLSSETLINEARSOLVER)
static inline int
IDASetLinearSolver (void *ida_mem, SUNLinearSolver LS, SUNMatrix A)
{
  return IDADlsSetLinearSolver (ida_mem, LS, A);
}
#  endif

#  if ! defined (HAVE_SUNLINSOL_DENSE) && defined (HAVE_SUNDENSELINEARSOLVER)
static inline SUNLinearSolver
SUNLinSol_Dense (N_Vector y, SUNMatrix A)
{
  return SUNDenseLinearSolver (y, A);
}
#  endif

#  if defined (HAVE_SUNDIALS_SUNLINSOL_KLU)
#    if ! defined (HAVE_SUNLINSOL_KLU) && defined (HAVE_SUNKLU)
static inline SUNLinearSolver
SUNLinSol_KLU (N_Vector y, SUNMatrix A)
{
  return SUNKLU (y, A);
}
#    endif
#  endif

static inline realtype *
nv_data_s (N_Vector& v)
{
#  if defined (HAVE_PRAGMA_GCC_DIAGNOSTIC)
  // Disable warning from GCC about old-style casts in Sundials
  // macro expansions.  Do this in a function so that this
  // diagnostic may still be enabled for the rest of the file.
#   pragma GCC diagnostic push
#   pragma GCC diagnostic ignored "-Wold-style-cast"
#  endif

  return NV_DATA_S (v);

#  if defined (HAVE_PRAGMA_GCC_DIAGNOSTIC)
  // Restore prevailing warning state for remainder of the file.
#   pragma GCC diagnostic pop
#  endif
}

class IDA
{
public:

  typedef
  ColumnVector (*DAERHSFuncIDA) (const ColumnVector& x,
                                 const ColumnVector& xdot,
                                 realtype t, const octave_value& idaf);

  typedef
  Matrix (*DAEJacFuncDense) (const ColumnVector& x,
                             const ColumnVector& xdot, realtype t,
                             realtype cj, const octave_value& idaj);

  typedef
  SparseMatrix (*DAEJacFuncSparse) (const ColumnVector& x,
                                    const ColumnVector& xdot,
                                    realtype t, realtype cj,
                                    const octave_value& idaj);

  typedef
  Matrix (*DAEJacCellDense) (Matrix *dfdy, Matrix *dfdyp,
                             realtype cj);

  typedef
  SparseMatrix (*DAEJacCellSparse) (SparseMatrix *dfdy,
                                    SparseMatrix *dfdyp, realtype cj);

  //Default
  IDA (void)
    : m_t0 (0.0), m_y0 (), m_yp0 (), m_havejac (false), m_havejacfcn (false),
      m_havejacsparse (false), m_mem (nullptr), m_num (), m_ida_fcn (),
      m_ida_jac (), m_dfdy (nullptr), m_dfdyp (nullptr), m_spdfdy (nullptr),
      m_spdfdyp (nullptr), m_fcn (nullptr), m_jacfcn (nullptr),
      m_jacspfcn (nullptr), m_jacdcell (nullptr), m_jacspcell (nullptr),
      m_sunJacMatrix (nullptr), m_sunLinearSolver (nullptr)
  { }


  IDA (realtype t, ColumnVector y, ColumnVector yp,
       const octave_value& ida_fcn, DAERHSFuncIDA daefun)
    : m_t0 (t), m_y0 (y), m_yp0 (yp), m_havejac (false), m_havejacfcn (false),
      m_havejacsparse (false), m_mem (nullptr), m_num (), m_ida_fcn (ida_fcn),
      m_ida_jac (), m_dfdy (nullptr), m_dfdyp (nullptr), m_spdfdy (nullptr),
      m_spdfdyp (nullptr), m_fcn (daefun), m_jacfcn (nullptr),
      m_jacspfcn (nullptr), m_jacdcell (nullptr), m_jacspcell (nullptr),
      m_sunJacMatrix (nullptr), m_sunLinearSolver (nullptr)
  { }


  ~IDA (void)
  {
    IDAFree (&m_mem);
    SUNLinSolFree (m_sunLinearSolver);
    SUNMatDestroy (m_sunJacMatrix);
#  if defined (HAVE_SUNDIALS_SUNCONTEXT)
    SUNContext_Free (&m_sunContext);
#  endif
  }

  IDA&
  set_jacobian (const octave_value& jac, DAEJacFuncDense j)
  {
    m_jacfcn = j;
    m_ida_jac = jac;
    m_havejac = true;
    m_havejacfcn = true;
    m_havejacsparse = false;

    return *this;
  }

  IDA&
  set_jacobian (const octave_value& jac, DAEJacFuncSparse j)
  {
    m_jacspfcn = j;
    m_ida_jac = jac;
    m_havejac = true;
    m_havejacfcn = true;
    m_havejacsparse = true;

    return *this;
  }

  IDA&
  set_jacobian (Matrix *dy, Matrix *dyp, DAEJacCellDense j)
  {
    m_jacdcell = j;
    m_dfdy = dy;
    m_dfdyp = dyp;
    m_havejac = true;
    m_havejacfcn = false;
    m_havejacsparse = false;

    return *this;
  }

  IDA&
  set_jacobian (SparseMatrix *dy, SparseMatrix *dyp,
                DAEJacCellSparse j)
  {
    m_jacspcell = j;
    m_spdfdy = dy;
    m_spdfdyp = dyp;
    m_havejac = true;
    m_havejacfcn = false;
    m_havejacsparse = true;

    return *this;
  }

  void set_userdata (void);

  void initialize (void);

  static ColumnVector NVecToCol (N_Vector& v, octave_f77_int_type n);

#  if defined (HAVE_SUNDIALS_SUNCONTEXT)
  N_Vector ColToNVec (const ColumnVector& data, octave_f77_int_type n);
#  else
  static N_Vector ColToNVec (const ColumnVector& data, octave_f77_int_type n);
#  endif

  void
  set_up (const ColumnVector& y);

  void
  set_tolerance (ColumnVector& abstol, realtype reltol);

  void
  set_tolerance (realtype abstol, realtype reltol);

  static int
  resfun (realtype t, N_Vector yy, N_Vector yyp,
          N_Vector rr, void *user_data);

  void
  resfun_impl (realtype t, N_Vector& yy,
               N_Vector& yyp, N_Vector& rr);
  static int
  jacdense (realtype t, realtype cj, N_Vector yy, N_Vector yyp,
            N_Vector, SUNMatrix JJ, void *user_data, N_Vector,
            N_Vector, N_Vector)
  {
    IDA *self = static_cast <IDA *> (user_data);
    self->jacdense_impl (t, cj, yy, yyp, JJ);
    return 0;
  }

  void
  jacdense_impl (realtype t, realtype cj,
                 N_Vector& yy, N_Vector& yyp, SUNMatrix& JJ);

#  if defined (HAVE_SUNDIALS_SUNLINSOL_KLU)
  static int
  jacsparse (realtype t, realtype cj, N_Vector yy, N_Vector yyp,
             N_Vector, SUNMatrix Jac, void *user_data, N_Vector,
             N_Vector, N_Vector)
  {
    IDA *self = static_cast <IDA *> (user_data);
    self->jacsparse_impl (t, cj, yy, yyp, Jac);
    return 0;
  }

  void
  jacsparse_impl (realtype t, realtype cj,
                  N_Vector& yy, N_Vector& yyp, SUNMatrix& Jac);
#  endif

  void set_maxstep (realtype maxstep);

  void set_initialstep (realtype initialstep);

  bool
  interpolate (octave_idx_type& cont, Matrix& output, ColumnVector& tout,
               int refine, realtype tend, bool haveoutputfcn,
               bool haveoutputsel, const octave_value& output_fcn,
               ColumnVector& outputsel, bool haveeventfunction,
               const octave_value& event_fcn, ColumnVector& te,
               Matrix& ye, ColumnVector& ie, ColumnVector& oldval,
               ColumnVector& oldisterminal, ColumnVector& olddir,
               octave_idx_type& temp, ColumnVector& yold,
               const octave_idx_type num_event_args);

  bool
  outputfun (const octave_value& output_fcn, bool haveoutputsel,
             const ColumnVector& output, realtype tout, realtype tend,
             ColumnVector& outputsel, const std::string& flag);


  bool
  event (const octave_value& event_fcn,
         ColumnVector& te, Matrix& ye, ColumnVector& ie,
         realtype tsol, const ColumnVector& y, const std::string& flag,
         const ColumnVector& yp, ColumnVector& oldval,
         ColumnVector& oldisterminal, ColumnVector& olddir,
         octave_idx_type cont, octave_idx_type& temp, realtype told,
         ColumnVector& yold,
         const octave_idx_type num_event_args);

  void set_maxorder (int maxorder);

  octave_value_list
  integrate (const octave_idx_type numt, const ColumnVector& tt,
             const ColumnVector& y0, const ColumnVector& yp0,
             const int refine, bool haverefine, bool haveoutputfcn,
             const octave_value& output_fcn, bool haveoutputsel,
             ColumnVector& outputsel, bool haveeventfunction,
             const octave_value& event_fcn,
             const octave_idx_type num_event_args);

  void print_stat (void);

private:

  realtype m_t0;
  ColumnVector m_y0;
  ColumnVector m_yp0;
  bool m_havejac;
  bool m_havejacfcn;
  bool m_havejacsparse;
  void *m_mem;
  octave_f77_int_type m_num;
  octave_value m_ida_fcn;
  octave_value m_ida_jac;
  Matrix *m_dfdy;
  Matrix *m_dfdyp;
  SparseMatrix *m_spdfdy;
  SparseMatrix *m_spdfdyp;
  DAERHSFuncIDA m_fcn;
  DAEJacFuncDense m_jacfcn;
  DAEJacFuncSparse m_jacspfcn;
  DAEJacCellDense m_jacdcell;
  DAEJacCellSparse m_jacspcell;
#  if defined (HAVE_SUNDIALS_SUNCONTEXT)
  SUNContext m_sunContext;
#  endif
  SUNMatrix m_sunJacMatrix;
  SUNLinearSolver m_sunLinearSolver;
};

int
IDA::resfun (realtype t, N_Vector yy, N_Vector yyp, N_Vector rr,
             void *user_data)
{
  IDA *self = static_cast <IDA *> (user_data);
  self->resfun_impl (t, yy, yyp, rr);
  return 0;
}

void
IDA::resfun_impl (realtype t, N_Vector& yy,
                  N_Vector& yyp, N_Vector& rr)
{
  ColumnVector y = IDA::NVecToCol (yy, m_num);

  ColumnVector yp = IDA::NVecToCol (yyp, m_num);

  ColumnVector res = (*m_fcn) (y, yp, t, m_ida_fcn);

  realtype *puntrr = nv_data_s (rr);

  for (octave_idx_type i = 0; i < m_num; i++)
    puntrr[i] = res(i);
}

#  if defined (HAVE_SUNDIALS_SUNCONTEXT)
#    define OCTAVE_SUNCONTEXT , m_sunContext
#  else
#    define OCTAVE_SUNCONTEXT
#  endif

void
IDA::set_up (const ColumnVector& y)
{
  N_Vector yy = ColToNVec (y, m_num);

  if (m_havejacsparse)
    {
#  if defined (HAVE_SUNDIALS_SUNLINSOL_KLU)
#    if defined (HAVE_SUNSPARSEMATRIX_REALLOCATE)
      // Initially allocate memory for 0 entries. We will reallocate when we
      // get the Jacobian matrix from the user and know the actual number of
      // entries.
      m_sunJacMatrix = SUNSparseMatrix (m_num, m_num, 0, CSC_MAT
                                        OCTAVE_SUNCONTEXT);
#    else
      octave_f77_int_type max_elems;
      if (math::int_multiply_overflow (m_num, m_num, &max_elems))
        error ("Unable to allocate memory for sparse Jacobian");

      m_sunJacMatrix = SUNSparseMatrix (m_num, m_num, max_elems, CSC_MAT
                                        OCTAVE_SUNCONTEXT);
#    endif
      if (! m_sunJacMatrix)
        error ("Unable to create sparse Jacobian for Sundials");

      m_sunLinearSolver = SUNLinSol_KLU (yy, m_sunJacMatrix
                                         OCTAVE_SUNCONTEXT);
      if (! m_sunLinearSolver)
        error ("Unable to create KLU sparse solver");

      if (IDASetLinearSolver (m_mem, m_sunLinearSolver, m_sunJacMatrix))
        error ("Unable to set sparse linear solver");

      IDASetJacFn (m_mem, IDA::jacsparse);

#  else
      error ("SUNDIALS SUNLINSOL KLU was unavailable or disabled when "
             "Octave was built");

#  endif

    }
  else
    {

      m_sunJacMatrix = SUNDenseMatrix (m_num, m_num OCTAVE_SUNCONTEXT);
      if (! m_sunJacMatrix)
        error ("Unable to create dense Jacobian for Sundials");

      m_sunLinearSolver = SUNLinSol_Dense (yy, m_sunJacMatrix
                                           OCTAVE_SUNCONTEXT);
      if (! m_sunLinearSolver)
        error ("Unable to create dense linear solver");

      if (IDASetLinearSolver (m_mem, m_sunLinearSolver, m_sunJacMatrix))
        error ("Unable to set dense linear solver");

      if (m_havejac && IDASetJacFn (m_mem, IDA::jacdense) != 0)
        error ("Unable to set dense Jacobian function");

    }
}

void
IDA::jacdense_impl (realtype t, realtype cj,
                    N_Vector& yy, N_Vector& yyp, SUNMatrix& JJ)

{
  octave_f77_int_type Neq = NV_LENGTH_S (yy);

  ColumnVector y = NVecToCol (yy, Neq);

  ColumnVector yp = NVecToCol (yyp, Neq);

  Matrix jac;

  if (m_havejacfcn)
    jac = (*m_jacfcn) (y, yp, t, cj, m_ida_jac);
  else
    jac = (*m_jacdcell) (m_dfdy, m_dfdyp, cj);

  octave_f77_int_type num_jac = to_f77_int (jac.numel ());
  std::copy (jac.fortran_vec (),
             jac.fortran_vec () + num_jac,
             SUNDenseMatrix_Data (JJ));
}

#  if defined (HAVE_SUNDIALS_SUNLINSOL_KLU)
void
IDA::jacsparse_impl (realtype t, realtype cj, N_Vector& yy, N_Vector& yyp,
                     SUNMatrix& Jac)

{
  ColumnVector y = NVecToCol (yy, m_num);

  ColumnVector yp = NVecToCol (yyp, m_num);

  SparseMatrix jac;

  if (m_havejacfcn)
    jac = (*m_jacspfcn) (y, yp, t, cj, m_ida_jac);
  else
    jac = (*m_jacspcell) (m_spdfdy, m_spdfdyp, cj);

#     if defined (HAVE_SUNSPARSEMATRIX_REALLOCATE)
  octave_f77_int_type nnz = to_f77_int (jac.nnz ());
  if (nnz > SUNSparseMatrix_NNZ (Jac))
    {
      // Allocate memory for sparse Jacobian defined in user function.
      // This will always be required at least once since we set the number
      // of non-zero elements to zero initially.
      if (SUNSparseMatrix_Reallocate (Jac, nnz))
        error ("Unable to allocate sufficient memory for IDA sparse matrix");
    }
#     endif

  SUNMatZero_Sparse (Jac);
  // We have to use "sunindextype *" here but still need to check that
  // conversion of each element to "octave_f77_int_type" is save.
  sunindextype *colptrs = SUNSparseMatrix_IndexPointers (Jac);
  sunindextype *rowvals = SUNSparseMatrix_IndexValues (Jac);

  for (octave_f77_int_type i = 0; i < m_num + 1; i++)
    colptrs[i] = to_f77_int (jac.cidx (i));

  double *d = SUNSparseMatrix_Data (Jac);
  for (octave_f77_int_type i = 0; i < to_f77_int (jac.nnz ()); i++)
    {
      rowvals[i] = to_f77_int (jac.ridx (i));
      d[i] = jac.data (i);
    }
}
#  endif

ColumnVector
IDA::NVecToCol (N_Vector& v, octave_f77_int_type n)
{
  ColumnVector data (n);
  realtype *punt = nv_data_s (v);

  for (octave_f77_int_type i = 0; i < n; i++)
    data(i) = punt[i];

  return data;
}

N_Vector
IDA::ColToNVec (const ColumnVector& data, octave_f77_int_type n)
{
  N_Vector v = N_VNew_Serial (n OCTAVE_SUNCONTEXT);

  realtype *punt = nv_data_s (v);

  for (octave_f77_int_type i = 0; i < n; i++)
    punt[i] = data(i);

  return v;
}

void
IDA::set_userdata (void)
{
  void *userdata = this;

  if (IDASetUserData (m_mem, userdata) != 0)
    error ("User data not set");
}

void
IDA::initialize (void)
{
  m_num = to_f77_int (m_y0.numel ());
#  if defined (HAVE_SUNDIALS_SUNCONTEXT)
  if (SUNContext_Create (nullptr, &m_sunContext) < 0)
    error ("__ode15__: unable to create context for SUNDIALS");
  m_mem = IDACreate (m_sunContext);
#  else
  m_mem = IDACreate ();
#  endif

  N_Vector yy = ColToNVec (m_y0, m_num);

  N_Vector yyp = ColToNVec (m_yp0, m_num);

  IDA::set_userdata ();

  if (IDAInit (m_mem, IDA::resfun, m_t0, yy, yyp) != 0)
    error ("IDA not initialized");
}

void
IDA::set_tolerance (ColumnVector& abstol, realtype reltol)
{
  N_Vector abs_tol = ColToNVec (abstol, m_num);

  if (IDASVtolerances (m_mem, reltol, abs_tol) != 0)
    error ("IDA: Tolerance not set");

  N_VDestroy_Serial (abs_tol);
}

void
IDA::set_tolerance (realtype abstol, realtype reltol)
{
  if (IDASStolerances (m_mem, reltol, abstol) != 0)
    error ("IDA: Tolerance not set");
}

octave_value_list
IDA::integrate (const octave_idx_type numt, const ColumnVector& tspan,
                const ColumnVector& y, const ColumnVector& yp,
                const int refine, bool haverefine, bool haveoutputfcn,
                const octave_value& output_fcn, bool haveoutputsel,
                ColumnVector& outputsel, bool haveeventfunction,
                const octave_value& event_fcn,
                const octave_idx_type num_event_args)
{
  // Set up output
  ColumnVector tout, yout (m_num), ypout (m_num), ysel (outputsel.numel ());
  ColumnVector ie, te, oldval, oldisterminal, olddir;
  Matrix output, ye;
  octave_idx_type cont = 0, temp = 0;
  bool status = false;
  std::string string = "";
  ColumnVector yold = y;

  realtype tsol = tspan(0);
  realtype tend = tspan(numt-1);

  N_Vector yyp = ColToNVec (yp, m_num);

  N_Vector yy = ColToNVec (y, m_num);

  // Initialize OutputFcn
  if (haveoutputfcn)
    status = IDA::outputfun (output_fcn, haveoutputsel, y,
                             tsol, tend, outputsel, "init");

  // Initialize Events
  if (haveeventfunction)
    status = IDA::event (event_fcn, te, ye, ie, tsol, y,
                         "init", yp, oldval, oldisterminal,
                         olddir, cont, temp, tsol, yold, num_event_args);

  if (numt > 2)
    {
      // First output value
      tout.resize (numt);
      tout(0) = tsol;
      output.resize (numt, m_num);

      for (octave_idx_type i = 0; i < m_num; i++)
        output.elem (0, i) = y.elem (i);

      //Main loop
      for (octave_idx_type j = 1; j < numt && status == 0; j++)
        {
          // IDANORMAL already interpolates tspan(j)

          if (IDASolve (m_mem, tspan (j), &tsol, yy, yyp, IDA_NORMAL) != 0)
            error ("IDASolve failed");

          yout = NVecToCol (yy, m_num);
          ypout = NVecToCol (yyp, m_num);
          tout(j) = tsol;

          for (octave_idx_type i = 0; i < m_num; i++)
            output.elem (j, i) = yout.elem (i);

          if (haveoutputfcn)
            status = IDA::outputfun (output_fcn, haveoutputsel, yout, tsol,
                                     tend, outputsel, string);

          if (haveeventfunction)
            status = IDA::event (event_fcn, te, ye, ie, tout(j), yout,
                                 string, ypout, oldval, oldisterminal,
                                 olddir, j, temp, tout(j-1), yold,
                                 num_event_args);

          // If integration is stopped, return only the reached steps
          if (status == 1)
            {
              output.resize (j + 1, m_num);
              tout.resize (j + 1);
            }

        }
    }
  else // numel (tspan) == 2
    {
      // First output value
      tout.resize (1);
      tout(0) = tsol;
      output.resize (1, m_num);

      for (octave_idx_type i = 0; i < m_num; i++)
        output.elem (0, i) = y.elem (i);

      bool posdirection = (tend > tsol);

      //main loop
      while (((posdirection == 1 && tsol < tend)
              || (posdirection == 0 && tsol > tend))
             && status == 0)
        {
          if (IDASolve (m_mem, tend, &tsol, yy, yyp, IDA_ONE_STEP) != 0)
            error ("IDASolve failed");

          if (haverefine)
            status = IDA::interpolate (cont, output, tout, refine, tend,
                                       haveoutputfcn, haveoutputsel,
                                       output_fcn, outputsel,
                                       haveeventfunction, event_fcn, te,
                                       ye, ie, oldval, oldisterminal,
                                       olddir, temp, yold,
                                       num_event_args);

          ypout = NVecToCol (yyp, m_num);
          cont += 1;
          output.resize (cont + 1, m_num); // This may be not efficient
          tout.resize (cont + 1);
          tout(cont) = tsol;
          yout = NVecToCol (yy, m_num);

          for (octave_idx_type i = 0; i < m_num; i++)
            output.elem (cont, i) = yout.elem (i);

          if (haveoutputfcn && ! haverefine && tout(cont) < tend)
            status = IDA::outputfun (output_fcn, haveoutputsel, yout, tsol,
                                     tend, outputsel, string);

          if (haveeventfunction && ! haverefine && tout(cont) < tend)
            status = IDA::event (event_fcn, te, ye, ie, tout(cont), yout,
                                 string, ypout, oldval, oldisterminal,
                                 olddir, cont, temp, tout(cont-1), yold,
                                 num_event_args);
        }

      if (status == 0)
        {
          // Interpolate in tend
          N_Vector dky = N_VNew_Serial (m_num OCTAVE_SUNCONTEXT);

          if (IDAGetDky (m_mem, tend, 0, dky) != 0)
            error ("IDA failed to interpolate y");

          tout(cont) = tend;
          yout = NVecToCol (dky, m_num);

          for (octave_idx_type i = 0; i < m_num; i++)
            output.elem (cont, i) = yout.elem (i);

          // Plot final value
          if (haveoutputfcn)
            {
              status = IDA::outputfun (output_fcn, haveoutputsel, yout,
                                       tend, tend, outputsel, string);

              // Events during last step
              if (haveeventfunction)
                status = IDA::event (event_fcn, te, ye, ie, tend, yout,
                                     string, ypout, oldval, oldisterminal,
                                     olddir, cont, temp, tout(cont-1),
                                     yold, num_event_args);
            }

          N_VDestroy_Serial (dky);
        }

      // Cleanup plotter
      status = IDA::outputfun (output_fcn, haveoutputsel, yout, tend, tend,
                               outputsel, "done");

    }

  // Index of Events (ie) variable must use 1-based indexing
  return ovl (tout, output, te, ye, ie + 1.0);
}

bool
IDA::event (const octave_value& event_fcn,
            ColumnVector& te, Matrix& ye, ColumnVector& ie, realtype tsol,
            const ColumnVector& y, const std::string& flag,
            const ColumnVector& yp, ColumnVector& oldval,
            ColumnVector& oldisterminal, ColumnVector& olddir,
            octave_idx_type cont, octave_idx_type& temp, realtype told,
            ColumnVector& yold,
            const octave_idx_type num_event_args)
{
  bool status = false;

  octave_value_list args;
  if (num_event_args == 2)
    args = ovl (tsol, y);
  else
    args = ovl (tsol, y, yp);

  // cont is the number of steps reached by the solver
  // temp is the number of events registered

  if (flag == "init")
    {
      octave_value_list output = feval (event_fcn, args, 3);
      oldval = output(0).vector_value ();
      oldisterminal = output(1).vector_value ();
      olddir = output(2).vector_value ();
    }
  else if (flag == "")
    {
      ColumnVector index (0);
      octave_value_list output = feval (event_fcn, args, 3);
      ColumnVector val = output(0).vector_value ();
      ColumnVector isterminal = output(1).vector_value ();
      ColumnVector dir = output(2).vector_value ();

      // Get the index of the changed values
      for (octave_idx_type i = 0; i < val.numel (); i++)
        {
          // Check for sign change and whether a rising / falling edge
          // either passes through zero or detaches from zero (bug #59063)
          if ((dir(i) != -1
               && ((val(i) >= 0 && oldval(i) < 0)
                   || (val(i) > 0 && oldval(i) <= 0))) // increasing
              || (dir(i) != 1
                  && ((val(i) <= 0 && oldval(i) > 0)
                      || (val(i) < 0 && oldval(i) >= 0)))) // decreasing
            {
              index.resize (index.numel () + 1);
              index (index.numel () - 1) = i;
            }
        }

      if (cont == 1 && index.numel () > 0)  // Events in first step
        {
          temp = 1; // register only the first event
          te.resize (1);
          ye.resize (1, m_num);
          ie.resize (1);

          // Linear interpolation
          ie(0) = index(0);
          te(0) = tsol - val (index(0)) * (tsol - told)
                  / (val (index(0)) - oldval (index(0)));

          ColumnVector ytemp
            = y - ((tsol - te(0)) * (y - yold) / (tsol - told));

          for (octave_idx_type i = 0; i < m_num; i++)
            ye.elem (0, i) = ytemp.elem (i);

        }
      else if (index.numel () > 0)
        // Not first step: register all events and test
        // if stop integration or not
        {
          te.resize (temp + index.numel ());
          ye.resize (temp + index.numel (), m_num);
          ie.resize (temp + index.numel ());

          for (octave_idx_type i = 0; i < index.numel (); i++)
            {

              if (isterminal (index(i)) == 1)
                status = 1; // Stop integration

              // Linear interpolation
              ie(temp+i) = index(i);
              te(temp+i) = tsol - val(index(i)) * (tsol - told)
                           / (val(index(i)) - oldval(index(i)));

              ColumnVector ytemp
                = y - (tsol - te (temp + i)) * (y - yold) / (tsol - told);

              for (octave_idx_type j = 0; j < m_num; j++)
                ye.elem (temp + i, j) = ytemp.elem (j);

            }

          temp += index.numel ();
        }

      // Update variables
      yold = y;
      told = tsol;
      olddir = dir;
      oldval = val;
      oldisterminal = isterminal;
    }

  return status;
}

bool
IDA::interpolate (octave_idx_type& cont, Matrix& output, ColumnVector& tout,
                  int refine, realtype tend, bool haveoutputfcn,
                  bool haveoutputsel, const octave_value& output_fcn,
                  ColumnVector& outputsel, bool haveeventfunction,
                  const octave_value& event_fcn, ColumnVector& te,
                  Matrix& ye, ColumnVector& ie, ColumnVector& oldval,
                  ColumnVector& oldisterminal, ColumnVector& olddir,
                  octave_idx_type& temp, ColumnVector& yold,
                  const octave_idx_type num_event_args)
{
  realtype h = 0, tcur = 0;
  bool status = false;

  N_Vector dky = N_VNew_Serial (m_num OCTAVE_SUNCONTEXT);

  N_Vector dkyp = N_VNew_Serial (m_num OCTAVE_SUNCONTEXT);

  ColumnVector yout (m_num);
  ColumnVector ypout (m_num);
  std::string string = "";

  if (IDAGetLastStep (m_mem, &h) != 0)
    error ("IDA failed to return last step");

  if (IDAGetCurrentTime (m_mem, &tcur) != 0)
    error ("IDA failed to return the current time");

  realtype tin = tcur - h;

  realtype step = h / refine;

  for (octave_idx_type i = 1;
       i < refine && tin + step * i < tend && status == 0;
       i++)
    {
      if (IDAGetDky (m_mem, tin + step*i, 0, dky) != 0)
        error ("IDA failed to interpolate y");

      if (IDAGetDky (m_mem, tin + step*i, 1, dkyp) != 0)
        error ("IDA failed to interpolate yp");

      cont += 1;
      output.resize (cont + 1, m_num);
      tout.resize (cont + 1);

      tout(cont) = tin + step * i;
      yout = NVecToCol (dky, m_num);
      ypout = NVecToCol (dkyp, m_num);

      for (octave_idx_type j = 0; j < m_num; j++)
        output.elem (cont, j) = yout.elem (j);

      if (haveoutputfcn)
        status = IDA::outputfun (output_fcn, haveoutputsel, yout,
                                 tout(cont), tend, outputsel, "");

      if (haveeventfunction)
        status = IDA::event (event_fcn, te, ye, ie, tout(cont),
                             yout, string, ypout, oldval,
                             oldisterminal, olddir, cont, temp,
                             tout(cont-1), yold, num_event_args);
    }

  N_VDestroy_Serial (dky);

  return status;
}

bool
IDA::outputfun (const octave_value& output_fcn, bool haveoutputsel,
                const ColumnVector& yout, realtype tsol,
                realtype tend, ColumnVector& outputsel,
                const std::string& flag)
{
  bool status = false;

  octave_value_list output;
  output(2) = flag;

  ColumnVector ysel (outputsel.numel ());
  if (haveoutputsel)
    {
      for (octave_idx_type i = 0; i < outputsel.numel (); i++)
        ysel(i) = yout(outputsel(i));

      output(1) = ysel;
    }
  else
    output(1) = yout;

  if (flag == "init")
    {
      ColumnVector toutput (2);
      toutput(0) = tsol;
      toutput(1) = tend;
      output(0) = toutput;

      feval (output_fcn, output, 0);
    }
  else if (flag == "")
    {
      output(0) = tsol;
      octave_value_list val = feval (output_fcn, output, 1);
      status = val(0).bool_value ();
    }
  else
    {
      // Cleanup plotter
      output(0) = tend;
      feval (output_fcn, output, 0);
    }

  return status;
}

void
IDA::set_maxstep (realtype maxstep)
{
  if (IDASetMaxStep (m_mem, maxstep) != 0)
    error ("IDA: Max Step not set");
}

void
IDA::set_initialstep (realtype initialstep)
{
  if (IDASetInitStep (m_mem, initialstep) != 0)
    error ("IDA: Initial Step not set");
}

void
IDA::set_maxorder (int maxorder)
{
  if (IDASetMaxOrd (m_mem, maxorder) != 0)
    error ("IDA: Max Order not set");
}

void
IDA::print_stat (void)
{
  long int nsteps = 0, netfails = 0, nrevals = 0;

  if (IDAGetNumSteps (m_mem, &nsteps) != 0)
    error ("IDA failed to return the number of internal steps");

  if (IDAGetNumErrTestFails (m_mem, &netfails) != 0)
    error ("IDA failed to return the number of internal errors");

  if (IDAGetNumResEvals (m_mem, &nrevals) != 0)
    error ("IDA failed to return the number of residual evaluations");

  octave_stdout << nsteps << " successful steps\n";
  octave_stdout << netfails << " failed attempts\n";
  octave_stdout << nrevals << " function evaluations\n";
  // octave_stdout << " partial derivatives\n";
  // octave_stdout << " LU decompositions\n";
  // octave_stdout << " solutions of linear systems\n";
}

static ColumnVector
ida_user_function (const ColumnVector& x, const ColumnVector& xdot,
                   double t, const octave_value& ida_fc)
{
  octave_value_list tmp;

  try
    {
      tmp = feval (ida_fc, ovl (t, x, xdot), 1);
    }
  catch (execution_exception& ee)
    {
      err_user_supplied_eval (ee, "__ode15__");
    }

  return tmp(0).vector_value ();
}

static Matrix
ida_dense_jac (const ColumnVector& x, const ColumnVector& xdot,
               double t, double cj, const octave_value& ida_jc)
{
  octave_value_list tmp;

  try
    {
      tmp = feval (ida_jc, ovl (t, x, xdot), 2);
    }
  catch (execution_exception& ee)
    {
      err_user_supplied_eval (ee, "__ode15__");
    }

  return tmp(0).matrix_value () + cj * tmp(1).matrix_value ();
}

static SparseMatrix
ida_sparse_jac (const ColumnVector& x, const ColumnVector& xdot,
                double t, double cj, const octave_value& ida_jc)
{
  octave_value_list tmp;

  try
    {
      tmp = feval (ida_jc, ovl (t, x, xdot), 2);
    }
  catch (execution_exception& ee)
    {
      err_user_supplied_eval (ee, "__ode15__");
    }

  return tmp(0).sparse_matrix_value () + cj * tmp(1).sparse_matrix_value ();
}

static Matrix
ida_dense_cell_jac (Matrix *dfdy, Matrix *dfdyp, double cj)
{
  return (*dfdy) + cj * (*dfdyp);
}

static SparseMatrix
ida_sparse_cell_jac (SparseMatrix *spdfdy, SparseMatrix *spdfdyp,
                     double cj)
{
  return (*spdfdy) + cj * (*spdfdyp);
}

static octave_value_list
do_ode15 (const octave_value& ida_fcn,
          const ColumnVector& tspan,
          const octave_idx_type numt,
          const realtype t0,
          const ColumnVector& y0,
          const ColumnVector& yp0,
          const octave_scalar_map& options,
          const octave_idx_type num_event_args)
{
  octave_value_list retval;

  // Create object
  IDA dae (t0, y0, yp0, ida_fcn, ida_user_function);

  // Set Jacobian
  bool havejac = options.getfield ("havejac").bool_value ();

  bool havejacsparse = options.getfield ("havejacsparse").bool_value ();

  bool havejacfcn = options.getfield ("havejacfcn").bool_value ();

  Matrix ida_dfdy, ida_dfdyp;
  SparseMatrix ida_spdfdy, ida_spdfdyp;

  if (havejac)
    {
      if (havejacfcn)
        {
          octave_value ida_jac = options.getfield ("Jacobian");

          if (havejacsparse)
            dae.set_jacobian (ida_jac, ida_sparse_jac);
          else
            dae.set_jacobian (ida_jac, ida_dense_jac);
        }
      else
        {
          Cell jaccell = options.getfield ("Jacobian").cell_value ();

          if (havejacsparse)
            {
              ida_spdfdy = jaccell(0).sparse_matrix_value ();
              ida_spdfdyp = jaccell(1).sparse_matrix_value ();

              dae.set_jacobian (&ida_spdfdy, &ida_spdfdyp,
                                ida_sparse_cell_jac);
            }
          else
            {
              ida_dfdy = jaccell(0).matrix_value ();
              ida_dfdyp = jaccell(1).matrix_value ();

              dae.set_jacobian (&ida_dfdy, &ida_dfdyp, ida_dense_cell_jac);
            }
        }
    }

  // Initialize IDA
  dae.initialize ();

  // Set tolerances
  realtype rel_tol = options.getfield ("RelTol").double_value ();

  bool haveabstolvec = options.getfield ("haveabstolvec").bool_value ();

  if (haveabstolvec)
    {
      ColumnVector abs_tol = options.getfield ("AbsTol").vector_value ();

      dae.set_tolerance (abs_tol, rel_tol);
    }
  else
    {
      realtype abs_tol = options.getfield ("AbsTol").double_value ();

      dae.set_tolerance (abs_tol, rel_tol);
    }

  //Set max step
  realtype maxstep = options.getfield ("MaxStep").double_value ();

  dae.set_maxstep (maxstep);

  //Set initial step
  if (! options.getfield ("InitialStep").isempty ())
    {
      realtype initialstep = options.getfield ("InitialStep").double_value ();

      dae.set_initialstep (initialstep);
    }

  //Set max order FIXME: it doesn't work
  int maxorder = options.getfield ("MaxOrder").int_value ();

  dae.set_maxorder (maxorder);

  //Set Refine
  const int refine = options.getfield ("Refine").int_value ();

  bool haverefine = (refine > 1);

  octave_value output_fcn;
  ColumnVector outputsel;

  // OutputFcn
  bool haveoutputfunction
    = options.getfield ("haveoutputfunction").bool_value ();

  if (haveoutputfunction)
    output_fcn = options.getfield ("OutputFcn");

  // OutputSel
  bool haveoutputsel = options.getfield ("haveoutputselection").bool_value ();

  if (haveoutputsel)
    outputsel = options.getfield ("OutputSel").vector_value ();

  octave_value event_fcn;

  // Events
  bool haveeventfunction
    = options.getfield ("haveeventfunction").bool_value ();

  if (haveeventfunction)
    event_fcn = options.getfield ("Events");

  // Set up linear solver
  dae.set_up (y0);

  // Integrate
  retval = dae.integrate (numt, tspan, y0, yp0, refine,
                          haverefine, haveoutputfunction,
                          output_fcn, haveoutputsel, outputsel,
                          haveeventfunction, event_fcn, num_event_args);

  // Statistics
  bool havestats = options.getfield ("havestats").bool_value ();

  if (havestats)
    dae.print_stat ();

  return retval;
}

#endif

DEFUN_DLD (__ode15__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{t}, @var{y} =} __ode15__ (@var{fcn}, @var{tspan}, @var{y0}, @var{yp0}, @var{ode_opt}, @var{num_event_args})
Undocumented internal function.
@end deftypefn */)
{

#if defined (HAVE_SUNDIALS)

  // Check number of parameters
  if (args.length () != 6)
    print_usage ();

  // Check ODE function
  octave_value ida_fcn = args(0);

  if (! ida_fcn.is_function_handle ())
    error ("__ode15__: FCN must be a function handle");

  // Check input tspan
  ColumnVector tspan
    = args(1).xvector_value ("__ode15__: TRANGE must be a vector of numbers");

  octave_idx_type numt = tspan.numel ();

  realtype t0 = tspan(0);

  if (numt < 2)
    error ("__ode15__: TRANGE must contain at least 2 elements");
  else if (tspan.issorted () == UNSORTED || tspan(0) == tspan(numt - 1))
    error ("__ode15__: TRANGE must be strictly monotonic");

  // input y0 and yp0
  ColumnVector y0
    = args(2).xvector_value ("__ode15__: initial state Y0 must be a vector");

  ColumnVector yp0
    = args(3).xvector_value ("__ode15__: initial state YP0 must be a vector");


  if (y0.numel () != yp0.numel ())
    error ("__ode15__: initial state Y0 and YP0 must have the same length");
  else if (y0.numel () < 1)
    error ("__ode15__: initial state YP0 must be a vector or a scalar");


  if (! args(4).isstruct ())
    error ("__ode15__: ODE_OPT argument must be a structure");

  octave_scalar_map options
    = args(4).xscalar_map_value ("__ode15__: ODE_OPT argument must be a scalar structure");

  // Provided number of arguments in the ode callback function
  octave_idx_type num_event_args
    = args(5).xidx_type_value ("__ode15__: NUM_EVENT_ARGS must be an integer");

  if (num_event_args != 2 && num_event_args != 3)
    error ("__ode15__: number of input arguments in event callback must be 2 or 3");

  return do_ode15 (ida_fcn, tspan, numt, t0, y0, yp0, options, num_event_args);

#else

  octave_unused_parameter (args);

  err_disabled_feature ("__ode15__", "sundials_ida, sundials_nvecserial");

#endif
}

/*
## No test needed for internal helper function.
%!assert (1)
*/

OCTAVE_END_NAMESPACE(octave)
