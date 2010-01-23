/*

Copyright (C) 2005, 2006, 2007, 2008 Nicolo' Giorgetti

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
#include <config.h>
#endif

#include <cfloat>
#include <csetjmp>
#include <ctime>

#include "lo-ieee.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "pager.h"

#if defined (HAVE_GLPK)

extern "C"
{
#if defined (HAVE_GLPK_GLPK_H)
#include <glpk/glpk.h>
#else
#include <glpk.h>
#endif

#if 0
#ifdef GLPK_PRE_4_14

#ifndef _GLPLIB_H
#include <glplib.h>
#endif
#ifndef lib_set_fault_hook
#define lib_set_fault_hook lib_fault_hook
#endif
#ifndef lib_set_print_hook
#define lib_set_print_hook lib_print_hook
#endif

#else

void _glp_lib_print_hook (int (*func)(void *info, char *buf), void *info);
void _glp_lib_fault_hook (int (*func)(void *info, char *buf), void *info);

#endif
#endif
}

#define NIntP 17
#define NRealP 10

int lpxIntParam[NIntP] = {
  0,
  1,
  0,
  1,
  0,
  -1,
  0,
  200,
  1,
  2,
  0,
  1,
  0,
  0,
  2,
  2,
  1
};

int IParam[NIntP] = {
  LPX_K_MSGLEV,
  LPX_K_SCALE,
  LPX_K_DUAL,
  LPX_K_PRICE,
  LPX_K_ROUND,
  LPX_K_ITLIM,
  LPX_K_ITCNT,
  LPX_K_OUTFRQ,
  LPX_K_MPSINFO,
  LPX_K_MPSOBJ,
  LPX_K_MPSORIG,
  LPX_K_MPSWIDE,
  LPX_K_MPSFREE,
  LPX_K_MPSSKIP,
  LPX_K_BRANCH,
  LPX_K_BTRACK,
  LPX_K_PRESOL
};


double lpxRealParam[NRealP] = {
  0.07,
  1e-7,
  1e-7,
  1e-9,
  -DBL_MAX,
  DBL_MAX,
  -1.0,
  0.0,
  1e-6,
  1e-7
};

int RParam[NRealP] = {
  LPX_K_RELAX,
  LPX_K_TOLBND,
  LPX_K_TOLDJ,
  LPX_K_TOLPIV,
  LPX_K_OBJLL,
  LPX_K_OBJUL,
  LPX_K_TMLIM,
  LPX_K_OUTDLY,
  LPX_K_TOLINT,
  LPX_K_TOLOBJ
};

static jmp_buf mark;  //-- Address for long jump to jump to

#if 0
int
glpk_fault_hook (void * /* info */, char *msg)
{
  error ("CRITICAL ERROR in GLPK: %s", msg);
  longjmp (mark, -1);
}

int
glpk_print_hook (void * /* info */, char *msg)
{
  message (0, "%s", msg);
  return 1;
}
#endif

int
glpk (int sense, int n, int m, double *c, int nz, int *rn, int *cn,
      double *a, double *b, char *ctype, int *freeLB, double *lb,
      int *freeUB, double *ub, int *vartype, int isMIP, int lpsolver,
      int save_pb, double *xmin, double *fmin, double *status,
      double *lambda, double *redcosts, double *time, double *mem)
{
  int errnum;
  int typx = 0;
  int method;

  clock_t t_start = clock();

#if 0
#ifdef GLPK_PRE_4_14
  lib_set_fault_hook (0, glpk_fault_hook);
#else
  _glp_lib_fault_hook (glpk_fault_hook, 0);
#endif

  if (lpxIntParam[0] > 1)
#ifdef GLPK_PRE_4_14
    lib_set_print_hook (0, glpk_print_hook);
#else
    _glp_lib_print_hook (glpk_print_hook, 0);
#endif
#endif

  LPX *lp = lpx_create_prob ();


  //-- Set the sense of optimization
  if (sense == 1)
    lpx_set_obj_dir (lp, LPX_MIN);
  else
    lpx_set_obj_dir (lp, LPX_MAX);

  //-- If the problem has integer structural variables switch to MIP
  if (isMIP)
    lpx_set_class (lp, LPX_MIP);

  lpx_add_cols (lp, n);
  for (int i = 0; i < n; i++)
    {
      //-- Define type of the structural variables
      if (! freeLB[i] && ! freeUB[i])
        {
          if (lb[i] != ub[i])
            lpx_set_col_bnds (lp, i+1, LPX_DB, lb[i], ub[i]);
          else
            lpx_set_col_bnds (lp, i+1, LPX_FX, lb[i], ub[i]);
        }
      else
        {
          if (! freeLB[i] && freeUB[i])
            lpx_set_col_bnds (lp, i+1, LPX_LO, lb[i], ub[i]);
          else
            {
              if (freeLB[i] && ! freeUB[i])
                lpx_set_col_bnds (lp, i+1, LPX_UP, lb[i], ub[i]);
              else
                lpx_set_col_bnds (lp, i+1, LPX_FR, lb[i], ub[i]);
            }
        }

      // -- Set the objective coefficient of the corresponding
      // -- structural variable. No constant term is assumed.
      lpx_set_obj_coef(lp,i+1,c[i]);

      if (isMIP)
        lpx_set_col_kind (lp, i+1, vartype[i]);
    }

  lpx_add_rows (lp, m);

  for (int i = 0; i < m; i++)
    {
      /* If the i-th row has no lower bound (types F,U), the
         corrispondent parameter will be ignored.
         If the i-th row has no upper bound (types F,L), the corrispondent
         parameter will be ignored.
         If the i-th row is of S type, the i-th LB is used, but
         the i-th UB is ignored.
      */

      switch (ctype[i])
        {
        case 'F':
          typx = LPX_FR;
          break;

        case 'U':
          typx = LPX_UP;
          break;

        case 'L':
          typx = LPX_LO;
          break;

        case 'S':
          typx = LPX_FX;
          break;

        case 'D':
          typx = LPX_DB;
          break;
        }
      
      lpx_set_row_bnds (lp, i+1, typx, b[i], b[i]);

    }

  lpx_load_matrix (lp, nz, rn, cn, a);

  if (save_pb)
    {
      static char tmp[] = "outpb.lp";
      if (lpx_write_cpxlp (lp, tmp) != 0)
        {
          error ("__glpk__: unable to write problem");
          longjmp (mark, -1);
        }
    }

  //-- scale the problem data (if required)
  //-- if (scale && (!presol || method == 1)) lpx_scale_prob(lp);
  //-- LPX_K_SCALE=IParam[1]  LPX_K_PRESOL=IParam[16]
  if (lpxIntParam[1] && (! lpxIntParam[16] || lpsolver != 1))
    lpx_scale_prob (lp);

  //-- build advanced initial basis (if required)
  if (lpsolver == 1 && ! lpxIntParam[16])
    lpx_adv_basis (lp);

  for(int i = 0; i < NIntP; i++)
    lpx_set_int_parm (lp, IParam[i], lpxIntParam[i]);

  for (int i = 0; i < NRealP; i++)
    lpx_set_real_parm (lp, RParam[i], lpxRealParam[i]);

  if (lpsolver == 1)
    method = 'S';
  else
    method = 'T';

  switch (method)
    {
    case 'S':
      {
        if (isMIP)
          {
            method = 'I';
            errnum = lpx_simplex (lp);
            errnum = lpx_integer (lp);
          }
        else
          errnum = lpx_simplex(lp);
      }
     break;

    case 'T':
      errnum = lpx_interior(lp);
      break;

    default:
      break;
#if 0
#ifdef GLPK_PRE_4_14
      insist (method != method);
#else
      static char tmp[] = "method != method";
      glpk_fault_hook (0, tmp);
#endif
#endif
    }

  /*  errnum assumes the following results:
      errnum = 0 <=> No errors
      errnum = 1 <=> Iteration limit exceeded.
      errnum = 2 <=> Numerical problems with basis matrix.
  */
  if (errnum == LPX_E_OK)
    {
      if (isMIP)
        {
          *status = lpx_mip_status (lp);
          *fmin = lpx_mip_obj_val (lp);
        }
      else
        {
          if (lpsolver == 1)
            {
              *status = lpx_get_status (lp);
              *fmin = lpx_get_obj_val (lp);
            }
          else
            {
              *status = lpx_ipt_status (lp);
              *fmin = lpx_ipt_obj_val (lp);
            }
        }

      if (isMIP)
        {
          for (int i = 0; i < n; i++)
            xmin[i] = lpx_mip_col_val (lp, i+1);
        }
      else
        {
          /* Primal values */
          for (int i = 0; i < n; i++)
            {
              if (lpsolver == 1)
                xmin[i] = lpx_get_col_prim (lp, i+1);
              else
                xmin[i] = lpx_ipt_col_prim (lp, i+1);
            }

          /* Dual values */
          for (int i = 0; i < m; i++)
            {
              if (lpsolver == 1)
                lambda[i] = lpx_get_row_dual (lp, i+1);
              else
                lambda[i] = lpx_ipt_row_dual (lp, i+1);
            }

          /* Reduced costs */
          for (int i = 0; i < lpx_get_num_cols (lp); i++)
            {
              if (lpsolver == 1)
                redcosts[i] = lpx_get_col_dual (lp, i+1);
              else
                redcosts[i] = lpx_ipt_col_dual (lp, i+1);
            }
        }

      *time = (clock () - t_start) / CLOCKS_PER_SEC;

#ifdef GLPK_PRE_4_14
      *mem = (lib_env_ptr () -> mem_tpeak);
#else
      *mem = 0;
#endif

      lpx_delete_prob (lp);
      return 0;
    }

   lpx_delete_prob (lp);

   *status = errnum;

   return errnum;
}

#endif

#define OCTAVE_GLPK_GET_REAL_PARAM(NAME, IDX) \
  do \
    { \
      if (PARAM.contains (NAME)) \
        { \
          Cell tmp = PARAM.contents (NAME); \
 \
          if (! tmp.is_empty ()) \
            { \
              lpxRealParam[IDX] = tmp(0).scalar_value (); \
 \
              if (error_state) \
                { \
                  error ("glpk: invalid value in param." NAME); \
                  return retval; \
                } \
            } \
          else \
            { \
              error ("glpk: invalid value in param." NAME); \
              return retval; \
            } \
        } \
    } \
  while (0)

#define OCTAVE_GLPK_GET_INT_PARAM(NAME, VAL) \
  do \
    { \
      if (PARAM.contains (NAME)) \
        { \
          Cell tmp = PARAM.contents (NAME); \
 \
          if (! tmp.is_empty ()) \
            { \
              VAL = tmp(0).int_value (); \
 \
              if (error_state) \
                { \
                  error ("glpk: invalid value in param." NAME); \
                  return retval; \
                } \
            } \
          else \
            { \
              error ("glpk: invalid value in param." NAME); \
              return retval; \
            } \
        } \
    } \
  while (0)

DEFUN_DLD (__glpk__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{values}] =} __glpk__ (@var{args})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  // The list of values to return.  See the declaration in oct-obj.h
  octave_value_list retval;

#if defined (HAVE_GLPK)

  int nrhs = args.length ();

  if (nrhs != 9)
    {
      print_usage ();
      return retval;
    }

  //-- 1nd Input. A column array containing the objective function
  //--            coefficients.
  volatile int mrowsc = args(0).rows();

  Matrix C (args(0).matrix_value ());

  if (error_state)
    {
      error ("__glpk__: invalid value of C");
      return retval;
    }

  double *c = C.fortran_vec ();
  Array<int> rn;
  Array<int> cn;
  ColumnVector a;
  volatile int mrowsA;
  volatile int nz = 0;

  //-- 2nd Input. A matrix containing the constraints coefficients.
  // If matrix A is NOT a sparse matrix
  if (args(1).is_sparse_type ())
    {
      SparseMatrix A = args(1).sparse_matrix_value (); // get the sparse matrix

      if (error_state)
        {
          error ("__glpk__: invalid value of A");
          return retval;
        }

      mrowsA = A.rows ();
      octave_idx_type Anc = A.cols ();
      octave_idx_type Anz = A.nzmax ();
      rn.resize (Anz+1, 1);
      cn.resize (Anz+1, 1);
      a.resize (Anz+1, 0.0);

      if (Anc != mrowsc)
        {
          error ("__glpk__: invalid value of A");
          return retval;
        }

      for (octave_idx_type j = 0; j < Anc; j++)
        for (octave_idx_type i = A.cidx(j); i < A.cidx(j+1); i++)
          {
            nz++;
            rn(nz) = A.ridx(i) + 1;
            cn(nz) = j + 1;
            a(nz) = A.data(i);
          }
    }
  else
    {
      Matrix A (args(1).matrix_value ()); // get the matrix

      if (error_state)
        {
          error ("__glpk__: invalid value of A");
          return retval;
        }

      mrowsA = A.rows ();
      rn.resize (mrowsA*mrowsc+1, 1);
      cn.resize (mrowsA*mrowsc+1, 1);
      a.resize (mrowsA*mrowsc+1, 0.0);

      for (int i = 0; i < mrowsA; i++)
        {
          for (int j = 0; j < mrowsc; j++)
            {
              if (A(i,j) != 0)
                {
                  nz++;
                  rn(nz) = i + 1;
                  cn(nz) = j + 1;
                  a(nz) = A(i,j);
                }
            }
        }

    }

  //-- 3rd Input. A column array containing the right-hand side value
  //               for each constraint in the constraint matrix.
  Matrix B (args(2).matrix_value ());

  if (error_state)
    {
      error ("__glpk__: invalid value of b");
      return retval;
    }

  double *b = B.fortran_vec ();

  //-- 4th Input. An array of length mrowsc containing the lower
  //--            bound on each of the variables.
  Matrix LB (args(3).matrix_value ());

  if (error_state || LB.length () < mrowsc)
    {
      error ("__glpk__: invalid value of lb");
      return retval;
    }

  double *lb = LB.fortran_vec ();

  //-- LB argument, default: Free
  Array<int> freeLB (mrowsc, 1);
  for (int i = 0; i < mrowsc; i++)
     {
       if (xisinf (lb[i]))
         {
           freeLB(i) = 1;
           lb[i] = -octave_Inf;
         }
       else
         freeLB(i) = 0;
     }

  //-- 5th Input. An array of at least length numcols containing the upper
  //--            bound on each of the variables.
  Matrix UB (args(4).matrix_value ());

  if (error_state || UB.length () < mrowsc)
    {
      error ("__glpk__: invalid value of ub");
      return retval;
    }

  double *ub = UB.fortran_vec ();

  Array<int> freeUB (mrowsc, 1);
  for (int i = 0; i < mrowsc; i++)
    {
      if (xisinf (ub[i]))
        {
          freeUB(i) = 1;
          ub[i] = octave_Inf;
        }
      else
        freeUB(i) = 0;
    }

  //-- 6th Input. A column array containing the sense of each constraint
  //--            in the constraint matrix.
  charMatrix CTYPE (args(5).char_matrix_value ());

  if (error_state)
    {
      error ("__glpk__: invalid value of ctype");
      return retval;
    }

  char *ctype = CTYPE.fortran_vec ();

  //-- 7th Input. A column array containing the types of the variables.
  charMatrix VTYPE (args(6).char_matrix_value ());

  if (error_state)
    {
      error ("__glpk__: invalid value of vtype");
      return retval;
    }

  Array<int> vartype (mrowsc, 1);
  volatile int isMIP = 0;
  for (int i = 0; i < mrowsc ; i++)
    {
      if (VTYPE(i,0) == 'I')
        {
          isMIP = 1;
          vartype(i) = LPX_IV;
        }
      else
        vartype(i) = LPX_CV;
    }

  //-- 8th Input. Sense of optimization.
  volatile int sense;
  double SENSE = args(7).scalar_value ();

  if (error_state)
    {
      error ("__glpk__: invalid value of sense");
      return retval;
    }

  if (SENSE >= 0)
    sense = 1;
  else
    sense = -1;

  //-- 9th Input. A structure containing the control parameters.
  Octave_map PARAM = args(8).map_value ();

  if (error_state)
    {
      error ("__glpk__: invalid value of param");
      return retval;
    }

  //-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //-- Integer parameters
  //-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //-- Level of messages output by the solver
  OCTAVE_GLPK_GET_INT_PARAM ("msglev", lpxIntParam[0]);
  if (lpxIntParam[0] < 0 || lpxIntParam[0] > 3)
    {
      error ("__glpk__: param.msglev must be 0 (no output [default]) or 1 (error messages only) or 2 (normal output) or 3 (full output)");
      return retval;
    }

  //-- scaling option
  OCTAVE_GLPK_GET_INT_PARAM ("scale", lpxIntParam[1]);
  if (lpxIntParam[1] < 0 || lpxIntParam[1] > 2)
    {
      error ("__glpk__: param.scale must be 0 (no scaling) or 1 (equilibration scaling [default]) or 2 (geometric mean scaling)");
      return retval;
    }

  //-- Dual dimplex option
  OCTAVE_GLPK_GET_INT_PARAM ("dual", lpxIntParam[2]);
  if (lpxIntParam[2] < 0 || lpxIntParam[2] > 1)
    {
      error ("__glpk__: param.dual must be 0 (do NOT use dual simplex [default]) or 1 (use dual simplex)");
      return retval;
    }

  //-- Pricing option
  OCTAVE_GLPK_GET_INT_PARAM ("price", lpxIntParam[3]);
  if (lpxIntParam[3] < 0 || lpxIntParam[3] > 1)
    {
      error ("__glpk__: param.price must be 0 (textbook pricing) or 1 (steepest edge pricing [default])");
      return retval;
    }

  //-- Solution rounding option
  OCTAVE_GLPK_GET_INT_PARAM ("round", lpxIntParam[4]);
  if (lpxIntParam[4] < 0 || lpxIntParam[4] > 1)
    {
      error ("__glpk__: param.round must be 0 (report all primal and dual values [default]) or 1 (replace tiny primal and dual values by exact zero)");
      return retval;
    }

  //-- Simplex iterations limit
  OCTAVE_GLPK_GET_INT_PARAM ("itlim", lpxIntParam[5]);

  //-- Simplex iterations count
  OCTAVE_GLPK_GET_INT_PARAM ("itcnt", lpxIntParam[6]);

  //-- Output frequency, in iterations
  OCTAVE_GLPK_GET_INT_PARAM ("outfrq", lpxIntParam[7]);

  //-- Branching heuristic option
  OCTAVE_GLPK_GET_INT_PARAM ("branch", lpxIntParam[14]);
  if (lpxIntParam[14] < 0 || lpxIntParam[14] > 2)
    {
      error ("__glpk__: param.branch must be (MIP only) 0 (branch on first variable) or 1 (branch on last variable) or 2 (branch using a heuristic by Driebeck and Tomlin [default]");
      return retval;
    }

  //-- Backtracking heuristic option
  OCTAVE_GLPK_GET_INT_PARAM ("btrack", lpxIntParam[15]);
  if (lpxIntParam[15] < 0 || lpxIntParam[15] > 2)
    {
      error ("__glpk__: param.btrack must be (MIP only) 0 (depth first search) or 1 (breadth first search) or 2 (backtrack using the best projection heuristic [default]");
      return retval;
    }

  //-- Presolver option
  OCTAVE_GLPK_GET_INT_PARAM ("presol", lpxIntParam[16]);
  if (lpxIntParam[16] < 0 || lpxIntParam[16] > 1)
    {
      error ("__glpk__: param.presol must be 0 (do NOT use LP presolver) or 1 (use LP presolver [default])");
      return retval;
    }

  //-- LPsolver option
  volatile int lpsolver = 1;
  OCTAVE_GLPK_GET_INT_PARAM ("lpsolver", lpsolver);
  if (lpsolver < 1 || lpsolver > 2)
    {
      error ("__glpk__: param.lpsolver must be 1 (simplex method) or 2 (interior point method)");
      return retval;
    }

  //-- Save option
  volatile int save_pb = 0;
  OCTAVE_GLPK_GET_INT_PARAM ("save", save_pb);
  save_pb = save_pb != 0;

  //-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //-- Real parameters
  //-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //-- Ratio test option
  OCTAVE_GLPK_GET_REAL_PARAM ("relax", 0);

  //-- Relative tolerance used to check if the current basic solution
  //-- is primal feasible
  OCTAVE_GLPK_GET_REAL_PARAM ("tolbnd", 1);

  //-- Absolute tolerance used to check if the current basic solution
  //-- is dual feasible
  OCTAVE_GLPK_GET_REAL_PARAM ("toldj", 2);

  //-- Relative tolerance used to choose eligible pivotal elements of
  //--  the simplex table in the ratio test
  OCTAVE_GLPK_GET_REAL_PARAM ("tolpiv", 3);

  OCTAVE_GLPK_GET_REAL_PARAM ("objll", 4);

  OCTAVE_GLPK_GET_REAL_PARAM ("objul", 5);

  OCTAVE_GLPK_GET_REAL_PARAM ("tmlim", 6);

  OCTAVE_GLPK_GET_REAL_PARAM ("outdly", 7);

  OCTAVE_GLPK_GET_REAL_PARAM ("tolint", 8);

  OCTAVE_GLPK_GET_REAL_PARAM ("tolobj", 9);

  //-- Assign pointers to the output parameters
  ColumnVector xmin (mrowsc, octave_NA);
  ColumnVector fmin (1, octave_NA);
  ColumnVector status (1);
  ColumnVector lambda (mrowsA, octave_NA);
  ColumnVector redcosts (mrowsc, octave_NA);
  ColumnVector time (1);
  ColumnVector mem (1);

  int jmpret = setjmp (mark);

  if (jmpret == 0)
    glpk (sense, mrowsc, mrowsA, c, nz, rn.fortran_vec (),
          cn.fortran_vec (), a.fortran_vec (), b, ctype,
          freeLB.fortran_vec (), lb, freeUB.fortran_vec (),
          ub, vartype.fortran_vec (), isMIP, lpsolver,
          save_pb, xmin.fortran_vec (), fmin.fortran_vec (),
          status.fortran_vec (), lambda.fortran_vec (),
          redcosts.fortran_vec (), time.fortran_vec (),
          mem.fortran_vec ());

  Octave_map extra;

  if (! isMIP)
    {
      extra.assign ("lambda", octave_value (lambda));
      extra.assign ("redcosts", octave_value (redcosts));
    }

  extra.assign ("time", octave_value (time));
  extra.assign ("mem", octave_value (mem));

  retval(3) = extra;
  retval(2) = octave_value(status);
  retval(1) = octave_value(fmin);
  retval(0) = octave_value(xmin);

#else

  gripe_not_supported ("glpk");

#endif

  return retval;
}
