////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2005-2023 The Octave Project Developers
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

#include <ctime>

#include <limits>

#include "Array.h"
#include "chMatrix.h"
#include "dColVector.h"
#include "dMatrix.h"
#include "dSparse.h"
#include "lo-ieee.h"

#include "defun-dld.h"
#include "error.h"
#include "errwarn.h"
#include "oct-map.h"
#include "ov.h"
#include "ovl.h"

#if defined (HAVE_GLPK)

extern "C"
{
#if defined (HAVE_GLPK_GLPK_H)
#  include <glpk/glpk.h>
#else
#  include <glpk.h>
#endif
}

struct control_params
{
  int msglev;
  int dual;
  int price;
  int itlim;
  int outfrq;
  int branch;
  int btrack;
  int presol;
  int rtest;
  int tmlim;
  int outdly;
  double tolbnd;
  double toldj;
  double tolpiv;
  double objll;
  double objul;
  double tolint;
  double tolobj;
};

static int
glpk (int sense, int n, int m, double *c, int nz, int *rn, int *cn,
      double *a, double *b, char *ctype, int *freeLB, double *lb,
      int *freeUB, double *ub, int *vartype, int isMIP, int lpsolver,
      int save_pb, int scale, const control_params& par,
      double *xmin, double& fmin, int& status,
      double *lambda, double *redcosts, double& time)
{
  int typx = 0;
  int errnum = 0;

  time = 0.0;
  status = -1;    // Initialize status to "bad" value

  clock_t t_start = clock ();

  glp_prob *lp = glp_create_prob ();

  // Set the sense of optimization
  if (sense == 1)
    glp_set_obj_dir (lp, GLP_MIN);
  else
    glp_set_obj_dir (lp, GLP_MAX);

  glp_add_cols (lp, n);
  for (int i = 0; i < n; i++)
    {
      // Define type of the structural variables
      if (! freeLB[i] && ! freeUB[i])
        {
          if (lb[i] != ub[i])
            glp_set_col_bnds (lp, i+1, GLP_DB, lb[i], ub[i]);
          else
            glp_set_col_bnds (lp, i+1, GLP_FX, lb[i], ub[i]);
        }
      else
        {
          if (! freeLB[i] && freeUB[i])
            glp_set_col_bnds (lp, i+1, GLP_LO, lb[i], ub[i]);
          else
            {
              if (freeLB[i] && ! freeUB[i])
                glp_set_col_bnds (lp, i+1, GLP_UP, lb[i], ub[i]);
              else
                glp_set_col_bnds (lp, i+1, GLP_FR, lb[i], ub[i]);
            }
        }

      // -- Set the objective coefficient of the corresponding
      // -- structural variable.  No constant term is assumed.
      glp_set_obj_coef(lp, i+1, c[i]);

      if (isMIP)
        glp_set_col_kind (lp, i+1, vartype[i]);
    }

  glp_add_rows (lp, m);

  for (int i = 0; i < m; i++)
    {
      // If the i-th row has no lower bound (types F,U), the
      // corrispondent parameter will be ignored.  If the i-th row has
      // no upper bound (types F,L), the corrispondent parameter will be
      // ignored.  If the i-th row is of S type, the i-th LB is used,
      // but the i-th UB is ignored.

      switch (ctype[i])
        {
        case 'F':
          typx = GLP_FR;
          break;

        case 'U':
          typx = GLP_UP;
          break;

        case 'L':
          typx = GLP_LO;
          break;

        case 'S':
          typx = GLP_FX;
          break;

        case 'D':
          typx = GLP_DB;
          break;
        }

      glp_set_row_bnds (lp, i+1, typx, b[i], b[i]);

    }

  glp_load_matrix (lp, nz, rn, cn, a);

  if (save_pb)
    {
      static char tmp[] = "outpb.lp";
      if (glp_write_lp (lp, nullptr, tmp) != 0)
        error ("__glpk__: unable to write problem");
    }

  // scale the problem data
  if (! par.presol || lpsolver != 1)
    glp_scale_prob (lp, scale);

  // build advanced initial basis (if required)
  if (lpsolver == 1 && ! par.presol)
    glp_adv_basis (lp, 0);

  // For MIP problems without a presolver, a first pass with glp_simplex
  // is required
  if ((! isMIP && lpsolver == 1)
      || (isMIP && ! par.presol))
    {
      glp_smcp smcp;
      glp_init_smcp (&smcp);
      smcp.msg_lev = par.msglev;
      smcp.meth = par.dual;
      smcp.pricing = par.price;
      smcp.r_test = par.rtest;
      smcp.tol_bnd = par.tolbnd;
      smcp.tol_dj = par.toldj;
      smcp.tol_piv = par.tolpiv;
      smcp.obj_ll = par.objll;
      smcp.obj_ul = par.objul;
      smcp.it_lim = par.itlim;
      smcp.tm_lim = par.tmlim;
      smcp.out_frq = par.outfrq;
      smcp.out_dly = par.outdly;
      smcp.presolve = par.presol;
      errnum = glp_simplex (lp, &smcp);
    }

  if (isMIP)
    {
      glp_iocp iocp;
      glp_init_iocp (&iocp);
      iocp.msg_lev = par.msglev;
      iocp.br_tech = par.branch;
      iocp.bt_tech = par.btrack;
      iocp.tol_int = par.tolint;
      iocp.tol_obj = par.tolobj;
      iocp.tm_lim = par.tmlim;
      iocp.out_frq = par.outfrq;
      iocp.out_dly = par.outdly;
      iocp.presolve = par.presol;
      errnum = glp_intopt (lp, &iocp);
    }

  if (! isMIP && lpsolver == 2)
    {
      glp_iptcp iptcp;
      glp_init_iptcp (&iptcp);
      iptcp.msg_lev = par.msglev;
      errnum = glp_interior (lp, &iptcp);
    }

  if (errnum == 0)
    {
      if (isMIP)
        {
          status = glp_mip_status (lp);
          fmin = glp_mip_obj_val (lp);
        }
      else
        {
          if (lpsolver == 1)
            {
              status = glp_get_status (lp);
              fmin = glp_get_obj_val (lp);
            }
          else
            {
              status = glp_ipt_status (lp);
              fmin = glp_ipt_obj_val (lp);
            }
        }

      if (isMIP)
        {
          for (int i = 0; i < n; i++)
            xmin[i] = glp_mip_col_val (lp, i+1);
        }
      else
        {
          // Primal values
          for (int i = 0; i < n; i++)
            {
              if (lpsolver == 1)
                xmin[i] = glp_get_col_prim (lp, i+1);
              else
                xmin[i] = glp_ipt_col_prim (lp, i+1);
            }

          // Dual values
          for (int i = 0; i < m; i++)
            {
              if (lpsolver == 1)
                lambda[i] = glp_get_row_dual (lp, i+1);
              else
                lambda[i] = glp_ipt_row_dual (lp, i+1);
            }

          // Reduced costs
          for (int i = 0; i < glp_get_num_cols (lp); i++)
            {
              if (lpsolver == 1)
                redcosts[i] = glp_get_col_dual (lp, i+1);
              else
                redcosts[i] = glp_ipt_col_dual (lp, i+1);
            }
        }
    }

  time = (clock () - t_start) / CLOCKS_PER_SEC;

  glp_delete_prob (lp);
  // Request that GLPK free all memory resources.
  // This prevents reported memory leaks, but isn't strictly necessary.
  // The memory blocks used are allocated once and don't grow with further
  // calls to glpk so they would be reclaimed anyways when Octave exits.
  glp_free_env ();

  return errnum;
}

#endif

OCTAVE_BEGIN_NAMESPACE(octave)

#define OCTAVE_GLPK_GET_REAL_PARAM(NAME, VAL)                           \
  do                                                                    \
    {                                                                   \
      octave_value tmp = PARAM.getfield (NAME);                         \
                                                                        \
      if (tmp.is_defined ())                                            \
        {                                                               \
          if (! tmp.isempty ())                                        \
            VAL = tmp.xscalar_value ("glpk: invalid value in PARAM" NAME); \
          else                                                          \
            error ("glpk: invalid value in PARAM" NAME);                \
        }                                                               \
    }                                                                   \
  while (0)

#define OCTAVE_GLPK_GET_INT_PARAM(NAME, VAL)                            \
  do                                                                    \
    {                                                                   \
      octave_value tmp = PARAM.getfield (NAME);                         \
                                                                        \
      if (tmp.is_defined ())                                            \
        {                                                               \
          if (! tmp.isempty ())                                        \
            VAL = tmp.xint_value ("glpk: invalid value in PARAM" NAME); \
          else                                                          \
            error ("glpk: invalid value in PARAM" NAME);                \
        }                                                               \
    }                                                                   \
  while (0)

DEFUN_DLD (__glpk__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {[@var{values}] =} __glpk__ (@var{args})
Undocumented internal function.
@end deftypefn */)
{
#if defined (HAVE_GLPK)

  // FIXME: Should we even need checking for an internal function?
  if (args.length () != 9)
    print_usage ();

  // 1st Input.  A column array containing the objective function coefficients.
  int mrowsc = args(0).rows ();

  Matrix C = args(0).xmatrix_value ("__glpk__: invalid value of C");

  double *c = C.fortran_vec ();
  Array<int> rn;
  Array<int> cn;
  ColumnVector a;
  int mrowsA;
  int nz = 0;

  // 2nd Input.  A matrix containing the constraints coefficients.
  // If matrix A is NOT a sparse matrix
  if (args(1).issparse ())
    {
      SparseMatrix A = args(1).xsparse_matrix_value ("__glpk__: invalid value of A");

      mrowsA = A.rows ();
      octave_idx_type Anc = A.cols ();
      octave_idx_type Anz = A.nnz ();
      rn.resize (dim_vector (Anz+1, 1));
      cn.resize (dim_vector (Anz+1, 1));
      a.resize (Anz+1, 0.0);

      if (Anc != mrowsc)
        error ("__glpk__: invalid value of A");

      for (octave_idx_type j = 0; j < Anc; j++)
        for (octave_idx_type i = A.cidx (j); i < A.cidx (j+1); i++)
          {
            nz++;
            rn(nz) = A.ridx (i) + 1;
            cn(nz) = j + 1;
            a(nz) = A.data(i);
          }
    }
  else
    {
      Matrix A = args(1).xmatrix_value ("__glpk__: invalid value of A");

      mrowsA = A.rows ();
      rn.resize (dim_vector (mrowsA*mrowsc+1, 1));
      cn.resize (dim_vector (mrowsA*mrowsc+1, 1));
      a.resize (mrowsA*mrowsc+1, 0.0);

      for (int i = 0; i < mrowsA; i++)
        {
          for (int j = 0; j < mrowsc; j++)
            {
              if (A(i, j) != 0)
                {
                  nz++;
                  rn(nz) = i + 1;
                  cn(nz) = j + 1;
                  a(nz) = A(i, j);
                }
            }
        }

    }

  // 3rd Input.  A column array containing the right-hand side value
  //             for each constraint in the constraint matrix.
  Matrix B = args(2).xmatrix_value ("__glpk__: invalid value of B");

  double *b = B.fortran_vec ();

  // 4th Input.  An array of length mrowsc containing the lower
  //             bound on each of the variables.
  Matrix LB = args(3).xmatrix_value ("__glpk__: invalid value of LB");

  if (LB.numel () < mrowsc)
    error ("__glpk__: invalid dimensions for LB");

  double *lb = LB.fortran_vec ();

  // LB argument, default: Free
  Array<int> freeLB (dim_vector (mrowsc, 1));
  for (int i = 0; i < mrowsc; i++)
    {
      if (math::isinf (lb[i]))
        {
          freeLB(i) = 1;
          lb[i] = -numeric_limits<double>::Inf ();
        }
      else
        freeLB(i) = 0;
    }

  // 5th Input.  An array of at least length numcols containing the upper
  //             bound on each of the variables.
  Matrix UB = args(4).xmatrix_value ("__glpk__: invalid value of UB");

  if (UB.numel () < mrowsc)
    error ("__glpk__: invalid dimensions for UB");

  double *ub = UB.fortran_vec ();

  Array<int> freeUB (dim_vector (mrowsc, 1));
  for (int i = 0; i < mrowsc; i++)
    {
      if (math::isinf (ub[i]))
        {
          freeUB(i) = 1;
          ub[i] = numeric_limits<double>::Inf ();
        }
      else
        freeUB(i) = 0;
    }

  // 6th Input.  A column array containing the sense of each constraint
  //             in the constraint matrix.
  charMatrix CTYPE = args(5).xchar_matrix_value ("__glpk__: invalid value of CTYPE");

  char *ctype = CTYPE.fortran_vec ();

  // 7th Input.  A column array containing the types of the variables.
  charMatrix VTYPE = args(6).xchar_matrix_value ("__glpk__: invalid value of VARTYPE");

  Array<int> vartype (dim_vector (mrowsc, 1));
  int isMIP = 0;
  for (int i = 0; i < mrowsc ; i++)
    {
      if (VTYPE(i, 0) == 'I')
        {
          isMIP = 1;
          vartype(i) = GLP_IV;
        }
      else
        vartype(i) = GLP_CV;
    }

  // 8th Input.  Sense of optimization.
  int sense;
  double SENSE = args(7).xscalar_value ("__glpk__: invalid value of SENSE");

  if (SENSE >= 0)
    sense = 1;
  else
    sense = -1;

  // 9th Input.  A structure containing the control parameters.
  octave_scalar_map PARAM = args(8).xscalar_map_value ("__glpk__: invalid value of PARAM");

  control_params par;

  // Integer parameters

  // Level of messages output by the solver
  par.msglev = 1;
  OCTAVE_GLPK_GET_INT_PARAM ("msglev", par.msglev);
  if (par.msglev < 0 || par.msglev > 3)
    error ("__glpk__: PARAM.msglev must be 0 (no output) or 1 (error and warning messages only [default]) or 2 (normal output) or 3 (full output)");

  // scaling option
  int scale = 16;
  OCTAVE_GLPK_GET_INT_PARAM ("scale", scale);
  if (scale < 0 || scale > 128)
    error ("__glpk__: PARAM.scale must either be 128 (automatic selection of scaling options), or a bitwise or of: 1 (geometric mean scaling), 16 (equilibration scaling), 32 (round scale factors to power of two), 64 (skip if problem is well scaled");

  // Dual simplex option
  par.dual = 1;
  OCTAVE_GLPK_GET_INT_PARAM ("dual", par.dual);
  if (par.dual < 1 || par.dual > 3)
    error ("__glpk__: PARAM.dual must be 1 (use two-phase primal simplex [default]) or 2 (use two-phase dual simplex) or 3 (use two-phase dual simplex, and if it fails, switch to the primal simplex)");

  // Pricing option
  par.price = 34;
  OCTAVE_GLPK_GET_INT_PARAM ("price", par.price);
  if (par.price != 17 && par.price != 34)
    error ("__glpk__: PARAM.price must be 17 (textbook pricing) or 34 (steepest edge pricing [default])");

  // Simplex iterations limit
  par.itlim = std::numeric_limits<int>::max ();
  OCTAVE_GLPK_GET_INT_PARAM ("itlim", par.itlim);

  // Output frequency, in iterations
  par.outfrq = 200;
  OCTAVE_GLPK_GET_INT_PARAM ("outfrq", par.outfrq);

  // Branching heuristic option
  par.branch = 4;
  OCTAVE_GLPK_GET_INT_PARAM ("branch", par.branch);
  if (par.branch < 1 || par.branch > 5)
    error ("__glpk__: PARAM.branch must be 1 (first fractional variable) or 2 (last fractional variable) or 3 (most fractional variable) or 4 (heuristic by Driebeck and Tomlin [default]) or 5 (hybrid pseudocost heuristic)");

  // Backtracking heuristic option
  par.btrack = 4;
  OCTAVE_GLPK_GET_INT_PARAM ("btrack", par.btrack);
  if (par.btrack < 1 || par.btrack > 4)
    error ("__glpk__: PARAM.btrack must be 1 (depth first search) or 2 (breadth first search) or 3 (best local bound) or 4 (best projection heuristic [default]");

  // Presolver option
  par.presol = 1;
  OCTAVE_GLPK_GET_INT_PARAM ("presol", par.presol);
  if (par.presol < 0 || par.presol > 1)
    error ("__glpk__: PARAM.presol must be 0 (do NOT use LP presolver) or 1 (use LP presolver [default])");

  // LPsolver option
  int lpsolver = 1;
  OCTAVE_GLPK_GET_INT_PARAM ("lpsolver", lpsolver);
  if (lpsolver < 1 || lpsolver > 2)
    error ("__glpk__: PARAM.lpsolver must be 1 (simplex method) or 2 (interior point method)");

  // Ratio test option
  par.rtest = 34;
  OCTAVE_GLPK_GET_INT_PARAM ("rtest", par.rtest);
  if (par.rtest != 17 && par.rtest != 34)
    error ("__glpk__: PARAM.rtest must be 17 (standard ratio test) or 34 (Harris' two-pass ratio test [default])");

  par.tmlim = std::numeric_limits<int>::max ();
  OCTAVE_GLPK_GET_INT_PARAM ("tmlim", par.tmlim);

  par.outdly = 0;
  OCTAVE_GLPK_GET_INT_PARAM ("outdly", par.outdly);

  // Save option
  int save_pb = 0;
  OCTAVE_GLPK_GET_INT_PARAM ("save", save_pb);
  save_pb = save_pb != 0;

  // Real parameters

  // Relative tolerance used to check if the current basic solution
  // is primal feasible
  par.tolbnd = 1e-7;
  OCTAVE_GLPK_GET_REAL_PARAM ("tolbnd", par.tolbnd);

  // Absolute tolerance used to check if the current basic solution
  // is dual feasible
  par.toldj = 1e-7;
  OCTAVE_GLPK_GET_REAL_PARAM ("toldj", par.toldj);

  // Relative tolerance used to choose eligible pivotal elements of
  //  the simplex table in the ratio test
  par.tolpiv = 1e-10;
  OCTAVE_GLPK_GET_REAL_PARAM ("tolpiv", par.tolpiv);

  par.objll = -std::numeric_limits<double>::max ();
  OCTAVE_GLPK_GET_REAL_PARAM ("objll", par.objll);

  par.objul = std::numeric_limits<double>::max ();
  OCTAVE_GLPK_GET_REAL_PARAM ("objul", par.objul);

  par.tolint = 1e-5;
  OCTAVE_GLPK_GET_REAL_PARAM ("tolint", par.tolint);

  par.tolobj = 1e-7;
  OCTAVE_GLPK_GET_REAL_PARAM ("tolobj", par.tolobj);

  // Assign pointers to the output parameters
  ColumnVector xmin (mrowsc, octave_NA);
  double fmin = octave_NA;
  ColumnVector lambda (mrowsA, octave_NA);
  ColumnVector redcosts (mrowsc, octave_NA);

  double time = 0.0;
  int status = -1;

  int errnum = glpk (sense, mrowsc, mrowsA, c, nz, rn.fortran_vec (),
                     cn.fortran_vec (), a.fortran_vec (), b, ctype,
                     freeLB.fortran_vec (), lb, freeUB.fortran_vec (),
                     ub, vartype.fortran_vec (), isMIP, lpsolver,
                     save_pb, scale, par, xmin.fortran_vec (), fmin,
                     status, lambda.fortran_vec (),
                     redcosts.fortran_vec (), time);

  octave_scalar_map extra;

  if (! isMIP)
    {
      extra.assign ("lambda", lambda);
      extra.assign ("redcosts", redcosts);
    }

  extra.assign ("time", time);
  extra.assign ("status", status);

  return ovl (xmin, fmin, errnum, extra);

#else

  octave_unused_parameter (args);

  err_disabled_feature ("glpk", "GNU Linear Programming Kit");

#endif
}

/*
## No test needed for internal helper function.
%!assert (1)
*/

OCTAVE_END_NAMESPACE(octave)
