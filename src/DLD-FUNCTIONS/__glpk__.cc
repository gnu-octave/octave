/*

Copyright (C) 2005 Nicolo' Giorgetti

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>
#include <csetjmp>
#include <ctime>

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "pager.h"

#if defined (HAVE_GLPK)

extern "C" {
#include <glpk.h>
}

#define OCTOUT octave_stdout
#define OCTERR octave_stdout
#define NIntP 17
#define NRealP 10

int lpxIntParam[NIntP] = {
  1,
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

jmp_buf mark;  //-- Address for long jump to jump to
int fperr; //-- Global error number


int
glpk_fault_hook (void * /* info */, char *msg)
{
  OCTERR << "*** SEVERE CRITICAL ERROR *** from GLPK !\n\n"<<msg<<" %s\n";
  longjmp (mark, -1);
}

int
glpk_print_hook (void * /* info */, char *msg)
{
  OCTERR << msg << "\n";
  return 1;
}


int
glpk (int sense, int n, int m, double *c, int nz, int *rn, int *cn,
      double *a, double *b, char *ctype, int *freeLB, double *lb,
      int *freeUB, double *ub, int *vartype, int isMIP, int lpsolver,
      int save_pb, double *xmin, double *fmin, double *status,
      double *lambda, double *redcosts, double *time, double *mem)
{
  int error;
  int typx = 0;
  int method;

  clock_t t_start = clock();

  lib_set_fault_hook (NULL, glpk_fault_hook);

  if (lpxIntParam[0] > 1)
    lib_set_print_hook (NULL, glpk_print_hook);

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
	lpx_set_col_bnds (lp, i+1, LPX_DB, lb[i], ub[i]);
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
      if (lpx_write_cpxlp (lp, "outpb.lp") != 0)
	{
	  OCTERR << "Unable to write problem\n";
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
	    error = lpx_simplex (lp);
	    error = lpx_integer (lp);
	  }
	else
	  error = lpx_simplex(lp);
      }
     break;

    case 'T':
      error = lpx_interior(lp);
      break;

    default:
      insist (method != method);
    }

  /*  error assumes the following results:
      error=0 <=> No errors
      error=1 <=> Iteration limit exceeded.
      error=2 <=> Numerical problems with basis matrix.
  */
  if (error == LPX_E_OK)
    {
      if (isMIP)
	{
	  *status = static_cast<double> (lpx_mip_status (lp));
	  *fmin = lpx_mip_obj_val (lp);
	}
      else
	{
	  if (lpsolver == 1)
	    {
	      *status = static_cast<double> (lpx_get_status (lp));
	      *fmin = lpx_get_obj_val (lp);
	    }
	  else
	    {
	      *status = static_cast<double> (lpx_ipt_status (lp));
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

      *time = static_cast<double> (clock () - t_start) / CLOCKS_PER_SEC;
      *mem = static_cast<double> (lib_env_ptr () -> mem_tpeak);

      lpx_delete_prob (lp);
      return 0;
    }

   lpx_delete_prob (lp);

   *status= static_cast<double> (error);

   return error;
}

#endif

DEFUN_DLD (__glpk__, args, ,
  "__glpk__: internal interface for the GLPK library.\n\
You should be using using glpk instead")
{
  // The list of values to return.  See the declaration in oct-obj.h
  octave_value_list retval;

#if defined (HAVE_GLPK)

  int nrhs = args.length ();

  if (nrhs < 1)
    {
      OCTERR<<"Use the script glpk for the optimization\n";
      return retval;
    }

  //-- 1nd Input. A column array containing the objective function
  //--            coefficients.
  int mrowsc = args(0).rows();

  Matrix C (args(0).matrix_value ());
  double *c = C.fortran_vec ();

  //-- 2nd Input. A matrix containing the constraints coefficients.
  // If matrix A is NOT a sparse matrix
  // if(!mxIsSparse(A_IN)){
  int mrowsA = args(1).rows();
  Matrix A (args(1).matrix_value ()); // get the matrix
  Array<int> rn (mrowsA*mrowsc+1);
  Array<int> cn (mrowsA*mrowsc+1);
  ColumnVector a (mrowsA*mrowsc+1, 0.0);

  volatile int nz = 0;
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

// DON'T DELETE THIS PART... REPRESENTS THE SPARSE MATRICES MANIPULATION
//	  }else{
//	    int i,j;
//	    int *jc,*ir;
//	    double *pr;
//	    int nelc,count,row;
//
//	    /* NOTE: nnz is the actual number of nonzeros and is stored as the
//       last element of the jc array where the size of the jc array is the
//       number of columns + 1 */
//	    nz = *(mxGetJc(A_IN) + mrowsc);
//	    jc = mxGetJc(A_IN);
//	    ir = mxGetIr(A_IN);
//	    pr = mxGetPr(A_IN);
//
//       rn=(int *)calloc(nz+1,sizeof(int));
//	    cn=(int *)calloc(nz+1,sizeof(int));
//	    a=(double *)calloc(nz+1,sizeof(double));
//
//       count=0; row=0;
//	    for(i=1;i<=mrowsc;i++){
//	      nelc=jc[i]-jc[i-1];
//	      for(j=0;j<nelc;j++){
//		      count++;
//		      rn[count]=ir[row]+1;
//		      cn[count]=i;
//		      a[count]=pr[row];
//		      row++;
//	      }
//	    }
//	  }

  //-- 3rd Input. A column array containing the right-hand side value
  //	           for each constraint in the constraint matrix.
  Matrix B (args(2).matrix_value ());
  double *b = B.fortran_vec ();

  //-- 4th Input. An array of length mrowsc containing the lower
  //--            bound on each of the variables.
  Matrix LB (args(3).matrix_value ());
  double *lb = LB.fortran_vec ();

  //-- LB argument, default: Free
  Array<int> freeLB (mrowsc);
  for (int i = 0; i < mrowsc; i++)
     {
       if (isinf (lb[i]))
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

  double *ub = UB.fortran_vec ();

  Array<int> freeUB (mrowsc);
  for (int i = 0; i < mrowsc; i++)
    {
      if (isinf (ub[i]))
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
  char *ctype = CTYPE.fortran_vec ();

  //-- 7th Input. A column array containing the types of the variables.
  charMatrix VTYPE (args(6).char_matrix_value ());

  Array<int> vartype (mrowsc);
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
  if (SENSE >= 0)
    sense = 1;
  else
    sense = -1;

  //-- 9th Input. A structure containing the control parameters.
  Octave_map PARAM = args(8).map_value ();

  //-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //-- Integer parameters
  //-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //-- Level of messages output by the solver
  if (PARAM.contains ("msglev"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("msglev"))(0);

      double numtmp = tmp.scalar_value ();
      if (numtmp != 0 && numtmp != 1 && numtmp != 2 && numtmp != 3)
	{
	  OCTOUT << "'msglev' parameter must be only:\n\t0 - no output,\n\t1 - error messages only),\n\t2 - normal output,\n\t3 - full output [default]\n";
	  return retval;
	}

      lpxIntParam[0] = static_cast<int> (numtmp);
   }

  //-- scaling option
  if (PARAM.contains ("scale"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("scale"))(0);
      double numtmp = tmp.scalar_value ();
      if (numtmp != 0 && numtmp != 1 && numtmp != 2)
	{
	  OCTOUT << "'scale' parameter must be only:\n\t0 - no scaling,\n\t1 - equilibration scaling,\n\t2 - geometric mean scaling\n";
	  return retval;
	}
      lpxIntParam[1] = static_cast<int> (numtmp);
    }

  //-- Dual dimplex option
  if (PARAM.contains ("dual"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("dual"))(0);
      double numtmp = tmp.scalar_value ();
      if (numtmp != 0 && numtmp != 1)
	{
	  OCTOUT<<"'dual' parameter must be only:\n\t0 - do not use the dual simplex [default],\n\t1 - use dual simplex\n";
	  return retval;
	}
      lpxIntParam[2] = static_cast<int> (numtmp);
    }

  //-- Pricing option
  if (PARAM.contains ("price"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("price"))(0);
      double numtmp = tmp.scalar_value();
      if (numtmp != 0 && numtmp != 1)
	{
	  OCTOUT << "'price' parameter must be only:\n\t0 - textbook pricing,\n\t1 - steepest edge pricing [default]\n";
	  return retval;
	}
      lpxIntParam[3] = static_cast<int> (numtmp);
   }

  //-- Solution rounding option
  if (PARAM.contains ("round"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("round"))(0);
      double numtmp = tmp.scalar_value ();
      if (numtmp != 0 && numtmp != 1)
	{
	  OCTOUT << "'round' parameter must be only:\n\t0 - report all primal and dual values [default],\n\t1 - replace tiny primal and dual values by exact zero\n";
	  return retval;
	}
      lpxIntParam[4] = static_cast<int> (numtmp);
    }

  //-- Simplex iterations limit
  if (PARAM.contains ("itlim"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("itlim"))(0);
      lpxIntParam[5] = static_cast<int> (tmp.scalar_value ());
    }

  //-- Simplex iterations count
  if (PARAM.contains ("itcnt"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("itcnt"))(0);
      lpxIntParam[6] = static_cast<int> (tmp.scalar_value ());
    }

  //-- Output frequency, in iterations
  if (PARAM.contains ("outfrq"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("outfrq"))(0);
      lpxIntParam[7] = static_cast<int> (tmp.scalar_value ());
    }

  //-- Branching heuristic option
  if (PARAM.contains("branch"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("branch"))(0);
      double numtmp = tmp.scalar_value ();
      if (numtmp != 0 && numtmp != 1 && numtmp != 2)
	{
	  OCTOUT << "'branch' parameter must be only (for MIP only):\n\t0 - branch on the first variable,\n\t1 - branch on the last variable,\n\t2 - branch using a heuristic by Driebeck and Tomlin [default]\n";
	  return retval;
	}
      lpxIntParam[14] = static_cast<int> (numtmp);
   }

  //-- Backtracking heuristic option
  if (PARAM.contains ("btrack"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("btrack"))(0);
      double numtmp = tmp.scalar_value ();
      if (numtmp != 0 && numtmp != 1 && numtmp != 2)
	{
	  OCTOUT << "'btrack' parameter must be only (for MIP only):\n\t0 - depth first search,\n\t1 - breadth first search,\n\t2 - backtrack using the best projection heuristic\n";
	  return retval;
	}
      lpxIntParam[15] = static_cast<int> (numtmp);
   }

  //-- Presolver option
  if (PARAM.contains ("presol"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("presol"))(0);
      double numtmp = tmp.scalar_value ();
      if (numtmp != 0 && numtmp != 1)
	{
	  OCTOUT << "'presol' parameter must be only:\n\t0 - LP presolver is ***NOT*** used,\n\t1 - LP presol is used\n";
	  return retval;
	}
      lpxIntParam[16] = static_cast<int> (numtmp);
    }

  //-- LPsolver option
  volatile int lpsolver = 1;
  if (PARAM.contains ("lpsolver"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("lpsolver"))(0);
      double numtmp = tmp.scalar_value ();
      if (numtmp != 1 && numtmp != 2)
	{
	  OCTOUT << "'lpsolver' parameter must be only:\n\t1 - simplex method,\n\t2 - interior point method\n";
	  return retval;
	}
      lpsolver = static_cast<int> (numtmp);
    }

  //-- Save option
  volatile int save_pb = 0;
  if (PARAM.contains ("save"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("save"))(0);
      save_pb = (tmp.scalar_value () != 0);
    }

  //-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //-- Real parameters
  //-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //-- Ratio test option
  if (PARAM.contains ("relax"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("relax"))(0);
      lpxRealParam[0] = tmp.scalar_value ();
    }

  //-- Relative tolerance used to check if the current basic solution
  //-- is primal feasible
  if (PARAM.contains ("tolbnd"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("tolbn"))(0);
      lpxRealParam[1] = tmp.scalar_value ();
    }

  //-- Absolute tolerance used to check if the current basic solution
  //-- is dual feasible
  if (PARAM.contains ("toldj"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("toldj"))(0);
      lpxRealParam[2] = tmp.scalar_value();
    }

  //-- Relative tolerance used to choose eligible pivotal elements of
  //--	the simplex table in the ratio test
  if (PARAM.contains ("tolpiv"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("tolpiv"))(0);
      lpxRealParam[3] = tmp.scalar_value ();
    }

  if (PARAM.contains ("objll"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("objll"))(0);
      lpxRealParam[4] = tmp.scalar_value ();
    }

  if (PARAM.contains ("objul"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("objul"))(0);
      lpxRealParam[5] = tmp.scalar_value ();
    }

  if (PARAM.contains ("tmlim"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("tmlim"))(0);
      lpxRealParam[6] = tmp.scalar_value ();
    }

  if (PARAM.contains ("outdly"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("outdly"))(0);
      lpxRealParam[7] = tmp.scalar_value ();
    }

  if (PARAM.contains ("tolint"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("tolint"))(0);
      lpxRealParam[8] = tmp.scalar_value ();
    }

  if (PARAM.contains ("tolobj"))
    {
      octave_value tmp = PARAM.contents (PARAM.seek ("tolobj"))(0);
      lpxRealParam[9] = tmp.scalar_value ();
    }

  //-- Assign pointers to the output parameters
  ColumnVector xmin (mrowsc);
  ColumnVector fmin (1);
  ColumnVector status (1);
  ColumnVector lambda (mrowsA);
  ColumnVector redcosts (mrowsc);
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
