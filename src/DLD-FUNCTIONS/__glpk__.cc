/*- --------------------------------------------------------------------
 --
 -- Copyright (C) 2005, Nicolo' Giorgetti, All rights reserved.
 -- E-mail: <giorgetti@dii.unisi.it>.
 --
 -- This file is part of GLPKOCT an Octave interface to GLPK.
 --
 -- GLPK is free software; you can redistribute it and/or modify it
 -- under the terms of the GNU General Public License as published by
 -- the Free Software Foundation; either version 2, or (at your option)
 -- any later version.
 --
 -- GLPK is distributed in the hope that it will be useful, but WITHOUT
 -- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 -- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
 -- License for more details.
 --
 -- You should have received a copy of the GNU General Public License
 -- along with GLPK; see the file COPYING. If not, write to the Free
 -- Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 -- 02111-1307, USA.
 --
 -- ---------------------------------------------------------------------*/

#include <cfloat>
#include <csetjmp>
#include <ctime>

//-- Octave headers
#include <oct.h>
#include <octave/ov-struct.h>
#include <octave/config.h>
#include <octave/error.h>

//-- GLPK C header
extern "C"{
#include "glpk.h"
}

#define OCTOUT octave_stdout
#define OCTERR octave_stdout
#define NIntP 17
#define NRealP 10

int lpxIntParam[NIntP]= {
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

int IParam[NIntP]={
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


double lpxRealParam[NRealP]={
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

int RParam[NRealP]={
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
int     fperr; //-- Global error number


int glpkmex_fault_hook(void *info,  char *msg)
{    
    OCTERR<<"*** SEVERE CRITICAL ERROR *** from GLPK !\n\n"<<msg<<" %s\n";
    longjmp( mark, -1 );
}

int glpkmex_print_hook(void *info,  char *msg)
{
    OCTERR<<msg<<"\n";
    return 1;
}


int glpk(int sense,int n, int m, double *c,int nz,int *rn,int *cn, 
         double *a,double *b, char *ctype,int *freeLB, double *lb, 
	 int *freeUB, double *ub, int *vartype, int isMIP, int lpsolver,
	 int save_pb, double *xmin, double *fmin, double *status,
         double *lambda, double *redcosts, double *time, double *mem)
{
   
   LPX *lp;
   int i,j;
   int error;
   clock_t  t_start;
   int typx=0;
   int method;

   t_start = clock();
   
   lib_set_fault_hook(NULL,glpkmex_fault_hook);
   

   if (lpxIntParam[0] > 1){
         lib_set_print_hook(NULL,glpkmex_print_hook);
   }
      
   lp=lpx_create_prob();
     
   
   //-- Set the sense of optimization
   if (sense==1) lpx_set_obj_dir(lp,LPX_MIN);
   else lpx_set_obj_dir(lp,LPX_MAX);

   //-- If the problem has integer structural variables switch to MIP
   if(isMIP) lpx_set_class(lp,LPX_MIP);

   lpx_add_cols(lp,n);   
   for(i=0;i<n;i++){
	
      //-- Define type of the structural variables
      if (!freeLB[i] && !freeUB[i]){
        lpx_set_col_bnds(lp,i+1,LPX_DB,lb[i],ub[i]);
      }else{
         if (!freeLB[i] && freeUB[i]){
            lpx_set_col_bnds(lp,i+1,LPX_LO,lb[i],ub[i]);
         }else{
            if (freeLB[i] && !freeUB[i]){
               lpx_set_col_bnds(lp,i+1,LPX_UP,lb[i],ub[i]);
            }else{
               lpx_set_col_bnds(lp,i+1,LPX_FR,lb[i],ub[i]);
            }
         }
      }
      // -- Set the objective coefficient of the corresponding 
      // -- structural variable. No constant term is assumed.
	  lpx_set_obj_coef(lp,i+1,c[i]);

      if(isMIP){
        lpx_set_col_kind(lp,i+1,vartype[i]);
      }
   }
   
   lpx_add_rows(lp,m);   
   for(i=0;i<m;i++){
   
      /* If the i-th row has no lower bound (types F,U), the
         corrispondent parameter will be ignored.
         If the i-th row has no upper bound (types F,L), the corrispondent
         parameter will be ignored.
         If the i-th row is of S type, the i-th LB is used, but
         the i-th UB is ignored.
      */
      switch(ctype[i]){
         case 'F': typx=LPX_FR; break;
         case 'U': typx=LPX_UP; break;
         case 'L': typx=LPX_LO; break;
         case 'S': typx=LPX_FX; break;
         case 'D': typx=LPX_DB; 
      }
      lpx_set_row_bnds(lp,i+1,typx,b[i],b[i]);

   }
   lpx_load_matrix(lp,nz,rn,cn,a);

   if (save_pb){
      if(lpx_write_cpxlp(lp, "outpb.lp") != 0){
        OCTERR<<"Unable to write problem\n";
        longjmp( mark, -1 );
      }
   }

   //-- scale the problem data (if required)
   //-- if (scale && (!presol || method == 1)) lpx_scale_prob(lp);
   //-- LPX_K_SCALE=IParam[1]  LPX_K_PRESOL=IParam[16]
   if (lpxIntParam[1] && (!lpxIntParam[16] || lpsolver!=1)){
      lpx_scale_prob(lp);
   }
   //-- build advanced initial basis (if required)
   if (lpsolver == 1 && !lpxIntParam[16]){
      lpx_adv_basis(lp);
   }
 
   for(i=0;i<NIntP;i++){
     lpx_set_int_parm(lp,IParam[i],lpxIntParam[i]);
   }
   for(i=0;i<NRealP;i++){
     lpx_set_real_parm(lp,RParam[i],lpxRealParam[i]);
   } 

   if(lpsolver==1) method='S';
   else method='T';

   switch(method){
   case 'S':
     if(isMIP){
       method='I';
       error=lpx_simplex(lp);
       error=lpx_integer(lp);
     }else{
       error=lpx_simplex(lp);
     }
     break;
   case 'T':
     error=lpx_interior(lp);
     break;
   default:
     insist(method != method);
   }

   /*
       error assumes the following results:
       error=0 <=> No errors
       error=1 <=> Iteration limit exceeded.
       error=2 <=> Numerical problems with basis matrix.
   */
   if(error==LPX_E_OK){
     if(isMIP){
       *status=(double)lpx_mip_status(lp);
       *fmin=lpx_mip_obj_val(lp);
     }else{
       if(lpsolver==1){
         *status=(double)lpx_get_status(lp);
         *fmin=lpx_get_obj_val(lp);
       }else{
         *status=(double)lpx_ipt_status(lp);
         *fmin=lpx_ipt_obj_val(lp);
       }
     }
     if(isMIP){
         for(i=0;i<n;i++)  xmin[i]=lpx_mip_col_val(lp,i+1);
     }else{
      /* Primal values */
      for(i=0;i<n;i++){
         if(lpsolver==1) xmin[i]=lpx_get_col_prim(lp,i+1);
         else xmin[i]=lpx_ipt_col_prim(lp,i+1);
      }
      /* Dual values */
      for(i=0; i<m; i++){
         if(lpsolver==1) lambda[i]=lpx_get_row_dual(lp,i+1);
         else lambda[i]=lpx_ipt_row_dual(lp,i+1);
      }
      /* Reduced costs */
      for(i=0; i<lpx_get_num_cols(lp); i++){
         if(lpsolver==1) redcosts[i]=lpx_get_col_dual(lp,i+1);
         else redcosts[i]=lpx_ipt_col_dual(lp,i+1);
      }
     }
     *time=((double)(clock() - t_start))/CLOCKS_PER_SEC;
     *mem=(double)lib_env_ptr()->mem_tpeak;      

     lpx_delete_prob(lp);
     return(0);
   }
   lpx_delete_prob(lp);
   *status=(double)error;
   return(error);
}





DEFUN_DLD(glpkoct, args, nlhs, "glpkoct: OCT interface for the GLPK library. Don't use glpkoct, use glpk.m instead")
{
   // The list of values to return.  See the declaration in oct-obj.h
   octave_value_list retval;

   int nrhs=args.length();
	
   if(nrhs<1){
      OCTERR<<"Use the script glpk for the optimization\n";
      return retval;
   }	
   
   //-- 1st Input. Sense of optimization.
   int sense;
   double SENSE = args(0).scalar_value ();
   if (SENSE>=0) sense=1;
   else sense =-1;
   
   //-- 2nd Input. A column array containing the objective function
   //--            coefficients.
   int mrowsc=args(1).rows();
   
   Matrix C(args(1).matrix_value());
   double *c=C.fortran_vec();
   	
   //-- 3rd Input. A matrix containing the constraints coefficients.
   // If matrix A is NOT a sparse matrix
   // if(!mxIsSparse(A_IN)){
   int mrowsA=args(2).rows();	
   Matrix A(args(2).matrix_value()); // get the matrix
   Array<int> rn (mrowsA*mrowsc+1);
   Array<int> cn (mrowsA*mrowsc+1);
   ColumnVector a (mrowsA*mrowsc+1, 0.0);

   int nz=0;
   for(int i=0;i<mrowsA;i++){
      for(int j=0;j<mrowsc;j++){
        if(A(i,j) != 0){
           nz++;
	   rn(nz)=i+1;
	   cn(nz)=j+1;
	   a(nz)=A(i,j);
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

   //-- 4th Input. A column array containing the right-hand side value
   //	           for each constraint in the constraint matrix.
   Matrix B(args(3).matrix_value());
   double *b=B.fortran_vec();
   	
   for(int i=0; i< mrowsA; i++){
   	if (isinf(b[i]) && b[i]<0) b[i]=-octave_Inf;
   	if (isinf(b[i]) && b[i]>0) b[i]=octave_Inf;
   }	

      	
   //-- 5th Input. A column array containing the sense of each constraint
   //--            in the constraint matrix.
   charMatrix CTYPE(args(4).char_matrix_value());	
	
   char *ctype=CTYPE.fortran_vec();
	
   //-- 6th Input. An array of length mrowsc containing the lower
   //--            bound on each of the variables.
   Matrix LB(args(5).matrix_value());
   		 		
   double *lb=LB.fortran_vec();
   
   //-- LB argument, default: Free
   Array<int> freeLB (mrowsc);
   for(int i=0;i<mrowsc;i++){
      if(isinf(lb[i])){
         freeLB(i)=1;
         lb[i]=-octave_Inf;
      }else freeLB(i)=0;
   }
   
   //-- 7th Input. An array of at least length numcols containing the upper
   //--            bound on each of the variables.
   Matrix UB(args(6).matrix_value());
   		
   double *ub=UB.fortran_vec();
 			
   Array<int> freeUB (mrowsc);
   for(int i=0;i<mrowsc;i++){
      if(isinf(ub[i])){
         freeUB(i)=1;
	 ub[i]=octave_Inf;
      }else freeUB(i)=0;
   }
		
   //-- 8th Input. A column array containing the types of the variables.
   charMatrix VTYPE(args(7).char_matrix_value());

   Array<int> vartype (mrowsc);
   int isMIP;
   for (int i = 0; i < mrowsc ; i++){
      if(VTYPE(i,0)=='I'){
         isMIP=1;
	 vartype(i)=LPX_IV;
      }else{
         vartype(i)=LPX_CV;
      }
   }

   //-- 9th Input. A structure containing the control parameters.
   Octave_map PARAM = args(8).map_value();
   
   //-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   //-- Integer parameters                                             
   //-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   //-- Level of messages output by the solver
   if(PARAM.contains("msglev")){
      octave_value tmp=PARAM.contents(PARAM.seek("msglev"))(0);

      double numtmp=tmp.scalar_value();
      if((numtmp != 0) && (numtmp != 1) && (numtmp != 2) && (numtmp != 3)){
         OCTOUT<<"'msglev' parameter must be only:\n\t0 - no output,\n\t1 - error messages only),\n\t2 - normal output,\n\t3 - full output [default]\n";
         return retval;
      }
         lpxIntParam[0]=(int) numtmp;
   }	

   //-- scaling option
   if(PARAM.contains("scale")){
      octave_value tmp=PARAM.contents(PARAM.seek("scale"))(0);
      double numtmp=tmp.scalar_value(); 
      if((numtmp != 0) && (numtmp != 1) && (numtmp != 2)){
         OCTOUT<<"'scale' parameter must be only:\n\t0 - no scaling,\n\t1 - equilibration scaling,\n\t2 - geometric mean scaling\n";
         return retval;
   }
      lpxIntParam[1]=(int) numtmp;
   }

   //-- Dual dimplex option
   if(PARAM.contains("dual")){
      octave_value tmp=PARAM.contents(PARAM.seek("dual"))(0);
      double numtmp=tmp.scalar_value();
      if((numtmp != 0) && (numtmp != 1)){
         OCTOUT<<"'dual' parameter must be only:\n\t0 - do not use the dual simplex [default],\n\t1 - use dual simplex\n";
         return retval;
      }
      lpxIntParam[2]=(int) numtmp;
   }

   //-- Pricing option
   if(PARAM.contains("price")){
      octave_value tmp=PARAM.contents(PARAM.seek("price"))(0);
      double numtmp=tmp.scalar_value();
      if((numtmp != 0) && (numtmp != 1)){
         OCTOUT<<"'price' parameter must be only:\n\t0 - textbook pricing,\n\t1 - steepest edge pricing [default]\n";
	 return retval;
      }
      lpxIntParam[3]=(int) numtmp;
   }

   //-- Solution rounding option 
   if(PARAM.contains("round")){
      octave_value tmp=PARAM.contents(PARAM.seek("round"))(0);
      double numtmp=tmp.scalar_value();
      if((numtmp != 0) && (numtmp != 1)){
         OCTOUT<<"'round' parameter must be only:\n\t0 - report all primal and dual values [default],\n\t1 - replace tiny primal and dual values by exact zero\n";
	return retval;
      }
      lpxIntParam[4]=(int) numtmp;
   }

   //-- Simplex iterations limit
   if(PARAM.contains("itlim")){
      octave_value tmp=PARAM.contents(PARAM.seek("itlim"))(0);
      lpxIntParam[5]=(int) tmp.scalar_value(); }

   //-- Simplex iterations count
   if(PARAM.contains("itcnt")){
      octave_value tmp=PARAM.contents(PARAM.seek("itcnt"))(0);
      lpxIntParam[6]=(int) tmp.scalar_value(); }
 
   //-- Output frequency, in iterations
   if(PARAM.contains("outfrq")){
      octave_value tmp=PARAM.contents(PARAM.seek("outfrq"))(0);
      lpxIntParam[7]=(int) tmp.scalar_value(); }
 
   //-- Branching heuristic option
   if(PARAM.contains("branch")){
      octave_value tmp=PARAM.contents(PARAM.seek("branch"))(0);
      double numtmp=tmp.scalar_value();			
      if((numtmp != 0) && (numtmp != 1) && (numtmp != 2)){
         OCTOUT<<"'branch' parameter must be only (for MIP only):\n\t0 - branch on the first variable,\n\t1 - branch on the last variable,\n\t2 - branch using a heuristic by Driebeck and Tomlin [default]\n";
         return retval;
      }
      lpxIntParam[14]=(int) numtmp;
   }

   //-- Backtracking heuristic option
   if(PARAM.contains("btrack")){
      octave_value tmp=PARAM.contents(PARAM.seek("btrack"))(0);
      double numtmp=tmp.scalar_value();	
      if((numtmp != 0) && (numtmp != 1) && (numtmp != 2)){
         OCTOUT<<"'btrack' parameter must be only (for MIP only):\n\t0 - depth first search,\n\t1 - breadth first search,\n\t2 - backtrack using the best projection heuristic\n";
	 return retval;
      }
      lpxIntParam[15]=(int) numtmp;
   }

   //-- Presolver option
   if(PARAM.contains("presol")){
      octave_value tmp=PARAM.contents(PARAM.seek("presol"))(0);
      double numtmp=tmp.scalar_value();
      if((numtmp != 0) && (numtmp != 1)){
         OCTOUT<<"'presol' parameter must be only:\n\t0 - LP presolver is ***NOT*** used,\n\t1 - LP presol is used\n";
	 return retval;
      }
      lpxIntParam[16]=(int) numtmp;
   }

   //-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   //-- Real parameters                                                
   //-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

   //-- Ratio test option
   if(PARAM.contains("relax")){
      octave_value tmp=PARAM.contents(PARAM.seek("relax"))(0);
      lpxRealParam[0]=tmp.scalar_value(); }

   //-- Relative tolerance used to check if the current basic solution
   //-- is primal feasible
   if(PARAM.contains("tolbnd")){
      octave_value tmp=PARAM.contents(PARAM.seek("tolbn"))(0);
      lpxRealParam[1]=tmp.scalar_value(); }

   //-- Absolute tolerance used to check if the current basic solution
   //-- is dual feasible
   if(PARAM.contains("toldj")){
      octave_value tmp=PARAM.contents(PARAM.seek("toldj"))(0);
      lpxRealParam[2]=tmp.scalar_value(); }
		
   //-- Relative tolerance used to choose eligible pivotal elements of
   //--	the simplex table in the ratio test
   if(PARAM.contains("tolpiv")){
      octave_value tmp=PARAM.contents(PARAM.seek("tolpiv"))(0);
      lpxRealParam[3]=tmp.scalar_value(); }
		
   if(PARAM.contains("objll")){
      octave_value tmp=PARAM.contents(PARAM.seek("objll"))(0);
      lpxRealParam[4]=tmp.scalar_value(); }
		
   if(PARAM.contains("objul")){
      octave_value tmp=PARAM.contents(PARAM.seek("objul"))(0);
      lpxRealParam[5]=tmp.scalar_value(); }
		
   if(PARAM.contains("tmlim")){
      octave_value tmp=PARAM.contents(PARAM.seek("tmlim"))(0);
      lpxRealParam[6]=tmp.scalar_value(); }
		
   if(PARAM.contains("outdly")){
      octave_value tmp=PARAM.contents(PARAM.seek("outdly"))(0);
      lpxRealParam[7]=tmp.scalar_value(); }
		
   if(PARAM.contains("tolint")){
      octave_value tmp=PARAM.contents(PARAM.seek("tolint"))(0);
      lpxRealParam[8]=tmp.scalar_value(); }
		
   if(PARAM.contains("tolobj")){
      octave_value tmp=PARAM.contents(PARAM.seek("tolobj"))(0);
      lpxRealParam[9]=tmp.scalar_value(); }
	

   //-- 10th Input. If the problem is a LP problem you may select which solver
   //--      use: RSM (Revised Simplex Method) or IPM (Interior Point Method).
   //--      If the problem is a MIP problem this field will be ignored.
   octave_value tmp=args(9).scalar_value();
   int lpsolver = (int) tmp.scalar_value();

   //-- 11th Input. Saves a copy of the problem if SAVE<>0.
   tmp=args(10).scalar_value();
   int save_pb = (tmp.scalar_value() != 0);

   //-- Assign pointers to the output parameters
   ColumnVector xmin (mrowsc);
   ColumnVector fmin (1);
   ColumnVector status (1);
   ColumnVector lambda (mrowsA);
   ColumnVector redcosts (mrowsc);
   ColumnVector time (1);
   ColumnVector mem (1);
	
   int jmpret = setjmp( mark );
   if (jmpret==0){
      int error=glpk(sense,mrowsc,mrowsA,c,nz,
		     rn.fortran_vec(),cn.fortran_vec(),a.fortran_vec(),b,ctype,
		     freeLB.fortran_vec(),lb,freeUB.fortran_vec(),ub,
		     vartype.fortran_vec(),isMIP,lpsolver,save_pb,
		     xmin.fortran_vec(),fmin.fortran_vec(),status.fortran_vec(),
		     lambda.fortran_vec(),redcosts.fortran_vec(), 
		     time.fortran_vec(),mem.fortran_vec());
   }

   // Set the output parameters
   retval(0)=octave_value(xmin);  // returns xmin
   retval(1)=octave_value(fmin);   // returns fmin
   retval(2)=octave_value(status); // returns status
	
   // Extra informations
   Octave_map extra("lambda",octave_value(lambda)); //returns lambda
   extra.assign("redcosts",octave_value(redcosts)); //returns the reduced costs
   extra.assign("time",octave_value(time)); //time to solve the optimization pb
   extra.assign("mem",octave_value(mem));   //memory used

   retval(3)=extra;
	
   return retval;
}

