/*

Copyright (C) 2004 David Bateman

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

In addition to the terms of the GPL, you are permitted to link
this program with any Open Source program, as defined by the
Open Source Initiative (www.opensource.org)

*/

/*

This is the Octave interface to the UMFPACK code, which bore the following
copyright

  UMFPACK Version 4.3 (Jan. 16, 2004), Copyright (c) 2004 by Timothy A.
  Davis.  All Rights Reserved.  See ../README for License.
  email: davis@cise.ufl.edu    CISE Department, Univ. of Florida.
  web: http://www.cise.ufl.edu/research/sparse/umfpack

*/

#include <cfloat>
#include <cstdlib>
#include <string>

#include <octave/config.h>
#include <octave/ov.h>
#include <octave/defun-dld.h>
#include <octave/pager.h>
#include <octave/ov-re-mat.h>

#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

// External UMFPACK functions in C
extern "C" {
#include "umfpack.h"
}

#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define MAX(a,b) (((a) > (b)) ? (a) : (b))

// Return an error message
static 
void umfpack_error (const char *s, int A_is_complex, int nargout,
		    octave_value_list retval, NDArray Control, 
		    NDArray Info, int status, int do_info)
{
  if (A_is_complex)
    {
      umfpack_zi_report_status (Control.fortran_vec (), status) ;
      umfpack_zi_report_info (Control.fortran_vec (), Info.fortran_vec ()) ;
    }
  else
    {
      umfpack_di_report_status (Control.fortran_vec (), status) ;
      umfpack_di_report_info (Control.fortran_vec (), Info.fortran_vec ()) ;
    }
  if (do_info > 0)
    // return Info
    retval (do_info) = octave_value (Info);

  error (s);
}

DEFUN_DLD (umfpack, args, nargout,
    "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{x}, @var{Info}] =} umfpack (@var{a}, '\\', @var{b})\n\
@deftypefnx {Loadable Function} {[@var{x}, @var{Info}] =} umfpack (@var{a}, '\\', @var{b}, @var{Control})\n\
@deftypefnx {Loadable Function} {[@var{x}, @var{Info}] =} umfpack (@var{a}, @var{Qinit}, '\\', @var{b}, @var{Control})\n\
@deftypefnx {Loadable Function} {[@var{x}, @var{Info}] =} umfpack (@var{a}, @var{Qinit}, '\\', b)\n\
@deftypefnx {Loadable Function} {[@var{x}, @var{Info}] =} umfpack (@var{b}, '/', A) ;\n\
@deftypefnx {Loadable Function} {[@var{x}, @var{Info}] =} umfpack (@var{b}, '/', @var{a}, @var{Control}) ;\n\
@deftypefnx {Loadable Function} {[@var{x}, @var{Info}] =} umfpack (@var{b}, '/', @var{a}, @var{Qinit}) ;\n\
@deftypefnx {Loadable Function} {[@var{x}, @var{Info}] =} umfpack (@var{b}, '/', @var{a}, @var{Qinit}, @var{Control}) ;\n\
@deftypefnx {Loadable Function} {[@var{l}, @var{u}, @var{p}, @var{q}, @var{r}, @var{Info}] =} umfpack (@var{a}) ;\n\
@deftypefnx {Loadable Function} {[@var{l}, @var{u}, @var{p}, @var{q}, @var{r}, @var{Info}] =} umfpack (@var{a}, @var{Control}) ;\n\
@deftypefnx {Loadable Function} {[@var{l}, @var{u}, @var{p}, @var{q}, @var{r}, @var{Info}] =} umfpack (@var{a}, @var{Qinit}) ;\n\
@deftypefnx {Loadable Function} {[@var{l}, @var{u}, @var{p}, @var{q}, @var{r}, @var{Info}] =} umfpack (@var{a}, @var{Qinit}, @var{Control}) ;\n\
@deftypefnx {Loadable Function} {[@var{l}, @var{u}, @var{p}, @var{q}] =} umfpack (@var{a}) ;\n\
@deftypefnx {Loadable Function} {[@var{l}, @var{u}, @var{p}, @var{q}] =} umfpack (@var{a}, @var{Control}) ;\n\
@deftypefnx {Loadable Function} {[@var{l}, @var{u}, @var{p}, @var{q}] =} umfpack (@var{a}, @var{Qinit}) ;\n\
@deftypefnx {Loadable Function} {[@var{l}, @var{u}, @var{p}, @var{q}] =} umfpack (@var{a}, @var{Qinit}, @var{Control}) ;\n\
@deftypefnx {Loadable Function} {[P1, Q1, Fr, Ch, Info] =} umfpack (@var{a}, 'symbolic') ;\n\
@deftypefnx {Loadable Function} {[P1, Q1, Fr, Ch, Info] =} umfpack (@var{a}, 'symbolic', @var{Control}) ;\n\
@deftypefnx {Loadable Function} {[P1, Q1, Fr, Ch, Info] =} umfpack (@var{a}, @var{Qinit}, 'symbolic') ;\n\
@deftypefnx {Loadable Function} {[P1, Q1, Fr, Ch, Info] =} umfpack (@var{a}, @var{Qinit}, 'symbolic', @var{Control});\n\
@deftypefnx {Loadable Function} {@var{Control} =} umfpack ;\n\
\n\
UMFPACK v4.3 is a Octave oct-file for solving sparse linear systems.\n\
\n\
@iftex\n\
@tex\n\
\\vskip 2ex\n\
\\hfil\\vbox{\n\
\\offinterlineskip\n\
\\tabskip=0pt\n\
\\halign{\n\
\\vrule height1.75ex depth1.25ex width 0.6pt #\\tabskip=1em &\n\
\\hfil #\\hfil &\\vrule # & \n\
\\hfil #\\hfil &\\vrule # width 0.6pt \\tabskip=0pt\\cr\n\
\\noalign{\\hrule height 0.6pt}\n\
&     UMFPACK v4.3                  && OCTAVE approximate equivalent  &\\cr\n\
\\noalign{\\hrule} \n\
& x = umfpack (A, '\\', b) ;        &&  x = A \\ b                    &\\cr\n\
&                                   &&                                &\\cr\n\
&x = umfpack (b, '/', A) ;          &&  x = b / A                     &\\cr\n\
&                                   &&                                &\\cr\n\
&[L,U,P,Q] = umfpack (A) ;          &&  [m,n] = size (A) ;            &\\cr\n\
&                                   &&  I = speye (n) ;               &\\cr\n\
&                                   &&  Q = I (:, colamd (A)) ;       &\\cr\n\
&                                   &&  [L,U,P] = lu (A*Q) ;          &\\cr\n\
&                                   &&                                &\\cr\n\
&[L,U,P,Q,R] = umfpack (A) ;        &&  [m,n] = size (A) ;            &\\cr\n\
&                                   &&  I = speye (n) ;               &\\cr\n\
&                                   &&  Q = I (:, colamd (A)) ;       &\\cr\n\
&                                   &&  r = full (sum (abs (A), 2)) ; &\\cr\n\
&                                   &&  r (find (r == 0)) = 1 ;       &\\cr\n\
&                                   &&  R = spdiags (r, 0, m, m) ;    &\\cr\n\
&                                   &&  [L,U,P] = lu ((R\\A)*Q) ;     &\\cr\n\
&                                   &&                                &\\cr\n\
&[P,Q,F,C] = umfpack (A, 'symbolic')&&  [m,n] = size (A) ;            &\\cr\n\
&                                   &&  I = speye (n) ;               &\\cr\n\
&                                   &&  Q = I (:, colamd (A)) ;       &\\cr\n\
&                                   &&  [count,h,parent,post] = ...   &\\cr\n\
&                                   &&  symbfact (A*Q, 'col') ;       &\\cr\n\
\\noalign{\\hrule height 0.6pt}\n\
}}\\hfil\n\
\\vskip 1ex\n\
@end tex\n\
@end iftex\n\
@ifinfo\n\
@multitable @columnfractions 0.43 .02 .43\n\
@item UMFPACK v4.3:                       @tab | \n\
@tab OCTAVE approx. equivalent\n\
@item -------------------------------     @tab | \n\
@tab --------------------------------\n\
@item x = umfpack (A, '\\', b) ;          @tab | \n\
@tab x = A \\ b\n\
@item                                     @tab | \n\
@tab\n\
@item x = umfpack (b, '/', A) ;           @tab | \n\
@tab  x = b / A\n\
@item                                     @tab | \n\
@tab\n\
@item [L,U,P,Q] = umfpack (A) ;           @tab | \n\
@tab  [m,n] = size (A) ;\n\
@item                                     @tab | \n\
@tab  I = speye (n) ;\n\
@item                                     @tab | \n\
@tab  Q = I (:, colamd (A)) ;\n\
@item                                     @tab | \n\
@tab  [L,U,P] = lu (A*Q) ;\n\
@item                                     @tab | \n\
@tab\n\
@item [L,U,P,Q,R] = umfpack (A) ;         @tab | \n\
@tab  [m,n] = size (A) ;\n\
@item                                     @tab | \n\
@tab  I = speye (n) ;\n\
@item                                     @tab | \n\
@tab  Q = I (:, colamd (A)) ;\n\
@item                                     @tab | \n\
@tab  r = full (sum (abs (A), 2)) ;\n\
@item                                     @tab | \n\
@tab  r (find (r == 0)) = 1 ;\n\
@item                                     @tab | \n\
@tab  R = spdiags (r, 0, m, m) ;\n\
@item                                     @tab | \n\
@tab  [L,U,P] = lu ((R\\A)*Q) ;\n\
@item                                     @tab | \n\
@tab\n\
@item [P,Q,F,C] = umfpack (A, 'symbolic') @tab | \n\
@tab  [m,n] = size (A) ; \n\
@item                                     @tab | \n\
@tab  I = speye (n) ;\n\
@item                                     @tab | \n\
@tab  Q = I (:, colamd (A)) ;\n\
@item                                     @tab | \n\
@tab  [count,h,parent,post] = ...\n\
@item                                     @tab | \n\
@tab  symbfact (A*Q, 'col') ;\n\
@end multitable\n\
@end ifinfo\n\
\n\
A must be sparse.  It can be complex, singular, and/or rectangular.\n\
A must be square for '/' or '\\'.  b must be a full real or complex\n\
vector.  For @code{[@var{l}, @var{u}, @var{p}, @var{q}, @var{r}] =\n\
umfpack (@var{a})}, the factorization is @code{@var{l} * @var{u} =\n\
@var{p} * (@var{r} \\ @var{a}) * @var{q}}. If @var{a} has a mostly\n\
symmetric nonzero pattern, then replace @dfn{colamd} with @dfn{amd}\n\
in the OCTAVE-equivalent column in the table above.\n\
\n\
Factor or solve a sparse linear system, returning either the solution\n\
@var{x} to @code{@var{A} * @var{x} = @var{b}} or @code{@var{A}' * @var{x}'\n\
= @var{b}'}, the factorization LU=PAQ, or LU=P(R\\A)Q.  A must be sparse.\n\
For the solve, A must be square and b must be a dense n-by-1 vector.  For LU\n\
factorization, A can be rectangular.  In both cases, A and/or b can be real\n\
or complex.\n\
\n\
UMFPACK analyzes the matrix and selects one of three strategies to factorize\n\
the matrix. It first finds a set of k initial pivot entries of zero\n\
Markowitz cost. This forms the first k rows and columns of L and U. The\n\
remaining submatrix S is then analyzed, based on the symmetry of the nonzero\n\
pattern of the submatrix and the values on the diagaonal.  The strategies\n\
include:\n\
\n\
@table @asis\n\
@item unsymmetric\n\
Use a COLAMD pre-ordering, a column elimination tree\n\
post-ordering, refine the column ordering during factorization,\n\
and make no effort at selecting pivots on the diagonal.\n\
@item 2-by-2\n\
Like the symmetric strategy (see below), except that local\n\
row permutations are first made to attempt to place large entries\n\
on the diagonal.\n\
@item symmetric\n\
Use an AMD pre-ordering on the matrix @code{@var{s} + @var{s}'}, an\n\
elimination tree post-ordering, do not refine the column ordering during\n\
factorization, and attempt to select pivots on the diagonal.\n\
@end table\n\
\n\
Each of the following uses of umfpack (except for 'Control = umfpack') is\n\
stand-alone.  That is, no call to umfpack is required for any subsequent\n\
call. In each usage, the Info output argument is optional.\n\
\n\
Usage:\n\
\n\
[x, Info] = umfpack (A, '\\', b) ;\n\
[x, Info] = umfpack (A, '\\', b, Control) ;\n\
[x, Info] = umfpack (A, Qinit, '\\', b, Control) ;\n\
[x, Info] = umfpack (A, Qinit, '\\', b) ;\n\
\n\
     Solves Ax=b (similar to x = A\\b in OCTAVE).\n\
\n\
[x, Info] = umfpack (b, '/', A) ;\n\
[x, Info] = umfpack (b, '/', A, Control) ;\n\
[x, Info] = umfpack (b, '/', A, Qinit) ;\n\
[x, Info] = umfpack (b, '/', A, Qinit, Control) ;\n\
\n\
     Solves A'x'=b' (similar to x = b/A in OCTAVE).\n\
\n\
[L, U, P, Q, R, Info] = umfpack (A) ;\n\
[L, U, P, Q, R, Info] = umfpack (A, Control) ;\n\
[L, U, P, Q, R, Info] = umfpack (A, Qinit) ;\n\
[L, U, P, Q, R, Info] = umfpack (A, Qinit, Control) ;\n\
\n\
     Returns the LU factorization of A.  P and Q are returned as permutation\n\
     matrices.  R is a diagonal sparse matrix of scale factors for the rows\n\
     of A, L is lower triangular, and U is upper triangular.  The\n\
     factorization is L*U = P*(R\\A)*Q.  You can turn off scaling by setting\n\
     Control (17) to zero (in which case R = speye (m)), or by using the\n\
     following syntaxes (in which case Control (17) is ignored):\n\
\n\
[L, U, P, Q] = umfpack (A) ;\n\
[L, U, P, Q] = umfpack (A, Control) ;\n\
[L, U, P, Q] = umfpack (A, Qinit) ;\n\
[L, U, P, Q] = umfpack (A, Qinit, Control) ;\n\
\n\
     Same as above, except that no row scaling is performed.  The Info array\n\
     is not returned, either.\n\
\n\
[P1, Q1, Fr, Ch, Info] = umfpack (A, 'symbolic') ;\n\
[P1, Q1, Fr, Ch, Info] = umfpack (A, 'symbolic', Control) ;\n\
[P1, Q1, Fr, Ch, Info] = umfpack (A, Qinit, 'symbolic') ;\n\
[P1, Q1, Fr, Ch, Info] = umfpack (A, Qinit, 'symbolic', Control);\n\
\n\
     Performs only the fill-reducing column pre-ordering (including the\n\
     elimination tree post-ordering) and symbolic factorization.  Q1 is the\n\
     initial column permutation (either from colamd, amd, or the input\n\
     ordering Qinit), possibly followed by a column elimination tree post-\n\
     ordering or a symmetric elimination tree post-ordering, depending on\n\
     the strategy used.\n\
\n\
     For the unsymmetric strategy, P1 is the row ordering induced by Q1\n\
     (row-merge order). For the 2-by-2 strategy, P1 is the row ordering that\n\
     places large entries on the diagonal of P1*A*Q1.  For the symmetric\n\
     strategy, P1 = Q1.\n\
\n\
     Fr is a (nfr+1)-by-4 array containing information about each frontal\n\
     matrix, where nfr <= n is the number of frontal matrices.  Fr (:,1) is\n\
     the number of pivot columns in each front, and Fr (:,2) is the parent\n\
     of each front in the supercolumn elimination tree.  Fr (k,2) is zero if\n\
     k is a root.  The first Fr (1,1) columns of P1*A*Q1 are the pivot\n\
     columns for the first front, the next Fr (2,1) columns of P1*A*Q1\n\
     are the pivot columns for the second front, and so on.\n\
\n\
     For the unsymmetric strategy, Fr (:,3) is the row index of the first\n\
     row in P1*A*Q1 whose leftmost nonzero entry is in a pivot column for\n\
     the kth front.  Fr (:,4) is the leftmost descendent of the kth front.\n\
     Rows in the range Fr (Fr (k,4),3) to Fr (k+1,3)-1 form the entire set\n\
     of candidate pivot rows for the kth front (some of these will typically\n\
     have been selected as pivot rows of fronts Fr (k,3) to k-1, before the\n\
     factorization reaches the kth front.  If front k is a leaf node, then\n\
     Fr (k,4) is k.\n\
\n\
     Ch is a (nchains+1)-by-3 array containing information about each\n\
     'chain' (unifrontal sequence) of frontal matrices, and where\n\
      nchains <= nfr is the number of chains. The ith chain consists of\n\
     frontal matrices. Chain (i,1) to Chain (i+1,1)-1, and the largest\n\
     front in chain i is Chain (i,2)-by-Chain (i,3).\n\
\n\
     This use of umfpack is not required to factor or solve a linear system\n\
     in OCTAVE.  It analyzes the matrix A and provides information only.\n\
     The OCTAVE statement @code{treeplot (Fr (:,2)')} plots the column\n\
     elimination tree.\n\
\n\
Control = umfpack ;\n\
\n\
      Returns a 20-by-1 vector of default parameter settings for umfpack.\n\
\n\
umfpack_report (Control, Info) ;\n\
\n\
      Prints the current Control settings, and Info\n\
\n\
If present, Qinit is a user-supplied 1-by-n permutation vector.  It is an\n\
initial fill-reducing column pre-ordering for A; if not present, then colamd\n\
or amd are used instead.  If present, Control is a user-supplied 20-by-1\n\
array.   Control and Info are optional; if Control is not present, defaults\n\
are used.  If a Control entry is NaN, then the default is used for that entry.\n\
\n\
UMFPACK Version 4.3 (Jan. 16, 2004), Copyright @copyright{} 2004 by\n\
Timothy A. Davis.  All Rights Reserved.\n\
\n\
UMFPACK License:\n\
\n\
@example\n\
Your use or distribution of UMFPACK or any modified version of\n\
UMFPACK implies that you agree to this License.\n\
\n\
THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY\n\
EXPRESSED OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.\n\
\n\
Permission is hereby granted to use or copy this program, provided\n\
that the Copyright, this License, and the Availability of the original\n\
version is retained on all copies.  User documentation of any code that\n\
uses UMFPACK or any modified version of UMFPACK code must cite the\n\
Copyright, this License, the Availability note, and 'Used by permission.'\n\
Permission to modify the code and to distribute modified code is granted,\n\
provided the Copyright, this License, and the Availability note are\n\
retained, and a notice that the code was modified is included.  This\n\
software was developed with support from the National Science Foundation,\n\
and is provided to you free of charge.\n\
@end example\n\
\n\
Availability: http://www.cise.ufl.edu/research/sparse/umfpack\n\
@end deftypefn\n\
@seealso{lu_normtest, colamd, amd, umfpack_solve}")
{
  octave_value_list retval;
  int nargin = args.length ();
  int op = 0;
  std::string operation;
  bool do_solve = false;
  int do_info = 0;

  ColumnVector User_Qinit;
  SparseComplexMatrix CAmatrix;
  ComplexMatrix CBmatrix;
  SparseMatrix Amatrix;
  Matrix Bmatrix;
  NDArray User_Control_matrix;

  bool A_is_complex = false;
  bool B_is_complex = false;
  bool X_is_complex = false;
  bool transpose = false;
  bool have_User_Qinit  = false;
  bool have_User_Control_matrix = false;
  bool do_numeric = true;
  bool no_scale = false;

  // find the operator
  for (int i = 0 ; i < nargin ; i++)
    {
      if (args(i).is_string ())
	{
	  op = i;
	  break;
	}
    }

  if (op > 0)
    {
      std::string op_type = args (op).string_value ();

      if (op_type == "\\")
	{

	  // matrix left divide, x = A\b 
	  
	  //  [x, Info] = umfpack (A, '\', b) ;
	  //  [x, Info] = umfpack (A, '\', b, Control) ;
	  //  [x, Info] = umfpack (A, Qinit, '\', b, Control) ;
	  //  [x, Info] = umfpack (A, Qinit, '\', b) ;

	  do_solve = true;
	  operation = "x = A\\b";

	  if (args(0).class_name () != "sparse")
	    {
	      error ("umfpack: input matrix A must be sparse");
	      return retval;
	    }
	  
	  if (args(0).is_complex_type ())
	    {
	      CAmatrix = args(0).sparse_complex_matrix_value ();
	      A_is_complex = true;
	    }
	  else
	    Amatrix = args(0).sparse_matrix_value ();


	  if (args(op+1).is_complex_type ())
	    {
	      CBmatrix = args(op+1).complex_matrix_value ();
	      B_is_complex = true;
	    }
	  else
	    Bmatrix = args(op+1).matrix_value ();

	  if (nargout == 2)
	    do_info = 1;

	  if (op == 2)
	    {
	      User_Qinit = args(1).column_vector_value ();
	      have_User_Qinit = true;
	    }

	  if ((op == 1 && nargin == 4) || (op == 2 && nargin == 5))
	    {
	      User_Control_matrix = args(nargin-1).array_value ();
	      have_User_Control_matrix = true;
	    }

	  if (error_state)
	    {
	      error ("umfpack: incorrect argument type");
	      return retval;
	    }

	  if (nargin < 3 || nargin > 5 || nargout > 2)
	    {
	      error ("umfpack: wrong number of arguments");
	      return retval;
	    }

	}
      else if (op_type == "/")
	{
	  // matrix right divide, x = b/A

	  // [x, Info] = umfpack (b, '/', A) ;
	  // [x, Info] = umfpack (b, '/', A, Control) ;
	  // [x, Info] = umfpack (b, '/', A, Qinit) ;
	  // [x, Info] = umfpack (b, '/', A, Qinit, Control) ;

	  do_solve = true;
	  operation = "x = b/A" ;

	  transpose = true;

	  if (args(2).class_name () != "sparse")
	    {
	      error ("umfpack: input matrix A must be sparse");
	      return retval;
	    }

	  if (args(2).is_complex_type ())
	    {
	      CAmatrix = args(2).sparse_complex_matrix_value ();
	      A_is_complex = true;
	    }
	  else
	    Amatrix = args(2).sparse_matrix_value ();

	  if (args(0).is_complex_type ())
	    {
	      CBmatrix = args(0).complex_matrix_value ();
	      B_is_complex = true;
	    }
	  else
	    Bmatrix = args(0).matrix_value ();

	  if (nargout == 2)
	    do_info = 1;

	  if (nargin == 5)
	    {
	      User_Qinit = args(3).column_vector_value ();
	      User_Control_matrix = args(4).array_value ();
	      have_User_Qinit = true;
	      have_User_Control_matrix = true;
	    }
	  else if (nargin == 4)
	    {
	      User_Control_matrix = args(3).array_value ();

	      if (User_Control_matrix.rows () == 1)
		{
		  User_Qinit = args(3).column_vector_value ();
		  have_User_Qinit = true;
		}
	      else
		have_User_Control_matrix = true;
	    }
	  else if (nargin < 3 || nargin > 5 || nargout > 2)
	    {
	      error ("umfpack: wrong number of arguments");
	      return retval;
	    }

	  if (error_state)
	    {
	      error ("umfpack: incorrect argument type");
	      return retval;
	    }
	}
      else if (op_type == "symbolic")
	{
	  // symbolic factorization only

	  // [P Q Fr Ch Info] = umfpack (A, 'symbolic') ;
	  // [P Q Fr Ch Info] = umfpack (A, 'symbolic', Control) ;
	  // [P Q Fr Ch Info] = umfpack (A, Qinit, 'symbolic') ;
	  // [P Q Fr Ch Info] = umfpack (A, Qinit, 'symbolic', Control) ;

	  operation = "symbolic factorization";
	  do_numeric = false;

	  if (args(0).class_name () != "sparse")
	    {
	      error ("umfpack: input matrix A must be sparse");
	      return retval;
	    }

	  if (args(0).is_complex_type ())
	    {
	      CAmatrix = args(0).sparse_complex_matrix_value ();
	      A_is_complex = true;
	    }
	  else
	    Amatrix = args(0).sparse_matrix_value ();

	  if (nargout == 5)
	    do_info = 4 ;
	  
	  if (op == 2)
	    {
	      User_Qinit = args(1).column_vector_value ();
	      have_User_Qinit = true;
	    }
	  if ((op == 1 && nargin == 3) || (op == 2 && nargin == 4))
	    {
	      User_Control_matrix = args(nargin-1).array_value ();
	      have_User_Control_matrix = true;
	    }

	  if (error_state)
	    {
	      error ("umfpack: incorrect argument type");
	      return retval;
	    }

	  if (nargin < 2 || nargin > 4 || nargout > 5 || nargout < 4)
	    {
	      error ("umfpack: wrong number of arguments") ;
	      return retval;
	    }
	}
      else
	{
	  error ("operator must be '/', '\\', or 'symbolic'") ;
	  return retval;
	}
    }
  else if (nargin > 0)
    {
      // LU factorization

      // with scaling:
      //   [L, U, P, Q, R, Info] = umfpack (A) ;
      //   [L, U, P, Q, R, Info] = umfpack (A, Qinit) ;
      //
      // scaling determined by Control settings:
      //   [L, U, P, Q, R, Info] = umfpack (A, Control) ;
      //   [L, U, P, Q, R, Info] = umfpack (A, Qinit, Control) ;
      //
      // with no scaling:
      //   [L, U, P, Q] = umfpack (A) ;
      //   [L, U, P, Q] = umfpack (A, Control) ;
      //   [L, U, P, Q] = umfpack (A, Qinit) ;
      //   [L, U, P, Q] = umfpack (A, Qinit, Control) ;

      operation = "numeric factorization" ;

      if (args(0).is_complex_type ())
	{
	  CAmatrix = args(0).sparse_complex_matrix_value ();
	  A_is_complex = true;
	}
      else
	Amatrix = args(0).sparse_matrix_value ();

      no_scale = nargout <= 4 ;

      if (nargout == 6)
	    do_info = 5 ;

      if (nargin == 3)
	{
	  User_Qinit = args(1).column_vector_value ();
	  User_Control_matrix = args(2).array_value ();
	  have_User_Qinit = true;
	  have_User_Control_matrix = true;
	}
      else if (nargin == 2)
	{
	  User_Control_matrix = args(1).array_value ();
	  
	  if (User_Control_matrix.rows () == 1)
	    {
	      User_Qinit = args(1).column_vector_value ();
	      have_User_Qinit = true;
	    }
	  else
	    have_User_Control_matrix = true;
	}
      else if (nargin > 3 || nargout > 6 || nargout < 4)
	{
	  error ("umfpack: wrong number of arguments") ;
	  return retval;
	}
    }
  else
    {
      // return default control settings

      // Control = umfpack ;
      // umfpack ;

      if (nargout > 1)
	{
	  error ("umfpack: wrong number of arguments") ;
	  return retval;
	}
      
      NDArray user_control (dim_vector (UMFPACK_CONTROL, 1));
      double *user_control_ptr = user_control.fortran_vec ();
      umfpack_di_defaults (user_control_ptr);
      retval(0) = user_control;
      return retval;
    }

  // check inputs
  
  int n_row = Amatrix.rows ();
  int n_col = Amatrix.cols ();
  int nn = MAX (n_row, n_col) ;
  int n_inner = MIN (n_row, n_col) ;
  if (do_solve && n_row != n_col)
    {
      error ("umfpack: input matrix A must square for '\\' or '/'") ;
      return retval;
    }
  if (n_row == 0 || n_col == 0)
    {
      error ("umfpack: input matrix A cannot have zero rows or zero columns") ;
      return retval;
    }

  /* The real/complex status of A determines which version to use, */
  /* (umfpack_di_* or umfpack_zi_*). */
  const int *Ap;
  const int *Ai;
  const double *Ax;
  const double *Bx;
  
  if (A_is_complex)
    {
      Ap = CAmatrix.cidx ();
      Ai = CAmatrix.ridx ();
      Ax = X_CAST (const double *, CAmatrix.data ());
    }
  else
    {
      Ap = Amatrix.cidx ();
      Ai = Amatrix.ridx ();
      Ax = Amatrix.data ();
    }

  if (B_is_complex)
    Bx = X_CAST (const double *, CBmatrix.fortran_vec ());
  else
    Bx = Bmatrix.fortran_vec ();

  if (do_solve)
    {
      int b_row = Bmatrix.rows ();
      int b_col = Bmatrix.cols ();

      if (!((transpose && b_row == 1 && b_col == nn) ||  
	     (!transpose && b_row == nn && b_col == 1)))  
	{
	  error ("umfpack: b has the wrong dimensions") ;
	  return retval;
	}

      X_is_complex = A_is_complex || B_is_complex ;
    }

  // set the Control parameters
  NDArray Control (dim_vector (UMFPACK_CONTROL, 1));
  double *Control_ptr = Control.fortran_vec ();
  if (A_is_complex)
    umfpack_zi_defaults (Control_ptr) ;
  else
    umfpack_di_defaults (Control_ptr) ;

  if (have_User_Control_matrix)
    {
      int size = MIN (UMFPACK_CONTROL, User_Control_matrix.length ());
      for (int i = 0 ; i < size ; i++)
	Control (i) = User_Control_matrix (i) ;
    }
  
  if (no_scale)
    {
      // turn off scaling for [L, U, P, Q] = umfpack (A) ;
      // ignoring the input value of Control (24) for the usage
      // [L, U, P, Q] = umfpack (A, Control) ;
      Control (UMFPACK_SCALE) = UMFPACK_SCALE_NONE ;
    }

  int print_level;
  if (xisnan (Control (UMFPACK_PRL)))
    print_level = UMFPACK_DEFAULT_PRL ;
  else
    print_level = int (Control (UMFPACK_PRL)) ;

  Control (UMFPACK_PRL) = print_level ;

  // check Qinit, if present
  int *Qinit = NULL;
  if (have_User_Qinit)
    {
      if(User_Qinit.rows () != 1 || User_Qinit.cols () != n_col)
	{
	  error ("umfpack: Qinit must be 1-by-n_col") ;
	  return retval;
	}

      Qinit = new int [n_col];
      for (int i = 0; i < n_col; i++)
	Qinit[i] = static_cast<int> (User_Qinit (i));
    }

  // report the inputs A and Qinit

  if (print_level >= 2)
    // print the operation
    octave_stdout << "\numfpack: " << operation;

  if (A_is_complex)
    {
      umfpack_zi_report_control (Control_ptr) ;

      if (print_level >= 3) 
	octave_stdout << "\nA: " ;

      umfpack_zi_report_matrix (n_row, n_col, Ap, Ai, Ax, NULL,
				1, Control_ptr) ;
      if (have_User_Qinit)
	{
	  if (print_level >= 3) 
	    octave_stdout << "\nQinit: " ;

	  umfpack_zi_report_perm (n_col, Qinit, Control_ptr) ;
	}
    }
  else
    {
      umfpack_di_report_control (Control_ptr) ;

      if (print_level >= 3) 
	octave_stdout << "\nA: " ;

       umfpack_di_report_matrix (n_row, n_col, Ap, Ai, Ax,
				 1, Control_ptr) ;
       if (have_User_Qinit)
	 {
	   if (print_level >= 3) 
	     octave_stdout << "\nQinit: " ;

	   umfpack_di_report_perm (n_col, Qinit, Control_ptr) ;
	 }
    }

#ifndef NO_TRANSPOSE_FORWARD_SLASH
  // create the array transpose for x = b/A
  if (transpose)
    if (A_is_complex)
      {
	CAmatrix = CAmatrix.transpose ();
	Ap = Amatrix.cidx ();
	Ai = Amatrix.ridx ();
	Ax = X_CAST (const double *, CAmatrix.data ());
      }
    else
      {
	Amatrix = Amatrix.transpose ();
	Ap = Amatrix.cidx ();
	Ai = Amatrix.ridx ();
	Ax = Amatrix.data ();
      }
#endif

  // perform the symbolic factorization
  
  NDArray InfoOut (dim_vector (1, UMFPACK_INFO));
  double * Info = InfoOut.fortran_vec ();
  void *Symbolic;
  int status, status2;
  if (A_is_complex)
    status = umfpack_zi_qsymbolic (n_row, n_col, Ap, Ai, Ax, NULL,
				   Qinit, &Symbolic, Control_ptr, 
				   Info);
  else
    status = umfpack_di_qsymbolic (n_row, n_col, Ap, Ai, Ax,
				   Qinit, &Symbolic, Control_ptr, 
				   Info);
  
  if (status < 0)
    {
      umfpack_error ("symbolic factorization failed", A_is_complex, 
		     nargout, retval, Control, InfoOut, status, do_info) ;
      return retval;
    }

  if (have_User_Qinit)
    delete [] Qinit;

  // report the Symbolic object

  if (A_is_complex)
    umfpack_zi_report_symbolic (Symbolic, Control_ptr) ;
  else
    umfpack_di_report_symbolic (Symbolic, Control_ptr) ;

  // perform numeric factorization, or just return symbolic factorization

  if (do_numeric)
    {
      // perform the numeric factorization
      void *Numeric;

      if (A_is_complex)
	status = umfpack_zi_numeric (Ap, Ai, Ax, NULL, Symbolic, &Numeric,
				     Control_ptr, Info) ;
      else
	status = umfpack_di_numeric (Ap, Ai, Ax, Symbolic, &Numeric,
				     Control_ptr, Info) ;

      // free the symbolic factorization
      if (A_is_complex)
	umfpack_zi_free_symbolic (&Symbolic) ;
      else
	umfpack_di_free_symbolic (&Symbolic) ;

      // report the Numeric object
      if (status < 0)
	{
	  umfpack_error ("numeric factorization failed", A_is_complex, 
			 nargout, retval, Control, InfoOut, status, do_info);
	  return retval;
	}

      if (A_is_complex)
	(void) umfpack_zi_report_numeric (Numeric, Control_ptr) ;
      else
	(void) umfpack_di_report_numeric (Numeric, Control_ptr) ;

      // return the solution or the factorization

      if (do_solve)
	{
	  int sys;
	  ComplexNDArray Xcmplx;
	  NDArray Xreal;

	  // solve Ax=b or A'x'=b', and return just the solution x

#ifndef NO_TRANSPOSE_FORWARD_SLASH
	  if (transpose)
	    {
	      // A.'x.'=b.' gives the same x=b/A as solving A'x'=b'
	      // since C=A.' was factorized, solve with sys = UMFPACK_A 
	      // since x and b are vectors, x.' and b.' are implicit 
	      if (X_is_complex)
		Xcmplx.resize (dim_vector (1, nn));
	      else
		Xreal.resize (dim_vector (1, nn));
	    }
	  else
	    {
	      if (X_is_complex)
		Xcmplx.resize (dim_vector (nn, 1));
	      else
		Xreal.resize (dim_vector (nn, 1));
	    }

	  sys = UMFPACK_A ;
#else
	  if (transpose)
	    {
	      // If A is real, A'x=b is the same as A.'x=b. 
	      // x and b are vectors, so x and b are the same as x' and b'. 
	      // If A is complex, then A.'x.'=b.' gives the same solution x 
	      // as the complex conjugate transpose.  If we used the A'x=b 
	      // option in umfpack_*_solve, we would have to form b' on 
	      // input and x' on output (negating the imaginary part). 
	      // We can save this work by just using the A.'x=b option in 
	      // umfpack_*_solve.  Then, forming x.' and b.' is implicit, 
	      // since x and b are just vectors anyway. 
	      // In both cases, the system to solve is A.'x=b
	      if (X_is_complex)
		Xcmplx.resize (dim_vector (1, nn));
	      else
		Xreal.resize (dim_vector (1, nn));

	      sys = UMFPACK_Aat ;
	    }
	  else
	    {
	      if (X_is_complex)
		Xcmplx.resize (dim_vector (nn, 1));
	      else
		Xreal.resize (dim_vector (nn, 1));
	      sys = UMFPACK_A ;
	    }
#endif

	  // print the right-hand-side, B
	  if (print_level >= 3) 
	    octave_stdout << "\nright-hand side, b: ";

	  if (B_is_complex)
	    (void) umfpack_zi_report_vector (nn, Bx, NULL, Control_ptr) ;
	  else
	    (void) umfpack_di_report_vector (nn, Bx, Control_ptr) ;

	  // solve the system
	  double * Xx;
	  if (X_is_complex)
	    Xx = X_CAST (double *, Xcmplx.fortran_vec ());
	  else
	    Xx = Xreal.fortran_vec ();
	  status2 = UMFPACK_OK ;

	  if (A_is_complex)
	    {
	      if (!B_is_complex)
		{
		  OCTAVE_LOCAL_BUFFER (double, Bz, nn);
		  for (int i = 0; i < nn; i++)
		    Bz[i] = 0.;

		  status = umfpack_zi_solve (sys, Ap, Ai, Ax, NULL, Xx, NULL, 
					     Bx, Bz, Numeric, Control_ptr, 
					     Info);
		}
	      else
		status = umfpack_zi_solve (sys, Ap, Ai, Ax, NULL, Xx, NULL, 
					   Bx, NULL, Numeric, Control_ptr, 
					   Info);
	    }
	  else
	    {
	      if (B_is_complex)
		{
		  // Ax=b when b is complex and A is sparse can be split
		  // into two systems, A*xr=br and A*xi=bi, where r denotes
		  // the real part and i the imaginary part of x and b.
		  OCTAVE_LOCAL_BUFFER (double, Tx, nn);
		  OCTAVE_LOCAL_BUFFER (double, Tz, nn);

		  status = umfpack_di_solve (sys, Ap, Ai, Ax, Tx, Bx,
					     Numeric, Control_ptr, Info);
		  status2 = umfpack_di_solve (sys, Ap, Ai, Ax, Tz, Bx,
					      Numeric, Control_ptr, Info) ;

		  for (int i = 0; i < nn; i++)
		    Xcmplx (i) = Complex (Tx[i], Tz[i]);
		}
	      else
		status = umfpack_di_solve (sys, Ap, Ai, Ax, Xx, Bx,
					   Numeric, Control_ptr, Info);
	    }
	  
	  // free the Numeric object
	  if (A_is_complex)
	    umfpack_zi_free_numeric (&Numeric) ;
	  else
	    umfpack_di_free_numeric (&Numeric) ;
	  
	  // check error status
	  if (status < 0 || status2 < 0)
	    {
	      umfpack_error ("solve failed", A_is_complex, nargout, 
			     retval, Control, InfoOut, status, do_info) ;
	      return retval;
	    }

	  // print the solution, X
	  if (print_level >= 3) 
	    octave_stdout << "\nsolution, x: ";

	  if (X_is_complex)
	    (void) umfpack_zi_report_vector (nn, Xx, NULL, Control_ptr);
	  else
	    (void) umfpack_di_report_vector (nn, Xx, Control_ptr);

	  // warn about singular or near-singular matrices
	  // no warning is given if Control (1) is zero
	  if (Control (UMFPACK_PRL) >= 1)
	    {
	      if (status == UMFPACK_WARNING_singular_matrix)
		{
		  warning ("matrix is singular");
		  warning ("Try increasing Control (%d) and Control (%d).",
			   1+UMFPACK_PIVOT_TOLERANCE, 
			   1+UMFPACK_SYM_PIVOT_TOLERANCE);
		  warning ("(Suppress this warning with Control (%d) = 0.)",
			   1+UMFPACK_PRL);
		}
	      else if (InfoOut (UMFPACK_RCOND) < DBL_EPSILON)
		{
		  warning ("matrix is nearly singular, rcond = %g",
			   InfoOut (UMFPACK_RCOND));
		  warning ("Try increasing Control (%d) and Control (%d).",
			   1+UMFPACK_PIVOT_TOLERANCE,
			   1+UMFPACK_SYM_PIVOT_TOLERANCE);
		  warning ("(Suppress this warning with Control (%d) = 0.)",
			   1+UMFPACK_PRL);
		}
	    }

	  // Setup the return value
	  if (X_is_complex)
	    retval (0) = octave_value (Xcmplx);
	  else
	    retval (0) = octave_value (Xreal);
	}
      else
	{
	  // get L, U, P, Q, and r
	  int lnz, unz, ignore1, ignore2, ignore3;

	  if (A_is_complex)
	    status = umfpack_zi_get_lunz (&lnz, &unz, &ignore1, &ignore2,
					  &ignore3, Numeric) ;
	  else
	    status = umfpack_di_get_lunz (&lnz, &unz, &ignore1, &ignore2,
					  &ignore3, Numeric) ;

	  if (status < 0)
	    {
	      if (A_is_complex)
		umfpack_zi_free_numeric (&Numeric) ;
	      else
		umfpack_di_free_numeric (&Numeric) ;

	      umfpack_error ("extracting LU factors failed", A_is_complex, 
			     nargout, retval, Control, InfoOut, status, 
			     do_info);
	      return retval;
	    }

	  // avoid malloc of zero-sized arrays
	  lnz = MAX (lnz, 1) ;
	  unz = MAX (unz, 1) ;

	  // get space for the *** ROW *** form of L
	  SparseMatrix Lreal;
	  SparseComplexMatrix Limag;
	  int *Ltp, *Ltj;
	  double *Ltx;
	  if (A_is_complex)
	    {
	      Limag = SparseComplexMatrix (n_inner, n_row, lnz);
	      Ltp = Limag.cidx ();
	      Ltj = Limag.ridx ();
	      Ltx = X_CAST (double *, Limag.data ());
	    }
	  else
	    {
	      Lreal = SparseMatrix (n_inner, n_row, lnz);
	      Ltp = Lreal.cidx ();
	      Ltj = Lreal.ridx ();
	      Ltx = Lreal.data ();
	    }

	  // create permanent copy of the output matrix U
	  int *Up, *Ui;
	  double *Ux;
	  SparseMatrix Ureal;
	  SparseComplexMatrix Uimag;

	  if (A_is_complex)
	    {
	      Uimag = SparseComplexMatrix (n_inner, n_col, unz);
	      Up = Uimag.cidx ();
	      Ui = Uimag.ridx ();
	      Ux = X_CAST (double *, Uimag.data ());
	    }
	  else
	    {
	      Ureal = SparseMatrix (n_inner, n_col, unz);
	      Up = Ureal.cidx ();
	      Ui = Ureal.ridx ();
	      Ux = Ureal.data ();
	    }

	  // temporary space for the integer permutation vectors
	  OCTAVE_LOCAL_BUFFER (int, P, n_row);
	  OCTAVE_LOCAL_BUFFER (int, Q, n_col);

	  // get scale factors, if requested 
	  status2 = UMFPACK_OK ;
	  SparseMatrix Rsout;
	  double * Rs;
	  if (!no_scale)
	    {
	      // create a diagonal sparse matrix for the scale factors 
	      Rsout = SparseMatrix (n_row, n_row, n_row);
	      for (int i = 0 ; i < n_row ; i++)
		{
		  Rsout.cidx (i) = i;
		  Rsout.ridx (i) = i;
		}
	      Rsout.cidx (n_row) = n_row;
	      Rs = Rsout.data ();
	    }
	  else
	    Rs = (double *) NULL ;

	  // get Lt, U, P, Q, and Rs from the numeric object
	  int do_recip;
	  if (A_is_complex)
	    {
	      status = umfpack_zi_get_numeric (Ltp, Ltj, Ltx, NULL, Up, Ui, 
					       Ux, NULL, P, Q, 
					       (double *) NULL, 
					       (double *) NULL,
					       &do_recip, Rs, Numeric) ;
	      umfpack_zi_free_numeric (&Numeric) ;
	    }
	  else
	    {
	      status = umfpack_di_get_numeric (Ltp, Ltj, Ltx, Up, Ui,
					       Ux, P, Q, (double *) NULL,
					       &do_recip, Rs, Numeric) ;
	      umfpack_di_free_numeric (&Numeric) ;
	    }

	  if (!no_scale)
	    retval (4) = octave_vale (Rsout);

	  // for the oct-file, -DNRECIPROCAL must be set,
	  // so do_recip must be FALSE

	  if (status < 0 || status2 < 0 || do_recip)
	    {
	      umfpack_error ("extracting LU factors failed", A_is_complex, 
			     nargout, retval, Control, InfoOut, status, 
			     do_info);
	      return retval;
	    }

	  if (A_is_complex)
	    retval (1) = octave_value (Uimag);
	  else
	    retval (1) = octave_valye (Ureal);
	  
	  // create sparse permutation matrix for P 
	  SparseMatrix Pout (n_row, n_row, n_row);
	  for (int k = 0 ; k < n_row ; k++)
	    {
	      Pout.cidx (k) = k ;
	      Pout.ridx (P [k]) = k;
	      Pout.data (k) = 1;
	    }
	  Pout.cidx (n_row) = n_row;
	  retval (2) = octave_value (Pout);

	  // create sparse permutation matrix for Q 
	  SparseMatrix Qout (n_col, n_col, n_col);
	  for (int k = 0 ; k < n_col ; k++)
	    {
	      Qout.cidx (k) = k ;
	      Qout.ridx (k) = Q[k];
	      Qout.data (k) = 1;
	    }
	  Qout.cidx (n_col) = n_col;
	  retval (3) = octave_value (Qout);
	  
	  // permanent copy of L 
	  if (A_is_complex)
	    retval (0) = octave_value (Limag.transpose());
	  else
	    retval (0) = octave_value (Lreal.transpose());

	  if (status < 0)
	    {
	      umfpack_error ("constructing L failed", A_is_complex, 
			     nargout, retval, Control, InfoOut, status, 
			     do_info) ;
	      return octave_value ();
	    }

	  // print L, U, P, and Q
	  if (A_is_complex)
	    {
	      if (print_level >= 3) 
		{
		  octave_stdout << "\nL: ";
		  int *Lp = Limag.cidx ();
		  int *Li = Limag.ridx ();
		  double *Lx = X_CAST (double *, Limag.data ());

		  (void) umfpack_zi_report_matrix (n_row, n_inner, Lp, Li,
						   Lx, NULL, 1, Control_ptr) ;
		}

	      if (print_level >= 3) 
		octave_stdout << "\nU: ";
	      (void) umfpack_zi_report_matrix (n_inner, n_col, Up, Ui,
					       Ux, NULL, 1, Control_ptr) ;
	      if (print_level >= 3)
		octave_stdout << "\nP: ";
	      (void) umfpack_zi_report_perm (n_row, P, Control_ptr);
	      if (print_level >= 3) 
		octave_stdout << "\nQ: ";
	      (void) umfpack_zi_report_perm (n_col, Q, Control_ptr);
	    }
	  else
	    {
	      if (print_level >= 3) 
		{
		  int *Lp = Lreal.cidx ();
		  int *Li = Lreal.ridx ();
		  double *Lx = Lreal.data ();
		  octave_stdout << "\nL: ";
		  (void) umfpack_di_report_matrix (n_row, n_inner, Lp, Li,
						   Lx, 1, Control_ptr);
		}

	      if (print_level >= 3) 
		octave_stdout << "\nU: ";
	      (void) umfpack_di_report_matrix (n_inner, n_col, Up, Ui,
					       Ux, 1, Control_ptr);
	      if (print_level >= 3) 
		octave_stdout << "\nP: ";
	      (void) umfpack_di_report_perm (n_row, P, Control_ptr);
	      if (print_level >= 3) 
		octave_stdout << "\nQ: ";
	      (void) umfpack_di_report_perm (n_col, Q, Control_ptr);
	    }
	}
    }
  else
    {
      // return the symbolic factorization
      int ignore1, ignore2, ignore3;
      OCTAVE_LOCAL_BUFFER (int, Q, n_col);
      OCTAVE_LOCAL_BUFFER (int, P, n_row);
      OCTAVE_LOCAL_BUFFER (int, Front_npivcol, nn + 1);
      OCTAVE_LOCAL_BUFFER (int, Front_parent, nn + 1);
      OCTAVE_LOCAL_BUFFER (int, Front_1strow, nn + 1);
      OCTAVE_LOCAL_BUFFER (int, Front_leftmostdesc, nn + 1);
      OCTAVE_LOCAL_BUFFER (int, Chain_start, nn + 1);
      OCTAVE_LOCAL_BUFFER (int, Chain_maxrows, nn + 1);
      OCTAVE_LOCAL_BUFFER (int, Chain_maxcols, nn + 1);

      int nz, nfronts, nchains;

      if (A_is_complex)
	{
	  status = umfpack_zi_get_symbolic (&ignore1, &ignore2, &ignore3,
					    &nz, &nfronts, &nchains, P, Q, 
					    Front_npivcol, Front_parent, 
					    Front_1strow, 
					    Front_leftmostdesc,
					    Chain_start, Chain_maxrows, 
					    Chain_maxcols, Symbolic) ;
	  umfpack_zi_free_symbolic (&Symbolic) ;
	}
      else
	{
	  status = umfpack_di_get_symbolic (&ignore1, &ignore2, &ignore3,
					    &nz, &nfronts, &nchains, P, Q, 
					    Front_npivcol, Front_parent, 
					    Front_1strow, 
					    Front_leftmostdesc,
					    Chain_start, Chain_maxrows, 
					    Chain_maxcols, Symbolic) ;
	  umfpack_di_free_symbolic (&Symbolic) ;
	}

      if (status < 0)
	{
	  umfpack_error ("extracting symbolic factors failed", 
			 A_is_complex, nargout, retval, Control, 
			 InfoOut, status, do_info) ;
	  return retval;
	}
      
      // create sparse permutation matrix for P
      SparseMatrix Pout (n_row, n_row, n_row);
      for (int k = 0 ; k < n_row ; k++)
	{
	  Pout.cidx (k) = k;
	  Pout.ridx (P [k]) = k;
	  Pout.data (k) = 1;
	}
      Pout.cidx (n_row) = n_row;
      retval (0) = octave_value (Pout);

      // create sparse permutation matrix for Q 
      SparseMatrix Qout (n_col, n_col, n_col);
      for (int k = 0 ; k < n_col ; k++)
	{
	  Qout.cidx (k) = k;
	  Qout.ridx (k) = Q[k];
	  Qout.data (k) = 1;
	}
      Qout.cidx (n_col) = n_col;
      retval (1) = octave_value (Qout);
      
      // create Fr 
      Matrix Frout (nfronts + 1, 4);
      for (int i = 0 ; i <= nfronts ; i++)
	{
	  // convert parent, 1strow, and leftmostdesc to 1-based 
	  Frout (i, 0) = (double) (Front_npivcol [i]) ;
	  Frout (i, 1) = (double) (Front_parent [i] + 1) ;
	  Frout (i, 2) = (double) (Front_1strow [i] + 1) ;
	  Frout (i, 3) = (double) (Front_leftmostdesc [i] + 1) ;
	}
      retval (2) = octave_value (Frout);
      
      // create Ch 
      Matrix Chout (nchains + 1, 3);
      for (int i = 0 ; i <= nchains ; i++)
	{
	  // convert to 1-based 
	  Chout (i, 0) = (double) (Chain_start [i] + 1) ;
	  Chout (i, 1) = (double) (Chain_maxrows [i]) ;
	  Chout (i, 2) = (double) (Chain_maxcols [i]) ;
	}
      Chout (0, nchains) = (double) Chain_start [nchains] + 1 ;
      Chout (1, nchains) = 0.;
      Chout (2, nchains) = 0.;
      retval (3) = octave_value (Chout);
    }

  // report Info
  if (A_is_complex)
    umfpack_zi_report_info (Control_ptr, Info);
  else
    umfpack_di_report_info (Control_ptr, Info);

  if (do_info > 0)
    retval (do_info) = InfoOut;

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
