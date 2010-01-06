/*

Copyright (C) 2005, 2008, 2009 David Bateman

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

#include "ov.h"
#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "quit.h"
#include "variables.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"
#include "oct-map.h"
#include "pager.h"
#include "unwind-prot.h"

#include "eigs-base.cc"

// Global pointer for user defined function.
static octave_function *eigs_fcn = 0;

// Have we warned about imaginary values returned from user function?
static bool warned_imaginary = false;

// Is this a recursive call?
static int call_depth = 0;

ColumnVector
eigs_func (const ColumnVector &x, int &eigs_error)
{
  ColumnVector retval;
  octave_value_list args;
  args(0) = x;

  if (eigs_fcn)
    {
      octave_value_list tmp = eigs_fcn->do_multi_index_op (1, args);

      if (error_state)
	{
	  eigs_error = 1;
	  gripe_user_supplied_eval ("eigs");
	  return retval;
	}

      if (tmp.length () && tmp(0).is_defined ())
	{
	  if (! warned_imaginary && tmp(0).is_complex_type ())
	    {
	      warning ("eigs: ignoring imaginary part returned from user-supplied function");
	      warned_imaginary = true;
	    }

	  retval = ColumnVector (tmp(0).vector_value ());

	  if (error_state)
	    {
	      eigs_error = 1;
	      gripe_user_supplied_eval ("eigs");
	    }
	}
      else
	{
	  eigs_error = 1;
	  gripe_user_supplied_eval ("eigs");
	}
    }

  return retval;
}

ComplexColumnVector
eigs_complex_func (const ComplexColumnVector &x, int &eigs_error)
{
  ComplexColumnVector retval;
  octave_value_list args;
  args(0) = x;

  if (eigs_fcn)
    {
      octave_value_list tmp = eigs_fcn->do_multi_index_op (1, args);

      if (error_state)
	{
	  eigs_error = 1;
	  gripe_user_supplied_eval ("eigs");
	  return retval;
	}

      if (tmp.length () && tmp(0).is_defined ())
	{
	  retval = ComplexColumnVector (tmp(0).complex_vector_value ());

	  if (error_state)
	    {
	      eigs_error = 1;
	      gripe_user_supplied_eval ("eigs");
	    }
	}
      else
	{
	  eigs_error = 1;
	  gripe_user_supplied_eval ("eigs");
	}
    }

  return retval;
}

DEFUN_DLD (eigs, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {@var{d} =} eigs (@var{a})\n\
@deftypefnx {Loadable Function} {@var{d} =} eigs (@var{a}, @var{k})\n\
@deftypefnx {Loadable Function} {@var{d} =} eigs (@var{a}, @var{k}, @var{sigma})\n\
@deftypefnx {Loadable Function} {@var{d} =} eigs (@var{a}, @var{k}, @var{sigma},@var{opts})\n\
@deftypefnx {Loadable Function} {@var{d} =} eigs (@var{a}, @var{b})\n\
@deftypefnx {Loadable Function} {@var{d} =} eigs (@var{a}, @var{b}, @var{k})\n\
@deftypefnx {Loadable Function} {@var{d} =} eigs (@var{a}, @var{b}, @var{k}, @var{sigma})\n\
@deftypefnx {Loadable Function} {@var{d} =} eigs (@var{a}, @var{b}, @var{k}, @var{sigma}, @var{opts})\n\
@deftypefnx {Loadable Function} {@var{d} =} eigs (@var{af}, @var{n})\n\
@deftypefnx {Loadable Function} {@var{d} =} eigs (@var{af}, @var{n}, @var{b})\n\
@deftypefnx {Loadable Function} {@var{d} =} eigs (@var{af}, @var{n}, @var{k})\n\
@deftypefnx {Loadable Function} {@var{d} =} eigs (@var{af}, @var{n}, @var{b}, @var{k})\n\
@deftypefnx {Loadable Function} {@var{d} =} eigs (@var{af}, @var{n}, @var{k}, @var{sigma})\n\
@deftypefnx {Loadable Function} {@var{d} =} eigs (@var{af}, @var{n}, @var{b}, @var{k}, @var{sigma})\n\
@deftypefnx {Loadable Function} {@var{d} =} eigs (@var{af}, @var{n}, @var{k}, @var{sigma}, @var{opts})\n\
@deftypefnx {Loadable Function} {@var{d} =} eigs (@var{af}, @var{n}, @var{b}, @var{k}, @var{sigma}, @var{opts})\n\
@deftypefnx {Loadable Function} {[@var{v}, @var{d}] =} eigs (@var{a}, @dots{})\n\
@deftypefnx {Loadable Function} {[@var{v}, @var{d}] =} eigs (@var{af}, @var{n}, @dots{})\n\
@deftypefnx {Loadable Function} {[@var{v}, @var{d}, @var{flag}] =} eigs (@var{a}, @dots{})\n\
@deftypefnx {Loadable Function} {[@var{v}, @var{d}, @var{flag}] =} eigs (@var{af}, @var{n}, @dots{})\n\
Calculate a limited number of eigenvalues and eigenvectors of @var{a},\n\
based on a selection criteria.  The number eigenvalues and eigenvectors to\n\
calculate is given by @var{k} whose default value is 6.\n\
\n\
By default @code{eigs} solve the equation\n\
@tex\n\
$A \\nu = \\lambda \\nu$\n\
@end tex\n\
@ifinfo\n\
@code{A * v = lambda * v}\n\
@end ifinfo\n\
, where\n\
@tex\n\
$\\lambda$ is a scalar representing one of the eigenvalues, and $\\nu$\n\
@end tex\n\
@ifinfo\n\
@code{lambda} is a scalar representing one of the eigenvalues, and @code{v}\n\
@end ifinfo\n\
is the corresponding eigenvector.  If given the positive definite matrix\n\
@var{B} then @code{eigs} solves the general eigenvalue equation\n\
@tex\n\
$A \\nu = \\lambda B \\nu$\n\
@end tex\n\
@ifinfo\n\
@code{A * v = lambda * B * v}\n\
@end ifinfo\n\
.\n\
\n\
The argument @var{sigma} determines which eigenvalues are returned.\n\
@var{sigma} can be either a scalar or a string.  When @var{sigma} is a scalar,\n\
the @var{k} eigenvalues closest to @var{sigma} are returned.  If @var{sigma}\n\
is a string, it must have one of the values\n\
\n\
@table @asis\n\
@item 'lm'\n\
Largest magnitude (default).\n\
\n\
@item 'sm'\n\
Smallest magnitude.\n\
\n\
@item 'la'\n\
Largest Algebraic (valid only for real symmetric problems).\n\
\n\
@item 'sa'\n\
Smallest Algebraic (valid only for real symmetric problems).\n\
\n\
@item 'be'\n\
Both ends, with one more from the high-end if @var{k} is odd (valid only for\n\
real symmetric problems).\n\
\n\
@item 'lr'\n\
Largest real part (valid only for complex or unsymmetric problems).\n\
\n\
@item 'sr'\n\
Smallest real part (valid only for complex or unsymmetric problems).\n\
\n\
@item 'li'\n\
Largest imaginary part (valid only for complex or unsymmetric problems).\n\
\n\
@item 'si'\n\
Smallest imaginary part (valid only for complex or unsymmetric problems).\n\
@end table\n\
\n\
If @var{opts} is given, it is a structure defining some of the options that\n\
@code{eigs} should use.  The fields of the structure @var{opts} are\n\
\n\
@table @code\n\
@item issym\n\
If @var{af} is given, then flags whether the function @var{af} defines a\n\
symmetric problem.  It is ignored if @var{a} is given.  The default is false.\n\
\n\
@item isreal\n\
If @var{af} is given, then flags whether the function @var{af} defines a\n\
real problem.  It is ignored if @var{a} is given.  The default is true.\n\
\n\
@item tol\n\
Defines the required convergence tolerance, given as @code{tol * norm (A)}.\n\
The default is @code{eps}.\n\
\n\
@item maxit\n\
The maximum number of iterations.  The default is 300.\n\
\n\
@item p\n\
The number of Lanzcos basis vectors to use.  More vectors will result in\n\
faster convergence, but a larger amount of memory.  The optimal value of 'p'\n\
is problem dependent and should be in the range @var{k} to @var{n}.  The\n\
default value is @code{2 * @var{k}}.\n\
\n\
@item v0\n\
The starting vector for the computation.  The default is to have @sc{arpack}\n\
randomly generate a starting vector.\n\
\n\
@item disp\n\
The level of diagnostic printout.  If @code{disp} is 0 then there is no\n\
printout.  The default value is 1.\n\
\n\
@item cholB\n\
Flag if @code{chol (@var{b})} is passed rather than @var{b}.  The default is\n\
false.\n\
\n\
@item permB\n\
The permutation vector of the Cholesky factorization of @var{b} if\n\
@code{cholB} is true.  That is @code{chol ( @var{b} (permB, permB))}.  The\n\
default is @code{1:@var{n}}.\n\
\n\
@end table\n\
\n\
It is also possible to represent @var{a} by a function denoted @var{af}.\n\
@var{af} must be followed by a scalar argument @var{n} defining the length\n\
of the vector argument accepted by @var{af}.  @var{af} can be passed either\n\
as an inline function, function handle or as a string.  In the case where\n\
@var{af} is passed as a string, the name of the string defines the function\n\
to use.\n\
\n\
@var{af} is a function of the form @code{function y = af (x), y = @dots{};\n\
endfunction}, where the required return value of @var{af} is determined by\n\
the value of @var{sigma}, and are\n\
\n\
@table @code\n\
@item A * x\n\
If @var{sigma} is not given or is a string other than 'sm'.\n\
\n\
@item A \\ x\n\
If @var{sigma} is 'sm'.\n\
\n\
@item (A - sigma * I) \\ x\n\
for standard eigenvalue problem, where @code{I} is the identity matrix of\n\
the same size as @code{A}.  If @var{sigma} is zero, this reduces the\n\
@code{A \\ x}.\n\
\n\
@item (A - sigma * B) \\ x\n\
for the general eigenvalue problem.\n\
@end table\n\
\n\
The return arguments of @code{eigs} depends on the number of return\n\
arguments.  With a single return argument, a vector @var{d} of length @var{k}\n\
is returned, represent the @var{k} eigenvalues that have been found.  With two\n\
return arguments, @var{v} is a @var{n}-by-@var{k} matrix whose columns are\n\
the @var{k} eigenvectors corresponding to the returned eigenvalues.  The\n\
eigenvalues themselves are then returned in @var{d} in the form of a\n\
@var{n}-by-@var{k} matrix, where the elements on the diagonal are the\n\
eigenvalues.\n\
\n\
Given a third return argument @var{flag}, @code{eigs} also returns the status\n\
of the convergence.  If @var{flag} is 0, then all eigenvalues have converged,\n\
otherwise not.\n\
\n\
This function is based on the @sc{arpack} package, written by R Lehoucq,\n\
K Maschhoff, D Sorensen and C Yang.  For more information see\n\
@url{http://www.caam.rice.edu/software/ARPACK/}.\n\
\n\
@seealso{eig, svds}\n\
@end deftypefn")
{
  octave_value_list retval;
#ifdef HAVE_ARPACK
  int nargin = args.length ();
  std::string fcn_name;
  octave_idx_type n = 0;
  octave_idx_type k = 6;
  Complex sigma = 0.;
  double sigmar, sigmai;
  bool have_sigma = false;
  std::string typ = "LM";
  Matrix amm, bmm, bmt;
  ComplexMatrix acm, bcm, bct;
  SparseMatrix asmm, bsmm, bsmt;
  SparseComplexMatrix ascm, bscm, bsct;
  int b_arg = 0;
  bool have_b = false;
  bool have_a_fun = false;
  bool a_is_complex = false;
  bool b_is_complex = false;
  bool symmetric = false;
  bool cholB = false;
  bool a_is_sparse = false;
  ColumnVector permB;
  int arg_offset = 0;
  double tol = DBL_EPSILON;
  int maxit = 300;
  int disp = 0;
  octave_idx_type p = -1;
  ColumnVector resid;
  ComplexColumnVector cresid;
  octave_idx_type info = 1;
  char bmat = 'I';

  warned_imaginary = false;

  unwind_protect frame;

  frame.protect_var (call_depth);
  call_depth++;

  if (call_depth > 1)
    {
      error ("eigs: invalid recursive call");
      if (fcn_name.length())
	clear_function (fcn_name);
      return retval;
    }

  if (nargin == 0)
    print_usage ();
  else if (args(0).is_function_handle () || args(0).is_inline_function ()
	   || args(0).is_string ())
    {
      if (args(0).is_string ())
	{
	  std::string name = args(0).string_value ();
	  std::string fname = "function y = ";
	  fcn_name = unique_symbol_name ("__eigs_fcn_");
	  fname.append (fcn_name);
	  fname.append ("(x) y = ");
	  eigs_fcn = extract_function (args(0), "eigs", fcn_name, fname,
				       "; endfunction");
	}
      else
	eigs_fcn = args(0).function_value ();

      if (!eigs_fcn)
	{
	  error ("eigs: unknown function");
	  return retval;
	}

      if (nargin < 2)
	{
	  error ("eigs: incorrect number of arguments");
	  return retval;
	}
      else
	{
	  n = args(1).nint_value ();
	  arg_offset = 1;
	  have_a_fun = true;
	}
    }
  else
    {
      if (args(0).is_complex_type ())
	{
	  if (args(0).is_sparse_type ())
	    {
	      ascm = (args(0).sparse_complex_matrix_value());
	      a_is_sparse = true;
	    }
	  else
	    acm = (args(0).complex_matrix_value());
	  a_is_complex = true;
	  symmetric = false; // ARAPACK doesn't special case complex symmetric
	}
      else
	{
	  if (args(0).is_sparse_type ())
	    {
	      asmm = (args(0).sparse_matrix_value());
	      a_is_sparse = true;
	      symmetric = asmm.is_symmetric();
	    }
	  else
	    {
	      amm = (args(0).matrix_value());
	      symmetric = amm.is_symmetric();
	    }
	}

    }

  // Note hold off reading B till later to avoid issues of double 
  // copies of the matrix if B is full/real while A is complex..
  if (!error_state && nargin > 1 + arg_offset && 
      !(args(1 + arg_offset).is_real_scalar ()))
    {
      if (args(1+arg_offset).is_complex_type ())
	{
	  b_arg = 1+arg_offset;
	  have_b = true;
	  bmat = 'G';
	  b_is_complex = true;
	  arg_offset++;
	}
      else
	{
	  b_arg = 1+arg_offset;
	  have_b = true;
	  bmat = 'G';
	  arg_offset++;
	}
    }

  if (!error_state && nargin > (1+arg_offset))
    k = args(1+arg_offset).nint_value ();

  if (!error_state && nargin > (2+arg_offset))
    {
      if (args(2+arg_offset).is_string ())
	{
	  typ = args(2+arg_offset).string_value ();

	  // Use STL function to convert to upper case
	  transform (typ.begin (), typ.end (), typ.begin (), toupper);

	  sigma = 0.;
	}
      else
	{
	  sigma = args(2+arg_offset).complex_value ();

	  if (! error_state)
	    have_sigma = true;
	  else
	    {
	      error ("eigs: sigma must be a scalar or a string");
	      return retval;
	    }
	}
    }

  sigmar = std::real (sigma);
  sigmai = std::imag (sigma);

  if (!error_state && nargin > (3+arg_offset))
    {
      if (args(3+arg_offset).is_map ())
	{
	  Octave_map map = args(3+arg_offset).map_value ();

	  // issym is ignored if A is not a function
	  if (map.contains ("issym") && have_a_fun)
	      symmetric = map.contents ("issym")(0).double_value () != 0.;

	  // isreal is ignored if A is not a function
	  if (map.contains ("isreal") && have_a_fun)
	      a_is_complex = ! (map.contents ("isreal")(0).double_value () != 0.);

	  if (map.contains ("tol"))
	    tol = map.contents ("tol")(0).double_value ();

	  if (map.contains ("maxit"))
	    maxit = map.contents ("maxit")(0).nint_value ();

	  if (map.contains ("p"))
	    p = map.contents ("p")(0).nint_value ();

	  if (map.contains ("v0"))
	    {
	      if (a_is_complex || b_is_complex)
		cresid = ComplexColumnVector 
		  (map.contents ("v0")(0).complex_vector_value());
	      else
		resid = ColumnVector (map.contents ("v0")(0).vector_value());
	    }

	  if (map.contains ("disp"))
	    disp = map.contents ("disp")(0).nint_value ();

	  if (map.contains ("cholB"))
	    cholB = map.contents ("cholB")(0).double_value () != 0.;

	  if (map.contains ("permB"))
	    permB = ColumnVector (map.contents ("permB")(0).vector_value ()) 
	      - 1.0;
	}
      else
	{
	  error ("eigs: options argument must be a structure");
	  return retval;
	}
    }

  if (nargin > (4+arg_offset))
    {
      error ("eigs: incorrect number of arguments");
      return retval;
    }

  if (have_b)
    {
      if (a_is_complex || b_is_complex)
	{
	  if (a_is_sparse)
	    bscm = args(b_arg).sparse_complex_matrix_value ();
	  else
	    bcm = args(b_arg).complex_matrix_value ();
	}
      else
	{
	  if (a_is_sparse)
	    bsmm = args(b_arg).sparse_matrix_value ();
	  else
	    bmm = args(b_arg).matrix_value ();
	}
    }

  // Mode 1 for SM mode seems unstable for some reason. 
  // Use Mode 3 instead, with sigma = 0.
  if (!error_state && !have_sigma && typ == "SM")
    have_sigma = true;

  if (!error_state)
    {
      octave_idx_type nconv;
      if (a_is_complex || b_is_complex)
	{
	  ComplexMatrix eig_vec;
	  ComplexColumnVector eig_val;


	  if (have_a_fun)
	    nconv = EigsComplexNonSymmetricFunc 
	      (eigs_complex_func, n, typ, sigma, k, p, info, eig_vec, eig_val,
	       cresid, octave_stdout, tol, (nargout > 1), cholB, disp, maxit);
	  else if (have_sigma)
	    {
	      if (a_is_sparse)
		nconv = EigsComplexNonSymmetricMatrixShift
		  (ascm, sigma, k, p, info, eig_vec, eig_val, bscm, permB,
		   cresid, octave_stdout, tol, (nargout > 1), cholB, disp, 
		   maxit);
	      else
		nconv = EigsComplexNonSymmetricMatrixShift
		  (acm, sigma, k, p, info, eig_vec, eig_val, bcm, permB, cresid,
		   octave_stdout, tol, (nargout > 1), cholB, disp, maxit);
	    }
	  else
	    {
	      if (a_is_sparse)
		nconv = EigsComplexNonSymmetricMatrix
		  (ascm, typ, k, p, info, eig_vec, eig_val, bscm, permB, cresid,
		   octave_stdout, tol, (nargout > 1), cholB, disp, maxit);
	      else
		nconv = EigsComplexNonSymmetricMatrix
		  (acm, typ, k, p, info, eig_vec, eig_val, bcm, permB, cresid,
		   octave_stdout, tol, (nargout > 1), cholB, disp, maxit);
	    }

	  if (nargout < 2)
	    retval (0) = eig_val;
	  else
	    {
	      retval(2) = double (info);
	      retval(1) = ComplexDiagMatrix (eig_val);
	      retval(0) = eig_vec;
	    }
	}
      else if (sigmai != 0.)
	{
	  // Promote real problem to a complex one.
	  ComplexMatrix eig_vec;
	  ComplexColumnVector eig_val;

	  if (have_a_fun)
	    nconv = EigsComplexNonSymmetricFunc 
	      (eigs_complex_func, n, typ,  sigma, k, p, info, eig_vec, eig_val,
	       cresid, octave_stdout, tol, (nargout > 1), cholB, disp, maxit);
	  else
	    {
	      if (a_is_sparse)
		nconv = EigsComplexNonSymmetricMatrixShift 
		  (SparseComplexMatrix (asmm), sigma, k, p, info, eig_vec,
		   eig_val, SparseComplexMatrix (bsmm), permB, cresid,
		   octave_stdout, tol, (nargout > 1), cholB, disp, maxit);
	      else
		nconv = EigsComplexNonSymmetricMatrixShift 
		  (ComplexMatrix (amm), sigma, k, p, info, eig_vec,
		   eig_val, ComplexMatrix (bmm), permB, cresid,
		   octave_stdout, tol, (nargout > 1), cholB, disp, maxit);
	    }

	  if (nargout < 2)
	    retval (0) = eig_val;
	  else
	    {
	      retval(2) = double (info);
	      retval(1) = ComplexDiagMatrix (eig_val);
	      retval(0) = eig_vec;
	    }
	}
      else
	{
	  if (symmetric)
	    {
	      Matrix eig_vec;
	      ColumnVector eig_val;

	      if (have_a_fun)
		nconv = EigsRealSymmetricFunc 
		  (eigs_func, n, typ, sigmar, k, p, info, eig_vec, eig_val,
		   resid, octave_stdout, tol, (nargout > 1), cholB, disp, 
		   maxit);
	      else if (have_sigma)
		{
		  if (a_is_sparse)
		    nconv = EigsRealSymmetricMatrixShift 
		      (asmm, sigmar, k, p, info, eig_vec, eig_val, bsmm, permB,
		       resid, octave_stdout, tol, (nargout > 1), cholB, disp, 
		       maxit);
		  else
		    nconv = EigsRealSymmetricMatrixShift 
		      (amm, sigmar, k, p, info, eig_vec, eig_val, bmm, permB,
		       resid, octave_stdout, tol, (nargout > 1), cholB, disp,
		       maxit);
		}
	      else
		{
		  if (a_is_sparse)
		    nconv = EigsRealSymmetricMatrix 
		      (asmm, typ, k, p, info, eig_vec, eig_val, bsmm, permB,
		       resid, octave_stdout, tol, (nargout > 1), cholB, disp,
		       maxit);
		  else
		    nconv = EigsRealSymmetricMatrix 
		      (amm, typ, k, p, info, eig_vec, eig_val, bmm, permB,
		       resid, octave_stdout, tol, (nargout > 1), cholB, disp,
		       maxit);
		}

	      if (nargout < 2)
		retval (0) = eig_val;
	      else
		{
		  retval(2) = double (info);
		  retval(1) = DiagMatrix (eig_val);
		  retval(0) = eig_vec;
		}
	    }
	  else
	    {
	      ComplexMatrix eig_vec;
	      ComplexColumnVector eig_val;

	      if (have_a_fun)
		nconv = EigsRealNonSymmetricFunc 
		  (eigs_func, n, typ, sigmar, k, p, info, eig_vec, eig_val,
		   resid, octave_stdout, tol, (nargout > 1), cholB, disp, 
		   maxit);
	      else if (have_sigma)
		{
		  if (a_is_sparse)
		    nconv = EigsRealNonSymmetricMatrixShift 
		      (asmm, sigmar, k, p, info, eig_vec, eig_val, bsmm, permB,
		       resid, octave_stdout, tol, (nargout > 1), cholB, disp, 
		       maxit);
		  else
		    nconv = EigsRealNonSymmetricMatrixShift 
		      (amm, sigmar, k, p, info, eig_vec, eig_val, bmm, permB,
		       resid, octave_stdout, tol, (nargout > 1), cholB, disp,
		       maxit);
		}
	      else
		{
		  if (a_is_sparse)
		    nconv = EigsRealNonSymmetricMatrix 
		      (asmm, typ, k, p, info, eig_vec, eig_val, bsmm, permB,
		       resid, octave_stdout, tol, (nargout > 1), cholB, disp,
		       maxit);
		  else
		    nconv = EigsRealNonSymmetricMatrix 
		      (amm, typ, k, p, info, eig_vec, eig_val, bmm, permB,
		       resid, octave_stdout, tol, (nargout > 1), cholB, disp,
		       maxit);
		}

	      if (nargout < 2)
		retval (0) = eig_val;
	      else
		{
		  retval(2) = double (info);
		  retval(1) = ComplexDiagMatrix (eig_val);
		  retval(0) = eig_vec;
		}
	    }
	}

      if (nconv <= 0)
	warning ("eigs: None of the %d requested eigenvalues converged", k);
      else if (nconv < k)
	warning ("eigs: Only %d of the %d requested eigenvalues converged", 
		 nconv, k);
    }

  if (! fcn_name.empty ())
    clear_function (fcn_name);
#else
  error ("eigs: not available in this version of Octave");
#endif

  return retval;
}

/* #### SPARSE MATRIX VERSIONS #### */

/*

%% Real positive definite tests, n must be even
%!shared n, k, A, d0, d2
%! n = 20;
%! k = 4;
%! A = sparse([3:n,1:n,1:(n-2)],[1:(n-2),1:n,3:n],[ones(1,n-2),4*ones(1,n),ones(1,n-2)]);
%! d0 = eig (A);
%! d2 = sort(d0);
%! [dum, idx] = sort( abs(d0));
%! d0 = d0(idx);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k);
%! assert (d1, d0(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A,k+1);
%! assert (d1, d0(end:-1:(end-k)),1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'lm');
%! assert (d1, d0(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'sm');
%! assert (d1, d0(k:-1:1), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'la');
%! assert (d1, d2(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'sa');
%! assert (d1, d2(1:k), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'be');
%! assert (d1, d2([1:floor(k/2), (end - ceil(k/2) + 1):end]), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k+1, 'be');
%! assert (d1, d2([1:floor((k+1)/2), (end - ceil((k+1)/2) + 1):end]), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 4.1);
%! [dum,idx0] = sort (abs(d0 - 4.1));
%! [dum,idx1] = sort (abs(d1 - 4.1));
%! assert (d1(idx1), d0(idx0(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs(A, speye(n), k, 'lm');
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! assert (eigs(A,k,4.1), eigs(A,speye(n),k,4.1), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! d1 = eigs(A, speye(n), k, 'lm', opts);
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! q = [2:n,1];
%! opts.permB=q;
%! d1 = eigs(A, speye(n)(q,q), k, 'lm', opts);
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! d1 = eigs(A, speye(n), k, 4.1, opts);
%! assert (abs(d1), eigs(A,k,4.1), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! q = [2:n,1];
%! opts.permB=q;
%! d1 = eigs(A, speye(n)(q,q), k, 4.1, opts);
%! assert (abs(d1), eigs(A,k,4.1), 1e-11);
%!testif HAVE_ARPACK
%! assert (eigs(A,k,4.1), eigs(A,speye(n),k,4.1), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A * x;
%! opts.issym = 1; opts.isreal = 1;
%! d1 = eigs (fn, n, k, 'lm', opts);
%! assert (d1, d0(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A \ x;
%! opts.issym = 1; opts.isreal = 1;
%! d1 = eigs (fn, n, k, 'sm', opts);
%! assert (d1, d0(k:-1:1), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) (A - 4.1 * eye(n)) \ x;
%! opts.issym = 1; opts.isreal = 1;
%! d1 = eigs (fn, n, k, 4.1, opts);
%! assert (d1, eigs(A,k,4.1), 1e-11);
%!testif HAVE_ARPACK
%! AA = speye (10);
%! fn = @(x) AA * x;
%! opts.issym = 1; opts.isreal = 1;
%! assert (eigs (fn, 10, AA, 3, 'lm', opts), [1; 1; 1]);
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'lm');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*speye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'sm');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*speye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'la');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*speye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'sa');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*speye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'be');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*speye(n))*v1(:,i))),0.,1e-11)
%! endfor

*/

/*

%% Real unsymmetric tests
%!shared n, k, A, d0
%! n = 20;
%! k = 4;
%! A =  sparse([3:n,1:n,1:(n-2)],[1:(n-2),1:n,3:n],[ones(1,n-2),1:n,-ones(1,n-2)]);
%! d0 = eig (A);
%! [dum, idx] = sort(abs(d0));
%! d0 = d0(idx);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k);
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A,k+1);
%! assert (abs(d1), abs(d0(end:-1:(end-k))),1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'lm');
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'sm');
%! assert (abs(d1), abs(d0(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'lr');
%! [dum, idx] = sort (real(d0));
%! d2 = d0(idx);
%! assert (real(d1), real(d2(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'sr');
%! [dum, idx] = sort (real(abs(d0)));
%! d2 = d0(idx);
%! assert (real(d1), real(d2(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'li');
%! [dum, idx] = sort (imag(abs(d0)));
%! d2 = d0(idx);
%! assert (sort(imag(d1)), sort(imag(d2(end:-1:(end-k+1)))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'si');
%! [dum, idx] = sort (imag(abs(d0)));
%! d2 = d0(idx);
%! assert (sort(imag(d1)), sort(imag(d2(1:k))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 4.1);
%! [dum,idx0] = sort (abs(d0 - 4.1));
%! [dum,idx1] = sort (abs(d1 - 4.1));
%! assert (abs(d1(idx1)), abs(d0(idx0(1:k))), 1e-11);
%! assert (sort(imag(d1(idx1))), sort(imag(d0(idx0(1:k)))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs(A, speye(n), k, 'lm');
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! d1 = eigs(A, speye(n), k, 'lm', opts);
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! q = [2:n,1];
%! opts.permB=q;
%! d1 = eigs(A, speye(n)(q,q), k, 'lm', opts);
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! d1 = eigs(A, speye(n), k, 4.1, opts);
%! assert (abs(d1), eigs(A,k,4.1), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! q = [2:n,1];
%! opts.permB=q;
%! d1 = eigs(A, speye(n)(q,q), k, 4.1, opts);
%! assert (abs(d1), eigs(A,k,4.1), 1e-11);
%!testif HAVE_ARPACK
%! assert (abs(eigs(A,k,4.1)), abs(eigs(A,speye(n),k,4.1)), 1e-11);
%!testif HAVE_ARPACK
%! assert (sort(imag(eigs(A,k,4.1))), sort(imag(eigs(A,speye(n),k,4.1))), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A * x;
%! opts.issym = 0; opts.isreal = 1;
%! d1 = eigs (fn, n, k, 'lm', opts);
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A \ x;
%! opts.issym = 0; opts.isreal = 1;
%! d1 = eigs (fn, n, k, 'sm', opts);
%! assert (abs(d1), d0(1:k), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) (A - 4.1 * eye(n)) \ x;
%! opts.issym = 0; opts.isreal = 1;
%! d1 = eigs (fn, n, k, 4.1, opts);
%! assert (abs(d1), eigs(A,k,4.1), 1e-11);
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'lm');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*speye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'sm');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*speye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'lr');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*speye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'sr');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*speye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'li');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*speye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'si');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*speye(n))*v1(:,i))),0.,1e-11)
%! endfor

*/

/*

%% Complex hermitian tests
%!shared n, k, A, d0
%! n = 20;
%! k = 4;
%! A = sparse([3:n,1:n,1:(n-2)],[1:(n-2),1:n,3:n],[1i*ones(1,n-2),4*ones(1,n),-1i*ones(1,n-2)]);
%! d0 = eig (A);
%! [dum, idx] = sort(abs(d0));
%! d0 = d0(idx);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k);
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A,k+1);
%! assert (abs(d1), abs(d0(end:-1:(end-k))),1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'lm');
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'sm');
%! assert (abs(d1), abs(d0(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'lr');
%! [dum, idx] = sort (real(abs(d0)));
%! d2 = d0(idx);
%! assert (real(d1), real(d2(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'sr');
%! [dum, idx] = sort (real(abs(d0)));
%! d2 = d0(idx);
%! assert (real(d1), real(d2(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'li');
%! [dum, idx] = sort (imag(abs(d0)));
%! d2 = d0(idx);
%! assert (sort(imag(d1)), sort(imag(d2(end:-1:(end-k+1)))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'si');
%! [dum, idx] = sort (imag(abs(d0)));
%! d2 = d0(idx);
%! assert (sort(imag(d1)), sort(imag(d2(1:k))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 4.1);
%! [dum,idx0] = sort (abs(d0 - 4.1));
%! [dum,idx1] = sort (abs(d1 - 4.1));
%! assert (abs(d1(idx1)), abs(d0(idx0(1:k))), 1e-11);
%! assert (sort(imag(d1(idx1))), sort(imag(d0(idx0(1:k)))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs(A, speye(n), k, 'lm');
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! d1 = eigs(A, speye(n), k, 'lm', opts);
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! q = [2:n,1];
%! opts.permB=q;
%! d1 = eigs(A, speye(n)(q,q), k, 'lm', opts);
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! d1 = eigs(A, speye(n), k, 4.1, opts);
%! assert (abs(abs(d1)), abs(eigs(A,k,4.1)), 1e-11);
%! assert (sort(imag(abs(d1))), sort(imag(eigs(A,k,4.1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! q = [2:n,1];
%! opts.permB=q;
%! d1 = eigs(A, speye(n)(q,q), k, 4.1, opts);
%! assert (abs(abs(d1)), abs(eigs(A,k,4.1)), 1e-11);
%! assert (sort(imag(abs(d1))), sort(imag(eigs(A,k,4.1))), 1e-11);
%!testif HAVE_ARPACK
%! assert (abs(eigs(A,k,4.1)), abs(eigs(A,speye(n),k,4.1)), 1e-11);
%!testif HAVE_ARPACK
%! assert (sort(imag(eigs(A,k,4.1))), sort(imag(eigs(A,speye(n),k,4.1))), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A * x;
%! opts.issym = 0; opts.isreal = 0;
%! d1 = eigs (fn, n, k, 'lm', opts);
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A \ x;
%! opts.issym = 0; opts.isreal = 0;
%! d1 = eigs (fn, n, k, 'sm', opts);
%! assert (abs(d1), d0(1:k), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) (A - 4.1 * eye(n)) \ x;
%! opts.issym = 0; opts.isreal = 0;
%! d1 = eigs (fn, n, k, 4.1, opts);
%! assert (abs(d1), eigs(A,k,4.1), 1e-11);
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'lm');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*speye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'sm');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*speye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'lr');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*speye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'sr');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*speye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'li');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*speye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'si');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*speye(n))*v1(:,i))),0.,1e-11)
%! endfor

*/

/* #### FULL MATRIX VERSIONS #### */

/*

%% Real positive definite tests, n must be even
%!shared n, k, A, d0, d2
%! n = 20;
%! k = 4;
%! A = full(sparse([3:n,1:n,1:(n-2)],[1:(n-2),1:n,3:n],[ones(1,n-2),4*ones(1,n),ones(1,n-2)]));
%! d0 = eig (A);
%! d2 = sort(d0);
%! [dum, idx] = sort( abs(d0));
%! d0 = d0(idx);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k);
%! assert (d1, d0(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A,k+1);
%! assert (d1, d0(end:-1:(end-k)),1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'lm');
%! assert (d1, d0(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'sm');
%! assert (d1, d0(k:-1:1), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'la');
%! assert (d1, d2(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'sa');
%! assert (d1, d2(1:k), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'be');
%! assert (d1, d2([1:floor(k/2), (end - ceil(k/2) + 1):end]), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k+1, 'be');
%! assert (d1, d2([1:floor((k+1)/2), (end - ceil((k+1)/2) + 1):end]), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 4.1);
%! [dum,idx0] = sort (abs(d0 - 4.1));
%! [dum,idx1] = sort (abs(d1 - 4.1));
%! assert (d1(idx1), d0(idx0(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs(A, eye(n), k, 'lm');
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! assert (eigs(A,k,4.1), eigs(A,eye(n),k,4.1), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! d1 = eigs(A, eye(n), k, 'lm', opts);
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! q = [2:n,1];
%! opts.permB=q;
%! d1 = eigs(A, eye(n)(q,q), k, 'lm', opts);
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! d1 = eigs(A, eye(n), k, 4.1, opts);
%! assert (abs(d1), eigs(A,k,4.1), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! q = [2:n,1];
%! opts.permB=q;
%! d1 = eigs(A, eye(n)(q,q), k, 4.1, opts);
%! assert (abs(d1), eigs(A,k,4.1), 1e-11);
%!testif HAVE_ARPACK
%! assert (eigs(A,k,4.1), eigs(A,eye(n),k,4.1), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A * x;
%! opts.issym = 1; opts.isreal = 1;
%! d1 = eigs (fn, n, k, 'lm', opts);
%! assert (d1, d0(end:-1:(end-k+1)), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A \ x;
%! opts.issym = 1; opts.isreal = 1;
%! d1 = eigs (fn, n, k, 'sm', opts);
%! assert (d1, d0(k:-1:1), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) (A - 4.1 * eye(n)) \ x;
%! opts.issym = 1; opts.isreal = 1;
%! d1 = eigs (fn, n, k, 4.1, opts);
%! assert (d1, eigs(A,k,4.1), 1e-11);
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'lm');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*eye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'sm');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*eye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'la');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*eye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'sa');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*eye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'be');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*eye(n))*v1(:,i))),0.,1e-11)
%! endfor

*/

/*

%% Real unsymmetric tests
%!shared n, k, A, d0
%! n = 20;
%! k = 4;
%! A =  full(sparse([3:n,1:n,1:(n-2)],[1:(n-2),1:n,3:n],[ones(1,n-2),1:n,-ones(1,n-2)]));
%! d0 = eig (A);
%! [dum, idx] = sort(abs(d0));
%! d0 = d0(idx);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k);
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A,k+1);
%! assert (abs(d1), abs(d0(end:-1:(end-k))),1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'lm');
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'sm');
%! assert (abs(d1), abs(d0(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'lr');
%! [dum, idx] = sort (real(d0));
%! d2 = d0(idx);
%! assert (real(d1), real(d2(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'sr');
%! [dum, idx] = sort (real(abs(d0)));
%! d2 = d0(idx);
%! assert (real(d1), real(d2(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'li');
%! [dum, idx] = sort (imag(abs(d0)));
%! d2 = d0(idx);
%! assert (sort(imag(d1)), sort(imag(d2(end:-1:(end-k+1)))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'si');
%! [dum, idx] = sort (imag(abs(d0)));
%! d2 = d0(idx);
%! assert (sort(imag(d1)), sort(imag(d2(1:k))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 4.1);
%! [dum,idx0] = sort (abs(d0 - 4.1));
%! [dum,idx1] = sort (abs(d1 - 4.1));
%! assert (abs(d1(idx1)), abs(d0(idx0(1:k))), 1e-11);
%! assert (sort(imag(d1(idx1))), sort(imag(d0(idx0(1:k)))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs(A, eye(n), k, 'lm');
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! d1 = eigs(A, eye(n), k, 'lm', opts);
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! q = [2:n,1];
%! opts.permB=q;
%! d1 = eigs(A, eye(n)(q,q), k, 'lm', opts);
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! d1 = eigs(A, eye(n), k, 4.1, opts);
%! assert (abs(d1), eigs(A,k,4.1), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! q = [2:n,1];
%! opts.permB=q;
%! d1 = eigs(A, eye(n)(q,q), k, 4.1, opts);
%! assert (abs(d1), eigs(A,k,4.1), 1e-11);
%!testif HAVE_ARPACK
%! assert (abs(eigs(A,k,4.1)), abs(eigs(A,eye(n),k,4.1)), 1e-11);
%!testif HAVE_ARPACK
%! assert (sort(imag(eigs(A,k,4.1))), sort(imag(eigs(A,eye(n),k,4.1))), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A * x;
%! opts.issym = 0; opts.isreal = 1;
%! d1 = eigs (fn, n, k, 'lm', opts);
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A \ x;
%! opts.issym = 0; opts.isreal = 1;
%! d1 = eigs (fn, n, k, 'sm', opts);
%! assert (abs(d1), d0(1:k), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) (A - 4.1 * eye(n)) \ x;
%! opts.issym = 0; opts.isreal = 1;
%! d1 = eigs (fn, n, k, 4.1, opts);
%! assert (abs(d1), eigs(A,k,4.1), 1e-11);
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'lm');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*eye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'sm');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*eye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'lr');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*eye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'sr');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*eye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'li');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*eye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'si');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*eye(n))*v1(:,i))),0.,1e-11)
%! endfor

*/

/*

%% Complex hermitian tests
%!shared n, k, A, d0
%! n = 20;
%! k = 4;
%! A = full(sparse([3:n,1:n,1:(n-2)],[1:(n-2),1:n,3:n],[1i*ones(1,n-2),4*ones(1,n),-1i*ones(1,n-2)]));
%! d0 = eig (A);
%! [dum, idx] = sort(abs(d0));
%! d0 = d0(idx);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k);
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A,k+1);
%! assert (abs(d1), abs(d0(end:-1:(end-k))),1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'lm');
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'sm');
%! assert (abs(d1), abs(d0(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'lr');
%! [dum, idx] = sort (real(abs(d0)));
%! d2 = d0(idx);
%! assert (real(d1), real(d2(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'sr');
%! [dum, idx] = sort (real(abs(d0)));
%! d2 = d0(idx);
%! assert (real(d1), real(d2(1:k)), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'li');
%! [dum, idx] = sort (imag(abs(d0)));
%! d2 = d0(idx);
%! assert (sort(imag(d1)), sort(imag(d2(end:-1:(end-k+1)))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 'si');
%! [dum, idx] = sort (imag(abs(d0)));
%! d2 = d0(idx);
%! assert (sort(imag(d1)), sort(imag(d2(1:k))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs (A, k, 4.1);
%! [dum,idx0] = sort (abs(d0 - 4.1));
%! [dum,idx1] = sort (abs(d1 - 4.1));
%! assert (abs(d1(idx1)), abs(d0(idx0(1:k))), 1e-11);
%! assert (sort(imag(d1(idx1))), sort(imag(d0(idx0(1:k)))), 1e-11);
%!testif HAVE_ARPACK
%! d1 = eigs(A, eye(n), k, 'lm');
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! d1 = eigs(A, eye(n), k, 'lm', opts);
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! q = [2:n,1];
%! opts.permB=q;
%! d1 = eigs(A, eye(n)(q,q), k, 'lm', opts);
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! d1 = eigs(A, eye(n), k, 4.1, opts);
%! assert (abs(abs(d1)), abs(eigs(A,k,4.1)), 1e-11);
%! assert (sort(imag(abs(d1))), sort(imag(eigs(A,k,4.1))), 1e-11);
%!testif HAVE_ARPACK
%! opts.cholB=true;
%! q = [2:n,1];
%! opts.permB=q;
%! d1 = eigs(A, eye(n)(q,q), k, 4.1, opts);
%! assert (abs(abs(d1)), abs(eigs(A,k,4.1)), 1e-11);
%! assert (sort(imag(abs(d1))), sort(imag(eigs(A,k,4.1))), 1e-11);
%!testif HAVE_ARPACK
%! assert (abs(eigs(A,k,4.1)), abs(eigs(A,eye(n),k,4.1)), 1e-11);
%!testif HAVE_ARPACK
%! assert (sort(imag(eigs(A,k,4.1))), sort(imag(eigs(A,eye(n),k,4.1))), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A * x;
%! opts.issym = 0; opts.isreal = 0;
%! d1 = eigs (fn, n, k, 'lm', opts);
%! assert (abs(d1), abs(d0(end:-1:(end-k+1))), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) A \ x;
%! opts.issym = 0; opts.isreal = 0;
%! d1 = eigs (fn, n, k, 'sm', opts);
%! assert (abs(d1), d0(1:k), 1e-11);
%!testif HAVE_ARPACK
%! fn = @(x) (A - 4.1 * eye(n)) \ x;
%! opts.issym = 0; opts.isreal = 0;
%! d1 = eigs (fn, n, k, 4.1, opts);
%! assert (abs(d1), eigs(A,k,4.1), 1e-11);
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'lm');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*eye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'sm');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*eye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'lr');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*eye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'sr');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*eye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'li');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*eye(n))*v1(:,i))),0.,1e-11)
%! endfor
%!testif HAVE_ARPACK
%! [v1,d1] = eigs(A, k, 'si');
%! d1 = diag(d1);
%! for i=1:k
%!  assert(max(abs((A - d1(i)*eye(n))*v1(:,i))),0.,1e-11)
%! endfor

*/

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
