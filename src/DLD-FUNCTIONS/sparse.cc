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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>
#include <string>

#include "variables.h"
#include "utils.h"
#include "pager.h"
#include "defun-dld.h"
#include "gripes.h"
#include "quit.h"

#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"
#include "ov-bool-sparse.h"

static bool
is_sparse (const octave_value& arg)
{
  return (arg.class_name () == "sparse");
}

DEFUN_DLD (issparse, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} issparse (@var{expr})\n\
Return 1 if the value of the expression @var{expr} is a sparse matrix.\n\
@end deftypefn") 
{
   if (args.length() != 1) 
     {
       print_usage("issparse");
       return octave_value ();
     }
   else 
     return octave_value (is_sparse (args(0)));
}

DEFUN_DLD (sparse, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{sparse_val} =} sparse (...)\n\
SPARSE: create a sparse matrix\n\
\n\
sparse can be called in the following ways:\n\
\n\
@enumerate\n\
@item @var{S} = sparse(@var{A})  where @var{A} is a full matrix\n\
\n\
@item @var{S} = sparse(@var{A},1)  where @var{A} is a full matrix, result\n\
is forced back to a full matrix is resulting matrix is sparse\n\
\n\
@item @var{S} = sparse(@var{i},@var{j},@var{s},@var{m},@var{n},@var{nzmax})  where\n\
   @itemize @w \n\
@var{i},@var{j}   are integer index vectors (1 x nnz) @* \n\
@var{s}     is the vector of real or complex entries (1 x nnz) @* \n\
@var{m},@var{n}   are the scalar dimentions of S @* \n\
@var{nzmax} is ignored (here for compatability with Matlab) @* \n\
\n\
        if multiple values are specified with the same @var{i},@var{j}\n\
        position, the corresponding values in @var{s} will be added\n\
   @end itemize\n\
\n\
@item The following usages are equivalent to (2) above:\n\
   @itemize @w \n\
@var{S} = sparse(@var{i},@var{j},@var{s},@var{m},@var{n})@*\n\
@var{S} = sparse(@var{i},@var{j},@var{s},@var{m},@var{n},'summation')@*\n\
@var{S} = sparse(@var{i},@var{j},@var{s},@var{m},@var{n},'sum')@*\n\
   @end itemize\n\
\n\
@item @var{S} = sparse(@var{i},@var{j},@var{s},@var{m},@var{n},'unique')@*\n\
\n\
   @itemize @w \n\
same as (2) above, except that rather than adding,\n\
if more than two values are specified for the same @var{i},@var{j}\n\
position, then the last specified value will be kept\n\
   @end itemize\n\
\n\
@item @var{S}=  sparse(@var{i},@var{j},@var{sv})          uses @var{m}=max(@var{i}), @var{n}=max(@var{j})\n\
\n\
@item @var{S}=  sparse(@var{m},@var{n})            does sparse([],[],[],@var{m},@var{n},0)\n\
\n\
@var{sv}, and @var{i} or @var{j} may be scalars, in\n\
which case they are expanded to all have the same length\n\
@end enumerate\n\
@seealso{full}\n\
@end deftypefn")
{
   octave_value retval;
   bool mutate = false;

   // WARNING: This function should always use constructions like
   //   retval = new octave_sparse_matrix (sm);
   // To avoid calling the maybe_mutate function. This is the only
   // function that should not call maybe_mutate, or at least only
   // in very particular cases.

   int nargin= args.length();
   if (nargin < 1 || (nargin == 4 && !args(3).is_string ()) || nargin > 6) 
     {
       print_usage ("sparse");
       return retval;
     }

   bool use_complex = false;
   bool use_bool = false;
   if (nargin > 2)
     {
       use_complex= args(2).is_complex_type();
       use_bool = args(2).is_bool_type ();
     }
   else
     {
       use_complex= args(0).is_complex_type();
       use_bool = args(0).is_bool_type ();
     }

   if (nargin == 2 && ! args(0).is_scalar_type() && args(1).is_scalar_type())
       mutate = (args(1).double_value() != 0.);

   if (nargin == 1 || (nargin == 2 && ! args(0).is_scalar_type() && 
		       args(1).is_scalar_type()))
     {
       octave_value arg = args (0);

       if (is_sparse (arg))
	 {
	   if (use_complex) 
	     {
	       SparseComplexMatrix sm (((const octave_sparse_complex_matrix&) arg
					.get_rep ())
				       .sparse_complex_matrix_value ());
	       retval = new octave_sparse_complex_matrix (sm);
	     }
	   else if (use_bool) 
	     {
	       SparseBoolMatrix sm (((const octave_sparse_bool_matrix&) arg
					.get_rep ())
				       .sparse_bool_matrix_value ());
	       retval = new octave_sparse_bool_matrix (sm);
	     }
	   else
	     {
	       SparseMatrix sm (((const octave_sparse_matrix&) arg
				 .get_rep ())
				.sparse_matrix_value ());
	       retval = new octave_sparse_matrix (sm);
	     }
	 }
       else
	 {
	   if (use_complex) 
	     {
	       SparseComplexMatrix sm (args (0).complex_matrix_value ());
	       if (error_state) 
		 return retval;
	       retval = new octave_sparse_complex_matrix (sm);
	     } 
	   else if (use_bool) 
	     {
	       SparseBoolMatrix sm (args (0).bool_matrix_value ());
	       if (error_state) 
		 return retval;
	       retval = new octave_sparse_bool_matrix (sm);
	     } 
	   else 
	     {
	       SparseMatrix sm (args (0).matrix_value ());
	       if (error_state) 
		 return retval;
	       retval = new octave_sparse_matrix (sm);
	     }
	 }
     }
   else 
     {
       octave_idx_type m = 1, n = 1;
       if (nargin == 2) 
	 {
	   m = args(0).int_value();
	   n = args(1).int_value();
	   if (error_state) return retval;

	   if (use_complex) 
	     retval = new octave_sparse_complex_matrix 
	       (SparseComplexMatrix (m, n));
	   else if (use_bool) 
	     retval = new octave_sparse_bool_matrix 
	       (SparseBoolMatrix (m, n));
	   else
	     retval = new octave_sparse_matrix 
	       (SparseMatrix (m, n));
	 }
       else 
	 {
	   if (args(0).is_empty () || args (1).is_empty () 
	       || args(2).is_empty ())
	     {
	       if (nargin > 4)
		 {
		   m = args(3).int_value();
		   n = args(4).int_value();
		 }

	       if (use_bool)
		 retval = new octave_sparse_bool_matrix 
		   (SparseBoolMatrix (m, n));
	       else
		 retval = new octave_sparse_matrix (SparseMatrix (m, n));
	     }
	   else
	     {
// 
//  I use this clumsy construction so that we can use
//  any orientation of args
	       ColumnVector ridxA = ColumnVector (args(0).vector_value 
					      (false, true));
	       ColumnVector cidxA = ColumnVector (args(1).vector_value 
						  (false, true));
	       ColumnVector coefA;
	       boolNDArray coefAB;
	       ComplexColumnVector coefAC;
	       bool assemble_do_sum = true; // this is the default in matlab6

	       if (use_complex) 
		 {
		   if (args(2).is_empty ())
		     coefAC = ComplexColumnVector (0);
		   else
		     coefAC = ComplexColumnVector 
		       (args(2).complex_vector_value (false, true));
		 }
	       else if (use_bool)
		 {
		   if (args(2).is_empty ())
		     coefAB = boolNDArray (dim_vector (1, 0));
		   else
		     coefAB = args(2).bool_array_value ();
		   dim_vector AB_dims = coefAB.dims ();
		   if (AB_dims.length() > 2 || (AB_dims(0) != 1 && 
						AB_dims(1) != 1))
		     error ("sparse: vector arguments required");
		 }
	       else 
		 if (args(2).is_empty ())
		   coefA = ColumnVector (0);
		 else
		   coefA = ColumnVector (args(2).vector_value (false, true));

	       if (error_state)
		 return retval;

	       // Confirm that i,j,s all have the same number of elements
	       octave_idx_type ns;
	       if (use_complex) 
		 ns = coefAC.length();
	       else if (use_bool) 
		 ns = coefAB.length();
	       else 
		 ns = coefA.length();

	       octave_idx_type ni = ridxA.length();
	       octave_idx_type nj = cidxA.length();
	       octave_idx_type nnz = (ni > nj ? ni : nj);
	       if ((ns != 1 && ns != nnz) ||
		   (ni != 1 && ni != nnz) ||
		   (nj != 1 && nj != nnz)) 
		 {
		   error ("sparse i, j and s must have the same length");
		   return retval;
		 }

	       if (nargin == 3 || nargin == 4) 
		 {
		   m = static_cast<octave_idx_type> (ridxA.max());
		   n = static_cast<octave_idx_type> (cidxA.max());

		   // if args(3) is not string, then ignore the value
		   // otherwise check for summation or unique
		   if (nargin == 4 && args(3).is_string())
		     {
		       std::string vv= args(3).string_value();
		       if (error_state) return retval;
		       
		       if ( vv == "summation" ||
			    vv == "sum" ) 
			 assemble_do_sum = true;
		       else
			 if ( vv == "unique" )
			   assemble_do_sum = false;
			 else {
			   error("sparse repeat flag must be 'sum' or 'unique'");
			   return retval;
			 }
		     }
		 } 
	       else 
		 {
		   m = args(3).int_value();
		   n = args(4).int_value();
		   if (error_state) 
		     return retval;

		   // if args(5) is not string, then ignore the value
		   // otherwise check for summation or unique
		   if (nargin >= 6 && args(5).is_string())
		     {
		       std::string vv= args(5).string_value();
		       if (error_state) return retval;
		       
		       if ( vv == "summation" ||
			    vv == "sum" ) 
			 assemble_do_sum = true;
		       else
			 if ( vv == "unique" )
			   assemble_do_sum = false;
			 else {
			   error("sparse repeat flag must be 'sum' or 'unique'");
			   return retval;
			 }
		     }
		   
		 }

	       // Convert indexing to zero-indexing used internally
	       ridxA -= 1.;
	       cidxA -= 1.;

	       if (use_complex) 
		 retval = new octave_sparse_complex_matrix 
		   (SparseComplexMatrix (coefAC, ridxA, cidxA, m, n, 
					 assemble_do_sum));
	       else if (use_bool) 
		 retval = new octave_sparse_bool_matrix 
		   (SparseBoolMatrix (coefAB, ridxA, cidxA, m, n, 
				      assemble_do_sum));
	       else
		 retval = new octave_sparse_matrix 
		   (SparseMatrix (coefA, ridxA, cidxA, m, n, 
				  assemble_do_sum));
	     }
	 }
     }

   // Only called in very particular cases, not the default case
   if (mutate)
     retval.maybe_mutate ();

   return retval;
}

DEFUN_DLD (full, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{FM} =} full (@var{SM})\n\
 returns a full storage matrix from a sparse one\n\
@seealso{sparse}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length() < 1) {
     print_usage ("full");
     return retval;
  }

  if (args(0).class_name () == "sparse")
    {
      if (args(0).type_name () == "sparse matrix") 
	retval = args(0).matrix_value ();
      else if (args(0).type_name () == "sparse complex matrix")
	retval = args(0).complex_matrix_value ();
      else if (args(0).type_name () == "sparse bool matrix")
	retval = args(0).bool_matrix_value ();
    } 
  else if (args(0).is_real_type())
    retval = args(0).matrix_value();
  else if (args(0).is_complex_type())
    retval = args(0).complex_matrix_value();
  else
    gripe_wrong_type_arg ("full", args(0));

  return retval;
}

DEFUN_DLD (nnz, args, ,
   "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{scalar} =} nnz (@var{SM})\n\
returns number of non zero elements in SM\n\
@seealso{sparse}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length() < 1) 
    {
      print_usage ("nnz");
      return retval;
    }

  if (args(0).class_name () == "sparse") 
    {
      // XXX FIXME XXX should nonzero be a method of octave_base_value so that the
      // below can be replaced with "retval = (double) (args(0).nonzero ());"
      const octave_value& rep = args(0).get_rep ();

      if (args(0).type_name () == "sparse matrix")
	retval = (double) ((const octave_sparse_matrix&) rep) .nonzero ();
      else if (args(0).type_name () == "sparse complex matrix")
	retval = (double) ((const octave_sparse_complex_matrix&) rep) .nonzero ();
      else if (args(0).type_name () == "sparse bool matrix")
	retval = (double) ((const octave_sparse_bool_matrix&) rep) .nonzero ();
    } 
  else if (args(0).type_name () == "complex matrix") 
    {
      const ComplexMatrix M = args(0).complex_matrix_value();
      octave_idx_type nnz = 0;
      for( octave_idx_type j = 0; j < M.cols(); j++)
	for( octave_idx_type i = 0; i < M.rows(); i++)
	  if (M (i, j) != 0.) 
	    nnz++;
      retval = (double) nnz;
    } 
  else if (args(0).type_name () == "matrix") 
    {
      const Matrix M = args(0).matrix_value();
      octave_idx_type nnz = 0;
      for( octave_idx_type j = 0; j < M.cols(); j++)
	for( octave_idx_type i = 0; i < M.rows(); i++)
	  if (M (i, j) != 0.) 
	    nnz++;
      retval = (double) nnz;
    } 
  else if (args(0).type_name () == "string") 
    {
      const charMatrix M = args(0).char_matrix_value();
      octave_idx_type nnz = 0;
      for( octave_idx_type j = 0; j < M.cols(); j++)
	for( octave_idx_type i = 0; i < M.rows(); i++)
	  if (M (i, j) != 0) 
	    nnz++;
      retval = (double) nnz;
    } 
  else if (args(0).type_name () == "scalar") 
      retval = args(0).scalar_value() != 0.0 ? 1.0 : 0.0;
  else if (args(0).type_name () == "complex scalar")
    retval = args(0).complex_value() != 0.0 ? 1.0 : 0.0;
  else
     gripe_wrong_type_arg ("nnz", args(0));

  return retval;
}

DEFUN_DLD (nzmax, args, ,
   "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{scalar} =} nzmax (@var{SM})\n\
Return the amount of storage allocated to the sparse matrix @var{SM}.\n\
Note that Octave tends to crop unused memory at the first oppurtunity\n\
for sparse objects. There are some cases of user created sparse objects\n\
where the value returned by @dfn{nzmaz} will not be the same as @dfn{nnz},\n\
but in general they will give the same result.\n\
@seealso{sparse, spalloc}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length() < 1) 
    {
      print_usage ("nzmax");
      return retval;
    }

  if (args(0).class_name () == "sparse") 
    {
      // XXX FIXME XXX should nnz be a method of octave_base_value so that the
      // below can be replaced with "retval = (double) (args(0).nz ());"
      const octave_value& rep = args(0).get_rep ();

      if (args(0).type_name () == "sparse matrix")
	retval = (double) ((const octave_sparse_matrix&) rep) .nnz ();
      else if (args(0).type_name () == "sparse complex matrix")
	retval = (double) ((const octave_sparse_complex_matrix&) rep) .nnz ();
      else if (args(0).type_name () == "sparse bool matrix")
	retval = (double) ((const octave_sparse_bool_matrix&) rep) .nnz ();
    }
  else
    error ("nzmax: argument must be a sparse matrix");

  return retval;
}

static octave_value_list
sparse_find (const SparseMatrix& v)
{
  octave_value_list retval;
  octave_idx_type nnz = v.nonzero ();
  dim_vector dv = v.dims ();
  octave_idx_type nr = dv(0);
  octave_idx_type nc = dv (1);

  ColumnVector I (nnz), J (nnz);
  ColumnVector S (nnz);

  for (octave_idx_type i = 0, cx = 0; i < nc; i++) 
    {
      OCTAVE_QUIT;
      for (octave_idx_type j = v.cidx(i); j < v.cidx(i+1); j++ ) 
	{
	  I (cx) = static_cast<double> (v.ridx(j) + 1);
	  J (cx) = static_cast<double> (i + 1);
	  S (cx) = v.data(j);
	  cx++;
	}
    }

  if (dv(0) == 1)
    {
      retval(0)= I.transpose ();
      retval(1)= J.transpose ();
      retval(2)= S.transpose ();
    }
  else
    {
      retval(0)= I;
      retval(1)= J;
      retval(2)= S;
    }
  retval(3)= (double) nr;
  retval(4)= (double) nc;
  return retval;
}

static octave_value_list
sparse_find (const SparseComplexMatrix& v)
{
  octave_value_list retval;
  octave_idx_type nnz = v.nonzero ();
  dim_vector dv = v.dims ();
  octave_idx_type nr = dv(0);
  octave_idx_type nc = dv (1);

  ColumnVector I (nnz), J (nnz);
  ComplexColumnVector S (nnz);

  for (octave_idx_type i = 0, cx = 0; i < nc; i++) 
    {
      OCTAVE_QUIT;
      for (octave_idx_type j = v.cidx(i); j < v.cidx(i+1); j++ ) 
	{
	  I (cx) = static_cast<double> (v.ridx(j) + 1);
	  J (cx) = static_cast<double> (i + 1);
	  S (cx) = v.data(j);
	  cx++;
	}
    }

  if (dv(0) == 1)
    {
      retval(0)= I.transpose ();
      retval(1)= J.transpose ();
      retval(2)= S.transpose ();
    }
  else
    {
      retval(0)= I;
      retval(1)= J;
      retval(2)= S;
    }
  retval(3)= (double) nr;
  retval(4)= (double) nc;
  return retval;
}

static octave_value_list
sparse_find (const SparseBoolMatrix& v)
{
  octave_value_list retval;
  octave_idx_type nnz = v.nonzero ();
  dim_vector dv = v.dims ();
  octave_idx_type nr = dv(0);
  octave_idx_type nc = dv (1);

  ColumnVector I (nnz), J (nnz);
  ColumnVector S (nnz);

  for (octave_idx_type i = 0, cx = 0; i < nc; i++) 
    {
      OCTAVE_QUIT;
      for (octave_idx_type j = v.cidx(i); j < v.cidx(i+1); j++ ) 
	{
	  I (cx) = static_cast<double> (v.ridx(j) + 1);
	  J (cx) = static_cast<double> (i + 1);
	  S (cx) = static_cast<double> (v.data(j));
	  cx++;
	}
    }

  if (dv(0) == 1)
    {
      retval(0)= I.transpose ();
      retval(1)= J.transpose ();
      retval(2)= S.transpose ();
    }
  else
    {
      retval(0)= I;
      retval(1)= J;
      retval(2)= S;
    }
  retval(3)= (double) nr;
  retval(4)= (double) nc;
  return retval;
}

// PKG_ADD: dispatch ("find", "spfind", "sparse matrix");
// PKG_ADD: dispatch ("find", "spfind", "sparse complex matrix");
// PKG_ADD: dispatch ("find", "spfind", "sparse bool matrix");
DEFUN_DLD (spfind, args, nargout ,
    "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[...] =} spfind (...)\n\
SPFIND: a sparse version of the find operator\n\
@enumerate\n\
    @item\n\
@var{x }= spfind( @var{a })\n\
    @itemize @w\n\
is analagous to @var{x}= find(@var{A}(:))@*\n\
where @var{A}= full(@var{a})\n\
    @end itemize\n\
    @item\n\
[@var{i},@var{j},@var{v},@var{nr},@var{nc}] = spfind( @var{a} )\n\
    @itemize @w\n\
returns column vectors @var{i},@var{j},@var{v} such that@*\n\
@var{a}= sparse(@var{i},@var{j},@var{v},@var{nr},@var{nc})\n\
    @end itemize\n\
@end enumerate\n\
@seealso{sparse}\n\
@end deftypefn")
{
   octave_value_list retval;
   int nargin = args.length ();

   if (nargin != 1) 
     {
       print_usage ("spfind");
       return retval;
     }
      

   octave_value arg = args(0);

   if (arg.class_name () == "sparse")
     {
       if (arg.type_name () == "sparse matrix")
	 retval = sparse_find (args(0).sparse_matrix_value ());
       else if (arg.type_name () == "sparse complex matrix" ) 
	 retval = sparse_find (args(0).sparse_complex_matrix_value ());
       else if (arg.type_name () == "sparse bool matrix" ) 
	 retval = sparse_find (args(0).sparse_bool_matrix_value ());
       else 
	 gripe_wrong_type_arg ("spfind", arg);
     }
   else
     gripe_wrong_type_arg ("spfind", arg);
     
   if (nargout == 1 || nargout ==0 ) 
     { 
       // only find location as fortran index
       octave_value_list tmp;
       tmp(0) = retval(0) + (retval(1)-1)*retval(3);
       retval = tmp;
     }

   return retval;
}

#define SPARSE_DIM_ARG_BODY(NAME, FUNC) \
    int nargin = args.length(); \
    octave_value retval; \
    if ((nargin != 1 ) && (nargin != 2)) \
      print_usage (#NAME); \
    else { \
      int dim = (nargin == 1 ? -1 : args(1).int_value(true) - 1); \
      if (error_state) return retval; \
      if (dim < -1 || dim > 1) { \
	error (#NAME ": invalid dimension argument = %d", dim + 1); \
        return retval; \
      } \
      if (args(0).type_id () == \
	  octave_sparse_matrix::static_type_id () || args(0).type_id () == \
	  octave_sparse_bool_matrix::static_type_id ()) { \
	  retval = args(0).sparse_matrix_value () .FUNC (dim); \
      } else if (args(0).type_id () == \
		 octave_sparse_complex_matrix::static_type_id ()) { \
	  retval = args(0).sparse_complex_matrix_value () .FUNC (dim); \
      } else \
	  print_usage (#NAME); \
    } \
    return retval

// PKG_ADD: dispatch ("prod", "spprod", "sparse matrix");
// PKG_ADD: dispatch ("prod", "spprod", "sparse complex matrix");
// PKG_ADD: dispatch ("prod", "spprod", "sparse bool matrix");
DEFUN_DLD (spprod, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{y} =} spprod (@var{x},@var{dim})\n\
Product of elements along dimension @var{dim}.  If @var{dim} is omitted,\n\
it defaults to 1 (column-wise products).\n\
@end deftypefn\n\
@seealso{spsum, spsumsq}")
{
  SPARSE_DIM_ARG_BODY (spprod, prod);
}

// PKG_ADD: dispatch ("cumprod", "spcumprod", "sparse matrix");
// PKG_ADD: dispatch ("cumprod", "spcumprod", "sparse complex matrix");
// PKG_ADD: dispatch ("cumprod", "spcumprod", "sparse bool matrix");
DEFUN_DLD (spcumprod, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{y} =} spcumprod (@var{x},@var{dim})\n\
Cumulative product of elements along dimension @var{dim}.  If @var{dim}\n\
is omitted, it defaults to 1 (column-wise cumulative products).\n\
@end deftypefn\n\
@seealso{spcumsum}")
{
  SPARSE_DIM_ARG_BODY (spcumprod, cumprod);
}

// PKG_ADD: dispatch ("sum", "spsum", "sparse matrix");
// PKG_ADD: dispatch ("sum", "spsum", "sparse complex matrix");
// PKG_ADD: dispatch ("sum", "spsum", "sparse bool matrix");
DEFUN_DLD (spsum, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{y} =} spsum (@var{x},@var{dim})\n\
Sum of elements along dimension @var{dim}.  If @var{dim} is omitted, it\n\
defaults to 1 (column-wise sum).\n\
@end deftypefn\n\
@seealso{spprod, spsumsq}")
{
  SPARSE_DIM_ARG_BODY (spsum, sum);
}

// PKG_ADD: dispatch ("cumsum", "spcumsum", "sparse matrix");
// PKG_ADD: dispatch ("cumsum", "spcumsum", "sparse complex matrix");
// PKG_ADD: dispatch ("cumsum", "spcumsum", "sparse bool matrix");
DEFUN_DLD (spcumsum, args, , 
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{y} =} spcumsum (@var{x},@var{dim})\n\
Cumulative sum of elements along dimension @var{dim}.  If @var{dim}\n\
is omitted, it defaults to 1 (column-wise cumulative sums).\n\
@end deftypefn\n\
@seealso{spcumprod}")
{
  SPARSE_DIM_ARG_BODY (spcumsum, cumsum);
}

// PKG_ADD: dispatch ("sumsq", "spsumsq", "sparse matrix");
// PKG_ADD: dispatch ("sumsq", "spsumsq", "sparse complex matrix");
// PKG_ADD: dispatch ("sumsq", "spsumsq", "sparse bool matrix");
DEFUN_DLD (spsumsq, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{y} =} spsumsq (@var{x},@var{dim})\n\
Sum of squares of elements along dimension @var{dim}.  If @var{dim}\n\
is omitted, it defaults to 1 (column-wise sum of squares).\n\
This function is equivalent to computing\n\
@example\n\
spsum (x .* spconj (x), dim)\n\
@end example\n\
but it uses less memory and avoids calling @code{spconj} if @var{x} is\n\
real.\n\
@end deftypefn\n\
@seealso{spprod, spsum}")
{
  SPARSE_DIM_ARG_BODY (spsumsq, sumsq);
}

#define MINMAX_BODY(FCN) \
 \
  octave_value_list retval;  \
 \
  int nargin = args.length (); \
 \
  if (nargin < 1 || nargin > 3 || nargout > 2) \
    { \
      print_usage (#FCN); \
      return retval; \
    } \
 \
  octave_value arg1; \
  octave_value arg2; \
  octave_value arg3; \
 \
  switch (nargin) \
    { \
    case 3: \
      arg3 = args(2); \
 \
    case 2: \
      arg2 = args(1); \
 \
    case 1: \
      arg1 = args(0); \
      break; \
 \
    default: \
      panic_impossible (); \
      break; \
    } \
 \
  int dim; \
  dim_vector dv = ((const octave_sparse_matrix&) arg1) .dims (); \
  if (error_state) \
    { \
      gripe_wrong_type_arg (#FCN, arg1);  \
      return retval; \
    } \
 \
  if (nargin == 3) \
    { \
      dim = arg3.nint_value () - 1;  \
      if (dim < 0 || dim >= dv.length ()) \
        { \
	  error ("%s: invalid dimension", #FCN); \
	  return retval; \
	} \
    } \
  else \
    { \
      dim = 0; \
      while ((dim < dv.length ()) && (dv (dim) <= 1)) \
	dim++; \
      if (dim == dv.length ()) \
	dim = 0; \
    } \
 \
  bool single_arg = (nargin == 1) || arg2.is_empty();	\
 \
  if (single_arg && (nargout == 1 || nargout == 0)) \
    { \
      if (arg1.type_id () == octave_sparse_matrix::static_type_id ()) \
	retval(0) = arg1.sparse_matrix_value () .FCN (dim); \
      else if (arg1.type_id () == \
	       octave_sparse_complex_matrix::static_type_id ()) \
	retval(0) = arg1.sparse_complex_matrix_value () .FCN (dim); \
      else \
	gripe_wrong_type_arg (#FCN, arg1); \
    } \
  else if (single_arg && nargout == 2) \
    { \
      Array2<octave_idx_type> index; \
 \
      if (arg1.type_id () == octave_sparse_matrix::static_type_id ()) \
	retval(0) = arg1.sparse_matrix_value () .FCN (index, dim); \
      else if (arg1.type_id () == \
	       octave_sparse_complex_matrix::static_type_id ()) \
	retval(0) = arg1.sparse_complex_matrix_value () .FCN (index, dim); \
      else \
	gripe_wrong_type_arg (#FCN, arg1); \
 \
      octave_idx_type len = index.numel (); \
 \
      if (len > 0) \
	{ \
	  double nan_val = lo_ieee_nan_value (); \
 \
	  NDArray idx (index.dims ()); \
 \
	  for (octave_idx_type i = 0; i < len; i++) \
	    { \
	      OCTAVE_QUIT; \
	      octave_idx_type tmp = index.elem (i) + 1; \
	      idx.elem (i) = (tmp <= 0) \
		? nan_val : static_cast<double> (tmp); \
	    } \
 \
	  retval(1) = idx; \
	} \
      else \
	retval(1) = NDArray (); \
    } \
  else \
    { \
      int arg1_is_scalar = arg1.is_scalar_type (); \
      int arg2_is_scalar = arg2.is_scalar_type (); \
 \
      int arg1_is_complex = arg1.is_complex_type (); \
      int arg2_is_complex = arg2.is_complex_type (); \
 \
      if (arg1_is_scalar) \
	{ \
	  if (arg1_is_complex || arg2_is_complex) \
	    { \
	      Complex c1 = arg1.complex_value (); \
	      \
	      SparseComplexMatrix m2 = arg2.sparse_complex_matrix_value (); \
	      \
	      if (! error_state) \
		{ \
		  SparseComplexMatrix result = FCN (c1, m2); \
		  if (! error_state) \
		    retval(0) = result; \
		} \
	    } \
	  else \
	    { \
	      double d1 = arg1.double_value (); \
	      SparseMatrix m2 = arg2.sparse_matrix_value (); \
	      \
	      if (! error_state) \
		{ \
		  SparseMatrix result = FCN (d1, m2); \
		  if (! error_state) \
		    retval(0) = result; \
		} \
	    } \
	} \
      else if (arg2_is_scalar) \
	{ \
	  if (arg1_is_complex || arg2_is_complex) \
	    { \
	      SparseComplexMatrix m1 = arg1.sparse_complex_matrix_value (); \
 \
	      if (! error_state) \
		{ \
		  Complex c2 = arg2.complex_value (); \
		  SparseComplexMatrix result = FCN (m1, c2); \
		  if (! error_state) \
		    retval(0) = result; \
		} \
	    } \
	  else \
	    { \
	      SparseMatrix m1 = arg1.sparse_matrix_value (); \
 \
	      if (! error_state) \
		{ \
		  double d2 = arg2.double_value (); \
		  SparseMatrix result = FCN (m1, d2); \
		  if (! error_state) \
		    retval(0) = result; \
		} \
	    } \
	} \
      else \
	{ \
	  if (arg1_is_complex || arg2_is_complex) \
	    { \
	      SparseComplexMatrix m1 = arg1.sparse_complex_matrix_value (); \
 \
	      if (! error_state) \
		{ \
		  SparseComplexMatrix m2 = arg2.sparse_complex_matrix_value (); \
 \
		  if (! error_state) \
		    { \
		      SparseComplexMatrix result = FCN (m1, m2); \
		      if (! error_state) \
			retval(0) = result; \
		    } \
		} \
	    } \
	  else \
	    { \
	      SparseMatrix m1 = arg1.sparse_matrix_value (); \
 \
	      if (! error_state) \
		{ \
		  SparseMatrix m2 = arg2.sparse_matrix_value (); \
 \
		  if (! error_state) \
		    { \
		      SparseMatrix result = FCN (m1, m2); \
		      if (! error_state) \
			retval(0) = result; \
		    } \
		} \
	    } \
	} \
    } \
 \
  return retval

// PKG_ADD: dispatch ("min", "spmin", "sparse matrix");
// PKG_ADD: dispatch ("min", "spmin", "sparse complex matrix");
// PKG_ADD: dispatch ("min", "spmin", "sparse bool matrix");
DEFUN_DLD (spmin, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} spmin (@var{x}, @var{y}, @var{dim})\n\
@deftypefnx {Mapping Function} {[@var{w}, @var{iw}] =} spmin (@var{x})\n\
@cindex Utility Functions\n\
For a vector argument, return the minimum value.  For a matrix\n\
argument, return the minimum value from each column, as a row\n\
vector, or over the dimension @var{dim} if defined. For two matrices\n\
(or a matrix and scalar), return the pair-wise minimum.\n\
Thus,\n\
\n\
@example\n\
min (min (@var{x}))\n\
@end example\n\
\n\
@noindent\n\
returns the smallest element of @var{x}, and\n\
\n\
@example\n\
@group\n\
min (2:5, pi)\n\
    @result{}  2.0000  3.0000  3.1416  3.1416\n\
@end group\n\
@end example\n\
@noindent\n\
compares each element of the range @code{2:5} with @code{pi}, and\n\
returns a row vector of the minimum values.\n\
\n\
For complex arguments, the magnitude of the elements are used for\n\
comparison.\n\
\n\
If called with one input and two output arguments,\n\
@code{min} also returns the first index of the\n\
minimum value(s). Thus,\n\
\n\
@example\n\
@group\n\
[x, ix] = min ([1, 3, 0, 2, 5])\n\
    @result{}  x = 0\n\
        ix = 3\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  MINMAX_BODY (min);
}

// PKG_ADD: dispatch ("max", "spmax", "sparse matrix");
// PKG_ADD: dispatch ("max", "spmax", "sparse complex matrix");
// PKG_ADD: dispatch ("max", "spmax", "sparse bool matrix");
DEFUN_DLD (spmax, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} spmax (@var{x}, @var{y}, @var{dim})\n\
@deftypefnx {Mapping Function} {[@var{w}, @var{iw}] =} spmax (@var{x})\n\
@cindex Utility Functions\n\
For a vector argument, return the maximum value.  For a matrix\n\
argument, return the maximum value from each column, as a row\n\
vector, or over the dimension @var{dim} if defined. For two matrices\n\
(or a matrix and scalar), return the pair-wise maximum.\n\
Thus,\n\
\n\
@example\n\
max (max (@var{x}))\n\
@end example\n\
\n\
@noindent\n\
returns the largest element of @var{x}, and\n\
\n\
@example\n\
@group\n\
max (2:5, pi)\n\
    @result{}  3.1416  3.1416  4.0000  5.0000\n\
@end group\n\
@end example\n\
@noindent\n\
compares each element of the range @code{2:5} with @code{pi}, and\n\
returns a row vector of the maximum values.\n\
\n\
For complex arguments, the magnitude of the elements are used for\n\
comparison.\n\
\n\
If called with one input and two output arguments,\n\
@code{max} also returns the first index of the\n\
maximum value(s). Thus,\n\
\n\
@example\n\
@group\n\
[x, ix] = max ([1, 3, 5, 2, 5])\n\
    @result{}  x = 5\n\
        ix = 3\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  MINMAX_BODY (max);
}

// PKG_ADD: dispatch ("atan2", "spatan2", "sparse matrix");
// PKG_ADD: dispatch ("atan2", "spatan2", "sparse complex matrix");
// PKG_ADD: dispatch ("atan2", "spatan2", "sparse bool matrix");
DEFUN_DLD (spatan2, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} spatan2 (@var{y}, @var{x})\n\
Compute atan (Y / X) for corresponding sparse matrix elements of Y and X.\n\
The result is in range -pi to pi.\n\
@end deftypefn\n")
{
  octave_value retval;
  int nargin = args.length ();
  if (nargin == 2) {  
    SparseMatrix a, b;
    double da, db;
    bool is_double_a = false;
    bool is_double_b = false;

    if (args(0).is_scalar_type ())
      {
	is_double_a = true;
	da = args(0).double_value();
      }
    else 
      a = args(0).sparse_matrix_value ();

    if (args(1).is_scalar_type ())
      {
	is_double_b = true;
	db = args(1).double_value();
      }
    else 
      b = args(1).sparse_matrix_value ();

    if (is_double_a && is_double_b)
      retval = Matrix (1, 1, atan2(da, db));
    else if (is_double_a)
      retval = atan2 (da, b);
    else if (is_double_b)
      retval = atan2 (a, db);
    else
      retval = atan2 (a, b);

  } else
    print_usage("spatan2");

  return retval;
}

static octave_value
make_spdiag (const octave_value& a, const octave_value& b)
{
  octave_value retval;

  if (a.is_complex_type ())
    {
      SparseComplexMatrix m = a.sparse_complex_matrix_value ();
      octave_idx_type k = b.nint_value(true);

      if (error_state) 
	return retval;

      octave_idx_type nr = m.rows ();
      octave_idx_type nc = m.columns ();
	
      if (nr == 0 || nc == 0)
	retval = m;
      else if (nr == 1 || nc == 1) 
	{
	  octave_idx_type roff = 0;
	  octave_idx_type coff = 0;
	  if (k > 0) 
	    {
	      roff = 0;
	      coff = k;
	    } 
	  else if (k < 0) 
	    {
	      k = -k;
	      roff = k;
	      coff = 0;
	    }

	  if (nr == 1) 
	    {
	      octave_idx_type n = nc + k;
	      octave_idx_type nz = m.nnz ();
	      SparseComplexMatrix r (n, n, nz);
	      for (octave_idx_type i = 0; i < coff+1; i++)
		r.xcidx (i) = 0;
	      for (octave_idx_type j = 0; j < nc; j++)
		{
		  for (octave_idx_type i = m.cidx(j); i < m.cidx(j+1); i++)
		    {
		      r.xdata (i) = m.data (i);
		      r.xridx (i) = j + roff;
		    }
		  r.xcidx (j+coff+1) = m.cidx(j+1);
		}
	      for (octave_idx_type i = nc+coff+1; i < n+1; i++)
		r.xcidx (i) = nz;
	      retval = r;
	    } 
	  else 
	    {
	      octave_idx_type n = nr + k;
	      octave_idx_type nz = m.nnz ();
	      octave_idx_type ii = 0;
	      octave_idx_type ir = m.ridx(0);
	      SparseComplexMatrix r (n, n, nz);
	      for (octave_idx_type i = 0; i < coff+1; i++)
		r.xcidx (i) = 0;
	      for (octave_idx_type i = 0; i < nr; i++)
		{
		  if (ir == i)
		    {
		      r.xdata (ii) = m.data (ii);
		      r.xridx (ii++) = ir + roff;
		      if (ii != nz)
			ir = m.ridx (ii);
		    }
		  r.xcidx (i+coff+1) = ii;
		}
	      for (octave_idx_type i = nr+coff+1; i < n+1; i++)
		r.xcidx (i) = nz;
	      retval = r;
	    }
	} 
      else 
	{
	  SparseComplexMatrix r = m.diag (k);
	  // Don't use numel, since it can overflow for very large matrices
	  if (r.rows () > 0 && r.cols () > 0)
	    retval = r;
	}
    } 
  else if (a.is_real_type ())
    {
      SparseMatrix m = a.sparse_matrix_value ();

      octave_idx_type k = b.nint_value(true);

      if (error_state) 
	return retval;

      octave_idx_type nr = m.rows ();
      octave_idx_type nc = m.columns ();
	
      if (nr == 0 || nc == 0)
	retval = m;
      else if (nr == 1 || nc == 1) 
	{
	  octave_idx_type roff = 0;
	  octave_idx_type coff = 0;
	  if (k > 0) 
	    {
	      roff = 0;
	      coff = k;
	    } 
	  else if (k < 0) 
	    {
	      k = -k;
	      roff = k;
	      coff = 0;
	    }

	  if (nr == 1) 
	    {
	      octave_idx_type n = nc + k;
	      octave_idx_type nz = m.nnz ();
	      SparseMatrix r (n, n, nz);

	      for (octave_idx_type i = 0; i < coff+1; i++)
		r.xcidx (i) = 0;
	      for (octave_idx_type j = 0; j < nc; j++)
		{
		  for (octave_idx_type i = m.cidx(j); i < m.cidx(j+1); i++)
		    {
		      r.xdata (i) = m.data (i);
		      r.xridx (i) = j + roff;
		    }
		  r.xcidx (j+coff+1) = m.cidx(j+1);
		}
	      for (octave_idx_type i = nc+coff+1; i < n+1; i++)
		r.xcidx (i) = nz;
	      retval = r;
	    } 
	  else 
	    {
	      octave_idx_type n = nr + k;
	      octave_idx_type nz = m.nnz ();
	      octave_idx_type ii = 0;
	      octave_idx_type ir = m.ridx(0);
	      SparseMatrix r (n, n, nz);
	      for (octave_idx_type i = 0; i < coff+1; i++)
		r.xcidx (i) = 0;
	      for (octave_idx_type i = 0; i < nr; i++)
		{
		  if (ir == i)
		    {
		      r.xdata (ii) = m.data (ii);
		      r.xridx (ii++) = ir + roff;
		      if (ii != nz)
			ir = m.ridx (ii);
		    }
		  r.xcidx (i+coff+1) = ii;
		}
	      for (octave_idx_type i = nr+coff+1; i < n+1; i++)
		r.xcidx (i) = nz;
	      retval = r;
	    }
	} 
      else 
	{
	  SparseMatrix r = m.diag (k);
	  if (r.rows () > 0 && r.cols () > 0)
	    retval = r;
	}
    }
  else
    gripe_wrong_type_arg ("spdiag", a);

  return retval;
}

// PKG_ADD: dispatch ("diag", "spdiag", "sparse matrix");
// PKG_ADD: dispatch ("diag", "spdiag", "sparse complex matrix");
// PKG_ADD: dispatch ("diag", "spdiag", "sparse bool matrix");
DEFUN_DLD (spdiag, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} spdiag (@var{v}, @var{k})\n\
Return a diagonal matrix with the sparse vector @var{v} on diagonal\n\
@var{k}. The second argument is optional. If it is positive, the vector is\n\
placed on the @var{k}-th super-diagonal. If it is negative, it is placed\n\
on the @var{-k}-th sub-diagonal.  The default value of @var{k} is 0, and\n\
the vector is placed on the main diagonal.  For example,\n\
\n\
@example\n\
spdiag ([1, 2, 3], 1)\n\
ans =\n\
\n\
Compressed Column Sparse (rows=4, cols=4, nnz=3)\n\
  (1 , 2) -> 1\n\
  (2 , 3) -> 2\n\
  (3 , 4) -> 3\n\
@end example\n\
\n\
@end deftypefn\n\
@seealso{diag}")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_defined ())
    retval = make_spdiag (args(0), octave_value(0.));
  else if (nargin == 2 && args(0).is_defined () && args(1).is_defined ())
    retval = make_spdiag (args(0), args(1));
  else
    print_usage ("spdiag");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
