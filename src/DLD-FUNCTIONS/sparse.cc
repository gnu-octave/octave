/*

Copyright (C) 2004, 2005, 2006, 2007 David Bateman
Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 Andy Adler

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
  return (arg.is_sparse_type ());
}

DEFUN_DLD (issparse, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} issparse (@var{expr})\n\
Return 1 if the value of the expression @var{expr} is a sparse matrix.\n\
@end deftypefn") 
{
   if (args.length() != 1) 
     {
       print_usage ();
       return octave_value ();
     }
   else 
     return octave_value (is_sparse (args(0)));
}

DEFUN_DLD (sparse, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{s} =} sparse (@var{a})\n\
Create a sparse matrix from the full matrix @var{a}.\n\
is forced back to a full matrix is resulting matrix is sparse\n\
\n\
@deftypefnx {Loadable Function} {@var{s} =} sparse (@var{i}, @var{j}, @var{sv}, @var{m}, @var{n}, @var{nzmax})\n\
Create a sparse matrix given integer index vectors @var{i} and @var{j},\n\
a 1-by-@code{nnz} vector of real of complex values @var{sv}, overall\n\
dimensions @var{m} and @var{n} of the sparse matrix.  The argument\n\
@code{nzmax} is ignored but accepted for compatibility with @sc{Matlab}.\n\
\n\
@strong{Note}: if multiple values are specified with the same\n\
@var{i}, @var{j} indices, the corresponding values in @var{s} will\n\
be added.\n\
\n\
The following are all equivalent:\n\
\n\
@example\n\
@group\n\
s = sparse (i, j, s, m, n)\n\
s = sparse (i, j, s, m, n, \"summation\")\n\
s = sparse (i, j, s, m, n, \"sum\")\n\
@end group\n\
@end example\n\
\n\
@deftypefnx {Loadable Function} {@var{s} =} sparse (@var{i}, @var{j}, @var{s}, @var{m}, @var{n}, \"unique\")\n\
Same as above, except that if more than two values are specified for the\n\
same @var{i}, @var{j} indices, the last specified value will be used.\n\
\n\
@deftypefnx {Loadable Function} {@var{s} =} sparse (@var{i}, @var{j}, @var{sv})\n\
Uses @code{@var{m} = max (@var{i})}, @code{@var{n} = max (@var{j})}\n\
\n\
@deftypefnx {Loadable Function} {@var{s} =} sparse (@var{m}, @var{n})\n\
Equivalent to @code{sparse ([], [], [], @var{m}, @var{n}, 0)}\n\
\n\
If any of @var{sv}, @var{i} or @var{j} are scalars, they are expanded\n\
to have a common size.\n\
@seealso{full}\n\
@end deftypefn")
{
   octave_value retval;

   // WARNING: This function should always use constructions like
   //   retval = new octave_sparse_matrix (sm);
   // To avoid calling the maybe_mutate function. This is the only
   // function that should not call maybe_mutate

   int nargin= args.length();
   if (nargin < 1 || (nargin == 4 && !args(3).is_string ()) || nargin > 6) 
     {
       print_usage ();
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

   if (nargin == 1)
     {
       octave_value arg = args (0);

       if (is_sparse (arg))
	 {
	   if (use_complex) 
	     {
	       SparseComplexMatrix sm = arg.sparse_complex_matrix_value ();
	       retval = new octave_sparse_complex_matrix (sm);
	     }
	   else if (use_bool) 
	     {
	       SparseBoolMatrix sm = arg.sparse_bool_matrix_value ();
	       retval = new octave_sparse_bool_matrix (sm);
	     }
	   else
	     {
	       SparseMatrix sm = arg.sparse_matrix_value ();
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
	   if (args(0).numel () == 1 && args(1).numel () == 1)
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
	     error ("sparse: expecting scalar values");
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

  if (args.length() < 1)
    {
      print_usage ();
      return retval;
    }

  if (args(0).is_sparse_type ())
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

#define SPARSE_DIM_ARG_BODY(NAME, FUNC) \
    int nargin = args.length(); \
    octave_value retval; \
    if ((nargin != 1 ) && (nargin != 2)) \
      print_usage (); \
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
	  print_usage (); \
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
@seealso{spsum, spsumsq}\n\
@end deftypefn")
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
@seealso{spcumsum}\n\
@end deftypefn")
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
@seealso{spprod, spsumsq}\n\
@end deftypefn")
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
@seealso{spcumprod}\n\
@end deftypefn")
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
@seealso{spprod, spsum}\n\
@end deftypefn")
{
  SPARSE_DIM_ARG_BODY (spsumsq, sumsq);
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
	      octave_idx_type nz = m.nzmax ();
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
	      octave_idx_type nz = m.nzmax ();
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
	      octave_idx_type nz = m.nzmax ();
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
	      octave_idx_type nz = m.nzmax ();
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

static octave_value
make_spdiag (const octave_value& a)
{
  octave_value retval;
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.columns ();

  if (nr == 0 || nc == 0)
    retval = SparseMatrix ();
  else
    retval = make_spdiag (a, octave_value (0.));

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
@group\n\
spdiag ([1, 2, 3], 1)\n\
ans =\n\
\n\
Compressed Column Sparse (rows=4, cols=4, nnz=3)\n\
  (1 , 2) -> 1\n\
  (2 , 3) -> 2\n\
  (3 , 4) -> 3\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
Given a matrix argument, instead of a vector, @code{spdiag} extracts the\n\
@var{k}-th diagonal of the sparse matrix.\n\
@seealso{diag}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1 && args(0).is_defined ())
    retval = make_spdiag (args(0));
  else if (nargin == 2 && args(0).is_defined () && args(1).is_defined ())
    retval = make_spdiag (args(0), args(1));
  else
    print_usage ();

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
