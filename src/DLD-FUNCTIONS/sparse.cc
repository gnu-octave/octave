/*

Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009 David Bateman
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
     return octave_value (args(0).is_sparse_type ());
}

DEFUN_DLD (sparse, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{s} =} sparse (@var{a})\n\
@deftypefnx {Loadable Function} {@var{s} =} sparse (@var{i}, @var{j}, @var{sv}, @var{m}, @var{n}, @var{nzmax})\n\
@deftypefnx {Loadable Function} {@var{s} =} sparse (@var{i}, @var{j}, @var{sv})\n\
@deftypefnx {Loadable Function} {@var{s} =} sparse (@var{i}, @var{j}, @var{s}, @var{m}, @var{n}, \"unique\")\n\
@deftypefnx {Loadable Function} {@var{s} =} sparse (@var{m}, @var{n})\n\
Create a sparse matrix from the full matrix or row, column, value triplets.\n\
If @var{a} is a full matrix, convert it to a sparse matrix representation,\n\
removing all zero values in the process.\n\
\n\
Given the integer index vectors @var{i} and @var{j}, a 1-by-@code{nnz} vector\n\
of real of complex values @var{sv}, overall dimensions @var{m} and @var{n}\n\
of the sparse matrix.  The argument @code{nzmax} is ignored but accepted for\n\
compatibility with @sc{matlab}.  If @var{m} or @var{n} are not specified their\n\
values are derived from the maximum index in the vectors @var{i} and @var{j}\n\
as given by @code{@var{m} = max (@var{i})}, @code{@var{n} = max (@var{j})}.\n\
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
Given the option \"unique\". if more than two values are specified for the\n\
same @var{i}, @var{j} indices, the last specified value will be used.\n\
\n\
@code{sparse(@var{m}, @var{n})} is equivalent to\n\
@code{sparse ([], [], [], @var{m}, @var{n}, 0)}\n\
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

       if (arg.is_sparse_type ())
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
       else if (arg.is_diag_matrix ())
         {
           if (arg.is_complex_type ())
             {
               SparseComplexMatrix sm = arg.sparse_complex_matrix_value ();
               retval = new octave_sparse_complex_matrix (sm);
             }
           else
             {
               SparseMatrix sm = arg.sparse_matrix_value ();
               retval = new octave_sparse_matrix (sm);
             }
         }
       else if (arg.is_perm_matrix ())
         {
           SparseMatrix sm = arg.sparse_matrix_value ();
           retval = new octave_sparse_matrix (sm);
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
