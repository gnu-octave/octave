/*

Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009 David Bateman
Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 Andy Adler
Copyright (C) 2010 VZLU Prague

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
#include "unwind-prot.h"

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
   int nargin = args.length ();

   // Temporarily disable sparse_auto_mutate if set (it's obsolete anyway).
   unwind_protect frame;
   frame.protect_var (Vsparse_auto_mutate);
   Vsparse_auto_mutate = false;

   if (nargin == 1)
     {
       octave_value arg = args (0);
       if (arg.is_bool_type ())
         retval = arg.sparse_bool_matrix_value ();
       else if (arg.is_complex_type ())
         retval = arg.sparse_complex_matrix_value ();
       else if (arg.is_numeric_type ())
         retval = arg.sparse_matrix_value ();
       else
         gripe_wrong_type_arg ("sparse", arg);
     }
   else if (nargin == 2)
     {
       octave_idx_type m = 0, n = 0;
       if (args(0).is_scalar_type () && args(1).is_scalar_type ())
         {
           m = args(0).idx_type_value ();
           n = args(1).idx_type_value ();
         }
       else
         error ("sparse: expecting scalar dimensions");

       if (! error_state)
         {
           if (m >= 0 && n >= 0)
             retval = SparseMatrix (m, n);
           else
             error ("sparse: dimensions must be nonnegative");
         }
     }
   else if (nargin >= 3)
     {
       bool summation = true;
       if (nargin > 3 && args(nargin-1).is_string ())
         {
           std::string opt = args(nargin-1).string_value ();
           if (opt == "unique")
             summation = false;
           else if (opt == "sum" || opt == "summation")
             summation = true;
           else
             error ("sparse: invalid option: %s", opt.c_str ());

           nargin -= 1;
         }

       if (! error_state)
         {
           octave_idx_type m = -1, n = -1, nzmax = -1;
           if (nargin == 6)
             {
               nzmax = args(5).idx_type_value ();
               nargin --;
             }

           if (nargin == 5)
             {
               if (args(3).is_scalar_type () && args(4).is_scalar_type ())
                 {
                   m = args(3).idx_type_value ();
                   n = args(4).idx_type_value ();
                 }
               else
                 error ("sparse: expecting scalar dimensions");


               if (! error_state && (m < 0 || n < 0))
                 error ("sparse: dimensions must be nonnegative");
             }
           else if (nargin != 3)
             print_usage ();

           if (! error_state)
             {
               idx_vector i = args(0).index_vector ();
               idx_vector j = args(1).index_vector ();

               if (args(2).is_bool_type ())
                 retval = SparseBoolMatrix (args(2).bool_array_value (), i, j,
                                            m, n, summation, nzmax);
               else if (args(2).is_complex_type ())
                 retval = SparseComplexMatrix (args(2).complex_array_value (),
                                               i, j, m, n, summation, nzmax);
               else if (args(2).is_numeric_type ())
                 retval = SparseMatrix (args(2).array_value (), i, j,
                                        m, n, summation, nzmax);
               else
                 gripe_wrong_type_arg ("sparse", args(2));
             }

         }
     }

   return retval;
}

DEFUN_DLD (spalloc, args, ,
    "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{s} =} spalloc (@var{m}, @var{n}, @var{nz})\n\
Creates a @var{m}-by-@var{n} sparse matrix with preallocated space for at most\n\
@var{nz} nonzero elements. This is useful for building the matrix incrementally\n\
by a sequence of indexed assignments. Subsequent indexed assignments will reuse\n\
the pre-allocated memory, provided they are of one of the simple forms\n\
\n\
@itemize\n\
@item @code{@var{s}(I:J) = @var{x}}\n\
@item @code{@var{s}(:,I:J) = @var{x}}\n\
@item @code{@var{s}(K:L,I:J) = @var{x}}\n\
@end itemize\n\
\n\
@b{and} that the following conditions are met:\n\
\n\
@itemize\n\
@item the assignment does not decrease nnz(@var{S}).\n\
@item after the assignment, nnz(@var{S}) does not exceed @var{nz}.\n\
@item no index is out of bounds.\n\
@end itemize\n\
\n\
Partial movement of data may still occur, but in general the assignment will be more\n\
memory and time-efficient under these circumstances. In particular, it is possible\n\
to efficiently build a pre-allocated sparse matrix from contiguous block of columns.\n\
\n\
The amount of preallocated memory for a given matrix may be queried using the function\n\
@code{nzmax}.\n\
@seealso{nzmax, sparse}\n\
@end deftypefn")
{
   octave_value retval;
   int nargin = args.length ();

   if (nargin == 2 || nargin == 3)
     {
       octave_idx_type m = args(0).idx_type_value ();
       octave_idx_type n = args(1).idx_type_value ();
       octave_idx_type nz = 0;
       if (nargin == 3)
         nz = args(2).idx_type_value ();
       if (error_state)
         ;
       else if (m >= 0 && n >= 0 && nz >= 0)
         retval = SparseMatrix (dim_vector (m, n), nz);
       else
         error ("spalloc: m,n,nz must be non-negative");
     }
   else
     print_usage ();

   return retval;
}
