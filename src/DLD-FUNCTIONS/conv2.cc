/*

Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
              Andy Adler
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

#include "oct-convn.h"

#include "defun-dld.h"
#include "error.h"
#include "oct-obj.h"
#include "utils.h"

enum Shape { SHAPE_FULL, SHAPE_SAME, SHAPE_VALID };

/*
%!test
%! b = [0,1,2,3;1,8,12,12;4,20,24,21;7,22,25,18];
%! assert(conv2([0,1;1,2],[1,2,3;4,5,6;7,8,9]),b);

%!test
%! b = single([0,1,2,3;1,8,12,12;4,20,24,21;7,22,25,18]);
%! assert(conv2(single([0,1;1,2]),single([1,2,3;4,5,6;7,8,9])),b);

%!assert (conv2 (1:3, 1:2, [1,2;3,4;5,6]),
%!        [1,4,4;5,18,16;14,48,40;19,62,48;15,48,36;]);
*/


DEFUN_DLD (conv2, args, ,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {y =} conv2 (@var{a}, @var{b}, @var{shape})\n\
@deftypefnx {Loadable Function} {y =} conv2 (@var{v1}, @var{v2}, @var{M}, @var{shape})\n\
\n\
Returns 2D convolution of @var{a} and @var{b} where the size\n\
of @var{c} is given by\n\
\n\
@table @asis\n\
@item @var{shape} = 'full'\n\
returns full 2-D convolution\n\
\n\
@item @var{shape} = 'same'\n\
same size as a. 'central' part of convolution\n\
\n\
@item @var{shape} = 'valid'\n\
only parts which do not include zero-padded edges\n\
@end table\n\
\n\
By default @var{shape} is 'full'.  When the third argument is a matrix\n\
returns the convolution of the matrix @var{M} by the vector @var{v1}\n\
in the column direction and by vector @var{v2} in the row direction\n\
@end deftypefn")
{
  octave_value retval;
  octave_value tmp;
  int nargin = args.length ();
  std::string shape = "full"; //default
  bool separable = false;
  convn_type ct;

  if (nargin < 2)
    {
     print_usage ();
     return retval;
    }
  else if (nargin == 3)
    {
      if (args(2).is_string ())
        shape = args(2).string_value ();
      else
        separable = true;
    }
  else if (nargin >= 4)
    {
      separable = true;
      shape = args(3).string_value ();
    }

  if (shape == "full")
    ct = convn_full;
  else if (shape == "same")
    ct = convn_same;
  else if (shape == "valid")
    ct = convn_valid;
  else
    {
      error ("conv2: shape type not valid");
      print_usage ();
      return retval;
    }

   if (separable)
     {
      // If user requests separable, check first two params are vectors

       if (! (1 == args(0).rows () || 1 == args(0).columns ())
           || ! (1 == args(1).rows () || 1 == args(1).columns ()))
         {
           print_usage ();
           return retval;
         }

       if (args(0).is_single_type () || 
           args(1).is_single_type () || 
           args(2).is_single_type ())
         {
           if (args(0).is_complex_type ()
               || args(1).is_complex_type ()
               || args(2).is_complex_type ())
             {
               FloatComplexMatrix a (args(2).float_complex_matrix_value ());
               if (args(1).is_real_type () && args(2).is_real_type ())
                 {
                   FloatColumnVector v1 (args(0).float_vector_value ());
                   FloatRowVector v2 (args(1).float_vector_value ());
                   retval = convn (a, v1, v2, ct);
                 }
               else
                 {
                   FloatComplexColumnVector v1 (args(0).float_complex_vector_value ());
                   FloatComplexRowVector v2 (args(1).float_complex_vector_value ());
                   retval = convn (a, v1, v2, ct);
                 }
             }
           else
             {
               FloatColumnVector v1 (args(0).float_vector_value ());
               FloatRowVector v2 (args(1).float_vector_value ());
               FloatMatrix a (args(2).float_matrix_value ());
               retval = convn (a, v1, v2, ct);
             }
         }
       else
         {
           if (args(0).is_complex_type ()
               || args(1).is_complex_type ()
               || args(2).is_complex_type ())
             {
               ComplexMatrix a (args(2).complex_matrix_value ());
               if (args(1).is_real_type () && args(2).is_real_type ())
                 {
                   ColumnVector v1 (args(0).vector_value ());
                   RowVector v2 (args(1).vector_value ());
                   retval = convn (a, v1, v2, ct);
                 }
               else
                 {
                   ComplexColumnVector v1 (args(0).complex_vector_value ());
                   ComplexRowVector v2 (args(1).complex_vector_value ());
                   retval = convn (a, v1, v2, ct);
                 }
             }
           else
             {
               ColumnVector v1 (args(0).vector_value ());
               RowVector v2 (args(1).vector_value ());
               Matrix a (args(2).matrix_value ());
               retval = convn (a, v1, v2, ct);
             }
         }
     } // if (separable)
   else
     {
       if (args(0).is_single_type () || 
           args(1).is_single_type ())
         {
           if (args(0).is_complex_type ()
               || args(1).is_complex_type ())
             {
               FloatComplexMatrix a (args(0).float_complex_matrix_value ());
               if (args(1).is_real_type ())
                 {
                   FloatMatrix b (args(1).float_matrix_value ());
                   retval = convn (a, b, ct);
                 }
               else
                 {
                   FloatComplexMatrix b (args(1).float_complex_matrix_value ());
                   retval = convn (a, b, ct);
                 }
             }
           else
             {
               FloatMatrix a (args(0).float_matrix_value ());
               FloatMatrix b (args(1).float_matrix_value ());
               retval = convn (a, b, ct);
             }
         }
       else
         {
           if (args(0).is_complex_type ()
               || args(1).is_complex_type ())
             {
               ComplexMatrix a (args(0).complex_matrix_value ());
               if (args(1).is_real_type ())
                 {
                   Matrix b (args(1).matrix_value ());
                   retval = convn (a, b, ct);
                 }
               else
                 {
                   ComplexMatrix b (args(1).complex_matrix_value ());
                   retval = convn (a, b, ct);
                 }
             }
           else
             {
               Matrix a (args(0).matrix_value ());
               Matrix b (args(1).matrix_value ());
               retval = convn (a, b, ct);
             }
         }

     } // if (separable)

   return retval;
}

DEFUN_DLD (convn, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {y =} conv2 (@var{a}, @var{b}, @var{shape})\n\
\n\
Returns n-D convolution of @var{a} and @var{b} where the size\n\
of @var{c} is given by\n\
\n\
@table @asis\n\
@item @var{shape} = 'full'\n\
returns full n-D convolution\n\
\n\
@item @var{shape} = 'same'\n\
same size as a. 'central' part of convolution\n\
\n\
@item @var{shape} = 'valid'\n\
only parts which do not include zero-padded edges\n\
@end table\n\
\n\
By default @var{shape} is 'full'.\n\
@end deftypefn")
{
  octave_value retval;
  octave_value tmp;
  int nargin = args.length ();
  std::string shape = "full"; //default
  bool separable = false;
  convn_type ct;

  if (nargin < 2 || nargin > 3)
    {
     print_usage ();
     return retval;
    }
  else if (nargin == 3)
    {
      if (args(2).is_string ())
        shape = args(2).string_value ();
      else
        separable = true;
    }

  if (shape == "full")
    ct = convn_full;
  else if (shape == "same")
    ct = convn_same;
  else if (shape == "valid")
    ct = convn_valid;
  else
    {
      error ("convn: shape type not valid");
      print_usage ();
      return retval;
    }

  if (args(0).is_single_type () || 
      args(1).is_single_type ())
    {
      if (args(0).is_complex_type ()
          || args(1).is_complex_type ())
        {
          FloatComplexNDArray a (args(0).float_complex_array_value ());
          if (args(1).is_real_type ())
            {
              FloatNDArray b (args(1).float_array_value ());
              retval = convn (a, b, ct);
            }
          else
            {
              FloatComplexNDArray b (args(1).float_complex_array_value ());
              retval = convn (a, b, ct);
            }
        }
      else
        {
          FloatNDArray a (args(0).float_array_value ());
          FloatNDArray b (args(1).float_array_value ());
          retval = convn (a, b, ct);
        }
    }
  else
    {
      if (args(0).is_complex_type ()
          || args(1).is_complex_type ())
        {
          ComplexNDArray a (args(0).complex_array_value ());
          if (args(1).is_real_type ())
            {
              NDArray b (args(1).array_value ());
              retval = convn (a, b, ct);
            }
          else
            {
              ComplexNDArray b (args(1).complex_array_value ());
              retval = convn (a, b, ct);
            }
        }
      else
        {
          NDArray a (args(0).array_value ());
          NDArray b (args(1).array_value ());
          retval = convn (a, b, ct);
        }
    }

   return retval;
}
