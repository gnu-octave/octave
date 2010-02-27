/*

Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
              Andy Adler

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

#include "defun-dld.h"
#include "error.h"
#include "oct-obj.h"
#include "utils.h"

enum Shape { SHAPE_FULL, SHAPE_SAME, SHAPE_VALID };

#if !defined (CXX_NEW_FRIEND_TEMPLATE_DECL)
extern MArray<double>
conv2 (const MArray<double>&, const MArray<double>&, const MArray<double>&,
       Shape);

extern MArray<Complex>
conv2 (const MArray<Complex>&, const MArray<Complex>&,
       const MArray<Complex>&, Shape);

extern MArray<float>
conv2 (const MArray<float>&, const MArray<float>&, const MArray<float>&,
       Shape);

extern MArray<FloatComplex>
conv2 (const MArray<FloatComplex>&, const MArray<FloatComplex>&,
       const MArray<FloatComplex>&, Shape);
#endif

template <class T>
MArray<T>
conv2 (const MArray<T>& R, const MArray<T>& C, const MArray<T>& A, Shape ishape)
{
  octave_idx_type  Rn = R.length ();
  octave_idx_type  Cm = C.length ();
  octave_idx_type  Am = A.rows ();
  octave_idx_type  An = A.columns ();

  // Calculate the size of the output matrix:
  // in order to stay Matlab compatible, it is based
  // on the third parameter if it's separable, and the
  // first if it's not

  octave_idx_type outM = 0;
  octave_idx_type outN = 0;
  octave_idx_type edgM = 0;
  octave_idx_type edgN = 0;

  switch (ishape)
    {
      case SHAPE_FULL:
        outM = Am + Cm - 1;
        outN = An + Rn - 1;
        edgM = Cm - 1;
        edgN = Rn - 1;
        break;

      case SHAPE_SAME:
        outM = Am;
        outN = An;
        // Follow the Matlab convention (ie + instead of -)
        edgM = (Cm - 1) /2;
        edgN = (Rn - 1) /2;
        break;

      case SHAPE_VALID:
        outM = Am - Cm + 1;
        outN = An - Rn + 1;
        if (outM < 0)
          outM = 0;
        if (outN < 0)
          outN = 0;
        edgM = edgN = 0;
        break;

      default:
        error ("conv2: invalid value of parameter ishape");
    }

  MArray<T> O (outM, outN);

  // X accumulates the 1-D conv for each row, before calculating
  //    the convolution in the other direction
  // There is no efficiency advantage to doing it in either direction
  //     first

  MArray<T> X (An, 1);

  for (octave_idx_type oi = 0; oi < outM; oi++)
    {
      for (octave_idx_type oj = 0; oj < An; oj++)
        {
          T sum = 0;

          octave_idx_type ci = Cm - 1 - std::max (0, edgM-oi);
          octave_idx_type ai = std::max (0, oi-edgM);
          const T* Ad = A.data() + ai + Am*oj;
          const T* Cd = C.data() + ci;
          for ( ; ci >= 0 && ai < Am; ci--, Cd--, ai++, Ad++)
            sum += (*Ad) * (*Cd);

          X(oj) = sum;
        }

      for (octave_idx_type oj = 0; oj < outN; oj++)
        {
          T sum = 0;

          octave_idx_type rj = Rn - 1 - std::max (0, edgN-oj);
          octave_idx_type aj = std::max (0, oj-edgN);
          const T* Xd = X.data() + aj;
          const T* Rd = R.data() + rj;

          for ( ; rj >= 0 && aj < An; rj--, Rd--, aj++, Xd++)
            sum += (*Xd) * (*Rd);

          O(oi,oj) = sum;
        }
    }

  return O;
}

#if !defined (CXX_NEW_FRIEND_TEMPLATE_DECL)
extern MArray<double>
conv2 (MArray<double>&, MArray<double>&, Shape);

extern MArray<Complex>
conv2 (MArray<Complex>&, MArray<Complex>&, Shape);

extern MArray<float>
conv2 (MArray<float>&, MArray<float>&, Shape);

extern MArray<FloatComplex>
conv2 (MArray<FloatComplex>&, MArray<FloatComplex>&, Shape);
#endif

template <class T>
MArray<T>
conv2 (const MArray<T>& A, const MArray<T>& B, Shape ishape)
{
  // Convolution works fastest if we choose the A matrix to be
  // the largest.

  // Here we calculate the size of the output matrix,
  // in order to stay Matlab compatible, it is based
  // on the third parameter if it's separable, and the
  // first if it's not

  // NOTE in order to be Matlab compatible, we give argueably
  // wrong sizes for 'valid' if the smallest matrix is first

  octave_idx_type Am = A.rows ();
  octave_idx_type An = A.columns ();
  octave_idx_type Bm = B.rows ();
  octave_idx_type Bn = B.columns ();

  octave_idx_type outM = 0;
  octave_idx_type outN = 0;
  octave_idx_type edgM = 0;
  octave_idx_type edgN = 0;

  switch (ishape)
    {
      case SHAPE_FULL:
        outM = Am + Bm - 1;
        outN = An + Bn - 1;
        edgM = Bm - 1;
        edgN = Bn - 1;
        break;

      case SHAPE_SAME:
        outM = Am;
        outN = An;
        edgM = (Bm - 1) /2;
        edgN = (Bn - 1) /2;
        break;

      case SHAPE_VALID:
        outM = Am - Bm + 1;
        outN = An - Bn + 1;
        if (outM < 0)
          outM = 0;
        if (outN < 0)
          outN = 0;
        edgM = edgN = 0;
        break;
    }

  MArray<T> O (outM, outN);

  T *Od = O.fortran_vec ();

  for (octave_idx_type oj = 0; oj < outN; oj++)
    {
      octave_idx_type aj0 = std::max (0, oj-edgN);
      octave_idx_type bj0 = Bn - 1 - std::max (0, edgN-oj);

      for (octave_idx_type oi = 0; oi < outM; oi++)
        {
          T sum = 0;

          octave_idx_type bi0 = Bm - 1 - std::max (0, edgM-oi);
          octave_idx_type ai0 = std::max (0, oi-edgM);

          for (octave_idx_type aj = aj0, bj = bj0; bj >= 0 && aj < An;
               bj--, aj++)
            {
              const T* Ad = A.data () + ai0 + Am*aj;
              const T* Bd = B.data () + bi0 + Bm*bj;

              for (octave_idx_type ai = ai0, bi = bi0; bi >= 0 && ai < Am;
                   bi--, ai++)
                {
                  sum += (*Ad++) * (*Bd--);
                  // Comment: it seems to be 2.5 x faster than this:
                  //        sum+= A(ai,aj) * B(bi,bj);
                }
            }

          *Od++ = sum;
        }
    }

  return O;
}

/*
%!test
%! b = [0,1,2,3;1,8,12,12;4,20,24,21;7,22,25,18];
%! assert(conv2([0,1;1,2],[1,2,3;4,5,6;7,8,9]),b);

%!test
%! b = single([0,1,2,3;1,8,12,12;4,20,24,21;7,22,25,18]);
%! assert(conv2(single([0,1;1,2]),single([1,2,3;4,5,6;7,8,9])),b);
*/

DEFUN_DLD (conv2, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {y =} conv2 (@var{a}, @var{b}, @var{shape})\n\
@deftypefnx {Loadable Function} {y =} conv2 (@var{v1}, @var{v2}, @var{M}, @var{shape})\n\
\n\
Returns 2D convolution of @var{a} and @var{b} where the size\n\
of @var{c} is given by\n\
\n\
@table @asis\n\
@item @var{shape} = 'full'\n\
returns full 2-D convolution\n\
@item @var{shape} = 'same'\n\
same size as a. 'central' part of convolution\n\
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
  Shape ishape;

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
    ishape = SHAPE_FULL;
  else if (shape == "same")
    ishape = SHAPE_SAME;
  else if (shape == "valid")
    ishape = SHAPE_VALID;
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
               FloatComplexColumnVector v1 (args(0).float_complex_vector_value ());
               FloatComplexColumnVector v2 (args(1).float_complex_vector_value ());
               FloatComplexMatrix a (args(2).float_complex_matrix_value ());
               FloatComplexMatrix c (conv2 (v1, v2, a, ishape));
               if (! error_state)
                 retval = c;
             }
           else
             {
               FloatColumnVector v1 (args(0).float_vector_value ());
               FloatColumnVector v2 (args(1).float_vector_value ());
               FloatMatrix a (args(2).float_matrix_value ());
               FloatMatrix c (conv2 (v1, v2, a, ishape));
               if (! error_state)
                 retval = c;
             }
         }
       else
         {
           if (args(0).is_complex_type ()
               || args(1).is_complex_type ()
               || args(2).is_complex_type ())
             {
               ComplexColumnVector v1 (args(0).complex_vector_value ());
               ComplexColumnVector v2 (args(1).complex_vector_value ());
               ComplexMatrix a (args(2).complex_matrix_value ());
               ComplexMatrix c (conv2 (v1, v2, a, ishape));
               if (! error_state)
                 retval = c;
             }
           else
             {
               ColumnVector v1 (args(0).vector_value ());
               ColumnVector v2 (args(1).vector_value ());
               Matrix a (args(2).matrix_value ());
               Matrix c (conv2 (v1, v2, a, ishape));
               if (! error_state)
                 retval = c;
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
               FloatComplexMatrix b (args(1).float_complex_matrix_value ());
               FloatComplexMatrix c (conv2 (a, b, ishape));
               if (! error_state)
                 retval = c;
             }
           else
             {
               FloatMatrix a (args(0).float_matrix_value ());
               FloatMatrix b (args(1).float_matrix_value ());
               FloatMatrix c (conv2 (a, b, ishape));
               if (! error_state)
                 retval = c;
             }
         }
       else
         {
           if (args(0).is_complex_type ()
               || args(1).is_complex_type ())
             {
               ComplexMatrix a (args(0).complex_matrix_value ());
               ComplexMatrix b (args(1).complex_matrix_value ());
               ComplexMatrix c (conv2 (a, b, ishape));
               if (! error_state)
                 retval = c;
             }
           else
             {
               Matrix a (args(0).matrix_value ());
               Matrix b (args(1).matrix_value ());
               Matrix c (conv2 (a, b, ishape));
               if (! error_state)
                 retval = c;
             }
         }

     } // if (separable)

   return retval;
}

template MArray<double>
conv2 (const MArray<double>&, const MArray<double>&, const MArray<double>&,
       Shape);

template MArray<double>
conv2 (const MArray<double>&, const MArray<double>&, Shape);

template MArray<Complex>
conv2 (const MArray<Complex>&, const MArray<Complex>&,
       const MArray<Complex>&, Shape);

template MArray<Complex>
conv2 (const MArray<Complex>&, const MArray<Complex>&, Shape);
