/*

Copyright (C) 2008 Soren Hauberg

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

#include <algorithm>

#include "dNDArray.h"
#include "CNDArray.h"

#include "defun-dld.h"

template <class T1, class T2>
class
octave_convn_traits
{
public:
  // The return type for a T1 by T2 convn operation.
  typedef T1 TR;
};

#define OCTAVE_CONVN_TRAIT(T1, T2, T3) \
  template<> \
  class octave_convn_traits <T1, T2> \
  { \
  public: \
    typedef T3 TR; \
  }

OCTAVE_CONVN_TRAIT (NDArray, NDArray, NDArray);
OCTAVE_CONVN_TRAIT (ComplexNDArray, NDArray, ComplexNDArray);
OCTAVE_CONVN_TRAIT (NDArray, ComplexNDArray, ComplexNDArray);
OCTAVE_CONVN_TRAIT (ComplexNDArray, ComplexNDArray, ComplexNDArray);

OCTAVE_CONVN_TRAIT (FloatNDArray, FloatNDArray, FloatNDArray);
OCTAVE_CONVN_TRAIT (FloatComplexNDArray, FloatNDArray, FloatComplexNDArray);
OCTAVE_CONVN_TRAIT (FloatNDArray, FloatComplexNDArray, FloatComplexNDArray);
OCTAVE_CONVN_TRAIT (FloatComplexNDArray, FloatComplexNDArray, FloatComplexNDArray);

// FIXME -- this function should maybe be available in liboctave?
template <class MTa, class MTb> 
octave_value
convn (const MTa& a, const MTb& b)
{
  octave_value retval;

  // Get sizes
  const octave_idx_type ndims = a.ndims ();
  const octave_idx_type b_numel = b.numel ();

  const dim_vector a_size = a.dims ();
  const dim_vector b_size = b.dims ();
  
  if (ndims != b.ndims ())
    {
      error ("__convn__: first and second argument must have same dimensionality");
      return retval;
    }

  // Allocate output
  dim_vector out_size (a_size);
  for (octave_idx_type n = 0; n < ndims; n++)
    out_size(n) = std::max (a_size(n) - b_size(n) + 1,
                            static_cast<octave_idx_type> (0));

  typedef typename octave_convn_traits<MTa, MTb>::TR MTout;

  MTout out (out_size);

  const octave_idx_type out_numel = out.numel ();
  
  // Iterate over every element of 'out'.
  dim_vector idx_dim (ndims, 1);

  Array<octave_idx_type> a_idx (idx_dim);
  Array<octave_idx_type> b_idx (idx_dim);
  Array<octave_idx_type> out_idx (idx_dim, 0);

  for (octave_idx_type i = 0; i < out_numel; i++)
    {
      OCTAVE_QUIT;

      // For each neighbour
      typename MTout::element_type sum = 0;

      for (octave_idx_type n = 0; n < ndims; n++)
        b_idx(n) = 0;

      for (octave_idx_type j = 0; j < b_numel; j++)
        {
          for (octave_idx_type n = 0; n < ndims; n++)
            a_idx(n) = out_idx(n) + (b_size(n) - 1 - b_idx(n));

          sum += a(a_idx) * b(b_idx);

          b.increment_index (b_idx, b_size);
      }
            
      // Compute filter result
      out(out_idx) = sum;

      // Prepare for next iteration
      out.increment_index (out_idx, out_size);
    }
    
  return out;
}

DEFUN_DLD (__convn__, args, , 
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __convn__(@var{a}, @var{b})\n\
Undocumented internal function.\n\
@end deftypefn\n\
")
{
  octave_value retval;

  if (args.length () == 2)
    {
      if (args(0).is_single_type() || args(1).is_single_type())
        {
          if (args(0).is_real_type ())
            {
              if (args(1).is_real_type ())
                {
                  const FloatNDArray a = args (0).float_array_value ();
                  const FloatNDArray b = args (1).float_array_value ();

                  if (! error_state)
                    retval = convn (a, b);
                }
              else if (args(1).is_complex_type ())
                {
                  const FloatNDArray a = args (0).float_array_value ();
                  const FloatComplexNDArray b = args (1).float_complex_array_value ();

                  if (! error_state)
                    retval = convn (a, b);
                }
              else
                error ("__convn__: invalid call");
            }
          else if (args(0).is_complex_type ())
            {
              if (args(1).is_complex_type ())
                {
                  const FloatComplexNDArray a = args (0).float_complex_array_value ();
                  const FloatComplexNDArray b = args (1).float_complex_array_value ();

                  if (! error_state)
                    retval = convn (a, b);
                }
              else if (args(1).is_real_type ())
                {
                  const FloatComplexNDArray a = args (0).float_complex_array_value ();
                  const FloatNDArray b = args (1).float_array_value ();

                  if (! error_state)
                    retval = convn (a, b);
                }
              else
                error ("__convn__: invalid call");
            }
          else
            error ("__convn__: invalid call");
        }
      else
        {
          if (args(0).is_real_type ())
            {
              if (args(1).is_real_type ())
                {
                  const NDArray a = args (0).array_value ();
                  const NDArray b = args (1).array_value ();

                  if (! error_state)
                    retval = convn (a, b);
                }
              else if (args(1).is_complex_type ())
                {
                  const NDArray a = args (0).array_value ();
                  const ComplexNDArray b = args (1).complex_array_value ();

                  if (! error_state)
                    retval = convn (a, b);
                }
              else
                error ("__convn__: invalid call");
            }
          else if (args(0).is_complex_type ())
            {
              if (args(1).is_complex_type ())
                {
                  const ComplexNDArray a = args (0).complex_array_value ();
                  const ComplexNDArray b = args (1).complex_array_value ();

                  if (! error_state)
                    retval = convn (a, b);
                }
              else if (args(1).is_real_type ())
                {
                  const ComplexNDArray a = args (0).complex_array_value ();
                  const NDArray b = args (1).array_value ();

                  if (! error_state)
                    retval = convn (a, b);
                }
              else
                error ("__convn__: invalid call");
            }
          else
            error ("__convn__: invalid call");
        }
    }
  else
    print_usage ();
    
  return retval;
}
