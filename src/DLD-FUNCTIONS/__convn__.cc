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

// FIXME -- this function should maybe be available in liboctave?
template <class MT, class ST> 
octave_value
convn (const MT& a, const MT& b)
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
    out_size(n) = std::max (a_size(n) - b_size(n) + 1, 0);

  MT out = MT (out_size);

  const octave_idx_type out_numel = out.numel ();
  
  // Iterate over every element of 'out'.
  dim_vector idx_dim (ndims);

  Array<octave_idx_type> a_idx (idx_dim);
  Array<octave_idx_type> b_idx (idx_dim);
  Array<octave_idx_type> out_idx (idx_dim, 0);

  for (octave_idx_type i = 0; i < out_numel; i++)
    {
      OCTAVE_QUIT;

      // For each neighbour
      ST sum = 0;

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
      if (args(0).is_real_type () && args(1).is_real_type ())
        {
          const NDArray a = args (0).array_value ();
          const NDArray b = args (1).array_value ();

	  if (! error_state)
	    retval = convn<NDArray, double> (a, b);
        }
      else if (args(0).is_complex_type () && args(1).is_complex_type ())
        {
          const ComplexNDArray a = args (0).complex_array_value ();
          const ComplexNDArray b = args (1).complex_array_value ();

	  if (! error_state)
	    retval = convn<ComplexNDArray, Complex> (a, b);
        }
      else
	error ("__convn__: first and second input should be real, or complex arrays");
    }
  else
    print_usage ();
    
  return retval;
}
