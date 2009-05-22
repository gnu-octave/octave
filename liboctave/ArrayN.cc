// Template array classes
/*

Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2007, 2009 John W. Eaton

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

#include <cassert>

#include <iostream>

#include "Array-util.h"
#include "ArrayN.h"
#include "idx-vector.h"
#include "lo-error.h"

// N-dimensional array class.

template <class T>
std::ostream&
operator << (std::ostream& os, const ArrayN<T>& a)
{
  dim_vector a_dims = a.dims ();

  int n_dims = a_dims.length ();

  os << n_dims << "-dimensional array";

  if (n_dims)
    os << " (" << a_dims.str () << ")";

  os <<"\n\n";

  if (n_dims)
    {
      os << "data:";

      Array<octave_idx_type> ra_idx (n_dims, 0);

      // Number of times the first 2d-array is to be displayed.

      octave_idx_type m = 1;
      for (int i = 2; i < n_dims; i++)
	m *= a_dims(i);

      if (m == 1)
        {
          octave_idx_type rows = 0;
          octave_idx_type cols = 0;

          switch (n_dims)
            {
	    case 2:
	      rows = a_dims(0);
	      cols = a_dims(1);

	      for (octave_idx_type j = 0; j < rows; j++)
		{
		  ra_idx(0) = j;
		  for (octave_idx_type k = 0; k < cols; k++)
		    {
		      ra_idx(1) = k;
		      os << " " << a.elem(ra_idx);
		    }
		  os << "\n";
		}
	      break;

	    default:
	      rows = a_dims(0);

	      for (octave_idx_type k = 0; k < rows; k++)
		{
		  ra_idx(0) = k;
		  os << " " << a.elem(ra_idx);
		}
	      break;
	    }

          os << "\n";
        }
      else
        {
          octave_idx_type rows = a_dims(0);
          octave_idx_type cols = a_dims(1);

          for (int i = 0; i < m; i++)
            {
              os << "\n(:,:,";

              for (int j = 2; j < n_dims - 1; j++)
		os << ra_idx(j) + 1 << ",";

	      os << ra_idx(n_dims - 1) + 1 << ") = \n";

	      for (octave_idx_type j = 0; j < rows; j++)
	        {
	          ra_idx(0) = j;

	          for (octave_idx_type k = 0; k < cols; k++)
	            {
		      ra_idx(1) = k;
		      os << " " << a.elem(ra_idx);
		    }

	          os << "\n";
	        }

	      os << "\n";

	      if (i != m - 1)
		increment_index (ra_idx, a_dims, 2);
            }
        }
    }

  return os;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
