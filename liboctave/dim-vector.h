/*

Copyright (C) 2003 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_dim_vector_h)
#define octave_dim_vector_h 1

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma interface
#endif

#include <cassert>

class
dim_vector
{
public:

  dim_vector (void) : ndims (0), dims (0) { }

  dim_vector (int n) : ndims (1), dims (new int [1]) { dims[0] = n; }

  dim_vector (int r, int c)
    : ndims (2), dims (new int [2]) { dims[0] = r; dims[1] = c; }

  dim_vector (int r, int c, int p)
    : ndims (3), dims (new int [3]) { dims[0] = r; dims[1] = c; dims[2] = p; }

  dim_vector (const dim_vector& dv)
    : ndims (dv.ndims)
    {
      if (dv.dims)
	{
	  dims = new int [ndims];

	  for (int i = 0; i < ndims; i++)
	    dims[i] = dv.dims[i];
	}
      else
	dims = 0;
    }

  dim_vector& operator = (const dim_vector& dv)
    {
      if (&dv != this)
	{
	  ndims = dv.ndims;

	  if (dv.dims)
	    {
	      dims = new int [ndims];

	      for (int i = 0; i < ndims; i++)
		dims[i] = dv.dims[i];
	    }
	}

      return *this;
    }

  ~dim_vector (void) { delete [] dims; }

  int length (void) const { return ndims; }

  int& elem (int i)
    {
      if (i >= ndims)
	resize (i+1);

      return dims[i];
    }

  int elem (int i) const { return i < ndims ? dims[i] : -1; }

  int& operator () (int i) { return elem (i); }

  int operator () (int i) const { return elem (i); }

  void resize (int n)
    {
      if (n > ndims)
	{
	  int *new_dims = new int [n];

	  for (int i = 0; i < ndims; i++)
	    new_dims[i] = dims[i];

	  for (int i = ndims; i < n; i++)
	    new_dims[i] = 0;

	  delete [] dims;

	  dims = new_dims;

	  ndims = n;
	}
      else
	ndims = n;
    }

private:

  int ndims;
  int *dims;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

