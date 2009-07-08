/*

Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009 John W. Eaton

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

#if !defined (octave_dim_vector_h)
#define octave_dim_vector_h 1

#include <cassert>

#include <sstream>
#include <string>

#include "lo-error.h"
#include "oct-types.h"

class
dim_vector
{
protected:

  class dim_vector_rep
  {
  public:

    octave_idx_type *dims;
    int ndims;
    int count;

    dim_vector_rep (void)
      : dims (new octave_idx_type [2]), ndims (2), count (1)
    {
      dims[0] = 0;
      dims[1] = 0;
    }


    dim_vector_rep (octave_idx_type n)
      : dims (new octave_idx_type [2]), ndims (2), count (1)
    {
      dims[0] = n;
      dims[1] = 1;
    }

    dim_vector_rep (octave_idx_type r, octave_idx_type c)
      : dims (new octave_idx_type [2]), ndims (2), count (1)
    {
      dims[0] = r;
      dims[1] = c;
    }

    dim_vector_rep (octave_idx_type r, octave_idx_type c, octave_idx_type p)
      : dims (new octave_idx_type [3]), ndims (3), count (1)
    {
      dims[0] = r;
      dims[1] = c;
      dims[2] = p;
    }

    dim_vector_rep (const dim_vector_rep& dv)
      : dims (new octave_idx_type [dv.ndims]),
	ndims (dv.ndims), count (1)
    {
      if (dims)
	{
	  for (int i = 0; i < ndims; i++)
	    dims[i] = dv.dims[i];
	}
    }

    dim_vector_rep (octave_idx_type n, const dim_vector_rep *dv,
		    int fill_value = 0)
      : dims (new octave_idx_type [n < 2 ? 2 : n]),
	ndims (n < 2 ? 2 : n), count (1)
    {
      if (n == 0)
	{
	  // Result is 0x0.
	  dims[0] = 0;
	  dims[1] = 0;
	}
      else if (n == 1)
	{
	  // Result is a column vector.
	  dims[0] = dv->dims[0];
	  dims[1] = 1;
	}
      else
	{
	  int dv_ndims = dv ? dv->ndims : 0;

	  int min_len = n < dv_ndims ? n : dv_ndims;

	  for (int i = 0; i < min_len; i++)
	    dims[i] = dv->dims[i];

	  for (int i = dv_ndims; i < n; i++)
	    dims[i] = fill_value;
	}
    }

    ~dim_vector_rep (void) { delete [] dims; }

    int length (void) const { return ndims; }

    octave_idx_type& elem (int i)
    {
      assert (i >= 0 && i < ndims);
      return dims[i];
    }

    octave_idx_type elem (int i) const
    {
      assert (i >= 0 && i < ndims);
      return dims[i];
    }

    void chop_trailing_singletons (void)
    {
      for (int i = ndims - 1; i > 1; i--)
	{
	  if (dims[i] == 1)
	    ndims--;
	  else
	    break;
	}
    }

    void chop_all_singletons (void)
    {
      int j = 0;

      for (int i = 0; i < ndims; i++)
	{
	  if (dims[i] != 1)
            dims[j++] = dims[i];
	}

      if (j == 1)
	dims[1] = 1;

      ndims = j > 2 ? j : 2;
    }

  private:

    // No assignment!

    dim_vector_rep& operator = (const dim_vector_rep& dv);
  };

  dim_vector_rep *rep;

  void make_unique (void)
  {
    if (rep->count > 1)
      {
	--rep->count;
	rep = new dim_vector_rep (*rep);
      }
  }

private:

  dim_vector_rep *nil_rep (void) const
  {
    static dim_vector_rep *nr = new dim_vector_rep ();

    return nr;
  }

public:

  explicit dim_vector (void)
    : rep (nil_rep ()) { rep->count++; }

  explicit dim_vector (octave_idx_type n)
    : rep (new dim_vector_rep (n)) { }

  explicit dim_vector (octave_idx_type r, octave_idx_type c)
    : rep (new dim_vector_rep (r, c)) { }

  explicit dim_vector (octave_idx_type r, octave_idx_type c, octave_idx_type p)
    : rep (new dim_vector_rep (r, c, p)) { }

  dim_vector (const dim_vector& dv)
    : rep (dv.rep) { rep->count++; }

  dim_vector& operator = (const dim_vector& dv)
  {
    if (&dv != this)
      {
	if (--rep->count <= 0)
	  delete rep;

	rep = dv.rep;
	rep->count++;
      }

    return *this;
  }

  ~dim_vector (void)
  {
    if (--rep->count <= 0)
      delete rep;
  }

  int length (void) const { return rep->length (); }

  octave_idx_type& elem (int i) { make_unique (); return rep->elem (i); }

  octave_idx_type elem (int i) const { return rep->elem (i); }

  octave_idx_type& operator () (int i) { return elem (i); }

  octave_idx_type operator () (int i) const { return elem (i); }

  void resize (int n, int fill_value = 0)
  {
    int len = length ();

    if (n != len)
      {
	dim_vector_rep *old_rep = rep;

	rep = new dim_vector_rep (n, old_rep, fill_value);

	if (--old_rep->count <= 0)
	  delete old_rep;
      }
  }

  std::string str (char sep = 'x') const
  {
    std::ostringstream buf;

    for (int i = 0; i < length (); i++)
      {
	buf << elem (i);

	if (i < length () - 1)
	  buf << sep;
      }

    std::string retval = buf.str ();

    return retval;
  }

  bool all_zero (void) const
  {
    bool retval = true;

    for (int i = 0; i < length (); i++)
      {
	if (elem (i) != 0)
	  {
	    retval = false;
	    break;
	  }
      }

    return retval;
  }

  bool any_zero (void) const
  {
    bool retval = false;

    for (int i = 0; i < length (); i++)
      {
	if (elem (i) == 0)
	  {
	    retval = true;
	    break;
	  }
      }

    return retval;
  }

  int
  num_ones (void) const
  {
    int retval = 0;

    for (int i = 0; i < length (); i++)
      if (elem (i) == 1)
	retval++;

    return retval;
  }

  bool
  all_ones (void) const
  {
    return (num_ones () == length ());
  }

  // This is the number of elements that a matrix with this dimension
  // vector would have, NOT the number of dimensions (elements in the
  // dimension vector).

  octave_idx_type numel (int n = 0) const
  {
    int n_dims = length ();

    octave_idx_type retval = 1;

    for (int i = n; i < n_dims; i++)
      retval *= elem (i);

    return retval;
  }

  bool any_neg (void) const
  {
    int n_dims = length (), i;
    for (i = 0; i < n_dims; i++)
      if (elem (i) < 0) break;
    return i < n_dims;
  }

  void chop_trailing_singletons (void)
  {
    make_unique ();
    rep->chop_trailing_singletons ();
  }

  void chop_all_singletons (void)
  {
    make_unique ();
    rep->chop_all_singletons ();
  }

  dim_vector squeeze (void) const
  {
    dim_vector new_dims = *this;

    bool dims_changed = 1;

    int k = 0;

    for (int i = 0; i < length (); i++)
      {
	if (elem (i) == 1)
	  dims_changed = true;
	else
	  new_dims(k++) = elem (i);
      }

    if (dims_changed)
      {
	if (k == 0)
	  new_dims = dim_vector (1, 1);
	else if (k == 1)
	  {
	    // There is one non-singleton dimension, so we need
	    // to decide the correct orientation.

	    if (elem (0) == 1)
	      {
		// The original dimension vector had a leading
		// singleton dimension.

		octave_idx_type tmp = new_dims(0);
	
		new_dims.resize (2);

 		new_dims(0) = 1;
		new_dims(1) = tmp;
	      }
	    else
	      {
		// The first element of the original dimension vector
		// was not a singleton dimension.

		new_dims.resize (2);

		new_dims(1) = 1;
	      }
	  }
	else
	  new_dims.resize(k);
      }
 
    return new_dims;
  }

  bool concat (const dim_vector& dvb, int dim = 0)
  {
    if (all_zero ())
      {
	*this = dvb;
	return true;
      }

    if (dvb.all_zero ())
      return true;

    int na = length ();
    int nb = dvb.length ();
  
    // Find the max and min value of na and nb
    int n_max = na > nb ? na : nb;
    int n_min = na < nb ? na : nb;
  
    // The elements of the dimension vectors can only differ
    // if the dim variable differs from the actual dimension
    // they differ.

    for (int i = 0; i < n_min; i++)
      {
	if (elem(i) != dvb(i) && dim != i)
	    return false;
      }
  
    // Ditto.
    for (int i = n_min; i < n_max; i++)
      {
	if (na > n_min)
	  {
	    if (elem(i) != 1 && dim != i)
	      return false;
	  }
	else 
	  {
	    if (dvb(i) != 1 && dim != i)
	      return false;
	  }
      }
    
    // If we want to add the dimension vectors at a dimension
    // larger than both, then we need to set n_max to this number
    // so that we resize *this to the right dimension.
    
    n_max = n_max > (dim + 1) ? n_max : (dim + 1);
    
    // Resize *this to the appropriate dimensions.
    
    if (n_max > na)
      {
	dim_vector_rep *old_rep = rep;

	rep = new dim_vector_rep (n_max, old_rep, 1);

	if (--old_rep->count <= 0)
	  delete old_rep;
      }
  
    // Larger or equal since dim has been decremented by one.

    if (dim >= nb)
      elem (dim)++;
    else
      elem (dim) += dvb(dim);

    return true;
  }

  // Forces certain dimensionality, preserving numel (). Missing dimensions are
  // set to 1, redundant are folded into the trailing one. If n = 1, the result
  // is 2d and the second dim is 1 (dim_vectors are always at least 2D).
  // If the original dimensions were all zero, the padding value is zero.
  dim_vector redim (int n) const
    {
      int n_dims = length ();
      if (n_dims == n)
        return *this;
      else
        {
          dim_vector retval;
          retval.resize (n == 1 ? 2 : n, 1);
          
          bool zeros = true;
          for (int i = 0; i < n && i < n_dims; i++)
            {
              retval(i) = elem (i);
              zeros = zeros && elem (i) == 0;
            }

          if (n < n_dims)
            {
              octave_idx_type k = 1;
              for (int i = n; i < n_dims; i++)
                k *= elem (i);
              retval(n - 1) *= k;
            }
          else if (zeros)
            {
              for (int i = n_dims; i < n; i++)
                retval.elem (i) = 0;
            }

          return retval;
        }
    }

  bool is_vector (void) const
    {
      return (length () == 2 && (elem (0) == 1 || elem (1) == 1));
    }

};

static inline bool
operator == (const dim_vector& a, const dim_vector& b)
{
  bool retval = true;

  int a_len = a.length ();
  int b_len = b.length ();

  if (a_len != b_len)
    retval = false;
  else
    {
      for (int i = 0; i < a_len; i++)
	{
	  if (a(i) != b(i))
	    {
	      retval = false;
	      break;
	    }
	}
    }

  return retval;
}

static inline bool
operator != (const dim_vector& a, const dim_vector& b)
{
  return ! operator == (a, b);
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
