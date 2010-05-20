/*

Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009 John W. Eaton
Copyirght (C) 2009, 2010 VZLU Prague

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
#include <limits>

#include <sstream>
#include <string>

#include "lo-error.h"
#include "lo-macros.h"

// Rationale: This implementation is more tricky than Array, but the
// big plus is that dim_vector requires only one allocation instead of
// two.  It is (slightly) patterned after GCC's basic_string
// implementation.  rep is a pointer to an array of memory, comprising
// count, length, and the data:
//
//          <count>
//          <ndims>
//  rep --> <dims[0]>
//          <dims[1]>
//          ...
//
// The inlines count(), ndims() recover this data from the rep.  Note
// that rep points to the beginning of dims to grant faster access
// (reinterpret_cast is assumed to be an inexpensive operation).

class
dim_vector
{
private:

  octave_idx_type *rep;

  octave_idx_type& ndims (void) const { return rep[-1]; }

  octave_idx_type& count (void) const { return rep[-2]; }

  // Construct a new rep with count = 1 and ndims given.

  static octave_idx_type *newrep (int ndims)
  {
    octave_idx_type *r = new octave_idx_type[ndims + 2];

    *r++ = 1;
    *r++ = ndims;

    return r;
  }

  // Clone this->rep.

  octave_idx_type *clonerep (void)
  {
    int l = ndims ();

    octave_idx_type *r = new octave_idx_type[l + 2];

    *r++ = 1;
    *r++ = l;

    for (int i = 0; i < l; i++)
      r[i] = rep[i];

    return r;
  }

  // Clone and resize this->rep to length n, filling by given value.

  octave_idx_type *resizerep (int n, octave_idx_type fill_value)
  {
    int l = ndims ();

    if (n < 2)
      n = 2;

    octave_idx_type *r = new octave_idx_type[n + 2];

    *r++ = 1;
    *r++ = n;

    if (l > n)
      l = n;

    int j;
    for (j = 0; j < l; j++)
      r[j] = rep[j];
    for (; j < n; j++)
      r[j] = fill_value;

    return r;
  }

  // Free the rep.

  void freerep (void)
  {
    assert (count () == 0);
    delete [] (rep - 2);
  }

  void make_unique (void)
  {
    if (count () > 1)
      {
        --count();
        rep = clonerep ();
      }
  }

public:

  // The constructor
  //
  //   dim_vector (n)
  //
  // creates an dimension vector with N rows and 1 column.  It is
  // deprecated because of the potentiol for confusion that it causes.
  // Additional constructors of the form
  //
  //   dim_vector (r, c)
  //   dim_vector (r, c, p)
  //   dim_vector (d1, d2, d3, d4, ...)
  //
  // are available for up to 7 dimensions.

  explicit dim_vector (octave_idx_type n) GCC_ATTR_DEPRECATED
    : rep (newrep (2))
  {
    rep[0] = n;
    rep[1] = 1;
  }

#define ASSIGN_REP(i) rep[i] = d ## i;
#define DIM_VECTOR_CTOR(N) \
  dim_vector (OCT_MAKE_DECL_LIST (octave_idx_type, d, N)) \
    : rep (newrep (N)) \
  { \
    OCT_ITERATE_MACRO (ASSIGN_REP, N) \
  }

  // Add more if needed.
  DIM_VECTOR_CTOR (2)
  DIM_VECTOR_CTOR (3)
  DIM_VECTOR_CTOR (4)
  DIM_VECTOR_CTOR (5)
  DIM_VECTOR_CTOR (6)
  DIM_VECTOR_CTOR (7)

#undef ASSIGN_REP
#undef DIM_VECTOR_CTOR

  octave_idx_type& elem (int i)
  {
#ifdef BOUNDS_CHECKING
    assert (i >= 0 && i < ndims ());
#endif
    make_unique ();
    return rep[i];
  }

  octave_idx_type elem (int i) const
  {
#ifdef BOUNDS_CHECKING
    assert (i >= 0 && i < ndims ());
#endif
    return rep[i];
  }

  void chop_trailing_singletons (void)
  {
    int l = ndims ();
    if (l > 2 && rep[l-1] == 1)
      {
        make_unique ();
        do
          l--;
        while (l > 2 && rep[l-1] == 1);
        ndims () = l;
      }
  }

  void chop_all_singletons (void)
  {
    make_unique ();

    int j = 0;
    int l = ndims();

    for (int i = 0; i < l; i++)
      {
        if (rep[i] != 1)
          rep[j++] = rep[i];
      }

    if (j == 1)
      rep[1] = 1;

    ndims () = j > 2 ? j : 2;
  }

private:
  
  static octave_idx_type *nil_rep (void)
    {
      static dim_vector zv (0, 0);
      return zv.rep;
    }

  explicit dim_vector (octave_idx_type *r)
    : rep (r) { }

public:

  explicit dim_vector (void) : rep (nil_rep ()) { count()++; }

  dim_vector (const dim_vector& dv) : rep (dv.rep) { count()++; }

  static dim_vector alloc (int n)
  {
    return dim_vector (newrep (n < 2 ? 2 : n));
  }

  dim_vector& operator = (const dim_vector& dv)
  {
    if (&dv != this)
      {
        if (--count() <= 0)
          freerep ();

        rep = dv.rep;
        count()++;
      }

    return *this;
  }

  ~dim_vector (void)
  {
    if (--count() <= 0)
      freerep ();
  }

  int length (void) const { return ndims (); }

  octave_idx_type& operator () (int i) { return elem (i); }

  octave_idx_type operator () (int i) const { return elem (i); }

  void resize (int n, int fill_value = 0)
  {
    int len = length ();

    if (n != len)
      {
        octave_idx_type *r = resizerep (n, fill_value);

        if (--count() <= 0)
          freerep ();

        rep = r;
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

  bool zero_by_zero (void) const
  {
    return length () == 2 && elem (0) == 0 && elem (1) == 0;
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

  int num_ones (void) const
  {
    int retval = 0;

    for (int i = 0; i < length (); i++)
      if (elem (i) == 1)
        retval++;

    return retval;
  }

  bool all_ones (void) const
  {
    return (num_ones () == length ());
  }

  // Return the number of elements that a matrix with this dimension
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

  // The following function will throw a std::bad_alloc ()
  // exception if the requested size is larger than can be indexed by
  // octave_idx_type. This may be smaller than the actual amount of
  // memory that can be safely allocated on a system.  However, if we
  // don't fail here, we can end up with a mysterious crash inside a
  // function that is iterating over an array using octave_idx_type
  // indices.

  octave_idx_type safe_numel (void) const
  {
    octave_idx_type idx_max = std::numeric_limits<octave_idx_type>::max () - 1;
    octave_idx_type n = 1;
    int n_dims = length ();

    for (int i = 0; i < n_dims; i++)
      {
        n *= rep[i];
        if (rep[i] != 0)
          idx_max /= rep[i];
        if (idx_max <= 0)
          throw std::bad_alloc ();
      }

    return n;
  }

  bool any_neg (void) const
  {
    int n_dims = length ();
    int i;

    for (i = 0; i < n_dims; i++)
      if (elem (i) < 0)
        break;

    return i < n_dims;
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
    if (dvb.zero_by_zero ())
      return true;

    if (zero_by_zero ())
      {
        *this = dvb;
        return true;
      }

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
      resize (n_max, 1);
  
    // Larger or equal since dim has been decremented by one.

    if (dim >= nb)
      elem (dim)++;
    else
      elem (dim) += dvb(dim);

    return true;
  }

  // Force certain dimensionality, preserving numel ().  Missing
  // dimensions are set to 1, redundant are folded into the trailing
  // one.  If n = 1, the result is 2d and the second dim is 1
  // (dim_vectors are always at least 2D).  If the original dimensions
  // were all zero, the padding value is zero.

  dim_vector redim (int n) const
    {
      int n_dims = length ();

      if (n_dims == n)
        return *this;
      else if (n_dims < n)
        {
          dim_vector retval = alloc (n);

          int pad = 0;
          for (int i = 0; i < n_dims; i++)
            {
              retval.rep[i] = rep[i];
              if (rep[i] != 0)
                pad = 1;
            }

          for (int i = n_dims; i < n; i++)
            retval.rep[i] = pad;

          return retval;
        }
      else
        {
          if (n < 1) n = 1;

          dim_vector retval = alloc (n);

          retval.rep[1] = 1;

          for (int i = 0; i < n-1; i++)
            retval.rep[i] = rep[i];

          int k = rep[n-1];
          for (int i = n; i < n_dims; i++)
            k *= rep[i];

          retval.rep[n-1] = k;

          return retval;
        }
    }

  dim_vector as_column (void) const
    {
      if (length () == 2 && elem (1) == 1)
        return *this;
      else
        return dim_vector (numel (), 1);
    }

  dim_vector as_row (void) const
    {
      if (length () == 2 && elem (0) == 1)
        return *this;
      else
        return dim_vector (1, numel ());
    }

  bool is_vector (void) const
    {
      return (length () == 2 && (elem (0) == 1 || elem (1) == 1));
    }

  int first_non_singleton (int def = 0) const
    {
      for (int i = 0; i < length (); i++)
        {
          if (elem (i) != 1)
            return i;
        }

      return def;      
    }

  // Compute a linear index from an index tuple.

  octave_idx_type compute_index (const octave_idx_type *idx) const
    {
      octave_idx_type k = 0;
      for (int i = length () - 1; i >= 0; i--)
        k = k * rep[i] + idx[i];

      return k;
    }

  // Ditto, but the tuple may be incomplete (nidx < length ()).

  octave_idx_type compute_index (const octave_idx_type *idx, int nidx) const
    {
      octave_idx_type k = 0;
      for (int i = nidx - 1; i >= 0; i--)
        k = k * rep[i] + idx[i];

      return k;
    }

  // Increment a multi-dimensional index tuple, optionally starting
  // from an offset position and return the index of the last index
  // position that was changed, or length () if just cycled over.

  int increment_index (octave_idx_type *idx, int start = 0) const
    {
      int i;
      for (i = start; i < length (); i++)
        {
          if (++(*idx) == rep[i])
            *idx++ = 0;
          else
            break;
        }
      return i;
    }

  // Return cumulative dimensions.

  dim_vector cumulative (void) const
    {
      int nd = length ();
      dim_vector retval = alloc (nd);

      octave_idx_type k = 1;
      for (int i = 0; i < nd; i++)
        retval.rep[i] = k *= rep[i];

      return retval;
    }

  // Compute a linear index from an index tuple.  Dimensions are
  // required to be cumulative.

  octave_idx_type cum_compute_index (const octave_idx_type *idx) const
    {
      octave_idx_type k = idx[0];

      for (int i = 1; i < length (); i++)
        k += rep[i-1] * idx[i];

      return k;
    }


  friend bool operator == (const dim_vector& a, const dim_vector& b);
};

inline bool
operator == (const dim_vector& a, const dim_vector& b)
{
  // Fast case.
  if (a.rep == b.rep)
    return true;

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

inline bool
operator != (const dim_vector& a, const dim_vector& b)
{
  return ! operator == (a, b);
}

#endif
