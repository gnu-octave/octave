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
#include <string>

#include "lo-sstream.h"

class
dim_vector
{
protected:

  class dim_vector_rep
  {
  public:

    int *dims;
    int ndims;
    int count;

    dim_vector_rep (void) : dims (0), ndims (0), count (1) { }

    dim_vector_rep (int n) : dims (new int [1]), ndims (1), count (1)
    {
      dims[0] = n;
    }

    dim_vector_rep (int r, int c) : dims (new int [2]), ndims (2), count (1)
    {
      dims[0] = r;
      dims[1] = c;
    }

    dim_vector_rep (int r, int c, int p)
      : dims (new int [3]), ndims (3), count (1)
    {
      dims[0] = r;
      dims[1] = c;
      dims[2] = p;
    }

    dim_vector_rep (const dim_vector_rep& dv)
      : dims (dv.ndims > 0 ? new int [dv.ndims] : 0),
	ndims (dv.ndims > 0 ? dv.ndims : 0), count (1)
    {
      if (dims)
	{
	  for (int i = 0; i < ndims; i++)
	    dims[i] = dv.dims[i];
	}
    }

    dim_vector_rep (int n, const dim_vector_rep *dv)
      : dims ((dv && n > 0) ? new int [n] : 0),
	ndims (n > 0 ? n : 0), count (1)
    {
      if (dims)
	{
	  int dv_ndims = dv ? dv->ndims : 0;

	  int min_len = n < dv_ndims ? n : dv_ndims;

	  for (int i = 0; i < min_len; i++)
	    dims[i] = dv->dims[i];

	  for (int i = dv_ndims; i < n; i++)
	    dims[i] = 0;
	}
    }

    ~dim_vector_rep (void) { delete [] dims; }

    int length (void) const { return ndims; }

    int& elem (int i)
    {
      assert (i >= 0 && i < ndims);
      return dims[i];
    }

    int elem (int i) const
    {
      assert (i >= 0 && i < ndims);
      return dims[i];
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

  explicit dim_vector (int n)
    : rep (new dim_vector_rep (n)) { }

  explicit dim_vector (int r, int c)
    : rep (new dim_vector_rep (r, c)) { }

  explicit dim_vector (int r, int c, int p)
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

  int& elem (int i) { make_unique (); return rep->elem (i); }

  int elem (int i) const { return rep->elem (i); }

  int& operator () (int i) { return elem (i); }

  int operator () (int i) const { return elem (i); }

  void resize (int n)
  {
    int len = length ();

    if (n != len)
      {
	dim_vector_rep *old_rep = rep;

	rep = new dim_vector_rep (n, old_rep);

	if (--old_rep->count <= 0)
	  delete old_rep;
      }
  }


  std::string str (char sep = 'x') const
  {
    OSSTREAM buf;

    for (int i = 0; i < length (); i++)
      {
	buf << elem (i);

	if (i < length () - 1)
	  buf << sep;
      }

    buf << OSSTREAM_ENDS;

    std::string retval = OSSTREAM_STR (buf);

    OSSTREAM_FREEZE (buf);

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

  int numel (void) const
  {
    int n_dims = length ();

    int retval = n_dims > 0 ? elem (0) : 0;

    for (int i = 1; i < n_dims; i++)
      retval *= elem (i);

    return retval;
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
