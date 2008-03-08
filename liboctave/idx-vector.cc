/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2002,
              2003, 2004, 2005, 2006, 2007 John W. Eaton

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

#include <iostream>

#include "Range.h"
#include "boolNDArray.h"
#include "dColVector.h"
#include "dNDArray.h"

#include "idx-vector.h"
#include "lo-error.h"
#include "lo-mappers.h"

#define IDX_VEC_REP idx_vector::idx_vector_rep

IDX_VEC_REP::idx_vector_rep (const IDX_VEC_REP& a)
  : data (0), len (a.len), num_zeros (a.num_zeros), num_ones (a.num_ones),
    range_base (a.range_base), range_step (a.range_step),
    max_val (a.max_val), min_val (a.min_val),
    frozen_at_z_len (a.frozen_at_z_len), frozen_len (a.frozen_len),
    colon (a.colon), range(a.range), initialized (a.initialized),
    frozen (a.frozen), colon_equiv_checked (a.colon_equiv_checked),
    colon_equiv (a.colon_equiv), orig_dims (a.orig_dims)
{
  if (len > 0)
    {
      if (! range)
	{
	  data = new octave_idx_type [len];

	  for (octave_idx_type i = 0; i < len; i++)
	    data[i] = a.data[i];
	}
    }
}

octave_idx_type
IDX_VEC_REP::tree_to_mat_idx (double x, bool& conversion_error)
{
  octave_idx_type retval = -1;

  conversion_error = false;

  if (D_NINT (x) != x)
    {
      (*current_liboctave_error_handler)
	("expecting integer index, found %f", x);

      conversion_error = true;
    }
  else
    retval = static_cast<octave_idx_type> (x - 1);

  return retval;
}

static inline bool
idx_is_inf_or_nan (double x)
{
  bool retval = false;

  if (xisnan (x))
    {
      (*current_liboctave_error_handler) ("NaN invalid as index");
      retval = true;
    }
  else if (xisinf (x))
    {
      (*current_liboctave_error_handler) ("Inf invalid as index");
      retval = true;
    }

  return retval;
}

IDX_VEC_REP::idx_vector_rep (const ColumnVector& v)
  : data (0), len (v.length ()), num_zeros (0), num_ones (0),
    range_base (0), range_step (0), max_val (0), min_val (0), count (1),
    frozen_at_z_len (0), frozen_len (0), colon (0), range(0),
    initialized (0), frozen (0), colon_equiv_checked (0),
    colon_equiv (0), orig_dims (len, 1)
{
  if (len == 0)
    {
      initialized = 1;
      return;
    }
  else
    {
      data = new octave_idx_type [len];

      bool conversion_error = false;

      for (octave_idx_type i = 0; i < len; i++)
	{
	  double d = v.elem (i);

	  if (idx_is_inf_or_nan (d))
	    return;
	  else
	    data[i] = tree_to_mat_idx (d, conversion_error);

	  if (conversion_error)
	    return;
	}
    }

  init_state ();
}

IDX_VEC_REP::idx_vector_rep (const NDArray& nda)
  : data (0), len (nda.length ()), num_zeros (0), num_ones (0),
    range_base (0), range_step (0), max_val (0), min_val (0), count (1),
    frozen_at_z_len (0), frozen_len (0), colon (0), range(0),
    initialized (0), frozen (0), colon_equiv_checked (0),
    colon_equiv (0), orig_dims (nda.dims ())
{
  if (len == 0)
    {
      initialized = 1;
      return;
    }
  else
    {
      octave_idx_type k = 0;
      data = new octave_idx_type [len];

      bool conversion_error = false;

      for (octave_idx_type i = 0; i < len; i++)
	{
	  double d = nda.elem (i);

	  if (idx_is_inf_or_nan (d))
	    return;
	  else
	    data[k++] = tree_to_mat_idx (d, conversion_error);

	  if (conversion_error)
	    return;
	}
    }

  init_state ();
}

IDX_VEC_REP::idx_vector_rep (const Range& r)
  : data (0), len (r.nelem ()), num_zeros (0), num_ones (0),
    range_base (0), range_step (0), max_val (0), min_val (0), 
    count (1), frozen_at_z_len (0), frozen_len (0), colon (0),
    range(1), initialized (0), frozen (0),
    colon_equiv_checked (0), colon_equiv (0), orig_dims (1, len)
{
  if (len < 0)
    {
      (*current_liboctave_error_handler) ("invalid range used as index");
      return;
    }
  else if (len == 0)
    {
      initialized = 1;
      return;
    }

  if (r.all_elements_are_ints ())
    {    
      range_base = static_cast<octave_idx_type> (r.base () - 1);
      range_step = static_cast<octave_idx_type> (r.inc ());

      init_state ();
    }
  else
    (*current_liboctave_error_handler)
      ("expecting integer index, found non integer Range");
}

IDX_VEC_REP::idx_vector_rep (double d)
  : data (0), len (1), num_zeros (0), num_ones (0),
    range_base (0), range_step (0), max_val (0), min_val (0), 
    count (1), frozen_at_z_len (0), frozen_len (0), colon (0),
    range(1), initialized (0), frozen (0), colon_equiv_checked (0),
    colon_equiv (0), orig_dims (1, 1)
{
  if (idx_is_inf_or_nan (d))
    return;
  else
    {
      bool conversion_error = false;

      range_base = tree_to_mat_idx (d, conversion_error);
      range_step = 1;

      if (conversion_error)
	return;
    }

  init_state ();
}

IDX_VEC_REP::idx_vector_rep (octave_idx_type i)
  : data (0), len (1), num_zeros (0), num_ones (0),
    range_base (tree_to_mat_idx (i)), range_step (1), 
    max_val (0), min_val (0), count (1), frozen_at_z_len (0),
    frozen_len (0), colon (0), range(1), initialized (0),
    frozen (0), colon_equiv_checked (0), colon_equiv (0), orig_dims (1, 1)
{
  init_state ();
}

IDX_VEC_REP::idx_vector_rep (char c)
  : data (0), len (0), num_zeros (0), num_ones (0), range_base (0),
    range_step (0), max_val (0), min_val (0), count (1),
    frozen_at_z_len (0), frozen_len (0), colon (1), range(0),
    initialized (0), frozen (0), colon_equiv_checked (0),
    colon_equiv (0), orig_dims (0, 0)
{
  assert (c == ':');

  init_state ();
}

IDX_VEC_REP::idx_vector_rep (bool b)
  : data (0), len (b ? 1 : 0), num_zeros (0), num_ones (0), range_base (0),
    range_step (0), max_val (0), min_val (0), count (1),
    frozen_at_z_len (0), frozen_len (0), colon (0), range(0),
    initialized (0), frozen (0), colon_equiv_checked (0),
    colon_equiv (0), orig_dims (len, len)
{
  if (len == 0)
    initialized = 1;
  else
    {
      data = new octave_idx_type [len];
      data[0] = 0;
      init_state ();
    }
}

IDX_VEC_REP::idx_vector_rep (const boolNDArray& bnda)
  : data (0), len (bnda.nnz ()), num_zeros (0), num_ones (0),
    range_base (0), range_step (0), max_val (0), min_val (0),
    count (1), frozen_at_z_len (0), frozen_len (0), colon (0),
    range(0), initialized (0), frozen (0),
    colon_equiv_checked (0), colon_equiv (0), orig_dims ()
{
  if (len == 0)
    {
      orig_dims = dim_vector (0, 0);
      initialized = 1;
    }
  else
    {
      data = new octave_idx_type [len];

      octave_idx_type ntot = bnda.length ();

      for (octave_idx_type i = 0, k = 0; i < ntot; i++, k < len)
	if (bnda.elem (i))
	  data[k++] = i;

      dim_vector dv = bnda.dims ();

      orig_dims = ((dv.length () == 2 && dv(0) == 1)
		   ? dim_vector (1, len) : orig_dims = dim_vector (len, 1));

      init_state ();
    }
}

IDX_VEC_REP&
IDX_VEC_REP::operator = (const IDX_VEC_REP& a)
{
  if (this != &a)
    {
      delete [] data;
      len = a.len;

      if (a.data)
	{
	  data = new octave_idx_type [len];

	  for (octave_idx_type i = 0; i < len; i++)
	    data[i] = a.data[i];
	}
      else
	data = 0;

      num_zeros = a.num_zeros;
      num_ones = a.num_ones;
      range_base = a.range_base;
      range_step = a.range_step;
      max_val = a.max_val;
      min_val = a.min_val;
      frozen_at_z_len = a.frozen_at_z_len;
      frozen_len = a.frozen_len;
      colon = a.colon;
      range = a.range;
      initialized = a.initialized;
      frozen = a.frozen;
      colon_equiv_checked = a.colon_equiv_checked;
      colon_equiv = a.colon_equiv;
      orig_dims = a.orig_dims;
    }

  return *this;
}

void
IDX_VEC_REP::init_state (void)
{
  num_zeros = 0;
  num_ones = 0;

  if (colon)
    {
      min_val = 0;
      max_val = 0;
    }
  else if (range)
    {
      if (range_step >= 0)
	{
	  min_val = range_base;
	  max_val = (len > 0) ? range_base + (len-1)*range_step : range_base;
	}
      else
	{
	  max_val = range_base;
	  min_val = (len > 0) ? range_base + (len-1)*range_step : range_base;
	}

      if ((range_base <= 0 && range_step > 0)
	  || (range_base >= 0 && range_step < 0))
	num_zeros = 1;

      if ((range_base <= 1 && range_step > 0)
	  || (range_base >= 1 && range_step < 0))
	num_zeros = 0;
    }
  else
    {
      min_val = max_val = data[0];

      octave_idx_type i = 0;
      do
	{
	  if (data[i] == -1)
	    num_zeros++;
	  else if (data[i] == 0)
	    num_ones++;

	  if (data[i] > max_val)
	    max_val = data[i];

	  if (data[i] < min_val)
	    min_val = data[i];
	}
      while (++i < len);
    }

  initialized = 1;
}

octave_idx_type
IDX_VEC_REP::checkelem (octave_idx_type n) const
{
  if (n < 0 || n >= len)
    {
      (*current_liboctave_error_handler) ("idx-vector: index out of range");
      return 0;
    }

  return elem (n);
}

static inline int
intcmp (const void *ii, const void *jj)
{
  return (*(static_cast<const octave_idx_type *> (ii)) - *(static_cast<const octave_idx_type *> (jj)));
}

static inline void
sort_data (octave_idx_type *d, octave_idx_type l)
{
  qsort (d, l, sizeof (octave_idx_type), intcmp);
}

static inline octave_idx_type
make_uniq (octave_idx_type *d, octave_idx_type l)
{
  if (l < 2)
    return l;

  octave_idx_type k = 0;
  for (octave_idx_type ii = 1; ii < l; ii++)
    {
      if (d[ii] != d[k])
	{
	  k++;
	  d[k] = d[ii];
	}
    }
  return k+1;
}

static inline octave_idx_type *
copy_data (const octave_idx_type *d, octave_idx_type l)
{
  octave_idx_type *new_data = new octave_idx_type [l];

  for (octave_idx_type ii = 0; ii < l; ii++)
    new_data[ii] = d[ii];

  return new_data;
}

int
IDX_VEC_REP::is_colon_equiv (octave_idx_type n, int sort_uniq)
{
  if (! colon_equiv_checked)
    {
      if (colon)
	{
	  colon_equiv = 1;
	}
      else if (range) 
	{
	  colon_equiv = (range_base == 0
			 && len == n
			 && (range_step == 1
			     || (range_step == -1 && sort_uniq)));
	}
      else if (static_cast<octave_idx_type> (len) > 1)
	{
	  if (sort_uniq)
	    {
	      octave_idx_type *tmp_data = copy_data (data, len);

	      sort_data (tmp_data, len);

	      octave_idx_type tmp_len = make_uniq (tmp_data, len);

	      colon_equiv = (tmp_len == n
			     && tmp_data[0] == 0
			     && tmp_data[tmp_len-1] == tmp_len - 1);

	      delete [] tmp_data;
	    }
	  else
	    {
	      if (len == n)
		{
		  colon_equiv = 1;

		  for (octave_idx_type ii = 0; ii < n; ii++)
		    if (data[ii] != ii)
		      {
			colon_equiv = 0;
			break;
		      }
		}
	    }
	}
      else
	colon_equiv = (len == n && (n == 0 || (n == 1 && data[0] == 0)));

      colon_equiv_checked = 1;
    }

  return colon_equiv;
}

void
IDX_VEC_REP::sort (bool uniq)
{
  if (range && len)
    {
      if (range_step < 0)
	{
	  range_base += (len-1)*(range_step);
	  range_step = -range_step;
	}
    }
  else if (len > 1)
    {
      sort_data (data, len);

      if (uniq)
	len = make_uniq (data, len);
    }
}

void
IDX_VEC_REP::shorten (octave_idx_type n)
{
  if (n > 0 && n <= len)
    len = n;
  else
    (*current_liboctave_error_handler)
      ("idx_vector::shorten: internal error!");
}

std::ostream&
IDX_VEC_REP::print (std::ostream& os) const
{
  if (colon)
    os << "colon" << std::endl;
  else if (range)
    os << "range_base: " << range_base
       << ", range_step: " << range_step << std::endl;
  else
    {
      for (octave_idx_type ii = 0; ii < len; ii++)
	os << data[ii] << "\n";
    }

  return os;
}

octave_idx_type
IDX_VEC_REP::freeze (octave_idx_type z_len, const char *tag, bool resize_ok)
{
  if (frozen)
    return frozen_len;

  frozen_len = -1;

  if (colon)
    frozen_len = z_len;
  else
    {
      if (len == 0)
	frozen_len = 0;
      else
	{
	  max_val = max ();
	  min_val = min ();

	  if (min_val < 0)
	    {
	      if (tag)
		(*current_liboctave_error_handler)
		  ("invalid %s index = %d", tag, min_val+1);
	      else
		(*current_liboctave_error_handler)
		  ("invalid index = %d", min_val+1);

	      initialized = 0;
	    }
	  else if (! resize_ok && max_val >= z_len)
	    {
	      if (tag)
		(*current_liboctave_error_handler)
		  ("invalid %s index = %d", tag, max_val+1);
	      else
		(*current_liboctave_error_handler)
		  ("invalid index = %d", max_val+1);

	      initialized = 0;
	    }
	  else
	    {
	      if (max_val >= z_len)
		{
		  if (tag)
		    (*current_liboctave_warning_with_id_handler)
		      ("Octave:resize-on-range-error",
		       "resizing object with %s index = %d out of bounds",
		       tag, max_val+1);
		  else
		    (*current_liboctave_warning_with_id_handler)
		      ("Octave:resize-on-range-error",
		       "resizing object with index = %d out of bounds",
		       max_val+1);
		}

	      frozen_len = length (z_len);
	    }
	}
    }

  frozen = 1;

  frozen_at_z_len = z_len ? z_len : len;

  return frozen_len;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
