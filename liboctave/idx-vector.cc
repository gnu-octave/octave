// idx-vector.cc                                       -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if defined (__GNUG__)
#pragma implementation
#endif

#include <iostream.h>
#include <stdlib.h>

#include "dMatrix.h"
#include "Range.h"
#include "idx-vector.h"
#include "user-prefs.h"
#include "error.h"
#include "utils.h"

idx_vector::idx_vector (const idx_vector& a)
{
  initialized = a.initialized;

  len = a.len;
  if (len > 0)
    {
      data = new int [len];
      for (int i = 0; i < len; i++)
	data[i] = a.data[i];

      num_zeros = a.num_zeros;
      num_ones = a.num_ones;
      one_zero = a.one_zero;

      max_val = a.max_val;
      min_val = a.min_val;
    }
  else
    data = (int *) 0;
}

static inline int
tree_to_mat_idx (double x)
{
  if (x > 0)
    return ((int) (x + 0.5) - 1);
  else
    return ((int) (x - 0.5) - 1);
}

idx_vector::idx_vector (const Matrix& m, int do_ftn_idx,
			const char *rc, int z_len)
{
  initialized = 0;

  int nr = m.rows ();
  int nc = m.columns ();

  if (nr == 0 || nc == 0)
    {
      len = 0;
      data = (int *) 0;
      num_zeros = 0;
      num_ones = 0;
      one_zero = 0;
      initialized = 1;
      return;
    }
  else if (nr > 1 && nc > 1 && do_ftn_idx)
    {
      const double *cop_out = m.data ();
      len = nr * nc;
      data = new int [len];
      for (int i = 0; i < len; i++)
	data[i] = tree_to_mat_idx (*cop_out++);
    }
  else if (nr == 1 && nc > 0)
    {
      len = nc;
      data = new int [len];
      for (int i = 0; i < len; i++)
	data[i] = tree_to_mat_idx (m.elem (0, i)); 
    }  
  else if (nc == 1 && nr > 0)
    {
      len = nr;
      data = new int [len];
      for (int i = 0; i < len; i++)
	data[i] = tree_to_mat_idx (m.elem (i, 0));
    }
  else
    {
      ::error ("invalid matrix used as index");
      return;
    }

  init_state (rc, z_len);
}

idx_vector::idx_vector (const Range& r)
{
  initialized = 0;

  len = r.nelem ();

  if (len < 0)
    {
      ::error ("invalid range used as index");
      return;
    }
  else if (len == 0)
    {
      data = (int *) 0;
      num_zeros = 0;
      num_ones = 0;
      one_zero = 0;
      initialized = 1;
      return;
    }

  double b = r.base ();
  double step = r.inc ();

  data = new int [len];

  for (int i = 0; i < len; i++)
    {
      double val = b + i * step;
      data[i] = tree_to_mat_idx (val);
    }

  init_state ();
}

idx_vector&
idx_vector::operator = (const idx_vector& a)
{
  if (this != &a)
    {
      initialized = a.initialized;

      delete [] data;
      len = a.len;
      data = new int [len];
      for (int i = 0; i < len; i++)
	data[i] = a.data[i];

      num_zeros = a.num_zeros;
      num_ones = a.num_ones;
      one_zero = a.one_zero;

      max_val = a.max_val;
      min_val = a.min_val;
    }
  return *this;
}

void
idx_vector::init_state (const char *rc, int z_len)
{
  one_zero = 1;
  num_zeros = 0;
  num_ones = 0;

  min_val = max_val = data[0];

  int i = 0;
  do
    {
      if (data[i] == -1)
	num_zeros++;
      else if (data[i] == 0)
	num_ones++;

      if (one_zero && data[i] != -1 && data[i] != 0)
	one_zero = 0;

      if (data[i] > max_val)
	max_val = data[i];

      if (data[i] < min_val)
	min_val = data[i];
    }
  while (++i < len);

  if (one_zero && z_len == len)
    {
      if (num_zeros == len)
	{
	  delete [] data;
	  len = 0;
	  data = (int *) 0;
	  num_zeros = 0;
	  num_ones = 0;
	  one_zero = 0;
	}
      else if (num_ones != len || user_pref.prefer_zero_one_indexing)
	convert_one_zero_to_idx ();
    }
  else if (min_val < 0)
    {
      ::error ("%s index %d out of range", rc, min_val+1);
      initialized = 0;
      return;
    }

  initialized = 1;
}

void
idx_vector::convert_one_zero_to_idx (void)
{
  if (num_ones == 0)
    {
      len = 0;
      max_val = 0;
      min_val = 0;
      delete [] data;
    }
  else
    {
      assert (num_ones + num_zeros == len);

      int *new_data = new int [num_ones];
      int count = 0;
      for (int i = 0; i < len; i++)
	if (data[i] == 0)
	  new_data[count++] = i;

      delete [] data;
      len = num_ones;
      data = new_data;

      min_val = max_val = data[0];

      i = 0;
      do
	{
	  if (data[i] > max_val)
	    max_val = data[i];

	  if (data[i] < min_val)
	    min_val = data[i];
	}
      while (++i < len);
    }
}

static inline int
intcmp (int *i, int *j)
{
  return (*i - *j);
}

int
idx_vector::checkelem (int n) const
{
  if (n < 0 || n >= len)
    {
      ::error ("idx-vector: index out of range");
      return 0;
    }

  return elem (n);
}

void
idx_vector::sort (void)
{
  qsort ((void *) data, len, sizeof (int),
	 (int (*)(const void*, const void*)) intcmp); 
}

void
idx_vector::sort_uniq (void)
{
  if (len > 0)
    {
      sort ();

      int *new_data = new int [len];
      new_data[0] = data[0];
      int k = 0;
      for (int i = 1; i < len; i++)
	{
	  if (data[i] != new_data[k])
	    {
	      k++;
	      new_data[k] = data[i];
	    }
	}
      delete [] data;
      len = k+1;
      data = new_data;
    }
}

void
idx_vector::shorten (int n)
{
  if (n > 0 && n <= len)
    len = n;
  else
    panic_impossible ();
}

ostream&
operator << (ostream& os, const idx_vector& a)
{
  for (int i = 0; i < a.len; i++)
    os << a.data[i] << "\n";
  return os;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
