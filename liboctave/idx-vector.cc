// idx-vector.cc                                       -*- C++ -*-
/*

Copyright (C) 1996 John W. Eaton

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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>

#include <iostream.h>

#include "Range.h"
#include "dColVector.h"
#include "dMatrix.h"

#include "idx-vector.h"
#include "lo-error.h"

#define IDX_VEC_REP idx_vector::idx_vector_rep

IDX_VEC_REP::idx_vector_rep (const IDX_VEC_REP& a)
{
  data = 0;
  initialized = a.initialized;
  frozen = a.frozen;
  colon_equiv_checked = a.colon_equiv_checked;
  colon_equiv = a.colon_equiv;

  colon = a.colon;

  orig_nr = a.orig_nr;
  orig_nc = a.orig_nc;

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
}

static inline int
tree_to_mat_idx (double x)
{
  if (x > 0)
    return ((int) (x + 0.5) - 1);
  else
    return ((int) (x - 0.5) - 1);
}

IDX_VEC_REP::idx_vector_rep (const ColumnVector& v)
{
  data = 0;
  initialized = 0;
  frozen = 0;
  colon_equiv_checked = 0;
  colon_equiv = 0;
  colon = 0;

  len = v.length ();

  orig_nr = len;
  orig_nc = 1;

  if (len == 0)
    {
      num_zeros = 0;
      num_ones = 0;
      one_zero = 0;
      max_val = 0;
      min_val = 0;
      initialized = 1;
      return;
    }
  else
    {
      data = new int [len];
      for (int i = 0; i < len; i++)
	data[i] = tree_to_mat_idx (v.elem (i));
    }

  init_state ();
}

IDX_VEC_REP::idx_vector_rep (const Matrix& m)
{
  data = 0;
  initialized = 0;
  frozen = 0;
  colon_equiv_checked = 0;
  colon_equiv = 0;
  colon = 0;

  orig_nr = m.rows ();
  orig_nc = m.columns ();

  len = orig_nr * orig_nc;

  if (len == 0)
    {
      num_zeros = 0;
      num_ones = 0;
      one_zero = 0;
      max_val = 0;
      min_val = 0;
      initialized = 1;
      return;
    }
  else
    {
      int k = 0;
      data = new int [len];
      for (int j = 0; j < orig_nc; j++)
	for (int i = 0; i < orig_nr; i++)
	  data[k++] = tree_to_mat_idx (m.elem (i, j));
    }

  init_state ();
}

IDX_VEC_REP::idx_vector_rep (const Range& r)
{
  data = 0;
  initialized = 0;
  frozen = 0;
  colon_equiv_checked = 0;
  colon_equiv = 0;
  colon = 0;

  len = r.nelem ();

  orig_nr = 1;
  orig_nc = len;

  if (len < 0)
    {
      (*current_liboctave_error_handler) ("invalid range used as index");
      return;
    }
  else if (len == 0)
    {
      num_zeros = 0;
      num_ones = 0;
      one_zero = 0;
      max_val = 0;
      min_val = 0;
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

IDX_VEC_REP::idx_vector_rep (char c)
{
  assert (c == ':');

  colon = 1;
  len = 0;
  num_zeros = 0;
  num_ones = 0;
  one_zero = 0;
  initialized = 0;
  frozen = 0;
  colon_equiv_checked = 0;
  colon_equiv = 0;
  data = 0;

  init_state ();
}

IDX_VEC_REP&
IDX_VEC_REP::operator = (const IDX_VEC_REP& a)
{
  if (this != &a)
    {
      initialized = a.initialized;
      frozen = a.frozen;
      colon_equiv_checked = a.colon_equiv_checked;
      colon_equiv = a.colon_equiv;

      colon = a.colon;

      orig_nr = a.orig_nr;
      orig_nc = a.orig_nc;

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
IDX_VEC_REP::init_state (void)
{
  num_zeros = 0;
  num_ones = 0;

  if (colon)
    {
      one_zero = 0;
      min_val = max_val = 0;
    }
  else
    {
      one_zero = 1;

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
    }

  initialized = 1;
}

void
IDX_VEC_REP::maybe_convert_one_zero_to_idx (int z_len, int prefer_zero_one)
{
  if (one_zero && z_len == len
      && (num_ones != len || prefer_zero_one))
    {
      if (num_ones == 0)
	{
	  len = 0;
	  max_val = 0;
	  min_val = 0;
	  delete [] data;
	  data = 0;
	}
      else
	{
	  assert (num_ones + num_zeros == len);

	  int *new_data = new int [num_ones];
	  int k = 0;
	  for (int i = 0; i < len; i++)
	    if (data[i] == 0)
	      new_data[k++] = i;

	  delete [] data;
	  len = num_ones;
	  data = new_data;

	  min_val = max_val = data[0];

	  int i = 0;
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
}

int
IDX_VEC_REP::checkelem (int n) const
{
  if (n < 0 || n >= len)
    {
      (*current_liboctave_error_handler) ("idx-vector: index out of range");
      return 0;
    }

  return elem (n);
}

static inline int
intcmp (int *ii, int *jj)
{
  return (*ii - *jj);
}

static inline void
sort_data (int *d, int l)
{
  qsort ((void *) d, l, sizeof (int),
	 (int (*)(const void*, const void*)) intcmp);
}

static inline int
make_uniq (int *d, int l)
{
  int k = 0;
  for (int ii = 1; ii < l; ii++)
    {
      if (d[ii] != d[k])
	{
	  k++;
	  d[k] = d[ii];
	}
    }
  return k+1;
}

static inline int *
copy_data (const int *d, int l)
{
  int *new_data = new int [l];

  for (int ii = 0; ii < l; ii++)
    new_data[ii] = d[ii];

  return new_data;
}

int
IDX_VEC_REP::is_colon_equiv (int n, int sort)
{
  if (! colon_equiv_checked)
    {
      if (colon)
	{
	  colon_equiv = 1;
	}
      else if (len > 0 && len > 1 && ! one_zero)
	{
	  int *tmp_data = copy_data (data, len);

	  if (sort)
	    sort_data (tmp_data, len);

	  int tmp_len = make_uniq (tmp_data, len);

	  colon_equiv = ((tmp_len == 0 && n == 0)
			 || (tmp_len == n
			     && tmp_data[0] == 0
			     && tmp_data[tmp_len-1] == tmp_len - 1));

	  delete [] tmp_data;
	}
      else
	colon_equiv = 0;

      colon_equiv_checked = 1;
    }

  return colon_equiv;
}

void
IDX_VEC_REP::shorten (int n)
{
  if (n > 0 && n <= len)
    len = n;
  else
    (*current_liboctave_error_handler)
      ("idx_vector::shorten: internal error!");
}

ostream&
IDX_VEC_REP::print (ostream& os) const
{
  for (int ii = 0; ii < len; ii++)
    os << data[ii] << "\n";
  return os;
}

int
IDX_VEC_REP::freeze (int z_len, const char *tag,
		     int prefer_zero_one, int resize_ok)
{
  if (frozen)
    {
      assert (frozen_at_z_len == z_len);
      return frozen_len;
    }

  frozen_len = -1;

  if (colon)
    frozen_len = z_len;
  else
    {
      if (len == 0)
	frozen_len = 0;
      else
	{
	  maybe_convert_one_zero_to_idx (z_len, prefer_zero_one);

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
	    frozen_len = length (z_len);
	}
    }

  frozen = 1;
  frozen_at_z_len = z_len;

  return frozen_len;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
