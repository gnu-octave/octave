/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 2000, 2002, 2003,
              2004, 2005, 2006, 2007 John W. Eaton

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

#if !defined (octave_idx_vector_h)
#define octave_idx_vector_h 1

#include <iostream>

#include "dim-vector.h"
#include "oct-inttypes.h"
#include "intNDArray.h"

class ColumnVector;
class boolNDArray;
class NDArray;
class Range;

class
OCTAVE_API
idx_vector
{
private:

  class
  OCTAVE_API
  idx_vector_rep
  {
  public:

    idx_vector_rep (void)
      : data (0), len (0), num_zeros (0), num_ones (0),
        range_base (0), range_step (0), max_val (0),
	min_val (0), count (1), frozen_at_z_len (0), frozen_len (0),
	colon (0), range(0), one_zero (0), initialized (0), frozen (0),
	colon_equiv_checked (0), colon_equiv (0), orig_dims () { }

    idx_vector_rep (const ColumnVector& v);

    idx_vector_rep (const NDArray& nda);

    template <class U>
    idx_vector_rep (const intNDArray<U>& inda)
      : data (0), len (inda.length ()), num_zeros (0), num_ones (0),
	range_base (0), range_step (0), max_val (0), min_val (0),
        count (1), frozen_at_z_len (0), frozen_len (0), colon (0),
        range(0), one_zero (0), initialized (0), frozen (0),
        colon_equiv_checked (0), colon_equiv (0), orig_dims (inda.dims ())
    {
      if (len == 0)
	{
	  initialized = 1;
	  return;
	}
      else
	{
	  data = new octave_idx_type [len];

	  for (octave_idx_type i = 0; i < len; i++)
	    data[i] = tree_to_mat_idx (inda.elem (i));
	}

      init_state ();
    }

    idx_vector_rep (const Range& r);

    idx_vector_rep (double d);

    idx_vector_rep (octave_idx_type i);

    idx_vector_rep (char c);

    idx_vector_rep (bool b);

    template <class U>
    idx_vector_rep (const octave_int<U>& i)
      : data (0), len (1), num_zeros (0), num_ones (0),
	range_base (0), range_step (0), max_val (0), min_val (0),
        count (1), frozen_at_z_len (0), frozen_len (0), colon (0),
        range(0), one_zero (0), initialized (0), frozen (0),
        colon_equiv_checked (0), colon_equiv (0), orig_dims (1, 1)
    {
      data = new octave_idx_type [len];

      data[0] = tree_to_mat_idx (i);

      init_state ();
    }

    idx_vector_rep (const boolNDArray& bnda);

    idx_vector_rep (const idx_vector_rep& a);

    ~idx_vector_rep (void) { delete [] data; }

    idx_vector_rep& operator = (const idx_vector_rep& a);

    int ok (void) { return initialized; }

    octave_idx_type capacity (void) const { return len; }
    octave_idx_type length (octave_idx_type colon_len) const { return colon ? colon_len : len; }

    octave_idx_type elem (octave_idx_type n) const
    {
      return colon ? n : (range ? range_base + range_step*n : data[n]);
    }

    octave_idx_type checkelem (octave_idx_type n) const;
    octave_idx_type operator () (octave_idx_type n) const { return checkelem (n); }

    octave_idx_type max (void) const { return max_val; }
    octave_idx_type min (void) const { return min_val; }

    int one_zero_only (void) const { return one_zero; }
    octave_idx_type zeros_count (void) const { return num_zeros; }
    octave_idx_type ones_count (void) const { return num_ones; }

    int is_colon (void) const { return colon; }
    int is_colon_equiv (octave_idx_type n, int sort_uniq);

    void sort (bool uniq);

    octave_idx_type orig_rows (void) const { return orig_dims(0); }
    octave_idx_type orig_columns (void) const { return orig_dims(1); }

    dim_vector orig_dimensions (void) const { return orig_dims; }

    // other stuff

    void shorten (octave_idx_type n); // Unsafe.  Avoid at all cost.

    octave_idx_type freeze (octave_idx_type z_len, const char *tag, bool resize_ok);

    // i/o

    std::ostream& print (std::ostream& os) const;

    octave_idx_type *data;
    octave_idx_type len;
    octave_idx_type num_zeros;
    octave_idx_type num_ones;
    octave_idx_type range_base;
    octave_idx_type range_step;
    octave_idx_type max_val;
    octave_idx_type min_val;

    int count;

    octave_idx_type frozen_at_z_len;
    octave_idx_type frozen_len;

    unsigned int colon : 1;
    unsigned int range : 1;
    unsigned int one_zero : 1;
    unsigned int initialized : 1;
    unsigned int frozen : 1;
    unsigned int colon_equiv_checked : 1;
    unsigned int colon_equiv : 1;

    dim_vector orig_dims;
 
    void init_state (void);

    void maybe_convert_one_zero_to_idx (octave_idx_type z_len);

    octave_idx_type tree_to_mat_idx (double x, bool& conversion_error);

    octave_idx_type tree_to_mat_idx (octave_idx_type i) { return i - 1; }

    template <class U> octave_idx_type tree_to_mat_idx (const octave_int<U>& i)
      { return i.value () - 1; }
  };

public:

  idx_vector (void) : rep (new idx_vector_rep ()) { }

  idx_vector (const ColumnVector& v) : rep (new idx_vector_rep (v)) { }

  idx_vector (const NDArray& nda) : rep (new idx_vector_rep (nda)) { }

  template <class U>
  idx_vector (const intNDArray<U>& inda) : rep (new idx_vector_rep (inda)) { }

  idx_vector (const Range& r) : rep (new idx_vector_rep (r)) { }

  idx_vector (double d) : rep (new idx_vector_rep (d)) { }

  idx_vector (octave_idx_type i) : rep (new idx_vector_rep (i)) { }

  idx_vector (char c) : rep (new idx_vector_rep (c)) { }

  idx_vector (bool b) : rep (new idx_vector_rep (b)) { }

  template <class U>
  idx_vector (const octave_int<U>& i) : rep (new idx_vector_rep (i)) { }

  idx_vector (const boolNDArray& bnda) : rep (new idx_vector_rep (bnda)) { }

  idx_vector (const idx_vector& a) : rep (a.rep) { rep->count++; }

  ~idx_vector (void)
    {
      if (--rep->count <= 0)
	delete rep;
    }

  idx_vector& operator = (const idx_vector& a)
    {
      if (this != &a)
	{
	  if (--rep->count <= 0)
	    delete rep;

	  rep = a.rep;
	  rep->count++;
	}
      return *this;
    }

  operator bool () const { return rep->ok (); }

  octave_idx_type capacity (void) const { return rep->capacity (); }
  octave_idx_type length (octave_idx_type cl) const { return rep->length (cl); }

  octave_idx_type elem (octave_idx_type n) const { return rep->elem (n); }
  octave_idx_type checkelem (octave_idx_type n) const { return rep->checkelem (n); }
  octave_idx_type operator () (octave_idx_type n) const { return rep->operator () (n); }

  octave_idx_type max (void) const { return rep->max (); }
  octave_idx_type min (void) const { return rep->min (); }

  int one_zero_only (void) const { return rep->one_zero_only (); }
  octave_idx_type zeros_count (void) const { return rep->zeros_count (); }
  octave_idx_type ones_count (void) const { return rep->ones_count (); }

  int is_colon (void) const { return rep->is_colon (); }
  int is_colon_equiv (octave_idx_type n, int sort_uniq = 0) const
    { return rep->is_colon_equiv (n, sort_uniq); }

  void sort (bool uniq = false) { rep->sort (uniq); }

  octave_idx_type orig_rows (void) const { return rep->orig_rows (); }
  octave_idx_type orig_columns (void) const { return rep->orig_columns (); }

  dim_vector orig_dimensions (void) const { return rep->orig_dimensions (); }

  int orig_empty (void) const
    { return (! is_colon () && orig_dimensions().any_zero ()); }

  // Unsafe.  Avoid at all cost.
  void shorten (octave_idx_type n) { rep->shorten (n); }

  // i/o

  octave_idx_type freeze (octave_idx_type z_len, const char *tag, bool resize_ok = false)
    { return rep->freeze (z_len, tag, resize_ok); }

  std::ostream& print (std::ostream& os) const { return rep->print (os); }

  friend std::ostream& operator << (std::ostream& os, const idx_vector& a)
    { return a.print (os); }

  void maybe_convert_one_zero_to_idx (octave_idx_type z_len)
    { rep->maybe_convert_one_zero_to_idx (z_len); }

private:

  idx_vector_rep *rep;

  void init_state (void) { rep->init_state (); }
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
