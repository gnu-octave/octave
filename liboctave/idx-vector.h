/*

Copyright (C) 1996, 1997 John W. Eaton

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
idx_vector
{
private:

  class
  idx_vector_rep
  {
  public:

    idx_vector_rep (void)
      : data (0), len (0), num_zeros (0), num_ones (0), max_val (0),
	min_val (0), count (1), frozen_at_z_len (0), frozen_len (0),
	colon (0), one_zero (0), initialized (0), frozen (0),
	colon_equiv_checked (0), colon_equiv (0), orig_dims () { }

    idx_vector_rep (const ColumnVector& v);

    idx_vector_rep (const NDArray& nda);

    template <class U>
    idx_vector_rep (const intNDArray<U>& inda)
      : data (0), len (inda.length ()), num_zeros (0), num_ones (0),
	max_val (0), min_val (0), count (1), frozen_at_z_len (0),
	frozen_len (0), colon (0), one_zero (0), initialized (0),
	frozen (0), colon_equiv_checked (0), colon_equiv (0),
	orig_dims (inda.dims ())
    {
      if (len == 0)
	{
	  initialized = 1;
	  return;
	}
      else
	{
	  data = new int [len];

	  bool conversion_error = false;

	  for (int i = 0; i < len; i++)
	    data[i] = tree_to_mat_idx (inda.elem (i), conversion_error);

	  if (conversion_error)
	    return;
	}

      init_state ();
    }

    idx_vector_rep (const Range& r);

    idx_vector_rep (double d);

    idx_vector_rep (int i);

    idx_vector_rep (char c);

    idx_vector_rep (bool b);

    template <class U>
    idx_vector_rep (const octave_int<U>& i)
      : data (0), len (1), num_zeros (0), num_ones (0),
	max_val (0), min_val (0), count (1), frozen_at_z_len (0),
	frozen_len (0), colon (0), one_zero (0), initialized (0),
	frozen (0), colon_equiv_checked (0), colon_equiv (0),
	orig_dims (1, 1)
    {
      data = new int [len];

      data[0] = tree_to_mat_idx (i);

      init_state ();
    }

    idx_vector_rep (const boolNDArray& bnda);

    idx_vector_rep (const idx_vector_rep& a);

    ~idx_vector_rep (void) { delete [] data; }

    idx_vector_rep& operator = (const idx_vector_rep& a);

    int ok (void) { return initialized; }

    int capacity (void) const { return len; }
    int length (int colon_len) const { return colon ? colon_len : len; }

    int elem (int n) const { return colon ? n : data[n]; }

    int checkelem (int n) const;
    int operator () (int n) const { return checkelem (n); }

    int max (void) const { return max_val; }
    int min (void) const { return min_val; }

    int one_zero_only (void) const { return one_zero; }
    int zeros_count (void) const { return num_zeros; }
    int ones_count (void) const { return num_ones; }

    int is_colon (void) const { return colon; }
    int is_colon_equiv (int n, int sort_uniq);

    void sort (bool uniq);

    int orig_rows (void) const { return orig_dims(0); }
    int orig_columns (void) const { return orig_dims(1); }

    dim_vector orig_dimensions (void) const { return orig_dims; }

    // other stuff

    void shorten (int n); // Unsafe.  Avoid at all cost.

    int freeze (int z_len, const char *tag, bool resize_ok, bool warn_resize);

    // i/o

    std::ostream& print (std::ostream& os) const;

    int *data;
    int len;
    int num_zeros;
    int num_ones;
    int max_val;
    int min_val;

    int count;
    int frozen_at_z_len;
    int frozen_len;

    unsigned int colon : 1;
    unsigned int one_zero : 1;
    unsigned int initialized : 1;
    unsigned int frozen : 1;
    unsigned int colon_equiv_checked : 1;
    unsigned int colon_equiv : 1;

    dim_vector orig_dims;
 
    void init_state (void);

    void maybe_convert_one_zero_to_idx (int z_len);

    int tree_to_mat_idx (double x, bool& conversion_error);

    int tree_to_mat_idx (int i) { return i - 1; }

    template <class U> int tree_to_mat_idx (const octave_int<U>& i)
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

  idx_vector (int i) : rep (new idx_vector_rep (i)) { }

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

  int capacity (void) const { return rep->capacity (); }
  int length (int cl) const { return rep->length (cl); }

  int elem (int n) const { return rep->elem (n); }
  int checkelem (int n) const { return rep->checkelem (n); }
  int operator () (int n) const { return rep->operator () (n); }

  int max (void) const { return rep->max (); }
  int min (void) const { return rep->min (); }

  int one_zero_only (void) const { return rep->one_zero_only (); }
  int zeros_count (void) const { return rep->zeros_count (); }
  int ones_count (void) const { return rep->ones_count (); }

  int is_colon (void) const { return rep->is_colon (); }
  int is_colon_equiv (int n, int sort_uniq = 0) const
    { return rep->is_colon_equiv (n, sort_uniq); }

  void sort (bool uniq = false) { rep->sort (uniq); }

  int orig_rows (void) const { return rep->orig_rows (); }
  int orig_columns (void) const { return rep->orig_columns (); }

  dim_vector orig_dimensions (void) const { return rep->orig_dimensions (); }

  int orig_empty (void) const
    { return (! is_colon () && orig_dimensions().any_zero ()); }

  // Unsafe.  Avoid at all cost.
  void shorten (int n) { rep->shorten (n); }

  // i/o

  int freeze (int z_len, const char *tag, bool resize_ok = false,
	      bool warn_resize = false)
    { return rep->freeze (z_len, tag, resize_ok, warn_resize); }

  std::ostream& print (std::ostream& os) const { return rep->print (os); }

  friend std::ostream& operator << (std::ostream& os, const idx_vector& a)
    { return a.print (os); }

  void maybe_convert_one_zero_to_idx (int z_len)
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
