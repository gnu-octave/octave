// idx-vector.h                                        -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

#if !defined (octave_idx_vector_h)
#define octave_idx_vector_h 1

class ostream;
class Matrix;
class Range;

class
idx_vector
{
public:
  idx_vector (void);
  idx_vector (const idx_vector& a);

  idx_vector (const Matrix& m, int do_ftn_idx,
	      const char *rc = 0, int z_len = 0);

  idx_vector (const Range& r);

 ~idx_vector (void);

  idx_vector& operator = (const idx_vector& a);

  operator void * () const;

  int capacity (void) const;
  int length (void) const;

  int elem (int n) const;
  int checkelem (int n) const;
  int operator () (int n) const;

// other stuff

  int max (void) const;
  int min (void) const;

  int one_zero_only (void) const;
  int zeros_count (void) const;
  int ones_count (void) const;

  void sort (void);
  void sort_uniq (void);

  void shorten (int n); // Unsafe.  Avoid at all cost.

// i/o

  friend ostream& operator << (ostream& os, const idx_vector& a);

private:

  int len;
  int one_zero;
  int num_zeros;
  int num_ones;
  int max_val;
  int min_val;
  int initialized;
  int *data;

  void init_state (const char *rc = 0, int z_len = 0);
  void convert_one_zero_to_idx (void);
};

inline idx_vector::idx_vector (void)
{
  len = 0;
  data = 0;
  num_zeros = 0;
  num_ones = 0;
  one_zero = 0;
  initialized = 0;
}

inline idx_vector::~idx_vector (void)
{
  delete [] data;
}

inline idx_vector::operator void * () const
{
  return initialized ? (void *) 1 : (void *) 0;
}

inline int idx_vector::capacity (void) const { return len; }
inline int idx_vector::length (void) const { return len; }

inline int idx_vector::elem (int n) const { return data[n]; }

inline int idx_vector::operator () (int n) const { return checkelem (n); }

inline int idx_vector::max (void) const { return max_val; }
inline int idx_vector::min (void) const { return min_val; }

inline int idx_vector::one_zero_only (void) const { return one_zero; }
inline int idx_vector::zeros_count (void) const { return num_zeros; }
inline int idx_vector::ones_count (void) const { return num_ones; }

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
