/*

Copyright (C) 2000 John W. Eaton

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

#if !defined (octave_Array_util_h)
#define octave_Array_util_h 1

#include <cassert>

#include "Array.h"
#include "dim-vector.h"
#include "idx-vector.h"
#include "lo-error.h"

extern bool index_in_bounds (const Array<int>& ra_idx,
			     const dim_vector& dimensions);

extern void increment_index (Array<int>& ra_idx,
			     const dim_vector& dimensions,
			     int start_dimension = 0);

extern int get_scalar_idx (Array<int>& idx, dim_vector& dims);

extern int num_ones (const Array<int>& ra_idx);

extern bool is_scalar (const dim_vector& dim);

extern bool any_ones (const Array<int>& arr);

extern int compute_index (const Array<int>& ra_idx, const dim_vector& dims);

extern Array<int> conv_to_int_array (const Array<idx_vector>& a);

extern Array<idx_vector> conv_to_array (const idx_vector *tmp, const int len);

extern dim_vector freeze (Array<idx_vector>& ra_idx,
			  const dim_vector& dimensions, int resize_ok);

extern bool vector_equivalent (const Array<int>& ra_idx);

extern bool all_ok (const Array<idx_vector>& ra_idx);

extern bool any_orig_empty (const Array<idx_vector>& ra_idx);

extern bool all_colon_equiv (const Array<idx_vector>& ra_idx,
			     const dim_vector& frozen_lengths);

extern bool is_in (int num, const idx_vector& idx);

extern int how_many_lgt (const int num, idx_vector& idxv);

extern bool all_ones (const Array<int>& arr);

extern Array<int> get_elt_idx (const Array<idx_vector>& ra_idx,
			       const Array<int>& result_idx);

extern Array<int> get_ra_idx (int idx, const dim_vector& dims);

extern dim_vector short_freeze (Array<idx_vector>& ra_idx,
				const dim_vector& dimensions,
				int resize_ok);

extern Array<int> calc_permutated_idx (const Array<int>& old_idx, 
				       const Array<int>& perm_vec, bool inv);

extern void gripe_nonconformant (const char *op, int op1_len, int op2_len);

extern void gripe_nonconformant (const char *op, int op1_nr, int op1_nc,
				 int op2_nr, int op2_nc);


extern void gripe_nonconformant (const char *op, dim_vector& op1_dims,
				 dim_vector& op2_dims);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
