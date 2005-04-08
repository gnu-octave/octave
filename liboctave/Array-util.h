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

extern bool index_in_bounds (const Array<octave_idx_type>& ra_idx,
			     const dim_vector& dimensions);

extern void increment_index (Array<octave_idx_type>& ra_idx,
			     const dim_vector& dimensions,
			     int start_dimension = 0);

extern octave_idx_type get_scalar_idx (Array<octave_idx_type>& idx, dim_vector& dims);

extern octave_idx_type num_ones (const Array<octave_idx_type>& ra_idx);

extern bool is_scalar (const dim_vector& dim);

extern bool any_ones (const Array<octave_idx_type>& arr);

extern octave_idx_type compute_index (const Array<octave_idx_type>& ra_idx, const dim_vector& dims);

extern Array<octave_idx_type> conv_to_int_array (const Array<idx_vector>& a);

extern Array<idx_vector> conv_to_array (const idx_vector *tmp, const octave_idx_type len);

extern dim_vector freeze (Array<idx_vector>& ra_idx,
			  const dim_vector& dimensions, int resize_ok);

extern bool vector_equivalent (const Array<octave_idx_type>& ra_idx);

extern bool all_ok (const Array<idx_vector>& ra_idx);

extern bool any_orig_empty (const Array<idx_vector>& ra_idx);

extern bool all_colon_equiv (const Array<idx_vector>& ra_idx,
			     const dim_vector& frozen_lengths);

extern bool is_in (octave_idx_type num, const idx_vector& idx);

extern octave_idx_type how_many_lgt (const octave_idx_type num, idx_vector& idxv);

extern bool all_ones (const Array<octave_idx_type>& arr);

extern Array<octave_idx_type> get_elt_idx (const Array<idx_vector>& ra_idx,
			       const Array<octave_idx_type>& result_idx);

extern Array<octave_idx_type> get_ra_idx (octave_idx_type idx, const dim_vector& dims);

extern dim_vector short_freeze (Array<idx_vector>& ra_idx,
				const dim_vector& dimensions,
				int resize_ok);

extern Array<octave_idx_type> calc_permutated_idx (const Array<octave_idx_type>& old_idx, 
				       const Array<octave_idx_type>& perm_vec, bool inv);

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
