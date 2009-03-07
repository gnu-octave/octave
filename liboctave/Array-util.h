/*

Copyright (C) 2000, 2003, 2004, 2005, 2006, 2007, 2008 John W. Eaton

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

#if !defined (octave_Array_util_h)
#define octave_Array_util_h 1

#include <cassert>

#include "Array.h"
#include "dim-vector.h"
#include "idx-vector.h"
#include "lo-error.h"

extern OCTAVE_API bool index_in_bounds (const Array<octave_idx_type>& ra_idx,
			     const dim_vector& dimensions);

extern OCTAVE_API void increment_index (Array<octave_idx_type>& ra_idx,
			     const dim_vector& dimensions,
			     int start_dimension = 0);

extern OCTAVE_API octave_idx_type get_scalar_idx (Array<octave_idx_type>& idx, dim_vector& dims);

extern OCTAVE_API octave_idx_type num_ones (const Array<octave_idx_type>& ra_idx);

extern OCTAVE_API bool is_scalar (const dim_vector& dim);

extern OCTAVE_API bool is_vector (const dim_vector& dim);

extern OCTAVE_API bool any_ones (const Array<octave_idx_type>& arr);

extern OCTAVE_API octave_idx_type compute_index (const Array<octave_idx_type>& ra_idx, const dim_vector& dims);

extern OCTAVE_API Array<octave_idx_type> conv_to_int_array (const Array<idx_vector>& a);

extern OCTAVE_API Array<idx_vector> conv_to_array (const idx_vector *tmp, const octave_idx_type len);

extern OCTAVE_API dim_vector freeze (Array<idx_vector>& ra_idx,
			  const dim_vector& dimensions, int resize_ok);

extern OCTAVE_API bool vector_equivalent (const dim_vector& dv);

extern OCTAVE_API bool all_ok (const Array<idx_vector>& ra_idx);

extern OCTAVE_API bool any_orig_empty (const Array<idx_vector>& ra_idx);

extern OCTAVE_API bool all_colon_equiv (const Array<idx_vector>& ra_idx,
			     const dim_vector& frozen_lengths);

extern OCTAVE_API bool all_ones (const Array<octave_idx_type>& arr);

extern OCTAVE_API Array<octave_idx_type> get_elt_idx (const Array<idx_vector>& ra_idx,
			       const Array<octave_idx_type>& result_idx);

extern OCTAVE_API Array<octave_idx_type> get_ra_idx (octave_idx_type idx, const dim_vector& dims);

extern OCTAVE_API dim_vector zero_dims_inquire (const Array<idx_vector>& ia,
                                                const dim_vector& rhdv);

extern OCTAVE_API dim_vector zero_dims_inquire (const idx_vector& i, const idx_vector& j,
                                                const dim_vector& rhdv);

struct
permute_vector
{
  octave_idx_type pidx;
  octave_idx_type iidx;
};

extern int OCTAVE_API permute_vector_compare (const void *a, const void *b);

extern void OCTAVE_API gripe_nan_to_logical_conversion (void);

extern void OCTAVE_API gripe_nonconformant (const char *op, int op1_len, int op2_len);

extern void OCTAVE_API gripe_nonconformant (const char *op, int op1_nr, int op1_nc,
				 int op2_nr, int op2_nc);


extern void OCTAVE_API gripe_nonconformant (const char *op, dim_vector& op1_dims,
				 dim_vector& op2_dims);

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
