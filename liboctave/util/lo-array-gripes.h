/*

Copyright (C) 2000-2016 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

// FIXME: All gripe_XXX functions deprecated in 4.2.  Remove file in 4.6

#if ! defined (octave_lo_array_gripes_h)
#define octave_lo_array_gripes_h 1

#include "octave-config.h"

#include "lo-array-errwarn.h"
#include "dim-vector.h"
#include "quit.h"

OCTAVE_DEPRECATED ("use 'octave::err_nan_to_logical_conversion' instead")
OCTAVE_NORETURN OCTAVE_API extern void
gripe_nan_to_logical_conversion (void);

OCTAVE_DEPRECATED ("use 'octave::err_nan_to_character_conversion' instead")
OCTAVE_NORETURN OCTAVE_API extern void
gripe_nan_to_character_conversion (void);

OCTAVE_DEPRECATED ("use 'octave::err_nonconformant' instead")
OCTAVE_NORETURN OCTAVE_API extern void
gripe_nonconformant (const char *op,
                     octave_idx_type op1_len,
                     octave_idx_type op2_len);

OCTAVE_DEPRECATED ("use 'octave::err_nonconformant' instead")
OCTAVE_NORETURN OCTAVE_API extern void
gripe_nonconformant (const char *op,
                     octave_idx_type op1_nr, octave_idx_type op1_nc,
                     octave_idx_type op2_nr, octave_idx_type op2_nc);

OCTAVE_DEPRECATED ("use 'octave::err_nonconformant' instead")
OCTAVE_NORETURN OCTAVE_API extern void
gripe_nonconformant (const char *op, const dim_vector& op1_dims,
                     const dim_vector& op2_dims);

OCTAVE_DEPRECATED ("use 'octave::err_index_out_of_range' instead")
OCTAVE_NORETURN OCTAVE_API extern void
gripe_index_out_of_range (int nd, int dim,
                          octave_idx_type iext, octave_idx_type ext,
                          const dim_vector& d);

OCTAVE_DEPRECATED ("use 'octave::err_index_out_of_range' instead")
OCTAVE_NORETURN OCTAVE_API extern void
gripe_index_out_of_range (int nd, int dim,
                          octave_idx_type iext, octave_idx_type ext);

OCTAVE_DEPRECATED ("use 'octave::err_del_index_out_of_range' instead")
OCTAVE_NORETURN OCTAVE_API extern void
gripe_del_index_out_of_range (bool is1d, octave_idx_type iext,
                              octave_idx_type ext);

OCTAVE_DEPRECATED ("use 'octave::err_invalid_index' instead")
OCTAVE_NORETURN OCTAVE_API extern void
gripe_invalid_index (double, octave_idx_type nd = 0,
                     octave_idx_type dim = 0,
                     const std::string& var = "");

OCTAVE_DEPRECATED ("use 'octave::err_invalid_index' instead")
OCTAVE_NORETURN OCTAVE_API extern void
gripe_invalid_index (octave_idx_type n, octave_idx_type nd = 0,
                     octave_idx_type dim = 0,
                     const std::string& var = "");

OCTAVE_DEPRECATED ("use 'octave::err_invalid_index' instead")
OCTAVE_NORETURN OCTAVE_API extern void
gripe_invalid_index (const std::string& idx, octave_idx_type nd = 0,
                     octave_idx_type dim = 0,
                     const std::string& var = "");

OCTAVE_DEPRECATED ("use 'octave::err_invalid_resize' instead")
OCTAVE_NORETURN OCTAVE_API extern void
gripe_invalid_resize (void);

OCTAVE_DEPRECATED ("use 'octave::err_singular_matrix' instead")
OCTAVE_API extern void
gripe_singular_matrix (double rcond = 0.0);

#endif

