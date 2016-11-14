/*

Copyright (C) 2016 Rik Wehbring

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

#if ! defined (octave_errwarn_h)
#define octave_errwarn_h 1

#include "octave-config.h"

#include <string>

#include "lo-array-errwarn.h"

class octave_value;
namespace octave
{
  class execution_exception;
}

////////////////////////////////////////////////////////////////////////////////
// Alphabetized list of common errors and warnings.
////////////////////////////////////////////////////////////////////////////////

OCTAVE_NORETURN OCTINTERP_API extern void
err_2_or_3_dim_plot (void);

OCTAVE_NORETURN OCTINTERP_API extern void
err_data_conversion (const char *from, const char *to);

OCTAVE_NORETURN OCTINTERP_API extern void
err_disabled_feature (const std::string& fcn, const std::string& feature,
                      const std::string& pkg = "Octave");

OCTAVE_NORETURN OCTINTERP_API extern void
err_indexed_cs_list (void);

OCTAVE_NORETURN OCTINTERP_API extern void
err_invalid_conversion (const std::string& from, const std::string& to);

OCTAVE_NORETURN OCTINTERP_API extern void
err_invalid_inquiry_subscript (void);

OCTAVE_NORETURN OCTINTERP_API extern void
err_nonbraced_cs_list_assignment (void);

OCTAVE_NORETURN OCTINTERP_API extern void
err_nonconformant (void);

OCTAVE_NORETURN OCTINTERP_API extern void
err_nonconformant (octave_idx_type r1, octave_idx_type c1,
                   octave_idx_type r2, octave_idx_type c2);

OCTAVE_NORETURN OCTINTERP_API extern void
err_not_implemented (const char *);

OCTAVE_NORETURN OCTINTERP_API extern void
err_range_invalid (void);

OCTAVE_NORETURN OCTINTERP_API extern void
err_square_matrix_required (const char *fcn, const char *name);

OCTAVE_NORETURN OCTINTERP_API extern void
err_string_invalid (void);

OCTAVE_NORETURN OCTINTERP_API extern void
err_unrecognized_data_fmt (const char *name);

OCTAVE_NORETURN OCTINTERP_API extern void
err_unrecognized_float_fmt (void);

OCTAVE_NORETURN OCTINTERP_API extern void
err_user_returned_invalid (const char *name);

OCTAVE_NORETURN OCTINTERP_API extern void
err_user_supplied_eval (const char *name);

OCTAVE_NORETURN OCTINTERP_API extern void
err_user_supplied_eval (octave::execution_exception& e, const char *name);

OCTAVE_NORETURN OCTINTERP_API extern void
err_wrong_type_arg (const char *name, const char *s);

OCTAVE_NORETURN OCTINTERP_API extern void
err_wrong_type_arg (octave::execution_exception& e, const char *name,
                    const char *s);

OCTAVE_NORETURN OCTINTERP_API extern void
err_wrong_type_arg (const char *name, const std::string& s);

OCTAVE_NORETURN OCTINTERP_API extern void
err_wrong_type_arg (octave::execution_exception& e, const char *name,
                    const std::string& s);

OCTAVE_NORETURN OCTINTERP_API extern void
err_wrong_type_arg (const char *name, const octave_value& tc);

OCTAVE_NORETURN OCTINTERP_API extern void
err_wrong_type_arg (octave::execution_exception& e, const char *name,
                    const octave_value& tc);

OCTAVE_NORETURN OCTINTERP_API extern void
err_wrong_type_arg (const std::string& name, const octave_value& tc);

OCTAVE_NORETURN OCTINTERP_API extern void
err_wrong_type_arg (octave::execution_exception& e, const std::string& name,
                    const octave_value& tc);

OCTAVE_NORETURN OCTINTERP_API extern void
err_wrong_type_arg (const char *s);

OCTAVE_NORETURN OCTINTERP_API extern void
err_wrong_type_arg (octave::execution_exception& e, const char *s);

OCTAVE_NORETURN OCTINTERP_API extern void
err_wrong_type_arg (const std::string& s);

OCTAVE_NORETURN OCTINTERP_API extern void
err_wrong_type_arg (octave::execution_exception& e, const std::string& s);

OCTAVE_NORETURN OCTINTERP_API extern void
err_wrong_type_arg (const octave_value& tc);

OCTAVE_NORETURN OCTINTERP_API extern void
err_wrong_type_arg (octave::execution_exception& e, const octave_value& tc);

OCTAVE_NORETURN OCTINTERP_API extern void
err_wrong_type_arg_for_binary_op (const octave_value& op);

OCTAVE_NORETURN OCTINTERP_API extern void
err_wrong_type_arg_for_unary_op (const octave_value& op);

OCTINTERP_API extern void
warn_array_as_logical (const dim_vector& dv);

OCTINTERP_API extern void
warn_complex_cmp (void);

OCTINTERP_API extern void
warn_data_file_in_path (const std::string& fcn, const std::string& file);

OCTINTERP_API extern void
warn_disabled_feature (const std::string& fcn, const std::string& feature,
                       const std::string& pkg = "Octave");

OCTINTERP_API extern void
warn_divide_by_zero (void);

OCTINTERP_API extern void
warn_empty_arg (const char *name);

OCTINTERP_API extern void
warn_implicit_conversion (const char *id, const char *from, const char *to);

OCTINTERP_API extern void
warn_implicit_conversion (const std::string& id, const std::string& from,
                          const std::string& to);

OCTINTERP_API extern void
warn_invalid_value_specified (const char *name);

OCTINTERP_API extern void
warn_logical_conversion (void);

OCTINTERP_API extern void
warn_wrong_type_arg (const char *name, const octave_value& tc);

#endif

