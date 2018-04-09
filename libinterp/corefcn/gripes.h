/*

Copyright (C) 1993-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

// FIXME: All gripe_XXX functions deprecated in 4.2.  Remove file in
// version 5.

#if ! defined (octave_gripes_h)
#define octave_gripes_h 1

#include "octave-config.h"

#include <string>

#include "lo-array-gripes.h"

class octave_value;
namespace octave
{
  class execution_exception;
}

////////////////////////////////////////////////////////////////////////////////
// Alphabetized list of gripes.
////////////////////////////////////////////////////////////////////////////////

OCTAVE_DEPRECATED (4.2, "use 'err_2_or_3_dim_plot' instead")
OCTAVE_NORETURN OCTINTERP_API extern void
gripe_2_or_3_dim_plot (void);

OCTAVE_DEPRECATED (4.2, "use 'err_data_conversion' instead")
OCTAVE_NORETURN OCTINTERP_API extern void
gripe_data_conversion (const char *from, const char *to);

OCTAVE_DEPRECATED (4.2, "use 'warn_data_file_in_path' instead")
OCTINTERP_API extern void
gripe_data_file_in_path (const std::string& fcn, const std::string& file);

OCTAVE_DEPRECATED (4.2, "use 'err_disabled_feature' or 'warn_disabled_feature' instead")
OCTAVE_NORETURN OCTINTERP_API extern void
gripe_disabled_feature (const std::string& fcn,
                        const std::string& feature,
                        const std::string& pkg="Octave");

OCTAVE_DEPRECATED (4.2, "use 'warn_divide_by_zero' instead")
OCTINTERP_API extern void
gripe_divide_by_zero (void);

OCTAVE_DEPRECATED (4.2, "use 'warn_empty_arg' instead")
OCTINTERP_API extern void
gripe_empty_arg (const char *name, bool is_error);

OCTAVE_DEPRECATED (4.2, "use 'warn_implicit_conversion' instead")
OCTINTERP_API extern void
gripe_implicit_conversion (const char *id, const char *from, const char *to);

OCTAVE_DEPRECATED (4.2, "use 'warn_implicit_conversion' instead")
OCTINTERP_API extern void
gripe_implicit_conversion (const std::string& id, const std::string& from,
                           const std::string& to);

OCTAVE_DEPRECATED (4.2, "use 'err_indexed_cs_list' instead")
OCTAVE_NORETURN OCTINTERP_API extern void
gripe_indexed_cs_list (void);

OCTAVE_DEPRECATED (4.2, "use 'err_invalid_conversion' instead")
OCTAVE_NORETURN OCTINTERP_API extern void
gripe_invalid_conversion (const std::string& from, const std::string& to);

OCTAVE_DEPRECATED (4.2, "use 'err_invalid_inquiry_subscript' instead")
OCTAVE_NORETURN OCTINTERP_API extern void
gripe_invalid_inquiry_subscript (void);

OCTAVE_DEPRECATED (4.2, "use 'warn_invalid_value_specified' instead")
OCTINTERP_API extern void
gripe_invalid_value_specified (const char *name);

OCTAVE_DEPRECATED (4.2, "use 'warn_logical_conversion' instead")
OCTINTERP_API extern void
gripe_logical_conversion (void);

OCTAVE_DEPRECATED (4.2, "use 'err_nonbraced_cs_list_assignment' instead")
OCTAVE_NORETURN OCTINTERP_API extern void
gripe_nonbraced_cs_list_assignment (void);

OCTAVE_DEPRECATED (4.2, "use 'err_nonconformant' instead")
OCTAVE_NORETURN OCTINTERP_API extern void
gripe_nonconformant (void);

OCTAVE_DEPRECATED (4.2, "use 'err_nonconformant' instead")
OCTAVE_NORETURN OCTINTERP_API extern void
gripe_nonconformant (octave_idx_type r1, octave_idx_type c1,
                     octave_idx_type r2, octave_idx_type c2);

OCTAVE_DEPRECATED (4.2, "use 'err_not_implemented' instead")
OCTAVE_NORETURN OCTINTERP_API extern void
gripe_not_implemented (const char *);

// FIXME: DEPRECATED: Deprecated in 4.2, remove in 5.0
OCTAVE_DEPRECATED (4.2, "use 'err_disabled_feature' or 'warn_disabled_feature' instead")
OCTAVE_NORETURN OCTINTERP_API extern void
gripe_not_supported (const char *);

OCTAVE_DEPRECATED (4.2, "use 'err_range_invalid' instead")
OCTAVE_NORETURN OCTINTERP_API extern void
gripe_range_invalid (void);

OCTAVE_DEPRECATED (4.2, "use 'err_square_matrix_required' instead")
OCTAVE_NORETURN OCTINTERP_API extern void
gripe_square_matrix_required (const char *name);

OCTAVE_DEPRECATED (4.2, "use 'err_string_invalid' instead")
OCTAVE_NORETURN OCTINTERP_API extern void
gripe_string_invalid (void);

OCTAVE_DEPRECATED (4.2, "use 'err_unrecognized_data_fmt' instead")
OCTAVE_NORETURN OCTINTERP_API extern void
gripe_unrecognized_data_fmt (const char *warn_for);

OCTAVE_DEPRECATED (4.2, "use 'err_unrecognized_float_fmt' instead")
OCTAVE_NORETURN OCTINTERP_API extern void
gripe_unrecognized_float_fmt (void);

OCTAVE_DEPRECATED (4.2, "use 'err_user_returned_invalid' instead")
OCTAVE_NORETURN OCTINTERP_API extern void
gripe_user_returned_invalid (const char *name);

OCTAVE_DEPRECATED (4.2, "use 'err_user_supplied_eval' instead")
OCTAVE_NORETURN OCTINTERP_API extern void
gripe_user_supplied_eval (const char *name);

OCTAVE_DEPRECATED (4.2, "use 'err_user_supplied_eval' instead")
OCTAVE_NORETURN OCTINTERP_API extern void
gripe_user_supplied_eval (octave::execution_exception& e, const char *name);

OCTAVE_DEPRECATED (4.2, "use 'warn_complex_cmp' instead")
OCTINTERP_API extern void
gripe_warn_complex_cmp (void);

OCTAVE_DEPRECATED (4.2, "use 'err_wrong_type_arg' instead")
OCTINTERP_API extern void
gripe_wrong_type_arg (const char *name, const char *s,
                      bool is_error = true);

OCTAVE_DEPRECATED (4.2, "use 'err_wrong_type_arg' instead")
OCTINTERP_API extern void
gripe_wrong_type_arg (octave::execution_exception& e,
                      const char *name, const char *s,
                      bool is_error = true);

OCTAVE_DEPRECATED (4.2, "use 'err_wrong_type_arg' instead")
OCTINTERP_API extern void
gripe_wrong_type_arg (const char *name, const std::string& s,
                      bool is_error = true);

OCTAVE_DEPRECATED (4.2, "use 'err_wrong_type_arg' instead")
OCTINTERP_API extern void
gripe_wrong_type_arg (octave::execution_exception& e,
                      const char *name, const std::string& s,
                      bool is_error = true);

OCTAVE_DEPRECATED (4.2, "use 'err_wrong_type_arg' or 'warn_wrong_type_arg' instead")
OCTINTERP_API extern void
gripe_wrong_type_arg (const char *name, const octave_value& tc,
                      bool is_error = true);

OCTAVE_DEPRECATED (4.2, "use 'err_wrong_type_arg' instead")
OCTINTERP_API extern void
gripe_wrong_type_arg (octave::execution_exception& e,
                      const char *name, const octave_value& tc,
                      bool is_error = true);

OCTAVE_DEPRECATED (4.2, "use 'err_wrong_type_arg' instead")
OCTINTERP_API extern void
gripe_wrong_type_arg (const std::string& name, const octave_value& tc,
                      bool is_error = true);

OCTAVE_DEPRECATED (4.2, "use 'err_wrong_type_arg' instead")
OCTINTERP_API extern void
gripe_wrong_type_arg (octave::execution_exception& e,
                      const std::string& name, const octave_value& tc,
                      bool is_error = true);

OCTAVE_DEPRECATED (4.2, "use 'err_wrong_type_arg' instead")
OCTINTERP_API extern void
gripe_wrong_type_arg (const char *s, bool is_error = true);

OCTAVE_DEPRECATED (4.2, "use 'err_wrong_type_arg' instead")
OCTINTERP_API extern void
gripe_wrong_type_arg (octave::execution_exception& e, const char *s,
                      bool is_error = true);

OCTAVE_DEPRECATED (4.2, "use 'err_wrong_type_arg' instead")
OCTINTERP_API extern void
gripe_wrong_type_arg (const std::string& s, bool is_error = true);

OCTAVE_DEPRECATED (4.2, "use 'err_wrong_type_arg' instead")
OCTINTERP_API extern void
gripe_wrong_type_arg (octave::execution_exception& e, const std::string& s,
                      bool is_error = true);

OCTAVE_DEPRECATED (4.2, "use 'err_wrong_type_arg' instead")
OCTINTERP_API extern void
gripe_wrong_type_arg (const octave_value& tc, bool is_error = true);

OCTAVE_DEPRECATED (4.2, "use 'err_wrong_type_arg' instead")
OCTINTERP_API extern void
gripe_wrong_type_arg (octave::execution_exception& e, const octave_value& tc,
                      bool is_error = true);

OCTAVE_DEPRECATED (4.2, "use 'err_wrong_type_arg_for_binary_op' instead")
OCTAVE_NORETURN OCTINTERP_API extern void
gripe_wrong_type_arg_for_binary_op (const octave_value& op);

OCTAVE_DEPRECATED (4.2, "use 'err_wrong_type_arg_for_unary_op' instead")
OCTAVE_NORETURN OCTINTERP_API extern void
gripe_wrong_type_arg_for_unary_op (const octave_value& op);

#endif
