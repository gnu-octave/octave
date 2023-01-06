////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2016-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"
#include "utils.h"

////////////////////////////////////////////////////////////////////////////////
// Alphabetized list of common errors and warnings.
////////////////////////////////////////////////////////////////////////////////

void
err_2_or_3_dim_plot (void)
{
  error ("plot: can only plot in 2 or 3 dimensions");
}

void
err_data_conversion (const char *from, const char *to)
{
  error ("unable to convert from %s to %s format", from, to);
}

void
err_disabled_feature (const std::string& fcn, const std::string& feature,
                      const std::string& pkg /* ="Octave" */)
{
  if (! fcn.empty ())
    error ("%s: support for %s was unavailable or disabled when %s was built",
           fcn.c_str (), feature.c_str (), pkg.c_str ());
  else
    error ("support for %s was unavailable or disabled when %s was built",
           feature.c_str (), pkg.c_str ());
}

void
err_indexed_cs_list (void)
{
  error ("a cs-list cannot be further indexed");
}

void
err_invalid_conversion (const std::string& from, const std::string& to)
{
  error ("invalid conversion from %s to %s", from.c_str (), to.c_str ());
}

void
err_invalid_inquiry_subscript (void)
{
  error ("invalid dimension inquiry of a non-existent value");
}

void
err_invalid_structure_assignment (void)
{
  error ("invalid dot name structure assignment because the structure array is empty.  Specify a subscript on the structure array to resolve.");
}

void
err_nonbraced_cs_list_assignment (void)
{
  error ("invalid assignment to cs-list outside multiple assignment");
}

void
err_nonconformant (void)
{
  error ("nonconformant matrices");
}

void
err_nonconformant (octave_idx_type r1, octave_idx_type c1,
                   octave_idx_type r2, octave_idx_type c2)
{
  error ("nonconformant matrices (op1 is %" OCTAVE_IDX_TYPE_FORMAT
         "x%" OCTAVE_IDX_TYPE_FORMAT ", op2 is %" OCTAVE_IDX_TYPE_FORMAT
         "x%" OCTAVE_IDX_TYPE_FORMAT ")", r1, c1, r2, c2);
}

void
err_not_implemented (const char *fcn)
{
  error ("%s: not implemented", fcn);
}

void
err_range_invalid (void)
{
  error ("range constant used in invalid context");
}

void
err_square_matrix_required (const char *fcn, const char *name)
{
  error ("%s: %s must be a square matrix", fcn, name);
}

void
err_string_invalid (void)
{
  error ("std::string constant used in invalid context");
}

void
err_unrecognized_data_fmt (const char *name)
{
  error ("%s: unrecognized data format requested", name);
}

void
err_unrecognized_float_fmt (void)
{
  error ("unrecognized floating point format requested");
}

void
err_user_returned_invalid (const char *name)
{
  error ("%s: user-supplied function returned invalid value", name);
}

void
err_user_supplied_eval (const char *name)
{
  octave::execution_exception ee;

  err_user_supplied_eval (ee, name);
}

void
err_user_supplied_eval (octave::execution_exception& ee, const char *name)
{
  error (ee, "%s: evaluation of user-supplied function failed", name);
}

void
err_wrong_type_arg (const char *name, const char *s)
{
  octave::execution_exception ee;

  err_wrong_type_arg (ee, name, s);
}

void
err_wrong_type_arg (octave::execution_exception& ee,
                    const char *name, const char *s)
{
  error (ee, "%s: wrong type argument '%s'", name, s);
}

void
err_wrong_type_arg (const char *name, const std::string& s)
{
  octave::execution_exception ee;

  err_wrong_type_arg (ee, name, s.c_str ());
}

void
err_wrong_type_arg (octave::execution_exception& ee,
                    const char *name, const std::string& s)
{
  err_wrong_type_arg (ee, name, s.c_str ());
}

void
err_wrong_type_arg (const char *name, const octave_value& tc)
{
  octave::execution_exception ee;

  err_wrong_type_arg (ee, name, tc);
}

void
err_wrong_type_arg (octave::execution_exception& ee,
                    const char *name, const octave_value& tc)
{
  std::string type = tc.type_name ();

  err_wrong_type_arg (ee, name, type);
}

void
err_wrong_type_arg (const std::string& name, const octave_value& tc)
{
  octave::execution_exception ee;

  err_wrong_type_arg (ee, name, tc);
}

void
err_wrong_type_arg (octave::execution_exception& ee,
                    const std::string& name, const octave_value& tc)
{
  err_wrong_type_arg (ee, name.c_str (), tc);
}

void
err_wrong_type_arg (const char *s)
{
  octave::execution_exception ee;

  err_wrong_type_arg (ee, s);
}

void
err_wrong_type_arg (octave::execution_exception& ee, const char *s)
{
  error (ee, "wrong type argument '%s'", s);
}

void
err_wrong_type_arg (const std::string& s)
{
  octave::execution_exception ee;

  err_wrong_type_arg (ee, s);
}

void
err_wrong_type_arg (octave::execution_exception& ee, const std::string& s)
{
  err_wrong_type_arg (ee, s.c_str ());
}

void
err_wrong_type_arg (const octave_value& tc)
{
  octave::execution_exception ee;

  err_wrong_type_arg (ee, tc);
}

void
err_wrong_type_arg (octave::execution_exception& ee, const octave_value& tc)
{
  std::string type = tc.type_name ();

  err_wrong_type_arg (ee, type);
}

void
err_wrong_type_arg_for_binary_op (const octave_value& op)
{
  std::string type = op.type_name ();
  error ("invalid operand '%s' for binary operator", type.c_str ());
}

void
err_wrong_type_arg_for_unary_op (const octave_value& op)
{
  std::string type = op.type_name ();
  error ("invalid operand '%s' for unary operator", type.c_str ());
}

void
warn_array_as_logical (const dim_vector& dv)
{
  warning_with_id ("Octave:array-as-logical",
                   "Using an object of size %s as "
                   "a boolean value implies all().",
                   dv.str ().c_str ());
}

/*
%!warning <boolean value implies all>
%! warning ("on", "Octave:array-as-logical");
%! if ([1 1 0])
%!   assert (false);
%! endif
*/

void
warn_complex_cmp (void)
{
  warning_with_id ("Octave:language-extension",
                   "comparing complex numbers is not supported in Matlab");
}

void
warn_data_file_in_path (const std::string& fcn, const std::string& file)
{
  warning_with_id ("Octave:data-file-in-path",
                   "%s: '%s' found by searching load path",
                   fcn.c_str (), file.c_str ());
}

void
warn_disabled_feature (const std::string& fcn, const std::string& feature,
                       const std::string& pkg /*="Octave"*/)
{
  if (! fcn.empty ())
    warning ("%s: support for %s was unavailable or disabled when %s was built",
             fcn.c_str (), feature.c_str (), pkg.c_str ());
  else
    warning ("support for %s was unavailable or disabled when %s was built",
             feature.c_str (), pkg.c_str ());
}

void
warn_empty_arg (const char *name)
{
  warning ("%s: argument is empty matrix", name);
}

void
warn_empty_index (const std::string& type_name)
{
  warning_with_id ("Octave:empty-index",
                   "'%s' object indexed with empty index list",
                   type_name.c_str ());
}

void
warn_implicit_conversion (const char *id, const char *from, const char *to)
{
  warning_with_id (id, "implicit conversion from %s to %s", from, to);
}

void
warn_implicit_conversion (const std::string& id,
                          const std::string& from, const std::string& to)
{
  warning_with_id (id.c_str (),
                   "implicit conversion from %s to %s",
                   from.c_str (), to.c_str ());
}

void
warn_invalid_value_specified (const char *name)
{
  warning ("invalid value specified for '%s'", name);
}

void
warn_logical_conversion (void)
{
  warning_with_id ("Octave:logical-conversion",
                   "value not equal to 1 or 0 converted to logical 1");
}

void
warn_wrong_type_arg (const char *name, const octave_value& tc)
{
  std::string type = tc.type_name ();

  warning ("%s: wrong type argument '%s'", name, type.c_str ());
}
