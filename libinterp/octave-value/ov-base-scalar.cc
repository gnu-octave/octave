////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

// This file should not include config.h.  It is only included in other
// C++ source files that should have included config.h before including
// this file.

#include <ostream>
#include <sstream>

#include "oct-inttypes-fwd.h"

#include "ovl.h"
#include "ov-base.h"
#include "ov-cx-mat.h"
#include "ov-re-mat.h"
#include "ov-base-scalar.h"
#include "pr-output.h"

template <typename ST>
octave_value
octave_base_scalar<ST>::subsref (const std::string& type,
                                 const std::list<octave_value_list>& idx)
{
  octave_value retval;

  switch (type[0])
    {
    case '(':
      retval = do_index_op (idx.front ());
      break;

    case '{':
    case '.':
      {
        std::string nm = type_name ();
        error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
      }
      break;

    default:
      panic_impossible ();
    }

  return retval.next_subsref (type, idx);
}

template <typename ST>
octave_value
octave_base_scalar<ST>::subsasgn (const std::string& type,
                                  const std::list<octave_value_list>& idx,
                                  const octave_value& rhs)
{
  octave_value retval;

  switch (type[0])
    {
    case '(':
      {
        if (type.length () != 1)
          {
            std::string nm = type_name ();
            error ("in indexed assignment of %s, last rhs index must be ()",
                   nm.c_str ());
          }

        retval = numeric_assign (type, idx, rhs);
      }
      break;

    case '{':
    case '.':
      {
        std::string nm = type_name ();
        error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
      }
      break;

    default:
      panic_impossible ();
    }

  return retval;
}

template <typename ST>
dim_vector
octave_base_scalar<ST>::dims (void) const
{
  static dim_vector dv (1, 1);
  return dv;
}

template <typename ST>
octave_value
octave_base_scalar<ST>::permute (const Array<int>& vec, bool inv) const
{
  return Array<ST> (dim_vector (1, 1), scalar).permute (vec, inv);
}

template <typename ST>
octave_value
octave_base_scalar<ST>::reshape (const dim_vector& new_dims) const
{
  return Array<ST> (dim_vector (1, 1), scalar).reshape (new_dims);
}

template <typename ST>
octave_value
octave_base_scalar<ST>::diag (octave_idx_type k) const
{
  return Array<ST> (dim_vector (1, 1), scalar).diag (k);
}

template <typename ST>
octave_value
octave_base_scalar<ST>::diag (octave_idx_type m, octave_idx_type n) const
{
  return Array<ST> (dim_vector (1, 1), scalar).diag (m, n);
}

template <typename ST>
bool
octave_base_scalar<ST>::is_true (void) const
{
  if (octave::math::isnan (scalar))
    octave::err_nan_to_logical_conversion ();

  return (scalar != ST ());
}

template <typename ST>
void
octave_base_scalar<ST>::print (std::ostream& os, bool pr_as_read_syntax)
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

template <typename ST>
void
octave_base_scalar<ST>::print_raw (std::ostream& os,
                                   bool pr_as_read_syntax) const
{
  indent (os);
  octave_print_internal (os, scalar, pr_as_read_syntax);
}

template <typename ST>
bool
octave_base_scalar<ST>::print_name_tag (std::ostream& os,
                                        const std::string& name) const
{
  indent (os);
  os << name << " = ";
  return false;
}

template <typename ST>
void
octave_base_scalar<ST>::short_disp (std::ostream& os) const
{
  std::ostringstream buf;
  octave_print_internal (buf, scalar);
  std::string tmp = buf.str ();
  std::size_t pos = tmp.find_first_not_of (' ');
  if (pos != std::string::npos)
    os << tmp.substr (pos);
  else if (! tmp.empty ())
    os << tmp[0];
}

template <typename ST>
float_display_format
octave_base_scalar<ST>::get_edit_display_format (void) const
{
  return make_format (scalar);
}

template <typename ST>
std::string
octave_base_scalar<ST>::edit_display (const float_display_format& fmt,
                                      octave_idx_type, octave_idx_type) const
{
  std::ostringstream buf;
  octave_print_internal (buf, fmt, scalar);
  return buf.str ();
}

template <typename ST>
octave_value
octave_base_scalar<ST>::fast_elem_extract (octave_idx_type n) const
{
  return (n == 0) ? octave_value (scalar) : octave_value ();
}

template <typename ST>
bool
octave_base_scalar<ST>::fast_elem_insert_self (void *where,
    builtin_type_t btyp) const
{

  // Don't use builtin_type () here to avoid an extra VM call.
  if (btyp == class_to_btyp<ST>::btyp)
    {
      *(reinterpret_cast<ST *>(where)) = scalar;
      return true;
    }
  else
    return false;
}
