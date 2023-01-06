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

#include <cinttypes>
#include <cmath>

#include <limits>
#include <sstream>

#include "lo-array-errwarn.h"
#include "lo-error.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Text constants used to shorten code below.

static const char *error_id_nonconformant_args
  = "Octave:nonconformant-args";

static const char *error_id_index_out_of_bounds
  = "Octave:index-out-of-bounds";

static const char *error_id_invalid_index = "Octave:invalid-index";

static const char *warning_id_nearly_singular_matrix
  = "Octave:nearly-singular-matrix";

static const char *warning_id_singular_matrix = "Octave:singular-matrix";

void
err_nan_to_logical_conversion (void)
{
  (*current_liboctave_error_handler)
    ("invalid conversion from NaN to logical");
}

void
err_nan_to_character_conversion (void)
{
  (*current_liboctave_error_handler)
    ("invalid conversion from NaN to character");
}

void
err_nonconformant (const char *op,
                   octave_idx_type op1_len, octave_idx_type op2_len)
{
  const char *err_id = error_id_nonconformant_args;

  (*current_liboctave_error_with_id_handler)
    (err_id, "%s: nonconformant arguments (op1 len: %" OCTAVE_IDX_TYPE_FORMAT
     ", op2 len: % " OCTAVE_IDX_TYPE_FORMAT ")",
     op, op1_len, op2_len);
}

void
err_nonconformant (const char *op,
                   octave_idx_type op1_nr, octave_idx_type op1_nc,
                   octave_idx_type op2_nr, octave_idx_type op2_nc)
{
  const char *err_id = error_id_nonconformant_args;

  (*current_liboctave_error_with_id_handler)
    (err_id, "%s: nonconformant arguments "
     "(op1 is %" OCTAVE_IDX_TYPE_FORMAT "x%" OCTAVE_IDX_TYPE_FORMAT ", "
     "op2 is %" OCTAVE_IDX_TYPE_FORMAT"x%" OCTAVE_IDX_TYPE_FORMAT ")",
     op, op1_nr, op1_nc, op2_nr, op2_nc);
}

void
err_nonconformant (const char *op,
                   const dim_vector& op1_dims, const dim_vector& op2_dims)
{
  const char *err_id = error_id_nonconformant_args;

  std::string op1_dims_str = op1_dims.str ();
  std::string op2_dims_str = op2_dims.str ();

  (*current_liboctave_error_with_id_handler)
    (err_id, "%s: nonconformant arguments (op1 is %s, op2 is %s)",
     op, op1_dims_str.c_str (), op2_dims_str.c_str ());
}

void
err_del_index_out_of_range (bool is1d, octave_idx_type idx,
                            octave_idx_type ext)
{
  const char *err_id = error_id_index_out_of_bounds;

  (*current_liboctave_error_with_id_handler)
    (err_id, "A(%s) = []: index out of bounds: value %" OCTAVE_IDX_TYPE_FORMAT
     " out of bound %" OCTAVE_IDX_TYPE_FORMAT,
     is1d ? "I" : "..,I,..", idx, ext);
}

// Show the expression that caused the error, e.g.,  "A(-1,_)",
// "A(0+1i)", "A(_,3)".  Show how many indices come before/after the
// offending one, e.g., (<error>), (<error>,_), or (_,<error>,...[x5]...)

std::string
index_exception::expression (void) const
{
  std::ostringstream buf;

  if (m_var.empty () || m_var == "<unknown>")
    buf << "index ";
  else
    buf << m_var;

  bool show_parens = m_dim > 0;

  if (show_parens)
    {
      if (m_dim < 5)
        {
          buf << '(';

          for (octave_idx_type i = 1; i < m_dim; i++)
            buf << "_,";
        }
      else
        buf << "(...[x" << m_dim - 1 << "]...";
    }

  buf << m_index;

  if (show_parens)
    {
      if (m_nd - m_dim < 5)
        {
          for (octave_idx_type i = 0; i < m_nd - m_dim; i++)
            buf << ",_";

          if (m_nd >= m_dim)
            buf << ')';
        }
      else
        buf << "...[x" << m_nd - m_dim << "]...)";
    }

  return buf.str ();
}

class invalid_index : public index_exception
{
public:

  invalid_index (const std::string& value, octave_idx_type ndim,
                 octave_idx_type dimen)
    : index_exception (value, ndim, dimen)
  {
    // Virtual, but the one we want to call is defined in this class.
    update_message ();
  }

  void update_message (void)
  {
    static std::string exp
      = std::to_string (std::numeric_limits<octave_idx_type>::digits);

    set_message (expression ()
                 + ": subscripts must be either integers 1 to (2^" + exp
                 + ")-1 or logicals");
  }

  // ID of error to throw
  const char * err_id (void) const
  {
    return error_id_invalid_index;
  }
};

// Complain if an index is negative, fractional, or too big.

void
err_invalid_index (const std::string& idx, octave_idx_type nd,
                   octave_idx_type dim, const std::string&)
{
  invalid_index e (idx, nd, dim);

  throw e;
}

void
err_invalid_index (octave_idx_type n, octave_idx_type nd,
                   octave_idx_type dim, const std::string& var)
{
  err_invalid_index (std::to_string (n + 1), nd, dim, var);
}

void
err_invalid_index (double n, octave_idx_type nd, octave_idx_type dim,
                   const std::string& var)
{
  std::ostringstream buf;
  buf << n + 1;

  if (! std::isnan (n))
    {
      // if  n  not an integer, but would be printed as one, show diff
      double nearest = std::floor (n + 1.5);
      if (n + 1 != nearest && (buf.str ().find ('.') == std::string::npos))
        buf << std::showpos << (n + 1 - nearest);
    }

  err_invalid_index (buf.str (), nd, dim, var);
}

// Complain for read access beyond the bounds of an array.

class out_of_range : public index_exception
{
public:

  out_of_range (const std::string& value, octave_idx_type nd,
                octave_idx_type dim, octave_idx_type ext,
                const dim_vector& size)
    : index_exception (value, nd, dim), m_size (size), m_extent (ext)
  {
    // Virtual, but the one we want to call is defined in this class.
    update_message ();
  }

  void update_message (void)
  {
    set_message (expression () + ": out of bound "
                 + std::to_string (m_extent)
                 + " (dimensions are " + m_size.str ('x') + ")");
  }

  // ID of error to throw.
  const char * err_id (void) const
  {
    return error_id_index_out_of_bounds;
  }

private:

  // Dimension of object being accessed.
  dim_vector m_size;

  // Length of dimension being accessed.
  octave_idx_type m_extent;
};

// Complain of an index that is out of range
void
err_index_out_of_range (int nd, int dim, octave_idx_type idx,
                        octave_idx_type ext, const dim_vector& dv)
{
  throw out_of_range (std::to_string (idx), nd, dim, ext, dv);
}

void
err_invalid_resize (void)
{
  (*current_liboctave_error_with_id_handler)
    ("Octave:invalid-resize",
     "Invalid resizing operation or ambiguous assignment to an out-of-bounds array element");
}

void
warn_singular_matrix (double rcond)
{
  if (rcond == 0.0)
    {
      (*current_liboctave_warning_with_id_handler)
        (warning_id_singular_matrix,
         "matrix singular to machine precision");
    }
  else
    {
      (*current_liboctave_warning_with_id_handler)
        (warning_id_nearly_singular_matrix,
         "matrix singular to machine precision, rcond = %g", rcond);
    }
}

OCTAVE_END_NAMESPACE(octave)

/* Tests in test/index.tst */
