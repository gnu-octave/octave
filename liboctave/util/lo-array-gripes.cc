/*

Copyright (C) 2003-2016 John W. Eaton
Copyright (C) 2009 VZLU Prague

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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <sstream>

#include "lo-array-gripes.h"
#include "lo-error.h"

// Text constants used to shorten code below.
static const char *error_id_nonconformant_args = "Octave:nonconformant-args";

static const char *error_id_index_out_of_bounds = "Octave:index-out-of-bounds";

static const char *error_id_invalid_index = "Octave:invalid-index";

static const char *warning_id_nearly_singular_matrix =
  "Octave:nearly-singular-matrix";

static const char *warning_id_singular_matrix = "Octave:singular-matrix";

void
gripe_nan_to_logical_conversion (void)
{
  (*current_liboctave_error_handler)
    ("invalid conversion from NaN to logical");
}

void
gripe_nan_to_character_conversion (void)
{
  (*current_liboctave_error_handler)
    ("invalid conversion from NaN to character");
}

void
gripe_nonconformant (const char *op, octave_idx_type op1_len,
                     octave_idx_type op2_len)
{
  const char *err_id = error_id_nonconformant_args;

  (*current_liboctave_error_with_id_handler)
    (err_id, "%s: nonconformant arguments (op1 len: %d, op2 len: %d)",
     op, op1_len, op2_len);
}

void
gripe_nonconformant (const char *op,
                     octave_idx_type op1_nr, octave_idx_type op1_nc,
                     octave_idx_type op2_nr, octave_idx_type op2_nc)
{
  const char *err_id = error_id_nonconformant_args;

  (*current_liboctave_error_with_id_handler)
    (err_id, "%s: nonconformant arguments (op1 is %dx%d, op2 is %dx%d)",
     op, op1_nr, op1_nc, op2_nr, op2_nc);
}

void
gripe_nonconformant (const char *op, const dim_vector& op1_dims,
                     const dim_vector& op2_dims)
{
  const char *err_id = error_id_nonconformant_args;

  std::string op1_dims_str = op1_dims.str ();
  std::string op2_dims_str = op2_dims.str ();

  (*current_liboctave_error_with_id_handler)
    (err_id, "%s: nonconformant arguments (op1 is %s, op2 is %s)",
     op, op1_dims_str.c_str (), op2_dims_str.c_str ());
}

void
gripe_del_index_out_of_range (bool is1d, octave_idx_type idx,
                              octave_idx_type ext)
{
  const char *err_id = error_id_index_out_of_bounds;

  (*current_liboctave_error_with_id_handler)
    (err_id, "A(%s) = []: index out of bounds: value %d out of bound %d",
     is1d ? "I" : "..,I,..", idx, ext);
}

namespace octave
{
  class invalid_index : public index_exception
  {
  public:

    invalid_index (const std::string& value, octave_idx_type ndim,
                   octave_idx_type dimen)
      : index_exception (value, ndim, dimen)
    { }

    std::string details (void) const
    {
#if defined (OCTAVE_ENABLE_64)
      return "subscripts must be either integers 1 to (2^63)-1 or logicals";
#else
      return "subscripts must be either integers 1 to (2^31)-1 or logicals";
#endif
    }

    // ID of error to throw
    const char *err_id (void) const
    {
      return error_id_invalid_index;
    }
  };
}

// Complain if an index is negative, fractional, or too big.

void
gripe_invalid_index (const std::string& idx, octave_idx_type nd,
                     octave_idx_type dim, const std::string&)
{
  octave::invalid_index e (idx, nd, dim);

  throw e;
}

void
gripe_invalid_index (octave_idx_type n, octave_idx_type nd,
                     octave_idx_type dim, const std::string& var)
{
  std::ostringstream buf;
  buf << n + 1;

#if defined (HAVE_PRAGMA_GCC_DIAGNOSTIC)
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#endif

  gripe_invalid_index (buf.str (), nd, dim, var);

#if defined (HAVE_PRAGMA_GCC_DIAGNOSTIC)
#  pragma GCC diagnostic pop
#endif
}

void
gripe_invalid_index (double n, octave_idx_type nd, octave_idx_type dim,
                     const std::string& var)
{
  std::ostringstream buf;
  buf << n + 1;

#if defined (HAVE_PRAGMA_GCC_DIAGNOSTIC)
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#endif

  gripe_invalid_index (buf.str (), nd, dim, var);

#if defined (HAVE_PRAGMA_GCC_DIAGNOSTIC)
#  pragma GCC diagnostic pop
#endif
}

namespace octave
{
  // Gripe and exception for read access beyond the bounds of an array.

  class out_of_range : public index_exception
  {
  public:

    out_of_range (const std::string& value, octave_idx_type nd_in,
                  octave_idx_type dim_in)
      : index_exception (value, nd_in, dim_in), extent (0)
    { }

    std::string details (void) const
    {
      std::string expl;

      if (nd >= size.ndims ())   // if not an index slice
        {
          if (var != "")
            expl = "but " + var + " has size ";
          else
            expl = "but object has size ";

          expl = expl + size.str ('x');
        }
      else
        {
          std::ostringstream buf;
          buf << extent;
          expl = "out of bound " + buf.str ();
        }

      return expl;
    }

    // ID of error to throw.
    const char *err_id (void) const
    {
      return error_id_index_out_of_bounds;
    }

    void set_size (const dim_vector& size_in) { size = size_in; }

    void set_extent (octave_idx_type ext) { extent = ext; }

  private:

    // Dimension of object being accessed.
    dim_vector size;

    // Length of dimension being accessed.
    octave_idx_type extent;
  };
}

// Complain of an index that is out of range, but we don't know matrix size
void
gripe_index_out_of_range (int nd, int dim, octave_idx_type idx,
                          octave_idx_type ext)
{
  std::ostringstream buf;
  buf << idx;
  octave::out_of_range e (buf.str (), nd, dim);

  e.set_extent (ext);
  // ??? Make details method give extent not size.
  e.set_size (dim_vector (1, 1, 1, 1, 1, 1,1));

  throw e;
}

// Complain of an index that is out of range
void
gripe_index_out_of_range (int nd, int dim, octave_idx_type idx,
                          octave_idx_type ext, const dim_vector& d)
{
  std::ostringstream buf;
  buf << idx;
  octave::out_of_range e (buf.str (), nd, dim);

  e.set_extent (ext);
  e.set_size (d);

  throw e;
}

void
gripe_invalid_resize (void)
{
  (*current_liboctave_error_with_id_handler)
    ("Octave:invalid-resize",
     "Invalid resizing operation or ambiguous assignment to an out-of-bounds array element");
}

void
gripe_singular_matrix (double rcond)
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

/* Tests in test/index.tst */

