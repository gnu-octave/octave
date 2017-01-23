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

#if ! defined (octave_lo_array_errwarn_h)
#define octave_lo_array_errwarn_h 1

#include "octave-config.h"

#include "dim-vector.h"
#include "quit.h"

namespace octave
{
  // Exception thrown by err_invalid_index
  // This is thrown when the invalid index is detected, at which point nd and dim
  // are usually not known.  It is caught at the place they are known, where a
  // new err_invalid_index  is called.
  //
  // Typically, this should be caught after any call to
  // octave_value_list::index_vector()

  class index_exception : public execution_exception
  {
  public:

    index_exception (const std::string& index_arg, octave_idx_type nd_arg = 0,
                     octave_idx_type dim_arg = -1, const char *var_arg = "")
      : index (index_arg), nd (nd_arg), dim (dim_arg), var (var_arg)
    { }

    ~index_exception (void) = default;

    // Erroneous index value.  Called in what, and by external code
    // (e.g., nth_element) to make a custom error message.
    std::string idx (void) const { return index; }

    // details set by subclass.
    virtual std::string details (void) const = 0;

    // ID of error to throw.
    virtual const char *err_id (void) const = 0;

    virtual std::string message (void) const;

    // Position of error: dimension in error, and number of dimensions.
    void set_pos (octave_idx_type nd_arg, octave_idx_type dim_arg)
    {
      nd = nd_arg;
      dim = dim_arg;
    }

    void set_pos_if_unset (octave_idx_type nd_arg, octave_idx_type dim_arg)
    {
      if (nd == 0)
        {
          nd  = nd_arg;
          dim = dim_arg;
        }
    }

    // Name of variable being indexed.  eye(2)(1,1) gives "<unknown>".
    void set_var (const std::string& var_arg = "")
    {
      var = var_arg;
    }

  private:

    // Value of invalid index.
    std::string index;

  protected:

    // Show what's wrong, e.g.,  A(-1,_), A(0+1i).
    std::string expression (void) const;

    // Number of dimensions of indexed object.
    octave_idx_type nd;

    // Dimension number in which invalid index occurred.
    octave_idx_type dim;

    // Name of variable being indexed.
    std::string var;

  };

  OCTAVE_NORETURN OCTAVE_API extern void
  err_nan_to_logical_conversion (void);

  OCTAVE_NORETURN OCTAVE_API extern void
  err_nan_to_character_conversion (void);

  OCTAVE_NORETURN OCTAVE_API extern void
  err_nonconformant (const char *op, octave_idx_type op1_len,
                     octave_idx_type op2_len);

  OCTAVE_NORETURN OCTAVE_API extern void
  err_nonconformant (const char *op,
                     octave_idx_type op1_nr, octave_idx_type op1_nc,
                     octave_idx_type op2_nr, octave_idx_type op2_nc);

  OCTAVE_NORETURN OCTAVE_API extern void
  err_nonconformant (const char *op,
                     const dim_vector& op1_dims, const dim_vector& op2_dims);

  OCTAVE_NORETURN OCTAVE_API extern void
  err_index_out_of_range (int nd, int dim, octave_idx_type iext,
                          octave_idx_type ext, const dim_vector& d);

  OCTAVE_NORETURN OCTAVE_API extern void
  err_index_out_of_range (int nd, int dim, octave_idx_type iext,
                          octave_idx_type ext);

  OCTAVE_NORETURN OCTAVE_API extern void
  err_del_index_out_of_range (bool is1d, octave_idx_type iext,
                              octave_idx_type ext);

  OCTAVE_NORETURN OCTAVE_API extern void
  err_invalid_index (double n, octave_idx_type nd = 0,
                     octave_idx_type dim = 0,
                     const std::string& var = "");

  OCTAVE_NORETURN OCTAVE_API extern void
  err_invalid_index (octave_idx_type n, octave_idx_type nd = 0,
                     octave_idx_type dim = 0,
                     const std::string& var = "");

  OCTAVE_NORETURN OCTAVE_API extern void
  err_invalid_index (const std::string& idx, octave_idx_type nd = 0,
                     octave_idx_type dim = 0,
                     const std::string& var = "");

  OCTAVE_NORETURN OCTAVE_API extern void
  err_invalid_resize (void);

  OCTAVE_API extern void
  warn_singular_matrix (double rcond = 0.0);
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED ("use 'octave::err_nan_to_logical_conversion' instead")
OCTAVE_NORETURN inline void
err_nan_to_logical_conversion (void)
{
  octave::err_nan_to_logical_conversion ();
}

OCTAVE_DEPRECATED ("use 'octave::err_nan_to_character_conversion' instead")
OCTAVE_NORETURN inline void
err_nan_to_character_conversion (void)
{
  octave::err_nan_to_character_conversion ();
}

OCTAVE_DEPRECATED ("use 'octave::err_nonconformant' instead")
OCTAVE_NORETURN inline void
err_nonconformant (const char *op, octave_idx_type op1_len,
                   octave_idx_type op2_len)
{
  octave::err_nonconformant (op, op1_len, op2_len);
}

OCTAVE_DEPRECATED ("use 'octave::err_nonconformant' instead")
OCTAVE_NORETURN inline void
err_nonconformant (const char *op,
                   octave_idx_type op1_nr, octave_idx_type op1_nc,
                   octave_idx_type op2_nr, octave_idx_type op2_nc)
{
  octave::err_nonconformant (op, op1_nr, op1_nc, op2_nr, op2_nc);
}

OCTAVE_DEPRECATED ("use 'octave::err_nonconformant' instead")
OCTAVE_NORETURN inline void
err_nonconformant (const char *op,
                   const dim_vector& op1_dims, const dim_vector& op2_dims)
{
  octave::err_nonconformant (op, op1_dims, op2_dims);
}

OCTAVE_DEPRECATED ("use 'octave::err_index_out_of_range' instead")
OCTAVE_NORETURN inline void
err_index_out_of_range (int nd, int dim, octave_idx_type iext,
                        octave_idx_type ext, const dim_vector& d)
{
  octave::err_index_out_of_range (nd, dim, iext, ext, d);
}

OCTAVE_DEPRECATED ("use 'octave::err_index_out_of_range' instead")
OCTAVE_NORETURN inline void
err_index_out_of_range (int nd, int dim, octave_idx_type iext,
                        octave_idx_type ext)
{
  octave::err_index_out_of_range (nd, dim, iext, ext);
}

OCTAVE_DEPRECATED ("use 'octave::err_del_index_out_of_range' instead")
OCTAVE_NORETURN inline void
err_del_index_out_of_range (bool is1d, octave_idx_type iext,
                            octave_idx_type ext)
{
  octave::err_del_index_out_of_range (is1d, iext, ext);
}

OCTAVE_NORETURN inline void
err_invalid_index (double n, octave_idx_type nd = 0,
                   octave_idx_type dim = 0,
                   const std::string& var = "")
{
  octave::err_invalid_index (n, nd, dim, var);
}

OCTAVE_DEPRECATED ("use 'octave::err_invalid_index' instead")
OCTAVE_NORETURN inline void
err_invalid_index (octave_idx_type n, octave_idx_type nd = 0,
                   octave_idx_type dim = 0,
                   const std::string& var = "")
{
  octave::err_invalid_index (n, nd, dim, var);
}

OCTAVE_DEPRECATED ("use 'octave::err_invalid_index' instead")
OCTAVE_NORETURN inline void
err_invalid_index (const std::string& idx, octave_idx_type nd = 0,
                   octave_idx_type dim = 0,
                   const std::string& var = "")
{
  octave::err_invalid_index (idx, nd, dim, var);
}

OCTAVE_DEPRECATED ("use 'octave::err_invalid_resize' instead")
OCTAVE_NORETURN inline void
err_invalid_resize (void)
{
  octave::err_invalid_resize ();
}

OCTAVE_DEPRECATED ("use 'octave::warn_singular_matrix' instead")
inline void
warn_singular_matrix (double rcond = 0.0)
{
  return octave::warn_singular_matrix (rcond);
}

#endif

#endif
