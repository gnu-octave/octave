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

#if ! defined (octave_lo_array_errwarn_h)
#define octave_lo_array_errwarn_h 1

#include "octave-config.h"

#include "dim-vector.h"
#include "quit.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Exception thrown by err_invalid_index
// This is thrown when the invalid index is detected, at which point nd and dim
// are usually not known.  It is caught at the place they are known, where a
// new err_invalid_index  is called.
//
// Typically, this should be caught after any call to
// octave_value_list::index_vector()

class OCTAVE_EXCEPTION_API index_exception : public execution_exception
{
public:

  index_exception (const std::string& index, octave_idx_type nd = 0,
                   octave_idx_type dim = -1, const char *var = "")
    : m_index (index), m_nd (nd), m_dim (dim), m_var (var)
  {
    set_message (expression ());
  }

  ~index_exception (void) = default;

  // ID of error to throw.
  virtual const char * err_id (void) const = 0;

  // By default, update message to show the erroneous index expression.
  virtual void update_message (void) { set_message (expression ()); }

  // Position of error: dimension in error, and number of dimensions.
  void set_pos (octave_idx_type nd_arg, octave_idx_type dim_arg)
  {
    m_nd = nd_arg;
    m_dim = dim_arg;

    update_message ();
  }

  void set_pos_if_unset (octave_idx_type nd_arg, octave_idx_type dim_arg)
  {
    if (m_nd == 0)
      {
        m_nd  = nd_arg;
        m_dim = dim_arg;

        update_message ();
      }
  }

  // Name of variable being indexed.  eye(2)(1,1) gives "<unknown>".
  void set_var (const std::string& var_arg = "")
  {
    m_var = var_arg;

    update_message ();
  }

private:

  // Value of invalid index.
  std::string m_index;

protected:

  // Show what's wrong, e.g.,  A(-1,_), A(0+1i).
  OCTAVE_API std::string expression (void) const;

  // Number of dimensions of indexed object.
  octave_idx_type m_nd;

  // Dimension number in which invalid index occurred.
  octave_idx_type m_dim;

  // Name of variable being indexed.
  std::string m_var;
};

OCTAVE_NORETURN extern OCTAVE_API void
err_nan_to_logical_conversion (void);

OCTAVE_NORETURN extern OCTAVE_API void
err_nan_to_character_conversion (void);

OCTAVE_NORETURN extern OCTAVE_API void
err_nonconformant (const char *op, octave_idx_type op1_len,
                   octave_idx_type op2_len);

OCTAVE_NORETURN extern OCTAVE_API void
err_nonconformant (const char *op,
                   octave_idx_type op1_nr, octave_idx_type op1_nc,
                   octave_idx_type op2_nr, octave_idx_type op2_nc);

OCTAVE_NORETURN extern OCTAVE_API void
err_nonconformant (const char *op,
                   const dim_vector& op1_dims, const dim_vector& op2_dims);

OCTAVE_NORETURN extern OCTAVE_API void
err_index_out_of_range (int ndims, int dim, octave_idx_type idx,
                        octave_idx_type ext, const dim_vector& dv);

OCTAVE_NORETURN extern OCTAVE_API void
err_del_index_out_of_range (bool is1d, octave_idx_type iext,
                            octave_idx_type ext);

OCTAVE_NORETURN extern OCTAVE_API void
err_invalid_index (double n, octave_idx_type nd = 0,
                   octave_idx_type dim = 0,
                   const std::string& var = "");

OCTAVE_NORETURN extern OCTAVE_API void
err_invalid_index (octave_idx_type n, octave_idx_type nd = 0,
                   octave_idx_type dim = 0,
                   const std::string& var = "");

OCTAVE_NORETURN extern OCTAVE_API void
err_invalid_index (const std::string& idx, octave_idx_type nd = 0,
                   octave_idx_type dim = 0,
                   const std::string& var = "");

OCTAVE_NORETURN extern OCTAVE_API void
err_invalid_resize (void);

extern OCTAVE_API void
warn_singular_matrix (double rcond = 0.0);

OCTAVE_END_NAMESPACE(octave)

#endif
