/*

Copyright (C) 2000-2015 John W. Eaton

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

#if !defined (octave_lo_array_gripes_h)
#define octave_lo_array_gripes_h 1

#include "dim-vector.h"
#include "quit.h"

// Exception thrown by gripe_invalid_index
// This is thrown when the invalid index is detected, at which point nd and dim
// are usually not known.  It is caught at the place they are known, where a
// new  gripe_invalid_index  is called.
//
// Typically, this should be caught after any call to
// octave_value_list::index_vector()
class index_exception : public octave_execution_exception
{
public:

  index_exception (const char *index_in, octave_idx_type nd_in = 0,
                   octave_idx_type dim_in = 0, const char *var_in = "")
    : index (index_in), nd (nd_in), dim (dim_in), var (var_in)
  { }

  ~index_exception (void) throw () { }

  // Erroneous index value.  Called in what, and by external code
  // (e.g., nth_element) to make a custom error message.
  const char *idx (void) const { return index.c_str (); }

  // details set by subclass.
  virtual const char* explain (void) const = 0;

  // ID of error to throw.
  virtual const char* id (void) const = 0;

  virtual const char* err (void) throw ();

  // Position of error: dimension in error, and number of dimensions.
  void set_pos (octave_idx_type nd_in, octave_idx_type dim_in)
  {
    nd = nd_in;
    dim = dim_in;
  }

  void set_pos_if_unset (octave_idx_type nd_in, octave_idx_type dim_in)
  {
    if (nd == 0)
      {
        nd  = nd_in;
        dim = dim_in;
      }
  }

  // Name of variable being indexed.  eye(2)(1,1) gives "<unknown>".
  void set_var (std::string var_in) { var = var_in; }

private:

  // Value of invalid index.
  std::string index;

  // Formatted message returned by what(), (not on stack).
  std::string msg;      

protected:

  // Show what's wrong, e.g.,  A(-1,_), A(0+1i).
  std::string access (void) const;

  // Number of dimensions of indexed object.
  octave_idx_type nd;

  // Dimension number in which invalid index occurred.
  octave_idx_type dim;

  // Name of variable being indexed.
  std::string var;

};

extern OCTAVE_API const char *error_id_nonconformant_args;

extern OCTAVE_API const char *error_id_index_out_of_bounds;

extern OCTAVE_API const char *error_id_invalid_index;

extern OCTAVE_API const char *warning_id_nearly_singular_matrix;

extern OCTAVE_API const char *warning_id_singular_matrix;

extern void OCTAVE_API
gripe_nan_to_logical_conversion (void);

extern void OCTAVE_API
gripe_nan_to_character_conversion (void);

extern void OCTAVE_API
gripe_nonconformant (const char *op,
                     octave_idx_type op1_len, octave_idx_type op2_len);

extern void OCTAVE_API
gripe_nonconformant (const char *op,
                     octave_idx_type op1_nr, octave_idx_type op1_nc,
                     octave_idx_type op2_nr, octave_idx_type op2_nc);


extern void OCTAVE_API
gripe_nonconformant (const char *op, const dim_vector& op1_dims,
                     const dim_vector& op2_dims);

extern void OCTAVE_API
gripe_index_out_of_range (int nd, int dim,
                          octave_idx_type iext, octave_idx_type ext,
                          const dim_vector& d);

extern void OCTAVE_API
gripe_index_out_of_range (int nd, int dim,
                          octave_idx_type iext, octave_idx_type ext);

extern void OCTAVE_API
gripe_del_index_out_of_range (bool is1d, octave_idx_type iext,
                              octave_idx_type ext);

extern void OCTAVE_API
gripe_invalid_index (double, octave_idx_type nd=0,
                     octave_idx_type dim=0, const char *var = NULL);

extern void OCTAVE_API
gripe_invalid_index (octave_idx_type n, octave_idx_type nd=0,
                     octave_idx_type dim=0, const char *var = NULL);

extern void OCTAVE_API
gripe_invalid_index (const char *idx, octave_idx_type nd=0,
                     octave_idx_type dim=0, const char *var = NULL);

extern void OCTAVE_API
gripe_invalid_resize (void);

extern void OCTAVE_API
gripe_singular_matrix (double rcond = 0.0);

#endif
