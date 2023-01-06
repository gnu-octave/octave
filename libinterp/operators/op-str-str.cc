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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "errwarn.h"
#include "ovl.h"
#include "ov.h"
#include "ov-str-mat.h"
#include "ov-typeinfo.h"
#include "ov-null-mat.h"
#include "ops.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// string unary ops.

DEFUNOP (transpose, char_matrix_str)
{
  const octave_char_matrix_str& v
    = dynamic_cast<const octave_char_matrix_str&> (a);

  if (v.ndims () > 2)
    error ("transpose not defined for N-D objects");

  return octave_value (v.char_matrix_value ().transpose (),
                       a.is_sq_string () ? '\'' : '"');
}

// string by string ops.

#define DEFCHARNDBINOP_FN(name, op, t1, t2, e1, e2, f)                  \
  static octave_value                                                   \
  CONCAT2(oct_binop_, name) (const octave_base_value& a1,               \
                             const octave_base_value& a2)               \
  {                                                                     \
    dim_vector a1_dims = a1.dims ();                                    \
    dim_vector a2_dims = a2.dims ();                                    \
                                                                        \
    bool a1_is_scalar = a1_dims.all_ones ();                            \
    bool a2_is_scalar = a2_dims.all_ones ();                            \
                                                                        \
    const octave_ ## t1& v1 = dynamic_cast<const octave_ ## t1&> (a1);  \
    const octave_ ## t2& v2 = dynamic_cast<const octave_ ## t2&> (a2);  \
                                                                        \
    if (a1_is_scalar)                                                   \
      {                                                                 \
        if (a2_is_scalar)                                               \
          return octave_value ((v1.e1 ## _value ())(0)                  \
                               op (v2.e2 ## _value ())(0));             \
        else                                                            \
          return octave_value (f ((v1.e1 ## _value ())(0),              \
                                  v2.e2 ## _value ()));                 \
      }                                                                 \
    else                                                                \
      {                                                                 \
        if (a2_is_scalar)                                               \
          return octave_value (f (v1.e1 ## _value (),                   \
                                  (v2.e2 ## _value ())(0)));            \
        else                                                            \
          return octave_value (f (v1.e1 ## _value (),                   \
                                  v2.e2 ## _value ()));                 \
      }                                                                 \
  }

DEFCHARNDBINOP_FN (lt, <, char_matrix_str, char_matrix_str, char_array,
                   char_array, mx_el_lt)
DEFCHARNDBINOP_FN (le, <=, char_matrix_str, char_matrix_str, char_array,
                   char_array, mx_el_le)
DEFCHARNDBINOP_FN (eq, ==, char_matrix_str, char_matrix_str, char_array,
                   char_array, mx_el_eq)
DEFCHARNDBINOP_FN (ge, >=, char_matrix_str, char_matrix_str, char_array,
                   char_array, mx_el_ge)
DEFCHARNDBINOP_FN (gt, >, char_matrix_str, char_matrix_str, char_array,
                   char_array, mx_el_gt)
DEFCHARNDBINOP_FN (ne, !=, char_matrix_str, char_matrix_str, char_array,
                   char_array, mx_el_ne)

DEFASSIGNOP (assign, char_matrix_str, char_matrix_str)
{
  octave_char_matrix_str& v1 = dynamic_cast<octave_char_matrix_str&> (a1);
  const octave_char_matrix_str& v2
    = dynamic_cast<const octave_char_matrix_str&> (a2);

  v1.assign (idx, v2.char_array_value ());
  return octave_value ();
}

DEFNULLASSIGNOP_FN (null_assign, char_matrix_str, delete_elements)

DEFNDCHARCATOP_FN (str_str, char_matrix_str, char_matrix_str, concat)

void
install_str_str_ops (octave::type_info& ti)
{
  INSTALL_UNOP_TI (ti, op_transpose, octave_char_matrix_str, transpose);
  INSTALL_UNOP_TI (ti, op_transpose, octave_char_matrix_sq_str, transpose);

  INSTALL_UNOP_TI (ti, op_hermitian, octave_char_matrix_str, transpose);
  INSTALL_UNOP_TI (ti, op_hermitian, octave_char_matrix_sq_str, transpose);

  INSTALL_BINOP_TI (ti, op_lt, octave_char_matrix_str, octave_char_matrix_str,
                    lt);
  INSTALL_BINOP_TI (ti, op_lt, octave_char_matrix_str, octave_char_matrix_sq_str,
                    lt);
  INSTALL_BINOP_TI (ti, op_lt, octave_char_matrix_sq_str, octave_char_matrix_str,
                    lt);
  INSTALL_BINOP_TI (ti, op_lt, octave_char_matrix_sq_str,
                    octave_char_matrix_sq_str,
                    lt);

  INSTALL_BINOP_TI (ti, op_le, octave_char_matrix_str, octave_char_matrix_str,
                    le);
  INSTALL_BINOP_TI (ti, op_le, octave_char_matrix_str, octave_char_matrix_sq_str,
                    le);
  INSTALL_BINOP_TI (ti, op_le, octave_char_matrix_sq_str, octave_char_matrix_str,
                    le);
  INSTALL_BINOP_TI (ti, op_le, octave_char_matrix_sq_str,
                    octave_char_matrix_sq_str,
                    le);

  INSTALL_BINOP_TI (ti, op_eq, octave_char_matrix_str, octave_char_matrix_str,
                    eq);
  INSTALL_BINOP_TI (ti, op_eq, octave_char_matrix_str, octave_char_matrix_sq_str,
                    eq);
  INSTALL_BINOP_TI (ti, op_eq, octave_char_matrix_sq_str, octave_char_matrix_str,
                    eq);
  INSTALL_BINOP_TI (ti, op_eq, octave_char_matrix_sq_str,
                    octave_char_matrix_sq_str,
                    eq);

  INSTALL_BINOP_TI (ti, op_ge, octave_char_matrix_str, octave_char_matrix_str,
                    ge);
  INSTALL_BINOP_TI (ti, op_ge, octave_char_matrix_str, octave_char_matrix_sq_str,
                    ge);
  INSTALL_BINOP_TI (ti, op_ge, octave_char_matrix_sq_str, octave_char_matrix_str,
                    ge);
  INSTALL_BINOP_TI (ti, op_ge, octave_char_matrix_sq_str,
                    octave_char_matrix_sq_str,
                    ge);

  INSTALL_BINOP_TI (ti, op_gt, octave_char_matrix_str, octave_char_matrix_str,
                    gt);
  INSTALL_BINOP_TI (ti, op_gt, octave_char_matrix_str, octave_char_matrix_sq_str,
                    gt);
  INSTALL_BINOP_TI (ti, op_gt, octave_char_matrix_sq_str, octave_char_matrix_str,
                    gt);
  INSTALL_BINOP_TI (ti, op_gt, octave_char_matrix_sq_str,
                    octave_char_matrix_sq_str,
                    gt);

  INSTALL_BINOP_TI (ti, op_ne, octave_char_matrix_str, octave_char_matrix_str,
                    ne);
  INSTALL_BINOP_TI (ti, op_ne, octave_char_matrix_str, octave_char_matrix_sq_str,
                    ne);
  INSTALL_BINOP_TI (ti, op_ne, octave_char_matrix_sq_str, octave_char_matrix_str,
                    ne);
  INSTALL_BINOP_TI (ti, op_ne, octave_char_matrix_sq_str,
                    octave_char_matrix_sq_str,
                    ne);

  INSTALL_CATOP_TI (ti, octave_char_matrix_str, octave_char_matrix_str, str_str);
  INSTALL_CATOP_TI (ti, octave_char_matrix_str, octave_char_matrix_sq_str,
                    str_str);
  INSTALL_CATOP_TI (ti, octave_char_matrix_sq_str, octave_char_matrix_str,
                    str_str);
  INSTALL_CATOP_TI (ti, octave_char_matrix_sq_str, octave_char_matrix_sq_str,
                    str_str);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_char_matrix_str,
                       octave_char_matrix_str,
                       assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_char_matrix_str,
                       octave_char_matrix_sq_str,
                       assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_char_matrix_sq_str,
                       octave_char_matrix_str,
                       assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_char_matrix_sq_str,
                       octave_char_matrix_sq_str, assign);

  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_char_matrix_str, octave_null_matrix,
                       null_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_char_matrix_str, octave_null_str,
                       null_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_char_matrix_str, octave_null_sq_str,
                       null_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_char_matrix_sq_str,
                       octave_null_matrix,
                       null_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_char_matrix_sq_str, octave_null_str,
                       null_assign);
  INSTALL_ASSIGNOP_TI (ti, op_asn_eq, octave_char_matrix_sq_str,
                       octave_null_sq_str,
                       null_assign);

}

OCTAVE_END_NAMESPACE(octave)
