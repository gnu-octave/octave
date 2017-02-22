/*

Copyright (C) 1996-2017 John W. Eaton

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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "errwarn.h"
#include "ovl.h"
#include "ov.h"
#include "ov-ch-mat.h"
#include "ov-scalar.h"
#include "ov-re-mat.h"
#include "ov-bool.h"
#include "ov-bool-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"

// char matrix unary ops.

DEFUNOP (transpose, char_matrix)
{
  const octave_char_matrix& v = dynamic_cast<const octave_char_matrix&> (a);

  return octave_value (v.matrix_value ().transpose ());
}

DEFNDCATOP_FN (chm_chm, char_matrix, char_matrix, char_array, char_array,
               concat)

DEFCATOP (chm_s, char_matrix, scalar)
{
  octave_char_matrix& v1 = dynamic_cast<octave_char_matrix&> (a1);
  const octave_scalar& v2 = dynamic_cast<const octave_scalar&> (a2);

  warn_implicit_conversion ("Octave:num-to-str",
                            v2.type_name (), v1.type_name ());

  return octave_value (v1.char_array_value (). concat (v2.array_value (),
                       ra_idx));
}

DEFCATOP (chm_m, char_matrix, matrix)
{
  octave_char_matrix& v1 = dynamic_cast<octave_char_matrix&> (a1);
  const octave_matrix& v2 = dynamic_cast<const octave_matrix&> (a2);

  warn_implicit_conversion ("Octave:num-to-str",
                            v2.type_name (), v1.type_name ());

  return octave_value (v1.char_array_value (). concat (v2.array_value (),
                       ra_idx));
}

DEFCATOP (s_chm, scalar, char_matrix)
{
  octave_scalar& v1 = dynamic_cast<octave_scalar&> (a1);
  const octave_char_matrix& v2 = dynamic_cast<const octave_char_matrix&> (a2);

  warn_implicit_conversion ("Octave:num-to-str",
                            v1.type_name (), v2.type_name ());

  return octave_value (v1.array_value (). concat (v2.char_array_value (),
                       ra_idx));
}

DEFCATOP (m_chm, matrix, char_matrix)
{
  octave_matrix& v1 = dynamic_cast<octave_matrix&> (a1);
  const octave_char_matrix& v2 = dynamic_cast<const octave_char_matrix&> (a2);

  warn_implicit_conversion ("Octave:num-to-str",
                            v1.type_name (), v2.type_name ());

  return octave_value (v1.array_value (). concat (v2.char_array_value (),
                       ra_idx));
}

void
install_chm_ops (void)
{
  INSTALL_UNOP (op_transpose, octave_char_matrix, transpose);
  INSTALL_UNOP (op_hermitian, octave_char_matrix, transpose);

  INSTALL_CATOP (octave_char_matrix, octave_char_matrix, chm_chm);
  INSTALL_CATOP (octave_char_matrix, octave_scalar, chm_s);
  INSTALL_CATOP (octave_char_matrix, octave_matrix, chm_m);
  INSTALL_CATOP (octave_scalar, octave_char_matrix, s_chm);
  INSTALL_CATOP (octave_matrix, octave_char_matrix, m_chm);
}
