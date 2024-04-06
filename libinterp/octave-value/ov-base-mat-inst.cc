////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2024 The Octave Project Developers
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

#include "ov-base-mat.cc"

// instantiate template class with types that need to be exported from library

template class OCTINTERP_API octave_base_matrix<boolNDArray>;
template class OCTINTERP_API octave_base_matrix<charNDArray>;
template class OCTINTERP_API octave_base_matrix<int8NDArray>;
template class OCTINTERP_API octave_base_matrix<int16NDArray>;
template class OCTINTERP_API octave_base_matrix<int32NDArray>;
template class OCTINTERP_API octave_base_matrix<int64NDArray>;
template class OCTINTERP_API octave_base_matrix<uint8NDArray>;
template class OCTINTERP_API octave_base_matrix<uint16NDArray>;
template class OCTINTERP_API octave_base_matrix<uint32NDArray>;
template class OCTINTERP_API octave_base_matrix<uint64NDArray>;
template class OCTINTERP_API octave_base_matrix<ComplexNDArray>;
template class OCTINTERP_API octave_base_matrix<FloatComplexNDArray>;
template class OCTINTERP_API octave_base_matrix<FloatNDArray>;
template class OCTINTERP_API octave_base_matrix<NDArray>;


// Cell is able to handle octave_value indexing by itself, so just forward
// everything.

template <>
octave_value
octave_base_matrix<Cell>::do_index_op (const octave_value_list& idx,
                                       bool resize_ok)
{
  return m_matrix.index (idx, resize_ok);
}

template <>
void
octave_base_matrix<Cell>::assign (const octave_value_list& idx, const Cell& rhs)
{
  m_matrix.assign (idx, rhs);
}

template <>
void
octave_base_matrix<Cell>::assign (const octave_value_list& idx,
                                  octave_value rhs)
{
  // FIXME: Really?
  if (rhs.iscell ())
    m_matrix.assign (idx, rhs.cell_value ());
  else
    m_matrix.assign (idx, Cell (rhs));
}

template <>
void
octave_base_matrix<Cell>::delete_elements (const octave_value_list& idx)
{
  m_matrix.delete_elements (idx);
}

// FIXME: this list of specializations is becoming so long that we should
// really ask whether octave_cell should inherit from octave_base_matrix at all.

template <>
std::string
octave_base_matrix<Cell>::edit_display (const float_display_format&,
                                        octave_idx_type i,
                                        octave_idx_type j) const
{
  octave_value val = m_matrix(i, j);

  std::string tname = val.type_name ();
  dim_vector dv = val.dims ();
  std::string dimstr = dv.str ();
  return "[" + dimstr + " " + tname + "]";
}

template <>
octave_value
octave_base_matrix<Cell>::fast_elem_extract (octave_idx_type n) const
{
  if (n < m_matrix.numel ())
    return Cell (m_matrix(n));
  else
    return octave_value ();
}

template <>
bool
octave_base_matrix<Cell>::fast_elem_insert (octave_idx_type n,
                                            const octave_value& x)
{
  const octave_base_matrix<Cell> *xrep
    = dynamic_cast<const octave_base_matrix<Cell> *> (&x.get_rep ());

  bool retval = xrep && xrep->m_matrix.numel () == 1 && n < m_matrix.numel ();
  if (retval)
    m_matrix(n) = xrep->m_matrix(0);

  return retval;
}

template class OCTINTERP_API octave_base_matrix<Cell>;
