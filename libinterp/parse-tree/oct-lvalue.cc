////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2020 The Octave Project Developers
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

#include "error.h"
#include "ovl.h"
#include "oct-lvalue.h"
#include "ov.h"

namespace octave
{
  bool octave_lvalue::is_defined (void) const
  {
    return ! is_black_hole () && m_frame->is_defined (m_sym);
  }

  bool octave_lvalue::is_undefined (void) const
  {
    return ! is_defined ();
  }

  void octave_lvalue::define (const octave_value& v)
  {
    m_frame->assign (m_sym, v);
  }

  void octave_lvalue::assign (octave_value::assign_op op,
                              const octave_value& rhs)
  {
    if (! is_black_hole ())
      m_frame->assign (op, m_sym, m_type, m_idx, rhs);
  }

  void octave_lvalue::set_index (const std::string& t,
                                 const std::list<octave_value_list>& i)
  {
    if (! m_idx.empty ())
      error ("invalid index expression in assignment");

    m_type = t;
    m_idx = i;
  }

  bool octave_lvalue::index_is_empty (void) const
  {
    bool retval = false;

    if (m_idx.size () == 1)
      {
        octave_value_list tmp = m_idx.front ();

        retval = (tmp.length () == 1 && tmp(0).isempty ());
      }

    return retval;
  }

  bool octave_lvalue::index_is_colon (void) const
  {
    bool retval = false;

    if (m_idx.size () == 1)
      {
        octave_value_list tmp = m_idx.front ();

        retval = (tmp.length () == 1 && tmp(0).is_magic_colon ());
      }

    return retval;
  }

  void octave_lvalue::do_unary_op (octave_value::unary_op op)
  {
    if (! is_black_hole ())
      m_frame->do_non_const_unary_op (op, m_sym, m_type, m_idx);
  }

  octave_value octave_lvalue::value (void) const
  {
    return (is_black_hole ()
            ? octave_value () : m_frame->value (m_sym, m_type, m_idx));
  }
}
