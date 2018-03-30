/*

Copyright (C) 1996-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "error.h"
#include "ovl.h"
#include "oct-lvalue.h"
#include "ov.h"

namespace octave
{
  void octave_lvalue::assign (octave_value::assign_op op,
                              const octave_value& rhs)
  {
    if (! is_black_hole ())
      {
        if (m_idx.empty ())
          m_sym.assign (op, rhs, m_context);
        else
          m_sym.assign (op, m_type, m_idx, rhs, m_context);
      }
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

  void octave_lvalue::do_unary_op (octave_value::unary_op op)
  {
    if (! is_black_hole ())
      {
        if (m_idx.empty ())
          m_sym.do_non_const_unary_op (op, m_context);
        else
          m_sym.do_non_const_unary_op (op, m_type, m_idx, m_context);
      }
  }

  octave_value octave_lvalue::value (void) const
  {
    octave_value retval;

    if (! is_black_hole ())
      {
        octave_value val = m_sym.varval (m_context);

        if (m_idx.empty ())
          retval = val;
        else
          {
            if (val.is_constant ())
              retval = val.subsref (m_type, m_idx);
            else
              {
                octave_value_list t = val.subsref (m_type, m_idx, 1);
                if (t.length () > 0)
                  retval = t(0);
              }
          }
      }

    return retval;
  }
}
