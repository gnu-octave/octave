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

#include "error.h"
#include "errwarn.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "ovl.h"
#include "oct-lvalue.h"
#include "ov.h"

OCTAVE_BEGIN_NAMESPACE(octave)

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

octave_idx_type octave_lvalue::numel (void) const
{
  // Return 1 if there is no index because without an index there
  // should be no way to have a cs-list here.  Cs-lists may be passed
  // around internally but they are not supposed to be stored as
  // single symbols in a stack frame.

  std::size_t num_indices = m_idx.size ();

  if (num_indices == 0)
    return 1;

  switch (m_type[num_indices-1])
    {
    case '(':
      return 1;

    case '{':
      {
        // FIXME: Duplicate code in '.' case below...

        // Evaluate, skipping the last index.

        std::string tmp_type = m_type;
        std::list<octave_value_list> tmp_idx = m_idx;

        tmp_type.pop_back ();
        tmp_idx.pop_back ();

        octave_value tmp = eval_for_numel (tmp_type, tmp_idx);

        octave_value_list tidx = m_idx.back ();

        if (tmp.is_undefined ())
          {
            if (tidx.has_magic_colon ())
              err_invalid_inquiry_subscript ();

            tmp = Cell ();
          }
        else if (tmp.is_zero_by_zero ()
                 && (tmp.is_matrix_type () || tmp.is_string ()))
          {
            tmp = Cell ();
          }

        return tmp.xnumel (tidx);
      }
      break;

    case '.':
      {
        // Evaluate, skipping either the last index or the last two
        // indices if we are looking at "(idx).field".

        std::string tmp_type = m_type;
        std::list<octave_value_list> tmp_idx = m_idx;

        tmp_type.pop_back ();
        tmp_idx.pop_back ();

        bool paren_dot = num_indices > 1 && m_type[num_indices-2] == '(';

        // Index for paren operator, if any.
        octave_value_list pidx;

        if (paren_dot)
          {
            pidx = tmp_idx.back ();

            tmp_type.pop_back ();
            tmp_idx.pop_back ();
          }

        octave_value tmp = eval_for_numel (tmp_type, tmp_idx);

        bool autoconv = (tmp.is_zero_by_zero ()
                         && (tmp.is_matrix_type () || tmp.is_string ()
                             || tmp.iscell ()));

        if (paren_dot)
          {
            // Use octave_map, not octave_scalar_map so that the
            // dimensions are 0x0, not 1x1.

            if (tmp.is_undefined ())
              {
                if (pidx.has_magic_colon ())
                  err_invalid_inquiry_subscript ();

                tmp = octave_map ();
              }
            else if (autoconv)
              tmp = octave_map ();

            return tmp.xnumel (pidx);
          }
        else if (tmp.is_undefined () || autoconv)
          return 1;
        else
          return tmp.xnumel (octave_value_list ());
      }
      break;

    default:
      panic_impossible ();
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

void octave_lvalue::unary_op (octave_value::unary_op op)
{
  if (! is_black_hole ())
    m_frame->non_const_unary_op (op, m_sym, m_type, m_idx);
}

octave_value octave_lvalue::value (void) const
{
  return (is_black_hole ()
          ? octave_value () : m_frame->value (m_sym, m_type, m_idx));
}

octave_value
octave_lvalue::eval_for_numel (const std::string& type,
                               const std::list<octave_value_list>& idx) const
{
  octave_value retval;

  try
    {
      retval = m_frame->varval (m_sym);

      if (retval.is_constant () && ! idx.empty ())
        retval = retval.subsref (type, idx);
    }
  catch (const execution_exception&)
    {
      // Ignore an error and treat it as undefined.  The error
      // could happen because there is an index is out of range
      // and we will be resizing a cell array.

      interpreter& interp = __get_interpreter__ ();

      interp.recover_from_exception ();

      retval = octave_value ();
    }

  return retval;
}

OCTAVE_END_NAMESPACE(octave)
