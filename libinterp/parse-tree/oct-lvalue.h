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

#if ! defined (octave_oct_lvalue_h)
#define octave_oct_lvalue_h 1

#include "octave-config.h"

#include <string>

#include "ovl.h"
#include "stack-frame.h"
#include "symrec.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class octave_lvalue
{
public:

  octave_lvalue (const symbol_record& sr,
                 const std::shared_ptr<stack_frame>& frame)
    : m_sym (sr), m_frame (frame), m_black_hole (false),
      m_type (), m_idx ()
  { }

  OCTAVE_DEFAULT_CONSTRUCT_COPY_MOVE_DELETE (octave_lvalue)

  bool is_black_hole () const { return m_black_hole; }

  void mark_black_hole () { m_black_hole = true; }

  bool is_defined () const;

  bool is_undefined () const;

  bool isstruct () const { return value().isstruct (); }

  void define (const octave_value& v);

  void assign (octave_value::assign_op, const octave_value&);

  octave_idx_type numel () const;

  void set_index (const std::string& t,
                  const std::list<octave_value_list>& i);

  void clear_index () { m_type = ""; m_idx.clear (); }

  std::string index_type () const { return m_type; }

  bool index_is_empty () const;

  bool index_is_colon () const;

  void unary_op (octave_value::unary_op op);

  octave_value value () const;

private:

  octave_value
  eval_for_numel (const std::string& type,
                  const std::list<octave_value_list>& idx) const;

  symbol_record m_sym;

  std::shared_ptr<stack_frame> m_frame;

  bool m_black_hole;

  std::string m_type;

  std::list<octave_value_list> m_idx;
};

OCTAVE_END_NAMESPACE(octave)

#endif
