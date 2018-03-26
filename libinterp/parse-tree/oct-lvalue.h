/*

Copyright (C) 1996-2017 John W. Eaton

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

#if ! defined (octave_oct_lvalue_h)
#define octave_oct_lvalue_h 1

#include "octave-config.h"

#include <string>

#include "ovl.h"
#include "symrec.h"

namespace octave
{
  class octave_lvalue
  {
  public:

    octave_lvalue (void)
      : m_sym (), m_context (0), m_black_hole (false), m_type (),
        m_idx (), m_nel (1)
    { }

    octave_lvalue (const symbol_record& sr, symbol_record::context_id context)
      : m_sym (sr), m_context (context), m_black_hole (false),
        m_type (), m_idx (), m_nel (1)
    { }

    octave_lvalue (const octave_lvalue&) = default;

    octave_lvalue& operator = (const octave_lvalue&) = default;

    ~octave_lvalue (void) = default;

    bool is_black_hole (void) const { return m_black_hole; }

    void mark_black_hole (void) { m_black_hole = true; }

    bool is_defined (void) const
    {
      return ! is_black_hole () && m_sym.is_defined (m_context);
    }

    bool is_undefined (void) const
    {
      return is_black_hole () || m_sym.is_undefined (m_context);
    }

    bool isstruct (void) const { return value().isstruct (); }

    void define (const octave_value& v) { m_sym.assign (v, m_context); }

    void assign (octave_value::assign_op, const octave_value&);

    void numel (octave_idx_type n) { m_nel = n; }

    octave_idx_type numel (void) const { return m_nel; }

    void set_index (const std::string& t, const std::list<octave_value_list>& i);

    void clear_index (void) { m_type = ""; m_idx.clear (); }

    std::string index_type (void) const { return m_type; }

    bool index_is_empty (void) const;

    void do_unary_op (octave_value::unary_op op);

    octave_value value (void) const;

  private:

    symbol_record m_sym;

    symbol_record::context_id m_context;

    bool m_black_hole;

    std::string m_type;

    std::list<octave_value_list> m_idx;

    octave_idx_type m_nel;
  };
}

#endif
