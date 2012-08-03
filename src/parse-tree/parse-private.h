/*

Copyright (C) 2012 John W. Eaton

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

#if !defined (octave_parse_private_h)
#define octave_parse_private_h 1

#include <stack>

#include "symtab.h"

// Keep track of symbol table information when parsing functions.
class symtab_context
{
private:

  class frame
  {
  public:
    frame (symbol_table::scope_id s, symbol_table::scope_id c)
      : m_scope (s), m_context (c) { }

    frame (const frame& f) : m_scope (f.m_scope), m_context (f.m_context) { }

    frame& operator = (const frame& f)
    {
      if (&f != this)
        {
          m_scope = f.m_scope;
          m_context = f.m_context;
        }

      return *this;
    }

    ~frame (void) { }

    symbol_table::scope_id scope (void) const { return m_scope; }
    symbol_table::scope_id context (void) const { return m_context; }

  private:

    symbol_table::scope_id m_scope;
    symbol_table::scope_id m_context;
  };

  std::stack<frame> frame_stack;

public:
  symtab_context (void) : frame_stack () { }

  void clear (void)
  {
    while (! frame_stack.empty ())
      frame_stack.pop ();
  }

  bool empty (void) const { return frame_stack.empty (); }

  void pop (void)
  {
    frame tmp = frame_stack.top ();

    symbol_table::set_scope_and_context (tmp.scope (), tmp.context ());

    frame_stack.pop ();
  }

  void push (void)
  {
    frame_stack.push (frame (symbol_table::current_scope (),
                             symbol_table::current_context ()));
  }
};

extern symtab_context parser_symtab_context;

#endif
