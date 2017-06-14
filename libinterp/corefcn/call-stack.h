/*

Copyright (C) 1993-2017 John W. Eaton

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

#if ! defined (octave_call_stack_h)
#define octave_call_stack_h 1

#include "octave-config.h"

#include <deque>
#include <string>

class octave_function;
class octave_user_code;
class octave_user_script;

#include "input.h"
#include "interpreter.h"
#include "oct-map.h"
#include "symtab.h"

namespace octave
{
  class
  OCTINTERP_API
  call_stack
  {
  public:

    class stack_frame
    {
    public:

      friend class call_stack;

      stack_frame (octave_function *fcn = nullptr,
                   symbol_table::scope *scope = 0,
                   symbol_table::context_id context = 0, size_t prev = 0)
        : m_fcn (fcn), m_line (-1), m_column (-1), m_scope (scope),
          m_context (context), m_prev (prev)
      { }

      stack_frame (const stack_frame& elt)
        : m_fcn (elt.m_fcn), m_line (elt.m_line), m_column (elt.m_column),
          m_scope (elt.m_scope), m_context (elt.m_context), m_prev (elt.m_prev)
      { }

      int line (void) const { return m_line; }

      int column (void) const { return m_column; }

      std::string fcn_file_name (void) const;

      std::string fcn_name (bool print_subfn = true) const;

      bool operator == (const stack_frame& rhs) const;

    private:

      octave_function *m_fcn;
      int m_line;
      int m_column;
      symbol_table::scope *m_scope;
      symbol_table::context_id m_context;
      size_t m_prev;
    };

    typedef std::deque<stack_frame>::iterator iterator;
    typedef std::deque<stack_frame>::const_iterator const_iterator;

    typedef std::deque<stack_frame>::reverse_iterator reverse_iterator;
    typedef std::deque<stack_frame>::const_reverse_iterator const_reverse_iterator;

    call_stack (interpreter& interp);

    // Current function (top of stack).
    octave_function * current (void) const
    {
      octave_function *retval = nullptr;

      if (! cs.empty ())
        {
          const stack_frame& elt = cs[curr_frame];
          retval = elt.m_fcn;
        }

      return retval;
    }

    // Current line in current function.
    int current_line (void) const;

    // Current column in current function.
    int current_column (void) const;

    // Caller function, may be built-in.

    octave_function * caller (void) const
    {
      return curr_frame > 1 ? cs[curr_frame-1].m_fcn : cs[0].m_fcn;
    }

    size_t current_frame (void) const { return curr_frame; }

    size_t size (void) const { return cs.size (); }

    size_t num_user_code_frames (octave_idx_type& curr_user_frame) const;

    symbol_table::scope *current_scope (void) const
    {
      return (curr_frame > 0 && curr_frame < cs.size ()
              ? cs[curr_frame].m_scope : 0);
    }

    symbol_table::context_id current_context (void) const
    {
      return (curr_frame > 0 && curr_frame < cs.size ()
              ? cs[curr_frame].m_context : 0);
    }

    // Function at location N on the call stack (N == 0 is current), may
    // be built-in.
    octave_function * element (size_t n)
    {
      octave_function *retval = nullptr;

      if (cs.size () > n)
        {
          stack_frame& elt = cs[n];
          retval = elt.m_fcn;
        }

      return retval;
    }

    // User code caller.
    octave_user_code * caller_user_code (size_t nskip = 0) const;

    // Line in user code caller.
    int caller_user_code_line (void) const;

    // Column in user code caller.
    int caller_user_code_column (void) const;

    // Current function that we are debugging.
    octave_user_code * debug_user_code (void) const;

    // Line number in current function that we are debugging.
    int debug_user_code_line (void) const;

    // Column number in current function that we are debugging.
    int debug_user_code_column (void) const;

    // Return TRUE if all elements on the call stack are scripts.
    bool all_scripts (void) const;

    void push (octave_function *fcn)
    {
      symbol_table& symtab = m_interpreter.get_symbol_table ();

      push (fcn, symtab.current_scope (), symtab.current_context ());
    }

    void push (octave_function *fcn, symbol_table::scope *scope,
               symbol_table::context_id context)
    {
      size_t prev_frame = curr_frame;
      curr_frame = cs.size ();
      cs.push_back (stack_frame (fcn, scope, context, prev_frame));

      symbol_table& symtab = m_interpreter.get_symbol_table ();

      symtab.set_scope_and_context (scope, context);
    }

    void push (void)
    {
      push (nullptr);
    }

    void push (symbol_table::scope *scope, symbol_table::context_id context)
    {
      push (nullptr, scope, context);
    }

    void set_location (int l, int c)
    {
      if (! cs.empty ())
        {
          stack_frame& elt = cs.back ();

          elt.m_line = l;
          elt.m_column = c;
        }
    }

    void set_line (int l)
    {
      if (! cs.empty ())
        {
          stack_frame& elt = cs.back ();

          elt.m_line = l;
        }
    }

    void set_column (int c)
    {
      if (! cs.empty ())
        {
          stack_frame& elt = cs.back ();

          elt.m_column = c;
        }
    }

    bool goto_frame (size_t n = 0, bool verbose = false);

    void restore_frame (size_t n)
    {
      goto_frame (n);
    }

    bool goto_frame_relative (int n, bool verbose = false);

    void goto_caller_frame (void);

    void goto_base_frame (void);

    std::list<call_stack::stack_frame>
    backtrace_frames (size_t nskip, octave_idx_type& curr_user_frame) const;

    std::list<call_stack::stack_frame>
    backtrace_frames (size_t nskip = 0) const
    {
      octave_idx_type curr_user_frame = -1;

      return backtrace_frames (nskip, curr_user_frame);
    }

    octave_map backtrace (size_t nskip, octave_idx_type& curr_user_frame,
                          bool print_subfn = true) const;

    octave_map backtrace (size_t nskip = 0)
    {
      octave_idx_type curr_user_frame = -1;

      return backtrace (nskip, curr_user_frame, true);
    }

    octave_map empty_backtrace (void) const;

    void pop (void)
    {
      if (cs.size () > 1)
        {
          const stack_frame& elt = cs.back ();
          curr_frame = elt.m_prev;
          cs.pop_back ();
          const stack_frame& new_elt = cs[curr_frame];

          symbol_table& symtab = m_interpreter.get_symbol_table ();

          symtab.set_scope_and_context (new_elt.m_scope, new_elt.m_context);
        }
    }

    void clear (void) { cs.clear (); }

  private:

    // The current call stack.
    std::deque<stack_frame> cs;

    size_t curr_frame;

    interpreter& m_interpreter;
  };
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED ("use 'octave::call_stack' instead")
typedef octave::call_stack octave_call_stack;

#endif

#endif
