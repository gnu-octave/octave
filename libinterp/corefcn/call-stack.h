/*

Copyright (C) 1993-2018 John W. Eaton

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

#if ! defined (octave_call_stack_h)
#define octave_call_stack_h 1

#include "octave-config.h"

#include <deque>
#include <string>

class octave_function;
class octave_map;
class octave_user_code;
class octave_user_script;
class octave_value;
class octave_value_list;

#include "symscope.h"

namespace octave
{
  class interpreter;
  class unwind_protect;

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
                   unwind_protect *up_frame = nullptr,
                   const symbol_scope& scope = symbol_scope (),
                   symbol_record::context_id context = 0, size_t prev = 0)
        : m_fcn (fcn), m_unwind_protect_frame (up_frame),
          m_line (-1), m_column (-1), m_scope (scope),
          m_context (context), m_prev (prev)
      { }

      stack_frame (const stack_frame& elt)
        : m_fcn (elt.m_fcn),
          m_unwind_protect_frame (elt.m_unwind_protect_frame),
          m_line (elt.m_line), m_column (elt.m_column),
          m_scope (elt.m_scope), m_context (elt.m_context),
          m_prev (elt.m_prev)
      { }

      int line (void) const { return m_line; }

      int column (void) const { return m_column; }

      std::string fcn_file_name (void) const;

      std::string fcn_name (bool print_subfn = true) const;

      bool operator == (const stack_frame& rhs) const;

    private:

      octave_function *m_fcn;
      unwind_protect *m_unwind_protect_frame;
      int m_line;
      int m_column;
      symbol_scope m_scope;
      symbol_record::context_id m_context;
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

    symbol_scope current_scope (void) const
    {
      return (curr_frame > 0 && curr_frame < cs.size ()
              ? cs[curr_frame].m_scope : symbol_scope ());
    }

    symbol_record::context_id current_context (void) const
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

    unwind_protect *curr_fcn_unwind_protect_frame (void) const;

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

    void push (octave_function *fcn = nullptr,
               unwind_protect *up_frame = nullptr);

    void push (octave_function *fcn, unwind_protect *up_frame,
               const symbol_scope& scope, symbol_record::context_id context);

    void push (const symbol_scope& scope, symbol_record::context_id context)
    {
      push (nullptr, nullptr, scope, context);
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

    octave_map backtrace (size_t nskip = 0);

    octave_map empty_backtrace (void) const;

    void pop (void);

    void clear (void) { cs.clear (); }

    octave_value max_stack_depth (const octave_value_list& args, int nargout);

  private:

    // The current call stack.
    std::deque<stack_frame> cs;

    size_t curr_frame;

    int m_max_stack_depth;

    interpreter& m_interpreter;
  };
}

#if defined (OCTAVE_USE_DEPRECATED_FUNCTIONS)

OCTAVE_DEPRECATED (4.4, "use 'octave::call_stack' instead")
typedef octave::call_stack octave_call_stack;

#endif

#endif
