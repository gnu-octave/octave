/*

Copyright (C) 1995-2018 John W. Eaton

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

#include "call-stack.h"
#include "defun.h"
#include "interpreter.h"
#include "oct-map.h"
#include "ov.h"
#include "ov-fcn.h"
#include "ov-fcn-handle.h"
#include "ov-usr-fcn.h"
#include "pager.h"
#include "variables.h"

// Use static fields for the best efficiency.
// NOTE: C++0x will allow these two to be merged into one.
static const char *bt_fieldnames[] =
  { "file", "name", "line", "column", "scope", "context", nullptr };
static const octave_fields bt_fields (bt_fieldnames);

namespace octave
{
  std::string
  call_stack::stack_frame::fcn_file_name (void) const
  {
    return m_fcn ? m_fcn->fcn_file_name () : "";
  }

  std::string
  call_stack::stack_frame::fcn_name (bool print_subfn) const
  {
    std::string retval;

    if (m_fcn)
      {
        std::string parent_fcn_name = m_fcn->parent_fcn_name ();

        if (print_subfn && ! parent_fcn_name.empty ())
          retval = parent_fcn_name + '>';

        if (m_fcn->is_anonymous_function ())
          retval += octave_fcn_handle::anonymous;
        else
          retval += m_fcn->name ();
      }
    else
      retval = "<unknown>";

    return retval;
  }

  bool
  call_stack::stack_frame::operator == (const call_stack::stack_frame& rhs) const
  {
    if (this->line () != rhs.line ())
      return false;
    else if (this->column () != rhs.column ())
      return false;
    else if (this->fcn_file_name () != rhs.fcn_file_name ())
      return false;
    else if (this->fcn_name () != rhs.fcn_name ())
      return false;
    else
      return true;
  }

  call_stack::call_stack (interpreter& interp)
    : cs (), curr_frame (0), m_max_stack_depth (1024), m_interpreter (interp)
  {
    symbol_table& symtab = m_interpreter.get_symbol_table ();

    push (nullptr, symtab.top_scope (), 0);
  }

  int
  call_stack::current_line (void) const
  {
    int retval = -1;

    if (! cs.empty ())
      {
        const stack_frame& elt = cs[curr_frame];
        retval = elt.m_line;
      }

    return retval;
  }

  int
  call_stack::current_column (void) const
  {
    int retval = -1;

    if (! cs.empty ())
      {
        const stack_frame& elt = cs[curr_frame];
        retval = elt.m_column;
      }

    return retval;
  }

  size_t
  call_stack::num_user_code_frames (octave_idx_type& curr_user_frame) const
  {
    size_t retval = 0;

    curr_user_frame = 0;

    // Look for the caller of dbstack.
    size_t xframe = cs[curr_frame].m_prev;

    bool found = false;

    size_t k = cs.size ();

    for (auto p = cs.crbegin (); p != cs.crend (); p++)
      {
        octave_function *f = (*p).m_fcn;

        if (--k == xframe)
          found = true;

        if (f && f->is_user_code ())
          {
            if (! found)
              curr_user_frame++;

            retval++;
          }
      }

    // We counted how many user frames were not the one, in reverse.
    // Now set curr_user_frame to be the index in the other direction.
    curr_user_frame = retval - curr_user_frame - 1;

    return retval;
  }

  octave_user_code *
  call_stack::caller_user_code (size_t nskip) const
  {
    octave_user_code *retval = nullptr;

    auto p = cs.cend ();

    while (p != cs.cbegin ())
      {
        const stack_frame& elt = *(--p);

        octave_function *f = elt.m_fcn;

        if (f && f->is_user_code ())
          {
            if (nskip > 0)
              nskip--;
            else
              {
                retval = dynamic_cast<octave_user_code *> (f);
                break;
              }
          }
      }

    return retval;
  }

  int
  call_stack::caller_user_code_line (void) const
  {
    int retval = -1;

    auto p = cs.cend ();

    while (p != cs.cbegin ())
      {
        const stack_frame& elt = *(--p);

        octave_function *f = elt.m_fcn;

        if (f && f->is_user_code ())
          {
            if (elt.m_line > 0)
              {
                retval = elt.m_line;
                break;
              }
          }
      }

    return retval;
  }

  int
  call_stack::caller_user_code_column (void) const
  {
    int retval = -1;

    auto p = cs.cend ();

    while (p != cs.cbegin ())
      {
        const stack_frame& elt = *(--p);

        octave_function *f = elt.m_fcn;

        if (f && f->is_user_code ())
          {
            if (elt.m_column)
              {
                retval = elt.m_column;
                break;
              }
          }
      }

    return retval;
  }

  octave_user_code *
  call_stack::debug_user_code (void) const
  {
    octave_user_code *retval = nullptr;

    // This should never happen...
    if (curr_frame == 0)
      return retval;

    // Start looking with the caller of the calling debug function.
    size_t i = cs[curr_frame].m_prev;

    while (i != 0)
      {
        const stack_frame& elt = cs[i--];

        octave_function *f = elt.m_fcn;

        if (f && f->is_user_code ())
          {
            retval = dynamic_cast<octave_user_code *> (f);
            break;
          }
      }

    return retval;
  }

  int
  call_stack::debug_user_code_line (void) const
  {
    int retval = -1;

    // This should never happen...
    if (curr_frame == 0)
      return retval;

    // Start looking with the caller of the calling debug function.
    size_t i = cs[curr_frame].m_prev;

    while (i != 0)
      {
        const stack_frame& elt = cs[i--];

        octave_function *f = elt.m_fcn;

        if (f && f->is_user_code ())
          {
            if (elt.m_line)
              {
                retval = elt.m_line;
                break;
              }
          }
      }

    return retval;
  }

  int
  call_stack::debug_user_code_column (void) const
  {
    int retval = -1;

    // This should never happen...
    if (curr_frame == 0)
      return retval;

    // Start looking with the caller of the calling debug function.
    size_t i = cs[curr_frame].m_prev;

    while (i != 0)
      {
        const stack_frame& elt = cs[i--];

        octave_function *f = elt.m_fcn;

        if (f && f->is_user_code ())
          {
            if (elt.m_column)
              {
                retval = elt.m_column;
                break;
              }
          }
      }

    return retval;
  }

  bool
  call_stack::all_scripts (void) const
  {
    bool retval = true;

    auto p = cs.cend ();

    while (p != cs.cbegin ())
      {
        const stack_frame& elt = *(--p);

        octave_function *f = elt.m_fcn;

        if (f && ! f->is_user_script ())
          {
            retval = false;
            break;
          }
      }

    return retval;
  }

  void
  call_stack::push (octave_function *fcn)
  {
    symbol_table& symtab = m_interpreter.get_symbol_table ();

    push (fcn, symtab.current_scope (), symtab.current_context ());
  }

  void
  call_stack::push (octave_function *fcn, const symbol_scope& scope,
                    symbol_record::context_id context)
  {
    size_t prev_frame = curr_frame;
    curr_frame = cs.size ();

    // m_max_stack_depth should never be less than zero.
    if (curr_frame > static_cast<size_t> (m_max_stack_depth))
      error ("max_stack_depth exceeded");

    cs.push_back (stack_frame (fcn, scope, context, prev_frame));

    symbol_table& symtab = m_interpreter.get_symbol_table ();

    symtab.set_scope_and_context (scope, context);
  }

  bool
  call_stack::goto_frame (size_t n, bool verbose)
  {
    bool retval = false;

    if (n < cs.size ())
      {
        retval = true;

        curr_frame = n;

        const stack_frame& elt = cs[n];

        symbol_table& symtab = m_interpreter.get_symbol_table ();

        symtab.set_scope_and_context (elt.m_scope, elt.m_context);

        if (verbose)
          octave_stdout << "stopped in " << elt.fcn_name ()
                        << " at line " << elt.m_line
                        << " column " << elt.m_column
                        << " [" << elt.fcn_file_name () << "] "
                        << "[context = " << elt.m_context << "])"
                        << std::endl;
      }

    return retval;
  }

  bool
  call_stack::goto_frame_relative (int nskip, bool verbose)
  {
    bool retval = false;

    int incr = 0;

    if (nskip < 0)
      incr = -1;
    else if (nskip > 0)
      incr = 1;

    // Start looking with the caller of dbup/dbdown/keyboard.
    size_t xframe = cs[curr_frame].m_prev;

    while (true)
      {
        if ((incr < 0 && xframe == 0) || (incr > 0 && xframe == cs.size () - 1))
          break;

        xframe += incr;

        const stack_frame& elt = cs[xframe];

        octave_function *f = elt.m_fcn;

        if (xframe == 0 || (f && f->is_user_code ()))
          {
            if (nskip > 0)
              nskip--;
            else if (nskip < 0)
              nskip++;

            if (nskip == 0)
              {
                curr_frame = xframe;
                cs[cs.size () - 1].m_prev = curr_frame;

                symbol_table& symtab = m_interpreter.get_symbol_table ();

                symtab.set_scope_and_context (elt.m_scope, elt.m_context);

                if (verbose)
                  {
                    std::ostringstream buf;

                    if (f)
                      buf << "stopped in " << elt.fcn_name ()
                          << " at line " << elt.m_line
                          << " [" << elt.fcn_file_name () << "] "
                          << std::endl;
                    else
                      buf << "at top level" << std::endl;

                    octave_stdout << buf.str ();
                  }

                retval = true;
                break;
              }
          }
        else if (incr == 0)  // Break out of infinite loop by choosing an incr.
          incr = -1;

        // There is no need to set scope and context here.  That will
        // happen when the dbup/dbdown/keyboard frame is popped and we
        // jump to the new "prev" frame set above.
      }

    return retval;
  }

  void
  call_stack::goto_caller_frame (void)
  {
    size_t xframe = curr_frame;

    bool skipped = false;

    while (xframe != 0)
      {
        xframe = cs[xframe].m_prev;

        const stack_frame& elt = cs[xframe];

        octave_function *f = elt.m_fcn;

        if (elt.m_scope == cs[0].m_scope || (f && f->is_user_code ()))
          {
            if (! skipped)
              // We found the current user code frame, so skip it.
              skipped = true;
            else
              {
                // We found the caller user code frame.
                stack_frame tmp (elt);
                tmp.m_prev = curr_frame;

                curr_frame = cs.size ();

                cs.push_back (tmp);

                symbol_table& symtab = m_interpreter.get_symbol_table ();

                symtab.set_scope_and_context (tmp.m_scope, tmp.m_context);

                break;
              }
          }
      }
  }

  void
  call_stack::goto_base_frame (void)
  {
    stack_frame tmp (cs[0]);
    tmp.m_prev = curr_frame;

    curr_frame = cs.size ();

    cs.push_back (tmp);

    symbol_table& symtab = m_interpreter.get_symbol_table ();

    symtab.set_scope_and_context (tmp.m_scope, tmp.m_context);
  }

  std::list<call_stack::stack_frame>
  call_stack::backtrace_frames (size_t nskip,
                                octave_idx_type& curr_user_frame) const
  {
    std::list<call_stack::stack_frame> retval;

    size_t user_code_frames = num_user_code_frames (curr_user_frame);

    size_t nframes = (nskip <= user_code_frames ? user_code_frames - nskip : 0);

    // Our list is reversed.
    curr_user_frame = nframes - curr_user_frame - 1;

    if (nframes > 0)
      {
        for (auto p = cs.crbegin (); p != cs.crend (); p++)
          {
            const stack_frame& elt = *p;

            octave_function *f = elt.m_fcn;

            if (f && f->is_user_code ())
              {
                if (nskip > 0)
                  nskip--;
                else
                  retval.push_back (elt);
              }
          }
      }

    return retval;
  }

  octave_map
  call_stack::backtrace (size_t nskip, octave_idx_type& curr_user_frame,
                         bool print_subfn) const
  {
    std::list<call_stack::stack_frame> frames
      = backtrace_frames (nskip, curr_user_frame);

    size_t nframes = frames.size ();

    octave_map retval (dim_vector (nframes, 1), bt_fields);

    Cell& file = retval.contents (0);
    Cell& name = retval.contents (1);
    Cell& line = retval.contents (2);
    Cell& column = retval.contents (3);
    Cell& context = retval.contents (5);

    octave_idx_type k = 0;

    for (const auto& frm : frames)
      {
        context(k) = frm.m_context;
        file(k)    = frm.fcn_file_name ();
        name(k)    = frm.fcn_name (print_subfn);
        line(k)    = frm.m_line;
        column(k)  = frm.m_column;

        k++;
      }

    return retval;
  }

  octave_map
  call_stack::backtrace (size_t nskip)
  {
    octave_idx_type curr_user_frame = -1;

    return backtrace (nskip, curr_user_frame, true);
  }

  octave_map
  call_stack::empty_backtrace (void) const
  {
    return octave_map (dim_vector (0, 1), bt_fields);
  }

  void
  call_stack::pop (void)
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

  octave_value
  call_stack::max_stack_depth (const octave_value_list& args, int nargout)
  {
    return set_internal_variable (m_max_stack_depth, args, nargout,
                                  "max_stack_depth", 0);
  }
}

DEFMETHOD (max_stack_depth, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} max_stack_depth ()
@deftypefnx {} {@var{old_val} =} max_stack_depth (@var{new_val})
@deftypefnx {} {} max_stack_depth (@var{new_val}, "local")
Query or set the internal limit on the number of times a function may
be called recursively.

If the limit is exceeded, an error message is printed and control returns to
the top level.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.

@seealso{max_recursion_depth}
@end deftypefn */)
{
  octave::call_stack& cs = interp.get_call_stack ();

  return cs.max_stack_depth (args, nargout);
}

/*
%!test
%! orig_val = max_stack_depth ();
%! old_val = max_stack_depth (2*orig_val);
%! assert (orig_val, old_val);
%! assert (max_stack_depth (), 2*orig_val);
%! max_stack_depth (orig_val);
%! assert (max_stack_depth (), orig_val);

%!error (max_stack_depth (1, 2))
*/

