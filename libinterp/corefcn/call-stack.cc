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

#include "lo-regexp.h"
#include "str-vec.h"

#include "call-stack.h"
#include "defun.h"
#include "interpreter.h"
#include "oct-map.h"
#include "ov.h"
#include "ov-fcn.h"
#include "ov-fcn-handle.h"
#include "ov-usr-fcn.h"
#include "pager.h"
#include "parse.h"
#include "syminfo.h"
#include "symrec.h"
#include "symscope.h"
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

  symbol_info_list
  call_stack::stack_frame::make_symbol_info_list
    (const std::list<symbol_record>& symrec_list) const
  {
    symbol_info_list symbol_stats;

    for (const auto& sr : symrec_list)
      {
        octave_value value = sr.varval (m_context);

        if (value.is_defined ())
          {
            symbol_info syminf (sr.name (), value, sr.is_automatic (),
                                value.iscomplex (), sr.is_formal (),
                                sr.is_global (), sr.is_persistent ());

            symbol_stats.append (syminf);
          }
      }

    return symbol_stats;
  }

  symbol_info_list
  call_stack::stack_frame::glob_symbol_info (const std::string& pat) const
  {
    return make_symbol_info_list (m_scope.glob (pat, true));
  }

  symbol_info_list
  call_stack::stack_frame::regexp_symbol_info (const std::string& pat) const
  {
    return make_symbol_info_list (m_scope.regexp (pat, true));
  }

  symbol_info_list call_stack::stack_frame::get_symbol_info (void) const
  {
    return make_symbol_info_list (m_scope.all_variables ());
  }

  call_stack::call_stack (interpreter& interp)
    : cs (), curr_frame (0), m_max_stack_depth (1024), m_interpreter (interp)
  {
    symbol_table& symtab = m_interpreter.get_symbol_table ();

    push (nullptr, nullptr, symtab.top_scope (), 0);
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

  unwind_protect *
  call_stack::curr_fcn_unwind_protect_frame (void) const
  {
    auto p = cs.cend ();

    while (p != cs.cbegin ())
      {
        const stack_frame& elt = *(--p);

        octave_function *f = elt.m_fcn;

        if (f && f->is_user_code ())
          return elt.m_unwind_protect_frame;
      }

    return nullptr;
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
  call_stack::push (octave_function *fcn, unwind_protect *up_frame)
  {
    symbol_table& symtab = m_interpreter.get_symbol_table ();

    push (fcn, up_frame, symtab.current_scope (), symtab.current_context ());
  }

  void
  call_stack::push (octave_function *fcn, unwind_protect *up_frame,
                    const symbol_scope& scope,
                    symbol_record::context_id context)
  {
    size_t prev_frame = curr_frame;
    curr_frame = cs.size ();

    // m_max_stack_depth should never be less than zero.
    if (curr_frame > static_cast<size_t> (m_max_stack_depth))
      error ("max_stack_depth exceeded");

    cs.push_back (stack_frame (fcn, up_frame, scope, context, prev_frame));

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

  symbol_info_list
  call_stack::glob_symbol_info (const std::string& pat) const
  {
    return cs[curr_frame].glob_symbol_info (pat);
  }

  symbol_info_list
  call_stack::regexp_symbol_info (const std::string& pat) const
  {
    return cs[curr_frame].glob_symbol_info (pat);
  }

  symbol_info_list call_stack::get_symbol_info (void) const
  {
    return cs[curr_frame].get_symbol_info ();
  }

  symbol_info_list call_stack::top_scope_symbol_info (void) const
  {
    return cs[0].get_symbol_info ();
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

static octave_value
do_who_two (octave::interpreter& interp, const string_vector& pats,
            bool global_only, bool have_regexp, bool return_list,
            bool verbose = false, std::string msg = "")
{
  octave::symbol_info_list symbol_stats;
  std::list<std::string> symbol_names;

  octave::tree_evaluator& tw = interp.get_evaluator ();
  octave::symbol_table& symtab = interp.get_symbol_table ();

  octave::symbol_scope scope = symtab.current_scope ();

  octave::symbol_record::context_id context = scope.current_context ();

  octave_idx_type npats = pats.numel ();

  for (octave_idx_type j = 0; j < npats; j++)
    {
      std::string pat = pats[j];

      std::list<octave::symbol_record> tmp
        = (have_regexp
           ? (global_only
              ? symtab.regexp_global_variables (pat)
              : symtab.regexp_variables (pat))
           : (global_only
              ? symtab.glob_global_variables (pat)
              : symtab.glob_variables (pat)));

      for (const auto& sr : tmp)
        {
          octave_value value = sr.varval (context);

          if (value.is_defined ())
            {
              if (verbose)
                {
                  octave::symbol_info
                    syminf (sr.name (), value, sr.is_automatic (),
                            value.iscomplex (), sr.is_formal (),
                            sr.is_global (), sr.is_persistent ());

                  symbol_stats.append (syminf);
                }
              else
                symbol_names.push_back (sr.name ());
            }
        }
    }

  if (return_list)
    {
      if (verbose)
        {
          octave::call_stack& cs = interp.get_call_stack ();

          std::string caller_function_name;
          octave_function *caller = cs.caller ();
          if (caller)
            caller_function_name = caller->name ();

          return symbol_stats.map_value (caller_function_name, 1);
        }
      else
        return Cell (string_vector (symbol_names));
    }
  else if (! (symbol_stats.empty () && symbol_names.empty ()))
    {
      if (msg.empty ())
        if (global_only)
          octave_stdout << "Global variables:\n\n";
        else
          octave_stdout << "Variables in the current scope:\n\n";
      else
        octave_stdout << msg;

      if (verbose)
        symbol_stats.display (octave_stdout, tw.whos_line_format ());
      else
        {
          string_vector names (symbol_names);

          names.list_in_columns (octave_stdout);
        }

      octave_stdout << "\n";
    }

  return octave_value ();
}

static octave_value
do_who (octave::interpreter& interp, int argc, const string_vector& argv,
        bool return_list, bool verbose = false)
{
  octave_value retval;

  octave::symbol_table& symtab = interp.get_symbol_table ();
  octave::call_stack& cs = interp.get_call_stack ();

  std::string my_name = argv[0];

  std::string file_name;

  bool from_file = false;
  bool global_only = false;
  bool have_regexp = false;

  int i = 1;
  while (i < argc)
    {
      if (argv[i] == "-file")
        {
          if (from_file)
            error ("%s: -file option may only be specified once",
                   my_name.c_str ());

          from_file = true;

          if (i == argc - 1)
            error ("%s: -file argument must be followed by a filename",
                   my_name.c_str ());

          file_name = argv[++i];
        }
      else if (argv[i] == "-regexp")
        {
          have_regexp = true;
        }
      else if (argv[i] == "global")
        global_only = true;
      else if (argv[i][0] == '-')
        warning ("%s: unrecognized option '%s'", my_name.c_str (),
                 argv[i].c_str ());
      else
        break;

      i++;
    }

  int npats = argc - i;
  string_vector pats;
  if (npats > 0)
    {
      pats.resize (npats);
      for (int j = 0; j < npats; j++)
        pats[j] = argv[i+j];
    }
  else
    {
      pats.resize (1);
      pats[0] = "*";
    }

  if (from_file)
    {
      // FIXME: This is an inefficient manner to implement this as the
      // variables are loaded in to a temporary context and then treated.
      // It would be better to refactor symbol_info_list to not store the
      // symbol records and then use it in load-save.cc (do_load) to
      // implement this option there so that the variables are never
      // stored at all.

      octave::unwind_protect frame;

      // Set up temporary scope.

      octave::symbol_scope tmp_scope ("$dummy_scope$");

      symtab.set_scope (tmp_scope);

      cs.push (tmp_scope, 0);
      frame.add_method (cs, &octave::call_stack::pop);

      octave::feval ("load", octave_value (file_name), 0);

      std::string newmsg = "Variables in the file " + file_name + ":\n\n";

      return do_who_two (interp, pats, global_only, have_regexp,
                         return_list, verbose, newmsg);
    }
  else
    return do_who_two (interp, pats, global_only, have_regexp,
                       return_list, verbose);
}

DEFMETHOD (who, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} who
@deftypefnx {} {} who pattern @dots{}
@deftypefnx {} {} who option pattern @dots{}
@deftypefnx {} {C =} who ("pattern", @dots{})
List currently defined variables matching the given patterns.

Valid pattern syntax is the same as described for the @code{clear} command.
If no patterns are supplied, all variables are listed.

By default, only variables visible in the local scope are displayed.

The following are valid options, but may not be combined.

@table @code
@item global
List variables in the global scope rather than the current scope.

@item -regexp
The patterns are considered to be regular expressions when matching the
variables to display.  The same pattern syntax accepted by the @code{regexp}
function is used.

@item -file
The next argument is treated as a filename.  All variables found within the
specified file are listed.  No patterns are accepted when reading variables
from a file.
@end table

If called as a function, return a cell array of defined variable names
matching the given patterns.
@seealso{whos, isglobal, isvarname, exist, regexp}
@end deftypefn */)
{
  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("who");

  return do_who (interp, argc, argv, nargout == 1);
}

/*
%!test
%! avar = magic (4);
%! ftmp = [tempname() ".mat"];
%! unwind_protect
%!   save (ftmp, "avar");
%!   vars = whos ("-file", ftmp);
%!   assert (numel (vars), 1);
%!   assert (isstruct (vars));
%!   assert (vars.name, "avar");
%!   assert (vars.size, [4, 4]);
%!   assert (vars.class, "double");
%!   assert (vars.bytes, 128);
%! unwind_protect_cleanup
%!   unlink (ftmp);
%! end_unwind_protect
*/

DEFMETHOD (whos, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} whos
@deftypefnx {} {} whos pattern @dots{}
@deftypefnx {} {} whos option pattern @dots{}
@deftypefnx {} {S =} whos ("pattern", @dots{})
Provide detailed information on currently defined variables matching the
given patterns.

Options and pattern syntax are the same as for the @code{who} command.

Extended information about each variable is summarized in a table with the
following default entries.

@table @asis
@item Attr
Attributes of the listed variable.  Possible attributes are:

@table @asis
@item blank
Variable in local scope

@item @code{a}
Automatic variable.  An automatic variable is one created by the
interpreter, for example @code{argn}.

@item @code{c}
Variable of complex type.

@item @code{f}
Formal parameter (function argument).

@item @code{g}
Variable with global scope.

@item @code{p}
Persistent variable.
@end table

@item Name
The name of the variable.

@item Size
The logical size of the variable.  A scalar is 1x1, a vector is
@nospell{1xN} or @nospell{Nx1}, a 2-D matrix is @nospell{MxN}.

@item Bytes
The amount of memory currently used to store the variable.

@item Class
The class of the variable.  Examples include double, single, char, uint16,
cell, and struct.
@end table

The table can be customized to display more or less information through
the function @code{whos_line_format}.

If @code{whos} is called as a function, return a struct array of defined
variable names matching the given patterns.  Fields in the structure
describing each variable are: name, size, bytes, class, global, sparse,
complex, nesting, persistent.
@seealso{who, whos_line_format}
@end deftypefn */)
{
  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("whos");

  return do_who (interp, argc, argv, nargout == 1, true);
}
