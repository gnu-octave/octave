////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1995-2023 The Octave Project Developers
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

#include "lo-regexp.h"
#include "str-vec.h"

#include "call-stack.h"
#include "defun.h"
#include "interpreter.h"
#include "interpreter-private.h"
#include "oct-map.h"
#include "ov.h"
#include "ov-fcn.h"
#include "ov-fcn-handle.h"
#include "ov-usr-fcn.h"
#include "pager.h"
#include "parse.h"
#include "stack-frame.h"
#include "syminfo.h"
#include "symrec.h"
#include "symscope.h"
#include "variables.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Use static fields for the best efficiency.
// NOTE: C++0x will allow these two to be merged into one.
static const char *bt_fieldnames[] =
{ "file", "name", "line", "column", nullptr };

static const octave_fields bt_fields (bt_fieldnames);

call_stack::call_stack (tree_evaluator& evaluator)
  : m_evaluator (evaluator), m_cs (), m_curr_frame (0),
    m_max_stack_depth (1024), m_global_values ()
{
  push (symbol_scope ("top scope"));
}

octave_function *call_stack::current_function (bool skip_first) const
{
  if (m_cs.empty ())
    error ("current_function: call stack is empty");

  octave_function *fcn = nullptr;

  std::size_t idx = m_curr_frame;

  if (idx > 0 && skip_first)
    --idx;

  while (true)
    {
      fcn = m_cs[idx]->function ();

      if (fcn || idx == 0)
        break;

      --idx;
    }

  return fcn;
}

int call_stack::current_line (void) const
{
  int retval = -1;

  if (! m_cs.empty ())
    {
      const std::shared_ptr<stack_frame> elt = m_cs[m_curr_frame];
      retval = elt->line ();
    }

  return retval;
}

int call_stack::current_column (void) const
{
  int retval = -1;

  if (! m_cs.empty ())
    {
      const std::shared_ptr<stack_frame> elt = m_cs[m_curr_frame];
      retval = elt->column ();
    }

  return retval;
}

octave_user_code *call_stack::current_user_code (void) const
{
  // Start at current frame.

  std::size_t xframe = find_current_user_frame ();

  if (xframe > 0)
    {
      const std::shared_ptr<stack_frame> elt = m_cs[xframe];

      octave_function *f = elt->function ();

      if (f && f->is_user_code ())
        return dynamic_cast<octave_user_code *> (f);
    }

  return nullptr;
}

int call_stack::current_user_code_line (void) const
{
  // Start at current frame.

  std::size_t xframe = find_current_user_frame ();

  if (xframe > 0)
    {
      const std::shared_ptr<stack_frame> elt = m_cs[xframe];

      octave_function *f = elt->function ();

      if (f && f->is_user_code ())
        {
          int line = elt->line ();

          if (line > 0)
            return line;
        }
    }

  return -1;
}

int call_stack::current_user_code_column (void) const
{
  // Start at current frame.

  std::size_t xframe = find_current_user_frame ();

  if (xframe > 0)
    {
      const std::shared_ptr<stack_frame> elt = m_cs[xframe];

      octave_function *f = elt->function ();

      if (f && f->is_user_code ())
        {
          int column = elt->column ();

          if (column > 0)
            return column;
        }
    }

  return -1;
}

unwind_protect *call_stack::curr_fcn_unwind_protect_frame (void)
{
  // Start at current frame.

  std::size_t xframe = find_current_user_frame ();

  if (xframe > 0)
    {
      const std::shared_ptr<stack_frame> elt = m_cs[xframe];

      octave_function *f = elt->function ();

      if (f && f->is_user_code ())
        return elt->unwind_protect_frame ();
    }

  return nullptr;
}

octave_user_code *call_stack::debug_user_code (void) const
{
  octave_user_code *retval = nullptr;

  // This should never happen...
  if (m_curr_frame == 0)
    return retval;

  std::size_t i = m_curr_frame;

  while (i != 0)
    {
      const std::shared_ptr<stack_frame> elt = m_cs[i--];

      octave_function *f = elt->function ();

      if (f && f->is_user_code ())
        {
          retval = dynamic_cast<octave_user_code *> (f);
          break;
        }
    }

  return retval;
}

int call_stack::debug_user_code_line (void) const
{
  int retval = -1;

  // This should never happen...
  if (m_curr_frame == 0)
    return retval;

  std::size_t i = m_curr_frame;

  while (i != 0)
    {
      const std::shared_ptr<stack_frame> elt = m_cs[i--];

      octave_function *f = elt->function ();

      if (f && f->is_user_code ())
        {
          if (elt->line ())
            {
              retval = elt->line ();
              break;
            }
        }
    }

  return retval;
}

int call_stack::debug_user_code_column (void) const
{
  int retval = -1;

  // This should never happen...
  if (m_curr_frame == 0)
    return retval;

  // Start looking with the caller of the calling debug function.
  std::size_t i = m_curr_frame;

  while (i != 0)
    {
      const std::shared_ptr<stack_frame> elt = m_cs[i--];

      octave_function *f = elt->function ();

      if (f && f->is_user_code ())
        {
          if (elt->column ())
            {
              retval = elt->column ();
              break;
            }
        }
    }

  return retval;
}

std::string call_stack::get_dispatch_class (void) const
{
  return m_cs[m_curr_frame]->get_dispatch_class ();
}

void call_stack::set_dispatch_class (const std::string& class_name)
{
  m_cs[m_curr_frame]->set_dispatch_class (class_name);
}

bool call_stack::is_class_method_executing (std::string& dispatch_class) const
{
  dispatch_class = "";

  octave_function *f = current_function ();

  bool retval = (f && f->is_class_method ());

  if (retval)
    dispatch_class = f->dispatch_class ();

  return retval;
}

bool call_stack::is_class_constructor_executing (std::string& dispatch_class) const
{
  dispatch_class = "";

  octave_function *f = current_function ();

  bool retval = (f && f->is_class_constructor ());

  if (retval)
    dispatch_class = f->dispatch_class ();

  return retval;
}

bool call_stack::all_scripts (void) const
{
  bool retval = true;

  auto p = m_cs.cend ();

  while (p != m_cs.cbegin ())
    {
      const std::shared_ptr<stack_frame> elt = *(--p);

      octave_function *f = elt->function ();

      if (f && ! f->is_user_script ())
        {
          retval = false;
          break;
        }
    }

  return retval;
}

void call_stack::get_new_frame_index_and_links
(std::size_t& new_frame_idx, std::shared_ptr<stack_frame>& parent_link,
 std::shared_ptr<stack_frame>& static_link) const
{
  // FIXME: is there a better way?

  std::size_t prev_frame_idx = m_curr_frame;

  new_frame_idx = m_cs.size ();

  // m_max_stack_depth should never be less than zero.
  if (new_frame_idx > static_cast<std::size_t> (m_max_stack_depth))
    error ("max_stack_depth exceeded");

  // There can't be any links to previous frames if this is the first
  // frame on the stack.

  if (new_frame_idx == 0)
    return;

  parent_link = m_cs[prev_frame_idx];

  octave_function *t_fcn = parent_link->function ();

  static_link = (t_fcn
                 ? (t_fcn->is_user_code ()
                    ? parent_link : parent_link->static_link ())
                 : parent_link);
}

void call_stack::push (const symbol_scope& scope)
{
  std::size_t new_frame_idx;
  std::shared_ptr<stack_frame> parent_link;
  std::shared_ptr<stack_frame> static_link;

  get_new_frame_index_and_links (new_frame_idx, parent_link, static_link);

  std::shared_ptr<stack_frame>
  new_frame (stack_frame::create (m_evaluator, scope, new_frame_idx,
                                  parent_link, static_link));

  m_cs.push_back (new_frame);

  m_curr_frame = new_frame_idx;
}

void call_stack::push (octave_user_function *fcn,
                       const std::shared_ptr<stack_frame>& closure_frames)
{
  std::size_t new_frame_idx;
  std::shared_ptr<stack_frame> parent_link;
  std::shared_ptr<stack_frame> static_link;

  get_new_frame_index_and_links (new_frame_idx, parent_link, static_link);

  std::shared_ptr<stack_frame>
  new_frame (stack_frame::create (m_evaluator, fcn, new_frame_idx,
                                  parent_link, static_link,
                                  closure_frames));

  m_cs.push_back (new_frame);

  m_curr_frame = new_frame_idx;
}

void call_stack::push (octave_user_function *fcn,
                       const stack_frame::local_vars_map& local_vars,
                       const std::shared_ptr<stack_frame>& closure_frames)
{
  std::size_t new_frame_idx;
  std::shared_ptr<stack_frame> parent_link;
  std::shared_ptr<stack_frame> static_link;

  get_new_frame_index_and_links (new_frame_idx, parent_link, static_link);

  std::shared_ptr<stack_frame>
  new_frame (stack_frame::create (m_evaluator, fcn, new_frame_idx,
                                  parent_link, static_link, local_vars,
                                  closure_frames));

  m_cs.push_back (new_frame);

  m_curr_frame = new_frame_idx;
}

void call_stack::push (octave_user_script *script)
{
  std::size_t new_frame_idx;
  std::shared_ptr<stack_frame> parent_link;
  std::shared_ptr<stack_frame> static_link;

  get_new_frame_index_and_links (new_frame_idx, parent_link, static_link);

  std::shared_ptr<stack_frame>
  new_frame (stack_frame::create (m_evaluator, script, new_frame_idx,
                                  parent_link, static_link));

  m_cs.push_back (new_frame);

  m_curr_frame = new_frame_idx;
}

void call_stack::push (octave_function *fcn)
{
  std::size_t new_frame_idx;
  std::shared_ptr<stack_frame> parent_link;
  std::shared_ptr<stack_frame> static_link;

  get_new_frame_index_and_links (new_frame_idx, parent_link, static_link);

  std::shared_ptr<stack_frame>
  new_frame (stack_frame::create (m_evaluator, fcn, new_frame_idx,
                                  parent_link, static_link));

  m_cs.push_back (new_frame);

  m_curr_frame = new_frame_idx;
}

bool call_stack::goto_frame (std::size_t n, bool verbose)
{
  bool retval = false;

  if (n < m_cs.size ())
    {
      retval = true;

      m_curr_frame = n;

      if (verbose)
        {
          const std::shared_ptr<stack_frame> elt = m_cs[n];

          elt->display_stopped_in_message (octave_stdout);
        }
    }

  return retval;
}

std::size_t call_stack::find_current_user_frame (void) const
{
  std::size_t user_frame = m_curr_frame;

  std::shared_ptr<stack_frame> frm = m_cs[user_frame];

  if (! (frm->is_user_fcn_frame () || frm->is_user_script_frame ()
         || frm->is_scope_frame ()))
    {
      frm = frm->static_link ();

      user_frame = frm->index ();
    }

  return user_frame;
}

std::shared_ptr<stack_frame> call_stack::current_user_frame (void) const
{
  std::size_t frame = find_current_user_frame ();

  return m_cs[frame];
}

// Go to the Nth frame (up if N is negative or down if positive) in
// the call stack that corresponds to a script, function, or scope
// beginning with the frame indexed by START.

std::size_t call_stack::dbupdown (std::size_t start, int n, bool verbose)
{
  if (start >= m_cs.size ())
    error ("invalid stack frame");

  // Can't go up from here.

  if (start == 0 && n < 0)
    {
      if (verbose)
        m_cs[start]->display_stopped_in_message (octave_stdout);

      return start;
    }

  std::shared_ptr<stack_frame> frm = m_cs[start];

  if (! (frm && (frm->is_user_fcn_frame ()
                 || frm->is_user_script_frame ()
                 || frm->is_scope_frame ())))
    error ("call_stack::dbupdown: invalid initial frame in call stack!");

  // Use index into the call stack to begin the search.  At this point
  // we iterate up or down using indexing instead of static links
  // because ... FIXME: it's a bit complicated, but deserves
  // explanation.  May be easiest with some pictures of the call stack
  // for an example or two.

  std::size_t xframe = frm->index ();

  if (n == 0)
    {
      if (verbose)
        frm->display_stopped_in_message (octave_stdout);

      return xframe;
    }

  int incr = 0;

  if (n < 0)
    {
      incr = -1;
      n = -n;
    }
  else if (n > 0)
    incr = 1;

  std::size_t last_good_frame = 0;

  while (true)
    {
      frm = m_cs[xframe];

      if (frm->is_user_fcn_frame () || frm->is_user_script_frame ()
          || frm->is_scope_frame ())
        {
          last_good_frame = xframe;

          if (n == 0)
            break;

          n--;
        }

      xframe += incr;

      if (xframe == 0)
        {
          last_good_frame = 0;
          break;
        }

      if (xframe == m_cs.size ())
        break;
    }

  if (verbose)
    m_cs[last_good_frame]->display_stopped_in_message (octave_stdout);

  return last_good_frame;
}

// Like dbupdown above but find the starting frame automatically from
// the current frame.  If the current frame is already a user
// function, script, or scope frame, use that.  Otherwise, follow
// the static link for the current frame.  If that is not a user
// function, script or scope frame then there is an error in the
// implementation.

std::size_t call_stack::dbupdown (int n, bool verbose)
{
  std::size_t start = find_current_user_frame ();

  return dbupdown (start, n, verbose);
}

// May be used to temporarily change the value ov m_curr_frame inside
// a function like evalin.  If used in a function like dbup, the new
// value of m_curr_frame would be wiped out when dbup returns and the
// stack frame for dbup is popped.

void call_stack::goto_caller_frame (void)
{
  std::size_t start = find_current_user_frame ();

  std::shared_ptr<stack_frame> caller_frame = m_cs[start]->static_link ();

  // Allow evalin ('caller', ...) to work when called from the
  // top-level prompt.

  m_curr_frame = caller_frame ? caller_frame->index () : 0;
}

void call_stack::goto_base_frame (void)
{
  if (m_curr_frame > 0)
    m_curr_frame = 0;
}

std::list<std::shared_ptr<stack_frame>>
                                     call_stack::backtrace_frames (octave_idx_type& curr_user_frame) const
{
  std::list<std::shared_ptr<stack_frame>> frames;

  // curr_frame is the index to the current frame in the overall call
  // stack, which includes any compiled function frames and scope
  // frames.  The curr_user_frame value we set is the index into the
  // subset of frames returned in the octave_map object.

  std::size_t curr_frame = find_current_user_frame ();

  // Don't include top-level stack frame in the list.

  for (std::size_t n = m_cs.size () - 1; n > 0; n--)
    {
      std::shared_ptr<stack_frame> frm = m_cs[n];

      if (frm->is_user_script_frame () || frm->is_user_fcn_frame ()
          || frm->is_scope_frame ())
        {
          if (frm->index () == curr_frame)
            curr_user_frame = frames.size ();

          frames.push_back (frm);
        }

      if (n == 0)
        break;
    }

  return frames;
}

std::list<std::shared_ptr<stack_frame>>
                                     call_stack::backtrace_frames (void) const
{
  octave_idx_type curr_user_frame = -1;

  return backtrace_frames (curr_user_frame);
}

std::list<frame_info>
call_stack::backtrace_info (octave_idx_type& curr_user_frame,
                            bool print_subfn) const
{
  std::list<std::shared_ptr<stack_frame>> frames
                                       = backtrace_frames (curr_user_frame);

  std::list<frame_info> retval;

  for (const auto& frm : frames)
    {
      if (frm->is_user_script_frame () || frm->is_user_fcn_frame ()
          || frm->is_scope_frame ())
        {
          retval.push_back (frame_info (frm->fcn_file_name (),
                                        frm->fcn_name (print_subfn),
                                        frm->line (), frm->column ()));
        }
    }

  return retval;
}

std::list<frame_info> call_stack::backtrace_info (void) const
{
  octave_idx_type curr_user_frame = -1;

  return backtrace_info (curr_user_frame, true);
}

octave_map call_stack::backtrace (octave_idx_type& curr_user_frame,
                                  bool print_subfn) const
{
  std::list<std::shared_ptr<stack_frame>> frames
                                       = backtrace_frames (curr_user_frame);

  std::size_t nframes = frames.size ();

  octave_map retval (dim_vector (nframes, 1), bt_fields);

  Cell& file = retval.contents (0);
  Cell& name = retval.contents (1);
  Cell& line = retval.contents (2);
  Cell& column = retval.contents (3);

  octave_idx_type k = 0;

  for (const auto& frm : frames)
    {
      if (frm->is_user_script_frame () || frm->is_user_fcn_frame ()
          || frm->is_scope_frame ())
        {
          file(k) = frm->fcn_file_name ();
          name(k) = frm->fcn_name (print_subfn);
          line(k) = frm->line ();
          column(k) = frm->column ();

          k++;
        }
    }

  return retval;
}

octave_map call_stack::backtrace (void) const
{
  octave_idx_type curr_user_frame = -1;

  return backtrace (curr_user_frame, true);
}

octave_map call_stack::empty_backtrace (void) const
{
  return octave_map (dim_vector (0, 1), bt_fields);
}

void call_stack::pop (void)
{
  // Never pop top scope.
  // FIXME: is it possible for this case to happen?

  if (m_cs.size () > 1)
    {
      std::shared_ptr<stack_frame> elt = m_cs.back ();

      std::shared_ptr<stack_frame> caller = elt->parent_link ();

      m_curr_frame = caller->index ();

      if (elt->is_closure_context ())
        elt->break_closure_cycles (elt);

      m_cs.pop_back ();
    }
}

void call_stack::clear (void)
{
  while (! m_cs.empty ())
    pop ();
}

symbol_info_list call_stack::all_variables (void)
{
  return m_cs[m_curr_frame]->all_variables ();
}

std::list<std::string> call_stack::global_variable_names (void) const
{
  std::list<std::string> retval;

  for (const auto& nm_ov : m_global_values)
    {
      if (nm_ov.second.is_defined ())
        retval.push_back (nm_ov.first);
    }

  retval.sort ();

  return retval;
}

std::list<std::string> call_stack::top_level_variable_names (void) const
{
  return m_cs[0]->variable_names ();
}

std::list<std::string> call_stack::variable_names (void) const
{
  return m_cs[m_curr_frame]->variable_names ();
}

void call_stack::clear_global_variable (const std::string& name)
{
  auto p = m_global_values.find (name);

  if (p != m_global_values.end ())
    p->second = octave_value ();
}

void call_stack::clear_global_variable_pattern (const std::string& pattern)
{
  glob_match pat (pattern);

  for (auto& nm_ov : m_global_values)
    {
      if (pat.match (nm_ov.first))
        nm_ov.second = octave_value ();
    }
}

void call_stack::clear_global_variable_regexp (const std::string& pattern)
{
  regexp pat (pattern);

  for (auto& nm_ov : m_global_values)
    {
      if (pat.is_match (nm_ov.first))
        nm_ov.second = octave_value ();
    }
}

void call_stack::clear_global_variables (void)
{
  for (auto& nm_ov : m_global_values)
    nm_ov.second = octave_value ();
}

symbol_info_list
call_stack::glob_symbol_info (const std::string& pattern) const
{
  return m_cs[m_curr_frame]->glob_symbol_info (pattern);
}

symbol_info_list
call_stack::regexp_symbol_info (const std::string& pattern) const
{
  return m_cs[m_curr_frame]->regexp_symbol_info (pattern);
}

symbol_info_list call_stack::get_symbol_info (void)
{
  return m_cs[m_curr_frame]->get_symbol_info ();
}

symbol_info_list call_stack::top_scope_symbol_info (void) const
{
  return m_cs[0]->get_symbol_info ();
}

octave_value call_stack::max_stack_depth (const octave_value_list& args,
    int nargout)
{
  return set_internal_variable (m_max_stack_depth, args, nargout,
                                "max_stack_depth", 0);
}

void call_stack::make_persistent (const symbol_record& sym)
{
  m_cs[m_curr_frame]->make_persistent (sym);
}

void call_stack::make_global (const symbol_record& sym)
{
  m_cs[m_curr_frame]->make_global (sym);
}

octave_value call_stack::global_varval (const std::string& name) const
{
  auto p = m_global_values.find (name);

  return p == m_global_values.end () ? octave_value () : p->second;
}

octave_value& call_stack::global_varref (const std::string& name)
{
  return m_global_values[name];
}

octave_value call_stack::get_top_level_value (const std::string& name) const
{
  return m_cs[0]->varval (name);
}

void call_stack::set_top_level_value (const std::string& name,
                                      const octave_value& value)
{
  m_cs[0]->assign (name, value);
}

octave_value call_stack::do_who (int argc, const string_vector& argv,
                                 bool return_list, bool verbose)
{
  octave_value retval;

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

  int npatterns = argc - i;
  string_vector patterns;
  if (npatterns > 0)
    {
      patterns.resize (npatterns);
      for (int j = 0; j < npatterns; j++)
        patterns[j] = argv[i+j];
    }
  else
    {
      patterns.resize (1);
      patterns[0] = "*";
    }

  if (from_file)
    {
      // FIXME: This is an inefficient manner to implement this as the
      // variables are loaded in to a temporary context and then treated.
      // It would be better to refactor symbol_info_list to not store the
      // symbol records and then use it in load-save.cc (do_load) to
      // implement this option there so that the variables are never
      // stored at all.

      // Set up temporary scope.

      symbol_scope tmp_scope ("$dummy_scope$");

      push (tmp_scope);

      unwind_action restore_scope ([=] (void) { pop (); });

      feval ("load", octave_value (file_name), 0);

      std::string newmsg = "Variables in the file " + file_name + ":\n\n";

      if (global_only)
        return do_global_who_two (patterns, have_regexp, return_list,
                                  verbose, newmsg);
      else
        return do_who_two (patterns, have_regexp, return_list, verbose,
                           newmsg);
    }
  else
    {
      if (global_only)
        return do_global_who_two (patterns, have_regexp, return_list,
                                  verbose);
      else
        return do_who_two (patterns, have_regexp, return_list, verbose);
    }
}

octave_value call_stack::do_who_two (const string_vector& patterns,
                                     bool have_regexp, bool return_list,
                                     bool verbose, const std::string& msg)
{
  return m_cs[m_curr_frame]->who (patterns, have_regexp, return_list,
                                  verbose, m_evaluator.whos_line_format (),
                                  msg);
}

octave_value call_stack::do_global_who_two (const string_vector& patterns,
    bool have_regexp,
    bool return_list, bool verbose,
    const std::string& msg)
{
  symbol_info_list symbol_stats;
  std::list<std::string> symbol_names;

  octave_idx_type npatterns = patterns.numel ();

  for (octave_idx_type j = 0; j < npatterns; j++)
    {
      std::string pattern = patterns[j];

      std::list<std::string> tmp;

      if (have_regexp)
        {
          regexp pat (pattern);

          for (auto& nm_ov : m_global_values)
            {
              if (pat.is_match (nm_ov.first))
                tmp.push_back (nm_ov.first);
            }
        }
      else
        {
          glob_match pat (pattern);

          for (auto& nm_ov : m_global_values)
            {
              if (pat.match (nm_ov.first))
                tmp.push_back (nm_ov.first);
            }
        }

      for (const auto& nm : tmp)
        {
          octave_value value = m_global_values[nm];

          if (value.is_defined ())
            {
              if (verbose)
                {
                  bool is_formal = false;
                  bool is_global = true;
                  bool is_persistent = false;

                  symbol_info syminf (nm, value, is_formal, is_global,
                                      is_persistent);

                  symbol_stats.append (syminf);
                }
              else
                symbol_names.push_back (nm);
            }
        }
    }

  if (return_list)
    {
      if (verbose)
        {
          std::string caller_fcn_name;
          octave_function *caller_fcn = caller_function ();
          if (caller_fcn)
            caller_fcn_name = caller_fcn->name ();

          return symbol_stats.map_value (caller_fcn_name, 1);
        }
      else
        return Cell (string_vector (symbol_names));
    }
  else if (! (symbol_stats.empty () && symbol_names.empty ()))
    {
      if (msg.empty ())
        octave_stdout << "Global variables:\n\n";
      else
        octave_stdout << msg;

      if (verbose)
        symbol_stats.display (octave_stdout,
                              m_evaluator.whos_line_format ());
      else
        {
          string_vector names (symbol_names);

          names.list_in_columns (octave_stdout);
        }

      octave_stdout << "\n";
    }

  return octave_value ();
}

void call_stack::display (void) const
{
  std::ostream& os = octave_stdout;

  std::size_t nframes = size ();

  for (std::size_t i = 0; i < nframes; i++)
    {
      m_cs[i]->display (false);
      if (i < nframes - 1)
        os << std::endl;
    }
}

void call_stack::set_auto_fcn_var (stack_frame::auto_var_type avt,
                                   const octave_value& val)
{
  m_cs[m_curr_frame]->set_auto_fcn_var (avt, val);
}

octave_value call_stack::get_auto_fcn_var (stack_frame::auto_var_type avt) const
{
  return m_cs[m_curr_frame]->get_auto_fcn_var (avt);
}

DEFMETHOD (max_stack_depth, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} max_stack_depth ()
@deftypefnx {} {@var{old_val} =} max_stack_depth (@var{new_val})
@deftypefnx {} {@var{old_val} =} max_stack_depth (@var{new_val}, "local")
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
  tree_evaluator& tw = interp.get_evaluator ();

  return tw.max_stack_depth (args, nargout);
}

/*
%!test
%! orig_val = max_stack_depth ();
%! old_val = max_stack_depth (2*orig_val);
%! assert (orig_val, old_val);
%! assert (max_stack_depth (), 2*orig_val);
%! max_stack_depth (orig_val);
%! assert (max_stack_depth (), orig_val);

%!error max_stack_depth (1, 2)
*/

DEFMETHOD (who, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} who
@deftypefnx {} {} who pattern @dots{}
@deftypefnx {} {} who option pattern @dots{}
@deftypefnx {} {C =} who (@dots{})
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

  tree_evaluator& tw = interp.get_evaluator ();

  return tw.do_who (argc, argv, nargout == 1);
}

/*
%!test
%! avar = magic (4);
%! ftmp = [tempname() ".mat"];
%! save_default_options ("-binary", "local");
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

  tree_evaluator& tw = interp.get_evaluator ();

  return tw.do_who (argc, argv, nargout == 1, true);
}

OCTAVE_END_NAMESPACE(octave)
