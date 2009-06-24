/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 2000, 2001, 2002,
              2003, 2004, 2005, 2006, 2007, 2008 John W. Eaton

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

#if !defined (octave_toplev_h)
#define octave_toplev_h 1

#include <cstdio>

#include <deque>
#include <string>

class octave_value;
class octave_value_list;
class octave_function;
class octave_user_script;
class tree_statement;
class tree_statement_list;
class charMatrix;

#include "quit.h"

#include "input.h"
#include "oct-map.h"


typedef void (*octave_exit_func) (int);
extern OCTINTERP_API octave_exit_func octave_exit;

extern OCTINTERP_API bool quit_allowed;

extern OCTINTERP_API bool quitting_gracefully;

extern OCTINTERP_API int exit_status;

extern OCTINTERP_API void
clean_up_and_exit (int);

extern OCTINTERP_API void recover_from_exception (void);

extern OCTINTERP_API int main_loop (void);

extern OCTINTERP_API void
do_octave_atexit (void);

extern OCTINTERP_API void
octave_add_atexit_function (const std::string& fname);

extern OCTINTERP_API bool
octave_remove_atexit_function (const std::string& fname);

// Current command to execute.
extern OCTINTERP_API tree_statement_list *global_command;

// TRUE means we are ready to interpret commands, but not everything
// is ready for interactive use.
extern OCTINTERP_API bool octave_interpreter_ready;

// TRUE means we've processed all the init code and we are good to go.
extern OCTINTERP_API bool octave_initialized;

class
OCTINTERP_API
octave_call_stack
{
private:

  struct call_stack_elt
  {
    call_stack_elt (octave_function *f, symbol_table::scope_id s,
		    symbol_table::context_id c, size_t p = 0)
      : fcn (f), stmt (0), scope (s), context (c), prev (p) { }

    call_stack_elt (const call_stack_elt& elt)
      : fcn (elt.fcn), stmt (elt.stmt), scope (elt.scope),
	context (elt.context), prev (elt.prev) { }

    octave_function *fcn;
    tree_statement *stmt;
    symbol_table::scope_id scope;
    symbol_table::context_id context;
    size_t prev;
  };

protected:

  octave_call_stack (void) : cs (), curr_frame (0) { }

public:

  typedef std::deque<call_stack_elt>::iterator iterator;
  typedef std::deque<call_stack_elt>::const_iterator const_iterator;

  typedef std::deque<call_stack_elt>::reverse_iterator reverse_iterator;
  typedef std::deque<call_stack_elt>::const_reverse_iterator const_reverse_iterator;

  static bool instance_ok (void)
  {
    bool retval = true;

    if (! instance)
      {
	instance = new octave_call_stack ();

	if (instance)
	  instance->do_push (0, symbol_table::top_scope (), 0);
	else
	  {
	    ::error ("unable to create call stack object!");

	    retval = false;
	  }
      }

    return retval;
  }

  // Current function (top of stack).
  static octave_function *current (void) { return top (); }

  // Current statement (top of stack).
  static tree_statement *current_statement (void) { return top_statement (); }

  // Current line in current function.
  static int current_line (void)
  {
    return instance_ok () ? instance->do_current_line () : -1;
  }

  // Current column in current function.
  static int current_column (void)
  {
    return instance_ok () ? instance->do_current_column () : -1;
  }

  // Line in user code caller.
  static int caller_user_code_line (void)
  {
    return instance_ok () ? instance->do_caller_user_code_line () : -1;
  }

  // Column in user code caller.
  static int caller_user_code_column (void)
  {
    return instance_ok () ? instance->do_caller_user_code_column () : -1;
  }

  // Caller function, may be built-in.
  static octave_function *caller (void)
  {
    return instance_ok () ? instance->do_caller () : 0;
  }

  static size_t current_frame (void)
  {
    return instance_ok () ? instance->do_current_frame () : 0;
  }

  static size_t size (void)
  {
    return instance_ok () ? instance->do_size () : 0;
  }

  static size_t num_user_code_frames (octave_idx_type& curr_user_frame)
  {
    return instance_ok ()
      ? instance->do_num_user_code_frames (curr_user_frame) : 0;
  }

  static symbol_table::scope_id current_scope (void)
  {
    return instance_ok () ? instance->do_current_scope () : 0;
  }

  static symbol_table::context_id current_context (void)
  {
    return instance_ok () ? instance->do_current_context () : 0;
  }

  // Function at location N on the call stack (N == 0 is current), may
  // be built-in.
  static octave_function *element (size_t n)
  {
    return instance_ok () ? instance->do_element (n) : 0;
  }
  
  // First user-defined function on the stack.
  static octave_user_code *caller_user_code (size_t nskip = 0)
  {
    return instance_ok () ? instance->do_caller_user_code (nskip) : 0;
  }

  static void
  push (octave_function *f,
	symbol_table::scope_id scope = symbol_table::current_scope (),
	symbol_table::context_id context = symbol_table::current_context ())
  {
    if (instance_ok ())
      instance->do_push (f, scope, context);
  }

  static void
  push (symbol_table::scope_id scope = symbol_table::current_scope (),
	symbol_table::context_id context = symbol_table::current_context ())
  {
    if (instance_ok ())
      instance->do_push (0, scope, context);
  }

  static octave_function *top (void)
  {
    return instance_ok () ? instance->do_top (): 0;
  }

  static tree_statement *top_statement (void)
  {
    return instance_ok () ? instance->do_top_statement (): 0;
  }

  static void set_statement (tree_statement *s)
  {
    if (instance_ok ())
      instance->do_set_statement (s);
  }

  static bool goto_frame (size_t n = 0, bool verbose = false)
  {
    return instance_ok () ? instance->do_goto_frame (n, verbose) : false;
  }

  static bool goto_frame_relative (int n, bool verbose = false)
  {
    return instance_ok ()
      ? instance->do_goto_frame_relative (n, verbose) : false;
  }

  static void goto_caller_frame (void)
  {
    if (instance_ok ())
      instance->do_goto_caller_frame ();
  }

  static void goto_base_frame (void)
  {
    if (instance_ok ())
      instance->do_goto_base_frame ();
  }

  static Octave_map backtrace (size_t nskip, octave_idx_type& curr_user_frame)
  {
    return instance_ok ()
      ? instance->do_backtrace (nskip, curr_user_frame) : Octave_map ();
  }

  static void pop (void)
  {
    if (instance_ok ())
      instance->do_pop ();
  }
  
  // A function for popping the top of the call stack that is suitable
  // for use as an unwind_protect handler.
  static void unwind_pop (void *) { pop (); }

  static void clear (void)
  {
    if (instance_ok ())
      instance->do_clear ();
  }

  static void backtrace_error_message (void)
  {
    if (instance_ok ())
      instance->do_backtrace_error_message ();
  }

private:

  // The current call stack.
  std::deque<call_stack_elt> cs;

  size_t curr_frame;

  static octave_call_stack *instance;

  int do_current_line (void) const;

  int do_current_column (void) const;

  int do_caller_user_code_line (void) const;

  int do_caller_user_code_column (void) const;

  octave_function *do_caller (void) const
  {
    return curr_frame > 1 ? cs[curr_frame-1].fcn : cs[0].fcn;
  }

  size_t do_current_frame (void) { return curr_frame; }

  size_t do_size (void) { return cs.size (); }

  size_t do_num_user_code_frames (octave_idx_type& curr_user_frame) const;

  symbol_table::scope_id do_current_scope (void) const
  {
    return curr_frame > 0 && curr_frame < cs.size ()
      ? cs[curr_frame].scope : 0;
  }

  symbol_table::context_id do_current_context (void) const
  {
    return curr_frame > 0 && curr_frame < cs.size ()
      ? cs[curr_frame].context : 0;
  }

  octave_function *do_element (size_t n)
  {
    octave_function *retval = 0;

    if (cs.size () > n)
      {
	call_stack_elt& elt = cs[n];
	retval = elt.fcn;
      }

    return retval;
  }

  octave_user_code *do_caller_user_code (size_t nskip) const; 

  void do_push (octave_function *f, symbol_table::scope_id scope,
		symbol_table::context_id context)
  {
    size_t prev_frame = curr_frame;
    curr_frame = cs.size ();
    cs.push_back (call_stack_elt (f, scope, context, prev_frame));
    symbol_table::set_scope_and_context (scope, context);
  }

  octave_function *do_top (void) const
  {
    octave_function *retval = 0;

    if (! cs.empty ())
      {
	const call_stack_elt& elt = cs.back ();
	retval = elt.fcn;
      }

    return retval;
  }

  tree_statement *do_top_statement (void) const
  {
    tree_statement *retval = 0;

    if (! cs.empty ())
      {
	const call_stack_elt& elt = cs.back ();
	retval = elt.stmt;
      }

    return retval;
  }

  void do_set_statement (tree_statement *s)
  {
    if (! cs.empty ())
      {
	call_stack_elt& elt = cs.back ();
	elt.stmt = s;
      }
  }

  Octave_map do_backtrace (size_t nskip,
			   octave_idx_type& curr_user_frame) const;

  bool do_goto_frame (size_t n, bool verbose);

  bool do_goto_frame_relative (int n, bool verbose);

  void do_goto_caller_frame (void);

  void do_goto_base_frame (void);

  void do_pop (void)
  {
    if (cs.size () > 1)
      {
	const call_stack_elt& elt = cs.back ();
	curr_frame = elt.prev;
	cs.pop_back ();
	const call_stack_elt& new_elt = cs[curr_frame];
	symbol_table::set_scope_and_context (new_elt.scope, new_elt.context);
      }
  }

  void do_clear (void) { cs.clear (); }

  void do_backtrace_error_message (void) const;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
