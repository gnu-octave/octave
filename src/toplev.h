/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 2000, 2001, 2002,
              2003, 2004, 2005, 2006, 2007 John W. Eaton

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

#include "input.h"
#include "oct-map.h"

extern OCTINTERP_API void
clean_up_and_exit (int) GCC_ATTR_NORETURN;

extern OCTINTERP_API void recover_from_exception (void);

extern int main_loop (void);

extern OCTINTERP_API void
do_octave_atexit (void);

extern OCTINTERP_API void
octave_add_atexit_function (const std::string& fname);

extern OCTINTERP_API bool
octave_remove_atexit_function (const std::string& fname);

// Current command to execute.
extern OCTINTERP_API tree_statement_list *global_command;

// Pointer to parent function that is currently being evaluated.
extern OCTINTERP_API octave_function *curr_parent_function;

// TRUE means we are ready to interpret commands, but not everything
// is ready for interactive use.
extern OCTINTERP_API bool octave_interpreter_ready;

// TRUE means we've processed all the init code and we are good to go.
extern OCTINTERP_API bool octave_initialized;

class
octave_call_stack
{
private:

  struct call_stack_elt
  {
    call_stack_elt (octave_function *f, symbol_table::scope_id s,
		    symbol_table::context_id c)
      : fcn (f), stmt (0), scope (s), context (c) { }

    octave_function *fcn;
    tree_statement *stmt;
    symbol_table::scope_id scope;
    symbol_table::context_id context;
  };

protected:

  octave_call_stack (void) : cs (), curr_frame (0) { }

public:

  typedef std::deque<call_stack_elt>::iterator iterator;
  typedef std::deque<call_stack_elt>::const_iterator const_iterator;
  typedef std::deque<call_stack_elt>::difference_type difference_type;

  static bool instance_ok (void)
  {
    bool retval = true;

    if (! instance)
      instance = new octave_call_stack ();

    if (! instance)
      {
	::error ("unable to create call stack object!");

	retval = false;
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
  static int caller_user_code_line (difference_type q = 0)
  {
    return instance_ok () ? instance->do_caller_user_code_line (q) : -1;
  }

  // Column in user code caller.
  static int caller_user_code_column (difference_type q = 0)
  {
    return instance_ok () ? instance->do_caller_user_code_column (q) : -1;
  }

  // Caller function, may be built-in.
  static octave_function *caller (void)
  {
    return element (1);
  }

  static size_t current_frame (void)
  {
    return instance_ok () ? instance->do_current_frame () : 0;
  }

  // Function at location N on the call stack (N == 0 is current), may
  // be built-in.
  static octave_function *element (size_t n)
  {
    return instance_ok () ? instance->do_element (n) : 0;
  }
  
  // First script on the stack.
  static octave_user_script *caller_script (difference_type q = 0)
  {
    return instance_ok () ? instance->do_caller_user_script (q) : 0;
  }

  // First user-defined function on the stack.
  static octave_user_function *caller_user_function (difference_type q = 0)
  {
    return instance_ok () ? instance->do_caller_user_function (q) : 0;
  }

  // First user-defined function on the stack.
  static octave_user_code *caller_user_code (difference_type q = 0)
  {
    return instance_ok () ? instance->do_caller_user_code (q) : 0;
  }

  static void
  push (octave_function *f,
	symbol_table::scope_id scope = symbol_table::current_scope (),
	symbol_table::context_id context = 0)
  {
    if (instance_ok ())
      instance->do_push (f, scope, context);
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

  static Octave_map backtrace (int n = 0)
  {
    return instance_ok () ? instance->do_backtrace (n) : Octave_map ();
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

private:

  // The current call stack.
  std::deque<call_stack_elt> cs;

  size_t curr_frame;

  static octave_call_stack *instance;

  int do_current_line (void) const;

  int do_current_column (void) const;

  int do_caller_user_code_line (difference_type q = 0) const;

  int do_caller_user_code_column (difference_type q = 0) const;

  size_t do_current_frame (void) { return curr_frame; }

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

  octave_user_script *do_caller_user_script (difference_type q = 0) const;

  octave_user_function *do_caller_user_function (difference_type q = 0) const;

  octave_user_code *do_caller_user_code (difference_type q = 0) const; 

  void do_push (octave_function *f, symbol_table::scope_id scope,
		symbol_table::context_id context)
  {
    if (Vdebugging)
      curr_frame++;

    cs.push_front (call_stack_elt (f, scope, context));
  }

  octave_function *do_top (void) const
  {
    octave_function *retval = 0;

    if (! cs.empty ())
      {
	const call_stack_elt& elt = cs.front ();
	retval = elt.fcn;
      }

    return retval;
  }

  tree_statement *do_top_statement (void) const
  {
    tree_statement *retval = 0;

    if (! cs.empty ())
      {
	const call_stack_elt& elt = cs.front ();
	retval = elt.stmt;
      }

    return retval;
  }

  void do_set_statement (tree_statement *s)
  {
    if (! cs.empty ())
      {
	call_stack_elt& elt = cs.front ();
	elt.stmt = s;
      }
  }

  Octave_map do_backtrace (int n) const;

  bool do_goto_frame (size_t n, bool verbose);

  bool do_goto_frame_relative (int n, bool verbose);

  void do_pop (void)
  {
    if (! cs.empty ())
      {
	if (Vdebugging)
	  curr_frame--;

	cs.pop_front ();
      }
  }

  void do_clear (void) { cs.clear (); }
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
