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

#include <list>
#include <string>

class octave_value;
class octave_value_list;
class octave_function;
class octave_user_script;
class tree_statement_list;
class charMatrix;

extern void
clean_up_and_exit (int) GCC_ATTR_NORETURN;

extern int main_loop (void);

extern void
do_octave_atexit (void);

// Current command to execute.
extern tree_statement_list *global_command;

// Pointer to parent function that is currently being evaluated.
extern octave_function *curr_parent_function;

// TRUE means we are ready to interpret commands, but not everything
// is ready for interactive use.
extern OCTINTERP_API bool octave_interpreter_ready;

// TRUE means we've processed all the init code and we are good to go.
extern OCTINTERP_API bool octave_initialized;

class
octave_call_stack
{
protected:

  octave_call_stack (void) : cs () { }

public:

  typedef std::list<octave_function *>::iterator iterator ;

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

  // Caller function, may be built-in.
  static octave_function *caller (void)
  {
    return instance_ok () ? instance->do_caller (): 0;
  }

  // First script on the stack.
  static octave_user_script *caller_script (void)
  {
    return instance_ok () ? instance->do_caller_user_script (): 0;
  }

  // First user-defined function on the stack.
  static octave_user_function *caller_user_function (void)
  {
    return instance_ok () ? instance->do_caller_user_function (): 0;
  }

  // First user-defined function on the stack.
  static octave_function *caller_user_script_or_function (void)
  {
    return instance_ok () ? instance->do_caller_user_script_or_function (): 0;
  }

  static void push (octave_function *f)
  {
    if (instance_ok ())
      instance->do_push (f);
  }

  static octave_function *top (void)
  {
    return instance_ok () ? instance->do_top (): 0;
  }

  static void pop (void)
  {
    if (instance_ok ())
      instance->do_pop ();
  }
  
  // A function for popping the top of the call stack that is suitable
  // for use as an unwind_protect handler.
  static void unwind_pop (void *) { pop (); }

  // A function for popping an octave_user_script from the top of the
  // call stack that is suitable for use as an unwind_protect handler.
  static void unwind_pop_script (void *);

  static void clear (void)
  {
    if (instance_ok ())
      instance->do_clear ();
  }
  
private:

  // The current call stack.
  std::list<octave_function *> cs;

  static octave_call_stack *instance;

  octave_function *do_caller (void);

  octave_user_script *do_caller_user_script (void);

  octave_user_function *do_caller_user_function (void);

  octave_function *do_caller_user_script_or_function (void);

  void do_push (octave_function *f) { cs.push_front (f); }

  octave_function *do_top (void) { return cs.empty () ? 0 : cs.front (); }

  void do_pop (void)
  {
    if (! cs.empty ())
      cs.pop_front ();
  }

  void do_clear (void) { cs.clear (); }
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
