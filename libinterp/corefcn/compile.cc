////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023 The Octave Project Developers
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

#include "ovl.h"
#include "ov.h"
#include "defun.h"
#include "variables.h"
#include "interpreter.h"

#include "pt-bytecode-vm.h"
#include "pt-bytecode-walk.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Cleverly hidden in pt-bytecode-vm.cc to prevent inlining here
extern "C" void dummy_mark_1 (void);
extern "C" void dummy_mark_2 (void);

DEFUN (__dummy_mark_1__, , ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} __dummy_mark_1__ ()

Dummy function that calls the c-function void dummy_mark_1 (void)
that does nothing.

Usefull for e.g. marking start and end for Callgrind analyzis
or as an entry point for gdb.

@end deftypefn */)
{
  dummy_mark_1 ();

  return {};
}

DEFUN (__dummy_mark_2__, , ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} __dummy_mark_2__ ()

Dummy function that calls the c-function void dummy_mark_2 (void)
that does nothing.

Usefull for e.g. marking start and end for Callgrind analyzis
or as an entry point for gdb.

@end deftypefn */)
{
  dummy_mark_2 ();

  return {};
}

DEFUN (__vm_clear_cache__, , ,
  doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} __vm_clear_cache__ ()

Internal function.

@end deftypefn */)
{
  octave::load_path::signal_clear_fcn_cache ();

  return octave_value {true};
}

DEFUN (__vm_print_trace__, , ,
  doc: /* -*- texinfo -*-
@deftypefn  {} {@var{prints_trace} =} __vm_print_trace__ ())

Internal function.

Print a debug trace from the VM. Toggles on or off each call.

There has to be a breakpoint set in some file for the trace
to actually print anything.

Returns true if a trace will be printed from now on, false otherwise.

@end deftypefn */)
{
  vm::m_trace_enabled = !vm::m_trace_enabled;

  return octave_value {vm::m_trace_enabled};
}

DEFUN (__ref_count__, args, ,
  doc: /* -*- texinfo -*-
@deftypefn  {} {@var{count} =} __ref_count__ (@var{obj}))

Internal function.

Returns reference count for an object.

@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin != 1)
    print_usage ();

  octave_value ov = args (0);

  return octave_value {ov.get_count ()};
}

DEFMETHOD (__vm_is_executing__, interp, , ,
  doc: /* -*- texinfo -*-
@deftypefn  {} {@var{is_executing} =} __vm_is_executing__ ())

Internal function.

Returns true if the VM is executing the function calling __vm_is_executing__ ().

False otherwise.

@end deftypefn */)
{
  bool bytecode_running = interp.get_evaluator ().get_current_stack_frame ()->is_bytecode_fcn_frame ();

  return octave_value {bytecode_running};
}

DEFMETHOD (__vm_profile__, interp, args, ,
  doc: /* -*- texinfo -*-
@deftypefn  {} {} __vm_profile__ on
@deftypefnx {} {} __vm_profile__ off
@deftypefnx {} {} __vm_profile__ resume
@deftypefnx {} {} __vm_profile__ clear
@deftypefnx {} {@var{T} =} __vm_profile__ ("info")
@deftypefnx {} {} __vm_profile__

Internal function.

Profile code running in the VM.

@table @code
@item profile on
Start the profiler, clearing all previously collected data if there is any.

@item profile off
Stop profiling.  The collected data can later be retrieved and examined
with @code{T = profile ("info")}.

@item profile clear
Clear all collected profiler data.

@item profile resume
Restart profiling without clearing the old data.  All newly collected
statistics are added to the existing ones.

@item profile
Toggles between profiling and printing the result of the profiler.
Clears the profiler on each print.

@item info
Prints the profiler data.

Not that output to a variable is not implemented yet.

@end table

@end deftypefn */)
{
  int nargin = args.length ();

  auto &evaler = interp.get_evaluator ();

  std::string arg0;

  if (nargin >= 1)
   arg0 = args (0).string_value ();

  if (!arg0.size ())
    {
      if (!vm::m_vm_profiler)
        {
          vm::m_vm_profiler = std::make_shared<vm_profiler> ();

          vm::m_profiler_enabled = true;
          evaler.vm_set_profiler_active (true);
        }
      else
        {
          evaler.vm_set_profiler_active (false);
          vm::m_profiler_enabled = false;
          auto p = vm::m_vm_profiler;
          vm::m_vm_profiler = nullptr;

          auto cpy = *p;
          cpy.print_to_stdout ();
        }
    }
  else if (arg0 == "on")
    {
      vm::m_profiler_enabled = false;
      vm::m_vm_profiler = std::make_shared<vm_profiler> ();
      vm::m_profiler_enabled = true;
      evaler.vm_set_profiler_active (true);
    }
  else if (arg0 == "resume")
    {
      if (!vm::m_vm_profiler)
        vm::m_vm_profiler = std::make_shared<vm_profiler> ();

      vm::m_profiler_enabled = true;
      evaler.vm_set_profiler_active (true);
    }
  else if (arg0 == "off")
    {
      evaler.vm_set_profiler_active (false);
      vm::m_profiler_enabled = false;
    }
  else if (arg0 == "clear")
    {
      evaler.vm_set_profiler_active (false);
      vm::m_profiler_enabled = false;
      vm::m_vm_profiler = nullptr;
    }
  else if (arg0 == "info")
    {
      auto p_vm_profiler = vm::m_vm_profiler;
      if (p_vm_profiler)
        {
          auto cpy = *p_vm_profiler;
          cpy.print_to_stdout ();
        }
      else
        warning ("Nothing recorded.");
    }
  else
    print_usage ();

  return octave_value {true};
}

DEFMETHOD (__print_bytecode__, interp, args, ,
  doc: /* -*- texinfo -*-
@deftypefn  {} {@var{success} =} __print_bytecode__ (@var{fn_name}))

Internal function.

Prints the bytecode of a function, if any.

@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin != 1)
    print_usage ();

  std::string fn_name = args(0).string_value ();
  symbol_table& symtab = interp.get_symbol_table ();

  octave_value ov = symtab.find_function (fn_name);

  if (!ov.is_defined ())
    {
      error ("Function not defined: %s", fn_name.c_str ());
    }

  octave_user_function *ufn = ov.user_function_value ();

  if (!ufn || (!ufn->is_user_function () && !ufn->is_user_script ()))
    {
      error ("Function not a user function or script: %s", fn_name.c_str ());
    }

  if (!ufn->is_compiled () && V__enable_vm_eval__)
    compile_user_function (*ufn, 0);
  else if (!ufn->is_compiled ())
    error ("Function not compiled: %s", fn_name.c_str ());

  if (!ufn->is_compiled ())
    error ("Function can't be compiled: %s", fn_name.c_str ());

  auto bc = ufn->get_bytecode ();

  print_bytecode (bc);

  return octave_value {true};
}

DEFMETHOD (__compile__, interp, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{success} =} __compile__ (@var{fn_name})
@deftypefnx  {} {@var{success} =} __compile__ (@var{fn_name}, "clear")
@deftypefnx  {} {@var{success} =} __compile__ (@var{fn_name}, "print")

Compile the specified function to bytecode.

The compiled function and its subfunctions will be executed
by the VM when called.

Returns true on success, otherwise false.

The @qcode{"print"} option prints the bytecode after compilation.

The @qcode{"clear"} option removes the bytecode from the function instead.

@end deftypefn */)
{
  int nargin = args.length ();

  if (! nargin)
    print_usage ();

  std::string fcn_to_compile;
  bool do_clear = false;
  bool do_print = false;

  for (int i = 0; i < nargin; i++)
    {
      auto arg = args(i);

      if (! arg.is_string())
        error ("Non string argument");

      std::string arg_s = arg.string_value ();

      if (i == 0)
        {
          fcn_to_compile = arg_s;
          continue;
        }

      if (arg_s == "clear")
        do_clear = true;

      if (arg_s == "print")
        do_print = true;
    }

  if (do_clear)
    {
      std::string name = fcn_to_compile;
      symbol_table& symtab = interp.get_symbol_table ();
      octave_value ov = symtab.find_function (name);

      if (!ov.is_defined ())
        {
          error ("Function not defined: %s", name.c_str ());
        }

      octave_user_function *ufn = ov.user_function_value ();

      if (!ufn || !ufn->is_user_function ())
        {
          error ("Function not a user function: %s", name.c_str ());
        }

      ufn->clear_bytecode ();

      return octave_value {true};
    }


  {
    std::string name = fcn_to_compile;
    symbol_table& symtab = interp.get_symbol_table ();
    octave_value ov = symtab.find_function (name);

    if (!ov.is_defined ())
      {
        error ("Function not defined: %s", name.c_str ());
      }

    if (!ov.is_user_function () && !ov.is_user_script ())
      {
        error ("Function is not a user function or script: %s", name.c_str ());
      }

    octave_user_code *ufn = ov.user_code_value ();

    if (!ufn || (!ufn->is_user_function () && !ufn->is_user_script ()))
      {
        error ("Function is not really user function or script: %s", name.c_str ());
      }

    // Throws on errors
    compile_user_function (*ufn, do_print);
  }

  return octave_value {true};
}

// If TRUE, use VM evaluator rather than tree walker.
// FIXME: Use OCTAVE_ENABLE_VM_EVALUATOR define to set it to true when
// the VM has been tested properly.
bool V__enable_vm_eval__ = false;

DEFUN (__enable_vm_eval__, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} __enable_vm_eval__ ()
@deftypefnx {} {@var{old_val} =} __enable_vm_eval__ (@var{new_val})
@deftypefnx {} {@var{old_val} =} __enable_vm_eval__ (@var{new_val}, "local")
Query or set whether Octave automatically compiles functions to bytecode
and executes them in a virtual machine (VM).

Note that the virtual machine feature is experimental.

The default value is currently false, while the VM is still experimental.
Users need to explicitly call @code{__enable_vm_eval__ (1)} to enable it.
In future, this will be set to the value of  the OCTAVE_ENABLE_VM_EVALUATOR
flag that was set when building Octave.

When false, Octave uses a traditional tree walker
to evaluate statements parsed from m-code.  When true, Octave translates parsed
statements to an intermediate representation that is then evaluated by a
virtual machine.

When called from inside a function with the @qcode{"local"} option, the setting
is changed locally for the function and any subroutines it calls.  The original
setting is restored when exiting the function.

Once compiled to bytecode, the function will always be evaluated by the
VM no matter the state of @qcode{"__enable_vm_eval__"}, until the bytecode is
cleared, by e.g. @qcode{"clear all"} or an modification to the
function's m-file.

@seealso{__compile__}

@end deftypefn */)
{
  return set_internal_variable (V__enable_vm_eval__, args, nargout,
                                "__enable_vm_eval__");
}

OCTAVE_END_NAMESPACE(octave)
