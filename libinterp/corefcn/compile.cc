////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023-2024 The Octave Project Developers
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

#if defined (OCTAVE_ENABLE_BYTECODE_EVALUATOR)
#  include "pt-bytecode-vm.h"
#  include "pt-bytecode-walk.h"
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

#if defined (OCTAVE_ENABLE_BYTECODE_EVALUATOR)

// If TRUE, use VM evaluator rather than tree walker.
bool V__vm_enable__ = true;

// Cleverly hidden in pt-bytecode-vm.cc to prevent inlining here
extern "C" void dummy_mark_1 (void);
extern "C" void dummy_mark_2 (void);

#endif

DEFUN (__dummy_mark_1__, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __dummy_mark_1__ ()

Dummy function that calls the C-function @code{void dummy_mark_1 (void)}
that does nothing.

This is useful for marking start and end for Callgrind analysis or as an entry
point for @code{gdb}.

@end deftypefn */)
{
#if defined (OCTAVE_ENABLE_BYTECODE_EVALUATOR)

  dummy_mark_1 ();

  return {};

#else

  err_disabled_feature ("__dummy_mark_1__", "byte-compiled functions");

#endif
}

DEFUN (__dummy_mark_2__, , ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} __dummy_mark_2__ ()

Dummy function that calls the C-function @code{void dummy_mark_2 (void)}
that does nothing.

This is useful for marking start and end for Callgrind analysis or as an entry
point for @code{gdb}.

@end deftypefn */)
{
#if defined (OCTAVE_ENABLE_BYTECODE_EVALUATOR)

  dummy_mark_2 ();

  return {};

#else

  err_disabled_feature ("__dummy_mark_2__", "byte-compiled functions");

#endif
}

DEFUN (__vm_clear_cache__, , ,
  doc: /* -*- texinfo -*-
@deftypefn {} {@var{val} =} __vm_clear_cache__ ()

Internal function.

@end deftypefn */)
{
#if defined (OCTAVE_ENABLE_BYTECODE_EVALUATOR)

  octave::load_path::signal_clear_fcn_cache ();

  return octave_value {true};

#else

  err_disabled_feature ("__vm_clear_cache__", "byte-compiled functions");

#endif
}

DEFUN (__vm_print_trace__, , ,
  doc: /* -*- texinfo -*-
@deftypefn {} {@var{print_trace} =} __vm_print_trace__ ())

Internal function.

Print a debug trace from the VM@.

Toggles on or off each call.

There has to be a breakpoint set in some file for the trace to actually print
anything.

The return value is true if a trace will be printed and false otherwise.

@end deftypefn */)
{
#if defined (OCTAVE_ENABLE_BYTECODE_EVALUATOR)

  vm::m_trace_enabled = !vm::m_trace_enabled;

  return octave_value {vm::m_trace_enabled};

#else

  err_disabled_feature ("__vm_print_trace__", "byte-compiled functions");

#endif
}

DEFUN (__ref_count__, args, ,
  doc: /* -*- texinfo -*-
@deftypefn {} {@var{count} =} __ref_count__ (@var{obj}))

Internal function.

Return the reference count for an object.

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
@deftypefn {} {@var{is_executing} =} __vm_is_executing__ ())

Internal function.

Return true if the VM is executing the function calling
@code{__vm_is_executing__ ()}, and false otherwise.

@end deftypefn */)
{
#if defined (OCTAVE_ENABLE_BYTECODE_EVALUATOR)

  auto frame = interp.get_evaluator ().get_current_stack_frame ();
  if (!frame)
    error ("Invalid current frame");

  auto caller_frame = frame->static_link ();
  if (!caller_frame)
    error ("Invalid caller frame");

  bool bytecode_running = caller_frame->is_bytecode_fcn_frame ();

  return octave_value {bytecode_running};

#else

  octave_unused_parameter (interp);

  err_disabled_feature ("__vm_is_executing__", "byte-compiled functions");

#endif
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
@item __vm_profile__ on
Start the profiler.  Any previously collected data is cleared.

@item __vm_profile__ off
Stop profiling.  The collected data can be retrieved and examined with
@code{T = profile ("info")}.

@item __vm_profile__ clear
Clear all collected profiler data.

@item __vm_profile__ resume
Restart profiling without clearing existing data.  All newly collected
statistics are added to the existing ones.

@item __vm_profile__
Toggle between profiling and printing the result of the profiler.
Clears the profiler on each print.

@item __vm_profile__ info
Print the profiler data.

Programming Note: The calling form that returns profiler data in a variable
is not implemented yet.

@end table

@end deftypefn */)
{
#if defined (OCTAVE_ENABLE_BYTECODE_EVALUATOR)

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

#else

  octave_unused_parameter (interp);
  octave_unused_parameter (args);

  err_disabled_feature ("__vm_profile__", "byte-compiled functions");

#endif
}

DEFMETHOD (__vm_print_bytecode__, interp, args, ,
  doc: /* -*- texinfo -*-
@deftypefn  {} {@var{success} =} __vm_print_bytecode__ (@var{fn_name}))
@deftypefnx {} {@var{success} =} __vm_print_bytecode__ (@var{fn_handle}))

Internal function.

Prints the bytecode of a function name or function handle, if any.

@end deftypefn */)
{
#if defined (OCTAVE_ENABLE_BYTECODE_EVALUATOR)

  int nargin = args.length ();

  if (nargin != 1)
    print_usage ();

  octave_value ov;

  if (args (0).is_string ())
    {
      std::string fn_name = args(0).string_value ();
      symbol_table& symtab = interp.get_symbol_table ();

      ov = symtab.find_function (fn_name);

      if (!ov.is_defined ())
        {
          error ("Function not defined: %s", fn_name.c_str ());
        }
    }
  else
    ov = args (0);

  octave_user_code *ufn = nullptr;
  octave_fcn_handle *h = nullptr;

  if (ov.is_function_handle ())
    {
      h = ov.fcn_handle_value ();
      if (!h)
        error ("Invalid function handle");
      ufn = h->user_function_value ();
    }
  else
   ufn = ov.user_code_value ();

  std::string fn_name = ufn->name ();

  if (!ufn || (!ufn->is_user_function () && !ufn->is_user_script ()))
    {
      error ("Function not a user function or script: %s", fn_name.c_str ());
    }

  // Nested functions need to be compiled via their parent
  bool is_nested = ufn->is_nested_function ();

  bool try_compile = !ufn->is_compiled () && V__vm_enable__ && !is_nested;

  if (try_compile && h && h->is_anonymous ())
    h->compile ();
  else if (try_compile)
    vm::maybe_compile_or_compiled (ufn, 0);
  else if (!ufn->is_compiled ())
    error ("Function not compiled: %s", fn_name.c_str ());

  if (!ufn->is_compiled ())
    error ("Function can't be compiled: %s", fn_name.c_str ());

  auto bc = ufn->get_bytecode ();

  print_bytecode (bc);

  return octave_value {true};

#else

  octave_unused_parameter (interp);
  octave_unused_parameter (args);

  err_disabled_feature ("__vm_print_bytecode__", "byte-compiled functions");

#endif
}

DEFMETHOD (__vm_is_compiled__, interp, args, ,
  doc: /* -*- texinfo -*-
@deftypefn  {} {@var{is_compiled} =} __vm_is_compiled__ (@var{fn_name})
@deftypefnx {} {@var{is_compiled} =} __vm_is_compiled__ (@var{fn_handle})

Internal function.

Returns true if the specified function name or function handle is compiled.

False otherwise.

@end deftypefn */)
{
#if defined (OCTAVE_ENABLE_BYTECODE_EVALUATOR)

  int nargin = args.length ();

  if (nargin != 1)
    print_usage ();

  std::string fcn_to_compile;
  octave_fcn_handle *handle_to_compile = nullptr;

  bool do_handle = false;

  if (args (0).is_string ())
    fcn_to_compile = args (0).string_value ();
  else if (args (0).is_function_handle ())
    {
      handle_to_compile = args (0).fcn_handle_value ();
      do_handle = true;
    }
  else
    error ("First argument need to be a function name or function handle.");

  try
    {
      if (do_handle)
        {
          octave_user_function *ufn = handle_to_compile->user_function_value ();
          if (!ufn)
            return octave_value {false};
          return octave_value {ufn->is_compiled ()};
        }
      else
        {
          std::string name = fcn_to_compile;
          symbol_table& symtab = interp.get_symbol_table ();
          octave_value ov = symtab.find_function (name);

          if (!ov.is_defined ())
            return octave_value {false};

          octave_user_code *ufn = ov.user_code_value ();
          if (!ufn)
            return octave_value {false};

          return octave_value {ufn->is_compiled ()};
        }
    }
  catch (execution_exception &)
    {
      return octave_value {false};
    }

#else

  octave_unused_parameter (interp);
  octave_unused_parameter (args);

  err_disabled_feature ("__vm_is_compiled__", "byte-compiled functions");

#endif
}

DEFMETHOD (__vm_compile__, interp, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{success} =} __vm_compile__ (@var{fn_name})
@deftypefnx {} {@var{success} =} __vm_compile__ (@var{fn_name}, "clear")
@deftypefnx {} {@var{success} =} __vm_compile__ (@var{fn_name}, "print")

Internal function.

Compile the specified function to bytecode.

The compiled function and its subfunctions will be executed
by the VM when called.

Returns true on success, otherwise false.

Don't recompile or clear the bytecode of a running function with __vm_compile__.

The @qcode{"print"} option prints the bytecode after compilation.

The @qcode{"clear"} option removes the bytecode from the function instead.

@end deftypefn */)
{
#if defined (OCTAVE_ENABLE_BYTECODE_EVALUATOR)

  int nargin = args.length ();

  if (! nargin)
    print_usage ();

  std::string fcn_to_compile;
  octave_fcn_handle *handle_to_compile = nullptr;

  bool do_clear = false;
  bool do_print = false;

  if (args (0).is_string ())
    fcn_to_compile = args (0).string_value ();
  else if (args (0).is_function_handle ())
    handle_to_compile = args (0).fcn_handle_value ();
  else
    error ("First argument need to be a function name or function handle.");

  for (int i = 1; i < nargin; i++)
    {
      auto arg = args(i);

      if (! arg.is_string())
        error ("Non string argument");

      std::string arg_s = arg.string_value ();

      if (arg_s == "clear")
        do_clear = true;

      if (arg_s == "print")
        do_print = true;
    }

  if (do_clear && handle_to_compile)
    {
      octave_user_function *ufn = handle_to_compile->user_function_value ();
      if (!ufn)
        error ("Invalid function handle");

      ufn->clear_bytecode ();

      return octave_value {true};
    }
  else if (do_clear)
    {
      std::string name = fcn_to_compile;
      symbol_table& symtab = interp.get_symbol_table ();
      octave_value ov = symtab.find_function (name);

      if (!ov.is_defined ())
        {
          error ("Function not defined: %s", name.c_str ());
        }

      octave_user_code *ufn = ov.user_code_value ();

      if (!ufn || (!ufn->is_user_function () && !ufn->is_user_script ()))
        {
          error ("Function not an user function or script: %s", name.c_str ());
        }

      ufn->clear_bytecode ();

      return octave_value {true};
    }

  if (handle_to_compile)
    {
      octave_user_function *ufn = handle_to_compile->user_function_value ();
      if (!ufn)
        error ("Invalid function handle");

      if (ufn->is_nested_function ())
        error ("Nested functions need to be compiled via their parent");

      // Anonymous functions need to be compiled via their handle
      // to get the locals.
      if (handle_to_compile->is_anonymous ())
        {
          handle_to_compile->compile ();
          if (do_print && ufn->is_compiled ())
            {
              auto bc = ufn->get_bytecode ();
              print_bytecode (bc);
            }

            return octave_value {true};
        }
      else
        {
          // Throws on errors
          compile_user_function (*ufn, do_print);

          return octave_value {true};
        }
    }
  else
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

      if (ufn->is_nested_function ())
        error ("Nested functions need to be compiled via their parent");

      // Throws on errors
      compile_user_function (*ufn, do_print);
    }

  return octave_value {true};

#else

  octave_unused_parameter (interp);
  octave_unused_parameter (args);

  err_disabled_feature ("__vm_compile__", "byte-compiled functions");

#endif
}

DEFUN (__vm_enable__, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} __vm_enable__ ()
@deftypefnx {} {@var{old_val} =} __vm_enable__ (@var{new_val})
@deftypefnx {} {@var{old_val} =} __vm_enable__ (@var{new_val}, "local")
Query or set the internal variable that determines whether Octave automatically
compiles functions to bytecode and executes them in a virtual machine (VM).

@strong{Warning:} The virtual machine feature is experimental.

The default value is false while the VM is still experimental.
Users must explicitly call @code{__vm_enable__ (1)} to use it.

When false, Octave uses a traditional tree walker to evaluate statements parsed
from m-code.  When true, Octave translates parsed statements to an intermediate
representation that is then evaluated by a virtual machine.

When called from inside a function with the @qcode{"local"} option, the setting
is changed locally for the function and any subroutines it calls.  The original
setting is restored when exiting the function.

Once compiled to bytecode, the function will always be evaluated by the VM
regardless of the state of @code{__vm_enable__}, until the bytecode is cleared
by, e.g., @qcode{"clear all"}, or a modification to the function's m-file.

@seealso{__vm_compile__}

@end deftypefn */)
{
#if defined (OCTAVE_ENABLE_BYTECODE_EVALUATOR)

  return set_internal_variable (V__vm_enable__, args, nargout,
                                "__vm_enable__");

#else

  octave_unused_parameter (args);
  octave_unused_parameter (nargout);

  err_disabled_feature ("__vm_enable__", "byte-compiled functions");

#endif
}

OCTAVE_END_NAMESPACE(octave)
