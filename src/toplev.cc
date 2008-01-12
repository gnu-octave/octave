/*

Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003,
              2004, 2005, 2006, 2007 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>
#include <cerrno>
#include <cstdlib>
#include <cstring>
#include <new>

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "cmd-edit.h"
#include "file-ops.h"
#include "lo-error.h"
#include "lo-mappers.h"
#include "oct-env.h"
#include "quit.h"
#include "str-vec.h"

#include <defaults.h>
#include "defun.h"
#include "error.h"
#include "file-io.h"
#include "input.h"
#include "lex.h"
#include <oct-conf.h>
#include "oct-hist.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "pager.h"
#include "parse.h"
#include "pathsearch.h"
#include "procstream.h"
#include "ov.h"
#include "pt-jump.h"
#include "pt-stmt.h"
#include "sighandlers.h"
#include "sysdep.h"
#include "syswait.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include <version.h>

// TRUE means we are exiting via the builtin exit or quit functions.
static bool quitting_gracefully = false;

// TRUE means we are ready to interpret commands, but not everything
// is ready for interactive use.
bool octave_interpreter_ready = false;

// TRUE means we've processed all the init code and we are good to go.
bool octave_initialized = false;

// Current command to execute.
tree_statement_list *global_command = 0;

// Pointer to parent function that is currently being evaluated.
octave_function *curr_parent_function = 0;

octave_call_stack *octave_call_stack::instance = 0;

octave_function *
octave_call_stack::do_caller (void)
{
  octave_function *retval = 0;

  if (cs.size () > 1)
    {
      iterator p = cs.begin ();
      retval = *++p;
    }

  return retval;
}

octave_user_script *
octave_call_stack::do_caller_user_script (void)
{
  octave_user_script *retval = 0;

  for (iterator p = cs.begin (); p != cs.end (); p++)
    {
      octave_function *f = *p;

      if (f && f->is_user_script ())
	{
	  retval = dynamic_cast<octave_user_script *> (f);
	  break;
	}
    }

  return retval;
}

octave_user_function *
octave_call_stack::do_caller_user_function (void)
{
  octave_user_function *retval = 0;

  for (iterator p = cs.begin (); p != cs.end (); p++)
    {
      octave_function *f = *p;

      if (f && f->is_user_function ())
	{
	  retval = dynamic_cast<octave_user_function *> (f);
	  break;
	}
    }

  return retval;
}

octave_function *
octave_call_stack::do_caller_user_script_or_function (void)
{
  octave_function *retval = 0;

  for (iterator p = cs.begin (); p != cs.end (); p++)
    {
      octave_function *f = *p;

      if (f && (f->is_user_script () || f->is_user_function ()))
	{
	  retval = f;
	  break;
	}
    }

  return retval;
}

void
octave_call_stack::unwind_pop_script (void *)
{
  octave_function *f = top ();
  pop ();
  assert (f && f->is_user_script ());
  delete f;
}

void
recover_from_exception (void)
{
  can_interrupt = true;
  octave_interrupt_immediately = 0;
  octave_interrupt_state = 0;
  octave_signal_caught = 0;
  octave_allocation_error = 0;
  octave_restore_signal_mask ();
  octave_catch_interrupts ();
}

int
main_loop (void)
{
  octave_save_signal_mask ();

  can_interrupt = true;

  octave_signal_hook = octave_signal_handler;
  octave_interrupt_hook = unwind_protect::run_all;
  octave_bad_alloc_hook = unwind_protect::run_all;

  octave_catch_interrupts ();

  octave_initialized = true;

  // The big loop.

  int retval = 0;
  do
    {
      try
	{
	  symbol_table::reset_scope ();

	  reset_error_handler ();

	  reset_parser ();

	  // This is the same as yyparse in parse.y.
	  retval = octave_parse ();

	  if (retval == 0)
	    {
	      if (global_command)
		{
		  global_command->eval ();

		  delete global_command;

		  global_command = 0;

		  OCTAVE_QUIT;

		  if (! (interactive || forced_interactive))
		    {
		      bool quit = (tree_return_command::returning
				   || tree_break_command::breaking);

		      if (tree_return_command::returning)
			tree_return_command::returning = 0;

		      if (tree_break_command::breaking)
			tree_break_command::breaking--;

		      if (quit)
			break;
		    }

		  if (error_state)
		    {
		      if (! (interactive || forced_interactive))
			{
			  // We should exit with a non-zero status.
			  retval = 1;
			  break;
			}
		    }
		  else
		    {
		      if (octave_completion_matches_called)
			octave_completion_matches_called = false;	    
		      else
			command_editor::increment_current_command_number ();
		    }
		}
	      else if (parser_end_of_input)
		break;
	    }
	}
      catch (octave_interrupt_exception)
	{
	  recover_from_exception ();
	  octave_stdout << "\n";
	}
      catch (std::bad_alloc)
	{
	  recover_from_exception ();
	  std::cerr
	    << "error: memory exhausted or requested size too large for range of Octave's index type -- trying to return to prompt"
	    << std::endl;
	}
    }
  while (retval == 0);

  return retval;
}

// Fix up things before exiting.

void
clean_up_and_exit (int retval)
{
  do_octave_atexit ();

  sysdep_cleanup ();

  exit (retval == EOF ? 0 : retval);
}

DEFUN (quit, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} exit (@var{status})\n\
@deftypefnx {Built-in Function} {} quit (@var{status})\n\
Exit the current Octave session.  If the optional integer value\n\
@var{status} is supplied, pass that value to the operating system as the\n\
Octave's exit status.  The default value is zero.\n\
@end deftypefn")
{
  octave_value_list retval;

  if (nargout == 0)
    {
      int exit_status = 0;

      quitting_gracefully = true;

      if (args.length () > 0)
	{
	  int tmp = args(0).nint_value ();

	  if (! error_state)
	    exit_status = tmp;
	}

      clean_up_and_exit (exit_status);
    }
  else
    error ("quit: invalid number of output arguments");

  return retval;
}

DEFALIAS (exit, quit);

DEFUN (warranty, , ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} warranty ()\n\
Describe the conditions for copying and distributing Octave.\n\
@end deftypefn")
{
  octave_value_list retval;

  octave_stdout << "\n" \
    OCTAVE_NAME_VERSION_AND_COPYRIGHT "\n\
\n\
This program is free software; you can redistribute it and/or modify\n\
it under the terms of the GNU General Public License as published by\n\
the Free Software Foundation; either version 3 of the License, or\n\
(at your option) any later version.\n\
\n\
This program is distributed in the hope that it will be useful,\n\
but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
GNU General Public License for more details.\n\
\n\
You should have received a copy of the GNU General Public License\n\
along with this program.  If not, see <http://www.gnu.org/licenses/>.\n\
\n";

  return retval;
}

// Execute a shell command.

static void
cleanup_iprocstream (void *p)
{
  iprocstream *cmd = static_cast<iprocstream *> (p);

  octave_child_list::remove (cmd->pid ());

  delete cmd;
}

static int
wait_for_input (int fid)
{
  int retval = -1;

#if defined (HAVE_SELECT)
  if (fid >= 0)
    {
      fd_set set;

      FD_ZERO (&set);
      FD_SET (fid, &set);

      retval = select (FD_SETSIZE, &set, 0, 0, 0);
    }
#else
  retval = 1;
#endif

  return retval;
}

static octave_value_list
run_command_and_return_output (const std::string& cmd_str)
{
  octave_value_list retval;

  iprocstream *cmd = new iprocstream (cmd_str.c_str ());

  if (cmd)
    {
      unwind_protect::add (cleanup_iprocstream, cmd);

      if (*cmd)
	{
	  int fid = cmd->file_number ();

	  std::ostringstream output_buf;

	  char ch;

	  for (;;)
	    {
	      if (cmd->get (ch))
		output_buf.put (ch);
	      else
		{
		  if (! cmd->eof () && errno == EAGAIN)
		    {
		      cmd->clear ();

		      if (wait_for_input (fid) != 1)
			break;			
		    }
		  else
		    break;
		}
	    }

	  int cmd_status = cmd->close ();

	  if (WIFEXITED (cmd_status))
	    cmd_status = WEXITSTATUS (cmd_status);
	  else
	    cmd_status = 127;

	  retval(0) = (double) cmd_status;
	  retval(1) = output_buf.str ();
	}

      unwind_protect::run ();
    }
  else
    error ("unable to start subprocess for `%s'", cmd_str.c_str ());

  return retval;
}

enum system_exec_type { et_sync, et_async };

DEFUN (system, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} system (@var{string}, @var{return_output}, @var{type})\n\
Execute a shell command specified by @var{string}.  The second\n\
argument is optional.  If @var{type} is @code{\"async\"}, the process\n\
is started in the background and the process id of the child process\n\
is returned immediately.  Otherwise, the process is started, and\n\
Octave waits until it exits.  If @var{type} argument is omitted, a\n\
value of @code{\"sync\"} is assumed.\n\
\n\
If two input arguments are given (the actual value of\n\
@var{return_output} is irrelevant) and the subprocess is started\n\
synchronously, or if @var{system} is called with one input argument and\n\
one or more output arguments, the output from the command is returned.\n\
Otherwise, if the subprocess is executed synchronously, its output is\n\
sent to the standard output.  To send the output of a command executed\n\
with @var{system} through the pager, use a command like\n\
\n\
@example\n\
disp (system (cmd, 1));\n\
@end example\n\
\n\
@noindent\n\
or\n\
\n\
@example\n\
printf (\"%s\n\", system (cmd, 1));\n\
@end example\n\
\n\
The @code{system} function can return two values.  The first is the\n\
exit status of the command and the second is any output from the\n\
command that was written to the standard output stream.  For example,\n\
\n\
@example\n\
[status, output] = system (\"echo foo; exit 2\");\n\
@end example\n\
\n\
@noindent\n\
will set the variable @code{output} to the string @samp{foo}, and the\n\
variable @code{status} to the integer @samp{2}.\n\
@end deftypefn")
{
  octave_value_list retval;

  unwind_protect::begin_frame ("Fsystem");

  int nargin = args.length ();

  if (nargin > 0 && nargin < 4)
    {
      bool return_output = (nargout > 1 || nargin > 1);

      std::string cmd_str = args(0).string_value ();

      system_exec_type type = et_sync;

      if (! error_state)
	{
	  if (nargin > 2)
	    {
	      std::string type_str = args(2).string_value ();

	      if (! error_state)
		{
		  if (type_str == "sync")
		    type = et_sync;
		  else if (type_str == "async")
		    type = et_async;
		  else
		    error ("system: third arg must be \"sync\" or \"async\"");
		}
	      else
		error ("system: third argument must be a string");
	    }
	}
      else
	error ("system: expecting std::string as first argument");

      if (! error_state)
	{
#if defined (__WIN32__) && ! defined (__CYGWIN__)
	  // Work around weird double-quote handling on Windows systems.
          if (type == et_sync)
            cmd_str = "\"" + cmd_str + "\"";
#endif

	  if (type == et_async)
	    {
	      // FIXME -- maybe this should go in sysdep.cc?
#ifdef HAVE_FORK
	      pid_t pid = fork ();

	      if (pid < 0) 
		error ("system: fork failed -- can't create child process");
	      else if (pid == 0)
		{
		  // FIXME -- should probably replace this
		  // call with something portable.

		  execl ("/bin/sh", "sh", "-c", cmd_str.c_str (),
			 static_cast<void *> (0));

		  panic_impossible ();
		}
	      else
		retval(0) = pid;
#elif defined (__WIN32__)
              STARTUPINFO si;
              PROCESS_INFORMATION pi;
              ZeroMemory (&si, sizeof (si));
              ZeroMemory (&pi, sizeof (pi));
	      OCTAVE_LOCAL_BUFFER (char, xcmd_str, cmd_str.length()+1);
	      strcpy (xcmd_str, cmd_str.c_str ());

              if (! CreateProcess (0, xcmd_str, 0, 0, FALSE, 0, 0, 0, &si, &pi))
                error ("system: CreateProcess failed -- can't create child process");
              else
                {
                  retval(0) = pi.dwProcessId;
                  CloseHandle (pi.hProcess);
                  CloseHandle (pi.hThread);
                }
#else
 	      error ("asynchronous system calls are not supported");
#endif
	    }
	  else if (return_output)
	    retval = run_command_and_return_output (cmd_str);
	  else
	    {
	      int status = system (cmd_str.c_str ());

	      // The value in status is as returned by waitpid.  If
	      // the process exited normally, extract the actual exit
	      // status of the command.  Otherwise, return 127 as a
	      // failure code.

	      if (WIFEXITED (status))
		status = WEXITSTATUS (status);

	      retval(0) = status;
	    }
	}
    }
  else
    print_usage ();

  unwind_protect::run_frame ("Fsystem");

  return retval;
}

DEFALIAS (shell_cmd, system);

// FIXME -- this should really be static, but that causes
// problems on some systems.
std::list<std::string> octave_atexit_functions;

void
do_octave_atexit (void)
{
  static bool deja_vu = false;

  while (! octave_atexit_functions.empty ())
    {
      std::string fcn = octave_atexit_functions.front ();

      octave_atexit_functions.pop_front ();

      reset_error_handler ();

      feval (fcn, octave_value_list (), 0);

      flush_octave_stdout ();
    }

  if (! deja_vu)
    {
      deja_vu = true;

      // Do this explicitly so that destructors for mex file objects
      // are called, so that functions registered with mexAtExit are
      // called.
      clear_mex_functions ();

      command_editor::restore_terminal_state ();

      // FIXME -- is this needed?  Can it cause any trouble?
      raw_mode (0);

      octave_history_write_timestamp ();

      command_history::clean_up_and_save ();

      close_files ();

      cleanup_tmp_files ();

      flush_octave_stdout ();

      if (! quitting_gracefully && (interactive || forced_interactive))
	{
	  octave_stdout << "\n";

	  // Yes, we want this to be separate from the call to
	  // flush_octave_stdout above.

	  flush_octave_stdout ();
	}
    }
}

DEFUN (atexit, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} atexit (@var{fcn})\n\
Register a function to be called when Octave exits.  For example,\n\
\n\
@example\n\
@group\n\
function bye_bye ()\n\
  disp (\"Bye bye\");\n\
endfunction\n\
atexit (\"bye_bye\");\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
will print the message \"Bye bye\" when Octave exits.\n\
\n\
@deftypefnx {Built-in Function} {} atexit (@var{fcn}, @var{flag})\n\
Register or unregister a function to be called when Octave exits,\n\
depending on @var{flag}.  If @var{flag} is true, the function is\n\
registered, if @var{flag} is false, it is unregistered.  For example,\n\
after registering the function @code{bye_bye} as above,\n\
\n\
@example\n\
atexit (\"bye_bye\", false);\n\
@end example\n\
\n\
@noindent\n\
will remove the function from the list and Octave will not call\n\
the function @code{bye_by} when it exits.\n\
\n\
Note that @code{atexit} only removes the first occurrence of a function\n\
from the list, so if a function was placed in the list multiple\n\
times with @code{atexit}, it must also be removed from the list\n\
multiple times.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      std::string arg = args(0).string_value ();

      if (! error_state)
        {
          bool add_mode = true;

          if (nargin == 2)
            {
              add_mode = args(1).bool_value ();

              if (error_state)
                error ("atexit: second argument must be a logical value");
            }

          if (! error_state)
	    {
	      if (add_mode)
		octave_atexit_functions.push_front (arg);
	      else
		{
		  bool found = false;
		  std::list<std::string>::iterator it;

		  for (std::list<std::string>::iterator p = octave_atexit_functions.begin ();
		       p != octave_atexit_functions.end (); p++)
		    {
		      if (*p == arg)
			{
			  octave_atexit_functions.erase (p);
			  found = true;
			  break;
			}
		    }

		  if (nargout > 0)
		    retval(0) = found;
		}
	    }
	}
      else
        error ("atexit: argument must be a string");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (octave_config_info, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} octave_config_info (@var{option})\n\
Return a structure containing configuration and installation\n\
information for Octave.\n\
\n\
if @var{option} is a string, return the configuration information for the\n\
specified option.\n\
\n\
@end deftypefn")
{
  octave_value retval;

#if defined (ENABLE_DYNAMIC_LINKING)
  bool octave_supports_dynamic_linking = true;
#else
  bool octave_supports_dynamic_linking = false;
#endif

  static bool initialized = false;
  static Octave_map m;

  struct conf_info_struct
  {
    bool subst_home;
    const char *key;
    const char *val;
  };

  static const conf_info_struct conf_info[] =
    {
      { false, "ALL_CFLAGS", OCTAVE_CONF_ALL_CFLAGS },
      { false, "ALL_CXXFLAGS", OCTAVE_CONF_ALL_CXXFLAGS },
      { false, "ALL_FFLAGS", OCTAVE_CONF_ALL_FFLAGS },
      { false, "ALL_LDFLAGS", OCTAVE_CONF_ALL_LDFLAGS },
      { false, "AR", OCTAVE_CONF_AR },
      { false, "ARFLAGS", OCTAVE_CONF_ARFLAGS },
      { false, "BLAS_LIBS", OCTAVE_CONF_BLAS_LIBS },
      { false, "CC", OCTAVE_CONF_CC },
      { false, "CC_VERSION", OCTAVE_CONF_CC_VERSION },
      { false, "CFLAGS", OCTAVE_CONF_CFLAGS },
      { false, "CPICFLAG", OCTAVE_CONF_CPICFLAG },
      { false, "CPPFLAGS", OCTAVE_CONF_CPPFLAGS },
      { false, "CURL_LIBS", OCTAVE_CONF_CURL_LIBS },
      { false, "CXX", OCTAVE_CONF_CXX },
      { false, "CXXCPP", OCTAVE_CONF_CXXCPP },
      { false, "CXXFLAGS", OCTAVE_CONF_CXXFLAGS },
      { false, "CXXPICFLAG", OCTAVE_CONF_CXXPICFLAG },
      { false, "CXX_VERSION", OCTAVE_CONF_CXX_VERSION },
      { false, "DEFAULT_PAGER", OCTAVE_DEFAULT_PAGER },
      { false, "DEFS", OCTAVE_CONF_DEFS },
      { false, "DL_LD", OCTAVE_CONF_DL_LD },
      { false, "DL_LDFLAGS", OCTAVE_CONF_DL_LDFLAGS },
      { false, "ENABLE_DYNAMIC_LINKING", OCTAVE_CONF_ENABLE_DYNAMIC_LINKING },
      { false, "EXEEXT", OCTAVE_CONF_EXEEXT },
      { false, "F2C", OCTAVE_CONF_F2C },
      { false, "F2CFLAGS", OCTAVE_CONF_F2CFLAGS },
      { false, "F77", OCTAVE_CONF_F77 },
      { false, "F77_FLOAT_STORE_FLAG", OCTAVE_CONF_F77_FLOAT_STORE_FLAG },
      { false, "FC", OCTAVE_CONF_FC },
      { false, "FFLAGS", OCTAVE_CONF_FFLAGS },
      { false, "FFTW_LIBS", OCTAVE_CONF_FFTW_LIBS },
      { false, "FLIBS", OCTAVE_CONF_FLIBS },
      { false, "FPICFLAG", OCTAVE_CONF_FPICFLAG },
      { false, "GLPK_LIBS", OCTAVE_CONF_GLPK_LIBS },
      { false, "GNUPLOT", OCTAVE_CONF_GNUPLOT },
      { false, "INCFLAGS", OCTAVE_CONF_INCFLAGS },
      { false, "LDFLAGS", OCTAVE_CONF_LDFLAGS },
      { false, "LD_CXX", OCTAVE_CONF_LD_CXX },
      { false, "LD_STATIC_FLAG", OCTAVE_CONF_LD_STATIC_FLAG },
      { false, "LEX", OCTAVE_CONF_LEX },
      { false, "LEXLIB", OCTAVE_CONF_LEXLIB },
      { false, "LFLAGS", OCTAVE_CONF_LFLAGS },
      { false, "LIBCRUFT", OCTAVE_CONF_LIBCRUFT },
      { false, "LIBEXT", OCTAVE_CONF_LIBEXT },
      { false, "LIBFLAGS", OCTAVE_CONF_LIBFLAGS },
      { false, "LIBOCTAVE", OCTAVE_CONF_LIBOCTAVE },
      { false, "LIBOCTINTERP", OCTAVE_CONF_LIBOCTINTERP },
      { false, "LIBREADLINE", OCTAVE_CONF_LIBREADLINE },
      { false, "LIBS", OCTAVE_CONF_LIBS },
      { false, "LN_S", OCTAVE_CONF_LN_S },
      { false, "MKOCTFILE_DL_LDFLAGS", OCTAVE_CONF_MKOCTFILE_DL_LDFLAGS },
      { false, "RANLIB", OCTAVE_CONF_RANLIB },
      { false, "RDYNAMIC_FLAG", OCTAVE_CONF_RDYNAMIC_FLAG },
      { false, "RLD_FLAG", OCTAVE_CONF_RLD_FLAG },
      { false, "SED", OCTAVE_CONF_SED },
      { false, "SHARED_LIBS", OCTAVE_CONF_SHARED_LIBS },
      { false, "SHLEXT", OCTAVE_CONF_SHLEXT },
      { false, "SHLEXT_VER", OCTAVE_CONF_SHLEXT_VER },
      { false, "SH_LD", OCTAVE_CONF_SH_LD },
      { false, "SH_LDFLAGS", OCTAVE_CONF_SH_LDFLAGS },
      { false, "SONAME_FLAGS", OCTAVE_CONF_SONAME_FLAGS },
      { false, "STATIC_LIBS", OCTAVE_CONF_STATIC_LIBS },
      { false, "UGLY_DEFS", OCTAVE_CONF_UGLY_DEFS },
      { false, "USE_64_BIT_IDX_T", OCTAVE_CONF_USE_64_BIT_IDX_T },
      { false, "XTRA_CFLAGS", OCTAVE_CONF_XTRA_CFLAGS },
      { false, "XTRA_CXXFLAGS", OCTAVE_CONF_XTRA_CXXFLAGS },
      { false, "YACC", OCTAVE_CONF_YACC },
      { false, "YFLAGS", OCTAVE_CONF_YFLAGS },
      { false, "api_version", OCTAVE_API_VERSION },
      { true, "archlibdir", OCTAVE_ARCHLIBDIR },
      { true, "bindir", OCTAVE_BINDIR },
      { false, "canonical_host_type", OCTAVE_CANONICAL_HOST_TYPE },
      { false, "config_opts", OCTAVE_CONF_config_opts },
      { true, "datadir", OCTAVE_DATADIR },
      { true, "datarootdir", OCTAVE_DATAROOTDIR },
      { true, "exec_prefix", OCTAVE_EXEC_PREFIX },
      { true, "fcnfiledir", OCTAVE_FCNFILEDIR },
      { true, "imagedir", OCTAVE_IMAGEDIR },
      { true, "includedir", OCTAVE_INCLUDEDIR },
      { true, "infodir", OCTAVE_INFODIR },
      { true, "infofile", OCTAVE_INFOFILE },
      { true, "libdir", OCTAVE_LIBDIR },
      { true, "libexecdir", OCTAVE_LIBEXECDIR },
      { true, "localapiarchlibdir", OCTAVE_LOCALAPIARCHLIBDIR },
      { true, "localapifcnfiledir", OCTAVE_LOCALAPIFCNFILEDIR },
      { true, "localapioctfiledir", OCTAVE_LOCALAPIOCTFILEDIR },
      { true, "localarchlibdir", OCTAVE_LOCALARCHLIBDIR },
      { true, "localfcnfiledir", OCTAVE_LOCALFCNFILEDIR },
      { true, "localoctfiledir", OCTAVE_LOCALOCTFILEDIR },
      { true, "localstartupfiledir", OCTAVE_LOCALSTARTUPFILEDIR },
      { true, "localverarchlibdir", OCTAVE_LOCALVERARCHLIBDIR },
      { true, "localverfcnfiledir", OCTAVE_LOCALVERFCNFILEDIR },
      { true, "localveroctfiledir", OCTAVE_LOCALVEROCTFILEDIR },
      { true, "man1dir", OCTAVE_MAN1DIR },
      { false, "man1ext", OCTAVE_MAN1EXT },
      { true, "mandir", OCTAVE_MANDIR },
      { true, "octfiledir", OCTAVE_OCTFILEDIR },
      { true, "octincludedir", OCTAVE_OCTINCLUDEDIR },
      { true, "octlibdir", OCTAVE_OCTLIBDIR },
      { true, "prefix", OCTAVE_PREFIX },
      { true, "startupfiledir", OCTAVE_STARTUPFILEDIR },
      { false, "version", OCTAVE_VERSION },
      { false, 0, 0 }
    };

  if (! initialized)
    {
      m.assign ("dld", octave_value (octave_supports_dynamic_linking));

      oct_mach_info::float_format ff = oct_mach_info::native_float_format ();
      m.assign ("float_format",
		octave_value (oct_mach_info::float_format_as_string (ff)));

      m.assign ("words_big_endian",
		octave_value (oct_mach_info::words_big_endian ()));

      m.assign ("words_little_endian",
		octave_value (oct_mach_info::words_little_endian ()));

      int i = 0;

      while (true)
	{
	  const conf_info_struct& elt = conf_info[i++];

	  const char *key = elt.key;

	  if (key)
	    {
	      if (elt.subst_home)
		m.assign (key, octave_value (subst_octave_home (elt.val)));
	      else
		m.assign (key, octave_value (elt.val));
	    }
	  else
	    break;
	}

      bool unix_system = true;
      bool mac_system = false;
      bool windows_system = false;

#if defined (WIN32)
      windows_system = true;
#if !defined (__CYGWIN__)
      unix_system = false;
#endif
#endif

#if defined (__APPLE__) && defined (__MACH__)
      mac_system = true;
#endif

      m.assign ("unix", octave_value (unix_system));
      m.assign ("mac", octave_value (mac_system));
      m.assign ("windows", octave_value (windows_system));

      initialized = true;
    }

  int nargin = args.length ();

  if (nargin == 1)
    {
      std::string arg = args(0).string_value ();

      if (! error_state)
	{
	  Cell c = m.contents (arg.c_str ());

	  if (c.is_empty ())
	    error ("octave_config_info: no info for `%s'", arg.c_str ());
	  else
	    retval = c(0);
	}
    }
  else if (nargin == 0)
    retval = m;
  else
    print_usage ();

  return retval;
}

#if defined (__GNUG__) && defined (DEBUG_NEW_DELETE)

int debug_new_delete = 0;

typedef void (*vfp)(void);
extern vfp __new_handler;

void *
__builtin_new (size_t sz)
{
  void *p;

  /* malloc (0) is unpredictable; avoid it.  */
  if (sz == 0)
    sz = 1;
  p = malloc (sz);
  while (p == 0)
    {
      (*__new_handler) ();
      p = malloc (sz);
    }

  if (debug_new_delete)
    std::cerr << "__builtin_new: " << p << std::endl;

  return p;
}

void
__builtin_delete (void *ptr)
{
  if (debug_new_delete)
    std::cerr << "__builtin_delete: " << ptr << std::endl;

  if (ptr)
    free (ptr);
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
