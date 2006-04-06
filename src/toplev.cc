/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

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
#include "lo-sstream.h"
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

static void
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
	  curr_sym_tab = top_level_sym_tab;

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
	    << "error: memory exhausted -- trying to return to prompt\n";
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

DEFCMD (casesen, args, ,
  "-*- texinfo -*-\n\
@deffn {Command} casesen arg\n\
Provided for compatibility with Matlab, but does nothing.\n\
@end deffn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("casesen");

  if (error_state)
    return retval;

  if (argc == 1 || (argc > 1 && argv[1] == "off"))
    warning ("casesen: sorry, Octave is always case sensitive");
  else if (argc > 1 && argv[1] == "on")
    ; // ok.
  else
    print_usage ("casesen");

  return retval;
}

DEFUN (quit, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} exit (@var{status})\n\
@deftypefnx {Built-in Function} {} quit (@var{status})\n\
Exit the current Octave session.  If the optional integer value\n\
@var{status} is supplied, pass that value to the operating system as the\n\
Octave's exit status.\n\
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
the Free Software Foundation; either version 2 of the License, or\n\
(at your option) any later version.\n\
\n\
This program is distributed in the hope that it will be useful,\n\
but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
GNU General Public License for more details.\n\
\n\
You should have received a copy of the GNU General Public License\n\
along with this program. If not, write to the Free Software\n\
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA\n\
02110-1301, USA.\n\
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
	  OSSTREAM output_buf;

	  // XXX FIXME XXX -- Perhaps we should read more than one
	  // character at a time and find a way to avoid the call to
	  // octave_usleep as well?

	  // This is a bit of a kluge...

	  octave_usleep (100);

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

		      octave_usleep (100);
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

	  output_buf << OSSTREAM_ENDS;

	  retval(0) = (double) cmd_status;
	  retval(1) = OSSTREAM_STR (output_buf);

	  OSSTREAM_FREEZE (output_buf);
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
	  if (type == et_async)
	    {
#ifdef HAVE_FORK
	      pid_t pid = fork ();

	      if (pid < 0) 
		error ("system: fork failed -- can't create child process");
	      else if (pid == 0)
		{
		  // XXX FIXME XXX -- should probably replace this
		  // call with something portable.

		  execl ("/bin/sh", "sh", "-c", cmd_str.c_str (),
			 static_cast<void *> (0));

		  panic_impossible ();
		}
	      else
		retval(0) = pid;
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
    print_usage ("system");

  unwind_protect::run_frame ("Fsystem");

  return retval;
}

DEFALIAS (shell_cmd, system);

// XXX FIXME XXX -- this should really be static, but that causes
// problems on some systems.
std::stack<std::string> octave_atexit_functions;

void
do_octave_atexit (void)
{
  static bool deja_vu = false;

  while (! octave_atexit_functions.empty ())
    {
      std::string fcn = octave_atexit_functions.top ();

      octave_atexit_functions.pop ();

      reset_error_handler ();

      feval (fcn, octave_value_list (), 0);

      flush_octave_stdout ();
    }

  if (! deja_vu)
    {
      deja_vu = true;

      command_editor::restore_terminal_state ();

      // XXX FIXME XXX -- is this needed?  Can it cause any trouble?
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

DEFUN (atexit, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} atexit (@var{fcn})\n\
Register a function to be called when Octave exits.  For example,\n\
\n\
@example\n\
@group\n\
function print_fortune ()\n\
  printf (\"\\n%s\\n\", system (\"fortune\"));\n\
  fflush (stdout);\n\
endfunction\n\
atexit (\"print_fortune\");\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
will print a message when Octave exits.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
#if defined (HAVE_ATEXIT) || defined (HAVE_ON_EXIT)
      std::string arg = args(0).string_value ();

      if (! error_state)
	octave_atexit_functions.push (arg);
      else
	error ("atexit: argument must be a string");
#else
      gripe_not_supported ("atexit");
#endif
    }
  else
    print_usage ("atexit");

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

  static const char * const conf_info[] =
    {
      "ALL_CFLAGS", OCTAVE_CONF_ALL_CFLAGS,
      "ALL_CXXFLAGS", OCTAVE_CONF_ALL_CXXFLAGS,
      "ALL_FFLAGS", OCTAVE_CONF_ALL_FFLAGS,
      "ALL_LDFLAGS", OCTAVE_CONF_ALL_LDFLAGS,
      "AR", OCTAVE_CONF_AR,
      "ARFLAGS", OCTAVE_CONF_ARFLAGS,
      "BLAS_LIBS", OCTAVE_CONF_BLAS_LIBS,
      "CC", OCTAVE_CONF_CC,
      "CC_VERSION", OCTAVE_CONF_CC_VERSION,
      "CFLAGS", OCTAVE_CONF_CFLAGS,
      "CPICFLAG", OCTAVE_CONF_CPICFLAG,
      "CPPFLAGS", OCTAVE_CONF_CPPFLAGS,
      "CXX", OCTAVE_CONF_CXX,
      "CXXCPP", OCTAVE_CONF_CXXCPP,
      "CXXFLAGS", OCTAVE_CONF_CXXFLAGS,
      "CXXPICFLAG", OCTAVE_CONF_CXXPICFLAG,
      "CXX_VERSION", OCTAVE_CONF_CXX_VERSION,
      "DEFAULT_PAGER", OCTAVE_DEFAULT_PAGER,
      "DEFS", OCTAVE_CONF_DEFS,
      "DLFCN_INCFLAGS", OCTAVE_CONF_DLFCN_INCFLAGS,
      "DL_LD", OCTAVE_CONF_DL_LD,
      "DL_LDFLAGS", OCTAVE_CONF_DL_LDFLAGS,
      "ENABLE_DYNAMIC_LINKING", OCTAVE_CONF_ENABLE_DYNAMIC_LINKING,
      "EXEEXT", OCTAVE_CONF_EXEEXT,
      "F2C", OCTAVE_CONF_F2C,
      "F2CFLAGS", OCTAVE_CONF_F2CFLAGS,
      "F77", OCTAVE_CONF_F77,
      "F77_FLOAT_STORE_FLAG", OCTAVE_CONF_F77_FLOAT_STORE_FLAG,
      "FC", OCTAVE_CONF_FC,
      "FFLAGS", OCTAVE_CONF_FFLAGS,
      "FFTW_LIBS", OCTAVE_CONF_FFTW_LIBS,
      "FLIBS", OCTAVE_CONF_FLIBS,
      "FPICFLAG", OCTAVE_CONF_FPICFLAG,
      "GLPK_LIBS", OCTAVE_CONF_GLPK_LIBS,
      "INCFLAGS", OCTAVE_CONF_INCFLAGS,
      "LDFLAGS", OCTAVE_CONF_LDFLAGS,
      "LD_CXX", OCTAVE_CONF_LD_CXX,
      "LD_STATIC_FLAG", OCTAVE_CONF_LD_STATIC_FLAG,
      "LEX", OCTAVE_CONF_LEX,
      "LEXLIB", OCTAVE_CONF_LEXLIB,
      "LFLAGS", OCTAVE_CONF_LFLAGS,
      "LIBCRUFT", OCTAVE_CONF_LIBCRUFT,
      "LIBDLFCN", OCTAVE_CONF_LIBDLFCN,
      "LIBEXT", OCTAVE_CONF_LIBEXT,
      "LIBFLAGS", OCTAVE_CONF_LIBFLAGS,
      "LIBOCTAVE", OCTAVE_CONF_LIBOCTAVE,
      "LIBOCTINTERP", OCTAVE_CONF_LIBOCTINTERP,
      "LIBPLPLOT", OCTAVE_CONF_LIBPLPLOT,
      "LIBREADLINE", OCTAVE_CONF_LIBREADLINE,
      "LIBS", OCTAVE_CONF_LIBS,
      "LN_S", OCTAVE_CONF_LN_S,
      "MKOCTFILE_DL_LDFLAGS", OCTAVE_CONF_MKOCTFILE_DL_LDFLAGS,
      "MKOCTFILE_INCFLAGS", OCTAVE_CONF_MKOCTFILE_INCFLAGS,
      "MKOCTFILE_LFLAGS", OCTAVE_CONF_MKOCTFILE_LFLAGS,
      "RANLIB", OCTAVE_CONF_RANLIB,
      "RDYNAMIC_FLAG", OCTAVE_CONF_RDYNAMIC_FLAG,
      "RLD_FLAG", OCTAVE_CONF_RLD_FLAG,
      "RUNTEST", OCTAVE_CONF_RUNTEST,
      "SED", OCTAVE_CONF_SED,
      "SHARED_LIBS", OCTAVE_CONF_SHARED_LIBS,
      "SHLEXT", OCTAVE_CONF_SHLEXT,
      "SHLEXT_VER", OCTAVE_CONF_SHLEXT_VER,
      "SH_LD", OCTAVE_CONF_SH_LD,
      "SH_LDFLAGS", OCTAVE_CONF_SH_LDFLAGS,
      "SONAME_FLAGS", OCTAVE_CONF_SONAME_FLAGS,
      "STATIC_LIBS", OCTAVE_CONF_STATIC_LIBS,
      "UGLY_DEFS", OCTAVE_CONF_UGLY_DEFS,
      "USE_64_BIT_IDX_T", OCTAVE_CONF_USE_64_BIT_IDX_T,
      "XTRA_CFLAGS", OCTAVE_CONF_XTRA_CFLAGS,
      "XTRA_CXXFLAGS", OCTAVE_CONF_XTRA_CXXFLAGS,
      "YACC", OCTAVE_CONF_YACC,
      "YFLAGS", OCTAVE_CONF_YFLAGS,
      "api_version", OCTAVE_API_VERSION,
      "archlibdir", OCTAVE_ARCHLIBDIR,
      "bindir", OCTAVE_BINDIR,
      "canonical_host_type", OCTAVE_CANONICAL_HOST_TYPE,
      "config_opts", OCTAVE_CONF_config_opts,
      "datadir", OCTAVE_DATADIR,
      "exec_prefix", OCTAVE_EXEC_PREFIX,
      "fcnfiledir", OCTAVE_FCNFILEDIR,
      "fcnfilepath", OCTAVE_FCNFILEPATH,
      "imagedir", OCTAVE_IMAGEDIR,
      "imagepath", OCTAVE_IMAGEPATH,
      "includedir", OCTAVE_INCLUDEDIR,
      "infodir", OCTAVE_INFODIR,
      "infofile", OCTAVE_INFOFILE,
      "libdir", OCTAVE_LIBDIR,
      "libexecdir", OCTAVE_LIBEXECDIR,
      "localapifcnfiledir", OCTAVE_LOCALAPIFCNFILEDIR,
      "localapioctfiledir", OCTAVE_LOCALAPIOCTFILEDIR,
      "localarchlibdir", OCTAVE_LOCALARCHLIBDIR,
      "localfcnfiledir", OCTAVE_LOCALFCNFILEDIR,
      "localfcnfilepath", OCTAVE_LOCALFCNFILEPATH,
      "localoctfiledir", OCTAVE_LOCALOCTFILEDIR,
      "localoctfilepath", OCTAVE_LOCALOCTFILEPATH,
      "localstartupfiledir", OCTAVE_LOCALSTARTUPFILEDIR,
      "localverarchlibdir", OCTAVE_LOCALVERARCHLIBDIR,
      "localverfcnfiledir", OCTAVE_LOCALVERFCNFILEDIR,
      "localveroctfiledir", OCTAVE_LOCALVEROCTFILEDIR,
      "man1dir", OCTAVE_MAN1DIR,
      "man1ext", OCTAVE_MAN1EXT,
      "mandir", OCTAVE_MANDIR,
      "octfiledir", OCTAVE_OCTFILEDIR,
      "octincludedir", OCTAVE_OCTINCLUDEDIR,
      "octlibdir", OCTAVE_OCTLIBDIR,
      "prefix", OCTAVE_PREFIX,
      "startupfiledir", OCTAVE_STARTUPFILEDIR,
      "version", OCTAVE_VERSION,
      0, 0
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
	  const char *key = conf_info[i++];

	  if (key)
	    m.assign (key, octave_value (conf_info[i++]));
	  else
	    break;
	}

      bool unix_system = true;
      bool windows_system = false;

#if defined (WIN32)
      windows_system = true;
#if !defined (__CYGWIN__)
      unix_system = false;
#endif
#endif

      m.assign ("unix", octave_value (unix_system));
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
    print_usage ("octave_config_info");

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

void
symbols_of_toplev (void)
{
  DEFCONST (argv, ,
    "-*- texinfo -*-\n\
@defvr {Built-in Constant} argv\n\
The command line arguments passed to Octave are available in this\n\
variable.  For example, if you invoked Octave using the command\n\
\n\
@example\n\
octave --no-line-editing --silent\n\
@end example\n\
\n\
@noindent\n\
@code{argv} would be a cell array of strings with the elements\n\
@code{--no-line-editing} and @code{--silent}.\n\
\n\
If you write an executable Octave script, @code{argv} will contain the\n\
list of arguments passed to the script.  @xref{Executable Octave Programs},\n\
for an example of how to create an executable Octave script.\n\
@end defvr");

  DEFCONST (program_invocation_name,
	    octave_env::get_program_invocation_name (),
    "-*- texinfo -*-\n\
@defvr {Built-in Constant} program_invocation_name\n\
@defvrx {Built-in Constant} program_name\n\
When Octave starts, the value of the built-in variable\n\
@code{program_invocation_name} is automatically set to the name that was\n\
typed at the shell prompt to run Octave, and the value of\n\
@code{program_name} is automatically set to the final component of\n\
@code{program_invocation_name}.  For example, if you typed\n\
@samp{@value{OCTAVEHOME}/bin/octave} to start Octave,\n\
@code{program_invocation_name} would have the value\n\
@code{\"@value{OCTAVEHOME}/bin/octave\"}, and @code{program_name} would\n\
have the value @code{\"octave\"}.\n\
\n\
If executing a script from the command line (e.g., @code{octave foo.m})\n\
or using an executable Octave script, the program name is set to the\n\
name of the script.  @xref{Executable Octave Programs}, for an example of\n\
how to create an executable Octave script.\n\
@end defvr");

  DEFCONST (program_name, octave_env::get_program_name (),
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} program_name\n\
See: program_invocation_name.\n\
@end defvr");

}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
