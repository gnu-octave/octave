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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>
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
#include "pt-plot.h"
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

// Pointer to function that is currently being evaluated.
octave_user_function *curr_function = 0;

// Original value of TEXMFDBS environment variable.
std::string octave_original_texmfdbs;

static void
recover_from_exception (void)
{
  unwind_protect::run_all ();
  can_interrupt = true;
  octave_interrupt_immediately = 0;
  octave_interrupt_state = 0;
  octave_allocation_error = 0;
  octave_restore_signal_mask ();
  octave_catch_interrupts ();
}

int
main_loop (void)
{
  octave_save_signal_mask ();

  if (octave_set_current_context)
    {
#if defined (USE_EXCEPTIONS_FOR_INTERRUPTS)
      panic_impossible ();
#else
      unwind_protect::run_all ();
      raw_mode (0);
      std::cout << "\n";
      octave_restore_signal_mask ();
#endif
    }

  can_interrupt = true;

  octave_catch_interrupts ();

  octave_initialized = true;

  // The big loop.

  int retval = 0;
  do
    {
      try
	{
	  curr_sym_tab = top_level_sym_tab;

	  reset_parser ();

	  retval = yyparse ();

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
	  std::cout << "\n";
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

DEFUN (computer, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} computer ()\n\
Print or return a string of the form @var{cpu}-@var{vendor}-@var{os}\n\
that identifies the kind of computer Octave is running on.  If invoked\n\
with an output argument, the value is returned instead of printed.  For\n\
example,\n\
\n\
@example\n\
@group\n\
computer ()\n\
     @print{} i586-pc-linux-gnu\n\
\n\
x = computer ()\n\
     @result{} x = \"i586-pc-linux-gnu\"\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin != 0)
    warning ("computer: ignoring extra arguments");

  std::string msg;

  if (strcmp (OCTAVE_CANONICAL_HOST_TYPE, "unknown") == 0)
    msg = "Hi Dave, I'm a HAL-9000";
  else
    msg = OCTAVE_CANONICAL_HOST_TYPE;

  if (nargout == 0)
    octave_stdout << msg << "\n";
  else
    retval = msg;

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
Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.\n\
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

	  // XXX FIXME XXX -- sometimes, the subprocess hasn't written
	  // anything before we try to read from the procstream.  The
	  // kluge below (simply waiting and trying again) is ugly,
	  // but it seems to work, at least most of the time.  It
	  // could probably still fail if the subprocess hasn't
	  // started writing after the snooze.  Isn't there a better
	  // way?  If there is, you should also fix the code for the
	  // ls function in dirfns.cc.

	  char ch;

	  if (cmd->get (ch))
	    output_buf.put (ch);
	  else
	    {
	      cmd->clear ();

	      octave_usleep (100);
	    }

	  while (cmd->get (ch))
	    output_buf.put (ch);

	  int cmd_status = cmd->close ();

	  if (WIFEXITED (cmd_status))
	    cmd_status = WEXITSTATUS (cmd_status);
	  else
	    cmd_status = 127;

	  output_buf << OSSTREAM_ENDS;

	  retval(1) = (double) cmd_status;
	  retval(0) = OSSTREAM_STR (output_buf);

	  OSSTREAM_FREEZE (output_buf);
	}

      unwind_protect::run ();
    }
  else
    error ("unable to start subprocess for `%s'", cmd_str.c_str ());

  return retval;
}

static void
restore_texmfdbs_envvar (void *ptr)
{
  std::string *s = static_cast<std::string *> (ptr);

  octave_env::putenv ("TEXMFDBS", *s);
}

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
Otherwise, if the subprocess is executed synchronously, it's output is\n\
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
The @code{system} function can return two values.  The first is any\n\
output from the command that was written to the standard output stream,\n\
and the second is the output status of the command.  For example,\n\
\n\
@example\n\
[output, status] = system (\"echo foo; exit 2\");\n\
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
      bool return_output = (nargout > 0 || nargin > 1);

      std::string cmd_str = args(0).string_value ();

      enum exec_type { sync, async };

      exec_type type = sync;

      if (! error_state)
	{
	  if (nargin > 2)
	    {
	      std::string type_str = args(2).string_value ();

	      if (! error_state)
		{
		  if (type_str == "sync")
		    type = sync;
		  else if (type_str == "async")
		    type = async;
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
	  // The value of TEXMFDBS that Octave puts in the environment
	  // will cause trouble if we are asked to run TeX, so we
	  // should reset it to whatever it was before Octave started.
	  //
	  // XXX FIXME XXX -- it would be better to fix the
	  // kpathsearch library to not always do TeX-specific
	  // things...

	  static std::string odb;

	  odb = octave_env::getenv ("TEXMFDBS");

	  unwind_protect::add (restore_texmfdbs_envvar, &odb);

	  octave_env::putenv ("TEXMFDBS", octave_original_texmfdbs);

	  if (type == async)
	    {
#ifdef HAVE_FORK
	      pid_t pid = fork ();

	      if (pid < 0) 
		error ("system: fork failed -- can't create child process");
	      else if (pid == 0)
		{
		  // XXX FIXME XXX -- should probably replace this
		  // call with something portable.

		  execl ("/bin/sh", "sh", "-c", cmd_str.c_str (), 0);

		  panic_impossible ();
		}
	      else
		retval(0) = static_cast<double> (pid);
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

      feval (fcn, octave_value_list (), 0);

      flush_octave_stdout ();
    }

  if (! deja_vu)
    {
      deja_vu = true;

      command_editor::restore_terminal_state ();

      // XXX FIXME XXX -- is this needed?  Can it cause any trouble?
      raw_mode (0);

      command_history::clean_up_and_save ();

      close_plot_stream ();

      close_files ();

      cleanup_tmp_files ();

      flush_octave_stdout ();

      if (!quitting_gracefully && (interactive || forced_interactive))
	std::cout << "\n";
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

  Octave_map m;

  m ["ALL_CFLAGS"](0) = OCTAVE_CONF_ALL_CFLAGS;
  m ["ALL_CXXFLAGS"](0) = OCTAVE_CONF_ALL_CXXFLAGS;
  m ["ALL_FFLAGS"](0) = OCTAVE_CONF_ALL_FFLAGS;
  m ["ALL_LDFLAGS"](0) = OCTAVE_CONF_ALL_LDFLAGS;
  m ["AR"](0) = OCTAVE_CONF_AR;
  m ["ARFLAGS"](0) = OCTAVE_CONF_ARFLAGS;
  m ["BLAS_LIBS"](0) = OCTAVE_CONF_BLAS_LIBS;
  m ["CC"](0) = OCTAVE_CONF_CC;
  m ["CC_VERSION"](0) = OCTAVE_CONF_CC_VERSION;
  m ["CFLAGS"](0) = OCTAVE_CONF_CFLAGS;
  m ["CPICFLAG"](0) = OCTAVE_CONF_CPICFLAG;
  m ["CPPFLAGS"](0) = OCTAVE_CONF_CPPFLAGS;
  m ["CXX"](0) = OCTAVE_CONF_CXX;
  m ["CXXCPP"](0) = OCTAVE_CONF_CXXCPP;
  m ["CXXFLAGS"](0) = OCTAVE_CONF_CXXFLAGS;
  m ["CXXPICFLAG"](0) = OCTAVE_CONF_CXXPICFLAG;
  m ["CXX_VERSION"](0) = OCTAVE_CONF_CXX_VERSION;
  m ["DEFAULT_PAGER"](0) = OCTAVE_DEFAULT_PAGER;
  m ["DLFCN_INCFLAGS"](0) = OCTAVE_CONF_DLFCN_INCFLAGS;
  m ["EXEEXT"](0) = OCTAVE_CONF_EXEEXT;
  m ["F2C"](0) = OCTAVE_CONF_F2C;
  m ["F2CFLAGS"](0) = OCTAVE_CONF_F2CFLAGS;
  m ["F77"](0) = OCTAVE_CONF_F77;
  m ["FC"](0) = OCTAVE_CONF_FC;
  m ["FFLAGS"](0) = OCTAVE_CONF_FFLAGS;
  m ["FFTW_LIBS"](0) = OCTAVE_CONF_FFTW_LIBS;
  m ["FLIBS"](0) = OCTAVE_CONF_FLIBS;
  m ["FPICFLAG"](0) = OCTAVE_CONF_FPICFLAG;
  m ["GLOB_INCFLAGS"](0) = OCTAVE_CONF_GLOB_INCFLAGS;
  m ["INCFLAGS"](0) = OCTAVE_CONF_INCFLAGS;
  m ["LDFLAGS"](0) = OCTAVE_CONF_LDFLAGS;
  m ["LD_CXX"](0) = OCTAVE_CONF_LD_CXX;
  m ["LD_STATIC_FLAG"](0) = OCTAVE_CONF_LD_STATIC_FLAG;
  m ["LEX"](0) = OCTAVE_CONF_LEX;
  m ["LEXLIB"](0) = OCTAVE_CONF_LEXLIB;
  m ["LFLAGS"](0) = OCTAVE_CONF_LFLAGS;
  m ["LIBCRUFT"](0) = OCTAVE_CONF_LIBCRUFT;
  m ["LIBDLFCN"](0) = OCTAVE_CONF_LIBDLFCN;
  m ["LIBEXT"](0) = OCTAVE_CONF_LIBEXT;
  m ["LIBFLAGS"](0) = OCTAVE_CONF_LIBFLAGS;
  m ["LIBGLOB"](0) = OCTAVE_CONF_LIBGLOB;
  m ["LIBKPATHSEA"](0) = OCTAVE_CONF_LIBKPATHSEA;
  m ["LIBOCTAVE"](0) = OCTAVE_CONF_LIBOCTAVE;
  m ["LIBOCTINTERP"](0) = OCTAVE_CONF_LIBOCTINTERP;
  m ["LIBPLPLOT"](0) = OCTAVE_CONF_LIBPLPLOT;
  m ["LIBREADLINE"](0) = OCTAVE_CONF_LIBREADLINE;
  m ["LIBS"](0) = OCTAVE_CONF_LIBS;
  m ["LN_S"](0) = OCTAVE_CONF_LN_S;
  m ["MKOCTFILE_INCFLAGS"](0) = OCTAVE_CONF_MKOCTFILE_INCFLAGS;
  m ["MKOCTFILE_LFLAGS"](0) = OCTAVE_CONF_MKOCTFILE_LFLAGS;
  m ["MKOCTFILE_SH_LDFLAGS"](0) = OCTAVE_CONF_MKOCTFILE_SH_LDFLAGS;
  m ["RANLIB"](0) = OCTAVE_CONF_RANLIB;
  m ["RDYNAMIC_FLAG"](0) = OCTAVE_CONF_RDYNAMIC_FLAG;
  m ["RLD_FLAG"](0) = OCTAVE_CONF_RLD_FLAG;
  m ["RUNTEST"](0) = OCTAVE_CONF_RUNTEST;
  m ["SED"](0) = OCTAVE_CONF_SED;
  m ["SHARED_LIBS"](0) = OCTAVE_CONF_SHARED_LIBS;
  m ["SHLEXT"](0) = OCTAVE_CONF_SHLEXT;
  m ["SHLEXT_VER"](0) = OCTAVE_CONF_SHLEXT_VER;
  m ["SH_LD"](0) = OCTAVE_CONF_SH_LD;
  m ["SH_LDFLAGS"](0) = OCTAVE_CONF_SH_LDFLAGS;
  m ["SONAME_FLAGS"](0) = OCTAVE_CONF_SONAME_FLAGS;
  m ["STATIC_LIBS"](0) = OCTAVE_CONF_STATIC_LIBS;
  m ["UGLY_DEFS"](0) = OCTAVE_CONF_DEFS;
  m ["UGLY_DEFS"](0) = OCTAVE_CONF_UGLY_DEFS;
  m ["ENABLE_DYNAMIC_LINKING"](0) = OCTAVE_CONF_ENABLE_DYNAMIC_LINKING;
  m ["XTRA_CFLAGS"](0) = OCTAVE_CONF_XTRA_CFLAGS;
  m ["XTRA_CXXFLAGS"](0) = OCTAVE_CONF_XTRA_CXXFLAGS;
  m ["YACC"](0) = OCTAVE_CONF_YACC;
  m ["YFLAGS"](0) = OCTAVE_CONF_YFLAGS;
  m ["archlibdir"](0) = OCTAVE_ARCHLIBDIR;
  m ["bindir"](0) = OCTAVE_BINDIR;
  m ["canonical_host_type"](0) = OCTAVE_CANONICAL_HOST_TYPE;
  m ["config_opts"](0) = OCTAVE_CONF_config_opts;
  m ["datadir"](0) = OCTAVE_DATADIR;
  m ["dld"](0) = octave_supports_dynamic_linking;
  m ["exec_prefix"](0) = OCTAVE_EXEC_PREFIX;
  m ["fcnfiledir"](0) = OCTAVE_FCNFILEDIR;
  m ["fcnfilepath"](0) = OCTAVE_FCNFILEPATH;
  m ["imagedir"](0) = OCTAVE_IMAGEDIR;
  m ["imagepath"](0) = OCTAVE_IMAGEPATH;
  m ["includedir"](0) = OCTAVE_INCLUDEDIR;
  m ["infodir"](0) = OCTAVE_INFODIR;
  m ["infofile"](0) = OCTAVE_INFOFILE;
  m ["libdir"](0) = OCTAVE_LIBDIR;
  m ["libexecdir"](0) = OCTAVE_LIBEXECDIR;
  m ["localarchlibdir"](0) = OCTAVE_LOCALARCHLIBDIR;
  m ["localfcnfiledir"](0) = OCTAVE_LOCALFCNFILEDIR;
  m ["localfcnfilepath"](0) = OCTAVE_LOCALFCNFILEPATH;
  m ["localoctfiledir"](0) = OCTAVE_LOCALOCTFILEDIR;
  m ["localoctfilepath"](0) = OCTAVE_LOCALOCTFILEPATH;
  m ["localstartupfiledir"](0) = OCTAVE_LOCALSTARTUPFILEDIR;
  m ["localverarchlibdir"](0) = OCTAVE_LOCALVERARCHLIBDIR;
  m ["localverfcnfiledir"](0) = OCTAVE_LOCALVERFCNFILEDIR;
  m ["localveroctfiledir"](0) = OCTAVE_LOCALVEROCTFILEDIR;
  m ["man1dir"](0) = OCTAVE_MAN1DIR;
  m ["man1ext"](0) = OCTAVE_MAN1EXT;
  m ["mandir"](0) = OCTAVE_MANDIR;
  m ["octfiledir"](0) = OCTAVE_OCTFILEDIR;
  m ["octincludedir"](0) = OCTAVE_OCTINCLUDEDIR;
  m ["octlibdir"](0) = OCTAVE_OCTLIBDIR;
  m ["prefix"](0) = OCTAVE_PREFIX;
  m ["startupfiledir"](0) = OCTAVE_STARTUPFILEDIR;
  m ["version"](0) = OCTAVE_VERSION;

  int nargin = args.length ();

  if (nargin == 1)
    {
      std::string arg = args(0).string_value ();

      if (! error_state)
	retval = m [arg.c_str ()](0);
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
    std::cout << "__builtin_new: " << p << std::endl;

  return p;
}

void
__builtin_delete (void *ptr)
{
  if (debug_new_delete)
    std::cout << "__builtin_delete: " << ptr << std::endl;

  if (ptr)
    free (ptr);
}

#endif

void
symbols_of_toplev (void)
{
  DEFCONST (argv, ,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} argv\n\
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
@defvr {Built-in Variable} program_invocation_name\n\
@defvrx {Built-in Variable} program_name\n\
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
@defvr {Built-in Variable} program_invocation_name\n\
@defvrx {Built-in Variable} program_name\n\
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

}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
