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
#include <csetjmp>
#include <csignal>
#include <cstdlib>
#include <cstring>
#include <ctime>

#include <string>

#include <fstream.h>
#include <iostream.h>
#include <strstream.h>

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
#include "str-vec.h"

#include <defaults.h>
#include "defun.h"
#include "dynamic-ld.h"
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

// Current command to execute.
tree_statement_list *global_command = 0;

// Pointer to function that is currently being evaluated.
octave_user_function *curr_function = 0;

// Top level context (?)
jmp_buf toplevel;

int
main_loop (void)
{
  // Allow the user to interrupt us without exiting.

  octave_save_signal_mask ();

  if (setjmp (toplevel) != 0)
    {
      raw_mode (0);

      cout << "\n";

      octave_restore_signal_mask ();
    }

  can_interrupt = true;

  octave_catch_interrupts ();

  // The big loop.

  int retval;
  do
    {
      curr_sym_tab = top_level_sym_tab;

      reset_parser ();

      retval = yyparse ();

      if (retval == 0 && global_command)
	{
	  global_command->eval ();

	  delete global_command;

	  global_command = 0;

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
		break;
	    }
	  else
	    {
	      if (octave_completion_matches_called)
		octave_completion_matches_called = false;	    
	      else
		command_editor::increment_current_command_number ();
	    }
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

DEFUN_TEXT (casesen, args, ,
  "casesen [on|off]")
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
  "computer ():\n\
\n\
Have Octave ask the system, \"What kind of computer are you?\"")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin != 0)
    warning ("computer: ignoring extra arguments");

  string msg;

  if (strcmp (CANONICAL_HOST_TYPE, "unknown") == 0)
    msg = "Hi Dave, I'm a HAL-9000";
  else
    msg = CANONICAL_HOST_TYPE;

  if (nargout == 0)
    octave_stdout << msg << "\n";
  else
    retval = msg;

  return retval;
}

DEFUN (quit, args, nargout,
  "quit (STATUS): exit Octave gracefully, returning STATUS to the system.\n\
\n\
STATUS should be an integer value.  If STATUS is missing, 0 is assumed.")
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
  "warranty (): describe copying conditions")
{
  octave_value_list retval;

  octave_stdout << "\n" OCTAVE_NAME_VERSION_AND_COPYRIGHT "\n\n\
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
run_command_and_return_output (const string& cmd_str)
{
  octave_value_list retval;

  iprocstream *cmd = new iprocstream (cmd_str.c_str ());

  if (cmd)
    {
      unwind_protect::add (cleanup_iprocstream, cmd);

      if (*cmd)
	{
	  ostrstream output_buf;

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

#if defined (HAVE_USLEEP)
	      usleep (100);
#else
	      sleep (1);
#endif
	    }

	  while (cmd->get (ch))
	    output_buf.put (ch);

	  int cmd_status = cmd->close ();

	  if (WIFEXITED (cmd_status))
	    cmd_status = WEXITSTATUS (cmd_status);
	  else
	    cmd_status = 127;

	  output_buf << ends;

	  char *msg = output_buf.str ();

	  retval(1) = (double) cmd_status;
	  retval(0) = msg;

	  delete [] msg;
	}

      unwind_protect::run ();
    }
  else
    error ("unable to start subprocess for `%s'", cmd_str.c_str ());

  return retval;
}

DEFUN (system, args, nargout,
  "system (STRING [, RETURN_OUTPUT] [, TYPE])\n\
\n\
Execute the shell command specified by STRING.\n\
\n\
If TYPE is \"async\", the process is started in the background and the\n\
pid of the child proces is returned immediately.  Otherwise, the\n\
process is started, and Octave waits until it exits.  If TYPE argument\n\
is omitted, a value of \"sync\" is assumed.\n\
\n\
If NARGIN == 2 (the actual value of RETURN_OUTPUT is irrelevant) and\n\
the subprocess is started synchronously, or if system() is called with\n\
NARGIN == 1 and NARGOUT > 0, the output from the command is returned.\n\
Otherwise, if the subprocess is executed synchronously, it's output is\n\
sent to the standard output.  To send the output of a command executed\n\
with system() through the pager, use a command like\n\
\n\
   disp (system (CMD, 1));\n\
\n\
or\n\
\n\
   printf (\"%s\\n\", system (CMD, 1));")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 0 && nargin < 4)
    {
      bool return_output = (nargout > 0 || nargin > 1);

      string cmd_str = args(0).string_value ();

      enum exec_type { sync, async };

      exec_type type = sync;

      if (! error_state)
	{
	  if (nargin > 2)
	    {
	      string type_str = args(2).string_value ();

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
	error ("system: expecting string as first argument");

      if (! error_state)
	{
	  if (type == async)
	    {
	      pid_t pid = fork ();

	      if (pid < 0) 
		error ("system: fork failed -- can't create child process");
	      else if (pid == 0)
		{
		  system (cmd_str.c_str ());
		  exit (0);
		  retval(0) = 0.0;
		}
	      else
		retval(0) = static_cast<double> (pid);
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

	      retval = static_cast<double> (status);
	    }
	}
    }
  else
    print_usage ("system");

  return retval;
}

DEFALIAS (shell_cmd, system);

// XXX FIXME XXX -- this should really be static, but that causes
// problems on some systems.
SLStack<string> octave_atexit_functions;

void
do_octave_atexit (void)
{
  static bool deja_vu = false;

  while (! octave_atexit_functions.empty ())
    {
      octave_value_list fcn = octave_atexit_functions.pop ();

      feval (fcn, 0);

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
	cout << "\n";
    }
}

DEFUN (atexit, args, ,
  "atexit (NAME): register NAME as a function to call when Octave exits\n\
\n\
Functions are called with no arguments in the reverse of the order in\n\
which they were registered with atexit()")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
#if defined (HAVE_ATEXIT) || defined (HAVE_ON_EXIT)
      string arg = args(0).string_value ();

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
  "octave_config_info (OPTION)\n\
\n\
If OPTION is a string, return the configuration information for the\n\
specified option.\n\
\n\
With no arguments, return a structure containing configuration\n\
information.")
{
  octave_value retval;

#if defined (WITH_DYNAMIC_LINKING) && (defined (WITH_DL) || defined (WITH_SHL))
  bool octave_supports_dynamic_linking = true;
#else
  bool octave_supports_dynamic_linking = false;
#endif

  Octave_map m;

  // XXX FIXME XXX -- should we perform OCTAVE_HOME substitution on these?

  m ["default_pager"] = DEFAULT_PAGER;
  m ["prefix"] = OCTAVE_PREFIX;
  m ["exec_prefix"] = OCTAVE_EXEC_PREFIX;
  m ["datadir"] = OCTAVE_DATADIR;
  m ["dld"] = static_cast<double> (octave_supports_dynamic_linking);
  m ["libdir"] = OCTAVE_LIBDIR;
  m ["bindir"] = OCTAVE_BINDIR;
  m ["infodir"] = OCTAVE_INFODIR;
  m ["fcnfiledir"] = OCTAVE_FCNFILEDIR;
  m ["localfcnfiledir"] = OCTAVE_LOCALFCNFILEDIR;
  m ["localstartupfiledir"] = OCTAVE_LOCALSTARTUPFILEDIR;
  m ["startupfiledir"] = OCTAVE_STARTUPFILEDIR;
  m ["localfcnfilepath"] = OCTAVE_LOCALFCNFILEPATH;
  m ["archlibdir"] = OCTAVE_ARCHLIBDIR;
  m ["localarchlibdir"] = OCTAVE_LOCALARCHLIBDIR;
  m ["octfiledir"] = OCTAVE_OCTFILEDIR;
  m ["localoctfilepath"] = OCTAVE_LOCALOCTFILEPATH;
  m ["fcnfilepath"] = OCTAVE_FCNFILEPATH;
  m ["imagepath"] = OCTAVE_IMAGEPATH;
  m ["canonical_host_type"] = CANONICAL_HOST_TYPE;
  m ["configure_options"] = config_opts;
  m ["F77"] = F77;
  m ["FFLAGS"] = FFLAGS;
  m ["FPICFLAG"] = FPICFLAG;
  m ["F2C"] = F2C;
  m ["F2CFLAGS"] = F2CFLAGS;
  m ["FLIBS"] = FLIBS;
  m ["CPPFLAGS"] = CPPFLAGS;
  m ["INCFLAGS"] = INCFLAGS;
  m ["CC"] = CC " " CC_VERSION;
  m ["CFLAGS"] = CFLAGS;
  m ["CPICFLAG"] = CPICFLAG;
  m ["CXX"] = CXX " " CXX_VERSION;
  m ["CXXFLAGS"] = CXXFLAGS;
  m ["CXXPICFLAG"] = CXXPICFLAG;
  m ["LDFLAGS"] = LDFLAGS;
  m ["LIBFLAGS"] = LIBFLAGS;
  m ["RLD_FLAG"] = RLD_FLAG;
  m ["TERMLIBS"] = TERMLIBS;
  m ["LIBS"] = LIBS;
  m ["LEXLIB"] = LEXLIB;
  m ["LIBPLPLOT"] = LIBPLPLOT;
  m ["LIBDLFCN"] = LIBDLFCN;
  m ["DEFS"] = DEFS;

  int nargin = args.length ();

  if (nargin == 1)
    {
      string arg = args(0).string_value ();

      if (! error_state)
	retval = octave_value (m [arg.c_str ()]);
    }
  else if (nargin == 0)
    retval = octave_value (m);
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
    cout << "__builtin_new: " << p << endl;

  return p;
}

void
__builtin_delete (void *ptr)
{
  if (debug_new_delete)
    cout << "__builtin_delete: " << ptr << endl;

  if (ptr)
    free (ptr);
}

#endif

void
symbols_of_toplev (void)
{
  DEFCONST (argv, ,
    "the command line arguments this program was invoked with");

  DEFCONST (program_invocation_name,
	    octave_env::get_program_invocation_name (),
    "the full name of the current program or script, including the\n\
directory specification");

  DEFCONST (program_name, octave_env::get_program_name (),
    "the name of the current program or script");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
