// octave.cc                                            -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

// Born February 20, 1992.

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>
#include <csetjmp>
#include <csignal>
#include <cstdlib>
#include <cstring>
#include <ctime>

#include <fstream.h>
#include <iostream.h>
#include <strstream.h>

#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif

#include <pwd.h>

#include "getopt.h"

#include "lo-error.h"

#include "builtins.h"
#include "defaults.h"
#include "defun.h"
#include "dynamic-ld.h"
#include "error.h"
#include "file-io.h"
#include "file-ops.h"
#include "help.h"
#include "input.h"
#include "lex.h"
#include "oct-hist.h"
#include "toplev.h"
#include "pager.h"
#include "parse.h"
#include "pathsearch.h"
#include "procstream.h"
#include "sighandlers.h"
#include "sysdep.h"
#include "pt-const.h"
#include "pt-misc.h"
#include "pt-plot.h"
#include "unwind-prot.h"
#include "user-prefs.h"
#include "utils.h"
#include "variables.h"
#include "version.h"

#if !defined (HAVE_ATEXIT) && defined (HAVE_ON_EXIT)
extern "C" int on_exit ();
#define atexit on_exit
#endif

// This is from readline's paren.c:
extern int rl_blink_matching_paren;

// Top level context (?)
jmp_buf toplevel;

// Nonzero means we read ~/.octaverc and ./.octaverc.
// (--norc; --ignore-init-file; -f)
static int read_init_files = 1;

// Nonzero means we don't print the usual startup message.
// (--quiet; --silent; -q)
static int inhibit_startup_message = 0;

// Nonzero means we turn on compatibility options.
// (--traditional)
static int traditional = 0;

// Usage message
static const char *usage_string = 
  "octave [-?Vdfhiqvx] [--debug] [--echo-commands] [--exec-path path]\n\
       [--help] [--ignore-init-file] [--info-file file] [--info-program prog]\n\
       [--interactive] [-p path] [--path path] [--silent] [--traditional]\n\
       [--verbose] [--version] [file]";

// This is here so that it's more likely that the usage message and
// the real set of options will agree.  Note: the `+' must come first
// to prevent getopt from permuting arguments!
static const char *short_opts = "+?Vdfhip:qvx";

// Long options.  See the comments in getopt.h for the meanings of the
// fields in this structure.
#define EXEC_PATH_OPTION 1
#define INFO_FILE_OPTION 2
#define INFO_PROG_OPTION 3
#define TRADITIONAL_OPTION 4
static struct option long_opts[] =
  {
    { "debug",            no_argument,       0, 'd' },
    { "echo-commands",    no_argument,       0, 'x' },
    { "exec-path",        required_argument, 0, EXEC_PATH_OPTION },
    { "help",             no_argument,       0, 'h' },
    { "interactive",      no_argument,       0, 'i' },
    { "info-file",        required_argument, 0, INFO_FILE_OPTION },
    { "info-program",     required_argument, 0, INFO_PROG_OPTION },
    { "ignore-init-file", no_argument,       0, 'f' },
    { "norc",             no_argument,       0, 'f' },
    { "path",             required_argument, 0, 'p' },
    { "quiet",            no_argument,       0, 'q' },
    { "silent",           no_argument,       0, 'q' },
    { "traditional",      no_argument,       0, TRADITIONAL_OPTION },
    { "verbose",          no_argument,       0, 'V' },
    { "version",          no_argument,       0, 'v' },
    { 0,                  0,                 0, 0 }
  };

// Store the command-line options for later use.

static void
intern_argv (int argc, char **argv)
{
  if (argc > 1)
    {
      int max_len = 0;
      for (int i = 1; i < argc; i++)
	{
	  int tmp_len = strlen (argv[i]);
	  if (tmp_len > max_len)
	    max_len = tmp_len;
	}

      octave_argv.resize (argc-1, max_len, 0);

      for (int i = 1; i < argc; i++)
	octave_argv.insert (argv[i], i-1, 0);

      bind_builtin_variable ("argv", octave_argv, 1, 1, 0);
    }
}

// Initialize some global variables for later use.

static void
initialize_globals (const string& name)
{
  raw_prog_name = name;
  size_t pos = raw_prog_name.rfind ('/');
  if (pos == NPOS)
    prog_name = raw_prog_name;
  else
    prog_name = raw_prog_name.substr (pos+1);

  struct passwd *entry = getpwuid (getuid ());
  if (entry)
    user_name = entry->pw_name;
  else
    user_name = "I have no name!";
  endpwent ();

  char hostname[256];
  if (gethostname (hostname, 255) < 0)
    host_name = "I have no host!";
  else
    host_name = hostname;

  char *hd = getenv ("HOME");
  home_directory = hd ? hd : "I have no home!";

  exec_path = default_exec_path ();

  load_path = default_path ();

  info_file = default_info_file ();

  info_prog = default_info_prog ();

  editor = default_editor ();
}

// Initialize by reading startup files.

static void
execute_startup_files (void)
{
  begin_unwind_frame ("execute_startup_files");

  // XXX FIXME XXX -- need to make it possible to set this in startup
  // files.

  unwind_protect_int (input_from_startup_file);
  unwind_protect_int (user_pref.echo_executing_commands);

  input_from_startup_file = 1;
  user_pref.echo_executing_commands = ECHO_OFF;

  int verbose = (verbose_flag && ! inhibit_startup_message);

  // Execute commands from the site-wide configuration file.  First
  // from the file $(prefix)/lib/octave/site/m/octaverc (if it exists),
  // then from the file $(prefix)/lib/octave/$(version)/m/octaverc (if
  // it exists).

  string lsd = get_local_site_defaults ();
  parse_and_execute (lsd, 0, verbose);

  string sd = get_site_defaults ();
  parse_and_execute (sd, 0, verbose);

  // Try to execute commands from $HOME/.octaverc and ./.octaverc.

  int home_rc_already_executed = 0;
  string home_rc;
  if (! home_directory.empty ())
    {
      home_rc = home_directory;
      home_rc.append ("/.octaverc");
      parse_and_execute (home_rc, 0, verbose);

      // Names alone are not enough.

      file_stat fs_home_rc (home_rc);

      if (fs_home_rc)
	{
	  file_stat fs_dot_rc ("./.octaverc");

	  if (fs_dot_rc && fs_home_rc.ino () == fs_dot_rc.ino ())
	    home_rc_already_executed = 1;
	}
    }

  if (! home_rc_already_executed)
    parse_and_execute ("./.octaverc", 0, verbose);

  run_unwind_frame ("execute_startup_files");
}

// Usage message with extra help.

static void
verbose_usage (void)
{
  cout << "\n" OCTAVE_NAME_VERSION_AND_COPYRIGHT "\n\
\n\
Usage: octave [options]\n\
\n\
Options:\n\
\n\
  -d, --debug             Enter parser debugging mode.\n\
  -x, --echo-commands     Echo commands as they are executed.\n\
  --exec-path PATH        Set path for executing subprograms.\n\
  -h, -?, --help          Print short help message and exit.\n\
  -f, --ignore-init-file  Don't read any initialization files.\n\
  --info-file FILE        Use top-level info file FILE.\n\
  --info-program PROGRAM  Use PROGRAM for reading info files.\n\
  -i, --interactive       Force interactive behavior.\n\
  -p PATH, --path PATH    Set initial LOADPATH to PATH.\n\
  -q, --silent            Don't print message at startup.\n\
  --traditional           Set compatibility variables.\n\
  -V, --verbose           Enable verbose output in some cases.\n\
  -v, --version           Print version number and exit.\n\
\n\
  FILE                    Execute commands from FILE.\n\
\n";

  exit (0);
}

// Terse usage messsage.

static void
usage (void)
{
  cerr << "usage: " << usage_string << "\n";
  exit (1);
}

static void
print_version_and_exit (void)
{
  cout << OCTAVE_NAME_AND_VERSION << "\n";
  exit (0);
}

static void
initialize_error_handlers ()
{
  set_liboctave_error_handler (error);
}

// What happens on --traditional.

static void
maximum_braindamage (void)
{
  bind_builtin_variable ("PS1", ">> ");
  bind_builtin_variable ("PS2", "");
  bind_builtin_variable ("beep_on_error", "true");
  bind_builtin_variable ("default_save_format", "mat-binary");
  bind_builtin_variable ("define_all_return_values", "true");
  bind_builtin_variable ("do_fortran_indexing", "true");
  bind_builtin_variable ("empty_list_elements_ok", "true");
  bind_builtin_variable ("implicit_str_to_num_ok", "true");
  bind_builtin_variable ("ok_to_lose_imaginary_part", "true");
  bind_builtin_variable ("page_screen_output", "false");
  bind_builtin_variable ("prefer_column_vectors", "false");
  bind_builtin_variable ("prefer_zero_one_indexing", "true");
  bind_builtin_variable ("print_empty_dimensions", "false");
  bind_builtin_variable ("treat_neg_dim_as_zero", "true");
  bind_builtin_variable ("warn_function_name_clash", "false");
  bind_builtin_variable ("whitespace_in_literal_matrix", "traditional");
}

// You guessed it.

int
main (int argc, char **argv)
{
  int echo_commands = ECHO_OFF;

  // The order of these calls is important, and initialize_globals
  // must come before the options are processed because some command
  // line options override defaults.

  init_user_prefs ();

  initialize_pager ();

  sysdep_init ();

  initialize_error_handlers ();

  initialize_globals (argv[0]);

  initialize_pathsearch (argv[0]);

  int optc;
  while ((optc = getopt_long (argc, argv, short_opts, long_opts, 0)) != EOF)
    {
      switch (optc)
	{
	case 'V':
	  verbose_flag++;
	  break;

	case 'd':
	  yydebug++;
	  break;

	case 'f':
	  read_init_files = 0;
	  break;

	case 'h':
	case '?':
	  verbose_usage ();
	  break;

	case 'i':
	  forced_interactive = 1;
	  break;

	case 'p':
	  if (optarg)
	    load_path = string (optarg);
	  break;

	case 'q':
	  inhibit_startup_message = 1;
	  break;

	case 'x':
	  echo_commands = (ECHO_SCRIPTS | ECHO_FUNCTIONS | ECHO_CMD_LINE);
	  break;

	case 'v':
	  print_version_and_exit ();
	  break;

	case EXEC_PATH_OPTION:
	  if (optarg)
	    exec_path = string (optarg);
	  break;

	case INFO_FILE_OPTION:
	  if (optarg)
	    info_file = string (optarg);
	  break;

	case INFO_PROG_OPTION:
	  if (optarg)
	    info_prog = string (optarg);
	  break;

	case TRADITIONAL_OPTION:
	  traditional = 1;
	  break;

	default:
	  usage ();
	  break;
	}
    }

#if defined (HAVE_ATEXIT) || defined (HAVE_ON_EXIT)
  // Make sure we clean up when we exit.  If we don't have atexit or
  // on_exit, we're going to leave some junk files around if we exit
  // abnormally.
  atexit (cleanup_tmp_files);
#endif

  // These can come after command line args since none of them set any
  // defaults that might be changed by command line options.

  install_signal_handlers ();

  initialize_file_io ();

  initialize_symbol_tables ();  

  install_builtins ();

  initialize_readline ();

  init_dynamic_linker ();

  if (! inhibit_startup_message)
    cout << OCTAVE_STARTUP_MESSAGE "\n" << endl;

  if (traditional)
    maximum_braindamage ();

  bind_builtin_variable ("echo_executing_commands",
			 (double) echo_commands);

  if (read_init_files)
    execute_startup_files ();

  initialize_history ();

  if (! inhibit_startup_message && reading_startup_message_printed)
    cout << endl;

  // Avoid counting commands executed from startup files.

  current_command_number = 1;

  // If there is an extra argument, see if it names a file to read.
  // Additional arguments are taken as command line options for the
  // script.

  int remaining_args = argc - optind;
  if (remaining_args > 0)
    {
      reading_script_file = 1;
      curr_fcn_file_name = argv[optind];
      curr_fcn_file_full_name = curr_fcn_file_name;

      FILE *infile = get_input_from_file (curr_fcn_file_name);

      if (infile)
	{
	  input_from_command_line_file = 1;

	  bind_builtin_variable ("program_invocation_name",
				 curr_fcn_file_name);

	  size_t pos = curr_fcn_file_name.rfind ('/');

	  string tmp = (pos != NPOS)
	    ? curr_fcn_file_name.substr (pos+1) : curr_fcn_file_name;

	  bind_builtin_variable ("program_name", tmp);

	  intern_argv (remaining_args, argv+optind);

	  rl_blink_matching_paren = 0;
	  switch_to_buffer (create_buffer (infile));
	}
      else
	clean_up_and_exit (1);
    }
  else
    {
      // Is input coming from a terminal?  If so, we are probably
      // interactive.

      interactive = (isatty (fileno (stdin)) && isatty (fileno (stdout)));

      intern_argv (argc, argv);

      switch_to_buffer (create_buffer (get_input_from_stdin ()));
    }

  // Force input to be echoed if not really interactive, but the user
  // has forced interactive behavior.

  if (!interactive && forced_interactive)
    {
      rl_blink_matching_paren = 0;

      // XXX FIXME XXX -- is this the right thing to do?

      bind_builtin_variable ("echo_executing_commands",
			     (double) ECHO_CMD_LINE);
    }

  if (! interactive)
    using_readline = 0;

  // Allow the user to interrupt us without exiting.

  if (setjmp (toplevel) != 0)
    {
      raw_mode (0);

      cout << "\n";
    }

  can_interrupt = 1;

  catch_interrupts ();

  // The big loop.

  int retval;
  do
    {
      curr_sym_tab = top_level_sym_tab;

      reset_parser ();

      retval = yyparse ();

      if (retval == 0 && global_command)
	{
	  global_command->eval (1);
	  delete global_command;
	  current_command_number++;
	}
    }
  while (retval == 0);

  if (retval == 1 && ! error_state)
    retval = 0;

  clean_up_and_exit (retval);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
