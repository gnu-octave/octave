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

// Born February 20, 1992.

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>
#include <csignal>
#include <cstdlib>
#include <cstring>
#include <ctime>

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
#include "file-stat.h"
#include "lo-error.h"
#include "oct-env.h"
#include "pathsearch.h"
#include "str-vec.h"

#include <defaults.h>
#include "defun.h"
#include "dynamic-ld.h"
#include "error.h"
#include "file-io.h"
#include "input.h"
#include "lex.h"
#include "oct-hist.h"
#include "ops.h"
#include "toplev.h"
#include "parse.h"
#include "pt-plot.h"
#include "procstream.h"
#include "prog-args.h"
#include "sighandlers.h"
#include "sysdep.h"
#include "ov.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include <version.h>

extern void install_builtins (void);

#if !defined (HAVE_ATEXIT) && defined (HAVE_ON_EXIT)
extern "C" int on_exit ();
#define atexit on_exit
#endif

// The command-line options.
static string_vector octave_argv;

// TRUE means we read ~/.octaverc and ./.octaverc.
// (--norc; --no-init-file; -f)
static bool read_init_files = true;

// TRUE means we read the site-wide octaverc files.
// (--norc; --no-site-file; -f)
static bool read_site_files = true;

// TRUE means we don't print the usual startup message.
// (--quiet; --silent; -q)
static bool inhibit_startup_message = false;

// TRUE means we turn on compatibility options.
// (--traditional)
static bool traditional = false;

// If TRUE, print verbose info in some cases.
// (--verbose; -V)
static bool verbose_flag = false;

// Usage message
static const char *usage_string = 
  "octave [-?HVdfhiqvx] [--debug] [--echo-commands] [--exec-path path]\n\
       [--help] [--info-file file] [--info-program prog] [--interactive]\n\
       [--no-history] [--no-init-file] [--no-line-editing] [--no-site-file]\n\
       [-p path] [--path path] [--silent] [--traditional] [--verbose]\n\
       [--version] [file]";

// This is here so that it's more likely that the usage message and
// the real set of options will agree.  Note: the `+' must come first
// to prevent getopt from permuting arguments!
static const char *short_opts = "+?HVdfhip:qvx";

// Long options.  See the comments in getopt.h for the meanings of the
// fields in this structure.
#define EXEC_PATH_OPTION 1
#define INFO_FILE_OPTION 2
#define INFO_PROG_OPTION 3
#define NO_INIT_FILE_OPTION 4
#define NO_LINE_EDITING_OPTION 5
#define NO_SITE_FILE_OPTION 6
#define TRADITIONAL_OPTION 7
long_options long_opts[] =
  {
    { "debug",            prog_args::no_arg,       0, 'd' },
    { "braindead",        prog_args::no_arg,       0, TRADITIONAL_OPTION },
    { "echo-commands",    prog_args::no_arg,       0, 'x' },
    { "exec-path",        prog_args::required_arg, 0, EXEC_PATH_OPTION },
    { "help",             prog_args::no_arg,       0, 'h' },
    { "info-file",        prog_args::required_arg, 0, INFO_FILE_OPTION },
    { "info-program",     prog_args::required_arg, 0, INFO_PROG_OPTION },
    { "interactive",      prog_args::no_arg,       0, 'i' },
    { "no-history",       prog_args::no_arg,       0, 'H' },
    { "no-init-file",     prog_args::no_arg,       0, NO_INIT_FILE_OPTION },
    { "no-line-editing",  prog_args::no_arg,       0, NO_LINE_EDITING_OPTION },
    { "no-site-file",     prog_args::no_arg,       0, NO_SITE_FILE_OPTION },
    { "norc",             prog_args::no_arg,       0, 'f' },
    { "path",             prog_args::required_arg, 0, 'p' },
    { "quiet",            prog_args::no_arg,       0, 'q' },
    { "silent",           prog_args::no_arg,       0, 'q' },
    { "traditional",      prog_args::no_arg,       0, TRADITIONAL_OPTION },
    { "verbose",          prog_args::no_arg,       0, 'V' },
    { "version",          prog_args::no_arg,       0, 'v' },
    { 0,                  0,                       0, 0 }
  };

// Store the command-line options for later use.

static void
intern_argv (int argc, char **argv)
{
  if (argc > 1)
    {
      // Skip program name in argv.

      octave_argv = string_vector (argv+1, argc-1);

      bind_builtin_variable ("argv", octave_argv, 1, 1, 0);
      bind_builtin_variable ("__argv__", octave_argv, 1, 1, 0);
    }

  bind_builtin_variable ("nargin", static_cast<double> (argc-1), 1, 1, 0);
}

static void
initialize_pathsearch (void)
{
  // This may seem odd, but doing it this way means that we don't have
  // to modify the kpathsea library...

  string odb = octave_env::getenv ("OCTAVE_DB_PATH");

  // For backward compatibility.

  if (odb.empty ())
    odb = octave_env::getenv ("OCTAVE_DB_DIR");

  if (odb.empty ())
    odb = Vdata_dir + string ("/octave:") + Vlibexec_dir + string ("/octave");

  octave_env::putenv ("TEXMFDBS", odb);
}

// Initialize by reading startup files.

static void
execute_startup_files (void)
{
  unwind_protect::begin_frame ("execute_startup_files");

  // XXX FIXME XXX -- need to make it possible to set this in startup
  // files.

  unwind_protect_bool (input_from_startup_file);

  input_from_startup_file = true;

  int verbose = (verbose_flag && ! inhibit_startup_message);

  if (read_site_files)
    {
      // Execute commands from the site-wide configuration file.
      // First from the file $(prefix)/lib/octave/site/m/octaverc
      // (if it exists), then from the file
      // $(prefix)/lib/octave/$(version)/m/octaverc (if it exists).

      parse_and_execute (Vlocal_site_defaults_file, verbose);

      parse_and_execute (Vsite_defaults_file, verbose);
    }

  if (read_init_files)
    {
      // Try to execute commands from $HOME/$OCTAVE_INITFILE and
      // $OCTAVE_INITFILE.  If $OCTAVE_INITFILE is not set, .octaverc
      // is assumed.

      int home_rc_already_executed = 0;

      string initfile = octave_env::getenv ("OCTAVE_INITFILE");

      if (initfile.empty ())
	initfile = ".octaverc";

      string home_dir = octave_env::get_home_directory ();

      string home_rc = home_dir + "/" + initfile;
      string local_rc = string ("./") + initfile;

      if (! home_dir.empty ())
	{
	  parse_and_execute (home_rc, verbose);

	  // Names alone are not enough.

	  file_stat fs_home_rc (home_rc);

	  if (fs_home_rc)
	    {
	      file_stat fs_dot_rc (local_rc);

	      if (fs_dot_rc && fs_home_rc.ino () == fs_dot_rc.ino ())
		home_rc_already_executed = 1;
	    }
	}

      if (! home_rc_already_executed)
	parse_and_execute (local_rc, verbose);
    }

  unwind_protect::run_frame ("execute_startup_files");
}

// Usage message with extra help.

static void
verbose_usage (void)
{
  cout << OCTAVE_NAME_VERSION_AND_COPYRIGHT "\n\
\n\
Usage: octave [options]\n\
\n\
Options:\n\
\n\
  --debug, -d             Enter parser debugging mode.\n\
  --echo-commands, -x     Echo commands as they are executed.\n\
  --exec-path PATH        Set path for executing subprograms.\n\
  --help, -h, -?          Print short help message and exit.\n\
  --norc, -f              Don't read any initialization files.\n\
  --info-file FILE        Use top-level info file FILE.\n\
  --info-program PROGRAM  Use PROGRAM for reading info files.\n\
  --interactive, -i       Force interactive behavior.\n\
  --no-history, -H        Don't save commands to the history list\n\
  --no-init-file          Don't read the ~/.octaverc or .octaverc files.\n\
  --no-line-editing       Don't use readline for command-line editing.\n\
  --no-site-file          Don't read the site-wide octaverc file.\n\
  --path PATH, -p PATH    Set initial LOADPATH to PATH.\n\
  --silent, -q            Don't print message at startup.\n\
  --traditional           Set compatibility variables.\n\
  --verbose, -V           Enable verbose output in some cases.\n\
  --version, -v           Print version number and exit.\n\
\n\
  FILE                    Execute commands from FILE.\n\
\n\
Additional information about Octave is available via the WWW at\n\
http://www.che.wisc.edu/octave/octave.html.\n\
\n\
Please report bugs to the mailing list `bug-octave@bevo.che.wisc.edu'.\n";

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
  bind_builtin_variable ("beep_on_error", 1.0);
  bind_builtin_variable ("crash_dumps_octave_core", 0.0);
  bind_builtin_variable ("default_eval_print_flag", 0.0);
  bind_builtin_variable ("default_global_variable_value", Matrix ());
  bind_builtin_variable ("default_save_format", "mat-binary");
  bind_builtin_variable ("define_all_return_values", 1.0);
  bind_builtin_variable ("do_fortran_indexing", 1.0);
  bind_builtin_variable ("empty_list_elements_ok", 1.0);
  bind_builtin_variable ("fixed_point_format", 1.0);
  bind_builtin_variable ("implicit_num_to_str_ok", 1.0);
  bind_builtin_variable ("implicit_str_to_num_ok", 1.0);
  bind_builtin_variable ("initialize_global_variables", 1.0);
  bind_builtin_variable ("ok_to_lose_imaginary_part", 1.0);
  bind_builtin_variable ("page_screen_output", 0.0);
  bind_builtin_variable ("prefer_column_vectors", 0.0);
  bind_builtin_variable ("print_empty_dimensions", 0.0);
  bind_builtin_variable ("treat_neg_dim_as_zero", 1.0);
  bind_builtin_variable ("warn_function_name_clash", 0.0);
  bind_builtin_variable ("whitespace_in_literal_matrix", "traditional");
}

// You guessed it.

int
main (int argc, char **argv)
{
  octave_env::set_program_name (argv[0]);

  dir_path::set_program_name (argv[0]);

  // The order of these calls is important.  The call to
  // install_defaults must come before install_builtins because
  // default variable values must be available for the variables to be
  // installed, and the call to install_builtins must come before the
  // options are processed because some command line options override
  // defaults by calling bind_builtin_variable.

  sysdep_init ();

  initialize_error_handlers ();

  install_defaults ();

  initialize_pathsearch ();

  install_signal_handlers ();

  initialize_file_io ();

  initialize_symbol_tables ();

  install_types ();

  install_ops ();

  install_builtins ();

  prog_args args (argc, argv, short_opts, long_opts);

  int optc;
  while ((optc = args.getopt ()) != EOF)
    {
      switch (optc)
	{
	case 'H':
	  bind_builtin_variable ("saving_history", 0.0);
	  break;

	case 'V':
	  verbose_flag = true;
	  break;

	case 'd':
	  yydebug++;
	  break;

	case 'f':
	  read_init_files = false;
	  read_site_files = false;
	  break;

	case 'h':
	case '?':
	  verbose_usage ();
	  break;

	case 'i':
	  forced_interactive = true;
	  break;

	case 'p':
	  if (args.optarg ())
	    bind_builtin_variable ("LOADPATH", args.optarg ());
	  break;

	case 'q':
	  inhibit_startup_message = true;
	  break;

	case 'x':
	  {
	    double tmp = (ECHO_SCRIPTS | ECHO_FUNCTIONS | ECHO_CMD_LINE);
	    bind_builtin_variable ("echo_executing_commands", tmp);
	  }
	  break;

	case 'v':
	  print_version_and_exit ();
	  break;

	case EXEC_PATH_OPTION:
	  if (args.optarg ())
	    bind_builtin_variable ("EXEC_PATH", args.optarg ());
	  break;

	case INFO_FILE_OPTION:
	  if (args.optarg ())
	    bind_builtin_variable ("INFO_FILE", args.optarg ());
	  break;

	case INFO_PROG_OPTION:
	  if (args.optarg ())
	    bind_builtin_variable ("INFO_PROGRAM", args.optarg ());
	  break;

	case NO_INIT_FILE_OPTION:
	  read_init_files = false;
	  break;

	case NO_LINE_EDITING_OPTION:
	  line_editing = false;
	  break;

	case NO_SITE_FILE_OPTION:
	  read_site_files = 0;
	  break;

	case TRADITIONAL_OPTION:
	  traditional = true;
	  break;

	default:
	  usage ();
	  break;
	}
    }

#if defined (HAVE_ATEXIT) || defined (HAVE_ON_EXIT)
  // Make sure we clean up when we exit.  Also allow users to register
  // functions.  If we don't have atexit or on_exit, we're going to
  // leave some junk files around if we exit abnormally.

  atexit (do_octave_atexit);

  atexit (cleanup_tmp_files);
#endif

  // These can come after command line args since none of them set any
  // defaults that might be changed by command line options.

  initialize_command_input ();

  if (! inhibit_startup_message)
    cout << OCTAVE_STARTUP_MESSAGE "\n" << endl;

  if (traditional)
    maximum_braindamage ();

  execute_startup_files ();

  command_history::read (false);

  if (! inhibit_startup_message && reading_startup_message_printed)
    cout << endl;

  // Avoid counting commands executed from startup files.

  command_editor::reset_current_command_number (1);

  // If there is an extra argument, see if it names a file to read.
  // Additional arguments are taken as command line options for the
  // script.

  int last_arg_idx = args.optind ();

  int remaining_args = argc - last_arg_idx;

  if (remaining_args > 0)
    {
      reading_script_file = true;

      curr_fcn_file_name = argv[last_arg_idx];
      curr_fcn_file_full_name = curr_fcn_file_name;

      FILE *infile = get_input_from_file (curr_fcn_file_name);

      if (infile)
	{
	  input_from_command_line_file = true;

	  bind_builtin_variable ("program_invocation_name",
				 curr_fcn_file_name);

	  size_t pos = curr_fcn_file_name.rfind ('/');

	  string tmp = (pos != NPOS)
	    ? curr_fcn_file_name.substr (pos+1) : curr_fcn_file_name;

	  bind_builtin_variable ("program_name", tmp);

	  intern_argv (remaining_args, argv+last_arg_idx);

	  command_editor::blink_matching_paren (false);

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

  if (! interactive && forced_interactive)
    {
      command_editor::blink_matching_paren (false);

      // XXX FIXME XXX -- is this the right thing to do?

      bind_builtin_variable ("echo_executing_commands",
			     static_cast<double> (ECHO_CMD_LINE));
    }

  if (! interactive)
    line_editing = false;

  int retval = main_loop ();

  if (retval == 1 && ! error_state)
    retval = 0;

  clean_up_and_exit (retval);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
