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
#include <cstdlib>
#include <cstring>
#include <ctime>

#include <fstream>
#include <iostream>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "cmd-edit.h"
#include "f77-fcn.h"
#include "file-ops.h"
#include "file-stat.h"
#include "lo-error.h"
#include "oct-env.h"
#include "pathsearch.h"
#include "str-vec.h"

#include <defaults.h>
#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "file-io.h"
#include "input.h"
#include "lex.h"
#include "octave.h"
#include "oct-hist.h"
#include "oct-obj.h"
#include "ops.h"
#include "toplev.h"
#include "parse.h"
#include "procstream.h"
#include "prog-args.h"
#include "sighandlers.h"
#include "sysdep.h"
#include "ov.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include <version.h>

// Kluge.
extern "C" void F77_FUNC (xerbla, XERBLA) (const char *, int, long);

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
  "octave [-?HVdfhiqvx] [--debug] [--echo-commands] [--eval CODE]\n\
       [--exec-path path] [--help] [--info-file file] [--info-program prog]\n\
       [--interactive] [--no-history] [--no-init-file] [--no-line-editing]\n\
       [--no-site-file] [-p path] [--path path] [--silent] [--traditional]\n\
       [--verbose] [--version] [file]";

// This is here so that it's more likely that the usage message and
// the real set of options will agree.  Note: the `+' must come first
// to prevent getopt from permuting arguments!
static const char *short_opts = "+?HVdfhip:qvx";

// The code to evaluate at startup (--eval CODE)
static std::string code_to_eval;

// If TRUE, don't exit after evaluating code given by --eval option.
static bool persist = false;

// Long options.  See the comments in getopt.h for the meanings of the
// fields in this structure.
#define EVAL_OPTION 1
#define EXEC_PATH_OPTION 2
#define INFO_FILE_OPTION 3
#define INFO_PROG_OPTION 4
#define NO_INIT_FILE_OPTION 5
#define NO_LINE_EDITING_OPTION 6
#define NO_SITE_FILE_OPTION 7
#define PERSIST_OPTION 8
#define TRADITIONAL_OPTION 9
long_options long_opts[] =
  {
    { "debug",            prog_args::no_arg,       0, 'd' },
    { "braindead",        prog_args::no_arg,       0, TRADITIONAL_OPTION },
    { "echo-commands",    prog_args::no_arg,       0, 'x' },
    { "eval",             prog_args::required_arg, 0, EVAL_OPTION },
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
    { "persist",          prog_args::no_arg,       0, PERSIST_OPTION },
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
  symbol_record *nargin_sr = top_level_sym_tab->lookup ("__nargin__", true);

  nargin_sr->mark_as_static ();

  nargin_sr->define (argc-1);

  Cell args;

  if (argc > 1)
    {
      Array<octave_value> tmp (argc-1);

      // Skip program name in argv.
      int i = argc;
      while (--i > 0)
	tmp(i-1) = octave_value (*(argv+i));

      args = Cell (tmp, argc-1, 1);
    }

  bind_builtin_constant ("argv", args, true, true);
}

static void
initialize_pathsearch (void)
{
  // This may seem odd, but doing it this way means that we don't have
  // to modify the kpathsea library...

  std::string odb = octave_env::getenv ("OCTAVE_DB_PATH");

  // For backward compatibility.

  if (odb.empty ())
    odb = octave_env::getenv ("OCTAVE_DB_DIR");

  if (odb.empty ())
    odb = Vdata_dir + file_ops::dir_sep_str + "octave:"
      + Vlibexec_dir + file_ops::dir_sep_str + "octave";
}

// Initialize by reading startup files.

static void
execute_startup_files (void)
{
  unwind_protect::begin_frame ("execute_startup_files");

  unwind_protect_bool (input_from_startup_file);

  input_from_startup_file = true;

  int verbose = (verbose_flag && ! inhibit_startup_message);

  if (read_site_files)
    {
      // Execute commands from the site-wide configuration file.
      // First from the file $(prefix)/lib/octave/site/m/octaverc
      // (if it exists), then from the file
      // $(prefix)/share/octave/$(version)/m/octaverc (if it exists).

      parse_and_execute (Vlocal_site_defaults_file, verbose);

      parse_and_execute (Vsite_defaults_file, verbose);
    }

  if (read_init_files)
    {
      // Try to execute commands from $HOME/$OCTAVE_INITFILE and
      // $OCTAVE_INITFILE.  If $OCTAVE_INITFILE is not set, .octaverc
      // is assumed.

      bool home_rc_already_executed = false;

      std::string initfile = octave_env::getenv ("OCTAVE_INITFILE");

      if (initfile.empty ())
	initfile = ".octaverc";

      std::string home_dir = octave_env::get_home_directory ();

      std::string home_rc = octave_env::make_absolute (initfile, home_dir);

      std::string local_rc;

      if (! home_dir.empty ())
	{
	  parse_and_execute (home_rc, verbose);

	  // Names alone are not enough.

	  file_stat fs_home_rc (home_rc);

	  if (fs_home_rc)
	    {
	      // We want to check for curr_dir after executing home_rc
	      // because doing that may change the working directory.

	      std::string curr_dir = octave_env::getcwd ();

	      local_rc = octave_env::make_absolute (initfile, curr_dir);

	      file_stat fs_dot_rc (local_rc);

	      if (fs_dot_rc && fs_home_rc.ino () == fs_dot_rc.ino ())
		home_rc_already_executed = true;
	    }
	}

      if (! home_rc_already_executed)
	{
	  if (local_rc.empty ())
	    {
	      std::string curr_dir = octave_env::getcwd ();

	      local_rc = octave_env::make_absolute (initfile, curr_dir);
	    }

	  parse_and_execute (local_rc, verbose);
	}
    }

  unwind_protect::run_frame ("execute_startup_files");
}

static int
execute_eval_option_code (const std::string& code)
{
  unwind_protect::begin_frame ("execute_eval_option_code");

  unwind_protect_bool (interactive);

  interactive = false;

  int parse_status = 0;

  eval_string (code, false, parse_status, 0);

  unwind_protect::run_frame ("execute_eval_option_code");

  return parse_status;
}

static void
restore_program_name (void *)
{
  bind_builtin_variable ("program_invocation_name",
			 octave_env::get_program_invocation_name ());

  bind_builtin_variable ("program_name", octave_env::get_program_name ());
}

static void
execute_command_line_file (const std::string& fname)
{
  unwind_protect::begin_frame ("execute_command_line_file");

  unwind_protect_bool (interactive);
  unwind_protect_bool (reading_script_file);
  unwind_protect_bool (input_from_command_line_file);

  unwind_protect_str (curr_fcn_file_name);
  unwind_protect_str (curr_fcn_file_full_name);

  unwind_protect::add (restore_program_name, 0);

  interactive = false;
  reading_script_file = true;
  input_from_command_line_file = true;

  curr_fcn_file_name = fname;
  curr_fcn_file_full_name = curr_fcn_file_name;

  bind_builtin_variable ("program_invocation_name", curr_fcn_file_name);

  size_t pos = curr_fcn_file_name.find_last_of (file_ops::dir_sep_chars);
  
  std::string tmp = (pos != NPOS)
    ? curr_fcn_file_name.substr (pos+1) : curr_fcn_file_name;

  bind_builtin_variable ("program_name", tmp);

  parse_and_execute (fname, false, "octave");
 
  unwind_protect::run_frame ("execute_command_line_file");
}

// Usage message with extra help.

static void
verbose_usage (void)
{
  std::cout << OCTAVE_NAME_VERSION_COPYRIGHT_COPYING_AND_WARRANTY "\n\
\n\
Usage: octave [options]\n\
\n\
Options:\n\
\n\
  --debug, -d             Enter parser debugging mode.\n\
  --echo-commands, -x     Echo commands as they are executed.\n\
  --eval CODE             Evaluate CODE.  Exit when done unless --persist.\n\
  --exec-path PATH        Set path for executing subprograms.\n\
  --help, -h, -?          Print short help message and exit.\n\
  --info-file FILE        Use top-level info file FILE.\n\
  --info-program PROGRAM  Use PROGRAM for reading info files.\n\
  --interactive, -i       Force interactive behavior.\n\
  --no-history, -H        Don't save commands to the history list\n\
  --no-init-file          Don't read the ~/.octaverc or .octaverc files.\n\
  --no-line-editing       Don't use readline for command-line editing.\n\
  --no-site-file          Don't read the site-wide octaverc file.\n\
  --norc, -f              Don't read any initialization files.\n\
  --path PATH, -p PATH    Set initial LOADPATH to PATH.\n\
  --persist               Go interactive after --eval or reading from FILE.\n\
  --silent, -q            Don't print message at startup.\n\
  --traditional           Set compatibility variables.\n\
  --verbose, -V           Enable verbose output in some cases.\n\
  --version, -v           Print version number and exit.\n\
\n\
  FILE                    Execute commands from FILE.  Exit when done\n\
                          unless --persist is also specified.\n\
\n"
OCTAVE_WWW_STATEMENT "\n\
\n"
OCTAVE_CONTRIB_STATEMENT "\n\
\n"
OCTAVE_BUGS_STATEMENT "\n";

  exit (0);
}

// Terse usage messsage.

static void
usage (void)
{
  std::cerr << "usage: " << usage_string << "\n";
  exit (1);
}

static void
print_version_and_exit (void)
{
  std::cout << OCTAVE_NAME_VERSION_COPYRIGHT_COPYING_WARRANTY_AND_BUGS "\n";
  exit (0);
}

static void
initialize_error_handlers ()
{
  set_liboctave_error_handler (error);
  set_liboctave_warning_handler (warning);
}

// What happens on --traditional.

static void
maximum_braindamage (void)
{
  persist = true;

  bind_builtin_variable ("PS1", ">> ");
  bind_builtin_variable ("PS2", "");
  bind_builtin_variable ("beep_on_error", true);
  bind_builtin_variable ("crash_dumps_octave_core", false);
  bind_builtin_variable ("default_save_format", "mat-binary");
  bind_builtin_variable ("fixed_point_format", true);
  bind_builtin_variable ("page_screen_output", false);
  bind_builtin_variable ("print_empty_dimensions", false);
  bind_builtin_variable ("warn_function_name_clash", false);
}

// You guessed it.

int
octave_main (int argc, char **argv, int embedded)
{
  octave_env::set_program_name (argv[0]);

  // The order of these calls is important.  The call to
  // install_defaults must come before install_builtins because
  // default variable values must be available for the variables to be
  // installed, and the call to install_builtins must come before the
  // options are processed because some command line options override
  // defaults by calling bind_builtin_variable.

  sysdep_init ();

  // The idea here is to force xerbla to be referenced so that we will
  // link to our own version instead of the one provided by the BLAS
  // library.  But octave_NaN should never be -1, so we should never
  // actually call xerbla.

  if (octave_NaN == -1)
    F77_FUNC (xerbla, XERBLA) ("octave", 13, 6L);

  initialize_error_handlers ();

  install_defaults ();

  initialize_pathsearch ();

  if (! embedded)
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
	  bind_builtin_variable ("saving_history", false);
	  break;

	case 'V':
	  verbose_flag = true;
	  break;

	case 'd':
	  // This is the same as yydebug in parse.y.
	  octave_debug++;
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

	case EVAL_OPTION:
	  if (args.optarg ())
	    {
	      if (code_to_eval.empty ())
		code_to_eval = args.optarg ();
	      else
		code_to_eval += std::string (" ") + args.optarg ();
	    }
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

	case PERSIST_OPTION:
	  persist = true;
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
#endif

  // These can come after command line args since none of them set any
  // defaults that might be changed by command line options.

  initialize_command_input ();

  if (! inhibit_startup_message)
    std::cout << OCTAVE_STARTUP_MESSAGE "\n" << std::endl;

  if (traditional)
    maximum_braindamage ();

  octave_interpreter_ready = true;

  execute_default_pkg_add_files ();

  execute_startup_files ();

  command_history::read (false);

  if (! inhibit_startup_message && reading_startup_message_printed)
    std::cout << std::endl;

  // Is input coming from a terminal?  If so, we are probably
  // interactive.

  interactive = (! embedded
		 && isatty (fileno (stdin)) && isatty (fileno (stdout)));

  if (! interactive)
    line_editing = false;

  // If there is an extra argument, see if it names a file to read.
  // Additional arguments are taken as command line options for the
  // script.

  int last_arg_idx = args.optind ();

  int remaining_args = argc - last_arg_idx;

  if (! code_to_eval.empty ())
    {
      // We probably want all the args for an --eval option.

      intern_argv (argc, argv);

      int parse_status = execute_eval_option_code (code_to_eval);

      if (! (persist && remaining_args > 0))
	return (parse_status || error_state ? 1 : 0);
    }

  if (remaining_args > 0)
    {
      // If we are running an executable script (#! /bin/octave) then
      // we should only see the args passed to the script.

      intern_argv (remaining_args, argv+last_arg_idx);

      execute_command_line_file (argv[last_arg_idx]);

      if (! persist)
	return (error_state ? 1 : 0);
    }

  // Avoid counting commands executed from startup files.

  command_editor::reset_current_command_number (1);

  // Now argv should have the full set of args.
  intern_argv (argc, argv);

  if (! embedded)
    switch_to_buffer (create_buffer (get_input_from_stdin ()));

  // Force input to be echoed if not really interactive, but the user
  // has forced interactive behavior.

  if (! interactive && forced_interactive)
    {
      command_editor::blink_matching_paren (false);

      // XXX FIXME XXX -- is this the right thing to do?

      bind_builtin_variable ("echo_executing_commands", ECHO_CMD_LINE);
    }

  if (embedded)
    return 1;

  int retval = main_loop ();

  if (retval == 1 && ! error_state)
    retval = 0;

  clean_up_and_exit (retval);

  return 0;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
