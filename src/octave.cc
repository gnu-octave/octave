/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
              2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009 John W. Eaton

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

// Born February 20, 1992.

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <ctime>

#include <iostream>

#include <getopt.h>

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
#include "display.h"
#include "error.h"
#include "file-io.h"
#include "input.h"
#include "lex.h"
#include "load-path.h"
#include "octave.h"
#include "oct-hist.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "ops.h"
#include "ov.h"
#include "ov-range.h"
#include "toplev.h"
#include "parse.h"
#include "procstream.h"
#include "sighandlers.h"
#include "sysdep.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include <version.h>

// Kluge.
extern "C" F77_RET_T
F77_FUNC (xerbla, XERBLA) (F77_CONST_CHAR_ARG_DECL, const octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL);

extern void install_builtins (void);

// The command-line options.
static string_vector octave_argv;

// The name used to invoke Octave.
static std::string
octave_program_invocation_name;

// The last component of octave_program_invocation_name.
static std::string octave_program_name;

// TRUE means we read ~/.octaverc and ./.octaverc.
// (--norc; --no-init-file; -f)
static bool read_init_files = true;

// TRUE means we read the site-wide octaverc files.
// (--norc; --no-site-file; -f)
static bool read_site_files = true;

// TRUE means we set the initial path to configured defaults.
// (--no-init-path)
static bool set_initial_path = true;

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
  "octave [-HVdfhiqvx] [--debug] [--echo-commands] [--eval CODE]\n\
       [--exec-path path] [--help] [--image-path path] [--info-file file]\n\
       [--info-program prog] [--interactive] [--line-editing]\n\
       [--no-history] [--no-init-file] [--no-init-path] [--no-line-editing]\n\
       [--no-site-file] [--no-window-system] [-p path] [--path path]\n\
       [--silent] [--traditional] [--verbose] [--version] [file]";

// This is here so that it's more likely that the usage message and
// the real set of options will agree.  Note: the `+' must come first
// to prevent getopt from permuting arguments!
static const char *short_opts = "+HVdfhip:qvx";

// The code to evaluate at startup (--eval CODE)
static std::string code_to_eval;

// If TRUE, don't exit after evaluating code given by --eval option.
static bool persist = false;

// Long options.  See the comments in getopt.h for the meanings of the
// fields in this structure.
#define DOC_CACHE_FILE_OPTION 1
#define EVAL_OPTION 2
#define EXEC_PATH_OPTION 3
#define IMAGE_PATH_OPTION 4
#define INFO_FILE_OPTION 5
#define INFO_PROG_OPTION 6
#define LINE_EDITING_OPTION 7
#define NO_INIT_FILE_OPTION 8
#define NO_INIT_PATH_OPTION 9
#define NO_LINE_EDITING_OPTION 10
#define NO_SITE_FILE_OPTION 11
#define NO_WINDOW_SYSTEM_OPTION 12
#define PERSIST_OPTION 13
#define TRADITIONAL_OPTION 14
struct option long_opts[] =
  {
    { "braindead",        no_argument,       0, TRADITIONAL_OPTION },
    { "debug",            no_argument,       0, 'd' },
    { "doc-cache-file",   required_argument, 0, DOC_CACHE_FILE_OPTION },
    { "echo-commands",    no_argument,       0, 'x' },
    { "eval",             required_argument, 0, EVAL_OPTION },
    { "exec-path",        required_argument, 0, EXEC_PATH_OPTION },
    { "help",             no_argument,       0, 'h' },
    { "image-path",       required_argument, 0, IMAGE_PATH_OPTION },
    { "info-file",        required_argument, 0, INFO_FILE_OPTION },
    { "info-program",     required_argument, 0, INFO_PROG_OPTION },
    { "interactive",      no_argument,       0, 'i' },
    { "line-editing",     no_argument,       0, LINE_EDITING_OPTION },
    { "no-history",       no_argument,       0, 'H' },
    { "no-init-file",     no_argument,       0, NO_INIT_FILE_OPTION },
    { "no-init-path",     no_argument,       0, NO_INIT_PATH_OPTION },
    { "no-line-editing",  no_argument,       0, NO_LINE_EDITING_OPTION },
    { "no-site-file",     no_argument,       0, NO_SITE_FILE_OPTION },
    { "no-window-system", no_argument,       0, NO_WINDOW_SYSTEM_OPTION },
    { "norc",             no_argument,       0, 'f' },
    { "path",             required_argument, 0, 'p' },
    { "persist",          no_argument,       0, PERSIST_OPTION },
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
  assert (symbol_table::at_top_level ());

  symbol_table::varref (".nargin.") = argc - 1;

  symbol_table::mark_hidden (".nargin.");

  if (argc > 0)
    {
      octave_argv.resize (argc - 1);

      // Skip program name in argv.
      int i = argc;
      while (--i > 0)
        octave_argv[i-1] = *(argv+i);
    }
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
    odb = Vdata_dir + file_ops::dir_sep_str () + "octave:"
      + Vlibexec_dir + file_ops::dir_sep_str () + "octave";
}

DEFUN (__version_info__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {retval =} __version_info__ (@var{name}, @var{version}, @var{release}, @var{date})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

  static octave_map vinfo;

  int nargin = args.length ();

  if (nargin == 4)
    {
      if (vinfo.nfields () == 0)
        {
          vinfo.assign ("Name", args (0));
          vinfo.assign ("Version", args (1));
          vinfo.assign ("Release", args (2));
          vinfo.assign ("Date", args (3));
        }
      else
        {
          octave_idx_type n = vinfo.numel () + 1;

          vinfo.resize (dim_vector (n, 1));

          octave_value idx (n);

          vinfo.assign (idx, "Name", Cell (octave_value (args (0))));
          vinfo.assign (idx, "Version", Cell (octave_value (args (1))));
          vinfo.assign (idx, "Release", Cell (octave_value (args (2))));
          vinfo.assign (idx, "Date", Cell (octave_value (args (3))));
        }
    }
  else if (nargin == 0)
    retval = vinfo;
  else
    print_usage ();

  return retval;
}

static void
initialize_version_info (void)
{
  octave_value_list args;

  args(3) = OCTAVE_RELEASE_DATE;
  args(2) = OCTAVE_RELEASE;
  args(1) = OCTAVE_VERSION;
  args(0) = "GNU Octave";

  F__version_info__ (args, 0);
}

// Initialize by reading startup files.

static void
execute_startup_files (void)
{
  unwind_protect frame;

  frame.protect_var (input_from_startup_file);

  input_from_startup_file = true;

  std::string context;

  bool verbose = (verbose_flag && ! inhibit_startup_message);

  bool require_file = false;

  if (read_site_files)
    {
      // Execute commands from the site-wide configuration file.
      // First from the file $(prefix)/lib/octave/site/m/octaverc
      // (if it exists), then from the file
      // $(prefix)/share/octave/$(version)/m/octaverc (if it exists).

      source_file (Vlocal_site_defaults_file, context, verbose, require_file);

      source_file (Vsite_defaults_file, context, verbose, require_file);
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

      if (! home_rc.empty ())
        {
          source_file (home_rc, context, verbose, require_file);

          // Names alone are not enough.

          file_stat fs_home_rc (home_rc);

          if (fs_home_rc)
            {
              // We want to check for curr_dir after executing home_rc
              // because doing that may change the working directory.

              local_rc = octave_env::make_absolute (initfile);

              home_rc_already_executed = same_file (home_rc, local_rc);
            }
        }

      if (! home_rc_already_executed)
        {
          if (local_rc.empty ())
            local_rc = octave_env::make_absolute (initfile);

          source_file (local_rc, context, verbose, require_file);
        }
    }
}

static int
execute_eval_option_code (const std::string& code)
{
  unwind_protect frame;

  octave_save_signal_mask ();

  can_interrupt = true;

  octave_signal_hook = octave_signal_handler;
  octave_interrupt_hook = 0;
  octave_bad_alloc_hook = 0;

  octave_catch_interrupts ();

  octave_initialized = true;

  frame.protect_var (interactive);

  interactive = false;

  int parse_status = 0;

  try
    {
      eval_string (code, false, parse_status, 0);
    }
  catch (octave_interrupt_exception)
    {
      recover_from_exception ();
      octave_stdout << "\n";
      if (quitting_gracefully)
        clean_up_and_exit (exit_status);
    }
  catch (std::bad_alloc)
    {
      std::cerr << "error: memory exhausted or requested size too large for range of Octave's index type -- eval failed"
                << std::endl;
    }

  return parse_status;
}

static void
execute_command_line_file (const std::string& fname)
{
  unwind_protect frame;

  octave_save_signal_mask ();

  can_interrupt = true;

  octave_signal_hook = octave_signal_handler;
  octave_interrupt_hook = 0;
  octave_bad_alloc_hook = 0;

  octave_catch_interrupts ();

  octave_initialized = true;

  frame.protect_var (interactive);
  frame.protect_var (reading_script_file);
  frame.protect_var (input_from_command_line_file);

  frame.protect_var (curr_fcn_file_name);
  frame.protect_var (curr_fcn_file_full_name);

  frame.protect_var (octave_program_invocation_name);
  frame.protect_var (octave_program_name);

  interactive = false;
  reading_script_file = true;
  input_from_command_line_file = true;

  curr_fcn_file_name = fname;
  curr_fcn_file_full_name = curr_fcn_file_name;

  octave_program_invocation_name = curr_fcn_file_name;

  size_t pos = curr_fcn_file_name.find_last_of (file_ops::dir_sep_chars ());
  
  std::string tmp = (pos != std::string::npos)
    ? curr_fcn_file_name.substr (pos+1) : curr_fcn_file_name;

  octave_program_name = tmp;

  try
    {
      std::string context;
      bool verbose = false;
      bool require_file = true;

      source_file (fname, context, verbose, require_file, "octave");
    }
  catch (octave_interrupt_exception)
    {
      recover_from_exception ();
      octave_stdout << "\n";
      if (quitting_gracefully)
        clean_up_and_exit (exit_status);
    }
  catch (std::bad_alloc)
    {
      std::cerr << "error: memory exhausted or requested size too large for range of Octave's index type -- execution of "
                << fname << " failed" << std::endl;
    }
}

// Usage message with extra help.

static void
verbose_usage (void)
{
  std::cout << OCTAVE_NAME_VERSION_COPYRIGHT_COPYING_AND_WARRANTY "\n\
\n\
Usage: octave [options] [FILE]\n\
\n\
Options:\n\
\n\
  --debug, -d             Enter parser debugging mode.\n\
  --doc-cache-file FILE   Use doc cache file FILE.\n\
  --echo-commands, -x     Echo commands as they are executed.\n\
  --eval CODE             Evaluate CODE.  Exit when done unless --persist.\n\
  --exec-path PATH        Set path for executing subprograms.\n\
  --help, -h,             Print short help message and exit.\n\
  --image-path PATH       Add PATH to head of image search path.\n\
  --info-file FILE        Use top-level info file FILE.\n\
  --info-program PROGRAM  Use PROGRAM for reading info files.\n\
  --interactive, -i       Force interactive behavior.\n\
  --line-editing          Force readline use for command-line editing.\n\
  --no-history, -H        Don't save commands to the history list\n\
  --no-init-file          Don't read the ~/.octaverc or .octaverc files.\n\
  --no-init-path          Don't initialize function search path.\n\
  --no-line-editing       Don't use readline for command-line editing.\n\
  --no-site-file          Don't read the site-wide octaverc file.\n\
  --no-window-system      Disable window system, including graphics.\n\
  --norc, -f              Don't read any initialization files.\n\
  --path PATH, -p PATH    Add PATH to head of function search path.\n\
  --persist               Go interactive after --eval or reading from FILE.\n\
  --silent, -q            Don't print message at startup.\n\
  --traditional           Set variables for closer MATLAB compatibility.\n\
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
  std::cerr << "\nusage: " << usage_string << "\n\n";
  exit (1);
}

static void
print_version_and_exit (void)
{
  std::cout << OCTAVE_NAME_VERSION_COPYRIGHT_COPYING_WARRANTY_AND_BUGS "\n";
  exit (0);
}

static void
lo_error_handler (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  verror_with_cfn (fmt, args);
  va_end (args);

  octave_throw_execution_exception ();
}

static void
lo_error_with_id_handler (const char *id, const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  verror_with_id_cfn (id, fmt, args);
  va_end (args);

  octave_throw_execution_exception ();
}

static void
initialize_error_handlers ()
{
  set_liboctave_error_handler (lo_error_handler);
  set_liboctave_error_with_id_handler (lo_error_with_id_handler);
  set_liboctave_warning_handler (warning);
  set_liboctave_warning_with_id_handler (warning_with_id);
}

// What happens on --traditional.

static void
maximum_braindamage (void)
{
  persist = true;

  bind_internal_variable ("PS1", ">> ");
  bind_internal_variable ("PS2", "");
  bind_internal_variable ("allow_noninteger_range_as_index", true);
  bind_internal_variable ("beep_on_error", true);
  bind_internal_variable ("confirm_recursive_rmdir", false);
  bind_internal_variable ("crash_dumps_octave_core", false);
  bind_internal_variable ("default_save_options", "-mat-binary");
  bind_internal_variable ("do_braindead_shortcircuit_evaluation", true);
  bind_internal_variable ("fixed_point_format", true);
  bind_internal_variable ("history_timestamp_format_string",
                         "%%-- %D %I:%M %p --%%");
  bind_internal_variable ("page_screen_output", false);
  bind_internal_variable ("print_empty_dimensions", false);

  disable_warning ("Octave:abbreviated-property-match");
  disable_warning ("Octave:fopen-file-in-path");
  disable_warning ("Octave:function-name-clash");
  disable_warning ("Octave:load-file-in-path");
  disable_warning ("Octave:possible-matlab-short-circuit-operator");
}

// You guessed it.

int
octave_main (int argc, char **argv, int embedded)
{
  octave_env::set_program_name (argv[0]);

  octave_program_invocation_name = octave_env::get_program_invocation_name ();
  octave_program_name = octave_env::get_program_name ();

  // The order of these calls is important.  The call to
  // install_defaults must come before install_builtins because
  // default variable values must be available for the variables to be
  // installed, and the call to install_builtins must come before the
  // options are processed because some command line options override
  // defaults by calling bind_internal_variable.

  init_signals ();

  sysdep_init ();

  // The idea here is to force xerbla to be referenced so that we will
  // link to our own version instead of the one provided by the BLAS
  // library.  But octave_NaN should never be -1, so we should never
  // actually call xerbla.

  if (octave_NaN == -1)
    F77_FUNC (xerbla, XERBLA) ("octave", 13, 6L);

  initialize_error_handlers ();

  initialize_default_warning_state ();

  install_defaults ();

  initialize_pathsearch ();

  if (! embedded)
    install_signal_handlers ();
  else
    quit_allowed = false;

  initialize_file_io ();

  install_types ();

  install_ops ();

  install_builtins ();

  bool forced_line_editing = false;

  bool read_history_file = true;

  while (true)
    {
      int long_idx;

      int optc = getopt_long (argc, argv, short_opts, long_opts, &long_idx);

      if (optc < 0)
        break;

      switch (optc)
        {
        case '?':
          // Unrecognized option.  getopt_long already printed a
          // message about that, so we will just print the usage string
          // and exit.
          usage ();
          break;

        case 'H':
          read_history_file = false;
          bind_internal_variable ("saving_history", false);
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
          verbose_usage ();
          break;

        case 'i':
          forced_interactive = true;
          break;

        case 'p':
          if (optarg)
            load_path::set_command_line_path (optarg);
          break;

        case 'q':
          inhibit_startup_message = true;
          break;

        case 'x':
          {
            double tmp = (ECHO_SCRIPTS | ECHO_FUNCTIONS | ECHO_CMD_LINE);
            bind_internal_variable ("echo_executing_commands", tmp);
          }
          break;

        case 'v':
          print_version_and_exit ();
          break;

        case DOC_CACHE_FILE_OPTION:
          if (optarg)
            bind_internal_variable ("doc_cache_file", optarg);
          break;

        case EVAL_OPTION:
          if (optarg)
            {
              if (code_to_eval.empty ())
                code_to_eval = optarg;
              else
                code_to_eval += std::string (" ") + optarg;
            }
          break;

        case EXEC_PATH_OPTION:
          if (optarg)
            set_exec_path (optarg);
          break;

        case IMAGE_PATH_OPTION:
          if (optarg)
            set_image_path (optarg);
          break;

        case INFO_FILE_OPTION:
          if (optarg)
            bind_internal_variable ("info_file", optarg);
          break;

        case INFO_PROG_OPTION:
          if (optarg)
            bind_internal_variable ("info_program", optarg);
          break;

        case LINE_EDITING_OPTION:
          forced_line_editing = true;
          break;

        case NO_INIT_FILE_OPTION:
          read_init_files = false;
          break;

        case NO_INIT_PATH_OPTION:
          set_initial_path = false;
          break;

        case NO_LINE_EDITING_OPTION:
          line_editing = false;
          break;

        case NO_SITE_FILE_OPTION:
          read_site_files = 0;
          break;

        case NO_WINDOW_SYSTEM_OPTION:
          display_info::no_window_system ();
          break;

        case PERSIST_OPTION:
          persist = true;
          break;

        case TRADITIONAL_OPTION:
          traditional = true;
          break;

        default:
          // getopt_long should print a message about unrecognized
          // options and return '?', which is handled above.  So if we
          // end up here, it is because there was an option but we
          // forgot to handle it.  That should be fatal.
          panic_impossible ();
          break;
        }
    }

  // Make sure we clean up when we exit.  Also allow users to register
  // functions.  If we don't have atexit or on_exit, we're going to
  // leave some junk files around if we exit abnormally.

  atexit (do_octave_atexit);

  // Is input coming from a terminal?  If so, we are probably
  // interactive.

  // If stdin is not a tty, then we are reading commands from a pipe or
  // a redirected file.
  stdin_is_tty = isatty (fileno (stdin));

  interactive = (! embedded && stdin_is_tty && isatty (fileno (stdout)));

  if (! interactive && ! forced_line_editing)
    line_editing = false;

  // Force default line editor if we don't want readline editing.
  if (! line_editing)
    command_editor::force_default_editor ();

  // These can come after command line args since none of them set any
  // defaults that might be changed by command line options.

  if (line_editing)
    initialize_command_input ();

  if (! inhibit_startup_message)
    std::cout << OCTAVE_STARTUP_MESSAGE "\n" << std::endl;

  if (traditional)
    maximum_braindamage ();

  octave_interpreter_ready = true;

  initialize_version_info ();

  // Make all command-line arguments available to startup files,
  // including PKG_ADD files.

  intern_argv (argc, argv);

  load_path::initialize (set_initial_path);

  execute_startup_files ();

  initialize_history (read_history_file);

  if (! inhibit_startup_message && reading_startup_message_printed)
    std::cout << std::endl;

  // If there is an extra argument, see if it names a file to read.
  // Additional arguments are taken as command line options for the
  // script.

  int last_arg_idx = optind;

  int remaining_args = argc - last_arg_idx;

  if (! code_to_eval.empty ())
    {
      int parse_status = execute_eval_option_code (code_to_eval);

      if (! (persist || remaining_args > 0))
        clean_up_and_exit (parse_status || error_state ? 1 : 0);
    }

  if (remaining_args > 0)
    {
      // If we are running an executable script (#! /bin/octave) then
      // we should only see the args passed to the script.

      intern_argv (remaining_args, argv+last_arg_idx);

      execute_command_line_file (argv[last_arg_idx]);

      if (! persist)
        clean_up_and_exit (error_state ? 1 : 0);
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

      // FIXME -- is this the right thing to do?

      bind_internal_variable ("echo_executing_commands", ECHO_CMD_LINE);
    }

  if (embedded)
    {
      // FIXME -- do we need to do any cleanup here before
      // returning?  If we don't, what will happen to Octave functions
      // that have been registered to execute with atexit, for example?

      return 1;
    }

  int retval = main_loop ();

  if (retval == 1 && ! error_state)
    retval = 0;

  clean_up_and_exit (retval);

  return 0;
}

DEFUN (argv, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} argv ()\n\
Return the command line arguments passed to Octave.  For example,\n\
if you invoked Octave using the command\n\
\n\
@example\n\
octave --no-line-editing --silent\n\
@end example\n\
\n\
@noindent\n\
@code{argv} would return a cell array of strings with the elements\n\
@option{--no-line-editing} and @option{--silent}.\n\
\n\
If you write an executable Octave script, @code{argv} will return the\n\
list of arguments passed to the script.  @xref{Executable Octave Programs},\n\
for an example of how to create an executable Octave script.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 0)
    retval = Cell (octave_argv);
  else
    print_usage ();

  return retval;
}

DEFUN (program_invocation_name, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} program_invocation_name ()\n\
Return the name that was typed at the shell prompt to run Octave.\n\
\n\
If executing a script from the command line (e.g., @code{octave foo.m})\n\
or using an executable Octave script, the program name is set to the\n\
name of the script.  @xref{Executable Octave Programs}, for an example of\n\
how to create an executable Octave script.\n\
@seealso{program_name}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 0)
    retval = octave_program_invocation_name;
  else
    print_usage ();

  return retval;
}

DEFUN (program_name, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} program_name ()\n\
Return the last component of the value returned by\n\
@code{program_invocation_name}.\n\
@seealso{program_invocation_name}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 0)
    retval = octave_program_name;
  else
    print_usage ();

  return retval;
}
