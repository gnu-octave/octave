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
#include "help.h"
#include "input.h"
#include "lex.h"
#include "oct-str.h"
#include "octave-hist.h"
#include "octave.h"
#include "pager.h"
#include "parse.h"
#include "pathsearch.h"
#include "procstream.h"
#include "sighandlers.h"
#include "statdefs.h"
#include "sysdep.h"
#include "tree-const.h"
#include "tree-misc.h"
#include "tree-plot.h"
#include "unwind-prot.h"
#include "user-prefs.h"
#include "utils.h"
#include "variables.h"
#include "version.h"

#if !defined (HAVE_ATEXIT) && defined (HAVE_ON_EXIT)
extern "C" int on_exit ();
#define atexit on_exit
#endif

// argv[0] for this program.
char *raw_prog_name = 0;

// Cleaned-up name of this program, not including path information.
char *prog_name = 0;

// Login name for user running this program.
char *user_name = 0;

// Name of the host we are running on.
char *host_name = 0;

// User's home directory.
char *home_directory = 0;

// Guess what?
char *the_current_working_directory = 0;

// Load path specified on command line.  (--path path; -p path)
char *load_path = 0;

// Name of the info file specified on command line.
// (--info-file file; -i file)
char *info_file = 0;

// Name of the editor to be invoked by the edit_history command.
char *editor = 0;

// If nonzero, don't do fancy line editing.
int no_line_editing = 0;

// If nonzero, print verbose info in some cases.
// (--verbose; -V)
int verbose_flag = 0;

// Command number, counting from the beginning of this session.
int current_command_number = 1;

// Nonzero means we are exiting via the builtin exit or quit functions.
int quitting_gracefully = 0;

// Current command to execute.
tree_statement_list *global_command = 0;

// Pointer to function that is currently being evaluated.
tree_function *curr_function = 0;

// Nonzero means input is coming from startup file.
int input_from_startup_file = 0;

// The command-line options.
Octave_str_obj octave_argv;

// Top level context (?)
jmp_buf toplevel;

// This is not really the right place to do this...
typedef void (*one_arg_error_handler_t) (const char*);
extern one_arg_error_handler_t set_Complex_error_handler
  (one_arg_error_handler_t f);

// This is from readline's paren.c:
extern int rl_blink_matching_paren;

static void
octave_Complex_error_handler (const char* msg)
{
  warning (msg);
}

// Nonzero means we read ~/.octaverc and ./.octaverc.
// (--norc; --ignore-init-file; -f)
static int read_init_files = 1;

// Nonzero means we printed messages about reading startup files.
static int reading_startup_message_printed = 0;

// Nonzero means we don't print the usual startup message.
// (--quiet; --silent; -q)
static int inhibit_startup_message = 0;

// Nonzero means we turn on compatibility options.
// (--traditional)
static int traditional = 0;

// Usage message
static const char *usage_string = 
  "octave [-?Vdfhiqvx] [-p path] [--debug] [--help] [--ignore-init-file]\n\
       [--info-file file] [--interactive] [--path path] [--silent]\n\
       [--traditional] [--verbose] [--version] [--echo-commands] [file]";

// This is here so that it's more likely that the usage message and
// the real set of options will agree.  Note: the `+' must come first
// to prevent getopt from permuting arguments!
static const char *short_opts = "+?Vdfhip:qvx";

// Long options.  See the comments in getopt.h for the meanings of the
// fields in this structure.
#define INFO_FILE_OPTION 1
#define TRADITIONAL_OPTION 2
static struct option long_opts[] =
  {
    { "debug",            no_argument,       0, 'd' },
    { "help",             no_argument,       0, 'h' },
    { "interactive",      no_argument,       0, 'i' },
    { "info-file",        required_argument, 0, INFO_FILE_OPTION },
    { "norc",             no_argument,       0, 'f' },
    { "ignore-init-file", no_argument,       0, 'f' },
    { "path",             required_argument, 0, 'p' },
    { "quiet",            no_argument,       0, 'q' },
    { "silent",           no_argument,       0, 'q' },
    { "traditional",      no_argument,       0, TRADITIONAL_OPTION },
    { "verbose",          no_argument,       0, 'V' },
    { "version",          no_argument,       0, 'v' },
    { "echo-commands",    no_argument,       0, 'x' },
    { 0,                  0,                 0, 0 }
  };

// Store the command-line options for later use.

static void
intern_argv (int argc, char **argv)
{
  if (argc > 1)
    {
      octave_argv.resize (argc-1);
      for (int i = 1; i < argc; i++)
	octave_argv.elem (i-1) = argv[i];

      bind_builtin_variable ("argv", octave_argv, 1, 1, 0);
    }
}

// Initialize some global variables for later use.

static void
initialize_globals (char *name)
{
  raw_prog_name = strsave (name);
  char *tmp = strrchr (raw_prog_name, '/');
  prog_name = tmp ? strsave (tmp+1) : strsave (raw_prog_name);

  kpse_set_progname (name);

  struct passwd *entry = getpwuid (getuid ());
  if (entry)
    user_name = strsave (entry->pw_name);
  else
    user_name = strsave ("I have no name!");
  endpwent ();

  char hostname[256];
  if (gethostname (hostname, 255) < 0)
    host_name = strsave ("I have no host!");
  else
    host_name = strsave (hostname);

  char *hd = getenv ("HOME");
  if (hd)
    home_directory = strsave (hd);
  else
    home_directory = strsave ("I have no home!");

  char *shell_path = getenv ("PATH");
  char *arch_dir = octave_arch_lib_dir ();
  char *bin_dir = octave_bin_dir ();

  int len = strlen (arch_dir) + strlen (bin_dir) + 7;

  char *putenv_cmd = 0;

  if (shell_path)
    {
      len += strlen (shell_path) + 1;
      putenv_cmd = new char [len];
      sprintf (putenv_cmd,
	       "PATH=%s" SEPCHAR_STR "%s" SEPCHAR_STR "%s",
	       shell_path, arch_dir, bin_dir);
    }
  else
    {
      putenv_cmd = new char [len];
      sprintf (putenv_cmd, "PATH=%s" SEPCHAR_STR "%s", arch_dir, bin_dir);
    }

  putenv (putenv_cmd);

  // This may seem odd, but doing it this way means that we don't have
  // to modify the kpathsea library...

  char *odb = getenv ("OCTAVE_DB_DIR");

  if (odb)
    putenv (strconcat ("TEXMF=", odb));
  else
    {
      char *oh = getenv ("OCTAVE_HOME");

      if (oh)
	{
	  len = strlen (oh) + 18;
	  putenv_cmd = new char [len];
	  sprintf (putenv_cmd, "TEXMF=%s/lib/octave", oh);
	  putenv (putenv_cmd);
	}
      else  
	putenv (strsave ("TEXMF=" OCTAVE_DATADIR "/octave"));
    }

  load_path = default_path ();

  info_file = default_info_file ();

  editor = default_editor ();
}

void
parse_and_execute (FILE *f, int print)
{
  begin_unwind_frame ("parse_and_execute");
  
  YY_BUFFER_STATE old_buf = current_buffer ();
  YY_BUFFER_STATE new_buf = create_buffer (f);

  add_unwind_protect (restore_input_buffer, (void *) old_buf);
  add_unwind_protect (delete_input_buffer, (void *) new_buf);

  switch_to_buffer (new_buf);

  unwind_protect_int (echo_input);
  unwind_protect_int (using_readline);
  unwind_protect_int (saving_history);

  echo_input = 0;
  using_readline = 0;
  saving_history = 0;

  unwind_protect_ptr (curr_sym_tab);

  int retval;
  do
    {
      reset_parser ();

      retval = yyparse ();

      if (retval == 0 && global_command)
	{
	  global_command->eval (print);
	  delete global_command;
	}
    }
  while (retval == 0);

  run_unwind_frame ("parse_and_execute");
}

void
parse_and_execute (char *s, int print, int verbose)
{
  begin_unwind_frame ("parse_and_execute_2");

  unwind_protect_int (reading_script_file);

  reading_script_file = 1;

  FILE *f = get_input_from_file (s, 0);
  if (f)
    {
      unwind_protect_int (input_line_number);
      unwind_protect_int (current_input_column);
      unwind_protect_int (echo_input);

      input_line_number = 0;
      current_input_column = 1;
      echo_input = 0;

      if (verbose)
	{
	  cout << "reading commands from " << s << " ... ";
	  reading_startup_message_printed = 1;
	  cout.flush ();
	}

      parse_and_execute (f, print);

      fclose (f);

      if (verbose)
	cout << "done." << endl;
    }

  run_unwind_frame ("parse_and_execute_2");
}

// Initialize by reading startup files.

static void
execute_startup_files (void)
{
  begin_unwind_frame ("execute_startup_files");

  unwind_protect_int (input_from_startup_file);
  input_from_startup_file = 1;

  int verbose = (verbose_flag && ! inhibit_startup_message);

  // Execute commands from the site-wide configuration file.

  char *sd = get_site_defaults ();

  parse_and_execute (sd, 0, verbose);

  // Try to execute commands from $HOME/.octaverc and ./.octaverc.

  char *home_rc = 0;
  if (home_directory)
    {
      home_rc = strconcat (home_directory, "/.octaverc");
      parse_and_execute (home_rc, 0, verbose);
    }

  // Names alone are not enough.

  struct stat home_rc_statbuf;
  stat (home_rc, &home_rc_statbuf);
  delete [] home_rc;

  struct stat dot_rc_statbuf;
  stat ("./.octaverc", &dot_rc_statbuf);

  if (home_rc_statbuf.st_ino != dot_rc_statbuf.st_ino)
    parse_and_execute ("./.octaverc", 0, verbose);

  run_unwind_frame ("execute_startup_files");
}

// Usage message with extra help.

static void
verbose_usage (void)
{
  cout << "\n" OCTAVE_NAME_VERSION_AND_COPYRIGHT "\n\n\
Usage: " << usage_string << "\n\
\n\
  -d, --debug             enter parser debugging mode\n\
  -f, --ignore-init-file  don't read any initialization files\n\
  -h, -?, --help          print short help message and exit\n\
  -i, --interactive       force interactive behavior\n\
  --info-file FILE        use top-level info file FILE\n\
  -p PATH, --path PATH    set initial LOADPATH to PATH\n\
  -q, --silent            don't print message at startup\n\
  --traditional           set compatibility variables\n\
  -V, --verbose           enable verbose output in some cases\n\
  -v, --version           print version number and exit\n\
  -x, --echo-commands     echo commands as they are executed\n\
\n\
  FILE                    execute commands from FILE\n\
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

// Fix up things before exiting.

void
clean_up_and_exit (int retval)
{
  raw_mode (0);

  clean_up_history ();

  close_plot_stream ();

  close_diary_file ();

  close_files ();

  cleanup_tmp_files ();

  if (!quitting_gracefully && (interactive || forced_interactive))
    cout << "\n";

  if (retval == EOF)
    retval = 0;

  exit (retval);

  // This is bogus but should prevent g++ from giving a warning saying
  // that this volatile function does return.

  panic_impossible ();
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
  set_Complex_error_handler (octave_Complex_error_handler);

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
  // The order of these calls is important, and initialize_globals
  // must come before the options are processed because some command
  // line options override defaults.

  init_user_prefs ();

  initialize_pager ();

  sysdep_init ();

  initialize_error_handlers ();

  initialize_globals (argv[0]);

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
	    load_path = strsave (optarg);
	  break;

	case 'q':
	  inhibit_startup_message = 1;
	  break;

	case 'x':
	  echo_input = 1;
	  break;

	case 'v':
	  print_version_and_exit ();
	  break;

	case INFO_FILE_OPTION:
	  if (optarg)
	    info_file = strsave (optarg);
	  break;

	case TRADITIONAL_OPTION:
	  traditional = 1;
	  break;

	default:
	  usage ();
	  break;
	}
    }

#if defined (HAVE_ATEXIT) || (HAVE_ON_EXIT)
  // Make sure we clean up when we exit.  If we don't have atexit or
  // on_exit, we're going to leave some junk files around if we exit
  // abnormally.
  atexit (cleanup_tmp_files);
#endif

  // These can come after command line args since none of them set any
  // defaults that might be changed by command line options.

  install_signal_handlers ();

  initialize_history ();

  initialize_file_io ();

  initialize_symbol_tables ();  

  install_builtins ();

  initialize_readline ();

  init_dynamic_linker ();

  if (! inhibit_startup_message)
    cout << OCTAVE_STARTUP_MESSAGE "\n" << endl;

  if (traditional)
    maximum_braindamage ();

  if (read_init_files)
    {
      saving_history = 0;
      execute_startup_files ();
      saving_history = 1;
    }

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

      FILE *infile = get_input_from_file (curr_fcn_file_name);

      if (infile)
	{
	  bind_builtin_variable ("program_invocation_name",
				 curr_fcn_file_name);

	  char *tmp = strrchr (curr_fcn_file_name, '/');
	  tmp = tmp ? tmp+1 : curr_fcn_file_name;

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
      echo_input = 1;
    }

  if (! interactive)
    using_readline = 0;

  // Allow the user to interrupt us without exiting.

  volatile sig_handler *saved_sigint_handler
    = octave_set_signal_handler (SIGINT, SIG_IGN);

  if (setjmp (toplevel) != 0)
    {
      raw_mode (0);

      cout << "\n";
    }

  can_interrupt = 1;

  octave_set_signal_handler (SIGINT, saved_sigint_handler);

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

DEFUN_TEXT ("casesen", Fcasesen, Scasesen, 2, 1,
  "casesen [on|off]")
{
  Octave_object retval;

  DEFINE_ARGV("casesen");

  if (argc == 1 || (argc > 1 && strcmp (argv[1], "off") == 0))
    warning ("casesen: sorry, Octave is always case sensitive");
  else if (argc > 1 && strcmp (argv[1], "on") == 0)
    ; // ok.
  else
    print_usage ("casesen");

  DELETE_ARGV;

  return retval;
}

DEFUN ("computer", Fcomputer, Scomputer, 1, 0,
  "computer ():\n\
\n\
Have Octave ask the system, \"What kind of computer are you?\"")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin != 0)
    warning ("computer: ignoring extra arguments");

  ostrstream output_buf;

  if (strcmp (TARGET_HOST_TYPE, "unknown") == 0)
    output_buf << "Hi Dave, I'm a HAL-9000";
  else
    output_buf << TARGET_HOST_TYPE;

  if (nargout == 0)
    {
      output_buf << "\n" << ends;
      maybe_page_output (output_buf);
    }
  else
    {
      output_buf << ends;
      char *msg = output_buf.str ();
      retval = msg;
      delete [] msg;
    }

  return retval;
}

DEFUN ("flops", Fflops, Sflops, 0, 1,
  "flops (): count floating point operations")
{
  int nargin = args.length ();

  if (nargin > 0)
    print_usage ("flops");

  warning ("flops is a flop, always returning zero");

  return 0.0;
}

DEFUN ("quit", Fquit, Squit, 0, 0,
  "quit (): exit Octave gracefully")
{
  Octave_object retval;
  quitting_gracefully = 1;
  clean_up_and_exit (0);
  return retval;
}

DEFALIAS (exit, quit);

DEFUN ("warranty", Fwarranty, Swarranty, 0, 0,
  "warranty (): describe copying conditions")
{
  Octave_object retval;

  ostrstream output_buf;
  output_buf << "\n" OCTAVE_NAME_VERSION_AND_COPYRIGHT "\n\n\
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

  output_buf << ends;
  maybe_page_output (output_buf);

  return retval;
}

// XXX FIXME XXX -- this may not be the best place for these...

Octave_object
feval (const Octave_object& args, int nargout)
{
  Octave_object retval;

  tree_fvc *fcn = is_valid_function (args(0), "feval", 1);
  if (fcn)
    {
      int tmp_nargin = args.length () - 1;
      Octave_object tmp_args;
      tmp_args.resize (tmp_nargin);
      for (int i = 0; i < tmp_nargin; i++)
	tmp_args(i) = args(i+1);
      retval = fcn->eval (0, nargout, tmp_args);
    }

  return retval;
}

DEFUN ("feval", Ffeval, Sfeval, -1, 1,
  "feval (NAME, ARGS, ...)\n\
\n\
evaluate NAME as a function, passing ARGS as its arguments")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin > 0)
    retval = feval (args, nargout);
  else
    print_usage ("feval");

  return retval;
}

static Octave_object
eval_string (const char *string, int print, int ans_assign,
	     int& parse_status, int nargout)
{
  begin_unwind_frame ("eval_string");

  unwind_protect_int (get_input_from_eval_string);
  unwind_protect_ptr (global_command);
  unwind_protect_ptr (current_eval_string);

  get_input_from_eval_string = 1;
  current_eval_string = string;

  YY_BUFFER_STATE old_buf = current_buffer ();
  YY_BUFFER_STATE new_buf = create_buffer (0);

  add_unwind_protect (restore_input_buffer, (void *) old_buf);
  add_unwind_protect (delete_input_buffer, (void *) new_buf);

  switch_to_buffer (new_buf);

  unwind_protect_ptr (curr_sym_tab);

  reset_parser ();

  parse_status = yyparse ();

  // Important to reset the idea of where input is coming from before
  // trying to eval the command we just parsed -- it might contain the
  // name of an function file that still needs to be parsed!

  tree_statement_list *command = global_command;

  run_unwind_frame ("eval_string");

  Octave_object retval;

  if (parse_status == 0 && command)
    {
      retval = command->eval (print, nargout);
      delete command;
    }

  return retval;
}

tree_constant
eval_string (const char *string, int print, int ans_assign,
	     int& parse_status)
{
  tree_constant retval;

  Octave_object tmp = eval_string (string, print, ans_assign,
				   parse_status, 1);

  retval = tmp(0);

  return retval;
}

static Octave_object
eval_string (const tree_constant& arg, int& parse_status, int nargout)
{
  const char *string = arg.string_value ();

  if (error_state)
    {
      error ("eval: expecting string argument");
      return -1.0;
    }

  // Yes Virginia, we always print here...

  return eval_string (string, 1, 1, parse_status, nargout);
}

DEFUN ("eval", Feval, Seval, 2, 1,
  "eval (TRY, CATCH)\n\
\n\
Evaluate the string TRY as octave code.  If that fails, evaluate the\n\
string CATCH.")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin > 0)
    {
      begin_unwind_frame ("Feval");

      if (nargin > 1)
	{
	  unwind_protect_int (suppress_octave_error_messages);
	  suppress_octave_error_messages = 1;
	}

      int parse_status = 0;

      retval = eval_string (args(0), parse_status, nargout);

      if (nargin > 1 && (parse_status != 0 || error_state))
	{
	  error_state = 0;
	  eval_string (args(1), parse_status, nargout);
	  retval = Octave_object ();
	}

      run_unwind_frame ("Feval");
    }
  else
    print_usage ("eval");

  return retval;
}

// Execute a shell command.

DEFUN ("system", Fsystem, Ssystem, 2, 1,
  "system (string [, return_output]): execute shell commands")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    {
      print_usage ("system");
      return retval;
    }

  tree_constant tc_command = args(0);

  const char *tmp_str = tc_command.string_value ();

  if (error_state)
    {
      error ("system: expecting string as first argument");
    }
  else
    {
      iprocstream *cmd = new iprocstream (tmp_str);

      add_unwind_protect (cleanup_iprocstream, cmd);

      int status = 127;

      if (cmd && *cmd)
	{
	  ostrstream output_buf;

	  char ch;
	  while (cmd->get (ch))
	    output_buf.put (ch);

	  output_buf << ends;

	  status = cmd->close ();

	  // The value in status is as returned by waitpid.  If the
	  // process exited normally, extract the actual exit status of
	  // the command.  Otherwise, return 127 as a failure code.

	  if ((status & 0xff) == 0)
	    status = (status & 0xff00) >> 8;

	  if (nargout > 0 || nargin > 1)
	    {
	      char *msg = output_buf.str ();

	      retval(1) = (double) status;
	      retval(0) = msg;

	      delete [] msg;
	    }
	  else
	    maybe_page_output (output_buf);
	}
      else
	error ("unable to start subprocess for `%s'", tmp_str);

      run_unwind_protect ();
    }

  return retval;
}

DEFALIAS (shell_cmd, system);

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
  p = (void *) malloc (sz);
  while (p == 0)
    {
      (*__new_handler) ();
      p = (void *) malloc (sz);
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

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
