// octave.cc                                            -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

// Born February 20, 1992.

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <sys/stat.h>
#include <time.h>
#include <pwd.h>
#include <setjmp.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <assert.h>
#include <iostream.h>
#include <strstream.h>
#include <fstream.h>

#include "getopt.h"

#include "lo-error.h"

#include "sighandlers.h"
#include "variables.h"
#include "error.h"
#include "tree-const.h"
#include "tree-plot.h"
#include "utils.h"
#include "input.h"
#include "pager.h"
#include "lex.h"
#include "help.h"
#include "octave.h"
#include "parse.h"
#include "unwind-prot.h"
#include "octave-hist.h"
#include "builtins.h"
#include "version.h"
#include "file-io.h"
#include "sysdep.h"
#include "defun.h"

#if !defined (HAVE_ATEXIT) && defined (HAVE_ON_EXIT)
extern "C" { int on_exit (); }
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

// Load path specified on command line.
char *load_path = 0;

// Name of the info file specified on command line.
char *info_file = 0;

// Name of the editor to be invoked by the edit_history command.
char *editor = 0;

// If nonzero, don't do fancy line editing.
int no_line_editing = 0;

// Command number, counting from the beginning of this session.
int current_command_number = 1;

// Nonzero means we are exiting via the builtin exit or quit functions.
int quitting_gracefully = 0;

// Current command to execute.
tree *global_command = 0;

// Pointer to function that is currently being evaluated.
tree_function *curr_function = 0;

// Nonzero means input is coming from startup file.
int input_from_startup_file = 0;

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
static int read_init_files = 1;

// Nonzero means we don\'t print the usual startup message.
static int inhibit_startup_message = 0;

// Usage message
static const char *usage_string = 
  "octave [-?dfhiqvx] [-p path] [--debug] [--help] [--interactive]\n\
         [--info-file file] [--norc] [--path path] [--quiet] [--version]\n\
         [--echo-commands] [file]";

// This is here so that it\'s more likely that the usage message and
// the real set of options will agree.
static const char *short_opts = "?dfhip:qvx";

// Long options.
#define INFO_FILE_OPTION 1
static struct option long_opts[] =
  {
    { "debug", 0, 0, 'd' },
    { "help", 0, 0, 'h' },
    { "interactive", 0, 0, 'i' },
    { "info-file", 1, 0, INFO_FILE_OPTION },
    { "norc", 0, 0, 'f' },
    { "path", 1, 0, 'p' },
    { "quiet", 0, 0, 'q' },
    { "version", 0, 0, 'v' },
    { "echo-commands", 0, 0, 'x' },
    { 0, 0, 0, 0 }
  };

/*
 * Initialize some global variables for later use.
 */
static void
initialize_globals (char *name)
{
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

  raw_prog_name = strsave (name);
  prog_name = strsave ("octave");

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
parse_and_execute (char *s, int print)
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

      parse_and_execute (f, print);
    }

  run_unwind_frame ("parse_and_execute_2");
}

/*
 * Initialize by reading startup files.
 */
static void
execute_startup_files (void)
{
  begin_unwind_frame ("execute_startup_files");

  unwind_protect_int (input_from_startup_file);
  input_from_startup_file = 1;

// Execute commands from the site-wide configuration file.

  char *sd = get_site_defaults ();

  parse_and_execute (sd, 0);

// Try to execute commands from $HOME/.octaverc and ./.octaverc.

  char *home_rc = 0;
  if (home_directory)
    {
      home_rc = strconcat (home_directory, "/.octaverc");
      parse_and_execute (home_rc, 0);
    }

// Names alone are not enough.

  struct stat home_rc_statbuf;
  stat (home_rc, &home_rc_statbuf);
  delete [] home_rc;

  struct stat dot_rc_statbuf;
  stat ("./.octaverc", &dot_rc_statbuf);

  if (home_rc_statbuf.st_ino != dot_rc_statbuf.st_ino)
    parse_and_execute ("./.octaverc", 0);

  run_unwind_frame ("execute_startup_files");
}

/*
 * Usage message with extra help.
 */
static void
verbose_usage (void)
{
  cout << "\n"
       << "  Octave, version " << version_string
       << ".  Copyright (C) 1992, 1993, 1994 John W. Eaton.\n"
       << "  This is free software with ABSOLUTELY NO WARRANTY.\n"
       << "\n"
       << "  usage: " << usage_string
       << "\n\n"
       << "     d : enter parser debugging mode\n"
       << "     f : don't read ~/.octaverc or .octaverc at startup\n"
       << "   h|? : print short help message and exit\n"
       << "     i : force interactive behavior\n"
       << "     q : don't print message at startup\n"
       << "     v : print version number and exit\n"
       << "     x : echo commands as they are executed\n"
       << "\n"
       << "  file : execute commands from named file\n"
       << "\n";

  exit (1);
}

/*
 * Terse usage messsage.
 */
static void
usage (void)
{
  cerr << "usage: " << usage_string << "\n";
  exit (1);
}

/*
 * Fix up things before exiting.
 */
void
clean_up_and_exit (int retval)
{
  raw_mode (0);

  clean_up_history ();

  close_plot_stream ();

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
  cout << "octave, version " << version_string << "\n";
  exit (0);
}

/*
 * You guessed it.
 */
int
main (int argc, char **argv)
{
// Allow for system dependent initialization.  See sysdep.cc for more
// details.
  sysdep_init ();

// This is not really the right place to do this...
  set_Complex_error_handler (octave_Complex_error_handler);

// Or this, probably...
  set_liboctave_error_handler (error);

// Do this first, since some command line arguments may override the
// defaults.
  initialize_globals (argv[0]);

  int optc;
  while ((optc = getopt_long (argc, argv, short_opts, long_opts, 0)) != EOF)
    {
      switch (optc)
	{
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

  initialize_history ();

  initialize_file_io ();

  initialize_symbol_tables ();

  install_builtins ();

  initialize_readline ();

  initialize_pager ();

  install_signal_handlers ();

  if (read_init_files)
    {
      saving_history = 0;
      execute_startup_files ();
      saving_history = 1;
    }

// Avoid counting commands executed from startup files.
  current_command_number = 1;

// If there is an extra argument, see if it names a file to read.

  int remaining_args = argc - optind;
  if (remaining_args > 1)
    {
      usage ();
    }
  else if (remaining_args == 1)
    {
      FILE *infile = get_input_from_file (argv[optind]);
      if (infile)
	{
	  rl_blink_matching_paren = 0;
	  switch_to_buffer (create_buffer (infile));
	}
      else
	clean_up_and_exit (1);
    }
  else
    {
      switch_to_buffer (create_buffer (get_input_from_stdin ()));

// Is input coming from a terminal?  If so, we are probably
// interactive.

      interactive = (isatty (fileno (stdin)) && isatty (fileno (stdout)));
    }

// Force input to be echoed if not really interactive, but the user
// has forced interactive behavior.

  if (!interactive && forced_interactive)
    {
      rl_blink_matching_paren = 0;
      echo_input = 1;
    }

  if (! (interactive || forced_interactive))
    using_readline = 0;

  if (! inhibit_startup_message)
    {
      cout << "Octave, version " << version_string
	   << ".  Copyright (C) 1992, 1993, 1994 John W. Eaton.\n"
	   << "This is free software with ABSOLUTELY NO WARRANTY.\n"
	   << "For details, type `warranty'.\n"
	   << endl;
    }

// Allow the user to interrupt us without exiting.

  volatile sig_handler *saved_sigint_handler = signal (SIGINT, SIG_IGN);

  if (setjmp (toplevel) != 0)
    {
      raw_mode (0);

      cout << "\n";
    }

  can_interrupt = 1;

  signal (SIGINT, saved_sigint_handler);

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

DEFALIAS (exit, quit);

DEFUN ("flops", Fflops, Sflops, 2, 1,
  "flops (): count floating point operations")
{
  int nargin = args.length ();

  if (nargin > 2)
    print_usage ("flops");

  warning ("flops is a flop, always returning zero");

  return 0.0;
}

DEFUN ("quit", Fquit, Squit, 1, 0,
  "quit (): exit Octave gracefully")
{
  Octave_object retval;
  quitting_gracefully = 1;
  clean_up_and_exit (0);
  return retval;
}

DEFUN ("warranty", Fwarranty, Swarranty, 1, 0,
  "warranty (): describe copying conditions")
{
  Octave_object retval;

  ostrstream output_buf;
  output_buf << "\n    Octave, version " << version_string
	     << ".  Copyright (C) 1992, 1993, 1994 John W. Eaton\n"
	     << "\n\
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
    Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.\n\
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

  tree_fvc *fcn = is_valid_function (args(1), "feval", 1);
  if (fcn)
    {
      int nargin = args.length () - 1;
      Octave_object tmp_args (nargin);
      for (int i = 0; i < nargin; i++)
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

  if (nargin > 1)
    retval = feval (args, nargout);
  else
    print_usage ("feval");

  return retval;
}

tree_constant
eval_string (const char *string, int print, int ans_assign,
	     int& parse_status)
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

  tree *command = global_command;

  run_unwind_frame ("eval_string");

  tree_constant retval;

  if (parse_status == 0 && command)
    {
      retval = command->eval (print);
      delete command;
    }

  return retval;
}

tree_constant
eval_string (const tree_constant& arg, int& parse_status)
{
  if (! arg.is_string_type ())
    {
      error ("eval: expecting string argument");
      return -1;
    }

  char *string = arg.string_value ();

// Yes Virginia, we always print here...

  return eval_string (string, 1, 1, parse_status);
}

DEFUN ("eval", Feval, Seval, 2, 1,
  "eval (STRING): evaluate STRING as octave code")
{
  Octave_object retval;

  int nargin = args.length ();

  if (nargin == 2)
    {
      int parse_status = 0;
      retval = eval_string (args(1), parse_status);
    }
  else
    print_usage ("eval");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
