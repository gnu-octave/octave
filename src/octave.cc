// octave.cc                                            -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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

#ifdef __GNUG__
#pragma implementation
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
#include <fstream.h>

#include "getopt.h"

#include "sighandlers.h"
#include "variables.h"
#include "error.h"
#include "tree-const.h"
#include "utils.h"
#include "builtins.h"
#include "input.h"
#include "pager.h"
#include "lex.h"
#include "octave.h"
#include "parse.h"
#include "unwind-prot.h"
#include "octave-hist.h"
#include "version.h"
#include "file-io.h"
#include "sysdep.h"

// Signal handler return type.
#ifndef RETSIGTYPE
#define RETSIGTYPE void
#endif
#if 0
#ifndef BADSIG
#define BADSIG (RETSIGTYPE (*)())-1
#endif
#endif

#ifdef sun
extern "C" { int on_exit (); }
#define atexit on_exit
#endif

// argv[0] for this program.
char *raw_prog_name = (char *) NULL;

// Cleaned-up name of this program, not including path information.
char *prog_name = (char *) NULL;

// Login name for user running this program.
char *user_name = (char *) NULL;

// Name of the host we are running on.
char *host_name = (char *) NULL;

// User's home directory.
char *home_directory = (char *) NULL;

// Guess what?
char *the_current_working_directory = (char *) NULL;

// Load path specified on command line.
char *load_path = (char *) NULL;

// Name of the info file specified on command line.
char *info_file = (char *) NULL;

// Name of the editor to be invoked by the edit_history command.
char *editor = (char *) NULL;

// If nonzero, don't do fancy line editing.
int no_line_editing = 0;

// Command number, counting from the beginning of this session.
int current_command_number = 1;

// Nonzero means we are exiting via the builtin exit or quit functions.
int quitting_gracefully = 0;

// Current command to execute.
tree *global_command = (tree *) NULL;

// Top level context (?)
jmp_buf toplevel;

// This is not really the right place to do this...
typedef void (*one_arg_error_handler_t) (const char*);
extern one_arg_error_handler_t set_Complex_error_handler
  (one_arg_error_handler_t f);

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
  if (hd == (char *) NULL)
    home_directory = strsave ("I have no home~!");
  else
    home_directory = strsave (hd);

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
      if (retval == 0 && global_command != NULL_TREE)
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
  if (f != (FILE *) NULL)
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
// Execute commands from the site-wide configuration file.

  char *sd = get_site_defaults ();

  parse_and_execute (sd, 0);

// Try to execute commands from $HOME/.octaverc and ./.octaverc.

  char *home_rc = (char *) NULL;
  if (home_directory != NULL)
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
}

/*
 * Usage message with extra help.
 */
static void
verbose_usage (void)
{
  cout << "\n"
       << "  Octave, version " << version_string
       << ".  Copyright (C) 1992, 1993, John W. Eaton.\n"
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
	  if (optarg != (char *) NULL)
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
	  if (optarg != (char *) NULL)
	    info_file = strsave (optarg);
	  break;
	default:
	  usage ();
	  break;
	}
    }

// Make sure we clean up when we exit.
  atexit (cleanup_tmp_files);

  initialize_history ();

  initialize_file_io ();

  initialize_symbol_tables ();

  install_builtins ();

  if (read_init_files)
    {
      saving_history = 0;
      execute_startup_files ();
      saving_history = 1;
    }

  initialize_readline ();

  initialize_pager ();

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
      if (infile == (FILE *) NULL)
	clean_up_and_exit (1);
      else
	switch_to_buffer (create_buffer (infile));
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
    echo_input = 1;

  if (! (interactive || forced_interactive))
    using_readline = 0;

  install_signal_handlers ();

  if (! inhibit_startup_message)
    {
      cout << "Octave, version " << version_string
	   << ".  Copyright (C) 1992, 1993, John W. Eaton.\n"
	   << "This is free software with ABSOLUTELY NO WARRANTY.\n"
	   << "For details, type `warranty'.\n"
	   << "\n";
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
      reset_parser ();
      retval = yyparse ();
      if (retval == 0 && global_command != NULL_TREE)
	{
	  global_command->eval (1);
	  delete global_command;
	  current_command_number++;
	}
    }
  while (retval == 0);

  clean_up_and_exit (retval);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
