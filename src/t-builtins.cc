// t-builtins.cc                                           -*- C++ -*-
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

/*

The function builtin_cd was adapted from a similar function from GNU
Bash, the Bourne Again SHell, copyright (C) 1987, 1989, 1991 Free
Software Foundation, Inc.

The function list_in_columns was adapted from a similar function from
GNU ls, print_many_per_line, copyright (C) 1985, 1988, 1990, 1991 Free
Software Foundation, Inc.

The function glob_pattern_p was taken from the file glob.c distributed
with GNU Bash, the Bourne Again SHell, copyright (C) 1985, 1988, 1989
Free Software Foundation, Inc.

*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <iostream.h>
#include <strstream.h>
#include <fstream.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <time.h>
#include <errno.h>
#include <signal.h>

#include "procstream.h"

#include "variables.h"
#include "symtab.h"
#include "error.h"
#include "input.h"
#include "pager.h"
#include "utils.h"
#include "sighandlers.h"
#include "builtins.h"
#include "t-builtins.h"
#include "octave.h"
#include "octave-hist.h"
#include "user-prefs.h"
#include "pr-output.h"
#include "defaults.h"
#include "tree.h"
#include "help.h"

extern "C"
{
#include "fnmatch.h"
}

// May need replacement for this on some machines.
extern "C"
{
  extern char *strerror (int);
  char *tilde_expand (char *s); /* From readline's tilde.c */
}

extern "C"
{
#include "info/info.h"
#include "info/dribble.h"
#include "info/terminal.h"

  extern int initialize_info_session ();
  extern int index_entry_exists ();
  extern int do_info_index_search ();
  extern void finish_info_session ();
  extern char *replace_in_documentation ();
}

// Is this a parametric plot?  Makes a difference for 3D plotting.
extern int parametric_plot;

// Should the graph window be cleared before plotting the next line?
extern int clear_before_plotting;

/*
 * Format a list in neat columns.  Mostly stolen from GNU ls.  This
 * should maybe be in utils.cc.
 */
static ostrstream&
list_in_columns (ostrstream& os, char **list)
{
// Compute the maximum name length.

  int max_name_length = 0;
  int total_names = 0;
  for (char **names = list; *names != (char *) NULL; names++)
    {
      total_names++;
      int name_length = strlen (*names);
      if (name_length > max_name_length)
	max_name_length = name_length;
    }

// Allow at least two spaces between names.

  max_name_length += 2;

// Calculate the maximum number of columns that will fit.

  int line_length = terminal_columns ();
  int cols = line_length / max_name_length;
  if (cols == 0)
    cols = 1;

// Calculate the number of rows that will be in each column except
// possibly  for a short column on the right.

  int rows = total_names / cols + (total_names % cols != 0);

// Recalculate columns based on rows.

  cols = total_names / rows + (total_names % rows != 0);

  names = list;
  int count;
  for (int row = 0; row < rows; row++)
    {
      count = row;
      int pos = 0;

// Print the next row.

      while (1)
	{
	  os << *(names + count);
	  int name_length = strlen (*(names + count));

	  count += rows;
	  if (count >= total_names)
	    break;

	  int spaces_to_pad = max_name_length - name_length;
	  for (int i = 0; i < spaces_to_pad; i++)
	    os << " ";
	  pos += max_name_length;
	}
      os << "\n";
    }

  return os;
}

tree_constant *
builtin_casesen (int argc, char **argv, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (argc == 1 || (argc > 1 && strcmp (argv[1], "off") == 0))
    warning ("casesen: sorry, Octave is always case sensitive");
  else if (argc > 1 && strcmp (argv[1], "on") == 0)
    ; // ok.
  else
    print_usage ("casesen");

  return retval;
}

/*
 * Change current working directory.
 */
tree_constant *
builtin_cd (int argc, char **argv, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (argc > 1)
    {
      static char *dirname = (char *) NULL;

      if (dirname)
	free (dirname);

      dirname = tilde_expand (argv[1]);

      if (dirname != (char *) NULL && !change_to_directory (dirname))
	{
	  error ("%s: %s", dirname, strerror (errno));
	  return retval;
	}
    }
  else
    {
      if (!home_directory)
	return retval;

      if (!change_to_directory (home_directory))
	{
          error ("%s: %s", home_directory, strerror (errno));
	  return retval;
	}
    }


  char *directory = get_working_directory ("cd");
  tree_constant *dir = new tree_constant (directory);
  bind_builtin_variable ("PWD", dir, 1);

  return retval;
}

#if 0
static int
in_list (char *s, char **list)
{
  while (*list != (char *) NULL)
    {
      if (strcmp (s, *list) == 0)
	return 1;
      list++;
    }

  return 0;
}
#endif

/*
 * Wipe out user-defined variables and functions given a list of
 * globbing patterns.
 */
tree_constant *
builtin_clear (int argc, char **argv, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

// Always clear the local table, but don't clear currently compiled
// functions unless we are at the top level.  (Allowing that to happen
// inside functions would result in pretty odd behavior...)

  int clear_user_functions = (curr_sym_tab == top_level_sym_tab);

  if (argc == 1)
    {
      curr_sym_tab->clear ();
      global_sym_tab->clear (clear_user_functions);
    }
  else
    {
      int lcount;
      char **lvars = curr_sym_tab->list (lcount, 0,
					 symbol_def::USER_VARIABLE,
					 SYMTAB_LOCAL_SCOPE);
      int gcount;
      char **gvars = curr_sym_tab->list (gcount, 0,
					 symbol_def::USER_VARIABLE,
					 SYMTAB_GLOBAL_SCOPE);
      int fcount;
      char **fcns = curr_sym_tab->list (fcount, 0,
					symbol_def::USER_FUNCTION,
					SYMTAB_ALL_SCOPES);

      while (--argc > 0)
	{
	  argv++;
	  if (*argv != (char *) NULL)
	    {
	      int i;
	      for (i = 0; i < lcount; i++)
		{
		  if (fnmatch (*argv, lvars[i], __FNM_FLAGS) == 0)
		    curr_sym_tab->clear (lvars[i]);
		}

	      int count;
	      for (i = 0; i < gcount; i++)
		{
		  if (fnmatch (*argv, gvars[i], __FNM_FLAGS) == 0)
		    {
		      count = curr_sym_tab->clear (gvars[i]);
		      if (count > 0)
			global_sym_tab->clear (gvars[i], clear_user_functions);
		    }
		}

	      for (i = 0; i < fcount; i++)
		{
		  if (fnmatch (*argv, fcns[i], __FNM_FLAGS) == 0)
		    {
		      count = curr_sym_tab->clear (fcns[i]);
		      if (count > 0)
			global_sym_tab->clear (fcns[i], clear_user_functions);
		    }
		}
	    }
	}

      delete [] lvars;
      delete [] gvars;
      delete [] fcns;

    }
  return retval;
}

/*
 * Associate a cryptic message with a variable name.
 */
tree_constant *
builtin_document (int argc, char **argv, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;
  if (argc == 3)
    document_symbol (argv[1], argv[2]);
  else
    print_usage ("document");
  return retval;
}

/*
 * Edit commands with your favorite editor.
 */
tree_constant *
builtin_edit_history (int argc, char **argv, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;
  do_edit_history (argc, argv);
  return retval;
}

/*
 * Set output format state.
 */
tree_constant *
builtin_format (int argc, char **argv, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;
  set_format_style (argc, argv);
  return retval;
}

static void
help_syms_list (ostrstream& output_buf, help_list *list,
		const char *desc)
{
  int count = 0;
  char **symbols = names (list, count);
  output_buf << "\n*** " << desc << ":\n\n";
  if (symbols != (char **) NULL && count > 0)
    list_in_columns (output_buf, symbols);
  delete [] symbols;
}

static void
simple_help (void)
{
  ostrstream output_buf;

  help_syms_list (output_buf, operator_help (), "operators");

  help_syms_list (output_buf, keyword_help (), "reserved words");

  help_syms_list (output_buf, builtin_text_functions_help (),
		  "text functions (these names are also reserved)");

  help_syms_list (output_buf, builtin_mapper_functions_help (),
		  "mapper functions");

  help_syms_list (output_buf, builtin_general_functions_help (),
		  "general functions");

  help_syms_list (output_buf, builtin_variables_help (),
		  "builtin variables");
      
// Also need to list variables and currently compiled functions from
// the symbol table, if there are any.

// Also need to search octave_path for script files.

  char **path = pathstring_to_vector (user_pref.loadpath);

  char **ptr = path;
  if (ptr != (char **) NULL)
    {
      while (*ptr != (char *) NULL)
	{
	  int count;
	  char **names = get_fcn_file_names (count, *ptr, 0);
	  output_buf << "\n*** function files in "
		     << make_absolute (*ptr, the_current_working_directory)
		     << ":\n\n";
	  if (names != (char **) NULL && count > 0)
	    list_in_columns (output_buf, names);
	  delete [] names;
	  ptr++;
	}
    }

  additional_help_message (output_buf);
  output_buf << ends;
  maybe_page_output (output_buf);
}

static int
try_info (const char *string, int force = 0)
{
  int status = 0;

  char *directory_name = strsave (user_pref.info_file);
  char *temp = filename_non_directory (directory_name);

  if (temp != directory_name)
    {
      *temp = 0;
      info_add_path (directory_name, INFOPATH_PREPEND);
    }

  delete [] directory_name;

  NODE *initial_node = info_get_node (user_pref.info_file, (char *)NULL);

  if (! initial_node)
    {
      warning ("can't find info file!\n");
      status = -1;
    }
  else
    {
      status = initialize_info_session (initial_node, 0);

      if (status == 0 && (force || index_entry_exists (windows, string)))
	{
	  terminal_clear_screen ();

	  terminal_prep_terminal ();

	  display_update_display (windows);

	  info_last_executed_command = (VFunction *)NULL;

	  if (! force)
	    do_info_index_search (windows, 0, string);

	  char *format = replace_in_documentation
	    ("Type \"\\[quit]\" to quit, \"\\[get-help-window]\" for help.");

	  window_message_in_echo_area (format);

	  info_read_and_dispatch ();

	  terminal_goto_xy (0, screenheight - 1);

	  terminal_clear_to_eol ();

	  terminal_unprep_terminal ();

	  status = 1;
	}

      finish_info_session (initial_node, 0);
    }

  return status;
}

/*
 * Print cryptic yet witty messages.
 */
tree_constant *
builtin_help (int argc, char **argv, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (argc == 1)
    {
      simple_help ();
    }
  else
    {
      if (argv[1] != (char *) NULL && strcmp (argv[1], "-i") == 0)
	{
	  argc--;
	  argv++;

	  if (argc == 1)
	    {
	      volatile sig_handler *old_sigint_handler;
	      old_sigint_handler = signal (SIGINT, SIG_IGN);

	      try_info ((char *) NULL, 1);

	      signal (SIGINT, old_sigint_handler);
	    }
	  else
	    {
	      while (--argc > 0)
		{
		  argv++;

		  if (*argv == (char *) NULL || **argv == '\0')
		    continue;

		  volatile sig_handler *old_sigint_handler;
		  old_sigint_handler = signal (SIGINT, SIG_IGN);

		  if (! try_info (*argv))
		    {
		      message ("help",
			       "sorry, `%s' is not indexed in the manual",
			       *argv); 
		      sleep (2);
		    }

		  signal (SIGINT, old_sigint_handler);
		}
	    }
	}
      else
	{
	  ostrstream output_buf;

	  char *fcn_file_name = (char *) NULL;
	  symbol_record *sym_rec;
	  help_list *op_help_list = operator_help ();
	  help_list *kw_help_list = keyword_help ();

	  while (--argc > 0)
	    {
	      argv++;

	      if (*argv == (char *) NULL || **argv == '\0')
		continue;

	      if (help_from_list (output_buf, op_help_list, *argv, 0))
		continue;

	      if (help_from_list (output_buf, kw_help_list, *argv, 0))
		continue;

	      sym_rec = curr_sym_tab->lookup (*argv, 0, 0);
	      if (sym_rec != (symbol_record *) NULL)
		{
		  char *h = sym_rec->help ();
		  if (h != (char *) NULL && *h != '\0')
		    {
		      output_buf << "\n*** " << *argv << ":\n\n"
				 << h << "\n";
		      continue;
		    }
		}

	      sym_rec = global_sym_tab->lookup (*argv, 0, 0);
	      if (sym_rec != (symbol_record *) NULL
		  && ! symbol_out_of_date (sym_rec))
		{
		  char *h = sym_rec->help ();
		  if (h != (char *) NULL && *h != '\0')
		    {
		      output_buf << "\n*** " << *argv << ":\n\n"
				 << h << "\n";
		      continue;
		    }
		}

// Try harder to find function files that might not be defined yet, or
// that appear to be out of date.  Don\'t execute commands from the
// file if it turns out to be a script file.

	      fcn_file_name = fcn_file_in_path (*argv);
	      if (fcn_file_name != (char *) NULL)
		{
		  sym_rec = global_sym_tab->lookup (*argv, 1, 0);
		  if (sym_rec != (symbol_record *) NULL)
		    {
		      tree_identifier tmp (sym_rec);
		      tmp.parse_fcn_file (0);
		      char *h = sym_rec->help ();
		      if (h != (char *) NULL && *h != '\0')
			{
			  output_buf << "\n*** " << *argv << ":\n\n"
				     << h << "\n"; 
			  continue;
			}
		    }
		}
	      delete [] fcn_file_name;

	      output_buf << "\nhelp: sorry, `" << *argv
			 << "' is not documented\n"; 
	    }

	  additional_help_message (output_buf);
	  output_buf << ends;
	  maybe_page_output (output_buf);
	}
    }

  return retval;
}

/*
 * Display, save, or load history.
 */
tree_constant *
builtin_history (int argc, char **argv, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  do_history (argc, argv);

  return retval;
}

/*
 * Change state flag that determines whether lines are added to plots
 * or drawn on new plots.
 */
tree_constant *
builtin_hold (int argc, char **argv, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;
  
  switch (argc)
    {
    case 1:
      clear_before_plotting = ! clear_before_plotting;
      break;
    case 2:
      if (strcasecmp (argv[1], "on") == 0)
	clear_before_plotting = 0;
      else if (strcasecmp (argv[1], "off") == 0)
	clear_before_plotting = 1;
      else
	print_usage ("hold");
      break;
    default:
      print_usage ("hold");
      break;
    }

  return retval;
}

static int
load_variable (char *nm, int force, istream& is)
{
// Is there already a symbol by this name?  If so, what is it?

  symbol_record *lsr = curr_sym_tab->lookup (nm, 0, 0);

  int is_undefined = 1;
  int is_variable = 0;
  int is_function = 0;
  int is_global = 0;

  if (lsr != (symbol_record *) NULL)
    {
      is_undefined = ! lsr->is_defined ();
      is_variable = lsr->is_variable ();
      is_function = lsr->is_function ();
      is_global = lsr->is_linked_to_global ();
    }

// Try to read data for this name.

  tree_constant tc;
  int global = tc.load (is);

  if (tc.const_type () == tree_constant_rep::unknown_constant)
    {
      error ("load: unable to load variable `%s'", nm);
      return 0;
    }

  symbol_record *sr = (symbol_record *) NULL;

  if (global)
    {
      if (is_global || is_undefined)
	{
	  if (force || is_undefined)
	    {
	      lsr = curr_sym_tab->lookup (nm, 1, 0);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: global variable name `%s' exists.", nm);
	      warning ("use `load -force' to overwrite");
	    }
	}
      else if (is_function)
	{
	  if (force)
	    {
	      lsr = curr_sym_tab->lookup (nm, 1, 0);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: `%s' is currently a function in this scope", nm);
	      warning ("`load -force' will load variable and hide function");
	    }
	}
      else if (is_variable)
	{
	  if (force)
	    {
	      lsr = curr_sym_tab->lookup (nm, 1, 0);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: local variable name `%s' exists.", nm);
	      warning ("use `load -force' to overwrite");
	    }
	}
      else
	panic_impossible ();
    }
  else
    {
      if (is_global)
	{
	  if (force || is_undefined)
	    {
	      lsr = curr_sym_tab->lookup (nm, 1, 0);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: global variable name `%s' exists.", nm);
	      warning ("use `load -force' to overwrite");
	    }
	}
      else if (is_function)
	{
	  if (force)
	    {
	      lsr = curr_sym_tab->lookup (nm, 1, 0);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: `%s' is currently a function in this scope", nm);
	      warning ("`load -force' will load variable and hide function");
	    }
	}
      else if (is_variable || is_undefined)
	{
	  if (force || is_undefined)
	    {
	      lsr = curr_sym_tab->lookup (nm, 1, 0);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: local variable name `%s' exists.", nm);
	      warning ("use `load -force' to overwrite");
	    }
	}
      else
	panic_impossible ();
    }

  if (sr != (symbol_record *) NULL)
    {
      tree_constant *tmp_tc = new tree_constant (tc);
      sr->define (tmp_tc);
      return 1;
    }
  else
    error ("load: unable to load variable `%s'", nm);

  return 0;
}

/*
 * Read variables from an input stream.
 *
 * BUGS:
 *
 *  -- This function is not terribly robust.
 */
tree_constant *
builtin_load (int argc, char **argv, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  argc--;
  argv++;

  int force = 0;
  if (argc > 0 && strcmp (*argv, "-force") == 0)
    {
      force++;
      argc--;
      argv++;
    }

  if (argc < 1)
    {
      error ("load: you must specify a single file to read");
      return retval;
    }

  static istream stream;
  static ifstream file;
  if (strcmp (*argv, "-") == 0)
    {
      stream = cin;
    }
  else
    {
      char *fname = tilde_expand (*argv);
      file.open (fname);
      if (! file)
	{
	  error ("load: couldn't open input file `%s'", *argv);
	  return retval;
	}
      stream = file;
    }

  int count = 0;
  char *nm = (char *) NULL;
  for (;;)
    {
// Read name for this entry or break on EOF.
      delete [] nm;
      nm = extract_keyword (stream, "name");
      if (nm == (char *) NULL)
	{
	  if (count == 0)
	    {
	      error ("load: no name keywords found in file `%s'", *argv);
	      error ("Are you sure this is an octave data file?");
	    }
	  break;
	}
      else
	count++;

      if (*nm == '\0')
	continue;

      if (! valid_identifier (nm))
	{
	  warning ("load: skipping bogus identifier `%s'");
	  continue;
	}

      load_variable (nm, force, stream);

      if (error_state)
	{
	  error ("reading file %s", *argv);
	  break;
	}
    }

  if (file);
    file.close ();

  return retval;
}

/*
 * Get a directory listing.
 */
tree_constant *
builtin_ls (int argc, char **argv, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  ostrstream ls_buf;

  ls_buf << "ls -C ";
  for (int i = 1; i < argc; i++)
    ls_buf << tilde_expand (argv[i]) << " ";

  ls_buf << ends;

  char *ls_command = ls_buf.str ();

  iprocstream cmd (ls_command);

  char ch;
  ostrstream output_buf;
  while (cmd.get (ch))
    output_buf.put (ch);

  output_buf << ends;

  maybe_page_output (output_buf);
  
  delete [] ls_command;

  return retval;
}

/*
 * Run previous commands from the history list.
 */
tree_constant *
builtin_run_history (int argc, char **argv, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;
  do_run_history (argc, argv);
  return retval;
}

/*
 * Return nonzero if PATTERN has any special globbing chars in it.
 */
static int
glob_pattern_p (char *pattern)
{
  char *p = pattern;
  char c;
  int open = 0;

  while ((c = *p++) != '\0')
    {
      switch (c)
	{
	case '?':
	case '*':
	  return 1;

	case '[':	// Only accept an open brace if there is a close
	  open++;	// brace to match it.  Bracket expressions must be
	  continue;	// complete, according to Posix.2

	case ']':
	  if (open)
	    return 1;
	  continue;
	  
	case '\\':
	  if (*p++ == '\0')
	    return 0;

	default:
	  continue;
	}
    }

  return 0;
}

/*
 * Write variables to an output stream.
 */
tree_constant *
builtin_save (int argc, char **argv, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  if (argc < 2)
    {
      print_usage ("save");
      return retval;
    }

  argc--;
  argv++;

  static ostream stream;
  static ofstream file;
  if (strcmp (*argv, "-") == 0)
    {
// XXX FIXME XXX -- should things intended for the screen end up in a 
// tree_constant (string)?
      stream = cout;
    }
  else if (argc == 1 && glob_pattern_p (*argv)) // Guard against things
    {						// like `save a*',
      print_usage ("save");			// which are probably
      return retval;				// mistakes...
    }
  else
    {
      char *fname = tilde_expand (*argv);
      file.open (fname);
      if (! file)
	{
	  error ("save: couldn't open output file `%s'", *argv);
	  return retval;
	}
      stream = file;

    }

  int prec = user_pref.save_precision;

  if (argc == 1)
    {
      int count;
      char **vars = curr_sym_tab->list (count, 0,
					symbol_def::USER_VARIABLE,
					SYMTAB_ALL_SCOPES);

      for (int i = 0; i < count; i++)
	curr_sym_tab->save (stream, vars[i],
			    is_globally_visible (vars[i]), prec);

      delete [] vars;
    }
  else
    {
      while (--argc > 0)
	{
	  argv++;

	  int count;
	  char **lvars = curr_sym_tab->list (count, 0,
					     symbol_def::USER_VARIABLE);

	  int saved_or_error = 0;
	  int i;
	  for (i = 0; i < count; i++)
	    {
	      if (fnmatch (*argv, lvars[i], __FNM_FLAGS) == 0
		  && curr_sym_tab->save (stream, lvars[i],
					 is_globally_visible (lvars[i]),
					 prec) != 0)
		saved_or_error++;
	    }

	  char **bvars = global_sym_tab->list (count, 0,
					       symbol_def::BUILTIN_VARIABLE);

	  for (i = 0; i < count; i++)
	    {
	      if (fnmatch (*argv, bvars[i], __FNM_FLAGS) == 0
		  && global_sym_tab->save (stream, bvars[i], 0, prec) != 0)
		saved_or_error++;
	    }

	  delete [] lvars;
	  delete [] bvars;

	  if (! saved_or_error)
	    warning ("save: no such variable `%s'", *argv);
	}
    }

  if (file);
    file.close ();

  return retval;
}

/*
 * Set plotting options.
 */
tree_constant *
builtin_set (int argc, char **argv, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  ostrstream plot_buf;

  if (argc > 1)
    {
      if (almost_match ("parametric", argv[1], 3))
	parametric_plot = 1;
      else if (almost_match ("noparametric", argv[1], 5))
	parametric_plot = 0;
    }

  for (int i = 0; i < argc; i++)
    plot_buf << argv[i] << " ";

  plot_buf << "\n" << ends;

  char *plot_command = plot_buf.str ();
  send_to_plot_stream (plot_command);

  delete [] plot_command;

  return retval;
}

/*
 * Set plotting options.
 */
tree_constant *
builtin_show (int argc, char **argv, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  ostrstream plot_buf;

  for (int i = 0; i < argc; i++)
    plot_buf << argv[i] << " ";

  plot_buf << "\n" << ends;

  char *plot_command = plot_buf.str ();
  send_to_plot_stream (plot_command);

  delete [] plot_command;

  return retval;
}

/*
 * List variable names.
 */
static void
print_symbol_info_line (ostrstream& output_buf, const symbol_record_info& s)
{
  output_buf << (s.is_read_only () ? " -" : " w");
  output_buf << (s.is_eternal () ? "- " : "d ");
#if 0
  output_buf << (s.hides_fcn () ? "f" : (s.hides_builtin () ? "F" : "-"));
#endif
  output_buf.form ("  %-16s", s.type_as_string ());
  if (s.is_function ())
    output_buf << "      -      -";
  else
    {
      output_buf.form ("%7d", s.rows ());
      output_buf.form ("%7d", s.columns ());
    }
  output_buf << "  " << s.name () << "\n";
}

static void
print_long_listing (ostrstream& output_buf, symbol_record_info *s)
{
  if (s == (symbol_record_info *) NULL)
    return;

  symbol_record_info *ptr = s;
  while (ptr->is_defined ())
    {
      print_symbol_info_line (output_buf, *ptr);
      ptr++;
    }
}

static int
maybe_list (const char *header, ostrstream& output_buf,
	    int show_verbose, symbol_table *sym_tab, unsigned type,
	    unsigned scope)
{
  int count;
  int status = 0;
  if (show_verbose)
    {
      symbol_record_info *symbols;
      symbols = sym_tab->long_list (count, 1, type, scope);
      if (symbols != (symbol_record_info *) NULL && count > 0)
	{
	  output_buf << "\n" << header << "\n\n"
		     << "prot  type               rows   cols  name\n"
		     << "====  ====               ====   ====  ====\n";

	  print_long_listing (output_buf, symbols);
	  status = 1;
	}
      delete [] symbols;
    }
  else
    {
      char **symbols = sym_tab->list (count, 1, type, scope);
      if (symbols != (char **) NULL && count > 0)
	{
	  output_buf << "\n" << header << "\n\n";
	  list_in_columns (output_buf, symbols);
	  status = 1;
	}
      delete [] symbols;
    }
  return status;
}

tree_constant *
builtin_who (int argc, char **argv, int nargout)
{
  tree_constant *retval = NULL_TREE_CONST;

  int show_builtins = 0;
  int show_functions = (curr_sym_tab == top_level_sym_tab);
  int show_variables = 1;
  int show_verbose = 0;

  if (argc > 1)
    {
      show_functions = 0;
      show_variables = 0;
    }

  for (int i = 1; i < argc; i++)
    {
      argv++;
      if (strcmp (*argv, "-all") == 0 || strcmp (*argv, "-a") == 0)
	{
	  show_builtins++;
	  show_functions++;
	  show_variables++;	  
	}
      else if (strcmp (*argv, "-builtins") == 0
	       || strcmp (*argv, "-b") == 0)
	show_builtins++;
      else if (strcmp (*argv, "-functions") == 0
	       || strcmp (*argv, "-f") == 0)
	show_functions++;
      else if (strcmp (*argv, "-long") == 0 
	       || strcmp (*argv, "-l") == 0)
	  show_verbose++;
      else if (strcmp (*argv, "-variables") == 0
	       || strcmp (*argv, "-v") == 0)
	show_variables++;
      else
	warning ("who: unrecognized option `%s'", *argv);
    }

// If the user specified -l and nothing else, show variables.  If
// evaluating this at the top level, also show functions.

  if (show_verbose && ! (show_builtins || show_functions || show_variables))
    {
      show_functions = (curr_sym_tab == top_level_sym_tab);
      show_variables = 1;
    }

  ostrstream output_buf;
  int pad_after = 0;

  if (show_builtins)
    {
      pad_after += maybe_list ("*** built-in variables:",
			       output_buf, show_verbose, global_sym_tab,
			       symbol_def::BUILTIN_VARIABLE,
			       SYMTAB_ALL_SCOPES);

      pad_after += maybe_list ("*** built-in functions:",
			       output_buf, show_verbose, global_sym_tab,
			       symbol_def::BUILTIN_FUNCTION,
			       SYMTAB_ALL_SCOPES);
    }

  if (show_functions)
    {
      pad_after += maybe_list ("*** currently compiled functions:",
			       output_buf, show_verbose, global_sym_tab,
			       symbol_def::USER_FUNCTION,
			       SYMTAB_ALL_SCOPES);
    }

  if (show_variables)
    {
      pad_after += maybe_list ("*** local user variables:",
			       output_buf, show_verbose, curr_sym_tab,
			       symbol_def::USER_VARIABLE,
			       SYMTAB_LOCAL_SCOPE); 

      pad_after += maybe_list ("*** globally visible user variables:",
			       output_buf, show_verbose, curr_sym_tab,
			       symbol_def::USER_VARIABLE,
			       SYMTAB_GLOBAL_SCOPE);
    }

  if (pad_after)
    output_buf << "\n";

  output_buf << ends;
  maybe_page_output (output_buf);

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
