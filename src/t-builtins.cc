// t-builtins.cc                                           -*- C++ -*-
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

/*

The function builtin_cd was adapted from a similar function from GNU
Bash, the Bourne Again SHell, copyright (C) 1987, 1989, 1991 Free
Software Foundation, Inc.

The function list_in_columns was adapted from a similar function from
GNU ls, print_many_per_line, copyright (C) 1985, 1988, 1990, 1991 Free
Software Foundation, Inc.

*/

#ifdef __GNUG__
#pragma implementation
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
#include <String.h>

#include "procstream.h"

#include "variables.h"
#include "symtab.h"
#include "error.h"
#include "input.h"
#include "pager.h"
#include "utils.h"
#include "builtins.h"
#include "t-builtins.h"
#include "octave.h"
#include "octave-hist.h"
#include "user-prefs.h"
#include "pr-output.h"
#include "tree.h"
#include "help.h"

// May need replacement for this on some machines.
extern "C"
{
  extern char *strerror (int);
  char *tilde_expand (char *s); /* From readline's tilde.c */
}

extern int symbol_out_of_date (symbol_record *s);

// Is this a parametric plot?  Makes a difference for 3D plotting.
extern int parametric_plot;

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

tree_constant
builtin_casesen (int argc, char **argv)
{
  tree_constant retval;

  if (argc == 1 || (argc > 1 && strcmp (argv[1], "off") == 0))
    message ("casesen", "sorry, octave is always case sensitive");
  else if (argc > 1 && strcmp (argv[1], "on") == 0)
    ; // ok.
  else
    usage ("casesen [on|off]");

  return retval;
}

/*
 * Change current working directory.
 */
tree_constant
builtin_cd (int argc, char **argv)
{
  tree_constant retval;

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
  bind_protected_variable ("PWD", dir);

  return retval;
}

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

/*
 * Wipe out user-defined variables and functions given a list of
 * regular expressions. 
 */
tree_constant
builtin_clear (int argc, char **argv)
{
  tree_constant retval;
  if (argc == 1)
    {
      curr_sym_tab->clear ();
      global_sym_tab->clear ();
    }
  else
    {
      int count;
      char **names = curr_sym_tab->list (count);

      int g_count;
      char **g_names = global_sym_tab->list (g_count);

      int num_cleared = 0;
      char **locals_cleared = new char * [count+1];
      locals_cleared[num_cleared] = (char *) NULL;

      while (--argc > 0)
	{
	  argv++;
	  if (*argv != (char *) NULL)
	    {
	      Regex rx (*argv);

	      int i;
	      for (i = 0; i < count; i++)
		{
		  String nm (names[i]);
		  if (nm.matches (rx) && curr_sym_tab->clear (names[i]))
		    {
		      locals_cleared[num_cleared++] = strsave (names[i]);
		      locals_cleared[num_cleared] = (char *) NULL;
		    }
		}

	      for (i = 0; i < g_count; i++)
		{
		  String nm (g_names[i]);
		  if (nm.matches (rx)
		      && ! in_list (g_names[i], locals_cleared))
		    {
		      global_sym_tab->clear (g_names[i]);
		    }
		}
	    }
	}

      int i = 0;
      while (locals_cleared[i] != (char *) NULL)
	delete [] locals_cleared[i++];
      delete [] locals_cleared;

      delete [] names;
      delete [] g_names;
      delete [] g_names;

    }
  return retval;
}

/*
 * Associate a cryptic message with a variable name.
 */
tree_constant
builtin_document (int argc, char **argv)
{
  tree_constant retval;
  if (argc == 3)
    {
      symbol_record *sym_rec = curr_sym_tab->lookup (argv[1], 0);
      if (sym_rec == (symbol_record *) NULL)
	{
	  sym_rec = global_sym_tab->lookup (argv[1], 0);
	  if (sym_rec == (symbol_record *) NULL)
	    {
	      error ("document: no such symbol `%s'", argv[1]);
	      return retval;
	    }
	}
      sym_rec->document (argv[2]);
    }
  else
    usage ("document symbol string ...");

  return retval;
}

/*
 * Edit commands with your favorite editor.
 */
tree_constant
builtin_edit_history (int argc, char **argv)
{
  tree_constant retval;
  do_edit_history (argc, argv);
  return retval;
}

/*
 * Set output format state.
 */
tree_constant
builtin_format (int argc, char **argv)
{
  tree_constant retval;
  set_format_style (argc, argv);
  return retval;
}

/*
 * Print cryptic yet witty messages.
 */
tree_constant
builtin_help (int argc, char **argv)
{
  tree_constant retval;

  ostrstream output_buf;
  if (argc == 1)
    {
      char **symbols;
      int count = 0;

      symbols = names (operator_help (), count);
      output_buf << "\n*** operators:\n\n";
      if (symbols != (char **) NULL && count > 0)
	list_in_columns (output_buf, symbols);
      delete [] symbols;

      symbols = names (keyword_help (), count);
      output_buf << "\n*** reserved words:\n\n";
      if (symbols != (char **) NULL && count > 0)
	list_in_columns (output_buf, symbols);
      delete [] symbols;

      symbols = names (builtin_text_functions_help (), count);
      output_buf
	<< "\n*** text functions (these names are also reserved):\n\n";
      if (symbols != (char **) NULL && count > 0)
	list_in_columns (output_buf, symbols);
      delete [] symbols;

      symbols = names (builtin_mapper_functions_help (), count);
      output_buf << "\n*** mapper functions:\n\n";
      if (symbols != (char **) NULL && count > 0)
	list_in_columns (output_buf, symbols);
      delete [] symbols;

      symbols = names (builtin_general_functions_help (), count);
      output_buf << "\n*** general functions:\n\n";
      if (symbols != (char **) NULL && count > 0)
	list_in_columns (output_buf, symbols);
      delete [] symbols;

      symbols = names (builtin_variables_help (), count);
      output_buf << "\n*** builtin variables:\n\n";
      if (symbols != (char **) NULL && count > 0)
	list_in_columns (output_buf, symbols);
      delete [] symbols;

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
	      char **names = get_m_file_names (count, *ptr, 0);
	      output_buf << "\n*** M-files in "
		      << make_absolute (*ptr, the_current_working_directory)
		      << ":\n\n";
	      if (names != (char **) NULL && count > 0)
		list_in_columns (output_buf, names);
	      delete [] names;
	      ptr++;
	    }
	}
    }
  else
    {
      symbol_record *sym_rec;
      help_list *op_help_list = operator_help ();
      help_list *kw_help_list = keyword_help ();
      for (int i = 1; i < argc; i++)
	{
	  if (argv[i] == (char *) NULL || argv[i][0] == '\0')
	    continue;

	  int j = 0;
	  char *name;
	  while ((name = op_help_list[j].name) != (char *) NULL)
	    {
	      if (strcmp (name, argv[i]) == 0)
		{
		 output_buf << "\n" << op_help_list[j].help << "\n";
		  goto next;
		}
	      j++;
	    }

	  j = 0;
	  while ((name = kw_help_list[j].name) != (char *) NULL)
	    {
	      if (strcmp (name, argv[i]) == 0)
		{
		  output_buf << "\n" << kw_help_list[j].help << "\n";
		  goto next;
		}
	      j++;
	    }

	  sym_rec = curr_sym_tab->lookup (argv[i], 0, 0);
	  if (sym_rec != (symbol_record *) NULL)
	    {
	      char *h = sym_rec->help ();
	      if (h != (char *) NULL && *h != '\0')
		{
		  output_buf << "\n" << h << "\n";
		  goto next;
		}
	    }

	  sym_rec = global_sym_tab->lookup (argv[i], 0, 0);
	  if (sym_rec != (symbol_record *) NULL
	      && ! symbol_out_of_date (sym_rec))
	    {
	      char *h = sym_rec->help ();
	      if (h != (char *) NULL && *h != '\0')
		{
		  output_buf << "\n" << h << "\n";
		  goto next;
		}
	    }

// Try harder to find M-files that might not be defined yet, or that
// appear to be out of date.  Don\'t execute commands from the file if
// it turns out to be a script file.

	  sym_rec = global_sym_tab->lookup (argv[i], 1, 0);
	  if (sym_rec != (symbol_record *) NULL)
	    {
	      tree_identifier tmp (sym_rec);
	      tmp.parse_m_file (0);
	      char *h = sym_rec->help ();
	      if (h != (char *) NULL && *h != '\0')
		{
		  output_buf << "\n" << h << "\n";
		  goto next;
		}
	    }
	  else
	    global_sym_tab->clear (argv[i]);

	  output_buf << "Sorry, `" << argv[i] << "' is not documented\n";

	next:
	  continue;
	}
    }

  output_buf << ends;
  maybe_page_output (output_buf);

  return retval;
}

/*
 * Display, save, or load history.
 */
tree_constant
builtin_history (int argc, char **argv)
{
  tree_constant retval;

  do_history (argc, argv);

  return retval;
}

static int
load_variable (char *nm, int force, istream& is)
{
  symbol_record *gsr = global_sym_tab->lookup (nm, 0, 0);
  symbol_record *lsr = curr_sym_tab->lookup (nm, 0, 0);

  if (! force
      && ((gsr != (symbol_record *) NULL && gsr->is_variable ())
	  || lsr != (symbol_record *) NULL))
    {
      message ("load",
        "variable name `%s' exists -- use `load -force' to overwrite", nm);
      return -1;
    }

// We found it.  Read data for this entry, and if that succeeds,
// insert it into symbol table.

  tree_constant tc;
  int global = tc.load (is);
  if (tc.const_type () != tree_constant_rep::unknown_constant)
    {
      symbol_record *sr;
      if (global)
	{
	  if (lsr != (symbol_record *) NULL)
	    {
	      warning ("load: replacing local symbol `%s' with global\
 value from file", nm);
	      curr_sym_tab->clear (nm);
	    }
	  sr = global_sym_tab->lookup (nm, 1, 0);
	}
      else
	{
	  if (gsr != (symbol_record *) NULL)
	    {
	      warning ("loading `%s' as a global variable", nm);
	      sr = gsr;
	    }
	  else
	    sr = curr_sym_tab->lookup (nm, 1, 0);
	}

      if (sr != (symbol_record *) NULL)
	{
	  tree_constant *tmp_tc = new tree_constant (tc);
	  sr->define (tmp_tc);
	  return 1;
	}
      else
	error ("load: unable to load variable `%s'", nm);
    }

  return 0;
}

/*
 * Read variables from an input stream.
 *
 * BUGS:
 *
 *  -- This function is not terribly robust.
 *  -- Symbols are only inserted into the current symbol table.
 */
tree_constant
builtin_load (int argc, char **argv)
{
  tree_constant retval;

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
      message ("load", "you must specify a single file to read");
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

  char nm [128]; // XXX FIXME XXX
  int count = 0;
  for (;;)
    {
// Read name for this entry or break on EOF.
      if (extract_keyword (stream, "name", nm) == 0 || nm == (char *) NULL)
	{
	  if (count == 0)
	    message ("load",
         "no name keywords found.  Are you sure this is an octave data file?");
	  break;
	}

      if (*nm == '\0')
	continue;

      if (! valid_identifier (nm))
	{
	  message ("load", "skipping bogus identifier `%s'", nm);
	  continue;
	}

      if (load_variable (nm, force, stream))
	count++;
    }

  if (file);
    file.close ();

  return retval;
}

/*
 * Get a directory listing.
 */
tree_constant
builtin_ls (int argc, char **argv)
{
  tree_constant retval;

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
tree_constant
builtin_run_history (int argc, char **argv)
{
  tree_constant retval;
  do_run_history (argc, argv);
  return retval;
}

/*
 * Write variables to an output stream.
 */
tree_constant
builtin_save (int argc, char **argv)
{
  tree_constant retval;

  if (argc < 2)
    {
      usage ("save file         -- save all variables in named file\n\
       save file var ... -- saved named variables");
      return retval;
    }

  argc--;
  argv++;

  static ostream stream;
  static ofstream file;
  if (strcmp (*argv, "-") == 0)
    {
// XXX FIXME XXX -- things intended for the screen should end up in a
// tree_constant (string)?
      stream = cout;
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

  if (argc == 1)
    {
      curr_sym_tab->save (stream);
      global_sym_tab->save (stream, 1);
    }
  else
    {
      while (--argc > 0)
	{
	  argv++;
	  if (! curr_sym_tab->save (stream, *argv))
	    if (! global_sym_tab->save (stream, *argv, 1))
	      {
		message ("save", "no such variable `%s'", *argv);
		continue;
	      }
	}
    }

  if (file);
    file.close ();

  return retval;
}

/*
 * Set plotting options.
 */
tree_constant
builtin_set (int argc, char **argv)
{
  tree_constant retval;

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
tree_constant
builtin_show (int argc, char **argv)
{
  tree_constant retval;

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
tree_constant
builtin_who (int argc, char **argv)
{
  tree_constant retval;
  int show_global = 0;
  int show_local = 1;
  int show_top = 0;
  int show_fcns = 0;

  if (argc > 1)
    show_local = 0;

  for (int i = 1; i < argc; i++)
    {
      argv++;
      if (strcmp (*argv, "-all") == 0)
	{
	  show_global++;	  
	  show_local++;
	  show_top++;
	  show_fcns++;
	}
      else if (strcmp (*argv, "-global") == 0)
	show_global++;
      else if (strcmp (*argv, "-local") == 0)
	show_local++;
      else if (strcmp (*argv, "-top") == 0)
	show_top++;
      else if (strcmp (*argv, "-fcn") == 0
	       || strcmp (*argv, "-fcns") == 0
	       || strcmp (*argv, "-functions") == 0)
	show_fcns++;
      else
	{
	  message ("who", "unrecognized option `%s'", *argv);
	  if (argc == 2)
	    show_local = 1;
	}
    }

  ostrstream output_buf;
  int pad_after = 0;
  if (show_global)
    {
      int count = 0;
      char **symbols = global_sym_tab->sorted_var_list (count);
      if (symbols != (char **) NULL && count > 0)
	{
	  output_buf << "\n*** global symbols:\n\n";
	  list_in_columns (output_buf, symbols);
	  delete [] symbols;
	  pad_after++;
	}
    }

  if (show_top)
    {
      int count = 0;
      char **symbols = top_level_sym_tab->sorted_var_list (count);
      if (symbols != (char **) NULL && count > 0)
	{
	  output_buf << "\n*** top level symbols:\n\n";
	  list_in_columns (output_buf, symbols);
	  delete [] symbols;
	  pad_after++;
	}
    }

  if (show_local)
    {
      if (show_top && curr_sym_tab == top_level_sym_tab)
	output_buf <<
	  "\ncurrent (local) symbol table == top level symbol table\n";
      else
	{
	  int count = 0;
	  char **symbols = curr_sym_tab->sorted_var_list (count);
	  if (symbols != (char **) NULL && count > 0)
	    {
	      output_buf << "\n*** local symbols:\n\n";
	      list_in_columns (output_buf, symbols);
	      delete [] symbols;
	      pad_after++;
	    }
	}
    }

  if (show_fcns)
    {
      int count = 0;
      char **symbols = global_sym_tab->sorted_fcn_list (count);
      if (symbols != (char **) NULL && count > 0)
	{
	  output_buf << "\n*** functions builtin or currently compiled:\n\n";
	  list_in_columns (output_buf, symbols);
	  delete [] symbols;
	  pad_after++;
	}
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
