// help.cc                                             -*- C++ -*-
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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <signal.h>
#include <stdlib.h>
#include <iostream.h>
#include <strstream.h>

#include "tree.h"
#include "sighandlers.h"
#include "user-prefs.h"
#include "tree-expr.h"
#include "variables.h"
#include "oct-obj.h"
#include "symtab.h"
#include "octave.h"
#include "dirfns.h"
#include "pager.h"
#include "error.h"
#include "utils.h"
#include "help.h"
#include "defun.h"

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

// XXX FIXME XXX
#undef __FUNCTION_DEF
#include <readline/tilde.h>
}

static help_list operators[] =
{
  { "!",
    "Logical not operator.  See also `~'.\n", },

  { "!=",
    "Logical not equals operator.  See also `~' and `<>'.\n", },

  { "\"",
    "String delimiter.\n", },

  { "#",
    "Begin comment character.  See also `%'.\n", },

  { "%",
    "Begin comment charcter.  See also `#'.\n", },

  { "&",
    "Logical and operator.  See also `&&'.\n", },

  { "&&",
    "Logical and operator.  See also `&'.\n", },

  { "'",
    "Matrix transpose operator.  For complex matrices, computes the\n\
complex conjugate (Hermitian) transpose.  See also `.''\n\
\n\
The single quote character may also be used to delimit strings, but\n\
it is better to use the double quote character, since that is never\n\
ambiguous\n", },

  { "(",
    "Array index or function argument delimiter.\n", },

  { ")",
    "Array index or function argument delimiter.\n", },

  { "*",
    "Multiplication operator.  See also `.*'\n", },

  { "**",
    "Power operator.  See also `^', `.**', and `.^'\n", },

  { "+",
    "Addition operator.\n", },

  { "++",
    "Increment operator.  As in C, may be applied as a prefix or postfix operator.\n", },

  { ",",
    "Array index, function argument, or command separator.\n", },

  { "-",
    "Subtraction or unary negation operator.\n", },

  { "--",
    "Decrement operator.  As in C, may be applied as a prefix or postfix operator.\n", },

  { ".'",
    "Matrix transpose operator.  For complex matrices, computes the\n\
transpose, *not* the complex conjugate transpose.  See also `''.\n", },

  { ".*",
    "Element by element multiplication operator.  See also `*'.\n", },

  { ".**",
    "Element by element power operator.  See also `**', `^', and `.^'.\n", },

  { "./",
    "Element by element division operator.  See also `/' and `\\'.\n", },

  { ".^",
    "Element by element power operator.  See also `**', `^', and `.^'.\n", },

  { "/",
    "Right division.  See also `\\' and `./'.\n", },

  { ":",
    "Select entire rows or columns of matrices.\n", },

  { ";",
    "Array row or command separator.  See also `,'.\n", },

  { "<",
    "Less than operator.\n", },

  { "<=",
    "Less than or equals operator.\n", },

  { "<>",
    "Logical not equals operator.  See also `!=' and `~='.\n", },

  { "=",
    "Assignment operator.\n", },

  { "==",
    "Equality test operator.\n", },

  { ">",
    "Greater than operator.\n", },

  { ">=",
    "Greater than or equals operator.\n", },

  { "[",
    "Return list delimiter.  See also `]'.\n", },

  { "\\",
    "Left division operator.  See also `/' and `./'.\n", },

  { "]",
    "Return list delimiter.  See also `['.\n", },

  { "^",
    "Power operator.  See also `**', `.^', and `.**.'\n", },

  { "|",
    "Logical or operator.  See also `||'.\n", },

  { "||",
    "Logical or operator.  See also `|'.\n", },

  { "~",
    "Logical not operator.  See also `!' and `~'.\n", },

  { "~=",
    "Logical not equals operator.  See also `<>' and `!='.\n", },

  { 0, 0, },
};

static help_list keywords[] =
{
  { "break",
    "Exit the innermost enclosing while or for loop.\n", },

  { "continue",
    "Jump to the end of the innermost enclosing while or for loop.\n", },

  { "else",
    "Alternate action for an if block.\n", },

  { "elseif",
    "Alternate conditional test for an if block.\n", },

  { "end",
    "Mark the end of any for, if, while, or function block.\n", },

  { "endfor",
    "Mark the end of a for loop.\n", },

  { "endfunction",
    "Mark the end of a function.\n", },

  { "endif",
    "Mark the end of an if block.\n", },

  { "endwhile",
    "Mark the end of a while loop.\n", },

  { "for",
    "Begin a for loop.\n", },

  { "function",
    "Begin a function body.\n", },

  { "global",
    "Declare variables to have global scope.\n", },

  { "gplot",
    "Produce 2-D plots using gnuplot-like command syntax.\n", },

  { "gsplot",
    "Produce 3-D plots using gnuplot-like command syntax.\n", },

  { "if",
    "Begin an if block.\n", },

  { "return",
    "Return from a function.\n", },

  { "while",
    "Begin a while loop.\n", },

  { 0, 0, },
};

/*
 * Return a copy of the operator or keyword names.
 */
char **
names (help_list *lst, int& count)
{
  count = 0;
  help_list *ptr = lst;
  while (ptr->name)
    {
      count++;
      ptr++;
    }

  if (count == 0)
    return 0;
    
  char **name_list = new char * [count+1];

  ptr = lst;
  int i = 0;
  while (ptr->name)
    {
      name_list[i++] = strsave (ptr->name);
      ptr++;
    }

  name_list[i] = 0;
  return name_list;
}

help_list *
operator_help (void)
{
  return operators;
}

help_list *
keyword_help (void)
{
  return keywords;
}

static void
help_syms_list (ostrstream& output_buf, help_list *list,
		const char *desc)
{
  int count = 0;
  char **symbols = names (list, count);
  output_buf << "\n*** " << desc << ":\n\n";
  if (symbols && count > 0)
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
  if (ptr)
    {
      while (*ptr)
	{
	  int count;
	  char **names = get_fcn_file_names (count, *ptr, 0);
	  output_buf << "\n*** function files in "
		     << make_absolute (*ptr, the_current_working_directory)
		     << ":\n\n";
	  if (names && count > 0)
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

  NODE *initial_node = info_get_node (user_pref.info_file, 0);

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

	  info_last_executed_command = 0;

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

DEFUN_TEXT ("help", Fhelp, Shelp, -1, 1,
  "help [-i] [topic ...]\n\
\n\
print cryptic yet witty messages")
{
  Octave_object retval;

  DEFINE_ARGV("help");

  if (argc == 1)
    {
      simple_help ();
    }
  else
    {
      if (argv[1] && strcmp (argv[1], "-i") == 0)
	{
	  argc--;
	  argv++;

	  if (argc == 1)
	    {
	      volatile sig_handler *old_sigint_handler;
	      old_sigint_handler = signal (SIGINT, SIG_IGN);

	      try_info (0, 1);

	      signal (SIGINT, old_sigint_handler);
	    }
	  else
	    {
	      while (--argc > 0)
		{
		  argv++;

		  if (! *argv || ! **argv)
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

	  char *fcn_file_name = 0;
	  symbol_record *sym_rec;
	  help_list *op_help_list = operator_help ();
	  help_list *kw_help_list = keyword_help ();

	  while (--argc > 0)
	    {
	      argv++;

	      if (! *argv || ! **argv)
		continue;

	      if (help_from_list (output_buf, op_help_list, *argv, 0))
		continue;

	      if (help_from_list (output_buf, kw_help_list, *argv, 0))
		continue;

	      sym_rec = curr_sym_tab->lookup (*argv, 0, 0);
	      if (sym_rec)
		{
		  char *h = sym_rec->help ();
		  if (h && *h)
		    {
		      output_buf << "\n*** " << *argv << ":\n\n"
				 << h << "\n";
		      continue;
		    }
		}

	      sym_rec = global_sym_tab->lookup (*argv, 0, 0);
	      if (sym_rec && ! symbol_out_of_date (sym_rec))
		{
		  char *h = sym_rec->help ();
		  if (h && *h)
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
	      if (fcn_file_name)
		{
		  sym_rec = global_sym_tab->lookup (*argv, 1, 0);
		  if (sym_rec)
		    {
		      tree_identifier tmp (sym_rec);
		      tmp.parse_fcn_file (0);
		      char *h = sym_rec->help ();
		      if (h && *h)
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

  DELETE_ARGV;

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
