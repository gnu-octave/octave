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

#include "tree-expr.h"
#include "tree-const.h"
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

#define boolean kpathsea_boolean
#define false kpathsea_false
#define true kpathsea_true
#include <kpathsea/pathsearch.h>
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

// Return a copy of the operator or keyword names.

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

void
additional_help_message (ostrstream& output_buf)
{
  output_buf
    << "\n"
    << "Additional help for builtin functions, operators, and variables\n"
    << "is available in the on-line version of the manual.\n"
    << "\n"
    << "Use the command `help -i <topic>' to search the manual index.\n";
}

void
print_usage (const char *string, int just_usage)
{
  ostrstream output_buf;

  symbol_record *sym_rec = global_sym_tab->lookup (string, 0, 0);
  if (sym_rec)
    {
      char *h = sym_rec->help ();
      if (h && *h)
	{
	  output_buf << "\n*** " << string << ":\n\n"
	    << h << "\n";

	  if (! just_usage)
	    additional_help_message (output_buf);
	  output_buf << ends;
	  maybe_page_output (output_buf);
	}
    }
}

static void
display_names_from_help_list (ostrstream& output_buf, help_list *list,
			      const char *desc)
{
  int count = 0;
  char **symbols = names (list, count);
  output_buf << "\n*** " << desc << ":\n\n";
  if (symbols && count > 0)
    list_in_columns (output_buf, symbols);
  delete [] symbols;
}

static char *
print_symbol_type (ostrstream& output_buf, symbol_record *sym_rec,
		   char *name, int print)
{
  char *retval = 0;

  if (sym_rec->is_user_function ())
    {
      tree_fvc *defn = sym_rec->def ();
      char *fn = defn->fcn_file_name ();
      if (fn)
	{
	  char *ff = fcn_file_in_path (fn);
	  ff = ff ? ff : fn;

	  if (print)
	    output_buf << name
	      << " is the function defined from:\n"
		<< ff << "\n";
	  else
	    retval = ff;
	}
      else
	{
	  if (print)
	    output_buf << name << " is a user-defined function\n";
	  else
	    retval = "user-defined function";
	}
    }
  else if (sym_rec->is_text_function ())
    {
      if (print)
	output_buf << name << " is a builtin text-function\n";
      else
	retval = "builtin text-function";
    }
  else if (sym_rec->is_builtin_function ())
    {
      if (print)
	output_buf << name << " is a builtin function\n";
      else
	retval = "builtin function";
    }
  else if (sym_rec->is_user_variable ())
    {
      if (print)
	output_buf << name << " is a user-defined variable\n";
      else
	retval = "user-defined variable";
    }
  else if (sym_rec->is_builtin_variable ())
    {
      if (print)
	output_buf << name << " is a builtin variable\n";
      else
	retval = "builtin variable";
    }
  else
    {
      if (print)
	output_buf << "which: `" << name
	  << "' has unknown type\n";
      else
	retval = "unknown type";
    }

  return retval;
}

static void
display_symtab_names (ostrstream& output_buf, char **names,
		      int count, const char *desc)
{
  output_buf << "\n*** " << desc << ":\n\n";
  if (names && count > 0)
    list_in_columns (output_buf, names);
}

static void
simple_help (void)
{
  ostrstream output_buf;

  display_names_from_help_list (output_buf, operator_help (),
				"operators");

  display_names_from_help_list (output_buf, keyword_help (),
				"reserved words");

#ifdef LIST_SYMBOLS
#undef LIST_SYMBOLS
#endif
#define LIST_SYMBOLS(type, msg) \
  do \
    { \
      int count; \
      char **names = global_sym_tab->list (count, 1, type); \
      display_symtab_names (output_buf, names, count, msg); \
      char **ptr = names; \
      while (*ptr) \
        delete [] *ptr++; \
      delete [] names; \
    } \
  while (0)

// XXX FIXME XXX -- is this distinction needed?
  LIST_SYMBOLS (symbol_def::TEXT_FUNCTION,
		"text functions (these names are also reserved)");

  LIST_SYMBOLS (symbol_def::MAPPER_FUNCTION, "mapper functions");

  LIST_SYMBOLS (symbol_def::BUILTIN_FUNCTION, "general functions");

  LIST_SYMBOLS (symbol_def::BUILTIN_VARIABLE, "builtin variables");

// Also need to list variables and currently compiled functions from
// the symbol table, if there are any.

// Also need to search octave_path for script files.

  char *path_elt = kpse_path_element (user_pref.loadpath);

  while (path_elt)
    {
      str_llist_type *elt_dirs = kpse_element_dirs (path_elt);

      str_llist_elt_type *dir;
      for (dir = *elt_dirs; dir; dir = STR_LLIST_NEXT (*dir))
	{
	  char *elt_dir = STR_LLIST (*dir);

	  if (elt_dir)
	    {
	      int count;
	      char **names = get_fcn_file_names (count, elt_dir, 0);

	      output_buf << "\n*** function files in "
		<< make_absolute (elt_dir, the_current_working_directory)
		  << ":\n\n";

	      if (names && count > 0)
		list_in_columns (output_buf, names);

	      delete [] names;
	    }
	}
      path_elt = kpse_path_element (0);
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

int
help_from_list (ostrstream& output_buf, const help_list *list,
		const char *string, int usage)
{
  char *name;
  while ((name = list->name) != 0)
    {
      if (strcmp (name, string) == 0)
	{
	  if (usage)
	    output_buf << "\nusage: ";
	  else
	    {
	      output_buf << "\n*** " << string << ":\n\n";
	    }

	  output_buf << list->help << "\n";

	  return 1;
	}
      list++;
    }
  return 0;
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

	      symbol_record *sym_rec = lookup_by_name (*argv);

	      if (sym_rec)
		{
		  char *h = sym_rec->help ();
		  if (h && *h)
		    {
		      print_symbol_type (output_buf, sym_rec, *argv, 1);
		      output_buf << "\n" << h << "\n";
		      continue;
		    }
		}

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

DEFUN_TEXT ("type", Ftype, Stype, -1, 1,
  "type NAME ...]\n\
\n\
display the definition of each NAME that refers to a function")
{
  Octave_object retval;

  DEFINE_ARGV("type");

  if (argc > 1)
    {
// XXX FIXME XXX -- we should really use getopt ()
      int quiet = 0;
      if (argv[1] && strcmp (argv[1], "-q") == 0)
	{
	  quiet = 1;
	  argc--;
	  argv++;
	}

      ostrstream output_buf;

      while (--argc > 0)
	{
	  argv++;

	  if (! *argv || ! **argv)
	    continue;

	  symbol_record *sym_rec = lookup_by_name (*argv);

	  if (sym_rec)
	    {
	      if (sym_rec->is_user_function ())
		{
		  tree_fvc *defn = sym_rec->def ();

		  if (nargout == 0 && ! quiet)
		    output_buf << *argv << " is a user-defined function\n";

		  defn->print_code (output_buf);
		}

// XXX FIXME XXX -- this code should be shared with Fwhich

	      else if (sym_rec->is_text_function ())
		output_buf << *argv << " is a builtin text-function\n";
	      else if (sym_rec->is_builtin_function ())
		output_buf << *argv << " is a builtin function\n";
	      else if (sym_rec->is_user_variable ())
		{
		  tree_fvc *defn = sym_rec->def ();

		  if (nargout == 0 && ! quiet)
		    output_buf << *argv << " is a user-defined variable\n";

		  defn->print_code (output_buf);

		  if (nargout == 0)
		    output_buf << "\n";
		}
	      else if (sym_rec->is_builtin_variable ())
		{
		  tree_fvc *defn = sym_rec->def ();

		  if (nargout == 0 && ! quiet)
		    output_buf << *argv << " is a builtin variable\n";

		  defn->print_code (output_buf);

		  if (nargout == 0)
		    output_buf << "\n";
		}
	      else
		output_buf << "type: `" << *argv << "' has unknown type!\n";
	    }
	  else
	    output_buf << "type: `" << *argv << "' undefined\n";
	}

      output_buf << ends;

      if (nargout == 0)
	maybe_page_output (output_buf);
      else
	{
	  char *s = output_buf.str ();
	  retval = s;
	  delete s;
	}
    }
  else
    print_usage ("type");

  DELETE_ARGV;

  return retval;
}

DEFUN_TEXT ("which", Fwhich, Swhich, -1, 1,
  "which NAME ...]\n\
\n\
display the type of each NAME.  If NAME is defined from an function\n\
file, print the full name of the file.")
{
  Octave_object retval;

  DEFINE_ARGV("which");

  if (argc > 1)
    {
      if (nargout > 0)
	retval.resize (argc-1, Matrix ());

      ostrstream output_buf;

      for (int i = 0; i < argc-1; i++)
	{
	  argv++;

	  if (! *argv || ! **argv)
	    continue;

	  symbol_record *sym_rec = lookup_by_name (*argv);

	  if (sym_rec)
	    {
	      int print = (nargout == 0);
	      char *tmp = print_symbol_type (output_buf, sym_rec,
					     *argv, print);
	      if (! print)
		retval(i) = tmp;
	    }
	  else
	    {
	      if (nargout == 0)
		output_buf << "which: `" << *argv << "' is undefined\n";
	      else
		retval(i) = "undefined";
	    }
	}
      output_buf << ends;
      maybe_page_output (output_buf);
    }
  else
    print_usage ("which");

  DELETE_ARGV;

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
