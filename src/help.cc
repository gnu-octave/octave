/*

Copyright (C) 1996 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <csignal>
#include <cstdlib>
#include <cstring>

#include <string>

#include <iostream.h>
#include <strstream.h>

#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif

#include "str-vec.h"

#include "defun.h"
#include "dirfns.h"
#include "error.h"
#include "help.h"
#include "input.h"
#include "oct-obj.h"
#include "pager.h"
#include "pathsearch.h"
#include "pt-const.h"
#include "pt-exp.h"
#include "pt-pr-code.h"
#include "sighandlers.h"
#include "symtab.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "user-prefs.h"
#include "utils.h"
#include "variables.h"

// If TRUE, don't print additional help message in help and usage
// functions.
static bool Vsuppress_verbose_help_message;

static help_list operators[] =
{
  { "!",
    "Logical not operator.  See also `~'.\n", },

  { "!=",
    "Logical not equals operator.  See also `~' and `<>'.\n", },

  { "\"",
    "String delimiter.\n", },

  { "#",
    "Begin comment character.  See also `%'.", },

  { "%",
    "Begin comment charcter.  See also `#'.", },

  { "&",
    "Logical and operator.  See also `&&'.", },

  { "&&",
    "Logical and operator.  See also `&'.", },

  { "'",
    "Matrix transpose operator.  For complex matrices, computes the\n\
complex conjugate (Hermitian) transpose.  See also `.''\n\
\n\
The single quote character may also be used to delimit strings, but\n\
it is better to use the double quote character, since that is never\n\
ambiguous", },

  { "(",
    "Array index or function argument delimiter.", },

  { ")",
    "Array index or function argument delimiter.", },

  { "*",
    "Multiplication operator.  See also `.*'", },

  { "**",
    "Power operator.  See also `^', `.**', and `.^'", },

  { "+",
    "Addition operator.", },

  { "++",
    "Increment operator.  As in C, may be applied as a prefix or postfix operator.", },

  { ",",
    "Array index, function argument, or command separator.", },

  { "-",
    "Subtraction or unary negation operator.", },

  { "--",
    "Decrement operator.  As in C, may be applied as a prefix or postfix operator.", },

  { ".'",
    "Matrix transpose operator.  For complex matrices, computes the\n\
transpose, *not* the complex conjugate transpose.  See also `''.", },

  { ".*",
    "Element by element multiplication operator.  See also `*'.", },

  { ".**",
    "Element by element power operator.  See also `**', `^', and `.^'.", },

  { "./",
    "Element by element division operator.  See also `/' and `\\'.", },

  { ".^",
    "Element by element power operator.  See also `**', `^', and `.^'.", },

  { "/",
    "Right division.  See also `\\' and `./'.", },

  { ":",
    "Select entire rows or columns of matrices.", },

  { ";",
    "Array row or command separator.  See also `,'.", },

  { "<",
    "Less than operator.", },

  { "<=",
    "Less than or equals operator.", },

  { "<>",
    "Logical not equals operator.  See also `!=' and `~='.", },

  { "=",
    "Assignment operator.", },

  { "==",
    "Equality test operator.", },

  { ">",
    "Greater than operator.", },

  { ">=",
    "Greater than or equals operator.", },

  { "[",
    "Return list delimiter.  See also `]'.", },

  { "\\",
    "Left division operator.  See also `/' and `./'.", },

  { "]",
    "Return list delimiter.  See also `['.", },

  { "^",
    "Power operator.  See also `**', `.^', and `.**.'", },

  { "|",
    "Logical or operator.  See also `||'.", },

  { "||",
    "Logical or operator.  See also `|'.", },

  { "~",
    "Logical not operator.  See also `!' and `~'.", },

  { "~=",
    "Logical not equals operator.  See also `<>' and `!='.", },

  { 0, 0, },
};

static help_list keywords[] =
{
  { "all_va_args",
    "Pass all unnamed arguments to another function call.", },

  { "break",
    "Exit the innermost enclosing while or for loop.", },

  { "catch",
    "begin the cleanup part of a try-catch block", },

  { "continue",
    "Jump to the end of the innermost enclosing while or for loop.", },

  { "else",
    "Alternate action for an if block.", },

  { "elseif",
    "Alternate conditional test for an if block.", },

  { "end",
    "Mark the end of any for, if, while, or function block.", },

  { "end_try_catch",
    "Mark the end of an try-catch block.", }, 

  { "end_unwind_protect",
    "Mark the end of an unwind_protect block.", }, 

  { "endfor",
    "Mark the end of a for loop.", },

  { "endfunction",
    "Mark the end of a function.", },

  { "endif",
    "Mark the end of an if block.", },

  { "endwhile",
    "Mark the end of a while loop.", },

  { "for",
    "Begin a for loop.", },

  { "function",
    "Begin a function body.", },

  { "global",
    "Declare variables to have global scope.", },

  { "gplot",
    "Produce 2-D plots using gnuplot-like command syntax.", },

  { "gsplot",
    "Produce 3-D plots using gnuplot-like command syntax.", },

  { "if",
    "Begin an if block.", },

  { "return",
    "Return from a function.", },

  { "try",
    "Begin a try-catch block.", }, 

  { "unwind_protect",
    "Begin an unwind_protect block.", }, 

  { "unwind_protect_cleanup",
    "Begin the cleanup section of an unwind_protect block.", }, 

  { "while",
    "Begin a while loop.", },

  { 0, 0, },
};

// Return a copy of the operator or keyword names.

string_vector
names (help_list *lst, int& count)
{
  string_vector retval;

  count = 0;
  help_list *ptr = lst;
  while (ptr->name)
    {
      count++;
      ptr++;
    }

  if (count > 0)
    {
      retval.resize (count);

      ptr = lst;
      for (int i = 0; i < count; i++)
	{
	  retval[i] = ptr->name;
	  ptr++;
	}
    }

  return retval;
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

#define VERBOSE_HELP_MESSAGE \
  "\n\
Additional help for builtin functions, operators, and variables\n\
is available in the on-line version of the manual.\n\
\n\
Use the command `help -i <topic>' to search the manual index.\n"

static void
additional_help_message (ostream& os)
{
#ifdef USE_GNU_INFO
  if (! Vsuppress_verbose_help_message)
    os << VERBOSE_HELP_MESSAGE;
#endif
}

void
print_usage (const string& nm, int just_usage)
{
  symbol_record *sym_rec = global_sym_tab->lookup (nm, 0, 0);
  if (sym_rec)
    {
      string h = sym_rec->help ();

      if (h.length () > 0)
	{
	  octave_stdout << "\n*** " << nm << ":\n\n"
	    << h << "\n";

	  if (! just_usage)
	    additional_help_message (octave_stdout);
	}
    }
  else
    warning ("no usage message found for `%s'", nm.c_str ());
}

static void
display_names_from_help_list (ostream& os, help_list *list,
			      const char *desc)
{
  int count = 0;
  string_vector symbols = names (list, count);
  if (! symbols.empty ())
    {
      os << "\n*** " << desc << ":\n\n";
      symbols.list_in_columns (os);
    }
}

static string
print_symbol_type (ostream& os, symbol_record *sym_rec,
		   const string& name, int print)
{
  string retval;

  if (sym_rec->is_user_function ())
    {
      tree_fvc *defn = sym_rec->def ();

      string fn = defn->fcn_file_name ();

      if (! fn.empty ())
	{
	  string ff = fcn_file_in_path (fn);

	  ff = ff.length () > 0 ? ff : fn;

	  if (print)
	    os << name
	       << " is the function defined from:\n"
	       << ff << "\n";
	  else
	    retval = ff;
	}
      else
	{
	  if (print)
	    os << name << " is a user-defined function\n";
	  else
	    retval = "user-defined function";
	}
    }
  else if (sym_rec->is_text_function ())
    {
      if (print)
	os << name << " is a builtin text-function\n";
      else
	retval = "builtin text-function";
    }
  else if (sym_rec->is_builtin_function ())
    {
      if (print)
	os << name << " is a builtin function\n";
      else
	retval = "builtin function";
    }
  else if (sym_rec->is_user_variable ())
    {
      if (print)
	os << name << " is a user-defined variable\n";
      else
	retval = "user-defined variable";
    }
  else if (sym_rec->is_builtin_variable ())
    {
      if (print)
	os << name << " is a builtin variable\n";
      else
	retval = "builtin variable";
    }
  else
    {
      if (print)
	os << "which: `" << name << "' has unknown type\n";
      else
	retval = "unknown type";
    }

  return retval;
}

static void
display_symtab_names (ostream& os, const string_vector& names,
		      int /* count */, const string& desc)
{
  if (! names.empty ())
    {
      os << "\n*** " << desc << ":\n\n";
      names.list_in_columns (os);
    }
}

static void
simple_help (void)
{
  display_names_from_help_list (octave_stdout, operator_help (),
				"operators");

  display_names_from_help_list (octave_stdout, keyword_help (),
				"reserved words");

#ifdef LIST_SYMBOLS
#undef LIST_SYMBOLS
#endif
#define LIST_SYMBOLS(type, msg) \
  do \
    { \
      int count; \
      string_vector names = global_sym_tab->list (count, 0, 0, 1, type); \
      display_symtab_names (octave_stdout, names, count, msg); \
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

  dir_path p (user_pref.loadpath);

  string_vector dirs = p.all_directories ();

  int len = dirs.length ();

  for (int i = 0; i < len; i++)
    {
      string_vector names = get_fcn_file_names (dirs[i]);

      if (! names.empty ())
	{
	  octave_stdout << "\n*** function files in "
			<< make_absolute (dirs[i],
					  the_current_working_directory)
			<< ":\n\n";

	  names.list_in_columns (octave_stdout);
	}
    }

  additional_help_message (octave_stdout);
}

#ifdef USE_GNU_INFO
static int
try_info (const string& nm)
{
  int status = 0;

  static char *cmd_str = 0;

  delete [] cmd_str;
  cmd_str = 0;

  ostrstream cmd_buf;

  cmd_buf << user_pref.info_prog << " --file " << user_pref.info_file;

  string directory_name = user_pref.info_file;
  size_t pos = directory_name.rfind ('/');

  if (pos != NPOS)
    {
      directory_name.resize (pos + 1);
      cmd_buf << " --directory " << directory_name;
    }

  if (nm.length () > 0)
    cmd_buf << " --index-search " << nm;

  cmd_buf << ends;

  cmd_str = cmd_buf.str ();

  volatile sig_handler *old_sigint_handler;
  old_sigint_handler = octave_set_signal_handler (SIGINT, SIG_IGN);

  status = system (cmd_str);

  octave_set_signal_handler (SIGINT, old_sigint_handler);

  if ((status & 0xff) == 0)
    status = (signed char) ((status & 0xff00) >> 8);
  else
    status = 127;

  return status;
}
#endif

static void
help_from_info (const string_vector& argv, int idx, int argc)
{
#ifdef USE_GNU_INFO
  if (idx == argc)
    try_info (string ());
  else
    {
      for (int i = idx; i < argc; i++)
	{
	  int status = try_info (argv[i]);

	  if (status)
	    {
	      if (status < 0)
		{
		  message ("help",
			   "sorry, `%s' is not indexed in the manual",
			   argv[i].c_str ());
		  sleep (2);
		}
	      else
		{
		  error ("help: unable to find info!");
		  break;
		}
	    }
	}
    }
#else
  message (0, "sorry, help -i is not available in this version of Octave");
#endif
}

int
help_from_list (ostream& os, const help_list *list,
		const string& nm, int usage)
{
  char *name;
  while ((name = list->name) != 0)
    {
      if (strcmp (name, nm.c_str ()) == 0)
	{
	  if (usage)
	    os << "\nusage: ";
	  else
	    {
	      os << "\n*** " << nm << ":\n\n";
	    }

	  os << list->help << "\n";

	  return 1;
	}
      list++;
    }
  return 0;
}

static void
builtin_help (int argc, const string_vector& argv)
{
  help_list *op_help_list = operator_help ();
  help_list *kw_help_list = keyword_help ();

  for (int i = 1; i < argc; i++)
    {
      if (help_from_list (octave_stdout, op_help_list, argv[i], 0))
	continue;

      if (help_from_list (octave_stdout, kw_help_list, argv[i], 0))
	continue;

      symbol_record *sym_rec = lookup_by_name (argv[i], 0);

      if (sym_rec && sym_rec->is_defined ())
	{
	  string h = sym_rec->help ();

	  if (h.length () > 0)
	    {
	      print_symbol_type (octave_stdout, sym_rec, argv[i], 1);
	      octave_stdout << "\n" << h << "\n";
	      continue;
	    }
	}

      string path = fcn_file_in_path (argv[i]);

      string h = get_help_from_file (path);

      if (! h.empty ())
	{
	  octave_stdout << argv[i] << " is the file:\n"
	    << path << "\n\n" << h << "\n";

	  continue;
	}

      octave_stdout << "\nhelp: sorry, `" << argv[i]
		    << "' is not documented\n"; 
    }

  additional_help_message (octave_stdout);
}

#ifdef USE_GNU_INFO
DEFUN_TEXT (help, args, ,
  "help [-i] [topic ...]\n\
\n\
print cryptic yet witty messages")
#else
DEFUN_TEXT (help, args, ,
  "help [topic ...]\n\
\n\
print cryptic yet witty messages")
#endif
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("help");

  if (error_state)
    return retval;

  if (argc == 1)
    {
      simple_help ();
    }
  else
    {
      if (argv[1] == "-i")
	{
	  help_from_info (argv, 2, argc);
	}
      else
	{
	  builtin_help (argc, argv);
	}
    }

  return retval;
}

DEFUN_TEXT (type, args, nargout,
  "type NAME ...]\n\
\n\
display the definition of each NAME that refers to a function")
{
  octave_value_list retval;

  begin_unwind_frame ("Ftype");

  unwind_protect_str (Vps4);
  Vps4 = "";

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("type");

  if (error_state)
    return retval;

  if (argc > 1)
    {
      // XXX FIXME XXX -- we should really use getopt ()

      int quiet = 0;
      int idx = 1;
      if (argv[idx] == "-q")
	{
	  quiet = 1;
	  idx++;
	}

      ostrstream output_buf;

      for (int i = idx; i < argc; i++)
	{
	  string id = argv[i];
	  string elts;

	  if (id[id.length () - 1] != '.')
	    {
	      size_t pos = id.find ('.');

	      if (pos != NPOS)
		{
		  elts = id.substr (pos+1);
		  id = id.substr (0, pos);
		}
	    }

	  symbol_record *sym_rec = lookup_by_name (id, 0);

	  if (sym_rec)
	    {
	      if (sym_rec->is_user_function ())
		{
		  tree_fvc *defn = sym_rec->def ();

		  if (nargout == 0 && ! quiet)
		    output_buf << argv[i] << " is a user-defined function\n";

		  tree_print_code tpc (output_buf);

		  defn->accept (tpc);
		}

	      // XXX FIXME XXX -- this code should be shared with
	      // Fwhich.

	      else if (sym_rec->is_text_function ())
		output_buf << argv[i] << " is a builtin text-function\n";
	      else if (sym_rec->is_builtin_function ())
		output_buf << argv[i] << " is a builtin function\n";
	      else if (sym_rec->is_user_variable ()
		       || sym_rec->is_builtin_variable ())
		{
		  tree_fvc *defn = sym_rec->def ();

		  assert (defn && defn->is_constant ());

		  octave_value *tmp = (octave_value *) defn;

		  int var_ok = 1;
		  if (tmp && tmp->is_map ())
		    {
		      if (! elts.empty ())
			{
			  octave_value ult =
			    tmp->lookup_map_element (elts, 0, 1);

			  if (! ult.is_defined ())
			    var_ok = 0;			    
			}
		    }

		  if (nargout == 0 && ! quiet)
		    {
		      if (var_ok)
			{
			  output_buf << argv[i];
			  if (sym_rec->is_user_variable ())
			    output_buf << " is a user-defined variable\n";
			  else
			    output_buf << " is a built-in variable\n";
			}
		      else
			{
			  if (! elts.empty ())
			    output_buf << "type: structure `" << id
			      << "' has no member `" << elts << "'\n";
			  else
			    output_buf << "type: `" << id
			      << "' has unknown type!";
			}
		    }
		  if (! tmp->is_map ())
		    {
		      tree_print_code tpc (output_buf);

		      tmp->accept (tpc);

		      if (nargout == 0)
			output_buf << "\n";
		    }
		}
	      else
		output_buf << "type: `" << argv[i] << "' has unknown type!\n";
	    }
	  else
	    output_buf << "type: `" << argv[i] << "' undefined\n";
	}

      output_buf << ends;

      char *s = output_buf.str ();

      if (nargout == 0)
	octave_stdout << s;
      else
	retval = s;

      delete [] s;
    }
  else
    print_usage ("type");

  run_unwind_frame ("Ftype");

  return retval;
}

DEFUN_TEXT (which, args, nargout,
  "which NAME ...\n\
\n\
display the type of each NAME.  If NAME is defined from an function\n\
file, print the full name of the file.")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("which");

  if (error_state)
    return retval;

  if (argc > 1)
    {
      if (nargout > 0)
	retval.resize (argc-1, Matrix ());

      for (int i = 1; i < argc; i++)
	{
	  symbol_record *sym_rec = lookup_by_name (argv[i], 0);

	  if (sym_rec)
	    {
	      int print = (nargout == 0);
	      string tmp = print_symbol_type (octave_stdout, sym_rec,
					      argv[i], print);
	      if (! print)
		retval(i) = tmp;
	    }
	  else
	    {
	      if (nargout == 0)
		octave_stdout << "which: `" << argv[i] << "' is undefined\n";
	      else
		retval(i) = "undefined";
	    }
	}
    }
  else
    print_usage ("which");

  return retval;
}

static int
suppress_verbose_help_message (void)
{
  Vsuppress_verbose_help_message
    = check_preference ("suppress_verbose_help_message");

  return 0;
}

void
symbols_of_help (void)
{
#ifdef USE_GNU_INFO
  DEFVAR (suppress_verbose_help_message, 0.0, 0, suppress_verbose_help_message,
    "suppress printing of message pointing to additional help in the\n\
help and usage functions");
#endif
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
