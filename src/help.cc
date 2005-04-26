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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>
#include <cstring>

#include <iostream>
#include <fstream>
#include <string>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "cmd-edit.h"
#include "file-ops.h"
#include "lo-sstream.h"
#include "oct-env.h"
#include "str-vec.h"

#include <defaults.h>
#include "defun.h"
#include "dirfns.h"
#include "error.h"
#include "fn-cache.h"
#include "gripes.h"
#include "help.h"
#include "input.h"
#include "oct-obj.h"
#include "ov-usr-fcn.h"
#include "pager.h"
#include "parse.h"
#include "pathsearch.h"
#include "procstream.h"
#include "sighandlers.h"
#include "symtab.h"
#include "syswait.h"
#include "toplev.h"
#include "utils.h"
#include "variables.h"
#include "version.h"

// Name of the info file specified on command line.
// (--info-file file)
std::string Vinfo_file;

// Name of the info reader we'd like to use.
// (--info-program program)
std::string Vinfo_prog;

// Name of the makeinfo program to run.
static std::string Vmakeinfo_prog = "makeinfo";

// If TRUE, don't print additional help message in help and usage
// functions.
static bool Vsuppress_verbose_help_message;

// XXX FIXME XXX -- maybe this should use string instead of char*.

struct help_list
{
  const char *name;
  const char *help;
};

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
    "Exit the innermost enclosing do, while or for loop.", },

  { "case",
    "A case statement in an switch. Octave cases are exclusive and do not\n\
fall-through as do C-language cases. A switch statement must have at least\n\
one case.",},

  { "catch",
    "begin the cleanup part of a try-catch block", },

  { "continue",
    "Jump to the end of the innermost enclosing do, while or for loop.", },

  { "do",
    "Begin a do-until loop. This differs from a do-while loop in that the\n\
body of the loop is executed at least once.",},

  { "else",
    "Alternate action for an if block.", },

  { "elseif",
    "Alternate conditional test for an if block.", },

  { "end",
    "Mark the end of any for, if, do, while, or function block.", },

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

  { "endswitch",
    "Mark the end of a switch block.", },

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

  { "otherwise",
    "The default statement in a switch block.", },

  { "persistent",
    "Declare variables as persistent.", },

  { "replot",
    "Replot a graphic.", },

  { "return",
    "Return from a function.", },

  { "static",
    "Declare variables as persistent.", },

  { "switch",
    "Begin a switch statement.",},

  { "try",
    "Begin a try-catch block.", }, 

  { "until",
    "End a do-until loop.",},

  { "unwind_protect",
    "Begin an unwind_protect block.", }, 

  { "unwind_protect_cleanup",
    "Begin the cleanup section of an unwind_protect block.", }, 

  { "varargin",
    "Pass an arbitrary number of arguments into a function.",},

  { "varargout",
    "Pass an arbitrary number of arguments out of a function.",},

  { "while",
    "Begin a while loop.", },

  { 0, 0, },
};

// Return a copy of the operator or keyword names.

static string_vector
names (help_list *lst)
{
  string_vector retval;

  int count = 0;
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

static help_list *
operator_help (void)
{
  return operators;
}

static help_list *
keyword_help (void)
{
  return keywords;
}

// It's not likely that this does the right thing now.  XXX FIXME XXX

string_vector
make_name_list (void)
{
  string_vector key = names (keyword_help ());
  int key_len = key.length ();

  string_vector fbi = fbi_sym_tab->name_list ();
  int fbi_len = fbi.length ();

  string_vector glb = global_sym_tab->name_list ();
  int glb_len = glb.length ();

  string_vector top = top_level_sym_tab->name_list ();
  int top_len = top.length ();

  string_vector lcl;
  if (top_level_sym_tab != curr_sym_tab)
    lcl = curr_sym_tab->name_list ();
  int lcl_len = lcl.length ();

  string_vector ffl = octave_fcn_file_name_cache::list_no_suffix ();
  int ffl_len = ffl.length ();

  int total_len = key_len + fbi_len + glb_len + top_len + lcl_len + ffl_len;

  string_vector list (total_len);

  // Put all the symbols in one big list.

  int j = 0;
  int i = 0;
  for (i = 0; i < key_len; i++)
    list[j++] = key[i];

  for (i = 0; i < fbi_len; i++)
    list[j++] = fbi[i];

  for (i = 0; i < glb_len; i++)
    list[j++] = glb[i];

  for (i = 0; i < top_len; i++)
    list[j++] = top[i];

  for (i = 0; i < lcl_len; i++)
    list[j++] = lcl[i];

  for (i = 0; i < ffl_len; i++)
    list[j++] = ffl[i];

  return list;
}

void
additional_help_message (std::ostream& os)
{
  if (! Vsuppress_verbose_help_message)
    os << "\n\
Additional help for built-in functions, operators, and variables\n\
is available in the on-line version of the manual.  Use the command\n\
`help -i <topic>' to search the manual index.\n\
\n\
Help and information about Octave is also available on the WWW\n\
at http://www.octave.org and via the help@octave.org\n\
mailing list.\n"; 
}

// XXX FIXME XXX -- this needs a major overhaul to cope with new
// symbol table stuff.

static void
display_names_from_help_list (std::ostream& os, help_list *list,
			      const char *desc)
{
  string_vector symbols = names (list);

  if (! symbols.empty ())
    {
      os << "\n*** " << desc << ":\n\n";

      symbols.qsort ();

      symbols.list_in_columns (os);
    }
}

static void
display_symtab_names (std::ostream& os, const string_vector& names,
		      const std::string& desc)
{
  if (! names.empty ())
    {
      os << "\n*** " << desc << ":\n\n";
      names.list_in_columns (os);
    }
}

#ifdef LIST_SYMBOLS
#undef LIST_SYMBOLS
#endif
#define LIST_SYMBOLS(type, msg) \
  do \
    { \
      string_vector names \
	= fbi_sym_tab->name_list (string_vector (), true, type); \
      display_symtab_names (octave_stdout, names, msg); \
    } \
  while (0)

static void
simple_help (void)
{
  octave_stdout << "Help is available for the topics listed below.\n";

  additional_help_message (octave_stdout);

  display_names_from_help_list (octave_stdout, operator_help (),
				"operators");

  display_names_from_help_list (octave_stdout, keyword_help (),
				"reserved words");

  // XXX FIXME XXX -- is this distinction needed?

  LIST_SYMBOLS (symbol_record::BUILTIN_CONSTANT, "built-in constants");

  LIST_SYMBOLS (symbol_record::BUILTIN_VARIABLE, "built-in variables");

  LIST_SYMBOLS (symbol_record::COMMAND, "commands");

  LIST_SYMBOLS (symbol_record::MAPPER_FUNCTION, "mapper functions");

  LIST_SYMBOLS (symbol_record::BUILTIN_FUNCTION, "general functions");

  // Also need to list variables and currently compiled functions from
  // the symbol table, if there are any.

  // Also need to search octave_path for script files.

  string_vector dirs = Vload_path_dir_path.all_directories ();

  int len = dirs.length ();

  for (int i = 0; i < len; i++)
    {
      string_vector names = octave_fcn_file_name_cache::list (dirs[i]);

      if (! names.empty ())
	{
	  std::string dir
	    = octave_env::make_absolute (dirs[i], octave_env::getcwd ());

	  octave_stdout << "\n*** function files in " << dir << ":\n\n";

	  names.qsort ();

	  names.list_in_columns (octave_stdout);
	}
    }
}

static int
try_info (const std::string& nm)
{
  int status = 0;

  OSSTREAM cmd_buf;

  cmd_buf << "\"" << Vinfo_prog << "\" --file \"" << Vinfo_file << "\"";

  std::string directory_name = Vinfo_file;
  size_t pos = directory_name.rfind ('/');

  if (pos != NPOS)
    {
      directory_name.resize (pos + 1);
      cmd_buf << " --directory \"" << directory_name << "\"";
    }

  if (nm.length () > 0)
    cmd_buf << " --index-search " << nm;

  cmd_buf << OSSTREAM_ENDS;

  volatile octave_interrupt_handler old_interrupt_handler
    = octave_ignore_interrupts ();

  status = system (OSSTREAM_C_STR (cmd_buf));

  OSSTREAM_FREEZE (cmd_buf);

  octave_set_interrupt_handler (old_interrupt_handler);

  if (WIFEXITED (status))
    status = WEXITSTATUS (status);
  else
    status = 127;

  return status;
}

static void
help_from_info (const string_vector& argv, int idx, int argc)
{
  if (idx == argc)
    try_info (std::string ());
  else
    {
      for (int i = idx; i < argc; i++)
	{
	  int status = try_info (argv[i]);

	  if (status)
	    {
	      if (status == 127)
		{
		  error ("help: unable to find info");
		  error ("help: you need info 2.18 or later (texinfo 3.12)");
		  break;
		}
	      else
		{
		  message ("help", "sorry, `%s' is not indexed in the manual",
			   argv[i].c_str ());
		}
	    }
	}
    }
}

static bool
looks_like_texinfo (const std::string& msg, size_t& p1)
{
  p1 = msg.find ('\n');

  std::string t = msg.substr (0, p1);

  if (p1 == NPOS)
    p1 = 0;

  size_t p2 = t.find ("-*- texinfo -*-");

  return (p2 != NPOS);
}

void
display_help_text (std::ostream& os, const std::string& msg)
{
  // Look for "-*- texinfo -*-" in first line of help message.  If it
  // is present, use makeinfo to format the rest of the message before
  // sending it to the output stream.  Otherwise, just print the
  // message.

  size_t pos;

  if (looks_like_texinfo (msg, pos))
    {
      std::string tmp_file_name = file_ops::tempnam ("", "");

      int cols = command_editor::terminal_cols ();

      if (cols > 16)
	cols--;

      if (cols > 64)
	cols -= 7;

      if (cols > 80)
	cols = 72;

      OSSTREAM buf;

      buf << "sed -e 's/^[#%][#%]* *//' -e 's/^ *@/@/' | "
	  << "\"" << Vmakeinfo_prog << "\""
	  << " -D \"VERSION " << OCTAVE_VERSION << "\""
	  << " -D \"OCTAVEHOME " << OCTAVE_PREFIX << "\""
	  << " -D \"TARGETHOSTTYPE " << OCTAVE_CANONICAL_HOST_TYPE << "\""
	  << " --fill-column " << cols
	  << " --no-warn"
	  << " --no-validate"
	  << " --no-headers"
	  << " --force"
	  << " --output \"" << tmp_file_name << "\""
	  << " > /dev/null 2>&1"
	  << OSSTREAM_ENDS;

      oprocstream filter (OSSTREAM_STR (buf));

      OSSTREAM_FREEZE (buf);

      if (filter && filter.is_open ())
	{
	  filter << "@macro seealso {args}\n"
		 << "\n"
		 << "@noindent\n"
		 << "See also: \\args\\.\n"
                 << "@end macro\n";

	  filter << msg.substr (pos+1) << std::endl;

	  int status = filter.close ();

	  std::ifstream tmp_file (tmp_file_name.c_str ());

	  if (WIFEXITED (status) && WEXITSTATUS (status) == 0)
	    {
	      int c;
	      while ((c = tmp_file.get ()) != EOF)
		os << (char) c;

	      tmp_file.close ();
	    }
	  else
	    {
	      warning ("help: Texinfo formatting filter exited abnormally");
	      warning ("help: raw Texinfo source of help text follows...");

	      os << "\n" << msg;
	    }

	  file_ops::unlink (tmp_file_name);
	}
      else
	os << msg;
    }
  else
    os << msg;
}

static bool
help_from_list (std::ostream& os, const help_list *list,
		const std::string& nm, int usage)
{
  const char *name;

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

	  display_help_text (os, list->help);

	  os << "\n";

	  return true;
	}
      list++;
    }

  return false;
}

static bool
help_from_symbol_table (std::ostream& os, const std::string& nm)
{
  bool retval = false;

  symbol_record *sym_rec = lookup_by_name (nm, 0);

  if (sym_rec && sym_rec->is_defined ())
    {
      std::string h = sym_rec->help ();

      if (h.length () > 0)
	{
	  sym_rec->which (os);
	  os << "\n";
	  display_help_text (os, h);
	  os << "\n";
	  retval = true;
	}
    }

  return retval;
}

static bool
help_from_file (std::ostream& os, const std::string& nm)
{
  bool retval = false;

  std::string path = fcn_file_in_path (nm);

  std::string h = get_help_from_file (path);

  if (! h.empty ())
    {
      os << nm << " is the file: " << path << "\n\n";
      display_help_text (os, h);
      os << "\n";
      retval = true;
    }

  return retval;
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

      if (help_from_symbol_table (octave_stdout, argv[i]))
	continue;

      if (help_from_file (octave_stdout, argv[i]))
	continue;

      octave_stdout << "\nhelp: sorry, `" << argv[i]
		    << "' is not documented\n"; 
    }

  additional_help_message (octave_stdout);
}

DEFCMD (help, args, ,
  "-*- texinfo -*-\n\
@deffn {Command} help\n\
Octave's @code{help} command can be used to print brief usage-style\n\
messages, or to display information directly from an on-line version of\n\
the printed manual, using the GNU Info browser.  If invoked without any\n\
arguments, @code{help} prints a list of all the available operators,\n\
functions, and built-in variables.  If the first argument is @code{-i},\n\
the @code{help} command searches the index of the on-line version of\n\
this manual for the given topics.\n\
\n\
For example, the command @kbd{help help} prints a short message\n\
describing the @code{help} command, and @kbd{help -i help} starts the\n\
GNU Info browser at this node in the on-line version of the manual.\n\
\n\
Once the GNU Info browser is running, help for using it is available\n\
using the command @kbd{C-h}.\n\
@end deffn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("help");

  if (error_state)
    return retval;

  if (argc == 1)
    simple_help ();
  else
    {
      if (argv[1] == "-i")
	help_from_info (argv, 2, argc);
      else
	builtin_help (argc, argv);
    }

  return retval;
}

static void
do_type (std::ostream& os, const std::string& name, bool pr_type_info,
	 bool quiet, bool pr_orig_txt)
{
  symbol_record *sym_rec = lookup_by_name (name, 0);

  if (sym_rec && sym_rec->is_defined ())
    sym_rec->type (os, pr_type_info, quiet, pr_orig_txt);
  else
    {
      std::string ff = fcn_file_in_path (name);

      if (! ff.empty ())
	{
	  std::ifstream fs (ff.c_str (), std::ios::in);

	  if (fs)
	    {
	      if (pr_type_info && ! quiet)
		os << name << " is the script file: " << ff << "\n\n";

	      char ch;

	      while (fs.get (ch))
		os << ch;
	    }
	  else
	    os << "unable to open `" << ff << "' for reading!\n";
	}
      else
	error ("type: `%s' undefined", name.c_str ());
    }
}

DEFCMD (type, args, nargout,
  "-*- texinfo -*-\n\
\n\
@deffn {Command} type options name @dots{}\n\
Display the definition of each @var{name} that refers to a function.\n\
\n\
Normally also displays if each @var{name} is user-defined or builtin;\n\
the @code{-q} option suppresses this behaviour.\n\
\n\
Currently, Octave can only display functions that can be compiled\n\
cleanly, because it uses its internal representation of the function to\n\
recreate the program text.\n\
\n\
Comments are not displayed because Octave's parser currently discards\n\
them as it converts the text of a function file to its internal\n\
representation.  This problem may be fixed in a future release.\n\
@end deffn")
{
  octave_value retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("type");

  if (! error_state)
    {
      if (argc > 1)
	{
	  // XXX FIXME XXX -- we should really use getopt ()

	  bool quiet = false;
	  bool pr_orig_txt = true;

	  int idx;

	  for (idx = 1; idx < argc; idx++)
	    {
	      if (argv[idx] == "-q" || argv[idx] == "-quiet")
		quiet = true;
	      else if (argv[idx] == "-t" || argv[idx] == "-transformed")
		pr_orig_txt = false;
	      else
		break;
	    }

	  if (idx < argc)
	    {
	      OSSTREAM output_buf;

	      for (int i = idx; i < argc; i++)
		{
		  std::string id = argv[i];

		  if (nargout == 0)
		    do_type (octave_stdout, id, true, quiet, pr_orig_txt);
		  else
		    do_type (output_buf, id, false, quiet, pr_orig_txt);

		  if (error_state)
		    goto abort;
		}

	      if (nargout != 0)
		{
		  output_buf << OSSTREAM_ENDS;

		  retval = OSSTREAM_STR (output_buf);

		  OSSTREAM_FREEZE (output_buf);
		}
	    }
	  else
	    print_usage ("type");
	}
      else
	print_usage ("type");
    }

 abort:

  return retval;
}

static std::string
do_which (const std::string& name)
{
  std::string retval;

  symbol_record *sym_rec = lookup_by_name (name, 0);

  if (sym_rec && sym_rec->is_defined ())
    retval = sym_rec->which ();
  else
    {
      std::string path = fcn_file_in_path (name);

      if (! path.empty ())
	retval = path;
      else
	retval = "undefined";
    }

  return retval;
}

static void
do_which (std::ostream& os, const std::string& name)
{
  symbol_record *sym_rec = lookup_by_name (name, 0);

  if (sym_rec && sym_rec->is_defined ())
    sym_rec->which (os);
  else
    {
      std::string path = fcn_file_in_path (name);

      if (! path.empty ())
	os << "which: `" << name << "' is the script file\n"
	   << path << "\n";
      else
	os << "which: `" << name << "' is undefined\n";
    }
}

DEFCMD (which, args, nargout,
  "-*- texinfo -*-\n\
@deffn {Command} which name @dots{}\n\
Display the type of each @var{name}.  If @var{name} is defined from a\n\
function file, the full name of the file is also displayed.\n\
@end deffn")
{
  octave_value_list retval;

  string_vector argv = args.make_argv ("which");

  if (! error_state)
    {
      int argc = argv.length ();

      if (nargout > 0)
	retval.resize (argc-1, Matrix ());

      if (argc > 1)
	{
	  for (int i = 1; i < argc; i++)
	    {
	      std::string id = argv[i];

	      if (nargout == 0)
		do_which (octave_stdout, id);
	      else
		retval(i-1) = do_which (id);
	    }
	}
      else
	print_usage (argv[0]);
    }

  return retval;
}

static int
info_file (void)
{
  int status = 0;

  std::string s = builtin_string_variable ("INFO_FILE");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("INFO_FILE");
      status = -1;
    }
  else
    Vinfo_file = s;

  return status;
}

static int
info_prog (void)
{
  int status = 0;

  std::string s = builtin_string_variable ("INFO_PROGRAM");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("INFO_PROGRAM");
      status = -1;
    }
  else
    Vinfo_prog = s;

  return status;
}

static int
makeinfo_prog (void)
{
  int status = 0;

  std::string s = builtin_string_variable ("MAKEINFO_PROGRAM");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("MAKEINFO_PROGRAM");
      status = -1;
    }
  else
    Vmakeinfo_prog = s;

  return status;
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
  DEFVAR (INFO_FILE, Vinfo_file, info_file,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} INFO_FILE\n\
The variable @code{INFO_FILE} names the location of the Octave info file.\n\
The default value is @code{\"@var{octave-home}/info/octave.info\"}, in\n\
which @var{octave-home} is the directory where all of Octave is installed.\n\
@end defvr");

  DEFVAR (INFO_PROGRAM, Vinfo_prog, info_prog,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} INFO_PROGRAM\n\
The variable @code{INFO_PROGRAM} names the info program to run.  Its\n\
default initial value is\n\
@code{\"@var{octave-home}/libexec/octave/@var{version}/exec/@var{arch}/info\"}\n\
in which @var{octave-home} is the directory where all of Octave is\n\
installed, @var{version} is the Octave version number, and @var{arch}\n\
is the system type (for example, @code{i686-pc-linux-gnu}).  The\n\
default initial value may be overridden by the environment variable\n\
@code{OCTAVE_INFO_PROGRAM}, or the command line argument\n\
@code{--info-program NAME}, or by setting the value of\n\
@code{INFO_PROGRAM} in a startup script\n\
@end defvr");

  DEFVAR (MAKEINFO_PROGRAM, Vmakeinfo_prog, makeinfo_prog,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} MAKEINFO_PROGRAM\n\
The variable @code{MAKEINFO_PROGRAM} names the makeinfo program that\n\
Octave runs to format help text that contains Texinfo markup commands.\n\
Its default initial value is @code{\"makeinfo\"}.\n\
@end defvr");

  DEFVAR (suppress_verbose_help_message, false, suppress_verbose_help_message,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} suppress_verbose_help_message\n\
If the value of @code{suppress_verbose_help_message} is nonzero, Octave\n\
will not add additional help information to the end of the output from\n\
the @code{help} command and usage messages for built-in commands.\n\
@end defvr");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
