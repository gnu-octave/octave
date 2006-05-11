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

#include <algorithm>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>

#ifdef HAVE_UNISTD_H
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <unistd.h>
#endif

#include "cmd-edit.h"
#include "file-ops.h"
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
#include "quit.h"

// Name of the info file specified on command line.
// (--info-file file)
std::string Vinfo_file;

// Name of the info reader we'd like to use.
// (--info-program program)
std::string Vinfo_program;

// Name of the makeinfo program to run.
static std::string Vmakeinfo_program = "makeinfo";

// If TRUE, don't print additional help message in help and usage
// functions.
static bool Vsuppress_verbose_help_message = false;

// FIXME -- maybe this should use string instead of char*.

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
    "Increment operator.  As in C, may be applied as a prefix or postfix\n\
operator.", },

  { ",",
    "Array index, function argument, or command separator.", },

  { "-",
    "Subtraction or unary negation operator.", },

  { "--",
    "Decrement operator.  As in C, may be applied as a prefix or postfix\n\
operator.", },

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

  { "if",
    "-*- texinfo -*-\n\
@deffn Keyword if (@var{cond}) @dots{} endif\n\
@deffnx Keyword if (@var{cond}) @dots{} else @dots{} endif\n\
@deffnx Keyword if (@var{cond}) @dots{} elseif (@var{cond}) @dots{} endif\n\
@deffnx Keyword if (@var{cond}) @dots{} elseif (@var{cond}) @dots{} else @dots{} endif\n\
Begin an if block.\n\
@seealso{switch}\n\
@end deffn", },

  { "otherwise",
    "The default statement in a switch block.", },

  { "persistent",
    "Declare variables as persistent.  A variable that has been declared\n\
persistent within a function will retain its contents in memory between\n\
subsequent calls to the same function.  The difference between persistent\n\
variables and global variables is that persistent variables are local in scope\n\
to a particular function and are not visible elsewhere.", },

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
    "Pass an arbitrary number of arguments into a function.  See also\n\
varargout, nargin, and nargout.",},

  { "varargout",
    "Pass an arbitrary number of arguments out of a function.  See also\n\
varargin, nargin, and nargout.",},

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

// It's not likely that this does the right thing now.  FIXME

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

  string_vector afl = autoloaded_functions ();
  int afl_len = afl.length ();

  int total_len = key_len + fbi_len + glb_len + top_len + lcl_len + ffl_len + afl_len;

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

  for (i = 0; i < afl_len; i++)
    list[j++] = afl[i];

  return list;
}

void
additional_help_message (std::ostream& os)
{
  if (! Vsuppress_verbose_help_message)
    os << "\
Additional help for built-in functions and operators is\n\
available in the on-line version of the manual.  Use the command\n\
`doc <topic>' to search the manual index.\n\
\n\
Help and information about Octave is also available on the WWW\n\
at http://www.octave.org and via the help@octave.org\n\
mailing list.\n"; 
}

// FIXME -- this needs a major overhaul to cope with new
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

  // FIXME -- is this distinction needed?

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

  string_vector autoloaded = autoloaded_functions ();

  if (! autoloaded.empty ())
    {
      octave_stdout << "\n*** autoloaded functions:\n\n";

      autoloaded.qsort ();

      autoloaded.list_in_columns (octave_stdout);
    }
}

static int
try_info (const std::string& nm)
{
  int retval = -1;

  warning ("please use `doc' instead of `help -i'");

  octave_value_list args;
  args(0) = nm;
  octave_value_list result = feval ("doc", args, 1);

  if (result.length () > 0)
    retval = result(0).int_value ();

  return retval;
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

	  if (status == 127)
	    break;
	  else if (status != 0)
	    message ("help", "`%s' is not indexed in the manual",
		     argv[i].c_str ());
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
      os.flush ();

      std::string tmp_file_name = file_ops::tempnam ("", "");

      int cols = command_editor::terminal_cols ();

      if (cols > 16)
	cols--;

      if (cols > 64)
	cols -= 7;

      if (cols > 80)
	cols = 72;

      std::ostringstream buf;

      buf << "sed -e 's/^[#%][#%]* *//' -e 's/^ *@/@/' | "
	  << "\"" << Vmakeinfo_program << "\""
	  << " -D \"VERSION " << OCTAVE_VERSION << "\""
	  << " -D \"OCTAVEHOME " << OCTAVE_PREFIX << "\""
	  << " -D \"TARGETHOSTTYPE " << OCTAVE_CANONICAL_HOST_TYPE << "\""
	  << " --fill-column " << cols
	  << " --no-warn"
	  << " --no-validate"
	  << " --no-headers"
	  << " --force"
	  << " --output \"" << tmp_file_name << "\"";

      oprocstream filter (buf.str ());

      if (filter && filter.is_open ())
	{
	  filter << "@macro seealso {args}\n"
		 << "@sp 1\n"
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
	      warning ("help:\n\n%s\n\n", msg.c_str ());
	    }

	  file_ops::unlink (tmp_file_name);
	}
      else
	os << msg;
    }
  else
    os << msg;
}

void
display_usage_text (std::ostream& os, const std::string& msg)
{
  std::string filtered_msg = msg;

  size_t pos;

  if (looks_like_texinfo (msg, pos))
    {
      std::ostringstream buf;

      buf << "-*- texinfo -*-\n";

      bool found_def = false;

      size_t msg_len = msg.length ();

      while (pos < msg_len)
	{
	  size_t new_pos = msg.find_first_of ('\n', pos);

	  if (new_pos == NPOS)
	    new_pos = msg_len-1;

	  std::string line = msg.substr (pos, new_pos-pos+1);

	  if (line.substr (0, 4) == "@def"
	      || line.substr (0, 8) == "@end def")
	    {
	      found_def = true;
	      buf << line;
	    }

	  pos = new_pos + 1;
	}

      if (found_def)
	filtered_msg = buf.str ();
    }

  display_help_text (os, filtered_msg);
}

static bool
help_from_list (std::ostream& os, const help_list *list,
		const std::string& nm, int usage, bool& symbol_found)
{
  bool retval = false;

  const char *name;

  while ((name = list->name) != 0)
    {
      if (strcmp (name, nm.c_str ()) == 0)
	{
	  symbol_found = true;

	  std::string h = list->help;

	  if (h.length () > 0)
	    {
	      if (usage)
		os << "\nusage: ";
	      else
		os << "\n*** " << nm << ":\n\n";

	      display_help_text (os, h);

	      os << "\n";

	      retval = true;
	    }
	  break;
	}
      list++;
    }

  return retval;;
}

std::string
extract_help_from_dispatch (const std::string& nm)
{
  std::string retval;

  symbol_record *builtin = fbi_sym_tab->lookup ("builtin:" + nm, 0);

  if (builtin)
    {
      // Check that builtin is up to date.
 
      // Don't try to fight octave's function name handling
      // mechanism.  Instead, move dispatch record out of the way,
      // and restore the builtin to its original name.
      symbol_record *dispatch = fbi_sym_tab->lookup (nm, 0);

      if (dispatch)
	{
	  dispatch->unprotect ();

	  fbi_sym_tab->rename (nm, "dispatch:" + nm);
	  fbi_sym_tab->rename ("builtin:" + nm, nm);

	  // Check for updates to builtin function; ignore errors
	  // that appear (they interfere with renaming), and remove
	  // the updated name from the current symbol table.  FIXME --
	  // check that updating a function updates it in all
	  // contexts.  It may be that it is updated only in the 
	  // current symbol table, and not the caller.  I believe this
	  // won't be a problem because the caller will go through the
	  // same logic and end up with the newer version.

	  octave_function *f = is_valid_function (nm);

	  if (f)
	    retval = builtin->help ();

	  curr_sym_tab->clear_function (nm);

	  // Move the builtin function out of the way and restore the
	  // dispatch fuction.  FIXME what if builtin wants to
	  // protect itself?

	  fbi_sym_tab->rename (nm, "builtin:" + nm);
	  fbi_sym_tab->rename ("dispatch:" + nm, nm);

	  dispatch->protect ();
	}
      else
	error ("failed to find dispatch record for `builtin:%s'", nm.c_str ());
    }

  return retval;
}

static bool
help_from_symbol_table (std::ostream& os, const std::string& nm,
			bool& symbol_found)
{
  bool retval = false;

  symbol_record *sym_rec = lookup_by_name (nm, 0);

  if (sym_rec && sym_rec->is_defined ())
    {
      symbol_found = true;

      std::string h = sym_rec->help ();

      if (h.length () > 0)
	{
	  h = extract_help_from_dispatch (nm) + h;
	  display_help_text (os, h);
	  if (! Vsuppress_verbose_help_message)
	    {
	      sym_rec->which (os);
	      os << "\n";
	    }
	  os << "\n";
	  retval = true;
	}
    }

  return retval;
}

static bool
help_from_file (std::ostream& os, const std::string& nm, bool& symbol_found)
{
  bool retval = false;

  std::string h = get_help_from_file (nm, symbol_found, true);

  if (h.length () > 0)
    {
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
      bool symbol_found = false;

      if (help_from_list (octave_stdout, op_help_list, argv[i], 0,
			  symbol_found))
	continue;

      if (help_from_list (octave_stdout, kw_help_list, argv[i], 0,
			  symbol_found))
	continue;

      if (help_from_symbol_table (octave_stdout, argv[i], symbol_found))
	continue;

      if (help_from_file (octave_stdout, argv[i], symbol_found))
	continue;

      if (symbol_found)
	octave_stdout << "\nhelp: `" << argv[i]
		      << "' is not documented\n"; 
      else
	octave_stdout << "\nhelp: `" << argv[i]
		      << "' not found\n"; 
    }

  additional_help_message (octave_stdout);
}

DEFCMD (help, args, ,
  "-*- texinfo -*-\n\
@deffn {Command} help\n\
Octave's @code{help} command can be used to print brief usage-style\n\
messages, or to display information directly from an on-line version of\n\
the printed manual, using the GNU Info browser.  If invoked without any\n\
arguments, @code{help} prints a list of all the available operators\n\
and functions.  If the first argument is @code{-i}, the @code{help}\n\
command searches the index of the on-line version of this manual for\n\
the given topics.\n\
\n\
For example, the command @kbd{help help} prints a short message\n\
describing the @code{help} command, and @kbd{help -i help} starts the\n\
GNU Info browser at this node in the on-line version of the manual.\n\
\n\
Once the GNU Info browser is running, help for using it is available\n\
using the command @kbd{C-h}.\n\
@seealso{doc, which, lookfor}\n\
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
	  // FIXME -- we should really use getopt ()

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
	      std::ostringstream output_buf;

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
		retval = output_buf.str ();
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
@seealso{help, lookfor}\n\
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

// FIXME 
// This function attempts to find the first sentence of a help string, though
// given that the user can create the help in an arbitrary format, your
// success might vary.. it works much better with help string formated in
// texinfo. Using regex might make this function much simpler.

std::string 
first_help_sentence (const std::string& h, bool short_sentence = true)
{
  std::string retval;

  size_t pos = 0;

  if (looks_like_texinfo (h, pos))
    { 
     // Get the parsed help string.
      pos = 0;
      std::ostringstream os;
      display_help_text (os, h);
      std::string h2 = os.str ();

      while (1)
	{
	  // Skip leading whitespace and get new line
	  pos = h2.find_first_not_of ("\n\t ", pos);

	  if (pos == NPOS)
	    break;

	  size_t new_pos = h2.find_first_of ('\n', pos);
	  std::string line = h2.substr (pos, new_pos-pos);

	  // Skip lines starting in "-"
	  if (line.find_first_of ('-') == 0)
	    {
	      pos = new_pos + 1;
	      continue;
	    }

	  break;
	}

      if (pos == NPOS)
	return retval;

      // At start of real text. Get first line with the sentence
      size_t new_pos = h2.find_first_of ('\n', pos);
      std::string line = h2.substr (pos, new_pos-pos);
      size_t dot_pos;

      while ((dot_pos = line.find_first_of ('.')) == NPOS)
	{
	  // Trim trailing blanks on line
	  line.substr (0, line.find_last_not_of ("\n\t ") + 1);

	  // Append next line
	  size_t tmp_pos = h2.find_first_not_of ("\n\t ", new_pos + 1);
	  if (tmp_pos == NPOS || h2.substr (tmp_pos, 1) == "\n")
	    break;

	  new_pos = h2.find_first_of ('\n', tmp_pos);
	  std::string next = h2.substr (tmp_pos, new_pos-tmp_pos);

	  if (short_sentence)
	    {
	      if ((tmp_pos = next.find_first_of ('.')) != NPOS)
		{
		  line = line + " " + next;
		  dot_pos = line.find_first_of ('.');
		}
	      break;
	    }
	  else
	    line = line + " " + next;
	}

      if (dot_pos == NPOS)
	retval = line;
      else
	retval = line.substr (0, dot_pos + 1);
    }
  else
    {
      std::string _upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
      std::string _lower = "abcdefghijklmnopqrstuvwxyz";
      std::string _alpha = _upper + _lower + "_";
      std::string _alphanum = _alpha + "1234567890";
      pos = 0;

      while (1)
	{
	  // Skip leading whitespace and get new line
	  pos = h.find_first_not_of ("\n\t ", pos);

	  if (pos == NPOS)
	    break;

	  size_t new_pos = h.find_first_of ('\n', pos);
	  std::string line = h.substr (pos, new_pos-pos);

	  // Make a lower case copy to simplify some tests
	  std::string lower = line;
	  std::transform (lower.begin (), lower.end (), lower.begin (), tolower);

	  // Skip lines starting in "-" or "Usage"
	  if (lower.find_first_of ('-') == 0
	      || lower.substr (0, 5) == "usage")
	    {
	      pos = (new_pos == NPOS ? NPOS : new_pos + 1);
	      continue;
	    }

	  size_t line_pos = 0;
	  size_t tmp_pos = 0;

	  // chop " blah : "
	  tmp_pos = line.find_first_not_of ("\t ", line.find_first_not_of 
					     (_alphanum, line_pos));
	  if (tmp_pos != NPOS && line.substr (tmp_pos, 1) == ":")
	    line_pos = line.find_first_not_of ("\t ", tmp_pos + 1);

	  if (line_pos == NPOS)
	    {
	      pos = (new_pos == NPOS ? NPOS : new_pos + 1);
	      continue;
	    }

	  // chop " function "
	  if (lower.substr (line_pos, 8) == "function")
	    line_pos =  line.find_first_not_of ("\t ", line_pos + 8);
	  
	  if (line_pos == NPOS)
	    {
	      pos = (new_pos == NPOS ? NPOS : new_pos + 1);
	      continue;
	    }

	  // chop " [a,b] = "
	  if (line.substr (line_pos, 1) == "[")
	    {
	      tmp_pos = line.find_first_not_of 
		("\t ", line.find_first_of ("]", line_pos) + 1);

	      if (tmp_pos != NPOS && line.substr (tmp_pos, 1) == "=")
		line_pos = line.find_first_not_of ("\t ",tmp_pos + 1);
	    }

	  if (line_pos == NPOS)
	    {
	      pos = (new_pos == NPOS ? NPOS : new_pos + 1);
	      continue;
	    }

	  // chop " a = "
	  if (line.find_first_not_of (_alpha, line_pos) != line_pos)
	    {
	      tmp_pos = line.find_first_not_of ("\t ", line.find_first_not_of 
						(_alphanum, line_pos));
	      if (tmp_pos != NPOS && line.substr (tmp_pos, 1) == "=")
		line_pos = line.find_first_not_of ("\t ", tmp_pos + 1);
	    }

	  if (line_pos == NPOS)
	    {
	      pos = new_pos + 1;
	      continue;
	    }

	  // chop " f(x) "
	  if (line.find_first_not_of (_alpha, line_pos) != line_pos)
	    {
	      tmp_pos = line.find_first_not_of ("\t ", line.find_first_not_of 
						(_alphanum, line_pos));
	      if (tmp_pos != NPOS && line.substr (tmp_pos, 1) == "(")
		line_pos = line.find_first_not_of ("\t ", line.find_first_of 
						   (")", tmp_pos) + 1);
	    }

	  if (line_pos == NPOS)
	    {
	      pos = (new_pos == NPOS ? NPOS : new_pos + 1);
	      continue;
	    }

	  // chop " ; "
	  if (line.substr (line_pos, 1) == ":"
	      || line.substr (line_pos, 1) == ";")
	    line_pos = line.find_first_not_of ("\t ", line_pos + 1);

	  if (line_pos == NPOS)
	    {
	      pos = (new_pos == NPOS ? NPOS : new_pos + 1);
	      continue;
	    }

	  // chop " BLAH "
	  if (line.length () > line_pos + 2
	      && line.find_first_of (_upper, line_pos) == line_pos
	      && line.find_first_of (_upper, line_pos+1) == line_pos + 1)
	    line_pos = line.find_first_not_of ("\t ", line.find_first_not_of 
			(_upper + "0123456789_", line_pos));

	  if (line_pos == NPOS)
	    {
	      pos = (new_pos == NPOS ? NPOS : new_pos + 1);
	      continue;
	    }

	  // chop " blah --- "
	  tmp_pos = line.find_first_not_of ("\t ", line.find_first_not_of 
					     (_alphanum, line_pos));
	  if (tmp_pos != NPOS && line.substr (tmp_pos, 1) == "-")
	    {
	      tmp_pos = line.find_first_not_of ("-", tmp_pos);
	      if (line.substr (tmp_pos, 1) == " "
		  || line.substr (tmp_pos, 1) == "\t")
		line_pos = line.find_first_not_of ("\t ", tmp_pos);
	    }

	  if (line_pos == NPOS)
	    {
	      pos = (new_pos == NPOS ? NPOS : new_pos + 1);
	      continue;
	    }

	  // chop " blah <TAB> "
	  if (line.find_first_not_of (_alpha, line_pos) != line_pos)
	    {
	      tmp_pos = line.find_first_not_of (" ", line.find_first_not_of 
						(_alphanum, line_pos));
	      if (tmp_pos != NPOS && line.substr (tmp_pos, 1) == "\t")
		line_pos = line.find_first_not_of ("\t ", line.find_first_of 
						   (")", tmp_pos) + 1);
	    }

	  if (line_pos == NPOS)
	    {
	      pos = (new_pos == NPOS ? NPOS : new_pos + 1);
	      continue;
	    }

	  // chop " blah  "
	  if (line.find_first_not_of (_alpha, line_pos) != line_pos)
	    {
	      tmp_pos = line.find_first_not_of (_alphanum, line_pos);

	      if (tmp_pos != NPOS
		  && (line.substr (tmp_pos, 2) == "\t\t"
		      || line.substr (tmp_pos, 2) == "\t "
		      || line.substr (tmp_pos, 2) == " \t"
		      || line.substr (tmp_pos, 2) == " "))
		line_pos = line.find_first_not_of ("\t ", tmp_pos);
	    }

	  if (line_pos == NPOS)
	    {
	      pos = (new_pos == NPOS ? NPOS : new_pos + 1);
	      continue;
	    }

	  // skip blah \n or \n blah
	  // skip blank line
	  // skip "# !/usr/bin/octave"
	  if ((line.substr (line_pos , 2) == "or"
	       && line.find_first_not_of ("\n\t ", line_pos + 2) == NPOS)
	      || line.find_first_not_of ("\n\t ", line_pos) == NPOS
	      || line.substr (line_pos, 2) == "!/")
	    {
	      pos = (new_pos == NPOS ? NPOS : new_pos + 1);
	      continue;
	    }

	  // Got the start of first sentence, break.
	  pos = pos + line_pos;
	  break;
	}

      if (pos == NPOS)
	return retval;

      // At start of real text. Get first line with the sentence
      size_t new_pos = h.find_first_of ('\n', pos);
      std::string line = h.substr (pos, new_pos-pos);
      size_t dot_pos;

      while ((dot_pos = line.find_first_of ('.')) == NPOS)
	{
	  // Trim trailing blanks on line
	  line = line.substr (0, line.find_last_not_of ("\n\t ") + 1);

	  // Append next line
	  size_t tmp_pos = h.find_first_not_of ("\t ", new_pos + 1);
	  if (tmp_pos == NPOS || h.substr (tmp_pos, 1) == "\n")
	    break;

	  new_pos = h.find_first_of ('\n', tmp_pos);
	  std::string next = h.substr (tmp_pos, new_pos-tmp_pos);

	  if (short_sentence)
	    {
	      // Only add the next line if it terminates the sentence, then break
	      if ((tmp_pos = next.find_first_of ('.')) != NPOS)
		{
		  line = line + " " + next;
		  dot_pos = line.find_first_of ('.');
		}
	      break;
	    }
	  else
	    line = line + " " + next;
	}

      if (dot_pos == NPOS)
	retval = line;
      else
	retval = line.substr (0, dot_pos + 1);
    }

  return retval;
}

static void
print_lookfor (const std::string& name, const std::string& line)
{
  const size_t deflen = 20;

  size_t max_width = command_editor::terminal_cols () - deflen;
  if (max_width < deflen)
    max_width = deflen;

  size_t name_len = name.length ();

  size_t width = max_width;
  if (name_len > deflen)
    {
      width = command_editor::terminal_cols () - name_len;
      if (width < deflen)
	width = deflen;
    }

  size_t pad_len = deflen > name_len ? deflen - name_len + 1 : 1;
  octave_stdout << name << std::string (pad_len, ' ');

  size_t pos = 0;

  while (1)
    {
      size_t new_pos = line.find_first_of ("\n\t ", pos);
      size_t end_pos = new_pos;

      if (line.length () - pos < width)
	new_pos = end_pos = NPOS;
      else
	while (new_pos != NPOS && new_pos - pos < width)
	  {
	    end_pos = new_pos;
	    new_pos = line.find_first_of ("\n\t ", new_pos + 1);
	  }

      octave_stdout << line.substr (pos, end_pos-pos) << std::endl;
		  
      if (end_pos == NPOS)
	break;

      pos = end_pos + 1;
      width = max_width;
      octave_stdout << std::string (deflen + 1, ' ');
    }
}

DEFCMD (lookfor, args, nargout, 
  "-*- texinfo -*-\n\
@deffn {Command} lookfor @var{str}\n\
@deffnx {Command} lookfor -all @var{str}\n\
@deffnx {Function} {[@var{fun}, @var{helpstring}] = } lookfor (@var{str})\n\
@deffnx {Function} {[@var{fun}, @var{helpstring}] = } lookfor ('-all', @var{str})\n\
Search for the string @var{str} in all of the functions found in the\n\
function search path.  By default @code{lookfor} searchs for @var{str}\n\
in the first sentence of the help string of each function found. The entire\n\
help string of each function found in the path can be search if\n\
the '-all' argument is supplied. All searches are case insensitive.\n\
\n\
Called with no output arguments, @code{lookfor} prints the list of matching\n\
functions to the terminal. Otherwise the output arguments @var{fun} and\n\
@var{helpstring} define the matching functions and the first sentence of\n\
each of their help strings.\n\
\n\
Note that the ability of @code{lookfor} to correctly identify the first\n\
sentence of the help of the functions is dependent on the format of the\n\
functions help. All of the functions in octave itself will correctly\n\
find the first sentence, but the same can not be guaranteed for other\n\
functions. Therefore the use of the '-all' argument might be necessary\n\
to find related functions that are not part of octave.\n\
@seealso{help, which}\n\
@end deffn")
{
  octave_value_list retval;
  int nargin = args.length ();
  bool first_sentence_only = true;

  if (nargin != 1 && nargin != 2)
    {
      usage ("lookfor");
      return retval;
    }

  string_vector ret[2];

  std::string txt;

  if (args(0).is_string ())
    {
      txt = args(0).string_value ();

      if (nargin == 2)
	{
	  if (args(1).is_string ())
	    {
	      std::string tmp = args(1).string_value ();

	      if (txt.substr(0,1) == "-")
		{
		  txt = tmp;
		  tmp = args(0).string_value ();
		}

	      if (tmp == "-all")
		first_sentence_only = false;
	      else
		error ("lookfor: unrecognized option argument");
	    }
	  else
	    error ("lookfor: arguments must be a string");
	}
    }
  else
    error ("lookfor: argument must be a string");

  if (!error_state)
    {
      // All tests in lower case
      std::transform (txt.begin (), txt.end (), txt.begin (), tolower);

      help_list *ptr = keyword_help ();
      while (ptr->name)
	{
	  std::string name = ptr->name;
	  std::string h = ptr->help;

	  if (name.find (txt) != NPOS)
	    {
	      if (nargout)
		{
		  ret[0].append (name);
		  ret[1].append (first_help_sentence (h));
		}
	      else
		print_lookfor (name, first_help_sentence (h));
	    }
	  else
	    {
	      std::string s;

	      if (first_sentence_only)
		s = first_help_sentence (h);
	      else
		s = h;
	      
	      std::transform (s.begin (), s.end (), s.begin (), tolower);

	      if (s.length () > 0 && s.find (txt) != NPOS)
		{
		  if (nargout)
		    {
		      ret[0].append (name);
		      ret[1].append (first_help_sentence (h));
		    }
		  else
		    print_lookfor (name, first_help_sentence (h));
		}
	    }

	  OCTAVE_QUIT;

	  ptr++;
	}

      ptr = operator_help ();
      while (ptr->name)
	{
	  std::string name = ptr->name;
	  std::string h = ptr->help;

	  if (name.find (txt) != NPOS)
	    {
	      if (nargout)
		{
		  ret[0].append (name);
		  ret[1].append (first_help_sentence (h));
		}
	      else
		print_lookfor (name, first_help_sentence (h));
	    }
	  else
	    {
	      std::string s;
	      if (first_sentence_only)
		s = first_help_sentence (h);
	      else
		s = h;
	      
	      std::transform (s.begin (), s.end (), s.begin (), tolower);

	      if (s.length () > 0 && s.find (txt) != NPOS)
		{
		  if (nargout)
		    {
		      ret[0].append (name);
		      ret[1].append (first_help_sentence (h));
		    }
		  else
		    print_lookfor (name, first_help_sentence (h));
		}
	    }

	  OCTAVE_QUIT;

	  ptr++;
	}

      // Check the symbol record table
      string_vector names
	= fbi_sym_tab->name_list (string_vector (), true);

      for (octave_idx_type i = 0; i < names.length (); i++)
	{
	  std::string name = names (i);

	  OCTAVE_QUIT;

	  symbol_record *sr = lookup_by_name (name, 0);
	  if (sr && sr->is_defined ()
	      && sr->type_name () != "overloaded function")
	    {
	      std::string h = sr->help ();

	      if (name.find (txt) != NPOS)
		{
		  if (nargout)
		    {
		      ret[0].append (name);
		      ret[1].append (first_help_sentence (h));
		    }
		  else
		    print_lookfor (name, first_help_sentence (h));
		}
	      else
		{
		  std::string s;

		  if (first_sentence_only)
		    s = first_help_sentence (h);
		  else
		    s = h;
	      
		  std::transform (s.begin (), s.end (), s.begin (), tolower);

		  if (s.length () > 0 && s.find (txt) != NPOS)
		    {
		      if (nargout)
			{
			  ret[0].append (name);
			  ret[1].append (first_help_sentence (h));
			}
		      else
			print_lookfor (name, first_help_sentence (h));
		    }
		}
	    }
	}

      string_vector dirs = Vload_path_dir_path.all_directories ();

      int len = dirs.length ();

      for (int i = 0; i < len; i++)
	{
	  names = octave_fcn_file_name_cache::list (dirs[i]);

	  if (! names.empty ())
	    {
	      for (int j = 0; j < names.length (); j++)
		{
		  std::string name = names (j);

		  OCTAVE_QUIT;

		  // Strip extension
		  size_t l = name.length ();
		  if (l > 4 && name.substr (l-4) == ".oct")
		    name = name.substr (0, l - 4);
		  else if (l > 2 && name.substr (l-2) == ".m")
		    name = name.substr (0, l - 2);
		  else
		    continue;

		  // Check if already in symbol table
		  symbol_record *sr = fbi_sym_tab->lookup (name);

		  if (!sr)
		    {
		      // Check if this version is first in the path
		      string_vector tmp (2);
		      tmp(0) = name + ".oct";
		      tmp(1) = name + ".m";
		      std::string file_name = 
			Vload_path_dir_path.find_first_of (tmp);

		      if (file_name == dirs[i] + tmp(0)
			  || file_name == dirs[i] + tmp(1))
			{
			  bool symbol_found;

			  std::string h;
			  if (file_name == dirs[i] + tmp(0))
			    {
			      // oct-file. Must load to get help
			      sr = lookup_by_name (name, false);

			      if (sr && sr->is_defined ())
				h = sr->help ();
			    }
			  else
			    h = get_help_from_file (file_name, symbol_found);

			  if (name.find (txt) != NPOS)
			    {
			      if (nargout)
				{
				  ret[0].append (name);
				  ret[1].append (first_help_sentence (h));
				}
			      else
				print_lookfor (name, first_help_sentence (h));
			    }
			  else
			    {
			      std::string s;
			      if (first_sentence_only)
				s = first_help_sentence (h);
			      else
				s = h;

			      std::transform (s.begin (), s.end (), s.begin (), tolower);

			      if (s.length () > 0 && s.find (txt) != NPOS)
				{
				  if (nargout)
				    {
				      ret[0].append (name);
				      ret[1].append (first_help_sentence (h));
				    }
				  else
				    print_lookfor (name, first_help_sentence (h));
				}
			    }
			}
		    }

		  // Check if this function has autoloaded functions attached to it
		  std::string file_name = Vload_path_dir_path.find_first_of (names(j));
		  string_vector autoload_fcns = reverse_lookup_autoload (file_name);

		  if (! autoload_fcns.empty ())
		    {
		      for (int k = 0; k < autoload_fcns.length (); k++)
			{
			  std::string aname = autoload_fcns (k);

			  // Check if already in symbol table
			  sr = fbi_sym_tab->lookup (aname);

			  if (!sr)
			    {
			      // Must load to get help
			      sr = lookup_by_name (aname, false);

			      std::string h;
			      if (sr && sr->is_defined ())
				h = sr->help ();

			      if (aname.find (txt) != NPOS)
				{
				  if (nargout)
				    {
				      ret[0].append (aname);
				      ret[1].append (first_help_sentence (h));
				    }
				  else
				    print_lookfor (aname, first_help_sentence (h));
				}
			      else
				{
				  std::string s;
				  if (first_sentence_only)
				    s = first_help_sentence (h);
				  else
				    s = h;

				  std::transform (s.begin (), s.end (), s.begin (), 
					     tolower);

				  if (s.length () > 0 && s.find (txt) != NPOS)
				    {
				      if (nargout)
					{
					  ret[0].append (aname);
					  ret[1].append (first_help_sentence (h));
					}
				      else
					print_lookfor (aname, first_help_sentence (h));
				    }
				}
			    }
			}
		    }
		}
	    }
	}

      if (nargout != 0)
	{
	  retval (1) = ret[1];
	  retval (0) = ret[0];
	}
    }
  else
    {
      error ("lookfor: argument must be a string");
    }

  return retval;
}

DEFUN (info_file, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} info_file ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} info_file (@var{new_val})\n\
Query or set the internal variable that specifies the name of the\n\
Octave info file.  The default value is\n\
@code{\"@var{octave-home}/info/octave.info\"}, in\n\
which @var{octave-home} is the directory where all of Octave is installed.\n\
@seealso{info_program, doc, help, makeinfo_program}\n\
@end deftypefn")
{
  return SET_NONEMPTY_INTERNAL_STRING_VARIABLE (info_file);
}

DEFUN (info_program, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} info_program ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} info_program (@var{new_val})\n\
Query or set the internal variable that specifies the name of the\n\
info program to run.  The default initial value is\n\
@code{\"@var{octave-home}/libexec/octave/@var{version}/exec/@var{arch}/info\"}\n\
in which @var{octave-home} is the directory where all of Octave is\n\
installed, @var{version} is the Octave version number, and @var{arch}\n\
is the system type (for example, @code{i686-pc-linux-gnu}).  The\n\
default initial value may be overridden by the environment variable\n\
@code{OCTAVE_INFO_PROGRAM}, or the command line argument\n\
@code{--info-program NAME}.\n\
@seealso{info_file, doc, help, makeinfo_program}\n\
@end deftypefn")
{
  return SET_NONEMPTY_INTERNAL_STRING_VARIABLE (info_program);
}

DEFUN (makeinfo_program, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} makeinfo_program ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} makeinfo_program (@var{new_val})\n\
Query or set the internal variable that specifies the name of the\n\
makeinfo program that Octave runs to format help text containing\n\
Texinfo markup commands.  The default initial value is @code{\"makeinfo\"}.\n\
@seealso{info_file, info_program, doc, help}\n\
@end deftypefn")
{
  return SET_NONEMPTY_INTERNAL_STRING_VARIABLE (makeinfo_program);
}

DEFUN (suppress_verbose_help_message, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} suppress_verbose_help_message ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} suppress_verbose_help_message (@var{new_val})\n\
Query or set the internal vaiable that controls whether Octave\n\
will add additional help information to the end of the output from\n\
the @code{help} command and usage messages for built-in commands.\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (suppress_verbose_help_message);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
