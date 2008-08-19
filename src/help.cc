/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
              2002, 2003, 2004, 2005, 2006, 2007 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

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
#include "file-stat.h"
#include "oct-env.h"
#include "str-vec.h"

#include <defaults.h>
#include "defun.h"
#include "dirfns.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "input.h"
#include "load-path.h"
#include "oct-obj.h"
#include "ov-usr-fcn.h"
#include "pager.h"
#include "parse.h"
#include "pathsearch.h"
#include "procstream.h"
#include "pt-pr-code.h"
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
  { "break",
    "-*- texinfo -*-\n\
@deffn Keyword break\n\
Exit the innermost enclosing do, while or for loop.\n\
@seealso{do, while, for, continue}\n\
@end deffn", },

  { "case",
    "-*- texinfo -*-\n\
@deffn Keyword case @{@var{value}@}\n\
A case statement in an switch. Octave cases are exclusive and do not\n\
fall-through as do C-language cases. A switch statement must have at least\n\
one case.  See @code{switch} for an example.\n\
@seealso{switch}\n\
@end deffn", },

  { "catch",
    "-*- texinfo -*-\n\
@deffn Keyword catch\n\
Begin the cleanup part of a try-catch block.\n\
@seealso{try}\n\
@end deffn", },

  { "continue",
    "-*- texinfo -*-\n\
@deffn Keyword continue\n\
Jump to the end of the innermost enclosing do, while or for loop.\n\
@seealso{do, while, for, break}\n\
@end deffn", },

  { "do",
    "-*- texinfo -*-\n\
@deffn Keyword do\n\
Begin a do-until loop. This differs from a do-while loop in that the\n\
body of the loop is executed at least once.\n\
@seealso{while}\n\
@end deffn", },

  { "else",
    "-*- texinfo -*-\n\
@deffn Keyword else\n\
Alternate action for an if block.  See @code{if} for an example.\n\
@seealso{if}\n\
@end deffn", },

  { "elseif",
    "-*- texinfo -*-\n\
@deffn Keyword elseif (@var{condition})\n\
Alternate conditional test for an if block.  See @code{if} for an example.\n\
@seealso{if}\n\
@end deffn", },

  { "end",
    "-*- texinfo -*-\n\
@deffn Keyword end\n\
Mark the end of any @code{for}, @code{if}, @code{do}, @code{while}, or @code{function} block.\n\
@seealso{for, if, do, while, function}\n\
@end deffn", },

  { "end_try_catch",
    "-*- texinfo -*-\n\
@deffn Keyword end_try_catch\n\
Mark the end of an @code{try-catch} block.\n\
@seealso{try, catch}\n\
@end deffn", }, 

  { "end_unwind_protect",
    "-*- texinfo -*-\n\
@deffn Keyword end_unwind_protect\n\
Mark the end of an unwind_protect block.\n\
@seealso{unwind_protect}\n\
@end deffn", }, 

  { "endfor",
    "-*- texinfo -*-\n\
@deffn Keyword endfor\n\
Mark the end of a for loop.  See @code{for} for an example.\n\
@seealso{for}\n\
@end deffn", },

  { "endfunction",
    "-*- texinfo -*-\n\
@deffn Keyword endfunction\n\
Mark the end of a function.\n\
@seealso{function}\n\
@end deffn", },

  { "endif",
    "-*- texinfo -*-\n\
@deffn Keyword endif\n\
Mark the end of an if block.  See @code{if} for an example.\n\
@seealso{if}\n\
@end deffn", },

  { "endswitch",
    "-*- texinfo -*-\n\
@deffn Keyword endswitch\n\
Mark the end of a switch block.  See @code{switch} for an example.\n\
@seealso{switch}\n\
@end deffn", },

  { "endwhile",
    "-*- texinfo -*-\n\
@deffn Keyword endwhile\n\
Mark the end of a while loop.  See @code{while} for an example.\n\
@seealso{do, while}\n\
@end deffn", },

  { "for",
    "-*- texinfo -*-\n\
@deffn Keyword for @var{i} = @var{range}\n\
Begin a for loop.\n\
@example\n\
for i = 1:10\n\
  i\n\
endfor\n\
@end example\n\
@seealso{do, while}\n\
@end deffn", },

  { "function",
    "-*- texinfo -*-\n\
@deffn Keyword function @var{outputs} = function (@var{input}, ...)\n\
@deffnx Keyword function {} function (@var{input}, ...)\n\
@deffnx Keyword function @var{outputs} = function\n\
Begin a function body with @var{outputs} as results and @var{inputs} as\n\
parameters.\n\
@seealso{return}\n\
@end deffn", },

  { "global",
    "-*- texinfo -*-\n\
@deffn Keyword global\n\
Declare variables to have global scope.\n\
@example\n\
global @var{x};\n\
if isempty (@var{x})\n\
  x = 1;\n\
endif\n\
@end example\n\
@seealso{persistent}\n\
@end deffn", },

  { "if",
    "-*- texinfo -*-\n\
@deffn Keyword if (@var{cond}) @dots{} endif\n\
@deffnx Keyword if (@var{cond}) @dots{} else @dots{} endif\n\
@deffnx Keyword if (@var{cond}) @dots{} elseif (@var{cond}) @dots{} endif\n\
@deffnx Keyword if (@var{cond}) @dots{} elseif (@var{cond}) @dots{} else @dots{} endif\n\
Begin an if block.\n\
@example\n\
x = 1;\n\
if (x == 1)\n\
  disp (\"one\");\n\
elseif (x == 2)\n\
  disp (\"two\");\n\
else\n\
  disp (\"not one or two\");\n\
endif\n\
@end example\n\
@seealso{switch}\n\
@end deffn", },

  { "otherwise",
    "-*- texinfo -*-\n\
@deffn Keyword otherwise\n\
The default statement in a switch block (similar to else in an if block).\n\
@seealso{switch}\n\
@end deffn", },

  { "persistent",
    "-*- texinfo -*-\n\
@deffn Keyword persistent @var{var}\n\
Declare variables as persistent.  A variable that has been declared\n\
persistent within a function will retain its contents in memory between\n\
subsequent calls to the same function.  The difference between persistent\n\
variables and global variables is that persistent variables are local in \n\
scope to a particular function and are not visible elsewhere.\n\
@seealso{global}\n\
@end deffn", },

  { "replot",
    "-*- texinfo -*-\n\
@deffn Keyword replot\n\
Replot a graphic.\n\
@seealso{plot}\n\
@end deffn", },

  { "return",
    "-*- texinfo -*-\n\
@deffn Keyword return\n\
Return from a function.\n\
@seealso{function}\n\
@end deffn", },

  { "static",
    "-*- texinfo -*-\n\
@deffn Keyword static\n\
This function has been deprecated in favor of persistent.\n\
@seealso{persistent}\n\
@end deffn", },

  { "switch",
    "-*- texinfo -*-\n\
@deffn Keyword switch @var{statement}\n\
Begin a switch block.\n\
@example\n\
yesno = \"yes\"\n\
\n\
switch yesno\n\
  case @{\"Yes\" \"yes\" \"YES\" \"y\" \"Y\"@}\n\
    value = 1;\n\
  case @{\"No\" \"no\" \"NO\" \"n\" \"N\"@}\n\
    value = 0;\n\
  otherwise\n\
    error (\"invalid value\");\n\
endswitch\n\
@end example\n\
@seealso{if, case, otherwise}\n\
@end deffn", },

  { "try",
    "-*- texinfo -*-\n\
@deffn Keyword try\n\
Begin a try-catch block.\n\
\n\
If an error occurs within a try block, then the catch code will be run and\n\
execution will proceed after the catch block (though it is often\n\
recommended to use the lasterr function to re-throw the error after cleanup\n\
is completed).\n\
@seealso{catch,unwind_protect}\n\
@end deffn", }, 

  { "until",
    "-*- texinfo -*-\n\
@deffn Keyword until\n\
End a do-until loop.\n\
@seealso{do}\n\
@end deffn", },

  { "unwind_protect",
    "-*- texinfo -*-\n\
@deffn Keyword unwind_protect\n\
Begin an unwind_protect block.\n\
\n\
If an error occurs within the first part of an unwind_protect block\n\
the commands within the unwind_protect_cleanup block are executed before\n\
the error is thrown.  If an error is not thrown, then the\n\
unwind_protect_cleanup block is still executed (in other words, the\n\
unwind_protect_cleanup will be run with or without an error in the\n\
unwind_protect block).\n\
@seealso{unwind_protect_cleanup,try}\n\
@end deffn", }, 

  { "unwind_protect_cleanup",
    "-*- texinfo -*-\n\
@deffn Keyword unwind_protect_cleanup\n\
Begin the cleanup section of an unwind_protect block.\n\
@seealso{unwind_protect}\n\
@end deffn", }, 

  { "varargin",
    "-*- texinfo -*-\n\
@deffn Keyword varargin\n\
Pass an arbitrary number of arguments into a function.\n\
@seealso{varargout, nargin, nargout}\n\
@end deffn", },

  { "varargout",
    "-*- texinfo -*-\n\
@deffn Keyword varargout\n\
Pass an arbitrary number of arguments out of a function.\n\
@seealso{varargin, nargin, nargout}\n\
@end deffn", },

  { "while",
    "-*- texinfo -*-\n\
@deffn Keyword while\n\
Begin a while loop.\n\
@seealso{do}\n\
@end deffn", },

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

  string_vector bif = symbol_table::built_in_function_names ();
  int bif_len = bif.length ();

  // FIXME -- is this really necessary here?
  string_vector glb = symbol_table::global_variable_names ();
  int glb_len = glb.length ();

  // FIXME -- is this really necessary here?
  string_vector top = symbol_table::top_level_variable_names ();
  int top_len = top.length ();

  string_vector lcl;
  if (! symbol_table::at_top_level ())
    lcl = symbol_table::variable_names ();
  int lcl_len = lcl.length ();

  string_vector ffl = load_path::fcn_names ();
  int ffl_len = ffl.length ();

  string_vector afl = autoloaded_functions ();
  int afl_len = afl.length ();

  int total_len = key_len + bif_len + glb_len + top_len + lcl_len
    + ffl_len + afl_len;

  string_vector list (total_len);

  // Put all the symbols in one big list.

  int j = 0;
  int i = 0;
  for (i = 0; i < key_len; i++)
    list[j++] = key[i];

  for (i = 0; i < bif_len; i++)
    list[j++] = bif[i];

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
display_symtab_names (std::ostream& os, const std::list<std::string>& names,
		      const std::string& desc)
{
  if (! names.empty ())
    {
      os << "\n*** " << desc << ":\n\n";

      string_vector sv (names);

      sv.list_in_columns (os);
    }
}

static void
simple_help (void)
{
  octave_stdout << "Help is available for the topics listed below.\n";

  additional_help_message (octave_stdout);

  display_names_from_help_list (octave_stdout, operator_help (),
				"operators");

  display_names_from_help_list (octave_stdout, keyword_help (),
				"reserved words");

  display_symtab_names (octave_stdout,
			symbol_table::built_in_function_names (),
			"built-in functions");

  // FIXME -- list functions defined on command line?

  load_path::display (octave_stdout);

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

  if (p1 == std::string::npos)
    p1 = 0;

  size_t p2 = t.find ("-*- texinfo -*-");

  return (p2 != std::string::npos);
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

      // Use double quotes to quote the sed patterns for Windows.

      buf << "sed -e \"s/^[#%][#%]* *//\" -e \"s/^ *@/@/\" | "
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

	  if (new_pos == std::string::npos)
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
raw_help_from_list (const help_list *list, const std::string& nm, 
		    std::string& h, bool& symbol_found)
{
  bool retval = false;

  const char *name;

  while ((name = list->name) != 0)
    {
      if (strcmp (name, nm.c_str ()) == 0)
	{
	  symbol_found = true;

	  h = list->help;

	  if (h.length () > 0)
	    retval = true;

	  break;
	}
      list++;
    }

  return retval;;
}

static bool
help_from_list (std::ostream& os, const help_list *list,
		const std::string& nm, int usage, bool& symbol_found)
{
  bool retval = false;

  std::string h;

  if (raw_help_from_list (list, nm, h, symbol_found))
    {
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
    }

  return retval;
}

static bool
raw_help_from_symbol_table (const std::string& nm, std::string& h, 
			    std::string& w, bool& symbol_found)
{
  bool retval = false;

  octave_value val = symbol_table::find_function (nm);

  if (val.is_defined ())
    {
      octave_function *fcn = val.function_value ();

      if (fcn)
	{
	  symbol_found = true;

	  h = fcn->doc_string ();

	  if (! h.empty ())
	    {
	      retval = true;

	      w = fcn->fcn_file_name ();

	      if (w.empty ())
		w = fcn->is_user_function ()
		  ? "command-line function" : "built-in function";
	    }
	}
    }

  return retval;
}

static bool
help_from_symbol_table (std::ostream& os, const std::string& nm,
			bool& symbol_found)
{
  bool retval = false;

  std::string h;
  std::string w;

  if (raw_help_from_symbol_table (nm, h, w, symbol_found))
    {
      if (h.length () > 0)
	{
	  h += "\n\n@noindent\n" + symbol_table::help_for_dispatch (nm);

	  display_help_text (os, h);

	  if (w.length () > 0 && ! Vsuppress_verbose_help_message)
	    os << w << "\n";

	  os << "\n";

	  retval = true;
	}
    }

  return retval;
}

static bool
raw_help_from_file (const std::string& nm, std::string& h, 
		    std::string& file, bool& symbol_found)
{
  bool retval = false;

  h = get_help_from_file (nm, symbol_found, file);

  if (h.length () > 0)
    retval = true;
  else if (! symbol_found)
    {
      file = contents_file_in_path (nm);
      
      if (! file.empty ())
	{
	  h = get_help_from_file (file, symbol_found);

	  if (h.length () > 0)
	    retval = true;
	}
    }

  return retval;
}

static bool
help_from_file (std::ostream& os, const std::string& nm, bool& symbol_found)
{
  bool retval = false;

  std::string h;
  std::string file;

  if (raw_help_from_file (nm, h, file, symbol_found))
    {
      if (h.length () > 0)
	{
	  // Strip extension
	  size_t l = file.length ();
	  if (l > 2 && file.substr (l-2) == ".m")
	    {
	      std::string tmp = file.substr (0, l - 2);

	      if (file_stat (tmp + ".oct"))
		file = tmp + ".oct";
	      else if (file_stat (tmp + ".mex"))
		file = tmp + ".mex";
	    }

	  os << nm << " is the file " << file << "\n\n";

	  display_help_text (os, h);

	  os << "\n";

	  retval = true;
	}
    }

  return retval;
}

std::string
raw_help (const std::string& nm, bool &symbol_found)
{
  std::string h;
  std::string w;
  std::string f;

  (raw_help_from_list (operator_help (), nm, h, symbol_found)
   || raw_help_from_list (keyword_help (), nm, h, symbol_found)
   || raw_help_from_symbol_table (nm, h, w, symbol_found)
   || raw_help_from_file (nm, h, f, symbol_found));

  return h;
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
@deffn {Command} help @var{name}\n\
Display the help text for @var{name}.\n\
If invoked without any arguments, @code{help} prints a list\n\
of all the available operators and functions.\n\
\n\
For example, the command @kbd{help help} prints a short message\n\
describing the @code{help} command.\n\
\n\
The help command can give you information about operators, but not the\n\
comma and semicolons that are used as command separators.  To get help\n\
for those, you must type @kbd{help comma} or @kbd{help semicolon}.\n\
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
display_file (std::ostream& os, const std::string& name,
	      const std::string& fname, const std::string& type,
	      bool pr_type_info, bool quiet)
{
  std::ifstream fs (fname.c_str (), std::ios::in);

  if (fs)
    {
      if (pr_type_info && ! quiet)
	os << name << " is the " << type << " defined from the file\n"
	   << fname << ":\n\n";

      char ch;

      while (fs.get (ch))
	os << ch;
    }
  else
    os << "unable to open `" << fname << "' for reading!\n";
}

static void
do_type (std::ostream& os, const std::string& name, bool pr_type_info,
	 bool quiet, bool pr_orig_txt)
{
  // FIXME -- should we bother with variables here (earlier versions
  // of Octave displayed them)?

  octave_value val = symbol_table::varval (name);

  if (val.is_defined ())
    {
      if (pr_type_info && ! quiet)
	os << name << " is a variable\n";

      val.print_raw (os, pr_orig_txt);

      if (pr_type_info)
	os << "\n";
    }
  else
    {
      val = symbol_table::find_function (name);

      if (val.is_defined ())
	{
	  octave_function *fcn = val.function_value ();

	  if (fcn)
	    {
	      std::string fn = fcn->fcn_file_name ();

	      if (fcn->is_builtin_function ())
		os << name << " is a built-in function" << std::endl;
	      else if (fcn->is_dld_function () || fcn->is_mex_function ())
		os << name
		   << " is a dyanmically loaded function from the file\n"
		   << fn << std::endl;
	      else if (pr_orig_txt && ! fn.empty ())
		display_file (os, name, fn, "function", pr_type_info, quiet);
	      else
		{
		  if (pr_type_info && ! quiet)
		    {
		      os << name;

		      if (fcn->is_user_function ())
			{
			  if (fn.empty ())
			    os << " is a command-line function:\n\n";
			  else
			    os << " is a function defined from the file\n"
			       << fn << ":\n\n";
			}
		    }

		  tree_print_code tpc (os, "", pr_orig_txt);

		  fcn->accept (tpc);
		}
	    }
	}
      else
	{
	  std::string fn = fcn_file_in_path (name);

	  if (! fn.empty ())
	    display_file (os, name, fn, "script", pr_type_info, quiet);
	  else
	    error ("type: `%s' undefined", name.c_str ());
	}
    }
}

DEFCMD (type, args, nargout,
  "-*- texinfo -*-\n\
\n\
@deffn {Command} type options name @dots{}\n\
Display the definition of each @var{name} that refers to a function.\n\
\n\
Normally also displays whether each @var{name} is user-defined or built-in;\n\
the @code{-q} option suppresses this behaviour.\n\
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
	    print_usage ();
	}
      else
	print_usage ();
    }

 abort:

  return retval;
}

std::string
do_which (const std::string& name)
{
  octave_value val = symbol_table::find_function (name);

  if (val.is_defined ())
    {
      octave_function *fcn = val.function_value ();

      if (fcn)
	{
	  std::string fn = fcn->fcn_file_name ();

	  return fn.empty ()
	    ? (fcn->is_user_function ()
	       ? "command-line function" : "built-in function")
	    : fn;
	}
    }

  return fcn_file_in_path (name);
}

static void
do_which (std::ostream& os, const std::string& name)
{
  std::string desc;

  octave_value val = symbol_table::find_function (name);

  if (val.is_defined ())
    {
      octave_function *fcn = val.function_value ();

      if (fcn)
	{
	  desc = fcn->fcn_file_name ();

	  if (desc.empty ())
	    {
	      if (fcn->is_user_function ())
		desc = "is a command-line function";
	      else
		desc = "is a built-in function";
	    }
	  else
	    desc = "is the function from the file " + desc;
	}
    }

  if (desc.empty ())
    {
      std::string fn = fcn_file_in_path (name);

      if (! fn.empty ())
	desc = "is the script file " + fn;
      else
	desc = "is undefined";
    }

  os << "which: `" << name << "' " << desc << std::endl;
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
	print_usage ();
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

	  if (pos == std::string::npos)
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

      if (pos == std::string::npos)
	return retval;

      // At start of real text. Get first line with the sentence
      size_t new_pos = h2.find_first_of ('\n', pos);
      std::string line = h2.substr (pos, new_pos-pos);
      size_t dot_pos;

      while ((dot_pos = line.find_first_of ('.')) == std::string::npos)
	{
	  // Trim trailing blanks on line
	  line.substr (0, line.find_last_not_of ("\n\t ") + 1);

	  // Append next line
	  size_t tmp_pos = h2.find_first_not_of ("\n\t ", new_pos + 1);
	  if (tmp_pos == std::string::npos || h2.substr (tmp_pos, 1) == "\n")
	    break;

	  new_pos = h2.find_first_of ('\n', tmp_pos);
	  std::string next = h2.substr (tmp_pos, new_pos-tmp_pos);

	  if (short_sentence)
	    {
	      if ((tmp_pos = next.find_first_of ('.')) != std::string::npos)
		{
		  line = line + " " + next;
		  dot_pos = line.find_first_of ('.');
		}
	      break;
	    }
	  else
	    line = line + " " + next;
	}

      if (dot_pos == std::string::npos)
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

	  if (pos == std::string::npos)
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
	      pos = (new_pos == std::string::npos ? std::string::npos : new_pos + 1);
	      continue;
	    }

	  size_t line_pos = 0;
	  size_t tmp_pos = 0;

	  // chop " blah : "
	  tmp_pos = line.find_first_not_of ("\t ", line.find_first_not_of 
					     (_alphanum, line_pos));
	  if (tmp_pos != std::string::npos && line.substr (tmp_pos, 1) == ":")
	    line_pos = line.find_first_not_of ("\t ", tmp_pos + 1);

	  if (line_pos == std::string::npos)
	    {
	      pos = (new_pos == std::string::npos ? std::string::npos : new_pos + 1);
	      continue;
	    }

	  // chop " function "
	  if (lower.substr (line_pos, 8) == "function")
	    line_pos =  line.find_first_not_of ("\t ", line_pos + 8);
	  
	  if (line_pos == std::string::npos)
	    {
	      pos = (new_pos == std::string::npos ? std::string::npos : new_pos + 1);
	      continue;
	    }

	  // chop " [a,b] = "
	  if (line.substr (line_pos, 1) == "[")
	    {
	      tmp_pos = line.find_first_not_of 
		("\t ", line.find_first_of ("]", line_pos) + 1);

	      if (tmp_pos != std::string::npos && line.substr (tmp_pos, 1) == "=")
		line_pos = line.find_first_not_of ("\t ",tmp_pos + 1);
	    }

	  if (line_pos == std::string::npos)
	    {
	      pos = (new_pos == std::string::npos ? std::string::npos : new_pos + 1);
	      continue;
	    }

	  // chop " a = "
	  if (line.find_first_not_of (_alpha, line_pos) != line_pos)
	    {
	      tmp_pos = line.find_first_not_of ("\t ", line.find_first_not_of 
						(_alphanum, line_pos));
	      if (tmp_pos != std::string::npos && line.substr (tmp_pos, 1) == "=")
		line_pos = line.find_first_not_of ("\t ", tmp_pos + 1);
	    }

	  if (line_pos == std::string::npos)
	    {
	      pos = new_pos + 1;
	      continue;
	    }

	  // chop " f(x) "
	  if (line.find_first_not_of (_alpha, line_pos) != line_pos)
	    {
	      tmp_pos = line.find_first_not_of ("\t ", line.find_first_not_of 
						(_alphanum, line_pos));
	      if (tmp_pos != std::string::npos && line.substr (tmp_pos, 1) == "(")
		line_pos = line.find_first_not_of ("\t ", line.find_first_of 
						   (")", tmp_pos) + 1);
	    }

	  if (line_pos == std::string::npos)
	    {
	      pos = (new_pos == std::string::npos ? std::string::npos : new_pos + 1);
	      continue;
	    }

	  // chop " ; "
	  if (line.substr (line_pos, 1) == ":"
	      || line.substr (line_pos, 1) == ";")
	    line_pos = line.find_first_not_of ("\t ", line_pos + 1);

	  if (line_pos == std::string::npos)
	    {
	      pos = (new_pos == std::string::npos ? std::string::npos : new_pos + 1);
	      continue;
	    }

	  // chop " BLAH "
	  if (line.length () > line_pos + 2
	      && line.find_first_of (_upper, line_pos) == line_pos
	      && line.find_first_of (_upper, line_pos+1) == line_pos + 1)
	    line_pos = line.find_first_not_of ("\t ", line.find_first_not_of 
			(_upper + "0123456789_", line_pos));

	  if (line_pos == std::string::npos)
	    {
	      pos = (new_pos == std::string::npos ? std::string::npos : new_pos + 1);
	      continue;
	    }

	  // chop " blah --- "
	  tmp_pos = line.find_first_not_of ("\t ", line.find_first_not_of 
					     (_alphanum, line_pos));
	  if (tmp_pos != std::string::npos && line.substr (tmp_pos, 1) == "-")
	    {
	      tmp_pos = line.find_first_not_of ("-", tmp_pos);
	      if (line.substr (tmp_pos, 1) == " "
		  || line.substr (tmp_pos, 1) == "\t")
		line_pos = line.find_first_not_of ("\t ", tmp_pos);
	    }

	  if (line_pos == std::string::npos)
	    {
	      pos = (new_pos == std::string::npos ? std::string::npos : new_pos + 1);
	      continue;
	    }

	  // chop " blah <TAB> "
	  if (line.find_first_not_of (_alpha, line_pos) != line_pos)
	    {
	      tmp_pos = line.find_first_not_of (" ", line.find_first_not_of 
						(_alphanum, line_pos));
	      if (tmp_pos != std::string::npos && line.substr (tmp_pos, 1) == "\t")
		line_pos = line.find_first_not_of ("\t ", line.find_first_of 
						   (")", tmp_pos) + 1);
	    }

	  if (line_pos == std::string::npos)
	    {
	      pos = (new_pos == std::string::npos ? std::string::npos : new_pos + 1);
	      continue;
	    }

	  // chop " blah  "
	  if (line.find_first_not_of (_alpha, line_pos) != line_pos)
	    {
	      tmp_pos = line.find_first_not_of (_alphanum, line_pos);

	      if (tmp_pos != std::string::npos
		  && (line.substr (tmp_pos, 2) == "\t\t"
		      || line.substr (tmp_pos, 2) == "\t "
		      || line.substr (tmp_pos, 2) == " \t"
		      || line.substr (tmp_pos, 2) == " "))
		line_pos = line.find_first_not_of ("\t ", tmp_pos);
	    }

	  if (line_pos == std::string::npos)
	    {
	      pos = (new_pos == std::string::npos ? std::string::npos : new_pos + 1);
	      continue;
	    }

	  // skip blah \n or \n blah
	  // skip blank line
	  // skip "# !/usr/bin/octave"
	  if ((line.substr (line_pos , 2) == "or"
	       && line.find_first_not_of ("\n\t ", line_pos + 2) == std::string::npos)
	      || line.find_first_not_of ("\n\t ", line_pos) == std::string::npos
	      || line.substr (line_pos, 2) == "!/")
	    {
	      pos = (new_pos == std::string::npos ? std::string::npos : new_pos + 1);
	      continue;
	    }

	  // Got the start of first sentence, break.
	  pos = pos + line_pos;
	  break;
	}

      if (pos == std::string::npos)
	return retval;

      // At start of real text. Get first line with the sentence
      size_t new_pos = h.find_first_of ('\n', pos);
      std::string line = h.substr (pos, new_pos-pos);
      size_t dot_pos;

      while ((dot_pos = line.find_first_of ('.')) == std::string::npos)
	{
	  // Trim trailing blanks on line
	  line = line.substr (0, line.find_last_not_of ("\n\t ") + 1);

	  // Append next line
	  size_t tmp_pos = h.find_first_not_of ("\t ", new_pos + 1);
	  if (tmp_pos == std::string::npos || h.substr (tmp_pos, 1) == "\n")
	    break;

	  new_pos = h.find_first_of ('\n', tmp_pos);
	  std::string next = h.substr (tmp_pos, new_pos-tmp_pos);

	  if (short_sentence)
	    {
	      // Only add the next line if it terminates the sentence, then break
	      if ((tmp_pos = next.find_first_of ('.')) != std::string::npos)
		{
		  line = line + " " + next;
		  dot_pos = line.find_first_of ('.');
		}
	      break;
	    }
	  else
	    line = line + " " + next;
	}

      if (dot_pos == std::string::npos)
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
	new_pos = end_pos = std::string::npos;
      else
	while (new_pos != std::string::npos && new_pos - pos < width)
	  {
	    end_pos = new_pos;
	    new_pos = line.find_first_of ("\n\t ", new_pos + 1);
	  }

      octave_stdout << line.substr (pos, end_pos-pos) << std::endl;
		  
      if (end_pos == std::string::npos)
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
function search path.  By default @code{lookfor} searches for @var{str}\n\
in the first sentence of the help string of each function found. The entire\n\
help string of each function found in the path can be searched if\n\
the '-all' argument is supplied. All searches are case insensitive.\n\
\n\
Called with no output arguments, @code{lookfor} prints the list of matching\n\
functions to the terminal. Otherwise the output arguments @var{fun} and\n\
@var{helpstring} define the matching functions and the first sentence of\n\
each of their help strings.\n\
\n\
Note that the ability of @code{lookfor} to correctly identify the first\n\
sentence of the help of the functions is dependent on the format of the\n\
functions help. All of the functions in Octave itself will correctly\n\
find the first sentence, but the same can not be guaranteed for other\n\
functions. Therefore the use of the '-all' argument might be necessary\n\
to find related functions that are not part of Octave.\n\
@seealso{help, which}\n\
@end deffn")
{
  octave_value_list retval;

  int nargin = args.length ();
  bool first_sentence_only = true;

  if (nargin != 1 && nargin != 2)
    {
      print_usage ();
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

	  if (name.find (txt) != std::string::npos)
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

	      if (s.length () > 0 && s.find (txt) != std::string::npos)
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

	  if (name.find (txt) != std::string::npos)
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

	      if (s.length () > 0 && s.find (txt) != std::string::npos)
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

      string_vector names;

#ifdef OLD_SYMTAB
      // Check the symbol record table
      names = fbi_sym_tab->name_list (string_vector (), true);

      for (octave_idx_type i = 0; i < names.length (); i++)
	{
	  std::string name = names (i);

	  OCTAVE_QUIT;

	  symbol_record *sr = lookup_by_name (name, 0);
	  if (sr && sr->is_defined ()
	      && sr->type_name () != "overloaded function")
	    {
	      std::string h = sr->help ();

	      if (name.find (txt) != std::string::npos)
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

		  if (s.length () > 0 && s.find (txt) != std::string::npos)
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
#endif

      string_vector dirs = load_path::dirs ();

      int len = dirs.length ();

      for (int i = 0; i < len; i++)
	{
	  names = load_path::files (dirs[i]);

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

#ifdef OLD_SYMTAB
		  // Check if already in symbol table
		  symbol_record *sr = fbi_sym_tab->lookup (name);

		  if (!sr)
		    {
		      // Check if this version is first in the path

		      std::string file_name = load_path::find_fcn (name);
		      
		      std::string dir = dirs[i];

		      if (! file_ops::is_dir_sep (dir[dir.length()-1]))
			dir += file_ops::dir_sep_str ();

		      if (file_name == dir + name + ".oct"
			  || file_name == dir + name + ".m")
			{
			  bool symbol_found;

			  std::string h;
			  if (file_name == dir + name + ".oct")
			    {
			      // oct-file. Must load to get help
			      sr = lookup_by_name (name, false);

			      if (sr && sr->is_defined ())
				h = sr->help ();
			    }
			  else
			    h = get_help_from_file (file_name, symbol_found);

			  if (name.find (txt) != std::string::npos)
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

			      if (s.length () > 0 && s.find (txt) != std::string::npos)
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
#endif

		  // Check if this function has autoloaded functions attached to it
		  std::string file_name = load_path::find_fcn (name);

		  string_vector autoload_fcns = reverse_lookup_autoload (file_name);

		  if (! autoload_fcns.empty ())
		    {
		      for (int k = 0; k < autoload_fcns.length (); k++)
			{
			  std::string aname = autoload_fcns (k);

#ifdef OLD_SYMTAB
			  // Check if already in symbol table
			  sr = fbi_sym_tab->lookup (aname);

			  if (!sr)
			    {
			      // Must load to get help
			      sr = lookup_by_name (aname, false);

			      std::string h;
			      if (sr && sr->is_defined ())
				h = sr->help ();

			      if (aname.find (txt) != std::string::npos)
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

				  if (s.length () > 0 && s.find (txt) != std::string::npos)
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
#endif
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
info program to run.  The default value is\n\
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
Query or set the internal variable that controls whether Octave\n\
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
