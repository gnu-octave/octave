/*

Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2002,
              2003, 2004, 2005, 2006, 2007 John W. Eaton

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

#include <cstdio>
#include <cstring>

#include <iomanip>
#include <set>
#include <string>

#include "file-stat.h"
#include "oct-env.h"
#include "file-ops.h"
#include "glob-match.h"
#include "str-vec.h"

#include <defaults.h>
#include "Cell.h"
#include "defun.h"
#include "dirfns.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "input.h"
#include "lex.h"
#include "load-path.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-usr-fcn.h"
#include "pager.h"
#include "parse.h"
#include "symtab.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// Should Octave always check to see if function files have changed
// since they were last compiled?
static int Vignore_function_time_stamp = 1;

// Defines layout for the whos/who -long command
static std::string Vwhos_line_format
  = "  %a:4; %ln:6; %cs:16:6:1;  %rb:12;  %lc:-1;\n";

void
clear_mex_functions (void)
{
  symbol_table::clear_mex_functions ();
}

void
clear_function (const std::string& nm)
{
  symbol_table::clear_function (nm);
}

void
clear_variable (const std::string& nm)
{
  symbol_table::clear_variable (nm);
}

void
clear_symbol (const std::string& nm)
{
  symbol_table::clear_symbol (nm);
}

// Attributes of variables and functions.

// Is this a command-style function?

static std::set <std::string> command_set;

void
mark_as_command (const std::string& s)
{
  command_set.insert (s);
}

static inline void
unmark_command (const std::string& s)
{
  command_set.erase (s);
}

DEFCMD (mark_as_command, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} mark_as_command (@var{name})\n\
Enter @var{name} into the list of commands.\n\
@seealso{unmark_command, iscommand}\n\
@end deftypefn")
{
  octave_value_list retval;

  if (symbol_table::at_top_level ())
    {
      int nargin = args.length ();

      if (nargin > 0)
	{
	  int argc = nargin + 1;

	  string_vector argv = args.make_argv ("mark_as_command");

	  if (! error_state)
	    {
	      for (int i = 1; i < argc; i++)
		mark_as_command (argv[i]);
	    }
	}
      else
	print_usage ();
    }
  else
    warning ("mark_as_command: invalid use inside function body");

  return retval;
}

DEFCMD (unmark_command, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} unmark_command (@var{name})\n\
Remove @var{name} from the list of commands.\n\
@seealso{mark_as_command, iscommand}\n\
@end deftypefn")
{
  octave_value_list retval;

  if (symbol_table::at_top_level ())
    {
      int nargin = args.length ();

      if (nargin > 0)
	{
	  int argc = nargin + 1;

	  string_vector argv = args.make_argv ("unmark_command");

	  if (! error_state)
	    {
	      for (int i = 1; i < argc; i++)
		unmark_command (argv[i]);
	    }
	}
      else
	print_usage ();
    }
  else
    warning ("mark_as_command: invalid use inside function body");

  return retval;
}

bool
is_command_name (const std::string& s)
{
  return command_set.find (s) != command_set.end ();
}


DEFCMD (iscommand, args, ,
"-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} iscommand (@var{name})\n\
Return true if @var{name} is a command style function.  If @var{name}\n\
is omitted, return a list of identifiers which are marked as commands with\n\
@code{mark_as_command}.\n\
@seealso{mark_as_command, unmark_command}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
      string_vector lst (command_set.size ());

      int i = 0;
      for (std::set<std::string>::const_iterator p = command_set.begin ();
	   p != command_set.end (); p++)
	lst[i++] = *p;

      retval = Cell (lst.qsort ());
    }
  else if (nargin == 1)
    {
      string_vector argv = args.make_argv ("iscommand");
	  
      if (! error_state)
	{
	  std::string s = argv[1];
	  retval = is_command_name(s);
	}
    }
  else
    print_usage ();

  return retval;
}

// Is this a raw input command?

static std::set <std::string> rawcommand_set;

void
mark_as_rawcommand (const std::string& s)
{
  command_set.insert (s);    
  rawcommand_set.insert (s);
}

void
unmark_rawcommand (const std::string& s)
{
  rawcommand_set.erase (s);
}

DEFCMD (mark_as_rawcommand, args, ,
"-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} mark_as_rawcommand (@var{name})\n\
Enter @var{name} into the list of raw input commands and to the list of\n\
command style functions.\n\
Raw input commands are like normal command style functions, but they\n\
receive their input unprocessed (ie. strings still contain the quotes\n\
and escapes they had when input). However, comments and continuations\n\
are handled as usual, you cannot pass a token starting with a comment\n\
character ('#' or '%') to your function, and the last token cannot be\n\
a continuation token ('\\' or '...').\n\
@seealso{unmark_rawcommand, israwcommand, iscommand, mark_as_command}\n\
@end deftypefn")
{
  octave_value_list retval;

  if (symbol_table::at_top_level ())
    {
      int nargin = args.length ();

      if (nargin > 0)
	{
	  int argc = nargin + 1;

	  string_vector argv = args.make_argv ("mark_as_rawcommand");

	  if (! error_state)
	    {
	      for (int i = 1; i < argc; i++)
		mark_as_rawcommand (argv[i]);
	    }
	}
      else
	print_usage ();
    }
  else
    warning ("mark_as_rawcommand: invalid use inside function body");

  return retval;
}

DEFCMD (unmark_rawcommand, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} unmark_rawcommand (@var{name})\n\
Remove @var{name} from the list of raw input commands.\n\
Note that this does not remove @var{name} from the list of command style\n\
functions.\n\
@seealso{mark_as_rawcommand, israwcommand, iscommand, unmark_command}\n\
@end deftypefn")
{
  octave_value_list retval;

  if (symbol_table::at_top_level ())
    {
      int nargin = args.length ();

      if (nargin > 0)
	{
	  int argc = nargin + 1;

	  string_vector argv = args.make_argv ("unmark_rawcommand");

	  if (! error_state)
	    {
	      for (int i = 1; i < argc; i++)
		unmark_rawcommand (argv[i]);
	    }
	}
      else
	print_usage ();
    }
  else
    warning ("unmark_rawcommand: invalid use inside function body");

  return retval;
}

bool
is_rawcommand_name (const std::string& s)
{
  return rawcommand_set.find (s) != rawcommand_set.end ();
}

DEFCMD (israwcommand, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} israwcommand (@var{name})\n\
Return true if @var{name} is a raw input command function.\n\
If @var{name} is omitted, return a list of identifiers which are marked as\n\
raw input commands with mark_as_rawcommand.\n\
@seealso{mark_as_rawcommand, unmark_rawcommand}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
      string_vector lst (rawcommand_set.size());
      
      int i = 0;
      for (std::set<std::string>::const_iterator p = rawcommand_set.begin ();
	   p != rawcommand_set.end ();
	   p++)
	lst[i++] = *p;

      retval = Cell (lst.qsort ());
    }
  else if (nargin == 1)
    {
      string_vector argv = args.make_argv ("israwcommand");
	  
      if (! error_state)
	{
	  std::string s = argv[1];
	  retval = is_rawcommand_name(s);
	}
    }
  else
    print_usage ();

  return retval;
}

// Is this octave_value a valid function?

octave_function *
is_valid_function (const std::string& fcn_name,
		   const std::string& warn_for, bool warn)
{
  octave_function *ans = 0;

  if (! fcn_name.empty ())
    {
      octave_value val = symbol_table::find_function (fcn_name);

      if (val.is_defined ())
	ans = val.function_value (true);
    }

  if (! ans && warn)
    error ("%s: the symbol `%s' is not valid as a function",
	   warn_for.c_str (), fcn_name.c_str ());

  return ans;
}

octave_function *
is_valid_function (const octave_value& arg,
		   const std::string& warn_for, bool warn)
{
  octave_function *ans = 0;

  std::string fcn_name;

  if (arg.is_string ())
    {
      fcn_name = arg.string_value ();

      if (! error_state)
	ans = is_valid_function (fcn_name, warn_for, warn);
      else if (warn)
	error ("%s: expecting function name as argument", warn_for.c_str ());
    }
  else if (warn)
    error ("%s: expecting function name as argument", warn_for.c_str ());

  return ans;
}

octave_function *
extract_function (const octave_value& arg, const std::string& warn_for,
		  const std::string& fname, const std::string& header,
		  const std::string& trailer)
{
  octave_function *retval = 0;

  retval = is_valid_function (arg, warn_for, 0);

  if (! retval)
    {
      std::string s = arg.string_value ();

      std::string cmd = header;
      cmd.append (s);
      cmd.append (trailer);

      if (! error_state)
	{
	  int parse_status;

	  eval_string (cmd, true, parse_status);

	  if (parse_status == 0)
	    {
	      retval = is_valid_function (fname, warn_for, 0);
      
	      if (! retval)
		{
		  error ("%s: `%s' is not valid as a function",
			 warn_for.c_str (), fname.c_str ());
		  return retval;
		}
	    }
	  else
	    error ("%s: `%s' is not valid as a function",
		   warn_for.c_str (), fname.c_str ());
	}
      else
	error ("%s: expecting first argument to be a string",
	       warn_for.c_str ());
    }

  return retval;
}

string_vector
get_struct_elts (const std::string& text)
{
  int n = 1;

  size_t pos = 0;

  size_t len = text.length ();

  while ((pos = text.find ('.', pos)) != NPOS)
    {
      if (++pos == len)
	break;

      n++;
    }

  string_vector retval (n);

  pos = 0;

  for (int i = 0; i < n; i++)
    {
      len = text.find ('.', pos);

      if (len != NPOS)
	len -= pos;

      retval[i] = text.substr (pos, len);

      if (len != NPOS)
	pos += len + 1;
    }

  return retval;
}

static inline bool
is_variable (const std::string& name)
{
  bool retval = false;

  if (! name.empty ())
    {
      octave_value val = symbol_table::varval (name);

      retval = val.is_defined ();
    }

  return retval;
}

string_vector
generate_struct_completions (const std::string& text,
			     std::string& prefix, std::string& hint)
{
  string_vector names;

  size_t pos = text.rfind ('.');

  if (pos != NPOS)
    {
      if (pos == text.length ())
	hint = "";
      else
	hint = text.substr (pos+1);

      prefix = text.substr (0, pos);

      std::string base_name = prefix;

      pos = base_name.find_first_of ("{(.");

      if (pos != NPOS)
	base_name = base_name.substr (0, pos);

      if (is_variable (base_name))
	{
	  int parse_status;

	  unwind_protect::begin_frame ("generate_struct_completions");

	  unwind_protect_int (error_state);
	  unwind_protect_int (warning_state);

	  unwind_protect_bool (discard_error_messages);
	  unwind_protect_bool (discard_warning_messages);

	  discard_error_messages = true;
	  discard_warning_messages = true;

	  octave_value tmp = eval_string (prefix, true, parse_status);

	  unwind_protect::run_frame ("generate_struct_completions");

	  if (tmp.is_defined () && tmp.is_map ())
	    names = tmp.map_keys ();
	}
    }

  return names;
}

// FIXME -- this will have to be much smarter to work
// "correctly".

bool
looks_like_struct (const std::string& text)
{
  bool retval = (! text.empty ()
		 && text != "."
		 && text.find_first_of (file_ops::dir_sep_chars) == NPOS
		 && text.find ("..") == NPOS
		 && text.rfind ('.') != NPOS);

#if 0
  symbol_record *sr = curr_sym_tab->lookup (text);

  if (sr && ! sr->is_function ())
    {
      int parse_status;

      unwind_protect::begin_frame ("looks_like_struct");

      unwind_protect_bool (discard_error_messages);
      unwind_protect_int (error_state);

      discard_error_messages = true;

      octave_value tmp = eval_string (text, true, parse_status);

      unwind_protect::run_frame ("looks_like_struct");

      retval = (tmp.is_defined () && tmp.is_map ());
    }
#endif

  return retval;
}

static octave_value
do_isglobal (const octave_value_list& args)
{
  octave_value retval = false;

  int nargin = args.length ();

  if (nargin != 1)
    {
      print_usage ();
      return retval;
    }

  std::string name = args(0).string_value ();

  if (error_state)
    {
      error ("isglobal: expecting std::string argument");
      return retval;
    }

  return symbol_table::is_global (name);
}

DEFUN (isglobal, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isglobal (@var{name})\n\
Return 1 if @var{name} is globally visible.  Otherwise, return 0.  For\n\
example,\n\
\n\
@example\n\
@group\n\
global x\n\
isglobal (\"x\")\n\
     @result{} 1\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  return do_isglobal (args);
}

DEFUN (is_global, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isglobal (@var{name})\n\
This function has been deprecated.  Use isglobal instead.\n\
@end deftypefn")
{
  return do_isglobal (args);
}

int
symbol_exist (const std::string& name, const std::string& type)
{
  int retval = 0;

  std::string struct_elts;
  std::string symbol_name = name;

  size_t pos = name.find ('.');

  if (pos != NPOS && pos > 0)
    {
      struct_elts = name.substr (pos+1);
      symbol_name = name.substr (0, pos);
    }

  // We shouldn't need to look in the global symbol table, since any
  // name that is visible in the current scope will be in the local
  // symbol table.

  octave_value_list evaluated_args;
  bool args_evaluated;

  octave_value val = symbol_table::find (symbol_name, 0, string_vector (),
					 evaluated_args, args_evaluated);

  if (val.is_defined ())
    {
      bool not_a_struct = struct_elts.empty ();
      bool var_ok = not_a_struct /* || val.is_map_element (struct_elts) */;

      if (! retval
	  && var_ok
	  && (type == "any" || type == "var")
	  && val.is_constant ())
	{
	  retval = 1;
	}

      if (! retval
	  && (type == "any" || type == "builtin"))
	{
	  if (not_a_struct && val.is_builtin_function ())
	    {
	      retval = 5;
	    }
	}

      if (! retval
	  && not_a_struct
	  && (type == "any" || type == "file")
	  && (val.is_user_function () || val.is_dld_function ()))
	{
	  octave_function *f = val.function_value (true);
	  std::string s = f ? f->fcn_file_name () : std::string ();

	  retval = s.empty () ? 103 : (val.is_user_function () ? 2 : 3);
	}
    }

  if (! (type == "var" || type == "builtin"))
    {
      if (! retval)
	{
	  std::string file_name = lookup_autoload (name);

	  if (file_name.empty ())
	    file_name = load_path::find_fcn (name);

	  size_t len = file_name.length ();

	  if (len > 0)
	    {
	      if (type == "any" || type == "file")
		{
		  if (len > 4 && (file_name.substr (len-4) == ".oct"
				  || file_name.substr (len-4) == ".mex"))
		    retval = 3;
		  else
		    retval = 2;
		}
	    }
	}

      if (! retval)
	{
	  std::string file_name = file_in_path (name, "");

	  if (file_name.empty ())
	    file_name = name;

	  file_stat fs (file_name);

	  if (fs)
	    {
	      if ((type == "any" || type == "file")
		  && fs.is_reg ())
		{
		  retval = 2;
		}
	      else if ((type == "any" || type == "dir")
		       && fs.is_dir ())
		{
		  retval = 7;
		}
	    }
	}
    }

  return retval;
}

#define GET_IDX(LEN) \
  static_cast<int> ((LEN-1) * static_cast<double> (rand ()) / RAND_MAX)

std::string
unique_symbol_name (const std::string& basename)
{
  static const std::string alpha
    = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

  static size_t len = alpha.length ();

  std::string nm = basename + alpha[GET_IDX (len)];

  size_t pos = nm.length ();

  if (nm.substr (0, 2) == "__")
    nm.append ("__");

  while (symbol_exist (nm, "any"))
    nm.insert (pos++, 1, alpha[GET_IDX (len)]);

  return nm;
}

DEFUN (exist, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} exist (@var{name}, @var{type})\n\
Return 1 if the name exists as a variable, 2 if the name is an\n\
absolute file name, an ordinary file in Octave's @code{path}, or (after\n\
appending @samp{.m}) a function file in Octave's @code{path}, 3 if the\n\
name is a @samp{.oct} or @samp{.mex} file in Octave's @code{path},\n\
5 if the name is a built-in function, 7 if the name is a directory, or 103\n\
if the name is a function not associated with a file (entered on\n\
the command line).\n\
\n\
Otherwise, return 0.\n\
\n\
This function also returns 2 if a regular file called @var{name}\n\
exists in Octave's search path.  If you want information about\n\
other types of files, you should use some combination of the functions\n\
@code{file_in_path} and @code{stat} instead.\n\
\n\
If the optional argument @var{type} is supplied, check only for\n\
symbols of the specified type.  Valid types are\n\
\n\
@table @samp\n\
@item \"var\"\n\
Check only for variables.\n\
@item \"builtin\"\n\
Check only for built-in functions.\n\
@item \"file\"\n\
Check only for files.\n\
@item \"dir\"\n\
Check only for directories.\n\
@end table\n\
@end deftypefn")
{
  octave_value retval = false;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      std::string name = args(0).string_value ();

      if (! error_state)
	{
	  std::string type
	    = (nargin == 2) ? args(1).string_value () : std::string ("any");

	  if (! error_state)
	    retval = symbol_exist (name, type);
	  else
	    error ("exist: expecting second argument to be a string");
	}
      else
	error ("exist: expecting first argument to be a string");
    }
  else
    print_usage ();

  return retval;
}

octave_value
lookup_function_handle (const std::string& nm)
{
  octave_value val = symbol_table::varval (nm);

  return val.is_function_handle () ? val : octave_value ();
}

octave_value
get_global_value (const std::string& nm, bool silent)
{
  octave_value val = symbol_table::varval (nm, symbol_table::global_scope ());

  if (val.is_undefined () && ! silent)
    error ("get_global_by_name: undefined symbol `%s'", nm.c_str ());

  return val;
}

void
set_global_value (const std::string& nm, const octave_value& val)
{
  symbol_table::varref (nm, symbol_table::global_scope ()) = val;
}

// Variable values.

octave_value
set_internal_variable (bool& var, const octave_value_list& args,
		       int nargout, const char *nm)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargout > 0 || nargin == 0)
    retval = var;

  if (nargin == 1)
    {
      bool bval = args(0).bool_value ();

      if (! error_state)
	var = bval;
      else
	error ("%s: expecting arg to be a logical value", nm);
    }
  else if (nargin > 1)
    print_usage ();

  return retval;
}

octave_value
set_internal_variable (char& var, const octave_value_list& args,
		       int nargout, const char *nm)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargout > 0 || nargin == 0)
    retval = var;

  if (nargin == 1)
    {
      std::string sval = args(0).string_value ();

      if (! error_state)
	{
	  switch (sval.length ())
	    {
	    case 1:
	      var = sval[0];
	      break;

	    case 0:
	      var = '\0';
	      break;

	    default:
	      error ("%s: argument must be a single character", nm);
	      break;
	    }
	}
      else
	error ("%s: argument must be a single character", nm);
    }
  else if (nargin > 1)
    print_usage ();

  return retval;
}

octave_value
set_internal_variable (int& var, const octave_value_list& args,
		       int nargout, const char *nm,
		       int minval, int maxval)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargout > 0 || nargin == 0)
    retval = var;

  if (nargin == 1)
    {
      int ival = args(0).int_value ();

      if (! error_state)
	{
	  if (ival < minval)
	    error ("%s: expecting arg to be greater than %d", minval);
	  else if (ival > maxval)
	    error ("%s: expecting arg to be less than or equal to %d", maxval);
	  else
	    var = ival;
	}
      else
	error ("%s: expecting arg to be an integer value", nm);
    }
  else if (nargin > 1)
    print_usage ();

  return retval;
}

octave_value
set_internal_variable (double& var, const octave_value_list& args,
		       int nargout, const char *nm,
		       double minval, double maxval)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargout > 0 || nargin == 0)
    retval = var;

  if (nargin == 1)
    {
      double dval = args(0).scalar_value ();

      if (! error_state)
	{
	  if (dval < minval)
	    error ("%s: expecting arg to be greater than %g", minval);
	  else if (dval > maxval)
	    error ("%s: expecting arg to be less than or equal to %g", maxval);
	  else
	    var = dval;
	}
      else
	error ("%s: expecting arg to be a scalar value", nm);
    }
  else if (nargin > 1)
    print_usage ();

  return retval;
}

octave_value
set_internal_variable (std::string& var, const octave_value_list& args,
		       int nargout, const char *nm, bool empty_ok)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargout > 0 || nargin == 0)
    retval = var;

  if (nargin == 1)
    {
      std::string sval = args(0).string_value ();

      if (! error_state)
	{
	  if (empty_ok || ! sval.empty ())
	    var = sval;
	  else
	    error ("%s: value must not be empty", nm);
	}
      else
	error ("%s: expecting arg to be a character string", nm);
    }
  else if (nargin > 1)
    print_usage ();

  return retval;
}

struct
whos_parameter
{
  char command;
  char modifier;
  int parameter_length;
  int first_parameter_length;
  int balance;
  std::string text;
  std::string line;
};

static void
print_descriptor (std::ostream& os, std::list<whos_parameter> params)
{
  // This method prints a line of information on a given symbol
  std::list<whos_parameter>::iterator i = params.begin ();
  std::ostringstream param_buf;

  while (i != params.end ())
    {
      whos_parameter param = *i;

      if (param.command != '\0')
        {
	  // Do the actual printing
	  switch (param.modifier)
	    {
	    case 'l':
	      os << std::setiosflags (std::ios::left) << std::setw (param.parameter_length);
	      param_buf << std::setiosflags (std::ios::left) << std::setw (param.parameter_length);
	      break;

	    case 'r':
	      os << std::setiosflags (std::ios::right) << std::setw (param.parameter_length);
	      param_buf << std::setiosflags (std::ios::right) << std::setw (param.parameter_length);
	      break;

	    case 'c':
	      if (param.command != 's')
	        {
		  os << std::setiosflags (std::ios::left)
		     << std::setw (param.parameter_length);
		  param_buf << std::setiosflags (std::ios::left)
			    << std::setw (param.parameter_length);
		}
	      break;

	    default:
	      os << std::setiosflags (std::ios::left) << std::setw (param.parameter_length);
	      param_buf << std::setiosflags (std::ios::left) << std::setw (param.parameter_length);
	    }

	  if (param.command == 's' && param.modifier == 'c')
	    {
	      int a, b;
	     
	      if (param.modifier == 'c')
	        {
		  a = param.first_parameter_length - param.balance;
		  a = (a < 0 ? 0 : a);
		  b = param.parameter_length - a - param.text . length ();
		  b = (b < 0 ? 0 : b);
		  os << std::setiosflags (std::ios::left) << std::setw (a)
		     << "" << std::resetiosflags (std::ios::left) << param.text
		     << std::setiosflags (std::ios::left)
		     << std::setw (b) << ""
		     << std::resetiosflags (std::ios::left);
		  param_buf << std::setiosflags (std::ios::left) << std::setw (a)
		     << "" << std::resetiosflags (std::ios::left) << param.line
		     << std::setiosflags (std::ios::left)
		     << std::setw (b) << ""
		     << std::resetiosflags (std::ios::left);
		}
	    }
	  else
	    {
	      os << param.text;
	      param_buf << param.line;
	    }
	  os << std::resetiosflags (std::ios::left)
	     << std::resetiosflags (std::ios::right);
	  param_buf << std::resetiosflags (std::ios::left)
		    << std::resetiosflags (std::ios::right);
	  i++;
	}
      else
	{
	  os << param.text;
	  param_buf << param.line;
	  i++;
	}
    }

  os << param_buf.str ();
}

class
symbol_info_list
{
private:
  struct symbol_info
  {
    symbol_info (const symbol_table::symbol_record& sr,
		 const std::string& expr_str = std::string (),
		 const octave_value& expr_val = octave_value ())
      : name (expr_str.empty () ? sr.name () : expr_str),
	is_automatic (sr.is_automatic ()),
	is_formal (sr.is_formal ()),
	is_global (sr.is_global ()),
	is_persistent (sr.is_persistent ()),
	varval (expr_val.is_undefined () ? sr.varval () : expr_val)
    { }

    void display_line (std::ostream& os,
		       const std::list<whos_parameter>& params) const
    {
      dim_vector dims = varval.dims ();
      std::string dims_str = dims.str ();

      std::list<whos_parameter>::const_iterator i = params.begin ();

      while (i != params.end ())
	{
	  whos_parameter param = *i;

	  if (param.command != '\0')
	    {
	      // Do the actual printing.

	      switch (param.modifier)
		{
		case 'l':
		  os << std::setiosflags (std::ios::left)
		     << std::setw (param.parameter_length);
		  break;

		case 'r':
		  os << std::setiosflags (std::ios::right)
		     << std::setw (param.parameter_length);
		  break;

		case 'c':
		  if (param.command == 's')
		    {
		      int front = param.first_parameter_length
			- dims_str.find ('x');
		      int back = param.parameter_length
			- dims_str.length ()
			- front;
		      front = (front > 0) ? front : 0;
		      back = (back > 0) ? back : 0;

		      os << std::setiosflags (std::ios::left)
			 << std::setw (front)
			 << ""
			 << std::resetiosflags (std::ios::left)
			 << dims_str
			 << std::setiosflags (std::ios::left)
			 << std::setw (back)
			 << ""
			 << std::resetiosflags (std::ios::left);
		    }
		  else
		    {
		      os << std::setiosflags (std::ios::left)
			 << std::setw (param.parameter_length);
		    }
		  break;

		default:
		  error ("whos_line_format: modifier `%c' unknown",
			 param.modifier);

		  os << std::setiosflags (std::ios::right)
		     << std::setw (param.parameter_length);
		}

	      switch (param.command)
		{
		case 'a':
		  {
		    char tmp[5];

		    tmp[0] = (is_automatic ? 'a' : ' ');
		    tmp[1] = (is_formal ? 'f' : ' ');
		    tmp[2] = (is_global ? 'g' : ' ');
		    tmp[3] = (is_persistent ? 'p' : ' ');
		    tmp[4] = 0;

		    os << tmp;
		  }
		  break;

		case 'b':
		  os << varval.byte_size ();
		  break;

		case 'c':
		  os << varval.class_name ();
		  break;

		case 'e':
		  os << varval.capacity ();
		  break;

		case 'n':
		  os << name;
		  break;

		case 's':
		  if (param.modifier != 'c')
		    os << dims_str;
		  break;

		case 't':
		  os << varval.type_name ();
		  break;
	    
		default:
		  error ("whos_line_format: command `%c' unknown",
			 param.command);
		}

	      os << std::resetiosflags (std::ios::left)
		 << std::resetiosflags (std::ios::right);
	      i++;
	    }
	  else
	    {
	      os << param.text;
	      i++;
	    }
	}
    }

    std::string name;
    bool is_automatic;
    bool is_formal;
    bool is_global;
    bool is_persistent;
    octave_value varval;
  };

public:
  symbol_info_list (void) : lst () { }

  symbol_info_list (const symbol_info_list& sil) : lst (sil.lst) { }

  symbol_info_list& operator = (const symbol_info_list& sil)
  {
    if (this != &sil)
      lst = sil.lst;

    return *this;
  }

  ~symbol_info_list (void) { }

  void append (const symbol_table::symbol_record& sr)
  {
    lst.push_back (symbol_info (sr));
  }

  void append (const symbol_table::symbol_record& sr,
	       const std::string& expr_str,
	       const octave_value& expr_val)
  {
    lst.push_back (symbol_info (sr, expr_str, expr_val));
  }

  size_t size (void) const { return lst.size (); }

  bool empty (void) const { return lst.empty (); }

  Octave_map
  map_value (const std::string& caller_function_name, int nesting_level) const
  {
    size_t len = lst.size ();

    Array<octave_value> name_info (len, 1);
    Array<octave_value> size_info (len, 1);
    Array<octave_value> bytes_info (len, 1);
    Array<octave_value> class_info (len, 1);
    Array<octave_value> global_info (len, 1);
    Array<octave_value> sparse_info (len, 1);
    Array<octave_value> complex_info (len, 1);
    Array<octave_value> nesting_info (len, 1);
    Array<octave_value> persistent_info (len, 1);

    std::list<symbol_info>::const_iterator p = lst.begin ();

    for (size_t j = 0; j < len; j++)
      {
	const symbol_info& si = *p++;

	Octave_map ni;

	ni.assign ("function", caller_function_name);
	ni.assign ("level", nesting_level);

	name_info(j) = si.name;
	global_info(j) = si.is_global;
	persistent_info(j) = si.is_persistent;

	octave_value val = si.varval;

	size_info(j) = val.size ();
	bytes_info(j) = val.byte_size ();
	class_info(j) = val.class_name ();
	sparse_info(j) = val.is_sparse_type ();
	complex_info(j) = val.is_complex_type ();
	nesting_info(j) = ni;
      }

    Octave_map info;

    info.assign ("name", name_info);
    info.assign ("size", size_info);
    info.assign ("bytes", bytes_info);
    info.assign ("class", class_info);
    info.assign ("global", global_info);
    info.assign ("sparse", sparse_info);
    info.assign ("complex", complex_info);
    info.assign ("nesting", nesting_info);
    info.assign ("persistent", persistent_info);

    return info;
  }

  void display (std::ostream& os)
  {
    if (! lst.empty ())
      {
	size_t bytes = 0;
	size_t elements = 0;

	std::list<whos_parameter> params = parse_whos_line_format ();

	print_descriptor (os, params);

	octave_stdout << "\n";

	for (std::list<symbol_info>::const_iterator p = lst.begin ();
	     p != lst.end (); p++)
	  {
	    p->display_line (os, params);

	    octave_value val = p->varval;

	    elements += val.capacity ();
	    bytes += val.byte_size ();
	  }

	os << "\nTotal is " << elements
	   << (elements == 1 ? " element" : " elements")
	   << " using " << bytes << (bytes == 1 ? " byte" : " bytes")
	   << "\n";
      }
  }

  // Parse the string whos_line_format, and return a parameter list,
  // containing all information needed to print the given
  // attributtes of the symbols.
  std::list<whos_parameter> parse_whos_line_format (void)
  {
    int idx;
    size_t format_len = Vwhos_line_format.length ();
    char garbage;
    std::list<whos_parameter> params;

    size_t bytes1;
    int elements1;

    std::string param_string = "abcenst";
    Array<int> param_length (dim_vector (param_string.length (), 1));
    Array<std::string> param_names (dim_vector (param_string.length (), 1));
    size_t pos_a, pos_b, pos_c, pos_e, pos_n, pos_s, pos_t;

    pos_a = param_string.find ('a'); // Attributes
    pos_b = param_string.find ('b'); // Bytes
    pos_c = param_string.find ('c'); // Class
    pos_e = param_string.find ('e'); // Elements
    pos_n = param_string.find ('n'); // Name
    pos_s = param_string.find ('s'); // Size
    pos_t = param_string.find ('t'); // Type

    param_names(pos_a) = "Attr";
    param_names(pos_b) = "Bytes";
    param_names(pos_c) = "Class";
    param_names(pos_e) = "Elements";
    param_names(pos_n) = "Name";
    param_names(pos_s) = "Size";
    param_names(pos_t) = "Type";

    for (size_t i = 0; i < param_string.length (); i++)
      param_length(i) = param_names(i) . length ();

    // Calculating necessary spacing for name column,
    // bytes column, elements column and class column

    for (std::list<symbol_info>::const_iterator p = lst.begin ();
	 p != lst.end (); p++)
      {
	std::stringstream ss1, ss2;
	std::string str;

	str = p->name;
	param_length(pos_n) = ((str.length ()
				> static_cast<size_t> (param_length(pos_n)))
			       ? str.length () : param_length(pos_n));

	octave_value val = p->varval;

	str = val.type_name ();
	param_length(pos_t) = ((str.length ()
				> static_cast<size_t> (param_length(pos_t)))
			       ? str.length () : param_length(pos_t));

	elements1 = val.capacity ();
	ss1 << elements1;
	str = ss1.str ();
	param_length(pos_e) = ((str.length ()
				> static_cast<size_t> (param_length(pos_e)))
			       ? str.length () : param_length(pos_e));

	bytes1 = val.byte_size ();
	ss2 << bytes1;
	str = ss2.str ();
	param_length(pos_b) = ((str.length ()
				> static_cast<size_t> (param_length(pos_b)))
			       ? str.length () : param_length (pos_b));
      }

    idx = 0;
    while (static_cast<size_t> (idx) < format_len)
      {
	whos_parameter param;
	param.command = '\0';

	if (Vwhos_line_format[idx] == '%')
	  {
	    bool error_encountered = false;
	    param.modifier = 'r';
	    param.parameter_length = 0;

	    int a = 0, b = -1, balance = 1;
	    unsigned int items;
	    size_t pos;
	    std::string cmd;

	    // Parse one command from whos_line_format
	    cmd = Vwhos_line_format.substr (idx, Vwhos_line_format.length ());
	    pos = cmd.find (';');
	    if (pos != NPOS)
	      cmd = cmd.substr (0, pos+1);
	    else
	      error ("parameter without ; in whos_line_format");

	    idx += cmd.length ();

	    // FIXME -- use iostream functions instead of sscanf!

	    if (cmd.find_first_of ("crl") != 1)
	      items = sscanf (cmd.c_str (), "%c%c:%d:%d:%d;",
			      &garbage, &param.command, &a, &b, &balance);
	    else
	      items = sscanf (cmd.c_str (), "%c%c%c:%d:%d:%d;",
			      &garbage, &param.modifier, &param.command,
			      &a, &b, &balance) - 1;

	    if (items < 2)
	      {
		error ("whos_line_format: parameter structure without command in whos_line_format");
		error_encountered = true;
	      }

	    // Insert data into parameter
	    param.first_parameter_length = 0;
	    pos = param_string.find (param.command);
	    if (pos != NPOS)
	      {
		param.parameter_length = param_length(pos);
		param.text = param_names(pos);
		param.line.assign (param_names(pos).length (), '=');

		param.parameter_length = (a > param.parameter_length
					  ? a : param.parameter_length);
		if (param.command == 's' && param.modifier == 'c' && b > 0)
		  param.first_parameter_length = b;
	      }
	    else
	      {
		error ("whos_line_format: '%c' is not a command",
		       param.command);
		error_encountered = true;
	      }

	    if (param.command == 's')
	      {
		// Have to calculate space needed for printing
		// matrix dimensions Space needed for Size column is
		// hard to determine in prior, because it depends on
		// dimensions to be shown. That is why it is
		// recalculated for each Size-command int first,
		// rest = 0, total;
		int rest = 0;
		int first = param.first_parameter_length;
		int total = param.parameter_length;

		for (std::list<symbol_info>::const_iterator p = lst.begin ();
		     p != lst.end (); p++)
		  {
		    octave_value val = p->varval;
		    dim_vector dims = val.dims ();
		    std::string dims_str = dims.str ();
		    int first1 = dims_str.find ('x');
		    int total1 = dims_str.length ();
		    int rest1 = total1 - first1;
		    rest = (rest1 > rest ? rest1 : rest);
		    first = (first1 > first ? first1 : first);
		    total = (total1 > total ? total1 : total);
		  }

		if (param.modifier == 'c')
		  {
		    if (first < balance)
		      first += balance - first;
		    if (rest + balance < param.parameter_length)
		      rest += param.parameter_length - rest - balance;

		    param.parameter_length = first + rest;
		    param.first_parameter_length = first;
		    param.balance = balance;
		  }
		else
		  {
		    param.parameter_length = total;
		    param.first_parameter_length = 0;
		  }
	      }
	    else if (param.modifier == 'c')
	      {
		error ("whos_line_format: modifier 'c' not available for command '%c'",
		       param.command);
		error_encountered = true;
	      }

	    // What happens if whos_line_format contains negative numbers
	    // at param_length positions?
	    param.balance = (b < 0 ? 0 : param.balance);
	    param.first_parameter_length = (b < 0 ? 0 :
					    param.first_parameter_length);
	    param.parameter_length = (a < 0
				      ? 0
				      : (param.parameter_length
					 < param_length(pos_s)
					 ? param_length(pos_s)
					 : param.parameter_length));

	    // Parameter will not be pushed into parameter list if ...
	    if (! error_encountered)
	      params.push_back (param);
	  }
	else
	  {
	    // Text string, to be printed as it is ...
	    std::string text;
	    size_t pos;
	    text = Vwhos_line_format.substr (idx, Vwhos_line_format.length ());
	    pos = text.find ('%');
	    if (pos != NPOS)
	      text = text.substr (0, pos);

	    // Push parameter into list ...
	    idx += text.length ();
	    param.text=text;
	    param.line.assign (text.length(), ' ');
	    params.push_back (param);
	  }
      }

    return params;
  }

private:
  std::list<symbol_info> lst;

};

static octave_value
do_who (int argc, const string_vector& argv, bool return_list,
	bool verbose = false)
{
  octave_value retval;

  std::string my_name = argv[0];

  bool global_only = false;

  int i;
  for (i = 1; i < argc; i++)
    {
      if (argv[i] == "-regexp" || argv[i] == "-file")
	{
	  error ("%s: `%s' option not implemented", my_name.c_str (),
		 argv[i].c_str ());

	  return retval;
	}
      else if (argv[i] == "global")
	global_only = true;
      else if (argv[i][0] == '-')
	warning ("%s: unrecognized option `%s'", my_name.c_str (),
		 argv[i].c_str ());
      else
	break;
    }

  int npats = argc - i;
  string_vector pats;
  if (npats > 0)
    {
      pats.resize (npats);
      for (int j = 0; j < npats; j++)
	pats[j] = argv[i+j];
    }
  else
    {
      pats.resize (++npats);
      pats[0] = "*";
    }
    
  symbol_table::scope_id scope = global_only
    ? symbol_table::global_scope () : symbol_table::current_scope ();

  symbol_info_list symbol_stats;
  std::list<std::string> symbol_names;

  for (int j = 0; j < npats; j++)
    {
      std::string pat = pats[j];

      size_t pos = pat.find_first_of (".({");

      if (pos != NPOS && pos > 0)
        {
	  if (verbose)
	    {
	      // NOTE: we can only display information for
	      // expressions based on global values if the variable is
	      // global in the current scope because we currently have
	      // no way of looking up the base value in the global
	      // scope and then evaluating the arguments in the
	      // current scope.

	      std::string base_name = pat.substr (0, pos);

	      if (symbol_table::is_variable (base_name))
		{
		  symbol_table::symbol_record sr
		    = symbol_table::find_symbol (base_name);

		  if (! global_only || sr.is_global ())
		    {
		      int parse_status;

		      octave_value expr_val
			= eval_string (pat, true, parse_status);

		      if (! error_state)
			symbol_stats.append (sr, pat, expr_val);
		      else
			return retval;
		    }
		}
	    }
	}
      else
	{
	  std::list<symbol_table::symbol_record> tmp
	    = symbol_table::glob_variables (pats[j], scope);

	  for (std::list<symbol_table::symbol_record>::const_iterator p = tmp.begin ();
	       p != tmp.end (); p++)
	    {
	      if (verbose)
		symbol_stats.append (*p);
	      else
		symbol_names.push_back (p->name ());
	    }
	}
    }

  if (return_list)
    {
      if (verbose)
	{
	  std::string caller_function_name;
	  octave_function *caller = octave_call_stack::caller ();
	  if (caller)
	    caller_function_name = caller->name ();

	  retval = symbol_stats.map_value (caller_function_name, 1);
	}
      else
	retval = Cell (string_vector (symbol_names));
    }
  else if (! (symbol_stats.empty () && symbol_names.empty ()))
    {
      if (global_only)
	octave_stdout << "Global variables:\n\n";
      else
	octave_stdout << "Variables in the current scope:\n\n";

      if (verbose)
	symbol_stats.display (octave_stdout);
      else
	{
	  string_vector names (symbol_names);

	  names.list_in_columns (octave_stdout);
	}

      octave_stdout << "\n";
    }

  return retval;
}

DEFCMD (who, args, nargout,
  "-*- texinfo -*-\n\
@deffn {Command} who options pattern @dots{}\n\
@deffnx {Command} whos options pattern @dots{}\n\
List currently defined symbols matching the given patterns.  The\n\
following are valid options.  They may be shortened to one character but\n\
may not be combined.\n\
\n\
@table @code\n\
@item -all\n\
List all currently defined symbols.\n\
\n\
@item -builtins\n\
List built-in functions.  This includes all currently\n\
compiled function files, but does not include all function files that\n\
are in the search path.\n\
\n\
@item -functions\n\
List user-defined functions.\n\
\n\
@item -long\n\
Print a long listing including the type and dimensions of any symbols.\n\
The symbols in the first column of output indicate whether it is\n\
possible to redefine the symbol, and whether it is possible for it to be\n\
cleared.\n\
\n\
@item -variables\n\
List user-defined variables.\n\
@end table\n\
\n\
Valid patterns are the same as described for the @code{clear} command\n\
above.  If no patterns are supplied, all symbols from the given category\n\
are listed.  By default, only user defined functions and variables\n\
visible in the local scope are displayed.\n\
\n\
The command @kbd{whos} is equivalent to @kbd{who -long}.\n\
@end deffn")
{
  octave_value retval;

  if (nargout < 2)
    {
      int argc = args.length () + 1;

      string_vector argv = args.make_argv ("who");

      if (! error_state)
	retval = do_who (argc, argv, nargout == 1);
    }
  else
    print_usage ();

  return retval;
}

DEFCMD (whos, args, nargout,
  "-*- texinfo -*-\n\
@deffn {Command} whos options pattern @dots{}\n\
See who.\n\
@end deffn")
{
  octave_value retval;

  if (nargout < 2)
    {
      int argc = args.length () + 1;

      string_vector argv = args.make_argv ("whos");

      if (! error_state)
	retval = do_who (argc, argv, nargout == 1, true);
    }
  else
    print_usage ();

  return retval;
}

// Defining variables.

void
bind_ans (const octave_value& val, bool print)
{
  static std::string ans = "ans";

  if (val.is_defined ())
    {
      if (val.is_cs_list ())
	{
	  octave_value_list lst = val.list_value ();

	  for (octave_idx_type i = 0; i < lst.length (); i++)
	    bind_ans (lst(i), print);
	}
      else
	{
	  symbol_table::varref (ans) = val;

	  if (print)
	    val.print_with_name (octave_stdout, ans);
	}
    }
}

void
bind_internal_variable (const std::string& fname, const octave_value& val)
{
  octave_value_list args;

  args(0) = val;

  feval (fname, args, 0);
}

void 
mlock (void)
{
  octave_function *fcn = octave_call_stack::caller ();

  if (fcn)
    fcn->lock ();
  else
    error ("mlock: invalid use outside a function");
}

void 
munlock (const std::string& nm)
{
  octave_value val = symbol_table::find_function (nm);

  if (val.is_defined ())
    {
      octave_function *fcn = val.function_value ();

      if (fcn)
	fcn->unlock ();
    }
}

bool
mislocked (const std::string& nm)
{
  bool retval = false;

  octave_value val = symbol_table::find_function (nm);

  if (val.is_defined ())
    {
      octave_function *fcn = val.function_value ();

      if (fcn)
	retval = fcn->islocked ();
    }

  return retval;
}

DEFCMD (mlock, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} mlock (@var{name})\n\
Lock the current function into memory so that it can't be cleared.\n\
@seealso{munlock, mislocked, persistent}\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 0)
    mlock ();
  else
    print_usage ();

  return retval;
}

DEFCMD (munlock, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} munlock (@var{fcn})\n\
Unlock the named function.  If no function is named\n\
then unlock the current function.\n\
@seealso{mlock, mislocked, persistent}\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length() == 1)
    {
      std::string name = args(0).string_value ();

      if (! error_state)
        munlock (name);
      else
	error ("munlock: expecting argument to be a function name");
    }
  else if (args.length () == 0)
    {
      octave_function *fcn = octave_call_stack::caller ();

      if (fcn)
        fcn->unlock ();
      else
        error ("munlock: invalid use outside a function");
    }
  else
    print_usage ();

  return retval;
}


DEFCMD (mislocked, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} mislocked (@var{fcn})\n\
Return true if the named function is locked.  If no function is named\n\
then return true if the current function is locked.\n\
@seealso{mlock, munlock, persistent}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length() == 1)
    {
      std::string name = args(0).string_value ();

      if (! error_state)
        retval = mislocked (name);
      else
	error ("mislocked: expecting argument to be a function name");
    }
  else if (args.length () == 0)
    {
      octave_function *fcn = octave_call_stack::caller ();

      if (fcn)
        retval = fcn->islocked ();
      else
        error ("mislocked: invalid use outside a function");
    }
  else
    print_usage ();

  return retval;
}

// Deleting names from the symbol tables.

static inline bool
name_matches_any_pattern (const std::string& nm,
			  const string_vector& argv, int argc, int idx)
{
  bool retval = false;

  for (int k = idx; k < argc; k++)
    {
      std::string patstr = argv[k];

      if (! patstr.empty ())
	{
	  glob_match pattern (patstr);

	  if (pattern.match (nm))
	    {
	      retval = true;
	      break;
	    }
	}
    }

  return retval;
}

static inline void
maybe_warn_exclusive (bool exclusive)
{
  if (exclusive)
    warning ("clear: ignoring --exclusive option");
}

static void
do_clear_functions (const string_vector& argv, int argc, int idx,
		    bool exclusive = false)
{
  if (idx == argc)
    symbol_table::clear_functions ();
  else
    {
      if (exclusive)
	{
	  string_vector fcns = symbol_table::user_function_names ();

	  int fcount = fcns.length ();

	  for (int i = 0; i < fcount; i++)
	    {
	      std::string nm = fcns[i];

	      if (! name_matches_any_pattern (nm, argv, argc, idx))
		symbol_table::clear_function (nm);
	    }
	}
      else
	{
	  while (idx < argc)
	    symbol_table::clear_function_pattern (argv[idx++]);
	}
    }
}

static void
do_clear_globals (const string_vector& argv, int argc, int idx,
		  bool exclusive = false)
{
  if (idx == argc)
    symbol_table::clear_variables(symbol_table::global_scope ());
  else
    {
      if (exclusive)
	{
	  string_vector gvars
	    = symbol_table::variable_names (symbol_table::global_scope ());

	  int gcount = gvars.length ();

	  for (int i = 0; i < gcount; i++)
	    {
	      std::string nm = gvars[i];

	      if (! name_matches_any_pattern (nm, argv, argc, idx))
		symbol_table::clear_global (nm);
	    }
	}
      else
	{
	  while (idx < argc)
	    symbol_table::clear_global_pattern (argv[idx++]);
	}
    }
}

static void
do_clear_variables (const string_vector& argv, int argc, int idx,
		    bool exclusive = false)
{
  if (idx == argc)
    symbol_table::clear_variables ();
  else
    {
      if (exclusive)
	{
	  string_vector lvars = symbol_table::variable_names ();

	  int lcount = lvars.length ();

	  for (int i = 0; i < lcount; i++)
	    {
	      std::string nm = lvars[i];

	      if (! name_matches_any_pattern (nm, argv, argc, idx))
		symbol_table::clear_variable (nm);
	    }
	}
      else
	{
	  while (idx < argc)
	    symbol_table::clear_variable_pattern (argv[idx++]);
	}
    }
}

static void
do_clear_symbols (const string_vector& argv, int argc, int idx,
		  bool exclusive = false)
{
  if (idx == argc)
    symbol_table::clear_variables ();
  else
    {
      if (exclusive)
	{
	  // FIXME -- is this really what we want, or do we
	  // somehow want to only clear the functions that are not
	  // shadowed by local variables?  It seems that would be a
	  // bit harder to do.

	  do_clear_variables (argv, argc, idx, exclusive);
	  do_clear_functions (argv, argc, idx, exclusive);
	}
      else
	{
	  while (idx < argc)
	    symbol_table::clear_symbol_pattern (argv[idx++]);
	}
    }
}

static void
do_matlab_compatible_clear (const string_vector& argv, int argc, int idx)
{
  // This is supposed to be mostly Matlab compatible.

  for (; idx < argc; idx++)
    {
      if (argv[idx] == "all"
	  && ! symbol_table::is_local_variable ("all"))
	{
	  symbol_table::clear_all ();
	}
      else if (argv[idx] == "functions"
	       && ! symbol_table::is_local_variable ("functions"))
	{
	  do_clear_functions (argv, argc, ++idx);
	}
      else if (argv[idx] == "global"
	       && ! symbol_table::is_local_variable ("global"))
	{
	  do_clear_globals (argv, argc, ++idx);
	}
      else if (argv[idx] == "variables"
	       && ! symbol_table::is_local_variable ("variables"))
	{
	  symbol_table::clear_variables ();
	}
      else
	{
	  symbol_table::clear_symbol_pattern (argv[idx]);
	}
    }
}

#define CLEAR_OPTION_ERROR(cond) \
  do \
    { \
      if (cond) \
        { \
          print_usage (); \
          return retval; \
        } \
    } \
  while (0)

DEFCMD (clear, args, ,
  "-*- texinfo -*-\n\
@deffn {Command} clear [options] pattern @dots{}\n\
Delete the names matching the given patterns from the symbol table.  The\n\
pattern may contain the following special characters:\n\
\n\
@table @code\n\
@item ?\n\
Match any single character.\n\
\n\
@item *\n\
Match zero or more characters.\n\
\n\
@item [ @var{list} ]\n\
Match the list of characters specified by @var{list}.  If the first\n\
character is @code{!} or @code{^}, match all characters except those\n\
specified by @var{list}.  For example, the pattern @samp{[a-zA-Z]} will\n\
match all lower and upper case alphabetic characters.\n\
@end table\n\
\n\
For example, the command\n\
\n\
@example\n\
clear foo b*r\n\
@end example\n\
\n\
@noindent\n\
clears the name @code{foo} and all names that begin with the letter\n\
@code{b} and end with the letter @code{r}.\n\
\n\
If @code{clear} is called without any arguments, all user-defined\n\
variables (local and global) are cleared from the symbol table.  If\n\
@code{clear} is called with at least one argument, only the visible\n\
names matching the arguments are cleared.  For example, suppose you have\n\
defined a function @code{foo}, and then hidden it by performing the\n\
assignment @code{foo = 2}.  Executing the command @kbd{clear foo} once\n\
will clear the variable definition and restore the definition of\n\
@code{foo} as a function.  Executing @kbd{clear foo} a second time will\n\
clear the function definition.\n\
\n\
The following options are available in both long and short form\n\
@table @code\n\
@item -all, -a\n\
Clears all local and global user-defined variables and all functions\n\
from the symbol table.\n\
\n\
@item -exclusive, -x\n\
Clears the variables that don't match the following pattern.\n\
\n\
@item -functions, -f\n\
Clears the function names and the built-in symbols names.\n\
@item -global, -g\n\
Clears the global symbol names.\n\
@item -variables, -v\n\
Clears the local variable names.\n\
@end table\n\
With the execption of @code{exclusive}, all long options can be used \n\
without the dash as well.\n\
@end deffn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("clear");

  if (! error_state)
    {
      if (argc == 1)
	{
	  symbol_table::clear_variables ();
	}
      else
	{
	  int idx = 0;

	  bool clear_all = false;
	  bool clear_functions = false;
	  bool clear_globals = false;
	  bool clear_variables = false;
	  bool exclusive = false;
	  bool have_dash_option = false;

	  while (++idx < argc)
	    {
	      if (argv[idx] == "-all" || argv[idx] == "-a")
		{
		  CLEAR_OPTION_ERROR (have_dash_option && ! exclusive);

		  have_dash_option = true;
		  clear_all = true;
		}
	      else if (argv[idx] == "-exclusive" || argv[idx] == "-x")
		{
		  have_dash_option = true;
		  exclusive = true;
		}
	      else if (argv[idx] == "-functions" || argv[idx] == "-f")
		{
		  CLEAR_OPTION_ERROR (have_dash_option && ! exclusive);

		  have_dash_option = true;
		  clear_functions = true;
		}
	      else if (argv[idx] == "-global" || argv[idx] == "-g")
		{
		  CLEAR_OPTION_ERROR (have_dash_option && ! exclusive);

		  have_dash_option = true;
		  clear_globals = true;
		}
	      else if (argv[idx] == "-variables" || argv[idx] == "-v")
		{
		  CLEAR_OPTION_ERROR (have_dash_option && ! exclusive);

		  have_dash_option = true;
		  clear_variables = true;
		}
	      else
		break;
	    }

	  if (idx <= argc)
	    {
	      if (! have_dash_option)
		{
		  do_matlab_compatible_clear (argv, argc, idx);
		}
	      else
		{
		  if (clear_all)
		    {
		      maybe_warn_exclusive (exclusive);

		      if (++idx < argc)
			warning
			  ("clear: ignoring extra arguments after -all");

		      symbol_table::clear_all ();
		    }
		  else if (clear_functions)
		    {
		      do_clear_functions (argv, argc, idx, exclusive);
		    }
		  else if (clear_globals)
		    {
		      do_clear_globals (argv, argc, idx, exclusive);
		    }
		  else if (clear_variables)
		    {
		      do_clear_variables (argv, argc, idx, exclusive);
		    }
		  else
		    {
		      do_clear_symbols (argv, argc, idx, exclusive);
		    }
		}
	    }
	}
    }

  return retval;
}

DEFUN (__print_symtab_info__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __print_symtab_info__ ()\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value_list retval;

  // FIXME -- what should this function do now?  Print a summary for
  // each scope?  Print the entire symbol table?  Accept a scope
  // argument?

  return retval;
}

DEFUN (__print_symbol_info__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __dump_symbol_info__ (@var{name})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value_list retval;

  // FIXME -- what should this function do now?

  return retval;
}

DEFUN (whos_line_format, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} whos_line_format ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} whos_line_format (@var{new_val})\n\
Query or set the format string used by the @code{whos}.\n\
\n\
The following escape sequences may be used in the format:\n\
@table @code\n\
@item %a\n\
Prints attributes of variables (g=global, p=persistent,\n\
f=formal parameter, a=automatic variable).\n\
@item %b\n\
Prints number of bytes occupied by variables.\n\
@item %c\n\
Prints class names of variables.\n\
@item %e\n\
Prints elements held by variables.\n\
@item %n\n\
Prints variable names.\n\
@item %s\n\
Prints dimensions of variables.\n\
@item %t\n\
Prints type names of variables.\n\
@end table\n\
\n\
Every command may also have a modifier:\n\
@table @code\n\
@item l\n\
Left alignment.\n\
@item r\n\
Right alignment (this is the default).\n\
@item c\n\
Centered (may only be applied to command %s).\n\
@end table\n\
\n\
A command is composed like this:\n\
\n\
@example\n\
%[modifier]<command>[:size_of_parameter[:center-specific[:balance]]];\n\
@end example\n\
\n\
Command and modifier is already explained. Size_of_parameter\n\
tells how many columns the parameter will need for printing.\n\
The @code{center-specific} parameter may only be applied to command\n\
@samp{%s}.\n\
The @code{balance} parameter specifies the offset for printing\n\
the dimensions string.\n\
\n\
The default format is \"  %a:4; %ln:6; %cs:16:6:1;  %rb:12;  %lc:-1;\\n\".\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (whos_line_format);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
