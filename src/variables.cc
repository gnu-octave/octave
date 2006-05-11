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

#include <cstdio>
#include <cstring>

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

// Symbol table for symbols at the top level.
symbol_table *top_level_sym_tab = 0;

// Symbol table for the current scope.
symbol_table *curr_sym_tab = 0;

// Symbol table for the current caller scope.
symbol_table *curr_caller_sym_tab = 0;

// Symbol table for global symbols.
symbol_table *global_sym_tab = 0;

// Symbol table for functions and built-in symbols.
symbol_table *fbi_sym_tab = 0;

bool
at_top_level (void)
{
  return (curr_sym_tab == top_level_sym_tab);
}

// Initialization.

// Create the initial symbol tables and set the current scope at the
// top level.

void
initialize_symbol_tables (void)
{
  if (! fbi_sym_tab)
    fbi_sym_tab = new symbol_table (2048, "FBI");

  if (! global_sym_tab)
    global_sym_tab = new symbol_table (2048, "GLOBAL");

  if (! top_level_sym_tab)
    top_level_sym_tab = new symbol_table (4096, "TOP");

  curr_caller_sym_tab = curr_sym_tab = top_level_sym_tab;
}

// Attributes of variables and functions.

// Is this a command-style function?

static std::set <std::string> command_set;

static inline bool
is_marked_as_command (const std::string& s)
{
  return command_set.find (s) != command_set.end ();
}

static inline void
mark_as_command (const std::string& s)
{
  command_set.insert (s);
}

static inline void
unmark_command (const std::string& s)
{
  command_set.erase (s);

  symbol_record *sr = fbi_sym_tab->lookup (s);

  if (sr)
    sr->unmark_command ();
}

DEFCMD (mark_as_command, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} mark_as_command (@var{name})\n\
Enter @var{name} into the list of commands.\n\
@seealso{unmark_command, iscommand}\n\
@end deftypefn")
{
  octave_value_list retval;

  if (at_top_level ())
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
	print_usage ("mark_as_command");
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

  if (at_top_level ())
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
	print_usage ("unmark_command");
    }
  else
    warning ("mark_as_command: invalid use inside function body");

  return retval;
}

bool
is_command_name (const std::string& s)
{
  bool retval = false;

  symbol_record *sr = fbi_sym_tab->lookup (s);

  if (sr)
    {
      if (sr->is_command ())
	retval = true;
      else if (is_marked_as_command (s))
	{
	  sr->mark_as_command ();
	  retval = true;
	}
    }
  else
    retval = is_marked_as_command (s);

  return retval;
}

DEFCMD (iscommand, args, ,
"-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} iscommand (@var{name})\n\
Return true if @var{name} is a command style function.  If @var{name}\n\
is omitted, return a list of identifiers which are marked as commands with\n\
mark_as_command.\n\
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
    print_usage ("iscommand");

  return retval;
}

// Is this a raw input command?

static std::set <std::string> rawcommand_set;

bool
is_marked_as_rawcommand (const std::string& s)
{
  return rawcommand_set.find (s) != rawcommand_set.end ();
}

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

  symbol_record *sr = fbi_sym_tab->lookup (s);

  if (sr)
    sr->unmark_rawcommand ();
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

  if (at_top_level ())
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
	print_usage ("mark_as_rawcommand");
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

  if (at_top_level ())
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
	print_usage ("unmark_rawcommand");
    }
  else
    warning ("unmark_rawcommand: invalid use inside function body");

  return retval;
}

bool
is_rawcommand_name (const std::string& s)
{
  bool retval = false;

  symbol_record *sr = fbi_sym_tab->lookup (s);

  if (sr)
    {
      if (sr->is_rawcommand ())
	retval = true;
      else if (is_marked_as_rawcommand (s))
	{
	  sr->mark_as_rawcommand ();
	  retval = true;
	}
    }
  else
    retval = is_marked_as_rawcommand (s);

  return retval;
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
    print_usage ("israwcommand");

  return retval;
}

// Is this a built-in function?

bool
is_builtin_function_name (const std::string& s)
{
  symbol_record *sr = fbi_sym_tab->lookup (s);
  return (sr && sr->is_builtin_function ());
}

// Is this a mapper function?

bool
is_mapper_function_name (const std::string& s)
{
  symbol_record *sr = fbi_sym_tab->lookup (s);
  return (sr && sr->is_mapper_function ());
}

// Is this function globally in this scope?

bool
is_globally_visible (const std::string& name)
{
  symbol_record *sr = curr_sym_tab->lookup (name);
  return (sr && sr->is_linked_to_global ());
}

// Is this octave_value a valid function?

octave_function *
is_valid_function (const std::string& fcn_name,
		   const std::string& warn_for, bool warn)
{
  octave_function *ans = 0;

  symbol_record *sr = 0;

  if (! fcn_name.empty ())
    {
      sr = fbi_sym_tab->lookup (fcn_name, true);

      lookup (sr, false);
    }

  if (sr)
    {
      octave_value tmp = sr->def ();
      ans = tmp.function_value (true);
    }

  if (! sr || ! ans || ! sr->is_function ())
    {
      if (warn)
	error ("%s: the symbol `%s' is not valid as a function",
	       warn_for.c_str (), fcn_name.c_str ());
      ans = 0;
    }

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
      symbol_record *sr = curr_sym_tab->lookup (name);

      if (! sr)
	sr = fbi_sym_tab->lookup (name);

      retval = (sr  && sr->is_variable ());
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
  octave_value retval = false;

  int nargin = args.length ();

  if (nargin != 1)
    {
      print_usage ("isglobal");
      return retval;
    }

  std::string name = args(0).string_value ();

  if (error_state)
    {
      error ("isglobal: expecting std::string argument");
      return retval;
    }

  symbol_record *sr = curr_sym_tab->lookup (name);

  retval = (sr && sr->is_linked_to_global ());

  return retval;
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

  symbol_record *sr = curr_sym_tab->lookup (symbol_name);

  if (! (sr && sr->is_defined ()))
    sr = fbi_sym_tab->lookup (symbol_name);

  if (sr && sr->is_defined ())
    {
      bool not_a_struct = struct_elts.empty ();
      bool var_ok = not_a_struct || sr->is_map_element (struct_elts);

      if (! retval
	  && var_ok
	  && (type == "any" || type == "var")
	  && sr->is_user_variable ())
	{
	  retval = 1;
	}

      if (! retval
	  && (type == "any" || type == "builtin"))
	{
	  if (not_a_struct && sr->is_builtin_function ())
	    {
	      retval = 5;
	    }
	}

      if (! retval
	  && not_a_struct
	  && (type == "any" || type == "file")
	  && (sr->is_user_function () || sr->is_dld_function ()))
	{
	  octave_value t = sr->def ();
	  octave_function *f = t.function_value (true);
	  std::string s = f ? f->fcn_file_name () : std::string ();

	  retval = s.empty () ? 103 : (sr->is_user_function () ? 2 : 3);
	}
    }

  if (! (type == "var" || type == "builtin"))
    {
      if (! retval)
	{
	  std::string file_name = lookup_autoload (name);

	  if (file_name.empty ())
	    {
	      string_vector names (2);

	      names(0) = name + ".oct";
	      names(1) = name + ".m";

	      file_name = Vload_path_dir_path.find_first_of (names);
	    }

	  size_t len = file_name.length ();

	  if (! file_name.empty ())
	    {
	      if (type == "any" || type == "file")
		{
		  if (file_name.substr (len-4) == ".oct")
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
Return 1 if the name exists as a variable, 2 if the name (after\n\
appending @samp{.m}) is a function file in Octave's @code{path}, 3 if the\n\
name is a @samp{.oct} file in Octave's @code{path}, 5 if the name is a\n\
built-in function, 7 if the name is a directory, or 103\n\
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
    print_usage ("exist");

  return retval;
}

// Return TRUE if F and G are both names for the same file.

static bool
same_file (const std::string& f, const std::string& g)
{
  std::string c_f = file_ops::canonicalize_file_name (f);
  std::string c_g = file_ops::canonicalize_file_name (g);

  file_stat f_fs (c_f);
  file_stat g_fs (c_g);

  return (f_fs.ino () == g_fs.ino () && f_fs.dev () == g_fs.dev ());
}

bool
fcn_out_of_date (octave_function *fcn, const std::string& ff, time_t tp)
{
  bool retval = false;

  fcn->mark_fcn_file_up_to_date (octave_time ());

  if (! (Vignore_function_time_stamp == 2
	 || (Vignore_function_time_stamp && fcn->is_system_fcn_file ())))
    {
      file_stat fs (ff);

      if (fs && fs.is_newer (tp))
	retval = true;
    }

  return retval;
}

// Is there a corresponding function file that is newer than the
// symbol definition?

static bool
symbol_out_of_date (symbol_record *sr)
{
  bool retval = false;

  if (sr)
    {
      octave_value ans = sr->def ();

      octave_function *fcn = ans.function_value (true);

      // No need to check nested functions.  They can only be executed
      // from within the parent function that contains them.  Parent
      // and nested functions will be updated simultaneously when we
      // check the parent.

      if (fcn && ! fcn->is_nested_function ())
	{
	  std::string ff = fcn->fcn_file_name ();

	  if (! ff.empty ())
	    {
	      if (fcn->time_checked () < Vlast_prompt_time)
		{
		  time_t tp = fcn->time_parsed ();

		  std::string nm = fcn->name ();

		  // FIXME -- the following code is repeated
		  // in load_fcn_from_file in parse.y.

		  string_vector names (2);

		  int nm_len = nm.length ();

		  std::string file;

		  if (octave_env::absolute_pathname (nm)
		      && ((nm_len > 4 && nm.substr (nm_len-4) == ".oct")
			  || (nm_len > 2 && nm.substr (nm_len-4) == ".m")))
		    {
		      file = nm;
		    }
		  else
		    {
		      file = lookup_autoload (nm);

		      if (file.empty ())
			{
			  names[0] = nm + ".oct";
			  names[1] = nm + ".m";

			  file = octave_env::make_absolute
			    (Vload_path_dir_path.find_first_of (names),
			     octave_env::getcwd ());
			}
		    }

		  if (same_file (file, ff))
		    {
		      retval = fcn_out_of_date (fcn, ff, tp);
		    }
		  else
		    {
		      // Check the full function name.  Maybe we alrady
		      // parsed it.

		      symbol_record *full_sr = fbi_sym_tab->lookup (file);

		      if (full_sr)
			{
			  octave_value v = full_sr->def ();

			  if (v.is_function ())
			    {
			      // OK, swap the aliases around.

			      // FIXME -- this is a bit
			      // tricky, so maybe some refactoring is
			      // in order here too...

			      symbol_record *short_sr = fbi_sym_tab->lookup (nm);

			      if (short_sr)
				short_sr->alias (full_sr);

			      // Make local symbol table entry point
			      // to correct global function too.

			      sr->alias (full_sr);

			      fcn = v.function_value ();

			      retval = fcn_out_of_date (fcn, file, tp);
			    }
			  else
			    retval = true;
			}
		      else
			retval = true;
		    }
		}
	    }
	}
    }

  return retval;
}

bool
lookup (symbol_record *sym_rec, bool exec_script)
{
  bool script_executed = false;

  if (! sym_rec->is_linked_to_global ())
    {
      if (sym_rec->is_defined ())
	{
	  if (sym_rec->is_function () && symbol_out_of_date (sym_rec))
	    script_executed = load_fcn_from_file (sym_rec, exec_script);
	}
      else if (! sym_rec->is_formal_parameter ())
	{
	  link_to_builtin_or_function (sym_rec);

	  if (! sym_rec->is_defined ())
	    script_executed = load_fcn_from_file (sym_rec, exec_script);
	  else if (sym_rec->is_function () && symbol_out_of_date (sym_rec))
	    script_executed = load_fcn_from_file (sym_rec, exec_script);
	}
    }

  return script_executed;
}

// Get the symbol record for the given name that is visible in the
// current scope.  Reread any function definitions that appear to be
// out of date.  If a function is available in a file but is not
// currently loaded, this will load it and insert the name in the
// current symbol table.

symbol_record *
lookup_by_name (const std::string& nm, bool exec_script)
{
  symbol_record *sym_rec = curr_sym_tab->lookup (nm, true);

  lookup (sym_rec, exec_script);

  return sym_rec;
}

octave_value
lookup_function (const std::string& nm)
{
  octave_value retval;

  symbol_record *sr = 0;

  if (curr_parent_function)
    {
      std::string parent = curr_parent_function->name ();

      sr = fbi_sym_tab->lookup (parent + ":" + nm);
    }

  if (! sr || ! sr->is_function ())
    {
      sr = fbi_sym_tab->lookup (nm, true);

      if (sr && ! sr->is_function ())
	load_fcn_from_file (sr, false);
    }

  if (sr)
    {
      octave_value v = sr->def ();

      if (v.is_function ())
	retval = v;
    }

  return retval;
}

octave_value
lookup_user_function (const std::string& nm)
{
  octave_value retval;

  symbol_record *sr = 0;

  if (curr_parent_function)
    {
      std::string parent = curr_parent_function->name ();

      sr = fbi_sym_tab->lookup (parent + ":" + nm);
    }

  if (! sr || ! sr->is_user_function ())
    {
      sr = fbi_sym_tab->lookup (nm, true);

      if (sr && ! sr->is_user_function ())
	load_fcn_from_file (sr, false);
    }

  if (sr)
    retval = sr->def ();

  return retval;
}

octave_value
lookup_function_handle (const std::string& nm)
{
  octave_value retval;

  symbol_record *sr = curr_sym_tab->lookup (nm, true);

  if (sr && sr->def ().is_function_handle ())
    retval = sr->def ();

  return retval;
}

octave_value
get_global_value (const std::string& nm, bool silent)
{
  octave_value retval;

  symbol_record *sr = global_sym_tab->lookup (nm);

  if (sr)
    {
      octave_value sr_def = sr->def ();

      if (sr_def.is_defined ())
	retval = sr_def;
      else if (! silent)
	error ("get_global_by_name: undefined symbol `%s'", nm.c_str ());
    }
  else if (! silent)
    error ("get_global_by_name: unknown symbol `%s'", nm.c_str ());

  return retval;
}

void
set_global_value (const std::string& nm, const octave_value& val)
{
  symbol_record *sr = global_sym_tab->lookup (nm, true);

  if (sr)
    sr->define (val);
  else
    panic_impossible ();
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
    print_usage (nm);

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
    print_usage (nm);

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
    print_usage (nm);

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
    print_usage (nm);

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
    print_usage (nm);

  return retval;
}

// Global stuff and links to builtin variables and functions.

// Make the definition of the symbol record sr be the same as the
// definition of the global variable of the same name, creating it if
// it doesn't already exist.

void
link_to_global_variable (symbol_record *sr)
{
  if (! sr->is_linked_to_global ())
    {
      sr->mark_as_linked_to_global ();

      if (! error_state)
	{
	  std::string nm = sr->name ();

	  symbol_record *gsr = global_sym_tab->lookup (nm, true);

	  // Make sure this symbol is a variable.

	  if (! gsr->is_user_variable ())
	    gsr->define (octave_value ());

	  sr->alias (gsr);
	}
    }
}

// Make the definition of the symbol record sr be the same as the
// definition of the builtin variable of the same name.

// Make the definition of the symbol record sr be the same as the
// definition of the builtin function, or user function of the same
// name, provided that the name has not been used as a formal parameter.

void
link_to_builtin_or_function (symbol_record *sr)
{
  std::string nm = sr->name ();

  symbol_record *tmp_sym = 0;

  if (curr_parent_function)
    {
      std::string parent = curr_parent_function->name ();

      tmp_sym = fbi_sym_tab->lookup (parent + ":" + nm);
    }

  if (! tmp_sym)
    tmp_sym = fbi_sym_tab->lookup (nm);

  if (tmp_sym
      && tmp_sym->is_function ()
      && ! tmp_sym->is_formal_parameter ())
    sr->alias (tmp_sym);
}

// Force a link to a function in the current symbol table.  This is
// used just after defining a function to avoid different behavior
// depending on whether or not the function has been evaluated after
// being defined.
//
// Return without doing anything if there isn't a function with the
// given name defined in the global symbol table.

void
force_link_to_function (const std::string& id_name)
{
  symbol_record *fsr = fbi_sym_tab->lookup (id_name, true);
  if (fsr->is_function ())
    {
      curr_sym_tab->clear (id_name);
      symbol_record *csr = curr_sym_tab->lookup (id_name, true);
      csr->alias (fsr);
    }
}

DEFUN (document, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} document (@var{symbol}, @var{text})\n\
Set the documentation string for @var{symbol} to @var{text}.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 2)
    {
      std::string name = args(0).string_value ();

      if (! error_state)
	{
	  std::string help = args(1).string_value ();

	  if (! error_state)
	    {
	      if (is_command_name (name)
		  || is_mapper_function_name (name)
		  || is_builtin_function_name (name))
		error ("document: can't redefine help for built-in functions");
	      else
		{
		  symbol_record *sym_rec = curr_sym_tab->lookup (name);

		  if (sym_rec)
		    sym_rec->document (help);
		  else
		    error ("document: no such symbol `%s'", name.c_str ());
		}
	    }
	}
    }
  else
    print_usage ("document");

  return retval;
}

// FIXME -- this function is duplicated in symtab.cc with the
// name maybe_list_cmp_fcn.

static int
symbol_record_name_compare (const void *a_arg, const void *b_arg)
{
  const symbol_record *a = static_cast<const symbol_record *> (a_arg);
  const symbol_record *b = static_cast<const symbol_record *> (b_arg);

  std::string a_nm = a->name ();
  std::string b_nm = b->name ();

  return a_nm.compare (b_nm);
}

static octave_value
do_who (int argc, const string_vector& argv, int return_list)
{
  octave_value retval;

  bool show_builtins = false;
  bool show_functions = false;
  bool show_variables = false;
  bool show_verbose = false;

  std::string my_name = argv[0];

  int i;
  for (i = 1; i < argc; i++)
    {
      if (argv[i] == "-all" || argv[i] == "-a")
	{
	  show_builtins = true;
	  show_functions = true;
	  show_variables = true;
	}
      else if (argv[i] == "-builtins" || argv[i] == "-b")
	show_builtins = true;
      else if (argv[i] == "-functions" || argv[i] == "-f")
	show_functions = true;
      else if (argv[i] == "-long" || argv[i] == "-l")
	show_verbose = true;
      else if (argv[i] == "-variables" || argv[i] == "-v")
	show_variables = true;
      else if (argv[i][0] == '-')
	warning ("%s: unrecognized option `%s'", my_name.c_str (),
		 argv[i].c_str ());
      else
	break;
    }

  // If no options were specified to select the type of symbol to
  // display, then set defaults.

  if (! (show_builtins || show_functions || show_variables))
    {
      show_functions = at_top_level ();
      show_variables = true;
    }

  int npats = argc - i;
  string_vector pats (npats);
  for (int j = 0; j < npats; j++)
    pats[j] = argv[i+j];

  // If the user specified -l and nothing else, show variables.  If
  // evaluating this at the top level, also show functions.

  if (show_verbose && ! (show_builtins || show_functions || show_variables))
    {
      show_functions = at_top_level ();
      show_variables = 1;
    }

  if (return_list)
    {
      // FIXME -- maybe symbol_list should return a std::list
      // object instead of an Array.

      dim_vector dv (0, 0);

      Array<symbol_record *> s3 (dv);
      Array<symbol_record *> s4 (dv);
      Array<symbol_record *> s5 (dv);
      Array<symbol_record *> s6 (dv);
      Array<symbol_record *> s7 (dv);

      if (show_builtins)
	{
	  s3 = fbi_sym_tab->symbol_list (pats, symbol_record::BUILTIN_FUNCTION,
					 SYMTAB_ALL_SCOPES);
	}

      if (show_functions)
	{
	  s4 = fbi_sym_tab->symbol_list (pats, symbol_record::DLD_FUNCTION,
					 SYMTAB_ALL_SCOPES);

	  s5 = fbi_sym_tab->symbol_list (pats, symbol_record::USER_FUNCTION,
					 SYMTAB_ALL_SCOPES);
	}

      if (show_variables)
	{
	  s6 = curr_sym_tab->symbol_list (pats, symbol_record::USER_VARIABLE,
					  SYMTAB_LOCAL_SCOPE);

	  s7 = curr_sym_tab->symbol_list (pats, symbol_record::USER_VARIABLE,
					  SYMTAB_GLOBAL_SCOPE);
	}

      octave_idx_type s3_len = s3.length ();
      octave_idx_type s4_len = s4.length ();
      octave_idx_type s5_len = s5.length ();
      octave_idx_type s6_len = s6.length ();
      octave_idx_type s7_len = s7.length ();

      octave_idx_type symbols_len = s3_len + s4_len + s5_len + s6_len + s7_len;

      Array<symbol_record *> symbols (dim_vector (symbols_len, 1));

      octave_idx_type k = 0;

      symbols.insert (s3, k, 0);
      k += s3_len;
      symbols.insert (s4, k, 0);
      k += s4_len;
      symbols.insert (s5, k, 0);
      k += s5_len;
      symbols.insert (s6, k, 0);
      k += s6_len;
      symbols.insert (s7, k, 0);

      symbols.qsort (symbol_record_name_compare);

      if (show_verbose)
	{
	  Array<octave_value> name_info (symbols_len, 1);
	  Array<octave_value> size_info (symbols_len, 1);
	  Array<octave_value> bytes_info (symbols_len, 1);
	  Array<octave_value> class_info (symbols_len, 1);
	  Array<octave_value> global_info (symbols_len, 1);
	  Array<octave_value> sparse_info (symbols_len, 1);
	  Array<octave_value> complex_info (symbols_len, 1);
	  Array<octave_value> nesting_info (symbols_len, 1);

	  for (octave_idx_type j = 0; j < symbols_len; j++)
	    {
	      symbol_record *sr = symbols(j);

	      Octave_map ni;

	      std::string caller_function_name;

	      octave_function *caller = octave_call_stack::caller ();
	      if (caller)
		caller_function_name = caller->name ();

	      ni.assign ("function", caller_function_name);
	      ni.assign ("level", 1);

	      name_info(j) = sr->name ();
	      size_info(j) = sr->size ();
	      bytes_info(j) = sr->byte_size ();
	      class_info(j) = sr->class_name ();
	      global_info(j) = sr->is_linked_to_global ();
	      sparse_info(j) = sr->is_sparse_type ();
	      complex_info(j) = sr->is_complex_type ();
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

	  retval = info;
	}
      else
	{
	  string_vector names;

	  if (symbols_len > 0)
	    {
	      names.resize (symbols_len);

	      for (octave_idx_type j = 0; j < symbols_len; j++)
		names[j] = symbols(j)->name ();
	    }

	  retval = Cell (names);
	}
    }
  else
    {
      int pad_after = 0;

      if (show_builtins)
	{
	  pad_after += fbi_sym_tab->maybe_list
	    ("*** built-in functions:", pats, octave_stdout,
	     show_verbose, symbol_record::BUILTIN_FUNCTION, SYMTAB_ALL_SCOPES);
	}

      if (show_functions)
	{
	  pad_after += fbi_sym_tab->maybe_list
	    ("*** dynamically linked functions:", pats,
	     octave_stdout, show_verbose, symbol_record::DLD_FUNCTION,
	     SYMTAB_ALL_SCOPES);

	  pad_after += fbi_sym_tab->maybe_list
	    ("*** currently compiled functions:", pats,
	     octave_stdout, show_verbose, symbol_record::USER_FUNCTION,
	     SYMTAB_ALL_SCOPES);
	}

      if (show_variables)
	{
	  pad_after += curr_sym_tab->maybe_list
	    ("*** local user variables:", pats, octave_stdout,
	     show_verbose, symbol_record::USER_VARIABLE, SYMTAB_LOCAL_SCOPE);

	  pad_after += curr_sym_tab->maybe_list
	    ("*** globally visible user variables:", pats,
	     octave_stdout, show_verbose, symbol_record::USER_VARIABLE,
	     SYMTAB_GLOBAL_SCOPE);
	}

      if (pad_after)
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

      if (error_state)
	return retval;

      retval = do_who (argc, argv, nargout == 1);
    }
  else
    print_usage ("who");

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
      int nargin = args.length ();

      octave_value_list tmp_args;

      for (int i = nargin; i > 0; i--)
	tmp_args(i) = args(i-1);

      tmp_args(0) = "-long";

      int argc = tmp_args.length () + 1;

      string_vector argv = tmp_args.make_argv ("whos");

      if (error_state)
	return retval;

      retval = do_who (argc, argv, nargout == 1);
    }
  else
    print_usage ("whos");

  return retval;
}

// Defining variables.

void
bind_ans (const octave_value& val, bool print)
{
  symbol_record *sr = curr_sym_tab->lookup ("ans", true);

  if (val.is_defined ())
    {
      sr->define (val);

      if (print)
	val.print_with_name (octave_stdout, "ans");
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
mlock (const std::string& nm)
{
  symbol_record *sr = fbi_sym_tab->lookup (nm, true);

  if (sr)
    sr->mark_as_static ();
}

void 
munlock (const std::string& nm)
{
  symbol_record *sr = fbi_sym_tab->lookup (nm);

  if (sr && sr->is_static ())
    sr->unmark_static ();
  else
    error ("munlock: %s is not locked", nm.c_str ());
}

bool
mislocked (const std::string& nm)
{
  symbol_record *sr = fbi_sym_tab->lookup (nm);

  return (sr && sr->is_static ());
}

DEFCMD (mlock, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} mlock (@var{name})\n\
Lock the named function into memory.  If no function is named\n\
then lock in the current function.\n\
@seealso{munlock, mislocked, persistent}\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      std::string name = args(0).string_value ();

      if (! error_state)
	mlock (name);
      else
	error ("mlock: expecting argument to be a function name");
    }
  else if (args.length () == 0)
    {
      octave_user_function *fcn = octave_call_stack::caller_user_function ();

      if (fcn)
        mlock (fcn->name ());
      else
        error ("mlock: invalid use outside a function");
    }
  else
    print_usage ("mlock");

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
      octave_user_function *fcn = octave_call_stack::caller_user_function ();

      if (fcn)
        mlock (fcn->name ());
      else
        error ("munlock: invalid use outside a function");
    }
  else
    print_usage ("munlock");

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
      octave_user_function *fcn = octave_call_stack::caller_user_function ();

      if (fcn)
        retval = mislocked (fcn->name ());
      else
        error ("mislocked: invalid use outside a function");
    }
  else
    print_usage ("mislocked");

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

static inline bool
is_local_variable (const std::string& nm)
{
  symbol_record *sr = curr_sym_tab->lookup (nm);

  return (sr && sr->is_variable ());
}

static inline void
maybe_warn_exclusive (bool exclusive)
{
  if (exclusive)
    warning ("clear: ignoring --exclusive option");
}

static inline void
do_clear_all (void)
{
  curr_sym_tab->clear ();
  fbi_sym_tab->clear_functions ();
  global_sym_tab->clear ();
}

static inline void
do_clear_functions (void)
{
  curr_sym_tab->clear_functions ();
  fbi_sym_tab->clear_functions ();
}

static inline void
do_clear_globals (void)
{
  curr_sym_tab->clear_globals ();
  global_sym_tab->clear ();
}

static inline void
do_clear_variables (void)
{
  curr_sym_tab->clear ();
}

static inline bool
do_clear_function (const std::string& nm)
{
  bool b1 = curr_sym_tab->clear_function (nm);

  bool b2 = fbi_sym_tab->clear_function (nm);

  return b1 || b2;
}

static inline bool
do_clear_global (const std::string& nm)
{
  bool b1 = curr_sym_tab->clear_global (nm);

  bool b2 = global_sym_tab->clear_variable (nm);

  return b1 || b2;
}

static inline bool
do_clear_variable (const std::string& nm)
{
  return curr_sym_tab->clear_variable (nm);
}

static inline bool
do_clear_symbol (const std::string& nm)
{
  bool cleared = curr_sym_tab->clear_variable (nm);

  if (! cleared)
    cleared = do_clear_function (nm);

  return cleared;
}

static inline bool
do_clear_function_pattern (const std::string& pat)
{
  bool b1 = curr_sym_tab->clear_function_pattern (pat);

  bool b2 = fbi_sym_tab->clear_function_pattern (pat);

  return b1 || b2;
}

static inline bool
do_clear_global_pattern (const std::string& pat)
{
  bool b1 = curr_sym_tab->clear_global_pattern (pat);

  bool b2 = global_sym_tab->clear_variable_pattern (pat);

  return b1 || b2;
}

static inline bool
do_clear_variable_pattern (const std::string& pat)
{
  return curr_sym_tab->clear_variable_pattern (pat);
}

static inline bool
do_clear_symbol_pattern (const std::string& pat)
{
  // FIXME -- if we have a variable v1 and a function v2 and
  // someone says clear v*, we will clear the variable but not the
  // function.  Is that really what should happen?  (I think it is
  // what Matlab does.)

  bool cleared = curr_sym_tab->clear_variable_pattern (pat);

  if (! cleared)
    cleared = do_clear_function_pattern (pat);

  return cleared;
}

static inline void
do_clear_functions (const string_vector& argv, int argc, int idx,
		    bool exclusive = false)
{
  if (idx == argc)
    do_clear_functions ();
  else
    {
      if (exclusive)
	{
	  string_vector lfcns = curr_sym_tab->user_function_name_list ();

	  int lcount = lfcns.length ();

	  for (int i = 0; i < lcount; i++)
	    {
	      std::string nm = lfcns[i];

	      if (! name_matches_any_pattern (nm, argv, argc, idx))
		do_clear_function (nm);
	    }

	  string_vector fcns = fbi_sym_tab->user_function_name_list ();

	  int fcount = fcns.length ();

	  for (int i = 0; i < fcount; i++)
	    {
	      std::string nm = fcns[i];

	      if (! name_matches_any_pattern (nm, argv, argc, idx))
		do_clear_function (nm);
	    }
	}
      else
	{
	  while (idx < argc)
	    do_clear_function_pattern (argv[idx++]);
	}
    }
}

static inline void
do_clear_globals (const string_vector& argv, int argc, int idx,
		  bool exclusive = false)
{
  if (idx == argc)
    do_clear_globals ();
  else
    {
      if (exclusive)
	{
	  string_vector lvars = curr_sym_tab->global_variable_name_list ();

	  int lcount = lvars.length ();

	  for (int i = 0; i < lcount; i++)
	    {
	      std::string nm = lvars[i];

	      if (! name_matches_any_pattern (nm, argv, argc, idx))
		do_clear_global (nm);
	    }

	  string_vector gvars = global_sym_tab->global_variable_name_list ();

	  int gcount = gvars.length ();

	  for (int i = 0; i < gcount; i++)
	    {
	      std::string nm = gvars[i];

	      if (! name_matches_any_pattern (nm, argv, argc, idx))
		do_clear_global (nm);
	    }
	}
      else
	{
	  while (idx < argc)
	    do_clear_global_pattern (argv[idx++]);
	}
    }
}

static inline void
do_clear_variables (const string_vector& argv, int argc, int idx,
		    bool exclusive = false)
{
  if (idx == argc)
    do_clear_variables ();
  else
    {
      if (exclusive)
	{
	  string_vector lvars = curr_sym_tab->variable_name_list ();

	  int lcount = lvars.length ();

	  for (int i = 0; i < lcount; i++)
	    {
	      std::string nm = lvars[i];

	      if (! name_matches_any_pattern (nm, argv, argc, idx))
		do_clear_variable (nm);
	    }
	}
      else
	{
	  while (idx < argc)
	    do_clear_variable_pattern (argv[idx++]);
	}
    }
}

static inline void
do_clear_symbols (const string_vector& argv, int argc, int idx,
		  bool exclusive = false)
{
  if (idx == argc)
    do_clear_variables ();
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
	    do_clear_symbol_pattern (argv[idx++]);
	}
    }
}

static void
do_matlab_compatible_clear (const string_vector& argv, int argc, int idx)
{
  // This is supposed to be mostly Matlab compatible.

  for (; idx < argc; idx++)
    {
      if (argv[idx] == "all" && ! is_local_variable ("all"))
	{
	  do_clear_all ();
	}
      else if (argv[idx] == "functions" && ! is_local_variable ("functions"))
	{
	  do_clear_functions (argv, argc, ++idx);
	}
      else if (argv[idx] == "global" && ! is_local_variable ("global"))
	{
	  do_clear_globals (argv, argc, ++idx);
	}
      else if (argv[idx] == "variables" && ! is_local_variable ("variables"))
	{
	  do_clear_variables ();
	}
      else
	{
	  do_clear_symbol_pattern (argv[idx]);
	}
    }
}

#define CLEAR_OPTION_ERROR(cond) \
  do \
    { \
      if (cond) \
        { \
          print_usage ("clear"); \
          return retval; \
        } \
    } \
  while (0)

bool
clear_function (const std::string& nm)
{
  return do_clear_function (nm);
}

bool
clear_variable (const std::string& nm)
{
  return do_clear_variable (nm);
}

bool
clear_symbol (const std::string& nm)
{
  return do_clear_symbol (nm);
}

DEFCMD (clear, args, ,
  "-*- texinfo -*-\n\
@deffn {Command} clear [-x] pattern @dots{}\n\
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
With -x, clear the variables that don't match the patterns.\n\
\n\
This command may not be used within a function body.\n\
@end deffn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("clear");

  if (! error_state)
    {
      if (argc == 1)
	{
	  do_clear_variables ();
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

		      curr_sym_tab->clear ();
		      fbi_sym_tab->clear_functions ();
		      global_sym_tab->clear ();
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
Print raw symbol table statistices.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      std::string arg = args(0).string_value ();

      if (arg == "fbi")
	fbi_sym_tab->print_info (octave_stdout);
      else if (arg == "global")
	global_sym_tab->print_info (octave_stdout);
      else if (arg == "top-level")
	top_level_sym_tab->print_info (octave_stdout);
      else
	{
	  symbol_record *fsr = fbi_sym_tab->lookup (arg, true);

	  if (fsr && fsr->is_user_function ())
	    {
	      octave_value tmp = fsr->def ();
	      const octave_base_value& rep = tmp.get_rep ();
	      
	      const octave_user_function& fcn
		= dynamic_cast<const octave_user_function&> (rep);

	      fcn.print_symtab_info (octave_stdout);
	    }
	  else
	    error ("no user-defined function named %s", arg.c_str ());
	}
    }
  else if (nargin == 0)
    curr_sym_tab->print_info (octave_stdout);
  else
    print_usage ("__print_symtab_info__");

  return retval;
}

DEFUN (__print_symbol_info__, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} __dump_symbol_info__ (@var{name})\n\
Print symbol table information for the symbol @var{name}.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      std::string symbol_name = args(0).string_value ();

      if (! error_state)
	{
	  symbol_record *sr = curr_sym_tab->lookup (symbol_name);

	  if (sr)
	    sr->print_info (octave_stdout);
	  else
	    error ("__print_symbol_info__: symbol %s not found",
		   symbol_name.c_str ());
	}
      else
	print_usage ("__print_symbol_info__");
    }
  else
    print_usage ("__print_symbol_info__");

  return retval;
}

DEFUN (ignore_function_time_stamp, args, nargout,
    "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} ignore_function_time_stamp ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} ignore_function_time_stamp (@var{new_val})\n\
Query or set the internal variable that controls whether Octave checks\n\
the time stamp on files each time it looks up functions defined in\n\
function files.  If the internal variable is set to @code{\"system\"},\n\
Octave will not automatically recompile function files in subdirectories of\n\
@file{@var{octave-home}/lib/@var{version}} if they have changed since\n\
they were last compiled, but will recompile other function files in the\n\
search path if they change.  If set to @code{\"all\"}, Octave will not\n\
recompile any function files unless their definitions are removed with\n\
@code{clear}.  If set to \"none\", Octave will always check time stamps\n\
on files to determine whether functions defined in function files\n\
need to recompiled.\n\
@end deftypefn")
{
  octave_value retval;

  if (nargout > 0)
    {
      switch (Vignore_function_time_stamp)
	{
	case 1:
	  retval = "system";
	  break;

	case 2:
	  retval = "all";
	  break;

	default:
	  retval = "none";
	  break;
	}
    }

  int nargin = args.length ();

  if (nargin == 1)
    {
      std::string sval = args(0).string_value ();

      if (! error_state)
	{
	  if (sval == "all")
	    Vignore_function_time_stamp = 2;
	  else if (sval == "system")
	    Vignore_function_time_stamp = 1;
	  else if (sval == "none")
	    Vignore_function_time_stamp = 0;
	  else
	    error ("ignore_function_time_stamp: expecting argument to be \"all\", \"system\", or \"none\"");
	}
      else
	error ("ignore_function_time_stamp: expecting argument to be character string");
    }
  else if (nargin > 1)
    print_usage ("ignore_function_time_stamp");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
