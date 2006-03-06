/*

Copyright (C) 2001 Ben Sapp

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

#include <iostream>
#include <fstream>
#include <string>

#include "defun.h"
#include "error.h"
#include "input.h"
#include "pager.h"
#include "oct-obj.h"
#include "utils.h"
#include "parse.h"
#include "symtab.h"
#include "gripes.h"
#include "ov.h"
#include "ov-usr-fcn.h"
#include "ov-fcn.h"
#include "pt-pr-code.h"
#include "pt.h"
#include "pt-bp.h"
#include "pt-stmt.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "variables.h"

static octave_user_function *
get_user_function (std::string str = "")
{
  octave_user_function *dbg_fcn = 0;

  if (str.compare (""))
    {
      symbol_record *ptr = curr_sym_tab->lookup (str);

      if (ptr && ptr->is_user_function ())
	{
	  octave_value tmp = ptr->def ();
	  dbg_fcn = dynamic_cast<octave_user_function *> (tmp.function_value ());
	}
      else
	{
	  ptr = lookup_by_name (str, false);
	
	  if (ptr && ptr->is_user_function ())
	    {
	      octave_value tmp = ptr->def ();
	      dbg_fcn = dynamic_cast<octave_user_function *> (tmp.function_value ());
	    }
	}
    }
  else if (curr_caller_function && curr_caller_function->is_user_function ())
    dbg_fcn = dynamic_cast<octave_user_function *> (curr_caller_function);

  return dbg_fcn;
}


DEFCMD (dbstop, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {rline =} dbstop (func, line)\n\
Set a breakpoint in a function\n\
@table @code\n\
@item func\n\
String representing the function name.  When already in debug\n\
mode this should be left out and only the line should be given.\n\
@item line\n\
Line you would like the breakpoint to be set on\n\
@end table\n\
\n\
The rline returned is the real line that the breakpoint was set at.\n\
@seealso{dbclear, dbstatus, dbnext}\n\
@end deftypefn")
{
  octave_value retval;

  int result = -1;
  int nargin = args.length ();

  string_vector argv = args.make_argv ("dbstop");

  if (error_state)
    return retval;

  if (nargin == 2)
    {
      std::string symbol_name = argv[1];

      std::string line_number = argv[2];

      int line = atoi (line_number.c_str ());

      octave_user_function *dbg_fcn = get_user_function (symbol_name);

      if (dbg_fcn)
	{
	  tree_statement_list *cmds = dbg_fcn->body ();
	  result = cmds->set_breakpoint (line);
	}
      else
	error ("dbstop: unable to find the function requested\n");
    }
  else if (nargin == 1)
    {
      std::string line_number = argv[1];

      int line = atoi (line_number.c_str ());

      octave_user_function *dbg_fcn = get_user_function ();

      if (dbg_fcn)
	{
	  tree_statement_list *cmds = dbg_fcn->body ();
	  result = cmds->set_breakpoint (line);
	}
      else
	error ("dbstop: unable to find the function requested\n");	
    }
  else
    error ("dbstop: one argument when in a function and two when not\n");

  retval = result;

  return retval;
}

DEFCMD (dbclear, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} dbclear (func, line)\n\
Delete a breakpoint in a function\n\
@table @code\n\
@item func\n\
String representing the function name.  When already in debug\n\
mode this should be left out and only the line should be given.\n\
@item line\n\
Line where you would like to remove the the breakpoint\n\
@end table\n\
No checking is done to make sure that the line you requested is really\n\
a breakpoint.   If you get the wrong line nothing will happen.\n\
@seealso{dbstop, dbstatus, dbwhere}\n\
@end deftypefn")
{
  octave_value retval;

  std::string symbol_name = "";
  std::string line_number;

  int line = -1;
  int nargin = args.length ();

  string_vector argv = args.make_argv ("dbclear");

  if (error_state)
    return retval;

  if (nargin == 1 || nargin == 2)
    {
      if (nargin == 2)
	{
	  symbol_name = argv[1];

	  octave_stdout << argv[1] << std::endl;
	  line_number = argv[2];
	}
      else if (nargin == 1)
	{
	  line_number = argv[1];
	}

      if (line_number.compare ("all") && line_number.compare ("ALL"))
	line = atoi (line_number.c_str ());
      else
	line = -1;

      octave_stdout << "symbol_name = " << symbol_name << std::endl;
      octave_user_function *dbg_fcn = get_user_function (symbol_name);

      if (dbg_fcn)
	{
	  tree_statement_list *cmds = dbg_fcn->body ();
	  cmds->delete_breakpoint (line);
	}
      else
	error ("dbclear: unable to find the function requested\n");
    }
  else
    error ("dbclear: expecting one or two arguements\n");

  return retval;
}

DEFCMD (dbstatus, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {lst =} dbstatus ([func])\n\
Return a vector containing the lines on which a function has \n\
breakpoints set.\n\
@table @code\n\
@item func\n\
String representing the function name.  When already in debug\n\
mode this should be left out.\n\
@end table\n\
@seealso{dbclear, dbwhere}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin != 0 && nargin != 1)
    {
      error ("dbstatus: only zero or one arguements accepted\n");
      return retval;
    }

  std::string symbol_name = "";

  if (nargin == 1)
    {
      if (args(0).is_string ())
	symbol_name = args(0).string_value ();
      else
	gripe_wrong_type_arg ("dbstatus", args(0));
    }

  octave_user_function *dbg_fcn = get_user_function (symbol_name);

  if (dbg_fcn)
    {
      tree_statement_list *cmds = dbg_fcn->body ();

      octave_value_list lst = cmds->list_breakpoints ();

      RowVector vec (lst.length (), 0.0);

      for (int i = 0; i < lst.length (); i++)
	{
	  vec(i) = lst(i).double_value ();

	  if (error_state)
	    panic_impossible ();
	}

      retval = octave_value (vec);
    }
  else
    error ("dbstatus: unable to find the function you requested\n");

  return retval;
}

DEFCMD (dbwhere, , ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} dbwhere ()\n\
Show where we are in the code\n\
@seealso{dbclear, dbstatus, dbstop}\n\
@end deftypefn")
{
  octave_value retval;

  octave_user_function *dbg_fcn = 0;

  if (curr_caller_function && curr_caller_function->is_user_function ())
    dbg_fcn = dynamic_cast<octave_user_function *> (curr_caller_function);

  if (dbg_fcn)
    {
      std::string name = dbg_fcn->name ();

      octave_stdout << name << ":";

      const tree *dbg_stmt = tree::break_statement;

      if (dbg_stmt)
	{
	  octave_stdout << "line " << dbg_stmt->line () << ", ";
	  octave_stdout << "column " << dbg_stmt->column () << std::endl;
	}
      else
	octave_stdout << "-1\n";
    }
  else
    error ("dbwhere: must be inside of a user function to use dbwhere\n");

  return retval;
}

// Copied and modified from the do_type command in help.cc
// Maybe we could share some code?
void
do_dbtype (std::ostream& os, const std::string& name, int start, int end)
{
  std::string ff = fcn_file_in_path (name);

  if (! ff.empty ())
    {
      std::ifstream fs (ff.c_str (), std::ios::in);

      if (fs)
	{
	  char ch;
	  int line = 1;
	
	  if (line >= start && line <= end)
	    os << line << "\t";
 	
	  while (fs.get (ch))
	    {
	      if (line >= start && line <= end)
		{
		  os << ch;
		}

	      if (ch == '\n')
		{
		  line++;
		  if (line >= start && line <= end)
		    os << line << "\t";
		}
	    }
	}
      else
	os << "dbtype: unable to open `" << ff << "' for reading!\n";
    }
  else
    os << "dbtype: unkown function";

}

DEFCMD (dbtype, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} dbtype ()\n\
List script file with line numbers.\n\
@seealso{dbclear, dbstatus, dbstop}\n\
@end deftypefn")
{
  octave_value retval;
  octave_user_function *dbg_fcn;

  int nargin = args.length ();
  string_vector argv = args.make_argv ("dbtype");

  if (! error_state)
    {
      switch (nargin)
	{
	case 0: // dbtype
	  dbg_fcn = get_user_function ();

	  if (dbg_fcn)
	    do_dbtype (octave_stdout, dbg_fcn->name (), 0, INT_MAX);
	  else
	    error ("dbtype: must be in a user function to give no arguments to dbtype\n");
	  break;

	case 1: // (dbtype func) || (dbtype start:end)
	  dbg_fcn = get_user_function (argv[1]);

	  if (dbg_fcn)
	    do_dbtype (octave_stdout, dbg_fcn->name (), 0, INT_MAX);
	  else
	    {
	      dbg_fcn = get_user_function ("");

	      if (dbg_fcn)
		{
		  std::string arg = argv[1];

		  size_t ind = arg.find (':');

		  if (ind != NPOS)
		    {
		      std::string start_str = arg.substr (0, ind);
		      std::string end_str = arg.substr (ind + 1);

		      int start = atoi (start_str.c_str ());
		      int end = atoi (end_str.c_str ());
		
		      if (start < end)
			do_dbtype (octave_stdout,
				   dbg_fcn->name (), start, end);
		      else
			error ("dbtype: the start line must be less than the end line\n");
		    }
		  else
		    error ("dbtype: if you specify lines it must be like `start:end`");
		}
	    }
	  break;

	case 2: // (dbtype func start:end)
	  dbg_fcn = get_user_function (argv[1]);

	  if (dbg_fcn)
	    {
	      std::string arg = argv[2];

	      size_t ind = arg.find (':');

	      if (ind != NPOS)
		{
		  std::string start_str = arg.substr (0, ind);
		  std::string end_str = arg.substr (ind + 1);

		  int start = atoi (start_str.c_str ());
		  int end = atoi (end_str.c_str ());
		
		  if (start < end)
		    do_dbtype (octave_stdout,
			       dbg_fcn->name (), start, end);
		  else
		    error ("dbtype: the start line must be less than the end line\n");
		}
	      else
		error ("dbtype: if you specify lines it must be like `start:end`");
	    }
	  break;

	default:
	  error ("dbtype: expecting zero, one, or two arguments\n");
	}
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
