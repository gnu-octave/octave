/*

Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007 Ben Sapp

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

#include <iostream>
#include <fstream>
#include <string>
#include <set>


#include "defun.h"
#include "error.h"
#include "help.h"
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
#include "ov-list.h"
#include "ov-struct.h"
#include "pt-pr-code.h"
#include "pt.h"
#include "pt-bp.h"
#include "pt-stmt.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "variables.h"

#include "debug.h"

// Initialize the singleton object
bp_table *bp_table::instance = NULL;

// Return a pointer to the user-defined function FNAME.  If FNAME is
// empty, search backward for the first user-defined function in the
// current call stack.
static octave_user_function *
get_user_function (const std::string& fname = "")
{
  octave_user_function *dbg_fcn = 0;

  if (fname == "")
    dbg_fcn = octave_call_stack::caller_user_function ();
  else
    {
      symbol_record *ptr = curr_sym_tab->lookup (fname);

      if (ptr && ptr->is_user_function ())
	{
	  octave_value tmp = ptr->def ();
	  dbg_fcn = dynamic_cast<octave_user_function *> (tmp.function_value ());
	}
      else
	{
	  ptr = lookup_by_name (fname, false);
	  if (ptr && ptr->is_user_function ())
	    {
	      octave_value tmp = ptr->def ();
	      dbg_fcn = dynamic_cast<octave_user_function *> (tmp.function_value ());
	    }
	}
    }

  return dbg_fcn;
}

static void
parse_dbfunction_params (const octave_value_list& args, 
			 std::string& symbol_name, 
			 intmap& lines)
{
  octave_idx_type len = 0;
  int nargin = args.length ();
  int idx = 0;
  int list_idx = 0;
  symbol_name = std::string ();

  // If we are already in a debugging function
  if (octave_call_stack::caller_user_function () != NULL)
    {
      idx = 0;
    }
  else
    {
      symbol_name = args (0).string_value ();
      if (error_state)
	return;
      idx = 1;
    }

  for (int i = idx; i < nargin; i++ )
    {
      if (args (i).is_string ())
	len += 1;
      else
	len += args (i).numel ();
    }

  lines = intmap();
  for (int i = idx; i < nargin; i++ )
    {
      if (args (i).is_string ())
	{
	  int line = atoi (args (i).string_value ().c_str ());
	  if (error_state)
	      break;
	  lines[list_idx++] = line;
	}
      else
	{
	  const NDArray arg = args (i).array_value ();
	  
	  if (error_state)
	    break;
	  
	  for (octave_idx_type j = 0; j < arg.nelem(); j++)
	    {
	      int line = static_cast<int> (arg.elem (j));
	      if (error_state)
		break;
	      lines[list_idx++] = line;
	    }
	  
	  if (error_state)
	    break;
	}
    } 
}

intmap
bp_table::add_breakpoint (const std::string& fname, 
			  const intmap& line)
{
  if (!instance_ok ())
    return intmap();

  octave_idx_type len = line.size ();
  intmap retval;
  octave_user_function *dbg_fcn = get_user_function (fname);

  if (dbg_fcn)
    {
      tree_statement_list *cmds = dbg_fcn->body ();
      for (int i = 0; i < len; i++)
	{
	  intmap::const_iterator p = line.find (i);
	  if (p != line.end ())
	    {
	      int lineno = p->second;
	      retval[i] = cmds->set_breakpoint (lineno);
	      if (retval[i] != 0)
		instance->bp_map[fname] = dbg_fcn;
	    }
	}
    }
  else
    error ("add_breakpoint: unable to find the function requested\n");

  return retval;
}


int 
bp_table::remove_breakpoint (const std::string& fname, 
			     const intmap& line)
{
  if (!instance_ok ())
    return 0;

  octave_idx_type len = line.size ();
  int retval = 0;

  if (len == 0)
    {
      intmap results = remove_all_breakpoints_in_file (fname);
      retval = results.size ();
    }
  else
    {
      octave_user_function *dbg_fcn = get_user_function (fname);
      if (dbg_fcn)
	{
	  tree_statement_list *cmds = dbg_fcn->body ();
	  for (int i = 0; i < len; i++)
	    {
	      intmap::const_iterator p = line.find (i);
	      if (p != line.end ())
		{
		  int lineno = p->second;
		  cmds->delete_breakpoint (lineno);
		}
	    }
	  octave_value_list results = cmds->list_breakpoints ();
	  if (results.length () == 0)
	    instance->bp_map.erase (instance->bp_map.find (fname));
	  retval = results.length ();
	}
      else
	error ("remove_breakpoint: unable to find the function requested\n");
    }
  return retval;
}


intmap
bp_table::remove_all_breakpoints_in_file (const std::string& fname)
{
  if (!instance_ok ())
    return intmap();

  octave_value_list bkpts;
  intmap retval;
  octave_user_function *dbg_fcn = get_user_function (fname);
  
  if (dbg_fcn)
    {
      tree_statement_list *cmds = dbg_fcn->body ();
      bkpts = cmds->list_breakpoints ();
      for (int i = 0; i < bkpts.length (); i++)
	{
	  int lineno = static_cast<int> (bkpts (i).int_value ());
	  cmds->delete_breakpoint (lineno);
	  retval[i] = lineno;
	}
      instance->bp_map.erase (instance->bp_map.find (fname));
    }
  else
    error ("remove_all_breakpoint_in_file: "
	   "unable to find the function requested\n");

  return retval;
}


void 
bp_table::remove_all_breakpoints (void)
{
  if (!instance_ok ())
    return;

  std::map< std::string, octave_user_function* >::iterator it;
  for (it = instance->bp_map.begin (); it != instance->bp_map.end (); it++)
    {
      remove_all_breakpoints_in_file (it->first);
    }
}

std::string 
do_find_bkpt_list (octave_value_list slist, 
		   std::string match)
{
  std::string retval;
  for (int i = 0; i < slist.length (); i++)
    {
      if (slist (i).string_value () == match)
	{
	  retval = slist (i).string_value ();
	  break;
	}
    }
  return retval;
}


std::map< std::string, intmap> 
bp_table::get_breakpoint_list (const octave_value_list& fname_list)
{
  std::map<std::string, intmap> retval;

  if (!instance_ok ())
    return retval;

  // Iterate through each of the files in the map and get the 
  // name and list of breakpoints
  std::map< std::string, octave_user_function* >::iterator it;
  for (it = instance->bp_map.begin (); it != instance->bp_map.end (); it++)
    {
      if (fname_list.length () == 0 || 
	  do_find_bkpt_list (fname_list, it->first) != "")
	{
	  octave_value_list bkpts = it->second->body ()->list_breakpoints ();
	  octave_idx_type len = bkpts.length (); 
	  intmap bkpts_vec;
	  for (int i = 0; i < len; i++)
	    bkpts_vec[i] = bkpts (i).double_value ();
	  retval[ it->first ] = bkpts_vec;
	}
    }
  return retval;
}

static octave_value
intmap_to_ov (const intmap& line) 
{
  int idx = 0;
  NDArray retval (dim_vector (1, line.size()));
  for (int i = 0; i < line.size(); i++ )
    {
      intmap::const_iterator p = line.find (i);
      if (p != line.end ())
	{
	  int lineno = p->second;
	  retval (idx++) = lineno;
	}
    }
  retval.resize (dim_vector (1, idx));
  return retval;
}

DEFCMD (dbstop, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {rline =} dbstop (func, line, @dots{})\n\
Set a breakpoint in a function\n\
@table @code\n\
@item func\n\
String representing the function name.  When already in debug\n\
mode this should be left out and only the line should be given.\n\
@item line\n\
Line you would like the breakpoint to be set on. Multiple\n\
lines might be given as separate arguments or as a vector.\n\
@end table\n\
\n\
The rline returned is the real line that the breakpoint was set at.\n\
@seealso{dbclear, dbstatus, dbnext}\n\
@end deftypefn")
{
  intmap retval;
  std::string symbol_name = "";
  intmap lines;
  parse_dbfunction_params (args, symbol_name, lines);

  if (!error_state)
    retval = bp_table::add_breakpoint (symbol_name, lines);

  return intmap_to_ov(retval);
}

DEFCMD (dbclear, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} dbclear (func, line, @dots{})\n\
Delete a breakpoint in a function\n\
@table @code\n\
@item func\n\
String representing the function name.  When already in debug\n\
mode this should be left out and only the line should be given.\n\
@item line\n\
Line where you would like to remove the breakpoint. Multiple\n\
lines might be given as separate arguments or as a vector.\n\
@end table\n\
No checking is done to make sure that the line you requested is really\n\
a breakpoint. If you get the wrong line nothing will happen.\n\
@seealso{dbstop, dbstatus, dbwhere}\n\
@end deftypefn")
{
  octave_value retval;
  std::string symbol_name = "";
  intmap lines;
  parse_dbfunction_params (args, symbol_name, lines);
      
  if (!error_state)
    bp_table::remove_breakpoint (symbol_name, lines);

  return retval;
}

DEFCMD (dbstatus, args, nargout,
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
  Octave_map retval;
  int nargin = args.length ();
  octave_value_list fcn_list;
  std::map< std::string, intmap> bp_list;
  std::string symbol_name = "";

  if (nargin != 0 && nargin != 1)
    {
      error ("dbstatus: only zero or one arguements accepted\n");
      return octave_value ();
    }

  if (nargin == 1)
    {
      if (args(0).is_string ())
	{
	  symbol_name = args (0).string_value ();
	  fcn_list (0) = symbol_name;
	  bp_list = bp_table::get_breakpoint_list (fcn_list);
	}
      else
	gripe_wrong_type_arg ("dbstatus", args (0));
    }
  else
    {
       octave_user_function *dbg_fcn = get_user_function ();
       if (dbg_fcn)
	 {
	   symbol_name = dbg_fcn->name ();
	   fcn_list (0) = symbol_name;
	 }
       bp_list = bp_table::get_breakpoint_list (fcn_list);
    }

  std::map< std::string, intmap>::iterator it;
  if (nargout == 1)
    {
      // Fill in an array for return
      int i = 0;
      Cell names (dim_vector (bp_list.size (), 1));
      Cell file (dim_vector (bp_list.size (), 1));
      Cell line (dim_vector (bp_list.size (), 1));
      for (it = bp_list.begin (); it != bp_list.end (); it++)
	{
	  names (i) = it->first;
	  line (i) = intmap_to_ov(it->second);
	  file (i)  = do_which (it->first);
	  i++;
	}
      retval.assign ("name", names);
      retval.assign ("file", file);
      retval.assign ("line", line);
      return octave_value (retval);
    }
  else
    {
      // Print out the breakpoint information
      for (it = bp_list.begin(); it != bp_list.end(); it++)
	{	  
	  octave_stdout << "Breakpoint in " << it->first << " at line(s) ";
	  for (int j = 0; j < it->second.size (); j++)
	    if (j < it->second.size()-1)
	      octave_stdout << it->second [j] << ", ";
	    else
	      octave_stdout << it->second [j] << "." << std::endl;
	}
      return octave_value ();
    }
}

DEFCMD (dbwhere, , ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} dbwhere ()\n\
Show where we are in the code\n\
@seealso{dbclear, dbstatus, dbstop}\n\
@end deftypefn")
{
  octave_value retval;

  octave_user_function *dbg_fcn = get_user_function ();

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
    os << "dbtype: unknown function " << name << "\n";

  os.flush ();
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
		
		      if (std::min (start, end) <= 0)
 			error ("dbtype: start and end lines must be >= 1\n");

		      if (start <= end)
			do_dbtype (octave_stdout, dbg_fcn->name (), start, end);
		      else
			error ("dbtype: start line must be less than end line\n");
		    }
		  else
		    error ("dbtype: line specification must be `start:end'");
		}
	    }
	  break;

	case 2: // (dbtype func start:end) , (dbtype func start)
	  dbg_fcn = get_user_function (argv[1]);

	  if (dbg_fcn)
	    {
	      std::string arg = argv[2];
	      int start = 0;
	      int end = 0;
	      size_t ind = arg.find (':');

	      if (ind != NPOS)
		{
		  std::string start_str = arg.substr (0, ind);
		  std::string end_str = arg.substr (ind + 1);

		  start = atoi (start_str.c_str ());
		  end = atoi (end_str.c_str ());
		  
		}
	      else
		{
		  start = atoi (arg.c_str ());
		  end = start;
		}

	      if (std::min (start, end) <= 0)
		error ("dbtype: start and end lines must be >= 1\n");
	      
	      if (start <= end)
		do_dbtype (octave_stdout, dbg_fcn->name (), start, end);
	      else
		error ("dbtype: start line must be less than end line\n");
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
