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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

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
#include "pt-stmt.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "variables.h"
#include "defun-dld.h"
#include "defun-int.h"

octave_user_function *
get_user_function (std::string str = "")
{
  octave_user_function *dbg_fcn = NULL;

  if (curr_sym_tab != top_level_sym_tab)
    {
      Array<symbol_record *> srs =  
	top_level_sym_tab->symbol_list ("*",
					symbol_record::USER_FUNCTION,
					SYMTAB_ALL_SCOPES);
      int len = srs.length ();
      
      for (int i = 0; i < len ; i++)
	{
	  symbol_record *ptr = srs(i);
	  
	  if (ptr && ptr->is_user_function ())
	    {
	      octave_value tmp = ptr->def ();
	      octave_user_function *tmp_fcn
		= static_cast<octave_user_function *> (tmp.function_value ());
	      symbol_table *st = tmp_fcn->sym_table ();

	      if (st == curr_sym_tab)
		{
		  dbg_fcn = tmp_fcn;
		  break;
		}
	    }
	}

    }
  else if (str.compare (""))
    {
      symbol_record *ptr = curr_sym_tab->lookup (str);
      
      if (ptr && ptr->is_user_function ())
	{
	  octave_value tmp = ptr->def ();
	  dbg_fcn = static_cast<octave_user_function *> (tmp.function_value ());
	}
      else
	{
	  symbol_record *ptr = lookup_by_name (str, false);
	  
	  if (ptr && ptr->is_user_function ())
	    {
	      octave_value tmp = ptr->def ();
	      dbg_fcn = static_cast<octave_user_function *> (tmp.function_value ());
	    }
	}
    }

  return dbg_fcn;
}

DEFUN_DLD (dbg_set, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {rline =} dbg_set (func, line)\n\
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
\n\
@end deftypefn\n\
@seealso{dbg_delete, dbg_list, dbg_cont}")
{
  octave_value_list retval;
  int result = -1;
  int nargin = args.length ();

  if (nargin == 2)
    { 
      if (!args(0).is_string ())
	{
	  gripe_wrong_type_arg ("dbg_set", args(0));
	  return retval;
	}

      std::string symbol_name = args(0).string_value ();
      
      if (!args(1).is_real_scalar ())
	{
	  gripe_wrong_type_arg ("dbg_set", args(1));
	  return retval;
	}

      int line = int(args(1).double_value ());
      octave_user_function *dbg_fcn = get_user_function (symbol_name);

      if (dbg_fcn)
	{
	  tree_statement_list *cmds = dbg_fcn->body ();
	  result = cmds->set_breakpoint (line);
	}
      else
	error ("unable to find the function requested\n");
    }
  else if (nargin == 1)
    {
      if (!args(0).is_real_scalar ())
	{
	  gripe_wrong_type_arg ("dbg_set", args(1));
	  return retval;
	}

      int line = int(args(0).double_value ());
      octave_user_function *dbg_fcn = get_user_function ();
      
      if (dbg_fcn)
	{
	  tree_statement_list *cmds = dbg_fcn->body ();
	  result = cmds->set_breakpoint (line);
	}
      else
	error ("unable to find the function requested\n");	 
    }
  else
    error ("one argument when in a function and two when not.\n");

  retval = double(result);
  return retval;
}

DEFUN_DLD (dbg_delete, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} dbg_delete (func, line)\n\
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
@end deftypefn\n\
@seealso{dbg_delete, dbg_list, dbg_cont}")
{
  octave_value_list retval;
  int line = -1;
  std::string symbol_name = "";
  int nargin = args.length ();
  
  if ((nargin != 1) && (nargin != 2))
    {
      error ("need one or two arguements\n");
      return retval;
    }
  
  if (nargin == 2)
    {
      if (!args(0).is_string ())
	{
	  gripe_wrong_type_arg ("dbg_delete", args(0));
	  return retval;
	}

      std::string symbol_name = args(0).string_value ();
      
      if (!args(1).is_real_scalar ())
	{
	  gripe_wrong_type_arg ("dbg_delete", args(1));
	  return retval;
	}

      line = int(args(1).double_value ());
    }
  else if (nargin == 1)
    {
      if (!args(1).is_real_scalar ())
	{
	  gripe_wrong_type_arg ("dbg_delete", args(1));
	  return retval;
	}

      line = int(args(1).double_value ());      
    }
  else
    {
      error ("need one or two arguements\n");
      return retval;
    }

  octave_user_function *dbg_fcn = get_user_function (symbol_name);
  
  if (dbg_fcn)
    {
      tree_statement_list *cmds = dbg_fcn->body ();
      cmds->delete_breakpoint (line);
    }
  else
    error ("unable to find the function requested\n");
  return retval;
}

DEFUN_DLD (dbg_list, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {lst =} dbg_list ([func])\n\
Return a vector containing the lines on which a function has \n\
breakpoints set.\n\
@table @code\n\
@item func\n\
String representing the function name.  When already in debug\n\
mode this should be left out.\n\
@end table\n\
@end deftypefn\n\
@seealso{dbg_delete, dbg_set, dbg_cont}")
{
  octave_value_list retval;
  octave_value_list lst;
  RowVector vec;
  int nargin = args.length ();

  if ((nargin != 0) && (nargin != 1))
    {
      error ("only zero or one arguements accepted\n");
      return retval;
    }

  std::string symbol_name = "";

  if (nargin == 1)
    {
      if (args(0).is_string ())
	symbol_name = args(0).string_value ();
      else
	gripe_wrong_type_arg ("dbg_list", args(0));
    }

  octave_user_function *dbg_fcn = get_user_function (symbol_name);
 
  if (dbg_fcn)
    {
      tree_statement_list *cmds = dbg_fcn->body ();
      lst = cmds->list_breakpoints ();
      vec = RowVector (lst.length (), 0.0);

      for (int i = 0; i < lst.length (); i++)
	{ 
	  if(lst(i).is_real_scalar ())
	    {
	      double x = lst(i).double_value ();

	      if (! error_state)
		vec(i) = x;
	      else
		panic_impossible ();
	    }
	  else
	    panic_impossible ();
	}
    }
  else
    error ("unable to find the function you requested");

  return octave_value(vec);
}


DEFUN_DLD (dbg_where, , ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} dbg_where ()\n\
Show where we are in the code\n\
@end deftypefn\n")
{
  octave_value retval;

  warning ("not implemented");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
