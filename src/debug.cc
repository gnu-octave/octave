/*

Copyright (C) 2001-2011 Ben Sapp
Copyright (C) 2007-2009 John Swensen

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

#include <deque>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <set>
#include <string>

#include "file-stat.h"

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
#include "ov-struct.h"
#include "pt-pr-code.h"
#include "pt-bp.h"
#include "pt-eval.h"
#include "pt-stmt.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "variables.h"

#include "debug.h"

// Initialize the singleton object
bp_table *bp_table::instance = 0;

static std::string
snarf_file (const std::string& fname)
{
  std::string retval;

  file_stat fs (fname);

  if (fs)
    {
      size_t sz = fs.size ();

      std::ifstream file (fname.c_str (), std::ios::in|std::ios::binary);

      if (file)
        {
          std::string buf (sz+1, 0);

          file.read (&buf[0], sz+1);

          if (file.eof ())
            {
              // Expected to read the entire file.

              retval = buf;
            }
          else
            error ("error reading file %s", fname.c_str ());
        }
    }

  return retval;
}

static std::deque<size_t>
get_line_offsets (const std::string& buf)
{
  // This could maybe be smarter.  Is deque the right thing to use
  // here?

  std::deque<size_t> offsets;

  offsets.push_back (0);

  size_t len = buf.length ();

  for (size_t i = 0; i < len; i++)
    {
      char c = buf[i];

      if (c == '\r' && ++i < len)
        {
          c = buf[i];

          if (c == '\n')
            offsets.push_back (i+1);
          else
            offsets.push_back (i);
        }
      else if (c == '\n')
        offsets.push_back (i+1);
    }

  offsets.push_back (len);

  return offsets;
}

std::string
get_file_line (const std::string& fname, size_t line)
{
  std::string retval;

  static std::string last_fname;

  static std::string buf;

  static std::deque<size_t> offsets;

  if (fname != last_fname)
    {
      buf = snarf_file (fname);

      offsets = get_line_offsets (buf);
    }

  if (line > 0)
    line--;

  if (line < offsets.size () - 1)
    {
      size_t bol = offsets[line];
      size_t eol = offsets[line+1];

      while (eol > 0 && eol > bol && (buf[eol-1] == '\n' || buf[eol-1] == '\r'))
        eol--;

      retval = buf.substr (bol, eol - bol);
    }

  return retval;
}

// Return a pointer to the user-defined function FNAME.  If FNAME is
// empty, search backward for the first user-defined function in the
// current call stack.

static octave_user_code *
get_user_code (const std::string& fname = std::string ())
{
  octave_user_code *dbg_fcn = 0;

  if (fname.empty ())
    dbg_fcn = octave_call_stack::caller_user_code ();
  else
    {
      octave_value fcn = symbol_table::find_function (fname);

      if (fcn.is_defined () && fcn.is_user_code ())
        dbg_fcn = fcn.user_code_value ();
    }

  return dbg_fcn;
}

static void
parse_dbfunction_params (const char *who, const octave_value_list& args, 
                         std::string& symbol_name, bp_table::intmap& lines)
{
  int nargin = args.length ();
  int idx = 0;
  int list_idx = 0;
  symbol_name = std::string ();
  lines = bp_table::intmap ();

  if (args.length () == 0)
    return;

  // If we are already in a debugging function.
  if (octave_call_stack::caller_user_code ())
    {
      idx = 0;
      symbol_name = get_user_code ()->name ();
    }
  else if (args(0).is_map ())
    {
      // Problem because parse_dbfunction_params() can only pass out a
      // single function
    }
  else if (args(0).is_string())
    {
      symbol_name = args(0).string_value ();
      if (error_state)
        return;
      idx = 1;
    }
  else
    error ("%s: invalid parameter specified", who);

  for (int i = idx; i < nargin; i++ )
    {
      if (args(i).is_string ())
        {
          int line = atoi (args(i).string_value().c_str ());
          if (error_state)
            break;
          lines[list_idx++] = line;
        }
      else if (args(i).is_map ())
        octave_stdout << who << ": accepting a struct" << std::endl;
      else
        {
          const NDArray arg = args(i).array_value ();
          
          if (error_state)
            break;
          
          for (octave_idx_type j = 0; j < arg.nelem (); j++)
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

bp_table::intmap
bp_table::do_add_breakpoint (const std::string& fname, 
                             const bp_table::intmap& line)
{
  intmap retval;

  octave_idx_type len = line.size ();

  octave_user_code *dbg_fcn = get_user_code (fname);

  if (dbg_fcn)
    {
      tree_statement_list *cmds = dbg_fcn->body ();

      if (cmds)
        {
          for (int i = 0; i < len; i++)
            {
              const_intmap_iterator p = line.find (i);

              if (p != line.end ())
                {
                  int lineno = p->second;

                  retval[i] = cmds->set_breakpoint (lineno);

                  if (retval[i] != 0)
                    {
                      bp_set.insert (fname);
                    }
                }
            }
        }
    }
  else
    error ("add_breakpoint: unable to find the function requested\n");

  tree_evaluator::debug_mode = bp_table::have_breakpoints () || Vdebugging;

  return retval;
}


int 
bp_table::do_remove_breakpoint (const std::string& fname, 
                                const bp_table::intmap& line)
{
  int retval = 0;

  octave_idx_type len = line.size ();

  if (len == 0)
    {
      intmap results = remove_all_breakpoints_in_file (fname);
      retval = results.size ();
    }
  else
    {
      octave_user_code *dbg_fcn = get_user_code (fname);

      if (dbg_fcn)
        {
          tree_statement_list *cmds = dbg_fcn->body ();

          if (cmds)
            {
              octave_value_list results = cmds->list_breakpoints ();

              if (results.length () > 0)
                {
                  for (int i = 0; i < len; i++)
                    {
                      const_intmap_iterator p = line.find (i);

                      if (p != line.end ())
                        cmds->delete_breakpoint (p->second);
                    }

                  results = cmds->list_breakpoints ();

                  bp_set_iterator it = bp_set.find (fname);
                  if (results.length () == 0 && it != bp_set.end ())
                    bp_set.erase (it);

                }

              retval = results.length ();
            }
        }
      else
        error ("remove_breakpoint: unable to find the function requested\n");
    }

  tree_evaluator::debug_mode = bp_table::have_breakpoints () || Vdebugging;

  return retval;
}


bp_table::intmap
bp_table::do_remove_all_breakpoints_in_file (const std::string& fname, 
                                             bool silent)
{
  intmap retval;

  octave_user_code *dbg_fcn = get_user_code (fname);
  
  if (dbg_fcn)
    {
      tree_statement_list *cmds = dbg_fcn->body ();

      if (cmds)
        {
          octave_value_list bkpts = cmds->list_breakpoints ();

          for (int i = 0; i < bkpts.length (); i++)
            {
              int lineno = static_cast<int> (bkpts(i).int_value ());
              cmds->delete_breakpoint (lineno);
              retval[i] = lineno;
            }

          bp_set_iterator it = bp_set.find (fname);
          if (it != bp_set.end ())
            bp_set.erase (it);

        }
    }
  else if (! silent)
    error ("remove_all_breakpoint_in_file: "
           "unable to find the function requested\n");

  tree_evaluator::debug_mode = bp_table::have_breakpoints () || Vdebugging;

  return retval;
}

void 
bp_table::do_remove_all_breakpoints (void)
{
  for (const_bp_set_iterator it = bp_set.begin (); it != bp_set.end (); it++)
    remove_all_breakpoints_in_file (*it);


  tree_evaluator::debug_mode = bp_table::have_breakpoints () || Vdebugging;
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
          retval = slist(i).string_value ();
          break;
        }
    }

  return retval;
}


bp_table::fname_line_map
bp_table::do_get_breakpoint_list (const octave_value_list& fname_list)
{
  fname_line_map retval;

  for (bp_set_iterator it = bp_set.begin (); it != bp_set.end (); it++)
    {
      if (fname_list.length () == 0
          || do_find_bkpt_list (fname_list, *it) != "")
        {
          octave_user_code *f = get_user_code (*it);

          if (f)
            {
              tree_statement_list *cmds = f->body ();

              if (cmds)
                {
                  octave_value_list bkpts = cmds->list_breakpoints ();
                  octave_idx_type len = bkpts.length (); 

                  if (len > 0)
                    {
                      bp_table::intmap bkpts_vec;
                      
                      for (int i = 0; i < len; i++)
                        bkpts_vec[i] = bkpts (i).double_value ();
                      
                      std::string symbol_name = f->name ();

                      retval[symbol_name] = bkpts_vec;
                    }
                }
            }
        }
    }

  return retval;
}

static octave_value
intmap_to_ov (const bp_table::intmap& line) 
{
  int idx = 0;

  NDArray retval (dim_vector (1, line.size ()));

  for (size_t i = 0; i < line.size (); i++)
    {
      bp_table::const_intmap_iterator p = line.find (i);

      if (p != line.end ())
        {
          int lineno = p->second;
          retval(idx++) = lineno;
        }
    }

  retval.resize (dim_vector (1, idx));

  return retval;
}

DEFUN (dbstop, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{rline} =} dbstop (@var{func}, @var{line}, @dots{})\n\
Set a breakpoint in a function\n\
@table @code\n\
@item func\n\
String representing the function name.  When already in debug\n\
mode this should be left out and only the line should be given.\n\
\n\
@item line\n\
Line number you would like the breakpoint to be set on.  Multiple\n\
lines might be given as separate arguments or as a vector.\n\
@end table\n\
\n\
The rline returned is the real line that the breakpoint was set at.\n\
@seealso{dbclear, dbstatus, dbstep}\n\
@end deftypefn")
{
  bp_table::intmap retval;
  std::string symbol_name;
  bp_table::intmap lines;

  parse_dbfunction_params ("dbstop", args, symbol_name, lines);

  if (lines.size () == 0)
    lines[0] = 1;

  if (! error_state)
    retval = bp_table::add_breakpoint (symbol_name, lines);

  return intmap_to_ov (retval);
}

DEFUN (dbclear, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} dbclear (@var{func}, @var{line}, @dots{})\n\
Delete a breakpoint in a function\n\
@table @code\n\
@item func\n\
String representing the function name.  When already in debug\n\
mode this should be left out and only the line should be given.\n\
\n\
@item line\n\
Line number where you would like to remove the breakpoint.  Multiple\n\
lines might be given as separate arguments or as a vector.\n\
@end table\n\
No checking is done to make sure that the line you requested is really\n\
a breakpoint.  If you get the wrong line nothing will happen.\n\
@seealso{dbstop, dbstatus, dbwhere}\n\
@end deftypefn")
{
  octave_value retval;
  std::string symbol_name = "";
  bp_table::intmap lines;

  parse_dbfunction_params ("dbclear", args, symbol_name, lines);
      
  if (! error_state)
    bp_table::remove_breakpoint (symbol_name, lines);

  return retval;
}

DEFUN (dbstatus, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {lst =} dbstatus (@var{func})\n\
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
  octave_map retval;
  int nargin = args.length ();
  octave_value_list fcn_list;
  bp_table::fname_line_map bp_list;
  std::string symbol_name;

  if (nargin != 0 && nargin != 1)
    {
      error ("dbstatus: only zero or one arguements accepted\n");
      return octave_value ();
    }

  if (nargin == 1)
    {
      if (args(0).is_string ())
        {
          symbol_name = args(0).string_value ();
          fcn_list(0) = symbol_name;
          bp_list = bp_table::get_breakpoint_list (fcn_list);
        }
      else
        gripe_wrong_type_arg ("dbstatus", args(0));
    }
  else
    {
       octave_user_code *dbg_fcn = get_user_code ();
       if (dbg_fcn)
         {
           symbol_name = dbg_fcn->name ();
           fcn_list(0) = symbol_name;
         }

       bp_list = bp_table::get_breakpoint_list (fcn_list);
    }

  if (nargout == 0)
    {
      // Print out the breakpoint information.

      for (bp_table::fname_line_map_iterator it = bp_list.begin ();
           it != bp_list.end (); it++)
        {         
          octave_stdout << "breakpoint in " << it->first << " at line(s) ";

          bp_table::intmap m = it->second;

          size_t nel = m.size ();

          for (size_t j = 0; j < nel; j++)
            octave_stdout << m[j] << ((j < nel - 1) ? ", " : ".");

          if (nel > 0)
            octave_stdout << std::endl;
        }
      return octave_value ();
    }
  else
    {
      // Fill in an array for return.

      int i = 0;
      Cell names (dim_vector (bp_list.size (), 1));
      Cell file (dim_vector (bp_list.size (), 1));
      Cell line (dim_vector (bp_list.size (), 1));

      for (bp_table::const_fname_line_map_iterator it = bp_list.begin ();
           it != bp_list.end (); it++)
        {
          names(i) = it->first;
          line(i) = intmap_to_ov (it->second);
          file(i) = do_which (it->first);
          i++;
        }

      retval.assign ("name", names);
      retval.assign ("file", file);
      retval.assign ("line", line);

      return octave_value (retval);
    }
}

DEFUN (dbwhere, , ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} dbwhere ()\n\
Show where we are in the code\n\
@seealso{dbclear, dbstatus, dbstop}\n\
@end deftypefn")
{
  octave_value retval;

  octave_user_code *dbg_fcn = get_user_code ();

  if (dbg_fcn)
    {
      bool have_file = true;

      std::string name = dbg_fcn->fcn_file_name ();

      if (name.empty ())
        {
          have_file = false;

          name = dbg_fcn->name ();
        }

      octave_stdout << "stopped in " << name << " at ";

      int l = octave_call_stack::caller_user_code_line ();

      if (l > 0)
        {
          octave_stdout << " line " << l << std::endl;

          if (have_file)
            {
              std::string line = get_file_line (name, l);

              if (! line.empty ())
                octave_stdout << l << ": " << line << std::endl;
            }
        }
      else
        octave_stdout << " <unknown line>" << std::endl;
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

DEFUN (dbtype, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} dbtype ()\n\
List script file with line numbers.\n\
@seealso{dbclear, dbstatus, dbstop}\n\
@end deftypefn")
{
  octave_value retval;
  octave_user_code *dbg_fcn;

  int nargin = args.length ();
  string_vector argv = args.make_argv ("dbtype");

  if (! error_state)
    {
      switch (nargin)
        {
        case 0: // dbtype
          dbg_fcn = get_user_code ();

          if (dbg_fcn)
            do_dbtype (octave_stdout, dbg_fcn->name (), 0, INT_MAX);
          else
            error ("dbtype: must be in a user function to give no arguments to dbtype\n");
          break;

        case 1: // (dbtype func) || (dbtype start:end)
          dbg_fcn = get_user_code (argv[1]);

          if (dbg_fcn)
            do_dbtype (octave_stdout, dbg_fcn->name (), 0, INT_MAX);
          else
            {
              dbg_fcn = get_user_code ();

              if (dbg_fcn)
                {
                  std::string arg = argv[1];

                  size_t ind = arg.find (':');

                  if (ind != std::string::npos)
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
          dbg_fcn = get_user_code (argv[1]);

          if (dbg_fcn)
            {
              std::string arg = argv[2];
              int start = 0;
              int end = 0;
              size_t ind = arg.find (':');

              if (ind != std::string::npos)
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

DEFUN (dbstack, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{stack}, @var{idx}]} dbstack (@var{n})\n\
Print or return current stack information.  With optional argument\n\
@var{n}, omit the @var{n} innermost stack frames.\n\
@seealso{dbclear, dbstatus, dbstop}\n\
@end deftypefn")
{
  octave_value_list retval;

  unwind_protect frame;

  octave_idx_type curr_frame = -1;

  size_t nskip = 0;

  if (args.length () == 1)
    {
      int n = 0;

      octave_value arg = args(0);

      if (arg.is_string ())
        {
          std::string s_arg = arg.string_value ();

          n = atoi (s_arg.c_str ());
        }
      else
        n = args(0).int_value ();

      if (n > 0)
        nskip = n;
      else
        error ("dbstack: expecting N to be a nonnegative integer");
    }

  if (! error_state)
    {
      octave_map stk = octave_call_stack::backtrace (nskip, curr_frame);

      if (nargout == 0)
        {
          octave_idx_type nframes_to_display = stk.numel ();

          if (nframes_to_display > 0)
            {
              octave_stdout << "stopped in:\n\n";

              Cell names = stk.contents ("name");
              Cell files = stk.contents ("file");
              Cell lines = stk.contents ("line");

              bool show_top_level = true;

              size_t max_name_len = 0;

              for (octave_idx_type i = 0; i < nframes_to_display; i++)
                {
                  std::string name = names(i).string_value ();

                  max_name_len = std::max (name.length (), max_name_len);
                }

              for (octave_idx_type i = 0; i < nframes_to_display; i++)
                {
                  std::string name = names(i).string_value ();
                  std::string file = files(i).string_value ();
                  int line = lines(i).int_value ();

                  if (show_top_level && i == curr_frame)
                    show_top_level = false;

                  octave_stdout << (i == curr_frame ? "  --> " : "      ")
                                << std::setw (max_name_len) << name
                                << " at line " << line
                                << " [" << file << "]"
                                << std::endl;
                }

              if (show_top_level)
                octave_stdout << "  --> top level" << std::endl;
            }
        }
      else
        {
          retval(1) = curr_frame < 0 ? 1 : curr_frame + 1;
          retval(0) = stk;
        }
    }

  return retval;
}

static void
do_dbupdown (const octave_value_list& args, const std::string& who)
{
  int n = 1;

  if (args.length () == 1)
    {
      octave_value arg = args(0);

      if (arg.is_string ())
        {
          std::string s_arg = arg.string_value ();

          n = atoi (s_arg.c_str ());
        }
      else
        n = args(0).int_value ();
    }

  if (! error_state)
    {
      if (who == "dbup")
        n = -n;

      if (! octave_call_stack::goto_frame_relative (n, true))
        error ("%s: invalid stack frame", who.c_str ());
    }
}

DEFUN (dbup, args, ,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {} dbup\n\
@deftypefnx {Loadable Function} {} dbup (@var{n})\n\
In debugging mode, move up the execution stack @var{n} frames.\n\
If @var{n} is omitted, move up one frame.\n\
@seealso{dbstack}\n\
@end deftypefn")
{
  octave_value retval;

  do_dbupdown (args, "dbup");

  return retval;
}

DEFUN (dbdown, args, ,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {} dbdown\n\
@deftypefnx {Loadable Function} {} dbdown (@var{n})\n\
In debugging mode, move down the execution stack @var{n} frames.\n\
If @var{n} is omitted, move down one frame.\n\
@seealso{dbstack}\n\
@end deftypefn")
{
  octave_value retval;

  do_dbupdown (args, "dbdown");

  return retval;
}

DEFUN (dbstep, args, ,
  "-*- texinfo -*-\n\
@deftypefn  {Command} {} dbstep\n\
@deftypefnx {Command} {} dbstep @var{n}\n\
@deftypefnx {Command} {} dbstep in\n\
@deftypefnx {Command} {} dbstep out\n\
In debugging mode, execute the next @var{n} lines of code.\n\
If @var{n} is omitted , execute the next single line of code.\n\
If the next line of code is itself\n\
defined in terms of an m-file remain in the existing function.\n\
\n\
Using @code{dbstep in} will cause execution of the next line to step into\n\
any m-files defined on the next line.  Using @code{dbstep out} will cause\n\
execution to continue until the current function returns.\n\
@seealso{dbcont, dbquit}\n\
@end deftypefn")
{
  if (Vdebugging)
    {
      int nargin = args.length ();
      
      if (nargin > 1)
        print_usage ();
      else if (nargin == 1)
        {
          if (args(0).is_string ())
            {
              std::string arg = args(0).string_value ();

              if (! error_state)
                {
                  if (arg == "in")
                    {
                      Vdebugging = false;

                      tree_evaluator::dbstep_flag = -1;
                    }
                  else if (arg == "out")
                    {
                      Vdebugging = false;

                      tree_evaluator::dbstep_flag = -2;
                    }
                  else
                    {
                      int n = atoi (arg.c_str ());

                      if (n > 0)
                        {
                          Vdebugging = false;

                          tree_evaluator::dbstep_flag = n;
                        }
                      else
                        error ("dbstep: invalid argument");
                    }
                }
            }
          else
            error ("dbstep: expecting character string as argument");
        }
      else
        {
          Vdebugging = false;

          tree_evaluator::dbstep_flag = 1;
        }
    }
  else
    error ("dbstep: can only be called in debug mode");

  return octave_value_list ();
}

DEFALIAS (dbnext, dbstep);

DEFUN (dbcont, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Command} {} dbcont\n\
In debugging mode, quit debugging mode and continue execution.\n\
@seealso{dbstep, dbquit}\n\
@end deftypefn")
{
  if (Vdebugging)
    {
      if (args.length () == 0)
        {
          Vdebugging = false;

          tree_evaluator::reset_debug_state ();
        }
      else
        print_usage ();
    }
  else
    error ("dbcont: can only be called in debug mode");

  return octave_value_list ();
}

DEFUN (dbquit, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Command} {} dbquit\n\
In debugging mode, quit debugging mode and return to the top level.\n\
@seealso{dbstep, dbcont}\n\
@end deftypefn")
{
  if (Vdebugging)
    {
      if (args.length () == 0)
        {
          Vdebugging = false;

          tree_evaluator::reset_debug_state ();

          octave_throw_interrupt_exception ();
        }
      else
        print_usage ();
    }
  else
    error ("dbquit: can only be called in debug mode");

  return octave_value_list ();
}

DEFUN (isdebugmode, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} isdebugmode ()\n\
Return true if debug mode is on, otherwise false.\n\
@seealso{dbstack, dbclear, dbstop, dbstatus}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 0)
    retval = Vdebugging;
  else
    print_usage ();

  return retval;
}
