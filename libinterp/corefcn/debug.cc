/*

Copyright (C) 2001-2018 Ben Sapp
Copyright (C) 2007-2009 John Swensen

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <deque>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <limits>
#include <set>
#include <string>

#include "dNDArray.h"

#include "bp-table.h"
#include "call-stack.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "file-ops.h"
#include "help.h"
#include "input.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "octave-preserve-stream-state.h"
#include "ov-usr-fcn.h"
#include "ov.h"
#include "ovl.h"
#include "pager.h"
#include "parse.h"
#include "pt-eval.h"
#include "unwind-prot.h"
#include "utils.h"
#include "utils.h"
#include "variables.h"

static octave_value
intmap_to_ov (const octave::bp_table::intmap& line)
{
  int idx = 0;

  NDArray retval (dim_vector (1, line.size ()));

  for (size_t i = 0; i < line.size (); i++)
    {
      auto p = line.find (i);

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
       doc: /* -*- texinfo -*-
@deftypefn  {} {} dbstop @var{func}
@deftypefnx {} {} dbstop @var{func} @var{line}
@deftypefnx {} {} dbstop @var{func} @var{line1} @var{line2} @dots{}
@deftypefnx {} {} dbstop @var{line1} @dots{}
@deftypefnx {} {} dbstop in @var{func}
@deftypefnx {} {} dbstop in @var{func} at @var{line}
@deftypefnx {} {} dbstop in @var{func} at @var{line} if "@var{condition}"
@deftypefnx {} {} dbstop if @var{event}
@deftypefnx {} {} dbstop if @var{event} @var{ID}
@deftypefnx {} {} dbstop (@var{bp_struct})
@deftypefnx {} {@var{rline} =} dbstop @dots{}

Set breakpoints for the built-in debugger.

@var{func} is the name of a function on the current @code{path}.  When
already in debug mode the @var{func} argument can be omitted and the current
function will be used.  Breakpoints at subfunctions are set with the scope
operator @samp{>}.  For example, If @file{file.m} has a subfunction
@code{func2}, then a breakpoint in @code{func2} can be specified by
@code{file>func2}.

@var{line} is the line number at which to break.  If @var{line} is not
specified, it defaults to the first executable line in the file
@file{func.m}.  Multiple lines can be specified in a single command; when
function syntax is used, the lines may also be passed as a single vector
argument (@code{[@var{line1}, @var{line2}, @dots{}]}).

@var{condition} is any Octave expression that can be evaluated in the code
context that exists at the breakpoint.  When the breakpoint is encountered,
@var{condition} will be evaluated, and execution will stop if
@var{condition} is true.  If @var{condition} cannot be evaluated, for
example because it refers to an undefined variable, an error will be thrown.
 Expressions with side effects (such as @code{y++ > 1}) will alter
variables, and should generally be avoided.  Conditions containing quotes
(@samp{"}, @samp{'}) or comment characters (@samp{#}, @samp{%}) must be
enclosed in quotes.  (This does not apply to conditions entered from the
editor's context menu.)  For example:

@example
dbstop in strread at 209 if 'any (format == "%f")'
@end example

The form specifying @var{event} does not cause a specific breakpoint at a
given function and line number.  Instead it causes debug mode to be entered
when certain unexpected events are encountered.  Possible values are

@table @code
@item error
Stop when an error is reported.  This is equivalent to specifying
both @code{debug_on_error (true)} and @code{debug_on_interrupt (true)}.

@item caught error
Stop when an error is caught by a try-catch block (not yet implemented).

@item interrupt
Stop when an interrupt (@kbd{Ctrl-C}) occurs.

@item naninf
Stop when code returns a non-finite value (not yet implemented).

@item warning
Stop when a warning is reported.  This is equivalent to specifying
@code{debug_on_warning (true)}.
@end table

The events @code{error}, @code{caught error}, and @code{warning} can all be
followed by a string specifying an error ID or warning ID@.  If that is
done, only errors with the specified ID will cause execution to stop.  To
stop on one of a set of IDs, multiple @code{dbstop} commands must be
issued.

Breakpoints and events can be removed using the @code{dbclear} command with
the same syntax.

It is possible to save all breakpoints and restore them at once by issuing
the commands @code{bp_state = dbstatus; @dots{}; dbstop (bp_state)}.

The optional output @var{rline} is the real line number where the breakpoint
was set.  This can differ from the specified line if the line is not
executable.  For example, if a breakpoint attempted on a blank line then
Octave will set the real breakpoint at the next executable line.

When a file is re-parsed, such as when it is modified outside the GUI,
all breakpoints within the file are cleared.

@seealso{dbclear, dbstatus, dbstep, debug_on_error, debug_on_warning, debug_on_interrupt}
@end deftypefn */)
{
  octave::bp_table::intmap retmap;
  std::string symbol_name = "";  // stays empty for "dbstop if error" etc
  octave::bp_table::intmap lines;
  std::string condition = "";
  octave_value retval;

  octave::bp_table& bptab = octave::__get_bp_table__ ("Fdbstop");

  if (args.length() >= 1 && ! args(0).isstruct ())
    {
      // explicit function / line / condition
      bptab.parse_dbfunction_params ("dbstop", args, symbol_name,
                                     lines, condition);

      if (lines.size () == 0)
        lines[0] = 1;

      if (symbol_name != "")
        {
          retmap = bptab.add_breakpoint (symbol_name, lines, condition);
          retval = intmap_to_ov (retmap);
        }
    }
  else if (args.length () != 1)
    {
      print_usage ();
    }
  else  // structure of the form output by dbstatus
    {
      octave_map mv = args(0).map_value ();
      if (mv.isfield ("bkpt") || mv.isfield ("errs") || mv.isfield ("warn")
          || mv.isfield ("intr"))
        {
          bptab.dbstop_process_map_args (mv);

          // Replace mv by "bkpt", to use the processing below.
          octave_value bkpt = mv.getfield ("bkpt");
          if (bkpt.isempty ())
            mv = octave_map ();
          else
            {
              if (bkpt.iscell () && bkpt.cell_value ().numel () > 0
                  && bkpt.cell_value () (0).isstruct ())
                mv = bkpt.cell_value () (0).map_value ();
              else
                {
                  error ("dbstop: invalid 'bkpt' field");
                  mv = octave_map ();
                }
            }
        }
      if (mv.isempty ())
        {
          // no changes requested.  Occurs if "errs" non-empty but "bkpt" empty
        }
      else if (! mv.isfield ("name") || ! mv.isfield ("line"))
        {
          error ("dbstop: Cell array must contain fields 'name' and 'line'");
          retval = octave_value (0);
        }
      else
        {
          bool use_cond = mv.isfield ("cond");
          Cell name = mv.getfield ("name");
          Cell line = mv.getfield ("line");
          Cell cond = (use_cond ? mv.getfield ("cond") : Cell ());
          std::string unconditional = "";
          for (octave_idx_type i = 0; i < line.numel (); i++)
            {
              lines [0] = line(i).double_value ();
              bptab.add_breakpoint (name(i).string_value (), lines,
                                    (use_cond
                                     ? cond(i).string_value ()
                                     : unconditional));
            }
          retval = octave_value (line.numel ());
        }
    }

  return retval;
}

DEFUN (dbclear, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} dbclear @var{func}
@deftypefnx {} {} dbclear @var{func} @var{line}
@deftypefnx {} {} dbclear @var{func} @var{line1} @var{line2} @dots{}
@deftypefnx {} {} dbclear @var{line} @dots{}
@deftypefnx {} {} dbclear all
@deftypefnx {} {} dbclear in @var{func}
@deftypefnx {} {} dbclear in @var{func} at @var{line}
@deftypefnx {} {} dbclear if @var{event}
@deftypefnx {} {} dbclear ("@var{func}")
@deftypefnx {} {} dbclear ("@var{func}", @var{line})
@deftypefnx {} {} dbclear ("@var{func}", @var{line1}, @var{line2}, @dots{})
@deftypefnx {} {} dbclear ("@var{func}", @var{line1}, @dots{})
@deftypefnx {} {} dbclear (@var{line}, @dots{})
@deftypefnx {} {} dbclear ("all")
Delete a breakpoint at line number @var{line} in the function @var{func}.

Arguments are

@table @var
@item func
Function name as a string variable.  When already in debug mode this
argument can be omitted and the current function will be used.

@item line
Line number from which to remove a breakpoint.  Multiple lines may be given
as separate arguments or as a vector.

@item event
An event such as @code{error}, @code{interrupt}, or @code{warning}
(@pxref{XREFdbstop,,dbstop} for details).
@end table

When called without a line number specification all breakpoints in the named
function are cleared.

If the requested line is not a breakpoint no action is performed.

The special keyword @qcode{"all"} will clear all breakpoints from all
files.
@seealso{dbstop, dbstatus, dbwhere}
@end deftypefn */)
{
  std::string symbol_name = "";  // stays empty for "dbclear if error" etc
  octave::bp_table::intmap lines;
  std::string dummy;             // "if" condition -- only used for dbstop

  int nargin = args.length ();

  octave::bp_table& bptab = octave::__get_bp_table__ ("Fdbclear");

  bptab.parse_dbfunction_params ("dbclear", args, symbol_name, lines, dummy);

  if (nargin == 1 && symbol_name == "all")
    {
      bptab.remove_all_breakpoints ();
      bptab.dbclear_all_signals ();
    }
  else
    {
      if (symbol_name != "")
        bptab.remove_breakpoint (symbol_name, lines);
    }

  return ovl ();
}

DEFMETHOD (dbstatus, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} dbstatus
@deftypefnx {} {} dbstatus @var{func}
@deftypefnx {} {@var{bp_list} =} dbstatus @dots{}
Report the location of active breakpoints.

When called with no input or output arguments, print the list of all
functions with breakpoints and the line numbers where those breakpoints are
set.

If a function name @var{func} is specified then only report breakpoints
for the named function and its subfunctions.

The optional return argument @var{bp_list} is a struct array with the
following fields.

@table @asis
@item name
The name of the function with a breakpoint.  A subfunction, say @code{func2}
within an m-file, say @file{file.m}, is specified as @code{file>func2}.

@item file
The name of the m-file where the function code is located.

@item line
The line number with the breakpoint.

@item cond
The condition that must be satisfied for the breakpoint to be active, or
the empty string for unconditional breakpoints.
@end table

@c Note: When @code{dbstatus} is called from the debug prompt within a function,
@c the list of breakpoints is automatically trimmed to the breakpoints in the
@c current function.
If @code{dbstop if error} is true but no explicit IDs are specified, the
return value will have an empty field called @qcode{"errs"}.  If IDs are
specified, the @code{errs} field will have one row per ID@.  If
@code{dbstop if error} is false, there is no @qcode{"errs"} field.
The @qcode{"warn"} field is set similarly by @code{dbstop if warning}.

@seealso{dbstop, dbclear, dbwhere, dblist, dbstack}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin != 0 && nargin != 1)
    error ("dbstatus: only zero or one arguments accepted\n");

  octave_value_list fcn_list;
  octave::bp_table::fname_bp_map bp_list;
  std::string symbol_name;

  octave::bp_table& bptab = octave::__get_bp_table__ ("Fdbstatus");

  if (nargin == 1)
    {
      if (! args(0).is_string ())
        err_wrong_type_arg ("dbstatus", args(0));

      symbol_name = args(0).string_value ();
      fcn_list(0) = symbol_name;
      bp_list = bptab.get_breakpoint_list (fcn_list);
    }
  else
    {
/*
      if (Vdebugging)
        {
          octave_user_code *dbg_fcn = get_user_code ();
          if (dbg_fcn)
            {
              symbol_name = dbg_fcn->name ();
              fcn_list(0) = symbol_name;
            }
        }
*/

      bp_list = bptab.get_breakpoint_list (fcn_list);
    }

  if (nargout == 0)
    {
      // Print out the breakpoint information.

      for (auto& fnm_bp_p: bp_list)
        {
          std::list<octave::bp_type> m = fnm_bp_p.second;

          // print unconditional breakpoints, if any, on a single line

          // first, check to see if there are any
          int have_unconditional = 0;
          for (const auto& bp : m)
            {
              if (bp.cond == "")
                {
                  if (have_unconditional++)
                    break;                   // stop once we know its plural
                }
            }
          // If we actually have some, print line numbers only
          if (have_unconditional)
            {
              const char *_s_ = (have_unconditional > 1) ? "s" : "";
              octave_stdout << "breakpoint" << _s_ << " in " << fnm_bp_p.first
                            << " at line" << _s_ << ' ';

              for (const auto& bp : m)
                {
                  if (bp.cond == "")
                    octave_stdout << bp.line << ' ';
                }
              octave_stdout << std::endl;
            }

          // print conditional breakpoints, one per line, with conditions
          for (const auto& bp : m)
            {
              if (bp.cond != "")
                octave_stdout << "breakpoint in " << fnm_bp_p.first
                              << " at line " << bp.line
                              << " if " << bp.cond << "\n";
            }
        }

      bptab.stop_on_err_warn_status (true);

      return ovl ();
    }
  else
    {
      octave::help_system& help_sys = interp.get_help_system ();

      // Fill in an array for return.
      int i = 0;
      octave_map retmap;
      octave_value retval;

      // count the number of breakpoints in all files
      int count = 0;
      for (const auto& fnm_bp_p : bp_list)
        count += fnm_bp_p.second.size ();

      Cell names (dim_vector (count, 1));
      Cell file  (dim_vector (count, 1));
      Cell line  (dim_vector (count, 1));
      Cell cond  (dim_vector (count, 1));

      for (const auto& fnm_bp_p : bp_list)
        {
          std::string filename = fnm_bp_p.first;
          const char *sub_fun = strchr (filename.c_str (), '>');
          if (sub_fun)
            filename = filename.substr(0, sub_fun - filename.c_str ());
          octave_value path_name;
          path_name
            = octave::sys::canonicalize_file_name (help_sys.which (filename));

          for (const auto& bp : fnm_bp_p.second)
            {
              names(i) = fnm_bp_p.first;
              file(i) = path_name;
              line(i) = octave_value (bp.line);
              cond(i) = octave_value (bp.cond);
              i++;
            }
        }

      retmap.assign ("name", names);
      retmap.assign ("file", file);
      retmap.assign ("line", line);
      retmap.assign ("cond", cond);

      octave_map ew = bptab.stop_on_err_warn_status (false);
      if (ew.isempty ())
        {
          retval = octave_value (retmap);
        }
      else
        {
          octave_map outer (dim_vector (3,1));
          outer.assign ("bkpt", Cell (retmap));
          for (auto f = ew.begin (); f != ew.end (); f++)
            outer.setfield (f->first, ew.contents (f));

          retval = octave_value (outer);
        }

      return retval;
    }
}

/*
%!test
%! dbclear all;   # Clear out breakpoints before test
%! dbstop @ftp/dir;
%! dbstop @audioplayer/set 70;
%! dbstop quantile>__quantile__;
%! dbstop ls;
%! s = dbstatus;
%! dbclear all;
%! assert (s(1).name, "@audioplayer/set>setproperty");
%! assert (s(2).name, "@ftp/dir");
%! assert (s(3).name, "ls");
%! assert (s(4).name, "quantile>__quantile__");
%! assert (s(2).file(end-10:end), [filesep "@ftp" filesep "dir.m"]);
*/

DEFMETHOD (dbwhere, interp, , ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} dbwhere
In debugging mode, report the current file and line number where execution
is stopped.
@seealso{dbstack, dblist, dbstatus, dbcont, dbstep, dbup, dbdown}
@end deftypefn */)
{
  octave_user_code *dbg_fcn = octave::get_user_code ();

  if (! dbg_fcn)
    {
      octave_stdout << "stopped at top level" << std::endl;
      return ovl ();
    }

  octave_stdout << "stopped in " << dbg_fcn->name () << " at ";

  octave::call_stack& cs = interp.get_call_stack ();

  int l = cs.debug_user_code_line ();

  if (l > 0)
    {
      octave_stdout << "line " << l;

      std::string file_name = dbg_fcn->fcn_file_name ();

      if (! file_name.empty ())
        {
          octave_stdout << " [" << file_name << ']' << std::endl;

          std::string line = dbg_fcn->get_code_line (l);

          if (! line.empty ())
            octave_stdout << l << ": " << line << std::endl;
        }
      else
        octave_stdout << std::endl;
    }
  else
    octave_stdout << "<unknown line>" << std::endl;

  return ovl ();
}

static void
do_dbtype (std::ostream& os, const std::string& name, int start, int end)
{
  std::string ff = fcn_file_in_path (name);

  if (ff.empty ())
    os << "dbtype: unknown function " << name << "\n";
  else
    {
      std::ifstream fs (ff.c_str (), std::ios::in);

      if (! fs)
        os << "dbtype: unable to open '" << ff << "' for reading!\n";
      else
        {
          int line = 1;
          std::string text;

          while (std::getline (fs, text) && line <= end)
            {
              if (line >= start)
                os << line << "\t" << text << "\n";

              line++;
            }
        }
    }

  os.flush ();
}

DEFUN (dbtype, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} dbtype
@deftypefnx {} {} dbtype @var{lineno}
@deftypefnx {} {} dbtype @var{startl:endl}
@deftypefnx {} {} dbtype @var{startl:end}
@deftypefnx {} {} dbtype @var{func}
@deftypefnx {} {} dbtype @var{func} @var{lineno}
@deftypefnx {} {} dbtype @var{func} @var{startl:endl}
@deftypefnx {} {} dbtype @var{func} @var{startl:end}
Display a script file with line numbers.

When called with no arguments in debugging mode, display the script file
currently being debugged.

An optional range specification can be used to list only a portion of the
file.  The special keyword @qcode{"end"} is a valid line number
specification for the last line of the file.

When called with the name of a function, list that script file with line
numbers.
@seealso{dblist, dbwhere, dbstatus, dbstop}
@end deftypefn */)
{
  octave_user_code *dbg_fcn;

  string_vector argv = args.make_argv ("dbtype");

  switch (args.length ())
    {
    case 0:  // dbtype
      dbg_fcn = octave::get_user_code ();

      if (! dbg_fcn)
        error ("dbtype: must be inside a user function to give no arguments to dbtype\n");

      do_dbtype (octave_stdout, dbg_fcn->fcn_file_name (),
                 0, std::numeric_limits<int>::max ());

      break;

    case 1:  // (dbtype start:end) || (dbtype func) || (dbtype lineno)
      {
        std::string arg = argv[1];

        size_t ind = arg.find (':');

        if (ind != std::string::npos)  // (dbtype start:end)
          {
            dbg_fcn = octave::get_user_code ();

            if (dbg_fcn)
              {
                std::string start_str = arg.substr (0, ind);
                std::string end_str = arg.substr (ind + 1);

                int start, end;
                start = atoi (start_str.c_str ());
                if (end_str == "end")
                  end = std::numeric_limits<int>::max ();
                else
                  end = atoi (end_str.c_str ());

                if (std::min (start, end) <= 0)
                  error ("dbtype: start and end lines must be >= 1\n");

                if (start > end)
                  error ("dbtype: start line must be less than end line\n");

                do_dbtype (octave_stdout, dbg_fcn->fcn_file_name (),
                           start, end);
              }
          }
        else  // (dbtype func) || (dbtype lineno)
          {
            int line = atoi (arg.c_str ());

            if (line == 0)  // (dbtype func)
              {
                dbg_fcn = octave::get_user_code (arg);

                if (! dbg_fcn)
                  error ("dbtype: function <%s> not found\n", arg.c_str ());

                do_dbtype (octave_stdout, dbg_fcn->fcn_file_name (),
                           0, std::numeric_limits<int>::max ());
              }
            else  // (dbtype lineno)
              {
                if (line <= 0)
                  error ("dbtype: start and end lines must be >= 1\n");

                dbg_fcn = octave::get_user_code ();

                if (dbg_fcn)
                  do_dbtype (octave_stdout, dbg_fcn->fcn_file_name (),
                             line, line);
              }
          }
      }
      break;

    case 2:  // (dbtype func start:end) || (dbtype func start)
      {
        dbg_fcn = octave::get_user_code (argv[1]);

        if (! dbg_fcn)
          error ("dbtype: function <%s> not found\n", argv[1].c_str ());

        std::string arg = argv[2];
        int start, end;
        size_t ind = arg.find (':');

        if (ind != std::string::npos)
          {
            std::string start_str = arg.substr (0, ind);
            std::string end_str = arg.substr (ind + 1);

            start = atoi (start_str.c_str ());
            if (end_str == "end")
              end = std::numeric_limits<int>::max ();
            else
              end = atoi (end_str.c_str ());
          }
        else
          {
            start = atoi (arg.c_str ());
            end = start;
          }

        if (std::min (start, end) <= 0)
          error ("dbtype: start and end lines must be >= 1\n");

        if (start > end)
          error ("dbtype: start line must be less than end line\n");

        do_dbtype (octave_stdout, dbg_fcn->fcn_file_name (), start, end);
      }
      break;

    default:
      error ("dbtype: expecting zero, one, or two arguments\n");
    }

  return ovl ();
}

DEFMETHOD (dblist, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} dblist
@deftypefnx {} {} dblist @var{n}
In debugging mode, list @var{n} lines of the function being debugged
centered around the current line to be executed.

If unspecified @var{n} defaults to 10 (+/- 5 lines)
@seealso{dbwhere, dbtype, dbstack}
@end deftypefn */)
{
  int n = 10;

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

      if (n < 0)
        error ("dblist: N must be a non-negative integer");
    }

  octave_user_code *dbg_fcn = octave::get_user_code ();

  if (! dbg_fcn)
    error ("dblist: must be inside a user function to use dblist\n");

  bool have_file = true;

  std::string name = dbg_fcn->fcn_file_name ();

  if (name.empty ())
    {
      have_file = false;
      name = dbg_fcn->name ();
    }

  octave::call_stack& cs = interp.get_call_stack ();

  int l = cs.debug_user_code_line ();

  if (l > 0)
    {
      if (have_file)
        {
          int l_min = std::max (l - n/2, 0);
          int l_max = l + n/2;
          do_dbtype (octave_stdout, name, l_min, l-1);

          std::string line = dbg_fcn->get_code_line (l);

          if (! line.empty ())
            octave_stdout << l << "-->\t" << line << std::endl;

          do_dbtype (octave_stdout, name, l+1, l_max);
        }
    }
  else
    {
      octave_stdout << "dblist: unable to determine source code line"
                    << std::endl;
    }

  return ovl ();
}

static octave_value_list
do_dbstack (octave::interpreter& interp, const octave_value_list& args,
            int nargout, std::ostream& os)
{
  int nargin = args.length ();

  if (nargin > 2)
    print_usage ();

  octave_value_list retval;

  octave::unwind_protect frame;

  octave_idx_type curr_frame = -1;

  size_t nskip = 0;

  if (nargin == 1 || nargin == 2)
    {
      int n = 0;

      for (octave_idx_type i = 0; i < nargin; i++)
        {
          octave_value arg = args(i);

          if (arg.is_string ())
            {
              std::string s_arg = arg.string_value ();

              // Skip "-completenames", octave returns full names anyway.
              if (s_arg == "-completenames")
                continue;

              n = atoi (s_arg.c_str ());
            }
          else
            n = arg.int_value ();

          if (n <= 0)
            error ("dbstack: N must be a non-negative integer");
        }

      if (n > 0)
        nskip = n;
    }

  octave::call_stack& cs = interp.get_call_stack ();

  if (nargout == 0)
    {
      octave_map stk = cs.backtrace (nskip, curr_frame);
      octave_idx_type nframes_to_display = stk.numel ();

      if (nframes_to_display > 0)
        {
          octave::preserve_stream_state stream_state (os);

          os << "stopped in:\n\n";

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

              os << (i == curr_frame ? "  --> " : "      ")
                 << std::setw (max_name_len) << name
                 << " at line " << line
                 << " [" << file << ']'
                 << std::endl;
            }

          if (show_top_level)
            os << "  --> top level" << std::endl;
        }
    }
  else
    {
      octave_map stk = cs.backtrace (nskip, curr_frame, false);

      retval = ovl (stk, curr_frame < 0 ? 1 : curr_frame + 1);
    }

  return retval;
}

// A function that can be easily called from a debugger print the Octave stack.
// This can be useful for finding what line of code the interpreter is
// currently executing when the debugger is stopped in some C++ function,
// for example.

void
show_octave_dbstack (void)
{
  do_dbstack (octave::__get_interpreter__ ("show_octave_dbstack"),
              octave_value_list (), 0, std::cerr);
}

DEFMETHOD (dbstack, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} dbstack
@deftypefnx {} {} dbstack @var{n}
@deftypefnx {} {} dbstack @var{-completenames}
@deftypefnx {} {[@var{stack}, @var{idx}] =} dbstack (@dots{})
Display or return current debugging function stack information.

With optional argument @var{n}, omit the @var{n} innermost stack frames.

Although accepted, the argument @var{-completenames} is silently ignored.
Octave always returns absolute filenames.

The arguments @var{n} and @var{-completenames} can be both specified in any
order.

The optional return argument @var{stack} is a struct array with the
following fields:

@table @asis
@item file
The name of the m-file where the function code is located.

@item name
The name of the function with a breakpoint.

@item line
The line number of an active breakpoint.

@item column
The column number of the line where the breakpoint begins.

@item scope
Undocumented.

@item context
Undocumented.
@end table

The return argument @var{idx} specifies which element of the @var{stack}
struct array is currently active.
@seealso{dbup, dbdown, dbwhere, dblist, dbstatus}
@end deftypefn */)
{
  return do_dbstack (interp, args, nargout, octave_stdout);
}

static void
do_dbupdown (octave::interpreter& interp, const octave_value_list& args,
             const std::string& who)
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

  if (who == "dbup")
    n = -n;

  octave::call_stack& cs = interp.get_call_stack ();

  if (! cs.goto_frame_relative (n, true))
    error ("%s: invalid stack frame", who.c_str ());
}

DEFMETHOD (dbup, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} dbup
@deftypefnx {} {} dbup @var{n}
In debugging mode, move up the execution stack @var{n} frames.

If @var{n} is omitted, move up one frame.
@seealso{dbstack, dbdown}
@end deftypefn */)
{
  do_dbupdown (interp, args, "dbup");

  return ovl ();
}

DEFMETHOD (dbdown, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} dbdown
@deftypefnx {} {} dbdown @var{n}
In debugging mode, move down the execution stack @var{n} frames.

If @var{n} is omitted, move down one frame.
@seealso{dbstack, dbup}
@end deftypefn */)
{
  do_dbupdown (interp, args, "dbdown");

  return ovl ();
}

DEFUN (dbstep, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} dbstep
@deftypefnx {} {} dbstep @var{n}
@deftypefnx {} {} dbstep in
@deftypefnx {} {} dbstep out
@deftypefnx {} {} dbnext @dots{}
In debugging mode, execute the next @var{n} lines of code.

If @var{n} is omitted, execute the next single line of code.  If the next
line of code is itself defined in terms of an m-file remain in the existing
function.

Using @code{dbstep in} will cause execution of the next line to step into
any m-files defined on the next line.

Using @code{dbstep out} will cause execution to continue until the current
function returns.

@code{dbnext} is an alias for @code{dbstep}.
@seealso{dbcont, dbquit}
@end deftypefn */)
{
  if (! Vdebugging)
    error ("dbstep: can only be called in debug mode");

  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  if (nargin == 1)
    {
      std::string arg = args(0).xstring_value ("dbstep: input argument must be a string");

      if (arg == "in")
        {
          Vdebugging = false;
          Vtrack_line_num = true;

          octave::tree_evaluator::dbstep_flag = -1;
        }
      else if (arg == "out")
        {
          Vdebugging = false;
          Vtrack_line_num = true;

          octave::tree_evaluator::dbstep_flag = -2;
        }
      else
        {
          int n = atoi (arg.c_str ());

          if (n < 1)
            error ("dbstep: invalid argument");

          Vdebugging = false;
          Vtrack_line_num = true;

          octave::tree_evaluator::dbstep_flag = n;
        }
    }
  else
    {
      Vdebugging = false;
      Vtrack_line_num = true;

      octave::tree_evaluator::dbstep_flag = 1;
    }

  return ovl ();
}

DEFALIAS (dbnext, dbstep);

DEFUN (dbcont, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} dbcont
Leave command-line debugging mode and continue code execution normally.
@seealso{dbstep, dbquit}
@end deftypefn */)
{
  if (! Vdebugging)
    error ("dbcont: can only be called in debug mode");

  if (args.length () != 0)
    print_usage ();

  Vdebugging = false;
  Vtrack_line_num = true;

  octave::tree_evaluator::reset_debug_state ();

  return ovl ();
}

DEFUN (dbquit, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} dbquit
Quit debugging mode immediately without further code execution and return to
the Octave prompt.
@seealso{dbcont, dbstep}
@end deftypefn */)
{
  if (! Vdebugging)
    error ("dbquit: can only be called in debug mode");

  if (args.length () != 0)
    print_usage ();

  // FIXME: there are too many debug mode flags!

  Vdebugging = false;

  octave::tree_evaluator::reset_debug_state ();
  octave::tree_evaluator::debug_mode = false;

  throw octave::interrupt_exception ();

  return ovl ();
}

DEFUN (isdebugmode, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {} isdebugmode ()
Return true if in debugging mode, otherwise false.
@seealso{dbwhere, dbstack, dbstatus}
@end deftypefn */)
{
  if (args.length () != 0)
    print_usage ();

  return ovl (Vdebugging);
}

DEFUN (__db_next_breakpoint_quiet__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} __db_next_breakpoint_quiet__ ()
@deftypefnx {} {} __db_next_breakpoint_quiet__ (@var{flag})
Disable line info printing at the next breakpoint.

With a logical argument @var{flag}, set the state on or off.
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  bool state = true;

  if (nargin == 1)
    state = args(0).bool_value ();

  octave::tree_evaluator::quiet_breakpoint_flag = state;

  return ovl ();
}
