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

#include <algorithm>
#include <limits>
#include <list>
#include <map>
#include <set>
#include <string>

#include "file-ops.h"

#include "bp-table.h"
#include "defun-int.h"
#include "call-stack.h"
#include "error.h"
#include "interpreter.h"
#include "interpreter-private.h"
#include "oct-map.h"
#include "octave-link.h"
#include "ov-usr-fcn.h"
#include "ov.h"
#include "ovl.h"
#include "pager.h"
#include "parse.h"
#include "pt-eval.h"
#include "pt-exp.h"
#include "pt-stmt.h"
#include "sighandlers.h"

namespace octave
{
  // Clear all reasons to stop, other than breakpoints.

  void bp_table::dbclear_all_signals (void)
  {
    Vdebug_on_error = false;
    bp_table::m_errors_that_stop.clear ();

    Vdebug_on_caught = false;
    bp_table::m_caught_that_stop.clear ();

    Vdebug_on_warning = false;
    bp_table::m_warnings_that_stop.clear ();

    Vdebug_on_interrupt = false;
  }

  // Process the "warn", "errs", "caught" and "intr" fields for a call of
  // "dbstop (p)".

  void bp_table::dbstop_process_map_args (const octave_map& mv)
  {
    // process errs
    // why so many levels of indirection needed?
    bool fail = false;
    Cell U = mv.contents ("errs");
    if (U.numel () != 1)
      fail = (U.numel () > 1);
    else
      {
        Array<octave_value> W = U.index (static_cast<octave_idx_type> (0));
        if (W.isempty () || W(0).isempty ())
          Vdebug_on_error = 1;    // like "dbstop if error" with no identifier
        else if (! W(0).iscell ())
          fail = true;
        else
          {
            Cell V = W(0).cell_value ();
            for (int i = 0; i < V.numel (); i++)
              {
                m_errors_that_stop.insert (V(i).string_value ());
                Vdebug_on_error = 1;
              }
          }
      }

    if (fail)
      error ("dbstop: invalid 'errs' field");

    // process caught
    // why so many levels of indirection needed?
    fail = false;
    U = mv.contents ("caught");
    if (U.numel () != 1)
      fail = (U.numel () > 1);
    else
      {
        Array<octave_value> W = U.index (static_cast<octave_idx_type> (0));
        if (W.isempty () || W(0).isempty ())
          Vdebug_on_caught = 1;    // like "dbstop if caught error" with no ID
        else if (! W(0).iscell ())
          fail = true;
        else
          {
            Cell V = W(0).cell_value ();
            for (int i = 0; i < V.numel (); i++)
              {
                m_caught_that_stop.insert (V(i).string_value ());
                Vdebug_on_caught = 1;
              }
          }
      }

    if (fail)
      error ("dbstop: invalid 'caught' field");

    // process warn
    // why so many levels of indirection needed?
    fail = false;
    U = mv.contents ("warn");
    if (U.numel () != 1)
      fail = (U.numel () > 1);
    else
      {
        Array<octave_value> W = U.index (static_cast<octave_idx_type> (0));
        if (W.isempty () || W(0).isempty ())
          Vdebug_on_warning = 1;    // like "dbstop if warning" with no identifier
        else if (! W(0).iscell ())
          fail = true;
        else
          {
            Cell V = W(0).cell_value ();
            for (int i = 0; i < V.numel (); i++)
              {
                m_warnings_that_stop.insert (V(i).string_value ());
                Vdebug_on_warning = 1;
              }
          }
      }

    if (fail)
      error ("dbstop: invalid 'warn' field");

    // process interrupt
    if (mv.isfield ("intr"))
      Vdebug_on_interrupt = 1;
  }

  // Insert a breakpoint in function fcn at line within file fname,
  // to stop only when condition is true.
  // Record in m_bp_set that fname contains a breakpoint.

  bool bp_table::add_breakpoint_1 (octave_user_code *fcn,
                                   const std::string& fname,
                                   const bp_table::intmap& line,
                                   const std::string& condition,
                                   bp_table::intmap& retval)
  {
    bool found = false;

    tree_statement_list *cmds = fcn->body ();

    std::string file = fcn->fcn_file_name ();

    if (cmds)
      {
        retval = cmds->add_breakpoint (file, line, condition);

        for (auto& idx_line_p : retval)
          {
            if (idx_line_p.second != 0)
              {
                // Normalize to store only the file name.
                // Otherwise, there can be an entry for both
                // file>subfunction and file, which causes a crash on
                // dbclear all
                const char *s = strchr (fname.c_str (), '>');
                if (s)
                  m_bp_set.insert (fname.substr (0, s - fname.c_str ()));
                else
                  m_bp_set.insert (fname);
                found = true;
                break;
              }
          }
      }

    return found;
  }

  // Cursory check that cond is a valid condition to use for a breakpoint.
  // Currently allows conditions with side-effects, like 'y+=10' and 'y++';
  // it is odd that the former is not flagged by "is_assignment_expression".
  // Throws an exception if not valid.

  bool bp_table::condition_valid (const std::string& cond)
  {
    if (cond.length () > 0)
      {
        // ; to reject partial expr like "y=="
        parser parser (cond + " ;", m_evaluator.get_interpreter ());
        parser.reset ();
        int parse_status = parser.run ();
        if (parse_status)
          error ("dbstop: Cannot parse condition '%s'", cond.c_str ());
        else
          {
            tree_statement *stmt = nullptr;
            if (! parser.m_stmt_list)
              error ("dbstop: "
                     "condition is not empty, but has nothing to evaluate");
            else
              {
                if (parser.m_stmt_list->length () == 1
                    && (stmt = parser.m_stmt_list->front ())
                    && stmt->is_expression ())
                  {
                    tree_expression *expr = stmt->expression ();
                    if (expr->is_assignment_expression ())
                      error ("dbstop: condition cannot be an assignment.  "
                             "Did you mean '=='?");
                  }
                else
                  error ("dbstop: condition must be an expression");
              }
          }
      }

    return true;
  }

  enum dbstop_args
  {
    dbstop_in,
    dbstop_at,
    dbstop_if,
    dbstop_none
  };

  // Parse parameters (args) of dbstop and dbclear commands.
  // For dbstop, who=="dbstop"; for dbclear, who=="dbclear".
  // The syntax is: dbstop [[in] symbol] [[at] line [line [...]]] [if condition]
  // where the form of condition depends on whether or not a file or line has
  // been seen.
  // Also execute "if [error|warning|interrupt|naninf]" clauses.

  void bp_table::parse_dbfunction_params (const char *who,
                                          const octave_value_list& args,
                                          std::string& symbol_name,
                                          bp_table::intmap& lines,
                                          std::string& cond)
  {
    int nargin = args.length ();
    int list_idx = 0;
    symbol_name = "";
    lines = bp_table::intmap ();

    if (nargin == 0 || ! args(0).is_string ())
      print_usage (who);

    // elements already processed
    bool seen_in = false, seen_at = false, seen_if = false;
    int pos = 0;
    dbstop_args tok = dbstop_none;
    while (pos < nargin)
      {
        // allow "in" and "at" to be implicit
        if (args(pos).is_string ())
          {
            std::string arg = args(pos).string_value ();
            if (arg == "in")
              {
                tok = dbstop_in;
                pos++;
              }
            else if (arg == "at")
              {
                tok = dbstop_at;
                pos++;
              }
            else if (arg == "if")
              {
                tok = dbstop_if;
                pos++;
              }
            else if (atoi (args(pos).string_value ().c_str ()) > 0)
              tok = dbstop_at;
            else
              tok = dbstop_in;
          }
        else
          tok = dbstop_at;

        if (pos >= nargin)
          error ("%s: '%s' missing argument", who,
                 (tok == dbstop_in
                  ? "in" : (tok == dbstop_at ? "at" : "if")));

        // process the actual arguments
        switch (tok)
          {
          case dbstop_in:
            symbol_name = args(pos).string_value ();
            if (seen_in)
              error ("%s: Too many function names specified -- %s",
                     who, symbol_name.c_str ());
            else if (seen_at || seen_if)
              error ("%s: function name must come before line number and 'if'",
                     who);
            seen_in = true;
            pos++;
            break;

          case dbstop_at:
            if (seen_at)
              error ("%s: Only one 'at' clause is allowed -- %s",
                     who, args(pos).string_value ().c_str ());
            else if (seen_if)
              error ("%s: line number must come before 'if' clause\n", who);
            seen_at = true;

            if (! seen_in)
              {
                // It was a line number.  Get function name from debugger.
                if (Vdebugging)
                  symbol_name = m_evaluator.get_user_code ()->profiler_name ();
                else
                  error ("%s: function name must come before line number "
                         "and 'if'", who);
                seen_in = true;
              }
            else if (seen_if)
              error ("%s: line number must come before 'if' clause\n", who);

            // Read a list of line numbers (or arrays thereof)
            for ( ; pos < nargin; pos++)
              {
                if (args(pos).is_string ())
                  {
                    int line = atoi (args(pos).string_value ().c_str ());

                    if (line > 0)
                      lines[list_idx++] = line;
                    else
                      break;        // may be "if"
                  }
                else if (args(pos).isnumeric ())
                  {
                    const NDArray arg = args(pos).array_value ();

                    for (octave_idx_type j = 0; j < arg.numel (); j++)
                      lines[list_idx++] = static_cast<int> (arg.elem (j));
                  }
                else
                  error ("%s: Invalid argument type %s",
                         who, args(pos).type_name ().c_str ());
              }
            break;

          case dbstop_if:
            if (seen_in)    // conditional breakpoint
              {
                cond = "";  // remaining arguments form condition
                for (; pos < nargin; pos++)
                  {
                    if (args(pos).is_string ())
                      cond += ' ' + args(pos).string_value ();
                    else
                      error ("%s: arguments to 'if' must all be strings", who);
                  }

                cond = cond.substr (1);   // omit initial space
              }
            else    // stop on event (error, warning, interrupt, NaN/inf)
              {
                std::string condition = args(pos).string_value ();
                int on_off = ! strcmp(who, "dbstop");

                // list of error/warning IDs to update
                std::set<std::string> *id_list = nullptr;
                bool *stop_flag = nullptr;         // Vdebug_on_... flag

                if (condition == "error")
                  {
                    id_list = &m_errors_that_stop;
                    stop_flag = &Vdebug_on_error;
                  }
                else if (condition == "warning")
                  {
                    id_list = &m_warnings_that_stop;
                    stop_flag = &Vdebug_on_warning;
                  }
                else if (condition == "caught" && nargin > pos+1
                         && args(pos+1).string_value () == "error")
                  {
                    id_list = &m_caught_that_stop;
                    stop_flag = &Vdebug_on_caught;
                    pos++;
                  }
                else if (condition == "interrupt")
                  {
                    Vdebug_on_interrupt = on_off;
                  }
                else if (condition == "naninf")
                  {
#if defined (DBSTOP_NANINF)
                    Vdebug_on_naninf = on_off;
                    enable_fpe (on_off);
#else
                    warning ("%s: condition '%s' not yet supported",
                             who, condition.c_str ());
#endif
                  }
                else
                  error ("%s: invalid condition %s",
                         who, condition.c_str ());

                // process ID list for "dbstop if error <error_ID>" etc
                if (id_list)
                  {
                    pos++;
                    if (pos < nargin)       // only affect a single error ID
                      {
                        if (! args(pos).is_string () || nargin > pos+1)
                          error ("%s: ID must be a single string", who);
                        else if (on_off == 1)
                          {
                            id_list->insert (args(pos).string_value ());
                            *stop_flag = true;
                          }
                        else
                          {
                            id_list->erase (args(pos).string_value ());
                            if (id_list->empty ())
                              *stop_flag = false;
                          }
                      }
                    else   // unqualified.  Turn all on or off
                      {
                        id_list->clear ();
                        *stop_flag = on_off;
                        if (stop_flag == &Vdebug_on_error)
                          {
                            // Matlab stops on both.
                            Vdebug_on_interrupt = on_off;
                          }
                      }
                  }

                pos = nargin;
              }
            break;

          default:      // dbstop_none should never occur
            break;
          }
      }
  }

  /*
    %!test
    %! dbclear all;   # Clear out breakpoints before test
    %! dbstop help;
    %! dbstop in ls;
    %! dbstop help at 100;
    %! dbstop in ls 100;
    %! dbstop help 201 if a==5;
    %! dbstop if error Octave:undefined-function;
    %! s = dbstatus;
    %! dbclear all;
    %! assert ({s.bkpt(:).name}, {"help", "help", "help>do_contents", "ls", "ls"});
    %! assert ([s.bkpt(:).line], [48, 100, 201, 58, 100]);
    %! assert (s.errs, {"Octave:undefined-function"});
  */

  // Return the sub/nested/main function of MAIN_FCN that contains
  // line number LINENO of the source file.
  // If END_LINE != 0, *END_LINE is set to last line of the returned function.

  static octave_user_code * find_fcn_by_line (octave_user_code *main_fcn,
                                              int lineno, int *end_line = nullptr)
  {
    octave_user_code *retval = nullptr;
    octave_user_code *next_fcn = nullptr;  // 1st function starting after lineno

    // Find innermost nested (or parent) function containing lineno.
    int earliest_end = std::numeric_limits<int>::max ();

    std::map<std::string, octave_value> subfcns = main_fcn->subfunctions ();
    for (const auto& str_val_p : subfcns)
      {
        if (str_val_p.second.is_user_function ())
          {
            auto *dbg_subfcn = str_val_p.second.user_function_value ();

            // Check if lineno is within dbg_subfcn.
            // FIXME: we could break when beginning_line() > lineno,
            // but that makes the code "fragile"
            // if the order of walking subfcns changes,
            // for a minor speed improvement in non-critical code.
            if (dbg_subfcn->ending_line () < earliest_end
                && dbg_subfcn->ending_line () >= lineno
                && dbg_subfcn->beginning_line () <= lineno)
              {
                earliest_end = dbg_subfcn->ending_line ();
                retval = find_fcn_by_line (dbg_subfcn, lineno, &earliest_end);
              }

            // Find the first fcn starting after lineno.
            // This is used if line is not inside any function.
            if (dbg_subfcn->beginning_line () >= lineno && ! next_fcn)
              next_fcn = dbg_subfcn;
          }
      }

    // The breakpoint is either in the subfunction found above,
    // or in the main function, which we check now.
    if (main_fcn->is_user_function ())
      {
        int e = dynamic_cast<octave_user_function *> (main_fcn)->ending_line ();
        if (e >= lineno && e < earliest_end)
          retval = main_fcn;

        if (! retval)
          retval = next_fcn;
      }
    else  // main_fcn is a script.
      {
        if (! retval)
          retval = main_fcn;
      }

    if (end_line && earliest_end < *end_line)
      *end_line = earliest_end;

    return retval;
  }

  // Given file name fname, find the subfunction at line and create
  // a breakpoint there.  Put the system into debug_mode.
  bp_table::intmap bp_table::add_breakpoint (const std::string& fname,
                                             const bp_table::intmap& line,
                                             const std::string& condition)
  {
    octave_user_code *main_fcn = m_evaluator.get_user_code (fname);

    if (! main_fcn)
      error ("add_breakpoint: unable to find function '%s'\n", fname.c_str ());

    condition_valid (condition);  // Throw error if condition not valid.

    intmap retval;

    octave_idx_type len = line.size ();

    for (int i = 0; i < len; i++)
      {
        const_intmap_iterator m = line.find (i);

        if (m != line.end ())
          {
            int lineno = m->second;

            octave_user_code *dbg_fcn = find_fcn_by_line (main_fcn, lineno);

            // We've found the right (sub)function.  Now insert the breakpoint.
            // We insert all breakpoints.
            // If multiple are in the same function, we insert multiple times.
            intmap ret_one;
            if (dbg_fcn
                && add_breakpoint_1 (dbg_fcn, fname, line, condition, ret_one))
              retval.insert (std::pair<int,int> (i, ret_one.find (i)->second));
          }
      }

    m_evaluator.debug_mode (bp_table::have_breakpoints () || Vdebugging);

    return retval;
  }

  int bp_table::remove_breakpoint_1 (octave_user_code *fcn,
                                     const std::string& fname,
                                     const bp_table::intmap& line)
  {
    int retval = 0;

    std::string file = fcn->fcn_file_name ();

    tree_statement_list *cmds = fcn->body ();

    // FIXME: move the operation on cmds to the tree_statement_list class?

    if (cmds)
      {
        octave_value_list results = cmds->list_breakpoints ();

        if (results.length () > 0)
          {
            octave_idx_type len = line.size ();

            for (int i = 0; i < len; i++)
              {
                const_intmap_iterator p = line.find (i);

                if (p != line.end ())
                  {
                    int lineno = p->second;

                    cmds->delete_breakpoint (lineno);

                    if (! file.empty ())
                      octave_link::update_breakpoint (false, file, lineno);
                  }
              }

            results = cmds->list_breakpoints ();

            auto it = m_bp_set.find (fname);
            if (results.empty () && it != m_bp_set.end ())
              m_bp_set.erase (it);
          }

        retval = results.length ();
      }

    return retval;
  }

  int bp_table::remove_breakpoint (const std::string& fname,
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
        octave_user_code *dbg_fcn = m_evaluator.get_user_code (fname);

        if (! dbg_fcn)
          error ("remove_breakpoint: unable to find function %s\n",
                 fname.c_str ());

        retval = remove_breakpoint_1 (dbg_fcn, fname, line);

        // Search subfunctions in the order they appear in the file.

        const std::list<std::string> subfcn_names
          = dbg_fcn->subfunction_names ();

        std::map<std::string, octave_value> subfcns
          = dbg_fcn->subfunctions ();

        for (const auto& subf_nm : subfcn_names)
          {
            const auto q = subfcns.find (subf_nm);

            if (q != subfcns.end ())
              {
                octave_user_code *dbg_subfcn = q->second.user_code_value ();

                retval += remove_breakpoint_1 (dbg_subfcn, fname, line);
              }
          }
      }

    m_evaluator.debug_mode (bp_table::have_breakpoints () || Vdebugging);

    return retval;
  }

  // Remove all breakpoints from a file, including those in subfunctions.

  bp_table::intmap
  bp_table::remove_all_breakpoints_in_file (const std::string& fname,
                                            bool silent)
  {
    intmap retval;

    octave_user_code *dbg_fcn = m_evaluator.get_user_code (fname);

    if (dbg_fcn)
      {
        std::string file = dbg_fcn->fcn_file_name ();

        tree_statement_list *cmds = dbg_fcn->body ();

        if (cmds)
          {
            retval = cmds->remove_all_breakpoints (file);

            auto it = m_bp_set.find (fname);
            if (it != m_bp_set.end ())
              m_bp_set.erase (it);
          }
      }
    else if (! silent)
      error ("remove_all_breakpoint_in_file: "
             "unable to find function %s\n", fname.c_str ());

    m_evaluator.debug_mode (bp_table::have_breakpoints () || Vdebugging);

    return retval;
  }

  void bp_table::remove_all_breakpoints (void)
  {
    // Odd loop structure required because delete will invalidate
    // m_bp_set iterators.
    for (auto it = m_bp_set.cbegin (), it_next = it;
         it != m_bp_set.cend ();
         it = it_next)
      {
        ++it_next;
        remove_all_breakpoints_in_file (*it);
      }

    m_evaluator.debug_mode (bp_table::have_breakpoints () || Vdebugging);
  }

  std::string find_bkpt_list (octave_value_list slist, std::string match)
  {
    std::string retval;

    for (int i = 0; i < slist.length (); i++)
      {
        if (slist(i).string_value () == match)
          {
            retval = slist(i).string_value ();
            break;
          }
      }

    return retval;
  }

  bp_table::fname_bp_map
  bp_table::get_breakpoint_list (const octave_value_list& fname_list)
  {
    fname_bp_map retval;

    // make copy since changes may invalidate iters of m_bp_set.
    std::set<std::string> tmp_bp_set = m_bp_set;

    for (auto& bp_fname : tmp_bp_set)
      {
        if (fname_list.empty ()
            || find_bkpt_list (fname_list, bp_fname) != "")
          {
            octave_user_code *dbg_fcn = m_evaluator.get_user_code (bp_fname);

            if (dbg_fcn)
              {
                tree_statement_list *cmds = dbg_fcn->body ();

                // FIXME: move the operation on cmds to the
                //        tree_statement_list class?
                if (cmds)
                  {
                    std::list<bp_type> bkpts = cmds->breakpoints_and_conds ();

                    if (! bkpts.empty ())
                      retval[bp_fname] = bkpts;
                  }

                // look for breakpoints in subfunctions
                const std::list<std::string> subf_nm
                  = dbg_fcn->subfunction_names ();

                std::map<std::string, octave_value> subfcns
                  = dbg_fcn->subfunctions ();

                for (const auto& subfcn_nm : subf_nm)
                  {
                    const auto q = subfcns.find (subfcn_nm);

                    if (q != subfcns.end ())
                      {
                        octave_user_code *dbg_subfcn
                          = q->second.user_code_value ();

                        cmds = dbg_subfcn->body ();
                        if (cmds)
                          {
                            std::list<bp_type> bkpts
                              = cmds->breakpoints_and_conds ();

                            if (! bkpts.empty ())
                              {
                                std::string key
                                  = bp_fname + '>' + dbg_subfcn->name ();

                                retval[key] = bkpts;
                              }
                          }
                      }
                  }
              }
          }
      }

    return retval;
  }

  // Report the status of "dbstop if error ..." and "dbstop if warning ..."
  // If to_screen is true, the output goes to octave_stdout; otherwise it is
  // returned.
  // If dbstop if error is true but no explicit IDs are specified, the return
  // value will have an empty field called "errs".  If IDs are specified, the
  // "errs" field will have a row per ID.  If dbstop if error is false, there
  // is no "errs" field.  The "warn" field is set similarly by dbstop if warning

  octave_map bp_table::stop_on_err_warn_status (bool to_screen)
  {
    octave_map retval;

    // print dbstop if error information
    if (Vdebug_on_error)
      {
        if (m_errors_that_stop.empty ())
          {
            if (to_screen)
              octave_stdout << "stop if error\n";
            else
              retval.assign ("errs", octave_value(""));
          }
        else
          {
            Cell errs (dim_vector (bp_table::m_errors_that_stop.size (), 1));
            int i = 0;

            for (const auto& e : m_errors_that_stop)
              {
                if (to_screen)
                  octave_stdout << "stop if error " << e << "\n";
                else
                  errs(i++) = e;
              }
            if (! to_screen)
              retval.assign ("errs", octave_value (errs));
          }
      }

    // print dbstop if caught error information
    if (Vdebug_on_caught)
      {
        if (m_caught_that_stop.empty ())
          {
            if (to_screen)
              octave_stdout << "stop if caught error\n";
            else
              retval.assign ("caught", octave_value(""));
          }
        else
          {
            Cell errs (dim_vector (m_caught_that_stop.size (), 1));
            int i = 0;

            for (const auto& e : m_caught_that_stop)
              {
                if (to_screen)
                  octave_stdout << "stop if caught error " << e << "\n";
                else
                  errs(i++) = e;
              }
            if (! to_screen)
              retval.assign ("caught", octave_value (errs));
          }
      }

    // print dbstop if warning information
    if (Vdebug_on_warning)
      {
        if (m_warnings_that_stop.empty ())
          {
            if (to_screen)
              octave_stdout << "stop if warning\n";
            else
              retval.assign ("warn", octave_value(""));
          }
        else
          {
            Cell warn (dim_vector (m_warnings_that_stop.size (), 1));
            int i = 0;

            for (const auto& w : m_warnings_that_stop)
              {
                if (to_screen)
                  octave_stdout << "stop if warning " << w << "\n";
                else
                  warn(i++) = w;
              }
            if (! to_screen)
              retval.assign ("warn", octave_value (warn));
          }
      }

    // print dbstop if interrupt information
    if (Vdebug_on_interrupt)
      {
        if (to_screen)
          octave_stdout << "stop if interrupt\n";
        else
          retval.assign ("intr", octave_value ());
      }

    return retval;
  }

  octave_user_code *
  get_user_code (const std::string& fname)
  {
    tree_evaluator& tw = __get_evaluator__ ("get_user_code");

    return tw.get_user_code (fname);
  }
}
