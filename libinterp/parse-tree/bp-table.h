////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2001-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if ! defined (octave_bp_table_h)
#define octave_bp_table_h 1

#include "octave-config.h"

#include <list>
#include <map>
#include <set>
#include <string>

class octave_map;
class octave_user_code;
class octave_value_list;

OCTAVE_BEGIN_NAMESPACE(octave)

class tree_evaluator;

struct bp_type
{
public:
  bp_type (int l, const std::string& c) : line (l), cond (c) { }

  //--------

  int line;
  std::string cond;
};

// Interface to breakpoints.
class OCTINTERP_API bp_table
{
public:

  bp_table (tree_evaluator& tw)
    : m_evaluator (tw), m_bp_set (), m_errors_that_stop (),
      m_caught_that_stop (), m_warnings_that_stop ()
  { }

  ~bp_table (void) = default;

  // Set of breakpoint lines.
  typedef std::set<int> bp_lines;

  typedef bp_lines::const_iterator const_bp_lines_iterator;
  typedef bp_lines::iterator bp_lines_iterator;

  typedef std::map <std::string, bp_lines> fname_line_map;

  typedef fname_line_map::const_iterator const_fname_line_map_iterator;
  typedef fname_line_map::iterator fname_line_map_iterator;

  typedef std::map <std::string, std::list<bp_type>> fname_bp_map;
  typedef fname_bp_map::const_iterator const_fname_bp_map_iterator;
  typedef fname_bp_map::iterator fname_bp_map_iterator;

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)
  OCTAVE_DEPRECATED (7, "use 'bp_table::add_breakpoints_in_function' instead")
  int add_breakpoint (const std::string& fname = "",
                      const std::string& class_name = "",
                      int line = 1,
                      const std::string& condition = "")
  {
    return add_breakpoint_in_function (fname, class_name, line, condition);
  }

  OCTAVE_DEPRECATED (7, "use 'bp_table::add_breakpoints_in_function' instead")
  bp_lines add_breakpoint (const std::string& fname = "",
                           const std::string& class_name = "",
                           const bp_lines& lines = bp_lines (),
                           const std::string& condition = "")
  {
    return add_breakpoints_in_function (fname, class_name, lines, condition);
  }
#endif

  // Add a breakpoint at the nearest executable line in a function.
  int add_breakpoint_in_function (const std::string& fname = "",
                                  const std::string& class_name = "",
                                  int line = 1,
                                  const std::string& condition = "");

  // Add a set of breakpoints at the nearest executable lines in a
  // function.
  bp_lines add_breakpoints_in_function (const std::string& fname = "",
                                        const std::string& class_name = "",
                                        const bp_lines& lines = bp_lines (),
                                        const std::string& condition = "");

  // Add a breakpoint at the nearest executable line in a file.
  int add_breakpoint_in_file (const std::string& file = "",
                              int line = 1,
                              const std::string& condition = "");

  // Add a set of breakpoints at the nearest executable lines in a
  // file.
  bp_lines add_breakpoints_in_file (const std::string& file = "",
                                    const bp_lines& lines = bp_lines (),
                                    const std::string& condition = "");

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)
  OCTAVE_DEPRECATED (7, "use 'bp_table::remove_breakpoint_from_function' instead")
  int remove_breakpoint (const std::string& fname = "",
                         int line = 1)
  {
    return remove_breakpoint_from_function (fname, line);
  }

  OCTAVE_DEPRECATED (7, "use 'bp_table::remove_breakpoints_from_function' instead")
  int remove_breakpoint (const std::string& fname = "",
                         const bp_lines& lines = bp_lines ())
  {
    return remove_breakpoints_from_function (fname, lines);
  }
#endif

  // Remove a breakpoint from the given line in file.
  int remove_breakpoint_from_function (const std::string& fname = "",
                                       int line = 1);

  // Remove a set of breakpoints from the given lines in file.
  int remove_breakpoints_from_function (const std::string& fname = "",
                                        const bp_lines& lines = bp_lines ());

  // Remove all the breakpoints in a specified function.
  bp_lines remove_all_breakpoints_from_function (const std::string& fname,
      bool silent = false);

  // Remove a breakpoint from the given line in file.
  int remove_breakpoint_from_file (const std::string& file = "",
                                   int line = 1);

  // Remove a set of breakpoints from the given lines in file.
  int remove_breakpoints_from_file (const std::string& file = "",
                                    const bp_lines& lines = bp_lines ());


#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)
  OCTAVE_DEPRECATED (7, "use 'bp_table::remove_all_breakpoints_from_function' instead")
  bp_lines remove_all_breakpoints_in_file (const std::string& fname,
      bool silent = false)
  {
    return remove_all_breakpoints_from_function (fname, silent);
  }
#endif

  // Remove all the breakpoints from a file.
  bp_lines remove_all_breakpoints_from_file (const std::string& file,
      bool silent = false);

  // Remove all the breakpoints registered with octave.
  void remove_all_breakpoints (void);

  // Return all breakpoints.  Each element of the map is a vector
  // containing the breakpoints corresponding to a given function name.
  fname_bp_map get_breakpoint_list (const octave_value_list& fname_list);

  bool have_breakpoints (void) { return (! m_bp_set.empty ()); }

  // Should we enter debugging for this particular error identifier?
  bool debug_on_err (const std::string& id)
  {
    return (m_errors_that_stop.empty () || m_errors_that_stop.count (id));
  }

  // Should we enter debugging for this particular identifier in a try/catch?
  bool debug_on_caught (const std::string& id)
  {
    return (m_caught_that_stop.empty () || m_caught_that_stop.count (id));
  }

  // Should we enter debugging for this particular warning identifier?
  bool debug_on_warn (const std::string& id)
  {
    return (m_warnings_that_stop.empty () || m_warnings_that_stop.count (id));
  }

  octave_map stop_on_err_warn_status (bool to_screen);

  void dbstop_process_map_args (const octave_map& mv);

  void dbclear_all_signals (void);

  bool condition_valid (const std::string& cond);

  void parse_dbfunction_params (const char *who,
                                const octave_value_list& args,
                                std::string& fcn_name,
                                std::string& class_name,
                                bp_table::bp_lines& lines,
                                std::string& cond);

private:

  typedef std::set<std::string>::const_iterator const_bp_set_iterator;
  typedef std::set<std::string>::iterator bp_set_iterator;

  tree_evaluator& m_evaluator;

  // Set of function (.m file) names containing at least one breakpoint.
  std::set<std::string> m_bp_set;

  // Set of error and warning message IDs that cause us to stop
  // *if* Vdebug_on_error / Vdebug_on_caught / Vdebug_on_warning is set.
  // Empty means stop on any error / caught error / warning.
  std::set<std::string> m_errors_that_stop;
  std::set<std::string> m_caught_that_stop;
  std::set<std::string> m_warnings_that_stop;

  void set_stop_flag (const char *who, const std::string& condition,
                      bool on_off);

  void process_id_list (const char *who, const std::string& condition,
                        const octave_value_list& args,
                        int nargin, int& pos, bool on_off,
                        std::set<std::string>& id_list);

  bool add_breakpoint_1 (octave_user_code *fcn, const std::string& fname,
                         const bp_lines& line, const std::string& condition,
                         bp_lines& retval);

  int remove_breakpoint_1 (octave_user_code *fcn, const std::string&,
                           const bp_lines& lines);

  bp_lines remove_all_breakpoints_in_file_1 (octave_user_code *fcn,
      const std::string& fname);
};

OCTAVE_END_NAMESPACE(octave)

#endif
