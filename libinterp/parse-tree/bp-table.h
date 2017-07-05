/*

Copyright (C) 2001-2017 Ben Sapp

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

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

struct
bp_type
{
  int line;
  std::string cond;

  bp_type (int l, const std::string& c) : line (l), cond (c) { }
};

// Interface to breakpoints.
class
OCTINTERP_API
bp_table
{
private:

  bp_table (void) : bp_set () { }

  ~bp_table (void) = default;

public:

  // mapping from (FIXME: arbitrary index??) to line number of breakpoint
  typedef std::map<int, int> intmap;

  typedef intmap::const_iterator const_intmap_iterator;
  typedef intmap::iterator intmap_iterator;

  typedef std::map <std::string, intmap> fname_line_map;

  typedef fname_line_map::const_iterator const_fname_line_map_iterator;
  typedef fname_line_map::iterator fname_line_map_iterator;

  typedef std::map <std::string, std::list<bp_type>> fname_bp_map;
  typedef fname_bp_map::const_iterator const_fname_bp_map_iterator;
  typedef fname_bp_map::iterator fname_bp_map_iterator;

  static bool instance_ok (void);

  // Add a breakpoint at the nearest executable line.
  static intmap add_breakpoint (const std::string& fname = "",
                                const intmap& lines = intmap (),
                                const std::string& condition = "")
  {
    return instance_ok ()
           ? instance->do_add_breakpoint (fname, lines, condition) : intmap ();
  }

  // Remove a breakpoint from a line in file.
  static int remove_breakpoint (const std::string& fname = "",
                                const intmap& lines = intmap ())
  {
    return instance_ok ()
           ? instance->do_remove_breakpoint (fname, lines) : 0;
  }

  // Remove all the breakpoints in a specified file.
  static intmap remove_all_breakpoints_in_file (const std::string& fname,
                                                bool silent = false)
  {
    return instance_ok ()
           ? instance->do_remove_all_breakpoints_in_file (fname, silent)
           : intmap ();
  }

  // Remove all the breakpoints registered with octave.
  static void remove_all_breakpoints (void)
  {
    if (instance_ok ())
      instance->do_remove_all_breakpoints ();
  }

  // Return all breakpoints.  Each element of the map is a vector
  // containing the breakpoints corresponding to a given function name.
  static fname_bp_map
  get_breakpoint_list (const octave_value_list& fname_list)
  {
    return instance_ok ()
           ? instance->do_get_breakpoint_list (fname_list) : fname_bp_map ();
  }

  static bool
  have_breakpoints (void)
  {
    return instance_ok () ? instance->do_have_breakpoints () : 0;
  }

  // Should we enter debugging for this particular error identifier?
  static bool
  debug_on_err (const std::string& ID)
  {
    return (errors_that_stop.empty () || errors_that_stop.count (ID));
  }

  // Should we enter debugging for this particular identifier in a try/catch?
  static bool
  debug_on_caught (const std::string& ID)
  {
    return (caught_that_stop.empty () || caught_that_stop.count (ID));
  }

  // Should we enter debugging for this particular warning identifier?
  static bool
  debug_on_warn (const std::string& ID)
  {
    return (warnings_that_stop.empty () || warnings_that_stop.count (ID));
  }

  static octave_map stop_on_err_warn_status (bool toScreen);

  static void dbstop_process_map_args (const octave_map& mv);

  static void dbclear_all_signals (void);

  static bool condition_valid (const std::string& cond);

  static void parse_dbfunction_params (const char *, const octave_value_list&,
                                       std::string&, bp_table::intmap&,
                                       std::string&);

private:

  typedef std::set<std::string>::const_iterator const_bp_set_iterator;
  typedef std::set<std::string>::iterator bp_set_iterator;

  // Set of function (.m file) names containing at least one breakpoint.
  std::set<std::string> bp_set;

  // Set of error and warning message IDs that cause us to stop
  // *if* Vdebug_on_error / Vdebug_on_caught / Vdebug_on_warning is set.
  // Empty means stop on any error / caught error / warning.
  static std::set<std::string> errors_that_stop;
  static std::set<std::string> caught_that_stop;
  static std::set<std::string> warnings_that_stop;

  static bp_table *instance;

  static void cleanup_instance (void) { delete instance; instance = 0; }

  bool do_add_breakpoint_1 (octave_user_code *fcn, const std::string& fname,
                            const intmap& line, const std::string& condition,
                            intmap& retval);

  intmap do_add_breakpoint (const std::string& fname, const intmap& lines,
                            const std::string& condition);

  int do_remove_breakpoint_1 (octave_user_code *fcn, const std::string&,
                              const intmap& lines);

  int do_remove_breakpoint (const std::string&, const intmap& lines);

  intmap do_remove_all_breakpoints_in_file_1 (octave_user_code *fcn,
                                              const std::string& fname);

  intmap do_remove_all_breakpoints_in_file (const std::string& fname,
                                            bool silent);

  void do_remove_all_breakpoints (void);

  fname_bp_map do_get_breakpoint_list (const octave_value_list& fname_list);

  bool do_have_breakpoints (void) { return (! bp_set.empty ()); }
};

extern octave_user_code * get_user_code (const std::string& fname = "");

#endif
