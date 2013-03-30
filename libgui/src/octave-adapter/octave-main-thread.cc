/*

Copyright (C) 2011-2012 Jacob Dawid

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

#include <clocale>
#include <string>

#include "builtin-defun-decls.h"
#include "octave.h"
#include "ov-builtin.h"
#include "ov-fcn-handle.h"

#include "octave-main-thread.h"
#include "octave-link.h"

static octave_value_list
pre_input_event_hook_fcn (const octave_value_list&, int)
{
  octave_value_list retval;

  octave_link::pre_input_event_hook_fcn ();

  return retval;
}

static octave_value_list
post_input_event_hook_fcn (const octave_value_list&, int)
{
  octave_value_list retval;

  octave_link::post_input_event_hook_fcn ();

  return retval;
}

static octave_value_list
enter_debugger_event_hook_fcn (const octave_value_list& args, int)
{
  octave_value_list retval;

  octave_link::enter_debugger_event_hook_fcn (args);

  return retval;
}

static octave_value_list
exit_debugger_event_hook_fcn (const octave_value_list& args, int)
{
  octave_value_list retval;

  octave_link::exit_debugger_event_hook_fcn (args);

  return retval;
}

static octave_value_list
dbstop_hook_fcn (const octave_value_list& args, int)
{
  octave_value_list retval;

  octave_link::update_breakpoint_hook_fcn (true, args);

  return retval;
}

static octave_value_list
dbclear_hook_fcn (const octave_value_list& args, int)
{
  octave_value_list retval;

  octave_link::update_breakpoint_hook_fcn (false, args);

  return retval;
}

static octave_value_list
edit_hook_fcn (const octave_value_list& args, int)
{
  octave_value_list retval;

  octave_link::edit_file (args);

  return retval;
}

octave_main_thread::octave_main_thread () : QThread ()
{
}

void
octave_main_thread::run ()
{
  // Matlab uses "C" locale for LC_NUMERIC class regardless of local setting
  setlocale (LC_NUMERIC, "C");

  emit ready ();

  octave_initialize_interpreter (octave_cmdline_argc, octave_cmdline_argv,
                                 octave_embedded);

  octave_value pre_fcn (new octave_builtin (pre_input_event_hook_fcn));
  octave_value pre_fcn_handle (new octave_fcn_handle (pre_fcn));
  Fadd_pre_input_event_hook (pre_fcn_handle);

  octave_value post_fcn (new octave_builtin (post_input_event_hook_fcn));
  octave_value post_fcn_handle (new octave_fcn_handle (post_fcn));
  Fadd_post_input_event_hook (post_fcn_handle);

  octave_value enter_debugger_fcn (new octave_builtin (enter_debugger_event_hook_fcn));
  octave_value enter_debugger_fcn_handle (new octave_fcn_handle (enter_debugger_fcn));
  Fadd_enter_debugger_event_hook (enter_debugger_fcn_handle);

  octave_value exit_debugger_fcn (new octave_builtin (exit_debugger_event_hook_fcn));
  octave_value exit_debugger_fcn_handle (new octave_fcn_handle (exit_debugger_fcn));
  Fadd_exit_debugger_event_hook (exit_debugger_fcn_handle);

  octave_value dbstop_fcn (new octave_builtin (dbstop_hook_fcn));
  octave_value dbstop_fcn_handle (new octave_fcn_handle (dbstop_fcn));
  Fadd_dbstop_hook (dbstop_fcn_handle);

  octave_value dbclear_fcn (new octave_builtin (dbclear_hook_fcn));
  octave_value dbclear_fcn_handle (new octave_fcn_handle (dbclear_fcn));
  Fadd_dbclear_hook (dbclear_fcn_handle);

  octave_value edit_fcn (new octave_builtin (edit_hook_fcn));
  octave_value edit_fcn_handle (new octave_fcn_handle (edit_fcn));
  Fadd_edit_hook (edit_fcn_handle);

  // Prime the history list.
  octave_link::update_history ();

  octave_execute_interpreter ();
}
