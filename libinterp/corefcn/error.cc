////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1993-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstdarg>
#include <cstdlib>
#include <cstring>

#include <algorithm>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>

#include "quit.h"

#include "bp-table.h"
#include "builtin-defun-decls.h"
#include "defun.h"
#include "error.h"
#include "event-manager.h"
#include "input.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "oct-map.h"
#include "octave.h"
#include "ov-usr-fcn.h"
#include "ov.h"
#include "ovl.h"
#include "pager.h"
#include "pt-eval.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

static std::string
format_message (const char *fmt, va_list args)
{
  if (! fmt)
    return "";

  std::ostringstream output_buf;

  octave::vformat (output_buf, fmt, args);

  return output_buf.str ();
}

OCTAVE_NORETURN
static void
error_1 (octave::execution_exception& ee, const char *id, const char *fmt,
         va_list args)
{
  octave::error_system& es = octave::__get_error_system__ ();

  es.error_1 (ee, id, fmt, args);
}

OCTAVE_NORETURN
static void
error_1 (const char *id, const char *fmt, va_list args)
{
  octave::error_system& es = octave::__get_error_system__ ();

  es.error_1 (id, fmt, args);
}

static int
check_state (const std::string& state)
{
  // -1: not found
  //  0: found, "off"
  //  1: found, "on"
  //  2: found, "error"

  if (state == "off")
    return 0;
  else if (state == "on")
    return 1;
  else if (state == "error")
    return 2;
  else
    return -1;
}

static void
vwarning (const char *id, const char *fmt, va_list args)
{
  octave::error_system& es = octave::__get_error_system__ ();

  es.vwarning (id, fmt, args);
}

static void
defun_usage_message (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  error_1 ("", fmt, args);
  va_end (args);
}

typedef void (*error_fcn)(const char *, const char *, ...);

static std::string
handle_message (error_fcn f, const char *id, const char *msg,
                const octave_value_list& args, bool have_fmt)
{
  std::string retval;

  std::string tmpstr;

  if (args.length () > 0)
    {
      octave_value arg;

      if (have_fmt)
        {
          octave_value_list tmp = octave::Fsprintf (args, 1);
          arg = tmp(0);
        }
      else
        arg = args(0);

      if (arg.is_defined ())
        {
          if (arg.isempty ())
            return retval;
          else if (arg.is_string ())
            {
              tmpstr = arg.string_value ();  // 2-stage assignment required
              msg = tmpstr.c_str ();         // in order to generate pointer
                                             // to valid memory.
            }
        }
    }

  // Ugh.

  std::size_t len = strlen (msg);

  if (len > 0)
    {
      if (msg[len - 1] == '\n')
        {
          if (len > 1)
            {
              std::string tmp_msg (msg, len - 1);
              f (id, "%s\n", tmp_msg.c_str ());
              retval = tmp_msg;
            }
        }
      else
        {
          f (id, "%s", msg);
          retval = msg;
        }
    }

  return retval;
}

// Determine whether the first argument to error or warning function
// should be handled as the message identifier or as the format string.

static bool
maybe_extract_message_id (const std::string& caller,
                          const octave_value_list& args,
                          octave_value_list& nargs,
                          std::string& id)
{
  nargs = args;
  id = "";

  int nargin = args.length ();

  bool have_fmt = nargin > 1;

  if (nargin > 0)
    {
      std::string arg1 = args(0).string_value ();

      // For compatibility with Matlab, an identifier must contain ':',
      // but not at the beginning or the end, and it must not contain '%'
      // (even if it is not a valid conversion operator) or whitespace.

      if (arg1.find_first_of ("% \f\n\r\t\v") == std::string::npos
          && arg1.find (':') != std::string::npos
          && arg1[0] != ':'
          && arg1.back () != ':')
        {
          if (nargin > 1)
            {
              id = arg1;

              nargs.resize (nargin-1);

              for (int i = 1; i < nargin; i++)
                nargs(i-1) = args(i);
            }
          else
            nargs(0) = "call to " + caller
                       + " with message identifier '" + arg1
                       + "' requires message";
        }
    }

  return have_fmt;
}

OCTAVE_BEGIN_NAMESPACE(octave)

static octave_scalar_map
init_warning_options (const std::string& state)
{
  octave_scalar_map initw;

  initw.setfield ("identifier", "all");
  initw.setfield ("state", state);

  return initw;
}

static octave_map
init_error_stack (interpreter& interp)
{
  tree_evaluator& tw = interp.get_evaluator ();

  return tw.empty_backtrace ();
}

error_system::error_system (interpreter& interp)
  : m_interpreter (interp),
    m_debug_on_error (false),
    m_debug_on_caught (false),
    m_debug_on_warning (false),
    m_discard_warning_messages (false),
    m_beep_on_error (false),
    m_backtrace_on_warning (true),
    m_verbose_warning (false),
    m_quiet_warning (false),
    m_warning_options (init_warning_options ("on")),
    m_last_error_message (),
    m_last_warning_message (),
    m_last_warning_id (),
    m_last_error_id (),
    m_last_error_stack (init_error_stack (interp))
{
  initialize_default_warning_state ();
}

octave_value
error_system::debug_on_error (const octave_value_list& args, int nargout)
{
  return set_internal_variable (m_debug_on_error, args, nargout,
                                "debug_on_error");
}

octave_value
error_system::debug_on_caught (const octave_value_list& args, int nargout)
{
  return set_internal_variable (m_debug_on_caught, args, nargout,
                                "debug_on_caught");
}

octave_value
error_system::debug_on_warning (const octave_value_list& args, int nargout)
{
  return set_internal_variable (m_debug_on_warning, args, nargout,
                                "debug_on_warning");
}

octave_value
error_system::discard_warning_messages (const octave_value_list& args,
                                        int nargout)
{
  return set_internal_variable (m_discard_warning_messages, args, nargout,
                                "discard_warning_messages");
}

octave_value
error_system::beep_on_error (const octave_value_list& args, int nargout)
{
  return set_internal_variable (m_beep_on_error, args, nargout,
                                "beep_on_error");
}

octave_value
error_system::backtrace_on_warning (const octave_value_list& args,
                                    int nargout)
{
  return set_internal_variable (m_backtrace_on_warning, args, nargout,
                                "backtrace_on_warning");
}

octave_value
error_system::verbose_warning (const octave_value_list& args, int nargout)
{
  return set_internal_variable (m_verbose_warning, args, nargout,
                                "verbose_warning");
}

octave_value
error_system::quiet_warning (const octave_value_list& args, int nargout)
{
  return set_internal_variable (m_quiet_warning, args, nargout,
                                "quiet_warning");
}

octave_value
error_system::last_error_message (const octave_value_list& args, int nargout)
{
  return set_internal_variable (m_last_error_message, args, nargout,
                                "last_error_message");
}

octave_value
error_system::last_warning_message (const octave_value_list& args,
                                    int nargout)
{
  return set_internal_variable (m_last_warning_message, args, nargout,
                                "last_warning_message");
}

octave_value
error_system::last_warning_id (const octave_value_list& args, int nargout)
{
  return set_internal_variable (m_last_warning_id, args, nargout,
                                "last_warning_id");
}

octave_value
error_system::last_error_id (const octave_value_list& args, int nargout)
{
  return set_internal_variable (m_last_error_id, args, nargout,
                                "last_error_id");
}

// Use static fields for the best efficiency.
// NOTE: C++0x will allow these two to be merged into one.
static const char *bt_fieldnames[] =
{ "file", "name", "line", "column", nullptr };

static const octave_fields bt_fields (bt_fieldnames);

octave_map
error_system::make_stack_map (const std::list<frame_info>& frames)
{
  std::size_t nframes = frames.size ();

  octave_map retval (dim_vector (nframes, 1), bt_fields);

  Cell& file = retval.contents (0);
  Cell& name = retval.contents (1);
  Cell& line = retval.contents (2);
  Cell& column = retval.contents (3);

  octave_idx_type k = 0;

  for (const auto& frm : frames)
    {
      file(k) = frm.file_name ();
      name(k) = frm.fcn_name ();
      line(k) = frm.line ();
      column(k) = frm.column ();

      k++;
    }

  return retval;
}

std::list<frame_info>
error_system::make_stack_frame_list (const octave_map& stack)
{
  std::list<frame_info> frames;

  Cell file = stack.contents ("file");
  Cell name = stack.contents ("name");
  Cell line = stack.contents ("line");
  Cell column = stack.contents ("column");

  octave_idx_type nel = name.numel ();

  for (octave_idx_type i = 0; i < nel; i++)
    frames.push_back (frame_info (file(i).string_value (),
                                  name(i).string_value (),
                                  line(i).int_value (),
                                  column(i).int_value ()));

  return frames;
}

// For given warning ID, return 0 if warnings are disabled, 1 if
// enabled, and 2 if the given ID should be an error instead of a
// warning.

int error_system::warning_enabled (const std::string& id)
{
  int retval = 0;

  int all_state = -1;
  int id_state = -1;

  octave_map opts = warning_options ();

  octave_idx_type nel = opts.numel ();

  if (nel > 0)
    {
      Cell identifier = opts.contents ("identifier");
      Cell state = opts.contents ("state");

      bool all_found = false;
      bool id_found = false;

      for (octave_idx_type i = 0; i < nel; i++)
        {
          octave_value ov = identifier(i);
          std::string ovs = ov.string_value ();

          if (! all_found && ovs == "all")
            {
              all_state = check_state (state(i).string_value ());

              if (all_state >= 0)
                all_found = true;
            }

          if (! id_found && ovs == id)
            {
              id_state = check_state (state(i).string_value ());

              if (id_state >= 0)
                id_found = true;
            }

          if (all_found && id_found)
            break;
        }
    }

  // If "all" is not present, assume warnings are enabled.
  if (all_state == -1)
    all_state = 1;

  if (all_state == 0)
    {
      if (id_state >= 0)
        retval = id_state;
    }
  else if (all_state == 1)
    {
      if (id_state == 0 || id_state == 2)
        retval = id_state;
      else
        retval = all_state;
    }
  else if (all_state == 2)
    {
      if (id_state == 0)
        retval= id_state;
      else
        retval = all_state;
    }

  return retval;
}

void error_system::vusage (const char *id, const char *fmt, va_list args)
{
  std::string str_id = id ? id : "";
  std::string message = format_message (fmt, args);

  throw_error ("usage", str_id, message);
}

void error_system::vwarning (const char *name, const char *id,
                             const char *fmt, va_list args)
{
  flush_stdout ();

  std::string base_msg = format_message (fmt, args);
  std::string msg_string;

  if (name)
    msg_string = std::string (name) + ": ";

  msg_string += base_msg;

  bool fmt_suppresses_backtrace = false;
  std::size_t fmt_len = (fmt ? strlen (fmt) : 0);
  fmt_suppresses_backtrace = (fmt_len > 0 && fmt[fmt_len-1] == '\n');

  if (! fmt_suppresses_backtrace)
    msg_string += '\n';

  last_warning_id (id);
  last_warning_message (base_msg);

  if (discard_warning_messages ())
    return;

  tree_evaluator& tw = m_interpreter.get_evaluator ();

  bool in_user_code = tw.in_user_code ();

  if (! quiet_warning ())
    {
      octave_diary << msg_string;
      std::cerr << msg_string;

      if (! fmt_suppresses_backtrace && in_user_code
          && backtrace_on_warning ()
          && ! discard_warning_messages ())
        {
          std::string bt_msg = tw.backtrace_message ();

          if (! bt_msg.empty ())
            bt_msg = "warning: called from\n" + bt_msg;

          octave_diary << bt_msg << std::endl;
          std::cerr << bt_msg << std::endl;
        }
    }

  bp_table& bptab = tw.get_bp_table ();

  if ((m_interpreter.interactive ()
       || application::forced_interactive ())
      && debug_on_warning () && in_user_code && bptab.debug_on_warn (id))
    {
      unwind_protect_var<bool> restore_var (m_debug_on_warning, false);

      tw.enter_debugger ();
    }
}

void error_system::error_1 (execution_exception& ee, const char *id,
                            const char *fmt, va_list args)
{
  ee.set_identifier (id);
  ee.set_message (format_message (fmt, args));

  throw_error (ee);
}

void error_system::error_1 (const char *id, const char *fmt,
                            va_list args)
{
  std::string message = format_message (fmt, args);

  std::list<frame_info> stack_info;

  throw_error ("error", id, message);
}

void error_system::vwarning (const char *id, const char *fmt, va_list args)
{
  int warn_opt = warning_enabled (id);

  if (warn_opt == 2)
    {
      // Handle this warning as an error.

      error_1 (id, fmt, args);
    }
  else if (warn_opt == 1)
    vwarning ("warning", id, fmt, args);
}

void error_system::rethrow_error (const std::string& id,
                                  const std::string& msg,
                                  const octave_map& stack)
{
  std::list<frame_info> stack_info;

  execution_exception ee ("error", id, msg, stack_info);

  if (! stack.isempty ())
    {
      if (! (stack.contains ("file") && stack.contains ("name")
             && stack.contains ("line")))
        error ("rethrow: STACK struct must contain the fields 'file', 'name', and 'line'");

      if (! stack.contains ("column"))
        {
          octave_map new_stack = stack;

          new_stack.setfield ("column", Cell (octave_value (-1)));

          ee.set_stack_info (make_stack_frame_list (new_stack));
        }
      else
        ee.set_stack_info (make_stack_frame_list (stack));
    }

  throw_error (ee);
}

void error_system::vpanic (const char *fmt, va_list args)
{
  // Is there any point in trying to write the panic message to the
  // diary?

  std::cerr << "panic: " << format_message (fmt, args) << std::endl;

  abort ();
}

void error_system::panic (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  vpanic (fmt, args);
  va_end (args);
}

octave_scalar_map error_system::warning_query (const std::string& id_arg)
{
  octave_scalar_map retval;

  std::string id = id_arg;

  if (id == "last")
    id = last_warning_id ();

  octave_map opts = warning_options ();

  Cell ident = opts.contents ("identifier");
  Cell state = opts.contents ("state");

  octave_idx_type nel = ident.numel ();

  panic_if (nel == 0);

  bool found = false;

  std::string val;

  for (octave_idx_type i = 0; i < nel; i++)
    {
      if (ident(i).string_value () == id)
        {
          val = state(i).string_value ();
          found = true;
          break;
        }
    }

  if (! found)
    {
      for (octave_idx_type i = 0; i < nel; i++)
        {
          if (ident(i).string_value () == "all")
            {
              val = state(i).string_value ();
              found = true;
              break;
            }
        }
    }

  // The warning state "all" is always supposed to remain in the list,
  // so we should always find a state, either explicitly or by using the
  // state for "all".
  panic_unless (found);

  retval.assign ("identifier", id);
  retval.assign ("state", val);

  return retval;
}

std::string error_system::default_warning_state (void)
{
  std::string retval = "on";

  octave_map opts = warning_options ();

  Cell ident = opts.contents ("identifier");
  Cell state = opts.contents ("state");

  octave_idx_type nel = ident.numel ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      if (ident(i).string_value () == "all")
        {
          retval = state(i).string_value ();
          break;
        }
    }

  return retval;
}

void error_system::display_warning_options (std::ostream& os)
{
  octave_map opts = warning_options ();

  Cell ident = opts.contents ("identifier");
  Cell state = opts.contents ("state");

  octave_idx_type nel = ident.numel ();

  std::string all_state = default_warning_state ();

  if (all_state == "on")
    os << "By default, warnings are enabled.";
  else if (all_state == "off")
    os << "By default, warnings are disabled.";
  else if (all_state == "error")
    os << "By default, warnings are treated as errors.";
  else
    panic_impossible ();

  if (nel > 1)
    {
      os << "\n";
      os << "Non-default warning states are:\n\n";
      os << "  State  Warning ID\n";
    }

  // The state for "all" is always supposed to be first in the list.

  for (octave_idx_type i = 1; i < nel; i++)
    {
      std::string tid = ident(i).string_value ();
      std::string tst = state(i).string_value ();

      os << std::setw (7) << tst << "  " << tid << "\n";
    }

  os << std::endl;
}

void error_system::set_warning_option (const std::string& state,
                                       const std::string& ident)
{
  std::string all_state = default_warning_state ();

  if (state != "on" && state != "off" && state != "error")
    error ("invalid warning state: %s", state.c_str ());

  octave_map opts = warning_options ();

  Cell tid = opts.contents ("identifier");
  Cell tst = opts.contents ("state");

  octave_idx_type nel = tid.numel ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      if (tid(i).string_value () == ident)
        {
          // We found it in the current list of options.  If the state
          // for "all" is same as arg1, we can simply remove the item
          // from the list.

          if (state == all_state && ident != "all")
            {
              for (i = i + 1; i < nel; i++)
                {
                  tid(i-1) = tid(i);
                  tst(i-1) = tst(i);
                }

              tid.resize (dim_vector (1, nel-1));
              tst.resize (dim_vector (1, nel-1));
            }
          else
            tst(i) = state;

          opts.clear ();

          opts.assign ("identifier", tid);
          opts.assign ("state", tst);

          warning_options (opts);

          return;
        }
    }

  // The option wasn't already in the list.  Append it.

  tid.resize (dim_vector (1, nel+1));
  tst.resize (dim_vector (1, nel+1));

  tid(nel) = ident;
  tst(nel) = state;

  opts.clear ();

  opts.assign ("identifier", tid);
  opts.assign ("state", tst);

  warning_options (opts);
}

void error_system::disable_warning (const std::string& id)
{
  set_warning_option ("off", id);
}

void error_system::initialize_default_warning_state (void)
{
  warning_options (init_warning_options ("on"));

  // Most people will want to have the following disabled.

  disable_warning ("Octave:array-as-logical");
  disable_warning ("Octave:array-to-scalar");
  disable_warning ("Octave:array-to-vector");
  disable_warning ("Octave:imag-to-real");
  disable_warning ("Octave:language-extension");
  disable_warning ("Octave:missing-semicolon");
  disable_warning ("Octave:neg-dim-as-zero");
  disable_warning ("Octave:separator-insert");
  disable_warning ("Octave:single-quote-string");
  disable_warning ("Octave:str-to-num");
  disable_warning ("Octave:mixed-string-concat");
  disable_warning ("Octave:variable-switch-label");
}

void error_system::interpreter_try (unwind_protect& frame)
{
  frame.protect_var (m_debug_on_error);
  m_debug_on_error = false;

  frame.protect_var (m_debug_on_warning);
  m_debug_on_warning = false;

  // Leave debug_on_caught as it was, so errors in try/catch are still
  // caught.
}

void error_system::throw_error (const std::string& err_type,
                                const std::string& id,
                                const std::string& message,
                                const std::list<frame_info>& stack_info_arg)
{
  std::list<frame_info> stack_info = stack_info_arg;

  if (stack_info.empty ())
    {
      tree_evaluator& tw = m_interpreter.get_evaluator ();

      stack_info = tw.backtrace_info ();

      // Print the error message only if it is different from the
      // previous one; makes the output more concise and readable.

      stack_info.unique ();
    }

  execution_exception ex (err_type, id, message, stack_info);

  throw_error (ex);
}

void error_system::throw_error (execution_exception& ex)
{
  throw ex;
}

void error_system::save_exception (const execution_exception& ee)
{
  last_error_id (ee.identifier ());
  std::string message = ee.message ();
  std::string xmsg
    = (message.size () > 0 && message.back () == '\n'
       ? message.substr (0, message.size () - 1) : message);
  last_error_message (xmsg);
  last_error_stack (make_stack_map (ee.stack_info ()));
}

// DEPRECATED in Octave 7.
void error_system::display_exception (const execution_exception& ee,
                                      std::ostream& os) const
{
  if (m_beep_on_error)
    os << "\a";

  ee.display (octave_diary);
  ee.display (os);
}

void error_system::display_exception (const execution_exception& ee) const
{
  // FIXME: How should we handle beep_on_error?

  ee.display (octave_diary);

  // FIXME: Handle display using an event manager message so that the
  // GUI or other client can receive error messages without needing to
  // capture them from std::cerr or some other stream.

  event_manager& evmgr = m_interpreter.get_event_manager ();

  evmgr.display_exception (ee, m_beep_on_error);
}

OCTAVE_END_NAMESPACE(octave)

void
vmessage (const char *name, const char *fmt, va_list args)
{
  std::string message;

  if (name)
    message = std::string (name) + ": ";

  message += format_message (fmt, args);

  octave_diary << message << std::endl;
  std::cerr << message << std::endl;
}

void
message (const char *name, const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  vmessage (name, fmt, args);
  va_end (args);
}

void
vusage_with_id (const char *id, const char *fmt, va_list args)
{
  octave::error_system& es = octave::__get_error_system__ ();

  es.vusage (id, fmt, args);
}

void
usage_with_id (const char *id, const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  vusage_with_id (id, fmt, args);
  va_end (args);
}

void
verror (const char *fmt, va_list args)
{
  error_1 ("", fmt, args);
}

void
error (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  verror (fmt, args);
  va_end (args);
}

void
verror (octave::execution_exception& ee, const char *fmt, va_list args)
{
  error_1 (ee, "", fmt, args);
}

void
error (octave::execution_exception& ee, const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  verror (ee, fmt, args);
  va_end (args);
}

void
verror_with_cfn (const char *fmt, va_list args)
{
  error_1 ("", fmt, args);
}

void
error_with_cfn (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  verror_with_cfn (fmt, args);
  va_end (args);
}

void
verror_with_id (const char *id, const char *fmt, va_list args)
{
  error_1 (id, fmt, args);
}

void
error_with_id (const char *id, const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  verror_with_id (id, fmt, args);
  va_end (args);
}

void
verror_with_id_cfn (const char *id, const char *fmt, va_list args)
{
  error_1 (id, fmt, args);
}

void
error_with_id_cfn (const char *id, const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  verror_with_id_cfn (id, fmt, args);
  va_end (args);
}

void
vwarning (const char *fmt, va_list args)
{
  vwarning ("", fmt, args);
}

void
warning (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  vwarning (fmt, args);
  va_end (args);
}

void
vwarning_with_id (const char *id, const char *fmt, va_list args)
{
  vwarning (id, fmt, args);
}

void
warning_with_id (const char *id, const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  vwarning (id, fmt, args);
  va_end (args);
}

void
vparse_error (const char *fmt, va_list args)
{
  error_1 ("", fmt, args);
}

void
parse_error (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  vparse_error (fmt, args);
  va_end (args);
}

void
vparse_error_with_id (const char *id, const char *fmt, va_list args)
{
  error_1 (id, fmt, args);
}

void
parse_error_with_id (const char *id, const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  vparse_error_with_id (id, fmt, args);
  va_end (args);
}

OCTAVE_NORETURN
void
vpanic (const char *fmt, va_list args)
{
  octave::error_system& es = octave::__get_error_system__ ();

  es.vpanic (fmt, args);
}

OCTAVE_NORETURN
void
panic (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  vpanic (fmt, args);
  va_end (args);
}

OCTAVE_BEGIN_NAMESPACE(octave)

void
defun_usage_message (const std::string& msg)
{
  ::defun_usage_message ("%s", msg.c_str ());
}

DEFMETHOD (rethrow, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn {} {} rethrow (@var{err})
Reissue a previous error as defined by @var{err}.

@var{err} is a structure that must contain at least the @qcode{"message"}
and @qcode{"identifier"} fields.  @var{err} can also contain a field
@qcode{"stack"} that gives information on the assumed location of the
error.  Typically @var{err} is returned from @code{lasterror}.
@seealso{lasterror, lasterr, error}
@end deftypefn */)
{
  if (args.length () != 1)
    print_usage ();

  const octave_scalar_map err = args(0).scalar_map_value ();

  if (! (err.contains ("message") && err.contains ("identifier")))
    error ("rethrow: ERR struct must contain the fields 'message' and 'identifier'");

  std::string msg = err.contents ("message").string_value ();
  std::string id = err.contents ("identifier").string_value ();

  octave_map err_stack = init_error_stack (interp);

  if (err.contains ("stack"))
    err_stack = err.contents ("stack").xmap_value ("ERR.STACK must be a struct");

  error_system& es = interp.get_error_system ();

  es.rethrow_error (id, msg, err_stack);

  return ovl ();
}

DEFMETHOD (error, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} error (@var{template}, @dots{})
@deftypefnx {} {} error (@var{id}, @var{template}, @dots{})
Display an error message and stop m-file execution.

Format the optional arguments under the control of the template string
@var{template} using the same rules as the @code{printf} family of
functions (@pxref{Formatted Output}) and print the resulting message
on the @code{stderr} stream.  This formatting is only done for
single-quoted character vectors if there are additional arguments
following the template string.  If there are no additional arguments, the
template string is used literally (i.e., without interpreting any escape
sequences in single-quoted character vectors).  The message is prefixed
by @samp{error: }.

Calling @code{error} also sets Octave's internal error state such that
control will return to the top level without evaluating any further
commands.  This is useful for aborting from functions or scripts.

If the error message does not end with a newline character, Octave will
print a traceback of all the function calls leading to the error.  For
example, given the following function definitions:

@example
@group
function f () g (); end
function g () h (); end
function h () nargin == 1 || error ("nargin != 1"); end
@end group
@end example

@noindent
calling the function @code{f} will result in a list of messages that
can help you to quickly find the exact location of the error:

@example
@group
f ()
error: nargin != 1
error: called from:
error:   h at line 1, column 27
error:   g at line 1, column 15
error:   f at line 1, column 15
@end group
@end example

If the error message ends in a newline character, Octave will print the
message but will not display any traceback messages as it returns
control to the top level.  For example, modifying the error message
in the previous example to end in a newline causes Octave to only print
a single message:

@example
@group
function h () nargin == 1 || error ("nargin != 1\n"); end
f ()
error: nargin != 1
@end group
@end example

A null string ("") input to @code{error} will be ignored and the code
will continue running as if the statement were a NOP@.  This is for
compatibility with @sc{matlab}.  It also makes it possible to write code
such as

@example
@group
err_msg = "";
if (CONDITION 1)
  err_msg = "CONDITION 1 found";
elseif (CONDITION2)
  err_msg = "CONDITION 2 found";
@dots{}
endif
error (err_msg);
@end group
@end example

@noindent
which will only stop execution if an error has been found.

Implementation Note: For compatibility with @sc{matlab}, escape
sequences in @var{template} (e.g., @qcode{"@backslashchar{}n"} =>
newline) are processed regardless of whether @var{template} has been defined
with single quotes, as long as there are two or more input arguments.  To
disable escape sequence expansion use a second backslash before the sequence
(e.g., @qcode{"@backslashchar{}@backslashchar{}n"}) or use the
@code{regexptranslate} function.
@seealso{warning, lasterror}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin == 0)
    print_usage ();

  octave_value retval;

  std::string id;
  std::string message;
  std::list<frame_info> stack_info;

  bool have_fmt = false;

  if (nargin == 1 && args(0).isstruct ())
    {
      // empty struct is not an error.  return and resume calling function.
      if (args(0).isempty ())
        return retval;

      octave_scalar_map m = args(0).scalar_map_value ();

      // empty struct is not an error.  return and resume calling function.
      if (m.nfields () == 0)
        return retval;

      if (m.contains ("message"))
        {
          octave_value c = m.getfield ("message");

          if (c.is_string ())
            message = c.string_value ();
        }

      if (m.contains ("identifier"))
        {
          octave_value c = m.getfield ("identifier");

          if (c.is_string ())
            id = c.string_value ();
        }

      if (m.contains ("stack"))
        {
          octave_value c = m.getfield ("stack");

          if (c.isstruct ())
            stack_info
              = error_system::make_stack_frame_list (c.map_value ());
        }
    }
  else
    {
      octave_value_list nargs = args;

      have_fmt = maybe_extract_message_id ("error", args, nargs, id);

      if (nargs.length () == 0)
        message = "unspecified error";
      else
        {
          octave_value arg;

          if (have_fmt)
            {
              octave_value_list tmp = Fsprintf (nargs, 1);
              arg = tmp(0);
            }
          else
            arg = nargs(0);

          if (arg.is_defined ())
            {
              if (arg.isempty ())
                message = "";
              else if (arg.is_string ())
                message = arg.string_value ();
            }
        }
    }

  if (message.empty ())
    return retval;

  error_system& es = interp.get_error_system ();

  es.throw_error ("error", id, message, stack_info);

  return retval;
}

DEFMETHOD (warning, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {} warning (@var{template}, @dots{})
@deftypefnx {} {} warning (@var{id}, @var{template}, @dots{})
@deftypefnx {} {} warning ("on", @var{id})
@deftypefnx {} {} warning ("off", @var{id})
@deftypefnx {} {} warning ("error", @var{id})
@deftypefnx {} {} warning ("query", @var{id})
@deftypefnx {} {} warning (@var{state}, @var{id}, "local")
@deftypefnx {} {} warning (@var{warning_struct})
@deftypefnx {} {@var{warning_struct} =} warning (@dots{})
@deftypefnx {} {} warning (@var{state}, @var{mode})

Display a warning message or control the behavior of Octave's warning system.

The first call form uses a template @var{template} and optional additional
arguments to display a message on the @code{stderr} stream.  The message is
formatted using the same rules as the @code{printf} family of functions
(@pxref{Formatted Output}) and prefixed by the character string
@w{@samp{warning: }}.  You should use this function when you want to notify the
user of an unusual condition, but only when it makes sense for your program to
go on.  For example:

@example
@group
warning ("foo: maybe something wrong here");
@end group
@end example

If the warning message does not end with a newline character, Octave will
print a traceback of all the function calls leading to the warning.  If the
warning message does end in a newline character, Octave will suppress the
traceback messages as it returns control to the top level.  For more details
and examples, @pxref{XREFerror,,@code{error}}.

The optional warning identifier @var{id} allows users to enable or disable
warnings tagged by this identifier.  A message identifier is a string of the
form @qcode{"NAMESPACE:WARNING-NAME"}.  Octave's own warnings use the
@qcode{"Octave"} namespace (@pxref{XREFwarning_ids,,@code{warning_ids}}).  For
example:

@example
@group
warning ("MyNameSpace:check-something",
         "foo: maybe something wrong here");
@end group
@end example

The second call form is meant to change and/or query the state of warnings.
The first input argument must be a string @var{state} (@qcode{"on"},
@qcode{"off"}, @qcode{"error"}, or @qcode{"query"}) followed by an optional
warning identifier @var{id} or @qcode{"all"} (default).

The optional output argument @var{warning_struct} is a structure or structure
array with fields @qcode{"state"} and @qcode{"identifier"}.  The @var{state}
argument may have the following values:

@table @asis
@item @qcode{"on"}|@qcode{"off"}:
Enable or disable the display of warnings identified by @var{id} and optionally
return their previous state @var{stout}.

@item @qcode{"error"}:
Turn warnings identified by @var{id} into errors and optionally return their
previous state @var{stout}.

@item @qcode{"query"}:
Return the current state of warnings identified by @var{id}.
@end table

A structure or structure array @var{warning_struct}, with fields
@qcode{"state"} and @qcode{"identifier"}, may be given as an input to achieve
equivalent results.  The following example shows how to temporarily disable a
warning and then restore its original state:

@example
@group
loglog (-1:10);
## Disable the previous warning and save its original state
[~, id] = lastwarn ();
warnstate = warning ("off", id);
loglog (-1:10);
## Restore its original state
warning (warnstate);
@end group
@end example

If a final argument @qcode{"local"} is provided then the warning state will be
set temporarily until the end of the current function.  Changes to warning
states that are set locally affect the current function and all functions
called from the current scope.  The previous warning state is restored on
return from the current function.  The @qcode{"local"} option is ignored if
used in the top-level workspace.

With no input argument @code{warning ()} is equivalent to
@code{warning ("query", "all")} except that in the absence of an output
argument, the state of warnings is displayed on @code{stderr}.

The level of verbosity of the warning system may also be controlled by two
modes @var{mode}:

@table @asis
@item @qcode{"backtrace"}:
enable/disable the display of the stack trace after the warning message

@item @qcode{"verbose"}:
enable/disable the display of additional information after the warning message
@end table

In this case the @var{state} argument may only be @qcode{"on"} or
@qcode{"off"}.

Implementation Note: For compatibility with @sc{matlab}, escape sequences in
@var{template} (e.g., @qcode{"@backslashchar{}n"} => newline) are processed
regardless of whether @var{template} has been defined with single quotes, as
long as there are two or more input arguments.  To disable escape sequence
expansion use a second backslash before the sequence (e.g.,
@qcode{"@backslashchar{}@backslashchar{}n"}) or use the
@code{regexptranslate} function.
@seealso{warning_ids, lastwarn, error}
@end deftypefn */)
{
  octave_value retval;

  int nargin = args.length ();
  bool done = false;

  error_system& es = interp.get_error_system ();

  if (nargin > 0 && args.all_strings_p ())
    {
      string_vector argv = args.make_argv ("warning");

      std::string arg1 = argv[1];
      std::transform (arg1.begin (), arg1.end (), arg1.begin (), tolower);
      std::string arg2 = "all";
      std::string arg2_lc = "all";

      if (nargin >= 2)
        {
          arg2 = argv[2];
          arg2_lc = arg2;
          std::transform (arg2_lc.begin (), arg2_lc.end (), arg2_lc.begin (),
                          tolower);
        }

      if (arg1 == "on" || arg1 == "off" || arg1 == "error")
        {
          // Prepare output structure
          octave_map old_warning_options;
          if (arg2_lc == "all")
            old_warning_options = es.warning_options ();
          else
            old_warning_options = octave_map (es.warning_query (arg2));

          if (nargin == 3)
            {
              std::string arg3_lc = argv[3];
              std::transform (arg3_lc.begin (), arg3_lc.end (),
                              arg3_lc.begin (), tolower);
              if (arg3_lc == "local" && ! interp.at_top_level ())
                {
                  octave_scalar_map val = es.warning_query (arg2);

                  octave_value curr_state = val.contents ("state");

                  // FIXME: this might be better with a dictionary object.
                  tree_evaluator& tw = interp.get_evaluator ();

                  octave_value curr_warning_states
                    = tw.get_auto_fcn_var (stack_frame::SAVED_WARNING_STATES);

                  octave_map m;

                  if (curr_warning_states.is_defined ())
                    m = curr_warning_states.map_value ();
                  else
                    {
                      string_vector fields (2);

                      fields(0) = "identifier";
                      fields(1) = "state";

                      m = octave_map (dim_vector (0, 1), fields);
                    }

                  Cell ids = m.contents ("identifier");
                  Cell states = m.contents ("state");

                  octave_idx_type nel = states.numel ();
                  bool found = false;
                  octave_idx_type i;
                  for (i = 0; i < nel; i++)
                    {
                      std::string id = ids(i).string_value ();

                      if (id == arg2)
                        {
                          states(i) = curr_state;
                          found = true;
                          break;
                        }
                    }

                  if (! found)
                    {
                      m.resize (dim_vector (nel+1, 1));

                      ids.resize (dim_vector (nel+1, 1));
                      states.resize (dim_vector (nel+1, 1));

                      ids(nel) = arg2;
                      states(nel) = curr_state;
                    }

                  m.contents ("identifier") = ids;
                  m.contents ("state") = states;

                  tw.set_auto_fcn_var (stack_frame::SAVED_WARNING_STATES, m);

                  // Now ignore the "local" argument,
                  // and continue to handle the current setting.
                  nargin--;
                }
            }

          if ((nargin == 1
               && (arg1 == "on" || arg1 == "off" || arg1 == "error"))
              || (nargin >= 2 && arg2_lc == "all"))
            {
              // If "all" is given implicitly or explicitly as ID.
              if (arg1 == "error")
                error (R"(warning: cannot specify "all" warning ID with state "error")");

              octave_map tmp;

              Cell id (1, 1);
              Cell st (1, 1);

              id(0) = "all";
              st(0) = arg1;

              tmp.assign ("identifier", id);
              tmp.assign ("state", st);

              es.warning_options (tmp);

              done = true;
            }
          else if (arg2_lc == "backtrace")
            {
              if (arg1 != "error")
                {
                  es.backtrace_on_warning (arg1 == "on");
                  done = true;
                }
            }
          else if (arg2_lc == "debug")
            {
              if (arg1 != "error")
                {
                  es.debug_on_warning (arg1 == "on");
                  done = true;
                }
            }
          else if (arg2_lc == "verbose")
            {
              if (arg1 != "error")
                {
                  es.verbose_warning (arg1 == "on");
                  done = true;
                }
            }
          else if (arg2_lc == "quiet")
            {
              if (arg1 != "error")
                {
                  es.quiet_warning (arg1 == "on");
                  done = true;
                }
            }
          else
            {
              if (arg2_lc == "last")
                arg2 = es.last_warning_id ();

              es.set_warning_option (arg1, arg2);

              done = true;
            }

          if (done && nargout > 0)
            retval = old_warning_options;
        }
      else if (arg1 == "query")
        {
          if (arg2_lc == "all")
            {
              if (nargout > 0)
                retval = es.warning_options ();
              else
                es.display_warning_options (octave_stdout);
            }
          else if (arg2_lc == "backtrace" || arg2_lc == "debug"
                   || arg2_lc == "verbose" || arg2_lc == "quiet")
            {
              if (nargout > 0)
                {
                  octave_scalar_map tmp;
                  tmp.assign ("identifier", arg2_lc);
                  if (arg2_lc == "backtrace")
                    tmp.assign ("state", es.backtrace_on_warning () ? "on" : "off");
                  else if (arg2_lc == "debug")
                    tmp.assign ("state", es.debug_on_warning () ? "on" : "off");
                  else if (arg2_lc == "verbose")
                    tmp.assign ("state", es.verbose_warning () ? "on" : "off");
                  else
                    tmp.assign ("state", es.quiet_warning () ? "on" : "off");

                  retval = tmp;
                }
              else
                {
                  if (arg2_lc == "backtrace")
                    octave_stdout << R"("backtrace" warning state is ")" <<
                                  (es.backtrace_on_warning () ? "on" : "off") <<
                                  "\"\n";
                  else if (arg2_lc == "debug")
                    octave_stdout << R"("debug" warning state is ")" <<
                                  (es.debug_on_warning () ? "on" : "off") <<
                                  "\"\n";
                  else if (arg2_lc == "verbose")
                    octave_stdout << R"("verbose" warning state is ")" <<
                                  (es.verbose_warning () ? "on" : "off") <<
                                  "\"\n";
                  else
                    octave_stdout << R"("quiet" warning state is ")" <<
                                  (es.quiet_warning () ? "on" : "off") <<
                                  "\"\n";
                }
            }
          else
            {
              if (nargout > 0)
                retval = es.warning_query (arg2);
              else
                {
                  octave_scalar_map tmp = es.warning_query (arg2);

                  octave_stdout << '"' << arg2 << R"(" warning state is ")" <<
                                tmp.getfield ("state").string_value () <<
                                "\"\n";
                }
            }

          done = true;
        }
    }
  else if (nargin == 0)
    {
      if (nargout > 0)
        retval = es.warning_options ();
      else
        es.display_warning_options (octave_stdout);

      done = true;
    }
  else if (nargin == 1)
    {
      octave_value arg = args(0);

      octave_map old_warning_options;

      if (arg.isstruct ())
        {
          octave_map m = arg.map_value ();

          if (! m.contains ("identifier") || ! m.contains ("state"))
            error ("warning: STATE structure must have fields 'identifier' and 'state'");

          // Simply step through the struct elements one at a time.

          Cell ident = m.contents ("identifier");
          Cell state = m.contents ("state");

          octave_idx_type nel = ident.numel ();

          // Prepare output structure
          old_warning_options = octave_map (m);
          Cell oldstate (state);

          for (octave_idx_type i = 0; i < nel; i++)
            {
              std::string tid = ident(i).string_value ();
              oldstate(i) = es.warning_query (tid).getfield ("state");
            }
          old_warning_options.setfield ("state", oldstate);

          // Set new values
          for (octave_idx_type i = 0; i < nel; i++)
            {
              std::string tst = state(i).string_value ();
              std::string tid = ident(i).string_value ();

              es.set_warning_option (tst, tid);
            }

          done = true;

          if (nargout > 0)
            retval = old_warning_options;
        }
    }

  if (! done)
    {
      octave_value_list nargs = args;

      std::string id;

      bool have_fmt = maybe_extract_message_id ("warning", args, nargs, id);

      std::string prev_msg = es.last_warning_message ();

      std::string curr_msg = handle_message (warning_with_id, id.c_str (),
                                             "unspecified warning", nargs,
                                             have_fmt);

      if (nargout > 0)
        retval = prev_msg;
    }

  return retval;
}

/*

%!test <*51997>
%! id = "Octave:logical-conversion";
%! current = warning ("query", id);
%! current_all = warning ();
%! previous = warning (current_all);
%! assert (previous, current_all);
%! previous = warning (current);
%! assert (previous, current);
%! previous = warning (current.state, id);
%! assert (previous, current);

%!test <*57290>
%! warning ("oN", "Octave:test-57290-ID");
%! warnst = warning ("QUery", "Octave:test-57290-ID");
%! assert (warnst.state, "on");
%! assert (warnst.identifier, "Octave:test-57290-ID");
%! warning ("OFF", "Octave:test-57290-ID");
%! warnst = warning ("QUery", "ALL");
%! idx = strcmp ({warnst.identifier}, "Octave:test-57290-ID");
%! assert (warnst(idx).state, "off");

%!error <cannot specify "all" warning ID> warning ("error")

*/

octave_value_list
set_warning_state (const std::string& id, const std::string& state)
{
  octave_value_list args;

  args(1) = id;
  args(0) = state;

  interpreter& interp = __get_interpreter__ ();

  return Fwarning (interp, args, 1);
}

octave_value_list
set_warning_state (const octave_value_list& args)
{
  interpreter& interp = __get_interpreter__ ();

  return Fwarning (interp, args, 1);
}

int
warning_enabled (const std::string& id)
{
  error_system& es = __get_error_system__ ();

  return es.warning_enabled (id);
}

void
disable_warning (const std::string& id)
{
  error_system& es = __get_error_system__ ();

  es.disable_warning (id);
}

DEFMETHOD (lasterror, interp, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{lasterr} =} lasterror ()
@deftypefnx {} {} lasterror (@var{err})
@deftypefnx {} {} lasterror ("reset")
Query or set the last error message structure.

When called without arguments, return a structure containing the last error
message and other information related to this error.  The elements of the
structure are:

@table @code
@item message
The text of the last error message

@item identifier
The message identifier of this error message

@item stack
A structure containing information on where the message occurred.  This may
be an empty structure if the information cannot be obtained.  The fields of
the structure are:

@table @code
@item file
The name of the file where the error occurred

@item name
The name of function in which the error occurred

@item line
The line number at which the error occurred

@item column
An optional field with the column number at which the error occurred
@end table
@end table

The last error structure may be set by passing a scalar structure,
@var{err}, as input.  Any fields of @var{err} that match those above are
set while any unspecified fields are initialized with default values.

If @code{lasterror} is called with the argument @qcode{"reset"}, all
fields are set to their default values.
@seealso{lasterr, error, lastwarn}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 1)
    print_usage ();

  error_system& es = interp.get_error_system ();

  octave_scalar_map err;

  err.assign ("message", es.last_error_message ());
  err.assign ("identifier", es.last_error_id ());

  err.assign ("stack", octave_value (es.last_error_stack ()));

  if (nargin == 1)
    {
      tree_evaluator& tw = interp.get_evaluator ();

      if (args(0).is_string ())
        {
          if (args(0).string_value () != "reset")
            error ("lasterror: unrecognized string argument");

          es.last_error_message ("");
          es.last_error_id ("");

          es.last_error_stack (tw.empty_backtrace ());
        }
      else if (args(0).isstruct ())
        {
          octave_scalar_map new_err = args(0).scalar_map_value ();
          octave_scalar_map new_err_stack;
          std::string new_error_message;
          std::string new_error_id;
          std::string new_error_file;
          std::string new_error_name;
          int new_error_line = -1;
          int new_error_column = -1;
          bool initialize_stack = false;

          if (new_err.contains ("message"))
            {
              const std::string tmp
                = new_err.getfield ("message").string_value ();
              new_error_message = tmp;
            }

          if (new_err.contains ("identifier"))
            {
              const std::string tmp
                = new_err.getfield ("identifier").string_value ();
              new_error_id = tmp;
            }

          if (new_err.contains ("stack"))
            {
              if (new_err.getfield ("stack").isempty ())
                initialize_stack = true;
              else
                {
                  new_err_stack
                    = new_err.getfield ("stack").scalar_map_value ();

                  if (new_err_stack.contains ("file"))
                    {
                      const std::string tmp
                        = new_err_stack.getfield ("file").string_value ();
                      new_error_file = tmp;
                    }

                  if (new_err_stack.contains ("name"))
                    {
                      const std::string tmp
                        = new_err_stack.getfield ("name").string_value ();
                      new_error_name = tmp;
                    }

                  if (new_err_stack.contains ("line"))
                    {
                      const int tmp
                        = new_err_stack.getfield ("line").nint_value ();
                      new_error_line = tmp;
                    }

                  if (new_err_stack.contains ("column"))
                    {
                      const int tmp
                        = new_err_stack.getfield ("column").nint_value ();
                      new_error_column = tmp;
                    }
                }
            }

          es.last_error_message (new_error_message);
          es.last_error_id (new_error_id);

          if (initialize_stack)
            es.last_error_stack (tw.empty_backtrace ());
          else if (new_err.contains ("stack"))
            {
              new_err_stack.setfield ("file", new_error_file);
              new_err_stack.setfield ("name", new_error_name);
              new_err_stack.setfield ("line", new_error_line);
              new_err_stack.setfield ("column", new_error_column);

              es.last_error_stack (new_err_stack);
            }
          else
            es.last_error_stack (tw.backtrace ());
        }
      else
        error ("lasterror: argument must be a structure or a string");
    }

  return ovl (err);
}

/*
## Test lasterror with empty error state
%!test
%! lasterror ("reset");
%! x = lasterror ();
%! assert (x.identifier, "")
%! assert (x.message, "")
%! assert (isempty (x.stack))
%! lasterror (x);
%! y = lasterror ();
%! assert (y, x);
*/

DEFMETHOD (lasterr, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {[@var{msg}, @var{msgid}] =} lasterr ()
@deftypefnx {} {} lasterr (@var{msg})
@deftypefnx {} {} lasterr (@var{msg}, @var{msgid})
Query or set the last error message.

When called without input arguments, return the last error message and
message identifier.

With one argument, set the last error message to @var{msg}.

With two arguments, also set the last message identifier.
@seealso{lasterror, error, lastwarn}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 2)
    print_usage ();

  error_system& es = interp.get_error_system ();

  string_vector argv = args.make_argv ("lasterr");

  std::string prev_error_id = es.last_error_id ();
  std::string prev_error_message = es.last_error_message ();

  if (nargin == 2)
    {
      es.last_error_id (argv[2]);
      es.last_error_message (argv[1]);
    }
  else if (nargin == 1)
    {
      es.last_error_id ("");
      es.last_error_message (argv[1]);
    }

  if (nargin == 0 || nargout > 0)
    return ovl (prev_error_message, prev_error_id);
  else
    return ovl ();
}

DEFMETHOD (lastwarn, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {[@var{msg}, @var{msgid}] =} lastwarn ()
@deftypefnx {} {} lastwarn (@var{msg})
@deftypefnx {} {} lastwarn (@var{msg}, @var{msgid})
Query or set the last warning message.

When called without input arguments, return the last warning message and
message identifier.

With one argument, set the last warning message to @var{msg}.

With two arguments, also set the last message identifier to @var{msgid}.
@seealso{warning, lasterror, lasterr}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 2)
    print_usage ();

  error_system& es = interp.get_error_system ();

  string_vector argv = args.make_argv ("lastwarn");

  std::string prev_warning_id = es.last_warning_id ();
  std::string prev_warning_message = es.last_warning_message ();

  if (nargin == 2)
    {
      es.last_warning_id (argv[2]);
      es.last_warning_message (argv[1]);
    }
  else if (nargin == 1)
    {
      es.last_warning_id ("");
      es.last_warning_message (argv[1]);
    }

  if (nargin == 0 || nargout > 0)
    return ovl (prev_warning_message, prev_warning_id);
  else
    return ovl ();
}

DEFMETHOD (beep_on_error, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} beep_on_error ()
@deftypefnx {} {@var{old_val} =} beep_on_error (@var{new_val})
@deftypefnx {} {@var{old_val} =} beep_on_error (@var{new_val}, "local")
Query or set the internal variable that controls whether Octave will try
to ring the terminal bell before printing an error message.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@end deftypefn */)
{
  error_system& es = interp.get_error_system ();

  return es.beep_on_error (args, nargout);
}

DEFMETHOD (debug_on_error, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} debug_on_error ()
@deftypefnx {} {@var{old_val} =} debug_on_error (@var{new_val})
@deftypefnx {} {@var{old_val} =} debug_on_error (@var{new_val}, "local")
Query or set the internal variable that controls whether Octave will try
to enter the debugger when an error is encountered.

This will also inhibit printing of the normal traceback message (you will
only see the top-level error message).

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{debug_on_warning, debug_on_interrupt}
@end deftypefn */)
{
  error_system& es = interp.get_error_system ();

  return es.debug_on_error (args, nargout);
}

DEFMETHOD (debug_on_warning, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} debug_on_warning ()
@deftypefnx {} {@var{old_val} =} debug_on_warning (@var{new_val})
@deftypefnx {} {@var{old_val} =} debug_on_warning (@var{new_val}, "local")
Query or set the internal variable that controls whether Octave will try
to enter the debugger when a warning is encountered.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{debug_on_error, debug_on_interrupt}
@end deftypefn */)
{
  error_system& es = interp.get_error_system ();

  return es.debug_on_warning (args, nargout);
}

void
interpreter_try (unwind_protect& frame)
{
  error_system& es = __get_error_system__ ();

  es.interpreter_try (frame);
}

OCTAVE_END_NAMESPACE(octave)
