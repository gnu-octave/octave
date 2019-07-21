/*

Copyright (C) 1993-2019 John W. Eaton

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

#include <cstdarg>
#include <cstdlib>
#include <cstring>

#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>

#include "bp-table.h"
#include "builtin-defun-decls.h"
#include "defun.h"
#include "error.h"
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

static void verror (bool save_last_error, std::ostream& os, const char *name,
                    const char *id, const char *fmt, va_list args,
                    bool with_cfn = false)
{
  octave::error_system& es = octave::__get_error_system__ ("verror");

  es.verror (save_last_error, os, name, id, fmt, args, with_cfn);
}

static void
vpr_where (std::ostream& os, const char *fmt, va_list args)
{
  if (fmt)
    {
      if (*fmt)
        {
          size_t len = strlen (fmt);

          if (len > 0)
            {
              if (fmt[len - 1] == '\n')
                {
                  if (len > 1)
                    {
                      std::string tmp_fmt (fmt, len - 1);
                      verror (false, os, nullptr, "", tmp_fmt.c_str (), args);
                    }
                }
              else
                verror (false, os, nullptr, "", fmt, args);
            }
        }
    }
  else
    panic ("vpr_where: invalid format");
}

static void
pr_where_internal (std::ostream& os, const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  vpr_where (os, fmt, args);
  va_end (args);
}

struct
error_stack_frame
{
  std::string name;
  int line;
  int column;
};

static void
pr_where (std::ostream& os, const char *who,
          const std::list<error_stack_frame>& frames)
{
  size_t nframes = frames.size ();

  if (nframes > 0)
    pr_where_internal (os, "%s: called from\n", who);

  for (const auto& frm : frames)
    {
      std::string fcn_name = frm.name;
      int line = frm.line;
      int column = frm.column;

      if (line > 0)
        {
          if (column > 0)
            pr_where_internal (os, "    %s at line %d column %d\n",
                        fcn_name.c_str (), line, column);
          else
            pr_where_internal (os, "    %s at line %d\n",
                               fcn_name.c_str (), line);
        }
      else
        pr_where_internal (os, "    %s\n", fcn_name.c_str ());
    }
}

static void
pr_where (std::ostream& os, const char *who)
{
  octave::tree_evaluator& tw = octave::__get_evaluator__ ("pr_where");

  std::list<octave::stack_frame *> call_stack_frames = tw.backtrace_frames ();

  // Print the error message only if it is different from the previous one;
  // Makes the output more concise and readable.
  call_stack_frames.unique ();

  std::list<error_stack_frame> frames;
  for (const auto *frm : call_stack_frames)
    {
      error_stack_frame frame;

      frame.name = frm->fcn_name ();
      frame.line = frm->line ();
      frame.column = frm->column ();

      frames.push_back (frame);
    }

  pr_where (os, who, frames);
}

static void
maybe_enter_debugger (octave::execution_exception& e,
                      bool show_stack_trace = false)
{
  octave::error_system& es
    = octave::__get_error_system__ ("maybe_enter_debugger");

  es.maybe_enter_debugger (e, show_stack_trace);
}

OCTAVE_NORETURN
static void
vusage (octave::execution_exception& e, const char *id,
        const char *fmt, va_list args)
{
  verror (true, std::cerr, "usage", id, fmt, args);

  maybe_enter_debugger (e);

  throw e;
}

OCTAVE_NORETURN
static void
error_1 (octave::execution_exception& e, std::ostream& os,
         const char *name, const char *id, const char *fmt,
         va_list args, bool with_cfn = false)
{
  octave::error_system& es = octave::__get_error_system__ ("error_1");

  es.error_1 (e, os, name, id, fmt, args, with_cfn);
}

OCTAVE_NORETURN
static void
error_1 (std::ostream& os, const char *name, const char *id,
         const char *fmt, va_list args, bool with_cfn = false)
{
  octave::error_system& es = octave::__get_error_system__ ("error_1");

  es.error_1 (os, name, id, fmt, args, with_cfn);
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
  octave::error_system& es = octave::__get_error_system__ ("warning");

  es.vwarning (id, fmt, args);
}

static std::list<error_stack_frame>
make_stack_frame_list (const octave_map& stack)
{
  std::list<error_stack_frame> frames;

  Cell name = stack.contents ("name");
  Cell line = stack.contents ("line");
  Cell column;
  bool have_column = false;
  if (stack.contains ("column"))
    {
      have_column = true;
      column = stack.contents ("column");
    }

  octave_idx_type nel = name.numel ();

  for (octave_idx_type i = 0; i < nel; i++)
    {
      error_stack_frame frame;

      frame.name = name(i).string_value ();
      frame.line = line(i).int_value ();
      frame.column = (have_column ? column(i).int_value () : -1);

      frames.push_back (frame);
    }

  return frames;
}

static void
defun_usage_message (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  error_1 (octave_stdout, nullptr, "", fmt, args);
  va_end (args);
}

typedef void (*error_fun)(const char *, const char *, ...);

static std::string
handle_message (error_fun f, const char *id, const char *msg,
                const octave_value_list& args, bool have_fmt)
{
  std::string retval;

  std::string tmpstr;

  if (args.length () > 0)
    {
      octave_value arg;

      if (have_fmt)
        {
          octave_value_list tmp = Fsprintf (args, 1);
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

  size_t len = strlen (msg);

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

namespace octave
{
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
      m_buffer_error_messages (0),
      m_in_try_catch (0),
      m_discard_error_messages (false),
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
  error_system::buffer_error_messages (const octave_value_list& args,
                                        int nargout)
  {
    return set_internal_variable (m_buffer_error_messages, args, nargout,
                                  "buffer_error_messages");
  }

  octave_value
  error_system::in_try_catch (const octave_value_list& args, int nargout)
  {
    return set_internal_variable (m_in_try_catch, args, nargout,
                                  "in_try_catch");
  }

  octave_value
  error_system::discard_error_messages (const octave_value_list& args,
                                        int nargout)
  {
    return set_internal_variable (m_discard_error_messages, args, nargout,
                                  "discard_error_messages");
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

  void error_system::verror (bool save_last_error, std::ostream& os,
                             const char *name, const char *id,
                             const char *fmt, va_list args, bool with_cfn)
  {
    if (discard_error_messages () && ! debug_on_caught ())
      return;

    if (! buffer_error_messages () || debug_on_caught ())
      flush_stdout ();

    // FIXME: we really want to capture the message before it has all the
    //        formatting goop attached to it.  We probably also want just the
    //        message, not the traceback information.

    std::ostringstream output_buf;

    vformat (output_buf, fmt, args);

    std::string base_msg = output_buf.str ();

    bool to_beep_or_not_to_beep_p = beep_on_error ();

    std::string msg_string;

    if (to_beep_or_not_to_beep_p)
      msg_string = "\a";

    if (name)
      {
        if (in_try_catch () && ! strcmp (name, "error"))
          msg_string += "caught error: ";
        else
          msg_string += std::string (name) + ": ";
      }

    tree_evaluator& tw = m_interpreter.get_evaluator ();

    // If with_fcn is specified, we'll attempt to prefix the message with the name
    // of the current executing function.  But we'll do so only if:
    // 1. the name is not empty (anonymous function)
    // 2. it is not already there (including the following colon)
    if (with_cfn)
      {
        std::string cfn = tw.current_function_name ();

        if (! cfn.empty ())
          {
            cfn += ':';
            if (cfn.length () > base_msg.length ()
                || base_msg.compare (0, cfn.length (), cfn) != 0)
              msg_string += cfn + ' ';
          }
      }

    msg_string += base_msg + '\n';

    if (save_last_error)
      {
        // This is the first error in a possible series.

        last_error_id (id);
        last_error_message (base_msg);
        last_error_stack (tw.in_user_code ()
                          ? tw.backtrace () : tw.empty_backtrace ());
      }

    if (! buffer_error_messages () || debug_on_caught ())
      {
        octave_diary << msg_string;
        os << msg_string;
      }
  }

  void error_system::maybe_enter_debugger (execution_exception& e,
                                           bool show_stack_trace)
  {
    tree_evaluator& tw = m_interpreter.get_evaluator ();
    bp_table& bptab = tw.get_bp_table ();

    if ((application::interactive ()
         || application::forced_interactive ())
        && ((debug_on_error ()
             && bptab.debug_on_err (last_error_id ()))
            || (debug_on_caught ()
                && bptab.debug_on_caught (last_error_id ())))
        && tw.in_user_code ())
      {
        unwind_protect frame;

        frame.protect_var (m_debug_on_error);
        m_debug_on_error = false;

        if (show_stack_trace)
          {
            std::string stack_trace = e.info ();

            if (! stack_trace.empty ())
              {
                std::cerr << stack_trace;

                e.set_stack_trace ();
              }
          }

        tw.enter_debugger ();
      }
  }

  void error_system::vwarning (const char *name, const char *id,
                               const char *fmt, va_list args)
  {
    if (discard_warning_messages ())
      return;

    flush_stdout ();

    std::ostringstream output_buf;

    vformat (output_buf, fmt, args);

    // FIXME: we really want to capture the message before it has all the
    //        formatting goop attached to it.  We probably also want just the
    //        message, not the traceback information.

    std::string base_msg = output_buf.str ();
    std::string msg_string;

    if (name)
      msg_string = std::string (name) + ": ";

    msg_string += base_msg + '\n';

    last_warning_id (id);
    last_warning_message (base_msg);

    if (! quiet_warning ())
      {
        octave_diary << msg_string;

        std::cerr << msg_string;
      }
  }

  void error_system::error_1 (execution_exception& e, std::ostream& os,
                              const char *name, const char *id,
                              const char *fmt, va_list args, bool with_cfn)
  {
    bool show_stack_trace = false;

    if (fmt)
      {
        if (*fmt)
          {
            size_t len = strlen (fmt);

            if (len > 0)
              {
                if (fmt[len - 1] == '\n')
                  {
                    if (len > 1)
                      {
                        std::string tmp_fmt (fmt, len - 1);
                        verror (true, os, name, id, tmp_fmt.c_str (),
                                args, with_cfn);
                      }

                    // If format ends with newline, suppress stack trace.
                    e.set_stack_trace ();
                  }
                else
                  {
                    verror (true, os, name, id, fmt, args, with_cfn);

                    tree_evaluator& tw = m_interpreter.get_evaluator ();

                    if (tw.in_user_code () && ! discard_error_messages ())
                      show_stack_trace = true;
                  }
              }
          }
      }
    else
      panic ("error_1: invalid format");

    maybe_enter_debugger (e, show_stack_trace);

    throw e;
  }

  void error_system::error_1 (std::ostream& os, const char *name,
                              const char *id, const char *fmt,
                              va_list args, bool with_cfn = false)
  {
    execution_exception e = make_execution_exception ("error");

    error_1 (e, os, name, id, fmt, args, with_cfn);
  }

  void error_system::vwarning (const char *id, const char *fmt, va_list args)
  {
    int warn_opt = warning_enabled (id);

    if (warn_opt == 2)
      {
        // Handle this warning as an error.

        error_1 (std::cerr, "error", id, fmt, args);
      }
    else if (warn_opt == 1)
      {
        bool fmt_suppresses_backtrace = false;
        size_t fmt_len = (fmt ? strlen (fmt) : 0);
        fmt_suppresses_backtrace = (fmt_len > 0 && fmt[fmt_len-1] == '\n');

        if (fmt_suppresses_backtrace && fmt_len > 1)
          {
            // Strip newline before issuing warning
            std::string tmp_fmt (fmt, fmt_len - 1);
            vwarning ("warning", id, tmp_fmt.c_str (), args);
          }
        else
          vwarning ("warning", id, fmt, args);

        tree_evaluator& tw = m_interpreter.get_evaluator ();

        bool in_user_code = tw.in_user_code ();

        if (! fmt_suppresses_backtrace && in_user_code
            && backtrace_on_warning ()
            && ! discard_warning_messages ())
          pr_where (std::cerr, "warning");

        bp_table& bptab = tw.get_bp_table ();

        if ((application::interactive ()
             || application::forced_interactive ())
            && debug_on_warning () && in_user_code && bptab.debug_on_warn (id))
          {
            unwind_protect frame;

            frame.protect_var (m_debug_on_warning);
            m_debug_on_warning = false;

            tw.enter_debugger ();
          }
      }
  }

  void error_system::rethrow_error (const char *id, const char *fmt, ...)
  {
    va_list args;
    va_start (args, fmt);
    verror (false, std::cerr, nullptr, id, fmt, args);
    va_end (args);
  }

  void error_system::rethrow_error (const std::string& id,
                                    const std::string& msg,
                                    const octave_map& stack)
  {
    execution_exception e = make_execution_exception ("error");

    if (! stack.isempty ()
        && ! (stack.contains ("file") && stack.contains ("name")
              && stack.contains ("line")))
      error ("rethrow: STACK struct must contain the fields 'file', 'name', and 'line'");

    last_error_id (id);
    last_error_message (msg);
    last_error_stack (stack);

    size_t len = msg.length ();

    std::string tmp_msg (msg);
    if (len > 1 && tmp_msg[len-1] == '\n')
      {
        tmp_msg.erase (len - 1);

        rethrow_error (id.c_str (), "%s\n", tmp_msg.c_str ());
      }
    else
      rethrow_error (id.c_str (), "%s", tmp_msg.c_str ());

    if (! stack.isempty ())
      {
        std::ostringstream buf;

        pr_where (buf, "error", make_stack_frame_list (stack));

        e.set_stack_trace (buf.str ());
      }

    throw e;
  }

  void error_system::vpanic (const char *fmt, va_list args)
  {
    buffer_error_messages (0);
    discard_error_messages (false);

    verror (false, std::cerr, "panic", "", fmt, args);

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

    assert (nel != 0);

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

    assert (found);

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
      os << "\n\n";

    // The state for all is always supposed to be first in the list.

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
    warning_options (octave::init_warning_options ("on"));

    // Most people will want to have the following disabled.

    disable_warning ("Octave:array-as-logical");
    disable_warning ("Octave:array-to-scalar");
    disable_warning ("Octave:array-to-vector");
    disable_warning ("Octave:imag-to-real");
    disable_warning ("Octave:language-extension");
    disable_warning ("Octave:missing-semicolon");
    disable_warning ("Octave:neg-dim-as-zero");
    disable_warning ("Octave:resize-on-range-error");
    disable_warning ("Octave:separator-insert");
    disable_warning ("Octave:single-quote-string");
    disable_warning ("Octave:str-to-num");
    disable_warning ("Octave:mixed-string-concat");
    disable_warning ("Octave:variable-switch-label");
  }

  void error_system::interpreter_try (octave::unwind_protect& frame,
                                      try_option opt)
  {
    switch (opt)
      {
      case buffer:
        frame.protect_var (m_buffer_error_messages);
        m_buffer_error_messages++;
        break;

      case discard:
        frame.protect_var (m_discard_error_messages);
        m_discard_error_messages = true;
        break;
      }

    frame.protect_var (m_debug_on_error);
    m_debug_on_error = false;

    frame.protect_var (m_debug_on_warning);
    m_debug_on_warning = false;

    // Leave debug_on_caught as it was, so errors in try/catch are still
    // caught.
  }
}

octave::execution_exception
make_execution_exception (const char *who)
{
  std::ostringstream buf;

  pr_where (buf, who);

  octave::execution_exception retval;

  retval.set_stack_trace (buf.str ());

  return retval;
}

void
vmessage (const char *name, const char *fmt, va_list args)
{
  verror (false, std::cerr, name, "", fmt, args);
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
vmessage_with_id (const char *name, const char *id, const char *fmt,
                  va_list args)
{
  verror (false, std::cerr, name, id, fmt, args);
}

void
message_with_id (const char *name, const char *id, const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  vmessage_with_id (name, id, fmt, args);
  va_end (args);
}

void
vusage_with_id (const char *id, const char *fmt, va_list args)
{
  octave::execution_exception e = make_execution_exception ("usage");

  vusage (e, id, fmt, args);
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
  error_1 (std::cerr, "error", "", fmt, args);
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
verror (octave::execution_exception& e, const char *fmt, va_list args)
{
  error_1 (e, std::cerr, "error", "", fmt, args);
}

void
error (octave::execution_exception& e, const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  verror (e, fmt, args);
  va_end (args);
}

void
verror_with_cfn (const char *fmt, va_list args)
{
  error_1 (std::cerr, "error", "", fmt, args, true);
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
  error_1 (std::cerr, "error", id, fmt, args);
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
  error_1 (std::cerr, "error", id, fmt, args, true);
}

void
error_with_id_cfn (const char *id, const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  verror_with_id_cfn (id, fmt, args);
  va_end (args);
}

int warning_enabled (const std::string& id)
{
  octave::error_system& es = octave::__get_error_system__ ("warning_enabled");

  return es.warning_enabled (id);
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
  error_1 (std::cerr, nullptr, "", fmt, args);
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
  error_1 (std::cerr, nullptr, id, fmt, args);
}

void
parse_error_with_id (const char *id, const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  vparse_error_with_id (id, fmt, args);
  va_end (args);
}

void
rethrow_error (const char *id, const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  error_1 (std::cerr, nullptr, id, fmt, args);
  va_end (args);
}

OCTAVE_NORETURN
void
vpanic (const char *fmt, va_list args)
{
  octave::error_system& es = octave::__get_error_system__ ("vpanic");

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

void
defun_usage_message (const std::string& msg)
{
  defun_usage_message ("%s", msg.c_str ());
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

  octave_map err_stack = octave::init_error_stack (interp);

  if (err.contains ("stack"))
    err_stack = err.contents ("stack").xmap_value ("ERR.STACK must be a struct");

  octave::error_system& es = interp.get_error_system ();

  es.rethrow_error (id, msg, err_stack);

  return ovl ();
}

DEFUN (error, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {} error (@var{template}, @dots{})
@deftypefnx {} {} error (@var{id}, @var{template}, @dots{})
Display an error message and stop m-file execution.

Format the optional arguments under the control of the template string
@var{template} using the same rules as the @code{printf} family of
functions (@pxref{Formatted Output}) and print the resulting message
on the @code{stderr} stream.  The message is prefixed by the character
string @samp{error: }.

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
sequences in @var{template} (e.g., @qcode{"@xbackslashchar{}n"} =>
newline) are processed regardless of whether @var{template} has been defined
with single quotes, as long as there are two or more input arguments.  To
disable escape sequence expansion use a second backslash before the sequence
(e.g., @qcode{"@xbackslashchar{}@xbackslashchar{}n"}) or use the
@code{regexptranslate} function.
@seealso{warning, lasterror}
@end deftypefn */)
{

  int nargin = args.length ();

  if (nargin == 0)
    print_usage ();

  octave_value retval;

  octave_value_list nargs = args;

  std::string id;

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
            nargs(0) = c.string_value ();
        }

      if (m.contains ("identifier"))
        {
          octave_value c = m.getfield ("identifier");

          if (c.is_string ())
            id = c.string_value ();
        }

      // FIXME: also need to handle "stack" field in error structure,
      //        but that will require some more significant surgery on
      //        handle_message, error_with_id, etc.
    }
  else
    have_fmt = maybe_extract_message_id ("error", args, nargs, id);

  handle_message (error_with_id, id.c_str (), "unspecified error",
                  nargs, have_fmt);

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
and examples, see @ref{XREFerror,,error}.

The optional warning identifier @var{id} allows users to enable or disable
warnings tagged by this identifier.  A message identifier is a string of the
form @qcode{"NAMESPACE:WARNING-NAME"}.  Octave's own warnings use the
@qcode{"Octave"} namespace (@pxref{XREFwarning_ids,,warning_ids}).  For
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
@var{template} (e.g., @qcode{"@xbackslashchar{}n"} => newline) are processed
regardless of whether @var{template} has been defined with single quotes, as
long as there are two or more input arguments.  To disable escape sequence
expansion use a second backslash before the sequence (e.g.,
@qcode{"@xbackslashchar{}@xbackslashchar{}n"}) or use the
@code{regexptranslate} function.
@seealso{warning_ids, lastwarn, error}
@end deftypefn */)
{
  octave_value retval;

  int nargin = args.length ();
  bool done = false;

  octave::error_system& es = interp.get_error_system ();

  if (nargin > 0 && args.all_strings_p ())
    {
      string_vector argv = args.make_argv ("warning");

      std::string arg1 = argv[1];
      std::string arg2 = "all";

      if (nargin >= 2)
        arg2 = argv[2];

      if (arg1 == "on" || arg1 == "off" || arg1 == "error")
        {
          // Prepare output structure
          octave_map old_warning_options;
          if (arg2 == "all")
            old_warning_options = es.warning_options ();
          else
            old_warning_options = octave_map (es.warning_query (arg2));

          if (nargin == 3 && argv[3] == "local" && ! interp.at_top_level ())
            {
              octave_scalar_map val = es.warning_query (arg2);

              octave_value curr_state = val.contents ("state");

              // FIXME: this might be better with a dictionary object.

              octave::tree_evaluator& tw = interp.get_evaluator ();

              octave_value curr_warning_states
                = tw.get_auto_fcn_var (octave::stack_frame::SAVED_WARNING_STATES);

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

              tw.set_auto_fcn_var (octave::stack_frame::SAVED_WARNING_STATES, m);

              // Now ignore the "local" argument and continue to
              // handle the current setting.
              nargin--;
            }

          if (nargin >= 2 && arg2 == "all")
            {
              // If "all" is explicitly given as ID.

              octave_map tmp;
              int is_error = (arg1 == "error");

              Cell id (1, 1 + 2*is_error);
              Cell st (1, 1 + 2*is_error);

              id(0) = arg2;
              st(0) = arg1;

              // Since internal Octave functions are not compatible,
              // and "all"=="error" causes any "on" to throw an error,
              // turning all warnings into errors should disable
              // Octave:language-extension.

              if (is_error)
                {
                  id(1) = "Octave:language-extension";
                  st(1) = "off";

                  id(2) = "Octave:single-quote-string";
                  st(2) = "off";
                }

              tmp.assign ("identifier", id);
              tmp.assign ("state", st);

              es.warning_options (tmp);

              done = true;
            }
          else if (arg2 == "backtrace")
            {
              if (arg1 != "error")
                {
                  es.backtrace_on_warning (arg1 == "on");
                  done = true;
                }
            }
          else if (arg2 == "debug")
            {
              if (arg1 != "error")
                {
                  es.debug_on_warning (arg1 == "on");
                  done = true;
                }
            }
          else if (arg2 == "verbose")
            {
              if (arg1 != "error")
                {
                  es.verbose_warning (arg1 == "on");
                  done = true;
                }
            }
          else if (arg2 == "quiet")
            {
              if (arg1 != "error")
                {
                  es.quiet_warning (arg1 == "on");
                  done = true;
                }
            }
          else
            {
              if (arg2 == "last")
                arg2 = es.last_warning_id ();

              es.set_warning_option (arg1, arg2);

              done = true;
            }

          if (done && nargout > 0)
            retval = old_warning_options;
        }
      else if (arg1 == "query")
        {
          if (arg2 == "all")
            retval = es.warning_options ();
          else if (arg2 == "backtrace" || arg2 == "debug"
                   || arg2 == "verbose" || arg2 == "quiet")
            {
              octave_scalar_map tmp;
              tmp.assign ("identifier", arg2);
              if (arg2 == "backtrace")
                tmp.assign ("state", es.backtrace_on_warning () ? "on" : "off");
              else if (arg2 == "debug")
                tmp.assign ("state", es.debug_on_warning () ? "on" : "off");
              else if (arg2 == "verbose")
                tmp.assign ("state", es.verbose_warning () ? "on" : "off");
              else
                tmp.assign ("state", es.quiet_warning () ? "on" : "off");

              retval = tmp;
            }
          else
            retval = es.warning_query (arg2);

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
%!test <*45753>
%! warning ("error");
%! assert (! isempty (help ("warning")));
*/

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
*/

octave_value_list
set_warning_state (const std::string& id, const std::string& state)
{
  octave_value_list args;

  args(1) = id;
  args(0) = state;

  octave::interpreter& interp
    = octave::__get_interpreter__ ("set_warning_state");

  return Fwarning (interp, args, 1);
}

octave_value_list
set_warning_state (const octave_value_list& args)
{
  octave::interpreter& interp
    = octave::__get_interpreter__ ("set_warning_state");

  return Fwarning (interp, args, 1);
}

void
disable_warning (const std::string& id)
{
  octave::error_system& es = octave::__get_error_system__ ("disable_warning");

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

  octave::error_system& es = interp.get_error_system ();

  octave_scalar_map err;

  err.assign ("message", es.last_error_message ());
  err.assign ("identifier", es.last_error_id ());

  err.assign ("stack", octave_value (es.last_error_stack ()));

  if (nargin == 1)
    {
      octave::tree_evaluator& tw = interp.get_evaluator ();

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

  octave::error_system& es = interp.get_error_system ();

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

With two arguments, also set the last message identifier.
@seealso{warning, lasterror, lasterr}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin > 2)
    print_usage ();

  octave::error_system& es = interp.get_error_system ();

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
@deftypefnx {} {} beep_on_error (@var{new_val}, "local")
Query or set the internal variable that controls whether Octave will try
to ring the terminal bell before printing an error message.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@end deftypefn */)
{
  octave::error_system& es = interp.get_error_system ();

  return es.beep_on_error (args, nargout);
}

DEFMETHOD (debug_on_error, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} debug_on_error ()
@deftypefnx {} {@var{old_val} =} debug_on_error (@var{new_val})
@deftypefnx {} {} debug_on_error (@var{new_val}, "local")
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
  octave::error_system& es = interp.get_error_system ();

  return es.debug_on_error (args, nargout);
}

DEFMETHOD (debug_on_warning, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} debug_on_warning ()
@deftypefnx {} {@var{old_val} =} debug_on_warning (@var{new_val})
@deftypefnx {} {} debug_on_warning (@var{new_val}, "local")
Query or set the internal variable that controls whether Octave will try
to enter the debugger when a warning is encountered.

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@seealso{debug_on_error, debug_on_interrupt}
@end deftypefn */)
{
  octave::error_system& es = interp.get_error_system ();

  return es.debug_on_warning (args, nargout);
}

std::string
last_error_message (void)
{
  octave::error_system& es
    = octave::__get_error_system__ ("last_error_message");

  return es.last_error_message ();
}

std::string
last_error_id (void)
{
  octave::error_system& es
    = octave::__get_error_system__ ("last_error_id");

  return es.last_error_id ();
}

octave_map
last_error_stack (void)
{
  octave::error_system& es
    = octave::__get_error_system__ ("last_error_stack");

  return es.last_error_stack ();
}

std::string
last_warning_message (void)
{
  octave::error_system& es
    = octave::__get_error_system__ ("last_warning_message");

  return es.last_warning_message ();
}

std::string
last_warning_id (void)
{
  octave::error_system& es
    = octave::__get_error_system__ ("last_warning_id");

  return es.last_warning_id ();
}

void
interpreter_try (octave::unwind_protect& frame,
                 octave::error_system::try_option opt)
{
  octave::error_system& es
    = octave::__get_error_system__ ("interpreter_try");

  es.interpreter_try (frame, opt);
}

// Deprecated variables and functions.

// This variable is obsolete and always has the value 0.
int error_state = 0;

void
reset_error_handler (void)
{
  octave::error_system& es
    = octave::__get_error_system__ ("reset_error_handler");

  es.reset ();
}
