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

#if ! defined (octave_error_h)
#define octave_error_h 1

#include "octave-config.h"

#include <cstdarg>
#include <cinttypes>
#include <string>

#include "unwind-prot.h"

#include "oct-map.h"

class octave_value_list;

OCTAVE_BEGIN_NAMESPACE(octave)

class execution_exception;

class error_system
{
public:

  OCTINTERP_API error_system (interpreter& interp);

  error_system (const error_system&) = delete;

  error_system& operator = (const error_system&) = delete;

  ~error_system (void) = default;

  OCTINTERP_API octave_value
  debug_on_error (const octave_value_list& args, int nargout);

  void set_debug_on_error (bool flag) { m_debug_on_error = flag; }

  bool debug_on_error (void) const { return m_debug_on_error; }

  bool debug_on_error (bool flag)
  {
    bool val = m_debug_on_error;
    m_debug_on_error = flag;
    return val;
  }

  OCTINTERP_API octave_value
  debug_on_caught (const octave_value_list& args, int nargout);

  void set_debug_on_caught (bool flag) { m_debug_on_caught = flag; }

  bool debug_on_caught (void) const { return m_debug_on_caught; }

  bool debug_on_caught (bool flag)
  {
    bool val = m_debug_on_caught;
    m_debug_on_caught = flag;
    return val;
  }

  OCTINTERP_API octave_value
  debug_on_warning (const octave_value_list& args, int nargout);

  void set_debug_on_warning (bool flag) { m_debug_on_warning = flag; }

  bool debug_on_warning (void) const { return m_debug_on_warning; }

  bool debug_on_warning (bool flag)
  {
    bool val = m_debug_on_warning;
    m_debug_on_warning = flag;
    return val;
  }

  OCTINTERP_API octave_value
  discard_warning_messages (const octave_value_list& args, int nargout);

  void set_discard_warning_messages (bool flag)
  {
    m_discard_warning_messages = flag;
  }

  bool discard_warning_messages (void) const
  {
    return m_discard_warning_messages;
  }

  bool discard_warning_messages (bool flag)
  {
    bool val = m_discard_warning_messages;
    m_discard_warning_messages = flag;
    return val;
  }

  OCTINTERP_API octave_value
  beep_on_error (const octave_value_list& args, int nargout);

  void set_beep_on_error (bool flag) { m_beep_on_error = flag; }

  bool beep_on_error (void) const { return m_beep_on_error; }

  bool beep_on_error (bool flag)
  {
    bool val = m_beep_on_error;
    m_beep_on_error = flag;
    return val;
  }

  OCTINTERP_API octave_value
  backtrace_on_warning (const octave_value_list& args, int nargout);

  void set_backtrace_on_warning (bool flag) { m_backtrace_on_warning = flag; }

  bool backtrace_on_warning (void) const { return m_backtrace_on_warning; }

  bool backtrace_on_warning (bool flag)
  {
    bool val = m_backtrace_on_warning;
    m_backtrace_on_warning = flag;
    return val;
  }

  OCTINTERP_API octave_value
  verbose_warning (const octave_value_list& args, int nargout);

  void set_verbose_warning (bool flag) { m_verbose_warning = flag; }

  bool verbose_warning (void) const { return m_verbose_warning; }

  bool verbose_warning (bool flag)
  {
    bool val = m_verbose_warning;
    m_verbose_warning = flag;
    return val;
  }

  OCTINTERP_API octave_value
  quiet_warning (const octave_value_list& args, int nargout);

  void set_quiet_warning (bool flag) { m_quiet_warning = flag; }

  bool quiet_warning (void) const { return m_quiet_warning; }

  bool quiet_warning (bool flag)
  {
    bool val = m_quiet_warning;
    m_quiet_warning = flag;
    return val;
  }

  octave_map warning_options (void) const { return m_warning_options; }

  void set_warning_options (const octave_map& val)
  { m_warning_options = val; }

  octave_map warning_options (const octave_map& new_val)
  {
    octave_map val = m_warning_options;
    m_warning_options = new_val;
    return val;
  }

  OCTINTERP_API octave_value
  last_error_message (const octave_value_list& args, int nargout);

  void set_last_error_message (const std::string& val)
  { m_last_error_message = val; }

  std::string last_error_message (void) const { return m_last_error_message; }

  std::string last_error_message (const std::string& s)
  {
    std::string val = m_last_error_message;
    m_last_error_message = s;
    return val;
  }

  OCTINTERP_API octave_value
  last_warning_message (const octave_value_list& args, int nargout);

  void set_last_warning_message (const std::string& val)
  { m_last_warning_message = val; }

  std::string last_warning_message (void) const
  { return m_last_warning_message; }

  std::string last_warning_message (const std::string& s)
  {
    std::string val = m_last_warning_message;
    m_last_warning_message = s;
    return val;
  }

  OCTINTERP_API octave_value
  last_warning_id (const octave_value_list& args, int nargout);

  void set_last_warning_id (const std::string& val)
  { m_last_warning_id = val; }

  std::string last_warning_id (void) const { return m_last_warning_id; }

  std::string last_warning_id (const std::string& s)
  {
    std::string val = m_last_warning_id;
    m_last_warning_id = s;
    return val;
  }

  OCTINTERP_API octave_value
  last_error_id (const octave_value_list& args, int nargout);

  void set_last_error_id (const std::string& val) { m_last_error_id = val; }

  std::string last_error_id (void) const { return m_last_error_id; }

  std::string last_error_id (const std::string& s)
  {
    std::string val = m_last_error_id;
    m_last_error_id = s;
    return val;
  }

  void set_last_error_stack (const octave_map& val)
  {
    m_last_error_stack = val;
  }

  octave_map last_error_stack (void) const { return m_last_error_stack; }

  octave_map last_error_stack (const octave_map& new_val)
  {
    octave_map val = m_last_error_stack;
    m_last_error_stack = new_val;
    return val;
  }

  static OCTINTERP_API octave_map
  make_stack_map (const std::list<frame_info>& frames);

  static OCTINTERP_API std::list<frame_info>
  make_stack_frame_list (const octave_map& stack);

  //! For given warning ID, return 0 if warnings are disabled, 1 if
  //! enabled, and 2 if the given ID should be an error instead of a
  //! warning.

  OCTINTERP_API int warning_enabled (const std::string& id);

  OCTINTERP_API void
  verror (bool save_last_error, std::ostream& os, const char *name,
          const char *id, const char *fmt, va_list args,
          bool with_cfn = false);

  OCTINTERP_API void
  vwarning (const char *name, const char *id, const char *fmt,
            va_list args);

  OCTAVE_NORETURN
  OCTINTERP_API void
  error_1 (execution_exception& ee, const char *id, const char *fmt,
           va_list args);

  OCTAVE_NORETURN
  OCTINTERP_API void error_1 (const char *id, const char *fmt, va_list args);

  OCTAVE_NORETURN
  OCTINTERP_API void vusage (const char *id, const char *fmt, va_list args);

  OCTINTERP_API void vwarning (const char *id, const char *fmt, va_list args);

  OCTAVE_NORETURN
  OCTINTERP_API void
  rethrow_error (const std::string& id, const std::string& msg,
                 const octave_map& stack);

  OCTAVE_NORETURN
  OCTINTERP_API void vpanic (const char *fmt, va_list args);

  OCTAVE_NORETURN
  OCTINTERP_API void panic (const char *fmt, ...);

  OCTINTERP_API octave_scalar_map warning_query (const std::string& id_arg);

  OCTINTERP_API std::string default_warning_state (void);

  OCTINTERP_API void display_warning_options (std::ostream& os);

  OCTINTERP_API void
  set_warning_option (const std::string& state, const std::string& id);

  OCTINTERP_API void disable_warning (const std::string& id);

  OCTINTERP_API void initialize_default_warning_state (void);

  OCTINTERP_API void interpreter_try (unwind_protect& frame);

  // Throw execution_exception or, if debug_on_error is TRUE, enter
  // debugger.  If stack_info is empty, use current call stack.

  OCTAVE_NORETURN
  OCTINTERP_API void
  throw_error (const std::string& err_type,
               const std::string& id,
               const std::string& message,
               const std::list<frame_info>& stack_info
               = std::list<frame_info> ());

  OCTAVE_NORETURN
  OCTINTERP_API void throw_error (execution_exception& ee);

  OCTINTERP_API void save_exception (const execution_exception& ee);

  // FIXME
  //#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)
  OCTAVE_DEPRECATED (7, "second argument is no longer accepted")
  OCTINTERP_API void display_exception (const execution_exception& ee,
                                        std::ostream& os) const;
  //#endif

  OCTINTERP_API void display_exception (const execution_exception& ee) const;

private:

  interpreter& m_interpreter;

  //! TRUE means that Octave will try to enter the debugger when an error
  //! is encountered.  This will also inhibit printing of the normal
  //! traceback message (you will only see the top-level error message).

  bool m_debug_on_error;

  //! TRUE means that Octave will try to enter the debugger when an error
  //! is encountered within the 'try' section of a 'try' / 'catch' block.

  bool m_debug_on_caught;

  //! TRUE means that Octave will try to enter the debugger when a warning
  //! is encountered.

  bool m_debug_on_warning;

  //! TRUE means warning messages are turned off.

  bool m_discard_warning_messages;

  //! TRUE means that Octave will try to beep obnoxiously before
  //! printing error messages.
  bool m_beep_on_error;

  //! TRUE means that Octave will try to display a stack trace when a
  //! warning is encountered.
  bool m_backtrace_on_warning;

  //! TRUE means that Octave will print a verbose warning.  Currently
  //! unused.
  bool m_verbose_warning;

  //! TRUE means that Octave will print no warnings, but lastwarn will
  //! be updated
  bool m_quiet_warning;

  //! A structure containing (most of) the current state of warnings.
  octave_map m_warning_options;

  //! The text of the last error message.
  std::string m_last_error_message;

  //! The text of the last warning message.
  std::string m_last_warning_message;

  //! The last warning message id.
  std::string m_last_warning_id;

  //! The last error message id.
  std::string m_last_error_id;

  //! The last file in which an error occurred.
  octave_map m_last_error_stack;
};

OCTAVE_END_NAMESPACE(octave)

// FIXME: should we move the following functions inside the octave
// namespace?  If so, should the functions outside of the namespace be
// deprecated?  Doing that might cause a lot of trouble...  If they are
// not deprecated and eventually removed, does it make sense to also
// define them inside the octave namespace?

extern OCTINTERP_API void
vmessage (const char *name, const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (2, 3)
extern OCTINTERP_API void message (const char *name, const char *fmt, ...);

extern OCTINTERP_API void vwarning (const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (1, 2)
extern OCTINTERP_API void warning (const char *fmt, ...);

OCTAVE_NORETURN
extern OCTINTERP_API void verror (const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (1, 2)
OCTAVE_NORETURN
extern OCTINTERP_API void error (const char *fmt, ...);

OCTAVE_NORETURN
extern OCTINTERP_API void
verror (octave::execution_exception&, const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (2, 3)
OCTAVE_NORETURN
extern OCTINTERP_API void
error (octave::execution_exception&, const char *fmt, ...);

OCTAVE_NORETURN
extern OCTINTERP_API void
verror_with_cfn (const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (1, 2)
OCTAVE_NORETURN
extern OCTINTERP_API void
error_with_cfn (const char *fmt, ...);

OCTAVE_NORETURN
extern OCTINTERP_API void
vparse_error (const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (1, 2)
OCTAVE_NORETURN
extern OCTINTERP_API void
parse_error (const char *fmt, ...);

OCTAVE_NORETURN
extern OCTINTERP_API void
vusage_with_id (const char *id, const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (2, 3)
OCTAVE_NORETURN
extern OCTINTERP_API void
usage_with_id (const char *id, const char *fmt, ...);

extern OCTINTERP_API void
vwarning_with_id (const char *id, const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (2, 3)
extern OCTINTERP_API void
warning_with_id (const char *id, const char *fmt, ...);

OCTAVE_NORETURN
extern OCTINTERP_API void
verror_with_id (const char *id, const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (2, 3)
OCTAVE_NORETURN
extern OCTINTERP_API void
error_with_id (const char *id, const char *fmt, ...);

OCTAVE_NORETURN
extern OCTINTERP_API void
verror_with_id_cfn (const char *id, const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (2, 3)
OCTAVE_NORETURN
extern OCTINTERP_API void
error_with_id_cfn (const char *id, const char *fmt, ...);

OCTAVE_NORETURN
extern OCTINTERP_API void
vparse_error_with_id (const char *id, const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (2, 3)
OCTAVE_NORETURN
extern OCTINTERP_API void
parse_error_with_id (const char *id, const char *fmt, ...);

OCTAVE_NORETURN
extern OCTINTERP_API void vpanic (const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (1, 2)
OCTAVE_NORETURN
extern OCTINTERP_API void panic (const char *fmt, ...);

#define panic_impossible()                                              \
  panic ("impossible state reached in file '%s' at line %d", __FILE__, __LINE__)

inline void
panic_if (bool cond)
{
#ifndef NDEBUG
  if (cond)
    panic_impossible ();
  else
    return;

#else
  octave_unused_parameter (cond);
#endif
}

inline void
panic_unless (bool cond)
{
  panic_if (! cond);
}

#define error_impossible()                                              \
  error ("impossible state reached in file '%s' at line %d", __FILE__, __LINE__)

inline void
error_if (bool cond)
{
#ifndef NDEBUG
  if (cond)
    error_impossible ();
  else
    return;

#else
  octave_unused_parameter (cond);
#endif
}

inline void
error_unless (bool cond)
{
  error_if (! cond);
}

OCTAVE_BEGIN_NAMESPACE(octave)

//! Helper function for print_usage defined in defun.cc.

extern OCTINTERP_API void defun_usage_message (const std::string& msg);

// Convenience functions.

extern OCTINTERP_API octave_value_list
set_warning_state (const std::string& id, const std::string& state);

extern OCTINTERP_API octave_value_list
set_warning_state (const octave_value_list& args);

extern OCTINTERP_API int warning_enabled (const std::string& id);

extern OCTINTERP_API void disable_warning (const std::string& id);

extern OCTINTERP_API void interpreter_try (octave::unwind_protect&);

OCTAVE_END_NAMESPACE(octave)

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)
OCTAVE_DEPRECATED (7, "use 'octave::defun_usage_message' instead")
inline void defun_usage_message (const std::string& msg)
{
  octave::defun_usage_message (msg);
}

OCTAVE_DEPRECATED (7, "use 'octave::set_warning_state' instead")
inline octave_value_list
set_warning_state (const std::string& id, const std::string& state)
{
  return octave::set_warning_state (id, state);
}

OCTAVE_DEPRECATED (7, "use 'octave::set_warning_state' instead")
inline octave_value_list set_warning_state (const octave_value_list& args)
{
  return octave::set_warning_state (args);
}

OCTAVE_DEPRECATED (7, "use 'octave::warning_enabled' instead")
inline int warning_enabled (const std::string& id)
{
  return octave::warning_enabled (id);
}

OCTAVE_DEPRECATED (7, "use 'octave::disable_warning' instead")
inline void disable_warning (const std::string& id)
{
  octave::disable_warning (id);
}

OCTAVE_DEPRECATED (7, "use 'octave::interpreter_try' instead")
inline void interpreter_try (octave::unwind_protect& uwp)
{
  octave::interpreter_try (uwp);
}

#endif

#endif
