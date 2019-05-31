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

#if ! defined (octave_error_h)
#define octave_error_h 1

#include "octave-config.h"

#include <cstdarg>
#include <cinttypes>
#include <string>

#include "unwind-prot.h"

#include "oct-map.h"

class octave_value_list;
namespace octave
{
  class execution_exception;
}

#define panic_impossible()                                              \
  panic ("impossible state reached in file '%s' at line %d", __FILE__, __LINE__)

OCTAVE_DEPRECATED (6, "use 'error_system::reset' instead")
extern OCTINTERP_API void reset_error_handler (void);

extern OCTINTERP_API int warning_enabled (const std::string& id);

extern OCTINTERP_API octave::execution_exception
make_execution_exception (const char *who);

extern OCTINTERP_API void
vmessage (const char *name, const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (2, 3)
extern OCTINTERP_API void message (const char *name, const char *fmt, ...);

extern OCTINTERP_API void vwarning (const char *fmt, va_list args);
OCTAVE_FORMAT_PRINTF (1, 2)
extern OCTINTERP_API void warning (const char *fmt, ...);

OCTAVE_NORETURN OCTINTERP_API extern
void verror (const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (1, 2)
OCTAVE_NORETURN OCTINTERP_API extern
void error (const char *fmt, ...);

OCTAVE_NORETURN OCTINTERP_API extern
void verror (octave::execution_exception&, const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (2, 3)
OCTAVE_NORETURN OCTINTERP_API extern
void error (octave::execution_exception&, const char *fmt, ...);

OCTAVE_NORETURN OCTINTERP_API extern
void verror_with_cfn (const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (1, 2)
OCTAVE_NORETURN OCTINTERP_API extern
void error_with_cfn (const char *fmt, ...);

OCTAVE_NORETURN OCTINTERP_API extern
void vparse_error (const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (1, 2)
OCTAVE_NORETURN OCTINTERP_API extern
void parse_error (const char *fmt, ...);

extern OCTINTERP_API void
vmessage_with_id (const char *id, const char *name,
                  const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (3, 4)
extern OCTINTERP_API void
message_with_id (const char *id, const char *name, const char *fmt, ...);

OCTAVE_NORETURN OCTINTERP_API extern
void vusage_with_id (const char *id, const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (2, 3)
OCTAVE_NORETURN OCTINTERP_API extern
void usage_with_id (const char *id, const char *fmt, ...);

extern OCTINTERP_API void
vwarning_with_id (const char *id, const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (2, 3)
extern OCTINTERP_API void
warning_with_id (const char *id, const char *fmt, ...);

OCTAVE_NORETURN OCTINTERP_API extern
void verror_with_id (const char *id, const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (2, 3)
OCTAVE_NORETURN OCTINTERP_API extern
void error_with_id (const char *id, const char *fmt, ...);

OCTAVE_NORETURN OCTINTERP_API extern
void verror_with_id_cfn (const char *id, const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (2, 3)
OCTAVE_NORETURN OCTINTERP_API extern
void error_with_id_cfn (const char *id, const char *fmt, ...);

OCTAVE_NORETURN OCTINTERP_API extern
void vparse_error_with_id (const char *id, const char *fmt, va_list args);

OCTAVE_FORMAT_PRINTF (2, 3)
OCTAVE_NORETURN OCTINTERP_API extern
void parse_error_with_id (const char *id, const char *fmt, ...);

OCTAVE_FORMAT_PRINTF (1, 2)
OCTAVE_NORETURN OCTINTERP_API extern
void panic (const char *fmt, ...);

//! Helper function for print_usage defined in defun.cc.

extern OCTINTERP_API void defun_usage_message (const std::string& msg);

extern OCTINTERP_API octave_value_list
set_warning_state (const std::string& id, const std::string& state);

extern OCTINTERP_API octave_value_list
set_warning_state (const octave_value_list& args);

extern OCTINTERP_API void disable_warning (const std::string& id);
extern OCTINTERP_API void initialize_default_warning_state (void);

OCTAVE_DEPRECATED (6, "this variable is obsolete and always has the value 0")
extern OCTINTERP_API int error_state;

extern OCTINTERP_API void interpreter_try (octave::unwind_protect&);

namespace octave
{
  class error_system
  {
  public:

    error_system (interpreter& interp);

    error_system (const error_system&) = delete;

    error_system& operator = (const error_system&) = delete;

    ~error_system (void) = default;

    void reset (void)
    {
      m_buffer_error_messages = 0;
      m_in_try_catch = 0;
      m_discard_error_messages = false;
    }

    octave_value debug_on_error (const octave_value_list& args, int nargout);

    void set_debug_on_error (bool flag) { m_debug_on_error = flag; }

    bool debug_on_error (void) const { return m_debug_on_error; }

    bool debug_on_error (bool flag)
    {
      bool val = m_debug_on_error;
      m_debug_on_error = flag;
      return val;
    }

    octave_value debug_on_caught (const octave_value_list& args, int nargout);

    void set_debug_on_caught (bool flag) { m_debug_on_caught = flag; }

    bool debug_on_caught (void) const { return m_debug_on_caught; }

    bool debug_on_caught (bool flag)
    {
      bool val = m_debug_on_caught;
      m_debug_on_caught = flag;
      return val;
    }

    octave_value debug_on_warning (const octave_value_list& args, int nargout);

    void set_debug_on_warning (bool flag) { m_debug_on_warning = flag; }

    bool debug_on_warning (void) const { return m_debug_on_warning; }

    bool debug_on_warning (bool flag)
    {
      bool val = m_debug_on_warning;
      m_debug_on_warning = flag;
      return val;
    }

    octave_value buffer_error_messages (const octave_value_list& args, int nargout);

    void set_buffer_error_messages (int val) { m_buffer_error_messages = val; }

    int buffer_error_messages (void) const { return m_buffer_error_messages; }

    int buffer_error_messages (int new_val)
    {
      int val = m_buffer_error_messages;
      m_buffer_error_messages = new_val;
      return val;
    }

    octave_value in_try_catch (const octave_value_list& args, int nargout);

    void set_in_try_catch (int val) { m_in_try_catch = val; }

    int in_try_catch (void) const { return m_in_try_catch; }

    int in_try_catch (int new_val)
    {
      int val = m_in_try_catch;
      m_in_try_catch = new_val;
      return val;
    }

    octave_value discard_error_messages (const octave_value_list& args, int nargout);

    void set_discard_error_messages (bool flag) { m_discard_error_messages = flag; }

    bool discard_error_messages (void) const { return m_discard_error_messages; }

    bool discard_error_messages (bool flag)
    {
      bool val = m_discard_error_messages;
      m_discard_error_messages = flag;
      return val;
    }

    octave_value discard_warning_messages (const octave_value_list& args, int nargout);

    void set_discard_warning_messages (bool flag) { m_discard_warning_messages = flag; }

    bool discard_warning_messages (void) const { return m_discard_warning_messages; }

    bool discard_warning_messages (bool flag)
    {
      bool val = m_discard_warning_messages;
      m_discard_warning_messages = flag;
      return val;
    }

    octave_value beep_on_error (const octave_value_list& args, int nargout);

    void set_beep_on_error (bool flag) { m_beep_on_error = flag; }

    bool beep_on_error (void) const { return m_beep_on_error; }

    bool beep_on_error (bool flag)
    {
      bool val = m_beep_on_error;
      m_beep_on_error = flag;
      return val;
    }

    octave_value backtrace_on_warning (const octave_value_list& args, int nargout);

    void set_backtrace_on_warning (bool flag) { m_backtrace_on_warning = flag; }

    bool backtrace_on_warning (void) const { return m_backtrace_on_warning; }

    bool backtrace_on_warning (bool flag)
    {
      bool val = m_backtrace_on_warning;
      m_backtrace_on_warning = flag;
      return val;
    }

    octave_value verbose_warning (const octave_value_list& args, int nargout);

    void set_verbose_warning (bool flag) { m_verbose_warning = flag; }

    bool verbose_warning (void) const { return m_verbose_warning; }

    bool verbose_warning (bool flag)
    {
      bool val = m_verbose_warning;
      m_verbose_warning = flag;
      return val;
    }

    octave_value quiet_warning (const octave_value_list& args, int nargout);

    void set_quiet_warning (bool flag) { m_quiet_warning = flag; }

    bool quiet_warning (void) const { return m_quiet_warning; }

    bool quiet_warning (bool flag)
    {
      bool val = m_quiet_warning;
      m_quiet_warning = flag;
      return val;
    }

    octave_map warning_options (void) const { return m_warning_options; }

    void set_warning_options (const octave_map& val) { m_warning_options = val; }

    octave_map warning_options (const octave_map& new_val)
    {
      octave_map val = m_warning_options;
      m_warning_options = new_val;
      return val;
    }

    octave_value last_error_message (const octave_value_list& args, int nargout);

    void set_last_error_message (const std::string& val) { m_last_error_message = val; }

    std::string last_error_message (void) const { return m_last_error_message; }

    std::string last_error_message (const std::string& s)
    {
      std::string val = m_last_error_message;
      m_last_error_message = s;
      return val;
    }

    octave_value last_warning_message (const octave_value_list& args, int nargout);

    void set_last_warning_message (const std::string& val) { m_last_warning_message = val; }

    std::string last_warning_message (void) const { return m_last_warning_message; }

    std::string last_warning_message (const std::string& s)
    {
      std::string val = m_last_warning_message;
      m_last_warning_message = s;
      return val;
    }

    octave_value last_warning_id (const octave_value_list& args, int nargout);

    void set_last_warning_id (const std::string& val) { m_last_warning_id = val; }

    std::string last_warning_id (void) const { return m_last_warning_id; }

    std::string last_warning_id (const std::string& s)
    {
      std::string val = m_last_warning_id;
      m_last_warning_id = s;
      return val;
    }

    octave_value last_error_id (const octave_value_list& args, int nargout);

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

    //! Tell the error handler whether to print messages, or just store
    //! them for later.  Used for handling errors in eval() and
    //! the 'unwind_protect' statement.

    int m_buffer_error_messages;

    //! The number of layers of try / catch blocks we're in.  Used to print
    //! "caught error" instead of "error" when "dbstop if caught error" is on.

    int m_in_try_catch;

    //! TRUE means error messages are turned off.

    bool m_discard_error_messages;

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
}

#endif
