////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2002-2023 The Octave Project Developers
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

#if ! defined (octave_quit_h)
#define octave_quit_h 1

#include "octave-config.h"

/* The signal header is just needed for the sig_atomic_t type.  */
#if defined (__cplusplus)
#  include <csignal>
#  include <iosfwd>
#  include <list>
#  include <stdexcept>
#  include <string>
extern "C" {
#else
#  include <signal.h>
#endif

#if defined (__cplusplus)

OCTAVE_BEGIN_NAMESPACE(octave)

class frame_info
{
public:

  frame_info (void) = default;

  frame_info (const std::string& file_name, const std::string& fcn_name,
              int line, int column)
    : m_file_name (file_name), m_fcn_name (fcn_name), m_line (line),
      m_column (column)
  { }

  frame_info (const frame_info&) = default;

  frame_info& operator = (const frame_info&) = default;

  ~frame_info (void) = default;

  std::string file_name (void) const { return m_file_name; }

  std::string fcn_name (void) const { return m_fcn_name; }

  int line (void) const { return m_line; }

  int column (void) const { return m_column; }

private:

  std::string m_file_name;

  std::string m_fcn_name;

  int m_line;

  int m_column;
};

inline bool operator == (const frame_info& a, const frame_info& b)
{
  return (a.file_name () == b.file_name ()
          && a.fcn_name () == b.fcn_name ()
          && a.line () == b.line ()
          && a.column () == b.column ());
}

class OCTAVE_EXCEPTION_API execution_exception : public std::runtime_error
{
public:

  typedef std::list<frame_info> stack_info_type;

  execution_exception (const std::string& err_type = "error",
                       const std::string& id = "",
                       const std::string& message = "unspecified error",
                       const stack_info_type& stack_info = stack_info_type ())
    : runtime_error (message), m_err_type (err_type), m_id (id),
      m_message (message), m_stack_info (stack_info)
  { }

  execution_exception (const execution_exception&) = default;

  execution_exception& operator = (const execution_exception&) = default;

  ~execution_exception (void) = default;

  void set_err_type (const std::string& et)
  {
    m_err_type = et;
  }

  std::string err_type (void) const { return m_err_type; }

  virtual std::string stack_trace (void) const;

  void set_identifier (const std::string& id)
  {
    m_id = id;
  }

  virtual std::string identifier (void) const { return m_id; }

  void set_message (const std::string& msg)
  {
    m_message = msg;
  }

  std::string message (void) const { return m_message; }

  // Provided for std::exception interface.
  const char * what (void) const noexcept { return m_message.c_str (); }

  virtual stack_info_type stack_info (void) const
  {
    return m_stack_info;
  }

  void set_stack_info (const stack_info_type& stack_info)
  {
    m_stack_info = stack_info;
  }

  virtual void display (std::ostream& os) const;

private:

  std::string m_err_type;

  std::string m_id;

  std::string m_message;

  stack_info_type m_stack_info;
};

class OCTAVE_EXCEPTION_API exit_exception : public std::exception
{
public:

  exit_exception (int exit_status = 0, bool safe_to_return = false)
    : std::exception (), m_exit_status (exit_status),
      m_safe_to_return (safe_to_return)
  { }

  exit_exception (const exit_exception&) = default;

  exit_exception& operator = (exit_exception&) = default;

  ~exit_exception (void) = default;

  const char * what (void) const noexcept { return "exit exception"; }

  int exit_status (void) const { return m_exit_status; }

  bool safe_to_return (void) const { return m_safe_to_return; }

private:

  int m_exit_status;

  bool m_safe_to_return;
};

class interrupt_exception : public std::exception
{
public:

  interrupt_exception (void) = default;

  interrupt_exception (const interrupt_exception&) = default;

  interrupt_exception& operator = (const interrupt_exception&) = default;

  ~interrupt_exception (void) = default;

  const char * what (void) const noexcept { return "interrupt exception"; }
};

OCTAVE_END_NAMESPACE(octave)

#endif

// The following enum values are deprecated and will eventually be
// removed from Octave, but there seems to be no universally good way
// to tag them with an attribute that will generate a warning.

enum
octave_exception
{
  octave_no_exception = 0,
  octave_exec_exception = 1,
  octave_alloc_exception = 3,
  octave_quit_exception = 4
};

/*
  > 0: interrupt pending
    0: no interrupt pending
  < 0: handling interrupt
*/
extern OCTAVE_API sig_atomic_t octave_interrupt_state;

extern OCTAVE_API volatile sig_atomic_t octave_signal_caught;

extern OCTAVE_API void octave_handle_signal (void);

#if defined (__cplusplus)

inline void octave_quit (void)
{
  if (octave_signal_caught)
    {
      octave_signal_caught = 0;
      octave_handle_signal ();
    }
}

#define OCTAVE_QUIT octave_quit ()

#else

#define OCTAVE_QUIT                             \
  do                                            \
    {                                           \
      if (octave_signal_caught)                 \
        {                                       \
          octave_signal_caught = 0;             \
          octave_handle_signal ();              \
        }                                       \
    }                                           \
  while (0)
#endif

/* The following macros are obsolete.  Interrupting immediately by
   calling siglongjmp or similar from a signal handler is asking for
   trouble.  Rather than remove them, however, please leave them in
   place so that old code that uses them will continue to compile.  They
   are defined to create a dummy do-while block to match the previous
   definitions.  */

#define BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE     \
  do                                                    \
    {

#define END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE       \
    }                                                   \
  while (0)

#if defined (__cplusplus)

/* Likewise, these are obsolete.  They are defined to create a
   dummy scope to match the previous versions that created a try-catch
   block.  */

#define BEGIN_INTERRUPT_WITH_EXCEPTIONS         \
  {

#define END_INTERRUPT_WITH_EXCEPTIONS           \
  }

#endif

#if defined (__cplusplus)
}

/* These should only be declared for C++ code, and should also be
   outside of any extern "C" block.  */

extern OCTAVE_API void (*octave_signal_hook) (void);
extern OCTAVE_API void (*octave_interrupt_hook) (void);

#endif

#endif
