////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2019-2023 The Octave Project Developers
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

#if ! defined (oct_process_h)
#define oct_process_h 1

#include "octave-config.h"

#include <string>

OCTAVE_BEGIN_NAMESPACE(octave)

class
process_execution_result
{
public:

  process_execution_result (void)
    : m_status (-1), m_err_msg (), m_exit_status (-1), m_stdout_output ()
  { }

  process_execution_result (int status, int exit_status,
                            const std::string& stdout_output,
                            const std::string& err_msg)
    : m_status (status), m_err_msg (err_msg), m_exit_status (exit_status),
      m_stdout_output (stdout_output)
  { }

  static OCTINTERP_API process_execution_result
  of_success (int exit_status, const std::string& stdout_output);

  static OCTINTERP_API process_execution_result
  of_error (int status, const std::string& err_msg);

  int status (void) const { return m_status; }

  int exit_status (void) const { return m_exit_status; }

  std::string err_msg (void) const { return m_err_msg; }

  std::string stdout_output (void) const { return m_stdout_output; }

private:

  // Launch status of the process, 0 for success, nonzero for error.
  int m_status;

  // Error message if executing command failed.
  std::string m_err_msg;

  // Exit status of the process.
  int m_exit_status;

  // Collected stdout output of the process.
  std::string m_stdout_output;
};

extern OCTINTERP_API process_execution_result
run_command_and_return_output (const std::string& cmd_str);

OCTAVE_END_NAMESPACE(octave)

#endif
