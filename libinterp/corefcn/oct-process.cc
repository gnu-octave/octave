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

#include <iostream>
#include <sstream>

#include "oct-process.h"
#include "procstream.h"
#include "sysdep.h"
#include "oct-syscalls.h"

OCTAVE_BEGIN_NAMESPACE(octave)

process_execution_result
process_execution_result::of_success (int exit_status,
                                      const std::string& stdout_output)
{
  return process_execution_result (0, exit_status, stdout_output, "");
}

process_execution_result
process_execution_result::of_error (int status, const std::string& err_msg)
{
  return process_execution_result (status, -1, "", err_msg);
}

// Execute a shell command, returning results as a C++ object
process_execution_result
run_command_and_return_output (const std::string& cmd_str)
{
  iprocstream cmd (cmd_str.c_str ());

  if (! cmd)
    {
      std::string msg = "unable to start subprocess for '" + cmd_str + "'";

      return process_execution_result::of_error (-1, msg);
    }

  std::ostringstream output_buf;

  char ch;

  for (;;)
    {
      if (cmd.get (ch))
        output_buf.put (ch);
      else
        {
          if (! cmd.eof () && errno == EAGAIN)
            cmd.clear ();
          else
            break;
        }
    }

  int cmd_status = cmd.close ();

  if (sys::wifexited (cmd_status))
    cmd_status = sys::wexitstatus (cmd_status);
  else
    cmd_status = 127;

  return process_execution_result::of_success (cmd_status, output_buf.str ());
}

OCTAVE_END_NAMESPACE(octave)
