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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstring>

#include <ostream>
#include <sstream>
#include <new>

#include "quit.h"

sig_atomic_t octave_interrupt_state = 0;

volatile sig_atomic_t octave_signal_caught = 0;

void (*octave_signal_hook) (void) = nullptr;
void (*octave_interrupt_hook) (void) = nullptr;

OCTAVE_BEGIN_NAMESPACE(octave)

std::string execution_exception::stack_trace (void) const
{
  std::size_t nframes = m_stack_info.size ();

  if (nframes == 0)
    return std::string ();

  std::ostringstream buf;

  buf << "error: called from\n";

  for (const auto& frm : m_stack_info)
    {
      buf << "    " << frm.fcn_name ();

      int line = frm.line ();

      if (line > 0)
        {
          buf << " at line " << line;

          int column = frm.column ();

          if (column > 0)
            buf << " column " << column;
        }

      buf << "\n";
    }

  return buf.str ();
}

void execution_exception::display (std::ostream& os) const
{
  if (! m_message.empty ())
    {
      os << m_err_type << ": " << m_message;

      if (m_message.back () != '\n')
        {
          os << "\n";

          std::string st = stack_trace ();

          if (! st.empty ())
            os << st;
        }
    }
}

OCTAVE_END_NAMESPACE(octave)

void
octave_handle_signal (void)
{
  if (octave_signal_hook)
    octave_signal_hook ();

  if (octave_interrupt_state > 0)
    {
      octave_interrupt_state = -1;

      throw octave::interrupt_exception ();
    }
}
