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

#include <iomanip>

#include "procstream.h"

OCTAVE_BEGIN_NAMESPACE(octave)

procstreambase::procstreambase (const std::string& command, int mode)
{
  pb_init ();

  if (! m_pb.open (command.c_str (), mode))
    std::ios::setstate (std::ios::badbit);
}

procstreambase::procstreambase (const char *command, int mode)
{
  pb_init ();

  if (! m_pb.open (command, mode))
    std::ios::setstate (std::ios::badbit);
}

void
procstreambase::open (const char *command, int mode)
{
  clear ();

  if (! m_pb.open (command, mode))
    std::ios::setstate (std::ios::badbit);
}

int
procstreambase::close (void)
{
  int status = 0;

  if (is_open ())
    {
      if (! m_pb.close ())
        std::ios::setstate (std::ios::failbit);

      status = m_pb.wait_status ();
    }

  return status;
}

OCTAVE_END_NAMESPACE(octave)
