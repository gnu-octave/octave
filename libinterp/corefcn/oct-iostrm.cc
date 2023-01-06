////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
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

#include <istream>
#include <ostream>

#include "error.h"
#include "oct-iostrm.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Position a stream at OFFSET relative to ORIGIN.

int
base_iostream::seek (off_t, int)
{
  invalid_operation ();
  return -1;
}

// Return current stream position.

off_t
base_iostream::tell (void)
{
  invalid_operation ();
  return -1;
}

// Return nonzero if EOF has been reached on this stream.

bool
base_iostream::eof (void) const
{
  invalid_operation ();
  return false;
}

void
base_iostream::invalid_operation (void) const
{
  // Note: use ::error to get error from error.h which halts operation.
  ::error ("%s: invalid operation", stream_type ());
}

// Return nonzero if EOF has been reached on this stream.

bool
istream::eof (void) const
{
  return m_istream && m_istream->eof ();
}

octave::stream
istream::create (std::istream *arg, const std::string& n)
{
  return octave::stream (new istream (arg, n));
}

// Return nonzero if EOF has been reached on this stream.

bool
ostream::eof (void) const
{
  return m_ostream && m_ostream->eof ();
}

octave::stream
ostream::create (std::ostream *arg, const std::string& n)
{
  return octave::stream (new ostream (arg, n));
}

OCTAVE_END_NAMESPACE(octave)
