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

#include "oct-strstrm.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Position a stream at OFFSET relative to ORIGIN.

int
base_strstream::seek (off_t, int)
{
  // Note: error is inherited from base_stream, not ::error.
  // This error function does not halt execution so "return ..." must exist.
  error ("fseek: invalid operation");
  return -1;
}

// Return current stream position.

off_t
base_strstream::tell (void)
{
  // Note: error is inherited from base_stream, not ::error.
  // This error function does not halt execution so "return ..." must exist.
  error ("ftell: invalid operation");
  return -1;
}

stream
istrstream::create (const char *data, std::ios::openmode arg_md,
                    mach_info::float_format flt_fmt,
                    const std::string& encoding)
{
  return stream (new istrstream (data, arg_md, flt_fmt, encoding));
}

stream
istrstream::create (const std::string& data, std::ios::openmode arg_md,
                    mach_info::float_format flt_fmt,
                    const std::string& encoding)
{
  return stream (new istrstream (data, arg_md, flt_fmt, encoding));
}

stream
ostrstream::create (std::ios::openmode arg_md,
                    mach_info::float_format flt_fmt,
                    const std::string& encoding)
{
  return stream (new ostrstream (arg_md, flt_fmt, encoding));
}

OCTAVE_END_NAMESPACE(octave)
