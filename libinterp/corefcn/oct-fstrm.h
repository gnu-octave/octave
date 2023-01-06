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

#if ! defined (octave_oct_fstrm_h)
#define octave_oct_fstrm_h 1

#include "octave-config.h"

#include <fstream>
#include <string>

#include "oct-stream.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class
fstream : public base_stream
{
public:

  fstream (const std::string& nm_arg,
           std::ios::openmode arg_md = std::ios::in | std::ios::out,
           mach_info::float_format flt_fmt = mach_info::native_float_format ());

  // No copying!

  fstream (const fstream&) = delete;

  fstream& operator = (const fstream&) = delete;

  static stream
  create (const std::string& nm_arg,
          std::ios::openmode arg_md = std::ios::in | std::ios::out,
          mach_info::float_format flt_fmt = mach_info::native_float_format ());

  // Position a stream at OFFSET relative to ORIGIN.

  int seek (off_t offset, int origin);

  // Return current stream position.

  off_t tell (void);

  // Return nonzero if EOF has been reached on this stream.

  bool eof (void) const;

  void do_close (void);

  // The name of the file.

  std::string name (void) const { return m_name; }

  std::istream * input_stream (void);

  std::ostream * output_stream (void);

protected:

  ~fstream (void) = default;

private:

  std::string m_name;

  std::fstream m_fstream;
};

OCTAVE_END_NAMESPACE(octave)

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)

OCTAVE_DEPRECATED (7, "use 'octave::fstream' instead")
typedef octave::fstream octave_fstream;

#endif

#endif
