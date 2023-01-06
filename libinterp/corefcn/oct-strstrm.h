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

#if ! defined (octave_oct_strstrm_h)
#define octave_oct_strstrm_h 1

#include "octave-config.h"

#include <string>
#include <sstream>

#include "oct-stream.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class
base_strstream : public base_stream
{
public:

  base_strstream (std::ios::openmode m = std::ios::out,
                  mach_info::float_format ff
                  = mach_info::native_float_format (),
                  const std::string& encoding = "utf-8")
    : base_stream (m, ff, encoding) { }

  // No copying!

  base_strstream (const base_strstream&) = delete;

  base_strstream& operator = (const base_strstream&) = delete;

protected:

  ~base_strstream (void) = default;

public:

  // Position a stream at OFFSET relative to ORIGIN.

  int seek (off_t, int);

  // Return current stream position.

  virtual off_t tell (void);

  // The name of the file.

  std::string name (void) const { return ""; }

  virtual std::streambuf * rdbuf (void) = 0;

  virtual bool bad (void) const = 0;

  virtual void clear (void) = 0;
};

class
istrstream : public base_strstream
{
public:

  istrstream (const char *data,
              std::ios::openmode arg_md = std::ios::out,
              mach_info::float_format ff = mach_info::native_float_format (),
              const std::string& encoding = "utf-8")
    : base_strstream (arg_md, ff, encoding), m_istream (data) { }

  istrstream (const std::string& data,
              std::ios::openmode arg_md = std::ios::out,
              mach_info::float_format ff = mach_info::native_float_format (),
              const std::string& encoding = "utf-8")
    : base_strstream (arg_md, ff, encoding), m_istream (data) { }

  // No copying!

  istrstream (const istrstream&) = delete;

  istrstream& operator = (const istrstream&) = delete;

protected:

  ~istrstream (void) = default;

public:


  static stream
  create (const char *data, std::ios::openmode arg_md = std::ios::out,
          mach_info::float_format ff = mach_info::native_float_format (),
          const std::string& encoding = "utf-8");

  static stream
  create (const std::string& data, std::ios::openmode arg_md = std::ios::out,
          mach_info::float_format ff = mach_info::native_float_format (),
          const std::string& encoding = "utf-8");

  // Return nonzero if EOF has been reached on this stream.

  bool eof (void) const { return m_istream.eof (); }

  std::istream * input_stream (void) { return &m_istream; }

  std::ostream * output_stream (void) { return nullptr; }

  off_t tell (void) { return m_istream.tellg (); }

  std::streambuf * rdbuf (void)
  {
    return m_istream ? m_istream.rdbuf () : nullptr;
  }

  bool bad (void) const { return m_istream.bad (); }

  void clear (void) { m_istream.clear (); }

private:

  std::istringstream m_istream;
};

class
ostrstream : public base_strstream
{
public:

  ostrstream (std::ios::openmode arg_md = std::ios::out,
              mach_info::float_format ff = mach_info::native_float_format (),
              const std::string& encoding = "utf-8")
    : base_strstream (arg_md, ff, encoding), m_ostream () { }

  // No copying!

  ostrstream (const ostrstream&) = delete;

  ostrstream& operator = (const ostrstream&) = delete;

protected:

  ~ostrstream (void) = default;

public:

  static stream
  create (std::ios::openmode arg_md = std::ios::out,
          mach_info::float_format ff = mach_info::native_float_format (),
          const std::string& encoding = "utf-8");

  // Return nonzero if EOF has been reached on this stream.

  bool eof (void) const { return m_ostream.eof (); }

  std::istream * input_stream (void) { return nullptr; }

  std::ostream * output_stream (void) { return &m_ostream; }

  std::string str (void) { return m_ostream.str (); }

  std::streambuf * rdbuf (void)
  {
    return m_ostream ? m_ostream.rdbuf () : nullptr;
  }

  bool bad (void) const { return m_ostream.bad (); }

  void clear (void) { m_ostream.clear (); }

private:

  std::ostringstream m_ostream;
};

OCTAVE_END_NAMESPACE(octave)

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)

OCTAVE_DEPRECATED (7, "use 'octave::base_strstream' instead")
typedef octave::base_strstream octave_base_strstream;

OCTAVE_DEPRECATED (7, "use 'octave::istrstream' instead")
typedef octave::istrstream octave_istrstream;

OCTAVE_DEPRECATED (7, "use 'octave::ostrstream' instead")
typedef octave::ostrstream octave_ostrstream;

#endif

#endif
