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

#if ! defined (octave_oct_iostrm_h)
#define octave_oct_iostrm_h 1

#include "octave-config.h"

#include <iosfwd>

#include "oct-stream.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class
base_iostream : public base_stream
{
public:

  base_iostream (const std::string& n = "",
                 std::ios::openmode m = std::ios::in | std::ios::out,
                 mach_info::float_format ff = mach_info::native_float_format ())
    : base_stream (m, ff), m_name (n) { }

  // No copying!

  base_iostream (const base_iostream&) = delete;

  base_iostream& operator = (const base_iostream&) = delete;

protected:

  ~base_iostream () = default;

public:

  // Position a stream at OFFSET relative to ORIGIN.

  int seek (off_t offset, int origin);

  // Return current stream position.

  off_t tell ();

  // Return nonzero if EOF has been reached on this stream.

  bool eof () const;

  // The name of the file.

  std::string name () const { return m_name; }

protected:

  void invalid_operation () const;

private:

  std::string m_name;

  virtual const char * stream_type () const = 0;
};

class
istream : public base_iostream
{
public:

  istream (std::istream *arg = nullptr, const std::string& n = "")
    : base_iostream (n, std::ios::in, mach_info::native_float_format ()),
      m_istream (arg)
  { }

  static stream
  create (std::istream *arg = nullptr, const std::string& n = "");

  // Return nonzero if EOF has been reached on this stream.

  bool eof () const;

  std::istream * input_stream () { return m_istream; }

  std::ostream * output_stream () { return nullptr; }

protected:

  ~istream () = default;

private:

  std::istream *m_istream;

  const char * stream_type () const { return "istream"; }

  // No copying!

  istream (const istream&) = delete;

  istream& operator = (const istream&) = delete;
};

class
ostream : public base_iostream
{
public:

  ostream (std::ostream *arg, const std::string& n = "")
    : base_iostream (n, std::ios::out, mach_info::native_float_format ()),
      m_ostream (arg)
  { }

  static stream
  create (std::ostream *arg, const std::string& n = "");

  // Return nonzero if EOF has been reached on this stream.

  bool eof () const;

  std::istream * input_stream () { return nullptr; }

  std::ostream * output_stream () { return m_ostream; }

protected:

  ~ostream () = default;

private:

  std::ostream *m_ostream;

  const char * stream_type () const { return "ostream"; }

  // No copying!

  ostream (const ostream&) = delete;

  ostream& operator = (const ostream&) = delete;
};

OCTAVE_END_NAMESPACE(octave)

#endif
