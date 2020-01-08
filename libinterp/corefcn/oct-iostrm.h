////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2020 The Octave Project Developers
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

class
octave_base_iostream : public octave::base_stream
{
public:

  octave_base_iostream (const std::string& n = "",
                        std::ios::openmode m = std::ios::in | std::ios::out,
                        octave::mach_info::float_format ff
                          = octave::mach_info::native_float_format ())
    : octave::base_stream (m, ff), m_name (n) { }

  // No copying!

  octave_base_iostream (const octave_base_iostream&) = delete;

  octave_base_iostream& operator = (const octave_base_iostream&) = delete;

protected:

  ~octave_base_iostream (void) = default;

public:

  // Position a stream at OFFSET relative to ORIGIN.

  int seek (off_t offset, int origin);

  // Return current stream position.

  off_t tell (void);

  // Return nonzero if EOF has been reached on this stream.

  bool eof (void) const;

  // The name of the file.

  std::string name (void) const { return m_name; }

protected:

  void invalid_operation (void) const;

private:

  std::string m_name;

  virtual const char * stream_type (void) const = 0;
};

class
octave_istream : public octave_base_iostream
{
public:

  octave_istream (std::istream *arg = nullptr, const std::string& n = "")
    : octave_base_iostream (n, std::ios::in,
                            octave::mach_info::native_float_format ()),
      m_istream (arg)
  { }

  static octave::stream
  create (std::istream *arg = nullptr, const std::string& n = "");

  // Return nonzero if EOF has been reached on this stream.

  bool eof (void) const;

  std::istream * input_stream (void) { return m_istream; }

  std::ostream * output_stream (void) { return nullptr; }

protected:

  ~octave_istream (void) = default;

private:

  std::istream *m_istream;

  const char * stream_type (void) const { return "octave_istream"; }

  // No copying!

  octave_istream (const octave_istream&) = delete;

  octave_istream& operator = (const octave_istream&) = delete;
};

class
octave_ostream : public octave_base_iostream
{
public:

  octave_ostream (std::ostream *arg, const std::string& n = "")
    : octave_base_iostream (n, std::ios::out,
                            octave::mach_info::native_float_format ()),
      m_ostream (arg)
  { }

  static octave::stream
  create (std::ostream *arg, const std::string& n = "");

  // Return nonzero if EOF has been reached on this stream.

  bool eof (void) const;

  std::istream * input_stream (void) { return nullptr; }

  std::ostream * output_stream (void) { return m_ostream; }

protected:

  ~octave_ostream (void) = default;

private:

  std::ostream *m_ostream;

  const char * stream_type (void) const { return "octave_ostream"; }

  // No copying!

  octave_ostream (const octave_ostream&) = delete;

  octave_ostream& operator = (const octave_ostream&) = delete;
};

#endif
