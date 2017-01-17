/*

Copyright (C) 1996-2016 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if ! defined (octave_oct_strstrm_h)
#define octave_oct_strstrm_h 1

#include "octave-config.h"

#include <string>
#include <sstream>

#include "oct-stream.h"

class
octave_base_strstream : public octave::base_stream
{
public:

  octave_base_strstream (std::ios::openmode m = std::ios::out,
                         octave::mach_info::float_format ff
                           = octave::mach_info::native_float_format ())
    : octave::base_stream (m, ff) { }

  // No copying!

  octave_base_strstream (const octave_base_strstream&) = delete;

  octave_base_strstream& operator = (const octave_base_strstream&) = delete;

protected:

  ~octave_base_strstream (void) = default;

public:

  // Position a stream at OFFSET relative to ORIGIN.

  int seek (off_t, int);

  // Return current stream position.

  virtual off_t tell (void);

  // The name of the file.

  std::string name (void) const { return ""; }

  virtual std::streambuf *rdbuf (void) = 0;

  virtual bool bad (void) const = 0;

  virtual void clear (void) = 0;
};

class
octave_istrstream : public octave_base_strstream
{
public:

  octave_istrstream (const char *data,
                     std::ios::openmode arg_md = std::ios::out,
                     octave::mach_info::float_format ff
                       = octave::mach_info::native_float_format ())
    : octave_base_strstream (arg_md, ff), is (data) { }

  octave_istrstream (const std::string& data,
                     std::ios::openmode arg_md = std::ios::out,
                     octave::mach_info::float_format ff
                       = octave::mach_info::native_float_format ())
    : octave_base_strstream (arg_md, ff), is (data.c_str ()) { }

  // No copying!

  octave_istrstream (const octave_istrstream&) = delete;

  octave_istrstream& operator = (const octave_istrstream&) = delete;

protected:

  ~octave_istrstream (void) = default;

public:


  static octave::stream
  create (const char *data, std::ios::openmode arg_md = std::ios::out,
          octave::mach_info::float_format ff
            = octave::mach_info::native_float_format ());

  static octave::stream
  create (const std::string& data, std::ios::openmode arg_md = std::ios::out,
          octave::mach_info::float_format ff
            = octave::mach_info::native_float_format ());

  // Return nonzero if EOF has been reached on this stream.

  bool eof (void) const { return is.eof (); }

  std::istream *input_stream (void) { return &is; }

  std::ostream *output_stream (void) { return 0; }

  off_t tell (void) { return is.tellg (); }

  std::streambuf *rdbuf (void) { return is ? is.rdbuf () : 0; }

  bool bad (void) const { return is.bad (); }

  void clear (void) { is.clear (); }

private:

  std::istringstream is;
};

class
octave_ostrstream : public octave_base_strstream
{
public:

  octave_ostrstream (std::ios::openmode arg_md = std::ios::out,
                     octave::mach_info::float_format ff
                       = octave::mach_info::native_float_format ())
    : octave_base_strstream (arg_md, ff), os () { }

  // No copying!

  octave_ostrstream (const octave_ostrstream&) = delete;

  octave_ostrstream& operator = (const octave_ostrstream&) = delete;

protected:

  ~octave_ostrstream (void) = default;

public:

  static octave::stream
  create (std::ios::openmode arg_md = std::ios::out,
          octave::mach_info::float_format ff
            = octave::mach_info::native_float_format ());

  // Return nonzero if EOF has been reached on this stream.

  bool eof (void) const { return os.eof (); }

  std::istream *input_stream (void) { return 0; }

  std::ostream *output_stream (void) { return &os; }

  std::string str (void) { return os.str (); }

  std::streambuf *rdbuf (void) { return os ? os.rdbuf () : 0; }

  bool bad (void) const { return os.bad (); }

  void clear (void) { os.clear (); }

private:

  std::ostringstream os;
};

#endif

