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

#if ! defined (octave_oct_stdstrm_h)
#define octave_oct_stdstrm_h 1

#include "octave-config.h"

#include <iomanip>

#include "oct-stream.h"
#include "c-file-ptr-stream.h"

OCTAVE_BEGIN_NAMESPACE(octave)

template <typename BUF_T, typename STREAM_T, typename FILE_T>
class
tstdiostream : public base_stream
{
public:

  tstdiostream (const std::string& n, FILE_T f = 0, int fid = 0,
                std::ios::openmode m = std::ios::in | std::ios::out,
                mach_info::float_format ff = mach_info::native_float_format (),
                const std::string& encoding = "utf-8",
                typename BUF_T::close_fcn cf = BUF_T::file_close)
    : base_stream (m, ff, encoding), m_name (n), m_mode (m),
      m_stream (f ? new STREAM_T (f, cf) : nullptr), m_fnum (fid)
  { }

  // No copying!

  tstdiostream (const tstdiostream&) = delete;

  tstdiostream& operator = (const tstdiostream&) = delete;

  // Position a stream at OFFSET relative to ORIGIN.

  int seek (off_t offset, int origin)
  {
    return m_stream ? m_stream->seek (offset, origin) : -1;
  }

  // Return current stream position.

  off_t tell (void) { return m_stream ? m_stream->tell () : -1; }

  // Return nonzero if EOF has been reached on this stream.

  bool eof (void) const { return m_stream ? m_stream->eof () : true; }

  // The name of the file.

  std::string name (void) const { return m_name; }

  std::istream * input_stream (void)
  {
    return (m_mode & std::ios::in) ? m_stream : nullptr;
  }

  std::ostream * output_stream (void)
  {
    return (m_mode & std::ios::out) ? m_stream : nullptr;
  }

  // FIXME: should not have to cast away const here.
  BUF_T * rdbuf (void) const
  {
    return m_stream ? (const_cast<STREAM_T *> (m_stream))->rdbuf () : nullptr;
  }

  int file_number (void) const { return m_fnum; }

  bool bad (void) const { return m_stream ? m_stream->bad () : true; }

  void clear (void)
  {
    if (m_stream)
      m_stream->clear ();
  }

  void do_close (void)
  {
    if (m_stream)
      m_stream->stream_close ();
  }

protected:

  ~tstdiostream (void) { delete m_stream; }

  //--------

  std::string m_name;

  std::ios::openmode m_mode;

  STREAM_T *m_stream;

  // The file number associated with this file.
  int m_fnum;
};

class
stdiostream
  : public tstdiostream<c_file_ptr_buf, io_c_file_ptr_stream, FILE *>
{
public:

  stdiostream (const std::string& n, FILE *f = nullptr,
               std::ios::openmode m = std::ios::in | std::ios::out,
               mach_info::float_format ff = mach_info::native_float_format (),
               const std::string& encoding = "utf-8",
               c_file_ptr_buf::close_fcn cf = c_file_ptr_buf::file_close)
    : tstdiostream<c_file_ptr_buf, io_c_file_ptr_stream, FILE *>
      (n, f, f ? fileno (f) : -1, m, ff, encoding, cf) { }

  static stream
  create (const std::string& n, FILE *f = nullptr,
          std::ios::openmode m = std::ios::in | std::ios::out,
          mach_info::float_format ff = mach_info::native_float_format (),
          const std::string& encoding = "utf-8",
          c_file_ptr_buf::close_fcn cf = c_file_ptr_buf::file_close)
  {
    return stream (new stdiostream (n, f, m, ff, encoding, cf));
  }

  // No copying!

  stdiostream (const stdiostream&) = delete;

  stdiostream& operator = (const stdiostream&) = delete;

protected:

  ~stdiostream (void) = default;
};

#if defined (HAVE_ZLIB)

class
zstdiostream
  : public tstdiostream<c_zfile_ptr_buf, io_c_zfile_ptr_stream, gzFile>
{
public:

  zstdiostream (const std::string& n, gzFile f = nullptr, int fid = 0,
                std::ios::openmode m = std::ios::in | std::ios::out,
                mach_info::float_format ff = mach_info::native_float_format (),
                const std::string& encoding = "utf-8",
                c_zfile_ptr_buf::close_fcn cf = c_zfile_ptr_buf::file_close)
    : tstdiostream<c_zfile_ptr_buf, io_c_zfile_ptr_stream, gzFile>
      (n, f, fid, m, ff, encoding, cf) { }

  static stream
  create (const std::string& n, gzFile f = nullptr, int fid = 0,
          std::ios::openmode m = std::ios::in | std::ios::out,
          mach_info::float_format ff = mach_info::native_float_format (),
          const std::string& encoding = "utf-8",
          c_zfile_ptr_buf::close_fcn cf = c_zfile_ptr_buf::file_close)
  {
    return stream (new zstdiostream (n, f, fid, m, ff, encoding, cf));
  }

  // No copying!

  zstdiostream (const zstdiostream&) = delete;

  zstdiostream& operator = (const zstdiostream&) = delete;

protected:

  ~zstdiostream (void) = default;
};

#endif

OCTAVE_END_NAMESPACE(octave)

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)

OCTAVE_DEPRECATED (7, "use 'octave::stdiostream' instead")
typedef octave::stdiostream octave_stdiostream;

OCTAVE_DEPRECATED (7, "use 'octave::zstdiostream' instead")
typedef octave::zstdiostream octave_zstdiostream;

#endif

#endif
