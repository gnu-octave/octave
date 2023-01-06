////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2000-2023 The Octave Project Developers
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

#if ! defined (octave_c_file_ptr_stream_h)
#define octave_c_file_ptr_stream_h 1

#include "octave-config.h"

#include <cstdio>
#include <istream>

#if defined (HAVE_ZLIB_H)
#  include <zlib.h>
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

class
c_file_ptr_buf : public std::streambuf
{
public:

  typedef std::streambuf::int_type int_type;

  typedef int (*close_fcn) (FILE *);

  FILE * stdiofile (void) { return m_f; }

  c_file_ptr_buf (FILE *f, close_fcn cf = file_close)
    : std::streambuf (), m_f (f), m_cf (cf)
  { }

  // No copying!

  c_file_ptr_buf (const c_file_ptr_buf&) = delete;

  c_file_ptr_buf& operator = (const c_file_ptr_buf&) = delete;

  ~c_file_ptr_buf (void);

  int_type overflow (int_type);

  int_type underflow (void) { return underflow_common (false); }

  int_type uflow (void) { return underflow_common (true); }

  int_type pbackfail (int_type);

  std::streamsize xsputn (const char *, std::streamsize);

  std::streamsize xsgetn (char *, std::streamsize);

  std::streampos seekoff (std::streamoff, std::ios::seekdir,
                          std::ios::openmode = std::ios::in | std::ios::out);

  std::streampos seekpos (std::streampos,
                          std::ios::openmode = std::ios::in | std::ios::out);

  int sync (void);

  int flush (void);

  int buf_close (void);

  int file_number () const { return m_f ? fileno (m_f) : -1; }

  int seek (off_t offset, int origin);

  off_t tell (void);

  void clear (void) { if (m_f) clearerr (m_f); }

  static int file_close (FILE *m_f);

protected:

  FILE *m_f;

  close_fcn m_cf;

private:

  int_type underflow_common (bool);
};

// FIXME: the following three classes could probably share some code...

template <typename STREAM_T, typename FILE_T, typename BUF_T>
class
c_file_ptr_stream : public STREAM_T
{
public:

  c_file_ptr_stream (FILE_T m_f,
                     typename BUF_T::close_fcn m_cf = BUF_T::file_close)
    : STREAM_T (nullptr), m_buf (new BUF_T (m_f, m_cf))
  { STREAM_T::init (m_buf); }

  // No copying!

  c_file_ptr_stream (const c_file_ptr_stream&) = delete;

  c_file_ptr_stream& operator = (const c_file_ptr_stream&) = delete;

  ~c_file_ptr_stream (void) { delete m_buf; m_buf = nullptr; }

  BUF_T * rdbuf (void) { return m_buf; }

  void stream_close (void) { if (m_buf) m_buf->buf_close (); }

  int seek (off_t offset, int origin)
  { return m_buf ? m_buf->seek (offset, origin) : -1; }

  off_t tell (void) { return m_buf ? m_buf->tell () : -1; }

  void clear (void) { if (m_buf) m_buf->clear (); STREAM_T::clear (); }

private:

  BUF_T *m_buf;
};

typedef c_file_ptr_stream<std::istream, FILE *, c_file_ptr_buf>
  i_c_file_ptr_stream;
typedef c_file_ptr_stream<std::ostream, FILE *, c_file_ptr_buf>
  o_c_file_ptr_stream;
typedef c_file_ptr_stream<std::iostream, FILE *, c_file_ptr_buf>
  io_c_file_ptr_stream;


#if defined (HAVE_ZLIB)

class
c_zfile_ptr_buf : public std::streambuf
{
public:

  typedef std::streambuf::int_type int_type;

  typedef int (*close_fcn) (gzFile);

  gzFile stdiofile (void) { return m_f; }

  c_zfile_ptr_buf (gzFile f, close_fcn cf = file_close)
    : std::streambuf (), m_f (f), m_cf (cf)
  { }

  // No copying!

  c_zfile_ptr_buf (const c_zfile_ptr_buf&) = delete;

  c_zfile_ptr_buf& operator = (const c_zfile_ptr_buf&) = delete;

  ~c_zfile_ptr_buf (void);

  int_type overflow (int_type);

  int_type underflow (void) { return underflow_common (false); }

  int_type uflow (void) { return underflow_common (true); }

  int_type pbackfail (int_type);

  std::streamsize xsputn (const char *, std::streamsize);

  std::streamsize xsgetn (char *, std::streamsize);

  std::streampos seekoff (std::streamoff, std::ios::seekdir,
                          std::ios::openmode = std::ios::in | std::ios::out);

  std::streampos seekpos (std::streampos,
                          std::ios::openmode = std::ios::in | std::ios::out);

  int sync (void);

  int flush (void);

  int buf_close (void);

  int file_number () const { return -1; }

  int seek (off_t offset, int origin)
  { return m_f ? gzseek (m_f, offset, origin) >= 0 : -1; }

  off_t tell (void) { return m_f ? gztell (m_f) : -1; }

  void clear (void) { if (m_f) gzclearerr (m_f); }

  static int file_close (gzFile m_f) { return ::gzclose (m_f); }

protected:

  gzFile m_f;

  close_fcn m_cf;

private:

  int_type underflow_common (bool);
};

typedef c_file_ptr_stream<std::istream, gzFile, c_zfile_ptr_buf>
  i_c_zfile_ptr_stream;
typedef c_file_ptr_stream<std::ostream, gzFile, c_zfile_ptr_buf>
  o_c_zfile_ptr_stream;
typedef c_file_ptr_stream<std::iostream, gzFile, c_zfile_ptr_buf>
  io_c_zfile_ptr_stream;

#endif

OCTAVE_END_NAMESPACE(octave)

#if defined (OCTAVE_PROVIDE_DEPRECATED_SYMBOLS)

OCTAVE_DEPRECATED (7, "use 'octave::i_c_file_ptr_stream' instead")
typedef octave::i_c_file_ptr_stream i_c_file_ptr_stream;

OCTAVE_DEPRECATED (7, "use 'octave::o_c_file_ptr_stream' instead")
typedef octave::o_c_file_ptr_stream o_c_file_ptr_stream;

OCTAVE_DEPRECATED (7, "use 'octave::io_c_file_ptr_stream' instead")
typedef octave::io_c_file_ptr_stream io_c_file_ptr_stream;

// FIXME: HAVE_ZLIB is not defined here because we do not (and should
// not) include config.h in this header file.  We need to find a way to
// define this interface without exposing gzFile.  Should this be a
// private header that can only be used if included after config.h in an
// Octave source file and not inside another header file?

#  if defined (HAVE_ZLIB)

OCTAVE_DEPRECATED (7, "use 'octave::i_c_zfile_ptr_stream' instead")
typedef octave::c_file_ptr_stream<std::istream, gzFile, octave::c_zfile_ptr_buf>
  i_c_zfile_ptr_stream;

OCTAVE_DEPRECATED (7, "use 'octave::o_c_zfile_ptr_stream' instead")
typedef octave::c_file_ptr_stream<std::ostream, gzFile, octave::c_zfile_ptr_buf>
  o_c_zfile_ptr_stream;

OCTAVE_DEPRECATED (7, "use 'octave::io_c_zfile_ptr_stream' instead")
typedef octave::c_file_ptr_stream<std::iostream, gzFile, octave::c_zfile_ptr_buf>
  io_c_zfile_ptr_stream;

#  endif

#endif

#endif
