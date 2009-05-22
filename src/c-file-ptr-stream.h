/*

Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2009
              John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_c_file_ptr_stream_h)
#define octave_c_file_ptr_stream_h 1

#include <cstdio>

#include <iosfwd>

class
c_file_ptr_buf : public std::streambuf
{
public:

#if !defined (CXX_ISO_COMPLIANT_LIBRARY)
  typedef int int_type;
#else
  typedef std::streambuf::int_type int_type;
#endif

  typedef int (*close_fcn) (FILE *);

  FILE* stdiofile (void) { return f; }

  c_file_ptr_buf (FILE *f_arg, close_fcn cf_arg = fclose)
    : std::streambuf (), f (f_arg), cf (cf_arg)
    { }

  ~c_file_ptr_buf (void);

  int_type overflow (int_type);

  int_type underflow (void) { return underflow_common (false); }

  int_type uflow (void) { return underflow_common (true); }

  int_type pbackfail (int_type);

  std::streamsize xsputn (const char*, std::streamsize);

  std::streamsize xsgetn (char *, std::streamsize);

  std::streampos seekoff (std::streamoff, std::ios::seekdir,
			  std::ios::openmode = std::ios::in | std::ios::out);
  
  std::streampos seekpos (std::streampos,
			  std::ios::openmode = std::ios::in | std::ios::out);

  int sync (void);

  int flush (void);

  int close (void);

  int file_number () const { return f ? fileno (f) : -1; }

  int seek (long offset, int origin)
    { return f ? fseek (f, offset, origin) : -1; }

  long tell (void) { return f ? ftell (f) : -1; }

  void clear (void) { if (f) clearerr (f); }

  static int fclose (FILE *f) { return ::fclose (f); }

protected:

  FILE *f;

  close_fcn cf;

private:

  int_type underflow_common (bool);
};

// FIXME -- the following three classes could probably share
// some code...

template <typename STREAM_T, typename FILE_T, typename BUF_T>
class
c_file_ptr_stream : public STREAM_T
{
public:

  c_file_ptr_stream (FILE_T f, typename BUF_T::close_fcn cf = BUF_T::fclose)
    : STREAM_T (0), buf (new BUF_T (f, cf)) { STREAM_T::init (buf); }

  ~c_file_ptr_stream (void) { delete buf; buf = 0; }

  BUF_T *rdbuf (void) { return buf; }

  void close (void) { if (buf) buf->close (); }

  int seek (long offset, int origin)
    { return buf ? buf->seek (offset, origin) : -1; }

  long tell (void) { return buf ? buf->tell () : -1; }

  void clear (void) { if (buf) buf->clear (); STREAM_T::clear (); }

private:

  BUF_T *buf;
};

typedef c_file_ptr_stream<std::istream, FILE *, c_file_ptr_buf> i_c_file_ptr_stream;
typedef c_file_ptr_stream<std::ostream, FILE *, c_file_ptr_buf> o_c_file_ptr_stream;
typedef c_file_ptr_stream<std::iostream, FILE *, c_file_ptr_buf> io_c_file_ptr_stream;

#ifdef HAVE_ZLIB

#ifdef HAVE_ZLIB_H
#include <zlib.h>
#endif

class
c_zfile_ptr_buf : public std::streambuf
{
public:

#if !defined (CXX_ISO_COMPLIANT_LIBRARY)
  typedef int int_type;
#else
  typedef std::streambuf::int_type int_type;
#endif

  typedef int (*close_fcn) (gzFile);

  gzFile stdiofile (void) { return f; }

  c_zfile_ptr_buf (gzFile f_arg, close_fcn cf_arg = fclose)
    : std::streambuf (), f (f_arg), cf (cf_arg)
    { }

  ~c_zfile_ptr_buf (void);

  int_type overflow (int_type);

  int_type underflow (void) { return underflow_common (false); }

  int_type uflow (void) { return underflow_common (true); }

  int_type pbackfail (int_type);

  std::streamsize xsputn (const char*, std::streamsize);

  std::streamsize xsgetn (char *, std::streamsize);

  std::streampos seekoff (std::streamoff, std::ios::seekdir,
			  std::ios::openmode = std::ios::in | std::ios::out);
  
  std::streampos seekpos (std::streampos,
			  std::ios::openmode = std::ios::in | std::ios::out);

  int sync (void);

  int flush (void);

  int close (void);

  int file_number () const { return -1; }

  int seek (long offset, int origin)
    { return f ? gzseek (f, offset, origin) : -1; }

  long tell (void) { return f ? gztell (f) : -1; }

  void clear (void) { if (f) gzclearerr (f); }

  static int fclose (gzFile f) { return ::gzclose (f); }

protected:

  gzFile f;

  close_fcn cf;

private:

  int_type underflow_common (bool);
};

typedef c_file_ptr_stream<std::istream, gzFile, c_zfile_ptr_buf> i_c_zfile_ptr_stream;
typedef c_file_ptr_stream<std::ostream, gzFile, c_zfile_ptr_buf> o_c_zfile_ptr_stream;
typedef c_file_ptr_stream<std::iostream, gzFile, c_zfile_ptr_buf> io_c_zfile_ptr_stream;

#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
