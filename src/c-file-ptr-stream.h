/*

Copyright (C) 2000 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_c_file_ptr_stream_h)
#define octave_c_file_ptr_stream_h 1

#include <iostream>
#include <cstdio>

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

class
i_c_file_ptr_stream : public std::istream
{
public:

  i_c_file_ptr_stream (FILE* f,
		       c_file_ptr_buf::close_fcn cf = c_file_ptr_buf::fclose)
    : std::istream (0), buf (new c_file_ptr_buf (f, cf)) { init (buf); }

  ~i_c_file_ptr_stream (void) { delete buf; buf = 0; }

  c_file_ptr_buf *rdbuf (void) { return buf; }

  void close (void) { if (buf) buf->close (); }

  int seek (long offset, int origin)
    { return buf ? buf->seek (offset, origin) : -1; }

  long tell (void) { return buf ? buf->tell () : -1; }

  void clear (void) { if (buf) buf->clear (); std::istream::clear (); }

private:

  c_file_ptr_buf *buf;
};

class
o_c_file_ptr_stream : public std::ostream
{
public:

  o_c_file_ptr_stream (FILE* f,
		       c_file_ptr_buf::close_fcn cf = c_file_ptr_buf::fclose)
    : std::ostream (0), buf (new c_file_ptr_buf (f, cf)) { init (buf); }

  ~o_c_file_ptr_stream (void) { delete buf; buf = 0; }

  c_file_ptr_buf *rdbuf (void) { return buf; }

  void close (void) { if (buf) buf->close (); }

  int seek (long offset, int origin)
    { return buf ? buf->seek (offset, origin) : -1; }

  long tell (void) { return buf ? buf->tell () : -1; }

  void clear (void) { if (buf) buf->clear (); std::ostream::clear (); }

private:

  c_file_ptr_buf *buf;
};

class
io_c_file_ptr_stream : public std::iostream
{
public:

  io_c_file_ptr_stream (FILE* f,
			c_file_ptr_buf::close_fcn cf = c_file_ptr_buf::fclose)
    : std::iostream (0), buf (new c_file_ptr_buf (f, cf)) { init (buf); }

  ~io_c_file_ptr_stream (void) { delete buf; buf = 0; }

  c_file_ptr_buf *rdbuf (void) { return buf; }

  void close (void) { if (buf) buf->close (); }

  int seek (long offset, int origin)
    { return buf ? buf->seek (offset, origin) : -1; }

  long tell (void) { return buf ? buf->tell () : -1; }

  void clear (void) { if (buf) buf->clear (); std::iostream::clear (); }

private:

  c_file_ptr_buf *buf;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
