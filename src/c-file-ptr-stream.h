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

#ifdef __GNUG__
#pragma interface
#endif

#include <iostream>
#include <fstream>
#include <cstdio>

//
// The c_file_ptr_buf requires a std::filebuf that accepts an open
// file descriptor. This feature, while not part of the ISO C++
// standard, is supported by a variety of C++ compiler runtimes,
// albeit in slightly different ways.
//
// The C++ runtime libraries shipped with GCC versions < 3.0, Sun Pro,
// Sun Workshop/Forte 5/6, Compaq C++ all support a non-standard filebuf
// constructor that takes an open file descriptor. The almost ISO compliant
// GNU C++ runtime shipped with GCC 3.0.x supports a different non-standard
// filebuf constructor that takes a FILE* instead; starting from GCC 3.1,
// the GNU C++ runtime removes all non-standard std::filebuf constructors
// and provides an extension template class __gnu_cxx::stdio_filebuf
// that supports the the 3.0.x behavior.
//
#if __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 1)
# include <ext/stdio_filebuf.h>
# define OCTAVE_STD_FILEBUF __gnu_cxx::stdio_filebuf<char>
#else
# define OCTAVE_STD_FILEBUF std::filebuf
#endif

class
c_file_ptr_buf : public OCTAVE_STD_FILEBUF
{
public:

#if !defined (CXX_ISO_COMPLIANT_LIBRARY)
  typedef int int_type;
#else
  typedef std::filebuf::int_type int_type;
#endif

  typedef int (*close_fcn) (FILE *);

  FILE* stdiofile (void) const { return f; }

  c_file_ptr_buf (FILE *f_arg, close_fcn cf_arg = ::fclose)
    : 
#if defined __GNUC__ && __GNUC__ >= 3
    OCTAVE_STD_FILEBUF (f_arg, std::ios::in | std::ios::out),
#else
    OCTAVE_STD_FILEBUF (f_arg ? fileno (f_arg) : -1),
#endif
    f (f_arg), cf (cf_arg),
    fd (f_arg ? fileno (f_arg) : -1)
    { }

  ~c_file_ptr_buf (void);

  int_type overflow (int_type);

  int_type underflow (void);

  int_type uflow (void);

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

  int file_number () const { return fd; }

protected:

  FILE *f;

  close_fcn cf;

private:

  int fd;
};

#undef OCTAVE_STD_FILEBUF

class
i_c_file_ptr_stream : public std::istream
{
public:

  i_c_file_ptr_stream (FILE* f, c_file_ptr_buf::close_fcn cf = ::fclose)
    : std::istream (0), buf (new c_file_ptr_buf (f, cf)) { init (buf); }

  ~i_c_file_ptr_stream (void) { delete buf; buf = 0; }

  c_file_ptr_buf *rdbuf (void) { return buf; }

  void close (void);

private:

  c_file_ptr_buf *buf;
};

class
o_c_file_ptr_stream : public std::ostream
{
public:

  o_c_file_ptr_stream (FILE* f, c_file_ptr_buf::close_fcn cf = ::fclose)
    : std::ostream (0), buf (new c_file_ptr_buf (f, cf)) { init (buf); }

  ~o_c_file_ptr_stream (void) { delete buf; buf = 0; }

  c_file_ptr_buf *rdbuf (void) { return buf; }

  void close (void);

private:

  c_file_ptr_buf *buf;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

