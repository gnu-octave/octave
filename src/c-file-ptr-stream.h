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
#include <stdio.h>

class
c_file_ptr_buf : public std::streambuf
{
public:

  FILE* stdiofile (void) const { return f; }

  c_file_ptr_buf (FILE *f_arg) : std::streambuf (), f (f_arg) { }

  ~c_file_ptr_buf (void);

  int overflow (int);

  int underflow (void);

  int uflow (void);

  int pbackfail (int);

  std::streamsize xsputn (const char*, std::streamsize);

  std::streamsize xsgetn (char *, std::streamsize);

  std::streampos seekoff (std::streamoff, std::ios::seekdir,
			  std::ios::openmode = std::ios::in | std::ios::out);
  
  std::streampos seekpos (std::streampos,
			  std::ios::openmode = std::ios::in | std::ios::out);

  int sync (void);

  int flush (void);

  int close (void);

protected:

  FILE *f;
};

class
i_c_file_ptr_stream : public std::istream
{
public:

  i_c_file_ptr_stream (FILE* f)
    : std::istream (), buf (new c_file_ptr_buf (f)) { init (buf); }

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

  o_c_file_ptr_stream (FILE* f)
    : std::ostream (), buf (new c_file_ptr_buf (f)) { init (buf); }

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

