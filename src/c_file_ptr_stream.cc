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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "c_file_ptr_stream.h"

#ifndef SEEK_SET
#define SEEK_SET 0
#endif

#ifndef SEEK_CUR
#define SEEK_CUR 1
#endif

#ifndef SEEK_END
#define SEEK_END 2
#endif

c_file_ptr_buf::~c_file_ptr_buf (void) { fflush (f); }

// XXX FIXME XXX -- I'm sure there is room for improvement here...

int
c_file_ptr_buf::overflow (int c)
{
  return (c != EOF) ? fputc (c, f) : fflush (f);
}

int
c_file_ptr_buf::underflow (void)
{
  return fgetc (f);
}

int
c_file_ptr_buf::uflow (void)
{
  return underflow ();
}

int
c_file_ptr_buf::pbackfail (int c)
{
  return (c != EOF) ? ungetc (c, f) : EOF;
}

std::streamsize
c_file_ptr_buf::xsputn (const char* s, std::streamsize n)
{
  return fwrite (s, 1, n, f);
}

std::streamsize
c_file_ptr_buf::xsgetn (char *s, std::streamsize n)
{
  return fread (s, 1, n, f);
}

static inline int
seekdir_to_whence (std::ios::seekdir dir)
{
  return ((dir == ios::beg) ? SEEK_SET :
	  (dir == ios::cur) ? SEEK_CUR :
	  (dir == ios::end) ? SEEK_END :
	  dir);
}

std::streampos
c_file_ptr_buf::seekoff (std::streamoff offset, std::ios::seekdir dir,
			 std::ios::openmode)
{
  // XXX FIXME XXX -- is this the right thing to do?

  fseek (f, offset, seekdir_to_whence (dir));

  return ftell (f);
}

std::streampos
c_file_ptr_buf::seekpos (std::streampos offset, std::ios::openmode)
{
  // XXX FIXME XXX -- is this the right thing to do?

  fseek (f, offset, SEEK_SET);

  return ftell (f);
}

int
c_file_ptr_buf::sync (void)
{
  return 0;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

