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

#include "c-file-ptr-stream.h"

#ifndef SEEK_SET
#define SEEK_SET 0
#endif

#ifndef SEEK_CUR
#define SEEK_CUR 1
#endif

#ifndef SEEK_END
#define SEEK_END 2
#endif

c_file_ptr_buf::~c_file_ptr_buf (void)
{
  flush ();

  close ();
}

// XXX FIXME XXX -- I'm sure there is room for improvement here...

int
c_file_ptr_buf::overflow (int c)
{
  if (f)
    return (c != EOF) ? fputc (c, f) : flush ();
  else
    return EOF;
}

int
c_file_ptr_buf::underflow (void)
{
  if (f)
    return fgetc (f);
  else
    return EOF;
}

int
c_file_ptr_buf::uflow (void)
{
  return underflow ();
}

int
c_file_ptr_buf::pbackfail (int c)
{
  return (c != EOF && f) ? ungetc (c, f) : EOF;
}

std::streamsize
c_file_ptr_buf::xsputn (const char* s, std::streamsize n)
{
  if (f)
    return fwrite (s, 1, n, f);
  else
    return 0;
}

std::streamsize
c_file_ptr_buf::xsgetn (char *s, std::streamsize n)
{
  if (f)
    return fread (s, 1, n, f);
  else
    return 0;
}

static inline int
seekdir_to_whence (std::ios::seekdir dir)
{
  return ((dir == std::ios::beg) ? SEEK_SET :
	  (dir == std::ios::cur) ? SEEK_CUR :
	  (dir == std::ios::end) ? SEEK_END :
	  dir);
}

std::streampos
c_file_ptr_buf::seekoff (std::streamoff offset, std::ios::seekdir dir,
			 std::ios::openmode)
{
  // XXX FIXME XXX -- is this the right thing to do?

  if (f)
    {
      fseek (f, offset, seekdir_to_whence (dir));

      return ftell (f);
    }
  else
    return 0;
}

std::streampos
c_file_ptr_buf::seekpos (std::streampos offset, std::ios::openmode)
{
  // XXX FIXME XXX -- is this the right thing to do?

  if (f)
    {
      fseek (f, offset, SEEK_SET);

      return ftell (f);
    }
  else
    return 0;
}

int
c_file_ptr_buf::sync (void)
{
  flush ();

  return 0;
}

int
c_file_ptr_buf::flush (void)
{
  return f ? fflush (f) : EOF;
}

int
c_file_ptr_buf::close (void)
{
  int retval = -1;

  if (f)
    {
      retval = fclose (f);
      f = 0;
    }

  return retval;
}

void
i_c_file_ptr_stream::close (void)
{
  if (buf)
    buf->close ();
}

void
o_c_file_ptr_stream::close (void)
{
  if (buf)
    buf->close ();
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

