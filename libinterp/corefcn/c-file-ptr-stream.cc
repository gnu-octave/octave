/*

Copyright (C) 2000-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <iomanip>

#include "filepos-wrappers.h"

#include "c-file-ptr-stream.h"

#if ! defined (SEEK_SET)
#  define SEEK_SET 0
#endif

#if ! defined (SEEK_CUR)
#  define SEEK_CUR 1
#endif

#if ! defined (SEEK_END)
#  define SEEK_END 2
#endif

c_file_ptr_buf::~c_file_ptr_buf (void)
{
  buf_close ();
}

// FIXME: I'm sure there is room for improvement here...

c_file_ptr_buf::int_type
c_file_ptr_buf::overflow (int_type c)
{
  if (f)
    return (c != traits_type::eof ()) ? std::fputc (c, f) : flush ();
  else
    return traits_type::not_eof (c);
}

c_file_ptr_buf::int_type
c_file_ptr_buf::underflow_common (bool bump)
{
  if (f)
    {
      int_type c = std::fgetc (f);

      if (! bump && c != traits_type::eof ())
        ungetc (c, f);

      return c;
    }
  else
    return traits_type::eof ();
}

c_file_ptr_buf::int_type
c_file_ptr_buf::pbackfail (int_type c)
{
  return ((c != traits_type::eof () && f)
          ? ungetc (c, f) : traits_type::not_eof (c));
}

std::streamsize
c_file_ptr_buf::xsputn (const char *s, std::streamsize n)
{
  if (f)
    return std::fwrite (s, 1, n, f);
  else
    return 0;
}

std::streamsize
c_file_ptr_buf::xsgetn (char *s, std::streamsize n)
{
  if (f)
    return std::fread (s, 1, n, f);
  else
    return 0;
}

static inline int
seekdir_to_whence (std::ios::seekdir dir)
{
  return (dir == std::ios::beg
          ? SEEK_SET : (dir == std::ios::cur
                        ? SEEK_CUR : (dir == std::ios::end
                                      ? SEEK_END : dir)));
}

std::streampos
c_file_ptr_buf::seekoff (std::streamoff offset,
                         std::ios::seekdir dir,
                         std::ios::openmode)
{
  if (f)
    {
      octave_fseeko_wrapper (f, offset, seekdir_to_whence (dir));

      return octave_ftello_wrapper (f);
    }
  else
    return 0;
}

std::streampos
c_file_ptr_buf::seekpos (std::streampos offset, std::ios::openmode)
{
  if (f)
    {
      octave_fseeko_wrapper (f, offset, SEEK_SET);

      return octave_ftello_wrapper (f);
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
  return f ? std::fflush (f) : traits_type::eof ();
}

int
c_file_ptr_buf::buf_close (void)
{
  int retval = -1;

  flush ();

  if (f)
    {
      retval = cf (f);
      f = nullptr;
    }

  return retval;
}

int
c_file_ptr_buf::seek (off_t offset, int origin)
{
  return f ? octave_fseeko_wrapper (f, offset, origin) : -1;
}

off_t
c_file_ptr_buf::tell (void)
{
  return f ? octave_ftello_wrapper (f) : -1;
}

int
c_file_ptr_buf::file_close (FILE *f)
{
  return std::fclose (f);
}

#if defined (HAVE_ZLIB)

c_zfile_ptr_buf::~c_zfile_ptr_buf (void)
{
  buf_close ();
}

// FIXME: I'm sure there is room for improvement here...

c_zfile_ptr_buf::int_type
c_zfile_ptr_buf::overflow (int_type c)
{
  if (f)
    return (c != traits_type::eof ()) ? gzputc (f, c) : flush ();
  else
    return traits_type::not_eof (c);
}

c_zfile_ptr_buf::int_type
c_zfile_ptr_buf::underflow_common (bool bump)
{
  if (f)
    {
      int_type c = gzgetc (f);

      if (! bump && c != traits_type::eof ())
        gzungetc (c, f);

      return c;
    }
  else
    return traits_type::eof ();
}

c_zfile_ptr_buf::int_type
c_zfile_ptr_buf::pbackfail (int_type c)
{
  return ((c != traits_type::eof () && f)
          ? gzungetc (c, f) : traits_type::not_eof (c));
}

std::streamsize
c_zfile_ptr_buf::xsputn (const char *s, std::streamsize n)
{
  if (f)
    return gzwrite (f, s, n);
  else
    return 0;
}

std::streamsize
c_zfile_ptr_buf::xsgetn (char *s, std::streamsize n)
{
  if (f)
    return gzread (f, s, n);
  else
    return 0;
}

std::streampos
c_zfile_ptr_buf::seekoff (std::streamoff /* offset */,
                          std::ios::seekdir /* dir */,
                          std::ios::openmode)
{
  // FIXME
#if 0
  if (f)
    {
      gzseek (f, offset, seekdir_to_whence (dir));

      return gztell (f);
    }
  else
    return 0;
#endif
  return -1;
}

std::streampos
c_zfile_ptr_buf::seekpos (std::streampos /* offset */, std::ios::openmode)
{
  // FIXME
#if 0
  if (f)
    {
      gzseek (f, offset, SEEK_SET);

      return gztell (f);
    }
  else
    return 0;
#endif
  return -1;
}

int
c_zfile_ptr_buf::sync (void)
{
  flush ();

  return 0;
}

int
c_zfile_ptr_buf::flush (void)
{
  // FIXME: do we need something more complex here, passing
  // something other than 0 for the second argument to gzflush and
  // checking the return value, etc.?

  return f ? gzflush (f, 0) : traits_type::eof ();
}

int
c_zfile_ptr_buf::buf_close (void)
{
  int retval = -1;

  flush ();

  if (f)
    {
      retval = cf (f);
      f = nullptr;
    }

  return retval;
}

#endif
