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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <iomanip>

#include "filepos-wrappers.h"

#include "c-file-ptr-stream.h"

OCTAVE_BEGIN_NAMESPACE(octave)

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
  if (m_f)
    return (c != traits_type::eof ()) ? std::fputc (c, m_f) : flush ();
  else
    return traits_type::not_eof (c);
}

c_file_ptr_buf::int_type
c_file_ptr_buf::underflow_common (bool bump)
{
  if (m_f)
    {
      int_type c = std::fgetc (m_f);

      if (! bump && c != traits_type::eof ())
        ungetc (c, m_f);

      return c;
    }
  else
    return traits_type::eof ();
}

c_file_ptr_buf::int_type
c_file_ptr_buf::pbackfail (int_type c)
{
  return ((c != traits_type::eof () && m_f)
          ? ungetc (c, m_f) : traits_type::not_eof (c));
}

std::streamsize
c_file_ptr_buf::xsputn (const char *s, std::streamsize n)
{
  if (m_f)
    return std::fwrite (s, 1, n, m_f);
  else
    return 0;
}

std::streamsize
c_file_ptr_buf::xsgetn (char *s, std::streamsize n)
{
  if (m_f)
    return std::fread (s, 1, n, m_f);
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
  if (m_f)
    {
      octave_fseeko_wrapper (m_f, offset, seekdir_to_whence (dir));

      return octave_ftello_wrapper (m_f);
    }
  else
    return 0;
}

std::streampos
c_file_ptr_buf::seekpos (std::streampos offset, std::ios::openmode)
{
  if (m_f)
    {
      octave_fseeko_wrapper (m_f, offset, SEEK_SET);

      return octave_ftello_wrapper (m_f);
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
  return m_f ? std::fflush (m_f) : traits_type::eof ();
}

int
c_file_ptr_buf::buf_close (void)
{
  int retval = -1;

  flush ();

  if (m_f)
    {
      retval = m_cf (m_f);
      m_f = nullptr;
    }

  return retval;
}

int
c_file_ptr_buf::seek (off_t offset, int origin)
{
  return m_f ? octave_fseeko_wrapper (m_f, offset, origin) : -1;
}

off_t
c_file_ptr_buf::tell (void)
{
  return m_f ? octave_ftello_wrapper (m_f) : -1;
}

int
c_file_ptr_buf::file_close (FILE *m_f)
{
  return std::fclose (m_f);
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
  if (m_f)
    return (c != traits_type::eof ()) ? gzputc (m_f, c) : flush ();
  else
    return traits_type::not_eof (c);
}

c_zfile_ptr_buf::int_type
c_zfile_ptr_buf::underflow_common (bool bump)
{
  if (m_f)
    {
      int_type c = gzgetc (m_f);

      if (! bump && c != traits_type::eof ())
        gzungetc (c, m_f);

      return c;
    }
  else
    return traits_type::eof ();
}

c_zfile_ptr_buf::int_type
c_zfile_ptr_buf::pbackfail (int_type c)
{
  return ((c != traits_type::eof () && m_f)
          ? gzungetc (c, m_f) : traits_type::not_eof (c));
}

std::streamsize
c_zfile_ptr_buf::xsputn (const char *s, std::streamsize n)
{
  if (m_f)
    return gzwrite (m_f, s, n);
  else
    return 0;
}

std::streamsize
c_zfile_ptr_buf::xsgetn (char *s, std::streamsize n)
{
  if (m_f)
    return gzread (m_f, s, n);
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
  if (m_f)
    {
      gzseek (m_f, offset, seekdir_to_whence (dir));

      return gztell (m_f);
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
  if (m_f)
    {
      gzseek (m_f, offset, SEEK_SET);

      return gztell (m_f);
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

  return m_f ? gzflush (m_f, 0) : traits_type::eof ();
}

int
c_zfile_ptr_buf::buf_close (void)
{
  int retval = -1;

  flush ();

  if (m_f)
    {
      retval = m_cf (m_f);
      m_f = nullptr;
    }

  return retval;
}

#endif

OCTAVE_END_NAMESPACE(octave)
