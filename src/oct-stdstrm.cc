/*

Copyright (C) 1996, 1997 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdio>

#include "oct-stdstrm.h"

// Position a stream at OFFSET relative to ORIGIN.

int
octave_base_stdiostream::seek (std::streamoff offset, std::ios::seekdir origin)
{
  int retval = -1;

  if (! bad ())
    {
      c_file_ptr_buf *sb = rdbuf ();

      if (sb)
	{
	  clear ();

	  sb->pubseekoff (offset, origin);
	  retval = bad () ? -1 : 0;
	}
    }

  return retval;
}

// Return current stream position.

long
octave_base_stdiostream::tell (void) const
{
  long retval = -1;

  if (! bad ())
    {
      c_file_ptr_buf *sb = rdbuf ();

      if (sb)
	{
	  retval = static_cast<long> (sb->pubseekoff (0, std::ios::cur));

	  if (bad ())
	    retval = -1;
	}
    }

  return retval;
}

octave_stream
octave_istdiostream::create (const std::string& n, FILE *f,
			     c_file_ptr_buf::close_fcn cf,
			     std::ios::openmode arg_md,
			     oct_mach_info::float_format flt_fmt)
{
  return octave_stream (new octave_istdiostream (n, f, cf, arg_md, flt_fmt));
}

octave_istdiostream::octave_istdiostream (const std::string& n, FILE *f,
					  c_file_ptr_buf::close_fcn cf,
					  std::ios::openmode arg_md,
					  oct_mach_info::float_format flt_fmt)
  : octave_base_stdiostream (n, arg_md, flt_fmt), is (0)
{
  if (f)
    is = new i_c_file_ptr_stream (f, cf);
}

octave_istdiostream::~octave_istdiostream (void)
{
  delete is;
}

void
octave_istdiostream::do_close (void)
{
  if (is)
    is->close ();
}

octave_stream
octave_ostdiostream::create (const std::string& n, FILE *f,
			     c_file_ptr_buf::close_fcn cf,
			     std::ios::openmode arg_md,
			     oct_mach_info::float_format flt_fmt)
{
  return octave_stream (new octave_ostdiostream (n, f, cf, arg_md, flt_fmt));
}

octave_ostdiostream::octave_ostdiostream (const std::string& n, FILE *f,
					  c_file_ptr_buf::close_fcn cf,
					  std::ios::openmode arg_md,
					  oct_mach_info::float_format flt_fmt)
  : octave_base_stdiostream (n, arg_md, flt_fmt), os (0)
{
  if (f)
    os = new o_c_file_ptr_stream (f, cf);
}

octave_ostdiostream::~octave_ostdiostream (void)
{
  delete os;
}

void
octave_ostdiostream::do_close (void)
{
  if (os)
    os->close ();
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
