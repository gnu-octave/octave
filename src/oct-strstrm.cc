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

#include "oct-strstrm.h"

// Position a stream at OFFSET relative to ORIGIN.

int
octave_base_strstream::seek (streamoff offset, ios::seek_dir origin)
{
  int retval = -1;

  if (! bad ())
    {
      std::streambuf *sb = rdbuf ();

      if (sb)
	{
	  clear ();

	  sb->seekoff (offset, origin);
	  retval = bad () ? -1 : 0;
	}
    }

  return retval;
}

// Return current stream position.

long
octave_base_strstream::tell (void) const
{
  long retval = -1;

  if (! bad ())
    {
      // XXX FIXME XXX -- shouldn't have to do this!

      std::streambuf *sb = (const_cast<octave_base_strstream *>(this))->rdbuf ();

      if (sb)
	{
	  retval = static_cast<long> (sb->seekoff (0, ios::cur));

	  if (bad ())
	    retval = -1;
	}
    }

  return retval;
}

octave_stream
octave_istrstream::create (const char *data, ios::openmode arg_md,
			   oct_mach_info::float_format flt_fmt)
{
  return octave_stream (new octave_istrstream (data, arg_md, flt_fmt));
}

octave_stream
octave_istrstream::create (const std::string& data, ios::openmode arg_md,
			   oct_mach_info::float_format flt_fmt)
{
  return octave_stream (new octave_istrstream (data, arg_md, flt_fmt));
}

octave_stream
octave_ostrstream::create (ios::openmode arg_md,
			   oct_mach_info::float_format flt_fmt)
{
  return octave_stream (new octave_ostrstream (arg_md, flt_fmt));
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
