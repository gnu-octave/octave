/*

Copyright (C) 1996 John W. Eaton

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

octave_base_stdiostream::~octave_base_stdiostream (void)
{
  if (fp)
    {
      fclose (fp);
      fp = 0;
    }
}

// Position a stream at OFFSET relative to ORIGIN.

int
octave_base_stdiostream::seek (streamoff offset, ios::seek_dir origin)
{
  int retval = -1;

  if (! bad ())
    {
      stdiobuf *sb = rdbuf ();

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
octave_base_stdiostream::tell (void) const
{
  long retval = -1;

  if (! bad ())
    {
      stdiobuf *sb = rdbuf ();

      if (sb)
	{
	  retval = static_cast<long> (sb->seekoff (0, ios::cur));

	  if (bad ())
	    retval = -1;
	}
    }

  return retval;
}

octave_istdiostream::octave_istdiostream (const string& n, FILE *f,
					  ios::openmode arg_md,
					  oct_mach_info::float_format flt_fmt)
  : octave_base_stdiostream (n, f, arg_md, flt_fmt), is (0)
{
  if (f)
    is = new istdiostream (f);
}

octave_istdiostream::~octave_istdiostream (void)
{
  delete is;
}

octave_ostdiostream::octave_ostdiostream (const string& n, FILE *f,
					  ios::openmode arg_md,
					  oct_mach_info::float_format flt_fmt)
  : octave_base_stdiostream (n, f, arg_md, flt_fmt), os (0)
{
  if (f)
    os = new ostdiostream (f);
}

octave_ostdiostream::~octave_ostdiostream (void)
{
  delete os;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
