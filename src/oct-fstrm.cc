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

#include <cerrno>
#include <cstring>

#include "error.h"
#include "oct-fstrm.h"

octave_fstream::octave_fstream (const string& nm_arg,
				ios::openmode md = ios::in|ios::out,
				octave_base_stream::arch_type at)
  : octave_base_stream (md, at), nm (nm_arg)
{
  // Override default protection of 0664 so that umask will appear to
  // do the right thing.

  fs.open (nm.c_str (), md, 0666);

  if (! fs)
    error (strerror (errno));
}

// Position a stream at OFFSET relative to ORIGIN.

int
octave_fstream::seek (streamoff offset, ios::seek_dir origin)
{
  int retval = -1;

  if (! fs.bad ())
    {
      fs.clear ();

      filebuf *fb = fs.rdbuf ();

      if (fb)
	{
	  fb->seekoff (offset, origin);
	  retval = fs.bad () ? -1 : 0;
	}
    }

  return retval;
}

// Return current stream position.

long
octave_fstream::tell (void) const
{
  long retval = -1;

  if (fs)
    {
      filebuf *fb = fs.rdbuf ();
      retval = (long) fb->seekoff (0, ios::cur);
    }

  return retval;
}

// Return non-zero if EOF has been reached on this stream.

bool
octave_fstream::eof (void) const
{
  return fs.eof ();
}

// The name of the file.

string
octave_fstream::name (void)
{
  return nm;
}

istream *
octave_fstream::input_stream (void)
{
  istream *retval = 0;

  if (mode () & ios::in)
    retval = &fs;

  return retval;
}

ostream *
octave_fstream::output_stream (void)
{
  ostream *retval = 0;

  if (mode () & ios::out)
    retval = &fs;

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
