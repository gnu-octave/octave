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

#if !defined (octave_octave_fstream_h)
#define octave_octave_fstream_h 1

#include <fstream.h>

#include "oct-stream.h"

class
octave_fstream : public octave_base_stream
{
public:

  octave_fstream (const string& nm_arg,
		  ios::openmode md = ios::in|ios::out,
		  octave_base_stream::arch_type at = at_native);

  ~octave_fstream (void) { }

  // Position a stream at OFFSET relative to ORIGIN.

  int seek (streampos offset, ios::seek_dir origin);

  // Return current stream position.

  long tell (void) const;

  // Return non-zero if EOF has been reached on this stream.

  bool eof (void) const;

  // The name of the file.

  string name (void);

  istream *input_stream (void);

  ostream *output_stream (void);

private:

  string nm;

  fstream fs;

  // No copying!

  octave_fstream (const octave_fstream&);

  octave_fstream& operator = (const octave_fstream&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
