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

#if !defined (octave_octave_stdiostream_h)
#define octave_octave_stdiostream_h 1

#include <stdiostream.h>

#include "oct-stream.h"

class
octave_base_stdiostream : public octave_base_stream
{
public:

  octave_base_stdiostream (const std::string& n, FILE *f,
			   ios::openmode arg_md = ios::in|ios::out,
			   oct_mach_info::float_format flt_fmt =
			   oct_mach_info::native)
    : octave_base_stream (arg_md, flt_fmt), nm (n), fp (f) { }

  // Position a stream at OFFSET relative to ORIGIN.

  int seek (streamoff offset, ios::seek_dir origin);

  // Return current stream position.

  long tell (void) const;

  // The name of the file.

  std::string name (void) const { return nm; }

  virtual stdiobuf *rdbuf (void) const = 0;

  virtual bool bad (void) const = 0;

  virtual void clear (void) = 0;

protected:

  std::string nm;

  FILE *fp;

  ~octave_base_stdiostream (void);

  // No copying!

  octave_base_stdiostream (const octave_base_stdiostream&);

  octave_base_stdiostream& operator = (const octave_base_stdiostream&);
};

class
octave_istdiostream : public octave_base_stdiostream
{
public:

  octave_istdiostream (const std::string& n, FILE *f = 0,
		       ios::openmode arg_md = ios::in,
		       oct_mach_info::float_format flt_fmt =
		       oct_mach_info::native);

  static octave_stream
  create (const std::string& n, FILE *f = 0, ios::openmode arg_md = ios::in,
	  oct_mach_info::float_format flt_fmt = oct_mach_info::native);

  // Return non-zero if EOF has been reached on this stream.

  bool eof (void) const { return is ? is->eof () : true; }

  std::istream *input_stream (void) { return is; }

  std::ostream *output_stream (void) { return 0; }

  // XXX FIXME XXX -- should not have to cast away const here.
  stdiobuf *rdbuf (void) const
    { return is ? (const_cast<std::istdiostream *> (is))->rdbuf () : 0; }

  bool bad (void) const { return is ? is->bad () : true; }

  void clear (void)
    {
      if (is)
	is->clear ();
    }

protected:

  std::istdiostream *is;

  ~octave_istdiostream (void);

private:

  // No copying!

  octave_istdiostream (const octave_istdiostream&);

  octave_istdiostream& operator = (const octave_istdiostream&);
};

class
octave_ostdiostream : public octave_base_stdiostream
{
public:

  octave_ostdiostream (const std::string& n, FILE *f = 0,
		       ios::openmode arg_md = ios::out,
		       oct_mach_info::float_format flt_fmt =
		       oct_mach_info::native);

  static octave_stream
  create (const std::string& n, FILE *f = 0, ios::openmode arg_md = ios::out,
	  oct_mach_info::float_format flt_fmt = oct_mach_info::native);

  // Return non-zero if EOF has been reached on this stream.

  bool eof (void) const { return os ? os->eof () : true; }

  std::istream *input_stream (void) { return 0; }

  std::ostream *output_stream (void) { return os; }

  // XXX FIXME XXX -- should not have to cast away const here.
  stdiobuf *rdbuf (void) const
    { return os ? (const_cast<std::ostdiostream *> (os))->rdbuf () : 0; }

  bool bad (void) const { return os ? os->bad () : true; }

  void clear (void)
    {
      if (os)
	os->clear ();
    }

protected:

  std::ostdiostream *os;

  ~octave_ostdiostream (void);

private:

  // No copying!

  octave_ostdiostream (const octave_ostdiostream&);

  octave_ostdiostream& operator = (const octave_ostdiostream&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
