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

#include "oct-stream.h"
#include "c-file-ptr-stream.h"

class
octave_stdiostream : public octave_base_stream
{
public:

  octave_stdiostream (const std::string& n, FILE *f = 0,
		      std::ios::openmode arg_md = std::ios::in|std::ios::out,
		      oct_mach_info::float_format flt_fmt = oct_mach_info::flt_fmt_native,
		      c_file_ptr_buf::close_fcn cf = c_file_ptr_buf::fclose)
    : octave_base_stream (arg_md, flt_fmt), nm (n), md (arg_md), s(0)
  {
    if (f)
      s = new io_c_file_ptr_stream (f, cf);
  }

  static octave_stream
  create (const std::string& n, FILE *f = 0,
	  std::ios::openmode arg_md = std::ios::in|std::ios::out,
	  oct_mach_info::float_format flt_fmt = oct_mach_info::flt_fmt_native,
	  c_file_ptr_buf::close_fcn cf = c_file_ptr_buf::fclose)
  {
    return octave_stream (new octave_stdiostream (n, f, arg_md, flt_fmt, cf));
  }

  // Position a stream at OFFSET relative to ORIGIN.

  int seek (std::streamoff offset, std::ios::seekdir origin);

  // Return current stream position.

  long tell (void) const;

  // Return non-zero if EOF has been reached on this stream.

  bool eof (void) const { return s ? s->eof () : true; }

  // The name of the file.

  std::string name (void) const { return nm; }

  std::istream *input_stream (void) { return (md & std::ios::in) ? s : 0; }

  std::ostream *output_stream (void) { return (md & std::ios::out) ? s : 0; }

  // XXX FIXME XXX -- should not have to cast away const here.
  c_file_ptr_buf *rdbuf (void) const
    { return s ? (const_cast<io_c_file_ptr_stream *> (s))->rdbuf () : 0; }

  bool bad (void) const { return s ? s->bad () : true; }

  void clear (void) { if (s) s->clear (); }

  void do_close (void) { if (s) s->close (); }

protected:

  std::string nm;

  std::ios::openmode md;

  io_c_file_ptr_stream *s;

  ~octave_stdiostream (void) { delete s; }

private:

  // No copying!

  octave_stdiostream (const octave_stdiostream&);

  octave_stdiostream& operator = (const octave_stdiostream&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
