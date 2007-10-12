/*

Copyright (C) 1996, 1997, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
              2006, 2007 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_octave_stdiostream_h)
#define octave_octave_stdiostream_h 1

#include "oct-stream.h"
#include "c-file-ptr-stream.h"

template <typename BUF_T, typename STREAM_T, typename FILE_T>
class
octave_tstdiostream : public octave_base_stream
{
public:

  octave_tstdiostream (const std::string& n, FILE_T f = 0,
		       std::ios::openmode m = std::ios::in|std::ios::out,
		       oct_mach_info::float_format ff
		         = oct_mach_info::native_float_format (),
		       typename BUF_T::close_fcn cf = BUF_T::fclose)
    : octave_base_stream (m, ff), nm (n), md (m),
      s (f ? new STREAM_T (f, cf) : 0)
  { }

  static octave_stream
  create (const std::string& n, FILE_T f = 0,
	  std::ios::openmode m = std::ios::in|std::ios::out,
	  oct_mach_info::float_format ff
	    = oct_mach_info::native_float_format (),
	  typename BUF_T::close_fcn cf = BUF_T::fclose)
  {
    return octave_stream (new octave_tstdiostream (n, f, m, ff, cf));
  }

  // Position a stream at OFFSET relative to ORIGIN.

  int seek (long offset, int origin)
    { return s ? s->seek (offset, origin) : -1; }

  // Return current stream position.

  long tell (void) { return s ? s->tell () : -1; }

  // Return non-zero if EOF has been reached on this stream.

  bool eof (void) const { return s ? s->eof () : true; }

  // The name of the file.

  std::string name (void) const { return nm; }

  std::istream *input_stream (void) { return (md & std::ios::in) ? s : 0; }

  std::ostream *output_stream (void) { return (md & std::ios::out) ? s : 0; }

  // FIXME -- should not have to cast away const here.
  BUF_T *rdbuf (void) const
    { return s ? (const_cast<STREAM_T *> (s))->rdbuf () : 0; }

  bool bad (void) const { return s ? s->bad () : true; }

  void clear (void) { if (s) s->clear (); }

  void do_close (void) { if (s) s->close (); }

protected:

  std::string nm;

  std::ios::openmode md;

  STREAM_T *s;

  ~octave_tstdiostream (void) { delete s; }

private:

  // No copying!

  octave_tstdiostream (const octave_tstdiostream&);

  octave_tstdiostream& operator = (const octave_tstdiostream&);
};

typedef octave_tstdiostream<c_file_ptr_buf, io_c_file_ptr_stream, FILE *> octave_stdiostream;

#ifdef HAVE_ZLIB

typedef octave_tstdiostream<c_zfile_ptr_buf, io_c_zfile_ptr_stream, gzFile> octave_zstdiostream;

#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
