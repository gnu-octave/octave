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

#if !defined (octave_octave_stream_h)
#define octave_octave_stream_h 1

#include <string>

#include <iostream.h>
#include <strstream.h>

#include "Array.h"
#include "data-conv.h"
#include "mach-info.h"

#include "oct-obj.h"
#include "str-vec.h"

struct
scanf_format_elt
{
  scanf_format_elt (const char *txt = 0, int w = 0, bool d = false,
		    char typ = '\0', char mod = '\0')
    : text (txt), width (w), discard (d), type (typ), modifier (mod) { }

  ~scanf_format_elt (void) { delete text; }

  const char *text;
  int width;
  bool discard;
  char type;
  char modifier;
};

class
scanf_format_list
{
public:

  scanf_format_list (const string& fmt = string ());

  ~scanf_format_list (void);

  int num_conversions (void) { return nconv; }

  // The length can be different than the number of conversions.
  // For example, "x %d y %d z" has 2 conversions but the length of
  // the list is 3 because of the characters that appear after the
  // last conversion.

  int length (void) { return list.length (); }

  const scanf_format_elt *first (void)
    {
      curr_idx = 0;
      return current ();
    }

  const scanf_format_elt *current (void) const
    { return list.length () > 0 ? list.elem (curr_idx) : 0; }

  const scanf_format_elt *next (void)
    {
      curr_idx++;
      if (curr_idx >= list.length ())
	curr_idx = 0;
      return current ();
    }

  void printme (void) const;

  bool ok (void) const { return (nconv >= 0); }

  operator void* () const { return ok () ? (void *) -1 : (void *) 0; }

  bool all_character_conversions (void);

  bool all_numeric_conversions (void);

private:

  // Number of conversions specified by this format string, or -1 if
  // invalid conversions have been found.
  int nconv;

  // Index to current element;
  int curr_idx;

  // List of format elements.
  Array<scanf_format_elt*> list;

  // Temporary buffer.
  ostrstream *buf;

  void add_elt_to_list (int width, bool discard, char type, char modifier,
			int& num_elts);

  void process_conversion (const string& s, int& i, int n, int& width,
			   bool& discard, char& type, char& modifier,
			   int& num_elts);

  int finish_conversion (const string& s, int& i, int n, int& width,
			 bool discard, char& type, char modifier,
			 int& num_elts);
  // No copying!

  scanf_format_list (const scanf_format_list&);

  scanf_format_list& operator = (const scanf_format_list&);
};

struct
printf_format_elt
{
  printf_format_elt (const char *txt = 0, int n = 0, char typ = '\0',
		     char mod = '\0')
    : text (txt), args (n), type (typ), modifier (mod) { }

  ~printf_format_elt (void) { delete text; }

  const char *text;
  int args;
  char type;
  char modifier;
};

class
printf_format_list
{
public:

  printf_format_list (const string& fmt = string ());

  ~printf_format_list (void);

  int num_conversions (void) { return nconv; }

  const printf_format_elt *first (void)
    {
      curr_idx = 0;
      return current ();
    }

  const printf_format_elt *current (void) const
    { return list.length () > 0 ? list.elem (curr_idx) : 0; }

  const printf_format_elt *next (void)
    {
      curr_idx++;
      if (curr_idx >= list.length ())
	curr_idx = 0;
      return current ();
    }

  void printme (void) const;

  bool ok (void) const { return (nconv >= 0); }

  operator void* () const { return ok () ? (void *) -1 : (void *) 0; }

private:

  // Number of conversions specified by this format string, or -1 if
  // invalid conversions have been found.
  int nconv;

  // Index to current element;
  int curr_idx;

  // List of format elements.
  Array<printf_format_elt*> list;

  // Temporary buffer.
  ostrstream *buf;

  void add_elt_to_list (int args, char type, char modifier,
			int& num_elts);

  void process_conversion (const string& s, int& i, int n, int& args,
			   char& modifier, char& type, int& num_elts);

  void finish_conversion (const string& s, int& i, int args,
			  char modifier, char& type, int& num_elts);

  // No copying!

  printf_format_list (const printf_format_list&);

  printf_format_list& operator = (const printf_format_list&);
};

// Provide an interface for Octave streams.

class
octave_base_stream
{
friend class octave_stream;

public:

  octave_base_stream (ios::openmode arg_md = ios::in|ios::out,
		      oct_mach_info::float_format ff = oct_mach_info::native)
    : md (arg_md), flt_fmt (ff), fail (false) { }

  virtual ~octave_base_stream (void) { }

  // The remaining functions are not specific to input or output only,
  // and must be provided by the derived classes.

  // Position a stream at OFFSET relative to ORIGIN.

  virtual int seek (streamoff offset, ios::seek_dir origin) = 0;

  // Return current stream position.

  virtual long tell (void) const = 0;

  // Return non-zero if EOF has been reached on this stream.

  virtual bool eof (void) const = 0;

  // The name of the file.

  virtual string name (void) = 0;

  // If the derived class provides this function and it returns a
  // pointer to a valid istream, scanf(), read(), getl(), and gets()
  // will automatically work for this stream.

  virtual istream *input_stream (void) { return 0; }

  // If the derived class provides this function and it returns a
  // pointer to a valid ostream, flush(), write(), and printf() will
  // automatically work for this stream.

  virtual ostream *output_stream (void) { return 0; }

  bool ok (void) const { return ! fail; }

  // Return current error message for this stream.

  string error (bool clear, int& err_num);

protected:

  int mode (void) { return md; }

  oct_mach_info::float_format float_format (void) { return flt_fmt; }

  // Set current error state and set fail to TRUE.

  void error (const string& msg);

  // Clear any error message and set fail to FALSE.

  void clear (void);

private:

  // The permission bits for the file.  Should be some combination of
  // ios::open_mode bits.
  int md;

  // Data format.
  oct_mach_info::float_format flt_fmt;

  // TRUE if an error has occurred.
  bool fail;

  // Should contain error message if fail is TRUE.
  string errmsg;

  // Functions that are defined for all input streams (input streams
  // are those that define is).

  string do_gets (int max_len, bool& err, bool strip_newline,
		  const char *fcn);

  string getl (int max_len, bool& err);
  string gets (int max_len, bool& err);

  octave_value do_read (int nr, int nc, oct_data_conv::data_type dt,
			int skip, oct_mach_info::float_format flt_fmt,
			int& count);

  octave_value read (const Matrix& size, oct_data_conv::data_type dt,
		     int skip, oct_mach_info::float_format flt_fmt,
		     int& count);

  octave_value do_char_scanf (scanf_format_list& fmt_list,
			      int nr, int nc, int& count);

  octave_value do_real_scanf (scanf_format_list& fmt_list,
			      int nr, int nc, int& count);

  octave_value do_scanf (scanf_format_list& fmt_list, int nr, int nc,
			 int& count);

  octave_value scanf (const string& fmt, const Matrix& size, int& count);

  octave_value do_oscanf (const scanf_format_elt *elt);

  octave_value_list oscanf (const string& fmt);

  // Functions that are defined for all output streams (output streams
  // are those that define os).

  int flush (void);

  int do_write (const Matrix& m, oct_data_conv::data_type dt, int skip,
		oct_mach_info::float_format flt_fmt);

  int write (const octave_value& data, oct_data_conv::data_type dt,
	     int skip, oct_mach_info::float_format flt_fmt);

  int do_printf (printf_format_list& fmt_list, const octave_value_list& args);

  int printf (const string& fmt, const octave_value_list& args);

  int puts (const string& s);

  // We can always do this in terms of seek(), so the derived class
  // only has to provide that.

  int rewind (void);

  void invalid_operation (const char *op, const char *rw);

  // No copying!

  octave_base_stream (const octave_base_stream&);

  octave_base_stream& operator = (const octave_base_stream&);
};

class
octave_stream
{
public:

  octave_stream (octave_base_stream *bs = 0, bool pf = false)
    : rep (bs), preserve (pf) { }

  ~octave_stream (void)
    {
      if (! preserve)
	delete rep;
    }

  int flush (void);

  string getl (int max_len, bool& err);
  string getl (const octave_value& max_len, bool& err);

  string gets (int max_len, bool& err);
  string gets (const octave_value& max_len, bool& err);

  int seek (streamoff offset, ios::seek_dir origin);
  int seek (const octave_value& offset, const octave_value& origin);

  long tell (void) const;

  int rewind (void);

  octave_value read (const Matrix& size, oct_data_conv::data_type dt,
		     int skip, oct_mach_info::float_format flt_fmt,
		     int& count);

  int write (const octave_value& data, oct_data_conv::data_type dt,
	     int skip, oct_mach_info::float_format flt_fmt);

  octave_value scanf (const string& fmt, const Matrix& size, int& count);

  octave_value_list oscanf (const string& fmt);

  int printf (const string& fmt, const octave_value_list& args);

  int puts (const string& s);
  int puts (const octave_value& s);

  bool eof (void) const;

  string error (bool clear, int& err_num);

  string error (bool clear = false)
    {
      int err_num;
      return error (clear, err_num);
    }

  bool ok (void) const { return rep && rep->ok (); }

  operator void* () const { return ok () ? (void *) -1 : (void *) 0; }

  string name (void);

  int mode (void);

  oct_mach_info::float_format float_format (void);

  static string mode_as_string (int mode);

private:

  // The actual representation of this stream.
  octave_base_stream *rep;

  // If true, do not delete rep.
  bool preserve;

  void invalid_stream_error (const char *op) const;

  bool stream_ok (const char *op, bool clear = true) const
    {
      bool retval = true;

      if (rep)
	{
	  if (clear)
	    rep->clear ();
	}
      else
	{
	  retval = false;
	  invalid_stream_error (op);
	}

      return retval;
    }

  void error (const string& msg)
    {
      if (rep)
	rep->error (msg);
    }

  // Must create named streams.

  octave_stream (void);

  // No copying!

  octave_stream (const octave_stream&);

  octave_stream& operator = (const octave_stream&);
};

class
octave_stream_list
{
protected:

  octave_stream_list (void) : list (32), curr_len (0) { }

public:

  ~octave_stream_list (void) { }

  static int insert (octave_base_stream *obs);

  static octave_stream *lookup (int fid);
  static octave_stream *lookup (const octave_value& fid);

  static int remove (int fid);
  static int remove (const octave_value& fid);

  static void clear (void);

  static string_vector get_info (int fid);
  static string_vector get_info (const octave_value& fid);

  static string list_open_files (void);

  static octave_value open_file_numbers (void);

  static int get_file_number (const octave_value& fid);

private:

  Array<octave_stream*> list;

  int curr_len;

  static octave_stream_list *instance;

  int do_insert (octave_base_stream *obs);

  octave_stream *do_lookup (int fid) const;
  octave_stream *do_lookup (const octave_value& fid) const;

  int do_remove (int fid);
  int do_remove (const octave_value& fid);

  void do_clear (void);

  string_vector do_get_info (int fid) const;
  string_vector do_get_info (const octave_value& fid) const;

  string do_list_open_files (void) const;

  octave_value do_open_file_numbers (void) const;

  int do_get_file_number (const octave_value& fid) const;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
