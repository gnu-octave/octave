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

#include <cassert>
#include <cstring>

#include <iomanip>
#include <strstream>
#include <string>

#include "lo-ieee.h"
#include "lo-mappers.h"
#include "lo-utils.h"
#include "str-vec.h"

#include "error.h"
#include "input.h"
#include "oct-stream.h"
#include "oct-obj.h"
#include "utils.h"

// Possible values for conv_err:
//
//   1 : not a real scalar
//   2 : value is NaN
//   3 : value is not an integer

static int
convert_to_valid_int (const octave_value& tc, int& conv_err)
{
  int retval = 0;

  conv_err = 0;

  double dval = tc.double_value ();

  if (! error_state)
    {
      if (! xisnan (dval))
	{
	  int ival = NINT (dval);

	  if (ival == dval)
	    retval = ival;
	  else
	    conv_err = 3;
	}
      else
	conv_err = 2;
    }
  else
    conv_err = 1;

  return retval;
}

static int
get_size (double d, const char *warn_for)
{
  int retval = -1;

  if (! xisnan (d))
    {
      if (! xisinf (d))
	{
	  if (d >= 0.0)
	    retval = NINT (d);
	  else
	    ::error ("%s: negative value invalid as size specification",
		     warn_for);
	}
      else
	retval = -1;
    }
  else
    ::error ("%s: NaN is invalid as size specification", warn_for);

  return retval;
}

static void
get_size (const Matrix& size, int& nr, int& nc, bool& one_elt_size_spec,
	  const char *warn_for)
{
  nr = -1;
  nc = -1;

  one_elt_size_spec = false;

  double dnr = -1.0;
  double dnc = -1.0;

  int sz_nr = size.rows ();
  int sz_nc = size.cols ();

  if (sz_nr == 1 && sz_nc == 1)
    {
      one_elt_size_spec = true;

      dnr = size (0, 0);
      dnc = 1.0;
    }
  else if (sz_nr == 1 && sz_nc > 0)
    {
      dnr = size (0, 0);

      if (sz_nc == 2)
	dnc = size (0, 1);
      else if (sz_nc > 2)
	::error ("%s: invalid size specification", warn_for);
    }
  else if (sz_nc == 1 && sz_nr > 0)
    {
      dnr = size (0, 0);

      if (sz_nr == 2)
	dnc = size (1, 0);
      else if (sz_nr > 2)
	::error ("%s: invalid size specification", warn_for);
    }
  else
    ::error ("%s: invalid size specification", warn_for);

  if (! error_state)
    {
      nr = get_size (dnr, warn_for);

      if (! error_state && dnc >= 0.0)
	nc = get_size (dnc, warn_for);
    }
}

scanf_format_list::scanf_format_list (const std::string& s)
  : nconv (0), curr_idx (0), list (16), buf (0)
{
  int num_elts = 0;

  int n = s.length ();

  int i = 0;

  int width = 0;
  bool discard = false;
  char modifier = '\0';
  char type = '\0';

  bool have_more = true;

  while (i < n)
    {
      have_more = true;

      if (! buf)
	buf = new std::ostrstream ();

      if (s[i] == '%')
	{
	  // Process percent-escape conversion type.

	  process_conversion (s, i, n, width, discard, type, modifier,
			      num_elts);
	  have_more = (buf != 0);
	}
      else if (isspace (s[i]))
	{
	  type = scanf_format_elt::whitespace_conversion;

	  width = 0;
	  discard = false;
	  modifier = '\0';
	  *buf << " ";

	  while (++i < n && isspace (s[i]))
	    /* skip whitespace */;

	  add_elt_to_list (width, discard, type, modifier, num_elts);

	  have_more = false;
	}
      else
	{
	  type = scanf_format_elt::literal_conversion;

	  width = 0;
	  discard = false;
	  modifier = '\0';

	  while (i < n && ! isspace (s[i]) && s[i] != '%')
	    *buf << s[i++];

	  add_elt_to_list (width, discard, type, modifier, num_elts);

	  have_more = false;
	}

      if (nconv < 0)
	{
	  have_more = false;
	  break;
	}
    }

  if (have_more)
    add_elt_to_list (width, discard, type, modifier, num_elts);

  list.resize (num_elts);

  delete buf;
}

scanf_format_list::~scanf_format_list (void)
{
  int n = list.length ();

  for (int i = 0; i < n; i++)
    {
      scanf_format_elt *elt = list(i);
      delete elt;
    }	
}

void
scanf_format_list::add_elt_to_list (int width, bool discard, char type,
				    char modifier, int& num_elts,
				    const std::string& char_class)
{
  if (buf)
    {
      *buf << ends;

      char *text = buf->str ();

      if (text)
	{
	  if (*text)
	    {
	      scanf_format_elt *elt
		= new scanf_format_elt (text, width, discard, type,
					modifier, char_class);

	      if (num_elts == list.length ())
		list.resize (2 * num_elts);

	      list(num_elts++) = elt;
	    }
	  else
	    delete [] text;
	}

      delete buf;
      buf = 0;
    }
}

static std::string
expand_char_class (const std::string& s)
{
  std::string retval;

  size_t len = s.length ();

  size_t i = 0;

  while (i < len)
    {
      unsigned char c = s[i++];

      if (c == '-' && i > 1 && i < len
	  && (unsigned char) s[i-2] <= (unsigned char) s[i])
	{
	  // Add all characters from the range except the first (we
	  // already added it below).

	  for (c = s[i-2]+1; c < s[i]; c++)
	    retval += c;
	}
      else
	{
	  // Add the character to the class.  Only add '-' if it is
	  // the last character in the class.

	  if (c != '-' || i == len)
	    retval += c;
	}
    }

  return retval;
}

void
scanf_format_list::process_conversion (const std::string& s, int& i, int n,
				       int& width, bool& discard, char& type,
				       char& modifier, int& num_elts)
{
  width = 0;
  discard = false;
  modifier = '\0';
  type = '\0';

  *buf << s[i++];

  bool have_width = false;

  while (i < n)
    {
      switch (s[i])
	{
	case '*':
	  if (discard)
	    nconv = -1;
	  else
	    {
	      discard = true;
	      *buf << s[i++];
	    }
	  break;

	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	  if (have_width)
	    nconv = -1;
	  else
	    {
	      char c = s[i++];
	      width = width * 10 + c - '0';
	      have_width = true;
	      *buf << c;
	      while (i < n && isdigit (s[i]))
		{
		  c = s[i++];
		  width = width * 10 + c - '0';
		  *buf << c;
		}
	    }
	  break;

	case 'h': case 'l': case 'L':
	  if (modifier != '\0')
	    nconv = -1;
	  else
	    modifier = s[i++];
	  break;

	case 'd': case 'i': case 'o': case 'u': case 'x':
	  if (modifier == 'L')
	    {
	      nconv = -1;
	      break;
	    }
	  goto fini;

	case 'e': case 'f': case 'g':
	  if (modifier == 'h')
	    {
	      nconv = -1;
	      break;
	    }

	  // No float or long double conversions, thanks.
	  *buf << 'l';

	  goto fini;

	case 'c': case 's': case 'p': case '%': case '[':
	  if (modifier != '\0')
	    {
	      nconv = -1;
	      break;
	    }
	  goto fini;

	fini:
	  {
	    if (finish_conversion (s, i, n, width, discard, type,
				   modifier, num_elts) == 0)
	      return;
	  }
	  break;

	default:
	  nconv = -1;
	  break;
	}

      if (nconv < 0)
	break;
    }

  nconv = -1;
}

int
scanf_format_list::finish_conversion (const std::string& s, int& i, int n,
				      int& width, bool discard, char& type,
				      char modifier, int& num_elts)
{
  int retval = 0;

  std::string char_class;

  if (s[i] == '%')
    *buf << s[i++];
  else
    {
      int beg_idx = -1;
      int end_idx = -1;

      type = s[i];

      if (s[i] == '[')
	{
	  *buf << s[i++];

	  if (i < n)
	    {
	      beg_idx = i;

	      if (s[i] == '^')
		{
		  type = '^';
		  *buf << s[i++];

		  if (i < n)
		    {
		      beg_idx = i;

		      if (s[i] == ']')
			*buf << s[i++];
		    }
		}
	      else if (s[i] == ']')
		*buf << s[i++];
	    }

	  while (i < n && s[i] != ']')
	    *buf << s[i++];

	  if (i < n && s[i] == ']')
	    {
	      end_idx = i-1;
	      *buf << s[i++];
	    }

	  if (s[i-1] != ']')
	    retval = nconv = -1;
	}
      else
	*buf << s[i++];

      nconv++;

      if (nconv > 0)
	{
	  if (beg_idx >= 0 && end_idx >= 0)
	    char_class = expand_char_class (s.substr (beg_idx,
						      end_idx - beg_idx +1));

	  add_elt_to_list (width, discard, type, modifier, num_elts,
			   char_class);
	}
    }

  return retval;
}

void
scanf_format_list::printme (void) const
{
  int n = list.length ();

  for (int i = 0; i < n; i++)
    {
      scanf_format_elt *elt = list(i);

      std::cerr
	<< "width:      " << elt->width << "\n"
	<< "discard:    " << elt->discard << "\n"
	<< "type:       ";

      if (elt->type == scanf_format_elt::literal_conversion)
	std::cerr << "literal text\n";
      else if (elt->type == scanf_format_elt::whitespace_conversion)
	std::cerr << "whitespace\n";
      else
	std::cerr << elt->type << "\n";

      std::cerr
	<< "modifier:   " << elt->modifier << "\n"
	<< "char_class: `" << undo_string_escapes (elt->char_class) << "'\n"
	<< "text:       `" << undo_string_escapes (elt->text) << "'\n\n";
    }
}

bool
scanf_format_list::all_character_conversions (void)
{
  int n = list.length ();

  if (n > 0)
    {
      for (int i = 0; i < n; i++)
	{
	  scanf_format_elt *elt = list(i);

	  switch (elt->type)
	    {
	    case 'c': case 's': case '%': case '[': case '^':
	    case scanf_format_elt::literal_conversion:
	    case scanf_format_elt::whitespace_conversion:
	      break;

	    default:
	      return false;
	      break;
	    }
	}

      return true;
    }
  else
    return false;
}

bool
scanf_format_list::all_numeric_conversions (void)
{
  int n = list.length ();

  if (n > 0)
    {
      for (int i = 0; i < n; i++)
	{
	  scanf_format_elt *elt = list(i);

	  switch (elt->type)
	    {
	    case 'd': case 'i': case 'o': case 'u': case 'x':
	    case 'e': case 'f': case 'g':
	      break;

	    default:
	      return false;
	      break;
	    }
	}

      return true;
    }
  else
    return false;
}

// Ugh again.

printf_format_list::printf_format_list (const std::string& s)
  : nconv (0), curr_idx (0), list (16), buf (0)
{
  int num_elts = 0;

  int n = s.length ();

  int i = 0;

  int args = 0;
  char modifier = '\0';
  char type = '\0';

  bool have_more = true;

  while (i < n)
    {
      have_more = true;

      if (! buf)
	buf = new std::ostrstream ();

      switch (s[i])
	{
	case '%':
	  process_conversion (s, i, n, args, type, modifier, num_elts);
	  have_more = (buf != 0);
	  break;

	default:
	  args = 0;
	  modifier = '\0';
	  type = '\0';
	  *buf << s[i++];
	  break;
	}

      if (nconv < 0)
	{
	  have_more = false;
	  break;
	}
    }

  if (have_more)
    add_elt_to_list (args, type, modifier, num_elts);

  list.resize (num_elts);

  delete buf;
}

printf_format_list::~printf_format_list (void)
{
  int n = list.length ();

  for (int i = 0; i < n; i++)
    {
      printf_format_elt *elt = list(i);
      delete elt;
    }	
}

void
printf_format_list::add_elt_to_list (int args, char type, char modifier,
				     int& num_elts)
{
  if (buf)
    {
      *buf << ends;

      char *text = buf->str ();

      if (text)
	{
	  if (*text)
	    {
	      printf_format_elt *elt
		= new printf_format_elt (text, args, type, modifier);

	      if (num_elts == list.length ())
		list.resize (2 * num_elts);

	      list(num_elts++) = elt;
	    }
	  else
	    delete [] text;
	}

      delete buf;
      buf = 0;
    }
}

void
printf_format_list::process_conversion (const std::string& s, int& i, int n,
					int& args, char& modifier,
					char& type, int& num_elts)
{
  args = 0;
  modifier = '\0';
  type = '\0';

  *buf << s[i++];

  bool next = false;

  while (i < n)
    {
      switch (s[i])
	{
	case '-': case '+': case ' ': case '0': case '#':
	  *buf << s[i++];
	  break;

	default:
	  next = true;
	  break;
	}

      if (next)
	break;
    }

  if (i < n)
    {
      if (s[i] == '*')
	{
	  args++;
	  *buf << s[i++];
	}
      else
	{
	  while (i < n && isdigit (s[i]))
	    *buf << s[i++];
	}
    }

  if (i < n && s[i] == '.')
    {
      *buf << s[i++];

      if (i < n)
	{
	  if (s[i] == '*')
	    {
	      args++;
	      *buf << s[i++];
	    }
	  else
	    {
	      while (i < n && isdigit (s[i]))
		*buf << s[i++];
	    }
	}
    }

  if (i < n)
    {
      switch (s[i])
	{
	case 'h': case 'l': case 'L':
	  modifier = s[i];
	  *buf << s[i++];
	  break;

	default:
	  break;
	}
    }

  if (i < n)
    finish_conversion (s, i, args, modifier, type, num_elts);
  else
    nconv = -1;
}

void
printf_format_list::finish_conversion (const std::string& s, int& i,
				       int args, char modifier,
				       char& type, int& num_elts)

{
  switch (s[i])
    {
    case 'd': case 'i': case 'o': case 'x': case 'X':
    case 'u': case 'c':
      if (modifier == 'L')
	{
	  nconv = -1;
	  break;
	}
      goto fini;

    case 'f': case 'e': case 'E': case 'g': case 'G':
      if (modifier == 'h' || modifier == 'l')
	{
	  nconv = -1;
	  break;
	}
      goto fini;

    case 's': case 'p': case '%':
      if (modifier != '\0')
	{
	  nconv = -1;
	  break;
	}
      goto fini;

    fini:

      if (s[i] == '%' && args == 0)
	*buf << s[i++];
      else
	{
	  if (s[i] != '%')
	    args++;

	  type = s[i];

	  *buf << s[i++];

	  add_elt_to_list (args, type, modifier, num_elts);

	  nconv++;
	}
      break;

    default:
      nconv = -1;
      break;
    }
}

void
printf_format_list::printme (void) const
{
  int n = list.length ();

  for (int i = 0; i < n; i++)
    {
      printf_format_elt *elt = list(i);

      std::cerr << elt->args<< "\t"
		<< elt->type << "\t"
		<< elt->modifier << "\t"
		<< undo_string_escapes (elt->text) << "\n";
    }
}

int
octave_base_stream::file_number (void)
{
  // Kluge alert!

  if (name () == "stdin")
    return 0;

  if (name () == "stdout")
    return 1;

  if (name () == "stderr")
    return 2;

  int retval = -1;

  std::istream *is = input_stream ();
  std::ostream *os = output_stream ();

  int i_fid = is ? ((filebuf *) (is->rdbuf ()))->fd () : -1;
  int o_fid = os ? ((filebuf *) (os->rdbuf ()))->fd () : -1;

  if (i_fid >= 0)
    {
      if (o_fid >= 0)
	retval = (i_fid == o_fid) ? i_fid : -1;
      else
	retval = i_fid;
    }
  else if (o_fid >= 0)
    retval = o_fid;

  return retval;
}

void
octave_base_stream::error (const std::string& msg)
{
  fail = true;
  errmsg = msg;
}

void
octave_base_stream::clear (void)
{
  fail = false;
  errmsg = "";
}

// Functions that are defined for all input streams (input streams
// are those that define is).

std::string
octave_base_stream::do_gets (int max_len, bool& err,
			     bool strip_newline, const char *fcn)
{
  std::string retval;

  err = false;

  std::istream *isp = input_stream ();

  if (isp)
    {
      std::istream& is = *isp;

      // XXX FIXME XXX -- this should probably be converted to use
      // sstream when that is available.
      std::ostrstream buf;

      int c = 0;
      int count = 0;
      int newline_stripped = 0;

      while (is && (c = is.get ()) != EOF)
	{
	  count++;

	  if (c == '\n')
	    {
	      if (! strip_newline)
		buf << (char) c;
	      else
		newline_stripped = 1;

	      break;
	    }
	  else
	    buf << (char) c;

	  if (max_len > 0 && count == max_len)
	    break;
	}

      if (is.fail ())
	{
	  err = true;
	  std::string msg = fcn;
	  msg.append (": read error");
	  error (msg);
	}
      else if (count == 0 && is.eof ())
	{
	  err = true;
	  std::string msg = fcn;
	  msg.append (": at end of file");
	  error (msg);
	}
      else
	{
	  buf << ends;
	  char *tmp = buf.str ();
	  retval = tmp;
	  delete [] tmp;
	}
    }
  else
    {
      err = true;
      invalid_operation (fcn, "reading");
    }

  return retval;
}

std::string
octave_base_stream::getl (int max_len, bool& err)
{
  return do_gets (max_len, err, true, "fgetl");
}

std::string
octave_base_stream::gets (int max_len, bool& err)
{
  return do_gets (max_len, err, false, "fgets");
}

octave_value
octave_base_stream::read (const Matrix& size,
			  oct_data_conv::data_type dt, int skip,
			  oct_mach_info::float_format flt_fmt, int& count)
{
  Matrix retval;

  count = 0;

  std::istream *isp = input_stream ();

  if (isp)
    {
      std::istream& is = *isp;

      int nr = -1;
      int nc = -1;

      bool ignore;

      get_size (size, nr, nc, ignore, "fread");

      if (! error_state)
	{
	  if (flt_fmt == oct_mach_info::unknown)
	    flt_fmt = float_format ();

	  int tmp = retval.read (is, nr, nc, dt, skip, flt_fmt);

	  if (tmp < 0)
	    error ("fread: read error");
	  else
	    count = tmp;
	}
    }
  else
    invalid_operation ("fread", "reading");

  return retval;
}

template <class T>
void
do_scanf_conv (std::istream& is, const char *fmt, T valptr, Matrix& mval,
	       double *data, int& idx, int& conv_count, int nr,
	       int max_size, bool discard) 
{
  is.scan (fmt, valptr);

  if (is)
    {
      if (idx == max_size && ! discard)
	{
	  max_size *= 2;

	  if (nr > 0)
	    mval.resize (nr, max_size / nr, 0.0);
	  else
	    mval.resize (max_size, 1, 0.0);

	  data = mval.fortran_vec ();
	}

      if (! discard)
	{
	  conv_count++;
	  data[idx++] = *(valptr);
	}
    }
}

template void
do_scanf_conv (std::istream&, const char*, int*, Matrix&, double*, int&,
	       int&, int, int, bool);

template void
do_scanf_conv (std::istream&, const char*, long int*, Matrix&, double*, int&,
	       int&, int, int, bool);

template void
do_scanf_conv (std::istream&, const char*, short int*, Matrix&, double*, int&,
	       int&, int, int, bool);

#if 0
template void
do_scanf_conv (std::istream&, const char*, float*, Matrix&, double*, int&,
	       int&, int, int, bool);
#endif

template void
do_scanf_conv (std::istream&, const char*, double*, Matrix&, double*, int&,
	       int&, int, int, bool);

#define DO_WHITESPACE_CONVERSION() \
  do \
    { \
      int c = EOF; \
 \
      while (is && (c = is.get ()) != EOF && isspace (c)) \
	/* skip whitespace */; \
 \
      if (c != EOF) \
	is.putback (c); \
    } \
  while (0)

#define DO_LITERAL_CONVERSION() \
  do \
    { \
      int c = EOF; \
 \
      int n = strlen (fmt); \
      int i = 0; \
 \
      while (i < n && is && (c = is.get ()) != EOF) \
	{ \
	  if (c == fmt[i]) \
	    { \
	      i++; \
	      continue; \
	    } \
	  else \
	    { \
	      is.putback (c); \
	      break; \
	    } \
	} \
 \
      if (i != n) \
	is.setstate (ios::failbit); \
    } \
  while (0)

#define BEGIN_C_CONVERSION() \
  is.unsetf (ios::skipws); \
 \
  int width = elt->width ? elt->width : 1; \
 \
  char *tmp = new char[width + 1]; \
 \
  int c = EOF; \
  int n = 0; \
 \
  while (is && n < width && (c = is.get ()) != EOF) \
    tmp[n++] = (char) c; \
 \
  tmp[n] = '\0'

// For a `%s' format, skip initial whitespace and then read until the
// next whitespace character.
#define BEGIN_S_CONVERSION() \
  int width = elt->width; \
 \
  char *tmp = 0; \
 \
  do \
    { \
      if (width) \
	{ \
	  tmp = new char [width+1]; \
 \
	  is.scan (fmt, tmp); \
 \
	  tmp[width] = '\0'; \
	} \
      else \
	{ \
	  ostrstream buf; \
 \
	  int c = EOF; \
 \
	  while (is && (c = is.get ()) != EOF && isspace (c)) \
	    /* skip leading whitespace */; \
 \
	  if (is && c != EOF) \
	    buf << (char) c; \
 \
	  while (is && (c = is.get ()) != EOF && ! isspace (c)) \
	    buf << (char) c; \
 \
	  if (isspace (c)) \
	    is.putback (c); \
 \
	  buf << ends; \
 \
	  tmp = buf.str (); \
	} \
    } \
  while (0)

// This format must match a nonempty sequence of characters.
#define BEGIN_CHAR_CLASS_CONVERSION() \
  int width = elt->width; \
 \
  char *tmp = 0; \
 \
  do \
    { \
      if (width) \
	{ \
	  tmp = new char[width+1]; \
 \
	  is.scan (fmt, tmp); \
 \
	  tmp[width] = '\0'; \
	} \
      else \
	{ \
	  ostrstream buf; \
 \
	  std::string char_class = elt->char_class; \
 \
	  int c = EOF; \
 \
	  if (elt->type == '[') \
	    { \
	      while (is && (c = is.get ()) != EOF \
		     && char_class.find (c) != NPOS) \
		buf << (char) c; \
	    } \
	  else \
	    { \
	      while (is && (c = is.get ()) != EOF \
		     && char_class.find (c) == NPOS) \
		buf << (char) c; \
	    } \
 \
	  if (c != EOF) \
	    is.putback (c); \
 \
	  buf << ends; \
 \
	  tmp = buf.str (); \
 \
	  if (strlen (tmp) == 0) \
	    is.setstate (ios::failbit); \
	} \
    } \
  while (0)

#define FINISH_CHARACTER_CONVERSION() \
  do \
    { \
      width = strlen (tmp); \
 \
      if (is) \
	{ \
	  int i = 0; \
 \
	  if (! discard) \
	    { \
	      conversion_count++; \
 \
	      while (i < width && tmp[i] != '\0') \
		{ \
		  if (data_index == max_size) \
		    { \
		      max_size *= 2; \
 \
		      if (nr > 0) \
			mval.resize (nr, max_size / nr, 0.0); \
		      else \
			{ \
			  if (all_char_conv && one_elt_size_spec) \
			    mval.resize (1, max_size, 0.0); \
			  else \
			    mval.resize (max_size, 1, 0.0); \
			} \
 \
		      data = mval.fortran_vec (); \
		    } \
 \
		  data[data_index++] = tmp[i++]; \
		} \
	    } \
	} \
 \
      delete [] tmp; \
    } \
  while (0)

octave_value
octave_base_stream::do_scanf (scanf_format_list& fmt_list,
			      int nr, int nc, bool one_elt_size_spec,
			      int& conversion_count)
{
  conversion_count = 0;

  int data_index = 0;

  octave_value retval = Matrix ();

  if (nr == 0 || nc == 0)
    {
      if (one_elt_size_spec)
	nc = 0;

      return Matrix (nr, nc, 0.0);
    }

  std::istream *isp = input_stream ();

  bool all_char_conv = fmt_list.all_character_conversions ();

  Matrix mval;
  double *data = 0;
  int max_size = 0;
  int max_conv = 0;

  int final_nr = 0;
  int final_nc = 0;

  if (all_char_conv)
    {
      if (one_elt_size_spec)
	{
	  max_size = 512;
	  mval.resize (1, max_size, 0.0);
	  data = mval.fortran_vec ();

	  if (nr > 0)
	    max_conv = nr;
	}
      else if (nr > 0 && nc > 0)
	{
	  mval.resize (nr, nc, 0.0);
	  data = mval.fortran_vec ();
	  max_size = max_conv = nr * nc;
	}
    }
  else if (nr > 0)
    {
      if (nc > 0)
	{
	  mval.resize (nr, nc, 0.0);
	  data = mval.fortran_vec ();
	  max_size = nr * nc;

	  max_conv = max_size;
	}
      else
	{
	  mval.resize (nr, 32, 0.0);
	  data = mval.fortran_vec ();
	  max_size = nr * 32;
	}
    }
  else
    {
      mval.resize (32, 1, 0.0);
      data = mval.fortran_vec ();
      max_size = 32;
    }

  if (isp)
    {
      std::istream& is = *isp;

      const scanf_format_elt *elt = fmt_list.first ();

      ios::fmtflags flags = is.flags ();

      for (;;)
	{
	  if (elt)
	    {
	      if (max_conv > 0 && conversion_count == max_conv)
		{
		  if (all_char_conv && one_elt_size_spec)
		    {
		      final_nr = 1;
		      final_nc = data_index;
		    }
		  else
		    {
		      final_nr = nr;
		      final_nc = (data_index - 1) / nr + 1;
		    }

		  break;
		}
	      else if (data_index == max_size)
		{
		  max_size *= 2;

		  if (nr > 0)
		    mval.resize (nr, max_size / nr, 0.0);
		  else
		    {
		      if (all_char_conv && one_elt_size_spec)
			mval.resize (1, max_size, 0.0);
		      else
			mval.resize (max_size, 1, 0.0);
		    }

		  data = mval.fortran_vec ();
		}

	      const char *fmt = elt->text;

	      bool discard = elt->discard;

	      switch (elt->type)
		{
		case scanf_format_elt::whitespace_conversion:
		  DO_WHITESPACE_CONVERSION ();
		  break;

		case scanf_format_elt::literal_conversion:
		  DO_LITERAL_CONVERSION ();
		  break;

		case '%':
		  {
		    int dummy;

		    is.scan (fmt, &dummy);
		  }
		  break;

		case 'd': case 'i': case 'o': case 'u': case 'x':
		  {
		    switch (elt->modifier)
		      {
		      case 'h':
			{
			  short int tmp;
			  do_scanf_conv (is, fmt, &tmp, mval, data,
					 data_index, conversion_count,
					 nr, max_size, discard);
			}
			break;

		      case 'l':
			{
			  long int tmp;
			  do_scanf_conv (is, fmt, &tmp, mval, data,
					 data_index, conversion_count,
					 nr, max_size, discard);
			}
			break;

		      default:
			{
			  int tmp;
			  do_scanf_conv (is, fmt, &tmp, mval, data,
					 data_index, conversion_count,
					 nr, max_size, discard);
			}
			break;
		      }
		  }
		  break;

		case 'e': case 'f': case 'g':
		  {
		    double tmp;

		    do_scanf_conv (is, fmt, &tmp, mval, data,
				   data_index, conversion_count,
				   nr, max_size, discard);
		  }
		  break;

		case 'c':
		  {
		    BEGIN_C_CONVERSION ();

		    FINISH_CHARACTER_CONVERSION ();

		    is.setf (flags);
		  }
		  break;

		case 's':
		  {
		    BEGIN_S_CONVERSION ();

		    FINISH_CHARACTER_CONVERSION ();
		  }
		  break;

		case '[': case '^':
		  {
		    BEGIN_CHAR_CLASS_CONVERSION ();

		    FINISH_CHARACTER_CONVERSION ();
		  }
		  break;

		case 'p':
		  error ("fscanf: unsupported format specifier");
		  break;

		default:
		  error ("fscanf: internal format error");
		  break;
		}

	      if (! ok ())
		{
		  break;
		}
	      else if (! is)
		{
		  if (all_char_conv)
		    {
		      if (one_elt_size_spec)
			{
			  final_nr = 1;
			  final_nc = data_index;
			}
		      else if (data_index > nr)
			{
			  final_nr = nr;
			  final_nc = (data_index - 1) / nr + 1;
			}
		      else
			{
			  final_nr = data_index;
			  final_nc = 1;
			}
		    }
		  else if (nr > 0)
		    {
		      if (data_index > nr)
			{
			  final_nr = nr;
			  final_nc = (data_index - 1) / nr + 1;
			}
		      else
			{
			  final_nr = data_index;
			  final_nc = 1;
			}
		    }
		  else
		    {
		      final_nr = data_index;
		      final_nc = 1;
		    }

		  // If it looks like we have a matching failure, then
		  // reset the failbit in the stream state.

		  if (is.rdstate () & ios::failbit)
		    is.clear (is.rdstate () & (~ios::failbit));

		  // XXX FIXME XXX -- is this the right thing to do?

		  if (interactive && name () == "stdin")
		    {
		      is.clear ();

		      // Skip to end of line.

		      bool err;
		      do_gets (-1, err, false, "fscanf");
		    }

		  break;
		}
	    }
	  else
	    {
	      error ("fscanf: internal format error");
	      break;
	    }

	  elt = fmt_list.next ();
	}
    }

  if (ok ())
    {
      mval.resize (final_nr, final_nc, 0.0);

      retval = mval;

      if (all_char_conv)
	retval = retval.convert_to_str ();
    }

  return retval;
}

octave_value
octave_base_stream::scanf (const string& fmt, const Matrix& size,
			   int& count)
{
  octave_value retval = Matrix ();

  count = 0;

  std::istream *isp = input_stream ();

  if (isp)
    {
      std::istream& is = *isp;

      scanf_format_list fmt_list (fmt);

      switch (fmt_list.num_conversions ())
	{
	case -1:
	  ::error ("fscanf: invalid format specified");
	  break;

	case 0:
	  {
	    const scanf_format_elt *elt = fmt_list.first ();

	    if (elt)
	      {
		is.clear ();

		is.scan (elt->text);

		if (! is)
		  {
		    // If it looks like we have a matching failure, then
		    // reset the failbit in the stream state.

		    if (is.rdstate () & ios::failbit)
		      is.clear (is.rdstate () & (~ios::failbit));
		    else
		      error ("fscanf: read error");

		    // XXX FIXME XXX -- is this the right thing to do?

		    if (interactive && name () == "stdin")
		      {
			is.clear ();

			// Skip to end of line.

			bool err;
			do_gets (-1, err, false, "fscanf");
		      }
		  }
	      }
	  }
	  break;

	default:
	  {
	    int nr = -1;
	    int nc = -1;

	    bool one_elt_size_spec;

	    get_size (size, nr, nc, one_elt_size_spec, "fscanf");

	    if (! error_state)
	      retval = do_scanf (fmt_list, nr, nc, one_elt_size_spec, count);
	  }
	  break;
	}
    }
  else
    invalid_operation ("fscanf", "reading");

  return retval;
}

bool
octave_base_stream::do_oscanf (const scanf_format_elt *elt,
			       octave_value& retval)
{
  bool quit = false;

  std::istream *isp = input_stream ();

  if (isp)
    {
      std::istream& is = *isp;

      ios::fmtflags flags = is.flags ();

      if (elt)
	{
	  const char *fmt = elt->text;

	  bool discard = elt->discard;

	  switch (elt->type)
	    {
	    case scanf_format_elt::whitespace_conversion:
	      DO_WHITESPACE_CONVERSION ();
	      break;

	    case scanf_format_elt::literal_conversion:
	      DO_LITERAL_CONVERSION ();
	      break;

	    case '%':
	      {
		int dummy;

		if (! is.scan (fmt, &dummy))
		  quit = true;
	      }
	      break;

	    case 'd': case 'i': case 'o': case 'u': case 'x':
	      {
		int tmp;

		if (is.scan (fmt, &tmp))
		  {
		    if (! discard)
		      retval = static_cast<double> (tmp);
		  }
		else
		  quit = true;
	      }
	      break;

	    case 'e': case 'f': case 'g':
	      {
		double tmp;

		if (is.scan (fmt, &tmp))
		  {
		    if (! discard)
		      retval = tmp;
		  }
		else
		  quit = true;
	      }
	      break;

	    case 'c':
	      {
		BEGIN_C_CONVERSION ();

		if (! discard)
		  retval = tmp;

		delete [] tmp;

		if (! is)
		  quit = true;

		is.setf (flags);
	      }
	      break;

	    case 's':
	      {
		BEGIN_S_CONVERSION ();

		if (! discard)
		  retval = tmp;

		delete [] tmp;

		if (! is)
		  quit = true;
	      }
	      break;

	    case '[': case '^':
	      {
		BEGIN_CHAR_CLASS_CONVERSION ();

		if (! discard)
		  retval = tmp;

		delete [] tmp;

		if (! is)
		  quit = true;
	      }
	      break;

	    case 'p':
	      error ("fscanf: unsupported format specifier");
	      break;

	    default:
	      error ("fscanf: internal format error");
	      break;
	    }
	}

      if (ok () && is.fail ())
	{
	  error ("fscanf: read error");

	  // XXX FIXME XXX -- is this the right thing to do?

	  if (interactive && name () == "stdin")
	    {
	      // Skip to end of line.

	      bool err;
	      do_gets (-1, err, false, "fscanf");
	    }
	}
    }

  return quit;
}

octave_value_list
octave_base_stream::oscanf (const std::string& fmt)
{
  octave_value_list retval;

  std::istream *isp = input_stream ();

  if (isp)
    {
      std::istream& is = *isp;

      scanf_format_list fmt_list (fmt);

      int nconv = fmt_list.num_conversions ();

      switch (nconv)
	{
	case -1:
	  ::error ("fscanf: invalid format specified");
	  break;

	case 0:
	  {
	    const scanf_format_elt *elt = fmt_list.first ();

	    if (elt)
	      {
		is.clear ();

		is.scan (elt->text);

		if (! is)
		  {
		    error ("fscanf: read error");

		    // XXX FIXME XXX -- is this the right thing to do?

		    if (interactive && name () == "stdin")
		      {
			is.clear ();

			// Skip to end of line.

			bool err;
			do_gets (-1, err, false, "fscanf");
		      }
		  }
	      }
	  }
	  break;

	default:
	  {
	    is.clear ();

	    int len = fmt_list.length ();

	    retval.resize (nconv+1, Matrix ());

	    const scanf_format_elt *elt = fmt_list.first ();

	    int num_values = 0;

	    bool quit = false;

	    for (int i = 0; i < len; i++)
	      {
		octave_value tmp;

		quit = do_oscanf (elt, tmp);

		if (quit)
		  break;
		else
		  {
		    if (tmp.is_defined ())
		      retval (num_values++) = tmp;

		    if (! ok ())
		      break;
		    elt = fmt_list.next ();
		  }
	      }

	    retval (nconv) = static_cast<double> (num_values);

	    if (! quit)
	      {
		// Pick up any trailing stuff.
		if (ok () && len > nconv)
		  {
		    octave_value tmp;
		    do_oscanf (elt, tmp);
		  }
	      }
	  }
	  break;
	}
    }
  else
    invalid_operation ("fscanf", "reading");

  return retval;
}

// Functions that are defined for all output streams (output streams
// are those that define os).

int
octave_base_stream::flush (void)
{
  int retval = -1;

  std::ostream *os = output_stream ();

  if (os)
    {
      os->flush ();

      if (os->good ())
	retval = 0;
    }
  else
    invalid_operation ("fflush", "writing");

  return retval;
}

int
octave_base_stream::write (const octave_value& data,
			   oct_data_conv::data_type dt, int skip,
			   oct_mach_info::float_format flt_fmt)
{
  int retval = -1;

  std::ostream *osp = output_stream ();

  if (osp)
    {
      std::ostream& os = *osp;

      Matrix mval = data.matrix_value ();

      if (! error_state)
	{
	  if (flt_fmt == oct_mach_info::unknown)
	    flt_fmt = float_format ();

	  int tmp = mval.write (os, dt, skip, flt_fmt);

	  if (tmp < 0)
	    error ("fwrite: write error");
	  else
	    retval = tmp;
	}
    }
  else
    invalid_operation ("fwrite", "writing");

  return retval;
}

class
printf_value_cache
{
public:

  enum state { ok, list_exhausted, conversion_error };

  printf_value_cache (const octave_value_list& args)
    : values (args), val_idx (0), elt_idx (0),
      n_vals (values.length ()), n_elts (0), data (0),
      curr_state (ok) { }

  ~printf_value_cache (void) { }

  // Get the current value as a double and advance the internal pointer.
  double double_value (void);

  // Get the current value as an int and advance the internal pointer.
  int int_value (void);

  // Get the current value as a string and advance the internal pointer.
  std::string string_value (void);

  operator bool () const { return (curr_state == ok); }

  bool no_more_values (void) { return curr_state == list_exhausted; }

  bool looking_at_string (void);

private:

  const octave_value_list values;
  int val_idx;
  int elt_idx;
  int n_vals;
  int n_elts;
  const double *data;
  Matrix curr_val;
  state curr_state;

  // Must create value cache with values!

  printf_value_cache (void);

  // No copying!

  printf_value_cache (const printf_value_cache&);

  printf_value_cache& operator = (const printf_value_cache&);
};

bool
printf_value_cache::looking_at_string (void)
{
  bool retval = false;

  int idx = -1;

  if (elt_idx == 0)
    idx = val_idx;
  else if (elt_idx >= n_elts)
    idx = val_idx + 1;

  if (idx >= 0 && idx < n_vals)
    {
      octave_value tmp_val = values (idx);

      // An empty string has zero rows and zero columns.

      if (tmp_val.is_string ())
	{
	  int nr = tmp_val.rows ();

	  retval = (nr == 1 || (nr == 0 && tmp_val.columns () == 0));
	}
    }

  return retval;
}

double
printf_value_cache::double_value (void)
{
  double retval = 0.0;

  while (val_idx < n_vals)
    {
      if (! data)
	{
	  octave_value tmp_val = values (val_idx);

	  curr_val = tmp_val.matrix_value ();

	  if (! error_state)
	    {
	      elt_idx = 0;
	      n_elts = curr_val.length ();
	      data = curr_val.data ();
	    }
	  else
	    {
	      curr_state = conversion_error;
	      break;
	    }
	}

      if (elt_idx < n_elts)
	{
	  return data[elt_idx++];
	  break;
	}
      else
	{
	  val_idx++;
	  data = 0;
	  continue;
	}
    }

  curr_state = list_exhausted;

  return retval;
}

int
printf_value_cache::int_value (void)
{
  int retval = 0;

  double dval = double_value ();

  if (! error_state)
    {
      if (D_NINT (dval) == dval)
	retval = NINT (dval);
      else
	curr_state = conversion_error;
    }

  return retval;
}

std::string
printf_value_cache::string_value (void)
{
  std::string retval;

  if (looking_at_string ())
    {
      if (elt_idx != 0)
	{
	  val_idx++;
	  elt_idx = 0;
	  data = 0;
	}

      retval = values (val_idx++).string_value ();
    }
  else
    curr_state = conversion_error;

  return retval;
}

// Ugh again and again.

template <class T>
void
do_printf_conv (std::ostream& os, const char *fmt, int nsa, int sa_1,
		int sa_2, bool have_arg, T arg)
{
  switch (nsa)
    {
    case 2:
      if (have_arg)
	os.form (fmt, sa_1, sa_2, arg);
      else
	os.form (fmt, sa_1, sa_2);
      break;

    case 1:
      if (have_arg)
	os.form (fmt, sa_1, arg);
      else
	os.form (fmt, sa_1);
      break;

    case 0:
      if (have_arg)
	os.form (fmt, arg);
      else
	os.form (fmt);
      break;

    default:
      ::error ("fprintf: internal error handling format");
      break;
    }
}

template void
do_printf_conv (std::ostream&, const char*, int, int, int, bool, int);

template void
do_printf_conv (std::ostream&, const char*, int, int, int, bool, long);

template void
do_printf_conv (std::ostream&, const char*, int, int, int, bool, double);

template void
do_printf_conv (std::ostream&, const char*, int, int, int, bool, const char*);

int
octave_base_stream::do_printf (printf_format_list& fmt_list,
			       const octave_value_list& args)
{
  int retval = -1;

  std::ostream *osp = output_stream ();

  if (osp)
    {
      std::ostream& os = *osp;

      const printf_format_elt *elt = fmt_list.first ();

      printf_value_cache val_cache (args);

      for (;;)
	{
	  if (elt)
	    {
	      int args = elt->args;
	      int nsa = args;

	      int doing_percent = elt->type == '%';

	      if (args > 0 && ! doing_percent)
		nsa--;

	      int sa_1 = 0;
	      int sa_2 = 0; 

	      if (nsa > 0)
		{
		  sa_1 = val_cache.int_value ();

		  if (! val_cache)
		    break;
		  else
		    {
		      if (nsa > 1)
			{
			  sa_2 = val_cache.int_value ();

			  if (! val_cache)
			    break;
			}
		    }
		}

	      const char *fmt = elt->text;

	      if (doing_percent || args == 0)
		do_printf_conv (os, fmt, nsa, sa_1, sa_2, false, 0.0);
	      else
		{
		  if (elt->type == 's' && val_cache.looking_at_string ())
		    {
		      std::string val = val_cache.string_value ();

		      if (val_cache)
			do_printf_conv (os, fmt, nsa, sa_1, sa_2, true,
					val.c_str ());
		      else
			break;
		    }
		  else
		    {
		      double val = val_cache.double_value ();

		      if (val_cache)
			{
			  switch (elt->type)
			    {
			    case 'd': case 'i': case 'o': case 'x':
			    case 'X': case 'u': case 'c':
			      {
				if (elt->modifier == 'l')
				  do_printf_conv (os, fmt, nsa, sa_1,
						  sa_2, true,
						  static_cast<long> (val));
				else
				  do_printf_conv (os, fmt, nsa, sa_1,
						  sa_2, true,
						  static_cast<int> (val));
			      }
			      break;

			    case 'f': case 'e': case 'E':
			    case 'g': case 'G':
			      do_printf_conv (os, fmt, nsa, sa_1,
					      sa_2, true, val);
			      break;

			    default:
			      error ("fprintf: invalid format specifier");
			      return -1;
			      break;
			    }
			}
		      else
			break;
		    }

		  if (val_cache.no_more_values ())
		    {
		      retval = 0;
		      break;
		    }
		}

	      if (os)
		retval += nsa + (doing_percent ? 0 : 1);
	      else
		{
		  error ("fprintf: write error");
		  retval = -1;
		  break;
		}
	    }
	  else
	    {
	      ::error ("fprintf: internal error handling format");
	      retval = -1;
	      break;
	    }

	  elt = fmt_list.next ();
	}
    }

  return retval;
}

int
octave_base_stream::printf (const std::string& fmt, const octave_value_list& args)
{
  int retval = -1;

  std::ostream *osp = output_stream ();

  if (osp)
    {
      std::ostream& os = *osp;

      printf_format_list fmt_list (fmt);

      switch (fmt_list.num_conversions ())
	{
	case -1:
	  ::error ("fprintf: invalid format specified");
	  break;

	case 0:
	  {
	    const printf_format_elt *elt = fmt_list.first ();

	    if (elt)
	      {
		os.form (elt->text);

		if (os)
		  retval = 0;
		else
		  error ("fprintf: write error");
	      }
	  }
	  break;

	default:
	  {
	    if (args.length () == 0)
	      ::error ("fprintf: no arguments available for specified format");
	    else
	      retval = do_printf (fmt_list, args);
	  }
	  break;
	}
    }
  else
    invalid_operation ("fprintf", "writing");

  return retval;
}

int
octave_base_stream::puts (const std::string& s)
{
  int retval = -1;

  std::ostream *osp = output_stream ();

  if (osp)
    {
      std::ostream& os = *osp;

      os << s;

      if (os)
	{
	  // XXX FIXME XXX -- why does this seem to be necessary?
	  // Without it, output from a loop like
	  //
	  //   for i = 1:100, fputs (stdout, "foo\n"); endfor
	  //
	  // doesn't seem to go to the pager immediately.

	  os.flush ();

	  if (os)
	    retval = 0;
	  else
	    error ("fputs: write error");
	}
      else
	error ("fputs: write error");
    }
  else
    invalid_operation ("fputs", "writing");

  return retval;
}

int
octave_base_stream::rewind (void)
{
  return seek (0, ios::beg);
}

// Return current error message for this stream.

std::string
octave_base_stream::error (bool clear_err, int& err_num)
{
  err_num = fail ? -1 : 0;

  std::string tmp = errmsg;

  if (clear_err)
    clear ();

  return tmp;
}

void
octave_base_stream::invalid_operation (const char *op, const char *rw)
{
  std::string msg = op;
  msg.append (": stream not open for ");
  msg.append (rw);
  error (msg);
}

octave_stream::octave_stream (octave_base_stream *bs = 0)
  : rep (bs)
{
  if (rep)
    rep->count = 1;
}

octave_stream::~octave_stream (void)
{
  if (rep && --rep->count == 0)
    delete rep;
}

octave_stream::octave_stream (const octave_stream& s)
  : rep (s.rep)
{
  if (rep)
    rep->count++;
}

octave_stream&
octave_stream::operator = (const octave_stream& s)
{
  if (rep != s.rep)
    {
      if (rep && --rep->count == 0)
	delete rep;

      rep = s.rep;

      if (rep)
	rep->count++;
    }

  return *this;
}

int
octave_stream::flush (void)
{
  int retval = -1;

  if (stream_ok ("fflush"))
    retval = rep->flush ();

  return retval;
}

std::string
octave_stream::getl (int max_len, bool& err)
{
  std::string retval;

  if (stream_ok ("getl"))
    retval = rep->getl (max_len, err);

  return retval;
}

std::string
octave_stream::getl (const octave_value& tc_max_len, bool& err)
{
  std::string retval;

  err = false;

  int conv_err = 0;

  int max_len = convert_to_valid_int (tc_max_len, conv_err);

  if (conv_err || max_len < 0)
    {
      err = true;
      ::error ("fgetl: invalid maximum length specified");
    }
  else
    retval = getl (max_len, err);

  return retval;
}

std::string
octave_stream::gets (int max_len, bool& err)
{
  std::string retval;

  if (stream_ok ("fgets"))
    retval = rep->gets (max_len, err);

  return retval;
}

std::string
octave_stream::gets (const octave_value& tc_max_len, bool& err)
{
  std::string retval;

  err = false;

  int conv_err = 0;

  int max_len = convert_to_valid_int (tc_max_len, conv_err);

  if (conv_err || max_len < 0)
    {
      err = true;
      ::error ("fgets: invalid maximum length specified");
    }
  else
    retval = gets (max_len, err);

  return retval;
}

int
octave_stream::seek (streamoff offset, ios::seek_dir origin)
{
  int retval = -1;

  if (stream_ok ("fseek"))
    retval = rep->seek (offset, origin);

  return retval;
}

int
octave_stream::seek (const octave_value& tc_offset,
		     const octave_value& tc_origin)
{
  int retval = -1;

  int conv_err = 0;

  int xoffset = convert_to_valid_int (tc_offset, conv_err);

  if (! conv_err)
    {
      ios::seek_dir origin = ios::beg;

      if (tc_origin.is_string ())
	{
	  std::string xorigin = tc_origin.string_value ();

	  if (xorigin == "bof")
	    origin = ios::beg;
	  else if (xorigin == "cof")
	    origin = ios::cur;
	  else if (xorigin == "eof")
	    origin = ios::end;
	  else
	    conv_err = -1;
	}
      else
	{
	  int xorigin = convert_to_valid_int (tc_origin, conv_err);

	  if (! conv_err)
	    {
	      if (xorigin == -1)
		origin = ios::beg;
	      else if (xorigin == 0)
		origin = ios::cur;
	      else if (xorigin == 1)
		origin = ios::end;
	      else
		conv_err = -1;
	    }
	}

      if (! conv_err)
	retval = seek (xoffset, origin);
      else
	error ("fseek: invalid value for origin");
    }
  else
    error ("fseek: invalid value for offset");

  return retval;
}

long
octave_stream::tell (void) const
{
  long retval = -1;

  if (stream_ok ("tell"))
    retval = rep->tell ();

  return retval;
}

int
octave_stream::rewind (void)
{
  int retval = -1;

  if (stream_ok ("frewind"))
    retval = rep->rewind ();

  return retval;
}

bool
octave_stream::is_open (void) const
{
  bool retval = false;

  if (stream_ok ("is_open"))
    retval = rep->is_open ();

  return retval;
}

void
octave_stream::close (void)
{
  if (stream_ok ("close"))
    rep->close ();
}

octave_value
octave_stream::read (const Matrix& size,
		     oct_data_conv::data_type dt, int skip,
		     oct_mach_info::float_format flt_fmt, int& count)
{
  octave_value retval;

  if (stream_ok ("fread"))
    retval = rep->read (size, dt, skip, flt_fmt, count);

  return retval;
}

int
octave_stream::write (const octave_value& data,
		      oct_data_conv::data_type dt, int skip,
		      oct_mach_info::float_format flt_fmt)
{
  int retval = -1;

  if (stream_ok ("fwrite"))
    retval = rep->write (data, dt, skip, flt_fmt);

  return retval;
}

octave_value
octave_stream::scanf (const std::string& fmt, const Matrix& size, int& count)
{
  octave_value retval;

  if (stream_ok ("fscanf"))
    retval = rep->scanf (fmt, size, count);

  return retval;
}

octave_value_list
octave_stream::oscanf (const std::string& fmt)
{
  octave_value_list retval;

  if (stream_ok ("fscanf"))
    retval = rep->oscanf (fmt);

  return retval;
}

int
octave_stream::printf (const std::string& fmt, const octave_value_list& args)
{
  int retval = -1;

  if (stream_ok ("fprintf"))
    retval = rep->printf (fmt, args);

  return retval;
}

int
octave_stream::puts (const std::string& s)
{
  int retval = -1;

  if (stream_ok ("fputs"))
    retval = rep->puts (s);

  return retval;
}

// XXX FIXME XXX -- maybe this should work for string arrays too.

int
octave_stream::puts (const octave_value& tc_s)
{
  int retval = -1;

  if (tc_s.is_string ())
    {
      std::string s = tc_s.string_value ();      
      retval = rep->puts (s);
    }
  else
    error ("fputs: argument must be a string");

  return retval;
}

bool
octave_stream::eof (void) const
{
  int retval = -1;

  if (stream_ok ("feof"))
    retval = rep->eof ();

  return retval;
}

std::string
octave_stream::error (bool clear, int& err_num)
{
  std::string retval;

  if (stream_ok ("ferror", false))
    retval = rep->error (clear, err_num);

  return retval;
}

std::string
octave_stream::name (void) const
{
  std::string retval;

  if (stream_ok ("name"))
    retval = rep->name ();

  return retval;
}

int
octave_stream::mode (void) const
{
  int retval = 0;

  if (stream_ok ("mode"))
    retval = rep->mode ();

  return retval;
}

oct_mach_info::float_format
octave_stream::float_format (void) const
{
  oct_mach_info::float_format retval = oct_mach_info::unknown;

  if (stream_ok ("float_format"))
    retval = rep->float_format ();

  return retval;
}

std::string
octave_stream::mode_as_string (int mode)
{
  std::string retval = "???";

  switch (mode)
    {
    case ios::in:
      retval = "r";
      break;

    case ios::out:
    case ios::out | ios::trunc:
      retval = "w";
      break;

    case ios::out | ios::app:
      retval = "a";
      break;

    case ios::in | ios::out:
      retval = "r+";
      break;

    case ios::in | ios::out | ios::trunc:
      retval = "w+";
      break;

    case ios::in | ios::out | ios::app:
      retval = "a+";
      break;

    case ios::in | ios::bin:
      retval = "rb";
      break;

    case ios::out | ios::bin:
    case ios::out | ios::trunc | ios::bin:
      retval = "wb";
      break;

    case ios::out | ios::app | ios::bin:
      retval = "ab";
      break;

    case ios::in | ios::out | ios::bin:
      retval = "r+b";
      break;

    case ios::in | ios::out | ios::trunc | ios::bin:
      retval = "w+b";
      break;

    case ios::in | ios::out | ios::app | ios::bin:
      retval = "a+b";
      break;

    default:
      break;
    }

  return retval;
}

void
octave_stream::invalid_stream_error (const char *op) const
{
  ::error ("%s: attempt to use invalid I/O stream", op);
}

octave_stream_list *octave_stream_list::instance = 0;

bool
octave_stream_list::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    instance = new octave_stream_list ();

  if (! instance)
    {
      ::error ("unable to create stream list object!");

      retval = false;
    }

  return retval;
}

octave_value
octave_stream_list::insert (const octave_stream& os)
{
  return (instance_ok ()) ? instance->do_insert (os) : octave_value (-1.0);
}

octave_stream
octave_stream_list::lookup (int fid, const std::string& who)
{
  return (instance_ok ()) ? instance->do_lookup (fid, who) : octave_stream ();
}

octave_stream
octave_stream_list::lookup (const octave_value& fid, const std::string& who)
{
  return (instance_ok ()) ? instance->do_lookup (fid, who) : octave_stream ();
}

int
octave_stream_list::remove (int fid, const std::string& who)
{
  return (instance_ok ()) ? instance->do_remove (fid, who) : -1;
}

int
octave_stream_list::remove (const octave_value& fid, const std::string& who)
{
  return (instance_ok ()) ? instance->do_remove (fid, who) : -1;
}

void
octave_stream_list::clear (void)
{
  if (instance)
    instance->do_clear ();
}

string_vector
octave_stream_list::get_info (int fid)
{
  return (instance_ok ()) ? instance->do_get_info (fid) : string_vector ();
}

string_vector
octave_stream_list::get_info (const octave_value& fid)
{
  return (instance_ok ()) ? instance->do_get_info (fid) : string_vector ();
}

std::string
octave_stream_list::list_open_files (void)
{
  return (instance_ok ()) ? instance->do_list_open_files () : std::string ();
}

octave_value
octave_stream_list::open_file_numbers (void)
{
  return (instance_ok ())
    ? instance->do_open_file_numbers () : octave_value ();
}

int
octave_stream_list::get_file_number (const octave_value& fid)
{
  return (instance_ok ()) ? instance->do_get_file_number (fid) : -1;
}

octave_value
octave_stream_list::do_insert (const octave_stream& os)
{
  octave_value retval;

  int stream_number = -1;

  // Insert item in first open slot, increasing size of list if
  // necessary.

  for (int i = 0; i < curr_len; i++)
    {
      octave_stream tmp = list(i);

      if (! tmp)
	{
	  list(i) = os;
	  stream_number = i;
	  break;
	}
    }

  if (stream_number < 0)
    {
      int total_len = list.length ();

      if (curr_len == total_len)
	list.resize (total_len * 2);

      list(curr_len) = os;
      stream_number = curr_len;
      curr_len++;
    }

  return octave_value (os, stream_number);
}

static void
gripe_invalid_file_id (int fid, const std::string& who)
{
  if (who.empty ())
    ::error ("invalid stream number = %d", fid);
  else
    ::error ("%s: invalid stream number = %d", who.c_str (), fid);
}

octave_stream
octave_stream_list::do_lookup (int fid, const std::string& who) const
{
  octave_stream retval;

  if (fid >= 0 && fid < curr_len)
    retval = list(fid);
  else
    gripe_invalid_file_id (fid, who);

  return retval;
}

octave_stream
octave_stream_list::do_lookup (const octave_value& fid,
			       const std::string& who) const
{
  octave_stream retval;

  int i = get_file_number (fid);

  if (! error_state)
    retval = do_lookup (i, who);

  return retval;
}

int
octave_stream_list::do_remove (int fid, const std::string& who)
{
  int retval = -1;

  // Can't remove stdin (std::cin), stdout (std::cout), or stderr
  // (std::cerr).

  if (fid > 2 && fid < curr_len)
    {
      octave_stream os = list(fid);

      if (os.is_valid ())
	{
	  os.close ();
	  list(fid) = octave_stream ();
	  retval = 0;
	}
      else
	gripe_invalid_file_id (fid, who);
    }
  else
    gripe_invalid_file_id (fid, who);

  return retval;
}

int
octave_stream_list::do_remove (const octave_value& fid, const std::string& who)
{
  int retval = -1;

  int i = get_file_number (fid);

  if (! error_state)
    retval = do_remove (i, who);

  return retval;
}

void
octave_stream_list::do_clear (void)
{
  // Do flush stdout and stderr.

  list(0) . flush ();
  list(1) . flush ();

  // But don't delete them or stdin.

  for (int i = 3; i < curr_len; i++)
    list(i) = octave_stream ();
}

string_vector
octave_stream_list::do_get_info (int fid) const
{
  string_vector retval;

  octave_stream os = do_lookup (fid);

  if (os.is_valid ())
    {
      retval.resize (3);

      retval(0) = os.name ();
      retval(1) = octave_stream::mode_as_string (os.mode ());
      retval(2) = oct_mach_info::float_format_as_string (os.float_format ());
    }
  else
    ::error ("invalid file id = %d", fid);

  return retval;
}

string_vector
octave_stream_list::do_get_info (const octave_value& fid) const
{
  string_vector retval;

  int conv_err = 0;

  int int_fid = convert_to_valid_int (fid, conv_err);

  if (! conv_err)
    retval = do_get_info (int_fid);
  else
    ::error ("file id must be a file object or integer value");

  return retval;
}

std::string
octave_stream_list::do_list_open_files (void) const
{
  std::string retval;

  // XXX FIXME XXX -- this should probably be converted to use sstream
  // when that is available.
  std::ostrstream buf;

  buf << "\n"
      << "  number  mode  arch       name\n"
      << "  ------  ----  ----       ----\n";

  for (int i = 0; i < curr_len; i++)
    {
      octave_stream os = list(i);

      if (os)
	{
	  std::string mode = octave_stream::mode_as_string (os.mode ());

	  std::string arch =
	    oct_mach_info::float_format_as_string (os.float_format ());

	  std::string name = os.name ();

	  buf << "  "
	      << setiosflags (ios::right)
	      << setw (4) << i << "     "
	      << setiosflags (ios::left)
	      << setw (3) << mode.c_str () << "  "
	      << setw (9) << arch.c_str () << "  "
	      << name << "\n";
	}
    }

  buf << "\n" << ends;

  char *tmp = buf.str ();

  retval = tmp;

  delete [] tmp;

  return retval;
}

octave_value
octave_stream_list::do_open_file_numbers (void) const
{
  Matrix retval (1, curr_len, 0.0);

  int num_open = 0;

  // Skip stdin, stdout, and stderr.

  for (int i = 3; i < curr_len; i++)
    {
      if (list(i))
	retval (0, num_open++) = i;
    }

  retval.resize ((num_open > 0), num_open);

  return retval;
}

int
octave_stream_list::do_get_file_number (const octave_value& fid) const
{
  int retval = -1;

  if (fid.is_string ())
    {
      std::string nm = fid.string_value ();

      // stdin (std::cin), stdout (std::cout), and stderr (std::cerr)
      // are unnamed.

      for (int i = 3; i < curr_len; i++)
	{
	  octave_stream os = list(i);

	  if (os && os.name () == nm)
	    {
	      retval = i;
	      break;
	    }
	}
    }
  else
    {
      int conv_err = 0;

      int int_fid = convert_to_valid_int (fid, conv_err);

      if (conv_err)
	::error ("file id must be a file object, std::string, or integer value");
      else
	retval = int_fid;
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
