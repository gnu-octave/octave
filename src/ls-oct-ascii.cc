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

// Author: John W. Eaton.

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstring>
#include <cctype>

#include <fstream>
#include <iomanip>
#include <iostream>
#include <string>

#include "byte-swap.h"
#include "data-conv.h"
#include "file-ops.h"
#include "glob-match.h"
#include "lo-mappers.h"
#include "lo-sstream.h"
#include "mach-info.h"
#include "oct-env.h"
#include "oct-time.h"
#include "quit.h"
#include "str-vec.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "load-save.h"
#include "oct-obj.h"
#include "oct-map.h"
#include "ov-cell.h"
#include "pager.h"
#include "pt-exp.h"
#include "symtab.h"
#include "sysdep.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include "version.h"
#include "dMatrix.h"

#include "ls-oct-ascii.h"

// The number of decimal digits to use when writing ascii data.
static int Vsave_precision;

// Functions for reading ascii data.

static Matrix
strip_infnan (const Matrix& m)
{
  int nr = m.rows ();
  int nc = m.columns ();

  Matrix retval (nr, nc);

  int k = 0;
  for (int i = 0; i < nr; i++)
    {
      for (int j = 0; j < nc; j++)
	{
	  double d = m (i, j);
	  if (xisnan (d))
	    goto next_row;
	  else
	    retval (k, j) = xisinf (d) ? (d > 0 ? OCT_RBV : -OCT_RBV) : d;
	}
      k++;

    next_row:
      continue;
    }

  if (k > 0)
    retval.resize (k, nc);

  return retval;
}

// Extract a KEYWORD and its value from stream IS, returning the
// associated value in a new string.
//
// Input should look something like:
//
//  [%#][ \t]*keyword[ \t]*:[ \t]*string-value[ \t]*\n

std::string
extract_keyword (std::istream& is, const char *keyword, const bool next_only)
{
  std::string retval;

  char c;
  while (is.get (c))
    {
      if (c == '%' || c == '#')
	{
	  OSSTREAM buf;
	
	  while (is.get (c) && (c == ' ' || c == '\t' || c == '%' || c == '#'))
	    ; // Skip whitespace and comment characters.

	  if (isalpha (c))
	    buf << c;

	  while (is.get (c) && isalpha (c))
	    buf << c;

	  buf << OSSTREAM_ENDS;
	  const char *tmp = OSSTREAM_C_STR (buf);
	  OSSTREAM_FREEZE (buf);
	  int match = (strncmp (tmp, keyword, strlen (keyword)) == 0);

	  if (match)
	    {
	      OSSTREAM value;
	      while (is.get (c) && (c == ' ' || c == '\t' || c == ':'))
		; // Skip whitespace and the colon.

	      if (c != '\n')
		{
		  value << c;
		  while (is.get (c) && c != '\n')
		    value << c;
		}
	      value << OSSTREAM_ENDS;
	      retval = OSSTREAM_STR (value);
	      OSSTREAM_FREEZE (value);
	      break;
	    }
	  else if (next_only)
	    break;
	}
    }

  int len = retval.length ();

  if (len > 0)
    {
      while (len)
	{
	  c = retval[len-1];

	  if (c == ' ' || c == '\t')
	    len--;
	  else
	    {
	      retval.resize (len);
	      break;
	    }
	}
    }

  return retval;
}

// Match KEYWORD on stream IS, placing the associated value in VALUE,
// returning TRUE if successful and FALSE otherwise.
//
// Input should look something like:
//
//  [%#][ \t]*keyword[ \t]*int-value.*\n

bool
extract_keyword (std::istream& is, const char *keyword, int& value, 
		 const bool next_only)
{
  bool status = false;
  value = 0;

  char c;
  while (is.get (c))
    {
      if (c == '%' || c == '#')
	{
	  OSSTREAM buf;

	  while (is.get (c) && (c == ' ' || c == '\t' || c == '%' || c == '#'))
	    ; // Skip whitespace and comment characters.

	  if (isalpha (c))
	    buf << c;

	  while (is.get (c) && isalpha (c))
	    buf << c;

	  buf << OSSTREAM_ENDS;
	  const char *tmp = OSSTREAM_C_STR (buf);
	  int match = (strncmp (tmp, keyword, strlen (keyword)) == 0);
	  OSSTREAM_FREEZE (buf);

	  if (match)
	    {
	      while (is.get (c) && (c == ' ' || c == '\t' || c == ':'))
		; // Skip whitespace and the colon.

	      is.putback (c);
	      if (c != '\n')
		is >> value;
	      if (is)
		status = true;
	      while (is.get (c) && c != '\n')
		; // Skip to beginning of next line;
	      break;
	    }
	  else if (next_only)
	    break;
	}
    }
  return status;
}

// Match one of the elements in KEYWORDS on stream IS, placing the
// matched keyword in KW and the associated value in VALUE,
// returning TRUE if successful and FALSE otherwise.
//
// Input should look something like:
//
//  [%#][ \t]*keyword[ \t]*int-value.*\n

bool
extract_keyword (std::istream& is, const string_vector& keywords,
		 std::string& kw, int& value, const bool next_only)
{
  bool status = false;
  kw = "";
  value = 0;

  char c;
  while (is.get (c))
    {
      if (c == '%' || c == '#')
	{
	  OSSTREAM buf;

	  while (is.get (c) && (c == ' ' || c == '\t' || c == '%' || c == '#'))
	    ; // Skip whitespace and comment characters.

	  if (isalpha (c))
	    buf << c;

	  while (is.get (c) && isalpha (c))
	    buf << c;

	  buf << OSSTREAM_ENDS;
	  std::string tmp = OSSTREAM_STR (buf);
	  OSSTREAM_FREEZE (buf);

	  for (int i = 0; i < keywords.length (); i++)
	    {
	      int match = (tmp == keywords[i]);

	      if (match)
		{
		  kw = keywords[i];

		  while (is.get (c) && (c == ' ' || c == '\t' || c == ':'))
		    ; // Skip whitespace and the colon.

		  is.putback (c);
		  if (c != '\n')
		    is >> value;
		  if (is)
		    status = true;
		  while (is.get (c) && c != '\n')
		    ; // Skip to beginning of next line;
		  return status;
		}
	    }

	  if (next_only)
	    break;
	}
    }
  return status;
}

// Match one of the elements in KEYWORDS on stream IS, placing the
// matched keyword in KW and the associated value in VALUE,
// returning TRUE if successful and FALSE otherwise.
//
// Input should look something like:
//
//  [%#][ \t]*keyword[ \t]*int-value.*\n

bool
extract_keyword (std::istream& is, const string_vector& keywords,
		 std::string& kw, int& value, const bool next_only)
{
  bool status = false;
  kw = "";
  value = 0;

  char c;
  while (is.get (c))
    {
      if (c == '%' || c == '#')
	{
	  OSSTREAM buf;

	  while (is.get (c) && (c == ' ' || c == '\t' || c == '%' || c == '#'))
	    ; // Skip whitespace and comment characters.

	  if (isalpha (c))
	    buf << c;

	  while (is.get (c) && isalpha (c))
	    buf << c;

	  buf << OSSTREAM_ENDS;
	  std::string tmp = OSSTREAM_STR (buf);
	  OSSTREAM_FREEZE (buf);

	  for (int i = 0; i < keywords.length (); i++)
	    {
	      int match = (tmp == keywords[i]);

	      if (match)
		{
		  kw = keywords[i];

		  while (is.get (c) && (c == ' ' || c == '\t' || c == ':'))
		    ; // Skip whitespace and the colon.

		  is.putback (c);
		  if (c != '\n')
		    is >> value;
		  if (is)
		    status = true;
		  while (is.get (c) && c != '\n')
		    ; // Skip to beginning of next line;
		  return status;
		}
	    }

	  if (next_only)
	    break;
	}
    }
  return status;
}

// Extract one value (scalar, matrix, string, etc.) from stream IS and
// place it in TC, returning the name of the variable.  If the value
// is tagged as global in the file, return TRUE in GLOBAL.
//
// Each type supplies its own function to load the data, and so this
// function is extensible.
//
// FILENAME is used for error messages.
//
// The data is expected to be in the following format:
//
// The input file must have a header followed by some data.
//
// All lines in the header must begin with a `#' character.
//
// The header must contain a list of keyword and value pairs with the
// keyword and value separated by a colon.
//
// Keywords must appear in the following order:
//
// # name: <name>
// # type: <type>
// # <info>
//
// Where, for the built in types are:
//
//  <name> : a valid identifier
//
//  <type> : <typename>
//         | global <typename>
//
//  <typename> : scalar
//             | complex scalar
//             | matrix
//             | complex matrix
//             | bool
//             | bool matrix
//             | string
//             | range
//
//  <info> : <matrix info>
//         | <string info>
//
//  <matrix info> : # rows: <integer>
//                : # columns: <integer>
//
//  <string info> : # elements: <integer>
//                : # length: <integer> (once before each string)
//
//  For backward compatibility the type "string array" is treated as a
// "string" type. Also "string" can have a single element with no elements
// line such that
//
//  <string info> : # length: <integer>
//
// Formatted ASCII data follows the header.
//
// Example:
//
//  # name: foo
//  # type: matrix
//  # rows: 2
//  # columns: 2
//    2  4
//    1  3
//
// Example:
//
//  # name: foo
//  # type: string
//  # elements: 5
//  # length: 4
//  this
//  # length: 2
//  is
//  # length: 1
//  a
//  # length: 6
//  string
//  # length: 5
//  array
//
// XXX FIXME XXX -- this format is fairly rigid, and doesn't allow for
// arbitrary comments.  Someone should fix that. It does allow arbitrary
// types however.

// Ugh.  The signature of the compare method is not standard in older
// versions of the GNU libstdc++.  Do this instead:

#define SUBSTRING_COMPARE_EQ(s, pos, n, t) (s.substr (pos, n) == t)

std::string
read_ascii_data (std::istream& is, const std::string& filename, bool& global,
		 octave_value& tc, int count)
{
  // Read name for this entry or break on EOF.

  std::string name = extract_keyword (is, "name");

  if (name.empty ())
    {
      if (count == 0)
	error ("load: empty name keyword or no data found in file `%s'",
	       filename.c_str ());

      return std::string ();
    }

  if (name == CELL_ELT_TAG)
    {
      // This is OK -- name won't be used.
    }
  else if (! valid_identifier (name))
    {
      error ("load: bogus identifier `%s' found in file `%s'",
	     name.c_str (), filename.c_str ());
      return std::string ();
    }

  // Look for type keyword.

  std::string tag = extract_keyword (is, "type");

  if (! tag.empty ())
    {
      std::string typ;
      size_t pos = tag.rfind (' ');

      if (pos != NPOS)
	{
	  global = SUBSTRING_COMPARE_EQ (tag, 0, 6, "global");

	  typ = global ? tag.substr (7) : tag;
	}
      else
	typ = tag;

      // Special case for backward compatiablity. A small bit of cruft
      if (SUBSTRING_COMPARE_EQ (typ, 0, 12, "string array"))
	tc = octave_value (charMatrix (), true);
      else
	tc = octave_value_typeinfo::lookup_type (typ);

      if (! tc.load_ascii (is))
	error ("load: trouble reading ascii file `%s'", filename.c_str ());
    }
  else
    error ("load: failed to extract keyword specifying value type");

  if (error_state)
    {
      error ("load: reading file %s", filename.c_str ());
      return std::string ();
    }

  return name;
}

// Save the data from TC along with the corresponding NAME, and global
// flag MARK_AS_GLOBAL on stream OS in the plain text format described
// above for load_ascii_data.  If NAME is empty, the name: line is not
// generated.  PRECISION specifies the number of decimal digits to print. 
// If STRIP_NAN_AND_INF is TRUE, rows containing NaNs are deleted,
// and Infinite values are converted to +/-OCT_RBV (A Real Big Value,
// but not so big that gnuplot can't handle it when trying to compute
// axis ranges, etc.).
//
// Assumes ranges and strings cannot contain Inf or NaN values.
//
// Returns 1 for success and 0 for failure.

// XXX FIXME XXX -- should probably write the help string here too.

bool
save_ascii_data (std::ostream& os, const octave_value& val_arg,
		 const std::string& name, bool& infnan_warned,
		 bool strip_nan_and_inf, bool mark_as_global,
		 int precision)
{
  bool success = true;

  if (! precision)
    precision = Vsave_precision;

  if (! name.empty ())
    os << "# name: " << name << "\n";

  long old_precision = os.precision ();
  os.precision (precision);

  octave_value val = val_arg;

  if (mark_as_global)
    os << "# type: global " << val.type_name () << "\n";
  else
    os << "# type: " << val.type_name() << "\n";

  success = val . save_ascii(os, infnan_warned, strip_nan_and_inf);

  os.precision (old_precision);

  return (os && success);
}

bool
save_ascii_data_for_plotting (std::ostream& os, const octave_value& t,
			      const std::string& name)
{
  bool infnan_warned = true;

  return save_ascii_data (os, t, name, infnan_warned, true, false, 0);
}

// Maybe this should be a static function in tree-plot.cc?

// If TC is matrix, save it on stream OS in a format useful for
// making a 3-dimensional plot with gnuplot.  If PARAMETRIC is
// TRUE, assume a parametric 3-dimensional plot will be generated.

bool
save_three_d (std::ostream& os, const octave_value& tc, bool parametric)
{
  bool fail = false;

  int nr = tc.rows ();
  int nc = tc.columns ();

  if (tc.is_real_matrix ())
    {
      os << "# 3D data...\n"
	 << "# type: matrix\n"
	 << "# total rows: " << nr << "\n"
	 << "# total columns: " << nc << "\n";

      if (parametric)
	{
	  int extras = nc % 3;
	  if (extras)
	    warning ("ignoring last %d columns", extras);

	  Matrix tmp = tc.matrix_value ();
	  tmp = strip_infnan (tmp);
	  nr = tmp.rows ();

	  for (int i = 0; i < nc-extras; i += 3)
	    {
	      os << tmp.extract (0, i, nr-1, i+2);
	      if (i+3 < nc-extras)
		os << "\n";
	    }
	}
      else
	{
	  Matrix tmp = tc.matrix_value ();
	  tmp = strip_infnan (tmp);
	  nr = tmp.rows ();

	  for (int i = 0; i < nc; i++)
	    {
	      os << tmp.extract (0, i, nr-1, i);
	      if (i+1 < nc)
		os << "\n";
	    }
	}
    }
  else
    {
      ::error ("for now, I can only save real matrices in 3D format");
      fail = true;
    }

  return (os && ! fail);
}

static int
save_precision (void)
{
  double val;
  if (builtin_real_scalar_variable ("save_precision", val)
      && ! xisnan (val))
    {
      int ival = NINT (val);
      if (ival >= 0 && ival == val)
	{
	  Vsave_precision = ival;
	  return 0;
	}
    }
  gripe_invalid_value_specified ("save_precision");
  return -1;
}

void
symbols_of_ls_oct_ascii (void)
{
  DEFVAR (save_precision, 15.0, save_precision,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} save_precision\n\
This variable specifies the number of digits to keep when saving data in\n\
text format.  The default value is 17.\n\
@end defvr");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

