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
// HDF5 support by Steven G. Johnson <stevenj@alum.mit.edu>
// Matlab v5 support by James R. Van Zandt <jrv@vanzandt.mv.com>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>
#include <cstring>
#include <cctype>

#include <fstream>
#include <iomanip>
#include <iostream>
#include <memory>
#include <string>

#ifdef HAVE_HDF5
#include <hdf5.h>
#endif

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

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "load-save.h"
#include "oct-obj.h"
#include "oct-map.h"
#include "pager.h"
#include "pt-exp.h"
#include "symtab.h"
#include "sysdep.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include "version.h"
#include "dMatrix.h"

#define PAD(l) (((l)<=4)?4:(((l)+7)/8)*8)
#define TAGLENGTH(l) ((l)<=4?4:8)

// Write octave-core file if Octave crashes or is killed by a signal.
static bool Vcrash_dumps_octave_core;

// The default output format.  May be one of "binary", "text",
// "mat-binary", or "hdf5".
static std::string Vdefault_save_format;

// The format string for the comment line at the top of text-format
// save files.  Passed to strftime.  Should begin with `#' and contain
// no newline characters.
static std::string Vsave_header_format_string;

// The number of decimal digits to use when writing ascii data.
static int Vsave_precision;

// Used when converting Inf to something that gnuplot can read.

#ifndef OCT_RBV
#define OCT_RBV DBL_MAX / 100.0
#endif

enum load_save_format
  {
    LS_ASCII,
    LS_BINARY,
    LS_MAT_ASCII,
    LS_MAT_BINARY,
    LS_MAT5_BINARY,
#ifdef HAVE_HDF5
    LS_HDF5,
#endif /* HAVE_HDF5 */
    LS_UNKNOWN
  };

  enum arrayclasstype
  {
    mxCELL_CLASS=1,		// cell array
    mxSTRUCT_CLASS,		// structure
    mxOBJECT_CLASS,		// object
    mxCHAR_CLASS,		// character array
    mxSPARSE_CLASS,		// sparse array
    mxDOUBLE_CLASS,		// double precision array
    mxSINGLE_CLASS,		// single precision floating point
    mxINT8_CLASS,		// 8 bit signed integer
    mxUINT8_CLASS,		// 8 bit unsigned integer
    mxINT16_CLASS,		// 16 bit signed integer
    mxUINT16_CLASS,		// 16 bit unsigned integer
    mxINT32_CLASS,		// 32 bit signed integer
    mxUINT32_CLASS		// 32 bit unsigned integer
  };

enum mat5_data_type
  {
    miINT8=1,			// 8 bit signed
    miUINT8,			// 8 bit unsigned
    miINT16,			// 16 bit signed
    miUINT16,			// 16 bit unsigned
    miINT32,			// 32 bit signed
    miUINT32,			// 32 bit unsigned
    miSINGLE,			// IEEE 754 single precision float
    miRESERVE1,
    miDOUBLE,			// IEEE 754 double precision float
    miRESERVE2,
    miRESERVE3,
    miINT64,			// 64 bit signed
    miUINT64,			// 64 bit unsigned
    miMATRIX			// MATLAB array
  };

#ifdef HAVE_HDF5
// this is only used for HDF5 import
// try to convert s into a valid identifier, replacing invalid chars with "_":
static void
make_valid_identifier (char *s)
{
  if (s)
    {
      for (; *s; ++s)
	if (! (isalnum (*s) || *s == '_'))
	  *s = '_';
    }
}
#endif /* HAVE_HDF5 */

// XXX FIXME XXX -- shouldn't this be implemented in terms of other
// functions that are already available?

// Install a variable with name NAME and the value specified TC in the
// symbol table.  If FORCE is TRUE, replace any existing definition
// for NAME.  If GLOBAL is TRUE, make the variable global.
//
// Assumes TC is defined.

static void
install_loaded_variable (int force, const std::string& name,
			 const octave_value& val,
			 int global, const std::string& doc)
{
  // Is there already a symbol by this name?  If so, what is it?

  symbol_record *lsr = curr_sym_tab->lookup (name);

  bool is_undefined = true;
  bool is_variable = false;
  bool is_function = false;
  bool is_global = false;

  if (lsr)
    {
      is_undefined = ! lsr->is_defined ();
      is_variable = lsr->is_variable ();
      is_function = lsr->is_function ();
      is_global = lsr->is_linked_to_global ();
    }

  symbol_record *sr = 0;

  if (global)
    {
      if (is_global || is_undefined)
	{
	  if (force || is_undefined)
	    {
	      lsr = curr_sym_tab->lookup (name, true);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: global variable name `%s' exists",
		       name.c_str ());
	      warning ("use `load -force' to overwrite");
	    }
	}
      else if (is_function)
	{
	  if (force)
	    {
	      lsr = curr_sym_tab->lookup (name, true);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: `%s' is currently a function in this scope",
		       name.c_str ());
	      warning ("`load -force' will load variable and hide function");
	    }
	}
      else if (is_variable)
	{
	  if (force)
	    {
	      lsr = curr_sym_tab->lookup (name, true);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: local variable name `%s' exists",
		       name.c_str ());
	      warning ("use `load -force' to overwrite");
	    }
	}
      else
	error ("load: unable to load data for unknown symbol type");
    }
  else
    {
      if (is_global)
	{
	  if (force || is_undefined)
	    {
	      lsr = curr_sym_tab->lookup (name, true);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: global variable name `%s' exists",
		       name.c_str ());
	      warning ("use `load -force' to overwrite");
	    }
	}
      else if (is_function)
	{
	  if (force)
	    {
	      lsr = curr_sym_tab->lookup (name, true);
	      link_to_global_variable (lsr);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: `%s' is currently a function in this scope",
		       name.c_str ());
	      warning ("`load -force' will load variable and hide function");
	    }
	}
      else if (is_variable || is_undefined)
	{
	  if (force || is_undefined)
	    {
	      lsr = curr_sym_tab->lookup (name, true);
	      sr = lsr;
	    }
	  else
	    {
	      warning ("load: local variable name `%s' exists",
		       name.c_str ());
	      warning ("use `load -force' to overwrite");
	    }
	}
      else
	error ("load: unable to load data for unknown symbol type");
    }

  if (sr)
    {
      sr->define (val);
      sr->document (doc);
      return;
    }
  else
    error ("load: unable to load variable `%s'", name.c_str ());

  return;
}

// Functions for reading ascii data.

// Skip white space and comments on stream IS.

static void
skip_comments (std::istream& is)
{
  char c = '\0';
  while (is.get (c))
    {
      if (c == ' ' || c == '\t' || c == '\n')
	; // Skip whitespace on way to beginning of next line.
      else
	break;
    }

  for (;;)
    {
      if (is && (c == '%' || c == '#'))
	while (is.get (c) && c != '\n')
	  ; // Skip to beginning of next line, ignoring everything.
      else
	break;
    }
}

// Extract a KEYWORD and its value from stream IS, returning the
// associated value in a new string.
//
// Input should look something like:
//
//  [%#][ \t]*keyword[ \t]*:[ \t]*string-value[ \t]*\n

static std::string
extract_keyword (std::istream& is, const char *keyword)
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
	}
    }

  int len = retval.length ();

  if (len > 0)
    {
      while (len)
	{
	  char c = retval[len-1];

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

static bool
extract_keyword (std::istream& is, const char *keyword, int& value)
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
	}
    }
  return status;
}

// Extract one value (scalar, matrix, string, etc.) from stream IS and
// place it in TC, returning the name of the variable.  If the value
// is tagged as global in the file, return TRUE in GLOBAL.
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
// Where:
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
//             | string
//             | range
//             | string array
//
//  <info> : <matrix info>
//         | <string info>
//         | <string array info>
//
//  <matrix info> : # rows: <integer>
//                : # columns: <integer>
//
//  <string info> : # length: <integer>
//
//  <string array info> : # elements: <integer>
//                      : # length: <integer> (once before each string)
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
//  # type: string array
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
// arbitrary comments, etc.  Someone should fix that.

// Ugh.  The signature of the compare method is not standard in older
// versions of the GNU libstdc++.  Do this instead:

#define SUBSTRING_COMPARE_EQ(s, pos, n, t) (s.substr (pos, n) == t)

static std::string
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

  if (! valid_identifier (name))
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

      if (SUBSTRING_COMPARE_EQ (typ, 0, 6, "scalar"))
	{
	  double tmp = octave_read_double (is);
	  if (is)
	    tc = tmp;
	  else
	    error ("load: failed to load scalar constant");
	}
      else if (SUBSTRING_COMPARE_EQ (typ, 0, 6, "matrix"))
	{
	  int nr = 0;
	  int nc = 0;

	  if (extract_keyword (is, "rows", nr) && nr >= 0
	      && extract_keyword (is, "columns", nc) && nc >= 0)
	    {
	      if (nr > 0 && nc > 0)
		{
		  Matrix tmp (nr, nc);
		  is >> tmp;
		  if (is)
		    tc = tmp;
		  else
		    error ("load: failed to load matrix constant");
		}
	      else if (nr == 0 || nc == 0)
		tc = Matrix (nr, nc);
	      else
		panic_impossible ();
	    }
	  else
	    error ("load: failed to extract number of rows and columns");
	}
      else if (SUBSTRING_COMPARE_EQ (typ, 0, 14, "complex scalar"))
	{
	  Complex tmp = octave_read_complex (is);
	  if (is)
	    tc = tmp;
	  else
	    error ("load: failed to load complex scalar constant");
	}
      else if (SUBSTRING_COMPARE_EQ (typ, 0, 14, "complex matrix"))
	{
	  int nr = 0;
	  int nc = 0;

	  if (extract_keyword (is, "rows", nr) && nr > 0
	      && extract_keyword (is, "columns", nc) && nc > 0)
	    {
	      ComplexMatrix tmp (nr, nc);
	      is >> tmp;
	      if (is)
		tc = tmp;
	      else
		error ("load: failed to load complex matrix constant");
	    }
	  else
	    error ("load: failed to extract number of rows and columns");
	}
      else if (SUBSTRING_COMPARE_EQ (typ, 0, 12, "string array"))
	{
	  int elements;
	  if (extract_keyword (is, "elements", elements) && elements >= 0)
	    {
	      // XXX FIXME XXX -- need to be able to get max length
	      // before doing anything.

	      charMatrix chm (elements, 0);
	      int max_len = 0;
	      for (int i = 0; i < elements; i++)
		{
		  int len;
		  if (extract_keyword (is, "length", len) && len >= 0)
		    {
		      OCTAVE_LOCAL_BUFFER (char, tmp, len+1);

		      if (len > 0 && ! is.read (X_CAST (char *, tmp), len))
			{
			  error ("load: failed to load string constant");
			  break;
			}
		      else
			{
			  tmp [len] = '\0';
			  if (len > max_len)
			    {
			      max_len = len;
			      chm.resize (elements, max_len, 0);
			    }
			  chm.insert (tmp, i, 0);
			}
		    }
		  else
		    error ("load: failed to extract string length for element %d", i+1);
		}

	      if (! error_state)
		tc = octave_value (chm, true);
	    }
	  else
	    error ("load: failed to extract number of string elements");
	}
      else if (SUBSTRING_COMPARE_EQ (typ, 0, 6, "string"))
	{
	  int len;
	  if (extract_keyword (is, "length", len) && len >= 0)
	    {
	      OCTAVE_LOCAL_BUFFER (char, tmp, len+1);

	      if (len > 0 && ! is.read (X_CAST (char *, tmp), len))
		{
		  error ("load: failed to load string constant");
		}
	      else
		{
		  tmp [len] = '\0';

		  if (is)
		    tc = tmp;
		  else
		    error ("load: failed to load string constant");
		}
	    }
	  else
	    error ("load: failed to extract string length");
	}
      else if (SUBSTRING_COMPARE_EQ (typ, 0, 5, "range"))
	{
	  // # base, limit, range comment added by save ().

	  skip_comments (is);
	  Range tmp;
	  is >> tmp;
	  if (is)
	    tc = tmp;
	  else
	    error ("load: failed to load range constant");
	}
      else
	error ("load: unknown constant type `%s'", tag.c_str ());
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

// Extract one value (scalar, matrix, string, etc.) from stream IS and
// place it in TC, returning the name of the variable.  If the value
// is tagged as global in the file, return TRUE in GLOBAL.  If SWAP
// is TRUE, swap bytes after reading.
//
// The data is expected to be in the following format:
//
// Header (one per file):
// =====================
//
//   object               type            bytes
//   ------               ----            -----
//   magic number         string             10
//
//   float format         integer             1  
//
//
// Data (one set for each item):
// ============================
//
//   object               type            bytes
//   ------               ----            -----
//   name_length          integer             4
//
//   name                 string    name_length
//
//   doc_length           integer             4
//
//   doc                  string     doc_length
//
//   global flag          integer             1
//
//   data type            integer             1
//
//   data (one of):
//
//     scalar:
//       data             real                8
//
//     complex scalar:
//       data             complex            16
//
//     matrix:
//       rows             integer             4
//       columns          integer             4
//       data             real            r*c*8
//
//     complex matrix:
//       rows             integer             4
//       columns          integer             4
//       data             complex        r*c*16
//
//     string:
//       length           int                 4
//       data             string         length
//
//     range:
//       base             real                8
//       limit            real                8
//       increment        real                8
//
//     string array
//       elements         int                 4
//
//       for each element:
//         length         int                 4
//         data           string         length
//
// FILENAME is used for error messages.

static std::string
read_binary_data (std::istream& is, bool swap,
		  oct_mach_info::float_format fmt,
		  const std::string& filename, bool& global,
		  octave_value& tc, std::string& doc)
{
  std::string retval;

  char tmp = 0;

  FOUR_BYTE_INT name_len = 0;
  FOUR_BYTE_INT doc_len = 0;

  doc.resize (0);

  // We expect to fail here, at the beginning of a record, so not
  // being able to read another name should not result in an error.

  is.read (X_CAST (char *, &name_len), 4);
  if (! is)
    return retval;
  if (swap)
    swap_4_bytes (X_CAST (char *, &name_len));

  {
    OCTAVE_LOCAL_BUFFER (char, name, name_len+1);
    name[name_len] = '\0';
    if (! is.read (X_CAST (char *, name), name_len))
      goto data_read_error;
    retval = name;
  }

  is.read (X_CAST (char *, &doc_len), 4);
  if (! is)
    goto data_read_error;
  if (swap)
    swap_4_bytes (X_CAST (char *, &doc_len));

  {
    OCTAVE_LOCAL_BUFFER (char, tdoc, doc_len+1);
    tdoc[doc_len] = '\0';
    if (! is.read (X_CAST (char *, tdoc), doc_len))
      goto data_read_error;
    doc = tdoc;
  }

  if (! is.read (X_CAST (char *, &tmp), 1))
    goto data_read_error;
  global = tmp ? 1 : 0;

  tmp = 0;
  if (! is.read (X_CAST (char *, &tmp), 1))
    goto data_read_error;

  switch (tmp)
    {
    case 1:
      {
	if (! is.read (X_CAST (char *, &tmp), 1))
	  goto data_read_error;
	double dtmp;
	read_doubles (is, &dtmp, X_CAST (save_type, tmp), 1, swap, fmt);
	if (error_state || ! is)
	  goto data_read_error;
	tc = dtmp;
      }
      break;

    case 2:
      {
	FOUR_BYTE_INT nr, nc;
	if (! is.read (X_CAST (char *, &nr), 4))
	  goto data_read_error;
	if (swap)
	  swap_4_bytes (X_CAST (char *, &nr));
	if (! is.read (X_CAST (char *, &nc), 4))
	  goto data_read_error;
	if (swap)
	  swap_4_bytes (X_CAST (char *, &nc));
	if (! is.read (X_CAST (char *, &tmp), 1))
	  goto data_read_error;
	Matrix m (nr, nc);
	double *re = m.fortran_vec ();
	int len = nr * nc;
	read_doubles (is, re, X_CAST (save_type, tmp), len, swap, fmt);
	if (error_state || ! is)
	  goto data_read_error;
	tc = m;
      }
      break;

    case 3:
      {
	if (! is.read (X_CAST (char *, &tmp), 1))
	  goto data_read_error;
	Complex ctmp;
	read_doubles (is, X_CAST (double *, &ctmp),
		      X_CAST (save_type, tmp), 2, swap, fmt);
	if (error_state || ! is)
	  goto data_read_error;
	tc = ctmp;
      }
      break;

    case 4:
      {
	FOUR_BYTE_INT nr, nc;
	if (! is.read (X_CAST (char *, &nr), 4))
	  goto data_read_error;
	if (swap)
	  swap_4_bytes (X_CAST (char *, &nr));
	if (! is.read (X_CAST (char *, &nc), 4))
	  goto data_read_error;
	if (swap)
	  swap_4_bytes (X_CAST (char *, &nc));
	if (! is.read (X_CAST (char *, &tmp), 1))
	  goto data_read_error;
	ComplexMatrix m (nr, nc);
	Complex *im = m.fortran_vec ();
	int len = nr * nc;
	read_doubles (is, X_CAST (double *, im),
		      X_CAST (save_type, tmp), 2*len, swap, fmt);
	if (error_state || ! is)
	  goto data_read_error;
	tc = m;
      }
      break;

    case 5:
      {
	FOUR_BYTE_INT len;
	if (! is.read (X_CAST (char *, &len), 4))
	  goto data_read_error;
	if (swap)
	  swap_4_bytes (X_CAST (char *, &len));
	OCTAVE_LOCAL_BUFFER (char, s, len+1);
	if (! is.read (X_CAST (char *, s), len))
	  goto data_read_error;
	s[len] = '\0';
	tc = s;
      }
      break;

    case 6:
      {
	if (! is.read (X_CAST (char *, &tmp), 1))
	  goto data_read_error;
	double bas, lim, inc;
	if (! is.read (X_CAST (char *, &bas), 8))
	  goto data_read_error;
	if (swap)
	  swap_8_bytes (X_CAST (char *, &bas));
	if (! is.read (X_CAST (char *, &lim), 8))
	  goto data_read_error;
	if (swap)
	  swap_8_bytes (X_CAST (char *, &lim));
	if (! is.read (X_CAST (char *, &inc), 8))
	  goto data_read_error;
	if (swap)
	  swap_8_bytes (X_CAST (char *, &inc));
	Range r (bas, lim, inc);
	tc = r;
      }
      break;

    case 7:
      {
	FOUR_BYTE_INT elements;
	if (! is.read (X_CAST (char *, &elements), 4))
	  goto data_read_error;
	if (swap)
	  swap_4_bytes (X_CAST (char *, &elements));
	charMatrix chm (elements, 0);
	int max_len = 0;
	for (int i = 0; i < elements; i++)
	  {
	    FOUR_BYTE_INT len;
	    if (! is.read (X_CAST (char *, &len), 4))
	      goto data_read_error;
	    if (swap)
	      swap_4_bytes (X_CAST (char *, &len));
	    OCTAVE_LOCAL_BUFFER (char, tmp, len+1);
	    if (! is.read (X_CAST (char *, tmp), len))
	      goto data_read_error;
	    if (len > max_len)
	      {
		max_len = len;
		chm.resize (elements, max_len, 0);
	      }
	    tmp [len] = '\0';
	    chm.insert (tmp, i, 0);
	  }
	tc = octave_value (chm, true);
      }
      break;

    default:
    data_read_error:
      error ("load: trouble reading binary file `%s'", filename.c_str ());
      break;
    }

  return retval;
}

// HDF5 input/output

#ifdef HAVE_HDF5

// Define this to 1 if/when HDF5 supports automatic conversion between
// integer and floating-point binary data:
#define HAVE_HDF5_INT2FLOAT_CONVERSIONS 0

// first, we need to define our own dummy stream subclass, since
// HDF5 needs to do its own file i/o

// hdf5_fstreambase is used for both input and output streams, modeled
// on the fstreambase class in <fstream.h>

class hdf5_fstreambase : virtual public std::ios
{
public:

  // HDF5 uses an "id" to refer to an open file
  hid_t file_id;

  // keep track of current item index in the file
  int current_item;

  hdf5_fstreambase () { file_id = -1; }

  hdf5_fstreambase (const char *name, int mode, int prot = 0)
    {
      if (mode == std::ios::in)
	file_id = H5Fopen (name, H5F_ACC_RDONLY, H5P_DEFAULT);
      else if (mode == std::ios::out)
	file_id = H5Fcreate (name, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

      if (file_id < 0)
	std::ios::setstate (std::ios::badbit);

      current_item = 0;
    }

  void close ()
    { 
      if (file_id >= 0)
	{
	  if (H5Fclose (file_id) < 0)
	    std::ios::setstate (std::ios::badbit);
	  file_id = -1;
	}
    }

  void open (const char *name, int mode, int prot = 0)
    {
      clear ();

      if (mode == std::ios::in)
	file_id = H5Fopen (name, H5F_ACC_RDONLY, H5P_DEFAULT);
      else if (mode == std::ios::out)
	file_id = H5Fcreate (name, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

      if (file_id < 0)
	std::ios::setstate (std::ios::badbit);

      current_item = 0;
    }
};

// input and output streams, subclassing istream and ostream
// so that we can pass them for stream parameters in the functions below.

class hdf5_ifstream : public hdf5_fstreambase, public std::istream
{
public:

  hdf5_ifstream () : hdf5_fstreambase (), std::istream (0) { }

  hdf5_ifstream (const char *name, int mode = std::ios::in, int prot = 0)
    : hdf5_fstreambase (name, mode, prot), std::istream (0) { }

  void open (const char *name, int mode = std::ios::in, int prot = 0)
    { hdf5_fstreambase::open (name, mode, prot); }
};

class hdf5_ofstream : public hdf5_fstreambase, public std::ostream
{
public:

  hdf5_ofstream () : hdf5_fstreambase (), std::ostream (0) { }

  hdf5_ofstream (const char *name, int mode = std::ios::out, int prot = 0)
    : hdf5_fstreambase (name, mode, prot), std::ostream (0) { }

  void open (const char *name, int mode = std::ios::out, int prot = 0)
    { hdf5_fstreambase::open (name, mode, prot); }
};

// Given two compound types t1 and t2, determine whether they 
// are compatible for reading/writing.  This function only
// works for non-nested types composed of simple elements (ints, floats...),
// which is all we need it for

bool
hdf5_types_compatible (hid_t t1, hid_t t2)
{
  int n;
  if ((n = H5Tget_nmembers (t1)) != H5Tget_nmembers (t2))
    return false;

  for (int i = 0; i < n; ++i)
    {
      hid_t mt1 = H5Tget_member_type (t1, i);
      hid_t mt2 = H5Tget_member_type (t2, i);

      if (H5Tget_class (mt1) != H5Tget_class (mt2))
	return false;

      H5Tclose (mt2);
      H5Tclose (mt1);
    }

  return true;
}

// Import a multidimensional (rank >= 3) dataset whose id is data_id, into tc.
// This works by calling itself recursively, building up lists of lists
//  of lists ... of 2d matrices.  rank and dims are the rank and dimensions
//  of the dataset.  type_id is the datatype to read into.  If it is
//  H5T_NATIVE_DOUBLE, we are reading a real matrix.  Otherwise, type_id
//  is assumed to be a complex type for reading a complex matrix.
//
//  Upon entry, we should have curdim = rank - 1, start = an array
//  of length rank = all zeros, and count = an array of length rank =
//  all ones except for the first two dimensions which equal the corresponding
//  entries in dims[]. 
//
//  Note that we process the dimensions in reverse order, reflecting
//  the fact that Octave is uses column-major (Fortran-order) data while
//  HDF5 is row-major.  This means that the HDF5 file is read
//  non-contiguously, but on the other hand means that for a 3d array
//  we get a list of xy-plane slices, which seems nice.  We could change
//  this behavior without much trouble; what is the best thing to do?
//
//  Returns a positive value upon success.

static herr_t
hdf5_import_multidim (hid_t data_id, hid_t space_id, hsize_t rank,
		      const hsize_t *dims, hsize_t curdim,
		      hssize_t *start, const hsize_t *count,
		      hid_t type_id, octave_value &tc)
{
  herr_t retval = 1;

  if (rank < 3 || curdim < 1 || curdim >= rank)
    return -1;

  if (curdim == 1)
    {
      // import 2d dataset for 1st 2 dims directly as a matrix
      int nr, nc;    // rows and columns
      nc = dims[0];  // octave uses column-major & HDF5 uses row-major
      nr = dims[1];

      hid_t mem_space_id = H5Screate_simple (2, dims, 0);

      if (mem_space_id < 0)
	return -1;

      if (H5Sselect_all (mem_space_id) < 0)
	return -1;
    
      if (H5Sselect_hyperslab (space_id, H5S_SELECT_SET,
			       start, 0, count, 0) < 0)
	{
	  H5Sclose (mem_space_id);
	  return -1;
	}
    
      if (type_id == H5T_NATIVE_DOUBLE)
	{
	  // real matrix
	  Matrix m (nr, nc);
	  double *re = m.fortran_vec ();
	  if (H5Dread (data_id, type_id, mem_space_id, space_id,
		       H5P_DEFAULT, (void *) re) < 0)
	    retval = -1;  // error
	  else
	    tc = m;
	}
      else
	{
	  // assume that we are using complex numbers
	  // complex matrix
	  ComplexMatrix m (nr, nc);
	  Complex *reim = m.fortran_vec ();
	  if (H5Dread (data_id, type_id, mem_space_id, space_id,
		       H5P_DEFAULT, (void *) X_CAST (double *, reim)) < 0)
	    retval = -1;  // error
	  else
	    tc = m;
	}
    
      H5Sclose (mem_space_id);

    }
  else
    {
      octave_value_list lst;

      for (hsize_t i = 0; i < dims[curdim]; ++i)
	{
	  octave_value slice;
	  start[curdim] = i;
	  retval = hdf5_import_multidim (data_id, space_id, rank,
					 dims, curdim-1, start, count,
					 type_id, slice);
	  if (retval < 0)
	    break;
	  lst.append (slice);
	}

      if (retval > 0)
	tc = octave_value (lst);
    }

  return retval;
}

// Return true if loc_id has the attribute named attr_name, and false
// otherwise.

bool
hdf5_check_attr (hid_t loc_id, const char *attr_name)
{
  bool retval = false;

  // we have to pull some shenanigans here to make sure
  // HDF5 doesn't print out all sorts of error messages if we
  // call H5Aopen for a non-existing attribute

  H5E_auto_t err_func;
  void *err_func_data;

  // turn off error reporting temporarily, but save the error
  // reporting function:

  H5Eget_auto (&err_func, &err_func_data);
  H5Eset_auto (0, 0);

  hid_t attr_id = H5Aopen_name (loc_id, attr_name);

  if (attr_id >= 0)
    {
      // successful
      retval = 1;
      H5Aclose (attr_id);
    }

  // restore error reporting:
  H5Eset_auto (err_func, err_func_data);

  return retval;
}

// Callback data structure for passing data to hdf5_read_next_data, below.

struct hdf5_callback_data
{
  // the following fields are set by hdf5_read_data on successful return:

  // the name of the variable
  char *name;

  // whether it is global
  bool global;

  // the value of the variable, in Octave form
  octave_value tc;

  // a documentation string (NULL if none)
  char *doc;

  // the following fields are input to hdf5_read_data:

  // HDF5 rep's of complex and range type
  hid_t complex_type, range_type;

  // whether to try extra hard to import "foreign" data
  bool import;
};

// This variable, set in read_hdf5_data(), tells whether we are using
// a version of HDF5 with a buggy H5Giterate (i.e. which neglects to
// increment the index parameter to the next unread item).
static bool have_h5giterate_bug = false;

// This function is designed to be passed to H5Giterate, which calls it
// on each data item in an HDF5 file.  For the item whose name is NAME in
// the group GROUP_ID, this function sets dv->tc to an Octave representation
// of that item.  (dv must be a pointer to hdf5_callback_data.)  (It also
// sets the other fields of dv).
//
// It returns 1 on success (in which case H5Giterate stops and returns),
// -1 on error, and 0 to tell H5Giterate to continue on to the next item
// (e.g. if NAME was a data type we don't recognize).

static herr_t
hdf5_read_next_data (hid_t group_id, const char *name, void *dv)
{
  hdf5_callback_data *d = (hdf5_callback_data *) dv;
  H5G_stat_t info;
  herr_t retval = 0;
  bool ident_valid = valid_identifier (name);

  OCTAVE_LOCAL_BUFFER (char, vname, strlen (name) + 1);

  strcpy (vname, name);

  if (! ident_valid && d->import)
    {
      // fix the identifier, replacing invalid chars with underscores
      make_valid_identifier (vname);

      // check again (in case vname was null, empty, or some such thing):
      ident_valid = valid_identifier (vname); 
    }

  H5Gget_objinfo (group_id, name, 1, &info);

  if (info.type == H5G_DATASET && ident_valid)
    {
      retval = 1;

      hid_t data_id = H5Dopen (group_id, name);

      if (data_id < 0)
	{
	  retval = data_id;

	  goto done;
	}

      hid_t type_id = H5Dget_type (data_id);

      hid_t type_class_id = H5Tget_class (type_id);

#if HAVE_HDF5_INT2FLOAT_CONVERSIONS
      if (type_class_id == H5T_INTEGER || type_class_id == H5T_FLOAT)
	{
#else
      // hdf5 doesn't (yet) support automatic float/integer conversions
      if (type_class_id == H5T_FLOAT)
	{
#endif
	  // read real matrix or scalar variable

	  hid_t space_id = H5Dget_space (data_id);

	  hsize_t rank = H5Sget_simple_extent_ndims (space_id);

	  if (rank == 0)
	    {
	      // real scalar:
	      double dtmp;
	      if (H5Dread (data_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, 
			   H5P_DEFAULT, (void *) &dtmp) < 0)
		retval = -1;  // error
	      else
		d->tc = dtmp;
	    }
	  else if (rank > 0 && rank <= 2)
	    {
	      // real matrix
	      OCTAVE_LOCAL_BUFFER (hsize_t, dims, rank);
	      OCTAVE_LOCAL_BUFFER (hsize_t, maxdims, rank);

	      H5Sget_simple_extent_dims (space_id, dims, maxdims);

	      int nr, nc;  // rows and columns
	      // octave uses column-major & HDF5 uses row-major
	      nc = dims[0];
	      nr = rank > 1 ? dims[1] : 1;
	      Matrix m (nr, nc);
	      double *re = m.fortran_vec ();
	      if (H5Dread (data_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, 
			   H5P_DEFAULT, (void *) re) < 0)
		retval = -1;  // error
	      else
		d->tc = m;
	    }
	  else if (rank >= 3 && d->import)
	    {
	      OCTAVE_LOCAL_BUFFER (hsize_t, dims, rank);
	      OCTAVE_LOCAL_BUFFER (hsize_t, maxdims, rank);

	      H5Sget_simple_extent_dims (space_id, dims, maxdims);

	      OCTAVE_LOCAL_BUFFER (hssize_t, start, rank);
	      OCTAVE_LOCAL_BUFFER (hsize_t, count, rank);

	      for (hsize_t i = 0; i < rank; ++i)
		{
		  start[i] = 0;
		  count[i] = 1;
		}
	      count[0] = dims[0];
	      count[1] = dims[1];
	      retval = hdf5_import_multidim (data_id, space_id,
					     rank, dims, rank-1,
					     start, count,
					     H5T_NATIVE_DOUBLE, d->tc);
	    }
	  else
	    {
	      warning ("load: can't read %d-dim. hdf5 dataset %s",
		       rank, name);
	      retval = 0;  // skip; we can't read 3+ dimensional datasets
	    }

	  H5Sclose (space_id);
	}
      else if (type_class_id == H5T_STRING)
	{
	  // read string variable
	  hid_t space_id = H5Dget_space (data_id);
	  hsize_t rank = H5Sget_simple_extent_ndims (space_id);

	  if (rank == 0)
	    {
	      // a single string:
	      int slen = H5Tget_size (type_id);
	      if (slen < 0)
		retval = -1;  // error
	      else
		{
		  char *s = new char [slen];
		  // create datatype for (null-terminated) string
		  // to read into:
		  hid_t st_id = H5Tcopy (H5T_C_S1);
		  H5Tset_size (st_id, slen);
		  if (H5Dread (data_id, st_id, H5S_ALL, H5S_ALL, 
			       H5P_DEFAULT, (void *) s) < 0)
		    {
		      delete [] s;
		      retval = -1;  // error
		    }
		  else
		    d->tc = s;

		  H5Tclose (st_id);
		}
	    }
	  else if (rank == 1)
	    {
	      // string vector
	      hsize_t elements, maxdim;
	      H5Sget_simple_extent_dims (space_id, &elements, &maxdim);
	      int slen = H5Tget_size (type_id);
	      if (slen < 0)
		retval = -1;  // error
	      else
		{
		  // hdf5 string arrays store strings of all the
		  // same physical length (I think), which is
		  // slightly wasteful, but oh well.

		  OCTAVE_LOCAL_BUFFER (char, s, elements * slen);

		  // create datatype for (null-terminated) string
		  // to read into:
		  hid_t st_id = H5Tcopy (H5T_C_S1);
		  H5Tset_size (st_id, slen);

		  if (H5Dread (data_id, st_id, H5S_ALL, H5S_ALL, 
			       H5P_DEFAULT, (void *) s) < 0)
		    retval = -1;  // error
		  else
		    {
		      charMatrix chm (elements, slen - 1);
		      for (hsize_t i = 0; i < elements; ++i)
			{
			  chm.insert (s + i*slen, i, 0);
			}
		      d->tc = octave_value (chm, true);
		    }

		  H5Tclose (st_id);
		}
	    }
	  else
	    {
	      warning ("load: can't read %d-dim. hdf5 string vector %s",
		       rank, name); 
	      // skip; we can't read higher-dimensional string vectors
	      retval = 0;
	    }
	}
      else if (type_class_id == H5T_COMPOUND)
	{
	  // check for complex or range data:

	  if (hdf5_types_compatible (type_id, d->complex_type))
	    {
	      // read complex matrix or scalar variable

	      hid_t space_id = H5Dget_space (data_id);
	      hsize_t rank = H5Sget_simple_extent_ndims (space_id);

	      if (rank == 0)
		{
		  // complex scalar:
		  Complex ctmp;
		  if (H5Dread (data_id, d->complex_type, H5S_ALL,
			       H5S_ALL, H5P_DEFAULT,
			       (void *) X_CAST (double *, &ctmp)) < 0)
		    retval = -1;  // error
		  else
		    d->tc = ctmp;
		}
	      else if (rank > 0 && rank <= 2)
		{
		  // complex matrix
		  OCTAVE_LOCAL_BUFFER (hsize_t, dims, rank);
		  OCTAVE_LOCAL_BUFFER (hsize_t, maxdims, rank);
		  H5Sget_simple_extent_dims (space_id, dims, maxdims);
		  int nr, nc;  // rows and columns
		  // octave uses column-major & HDF5 uses row-major
		  nc = dims[0];
		  nr = rank > 1 ? dims[1] : 1;
		  ComplexMatrix m (nr, nc);
		  Complex *reim = m.fortran_vec ();
		  if (H5Dread (data_id, d->complex_type, H5S_ALL,
			       H5S_ALL, H5P_DEFAULT,
			       (void *) X_CAST (double *, reim)) < 0)
		    retval = -1;  // error
		  else
		    d->tc = m;
		}
	      else if (rank >= 3 && d->import)
		{
		  OCTAVE_LOCAL_BUFFER (hsize_t, dims, rank);
		  OCTAVE_LOCAL_BUFFER (hsize_t, maxdims, rank);
		  H5Sget_simple_extent_dims (space_id, dims, maxdims);
		  OCTAVE_LOCAL_BUFFER (hssize_t, start, rank);
		  OCTAVE_LOCAL_BUFFER (hsize_t, count, rank);
		  for (hsize_t i = 0; i < rank; ++i)
		    {
		      start[i] = 0;
		      count[i] = 1;
		    }
		  count[0] = dims[0];
		  count[1] = dims[1];
		  retval = hdf5_import_multidim (data_id, space_id,
						 rank, dims, rank-1,
						 start, count,
						 d->complex_type,
						 d->tc);
		}
	      else
		{
		  warning ("load: can't read %d-dim. hdf5 dataset %s",
			   rank, name);
		  // skip; we can't read 3+ dimensional datasets
		  retval = 0;
		}
	      H5Sclose (space_id);
	    }
	  else if (hdf5_types_compatible (type_id, d->range_type))
	    {
	      // read range variable:
	      hid_t space_id = H5Dget_space (data_id);
	      hsize_t rank = H5Sget_simple_extent_ndims (space_id);

	      if (rank == 0)
		{
		  double rangevals[3];
		  if (H5Dread (data_id, d->range_type, H5S_ALL, H5S_ALL, 
			       H5P_DEFAULT, (void *) rangevals) < 0)
		    retval = -1;  // error
		  else
		    {
		      Range r (rangevals[0], rangevals[1], rangevals[2]);
		      d->tc = r;
		    }
		}
	      else
		{
		  warning ("load: can't read range array `%s' in hdf5 file",
			   name);
		  // skip; we can't read arrays of range variables
		  retval = 0;
		}

	      H5Sclose (space_id);
	    }
	  else
	    {
	      warning ("load: can't read `%s' (unknown compound datatype)",
		       name);
	      retval = 0; // unknown datatype; skip.
	    }
	}
      else
	{
	  warning ("load: can't read `%s' (unknown datatype)", name);
	  retval = 0; // unknown datatype; skip
	}

      H5Tclose (type_id);

      // check for OCTAVE_GLOBAL attribute:
      d->global = hdf5_check_attr (data_id, "OCTAVE_GLOBAL");

      H5Dclose (data_id);
    }
  else if (info.type == H5G_GROUP && ident_valid)
    {
      // read in group as a list or a structure
      retval = 1;

      hid_t subgroup_id = H5Gopen (group_id, name);

      if (subgroup_id < 0)
	{
	  retval = subgroup_id;
	  goto done;
	}

      // an HDF5 group is treated as an octave structure by
      // default (since that preserves name information), and an
      // octave list otherwise.

      bool is_list = hdf5_check_attr (subgroup_id, "OCTAVE_LIST");

      hdf5_callback_data dsub;
      dsub.name = dsub.doc = 0;
      dsub.global = 0;
      dsub.complex_type = d->complex_type;
      dsub.range_type = d->range_type;
      dsub.import = d->import;

      herr_t retval2;
      octave_value_list lst;
      Octave_map m;
      int current_item = 0;
      while ((retval2 = H5Giterate (group_id, name, &current_item,
				    hdf5_read_next_data, &dsub)) > 0)
	{
	  if (is_list)
	    lst.append (dsub.tc);
	  else
	    m [dsub.name] = dsub.tc;

	  if (dsub.name)
	    delete [] dsub.name;

	  if (dsub.doc)
	    delete [] dsub.doc;

	  if (have_h5giterate_bug)
	    current_item++;  // H5Giterate returned the last index processed
	}

      if (retval2 < 0)
	retval = retval2;
      else
	{
	  d->global = hdf5_check_attr (group_id, "OCTAVE_GLOBAL");

	  if (is_list)
	    d->tc = octave_value (lst);
	  else
	    d->tc = m;
	}

      H5Gclose (subgroup_id);
    }
  else if (! ident_valid)
    {
      // should we attempt to handle invalid identifiers by converting
      // bad characters to '_', say?
      warning ("load: skipping invalid identifier `%s' in hdf5 file",
	       name);
    }

 done:

  if (retval < 0)
    error ("load: error while reading hdf5 item %s", name);

  if (retval > 0)
    {
      // get documentation string, if any:
      int comment_length = H5Gget_comment (group_id, name, 0, 0);

      if (comment_length > 1)
	{
	  d->doc = new char[comment_length];
	  H5Gget_comment (group_id, name, comment_length, d->doc);
	}
      else if (strcmp (name, vname) != 0)
	{
	  // the name was changed by import; store the original name
	  // as the documentation string:
	  d->doc = new char [strlen (name) + 1];
	  strcpy (d->doc, name);
	}
      else
	d->doc = 0;

      // copy name (actually, vname):
      d->name = new char [strlen (vname) + 1];
      strcpy (d->name, vname);
    }

  return retval;
}

// The following two subroutines create HDF5 representations of the way
// we will store Octave complex and range types (pairs and triplets of
// floating-point numbers, respectively).  NUM_TYPE is the HDF5 numeric
// type to use for storage (e.g. H5T_NATIVE_DOUBLE to save as 'double').
// Note that any necessary conversions are handled automatically by HDF5.

static hid_t
hdf5_make_complex_type (hid_t num_type)
{
  hid_t type_id = H5Tcreate (H5T_COMPOUND, sizeof (double) * 2);

  H5Tinsert (type_id, "real", 0 * sizeof (double), num_type);
  H5Tinsert (type_id, "imag", 1 * sizeof (double), num_type);

  return type_id;
}

static hid_t
hdf5_make_range_type (hid_t num_type)
{
  hid_t type_id = H5Tcreate (H5T_COMPOUND, sizeof (double) * 3);

  H5Tinsert (type_id, "base", 0 * sizeof (double), num_type);
  H5Tinsert (type_id, "limit", 1 * sizeof (double), num_type);
  H5Tinsert (type_id, "increment", 2 * sizeof (double), num_type);

  return type_id;
}

// Read the next Octave variable from the stream IS, which must really be
// an hdf5_ifstream.  Return the variable value in tc, its doc string
// in doc, and whether it is global in global.  The return value is
// the name of the variable, or NULL if none were found or there was
// and error.  If import is true, we try extra hard to import "foreign"
// datasets (not created by Octave), although we usually do a reasonable
// job anyway.  (c.f. load -import documentation.)
static std::string
read_hdf5_data (std::istream& is,
		const std::string& filename, bool& global,
		octave_value& tc, std::string& doc, bool import)
{
  std::string retval;

  doc.resize (0);

  hdf5_ifstream& hs = (hdf5_ifstream&) is;
  hdf5_callback_data d;

  d.name = 0;
  d.global = 0;
  d.doc = 0;
  d.complex_type = hdf5_make_complex_type (H5T_NATIVE_DOUBLE);
  d.range_type = hdf5_make_range_type (H5T_NATIVE_DOUBLE);
  d.import = import;

  // Versions of HDF5 prior to 1.2.2 had a bug in H5Giterate where it
  // would return the index of the last item processed instead of the
  // next item to be processed, forcing us to increment the index manually.

  unsigned int vers_major, vers_minor, vers_release;

  H5get_libversion (&vers_major, &vers_minor, &vers_release);

  // XXX FIXME XXX -- this test looks wrong.
  have_h5giterate_bug
    = (vers_major < 1
       || (vers_major == 1 && (vers_minor < 2
			       || (vers_minor == 2 && vers_release < 2))));

  herr_t H5Giterate_retval = H5Giterate (hs.file_id, "/", &hs.current_item,
					 hdf5_read_next_data, &d);

  if (have_h5giterate_bug)
    {
      // H5Giterate sets current_item to the last item processed; we want
      // the index of the next item (for the next call to read_hdf5_data)

      hs.current_item++;
    }

  if (H5Giterate_retval > 0)
    {
      global = d.global;
      tc = d.tc;
      if (d.doc)
	doc = d.doc;
    }
  else
    {
      // an error occurred (H5Giterate_retval < 0) or there are no
      // more datasets print an error message if retval < 0?
      // hdf5_read_next_data already printed one, probably.
    }

  H5Tclose (d.complex_type);
  H5Tclose (d.range_type);

  if (d.name)
    retval = d.name;

  return retval;
}

#endif /* HAVE_HDF5 */

static std::string
get_mat_data_input_line (std::istream& is)
{
  std::string retval;

  bool have_data = false;

  do
    {
      retval = "";

      char c;
      while (is.get (c))
	{
	  if (c == '\n')
	    break;

	  if (c == '%' || c == '#')
	    {
	      // skip to end of line
	      while (is.get (c) && c != '\n')
		;

	      break;
	    }

	  if (! is.eof ())
	    {
	      if (! have_data && c != ' ' && c != '\t')
		have_data = true;

	      retval += c;
	    }
	}
    }
  while (! (have_data || is.eof ()));

  return retval;
}

static void
get_lines_and_columns (std::istream& is, const std::string& filename, int& nr, int& nc)
{
  std::streampos pos = is.tellg ();

  int file_line_number = 0;

  nr = 0;
  nc = 0;

  while (is && ! error_state)
    {
      OCTAVE_QUIT;

      std::string buf = get_mat_data_input_line (is);

      file_line_number++;

      size_t beg = buf.find_first_not_of (", \t");

      // If we see a CR as the last character in the buffer, we had a
      // CRLF pair as the line separator.  Any other CR in the text
      // will not be considered as whitespace.

      if (beg != NPOS && buf[beg] == '\r' && beg == buf.length () - 1)
	{
	  // We had a blank line ending with a CRLF.  Handle it the
	  // same as an empty line.
	  beg = NPOS;
	}

      int tmp_nc = 0;

      while (beg != NPOS)
	{
	  tmp_nc++;

	  size_t end = buf.find_first_of (", \t", beg);

	  if (end != NPOS)
	    {
	      beg = buf.find_first_not_of (", \t", end);

	      if (buf[beg] == '\r' && beg == buf.length () - 1)
		{
		  // We had a line with trailing spaces and
		  // ending with a CRLF, so this should look like EOL,
		  // not a new colum.
		  break;
		}
	    }
	  else
	    break;
	}

      if (tmp_nc > 0)
	{
	  if (nc == 0)
	    {
	      nc = tmp_nc;
	      nr++;
	    }
	  else if (nc == tmp_nc)
	    nr++;
	  else
	    error ("load: %s: inconsistent number of columns near line %d",
		   filename.c_str (), file_line_number);
	}
    }

  if (nr == 0 || nc == 0)
    error ("load: file `%s' seems to be empty!", filename.c_str ());

  is.clear ();
  is.seekg (pos, std::ios::beg);
}

// Extract a matrix from a file of numbers only.
//
// Comments are not allowed.  The file should only have numeric values.
//
// Reads the file twice.  Once to find the number of rows and columns,
// and once to extract the matrix.
//
// FILENAME is used for error messages.
//
// This format provides no way to tag the data as global.

static std::string
read_mat_ascii_data (std::istream& is, const std::string& filename,
		     octave_value& tc)
{
  std::string retval;

  std::string varname;

  size_t pos = filename.rfind ('/');

  if (pos != NPOS)
    varname = filename.substr (pos+1);
  else
    varname = filename;

  pos = varname.find ('.');

  if (pos != NPOS)
    varname = varname.substr (0, pos);

  size_t len = varname.length ();
  for (size_t i = 0; i < len; i++)
    {
      char c = varname[i];
      if (! (isalnum (c) || c == '_'))
	varname[i] = '_';
    }

  if (! isalpha (varname[0]))
    varname.insert (0, "X");

  if (valid_identifier (varname))
    {
      int nr = 0;
      int nc = 0;

      int total_count = 0;

      get_lines_and_columns (is, filename, nr, nc);

      OCTAVE_QUIT;

      if (! error_state && nr > 0 && nc > 0)
	{
	  Matrix tmp (nr, nc);

	  if (nr < 1 || nc < 1)
	    is.clear (std::ios::badbit);
	  else
	    {
	      double d;
	      for (int i = 0; i < nr; i++)
		{
		  std::string buf = get_mat_data_input_line (is);

#ifdef HAVE_SSTREAM
		  std::istringstream tmp_stream (buf);
#else
		  std::istrstream tmp_stream (buf.c_str ());
#endif

		  for (int j = 0; j < nc; j++)
		    {
		      OCTAVE_QUIT;

		      d = octave_read_double (tmp_stream);

		      if (tmp_stream || tmp_stream.eof ())
			{
			  tmp.elem (i, j) = d;
			  total_count++;

			  // Skip whitespace and commas.
			  char c;
			  while (1)
			    {
			      tmp_stream >> c;

			      if (! tmp_stream)
				break;

			      if (! (c == ' ' || c == '\t' || c == ','))
				{
				  tmp_stream.putback (c);
				  break;
				}
			    }

			  if (tmp_stream.eof ())
			    break;
			}
		      else
			{
			  error ("load: failed to read matrix from file `%s'",
				 filename.c_str ());

			  return retval;
			}

		    }
		}
	    }

	  if (is || is.eof ())
	    {
	      // XXX FIXME XXX -- not sure this is best, but it works.

	      if (is.eof ())
		is.clear ();

	      int expected = nr * nc;

	      if (expected == total_count)
		{
		  tc = tmp;
		  retval = varname;
		}
	      else
		error ("load: expected %d elements, found %d",
		       expected, total_count);
	    }
	  else
	    error ("load: failed to read matrix from file `%s'",
		   filename.c_str ());
	}
      else
	error ("load: unable to extract matrix size from file `%s'",
	       filename.c_str ());
    }
  else
    error ("load: unable to convert filename `%s' to valid identifier",
	   filename.c_str ());

  return retval;
}

// Read LEN elements of data from IS in the format specified by
// PRECISION, placing the result in DATA.  If SWAP is TRUE, swap
// the bytes of each element before copying to DATA.  FLT_FMT
// specifies the format of the data if we are reading floating point
// numbers.

static void
read_mat_binary_data (std::istream& is, double *data, int precision,
		      int len, bool swap,
		      oct_mach_info::float_format flt_fmt)
{
  switch (precision)
    {
    case 0:
      read_doubles (is, data, LS_DOUBLE, len, swap, flt_fmt);
      break;

    case 1:
      read_doubles (is, data, LS_FLOAT, len, swap, flt_fmt);
      break;

    case 2:
      read_doubles (is, data, LS_INT, len, swap, flt_fmt);
      break;

    case 3:
      read_doubles (is, data, LS_SHORT, len, swap, flt_fmt);
      break;

    case 4:
      read_doubles (is, data, LS_U_SHORT, len, swap, flt_fmt);
      break;

    case 5:
      read_doubles (is, data, LS_U_CHAR, len, swap, flt_fmt);
      break;

    default:
      break;
    }
}

static int
read_mat_file_header (std::istream& is, bool& swap, FOUR_BYTE_INT& mopt, 
		      FOUR_BYTE_INT& nr, FOUR_BYTE_INT& nc,
		      FOUR_BYTE_INT& imag, FOUR_BYTE_INT& len,
		      int quiet = 0)
{
  swap = false;

  // We expect to fail here, at the beginning of a record, so not
  // being able to read another mopt value should not result in an
  // error.

  is.read (X_CAST (char *, &mopt), 4);
  if (! is)
    return 1;

  if (! is.read (X_CAST (char *, &nr), 4))
    goto data_read_error;

  if (! is.read (X_CAST (char *, &nc), 4))
    goto data_read_error;

  if (! is.read (X_CAST (char *, &imag), 4))
    goto data_read_error;

  if (! is.read (X_CAST (char *, &len), 4))
    goto data_read_error;

// If mopt is nonzero and the byte order is swapped, mopt will be
// bigger than we expect, so we swap bytes.
//
// If mopt is zero, it means the file was written on a little endian
// machine, and we only need to swap if we are running on a big endian
// machine.
//
// Gag me.

  if (oct_mach_info::words_big_endian () && mopt == 0)
    swap = true;

  // mopt is signed, therefore byte swap may result in negative value.

  if (mopt > 9999 || mopt < 0)
    swap = true;

  if (swap)
    {
      swap_4_bytes (X_CAST (char *, &mopt));
      swap_4_bytes (X_CAST (char *, &nr));
      swap_4_bytes (X_CAST (char *, &nc));
      swap_4_bytes (X_CAST (char *, &imag));
      swap_4_bytes (X_CAST (char *, &len));
    }

  if (mopt > 9999 || mopt < 0 || imag > 1 || imag < 0)
    {
      if (! quiet)
	error ("load: can't read binary file");
      return -1;
    }

  return 0;

 data_read_error:
  return -1;
}

// We don't just use a cast here, because we need to be able to detect
// possible errors.

static oct_mach_info::float_format
mopt_digit_to_float_format (int mach)
{
  oct_mach_info::float_format flt_fmt = oct_mach_info::unknown;

  switch (mach)
    {
    case 0:
      flt_fmt = oct_mach_info::ieee_little_endian;
      break;

    case 1:
      flt_fmt = oct_mach_info::ieee_big_endian;
      break;

    case 2:
      flt_fmt = oct_mach_info::vax_d;
      break;

    case 3:
      flt_fmt = oct_mach_info::vax_g;
      break;

    case 4:
      flt_fmt = oct_mach_info::cray;
      break;

    default:
      flt_fmt = oct_mach_info::unknown;
      break;
    }

  return flt_fmt;
}

static int
float_format_to_mopt_digit (oct_mach_info::float_format flt_fmt)
{
  int retval = -1;

  switch (flt_fmt)
    {
    case oct_mach_info::ieee_little_endian:
      retval = 0;
      break;

    case oct_mach_info::ieee_big_endian:
      retval = 1;
      break;

    case oct_mach_info::vax_d:
      retval = 2;
      break;

    case oct_mach_info::vax_g:
      retval = 3;
      break;

    case oct_mach_info::cray:
      retval = 4;
      break;

    default:
      break;
    }

  return retval;
}

// Extract one value (scalar, matrix, string, etc.) from stream IS and
// place it in TC, returning the name of the variable.
//
// The data is expected to be in Matlab version 4 .mat format, though
// not all the features of that format are supported.
//
// FILENAME is used for error messages.
//
// This format provides no way to tag the data as global.

static std::string
read_mat_binary_data (std::istream& is, const std::string& filename,
		      octave_value& tc)
{
  std::string retval;

  // These are initialized here instead of closer to where they are
  // first used to avoid errors from gcc about goto crossing
  // initialization of variable.

  Matrix re;
  oct_mach_info::float_format flt_fmt = oct_mach_info::unknown;
  bool swap = false;
  int type = 0;
  int prec = 0;
  int order = 0;
  int mach = 0;
  int dlen = 0;

  FOUR_BYTE_INT mopt, nr, nc, imag, len;

  int err = read_mat_file_header (is, swap, mopt, nr, nc, imag, len);
  if (err)
    {
      if (err < 0)
	goto data_read_error;
      else
	return retval;
    }

  type = mopt % 10;  // Full, sparse, etc.
  mopt /= 10;        // Eliminate first digit.
  prec = mopt % 10;  // double, float, int, etc.
  mopt /= 10;        // Eliminate second digit.
  order = mopt % 10; // Row or column major ordering.
  mopt /= 10;        // Eliminate third digit.
  mach = mopt % 10;  // IEEE, VAX, etc.

  flt_fmt = mopt_digit_to_float_format (mach);

  if (flt_fmt == oct_mach_info::unknown)
    {
      error ("load: unrecognized binary format!");
      return retval;
    }

  if (type != 0 && type != 1)
    {
      error ("load: can't read sparse matrices");
      return retval;
    }

  if (imag && type == 1)
    {
      error ("load: encountered complex matrix with string flag set!");
      return retval;
    }

  // LEN includes the terminating character, and the file is also
  // supposed to include it, but apparently not all files do.  Either
  // way, I think this should work.

  {
    OCTAVE_LOCAL_BUFFER (char, name, len+1);
    name[len] = '\0';
    if (! is.read (X_CAST (char *, name), len))
      goto data_read_error;
    retval = name;

    dlen = nr * nc;
    if (dlen < 0)
      goto data_read_error;

    if (order)
      {
	int tmp = nr;
	nr = nc;
	nc = tmp;
      }

      re.resize (nr, nc);

      read_mat_binary_data (is, re.fortran_vec (), prec, dlen, swap, flt_fmt);

      if (! is || error_state)
	{
	  error ("load: reading matrix data for `%s'", name);
	  goto data_read_error;
	}

      if (imag)
	{
	  Matrix im (nr, nc);

	  read_mat_binary_data (is, im.fortran_vec (), prec, dlen, swap,
				flt_fmt);

	  if (! is || error_state)
	    {
	      error ("load: reading imaginary matrix data for `%s'", name);
	      goto data_read_error;
	    }

	  ComplexMatrix ctmp (nr, nc);

	  for (int j = 0; j < nc; j++)
	    for (int i = 0; i < nr; i++)
	      ctmp (i, j) = Complex (re (i, j), im (i, j));

	  tc = order ? ctmp.transpose () : ctmp;
	}
      else
	tc = order ? re.transpose () : re;

      if (type == 1)
	tc = tc.convert_to_str ();

      return retval;
    }

 data_read_error:
  error ("load: trouble reading binary file `%s'", filename.c_str ());
  return retval;
}

// Read COUNT elements of data from IS in the format specified by TYPE,
// placing the result in DATA.  If SWAP is TRUE, swap the bytes of
// each element before copying to DATA.  FLT_FMT specifies the format
// of the data if we are reading floating point numbers.

static void
read_mat5_binary_data (std::istream& is, double *data,
		       int count, bool swap, mat5_data_type type,
		       oct_mach_info::float_format flt_fmt)
{
  
  switch (type)
    {
    case miINT8:
      read_doubles (is, data, LS_CHAR, count, swap, flt_fmt);
      break;

    case miUINT8:
      read_doubles (is, data, LS_U_CHAR, count, swap, flt_fmt);
      break;

    case miINT16:
      read_doubles (is, data, LS_SHORT, count, swap, flt_fmt);
      break;

    case miUINT16:
      read_doubles (is, data, LS_U_SHORT, count, swap, flt_fmt);
      break;

    case miINT32:
      read_doubles (is, data, LS_INT, count, swap, flt_fmt);
      break;

    case miUINT32:
      read_doubles (is, data, LS_U_INT, count, swap, flt_fmt);
      break;

    case miSINGLE:
      read_doubles (is, data, LS_FLOAT, count, swap, flt_fmt);
      break;

    case miRESERVE1:
      break;

    case miDOUBLE:
      read_doubles (is, data, LS_DOUBLE, count, swap, flt_fmt);
      break;

    case miRESERVE2:
    case miRESERVE3:
      break;

    case miINT64:
#ifdef EIGHT_BYTE_INT
      read_doubles (is, data, LS_LONG, count, swap, flt_fmt);
#endif
      break;

    case miUINT64:
#ifdef EIGHT_BYTE_INT
      read_doubles (is, data, LS_U_LONG, count, swap, flt_fmt);
#endif
      break;

    case miMATRIX:
    default:
      break;
    }
}

// Read one element tag from stream IS, 
// place the type code in TYPE and the byte count in BYTES
// return nonzero on error
static int
read_mat5_tag (std::istream& is, bool swap, int& type, int& bytes)
{
  unsigned int upper;
  FOUR_BYTE_INT temp;

  if (! is.read (X_CAST (char *, &temp), 4 ))
    goto data_read_error;

  if (swap)
    swap_4_bytes ((char *)&temp);

  upper = (temp >> 16) & 0xffff;
  type = temp & 0xffff;

  if (upper)
    {
      // "compressed" format
      bytes = upper;
    }
  else
    {
      if (! is.read (X_CAST (char *, &temp), 4 ))
	goto data_read_error;
      if (swap)
	swap_4_bytes ((char *)&temp);
      bytes = temp;
    }

  return 0;

 data_read_error:
  return 1;
}

// Extract one data element (scalar, matrix, string, etc.) from stream
// IS and place it in TC, returning the name of the variable.
//
// The data is expected to be in Matlab's "Version 5" .mat format,
// though not all the features of that format are supported.
//
// FILENAME is used for error messages.

static std::string
read_mat5_binary_element (std::istream& is, const std::string& filename,
			  bool swap, bool& global, octave_value& tc)
{
  std::string retval;

  // These are initialized here instead of closer to where they are
  // first used to avoid errors from gcc about goto crossing
  // initialization of variable.

  Matrix re;
  oct_mach_info::float_format flt_fmt = oct_mach_info::unknown;
  int type = 0;
  bool imag;
  bool logicalvar;
  enum arrayclasstype arrayclass;
  FOUR_BYTE_INT junk;
  FOUR_BYTE_INT flags;
  FOUR_BYTE_INT nr;
  FOUR_BYTE_INT nc;
  FOUR_BYTE_INT dimension_length;
  int len;
  int element_length;
  std::streampos pos;
  TWO_BYTE_INT number;
  number = *(TWO_BYTE_INT *)"\x00\x01";

  // MAT files always use IEEE floating point
  if ((number == 1) ^ swap)
    flt_fmt = oct_mach_info::ieee_big_endian;
  else
    flt_fmt = oct_mach_info::ieee_little_endian;

  // element type and length
  if (read_mat5_tag (is, swap, type, element_length))
    return retval;			// EOF

  if (type != miMATRIX)
    {
      error ("load: invalid element type");
      goto early_read_error;
    }
  pos = is.tellg ();

  // array flags subelement
  if (read_mat5_tag (is, swap, type, len) || type != miUINT32 || len != 8)
    {
      error ("load: invalid array flags subelement");
      goto early_read_error;
    }

  read_int (is, swap, flags);
  imag = (flags & 0x0800) != 0;	// has an imaginary part?
  global = (flags & 0x0400) != 0; // global variable?
  logicalvar = (flags & 0x0200) != 0; // we don't use this yet
  arrayclass = (arrayclasstype)(flags & 0xff);
  read_int (is, swap, junk);	// an "undefined" entry
  
  // dimensions array subelement
  {
    std::streampos pos;

    if (read_mat5_tag (is, swap, type, dimension_length) || type != miINT32)
      {
	error ("load: invalid dimensions array subelement");
	goto early_read_error;
      }

    pos = is.tellg ();
    read_int (is, swap, nr);
    read_int (is, swap, nc);
    re.resize (nr, nc);

    // delay checking for a multidimensional array until we have read
    // the variable name
    is.seekg (pos + static_cast<std::streamoff> (dimension_length));
  }

  // array name subelement
  {
    std::streampos pos;
      
    if (read_mat5_tag (is, swap, type, len) || type != miINT8)
      {
	error ("load: invalid array name subelement");
	goto early_read_error;
      }

    pos = is.tellg ();
    OCTAVE_LOCAL_BUFFER (char, name, len+1);

    if (len)			// structure field subelements have
				// zero-length array name subelements
      {
	if (! is.read (X_CAST (char *, name), len ))
	  goto data_read_error;
	
	is.seekg (pos + static_cast<std::streamoff> (PAD (len)));
      }

    name[len] = '\0';
    retval = name;
  }  

  if (dimension_length != 8)
    {
      error ("load: multidimension arrays are not implemented");
      goto skip_ahead;
    }

  switch (arrayclass)
    {
    case mxCELL_CLASS:
      warning ("load: cell arrays are not implemented");
      goto skip_ahead;

    case mxOBJECT_CLASS:
      warning ("load: objects are not implemented");
      goto skip_ahead;

    case mxSPARSE_CLASS:
      warning ("load: sparse arrays are not implemented");
      goto skip_ahead;

    case mxSTRUCT_CLASS:
      {
	Octave_map m;
	FOUR_BYTE_INT type;
	FOUR_BYTE_INT len;
	FOUR_BYTE_INT field_name_length;
	int i;

	// field name length subelement -- actually the maximum length
	// of a field name.  The Matlab docs promise this will always
	// be 32.  We read and use the actual value, on the theory
	// that eventually someone will recognize that's a waste of
	// space.
	if (read_mat5_tag (is, swap, type, len) || type != miINT32)
	  {
	    error ("load: invalid field name subelement");
	    goto data_read_error;
	  }

	if (! is.read (X_CAST (char *, &field_name_length), len ))
	  goto data_read_error;

	if (swap)
	  swap_4_bytes ((char *)&field_name_length);

	// field name subelement.  The length of this subelement tells
	// us how many fields there are.
	if (read_mat5_tag (is, swap, type, len) || type != miINT8)
	  {
	    error ("load: invalid field name subelement");
	    goto data_read_error;
	  }

	OCTAVE_LOCAL_BUFFER (char, elname, len);

	if (! is.read (elname, len))
	  goto data_read_error;

	// fields subelements
	for (i = 0; i < len/field_name_length; i++)
	  {
	    octave_value fieldtc;
	    read_mat5_binary_element (is, filename, swap, global, fieldtc);
	    m[elname + i*field_name_length] = fieldtc;
	  }

	tc = m;
      }
      break;

    case mxCHAR_CLASS:
      // handle as a numerical array to start with

    case mxDOUBLE_CLASS:
    case mxSINGLE_CLASS:
    case mxINT8_CLASS:
    case mxUINT8_CLASS:
    case mxINT16_CLASS:
    case mxUINT16_CLASS:
    case mxINT32_CLASS:
    case mxUINT32_CLASS:
    default:
      // handle all these numerical formats as double arrays
      
      // real data subelement
      {
	std::streampos pos;
	
	if (read_mat5_tag (is, swap, type, len))
	  {
	    error ("load: reading matrix data for `%s'", retval.c_str ());
	    goto data_read_error;
	  }

	pos = is.tellg ();
	read_mat5_binary_data (is, re.fortran_vec (), nr*nc, swap,
			       (enum mat5_data_type) type, flt_fmt);

	if (! is || error_state)
	  {
	    error ("load: reading matrix data for `%s'", retval.c_str ());
	    goto data_read_error;
	  }

	is.seekg (pos + static_cast<std::streamoff> (PAD (len)));
      }
      
      // imaginary data subelement
      if (imag)
	{
	  Matrix im (nr, nc);
	  
	  if (read_mat5_tag (is, swap, type, len))
	    {
	      error ("load: reading matrix data for `%s'", retval.c_str ());
	      goto data_read_error;
	    }

	  read_mat5_binary_data (is, im.fortran_vec (), nr*nc, swap,
				 (enum mat5_data_type) type, flt_fmt);

	  if (! is || error_state)
	    {
	      error ("load: reading imaginary matrix data for `%s'",
		     retval.c_str ());
	      goto data_read_error;
	    }

	  ComplexMatrix ctmp (nr, nc);

	  for (int j = 0; j < nc; j++)
	    for (int i = 0; i < nr; i++)
	      ctmp (i, j) = Complex (re (i, j), im (i, j));

	  tc = ctmp;
	}
      else
	tc = re;

      if (arrayclass == mxCHAR_CLASS)
	tc = tc.convert_to_str ();
    }

  is.seekg (pos + static_cast<std::streamoff> (element_length));

  return retval;

 data_read_error:
 early_read_error:
  error ("load: trouble reading binary file `%s'", filename.c_str ());
  return std::string ();

 skip_ahead:
  warning ("skipping over `%s'", retval.c_str ());
  is.seekg (pos + static_cast<std::streamoff> (element_length));
  return read_mat5_binary_element (is, filename, swap, global, tc);
}

static int
read_mat5_binary_file_header (std::istream& is, bool& swap,
			      bool quiet = false)
{
  TWO_BYTE_INT version=0, magic=0;

  is.seekg (124, std::ios::beg);
  is.read (X_CAST (char *, &version), 2);
  is.read (X_CAST (char *, &magic), 2);

  if (magic == 0x4d49)
    swap = 0;
  else if (magic == 0x494d)
    swap = 1;
  else
    {
      if (! quiet)
	error ("load: can't read binary file");
      return -1;
    }

  if (! swap)			// version number is inverse swapped!
    version = ((version >> 8) & 0xff) + ((version & 0xff) << 8);

  if (version != 1 && !quiet)
    warning ("load: found version %d binary MAT file, "
	     "but only prepared for version 1", version);

  return 0;
}

// Return TRUE if NAME matches one of the given globbing PATTERNS.

static bool
matches_patterns (const string_vector& patterns, int pat_idx,
		  int num_pat, const std::string& name)
{
  for (int i = pat_idx; i < num_pat; i++)
    {
      glob_match pattern (patterns[i]);

      if (pattern.match (name))
	return true;
    }

  return false;
}

static int
read_binary_file_header (std::istream& is, bool& swap,
			 oct_mach_info::float_format& flt_fmt,
			 bool quiet = false)
{
  const int magic_len = 10;
  char magic[magic_len+1];
  is.read (X_CAST (char *, magic), magic_len);
  magic[magic_len] = '\0';

  if (strncmp (magic, "Octave-1-L", magic_len) == 0)
    swap = oct_mach_info::words_big_endian ();
  else if (strncmp (magic, "Octave-1-B", magic_len) == 0)
    swap = ! oct_mach_info::words_big_endian ();
  else
    {
      if (! quiet)
	error ("load: can't read binary file");
      return -1;
    }
	
  char tmp = 0;
  is.read (X_CAST (char *, &tmp), 1);

  flt_fmt = mopt_digit_to_float_format (tmp);

  if (flt_fmt == oct_mach_info::unknown)
    {
      if (! quiet)
        error ("load: unrecognized binary format!");

      return -1;
    }

  return 0;
}

static load_save_format
get_file_format (const std::string& fname, const std::string& orig_fname)
{
  load_save_format retval = LS_UNKNOWN;

#ifdef HAVE_HDF5
  // check this before we open the file
  if (H5Fis_hdf5 (fname.c_str ()) > 0)
    return LS_HDF5;
#endif /* HAVE_HDF5 */

  std::ifstream file (fname.c_str ());

  if (! file)
    {
      error ("load: couldn't open input file `%s'", orig_fname.c_str ());
      return retval;
    }

  oct_mach_info::float_format flt_fmt = oct_mach_info::unknown;

  bool swap = false;

  if (read_binary_file_header (file, swap, flt_fmt, true) == 0)
    retval = LS_BINARY;
  else
    {
      file.seekg (0, std::ios::beg);

      FOUR_BYTE_INT mopt, nr, nc, imag, len;

      int err = read_mat_file_header (file, swap, mopt, nr, nc, imag, len, 1);

      if (! err)
	retval = LS_MAT_BINARY;
      else
	{
	  file.clear ();
	  file.seekg (0, std::ios::beg);

	  err = read_mat5_binary_file_header (file, swap, true);

	  if (! err)
  	    {
	      file.clear ();
	      file.seekg (0, std::ios::beg);
	      retval = LS_MAT5_BINARY;
  	    }
  	  else
  	    {
	      file.clear ();
	      file.seekg (0, std::ios::beg);

	      std::string tmp = extract_keyword (file, "name");

	      if (! tmp.empty ())
		retval = LS_ASCII;
	      else
		{
		  // Try reading the file as numbers only, determining the
		  // number of rows and columns from the data.  We don't
		  // even bother to check to see if the first item in the
		  // file is a number, so that get_complete_line() can
		  // skip any comments that might appear at the top of the
		  // file.
		  
		  retval = LS_MAT_ASCII;
		}
	    }
	}
    }

  file.close ();

  if (retval == LS_UNKNOWN)
    error ("load: unable to determine file format for `%s'",
	   orig_fname.c_str ());

  return retval;
}

static octave_value
do_load (std::istream& stream, const std::string& orig_fname, bool force,
	 load_save_format format, oct_mach_info::float_format flt_fmt,
	 bool list_only, bool swap, bool verbose, bool import,
	 const string_vector& argv, int argv_idx, int argc, int nargout)
{
  octave_value retval;

  Octave_map retstruct;

  OSSTREAM output_buf;

  int count = 0;

  for (;;)
    {
      bool global = false;
      octave_value tc;

      std::string name;
      std::string doc;

      switch (format)
	{
	case LS_ASCII:
	  name = read_ascii_data (stream, orig_fname, global, tc, count);
	  break;

	case LS_BINARY:
	  name = read_binary_data (stream, swap, flt_fmt, orig_fname,
				   global, tc, doc);
	  break;

	case LS_MAT_ASCII:
	  name = read_mat_ascii_data (stream, orig_fname, tc);
	  break;

	case LS_MAT_BINARY:
	  name = read_mat_binary_data (stream, orig_fname, tc);
	  break;

#ifdef HAVE_HDF5
	case LS_HDF5:
	  name = read_hdf5_data (stream, orig_fname,
				 global, tc, doc, import);
	  break;
#endif /* HAVE_HDF5 */

	case LS_MAT5_BINARY:
	  name = read_mat5_binary_element (stream, orig_fname, swap,
					   global, tc);
	  break;

	default:
	  gripe_unrecognized_data_fmt ("load");
	  break;
	}

      if (error_state || stream.eof () || name.empty ())
	break;
      else if (! error_state && ! name.empty ())
	{
	  if (tc.is_defined ())
	    {
	      if (format == LS_MAT_ASCII && argv_idx < argc)
		warning ("load: loaded ASCII file `%s' -- ignoring extra args",
			 orig_fname.c_str ());

	      if (format == LS_MAT_ASCII
		  || argv_idx == argc
		  || matches_patterns (argv, argv_idx, argc, name))
		{
		  count++;
		  if (list_only)
		    {
		      if (verbose)
			{
			  if (count == 1)
			    output_buf
			      << "type               rows   cols   name\n"
			      << "====               ====   ====   ====\n";

			  output_buf
			    << std::setiosflags (std::ios::left)
			    << std::setw (16) << tc.type_name () . c_str ()
			    << std::setiosflags (std::ios::right)
			    << std::setw (7) << tc.rows ()
			    << std::setw (7) << tc.columns ()
			    << "   ";
			}
		      output_buf << name << "\n";
		    }
		  else
		    {
		      if (nargout == 1)
			{
			  if (format == LS_MAT_ASCII)
			    retval = tc;
			  else
			    retstruct[name] = tc;
			}
		      else
			install_loaded_variable (force, name, tc, global, doc);
		    }
		}

	      // Only attempt to read one item from a headless text file.

	      if (format == LS_MAT_ASCII)
		break;
	    }
	  else
	    error ("load: unable to load variable `%s'", name.c_str ());
	}
      else
	{
	  if (count == 0)
	    error ("load: are you sure `%s' is an Octave data file?",
		   orig_fname.c_str ());

	  break;
	}
    }

  if (list_only && count)
    {
      output_buf << OSSTREAM_ENDS;
      std::string msg = OSSTREAM_STR (output_buf);
      OSSTREAM_FREEZE (output_buf);

      if (nargout > 0)
	retval = msg;
      else
	octave_stdout << msg;
    }
  else if (! retstruct.empty ())
    retval = retstruct;

  return retval;
}

// HDF5 load/save documentation is included in the Octave manual
// regardless, but if HDF5 is not linked in we also include a
// sentence noting this, so the user understands that the features
// aren't available.  Define a macro for this sentence:

#ifdef HAVE_HDF5
#define HAVE_HDF5_HELP_STRING ""
#else /* ! HAVE_HDF5 */
#define HAVE_HDF5_HELP_STRING "\n\
HDF5 load and save are not available, as this Octave executable was\n\
not linked with the HDF5 library."
#endif /* ! HAVE HDF5 */

DEFCMD (load, args, nargout,
  "-*- texinfo -*-\n\
@deffn {Command} load options file v1 v2 @dots{}\n\
Load the named variables from the file @var{file}.  As with @code{save},\n\
you may specify a list of variables and @code{load} will only extract\n\
those variables with names that match.  For example, to restore the\n\
variables saved in the file @file{data}, use the command\n\
\n\
@example\n\
load data\n\
@end example\n\
\n\
Octave will refuse to overwrite existing variables unless you use the\n\
option @samp{-force}.\n\
\n\
If a variable that is not marked as global is loaded from a file when a\n\
global symbol with the same name already exists, it is loaded in the\n\
global symbol table.  Also, if a variable is marked as global in a file\n\
and a local symbol exists, the local symbol is moved to the global\n\
symbol table and given the value from the file.  Since it seems that\n\
both of these cases are likely to be the result of some sort of error,\n\
they will generate warnings.\n\
\n\
If invoked with a single output argument, Octave returns data instead\n\
of inserting variables in the symbol table.  If the data file contains\n\
only numbers (TAB- or space-delimited columns), a matrix of values is\n\
returned.  Otherwise, @code{load} returns a structure with members\n\
 corresponding to the names of the variables in the file.\n\
\n\
The @code{load} command can read data stored in Octave's text and\n\
binary formats, and @sc{Matlab}'s binary format.  It will automatically\n\
detect the type of file and do conversion from different floating point\n\
formats (currently only IEEE big and little endian, though other formats\n\
may added in the future).\n\
\n\
Valid options for @code{load} are listed in the following table.\n\
\n\
@table @code\n\
@item -force\n\
Force variables currently in memory to be overwritten by variables with\n\
the same name found in the file.\n\
\n\
@item -ascii\n\
Force Octave to assume the file is in Octave's text format.\n\
\n\
@item -binary\n\
Force Octave to assume the file is in Octave's binary format.\n\
\n\
@item -mat-binary\n\
Force Octave to assume the file is in @sc{Matlab}'s binary format.\n\
\n\
@item -mat4-binary\n\
Force Octave to assume the file is in the binary format written by\n\
@sc{Matlab} version 4.\n\
\n\
@item -hdf5\n\
Force Octave to assume the file is in HDF5 format.\n\
(HDF5 is a free, portable binary format developed by the National\n\
Center for Supercomputing Applications at the University of Illinois.)\n\
Note that Octave can read HDF5 files not created by itself, but may\n\
skip some datasets in formats that it cannot support.  In particular,\n\
it will skip datasets of data types that it does not recognize, with\n\
dimensionality > 2, or with names that aren't valid Octave identifiers\n\
See, however, the @samp{-import} option to ameliorate this somewhat.\n"

HAVE_HDF5_HELP_STRING

"\n\
@item -import\n\
Make a stronger attempt to import foreign datasets.  Currently, this means\n\
that for HDF5 files, invalid characters in names are converted to @samp{_},\n\
and datasets with dimensionality > 2 are imported as lists of matrices (or\n\
lists of lists of matrices, or ...).\n\
\n\
@end table\n\
@end deffn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("load");

  if (error_state)
    return retval;

  // It isn't necessary to have the default load format stored in a
  // user preference variable since we can determine the type of file
  // as we are reading.

  load_save_format format = LS_UNKNOWN;

  bool force = false;
  bool list_only = false;
  bool verbose = false;
  bool import = false;

  int i;
  for (i = 1; i < argc; i++)
    {
      if (argv[i] == "-force" || argv[i] == "-f")
	{
	  force = true;
	}
      else if (argv[i] == "-list" || argv[i] == "-l")
	{
	  list_only = true;
	}
      else if (argv[i] == "-verbose" || argv[i] == "-v")
	{
	  verbose = true;
	}
      else if (argv[i] == "-ascii" || argv[i] == "-a")
	{
	  format = LS_ASCII;
	}
      else if (argv[i] == "-binary" || argv[i] == "-b")
	{
	  format = LS_BINARY;
	}
      else if (argv[i] == "-mat-binary" || argv[i] == "-m")
	{
	  format = LS_MAT5_BINARY;
	}
      else if (argv[i] == "-mat4-binary" || argv[i] == "-4" || argv[i] == "-v4")
	{
	  format = LS_MAT_BINARY;
	}
      else if (argv[i] == "-hdf5" || argv[i] == "-h")
	{
#ifdef HAVE_HDF5
	  format = LS_HDF5;
#else /* ! HAVE_HDF5 */
	  error ("load: octave executable was not linked with HDF5 library");
	  return retval;
#endif /* ! HAVE_HDF5 */
	}
      else if (argv[i] == "-import" || argv[i] == "-i")
	{
	  import = true;
	}
      else
	break;
    }

  if (i == argc)
    {
      print_usage ("load");
      return retval;
    }

  std::string orig_fname = argv[i];

  oct_mach_info::float_format flt_fmt = oct_mach_info::unknown;

  bool swap = false;

  if (argv[i] == "-")
    {
      i++;

#ifdef HAVE_HDF5
      if (format == LS_HDF5)
	error ("load: cannot read HDF5 format from stdin");
      else
#endif /* HAVE_HDF5 */
      if (format != LS_UNKNOWN)
	{
	  // XXX FIXME XXX -- if we have already seen EOF on a
	  // previous call, how do we fix up the state of std::cin so
	  // that we can get additional input?  I'm afraid that we
	  // can't fix this using std::cin only.

	  retval = do_load (std::cin, orig_fname, force, format, flt_fmt,
			    list_only, swap, verbose, import, argv, i, argc,
			    nargout);
	}
      else
	error ("load: must specify file format if reading from stdin");
    }
  else
    {
      std::string fname = file_ops::tilde_expand (argv[i]);

      if (format == LS_UNKNOWN)
	format = get_file_format (fname, orig_fname);

#ifdef HAVE_HDF5
      if (format == LS_HDF5)
	{
	  i++;

	  hdf5_ifstream hdf5_file (fname.c_str ());

	  if (hdf5_file.file_id >= 0)
	    {
	      retval = do_load (hdf5_file, orig_fname, force, format,
				flt_fmt, list_only, swap, verbose,
				import, argv, i, argc, nargout);

	      hdf5_file.close ();
	    }
	  else
	    error ("load: couldn't open input file `%s'",
		   orig_fname.c_str ());
	}
      else
#endif /* HAVE_HDF5 */
	// don't insert any statements here; the "else" above has to
	// go with the "if" below!!!!!
      if (format != LS_UNKNOWN)
	{
	  i++;

	  std::ios::openmode mode = std::ios::in;
	  if (format == LS_BINARY ||
	      format == LS_MAT_BINARY ||
	      format == LS_MAT5_BINARY)
	    mode |= std::ios::binary;

	  std::ifstream file (fname.c_str (), mode);

	  if (file)
	    {
	      if (format == LS_BINARY)
		{
		  if (read_binary_file_header (file, swap, flt_fmt) < 0)
		    {
		      file.close ();
		      return retval;
		    }
		}
	      else if (format == LS_MAT5_BINARY)
		{
		  if (read_mat5_binary_file_header (file, swap, false) < 0)
		    {
		      file.close ();
		      return retval;
		    }
		}

	      retval = do_load (file, orig_fname, force, format,
				flt_fmt, list_only, swap, verbose, import,
				argv, i, argc, nargout);
	      file.close ();
	    }
	  else
	    error ("load: couldn't open input file `%s'",
		   orig_fname.c_str ());
	}
    }

  return retval;
}

// Return TRUE if PATTERN has any special globbing chars in it.

static bool
glob_pattern_p (const std::string& pattern)
{
  int open = 0;

  int len = pattern.length ();

  for (int i = 0; i < len; i++)
    {
      char c = pattern[i];

      switch (c)
	{
	case '?':
	case '*':
	  return true;

	case '[':	// Only accept an open brace if there is a close
	  open++;	// brace to match it.  Bracket expressions must be
	  continue;	// complete, according to Posix.2

	case ']':
	  if (open)
	    return true;
	  continue;
	  
	case '\\':
	  if (i == len - 1)
	    return false;

	default:
	  continue;
	}
    }

  return false;
}

// MAX_VAL and MIN_VAL are assumed to have integral values even though
// they are stored in doubles.

static save_type
get_save_type (double max_val, double min_val)
{
  save_type st = LS_DOUBLE;

  if (max_val < 256 && min_val > -1)
    st = LS_U_CHAR;
  else if (max_val < 65536 && min_val > -1)
    st = LS_U_SHORT;
  else if (max_val < 4294967295UL && min_val > -1)
    st = LS_U_INT;
  else if (max_val < 128 && min_val >= -128)
    st = LS_CHAR;
  else if (max_val < 32768 && min_val >= -32768)
    st = LS_SHORT;
  else if (max_val <= 2147483647L && min_val >= -2147483647L)
    st = LS_INT;

  return st;
}

// Save the data from TC along with the corresponding NAME, help
// string DOC, and global flag MARK_AS_GLOBAL on stream OS in the
// binary format described above for read_binary_data.

static bool
save_binary_data (std::ostream& os, const octave_value& tc,
		  const std::string& name, const std::string& doc,
		  bool mark_as_global, bool save_as_floats) 
{
  FOUR_BYTE_INT name_len = name.length ();

  os.write (X_CAST (char *, &name_len), 4);
  os << name;

  FOUR_BYTE_INT doc_len = doc.length ();

  os.write (X_CAST (char *, &doc_len), 4);
  os << doc;

  char tmp;

  tmp = mark_as_global;
  os.write (X_CAST (char *, &tmp), 1);

  if (tc.is_string ())
    {
      tmp = 7;
      os.write (X_CAST (char *, &tmp), 1);
      FOUR_BYTE_INT nr = tc.rows ();
      os.write (X_CAST (char *, &nr), 4);
      charMatrix chm = tc.char_matrix_value ();
      for (int i = 0; i < nr; i++)
	{
	  FOUR_BYTE_INT len = chm.cols ();
	  os.write (X_CAST (char *, &len), 4);
	  std::string tstr = chm.row_as_string (i);
	  const char *tmp = tstr.data ();
	  os.write (X_CAST (char *, tmp), len);
	}
    }
  else if (tc.is_range ())
    {
      tmp = 6;
      os.write (X_CAST (char *, &tmp), 1);
      tmp = (char) LS_DOUBLE;
      os.write (X_CAST (char *, &tmp), 1);
      Range r = tc.range_value ();
      double bas = r.base ();
      double lim = r.limit ();
      double inc = r.inc ();
      os.write (X_CAST (char *, &bas), 8);
      os.write (X_CAST (char *, &lim), 8);
      os.write (X_CAST (char *, &inc), 8);
    }
  else if (tc.is_real_scalar ())
    {
      tmp = 1;
      os.write (X_CAST (char *, &tmp), 1);
      tmp = (char) LS_DOUBLE;
      os.write (X_CAST (char *, &tmp), 1);
      double tmp = tc.double_value ();
      os.write (X_CAST (char *, &tmp), 8);
    }
  else if (tc.is_real_matrix ())
    {
      tmp = 2;
      os.write (X_CAST (char *, &tmp), 1);
      Matrix m = tc.matrix_value ();
      FOUR_BYTE_INT nr = m.rows ();
      FOUR_BYTE_INT nc = m.columns ();
      os.write (X_CAST (char *, &nr), 4);
      os.write (X_CAST (char *, &nc), 4);
      int len = nr * nc;
      save_type st = LS_DOUBLE;
      if (save_as_floats)
	{
	  if (m.too_large_for_float ())
	    {
	      warning ("save: some values too large to save as floats --");
	      warning ("save: saving as doubles instead");
	    }
	  else
	    st = LS_FLOAT;
	}
      else if (len > 8192) // XXX FIXME XXX -- make this configurable.
	{
	  double max_val, min_val;
	  if (m.all_integers (max_val, min_val))
	    st = get_save_type (max_val, min_val);
	}
      const double *mtmp = m.data ();
      write_doubles (os, mtmp, st, len);
    }
  else if (tc.is_complex_scalar ())
    {
      tmp = 3;
      os.write (X_CAST (char *, &tmp), 1);
      tmp = (char) LS_DOUBLE;
      os.write (X_CAST (char *, &tmp), 1);
      Complex tmp = tc.complex_value ();
      os.write (X_CAST (char *, &tmp), 16);
    }
  else if (tc.is_complex_matrix ())
    {
      tmp = 4;
      os.write (X_CAST (char *, &tmp), 1);
      ComplexMatrix m = tc.complex_matrix_value ();
      FOUR_BYTE_INT nr = m.rows ();
      FOUR_BYTE_INT nc = m.columns ();
      os.write (X_CAST (char *, &nr), 4);
      os.write (X_CAST (char *, &nc), 4);
      int len = nr * nc;
      save_type st = LS_DOUBLE;
      if (save_as_floats)
	{
	  if (m.too_large_for_float ())
	    {
	      warning ("save: some values too large to save as floats --");
	      warning ("save: saving as doubles instead");
	    }
	  else
	    st = LS_FLOAT;
	}
      else if (len > 4096) // XXX FIXME XXX -- make this configurable.
	{
	  double max_val, min_val;
	  if (m.all_integers (max_val, min_val))
	    st = get_save_type (max_val, min_val);
	}
      const Complex *mtmp = m.data ();
      write_doubles (os, X_CAST (const double *, mtmp), st, 2*len);
    }
  else
    gripe_wrong_type_arg ("save", tc, false);

  return os;
}

#ifdef HAVE_HDF5

// Add an attribute named attr_name to loc_id (a simple scalar
// attribute with value 1).  Return value is >= 0 on success.
static herr_t
hdf5_add_attr (hid_t loc_id, const char *attr_name)
{
  herr_t retval = 0;

  hid_t as_id = H5Screate (H5S_SCALAR);

  if (as_id >= 0)
    {
      hid_t a_id = H5Acreate (loc_id, attr_name,
			      H5T_NATIVE_UCHAR, as_id, H5P_DEFAULT);

      if (a_id >= 0)
	{
	  unsigned char attr_val = 1;

	  retval = H5Awrite (a_id, H5T_NATIVE_UCHAR, (void*) &attr_val);

	  H5Aclose (a_id);
	}
      else
	retval = a_id;

      H5Sclose (as_id);
    }
  else
    retval = as_id;

  return retval;
}


// save_type_to_hdf5 is not currently used, since hdf5 doesn't yet support
// automatic float<->integer conversions:

#if HAVE_HDF5_INT2FLOAT_CONVERSIONS

// return the HDF5 type id corresponding to the Octave save_type

static hid_t
save_type_to_hdf5 (save_type st)
{
  switch (st)
    {
    case LS_U_CHAR:
      return H5T_NATIVE_UCHAR;

    case LS_U_SHORT:
      return H5T_NATIVE_USHORT;

    case LS_U_INT:
      return H5T_NATIVE_UINT;

    case LS_CHAR:
      return H5T_NATIVE_CHAR;

    case LS_SHORT:
      return H5T_NATIVE_SHORT;

    case LS_INT:
      return H5T_NATIVE_INT;

    case LS_FLOAT:
      return H5T_NATIVE_FLOAT;

    case LS_DOUBLE:
    default:
      return H5T_NATIVE_DOUBLE;
    }
}
#endif /* HAVE_HDF5_INT2FLOAT_CONVERSIONS */

// Add the data from TC to the HDF5 location loc_id, which could
// be either a file or a group within a file.  Return true if
// successful.  This function calls itself recursively for lists
// (stored as HDF5 groups).

static bool
add_hdf5_data (hid_t loc_id, const octave_value& tc,
	       const std::string& name, const std::string& doc,
	       bool mark_as_global, bool save_as_floats)
{
  hsize_t dims[3];
  hid_t type_id = -1, space_id = -1, data_id = -1;
  bool data_is_group = 0;
  bool retval = 0;

  if (tc.is_string ())
    {
      int nr = tc.rows ();
      charMatrix chm = tc.char_matrix_value ();
      int nc = chm.cols ();

      // create datatype for (null-terminated) string to write from:
      type_id = H5Tcopy (H5T_C_S1); H5Tset_size (type_id, nc + 1);
      if (type_id < 0)
	goto error_cleanup;

      dims[0] = nr;
      space_id = H5Screate_simple (nr > 0 ? 1 : 0, dims, (hsize_t*) 0);
      if (space_id < 0)
	goto error_cleanup;

      data_id = H5Dcreate (loc_id, name.c_str (), 
			   type_id, space_id, H5P_DEFAULT);
      if (data_id < 0)
	goto error_cleanup;

      OCTAVE_LOCAL_BUFFER (char, s, nr * (nc + 1));

      for (int i = 0; i < nr; ++i)
	{
	  std::string tstr = chm.row_as_string (i);
	  strcpy (s + i * (nc+1), tstr.c_str ());
	}

      if (H5Dwrite (data_id, type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT,
		    (void*) s) < 0) {
	goto error_cleanup;
      }
    }
  else if (tc.is_range ())
    {
      space_id = H5Screate_simple (0, dims, (hsize_t*) 0);
      if (space_id < 0)
	goto error_cleanup;

      type_id = hdf5_make_range_type (H5T_NATIVE_DOUBLE);
      if (type_id < 0)
	goto error_cleanup;

      data_id = H5Dcreate (loc_id, name.c_str (), 
			   type_id, space_id, H5P_DEFAULT);
      if (data_id < 0)
	goto error_cleanup;
    
      Range r = tc.range_value ();
      double range_vals[3];
      range_vals[0] = r.base ();
      range_vals[1] = r.limit ();
      range_vals[2] = r.inc ();

      if (H5Dwrite (data_id, type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT,
		    (void*) range_vals) < 0)
	goto error_cleanup;
    }
  else if (tc.is_real_scalar ())
    {
      space_id = H5Screate_simple (0, dims, (hsize_t*) 0);
      if (space_id < 0) goto error_cleanup;

      data_id = H5Dcreate (loc_id, name.c_str (), 
			   H5T_NATIVE_DOUBLE, space_id, H5P_DEFAULT);
      if (data_id < 0)
	goto error_cleanup;

      double tmp = tc.double_value ();
      if (H5Dwrite (data_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,
		    H5P_DEFAULT, (void*) &tmp) < 0)
	goto error_cleanup;
    }
  else if (tc.is_real_matrix ())
    {
      Matrix m = tc.matrix_value ();
      dims[1] = m.rows ();    // Octave uses column-major, while
      dims[0] = m.columns (); // HDF5 uses row-major ordering

      space_id = H5Screate_simple (dims[1] > 1 ?2:1, dims, (hsize_t*) 0);
      if (space_id < 0)
	goto error_cleanup;

      hid_t save_type_id = H5T_NATIVE_DOUBLE;

      if (save_as_floats)
	{
	  if (m.too_large_for_float ())
	    {
	      warning ("save: some values too large to save as floats --");
	      warning ("save: saving as doubles instead");
	    }
	  else
	    save_type_id = H5T_NATIVE_FLOAT;
	}
#if HAVE_HDF5_INT2FLOAT_CONVERSIONS
      // hdf5 currently doesn't support float/integer conversions
      else
	{
	  double max_val, min_val;

	  if (m.all_integers (max_val, min_val))
	    save_type_id
	      = save_type_to_hdf5 (get_save_type (max_val, min_val));
	}
#endif /* HAVE_HDF5_INT2FLOAT_CONVERSIONS */

      data_id = H5Dcreate (loc_id, name.c_str (), 
			   save_type_id, space_id, H5P_DEFAULT);
      if (data_id < 0)
	goto error_cleanup;
    
      double *mtmp = m.fortran_vec ();
      if (H5Dwrite (data_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,
		    H5P_DEFAULT, (void*) mtmp) < 0)
	goto error_cleanup;
    }
  else if (tc.is_complex_scalar ())
    {
      space_id = H5Screate_simple (0, dims, (hsize_t*) 0);
      if (space_id < 0)
	goto error_cleanup;

      type_id = hdf5_make_complex_type (H5T_NATIVE_DOUBLE);
      if (type_id < 0)
	goto error_cleanup;

      data_id = H5Dcreate (loc_id, name.c_str (), 
			   type_id, space_id, H5P_DEFAULT);
      if (data_id < 0)
	goto error_cleanup;

      Complex tmp = tc.complex_value ();
      if (H5Dwrite (data_id, type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT,
		    (void*) X_CAST (double*, &tmp)) < 0)
	goto error_cleanup;
    }
  else if (tc.is_complex_matrix ())
    {
      ComplexMatrix m = tc.complex_matrix_value ();

      dims[1] = m.rows ();    // Octave uses column-major, while
      dims[0] = m.columns (); // HDF5 uses row-major ordering

      space_id = H5Screate_simple (dims[1] > 1 ?2:1, dims, (hsize_t*) 0);
      if (space_id < 0)
	goto error_cleanup;

      hid_t save_type_id = H5T_NATIVE_DOUBLE;

      if (save_as_floats)
	{
	  if (m.too_large_for_float ())
	    {
	      warning ("save: some values too large to save as floats --");
	      warning ("save: saving as doubles instead");
	    }
	  else
	    save_type_id = H5T_NATIVE_FLOAT;
	}
#if HAVE_HDF5_INT2FLOAT_CONVERSIONS
      // hdf5 currently doesn't support float/integer conversions
      else
	{
	  double max_val, min_val;

	  if (m.all_integers (max_val, min_val))
	    save_type_id
	      = save_type_to_hdf5 (get_save_type (max_val, min_val));
	}
#endif /* HAVE_HDF5_INT2FLOAT_CONVERSIONS */

      type_id = hdf5_make_complex_type (save_type_id);
      if (type_id < 0) goto error_cleanup;

      data_id = H5Dcreate (loc_id, name.c_str (), 
			   type_id, space_id, H5P_DEFAULT);
      if (data_id < 0)
	goto error_cleanup;
    
      hid_t complex_type_id = hdf5_make_complex_type (H5T_NATIVE_DOUBLE);
      if (complex_type_id < 0)
	goto error_cleanup;

      Complex *mtmp = m.fortran_vec ();
      if (H5Dwrite (data_id, complex_type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT,
		    (void*) X_CAST (double *, mtmp)) < 0)
	{
	  H5Tclose (complex_type_id);
	  goto error_cleanup;
	}

      H5Tclose (complex_type_id);
    }
  else if (tc.is_list ())
    {
      data_id = H5Gcreate (loc_id, name.c_str (), 0);
      if (data_id < 0)
	goto error_cleanup;

      data_is_group = 1;

      // recursively add each element of the list to this group
      octave_value_list lst = tc.list_value ();

      for (int i = 0; i < lst.length (); ++i)
	{
	  // should we use lst.name_tags () to label the elements?
	  char s[20];
	  sprintf (s, "%d", i);
	  bool retval2 = add_hdf5_data (data_id, lst (i), s, "",
					false, save_as_floats);
	  if (! retval2)
	    goto error_cleanup;
	}

      // mark with an attribute "OCTAVE_LIST" with value 1
      // to distinguish from structures (also stored as HDF5 groups):
      if (hdf5_add_attr (data_id, "OCTAVE_LIST") < 0)
	goto error_cleanup;
    }
  else if (tc.is_map ())
    {
      // an Octave structure
      data_id = H5Gcreate (loc_id, name.c_str (), 0);
      if (data_id < 0)
	goto error_cleanup;

      data_is_group = 1;

      // recursively add each element of the structure to this group
      Octave_map m = tc.map_value ();
      Octave_map::iterator i = m.begin ();
      while (i != m.end ())
	{
	  // XXX FIXME XXX -- if the length of the structure array is
	  // 1, should we really create a list object?
	  bool retval2 = add_hdf5_data (data_id, octave_value (m.contents (i)),
					m.key (i), "", false, save_as_floats);
	  if (! retval2)
	    goto error_cleanup;

	  i++;
	}
    }
  else
    {
      gripe_wrong_type_arg ("save", tc, false);
      goto error_cleanup;
    }

  // attach doc string as comment:
  if (doc.length () > 0
      && H5Gset_comment (loc_id, name.c_str (), doc.c_str ()) < 0)
    goto error_cleanup;

  retval = 1;

  // if it's global, add an attribute "OCTAVE_GLOBAL" with value 1
  if (mark_as_global)
    retval = hdf5_add_attr (data_id, "OCTAVE_GLOBAL") >= 0;

 error_cleanup:

  if (! retval)
    error ("save: error while writing `%s' to hdf5 file", name.c_str ());

  if (data_id >= 0)
    {
      if (data_is_group)
	H5Gclose (data_id);
      else
	H5Dclose (data_id);
    }

  if (space_id >= 0)
    H5Sclose (space_id);

  if (type_id >= 0)
    H5Tclose (type_id);

  return retval;
}

// Write data from TC in HDF5 (binary) format to the stream OS,
// which must be an hdf5_ofstream, returning true on success.

static bool
save_hdf5_data (std::ostream& os, const octave_value& tc,
		const std::string& name, const std::string& doc,
		bool mark_as_global, bool save_as_floats)
{
  hdf5_ofstream& hs = (hdf5_ofstream&) os;

  return add_hdf5_data (hs.file_id, tc, name, doc,
			mark_as_global, save_as_floats);
}

#endif /* HAVE_HDF5 */

static int 
write_mat5_tag (std::ostream& is, int type, int bytes)
{
  FOUR_BYTE_INT temp;

  if (bytes <= 4)
    temp = (bytes << 16) + type;
  else
    {
      temp = type;
      if (! is.write ((char *)&temp, 4))
	goto data_write_error;
      temp = bytes;
    }

  if (! is.write ((char *)&temp, 4))
    goto data_write_error;

  return 0;

 data_write_error:
  return 1;
}

// write out the numeric values in M to OS,
// preceded by the appropriate tag.
static void 
write_mat5_array (std::ostream& os, Matrix& m, const int save_as_floats)
{
  int nr = m.rows ();
  int nc = m.columns ();
  double max_val, min_val;
  save_type st = LS_DOUBLE;
  mat5_data_type mst;
  int size;
  unsigned len;
  double *data = m.fortran_vec ();

// Have to use copy here to avoid writing over data accessed via
// Matrix::data().

#define MAT5_DO_WRITE(TYPE, data, count, stream)			\
  do									\
    {									\
      OCTAVE_LOCAL_BUFFER (TYPE, ptr, count);				\
      for (int i = 0; i < count; i++)					\
        ptr[i] = X_CAST (TYPE, data[i]);				\
      stream.write (X_CAST (char *, ptr), count * sizeof (TYPE));	\
    }									\
  while (0)

  if (save_as_floats)
    {
      if (m.too_large_for_float ())
	{
	  warning ("save: some values too large to save as floats --");
	  warning ("save: saving as doubles instead");
	}
      else
	st = LS_FLOAT;
    }

  if (m.all_integers (max_val, min_val))
    st = get_save_type (max_val, min_val);

  switch (st)
    {
    default:
    case LS_DOUBLE:  mst = miDOUBLE; size = 8; break;
    case LS_FLOAT:   mst = miSINGLE; size = 4; break;
    case LS_U_CHAR:  mst = miUINT8;  size = 1; break;
    case LS_U_SHORT: mst = miUINT16; size = 2; break;
    case LS_U_INT:   mst = miUINT32; size = 4; break;
    case LS_CHAR:    mst = miINT8;   size = 1; break;
    case LS_SHORT:   mst = miINT16;  size = 2; break;
    case LS_INT:     mst = miINT32;  size = 4; break;
    }

  len = nr*nc*size;
  write_mat5_tag (os, mst, len);

  {
    switch (st)
      {
      case LS_U_CHAR:
	MAT5_DO_WRITE (unsigned char, data, nr*nc, os);
	break;
	
      case LS_U_SHORT:
	MAT5_DO_WRITE (unsigned TWO_BYTE_INT, data, nr*nc, os);
	break;
	
      case LS_U_INT:
	MAT5_DO_WRITE (unsigned FOUR_BYTE_INT, data, nr*nc, os);
	break;
	
	// provide for 64 bit ints, even though get_save_type does
	// not yet implement them
#ifdef EIGHT_BYTE_INT
      case LS_U_LONG:
	MAT5_DO_WRITE (unsigned EIGHT_BYTE_INT, data, nr*nc, os);
	break;
#endif

      case LS_CHAR:
	MAT5_DO_WRITE (signed char, data, nr*nc, os);
	break;
	
      case LS_SHORT:
	MAT5_DO_WRITE (TWO_BYTE_INT, data, nr*nc, os);
	break;

      case LS_INT:
	MAT5_DO_WRITE (FOUR_BYTE_INT, data, nr*nc, os);
	break;

#ifdef EIGHT_BYTE_INT
      case LS_LONG:
	MAT5_DO_WRITE (EIGHT_BYTE_INT, data, nr*nc, os);
	break;
#endif

      case LS_FLOAT:
	MAT5_DO_WRITE (float, data, nr*nc, os);
	break;

      case LS_DOUBLE: // No conversion necessary.
	os.write (X_CAST (char *, data), len);
	break;

      default:
	(*current_liboctave_error_handler)
	  ("unrecognized data format requested");
	break;
      }
  }
  if (PAD (len) > len)
    {
      static char buf[9]="\x00\x00\x00\x00\x00\x00\x00\x00";
      os.write (buf, PAD (len) - len);
    }
}

// save the data from TC along with the corresponding NAME on stream
// OS in the MatLab version 5 binary format.  Return true on success.

static bool
save_mat5_binary_element (std::ostream& os,
			  const octave_value& tc, const std::string& name,
			  bool mark_as_global, bool save_as_floats) 
{
  FOUR_BYTE_INT flags=0;
  FOUR_BYTE_INT junk=0;
  FOUR_BYTE_INT nr;
  FOUR_BYTE_INT nc;
  std::streampos fixup, contin;

  // element type and length
  fixup = os.tellp ();
  write_mat5_tag (os, miMATRIX, 99); // we don't know the real length yet
  
  // array flags subelement
  write_mat5_tag (os, miUINT32, 8);

  if (mark_as_global)
    flags |= 0x0400;

  if (tc.is_complex_scalar () || tc.is_complex_matrix ())
    flags |= 0x0800;

  if (tc.is_string ())
    flags |= mxCHAR_CLASS;
  else if (tc.is_real_scalar ())
    flags |= mxDOUBLE_CLASS;
  else if (tc.is_real_matrix () || tc.is_range ())
    flags |= mxDOUBLE_CLASS;
  else if (tc.is_complex_scalar ())
    flags |= mxDOUBLE_CLASS;
  else if (tc.is_complex_matrix ())
    flags |= mxDOUBLE_CLASS;
  else if (tc.is_map ()) 
    flags |= mxSTRUCT_CLASS;
  else
    {
      gripe_wrong_type_arg ("save", tc, false);
      goto error_cleanup;
    }

  os.write ((char *)&flags, 4);
  os.write ((char *)&junk, 4);
  
  // dimensions array subelement
  if (tc.is_map ())
    {
      nr = nc = 1;
    }
  else
    {
      nr = tc.rows ();
      nc = tc.columns ();
    }

  write_mat5_tag (os, miINT32, 8);
  os.write ((char *)&nr, 4);
  os.write ((char *)&nc, 4);

  // array name subelement
  {
    int namelen = name.length ();

    if (namelen > 31)
      namelen = 31; // only 31 char names permitted in mat file

    int paddedlength = PAD (namelen);

    write_mat5_tag (os, miINT8, namelen);
    OCTAVE_LOCAL_BUFFER (char, paddedname, paddedlength);
    memset (paddedname, 0, paddedlength);
    strncpy (paddedname, name.c_str (), namelen);
    os.write (paddedname, paddedlength);
  }

  // data element
  if (tc.is_string ())
    {
      charMatrix chm = tc.char_matrix_value ();
      int nc = chm.cols ();
      int len = nr*nc*2;
      int paddedlength = PAD (nr*nc*2);

      OCTAVE_LOCAL_BUFFER (TWO_BYTE_INT, buf, nc*nr+3);
      write_mat5_tag (os, miUINT16, len);

      for (int i = 0; i < nr; i++)
	{
	  std::string tstr = chm.row_as_string (i);
	  const char *s = tstr.data ();

	  for (int j = 0; j < nc; j++)
	    buf[j*nr+i] = *s++;
	}
      os.write ((char *)buf, nr*nc*2);
      
      if (paddedlength > len)
	os.write ((char *)buf, paddedlength - len);
    }
  else if (tc.is_real_scalar () || tc.is_real_matrix () || tc.is_range ())
    {
      Matrix m = tc.matrix_value ();

      write_mat5_array (os, m, save_as_floats);
    }
  else if (tc.is_complex_scalar () || tc.is_complex_matrix ()) 
    {
      ComplexMatrix m_cmplx = tc.complex_matrix_value ();
      Matrix m = ::real (m_cmplx);

      for (int part=0; part < 2; part++)
	{
	  // real part, then complex part
	  write_mat5_array (os, m, save_as_floats);
	  m = ::imag (m_cmplx);
	}
    }
  else if (tc.is_map ()) 
    {
      // an Octave structure */
      // recursively write each element of the structure
      Octave_map m = tc.map_value ();

      {
	char buf[32];
	FOUR_BYTE_INT maxfieldnamelength = 32;
	int fieldcnt = 0;

	for (Octave_map::iterator i = m.begin (); i != m.end (); i++)
	  fieldcnt++;

	write_mat5_tag (os, miINT32, 4);
	os.write ((char *)&maxfieldnamelength, 4);
	write_mat5_tag (os, miINT8, fieldcnt*32);

	for (Octave_map::iterator i = m.begin (); i != m.end (); i++)
	  {
	    // write the name of each element
	    std::string tstr = m.key (i);
	    memset (buf, 0, 32);
	    strncpy (buf, tstr.c_str (), 31); // only 31 char names permitted
	    os.write (buf, 32);
	  }

	for (Octave_map::iterator i = m.begin (); i != m.end (); i++)
	  {
	    // write the data of each element
	    // XXX FIXME XXX -- if the length of the structure array is
	    // 1, should we really create a list object?
	    bool retval2
	      = save_mat5_binary_element (os, octave_value (m.contents (i)),
					  "", mark_as_global, save_as_floats);

	    if (! retval2)
	      goto error_cleanup;
	  }
      }
    }
  else
    gripe_wrong_type_arg ("save", tc, false);

  contin = os.tellp ();
  os.seekp (fixup);
  write_mat5_tag (os, miMATRIX, 
                  static_cast<int>(contin - fixup) - 8); // the actual length
  os.seekp (contin);

  return true;

 error_cleanup:
  error ("save: error while writing `%s' to MAT file", name.c_str ());

  return false;
}

// Save the data from TC along with the corresponding NAME on stream OS 
// in the MatLab version 4 binary format.

static bool
save_mat_binary_data (std::ostream& os, const octave_value& tc,
		      const std::string& name) 
{
  FOUR_BYTE_INT mopt = 0;

  mopt += tc.is_string () ? 1 : 0;

  oct_mach_info::float_format flt_fmt =
    oct_mach_info::native_float_format ();;

  mopt += 1000 * float_format_to_mopt_digit (flt_fmt);

  os.write (X_CAST (char *, &mopt), 4);
  
  FOUR_BYTE_INT nr = tc.rows ();
  os.write (X_CAST (char *, &nr), 4);

  FOUR_BYTE_INT nc = tc.columns ();
  os.write (X_CAST (char *, &nc), 4);

  int len = nr * nc;

  FOUR_BYTE_INT imag = tc.is_complex_type () ? 1 : 0;
  os.write (X_CAST (char *, &imag), 4);

  // LEN includes the terminating character, and the file is also
  // supposed to include it.

  FOUR_BYTE_INT name_len = name.length () + 1;

  os.write (X_CAST (char *, &name_len), 4);
  os << name << '\0';

  if (tc.is_string ())
    {
      unwind_protect::begin_frame ("save_mat_binary_data");
      unwind_protect_int (Vimplicit_str_to_num_ok);
      Vimplicit_str_to_num_ok = true;
      Matrix m = tc.matrix_value ();
      os.write (X_CAST (char *, m.data ()), 8 * len);
      unwind_protect::run_frame ("save_mat_binary_data");
    }
  else if (tc.is_range ())
    {
      Range r = tc.range_value ();
      double base = r.base ();
      double inc = r.inc ();
      int nel = r.nelem ();
      for (int i = 0; i < nel; i++)
	{
	  double x = base + i * inc;
	  os.write (X_CAST (char *, &x), 8);
	}
    }
  else if (tc.is_real_scalar ())
    {
      double tmp = tc.double_value ();
      os.write (X_CAST (char *, &tmp), 8);
    }
  else if (tc.is_real_matrix ())
    {
      Matrix m = tc.matrix_value ();
      os.write (X_CAST (char *, m.data ()), 8 * len);
    }
  else if (tc.is_complex_scalar ())
    {
      Complex tmp = tc.complex_value ();
      os.write (X_CAST (char *, &tmp), 16);
    }
  else if (tc.is_complex_matrix ())
    {
      ComplexMatrix m_cmplx = tc.complex_matrix_value ();
      Matrix m = ::real (m_cmplx);
      os.write (X_CAST (char *, m.data ()), 8 * len);
      m = ::imag (m_cmplx);
      os.write (X_CAST (char *, m.data ()), 8 * len);
    }
  else
    gripe_wrong_type_arg ("save", tc, false);

  return os;
}

static void
ascii_save_type (std::ostream& os, const char *type, bool mark_as_global)
{
  if (mark_as_global)
    os << "# type: global ";
  else
    os << "# type: ";

  os << type << "\n";
}

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

static ComplexMatrix
strip_infnan (const ComplexMatrix& m)
{
  int nr = m.rows ();
  int nc = m.columns ();

  ComplexMatrix retval (nr, nc);

  int k = 0;
  for (int i = 0; i < nr; i++)
    {
      for (int j = 0; j < nc; j++)
	{
	  Complex c = m (i, j);
	  if (xisnan (c))
	    goto next_row;
	  else
	    {
	      double re = real (c);
	      double im = imag (c);

	      re = xisinf (re) ? (re > 0 ? OCT_RBV : -OCT_RBV) : re;
	      im = xisinf (im) ? (im > 0 ? OCT_RBV : -OCT_RBV) : im;

	      retval (k, j) = Complex (re, im);
	    }
	}
      k++;

    next_row:
      continue;
    }

  if (k > 0)
    retval.resize (k, nc);

  return retval;
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

static bool
save_ascii_data (std::ostream& os, const octave_value& tc,
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

  if (tc.is_string ())
    {
      ascii_save_type (os, "string array", mark_as_global);
      charMatrix chm = tc.char_matrix_value ();
      int elements = chm.rows ();
      os << "# elements: " << elements << "\n";
      for (int i = 0; i < elements; i++)
	{
	  unsigned len = chm.cols ();
	  os << "# length: " << len << "\n";
	  std::string tstr = chm.row_as_string (i, false, true);
	  const char *tmp = tstr.data ();
	  if (tstr.length () > len)
	    panic_impossible ();
	  os.write (X_CAST (char *, tmp), len);
	  os << "\n";
	}
    }
  else if (tc.is_range ())
    {
      ascii_save_type (os, "range", mark_as_global);
      Range tmp = tc.range_value ();
      os << "# base, limit, increment\n";
      octave_write_double (os, tmp.base ());
      os << " ";
      octave_write_double (os, tmp.limit ());
      os << " ";
      octave_write_double (os, tmp.inc ());
      os << "\n";
    }
  else if (tc.is_real_scalar ())
    {
      ascii_save_type (os, "scalar", mark_as_global);

      double d = tc.double_value ();

      if (strip_nan_and_inf)
	{
	  if (xisnan (d))
	    {
	      error ("only value to plot is NaN");
	      success = false;
	    }
	  else
	    {
	      d = xisinf (d) ? (d > 0 ? OCT_RBV : -OCT_RBV) : d;
	      octave_write_double (os, d);
	      os << "\n";
	    }
	}
      else
	{
	  if (! infnan_warned && (xisnan (d) || xisinf (d)))
	    {
	      warning ("save: Inf or NaN values may not be reloadable");
	      infnan_warned = true;
	    }

	  octave_write_double (os, d);
	  os << "\n";
	}
    }
  else if (tc.is_real_matrix ())
    {
      ascii_save_type (os, "matrix", mark_as_global);

      os << "# rows: " << tc.rows () << "\n"
	 << "# columns: " << tc.columns () << "\n";

      Matrix tmp = tc.matrix_value ();

      if (strip_nan_and_inf)
	tmp = strip_infnan (tmp);
      else if (! infnan_warned && tmp.any_element_is_inf_or_nan ())
	{
	  warning ("save: Inf or NaN values may not be reloadable");
	  infnan_warned = true;
	}

      os << tmp;
    }
  else if (tc.is_complex_scalar ())
    {
      ascii_save_type (os, "complex scalar", mark_as_global);

      Complex c = tc.complex_value ();

      if (strip_nan_and_inf)
	{
	  if (xisnan (c))
	    {
	      error ("only value to plot is NaN");
	      success = false;
	    }
	  else
	    {
	      double re = real (c);
	      double im = imag (c);

	      re = xisinf (re) ? (re > 0 ? OCT_RBV : -OCT_RBV) : re;
	      im = xisinf (im) ? (im > 0 ? OCT_RBV : -OCT_RBV) : im;

	      c = Complex (re, im);

	      octave_write_complex (os, c);
	      os << "\n";
	    }
	}
      else
	{
	  if (! infnan_warned && (xisnan (c) || xisinf (c)))
	    {
	      warning ("save: Inf or NaN values may not be reloadable");
	      infnan_warned = true;
	    }

	  octave_write_complex (os, c);
	  os << "\n";
	}
    }
  else if (tc.is_complex_matrix ())
    {
      ascii_save_type (os, "complex matrix", mark_as_global);

      os << "# rows: " << tc.rows () << "\n"
	 << "# columns: " << tc.columns () << "\n";

      ComplexMatrix tmp = tc.complex_matrix_value ();

      if (strip_nan_and_inf)
	tmp = strip_infnan (tmp);
      else if (! infnan_warned && tmp.any_element_is_inf_or_nan ())
	{
	  warning ("save: Inf or NaN values may not be reloadable");
	  infnan_warned = true;
	}

      os << tmp;
    }
  else
    gripe_wrong_type_arg ("save", tc, false);

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

// Save the info from sr on stream os in the format specified by fmt.

static void
do_save (std::ostream& os, symbol_record *sr, load_save_format fmt,
	 int save_as_floats, bool& infnan_warned)
{
  if (! sr->is_variable ())
    {
      error ("save: can only save variables, not functions");
      return;
    }

  std::string name = sr->name ();
  std::string help = sr->help ();
  int global = sr->is_linked_to_global ();

  octave_value tc = sr->def ();

  if (tc.is_undefined ())
    return;

  switch (fmt)
    {
    case LS_ASCII:
      save_ascii_data (os, tc, name, infnan_warned, false, global, 0);
      break;

    case LS_BINARY:
      save_binary_data (os, tc, name, help, global, save_as_floats);
      break;

    case LS_MAT_BINARY:
      save_mat_binary_data (os, tc, name);
      break;

#ifdef HAVE_HDF5
    case LS_HDF5:
      save_hdf5_data (os, tc, name, help, global, save_as_floats);
      break;
#endif /* HAVE_HDF5 */

    case LS_MAT5_BINARY:
      save_mat5_binary_element (os, tc, name, global, save_as_floats);
      break;

    default:
      gripe_unrecognized_data_fmt ("save");
      break;
    }
}

// Save variables with names matching PATTERN on stream OS in the
// format specified by FMT.  If SAVE_BUILTINS is TRUE, also save
// builtin variables with names that match PATTERN.

static int
save_vars (std::ostream& os, const std::string& pattern, bool save_builtins,
	   load_save_format fmt, int save_as_floats)
{
  Array<symbol_record *> vars = curr_sym_tab->glob
    (pattern, symbol_record::USER_VARIABLE, SYMTAB_ALL_SCOPES);

  int saved = vars.length ();

  bool infnan_warned = false;

  for (int i = 0; i < saved; i++)
    {
      do_save (os, vars (i), fmt, save_as_floats, infnan_warned);

      if (error_state)
	break;
    }

  if (! error_state && save_builtins)
    {
      vars = fbi_sym_tab->glob
	(pattern, symbol_record::BUILTIN_VARIABLE, SYMTAB_ALL_SCOPES);

      int count = vars.length ();

      saved += count;

      for (int i = 0; i < count; i++)
	{
	  do_save (os, vars (i), fmt, save_as_floats, infnan_warned);

	  if (error_state)
	    break;
	}
    }

  return saved;
}

static load_save_format
get_default_save_format (void)
{
  load_save_format retval = LS_ASCII;

  std::string fmt = Vdefault_save_format;

  if (fmt == "binary")
    retval = LS_BINARY;
  else if (fmt == "mat-binary" || fmt =="mat_binary")
    retval = LS_MAT5_BINARY;
  else if (fmt == "mat4-binary" || fmt =="mat4_binary")
    retval = LS_MAT_BINARY;
#ifdef HAVE_HDF5
  else if (fmt == "hdf5")
    retval = LS_HDF5;
#endif /* HAVE_HDF5 */
      
  return retval;
}

static void
write_header (std::ostream& os, load_save_format format)
{
  switch (format)
    {
    case LS_BINARY:
      {
	os << (oct_mach_info::words_big_endian ()
	       ? "Octave-1-B" : "Octave-1-L");

	oct_mach_info::float_format flt_fmt =
	  oct_mach_info::native_float_format ();

	char tmp = (char) float_format_to_mopt_digit (flt_fmt);

	os.write (X_CAST (char *, &tmp), 1);
      }
      break;

    case LS_MAT5_BINARY:
      {
	char const * versionmagic;
	TWO_BYTE_INT number = *(TWO_BYTE_INT *)"\x00\x01";
	struct tm bdt;
	time_t now;
	char headertext[128];

	time (&now);
	bdt = *gmtime (&now);
	memset (headertext, ' ', 124);
	// ISO 8601 format date
	strftime (headertext, 124, "MATLAB 5.0 MAT-file, written by Octave "
		 OCTAVE_VERSION ", %Y-%m-%d %T UTC", &bdt);

	// The first pair of bytes give the version of the MAT file
	// format.  The second pair of bytes form a magic number which
	// signals a MAT file.  MAT file data are always written in
	// native byte order.  The order of the bytes in the second
	// pair indicates whether the file was written by a big- or
	// little-endian machine.  However, the version number is
	// written in the *opposite* byte order from everything else!
	if (number == 1)
	  versionmagic = "\x01\x00\x4d\x49"; // this machine is big endian
	else
	  versionmagic = "\x00\x01\x49\x4d"; // this machine is little endian

	memcpy (headertext+124, versionmagic, 4);
	os.write (headertext, 128);
      }

      break;

#ifdef HAVE_HDF5
    case LS_HDF5:
#endif /* HAVE_HDF5 */
    case LS_ASCII:
      {
	octave_localtime now;

	std::string comment_string = now.strftime (Vsave_header_format_string);

	if (! comment_string.empty ())
	  {
#ifdef HAVE_HDF5
	    if (format == LS_HDF5)
	      {
		hdf5_ofstream& hs = (hdf5_ofstream&) os;
		H5Gset_comment (hs.file_id, "/", comment_string.c_str ());
	      }
	    else
#endif /* HAVE_HDF5 */
	      os << comment_string << "\n";
	  }
      }
    break;

    default:
      break;
    }
}

static void
save_vars (const string_vector& argv, int argv_idx, int argc,
	   std::ostream& os, bool save_builtins, load_save_format fmt,
	   bool save_as_floats, bool write_header_info)
{
  if (write_header_info)
    write_header (os, fmt);

  if (argv_idx == argc)
    {
      save_vars (os, "*", save_builtins, fmt, save_as_floats);
    }
  else
    {
      for (int i = argv_idx; i < argc; i++)
	{
	  if (! save_vars (os, argv[i], save_builtins, fmt, save_as_floats))
	    {
	      warning ("save: no such variable `%s'", argv[i].c_str ());
	    }
	}
    }
}

void
save_user_variables (void)
{
  if (Vcrash_dumps_octave_core)
    {
      // XXX FIXME XXX -- should choose better file name?

      const char *fname = "octave-core";

      message (0, "attempting to save variables to `%s'...", fname);

      load_save_format format = get_default_save_format ();

      std::ios::openmode mode = std::ios::out|std::ios::trunc;
      if (format == LS_BINARY ||
	  format == LS_MAT_BINARY ||
	  format == LS_MAT5_BINARY)
	mode |= std::ios::binary;

#ifdef HAVE_HDF5
      if (format == LS_HDF5)
	{
	  hdf5_ofstream file (fname);

	  if (file.file_id >= 0)
	    {
	      save_vars (string_vector (), 0, 0, file,
			 false, format, false, true);

	      message (0, "save to `%s' complete", fname);

	      file.close ();
	    }
	  else
	    warning ("unable to open `%s' for writing...", fname);
	}
      else
#endif /* HAVE_HDF5 */
	// don't insert any commands here!  The open brace below must
	// go with the else above!
	{
	  std::ofstream file (fname, mode);
	  
	  if (file)
	    {
	      save_vars (string_vector (), 0, 0, file,
			 false, format, false, true);
	      message (0, "save to `%s' complete", fname);
	      file.close ();
	    }
	  else
	    warning ("unable to open `%s' for writing...", fname);
	}
    }
}

DEFCMD (save, args, ,
  "-*- texinfo -*-\n\
@deffn {Command} save options file v1 v2 @dots{}\n\
Save the named variables @var{v1}, @var{v2}, @dots{} in the file\n\
@var{file}.  The special filename @samp{-} can be used to write the\n\
output to your terminal.  If no variable names are listed, Octave saves\n\
all the variables in the current scope.  Valid options for the\n\
@code{save} command are listed in the following table.  Options that\n\
modify the output format override the format specified by the built-in\n\
variable @code{default_save_format}.\n\
\n\
@table @code\n\
@item -ascii\n\
Save the data in Octave's text data format.\n\
\n\
@item -binary\n\
Save the data in Octave's binary data format.\n\
\n\
@item -float-binary\n\
Save the data in Octave's binary data format but only using single\n\
precision.  You should use this format only if you know that all the\n\
values to be saved can be represented in single precision.\n\
\n\
@item -mat-binary\n\
Save the data in @sc{Matlab}'s binary data format.\n\
\n\
@item -mat4-binary\n\
Save the data in the binary format written by @sc{Matlab} version 4.\n\
\n\
@item -hdf5\n\
Save the data in HDF5 format.\n\
(HDF5 is a free, portable binary format developed by the National\n\
Center for Supercomputing Applications at the University of Illinois.)\n"

HAVE_HDF5_HELP_STRING

"\n\
@item -float-hdf5\n\
Save the data in HDF5 format but only using single precision.\n\
You should use this format only if you know that all the\n\
values to be saved can be represented in single precision.\n\
\n\
@item -save-builtins\n\
Force Octave to save the values of built-in variables too.  By default,\n\
Octave does not save built-in variables.\n\
@end table\n\
\n\
The list of variables to save may include wildcard patterns containing\n\
the following special characters:\n\
@table @code\n\
@item ?\n\
Match any single character.\n\
\n\
@item *\n\
Match zero or more characters.\n\
\n\
@item [ @var{list} ]\n\
Match the list of characters specified by @var{list}.  If the first\n\
character is @code{!} or @code{^}, match all characters except those\n\
specified by @var{list}.  For example, the pattern @samp{[a-zA-Z]} will\n\
match all lower and upper case alphabetic characters. \n\
@end table\n\
\n\
Except when using the @sc{Matlab} binary data file format, saving global\n\
variables also saves the global status of the variable, so that if it is\n\
restored at a later time using @samp{load}, it will be restored as a\n\
global variable.\n\
\n\
The command\n\
\n\
@example\n\
save -binary data a b*\n\
@end example\n\
\n\
@noindent\n\
saves the variable @samp{a} and all variables beginning with @samp{b} to\n\
the file @file{data} in Octave's binary format.\n\
@end deffn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("save");

  if (error_state)
    return retval;

  // Here is where we would get the default save format if it were
  // stored in a user preference variable.

  bool save_builtins = false;

  bool save_as_floats = false;

  load_save_format format = get_default_save_format ();

  bool append = false;

  int i;
  for (i = 1; i < argc; i++)
    {
      if (argv[i] == "-append")
	{
	  append = true;
	}
      else if (argv[i] == "-ascii" || argv[i] == "-a")
	{
	  format = LS_ASCII;
	}
      else if (argv[i] == "-binary" || argv[i] == "-b")
	{
	  format = LS_BINARY;
	}
      else if (argv[i] == "-hdf5" || argv[i] == "-h")
	{
#ifdef HAVE_HDF5
	  format = LS_HDF5;
#else /* ! HAVE_HDF5 */
	  error ("save: octave executable was not linked with HDF5 library");
	  return retval;
#endif /* ! HAVE_HDF5 */
	}
      else if (argv[i] == "-mat-binary" || argv[i] == "-m")
	{
	  format = LS_MAT5_BINARY;
	}
      else if (argv[i] == "-mat4-binary" || argv[i] == "-4" || argv[i] == "-v4")
	{
	  format = LS_MAT_BINARY;
	}
      else if (argv[i] == "-float-binary" || argv[i] == "-f")
	{
	  format = LS_BINARY;
	  save_as_floats = true;
	}
      else if (argv[i] == "-float-hdf5")
	{
#ifdef HAVE_HDF5
	  format = LS_HDF5;
	  save_as_floats = true;
#else /* ! HAVE_HDF5 */
	  error ("save: octave executable was not linked with HDF5 library");
	  return retval;
#endif /* ! HAVE_HDF5 */
	}
      else if (argv[i] == "-save-builtins")
	{
	  save_builtins = true;
	}
      else
	break;
    }

  if (i == argc)
    {
      print_usage ("save");
      return retval;
    }

  if (save_as_floats && format == LS_ASCII)
    {
      error ("save: cannot specify both -ascii and -float-binary");
      return retval;
    }

  if (argv[i] == "-")
    {
      i++;

#ifdef HAVE_HDF5
      if (format == LS_HDF5)
        error ("load: cannot write HDF5 format to stdout");
      else
#endif /* HAVE_HDF5 */
	// don't insert any commands here!  the brace below must go
	// with the "else" above!
	{
	  // XXX FIXME XXX -- should things intended for the screen end up
	  // in a octave_value (string)?
	  
	  save_vars (argv, i, argc, octave_stdout, save_builtins, format,
		     save_as_floats, true);
	}
    }

  // Guard against things like `save a*', which are probably mistakes...

  else if (i == argc - 1 && glob_pattern_p (argv[i]))
    {
      print_usage ("save");
      return retval;
    }
  else
    {
      std::string fname = file_ops::tilde_expand (argv[i]);

      i++;

      std::ios::openmode mode = std::ios::out;
      if (format == LS_BINARY ||
	  format == LS_MAT_BINARY ||
	  format == LS_MAT5_BINARY)
	mode |= std::ios::binary;

      mode |= append ? std::ios::ate : std::ios::trunc;

#ifdef HAVE_HDF5
      if (format == LS_HDF5)
	{
	  hdf5_ofstream hdf5_file (fname.c_str ());

	  if (hdf5_file.file_id >= 0) {
	    save_vars (argv, i, argc, hdf5_file, save_builtins, format,
		       save_as_floats, true);

	    hdf5_file.close ();
	  }
	else
	  {
	    error ("save: couldn't open output file `%s'", fname.c_str ());
	    return retval;
	  }
	}
      else
#endif /* HAVE_HDF5 */
	// don't insert any statements here!  The brace below must go
	// with the "else" above!
	{
	  std::ofstream file (fname.c_str (), mode);
	  
	  if (file)
	    {
	      bool write_header_info
		= ((file.rdbuf ())->pubseekoff (0, std::ios::cur)
		   == static_cast<std::streampos> (0));
	      
	      save_vars (argv, i, argc, file, save_builtins, format,
			 save_as_floats, write_header_info);
	    }
	  else
	    {
	      error ("save: couldn't open output file `%s'", fname.c_str ());
	      return retval;
	    }
	}
    }

  return retval;
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
crash_dumps_octave_core (void)
{
  Vcrash_dumps_octave_core = check_preference ("crash_dumps_octave_core");
  return 0;
}


static int
default_save_format (void)
{
  int status = 0;

  std::string s = builtin_string_variable ("default_save_format");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("default_save_format");
      status = -1;
    }
  else
    Vdefault_save_format = s;

  return status;
}

static std::string
default_save_header_format (void)
{
  return
    std::string ("# Created by Octave " OCTAVE_VERSION ", %a %b %d %H:%M:%S %Y %Z <")
    + octave_env::get_user_name ()
    + std::string ("@")
    + octave_env::get_host_name ()
    + std::string (">");
}

static int
save_header_format_string (void)
{
  int status = 0;

  octave_value v = builtin_any_variable ("save_header_format_string");

  if (v.is_string ())
    Vsave_header_format_string = v.string_value ();
  else
    {
      gripe_invalid_value_specified ("save_header_format_string");
      status = -1;
    }

  return status;
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
symbols_of_load_save (void)
{
  DEFVAR (crash_dumps_octave_core, true, crash_dumps_octave_core,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} crash_dumps_octave_core\n\
If this variable is set to a nonzero value, Octave tries to save all\n\
current variables the the file \"octave-core\" if it crashes or receives a\n\
hangup, terminate or similar signal.  The default value is 1.\n\
@end defvr");

  DEFVAR (default_save_format, "ascii", default_save_format,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} default_save_format\n\
This variable specifies the default format for the @code{save} command.\n\
It should have one of the following values: @code{\"ascii\"},\n\
@code{\"binary\"}, @code{float-binary}, or @code{\"mat-binary\"}.  The\n\
initial default save format is Octave's text format.\n\
@end defvr");

  DEFVAR (save_header_format_string, default_save_header_format (),
	  save_header_format_string,
    "-*- texinfo -*-\n\
@defvr {Built-in Variable} save_header_format_string\n\
This variable specifies the the format string for the comment line\n\
that is written at the beginning of text-format data files saved by\n\
Octave.  The format string is passed to @code{strftime} and should\n\
begin with the character @samp{#} and contain no newline characters.\n\
If the value of @code{save_header_format_string} is the empty string,\n\
the header comment is omitted from text-format data files.  The\n\
default value is\n\
\n\
@example\n\
\"# Created by Octave VERSION, %a %b %d %H:%M:%S %Y %Z <USER@@HOST>\"\n\
@end example\n\
@seealso{strftime}\n\
@end defvr");

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
