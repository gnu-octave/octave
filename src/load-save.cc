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

#include <cfloat>
#include <cstring>
#include <cctype>

#include <string>

#include <iomanip.h>
#include <iostream.h>
#include <fstream.h>
#include <strstream.h>

#include "byte-swap.h"
#include "data-conv.h"
#include "file-ops.h"
#include "glob-match.h"
#include "lo-mappers.h"
#include "mach-info.h"
#include "str-vec.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "load-save.h"
#include "oct-obj.h"
#include "pager.h"
#include "pt-exp.h"
#include "symtab.h"
#include "sysdep.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// The default output format.  May be one of "binary", "text", or
// "mat-binary".
static string Vdefault_save_format;

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
    LS_UNKNOWN,
  };

// Return nonzero if S is a valid identifier.

static bool
valid_identifier (const char *s)
{
  if (! s || ! (isalnum (*s) || *s == '_'))
     return false;

  while (*++s != '\0')
    if (! (isalnum (*s) || *s == '_'))
      return false;

  return true;
}

static bool
valid_identifier (const string& s)
{
  return valid_identifier (s.c_str ());
}

// XXX FIXME XXX -- shouldn't this be implemented in terms of other
// functions that are already available?

// Install a variable with name NAME and the value specified TC in the
// symbol table.  If FORCE is nonzero, replace any existing definition
// for NAME.  If GLOBAL is nonzero, make the variable global.
//
// Assumes TC is defined.

static void
install_loaded_variable (int force, char *name, const octave_value& val,
			 int global, char *doc)
{
  // Is there already a symbol by this name?  If so, what is it?

  symbol_record *lsr = curr_sym_tab->lookup (name);

  int is_undefined = 1;
  int is_variable = 0;
  int is_function = 0;
  int is_global = 0;

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
	      warning ("load: global variable name `%s' exists.", name);
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
	      warning ("load: `%s' is currently a function in this scope", name);
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
	      warning ("load: local variable name `%s' exists.", name);
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
	      warning ("load: global variable name `%s' exists.", name);
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
	      warning ("load: `%s' is currently a function in this scope", name);
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
	      warning ("load: local variable name `%s' exists.", name);
	      warning ("use `load -force' to overwrite");
	    }
	}
      else
	error ("load: unable to load data for unknown symbol type");
    }

  if (sr)
    {
      sr->define (val);
      if (doc)
	sr->document (doc);
      return;
    }
  else
    error ("load: unable to load variable `%s'", name);

  return;
}

// Functions for reading ascii data.

// Skip white space and comments on stream IS.

static void
skip_comments (istream& is)
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
      if (is && c == '#')
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
//  #[ \t]*keyword[ \t]*:[ \t]*string-value[ \t]*\n

static char *
extract_keyword (istream& is, const char *keyword)
{
  char *retval = 0;

  char c;
  while (is.get (c))
    {
      if (c == '#')
	{
	  ostrstream buf;
	
	  while (is.get (c) && (c == ' ' || c == '\t' || c == '#'))
	    ; // Skip whitespace and comment characters.

	  if (isalpha (c))
	    buf << c;

	  while (is.get (c) && isalpha (c))
	    buf << c;

	  buf << ends;
	  char *tmp = buf.str ();
	  int match = (strncmp (tmp, keyword, strlen (keyword)) == 0);
	  delete [] tmp;

	  if (match)
	    {
	      ostrstream value;
	      while (is.get (c) && (c == ' ' || c == '\t' || c == ':'))
		; // Skip whitespace and the colon.

	      if (c != '\n')
		{
		  value << c;
		  while (is.get (c) && c != '\n')
		    value << c;
		}
	      value << ends;
	      retval = value.str ();
	      break;
	    }
	}
    }

  if (retval)
    {
      int len = strlen (retval);
      if (len > 0)
	{
	  char *ptr = retval + len - 1;
	  while (*ptr == ' ' || *ptr == '\t')
	    ptr--;
	  *(ptr+1) = '\0';
	}
    }

  return retval;
}

// Match KEYWORD on stream IS, placing the associated value in VALUE,
// returning 1 if successful and 0 otherwise.
//
// Input should look something like:
//
//  [ \t]*keyword[ \t]*int-value.*\n

static int
extract_keyword (istream& is, const char *keyword, int& value)
{
  int status = 0;
  value = 0;

  char c;
  while (is.get (c))
    {
      if (c == '#')
	{
	  ostrstream buf;

	  while (is.get (c) && (c == ' ' || c == '\t' || c == '#'))
	    ; // Skip whitespace and comment characters.

	  if (isalpha (c))
	    buf << c;

	  while (is.get (c) && isalpha (c))
	    buf << c;

	  buf << ends;
	  char *tmp = buf.str ();
	  int match = (strncmp (tmp, keyword, strlen (keyword)) == 0);
	  delete [] tmp;

	  if (match)
	    {
	      while (is.get (c) && (c == ' ' || c == '\t' || c == ':'))
		; // Skip whitespace and the colon.

	      is.putback (c);
	      if (c != '\n')
		is >> value;
	      if (is)
		status = 1;
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
// is tagged as global in the file, return nonzero in GLOBAL.
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

static char *
read_ascii_data (istream& is, const string& filename, int& global,
		 octave_value& tc)
{
  // Read name for this entry or break on EOF.

  char *name = extract_keyword (is, "name");

  if (! name)
    return 0;

  if (! *name)
    {
      error ("load: empty name keyword found in file `%s'",
	     filename.c_str ());
      delete [] name;
      return 0;
    }
      

  if (! valid_identifier (name))
    {
      error ("load: bogus identifier `%s' found in file `%s'", name,
	     filename.c_str ());
      delete [] name;
      return 0;
    }

  // Look for type keyword.

  char *tag = extract_keyword (is, "type");

  if (tag && *tag)
    {
      char *ptr = strchr (tag, ' ');
      if (ptr)
	{
	  *ptr = '\0';
	  global = (strncmp (tag, "global", 6) == 0);
	  *ptr = ' ';
	  if (global)
	    ptr++;
	  else
	    ptr = tag;
	}
      else
	ptr = tag;

      if (strncmp (ptr, "scalar", 6) == 0)
	{
	  double tmp;
	  is >> tmp;
	  if (is)
	    tc = tmp;
	  else
	    error ("load: failed to load scalar constant");
	}
      else if (strncmp (ptr, "matrix", 6) == 0)
	{
	  int nr = 0, nc = 0;

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
      else if (strncmp (ptr, "complex scalar", 14) == 0)
	{
	  Complex tmp;
	  is >> tmp;
	  if (is)
	    tc = tmp;
	  else
	    error ("load: failed to load complex scalar constant");
	}
      else if (strncmp (ptr, "complex matrix", 14) == 0)
	{
	  int nr = 0, nc = 0;

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
      else if (strncmp (ptr, "string array", 12) == 0)
	{
	  int elements;
	  if (extract_keyword (is, "elements", elements) && elements > 0)
	    {
	      // XXX FIXME XXX -- need to be able to get max length
	      // before doing anything.

	      charMatrix chm (elements, 0);
	      int max_len = 0;
	      for (int i = 0; i < elements; i++)
		{
		  int len;
		  if (extract_keyword (is, "length", len) && len > 0)
		    {
		      char *tmp = new char [len+1];
		      if (! is.read (tmp, len))
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
		      delete [] tmp;
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
      else if (strncmp (ptr, "string", 6) == 0)
	{
	  int len;
	  if (extract_keyword (is, "length", len) && len > 0)
	    {
	      char *tmp = new char [len+1];
	      is.get (tmp, len+1, EOF);
	      if (is)
		tc = tmp;
	      else
		error ("load: failed to load string constant");
	    }
	  else
	    error ("load: failed to extract string length");
	}
      else if (strncmp (ptr, "range", 5) == 0)
	{
	  // # base, limit, range comment added by save().

	  skip_comments (is);
	  Range tmp;
	  is >> tmp;
	  if (is)
	    tc = tmp;
	  else
	    error ("load: failed to load range constant");
	}
      else
	error ("load: unknown constant type `%s'", tag);
    }
  else
    error ("load: failed to extract keyword specifying value type");

  delete [] tag;

  if (error_state)
    {
      error ("load: reading file %s", filename.c_str ());
      return 0;
    }

  return name;
}

// Extract one value (scalar, matrix, string, etc.) from stream IS and
// place it in TC, returning the name of the variable.  If the value
// is tagged as global in the file, return nonzero in GLOBAL.  If SWAP
// is nonzero, swap bytes after reading.
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

static char *
read_binary_data (istream& is, int swap,
		  oct_mach_info::float_format fmt,
		  const string& filename, int& global,
		  octave_value& tc, char *&doc)
{
  char tmp = 0;

  FOUR_BYTE_INT name_len = 0, doc_len = 0;
  char *name = 0;

  doc = 0;

  // We expect to fail here, at the beginning of a record, so not
  // being able to read another name should not result in an error.

  is.read (&name_len, 4);
  if (! is)
    return 0;
  if (swap)
    swap_4_bytes (static_cast<char *> (&name_len));

  name = new char [name_len+1];
  name[name_len] = '\0';
  if (! is.read (name, name_len))
    goto data_read_error;

  is.read (&doc_len, 4);
  if (! is)
    goto data_read_error;
  if (swap)
    swap_4_bytes (static_cast<char *> (&doc_len));

  doc = new char [doc_len+1];
  doc[doc_len] = '\0';
  if (! is.read (doc, doc_len))
    goto data_read_error;

  if (! is.read (&tmp, 1))
    goto data_read_error;
  global = tmp ? 1 : 0;

  tmp = 0;
  if (! is.read (&tmp, 1))
    goto data_read_error;

  switch (tmp)
    {
    case 1:
      {
	if (! is.read (&tmp, 1))
	  goto data_read_error;
	double dtmp;
	read_doubles (is, &dtmp, static_cast<save_type> (tmp), 1, swap, fmt);
	if (error_state || ! is)
	  goto data_read_error;
	tc = dtmp;
      }
      break;

    case 2:
      {
	FOUR_BYTE_INT nr, nc;
	if (! is.read (&nr, 4))
	  goto data_read_error;
	if (swap)
	  swap_4_bytes (static_cast<char *> (&nr));
	if (! is.read (&nc, 4))
	  goto data_read_error;
	if (swap)
	  swap_4_bytes (static_cast<char *> (&nc));
	if (! is.read (&tmp, 1))
	  goto data_read_error;
	Matrix m (nr, nc);
	double *re = m.fortran_vec ();
	int len = nr * nc;
	read_doubles (is, re, static_cast<save_type> (tmp), len, swap, fmt);
	if (error_state || ! is)
	  goto data_read_error;
	tc = m;
      }
      break;

    case 3:
      {
	if (! is.read (&tmp, 1))
	  goto data_read_error;
	Complex ctmp;
	read_doubles (is, static_cast<double *> (&ctmp),
		      static_cast<save_type> (tmp), 2, swap, fmt);
	if (error_state || ! is)
	  goto data_read_error;
	tc = ctmp;
      }
      break;

    case 4:
      {
	FOUR_BYTE_INT nr, nc;
	if (! is.read (&nr, 4))
	  goto data_read_error;
	if (swap)
	  swap_4_bytes (static_cast<char *> (&nr));
	if (! is.read (&nc, 4))
	  goto data_read_error;
	if (swap)
	  swap_4_bytes (static_cast<char *> (&nc));
	if (! is.read (&tmp, 1))
	  goto data_read_error;
	ComplexMatrix m (nr, nc);
	Complex *im = m.fortran_vec ();
	int len = nr * nc;
	read_doubles (is, static_cast<double *> (im),
		      static_cast<save_type> (tmp), 2*len, swap, fmt);
	if (error_state || ! is)
	  goto data_read_error;
	tc = m;
      }
      break;

    case 5:
      {
	FOUR_BYTE_INT len;
	if (! is.read (&len, 4))
	  goto data_read_error;
	if (swap)
	  swap_4_bytes (static_cast<char *> (&len));
	char *s = new char [len+1];
	if (! is.read (s, len))
	  {
	    delete [] s;
	    goto data_read_error;
	  }
	s[len] = '\0';
	tc = s;
      }
      break;

    case 6:
      {
	if (! is.read (&tmp, 1))
	  goto data_read_error;
	double bas, lim, inc;
	if (! is.read (&bas, 8))
	  goto data_read_error;
	if (swap)
	  swap_8_bytes (static_cast<char *> (&bas));
	if (! is.read (&lim, 8))
	  goto data_read_error;
	if (swap)
	  swap_8_bytes (static_cast<char *> (&lim));
	if (! is.read (&inc, 8))
	  goto data_read_error;
	if (swap)
	  swap_8_bytes (static_cast<char *> (&inc));
	Range r (bas, lim, inc);
	tc = r;
      }
      break;

    case 7:
      {
	FOUR_BYTE_INT elements;
	if (! is.read (&elements, 4))
	  goto data_read_error;
	if (swap)
	  swap_4_bytes (static_cast<char *> (&elements));
	charMatrix chm (elements, 0);
	int max_len = 0;
	for (int i = 0; i < elements; i++)
	  {
	    FOUR_BYTE_INT len;
	    if (! is.read (&len, 4))
	      goto data_read_error;
	    if (swap)
	      swap_4_bytes (static_cast<char *> (&len));
	    char *tmp = new char [len+1];
	    if (! is.read (tmp, len))
	      {
		delete [] tmp;
		goto data_read_error;
	      }
	    if (len > max_len)
	      {
		max_len = len;
		chm.resize (elements, max_len, 0);
	      }
	    tmp [len] = '\0';
	    chm.insert (tmp, i, 0);
	    delete [] tmp;
	  }
	tc = octave_value (chm, true);
      }
      break;

    default:
    data_read_error:
      error ("load: trouble reading binary file `%s'", filename.c_str ());
      delete [] name;
      name = 0;
      break;
    }

  return name;
}

static void
get_lines_and_columns (istream& is, const string& filename, int& nr, int& nc)
{
  streampos pos = is.tellg ();

  int file_line_number = 0;

  nr = 0;
  nc = 0;

  while (is && ! error_state)
    {
      string buf;

      char c;
      while (is.get (c))
	{
	  if (c == '\n')
	    break;

	  buf += c;
	}

      file_line_number++;

      size_t beg = buf.find_first_not_of (" \t");

      int tmp_nc = 0;

      while (beg != NPOS)
	{
	  tmp_nc++;

	  size_t end = buf.find_first_of (" \t", beg);

	  if (end != NPOS)
	    beg = buf.find_first_not_of (" \t", end);
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
  is.seekg (pos, ios::beg);
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

static char *
read_mat_ascii_data (istream& is, const string& filename,
		     octave_value& tc)
{
  char *name = 0;

  string varname;

  size_t pos = filename.find ('.');

  if (pos != NPOS)
    varname = filename.substr (0, pos);
  else
    varname = filename;

  if (valid_identifier (varname.c_str ()))
    {
      int nr = 0;
      int nc = 0;

      get_lines_and_columns (is, filename, nr, nc);

      if (! error_state && nr > 0 && nc > 0)
	{
	  Matrix tmp (nr, nc);

	  is >> tmp;

	  if (is)
	    {
	      tc = tmp;

	      name = strsave (varname.c_str ());
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

  return name;
}

// Read LEN elements of data from IS in the format specified by
// PRECISION, placing the result in DATA.  If SWAP is nonzero, swap
// the bytes of each element before copying to DATA.  FLT_FMT
// specifies the format of the data if we are reading floating point
// numbers.

static void
read_mat_binary_data (istream& is, double *data, int precision,
		      int len, int swap,
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
read_mat_file_header (istream& is, int& swap, FOUR_BYTE_INT& mopt, 
		      FOUR_BYTE_INT& nr, FOUR_BYTE_INT& nc,
		      FOUR_BYTE_INT& imag, FOUR_BYTE_INT& len,
		      int quiet = 0)
{
  swap = 0;

  // We expect to fail here, at the beginning of a record, so not
  // being able to read another mopt value should not result in an
  // error.

  is.read (&mopt, 4);
  if (! is)
    return 1;

  if (! is.read (&nr, 4))
    goto data_read_error;

  if (! is.read (&nc, 4))
    goto data_read_error;

  if (! is.read (&imag, 4))
    goto data_read_error;

  if (! is.read (&len, 4))
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
    swap = 1;

  // mopt is signed, therefore byte swap may result in negative value.

  if (mopt > 9999 || mopt < 0)
    swap = 1;

  if (swap)
    {
      swap_4_bytes (static_cast<char *> (&mopt));
      swap_4_bytes (static_cast<char *> (&nr));
      swap_4_bytes (static_cast<char *> (&nc));
      swap_4_bytes (static_cast<char *> (&imag));
      swap_4_bytes (static_cast<char *> (&len));
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
// The data is expected to be in Matlab's .mat format, though not all
// the features of that format are supported.
//
// FILENAME is used for error messages.
//
// This format provides no way to tag the data as global.

static char *
read_mat_binary_data (istream& is, const string& filename,
		      octave_value& tc)
{
  // These are initialized here instead of closer to where they are
  // first used to avoid errors from gcc about goto crossing
  // initialization of variable.

  Matrix re;
  oct_mach_info::float_format flt_fmt = oct_mach_info::unknown;
  char *name = 0;
  int swap = 0, type = 0, prec = 0, mach = 0, dlen = 0;

  FOUR_BYTE_INT mopt, nr, nc, imag, len;

  int err = read_mat_file_header (is, swap, mopt, nr, nc, imag, len);
  if (err)
    {
      if (err < 0)
	goto data_read_error;
      else
	return 0;
    }

  type = mopt % 10; // Full, sparse, etc.
  mopt /= 10;       // Eliminate first digit.
  prec = mopt % 10; // double, float, int, etc.
  mopt /= 100;      // Skip unused third digit too.
  mach = mopt % 10; // IEEE, VAX, etc.

  flt_fmt = mopt_digit_to_float_format (mach);

  if (flt_fmt == oct_mach_info::unknown)
    {
      error ("load: unrecognized binary format!");
      return 0;
    }

  if (type != 0 && type != 1)
    {
      error ("load: can't read sparse matrices");
      return 0;
    }

  if (imag && type == 1)
    {
      error ("load: encountered complex matrix with string flag set!");
      return 0;
    }

  // LEN includes the terminating character, and the file is also
  // supposed to include it, but apparently not all files do.  Either
  // way, I think this should work.

  name = new char [len+1];
  if (! is.read (name, len))
    goto data_read_error;
  name[len] = '\0';

  dlen = nr * nc;
  if (dlen < 0)
    goto data_read_error;

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

      read_mat_binary_data (is, im.fortran_vec (), prec, dlen, swap, flt_fmt);

      if (! is || error_state)
	{
	  error ("load: reading imaginary matrix data for `%s'", name);
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

  if (type == 1)
    tc = tc.convert_to_str ();

  return name;

 data_read_error:
  error ("load: trouble reading binary file `%s'", filename.c_str ());
  delete [] name;
  return 0;
}

// Return nonzero if NAME matches one of the given globbing PATTERNS.

static bool
matches_patterns (const string_vector& patterns, int pat_idx,
		  int num_pat, const string& name)
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
read_binary_file_header (istream& is, int& swap,
			 oct_mach_info::float_format& flt_fmt,
			 int quiet = 0) 
{
  int magic_len = 10;
  char magic [magic_len+1];
  is.read (magic, magic_len);
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
  is.read (&tmp, 1);

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
get_file_format (const string& fname, const string& orig_fname)
{
  load_save_format retval = LS_UNKNOWN;

  ifstream file (fname.c_str ());

  if (! file)
    {
      error ("load: couldn't open input file `%s'", orig_fname.c_str ());
      return retval;
    }

  int swap;
  oct_mach_info::float_format flt_fmt = oct_mach_info::unknown;

  if (read_binary_file_header (file, swap, flt_fmt, 1) == 0)
    retval = LS_BINARY;
  else
    {
      file.seekg (0, ios::beg);

      FOUR_BYTE_INT mopt, nr, nc, imag, len;

      int err = read_mat_file_header (file, swap, mopt, nr, nc, imag, len, 1);

      if (! err)
	retval = LS_MAT_BINARY;
      else
	{
	  file.clear ();
	  file.seekg (0, ios::beg);

	  char *tmp = extract_keyword (file, "name");

	  if (tmp)
	    {
	      retval = LS_ASCII;

	      delete [] tmp;
	    }
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

  file.close ();

  if (retval == LS_UNKNOWN)
    error ("load: unable to determine file format for `%s'",
	   orig_fname.c_str ());

  return retval;
}

static octave_value_list
do_load (istream& stream, const string& orig_fname, int force,
	 load_save_format format, oct_mach_info::float_format flt_fmt,
	 int list_only, int swap, int verbose, const string_vector& argv,
	 int argv_idx, int argc, int nargout)
{
  octave_value_list retval;

  ostrstream output_buf;
  int count = 0;
  for (;;)
    {
      int global = 0;
      octave_value tc;

      char *name = 0;
      char *doc = 0;

      switch (format)
	{
	case LS_ASCII:
	  name = read_ascii_data (stream, orig_fname, global, tc);
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

	default:
	  gripe_unrecognized_data_fmt ("load");
	  break;
	}

      if (error_state || stream.eof () || ! name)
	{
	  delete [] name;
	  delete [] doc;

	  break;
	}
      else if (! error_state && name)
	{
	  if (tc.is_defined ())
	    {
	      if (argv_idx == argc
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
			    << setiosflags (ios::left)
			    << setw (16) << tc.type_name () . c_str ()
			    << setiosflags (ios::right)
			    << setw (7) << tc.rows ()
			    << setw (7) << tc.columns ()
			    << "   ";
			}
		      output_buf << name << "\n";
		    }
		  else
		    {
		      install_loaded_variable (force, name, tc, global, doc);
		    }
		}

	      delete [] name;
	      delete [] doc;

	      // Only attempt to read one item from a headless text file.

	      if (format == LS_MAT_ASCII)
		break;
	    }
	  else
	    error ("load: unable to load variable `%s'", name);
	}
      else
	{
	  if (count == 0)
	    error ("load: are you sure `%s' is an Octave data file?",
		   orig_fname.c_str ());

	  delete [] name;
	  delete [] doc;

	  break;
	}
    }

  if (list_only && count)
    {
      output_buf << ends;

      char *msg = output_buf.str ();

      if (nargout > 0)
	retval = msg;
      else
	octave_stdout << msg;

      delete [] msg;
    }

  return retval;
}

DEFUN_TEXT (load, args, nargout,
  "load [-force] [-ascii] [-binary] [-mat-binary] file [pattern ...]\n\
\n\
Load variables from a file.\n\
\n\
If no argument is supplied to select a format, load tries to read the\n\
named file as an Octave binary, then as a .mat file, and then as an\n\
Octave text file.\n\
\n\
If the option -force is given, variables with the same names as those\n\
found in the file will be replaced with the values read from the file.")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("load");

  if (error_state)
    return retval;

  int force = 0;

  // It isn't necessary to have the default load format stored in a
  // user preference variable since we can determine the type of file
  // as we are reading.

  load_save_format format = LS_UNKNOWN;

  int list_only = 0;
  int verbose = 0;

  int i;
  for (i = 1; i < argc; i++)
    {
      if (argv[i] == "-force" || argv[i] == "-f")
	{
	  force++;
	}
      else if (argv[i] == "-list" || argv[i] == "-l")
	{
	  list_only = 1;
	}
      else if (argv[i] == "-verbose" || argv[i] == "-v")
	{
	  verbose = 1;
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
	  format = LS_MAT_BINARY;
	}
      else
	break;
    }

  if (i == argc)
    {
      print_usage ("load");
      return retval;
    }

  string orig_fname = argv[i];

  oct_mach_info::float_format flt_fmt = oct_mach_info::unknown;

  int swap = 0;

  if (argv[i] == "-")
    {
      i++;

      if (format != LS_UNKNOWN)
	{
	  // XXX FIXME XXX -- if we have already seen EOF on a
	  // previous call, how do we fix up the state of cin so that
	  // we can get additional input?  I'm afraid that we can't
	  // fix this using cin only.

	  retval = do_load (cin, orig_fname, force, format, flt_fmt,
			    list_only, swap, verbose, argv, i, argc,
			    nargout);
	}
      else
	error ("load: must specify file format if reading from stdin");
    }
  else
    {
      string fname = file_ops::tilde_expand (argv[i]);

      if (format == LS_UNKNOWN)
	format = get_file_format (fname, orig_fname);

      if (format != LS_UNKNOWN)
	{
	  i++;

	  unsigned mode = ios::in;
	  if (format == LS_BINARY || format == LS_MAT_BINARY)
	    mode |= ios::bin;

	  ifstream file (fname.c_str (), mode);

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

	      retval = do_load (file, orig_fname, force, format,
				flt_fmt, list_only, swap, verbose,
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

// Return nonzero if PATTERN has any special globbing chars in it.

static int
glob_pattern_p (const string& pattern)
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
	  return 1;

	case '[':	// Only accept an open brace if there is a close
	  open++;	// brace to match it.  Bracket expressions must be
	  continue;	// complete, according to Posix.2

	case ']':
	  if (open)
	    return 1;
	  continue;
	  
	case '\\':
	  if (i == len - 1)
	    return 0;

	default:
	  continue;
	}
    }

  return 0;
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
  else if (max_val < 4294967295 && min_val > -1)
    st = LS_U_INT;
  else if (max_val < 128 && min_val >= -128)
    st = LS_CHAR;
  else if (max_val < 32768 && min_val >= -32768)
    st = LS_SHORT;
  else if (max_val < 2147483648 && min_val > -2147483648)
    st = LS_INT;

  return st;
}

// Save the data from TC along with the corresponding NAME, help
// string DOC, and global flag MARK_AS_GLOBAL on stream OS in the
// binary format described above for read_binary_data.

static bool
save_binary_data (ostream& os, const octave_value& tc,
		  const string& name, const string& doc,
		  int mark_as_global, int save_as_floats) 
{
  FOUR_BYTE_INT name_len = name.length ();

  os.write (&name_len, 4);
  os << name;

  FOUR_BYTE_INT doc_len = doc.length ();

  os.write (&doc_len, 4);
  os << doc;

  char tmp;

  tmp = mark_as_global;
  os.write (&tmp, 1);

  if (tc.is_real_scalar ())
    {
      tmp = 1;
      os.write (&tmp, 1);
      tmp = (char) LS_DOUBLE;
      os.write (&tmp, 1);
      double tmp = tc.double_value ();
      os.write (&tmp, 8);
    }
  else if (tc.is_real_matrix ())
    {
      tmp = 2;
      os.write (&tmp, 1);
      Matrix m = tc.matrix_value ();
      FOUR_BYTE_INT nr = m.rows ();
      FOUR_BYTE_INT nc = m.columns ();
      os.write (&nr, 4);
      os.write (&nc, 4);
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
      os.write (&tmp, 1);
      tmp = (char) LS_DOUBLE;
      os.write (&tmp, 1);
      Complex tmp = tc.complex_value ();
      os.write (&tmp, 16);
    }
  else if (tc.is_complex_matrix ())
    {
      tmp = 4;
      os.write (&tmp, 1);
      ComplexMatrix m = tc.complex_matrix_value ();
      FOUR_BYTE_INT nr = m.rows ();
      FOUR_BYTE_INT nc = m.columns ();
      os.write (&nr, 4);
      os.write (&nc, 4);
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
      write_doubles (os, static_cast<const double *> (mtmp), st, 2*len);
    }
  else if (tc.is_string ())
    {
      tmp = 7;
      os.write (&tmp, 1);
      FOUR_BYTE_INT nr = tc.rows ();
      os.write (&nr, 4);
      charMatrix chm = tc.char_matrix_value ();
      for (int i = 0; i < nr; i++)
	{
	  FOUR_BYTE_INT len = chm.cols ();
	  os.write (&len, 4);
	  string tstr = chm.row_as_string (i);
	  const char *tmp = tstr.data ();
	  os.write (tmp, len);
	}
    }
  else if (tc.is_range ())
    {
      tmp = 6;
      os.write (&tmp, 1);
      tmp = (char) LS_DOUBLE;
      os.write (&tmp, 1);
      Range r = tc.range_value ();
      double bas = r.base ();
      double lim = r.limit ();
      double inc = r.inc ();
      os.write (&bas, 8);
      os.write (&lim, 8);
      os.write (&inc, 8);
    }
  else
    gripe_wrong_type_arg ("save", tc, false);

  return os;
}

// Save the data from TC along with the corresponding NAME on stream OS 
// in the MatLab binary format.

static bool
save_mat_binary_data (ostream& os, const octave_value& tc,
		      const string& name) 
{
  FOUR_BYTE_INT mopt = 0;

  mopt += tc.is_string () ? 1 : 0;

  oct_mach_info::float_format flt_fmt =
    oct_mach_info::native_float_format ();;

  mopt += 1000 * float_format_to_mopt_digit (flt_fmt);

  os.write (&mopt, 4);
  
  FOUR_BYTE_INT nr = tc.rows ();
  os.write (&nr, 4);

  FOUR_BYTE_INT nc = tc.columns ();
  os.write (&nc, 4);

  int len = nr * nc;

  FOUR_BYTE_INT imag = tc.is_complex_type () ? 1 : 0;
  os.write (&imag, 4);

  // LEN includes the terminating character, and the file is also
  // supposed to include it.

  FOUR_BYTE_INT name_len = name.length () + 1;

  os.write (&name_len, 4);
  os << name << '\0';

  if (tc.is_real_scalar ())
    {
      double tmp = tc.double_value ();
      os.write (&tmp, 8);
    }
  else if (tc.is_real_matrix ())
    {
      Matrix m = tc.matrix_value ();
      os.write (m.data (), 8 * len);
    }
  else if (tc.is_complex_scalar ())
    {
      Complex tmp = tc.complex_value ();
      os.write (&tmp, 16);
    }
  else if (tc.is_complex_matrix ())
    {
      ComplexMatrix m_cmplx = tc.complex_matrix_value ();
      Matrix m = ::real(m_cmplx);
      os.write (m.data (), 8 * len);
      m = ::imag(m_cmplx);
      os.write (m.data (), 8 * len);
    }
  else if (tc.is_string ())
    {
      unwind_protect::begin_frame ("save_mat_binary_data");
      unwind_protect_int (Vimplicit_str_to_num_ok);
      Vimplicit_str_to_num_ok = 1;
      Matrix m = tc.matrix_value ();
      os.write (m.data (), 8 * len);
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
	  os.write (&x, 8);
	}
    }
  else
    gripe_wrong_type_arg ("save", tc, false);

  return os;
}

static void
ascii_save_type (ostream& os, const char *type, bool mark_as_global)
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
// If STRIP_NAN_AND_INF is nonzero, rows containing NaNs are deleted,
// and Infinite values are converted to +/-OCT_RBV (A Real Big Value,
// but not so big that gnuplot can't handle it when trying to compute
// axis ranges, etc.).
//
// Assumes ranges and strings cannot contain Inf or NaN values.
//
// Returns 1 for success and 0 for failure.

// XXX FIXME XXX -- should probably write the help string here too.

bool
save_ascii_data (ostream& os, const octave_value& tc,
		 const string& name, bool strip_nan_and_inf,
		 bool mark_as_global, int precision) 
{
  bool success = true;

  if (! precision)
    precision = Vsave_precision;

  if (! name.empty ())
    os << "# name: " << name << "\n";

  long old_precision = os.precision ();
  os.precision (precision);

  if (tc.is_real_scalar ())
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
	      os << d << "\n";
	    }
	}
      else
	os << d << "\n";
    }
  else if (tc.is_real_matrix ())
    {
      ascii_save_type (os, "matrix", mark_as_global);
      os << "# rows: " << tc.rows () << "\n"
	 << "# columns: " << tc.columns () << "\n";

      Matrix tmp = tc.matrix_value ();
      if (strip_nan_and_inf)
	tmp = strip_infnan (tmp);

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

	      os << c << "\n";
	    }
	}
      else
	os << c << "\n";
    }
  else if (tc.is_complex_matrix ())
    {
      ascii_save_type (os, "complex matrix", mark_as_global);
      os << "# rows: " << tc.rows () << "\n"
	 << "# columns: " << tc.columns () << "\n";

      ComplexMatrix tmp = tc.complex_matrix_value ();
      if (strip_nan_and_inf)
	tmp = strip_infnan (tmp);

      os << tmp;
    }
  else if (tc.is_string ())
    {
      ascii_save_type (os, "string array", mark_as_global);
      charMatrix chm = tc.char_matrix_value ();
      int elements = chm.rows ();
      os << "# elements: " << elements << "\n";
      for (int i = 0; i < elements; i++)
	{
	  int len = chm.cols ();
	  os << "# length: " << len << "\n";
	  string tstr = chm.row_as_string (i);
	  const char *tmp = tstr.data ();
	  os.write (tmp, len);
	  os << "\n";
	}
    }
  else if (tc.is_range ())
    {
      ascii_save_type (os, "range", mark_as_global);
      Range tmp = tc.range_value ();
      os << "# base, limit, increment\n"
	 << tmp.base () << " "
	 << tmp.limit () << " "
	 << tmp.inc () << "\n";
    }
  else
    gripe_wrong_type_arg ("save", tc, false);

  os.precision (old_precision);

  return (os && success);
}

// Save the info from sr on stream os in the format specified by fmt.

static void
do_save (ostream& os, symbol_record *sr, load_save_format fmt,
	 int save_as_floats)
{
  if (! sr->is_variable ())
    {
      error ("save: can only save variables, not functions");
      return;
    }

  string name = sr->name ();
  string help = sr->help ();
  int global = sr->is_linked_to_global ();

  octave_value tc = sr->def ();

  if (tc.is_undefined ())
    return;

  switch (fmt)
    {
    case LS_ASCII:
      save_ascii_data (os, tc, name, false, global);
      break;

    case LS_BINARY:
      save_binary_data (os, tc, name, help, global, save_as_floats);
      break;

    case LS_MAT_BINARY:
      save_mat_binary_data (os, tc, name);
      break;

    default:
      gripe_unrecognized_data_fmt ("save");
      break;
    }
}

// Save variables with names matching PATTERN on stream OS in the
// format specified by FMT.  If SAVE_BUILTINS is nonzero, also save
// builtin variables with names that match PATTERN.

static int
save_vars (ostream& os, const string& pattern, int save_builtins,
	   load_save_format fmt, int save_as_floats)
{
  int count;

  symbol_record **vars = curr_sym_tab->glob
    (count, pattern, symbol_record::USER_VARIABLE, SYMTAB_ALL_SCOPES);

  int saved = count;

  int i;

  for (i = 0; i < count; i++)
    {
      do_save (os, vars[i], fmt, save_as_floats);

      if (error_state)
	break;
    }

  delete [] vars;

  if (! error_state && save_builtins)
    {
      symbol_record **vars = global_sym_tab->glob
	(count, pattern, symbol_record::BUILTIN_VARIABLE, SYMTAB_ALL_SCOPES);

      saved += count;

      for (i = 0; i < count; i++)
	{
	  do_save (os, vars[i], fmt, save_as_floats);

	  if (error_state)
	    break;
	}

      delete [] vars;
    }

  return saved;
}

static load_save_format
get_default_save_format (void)
{
  load_save_format retval = LS_ASCII;

  string fmt = Vdefault_save_format;

  if (fmt == "binary")
    retval = LS_BINARY;
  else if (fmt == "mat-binary" || fmt =="mat_binary")
    retval = LS_MAT_BINARY;
      
  return retval;
}

static void
write_binary_header (ostream& os, load_save_format format)
{
  if (format == LS_BINARY)
    {
      os << (oct_mach_info::words_big_endian ()
	     ? "Octave-1-B" : "Octave-1-L");

      oct_mach_info::float_format flt_fmt =
	oct_mach_info::native_float_format ();

      char tmp = (char) float_format_to_mopt_digit (flt_fmt);

      os.write (&tmp, 1);
    }
}

static void
save_vars (const string_vector& argv, int argv_idx, int argc,
	   ostream& os, int save_builtins, load_save_format fmt,
	   int save_as_floats) 
{
  write_binary_header (os, fmt);

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
  // XXX FIXME XXX -- should choose better file name?

  const char *fname = "octave-core";

  message (0, "attempting to save variables to `%s'...", fname);

  load_save_format format = get_default_save_format ();

  unsigned mode = ios::out|ios::trunc;
  if (format == LS_BINARY || format == LS_MAT_BINARY)
    mode |= ios::bin;

  ofstream file (fname, mode);

  if (file)
    {
      save_vars (string_vector (), 0, 0, file, 0, format, 0);
      message (0, "save to `%s' complete", fname);
    }
  else
    warning ("unable to open `%s' for writing...", fname);
}

DEFUN_TEXT (save, args, ,
  "save [-ascii] [-binary] [-float-binary] [-mat-binary] \n\
     [-save-builtins] file [pattern ...]\n\
\n\
save variables in a file")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("save");

  if (error_state)
    return retval;

  // Here is where we would get the default save format if it were
  // stored in a user preference variable.

  int save_builtins = 0;

  int save_as_floats = 0;

  load_save_format format = get_default_save_format ();

  int i;
  for (i = 1; i < argc; i++)
    {
      if (argv[i] == "-ascii" || argv[i] == "-a")
	{
	  format = LS_ASCII;
	}
      else if (argv[i] == "-binary" || argv[i] == "-b")
	{
	  format = LS_BINARY;
	}
      else if (argv[i] == "-mat-binary" || argv[i] == "-m")
	{
	  format = LS_MAT_BINARY;
	}
      else if (argv[i] == "-float-binary" || argv[i] == "-f")
	{
	  format = LS_BINARY;
	  save_as_floats = 1;
	}
      else if (argv[i] == "-save-builtins")
	{
	  save_builtins = 1;
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

      // XXX FIXME XXX -- should things intended for the screen end up
      // in a octave_value (string)?

      save_vars (argv, i, argc, octave_stdout, save_builtins, format,
		 save_as_floats);
    }

  // Guard against things like `save a*', which are probably mistakes...

  else if (i == argc - 1 && glob_pattern_p (argv[i]))
    {
      print_usage ("save");
      return retval;
    }
  else
    {
      string fname = file_ops::tilde_expand (argv[i]);

      i++;

      unsigned mode = ios::out|ios::trunc;
      if (format == LS_BINARY || format == LS_MAT_BINARY)
	mode |= ios::bin;

      ofstream file (fname.c_str (), mode);

      if (file)
	{
	  save_vars (argv, i, argc, file, save_builtins, format,
		     save_as_floats);
	}
      else
	{
	  error ("save: couldn't open output file `%s'", fname.c_str ());
	  return retval;
	}
    }

  return retval;
}

// Maybe this should be a static function in tree-plot.cc?

// If TC is matrix, save it on stream OS in a format useful for
// making a 3-dimensional plot with gnuplot.  If PARAMETRIC is
// nonzero, assume a parametric 3-dimensional plot will be generated.

bool
save_three_d (ostream& os, const octave_value& tc, bool parametric)
{
  int fail = 0;

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
      fail = 1;
    }

  return (os && ! fail);
}

static int
default_save_format (void)
{
  int status = 0;

  string s = builtin_string_variable ("default_save_format");

  if (s.empty ())
    {
      gripe_invalid_value_specified ("default_save_format");
      status = -1;
    }
  else
    Vdefault_save_format = s;

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
  DEFVAR (default_save_format, "ascii", 0, default_save_format,
    "default format for files created with save, may be one of\n\
\"binary\", \"text\", or \"mat-binary\"");

  DEFVAR (save_precision, 15.0, 0, save_precision,
    "number of significant figures kept by the ASCII save command");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
