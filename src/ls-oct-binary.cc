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

#include <fstream>
#include <iomanip>
#include <iostream>
#include <memory>
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

#include "ls-utils.h"
#include "ls-oct-binary.h"

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

std::string
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
	    OCTAVE_LOCAL_BUFFER (char, btmp, len+1);
	    if (! is.read (X_CAST (char *, btmp), len))
	      goto data_read_error;
	    if (len > max_len)
	      {
		max_len = len;
		chm.resize (elements, max_len, 0);
	      }
	    btmp [len] = '\0';
	    chm.insert (btmp, i, 0);
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

// Save the data from TC along with the corresponding NAME, help
// string DOC, and global flag MARK_AS_GLOBAL on stream OS in the
// binary format described above for read_binary_data.

bool
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
	  const char *btmp = tstr.data ();
	  os.write (X_CAST (char *, btmp), len);
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
      double dtmp = tc.double_value ();
      os.write (X_CAST (char *, &dtmp), 8);
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
      Complex ctmp = tc.complex_value ();
      os.write (X_CAST (char *, &ctmp), 16);
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

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

