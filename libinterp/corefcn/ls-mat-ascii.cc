////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cctype>

#include <iomanip>
#include <istream>
#include <ostream>
#include <sstream>
#include <string>

#include "byte-swap.h"
#include "dMatrix.h"
#include "data-conv.h"
#include "file-ops.h"
#include "glob-match.h"
#include "lo-mappers.h"
#include "mach-info.h"
#include "oct-env.h"
#include "oct-time.h"
#include "quit.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "interpreter-private.h"
#include "interpreter.h"
#include "lex.h"
#include "load-save.h"
#include "ls-ascii-helper.h"
#include "ls-mat-ascii.h"
#include "oct-map.h"
#include "ov-cell.h"
#include "ov.h"
#include "pager.h"
#include "sysdep.h"
#include "utils.h"
#include "variables.h"
#include "version.h"

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
          if (c == '\n' || c == '\r')
            {
              is.putback (c);
              octave::skip_preceeding_newline (is);
              break;
            }

          if (c == '%' || c == '#')
            {
              octave::skip_until_newline (is, false);
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
  while (! (have_data || is.eof () || is.fail ()));

  return retval;
}

static void
get_lines_and_columns (std::istream& is,
                       octave_idx_type& nr, octave_idx_type& nc,
                       const std::string& filename = "",
                       bool quiet = false, bool check_numeric = false)
{
  std::streampos pos = is.tellg ();

  int file_line_number = 0;

  nr = 0;
  nc = 0;

  while (is)
    {
      octave_quit ();

      std::string buf = get_mat_data_input_line (is);

      file_line_number++;

      std::size_t beg = buf.find_first_not_of (", \t");

      // If we see a CR as the last character in the buffer, we had a
      // CRLF pair as the line separator.  Any other CR in the text
      // will not be considered as whitespace.

      if (beg != std::string::npos && buf[beg] == '\r'
          && beg == buf.length () - 1)
        {
          // We had a blank line ending with a CRLF.  Handle it the
          // same as an empty line.
          beg = std::string::npos;
        }

      octave_idx_type tmp_nc = 0;

      while (beg != std::string::npos)
        {
          tmp_nc++;

          std::size_t end = buf.find_first_of (", \t", beg);

          if (end != std::string::npos)
            {
              if (check_numeric)
                {
                  std::istringstream tmp_stream (buf.substr (beg, end-beg));

                  octave::read_value<double> (tmp_stream);

                  if (tmp_stream.fail ())
                    {
                      if (! quiet)
                        error ("load: %s: non-numeric data found near line %d",
                               filename.c_str (), file_line_number);

                      nr = 0;
                      nc = 0;

                      goto done;
                    }
                }

              beg = buf.find_first_not_of (", \t", end);

              if (beg == std::string::npos
                  || (buf[beg] == '\r' && beg == buf.length () - 1))
                {
                  // We had a line with trailing spaces and ending with a CRLF,
                  // so this should look like EOL, not a new column.
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
            {
              if (! quiet)
                error ("load: %s: inconsistent number of columns near line %d",
                       filename.c_str (), file_line_number);

              nr = 0;
              nc = 0;

              goto done;
            }
        }
    }

  if (! quiet && (nr == 0 || nc == 0))
    error ("load: file '%s' seems to be empty!", filename.c_str ());

done:

  is.clear ();
  is.seekg (pos);
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

std::string
read_mat_ascii_data (std::istream& is, const std::string& filename,
                     octave_value& tc)
{
  std::string varname;

  std::size_t pos = filename.rfind ('/');

  if (pos != std::string::npos)
    varname = filename.substr (pos+1);
  else
    varname = filename;

  pos = varname.rfind ('.');

  if (pos != std::string::npos)
    varname = varname.substr (0, pos);

  std::size_t len = varname.length ();
  for (std::size_t i = 0; i < len; i++)
    {
      char c = varname[i];
      if (! (isalnum (c) || c == '_'))
        varname[i] = '_';
    }

  if (octave::iskeyword (varname) || ! isalpha (varname[0]))
    varname.insert (0, "X");

  if (! octave::valid_identifier (varname))
    error ("load: unable to convert filename '%s' to valid identifier",
           filename.c_str ());

  octave_idx_type nr = 0;
  octave_idx_type nc = 0;

  octave_idx_type total_count = 0;

  get_lines_and_columns (is, nr, nc, filename);

  octave_quit ();

  if (nr <= 0 || nc <= 0)
    error ("load: unable to extract matrix size from file '%s'",
           filename.c_str ());

  Matrix tmp (nr, nc);

  double d;
  for (octave_idx_type i = 0; i < nr; i++)
    {
      std::string buf = get_mat_data_input_line (is);

      std::istringstream tmp_stream (buf);

      for (octave_idx_type j = 0; j < nc; j++)
        {
          octave_quit ();

          d = octave::read_value<double> (tmp_stream);

          if (! tmp_stream && ! tmp_stream.eof ())
            error ("load: failed to read matrix from file '%s'",
                   filename.c_str ());

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
    }

  if (! is && ! is.eof ())
    error ("load: failed to read matrix from file '%s'", filename.c_str ());

  // FIXME: not sure this is best, but it works.
  if (is.eof ())
    is.clear ();

  octave_idx_type expected = nr * nc;

  if (expected != total_count)
    error ("load: expected %" OCTAVE_IDX_TYPE_FORMAT " elements, found "
           "%" OCTAVE_IDX_TYPE_FORMAT, expected, total_count);

  tc = tmp;

  return varname;
}

bool
save_mat_ascii_data (std::ostream& os, const octave_value& val,
                     int precision, bool tabs)
{
  bool success = true;

  if (val.iscomplex ())
    warning ("save: omitting imaginary part for ASCII file");

  if (val.ndims () > 2)
    {
      warning ("save: skipping variable which is not a 2-D matrix");
      return true;
    }

  Matrix m;

  try
    {
      m = val.matrix_value (true);
    }
  catch (const octave::execution_exception&)
    {
      octave::interpreter& interp = octave::__get_interpreter__ ();

      interp.recover_from_exception ();

      success = false;
    }

  if (success)
    {
      long old_precision = os.precision ();

      os.precision (precision);

      std::ios::fmtflags oflags
        = os.flags (static_cast<std::ios::fmtflags> (std::ios::scientific));

      if (tabs)
        {
          for (octave_idx_type i = 0; i < m.rows (); i++)
            {
              for (octave_idx_type j = 0; j < m.cols (); j++)
                {
                  // Omit leading tabs.
                  if (j != 0) os << '\t';
                  octave::write_value<double> (os, m(i, j));
                }
              os << "\n";
            }
        }
      else
        os << m;

      // Restore format
      os.flags (oflags);
      os.precision (old_precision);
    }

  return (os && success);
}

bool
looks_like_mat_ascii_file (std::istream& is, const std::string& filename)
{
  bool retval = false;
  octave_idx_type nr = 0;
  octave_idx_type nc = 0;

  get_lines_and_columns (is, nr, nc, filename, true, true);
  retval = (nr != 0 && nc != 0);

  return retval;
}
