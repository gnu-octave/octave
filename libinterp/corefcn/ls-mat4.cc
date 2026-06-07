////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2026 The Octave Project Developers
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

#include <cstdint>
#include <cstring>

#include <istream>
#include <limits>
#include <ostream>
#include <string>
#include <utility>

#include "CMatrix.h"
#include "CSparse.h"
#include "byte-swap.h"
#include "dMatrix.h"
#include "dSparse.h"
#include "data-conv.h"
#include "lo-utils.h"
#include "mach-info.h"
#include "oct-locbuf.h"

#include "error.h"
#include "load-save.h"
#include "ls-mat4.h"


// Read LEN elements of data from IS in the format specified by PRECISION,
// placing the result in DATA.  If SWAP is TRUE, swap the bytes of each element
// before copying to DATA.  FLT_FMT specifies the format of the data if we are
// reading floating point numbers.

static void
read_mat_binary_data (std::istream& is, double *data,
                      int precision, octave_idx_type len, bool swap,
                      octave::mach_info::float_format flt_fmt)
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

int
read_mat_file_header (std::istream& is, bool& swap, int32_t& mopt,
                      int32_t& nr, int32_t& nc, int32_t& imag, int32_t& len,
                      int quiet)
{
  swap = false;

  // We expect to fail here, at the beginning of a record, so not being able to
  // read another mopt value should not result in an error.
  is.read (reinterpret_cast<char *> (&mopt), 4);
  if (! is)
    return 1;

  if (! is.read (reinterpret_cast<char *> (&nr), 4))
    return -1;

  if (! is.read (reinterpret_cast<char *> (&nc), 4))
    return -1;

  if (! is.read (reinterpret_cast<char *> (&imag), 4))
    return -1;

  if (! is.read (reinterpret_cast<char *> (&len), 4))
    return -1;

  // If mopt is nonzero and the byte order is swapped, mopt will be bigger than
  // we expect, so we swap bytes.
  //
  // If mopt is zero, it means the file was written on a little endian machine,
  // and we only need to swap if we are running on a big endian machine.
  //
  // Gag me.
  if (octave::mach_info::words_big_endian () && mopt == 0)
    swap = true;

  // mopt is signed, therefore byte swap may result in a negative value.
  if (mopt < 0 || mopt > 9999)
    swap = true;

  if (swap)
    {
      swap_bytes<4> (&mopt);
      swap_bytes<4> (&nr);
      swap_bytes<4> (&nc);
      swap_bytes<4> (&imag);
      swap_bytes<4> (&len);
    }

  if (mopt < 0 || mopt > 9999 || imag < 0 || imag > 1)
    {
      if (! quiet)
        error ("load: unrecognized MATv4 header");

      return -1;
    }

  return 0;
}

// We don't just use a cast here, because we need to be able to detect
// possible errors.

octave::mach_info::float_format
mopt_digit_to_float_format (int mach)
{
  octave::mach_info::float_format flt_fmt = octave::mach_info::flt_fmt_unknown;

  switch (mach)
    {
    case 0:
      flt_fmt = octave::mach_info::flt_fmt_ieee_little_endian;
      break;

    case 1:
      flt_fmt = octave::mach_info::flt_fmt_ieee_big_endian;
      break;

    case 2: // VAX D-Float
      error ("load: unsupported format MATv4 VAX D-Float");

    case 3: // VAX G-Float
      error ("load: unsupported format MATv4 VAX G-Float");

    case 4: // Cray
      error ("load: unsupported format MATv4 Cray");

    default:
      error ("load: unrecognized MATv4 MACHINE format");
    }

  return flt_fmt;
}

int
float_format_to_mopt_digit (octave::mach_info::float_format flt_fmt)
{
  int retval;

  switch (flt_fmt)
    {
    case octave::mach_info::flt_fmt_ieee_little_endian:
      retval = 0;
      break;

    case octave::mach_info::flt_fmt_ieee_big_endian:
      retval = 1;
      break;

    default:
      error ("float_format_to_mopt_digit: unrecognized flt_format, please report");
      break;
    }

  return retval;
}

// Extract one value (scalar, matrix, string, etc.) from stream IS and place it
// in TC, returning the name of the variable.
//
// The data is expected to be in Matlab version 4 .mat format, though not all
// the features of that format are supported.
//
// FILENAME is used for error messages.
//
// This format provides no way to tag the data as global.

std::string
read_mat_binary_data (std::istream& is, const std::string& filename,
                      octave_value& tc)
{
  std::string retval;

  int32_t mopt, nr, nc, imag, namelen;
  bool swap = false;

  int err = read_mat_file_header (is, swap, mopt, nr, nc, imag, namelen);
  if (err)
    {
      if (err < 0)
        error ("load: unable to read header of MATv4 file '%s'", filename.c_str ());

      return retval;
    }

  int type  = 0;
  int prec  = 0;
  int order = 0;
  int mach  = 0;

  type = mopt % 10;  // Sparse, string, full array.
  mopt /= 10;        // Eliminate first digit.
  prec = mopt % 10;  // Storage precision: double, float, int, etc.
  mopt /= 10;        // Eliminate second digit.
  order = mopt % 10; // Row or column major ordering.
  mopt /= 10;        // Eliminate third digit.
  mach = mopt % 10;  // Little-endian, Big-endian, VAX, etc.

  octave::mach_info::float_format flt_fmt;
  flt_fmt = mopt_digit_to_float_format (mach);

  if (imag && type == 1)
    error ("load: found MATv4 complex matrix with string flag set");

  octave_idx_type datalen = 0;

  // LEN includes the terminating character, and the file is also supposed to
  // include it, but apparently not all files do.  Either way, this should
  // work.
  {
    OCTAVE_LOCAL_BUFFER (char, name, namelen+1);
    name[namelen] = '\0';
    if (! is.read (name, namelen))
      error ("load: could not read NAME field from MATv4 file '%s'", filename.c_str ());
    retval = name;

    bool mult_overflow =
      octave::math::int_multiply_overflow (static_cast<octave_idx_type> (nr),
                                           static_cast<octave_idx_type> (nc),
                                           &datalen);
    if (mult_overflow)
      error ("load: size of data exceeds octave_idx_type in MATv4 file '%s'",
             filename.c_str ());

    if (order)
      std::swap (nr, nc);

    if (type == 2)  // Sparse
      {
        if (nc == 4)
          {
            octave_idx_type nr_new, nc_new;
            Array<Complex> data (dim_vector (1, nr - 1));
            Array<octave_idx_type> c (dim_vector (1, nr - 1));
            Array<octave_idx_type> r (dim_vector (1, nr - 1));
            OCTAVE_LOCAL_BUFFER (double, dtmp, nr);
            OCTAVE_LOCAL_BUFFER (double, ctmp, nr);

            read_mat_binary_data (is, dtmp, prec, nr, swap, flt_fmt);
            for (octave_idx_type i = 0; i < nr - 1; i++)
              r.xelem (i) = dtmp[i] - 1;
            nr_new = dtmp[nr - 1];
            read_mat_binary_data (is, dtmp, prec, nr, swap, flt_fmt);
            for (octave_idx_type i = 0; i < nr - 1; i++)
              c.xelem (i) = dtmp[i] - 1;
            nc_new = dtmp[nr - 1];
            read_mat_binary_data (is, dtmp, prec, nr - 1, swap, flt_fmt);
            read_mat_binary_data (is, ctmp, prec, 1, swap, flt_fmt);
            read_mat_binary_data (is, ctmp, prec, nr - 1, swap, flt_fmt);

            for (octave_idx_type i = 0; i < nr - 1; i++)
              data.xelem (i) = Complex (dtmp[i], ctmp[i]);
            read_mat_binary_data (is, ctmp, prec, 1, swap, flt_fmt);

            SparseComplexMatrix smc = SparseComplexMatrix (data, r, c,
                                                           nr_new, nc_new);

            tc = (order ? smc.transpose () : smc);
          }
        else
          {
            octave_idx_type nr_new, nc_new;
            Array<double> data (dim_vector (1, nr - 1));
            Array<octave_idx_type> c (dim_vector (1, nr - 1));
            Array<octave_idx_type> r (dim_vector (1, nr - 1));
            OCTAVE_LOCAL_BUFFER (double, dtmp, nr);

            read_mat_binary_data (is, dtmp, prec, nr, swap, flt_fmt);
            for (octave_idx_type i = 0; i < nr - 1; i++)
              r.xelem (i) = dtmp[i] - 1;
            nr_new = dtmp[nr - 1];
            read_mat_binary_data (is, dtmp, prec, nr, swap, flt_fmt);
            for (octave_idx_type i = 0; i < nr - 1; i++)
              c.xelem (i) = dtmp[i] - 1;
            nc_new = dtmp[nr - 1];
            read_mat_binary_data (is, data.rwdata (), prec, nr - 1,
                                  swap, flt_fmt);
            read_mat_binary_data (is, dtmp, prec, 1, swap, flt_fmt);

            SparseMatrix sm = SparseMatrix (data, r, c, nr_new, nc_new);

            tc = (order ? sm.transpose () : sm);
          }
      }
    else  // Full Matrix (Real, Complex, or String)
      {
        Matrix re (nr, nc);

        read_mat_binary_data (is, re.rwdata (), prec, datalen, swap, flt_fmt);

        if (! is)
          {
            if (! imag)
              error ("load: reading matrix data for variable '%s'", name);
            else
              error ("load: reading real matrix data for variable '%s'", name);
          }

        if (imag)
          {
            Matrix im (nr, nc);

            read_mat_binary_data (is, im.rwdata (), prec, datalen, swap,
                                  flt_fmt);

            if (! is)
              error ("load: reading imaginary matrix data for variable '%s'", name);

            ComplexMatrix ctmp (nr, nc);

            for (octave_idx_type j = 0; j < nc; j++)
              for (octave_idx_type i = 0; i < nr; i++)
                ctmp.xelem (i, j) = Complex (re(i, j), im(i, j));

            tc = (order ? ctmp.transpose () : ctmp);
          }
        else
          tc = (order ? re.transpose () : re);

        if (type == 1)
          tc = tc.convert_to_str (false, true, '\'');
      }

    return retval;
  }
}

static void
warn_dim_too_large (const std::string& name)
{
  warning_with_id ("Octave:save:dimension-too-large",
                   "save: skipping %s: dimension too large for MATv4 format",
                   name.c_str ());
}

static void
warn_wrong_type_arg (const std::string& name, const octave_value& tc)
{
  std::string type = tc.type_name ();

  warning_with_id ("Octave:save:invalid-type",
                   "save: skipping %s: invalid type argument '%s'",
                   name.c_str (), type.c_str ());
}

// Save the data from TC along with the corresponding NAME on stream OS in the
// MatLab version 4 binary format.  Return true on success.
bool
save_mat_binary_data (std::ostream& os, const octave_value& tc,
                      const std::string& name)
{
  octave_idx_type len;
  int32_t nr, nc;
  constexpr octave_idx_type max_dim_val = std::numeric_limits<int32_t>::max ();
  const int MAX_NAMELEN = 63;  // A Matlab limit, but good idea to enforce

  // Validate rows, cols, and len
  if (tc.rows () > max_dim_val)
    {
      warn_dim_too_large (name);
      return true;
    }
  else
    nr = tc.rows ();

  if (tc.columns () > max_dim_val)
    {
      warn_dim_too_large (name);
      return true;
    }
  else
    nc = tc.columns ();

  // FIXME: Maybe validate len?
  /*
  if (tc.issparse ())
    len = tc.nnz ();
  else
    len = static_cast<octave_idx_type> (nr) * nc;
  */

  if (! (tc.is_double_type () || tc.is_string ()))
    {
      warn_wrong_type_arg (name, tc);
      return true;
    }

  int32_t mopt = 0;

  // Set (M)achine format (litte-endian or big-endian)
  octave::mach_info::float_format flt_fmt
    = octave::mach_info::native_float_format ();

  mopt += 1000 * float_format_to_mopt_digit (flt_fmt);

  // Octave saves all data in column-major order (O = 0)
  // Octave saves all data as 8-byte floating point (P = 0)

  // Set (T)ype of variable
  mopt += tc.issparse () ? 2 : tc.is_string () ? 1 : 0;

  os.write (reinterpret_cast<char *> (&mopt), 4);

  // Write Number of Rows, Number of Columns, and Imaginary Flag
  if (tc.issparse ())
    {
      len = tc.nnz ();
      uint32_t nnz = len + 1;
      os.write (reinterpret_cast<char *> (&nnz), 4);

      uint32_t iscmplx = (tc.iscomplex () ? 4 : 3);
      os.write (reinterpret_cast<char *> (&iscmplx), 4);

      uint32_t tmp = 0;
      os.write (reinterpret_cast<char *> (&tmp), 4);
    }
  else
    {
      os.write (reinterpret_cast<char *> (&nr), 4);
      os.write (reinterpret_cast<char *> (&nc), 4);

      int32_t imag = (tc.iscomplex () ? 1 : 0);
      os.write (reinterpret_cast<char *> (&imag), 4);

      len = static_cast<octave_idx_type> (nr) * nc;
    }

  // Write length of variable name (possibly truncated) 
  int32_t namelen = name.length ();
  if (namelen > MAX_NAMELEN)
    namelen = MAX_NAMELEN;
  // NAME_LEN must include the terminating character '\0'.
  namelen += 1;

  os.write (reinterpret_cast<char *> (&namelen), 4);
  OCTAVE_LOCAL_BUFFER (char, namebuf, namelen);
  strncpy (namebuf, name.c_str (), namelen - 1);
  namebuf[namelen - 1] = '\0'; 
  os.write (namebuf, namelen);

  // Write data
  if (tc.is_string ())
    {
      charMatrix chm = tc.char_matrix_value ();

      octave_idx_type nrow = chm.rows ();
      octave_idx_type ncol = chm.cols ();

      OCTAVE_LOCAL_BUFFER (double, buf, ncol*nrow);

      for (octave_idx_type i = 0; i < nrow; i++)
        {
          std::string tstr = chm.row_as_string (i);
          const char *s = tstr.data ();

          for (octave_idx_type j = 0; j < ncol; j++)
            buf[j*nrow+i] = static_cast<double> (*s++ & 0x00FF);
        }
      std::streamsize n_bytes = static_cast<std::streamsize> (nrow) *
                                static_cast<std::streamsize> (ncol) *
                                sizeof (double);
      os.write (reinterpret_cast<char *> (buf), n_bytes);
    }
  else if (tc.issparse ())
    {
      double ds;
      OCTAVE_LOCAL_BUFFER (double, dtmp, len);
      if (tc.is_complex_matrix ())
        {
          SparseComplexMatrix m = tc.sparse_complex_matrix_value ();

          for (octave_idx_type i = 0; i < len; i++)
            dtmp[i] = m.ridx (i) + 1;
          std::streamsize n_bytes = 8 * static_cast<std::streamsize> (len);
          os.write (reinterpret_cast<const char *> (dtmp), n_bytes);
          ds = nr;
          os.write (reinterpret_cast<const char *> (&ds), 8);

          octave_idx_type ii = 0;
          for (octave_idx_type j = 0; j < nc; j++)
            for (octave_idx_type i = m.cidx (j); i < m.cidx (j+1); i++)
              dtmp[ii++] = j + 1;
          os.write (reinterpret_cast<const char *> (dtmp), n_bytes);
          ds = nc;
          os.write (reinterpret_cast<const char *> (&ds), 8);

          for (octave_idx_type i = 0; i < len; i++)
            dtmp[i] = std::real (m.data (i));
          os.write (reinterpret_cast<const char *> (dtmp), n_bytes);
          ds = 0.;
          os.write (reinterpret_cast<const char *> (&ds), 8);

          for (octave_idx_type i = 0; i < len; i++)
            dtmp[i] = std::imag (m.data (i));
          os.write (reinterpret_cast<const char *> (dtmp), n_bytes);
          os.write (reinterpret_cast<const char *> (&ds), 8);
        }
      else
        {
          SparseMatrix m = tc.sparse_matrix_value ();

          for (octave_idx_type i = 0; i < len; i++)
            dtmp[i] = m.ridx (i) + 1;
          std::streamsize n_bytes = 8 * static_cast<std::streamsize> (len);
          os.write (reinterpret_cast<const char *> (dtmp), n_bytes);
          ds = nr;
          os.write (reinterpret_cast<const char *> (&ds), 8);

          octave_idx_type ii = 0;
          for (octave_idx_type j = 0; j < nc; j++)
            for (octave_idx_type i = m.cidx (j); i < m.cidx (j+1); i++)
              dtmp[ii++] = j + 1;
          os.write (reinterpret_cast<const char *> (dtmp), n_bytes);
          ds = nc;
          os.write (reinterpret_cast<const char *> (&ds), 8);

          os.write (reinterpret_cast<const char *> (m.data ()), n_bytes);
          ds = 0.;
          os.write (reinterpret_cast<const char *> (&ds), 8);
        }
    }
  else if (tc.is_real_scalar ())
    {
      double tmp = tc.double_value ();
      os.write (reinterpret_cast<char *> (&tmp), 8);
    }
  else if (tc.is_real_matrix () || tc.is_range ())
    {
      Matrix m = tc.matrix_value ();
      std::streamsize n_bytes = 8 * static_cast<std::streamsize> (len);
      os.write (reinterpret_cast<const char *> (m.data ()), n_bytes);
    }
  else if (tc.is_complex_scalar ())
    {
      Complex tmp = tc.complex_value ();
      os.write (reinterpret_cast<char *> (&tmp), 16);
    }
  else if (tc.is_complex_matrix ())
    {
      ComplexMatrix m_cmplx = tc.complex_matrix_value ();
      Matrix m = ::real (m_cmplx);
      std::streamsize n_bytes = 8 * static_cast<std::streamsize> (len);
      os.write (reinterpret_cast<const char *> (m.data ()), n_bytes);
      m = ::imag (m_cmplx);
      os.write (reinterpret_cast<const char *> (m.data ()), n_bytes);
    }
  else
    {
      error ("save: operation failed while writing variable %s", name.c_str ());
    }

  return ! os.fail ();
}
