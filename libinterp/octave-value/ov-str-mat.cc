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

#include <istream>
#include <ostream>
#include <vector>

#include "data-conv.h"
#include "lo-ieee.h"
#include "mach-info.h"
#include "mx-base.h"
#include "oct-locbuf.h"

#include "byte-swap.h"
#include "defun.h"
#include "errwarn.h"
#include "ls-ascii-helper.h"
#include "ls-hdf5.h"
#include "ls-oct-text.h"
#include "ls-utils.h"
#include "ovl.h"
#include "oct-hdf5.h"
#include "oct-stream.h"
#include "ops.h"
#include "ov-scalar.h"
#include "ov-re-mat.h"
#include "ov-str-mat.h"
#include "pr-output.h"
#include "utils.h"


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_char_matrix_str, "string", "char");
DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_char_matrix_sq_str, "sq_string",
                                     "char");

static octave_base_value *
default_numeric_conversion_function (const octave_base_value& a)
{
  octave_base_value *retval = nullptr;

  const octave_char_matrix_str& v
    = dynamic_cast<const octave_char_matrix_str&> (a);

  NDArray nda = v.array_value (true);

  if (nda.numel () == 1)
    retval = new octave_scalar (nda(0));
  else
    retval = new octave_matrix (nda);

  return retval;
}

octave_base_value::type_conv_info
octave_char_matrix_str::numeric_conversion_function (void) const
{
  return octave_base_value::type_conv_info (default_numeric_conversion_function,
         octave_matrix::static_type_id ());
}

octave_value
octave_char_matrix_str::do_index_op_internal (const octave_value_list& idx,
    bool resize_ok, char type)
{
  octave_value retval;

  octave_idx_type len = idx.length ();

  // If we catch an indexing error in index_vector, we flag an error in
  // index k.  Ensure it is the right value before each idx_vector call.
  // Same variable as used in the for loop in the default case.

  octave_idx_type k = 0;

  try
    {
      switch (len)
        {
        case 0:
          retval = octave_value (m_matrix, type);
          break;

        case 1:
          {
            octave::idx_vector i = idx (0).index_vector ();

            retval = octave_value (charNDArray (m_matrix.index (i, resize_ok)),
                                   type);
          }
          break;

        case 2:
          {
            octave::idx_vector i = idx (0).index_vector ();
            k = 1;
            octave::idx_vector j = idx (1).index_vector ();

            retval = octave_value (charNDArray (m_matrix.index (i, j, resize_ok)),
                                   type);
          }
          break;

        default:
          {
            Array<octave::idx_vector> idx_vec (dim_vector (len, 1));

            for (k = 0; k < len; k++)
              idx_vec(k) = idx(k).index_vector ();

            retval = octave_value (charNDArray (m_matrix.index (idx_vec, resize_ok)), type);
          }
          break;
        }
    }
  catch (octave::index_exception& ie)
    {
      // Rethrow to allow more info to be reported later.
      ie.set_pos_if_unset (len, k+1);
      throw;
    }

  return retval;
}

octave_value
octave_char_matrix_str::resize (const dim_vector& dv, bool fill) const
{
  charNDArray retval (m_matrix);
  if (fill)
    retval.resize (dv, 0);
  else
    retval.resize (dv);
  return octave_value (retval, is_sq_string () ? '\'' : '"');
}

#define CHAR_MATRIX_CONV(TNAME, FCN)                                    \
                                                                        \
  if (! force_string_conv)                                              \
    err_invalid_conversion ("string", TNAME);                           \
                                                                        \
  warn_implicit_conversion ("Octave:str-to-num", "string", TNAME);      \
                                                                        \
  return octave_char_matrix::FCN ()

double
octave_char_matrix_str::double_value (bool force_string_conv) const
{
  CHAR_MATRIX_CONV ("real scalar", double_value);
}

Complex
octave_char_matrix_str::complex_value (bool force_string_conv) const
{
  CHAR_MATRIX_CONV ("complex scalar", complex_value);
}

Matrix
octave_char_matrix_str::matrix_value (bool force_string_conv) const
{
  CHAR_MATRIX_CONV ("real matrix", matrix_value);
}

ComplexMatrix
octave_char_matrix_str::complex_matrix_value (bool force_string_conv) const
{
  CHAR_MATRIX_CONV ("complex matrix", complex_matrix_value);
}

NDArray
octave_char_matrix_str::array_value (bool force_string_conv) const
{
  CHAR_MATRIX_CONV ("real N-D array", array_value);
}

ComplexNDArray
octave_char_matrix_str::complex_array_value (bool force_string_conv) const
{
  CHAR_MATRIX_CONV ("complex N-D array", complex_array_value);
}

string_vector
octave_char_matrix_str::string_vector_value (bool) const
{
  string_vector retval;

  if (m_matrix.ndims () != 2)
    error ("invalid conversion of charNDArray to string_vector");

  charMatrix chm (m_matrix);

  octave_idx_type n = chm.rows ();

  retval.resize (n);

  for (octave_idx_type i = 0; i < n; i++)
    retval[i] = chm.row_as_string (i);

  return retval;
}

std::string
octave_char_matrix_str::string_value (bool) const
{
  if (m_matrix.ndims () != 2)
    error ("invalid conversion of charNDArray to string");

  charMatrix chm (m_matrix);

  if (chm.rows () > 1)
    warning_with_id ("Octave:charmat-truncated",
                     "multi-row character matrix converted to a string, only the first row is used");

  // FIXME: Is this correct?
  return chm.row_as_string (0);
}

/*
%!test <*49536>
%! warning ("on", "Octave:charmat-truncated", "local");
%! s = char ("this", "is", "a", "char", "matrix");
%! fail ("sprintf (s)", ...
%!       "warning",     ...
%!       "multi-row character matrix converted to a string");
*/

Array<std::string>
octave_char_matrix_str::cellstr_value (void) const
{
  Array<std::string> retval;

  if (m_matrix.ndims () != 2)
    error ("cellstr: cannot convert multidimensional arrays");

  const charMatrix chm (m_matrix);
  octave_idx_type nr = chm.rows ();
  retval.clear (nr, 1);
  for (octave_idx_type i = 0; i < nr; i++)
    retval.xelem (i) = chm.row_as_string (i);

  return retval;
}

void
octave_char_matrix_str::print_raw (std::ostream& os,
                                   bool pr_as_read_syntax) const
{
  octave_print_internal (os, m_matrix, pr_as_read_syntax,
                         current_print_indent_level (), true);
}

void
octave_char_matrix_str::short_disp (std::ostream& os) const
{
  if (m_matrix.ndims () == 2 && numel () > 0)
    {
      charMatrix chm (m_matrix);
      std::string tmp = chm.row_as_string (0);

      // FIXME: should this be configurable?
      std::size_t max_len = 100;

      os << (tmp.length () > max_len ? tmp.substr (0, 100) : tmp);
    }
}

std::string
octave_char_matrix_str::edit_display (const float_display_format&,
                                      octave_idx_type i,
                                      octave_idx_type) const
{
  if (i == 0)
    {
      if (rows () == 1)
        {
          std::string retval = string_value ();

          if (! is_sq_string ())
            retval = octave::undo_string_escapes (retval);

          return retval;
        }
      else if (is_zero_by_zero ())
        return "";
    }

  std::string tname = type_name ();
  dim_vector dv = m_matrix.dims ();
  std::string dimstr = dv.str ();
  return "[" + dimstr + " " + tname + "]";
}

bool
octave_char_matrix_str::save_ascii (std::ostream& os)
{
  dim_vector dv = dims ();
  if (dv.ndims () > 2)
    {
      charNDArray tmp = char_array_value ();
      os << "# ndims: " << dv.ndims () << "\n";
      for (int i=0; i < dv.ndims (); i++)
        os << ' ' << dv(i);
      os << "\n";
      os.write (tmp.fortran_vec (), dv.numel ());
      os << "\n";
    }
  else
    {
      // Keep this case, rather than use generic code above for
      // backward compatibility.  Makes load_ascii much more complex!!
      charMatrix chm = char_matrix_value ();
      octave_idx_type elements = chm.rows ();
      os << "# elements: " << elements << "\n";
      for (octave_idx_type i = 0; i < elements; i++)
        {
          unsigned len = chm.cols ();
          os << "# length: " << len << "\n";
          std::string tstr = chm.row_as_string (i);
          const char *tmp = tstr.data ();
          if (tstr.length () > len)
            panic_impossible ();
          os.write (tmp, len);
          os << "\n";
        }
    }

  return true;
}

bool
octave_char_matrix_str::load_ascii (std::istream& is)
{
  string_vector keywords(3);

  keywords[0] = "ndims";
  keywords[1] = "elements";
  keywords[2] = "length";

  std::string kw;
  int val = 0;

  if (! extract_keyword (is, keywords, kw, val, true))
    error ("load: failed to extract number of rows and columns");

  if (kw == "ndims")
    {
      int mdims = val;

      if (mdims < 0)
        error ("load: failed to extract matrix size");

      dim_vector dv;
      dv.resize (mdims);

      for (int i = 0; i < mdims; i++)
        is >> dv(i);

      if (! is)
        error ("load: failed to read dimensions");

      charNDArray tmp(dv);

      if (tmp.isempty ())
        m_matrix = tmp;
      else
        {
          char *ftmp = tmp.fortran_vec ();

          octave::skip_preceeding_newline (is);

          if (! is.read (ftmp, dv.numel ()) || ! is)
            error ("load: failed to load string constant");

          m_matrix = tmp;
        }
    }
  else if (kw == "elements")
    {
      int elements = val;

      if (elements < 0)
        error ("load: failed to extract number of string elements");

      // FIXME: need to be able to get max length before doing anything.

      charMatrix chm (elements, 0);
      int max_len = 0;
      for (int i = 0; i < elements; i++)
        {
          int len;
          if (! extract_keyword (is, "length", len) || len < 0)
            error ("load: failed to extract string length for element %d",
                   i+1);

          // Use this instead of a C-style character
          // buffer so that we can properly handle
          // embedded NUL characters.
          charMatrix tmp (1, len);
          char *ptmp = tmp.fortran_vec ();

          if (len > 0 && ! is.read (ptmp, len))
            error ("load: failed to load string constant");

          if (len > max_len)
            {
              max_len = len;
              chm.resize (elements, max_len, 0);
            }

          chm.insert (tmp, i, 0);
        }

      m_matrix = chm;
    }
  else if (kw == "length")
    {
      int len = val;

      if (len >= 0)
        {
          // This is cruft for backward compatibility,
          // but relatively harmless.

          // Use this instead of a C-style character buffer so
          // that we can properly handle embedded NUL characters.
          charMatrix tmp (1, len);
          char *ptmp = tmp.fortran_vec ();

          if (len > 0 && ! is.read (ptmp, len))
            error ("load: failed to load string constant");

          if (! is)
            error ("load: failed to load string constant");

          m_matrix = tmp;
        }
    }
  else
    panic_impossible ();

  return true;
}

bool
octave_char_matrix_str::save_binary (std::ostream& os,
                                     bool /* save_as_floats */)
{
  dim_vector dv = dims ();
  if (dv.ndims () < 1)
    return false;

  // Use negative value for ndims to differentiate with old format!!
  int32_t tmp = - dv.ndims ();
  os.write (reinterpret_cast<char *> (&tmp), 4);
  for (int i=0; i < dv.ndims (); i++)
    {
      tmp = dv(i);
      os.write (reinterpret_cast<char *> (&tmp), 4);
    }

  charNDArray m = char_array_value ();
  os.write (m.fortran_vec (), dv.numel ());
  return true;
}

bool
octave_char_matrix_str::load_binary (std::istream& is, bool swap,
                                     octave::mach_info::float_format /* fmt */)
{
  int32_t elements;
  if (! is.read (reinterpret_cast<char *> (&elements), 4))
    return false;
  if (swap)
    swap_bytes<4> (&elements);

  if (elements < 0)
    {
      int32_t mdims = - elements;
      int32_t di;
      dim_vector dv;
      dv.resize (mdims);

      for (int i = 0; i < mdims; i++)
        {
          if (! is.read (reinterpret_cast<char *> (&di), 4))
            return false;
          if (swap)
            swap_bytes<4> (&di);
          dv(i) = di;
        }

      // Convert an array with a single dimension to be a row vector.
      // Octave should never write files like this, other software
      // might.

      if (mdims == 1)
        {
          mdims = 2;
          dv.resize (mdims);
          dv(1) = dv(0);
          dv(0) = 1;
        }

      charNDArray m(dv);
      char *tmp = m.fortran_vec ();
      is.read (tmp, dv.numel ());

      if (! is)
        return false;

      m_matrix = m;
    }
  else
    {
      charMatrix chm (elements, 0);
      int max_len = 0;
      for (int i = 0; i < elements; i++)
        {
          int32_t len;
          if (! is.read (reinterpret_cast<char *> (&len), 4))
            return false;
          if (swap)
            swap_bytes<4> (&len);
          charMatrix btmp (1, len);
          char *pbtmp = btmp.fortran_vec ();
          if (! is.read (pbtmp, len))
            return false;
          if (len > max_len)
            {
              max_len = len;
              chm.resize (elements, max_len, 0);
            }
          chm.insert (btmp, i, 0);
        }
      m_matrix = chm;
    }
  return true;
}

bool
octave_char_matrix_str::save_hdf5 (octave_hdf5_id loc_id, const char *name,
                                   bool /* save_as_floats */)
{
  bool retval = false;

#if defined (HAVE_HDF5)

  dim_vector dv = dims ();
  int empty = save_hdf5_empty (loc_id, name, dv);
  if (empty)
    return (empty > 0);

  int rank = dv.ndims ();
  hid_t space_hid, data_hid;
  space_hid = data_hid = -1;
  charNDArray m = char_array_value ();

  OCTAVE_LOCAL_BUFFER (hsize_t, hdims, rank);

  // Octave uses column-major, while HDF5 uses row-major ordering
  for (int i = 0; i < rank; i++)
    hdims[i] = dv(rank-i-1);

  space_hid = H5Screate_simple (rank, hdims, nullptr);
  if (space_hid < 0)
    return false;
#if defined (HAVE_HDF5_18)
  data_hid = H5Dcreate (loc_id, name, H5T_NATIVE_CHAR, space_hid,
                        octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                        octave_H5P_DEFAULT);
#else
  data_hid = H5Dcreate (loc_id, name, H5T_NATIVE_CHAR, space_hid,
                        octave_H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (char, s, dv.numel ());

  for (int i = 0; i < dv.numel (); ++i)
    s[i] = m(i);

  retval = H5Dwrite (data_hid, H5T_NATIVE_CHAR, octave_H5S_ALL, octave_H5S_ALL,
                     octave_H5P_DEFAULT, s) >= 0;

  H5Dclose (data_hid);
  H5Sclose (space_hid);

#else
  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);

  warn_save ("hdf5");
#endif

  return retval;
}

bool
octave_char_matrix_str::load_hdf5 (octave_hdf5_id loc_id, const char *name)
{
  bool retval = false;

#if defined (HAVE_HDF5)

  dim_vector dv;
  int empty = load_hdf5_empty (loc_id, name, dv);
  if (empty > 0)
    m_matrix.resize (dv);
  if (empty)
    return (empty > 0);

#if defined (HAVE_HDF5_18)
  hid_t data_hid = H5Dopen (loc_id, name, octave_H5P_DEFAULT);
#else
  hid_t data_hid = H5Dopen (loc_id, name);
#endif
  hid_t space_hid = H5Dget_space (data_hid);
  hsize_t rank = H5Sget_simple_extent_ndims (space_hid);
  hid_t type_hid = H5Dget_type (data_hid);
  hid_t type_class_hid = H5Tget_class (type_hid);

  if (type_class_hid == H5T_INTEGER)
    {
      if (rank < 1)
        {
          H5Tclose (type_hid);
          H5Sclose (space_hid);
          H5Dclose (data_hid);
          return false;
        }

      OCTAVE_LOCAL_BUFFER (hsize_t, hdims, rank);
      OCTAVE_LOCAL_BUFFER (hsize_t, maxdims, rank);

      H5Sget_simple_extent_dims (space_hid, hdims, maxdims);

      // Octave uses column-major, while HDF5 uses row-major ordering
      if (rank == 1)
        {
          dv.resize (2);
          dv(0) = 1;
          dv(1) = hdims[0];
        }
      else
        {
          dv.resize (rank);
          for (hsize_t i = 0, j = rank - 1; i < rank; i++, j--)
            dv(j) = hdims[i];
        }

      charNDArray m (dv);
      char *str = m.fortran_vec ();
      if (H5Dread (data_hid, H5T_NATIVE_CHAR, octave_H5S_ALL, octave_H5S_ALL,
                   octave_H5P_DEFAULT, str) >= 0)
        {
          retval = true;
          m_matrix = m;
        }

      H5Tclose (type_hid);
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      return true;
    }
  else
    {
      // This is cruft for backward compatibility and easy data importation
      if (rank == 0)
        {
          // a single string:
          int slen = H5Tget_size (type_hid);
          if (slen < 0)
            {
              H5Tclose (type_hid);
              H5Sclose (space_hid);
              H5Dclose (data_hid);
              return false;
            }
          else
            {
              OCTAVE_LOCAL_BUFFER (char, s, slen+1);
              // create datatype for (null-terminated) string to read into:
              hid_t st_id = H5Tcopy (H5T_C_S1);
              H5Tset_size (st_id, slen+1);
              if (H5Dread (data_hid, st_id, octave_H5S_ALL,
                           octave_H5S_ALL, octave_H5P_DEFAULT, s) < 0)
                {
                  H5Tclose (st_id);
                  H5Tclose (type_hid);
                  H5Sclose (space_hid);
                  H5Dclose (data_hid);
                  return false;
                }

              m_matrix = charMatrix (s);

              H5Tclose (st_id);
              H5Tclose (type_hid);
              H5Sclose (space_hid);
              H5Dclose (data_hid);
              return true;
            }
        }
      else if (rank == 1)
        {
          // string vector
          hsize_t elements, maxdim;
          H5Sget_simple_extent_dims (space_hid, &elements, &maxdim);
          int slen = H5Tget_size (type_hid);
          if (slen < 0)
            {
              H5Tclose (type_hid);
              H5Sclose (space_hid);
              H5Dclose (data_hid);
              return false;
            }
          else
            {
              // hdf5 string arrays store strings of all the
              // same physical length (I think), which is
              // slightly wasteful, but oh well.

              OCTAVE_LOCAL_BUFFER (char, s, elements * (slen+1));

              // create datatype for (null-terminated) string
              // to read into:
              hid_t st_id = H5Tcopy (H5T_C_S1);
              H5Tset_size (st_id, slen+1);

              if (H5Dread (data_hid, st_id, octave_H5S_ALL,
                           octave_H5S_ALL, octave_H5P_DEFAULT, s) < 0)
                {
                  H5Tclose (st_id);
                  H5Tclose (type_hid);
                  H5Sclose (space_hid);
                  H5Dclose (data_hid);
                  return false;
                }

              charMatrix chm (elements, slen, ' ');
              for (hsize_t i = 0; i < elements; ++i)
                {
                  chm.insert (s + i*(slen+1), i, 0);
                }

              m_matrix = chm;

              H5Tclose (st_id);
              H5Tclose (type_hid);
              H5Sclose (space_hid);
              H5Dclose (data_hid);
              return true;
            }
        }
      else
        {
          H5Tclose (type_hid);
          H5Sclose (space_hid);
          H5Dclose (data_hid);
          return false;
        }
    }

#else
  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);

  warn_load ("hdf5");
#endif

  return retval;
}
