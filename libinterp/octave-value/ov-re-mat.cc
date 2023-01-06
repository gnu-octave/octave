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

#include <clocale>
#include <istream>
#include <limits>
#include <ostream>
#include <vector>

#include "dNDArray.h"
#include "fNDArray.h"
#include "int8NDArray.h"
#include "int16NDArray.h"
#include "int32NDArray.h"
#include "int64NDArray.h"
#include "uint8NDArray.h"
#include "uint16NDArray.h"
#include "uint32NDArray.h"
#include "uint64NDArray.h"

#include "data-conv.h"
#include "lo-ieee.h"
#include "lo-utils.h"
#include "lo-specfun.h"
#include "lo-mappers.h"
#include "mach-info.h"
#include "mx-base.h"
#include "quit.h"
#include "oct-locbuf.h"

#include "defun.h"
#include "errwarn.h"
#include "mxarray.h"
#include "ovl.h"
#include "oct-lvalue.h"
#include "oct-hdf5.h"
#include "oct-stream.h"
#include "ops.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-base-mat.cc"
#include "ov-scalar.h"
#include "ov-re-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-re-sparse.h"
#include "ov-re-diag.h"
#include "ov-cx-diag.h"
#include "ov-lazy-idx.h"
#include "ov-perm.h"
#include "pr-flt-fmt.h"
#include "pr-output.h"
#include "variables.h"

#include "byte-swap.h"
#include "ls-oct-text.h"
#include "ls-utils.h"
#include "ls-hdf5.h"


template class octave_base_matrix<NDArray>;

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_matrix, "matrix", "double");

static octave_base_value *
default_numeric_demotion_function (const octave_base_value& a)
{
  const octave_matrix& v = dynamic_cast<const octave_matrix&> (a);

  return new octave_float_matrix (v.float_array_value ());
}

octave_base_value::type_conv_info
octave_matrix::numeric_demotion_function (void) const
{
  return octave_base_value::type_conv_info
         (default_numeric_demotion_function,
          octave_float_matrix::static_type_id ());
}

octave_base_value *
octave_matrix::try_narrowing_conversion (void)
{
  octave_base_value *retval = nullptr;

  if (m_matrix.numel () == 1)
    retval = new octave_scalar (m_matrix (0));

  return retval;
}

double
octave_matrix::double_value (bool) const
{
  if (isempty ())
    err_invalid_conversion ("real matrix", "real scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            "real matrix", "real scalar");

  return m_matrix(0, 0);
}

float
octave_matrix::float_value (bool) const
{
  if (isempty ())
    err_invalid_conversion ("real matrix", "real scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            "real matrix", "real scalar");

  return m_matrix(0, 0);
}

// FIXME

Matrix
octave_matrix::matrix_value (bool) const
{
  return Matrix (m_matrix);
}

FloatMatrix
octave_matrix::float_matrix_value (bool) const
{
  return FloatMatrix (Matrix (m_matrix));
}

Complex
octave_matrix::complex_value (bool) const
{
  if (rows () == 0 || columns () == 0)
    err_invalid_conversion ("real matrix", "complex scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            "real matrix", "complex scalar");

  return Complex (m_matrix(0, 0), 0);
}

FloatComplex
octave_matrix::float_complex_value (bool) const
{
  float tmp = lo_ieee_float_nan_value ();

  FloatComplex retval (tmp, tmp);

  if (rows () == 0 || columns () == 0)
    err_invalid_conversion ("real matrix", "complex scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            "real matrix", "complex scalar");

  retval = m_matrix(0, 0);

  return retval;
}

// FIXME

ComplexMatrix
octave_matrix::complex_matrix_value (bool) const
{
  return ComplexMatrix (Matrix (m_matrix));
}

FloatComplexMatrix
octave_matrix::float_complex_matrix_value (bool) const
{
  return FloatComplexMatrix (Matrix (m_matrix));
}

ComplexNDArray
octave_matrix::complex_array_value (bool) const
{
  return ComplexNDArray (m_matrix);
}

FloatComplexNDArray
octave_matrix::float_complex_array_value (bool) const
{
  return FloatComplexNDArray (m_matrix);
}

boolNDArray
octave_matrix::bool_array_value (bool warn) const
{
  if (m_matrix.any_element_is_nan ())
    octave::err_nan_to_logical_conversion ();
  if (warn && m_matrix.any_element_not_one_or_zero ())
    warn_logical_conversion ();

  return boolNDArray (m_matrix);
}

charNDArray
octave_matrix::char_array_value (bool) const
{
  charNDArray retval (dims ());

  octave_idx_type nel = numel ();

  for (octave_idx_type i = 0; i < nel; i++)
    retval.elem (i) = static_cast<char> (m_matrix.elem (i));

  return retval;
}

SparseMatrix
octave_matrix::sparse_matrix_value (bool) const
{
  return SparseMatrix (Matrix (m_matrix));
}

SparseComplexMatrix
octave_matrix::sparse_complex_matrix_value (bool) const
{
  // FIXME: Need a SparseComplexMatrix (Matrix) constructor to make
  // this function more efficient.  Then this should become
  // return SparseComplexMatrix (matrix.matrix_value ());
  return SparseComplexMatrix (sparse_matrix_value ());
}

octave_value
octave_matrix::as_double (void) const
{
  return NDArray (m_matrix);
}

octave_value
octave_matrix::as_single (void) const
{
  return FloatNDArray (m_matrix);
}

octave_value
octave_matrix::as_int8 (void) const
{
  return int8NDArray (m_matrix);
}

octave_value
octave_matrix::as_int16 (void) const
{
  return int16NDArray (m_matrix);
}

octave_value
octave_matrix::as_int32 (void) const
{
  return int32NDArray (m_matrix);
}

octave_value
octave_matrix::as_int64 (void) const
{
  return int64NDArray (m_matrix);
}

octave_value
octave_matrix::as_uint8 (void) const
{
  return uint8NDArray (m_matrix);
}

octave_value
octave_matrix::as_uint16 (void) const
{
  return uint16NDArray (m_matrix);
}

octave_value
octave_matrix::as_uint32 (void) const
{
  return uint32NDArray (m_matrix);
}

octave_value
octave_matrix::as_uint64 (void) const
{
  return uint64NDArray (m_matrix);
}

octave_value
octave_matrix::diag (octave_idx_type k) const
{
  octave_value retval;
  if (k == 0 && m_matrix.ndims () == 2
      && (m_matrix.rows () == 1 || m_matrix.columns () == 1))
    retval = DiagMatrix (DiagArray2<double> (m_matrix));
  else
    retval = octave_base_matrix<NDArray>::diag (k);

  return retval;
}

octave_value
octave_matrix::diag (octave_idx_type m, octave_idx_type n) const
{
  if (m_matrix.ndims () != 2
      || (m_matrix.rows () != 1 && m_matrix.columns () != 1))
    error ("diag: expecting vector argument");

  Matrix mat (m_matrix);

  return mat.diag (m, n);
}

// We override these two functions to allow reshaping both
// the matrix and the index cache.
octave_value
octave_matrix::reshape (const dim_vector& new_dims) const
{
  if (m_idx_cache)
    {
      return new octave_matrix (m_matrix.reshape (new_dims),
                                octave::idx_vector (m_idx_cache->as_array ().reshape (new_dims),
                                    m_idx_cache->extent (0)));
    }
  else
    return octave_base_matrix<NDArray>::reshape (new_dims);
}

octave_value
octave_matrix::squeeze (void) const
{
  if (m_idx_cache)
    {
      return new octave_matrix (m_matrix.squeeze (),
                                octave::idx_vector (m_idx_cache->as_array ().squeeze (),
                                    m_idx_cache->extent (0)));
    }
  else
    return octave_base_matrix<NDArray>::squeeze ();
}

octave_value
octave_matrix::sort (octave_idx_type dim, sortmode mode) const
{
  if (m_idx_cache)
    {
      // This is a valid index matrix, so sort via integers because it's
      // generally more efficient.
      return octave_lazy_index (*m_idx_cache).sort (dim, mode);
    }
  else
    return octave_base_matrix<NDArray>::sort (dim, mode);
}

octave_value
octave_matrix::sort (Array<octave_idx_type>& sidx, octave_idx_type dim,
                     sortmode mode) const
{
  if (m_idx_cache)
    {
      // This is a valid index matrix, so sort via integers because it's
      // generally more efficient.
      return octave_lazy_index (*m_idx_cache).sort (sidx, dim, mode);
    }
  else
    return octave_base_matrix<NDArray>::sort (sidx, dim, mode);
}

sortmode
octave_matrix::issorted (sortmode mode) const
{
  if (m_idx_cache)
    {
      // This is a valid index matrix, so check via integers because it's
      // generally more efficient.
      return m_idx_cache->as_array ().issorted (mode);
    }
  else
    return octave_base_matrix<NDArray>::issorted (mode);
}
Array<octave_idx_type>
octave_matrix::sort_rows_idx (sortmode mode) const
{
  if (m_idx_cache)
    {
      // This is a valid index matrix, so sort via integers because it's
      // generally more efficient.
      return octave_lazy_index (*m_idx_cache).sort_rows_idx (mode);
    }
  else
    return octave_base_matrix<NDArray>::sort_rows_idx (mode);
}

sortmode
octave_matrix::is_sorted_rows (sortmode mode) const
{
  if (m_idx_cache)
    {
      // This is a valid index matrix, so check via integers because it's
      // generally more efficient.
      return m_idx_cache->as_array ().is_sorted_rows (mode);
    }
  else
    return octave_base_matrix<NDArray>::is_sorted_rows (mode);
}

octave_value
octave_matrix::convert_to_str_internal (bool, bool, char type) const
{
  octave_value retval;
  dim_vector dv = dims ();
  octave_idx_type nel = dv.numel ();

  charNDArray chm (dv);

  bool warned = false;

  for (octave_idx_type i = 0; i < nel; i++)
    {
      octave_quit ();

      double d = m_matrix(i);

      if (octave::math::isnan (d))
        octave::err_nan_to_character_conversion ();

      int ival = octave::math::nint (d);

      if (ival < 0 || ival > std::numeric_limits<unsigned char>::max ())
        {
          // FIXME: is there something better we could do?

          ival = 0;

          if (! warned)
            {
              ::warning ("range error for conversion to character value");
              warned = true;
            }
        }

      chm(i) = static_cast<char> (ival);
    }

  retval = octave_value (chm, type);

  return retval;
}

bool
octave_matrix::save_ascii (std::ostream& os)
{
  dim_vector dv = dims ();

  if (dv.ndims () > 2)
    {
      NDArray tmp = array_value ();

      os << "# ndims: " << dv.ndims () << "\n";

      for (int i=0; i < dv.ndims (); i++)
        os << ' ' << dv(i);

      os << "\n" << tmp;
    }
  else
    {
      // Keep this case, rather than use generic code above for backward
      // compatibility.  Makes load_ascii much more complex!!
      os << "# rows: " << rows () << "\n"
         << "# columns: " << columns () << "\n";

      os << matrix_value ();
    }

  return true;
}

bool
octave_matrix::load_ascii (std::istream& is)
{
  string_vector keywords(2);

  keywords[0] = "ndims";
  keywords[1] = "rows";

  std::string kw;
  octave_idx_type val = 0;

  if (! extract_keyword (is, keywords, kw, val, true))
    error ("load: failed to extract number of rows and columns");

  // Set "C" locale for the duration of this function to avoid the performance
  // panelty of frequently switching the locale when reading floating point
  // values from the stream.
  char *prev_locale = std::setlocale (LC_ALL, nullptr);
  std::string old_locale (prev_locale ? prev_locale : "");
  std::setlocale (LC_ALL, "C");
  octave::unwind_action act
  ([&old_locale] () { std::setlocale (LC_ALL, old_locale.c_str ()); });

  if (kw == "ndims")
    {
      int mdims = static_cast<int> (val);

      if (mdims < 0)
        error ("load: failed to extract number of dimensions");

      dim_vector dv;
      dv.resize (mdims);

      for (int i = 0; i < mdims; i++)
        is >> dv(i);

      if (! is)
        error ("load: failed to read dimensions");

      NDArray tmp(dv);

      is >> tmp;

      if (! is)
        error ("load: failed to load matrix constant");

      m_matrix = tmp;
    }
  else if (kw == "rows")
    {
      octave_idx_type nr = val;
      octave_idx_type nc = 0;

      if (nr < 0 || ! extract_keyword (is, "columns", nc) || nc < 0)
        error ("load: failed to extract number of rows and columns");

      if (nr > 0 && nc > 0)
        {
          Matrix tmp (nr, nc);
          is >> tmp;
          if (! is)
            error ("load: failed to load matrix constant");

          m_matrix = tmp;
        }
      else if (nr == 0 || nc == 0)
        m_matrix = Matrix (nr, nc);
      else
        panic_impossible ();
    }
  else
    panic_impossible ();

  return true;
}

bool
octave_matrix::save_binary (std::ostream& os, bool save_as_floats)
{

  dim_vector dv = dims ();
  if (dv.ndims () < 1)
    return false;

  // Use negative value for ndims to differentiate with old format!!
  int32_t tmp = - dv.ndims ();
  os.write (reinterpret_cast<char *> (&tmp), 4);
  for (int i = 0; i < dv.ndims (); i++)
    {
      tmp = dv(i);
      os.write (reinterpret_cast<char *> (&tmp), 4);
    }

  NDArray m = array_value ();
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
  else if (dv.numel () > 8192) // FIXME: make this configurable.
    {
      double max_val, min_val;
      if (m.all_integers (max_val, min_val))
        st = octave::get_save_type (max_val, min_val);
    }

  const double *mtmp = m.data ();
  write_doubles (os, mtmp, st, dv.numel ());

  return true;
}

bool
octave_matrix::load_binary (std::istream& is, bool swap,
                            octave::mach_info::float_format fmt)
{
  char tmp;
  int32_t mdims;
  if (! is.read (reinterpret_cast<char *> (&mdims), 4))
    return false;
  if (swap)
    swap_bytes<4> (&mdims);
  if (mdims < 0)
    {
      mdims = - mdims;
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

      if (! is.read (reinterpret_cast<char *> (&tmp), 1))
        return false;

      NDArray m(dv);
      double *re = m.fortran_vec ();
      read_doubles (is, re, static_cast<save_type> (tmp), dv.numel (),
                    swap, fmt);

      if (! is)
        return false;

      m_matrix = m;
    }
  else
    {
      int32_t nr, nc;
      nr = mdims;
      if (! is.read (reinterpret_cast<char *> (&nc), 4))
        return false;
      if (swap)
        swap_bytes<4> (&nc);
      if (! is.read (reinterpret_cast<char *> (&tmp), 1))
        return false;
      Matrix m (nr, nc);
      double *re = m.fortran_vec ();
      octave_idx_type len = static_cast<octave_idx_type> (nr) * nc;
      read_doubles (is, re, static_cast<save_type> (tmp), len, swap, fmt);

      if (! is)
        return false;

      m_matrix = m;
    }
  return true;
}

bool
octave_matrix::save_hdf5 (octave_hdf5_id loc_id, const char *name,
                          bool save_as_floats)
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
  NDArray m = array_value ();

  OCTAVE_LOCAL_BUFFER (hsize_t, hdims, rank);

  // Octave uses column-major, while HDF5 uses row-major ordering
  for (int i = 0; i < rank; i++)
    hdims[i] = dv(rank-i-1);

  space_hid = H5Screate_simple (rank, hdims, nullptr);

  if (space_hid < 0) return false;

  hid_t save_type_hid = H5T_NATIVE_DOUBLE;

  if (save_as_floats)
    {
      if (m.too_large_for_float ())
        {
          warning ("save: some values too large to save as floats --");
          warning ("save: saving as doubles instead");
        }
      else
        save_type_hid = H5T_NATIVE_FLOAT;
    }
#if defined (HAVE_HDF5_INT2FLOAT_CONVERSIONS)
  // hdf5 currently doesn't support float/integer conversions
  else
    {
      double max_val, min_val;

      if (m.all_integers (max_val, min_val))
        save_type_hid
          = save_type_to_hdf5 (octave::get_save_type (max_val, min_val));
    }
#endif

#if defined (HAVE_HDF5_18)
  data_hid = H5Dcreate (loc_id, name, save_type_hid, space_hid,
                        octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                        octave_H5P_DEFAULT);
#else
  data_hid = H5Dcreate (loc_id, name, save_type_hid, space_hid,
                        octave_H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      return false;
    }

  double *mtmp = m.fortran_vec ();
  retval = H5Dwrite (data_hid, H5T_NATIVE_DOUBLE, octave_H5S_ALL,
                     octave_H5S_ALL, octave_H5P_DEFAULT, mtmp) >= 0;

  H5Dclose (data_hid);
  H5Sclose (space_hid);

#else
  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);
  octave_unused_parameter (save_as_floats);

  warn_save ("hdf5");
#endif

  return retval;
}

bool
octave_matrix::load_hdf5 (octave_hdf5_id loc_id, const char *name)
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
  hid_t space_id = H5Dget_space (data_hid);

  hsize_t rank = H5Sget_simple_extent_ndims (space_id);

  if (rank < 1)
    {
      H5Sclose (space_id);
      H5Dclose (data_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (hsize_t, hdims, rank);
  OCTAVE_LOCAL_BUFFER (hsize_t, maxdims, rank);

  H5Sget_simple_extent_dims (space_id, hdims, maxdims);

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

  NDArray m (dv);
  double *re = m.fortran_vec ();
  if (H5Dread (data_hid, H5T_NATIVE_DOUBLE, octave_H5S_ALL, octave_H5S_ALL,
               octave_H5P_DEFAULT, re) >= 0)
    {
      retval = true;
      m_matrix = m;
    }

  H5Sclose (space_id);
  H5Dclose (data_hid);

#else
  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);

  warn_load ("hdf5");
#endif

  return retval;
}

void
octave_matrix::print_raw (std::ostream& os,
                          bool pr_as_read_syntax) const
{
  octave_print_internal (os, m_matrix, pr_as_read_syntax,
                         current_print_indent_level ());
}

mxArray *
octave_matrix::as_mxArray (bool interleaved) const
{
  mxArray *retval = new mxArray (interleaved, mxDOUBLE_CLASS, dims (), mxREAL);

  mxDouble *pd = static_cast<mxDouble *> (retval->get_data ());

  mwSize nel = numel ();

  const double *pdata = m_matrix.data ();

  for (mwIndex i = 0; i < nel; i++)
    pd[i] = pdata[i];

  return retval;
}

// This uses a smarter strategy for doing the complex->real mappers.  We
// allocate an array for a real result and keep filling it until a complex
// result is produced.
static octave_value
do_rc_map (const NDArray& a, Complex (&fcn) (double))
{
  octave_idx_type n = a.numel ();
  NDArray rr (a.dims ());

  for (octave_idx_type i = 0; i < n; i++)
    {
      octave_quit ();

      Complex tmp = fcn (a(i));
      if (tmp.imag () == 0.0)
        rr.xelem (i) = tmp.real ();
      else
        {
          ComplexNDArray rc (a.dims ());

          for (octave_idx_type j = 0; j < i; j++)
            rc.xelem (j) = rr.xelem (j);

          rc.xelem (i) = tmp;

          for (octave_idx_type j = i+1; j < n; j++)
            {
              octave_quit ();

              rc.xelem (j) = fcn (a(j));
            }

          return new octave_complex_matrix (rc);
        }
    }

  return rr;
}

octave_value
octave_matrix::map (unary_mapper_t umap) const
{
  switch (umap)
    {
    case umap_imag:
      return NDArray (m_matrix.dims (), 0.0);

    case umap_real:
    case umap_conj:
      return m_matrix;

      // Mappers handled specially.
#define ARRAY_METHOD_MAPPER(UMAP, FCN)        \
    case umap_ ## UMAP:                       \
      return octave_value (m_matrix.FCN ())

      ARRAY_METHOD_MAPPER (abs, abs);
      ARRAY_METHOD_MAPPER (isnan, isnan);
      ARRAY_METHOD_MAPPER (isinf, isinf);
      ARRAY_METHOD_MAPPER (isfinite, isfinite);

#define ARRAY_MAPPER(UMAP, TYPE, FCN)                 \
    case umap_ ## UMAP:                               \
      return octave_value (m_matrix.map<TYPE> (FCN))

#define RC_ARRAY_MAPPER(UMAP, TYPE, FCN)      \
    case umap_ ## UMAP:                       \
      return do_rc_map (m_matrix, FCN)

      RC_ARRAY_MAPPER (acos, Complex, octave::math::rc_acos);
      RC_ARRAY_MAPPER (acosh, Complex, octave::math::rc_acosh);
      ARRAY_MAPPER (angle, double, std::arg);
      ARRAY_MAPPER (arg, double, std::arg);
      RC_ARRAY_MAPPER (asin, Complex, octave::math::rc_asin);
      ARRAY_MAPPER (asinh, double, octave::math::asinh);
      ARRAY_MAPPER (atan, double, ::atan);
      RC_ARRAY_MAPPER (atanh, Complex, octave::math::rc_atanh);
      ARRAY_MAPPER (erf, double, octave::math::erf);
      ARRAY_MAPPER (erfinv, double, octave::math::erfinv);
      ARRAY_MAPPER (erfcinv, double, octave::math::erfcinv);
      ARRAY_MAPPER (erfc, double, octave::math::erfc);
      ARRAY_MAPPER (erfcx, double, octave::math::erfcx);
      ARRAY_MAPPER (erfi, double, octave::math::erfi);
      ARRAY_MAPPER (dawson, double, octave::math::dawson);
      ARRAY_MAPPER (gamma, double, octave::math::gamma);
      RC_ARRAY_MAPPER (lgamma, Complex, octave::math::rc_lgamma);
      ARRAY_MAPPER (cbrt, double, octave::math::cbrt);
      ARRAY_MAPPER (ceil, double, ::ceil);
      ARRAY_MAPPER (cos, double, ::cos);
      ARRAY_MAPPER (cosh, double, ::cosh);
      ARRAY_MAPPER (exp, double, ::exp);
      ARRAY_MAPPER (expm1, double, octave::math::expm1);
      ARRAY_MAPPER (fix, double, octave::math::fix);
      ARRAY_MAPPER (floor, double, ::floor);
      RC_ARRAY_MAPPER (log, Complex, octave::math::rc_log);
      RC_ARRAY_MAPPER (log2, Complex, octave::math::rc_log2);
      RC_ARRAY_MAPPER (log10, Complex, octave::math::rc_log10);
      RC_ARRAY_MAPPER (log1p, Complex, octave::math::rc_log1p);
      ARRAY_MAPPER (round, double, octave::math::round);
      ARRAY_MAPPER (roundb, double, octave::math::roundb);
      ARRAY_MAPPER (signum, double, octave::math::signum);
      ARRAY_MAPPER (sin, double, ::sin);
      ARRAY_MAPPER (sinh, double, ::sinh);
      RC_ARRAY_MAPPER (sqrt, Complex, octave::math::rc_sqrt);
      ARRAY_MAPPER (tan, double, ::tan);
      ARRAY_MAPPER (tanh, double, ::tanh);
      ARRAY_MAPPER (isna, bool, octave::math::isna);
      ARRAY_MAPPER (xsignbit, double, octave::math::signbit);

    // Special cases for Matlab compatibility.
    case umap_xtolower:
    case umap_xtoupper:
      return m_matrix;

    case umap_xisalnum:
    case umap_xisalpha:
    case umap_xisascii:
    case umap_xiscntrl:
    case umap_xisdigit:
    case umap_xisgraph:
    case umap_xislower:
    case umap_xisprint:
    case umap_xispunct:
    case umap_xisspace:
    case umap_xisupper:
    case umap_xisxdigit:
      {
        octave_value str_conv = convert_to_str (true, true);
        return str_conv.map (umap);
      }

    default:
      return octave_base_value::map (umap);
    }
}
