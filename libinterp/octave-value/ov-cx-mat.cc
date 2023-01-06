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
#include <ostream>
#include <vector>

#include "dNDArray.h"
#include "fNDArray.h"

#include "data-conv.h"
#include "lo-ieee.h"
#include "lo-specfun.h"
#include "lo-mappers.h"
#include "mx-base.h"
#include "mach-info.h"
#include "oct-locbuf.h"

#include "errwarn.h"
#include "mxarray.h"
#include "ovl.h"
#include "oct-hdf5.h"
#include "oct-stream.h"
#include "ops.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-base-mat.cc"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "pr-output.h"

#include "byte-swap.h"
#include "ls-oct-text.h"
#include "ls-hdf5.h"
#include "ls-utils.h"


template class octave_base_matrix<ComplexNDArray>;

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_complex_matrix,
                                     "complex matrix", "double");

static octave_base_value *
default_numeric_demotion_function (const octave_base_value& a)
{
  const octave_complex_matrix& v = dynamic_cast<const octave_complex_matrix&> (a);

  return new octave_float_complex_matrix (v.float_complex_array_value ());
}

octave_base_value::type_conv_info
octave_complex_matrix::numeric_demotion_function (void) const
{
  return octave_base_value::type_conv_info
         (default_numeric_demotion_function,
          octave_float_complex_matrix::static_type_id ());
}

octave_base_value *
octave_complex_matrix::try_narrowing_conversion (void)
{
  octave_base_value *retval = nullptr;

  if (m_matrix.numel () == 1)
    {
      Complex c = m_matrix (0);

      if (c.imag () == 0.0)
        retval = new octave_scalar (c.real ());
      else
        retval = new octave_complex (c);
    }
  else if (m_matrix.all_elements_are_real ())
    retval = new octave_matrix (::real (m_matrix));

  return retval;
}

double
octave_complex_matrix::double_value (bool force_conversion) const
{
  if (! force_conversion)
    warn_implicit_conversion ("Octave:imag-to-real",
                              "complex matrix", "real scalar");

  if (rows () == 0 || columns () == 0)
    err_invalid_conversion ("complex matrix", "real scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            "complex matrix", "real scalar");

  return std::real (m_matrix(0, 0));
}

float
octave_complex_matrix::float_value (bool force_conversion) const
{
  if (! force_conversion)
    warn_implicit_conversion ("Octave:imag-to-real",
                              "complex matrix", "real scalar");

  if (rows () == 0 || columns () == 0)
    err_invalid_conversion ("complex matrix", "real scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            "complex matrix", "real scalar");

  return std::real (m_matrix(0, 0));
}

NDArray
octave_complex_matrix::array_value (bool force_conversion) const
{
  NDArray retval;

  if (! force_conversion)
    warn_implicit_conversion ("Octave:imag-to-real",
                              "complex matrix", "real matrix");

  retval = ::real (m_matrix);

  return retval;
}

Matrix
octave_complex_matrix::matrix_value (bool force_conversion) const
{
  Matrix retval;

  if (! force_conversion)
    warn_implicit_conversion ("Octave:imag-to-real",
                              "complex matrix", "real matrix");

  retval = ::real (ComplexMatrix (m_matrix));

  return retval;
}

FloatMatrix
octave_complex_matrix::float_matrix_value (bool force_conversion) const
{
  FloatMatrix retval;

  if (! force_conversion)
    warn_implicit_conversion ("Octave:imag-to-real",
                              "complex matrix", "real matrix");

  retval = ::real (ComplexMatrix (m_matrix));

  return retval;
}

Complex
octave_complex_matrix::complex_value (bool) const
{
  if (rows () == 0 || columns () == 0)
    err_invalid_conversion ("complex matrix", "complex scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            "complex matrix", "complex scalar");

  return m_matrix(0, 0);
}

FloatComplex
octave_complex_matrix::float_complex_value (bool) const
{
  float tmp = lo_ieee_float_nan_value ();

  FloatComplex retval (tmp, tmp);

  if (rows () == 0 || columns () == 0)
    err_invalid_conversion ("complex matrix", "complex scalar");

  warn_implicit_conversion ("Octave:array-to-scalar",
                            "complex matrix", "complex scalar");

  retval = m_matrix(0, 0);

  return retval;
}

ComplexMatrix
octave_complex_matrix::complex_matrix_value (bool) const
{
  return ComplexMatrix (m_matrix);
}

FloatComplexMatrix
octave_complex_matrix::float_complex_matrix_value (bool) const
{
  return FloatComplexMatrix (ComplexMatrix (m_matrix));
}

boolNDArray
octave_complex_matrix::bool_array_value (bool warn) const
{
  if (m_matrix.any_element_is_nan ())
    octave::err_nan_to_logical_conversion ();
  if (warn && (! m_matrix.all_elements_are_real ()
               || real (m_matrix).any_element_not_one_or_zero ()))
    warn_logical_conversion ();

  return mx_el_ne (m_matrix, Complex (0.0));
}

charNDArray
octave_complex_matrix::char_array_value (bool frc_str_conv) const
{
  charNDArray retval;

  if (! frc_str_conv)
    warn_implicit_conversion ("Octave:num-to-str",
                              "complex matrix", "string");
  else
    {
      retval = charNDArray (dims ());
      octave_idx_type nel = numel ();

      for (octave_idx_type i = 0; i < nel; i++)
        retval.elem (i) = static_cast<char> (std::real (m_matrix.elem (i)));
    }

  return retval;
}

FloatComplexNDArray
octave_complex_matrix::float_complex_array_value (bool) const
{
  return FloatComplexNDArray (m_matrix);
}

SparseMatrix
octave_complex_matrix::sparse_matrix_value (bool force_conversion) const
{
  SparseMatrix retval;

  if (! force_conversion)
    warn_implicit_conversion ("Octave:imag-to-real",
                              "complex matrix", "real matrix");

  retval = SparseMatrix (::real (ComplexMatrix (m_matrix)));

  return retval;
}

SparseComplexMatrix
octave_complex_matrix::sparse_complex_matrix_value (bool) const
{
  return SparseComplexMatrix (ComplexMatrix (m_matrix));
}

octave_value
octave_complex_matrix::as_double (void) const
{
  return m_matrix;
}

octave_value
octave_complex_matrix::as_single (void) const
{
  return FloatComplexNDArray (m_matrix);
}

octave_value
octave_complex_matrix::diag (octave_idx_type k) const
{
  octave_value retval;
  if (k == 0 && m_matrix.ndims () == 2
      && (m_matrix.rows () == 1 || m_matrix.columns () == 1))
    retval = ComplexDiagMatrix (DiagArray2<Complex> (m_matrix));
  else
    retval = octave_base_matrix<ComplexNDArray>::diag (k);

  return retval;
}

octave_value
octave_complex_matrix::diag (octave_idx_type m, octave_idx_type n) const
{
  if (m_matrix.ndims () != 2
      || (m_matrix.rows () != 1 && m_matrix.columns () != 1))
    error ("diag: expecting vector argument");

  ComplexMatrix mat (m_matrix);

  return mat.diag (m, n);
}

bool
octave_complex_matrix::save_ascii (std::ostream& os)
{
  dim_vector dv = dims ();
  if (dv.ndims () > 2)
    {
      ComplexNDArray tmp = complex_array_value ();

      os << "# ndims: " << dv.ndims () << "\n";

      for (int i = 0; i < dv.ndims (); i++)
        os << ' ' << dv(i);

      os << "\n" << tmp;
    }
  else
    {
      // Keep this case, rather than use generic code above for backward
      // compatibility.  Makes load_ascii much more complex!!
      os << "# rows: " << rows () << "\n"
         << "# columns: " << columns () << "\n";

      os << complex_matrix_value ();
    }

  return true;
}

bool
octave_complex_matrix::load_ascii (std::istream& is)
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

      ComplexNDArray tmp(dv);

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
          ComplexMatrix tmp (nr, nc);
          is >> tmp;
          if (! is)
            error ("load: failed to load matrix constant");

          m_matrix = tmp;
        }
      else if (nr == 0 || nc == 0)
        m_matrix = ComplexMatrix (nr, nc);
      else
        panic_impossible ();
    }
  else
    panic_impossible ();

  return true;
}

bool
octave_complex_matrix::save_binary (std::ostream& os, bool save_as_floats)
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

  ComplexNDArray m = complex_array_value ();
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
  else if (dv.numel () > 4096) // FIXME: make this configurable.
    {
      double max_val, min_val;
      if (m.all_integers (max_val, min_val))
        st = octave::get_save_type (max_val, min_val);
    }

  const Complex *mtmp = m.data ();
  write_doubles (os, reinterpret_cast<const double *> (mtmp), st,
                 2 * dv.numel ());

  return true;
}

bool
octave_complex_matrix::load_binary (std::istream& is, bool swap,
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

      ComplexNDArray m(dv);
      Complex *im = m.fortran_vec ();
      read_doubles (is, reinterpret_cast<double *> (im),
                    static_cast<save_type> (tmp), 2 * dv.numel (), swap, fmt);

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
      ComplexMatrix m (nr, nc);
      Complex *im = m.fortran_vec ();
      octave_idx_type len = static_cast<octave_idx_type> (nr) * nc;
      read_doubles (is, reinterpret_cast<double *> (im),
                    static_cast<save_type> (tmp), 2*len, swap, fmt);

      if (! is)
        return false;

      m_matrix = m;
    }
  return true;
}

bool
octave_complex_matrix::save_hdf5 (octave_hdf5_id loc_id, const char *name,
                                  bool save_as_floats)
{
#if defined (HAVE_HDF5)

  dim_vector dv = dims ();
  int empty = save_hdf5_empty (loc_id, name, dv);
  if (empty)
    return (empty > 0);

  int rank = dv.ndims ();
  hid_t space_hid, data_hid, type_hid;
  space_hid = data_hid = type_hid = -1;
  bool retval = true;
  ComplexNDArray m = complex_array_value ();

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

  type_hid = hdf5_make_complex_type (save_type_hid);
  if (type_hid < 0)
    {
      H5Sclose (space_hid);
      return false;
    }
#if defined (HAVE_HDF5_18)
  data_hid = H5Dcreate (loc_id, name, type_hid, space_hid,
                        octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                        octave_H5P_DEFAULT);
#else
  data_hid = H5Dcreate (loc_id, name, type_hid, space_hid, octave_H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      return false;
    }

  hid_t complex_type_hid = hdf5_make_complex_type (H5T_NATIVE_DOUBLE);
  if (complex_type_hid < 0) retval = false;

  if (retval)
    {
      Complex *mtmp = m.fortran_vec ();
      if (H5Dwrite (data_hid, complex_type_hid, octave_H5S_ALL, octave_H5S_ALL,
                    octave_H5P_DEFAULT, mtmp)
          < 0)
        {
          H5Tclose (complex_type_hid);
          retval = false;
        }
    }

  H5Tclose (complex_type_hid);
  H5Dclose (data_hid);
  H5Tclose (type_hid);
  H5Sclose (space_hid);

  return retval;

#else
  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);
  octave_unused_parameter (save_as_floats);

  warn_save ("hdf5");

  return false;
#endif
}

bool
octave_complex_matrix::load_hdf5 (octave_hdf5_id loc_id, const char *name)
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
  hid_t type_hid = H5Dget_type (data_hid);

  hid_t complex_type = hdf5_make_complex_type (H5T_NATIVE_DOUBLE);

  if (! hdf5_types_compatible (type_hid, complex_type))
    {
      H5Tclose (complex_type);
      H5Dclose (data_hid);
      return false;
    }

  hid_t space_id = H5Dget_space (data_hid);

  hsize_t rank = H5Sget_simple_extent_ndims (space_id);

  if (rank < 1)
    {
      H5Tclose (complex_type);
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

  ComplexNDArray m (dv);
  Complex *reim = m.fortran_vec ();
  if (H5Dread (data_hid, complex_type, octave_H5S_ALL, octave_H5S_ALL,
               octave_H5P_DEFAULT, reim)
      >= 0)
    {
      retval = true;
      m_matrix = m;
    }

  H5Tclose (complex_type);
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
octave_complex_matrix::print_raw (std::ostream& os,
                                  bool pr_as_read_syntax) const
{
  octave_print_internal (os, m_matrix, pr_as_read_syntax,
                         current_print_indent_level ());
}

mxArray *
octave_complex_matrix::as_mxArray (bool interleaved) const
{
  mxArray *retval = new mxArray (interleaved, mxDOUBLE_CLASS, dims (),
                                 mxCOMPLEX);

  mwSize nel = numel ();

  const Complex *pdata = m_matrix.data ();

  if (interleaved)
    {
      mxComplexDouble *pd
        = static_cast<mxComplexDouble *> (retval->get_data ());

      for (mwIndex i = 0; i < nel; i++)
        {
          pd[i].real = pdata[i].real ();
          pd[i].imag = pdata[i].imag ();
        }
    }
  else
    {
      mxDouble *pr = static_cast<mxDouble *> (retval->get_data ());
      mxDouble *pi = static_cast<mxDouble *> (retval->get_imag_data ());

      for (mwIndex i = 0; i < nel; i++)
        {
          pr[i] = pdata[i].real ();
          pi[i] = pdata[i].imag ();
        }
    }

  return retval;
}

octave_value
octave_complex_matrix::map (unary_mapper_t umap) const
{
  switch (umap)
    {
    // Mappers handled specially.
    case umap_real:
      return ::real (m_matrix);
    case umap_imag:
      return ::imag (m_matrix);
    case umap_conj:
      return ::conj (m_matrix);

    // Special cases for Matlab compatibility.
    case umap_xtolower:
    case umap_xtoupper:
      return m_matrix;

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

      ARRAY_MAPPER (acos, Complex, octave::math::acos);
      ARRAY_MAPPER (acosh, Complex, octave::math::acosh);
      ARRAY_MAPPER (angle, double, std::arg);
      ARRAY_MAPPER (arg, double, std::arg);
      ARRAY_MAPPER (asin, Complex, octave::math::asin);
      ARRAY_MAPPER (asinh, Complex, octave::math::asinh);
      ARRAY_MAPPER (atan, Complex, octave::math::atan);
      ARRAY_MAPPER (atanh, Complex, octave::math::atanh);
      ARRAY_MAPPER (erf, Complex, octave::math::erf);
      ARRAY_MAPPER (erfc, Complex, octave::math::erfc);
      ARRAY_MAPPER (erfcx, Complex, octave::math::erfcx);
      ARRAY_MAPPER (erfi, Complex, octave::math::erfi);
      ARRAY_MAPPER (dawson, Complex, octave::math::dawson);
      ARRAY_MAPPER (ceil, Complex, octave::math::ceil);
      ARRAY_MAPPER (cos, Complex, std::cos);
      ARRAY_MAPPER (cosh, Complex, std::cosh);
      ARRAY_MAPPER (exp, Complex, std::exp);
      ARRAY_MAPPER (expm1, Complex, octave::math::expm1);
      ARRAY_MAPPER (fix, Complex, octave::math::fix);
      ARRAY_MAPPER (floor, Complex, octave::math::floor);
      ARRAY_MAPPER (log, Complex, std::log);
      ARRAY_MAPPER (log2, Complex, octave::math::log2);
      ARRAY_MAPPER (log10, Complex, std::log10);
      ARRAY_MAPPER (log1p, Complex, octave::math::log1p);
      ARRAY_MAPPER (round, Complex, octave::math::round);
      ARRAY_MAPPER (roundb, Complex, octave::math::roundb);
      ARRAY_MAPPER (signum, Complex, octave::math::signum);
      ARRAY_MAPPER (sin, Complex, std::sin);
      ARRAY_MAPPER (sinh, Complex, std::sinh);
      ARRAY_MAPPER (sqrt, Complex, std::sqrt);
      ARRAY_MAPPER (tan, Complex, std::tan);
      ARRAY_MAPPER (tanh, Complex, std::tanh);
      ARRAY_MAPPER (isna, bool, octave::math::isna);

    default:
      return octave_base_value::map (umap);
    }
}
