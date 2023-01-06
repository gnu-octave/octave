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

#include <istream>
#include <ostream>
#include <sstream>

#include "lo-ieee.h"
#include "lo-specfun.h"
#include "lo-mappers.h"

#include "mxarray.h"
#include "ovl.h"
#include "oct-hdf5.h"
#include "oct-stream.h"
#include "ops.h"
#include "ov-complex.h"
#include "ov-flt-complex.h"
#include "ov-base.h"
#include "ov-base-scalar.h"
#include "ov-base-scalar.cc"
#include "ov-cx-mat.h"
#include "ov-scalar.h"
#include "errwarn.h"
#include "pr-output.h"
#include "ops.h"

#include "ls-oct-text.h"
#include "ls-hdf5.h"

// Prevent implicit instantiations on some systems (Windows, others?)
// that can lead to duplicate definitions of static data members.

extern template class octave_base_scalar<double>;
extern template class octave_base_scalar<FloatComplex>;

template class octave_base_scalar<Complex>;

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_complex,
                                     "complex scalar", "double");

OCTAVE_BEGIN_NAMESPACE(octave)

// Complain if a complex value is used as a subscript.

class complex_index_exception : public index_exception
{
public:

  complex_index_exception (const std::string& value)
    : index_exception (value)
  {
    // Virtual, but the one we want to call is defined in this class.
    update_message ();
  }

  ~complex_index_exception (void) = default;

  void update_message (void)
  {
    set_message (expression ()
                 + ": subscripts must be real (forgot to initialize i or j?)");
  }

  // ID of error to throw.
  const char * err_id (void) const
  {
    return "Octave:invalid-index";
  }
};

OCTAVE_END_NAMESPACE(octave)

static octave_base_value *
default_numeric_demotion_function (const octave_base_value& a)
{
  const octave_complex& v = dynamic_cast<const octave_complex&> (a);

  return new octave_float_complex (v.float_complex_value ());
}

octave_base_value::type_conv_info
octave_complex::numeric_demotion_function (void) const
{
  return
    octave_base_value::type_conv_info (default_numeric_demotion_function,
                                       octave_float_complex::static_type_id ());
}

octave_base_value *
octave_complex::try_narrowing_conversion (void)
{
  octave_base_value *retval = nullptr;

  double im = scalar.imag ();

  if (im == 0.0)
    retval = new octave_scalar (scalar.real ());

  return retval;
}

octave_value
octave_complex::do_index_op (const octave_value_list& idx, bool resize_ok)
{
  // FIXME: this doesn't solve the problem of
  //
  //   a = i; a([1,1], [1,1], [1,1])
  //
  // and similar constructions.  Hmm...

  // FIXME: using this constructor avoids narrowing the
  // 1x1 matrix back to a scalar value.  Need a better solution
  // to this problem.

  octave_value tmp (new octave_complex_matrix (complex_matrix_value ()));

  return tmp.index_op (idx, resize_ok);
}

// Can't make an index_vector from a complex number.  Throw an error.
octave::idx_vector
octave_complex::index_vector (bool) const
{
  std::ostringstream buf;
  buf << scalar.real () << std::showpos << scalar.imag () << 'i';
  octave::complex_index_exception cie (buf.str ());

  throw cie;
}

double
octave_complex::double_value (bool force_conversion) const
{
  if (! force_conversion)
    warn_implicit_conversion ("Octave:imag-to-real",
                              "complex scalar", "real scalar");

  return scalar.real ();
}

float
octave_complex::float_value (bool force_conversion) const
{
  if (! force_conversion)
    warn_implicit_conversion ("Octave:imag-to-real",
                              "complex scalar", "real scalar");

  return scalar.real ();
}

Matrix
octave_complex::matrix_value (bool force_conversion) const
{
  Matrix retval;

  if (! force_conversion)
    warn_implicit_conversion ("Octave:imag-to-real",
                              "complex scalar", "real matrix");

  retval = Matrix (1, 1, scalar.real ());

  return retval;
}

FloatMatrix
octave_complex::float_matrix_value (bool force_conversion) const
{
  FloatMatrix retval;

  if (! force_conversion)
    warn_implicit_conversion ("Octave:imag-to-real",
                              "complex scalar", "real matrix");

  retval = FloatMatrix (1, 1, scalar.real ());

  return retval;
}

NDArray
octave_complex::array_value (bool force_conversion) const
{
  NDArray retval;

  if (! force_conversion)
    warn_implicit_conversion ("Octave:imag-to-real",
                              "complex scalar", "real matrix");

  retval = NDArray (dim_vector (1, 1), scalar.real ());

  return retval;
}

FloatNDArray
octave_complex::float_array_value (bool force_conversion) const
{
  FloatNDArray retval;

  if (! force_conversion)
    warn_implicit_conversion ("Octave:imag-to-real",
                              "complex scalar", "real matrix");

  retval = FloatNDArray (dim_vector (1, 1), scalar.real ());

  return retval;
}

Complex
octave_complex::complex_value (bool) const
{
  return scalar;
}

FloatComplex
octave_complex::float_complex_value (bool) const
{
  return static_cast<FloatComplex> (scalar);
}

ComplexMatrix
octave_complex::complex_matrix_value (bool) const
{
  return ComplexMatrix (1, 1, scalar);
}

FloatComplexMatrix
octave_complex::float_complex_matrix_value (bool) const
{
  return FloatComplexMatrix (1, 1, static_cast<FloatComplex> (scalar));
}

ComplexNDArray
octave_complex::complex_array_value (bool /* force_conversion */) const
{
  return ComplexNDArray (dim_vector (1, 1), scalar);
}

FloatComplexNDArray
octave_complex::float_complex_array_value (bool /* force_conversion */) const
{
  return FloatComplexNDArray (dim_vector (1, 1),
                              static_cast<FloatComplex> (scalar));
}

octave_value
octave_complex::resize (const dim_vector& dv, bool fill) const
{
  if (fill)
    {
      ComplexNDArray retval (dv, Complex (0));

      if (dv.numel ())
        retval(0) = scalar;

      return retval;
    }
  else
    {
      ComplexNDArray retval (dv);

      if (dv.numel ())
        retval(0) = scalar;

      return retval;
    }
}

octave_value
octave_complex::as_double (void) const
{
  return scalar;
}

octave_value
octave_complex::as_single (void) const
{
  return FloatComplex (scalar);
}

octave_value
octave_complex::diag (octave_idx_type m, octave_idx_type n) const
{
  return ComplexDiagMatrix (Array<Complex> (dim_vector (1, 1), scalar), m, n);
}

bool
octave_complex::save_ascii (std::ostream& os)
{
  Complex c = complex_value ();

  octave::write_value<Complex> (os, c);

  os << "\n";

  return true;
}

bool
octave_complex::load_ascii (std::istream& is)
{
  scalar = octave::read_value<Complex> (is);

  if (! is)
    error ("load: failed to load complex scalar constant");

  return true;
}

bool
octave_complex::save_binary (std::ostream& os, bool /* save_as_floats */)
{
  char tmp = static_cast<char> (LS_DOUBLE);
  os.write (reinterpret_cast<char *> (&tmp), 1);
  Complex ctmp = complex_value ();
  os.write (reinterpret_cast<char *> (&ctmp), 16);

  return true;
}

bool
octave_complex::load_binary (std::istream& is, bool swap,
                             octave::mach_info::float_format fmt)
{
  char tmp;
  if (! is.read (reinterpret_cast<char *> (&tmp), 1))
    return false;

  Complex ctmp;
  read_doubles (is, reinterpret_cast<double *> (&ctmp),
                static_cast<save_type> (tmp), 2, swap, fmt);

  if (! is)
    return false;

  scalar = ctmp;
  return true;
}

bool
octave_complex::save_hdf5 (octave_hdf5_id loc_id, const char *name,
                           bool /* save_as_floats */)
{
  bool retval = false;

#if defined (HAVE_HDF5)

  hsize_t dimens[3] = {0};
  hid_t space_hid, type_hid, data_hid;
  space_hid = type_hid = data_hid = -1;

  space_hid = H5Screate_simple (0, dimens, nullptr);
  if (space_hid < 0)
    return false;

  type_hid = hdf5_make_complex_type (H5T_NATIVE_DOUBLE);
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

  Complex tmp = complex_value ();
  retval = H5Dwrite (data_hid, type_hid, octave_H5S_ALL, octave_H5S_ALL,
                     octave_H5P_DEFAULT, &tmp) >= 0;

  H5Dclose (data_hid);
  H5Tclose (type_hid);
  H5Sclose (space_hid);

#else
  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);

  warn_save ("hdf5");
#endif

  return retval;
}

bool
octave_complex::load_hdf5 (octave_hdf5_id loc_id, const char *name)
{
  bool retval = false;

#if defined (HAVE_HDF5)

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

  if (rank != 0)
    {
      H5Tclose (complex_type);
      H5Sclose (space_id);
      H5Dclose (data_hid);
      return false;
    }

  // complex scalar:
  Complex ctmp;
  if (H5Dread (data_hid, complex_type, octave_H5S_ALL, octave_H5S_ALL,
               octave_H5P_DEFAULT, &ctmp) >= 0)
    {
      retval = true;
      scalar = ctmp;
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

mxArray *
octave_complex::as_mxArray (bool interleaved) const
{
  mxArray *retval = new mxArray (interleaved, mxDOUBLE_CLASS, 1, 1, mxCOMPLEX);

  if (interleaved)
    {
      mxComplexDouble *pd
        = reinterpret_cast<mxComplexDouble *> (retval->get_complex_doubles ());

      pd[0].real = scalar.real ();
      pd[0].imag = scalar.imag ();
    }
  else
    {
      mxDouble *pr = static_cast<mxDouble *> (retval->get_data ());
      mxDouble *pi = static_cast<mxDouble *> (retval->get_imag_data ());

      pr[0] = scalar.real ();
      pi[0] = scalar.imag ();
    }

  return retval;
}

octave_value
octave_complex::map (unary_mapper_t umap) const
{
  switch (umap)
    {
#define SCALAR_MAPPER(UMAP, FCN)              \
    case umap_ ## UMAP:                       \
      return octave_value (FCN (scalar))

      SCALAR_MAPPER (abs, std::abs);
      SCALAR_MAPPER (acos, octave::math::acos);
      SCALAR_MAPPER (acosh, octave::math::acosh);
      SCALAR_MAPPER (angle, std::arg);
      SCALAR_MAPPER (arg, std::arg);
      SCALAR_MAPPER (asin, octave::math::asin);
      SCALAR_MAPPER (asinh, octave::math::asinh);
      SCALAR_MAPPER (atan, octave::math::atan);
      SCALAR_MAPPER (atanh, octave::math::atanh);
      SCALAR_MAPPER (erf, octave::math::erf);
      SCALAR_MAPPER (erfc, octave::math::erfc);
      SCALAR_MAPPER (erfcx, octave::math::erfcx);
      SCALAR_MAPPER (erfi, octave::math::erfi);
      SCALAR_MAPPER (dawson, octave::math::dawson);
      SCALAR_MAPPER (ceil, octave::math::ceil);
      SCALAR_MAPPER (conj, std::conj);
      SCALAR_MAPPER (cos, std::cos);
      SCALAR_MAPPER (cosh, std::cosh);
      SCALAR_MAPPER (exp, std::exp);
      SCALAR_MAPPER (expm1, octave::math::expm1);
      SCALAR_MAPPER (fix, octave::math::fix);
      SCALAR_MAPPER (floor, octave::math::floor);
      SCALAR_MAPPER (imag, std::imag);
      SCALAR_MAPPER (log, std::log);
      SCALAR_MAPPER (log2, octave::math::log2);
      SCALAR_MAPPER (log10, std::log10);
      SCALAR_MAPPER (log1p, octave::math::log1p);
      SCALAR_MAPPER (real, std::real);
      SCALAR_MAPPER (round, octave::math::round);
      SCALAR_MAPPER (roundb, octave::math::roundb);
      SCALAR_MAPPER (signum, octave::math::signum);
      SCALAR_MAPPER (sin, std::sin);
      SCALAR_MAPPER (sinh, std::sinh);
      SCALAR_MAPPER (sqrt, std::sqrt);
      SCALAR_MAPPER (tan, std::tan);
      SCALAR_MAPPER (tanh, std::tanh);
      SCALAR_MAPPER (isfinite, octave::math::isfinite);
      SCALAR_MAPPER (isinf, octave::math::isinf);
      SCALAR_MAPPER (isna, octave::math::isna);
      SCALAR_MAPPER (isnan, octave::math::isnan);

    // Special cases for Matlab compatibility.
    case umap_xtolower:
    case umap_xtoupper:
      return scalar;

    default:
      return octave_base_value::map (umap);
    }
}
