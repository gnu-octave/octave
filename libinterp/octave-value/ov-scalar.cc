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

#include "oct-inttypes-fwd.h"

#include "data-conv.h"
#include "mach-info.h"
#include "lo-specfun.h"
#include "lo-mappers.h"

#include "defun.h"
#include "errwarn.h"
#include "mxarray.h"
#include "ovl.h"
#include "oct-hdf5.h"
#include "oct-stream.h"
#include "ov-scalar.h"
#include "ov-float.h"
#include "ov-base.h"
#include "ov-base-scalar.h"
#include "ov-base-scalar.cc"
#include "ov-re-mat.h"
#include "ov-typeinfo.h"
#include "pr-output.h"
#include "xdiv.h"
#include "xpow.h"
#include "ops.h"

#include "ls-oct-text.h"
#include "ls-hdf5.h"

// Prevent implicit instantiations on some systems (Windows, others?)
// that can lead to duplicate definitions of static data members.

extern template class octave_base_scalar<float>;

template class octave_base_scalar<double>;

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_scalar, "scalar", "double");

static octave_base_value *
default_numeric_demotion_function (const octave_base_value& a)
{
  const octave_scalar& v = dynamic_cast<const octave_scalar&> (a);

  return new octave_float_scalar (v.float_value ());
}

octave_base_value::type_conv_info
octave_scalar::numeric_demotion_function (void) const
{
  return octave_base_value::type_conv_info
         (default_numeric_demotion_function,
          octave_float_scalar::static_type_id ());
}

octave_value
octave_scalar::do_index_op (const octave_value_list& idx, bool resize_ok)
{
  // FIXME: this doesn't solve the problem of
  //
  //   a = 1; a([1,1], [1,1], [1,1])
  //
  // and similar constructions.  Hmm...

  // FIXME: using this constructor avoids narrowing the
  // 1x1 matrix back to a scalar value.  Need a better solution
  // to this problem.

  octave_value tmp (new octave_matrix (matrix_value ()));

  return tmp.index_op (idx, resize_ok);
}

octave_value
octave_scalar::resize (const dim_vector& dv, bool fill) const
{
  if (fill)
    {
      NDArray retval (dv, 0);

      if (dv.numel ())
        retval(0) = scalar;

      return retval;
    }
  else
    {
      NDArray retval (dv);

      if (dv.numel ())
        retval(0) = scalar;

      return retval;
    }
}

octave_value
octave_scalar::as_double (void) const
{
  return scalar;
}

octave_value
octave_scalar::as_single (void) const
{
  return static_cast<float> (scalar);
}

octave_value
octave_scalar::as_int8 (void) const
{
  return octave_int8 (scalar);
}

octave_value
octave_scalar::as_int16 (void) const
{
  return octave_int16 (scalar);
}

octave_value
octave_scalar::as_int32 (void) const
{
  return octave_int32 (scalar);
}

octave_value
octave_scalar::as_int64 (void) const
{
  return octave_int64 (scalar);
}

octave_value
octave_scalar::as_uint8 (void) const
{
  return octave_uint8 (scalar);
}

octave_value
octave_scalar::as_uint16 (void) const
{
  return octave_uint16 (scalar);
}

octave_value
octave_scalar::as_uint32 (void) const
{
  return octave_uint32 (scalar);
}

octave_value
octave_scalar::as_uint64 (void) const
{
  return octave_uint64 (scalar);
}

octave_value
octave_scalar::diag (octave_idx_type m, octave_idx_type n) const
{
  return DiagMatrix (Array<double> (dim_vector (1, 1), scalar), m, n);
}

octave_value
octave_scalar::convert_to_str_internal (bool, bool, char type) const
{
  octave_value retval;

  if (octave::math::isnan (scalar))
    octave::err_nan_to_character_conversion ();

  int ival = octave::math::nint (scalar);

  if (ival < 0 || ival > std::numeric_limits<unsigned char>::max ())
    {
      // FIXME: is there something better we could do?

      ival = 0;

      ::warning ("range error for conversion to character value");
    }

  retval = octave_value (std::string (1, static_cast<char> (ival)), type);

  return retval;
}

bool
octave_scalar::save_ascii (std::ostream& os)
{
  double d = double_value ();

  octave::write_value<double> (os, d);

  os << "\n";

  return true;
}

bool
octave_scalar::load_ascii (std::istream& is)
{
  scalar = octave::read_value<double> (is);

  if (! is)
    error ("load: failed to load scalar constant");

  return true;
}

bool
octave_scalar::save_binary (std::ostream& os, bool /* save_as_floats */)
{
  char tmp = LS_DOUBLE;
  os.write (reinterpret_cast<char *> (&tmp), 1);
  double dtmp = double_value ();
  os.write (reinterpret_cast<char *> (&dtmp), 8);

  return true;
}

bool
octave_scalar::load_binary (std::istream& is, bool swap,
                            octave::mach_info::float_format fmt)
{
  char tmp;
  if (! is.read (reinterpret_cast<char *> (&tmp), 1))
    return false;

  double dtmp;
  read_doubles (is, &dtmp, static_cast<save_type> (tmp), 1, swap, fmt);

  if (! is)
    return false;

  scalar = dtmp;
  return true;
}

bool
octave_scalar::save_hdf5 (octave_hdf5_id loc_id, const char *name,
                          bool /* save_as_floats */)
{
  bool retval = false;

#if defined (HAVE_HDF5)

  hsize_t dimens[3] = {0};
  hid_t space_hid, data_hid;
  space_hid = data_hid = -1;

  space_hid = H5Screate_simple (0, dimens, nullptr);
  if (space_hid < 0) return false;

#if defined (HAVE_HDF5_18)
  data_hid = H5Dcreate (loc_id, name, H5T_NATIVE_DOUBLE, space_hid,
                        octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                        octave_H5P_DEFAULT);
#else
  data_hid = H5Dcreate (loc_id, name, H5T_NATIVE_DOUBLE, space_hid,
                        octave_H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      return false;
    }

  double tmp = double_value ();
  retval = H5Dwrite (data_hid, H5T_NATIVE_DOUBLE, octave_H5S_ALL,
                     octave_H5S_ALL, octave_H5P_DEFAULT, &tmp) >= 0;

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
octave_scalar::load_hdf5 (octave_hdf5_id loc_id, const char *name)
{
#if defined (HAVE_HDF5)

#if defined (HAVE_HDF5_18)
  hid_t data_hid = H5Dopen (loc_id, name, octave_H5P_DEFAULT);
#else
  hid_t data_hid = H5Dopen (loc_id, name);
#endif
  hid_t space_id = H5Dget_space (data_hid);

  hsize_t rank = H5Sget_simple_extent_ndims (space_id);

  if (rank != 0)
    {
      H5Dclose (data_hid);
      return false;
    }

  double dtmp;
  if (H5Dread (data_hid, H5T_NATIVE_DOUBLE, octave_H5S_ALL, octave_H5S_ALL,
               octave_H5P_DEFAULT, &dtmp) < 0)
    {
      H5Dclose (data_hid);
      return false;
    }

  scalar = dtmp;

  H5Dclose (data_hid);

  return true;

#else
  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);

  warn_load ("hdf5");

  return false;
#endif
}

mxArray *
octave_scalar::as_mxArray (bool interleaved) const
{
  mxArray *retval = new mxArray (interleaved, mxDOUBLE_CLASS, 1, 1, mxREAL);

  mxDouble *pd = static_cast<mxDouble *> (retval->get_data ());

  pd[0] = scalar;

  return retval;
}

octave_value
octave_scalar::map (unary_mapper_t umap) const
{
  switch (umap)
    {
    case umap_imag:
      return 0.0;

    case umap_real:
    case umap_conj:
      return scalar;

#define SCALAR_MAPPER(UMAP, FCN)              \
    case umap_ ## UMAP:                       \
      return octave_value (FCN (scalar))

      SCALAR_MAPPER (abs, ::fabs);
      SCALAR_MAPPER (acos, octave::math::rc_acos);
      SCALAR_MAPPER (acosh, octave::math::rc_acosh);
      SCALAR_MAPPER (angle, std::arg);
      SCALAR_MAPPER (arg, std::arg);
      SCALAR_MAPPER (asin, octave::math::rc_asin);
      SCALAR_MAPPER (asinh, octave::math::asinh);
      SCALAR_MAPPER (atan, ::atan);
      SCALAR_MAPPER (atanh, octave::math::rc_atanh);
      SCALAR_MAPPER (erf, octave::math::erf);
      SCALAR_MAPPER (erfinv, octave::math::erfinv);
      SCALAR_MAPPER (erfcinv, octave::math::erfcinv);
      SCALAR_MAPPER (erfc, octave::math::erfc);
      SCALAR_MAPPER (erfcx, octave::math::erfcx);
      SCALAR_MAPPER (erfi, octave::math::erfi);
      SCALAR_MAPPER (dawson, octave::math::dawson);
      SCALAR_MAPPER (gamma, octave::math::gamma);
      SCALAR_MAPPER (lgamma, octave::math::rc_lgamma);
      SCALAR_MAPPER (cbrt, octave::math::cbrt);
      SCALAR_MAPPER (ceil, ::ceil);
      SCALAR_MAPPER (cos, ::cos);
      SCALAR_MAPPER (cosh, ::cosh);
      SCALAR_MAPPER (exp, ::exp);
      SCALAR_MAPPER (expm1, octave::math::expm1);
      SCALAR_MAPPER (fix, octave::math::fix);
      SCALAR_MAPPER (floor, std::floor);
      SCALAR_MAPPER (log, octave::math::rc_log);
      SCALAR_MAPPER (log2, octave::math::rc_log2);
      SCALAR_MAPPER (log10, octave::math::rc_log10);
      SCALAR_MAPPER (log1p, octave::math::rc_log1p);
      SCALAR_MAPPER (round, octave::math::round);
      SCALAR_MAPPER (roundb, octave::math::roundb);
      SCALAR_MAPPER (signum, octave::math::signum);
      SCALAR_MAPPER (sin, ::sin);
      SCALAR_MAPPER (sinh, ::sinh);
      SCALAR_MAPPER (sqrt, octave::math::rc_sqrt);
      SCALAR_MAPPER (tan, ::tan);
      SCALAR_MAPPER (tanh, ::tanh);
      SCALAR_MAPPER (isfinite, octave::math::isfinite);
      SCALAR_MAPPER (isinf, octave::math::isinf);
      SCALAR_MAPPER (isna, octave::math::isna);
      SCALAR_MAPPER (isnan, octave::math::isnan);
      SCALAR_MAPPER (xsignbit, octave::math::signbit);

    // Special cases for Matlab compatibility.
    case umap_xtolower:
    case umap_xtoupper:
      return scalar;

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

bool
octave_scalar::fast_elem_insert_self (void *where, builtin_type_t btyp) const
{

  // Support inline real->complex conversion.
  if (btyp == btyp_double)
    {
      *(reinterpret_cast<double *>(where)) = scalar;
      return true;
    }
  else if (btyp == btyp_complex)
    {
      *(reinterpret_cast<Complex *>(where)) = scalar;
      return true;
    }
  else
    return false;
}
