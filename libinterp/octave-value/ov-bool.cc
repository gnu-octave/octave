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

#include "mx-base.h"

#include "errwarn.h"
#include "mxarray.h"
#include "oct-hdf5.h"
#include "ovl.h"
#include "ops.h"
#include "ov-bool.h"
#include "ov-bool-mat.h"
#include "ov-base.h"
#include "ov-base-scalar.h"
#include "ov-base-scalar.cc"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "pr-output.h"

#include "ls-oct-text.h"
#include "ls-hdf5.h"

// Prevent implicit instantiations on some systems (Windows, others?)
// that can lead to duplicate definitions of static data members.

extern template class octave_base_scalar<double>;

template class octave_base_scalar<bool>;

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_bool, "bool", "logical");

static octave_base_value *
default_numeric_conversion_function (const octave_base_value& a)
{
  const octave_bool& v = dynamic_cast<const octave_bool&> (a);

  return new octave_scalar (v.bool_value ());
}

octave_base_value::type_conv_info
octave_bool::numeric_conversion_function (void) const
{
  return octave_base_value::type_conv_info (default_numeric_conversion_function,
         octave_scalar::static_type_id ());

}

octave_value
octave_bool::do_index_op (const octave_value_list& idx, bool resize_ok)
{
  // FIXME: this doesn't solve the problem of
  //
  //   a = 1; a([1,1], [1,1], [1,1])
  //
  // and similar constructions.  Hmm...

  // FIXME: using this constructor avoids narrowing the
  // 1x1 matrix back to a scalar value.  Need a better solution
  // to this problem.

  octave_value tmp (new octave_bool_matrix (bool_matrix_value ()));

  return tmp.index_op (idx, resize_ok);
}

octave_value
octave_bool::as_double (void) const
{
  return static_cast<double> (scalar);
}

octave_value
octave_bool::as_single (void) const
{
  return static_cast<float> (scalar);
}

octave_value
octave_bool::as_int8 (void) const
{
  return octave_int8 (scalar);
}

octave_value
octave_bool::as_int16 (void) const
{
  return octave_int16 (scalar);
}

octave_value
octave_bool::as_int32 (void) const
{
  return octave_int32 (scalar);
}

octave_value
octave_bool::as_int64 (void) const
{
  return octave_int64 (scalar);
}

octave_value
octave_bool::as_uint8 (void) const
{
  return octave_uint8 (scalar);
}

octave_value
octave_bool::as_uint16 (void) const
{
  return octave_uint16 (scalar);
}

octave_value
octave_bool::as_uint32 (void) const
{
  return octave_uint32 (scalar);
}

octave_value
octave_bool::as_uint64 (void) const
{
  return octave_uint64 (scalar);
}

octave_value
octave_bool::resize (const dim_vector& dv, bool fill) const
{
  if (fill)
    {
      boolNDArray retval (dv, false);
      if (dv.numel ())
        retval(0) = scalar;
      return retval;
    }
  else
    {
      boolNDArray retval (dv);
      if (dv.numel ())
        retval(0) = scalar;
      return retval;
    }
}

octave_value
octave_bool::convert_to_str_internal (bool, bool, char type) const
{
  char s[2];
  s[0] = static_cast<char> (scalar);
  s[1] = '\0';

  return octave_value (s, type);
}

bool
octave_bool::save_ascii (std::ostream& os)
{
  double d = double_value ();

  octave::write_value<double> (os, d);
  os << "\n";

  return true;
}

bool
octave_bool::load_ascii (std::istream& is)
{
  scalar = (octave::read_value<double> (is) != 0.0);

  if (! is)
    error ("load: failed to load scalar constant");

  return true;
}

bool
octave_bool::save_binary (std::ostream& os, bool /* save_as_floats */)
{
  char tmp = (scalar ? 1 : 0);
  os.write (reinterpret_cast<char *> (&tmp), 1);

  return true;
}

bool
octave_bool::load_binary (std::istream& is, bool /* swap */,
                          octave::mach_info::float_format /* fmt */)
{
  char tmp;
  if (! is.read (reinterpret_cast<char *> (&tmp), 1))
    return false;
  scalar = (tmp ? 1 : 0);
  return true;
}

bool
octave_bool::save_hdf5 (octave_hdf5_id loc_id, const char *name,
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
octave_bool::load_hdf5 (octave_hdf5_id loc_id, const char *name)
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

  scalar = (dtmp != 0.0);

  H5Dclose (data_hid);

#else
  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);

  warn_load ("hdf5");
#endif

  return true;
}

mxArray *
octave_bool::as_mxArray (bool interleaved) const
{
  mxArray *retval = new mxArray (interleaved, mxLOGICAL_CLASS, 1, 1, mxREAL);

  mxLogical *pd = static_cast<mxLogical *> (retval->get_data ());

  pd[0] = scalar;

  return retval;
}
