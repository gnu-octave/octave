////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2020-2023 The Octave Project Developers
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
#include "ov-magic-int.h"
#include "ov-base-scalar.h"
#include "ov-re-mat.h"
#include "ov-typeinfo.h"
#include "pr-output.h"
#include "xdiv.h"
#include "xpow.h"
#include "ops.h"

#include "ls-oct-text.h"
#include "ls-hdf5.h"

// NOTE: Although there is some additional overhead, for all but the
// simplest data type extraction operations, we convert to an
// octave_scalar object and forward the operation to avoid code
// duplication and ensure that operations on magic_int objects are
// identical to operations on octave_scalar objects.  We could also
// avoid code duplication by deriving octave_magic_int from
// octave_scalar, but then we would need to store both the double and
// octave_uint64 or octave_int64 values, doubling the storage
// requirement.

static octave_base_value *
default_numeric_conv_fcn (const octave_base_value& a)
{
  return new octave_scalar (a.double_value ());
}

template <typename T>
octave_value
octave_base_magic_int<T>::do_index_op (const octave_value_list& idx,
                                       bool resize_ok)
{
  octave_value tmp (double_value ());

  return tmp.index_op (idx, resize_ok);
}

template <typename T>
octave::idx_vector
octave_base_magic_int<T>::index_vector (bool require_integers) const
{
  octave_value tmp (double_value ());

  return tmp.index_vector (require_integers);
}

template <typename T>
octave_value
octave_base_magic_int<T>::resize (const dim_vector& dv, bool fill) const
{
  octave_value tmp (double_value ());

  return tmp.resize (dv, fill);
}

template <typename T>
octave_value
octave_base_magic_int<T>::as_double (void) const
{
  return static_cast<double> (scalar_ref ());
}

template <typename T>
octave_value
octave_base_magic_int<T>::as_single (void) const
{
  return static_cast<float> (scalar_ref ());
}

template <typename T>
octave_value
octave_base_magic_int<T>::as_int8 (void) const
{
  return octave_int8 (scalar_ref ());
}

template <typename T>
octave_value
octave_base_magic_int<T>::as_int16 (void) const
{
  return octave_int16 (scalar_ref ());
}

template <typename T>
octave_value
octave_base_magic_int<T>::as_int32 (void) const
{
  return octave_int32 (scalar_ref ());
}

template <typename T>
octave_value
octave_base_magic_int<T>::as_int64 (void) const
{
  return octave_int64 (scalar_ref ());
}

template <typename T>
octave_value
octave_base_magic_int<T>::as_uint8 (void) const
{
  return octave_uint8 (scalar_ref ());
}

template <typename T>
octave_value
octave_base_magic_int<T>::as_uint16 (void) const
{
  return octave_uint16 (scalar_ref ());
}

template <typename T>
octave_value
octave_base_magic_int<T>::as_uint32 (void) const
{
  return octave_uint32 (scalar_ref ());
}

template <typename T>
octave_value
octave_base_magic_int<T>::as_uint64 (void) const
{
  return octave_uint64 (scalar_ref ());
}

template <typename T>
octave_value
octave_base_magic_int<T>::diag (octave_idx_type m, octave_idx_type n) const
{
  octave_value tmp (double_value ());

  return tmp.diag (m, n);
}

template <typename T>
octave_value
octave_base_magic_int<T>::convert_to_str_internal (bool, bool, char type) const
{
  octave_value retval;

  int ival;

  if (scalar_ref ().value () > std::numeric_limits<unsigned char>::max ())
    {
      // FIXME: is there something better we could do?

      ival = 0;

      ::warning ("range error for conversion to character value");
    }
  else
    ival = scalar_ref ().value ();

  retval = octave_value (std::string (1, static_cast<char> (ival)), type);

  return retval;
}


template <typename T>
bool
octave_base_magic_int<T>::save_ascii (std::ostream& os)
{
  octave_value tmp (double_value ());

  return tmp.save_ascii (os);
}

template <typename T>
OCTAVE_NORETURN bool
octave_base_magic_int<T>::load_ascii (std::istream&)
{
  error ("octave_base_magic_int<T>::load_ascii: internal error");
}

template <typename T>
bool
octave_base_magic_int<T>::save_binary (std::ostream& os, bool save_as_floats)
{
  octave_value tmp (double_value ());

  return tmp.save_binary (os, save_as_floats);
}

template <typename T>
OCTAVE_NORETURN bool
octave_base_magic_int<T>::load_binary (std::istream&, bool,
                                       octave::mach_info::float_format)
{
  error ("octave_base_magic_int<T>::load_binary: internal error");
}

template <typename T>
bool
octave_base_magic_int<T>::save_hdf5 (octave_hdf5_id loc_id, const char *name,
                                     bool save_as_floats)
{
  bool retval = false;

#if defined (HAVE_HDF5)

  octave_value tmp (double_value ());

  return tmp.save_hdf5 (loc_id, name, save_as_floats);

#else

  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);
  octave_unused_parameter (save_as_floats);

  octave_base_value::warn_save ("hdf5");

#endif

  return retval;
}

template <typename T>
bool
octave_base_magic_int<T>::load_hdf5 (octave_hdf5_id, const char *)
{
#if defined (HAVE_HDF5)

  error ("octave_base_magic_int<T>::load_binary: internal error");

  return false;

#else

  octave_base_value::warn_load ("hdf5");

  return false;

#endif
}

template <typename T>
mxArray *
octave_base_magic_int<T>::as_mxArray (bool interleaved) const
{
  octave_value tmp (double_value ());

  return tmp.as_mxArray (interleaved);
}

template <typename T>
octave_value
octave_base_magic_int<T>::map (octave_base_value::unary_mapper_t umap) const
{
  octave_value tmp (double_value ());

  return tmp.map (umap);
}

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_magic_uint, "magic_uint",
                                     "double");

octave_base_value::type_conv_info
octave_magic_uint::numeric_conversion_function (void) const
{
  return octave_base_value::type_conv_info (default_numeric_conv_fcn,
         octave_scalar::static_type_id ());
}

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_magic_int, "magic_int",
                                     "double");

octave_base_value::type_conv_info
octave_magic_int::numeric_conversion_function (void) const
{
  return octave_base_value::type_conv_info (default_numeric_conv_fcn,
         octave_scalar::static_type_id ());
}
