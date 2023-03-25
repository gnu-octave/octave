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

#if defined (HAVE_HDF5)

#include <cctype>

#include <iomanip>
#include <istream>
#include <limits>
#include <ostream>
#include <string>
#include <vector>

#include "byte-swap.h"
#include "data-conv.h"
#include "file-ops.h"
#include "glob-match.h"
#include "lo-mappers.h"
#include "mach-info.h"
#include "oct-env.h"
#include "oct-time.h"
#include "quit.h"
#include "str-vec.h"
#include "oct-locbuf.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "interpreter.h"
#include "interpreter-private.h"
#include "load-save.h"
#include "oct-hdf5.h"
#include "ovl.h"
#include "oct-map.h"
#include "ov-cell.h"
#include "pager.h"
#include "sysdep.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include "version.h"
#include "dMatrix.h"
#include "ov-lazy-idx.h"

#include "ls-utils.h"
#include "ls-hdf5.h"

#if defined (HAVE_HDF5)

static hid_t
check_hdf5_id_value (octave_hdf5_id id, const char *who)
{
  if (id > std::numeric_limits<hid_t>::max ())
    error ("%s: internal error: ID too large for hid_t", who);

  return static_cast<hid_t> (id);
}

#endif

hdf5_fstreambase::hdf5_fstreambase (const char *name, int mode, int /* prot */)
  : file_id (-1), current_item (-1)
{
#if defined (HAVE_HDF5)
  open_create (name, mode);

  current_item = 0;

#else
  err_disabled_feature ("hdf5_fstreambase", "HDF5");
#endif
}

void
hdf5_fstreambase::close (void)
{
#if defined (HAVE_HDF5)

  if (file_id >= 0)
    {
      if (H5Fclose (file_id) < 0)
        std::ios::setstate (std::ios::badbit);
      file_id = -1;
    }

#else
  // This shouldn't happen because construction of hdf5_fstreambase
  // objects is supposed to be impossible if HDF5 is not available.

  panic_impossible ();
#endif
}

void
hdf5_fstreambase::open (const char *name, int mode, int)
{
#if defined (HAVE_HDF5)

  clear ();

  open_create (name, mode);

  current_item = 0;

#else
  // This shouldn't happen because construction of hdf5_fstreambase
  // objects is supposed to be impossible if HDF5 is not available.

  panic_impossible ();
#endif
}

void
hdf5_fstreambase::open_create (const char *name, int mode)
{
#if defined (HAVE_HDF5)
  // Open the HDF5 file NAME.  If it does not exist, create the file.

#  if defined (HAVE_HDF5_UTF8)
  const char *fname = name;
#  else
  std::string fname_str (name);
  std::string ascii_fname_str = octave::sys::get_ASCII_filename (fname_str);
  const char *fname = ascii_fname_str.c_str ();
#  endif

  if (mode & std::ios::in)
    file_id = H5Fopen (fname, H5F_ACC_RDONLY, octave_H5P_DEFAULT);
  else if (mode & std::ios::out)
    {
      if (mode & std::ios::app && H5Fis_hdf5 (fname) > 0)
        file_id = H5Fopen (fname, H5F_ACC_RDWR, octave_H5P_DEFAULT);
      else
#  if defined (HAVE_HDF5_UTF8)
        file_id = H5Fcreate (fname, H5F_ACC_TRUNC, octave_H5P_DEFAULT,
                             octave_H5P_DEFAULT);
#  else
        {
          // Check whether file already exists
          std::string abs_ascii_fname
            = octave::sys::canonicalize_file_name (ascii_fname_str);
          if (! abs_ascii_fname.empty ())
            {
              // Use the existing file
              file_id = H5Fcreate (fname, H5F_ACC_TRUNC,
                                   octave_H5P_DEFAULT, octave_H5P_DEFAULT);
              if (file_id < 0)
                std::ios::setstate (std::ios::badbit);

              return;
            }

          // Check whether filename contains non-ASCII (UTF-8) characters.
          std::string::const_iterator first_non_ASCII
            = std::find_if (fname_str.begin (), fname_str.end (),
          [](char c) { return (c < 0 || c >= 128); });
          if (first_non_ASCII == fname_str.end ())
            {
              // No non-ASCII characters
              file_id = H5Fcreate (name, H5F_ACC_TRUNC, octave_H5P_DEFAULT,
                                   octave_H5P_DEFAULT);
              if (file_id < 0)
                std::ios::setstate (std::ios::badbit);

              return;
            }

          // Create file in temp folder
          std::string tmp_name = octave::sys::tempnam ("", "oct-");
          octave_hdf5_id hdf5_fid = H5Fcreate (tmp_name.c_str (), H5F_ACC_TRUNC,
                                               octave_H5P_DEFAULT,
                                               octave_H5P_DEFAULT);
          if (hdf5_fid < 0)
            {
              file_id = -1;
              std::ios::setstate (std::ios::badbit);
              return;
            }

          // Close file
          H5Fclose (hdf5_fid);

          // Move temporary file to final destination
          std::string msg;
          int res = octave::sys::rename (tmp_name, name, msg);
          if (res < 0)
            {
              std::ios::setstate (std::ios::badbit);
              file_id = -1;
              return;
            }

          // Open file at final location
          ascii_fname_str = octave::sys::get_ASCII_filename (fname_str);
          ascii_fname = ascii_fname_str.c_str ();
          file_id = H5Fopen (ascii_fname, H5F_ACC_RDWR, octave_H5P_DEFAULT);
        }
#  endif
    }
  if (file_id < 0)
    std::ios::setstate (std::ios::badbit);

  return;

#else
  // This shouldn't happen because construction of hdf5_fstreambase
  // objects is supposed to be impossible if HDF5 is not available.

  panic_impossible ();
#endif
}

static std::string
make_valid_identifier (const std::string& nm)
{
  std::string retval;

  std::size_t nm_len = nm.length ();

  if (nm_len > 0)
    {
      if (! isalpha (nm[0]))
        retval += '_';

      for (std::size_t i = 0; i < nm_len; i++)
        {
          char c = nm[i];
          retval += (isalnum (c) || c == '_') ? c : '_';
        }
    }

  return retval;
}

// Given two compound types t1 and t2, determine whether they
// are compatible for reading/writing.  This function only
// works for non-nested types composed of simple elements (ints, floats...),
// which is all we need it for

bool
hdf5_types_compatible (octave_hdf5_id t1, octave_hdf5_id t2)
{
#if defined (HAVE_HDF5)

  int n;
  if ((n = H5Tget_nmembers (t1)) != H5Tget_nmembers (t2))
    return false;

  for (int i = 0; i < n; ++i)
    {
      hid_t mt1 = H5Tget_member_type (t1, i);
      hid_t mt2 = H5Tget_member_type (t2, i);

      if (H5Tget_class (mt1) != H5Tget_class (mt2))
        return false;

      H5Tclose (mt2);
      H5Tclose (mt1);
    }

  return true;

#else
  err_disabled_feature ("hdf5_types_compatible", "HDF5");
#endif
}

// Return true if loc_id has the attribute named attr_name, and false
// otherwise.

bool
hdf5_check_attr (octave_hdf5_id loc_id, const char *attr_name)
{
#if defined (HAVE_HDF5)

  bool retval = false;

  // we have to pull some shenanigans here to make sure
  // HDF5 doesn't print out all sorts of error messages if we
  // call H5Aopen for a non-existing attribute

  H5E_auto_t err_fcn;
  void *err_fcn_data;

  // turn off error reporting temporarily, but save the error
  // reporting function:

#if defined (HAVE_HDF5_18)
  H5Eget_auto (octave_H5E_DEFAULT, &err_fcn, &err_fcn_data);
  H5Eset_auto (octave_H5E_DEFAULT, nullptr, nullptr);
#else
  H5Eget_auto (&err_fcn, &err_fcn_data);
  H5Eset_auto (nullptr, nullptr);
#endif

  hid_t attr_id = H5Aopen_name (loc_id, attr_name);

  if (attr_id >= 0)
    {
      // successful
      retval = true;
      H5Aclose (attr_id);
    }

  // restore error reporting:
#if defined (HAVE_HDF5_18)
  H5Eset_auto (octave_H5E_DEFAULT, err_fcn, err_fcn_data);
#else
  H5Eset_auto (err_fcn, err_fcn_data);
#endif
  return retval;

#else
  err_disabled_feature ("hdf5_check_attr", "HDF5");
#endif
}

bool
hdf5_get_scalar_attr (octave_hdf5_id loc_id, octave_hdf5_id type_id,
                      const char *attr_name, void *buf)
{
#if defined (HAVE_HDF5)

  bool retval = false;

  // we have to pull some shenanigans here to make sure
  // HDF5 doesn't print out all sorts of error messages if we
  // call H5Aopen for a non-existing attribute

  H5E_auto_t err_fcn;
  void *err_fcn_data;

  // turn off error reporting temporarily, but save the error
  // reporting function:

#if defined (HAVE_HDF5_18)
  H5Eget_auto (octave_H5E_DEFAULT, &err_fcn, &err_fcn_data);
  H5Eset_auto (octave_H5E_DEFAULT, nullptr, nullptr);
#else
  H5Eget_auto (&err_fcn, &err_fcn_data);
  H5Eset_auto (nullptr, nullptr);
#endif

  hid_t attr_id = H5Aopen_name (loc_id, attr_name);

  if (attr_id >= 0)
    {
      hid_t space_id = H5Aget_space (attr_id);

      hsize_t rank = H5Sget_simple_extent_ndims (space_id);

      if (rank == 0)
        retval = H5Aread (attr_id, type_id, buf) >= 0;
      H5Aclose (attr_id);
    }

  // restore error reporting:
#if defined (HAVE_HDF5_18)
  H5Eset_auto (octave_H5E_DEFAULT, err_fcn, err_fcn_data);
#else
  H5Eset_auto (err_fcn, err_fcn_data);
#endif
  return retval;

#else
  err_disabled_feature ("hdf5_get_scalar_attr", "HDF5");
#endif
}

// The following subroutines creates an HDF5 representations of the way
// we will store Octave complex types (pairs of floating-point numbers).
// NUM_TYPE is the HDF5 numeric type to use for storage (e.g.
// H5T_NATIVE_DOUBLE to save as 'double').  Note that any necessary
// conversions are handled automatically by HDF5.

octave_hdf5_id
hdf5_make_complex_type (octave_hdf5_id num_type)
{
#if defined (HAVE_HDF5)

  hid_t type_id = H5Tcreate (H5T_COMPOUND, sizeof (double) * 2);

  H5Tinsert (type_id, "real", 0 * sizeof (double), num_type);
  H5Tinsert (type_id, "imag", 1 * sizeof (double), num_type);

  return type_id;

#else
  err_disabled_feature ("hdf5_make_complex_type", "HDF5");
#endif
}

#if defined (HAVE_HDF5)

// The following subroutine creates an HDF5 representation of the way
// we will store Octave range types (triplets of floating-point numbers).
// NUM_TYPE is the HDF5 numeric type to use for storage
// (e.g., H5T_NATIVE_DOUBLE to save as 'double').
// Note that any necessary conversions are handled automatically by HDF5.

static hid_t
hdf5_make_range_type (hid_t num_type)
{
  hid_t type_id = H5Tcreate (H5T_COMPOUND, sizeof (double) * 3);

  H5Tinsert (type_id, "base", 0 * sizeof (double), num_type);
  H5Tinsert (type_id, "limit", 1 * sizeof (double), num_type);
  H5Tinsert (type_id, "increment", 2 * sizeof (double), num_type);

  return type_id;
}

static herr_t
load_inline_fcn (hid_t loc_id, const char *name, octave_value& retval)
{
#if defined (HAVE_HDF5)

  hid_t group_hid, data_hid, space_hid, type_hid, type_class_hid, st_id;
  hsize_t rank;
  int slen;

#if defined (HAVE_HDF5_18)
  group_hid = H5Gopen (loc_id, name, octave_H5P_DEFAULT);
#else
  group_hid = H5Gopen (loc_id, name);
#endif
  if (group_hid < 0) return -1;

#if defined (HAVE_HDF5_18)
  data_hid = H5Dopen (group_hid, "args", octave_H5P_DEFAULT);
#else
  data_hid = H5Dopen (group_hid, "args");
#endif
  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 2)
    {
      H5Dclose (data_hid);
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return -1;
    }

  OCTAVE_LOCAL_BUFFER (hsize_t, hdims, rank);
  OCTAVE_LOCAL_BUFFER (hsize_t, maxdims, rank);

  H5Sget_simple_extent_dims (space_hid, hdims, maxdims);

  octave_value_list args (hdims[1]+1);

  OCTAVE_LOCAL_BUFFER (char, s1, hdims[0] * hdims[1]);

  if (H5Dread (data_hid, H5T_NATIVE_UCHAR, octave_H5S_ALL, octave_H5S_ALL,
               octave_H5P_DEFAULT, s1) < 0)
    {
      H5Dclose (data_hid);
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return -1;
    }

  H5Dclose (data_hid);
  H5Sclose (space_hid);

  for (std::size_t i = 0; i < hdims[1]; i++)
    args(i+1) = std::string (s1 + i*hdims[0]);

#if defined (HAVE_HDF5_18)
  data_hid = H5Dopen (group_hid, "nm", octave_H5P_DEFAULT);
#else
  data_hid = H5Dopen (group_hid, "nm");
#endif

  if (data_hid < 0)
    {
      H5Gclose (group_hid);
      return -1;
    }

  type_hid = H5Dget_type (data_hid);
  type_class_hid = H5Tget_class (type_hid);

  if (type_class_hid != H5T_STRING)
    {
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return -1;
    }

  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return -1;
    }

  slen = H5Tget_size (type_hid);
  if (slen < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return -1;
    }

  OCTAVE_LOCAL_BUFFER (char, nm_tmp, slen);

  // create datatype for (null-terminated) string to read into:
  st_id = H5Tcopy (H5T_C_S1);
  H5Tset_size (st_id, slen);

  if (H5Dread (data_hid, st_id, octave_H5S_ALL, octave_H5S_ALL,
               octave_H5P_DEFAULT, nm_tmp) < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return -1;
    }
  H5Tclose (st_id);
  H5Dclose (data_hid);
  // NAME is obsolete and unused.
  // std::string name (nm_tmp);

#if defined (HAVE_HDF5_18)
  data_hid = H5Dopen (group_hid, "iftext", octave_H5P_DEFAULT);
#else
  data_hid = H5Dopen (group_hid, "iftext");
#endif

  if (data_hid < 0)
    {
      H5Gclose (group_hid);
      return -1;
    }

  type_hid = H5Dget_type (data_hid);
  type_class_hid = H5Tget_class (type_hid);

  if (type_class_hid != H5T_STRING)
    {
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return -1;
    }

  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return -1;
    }

  slen = H5Tget_size (type_hid);
  if (slen < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return -1;
    }

  OCTAVE_LOCAL_BUFFER (char, iftext_tmp, slen);

  // create datatype for (null-terminated) string to read into:
  st_id = H5Tcopy (H5T_C_S1);
  H5Tset_size (st_id, slen);

  if (H5Dread (data_hid, st_id, octave_H5S_ALL, octave_H5S_ALL,
               octave_H5P_DEFAULT, iftext_tmp) < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return -1;
    }
  H5Tclose (st_id);
  H5Dclose (data_hid);

  args(0) = std::string (iftext_tmp);

  octave::interpreter& interp = octave::__get_interpreter__ ();

  octave_value_list tmp = interp.feval ("inline", args, 1);

  if (tmp.length () > 0)
    {
      retval = tmp(0);
      return 1;
    }

#else
  octave_unused_parameter (loc_id);
  octave_unused_parameter (name);
  octave_unused_parameter (retval);

  warn_load ("hdf5");
#endif

  return -1;
}

// This function is designed to be passed to H5Giterate, which calls it
// on each data item in an HDF5 file.  For the item whose name is NAME in
// the group GROUP_ID, this function sets dv->tc to an Octave representation
// of that item.  (dv must be a pointer to hdf5_callback_data.)  (It also
// sets the other fields of dv).
//
// It returns 1 on success (in which case H5Giterate stops and returns),
// -1 on error, and 0 to tell H5Giterate to continue on to the next item
// (e.g., if NAME was a data type we don't recognize).
//
// This function must not throw an exception.

static herr_t
hdf5_read_next_data_internal (hid_t group_id, const char *name, void *dv)
{
  hdf5_callback_data *d = static_cast<hdf5_callback_data *> (dv);
  hid_t type_id = -1;
  hid_t type_class_id = -1;
  hid_t data_id = -1;
  hid_t subgroup_id = -1;
  hid_t space_id = -1;

  H5G_stat_t info;
  herr_t retval = 0;
  bool ident_valid = octave::valid_identifier (name);

  std::string vname = name;

  octave::type_info& type_info = octave::__get_type_info__ ();

  // Allow identifiers as all digits so we can load lists saved by
  // earlier versions of Octave.

  if (! ident_valid)
    {
      // fix the identifier, replacing invalid chars with underscores
      vname = make_valid_identifier (vname);

      // check again (in case vname was null, empty, or some such thing):
      ident_valid = octave::valid_identifier (vname);
    }

  H5Gget_objinfo (group_id, name, 1, &info);

  if (info.type == H5G_GROUP && ident_valid)
    {
#if defined (HAVE_HDF5_18)
      subgroup_id = H5Gopen (group_id, name, octave_H5P_DEFAULT);
#else
      subgroup_id = H5Gopen (group_id, name);
#endif

      if (subgroup_id < 0)
        {
          retval = subgroup_id;
          goto done;
        }

      if (hdf5_check_attr (subgroup_id, "OCTAVE_NEW_FORMAT"))
        {
#if defined (HAVE_HDF5_18)
          data_id = H5Dopen (subgroup_id, "type", octave_H5P_DEFAULT);
#else
          data_id = H5Dopen (subgroup_id, "type");
#endif

          if (data_id < 0)
            {
              retval = data_id;
              goto done;
            }

          type_id = H5Dget_type (data_id);

          type_class_id = H5Tget_class (type_id);

          if (type_class_id != H5T_STRING)
            goto done;

          space_id = H5Dget_space (data_id);
          hsize_t rank = H5Sget_simple_extent_ndims (space_id);

          if (rank != 0)
            goto done;

          int slen = H5Tget_size (type_id);
          if (slen < 0)
            goto done;

          OCTAVE_LOCAL_BUFFER (char, typ, slen);

          // create datatype for (null-terminated) string to read into:
          hid_t st_id = H5Tcopy (H5T_C_S1);
          H5Tset_size (st_id, slen);

          if (H5Dread (data_id, st_id, octave_H5S_ALL, octave_H5S_ALL,
                       octave_H5P_DEFAULT, typ) < 0)
            goto done;

          H5Tclose (st_id);
          H5Dclose (data_id);

          if (std::string (typ, slen-1) == "inline function")
            {
              retval = load_inline_fcn (subgroup_id, name, d->tc);
            }
          else
            {
              d->tc = type_info.lookup_type (std::string (typ, slen-1));

              try
                {
                  retval = (d->tc.load_hdf5 (subgroup_id, "value") ? 1 : -1);
                }
              catch (const octave::execution_exception& ee)
                {
                  retval = -1;
                }
            }

          // check for OCTAVE_GLOBAL attribute:
          d->global = hdf5_check_attr (subgroup_id, "OCTAVE_GLOBAL");

          H5Gclose (subgroup_id);
        }
      else
        {
          // It seems that this block only applies to an old list type
          // and that we shouldn't need to handle the old inline
          // function type here.

          // an HDF5 group is treated as an octave structure by
          // default (since that preserves name information), and an
          // octave list otherwise.

          if (hdf5_check_attr (subgroup_id, "OCTAVE_LIST"))
            d->tc = type_info.lookup_type ("list");
          else
            d->tc = type_info.lookup_type ("struct");

          // check for OCTAVE_GLOBAL attribute:
          d->global = hdf5_check_attr (subgroup_id, "OCTAVE_GLOBAL");

          H5Gclose (subgroup_id);

          try
            {
              retval = (d->tc.load_hdf5 (group_id, name) ? 1 : -1);
            }
          catch (const octave::execution_exception& ee)
            {
              retval = -1;
            }
        }

    }
  else if (info.type == H5G_DATASET && ident_valid)
    {
      // It seems that this block only applies to an old version of
      // Octave HDF5 files and that it is probably not important to
      // handle the old inline function type here.

      // For backwards compatibility.
#if defined (HAVE_HDF5_18)
      data_id = H5Dopen (group_id, name, octave_H5P_DEFAULT);
#else
      data_id = H5Dopen (group_id, name);
#endif

      if (data_id < 0)
        {
          retval = data_id;
          goto done;
        }

      type_id = H5Dget_type (data_id);

      type_class_id = H5Tget_class (type_id);

      if (type_class_id == H5T_FLOAT)
        {
          space_id = H5Dget_space (data_id);

          hsize_t rank = H5Sget_simple_extent_ndims (space_id);

          if (rank == 0)
            d->tc = type_info.lookup_type ("scalar");
          else
            d->tc = type_info.lookup_type ("matrix");

          H5Sclose (space_id);
        }
      else if (type_class_id == H5T_INTEGER)
        {
          // What integer type do we really have..
          std::string int_typ;
#if defined (HAVE_H5T_GET_NATIVE_TYPE)
          // FIXME: test this code and activated with an autoconf
          // test!! It is also incorrect for 64-bit indexing!!

          switch (H5Tget_native_type (type_id, H5T_DIR_ASCEND))
            {
            case H5T_NATIVE_CHAR:
              int_typ = "int8 ";
              break;

            case H5T_NATIVE_SHORT:
              int_typ = "int16 ";
              break;

            case H5T_NATIVE_INT:
            case H5T_NATIVE_LONG:
              int_typ = "int32 ";
              break;

            case H5T_NATIVE_LLONG:
              int_typ = "int64 ";
              break;

            case H5T_NATIVE_UCHAR:
              int_typ = "uint8 ";
              break;

            case H5T_NATIVE_USHORT:
              int_typ = "uint16 ";
              break;

            case H5T_NATIVE_UINT:
            case H5T_NATIVE_ULONG:
              int_typ = "uint32 ";
              break;

            case H5T_NATIVE_ULLONG:
              int_typ = "uint64 ";
              break;
            }
#else
          hid_t int_sign = H5Tget_sign (type_id);

          if (int_sign == H5T_SGN_ERROR)
            warning ("load: can't read '%s' (unknown datatype)", name);
          else
            {
              if (int_sign == H5T_SGN_NONE)
                int_typ.push_back ('u');
              int_typ.append ("int");

              int slen = H5Tget_size (type_id);
              if (slen < 0)
                warning ("load: can't read '%s' (unknown datatype)", name);
              else
                {
                  switch (slen)
                    {
                    case 1:
                      int_typ.append ("8 ");
                      break;

                    case 2:
                      int_typ.append ("16 ");
                      break;

                    case 4:
                      int_typ.append ("32 ");
                      break;

                    case 8:
                      int_typ.append ("64 ");
                      break;

                    default:
                      warning ("load: can't read '%s' (unknown datatype)",
                               name);
                      int_typ = "";
                      break;
                    }
                }
            }
#endif
          if (int_typ == "")
            warning ("load: can't read '%s' (unknown datatype)", name);
          else
            {
              // Matrix or scalar?
              space_id = H5Dget_space (data_id);

              hsize_t rank = H5Sget_simple_extent_ndims (space_id);

              if (rank == 0)
                int_typ.append ("scalar");
              else
                int_typ.append ("matrix");

              d->tc = type_info.lookup_type (int_typ);
              H5Sclose (space_id);
            }
        }
      else if (type_class_id == H5T_STRING)
        d->tc = type_info.lookup_type ("string");
      else if (type_class_id == H5T_COMPOUND)
        {
          hid_t complex_type = hdf5_make_complex_type (H5T_NATIVE_DOUBLE);
          hid_t range_type = hdf5_make_range_type (H5T_NATIVE_DOUBLE);

          if (hdf5_types_compatible (type_id, complex_type))
            {
              // read complex matrix or scalar variable
              space_id = H5Dget_space (data_id);
              hsize_t rank = H5Sget_simple_extent_ndims (space_id);

              if (rank == 0)
                d->tc = type_info.lookup_type ("complex scalar");
              else
                d->tc = type_info.lookup_type ("complex matrix");

              H5Sclose (space_id);
            }
          else if (hdf5_types_compatible (type_id, range_type))
            {
              // If it's not a complex, check if it's a range
              d->tc = octave_value_typeinfo::lookup_type ("range");
            }
          else // Otherwise, just ignore it with a warning.
            {
              warning ("load: can't read '%s' (unknown datatype)", name);
              retval = 0;  // unknown datatype; skip
              return retval;
            }

          H5Tclose (range_type);
          H5Tclose (complex_type);
        }
      else
        {
          warning ("load: can't read '%s' (unknown datatype)", name);
          retval = 0;  // unknown datatype; skip
          return retval;
        }

      // check for OCTAVE_GLOBAL attribute:
      d->global = hdf5_check_attr (data_id, "OCTAVE_GLOBAL");

      try
        {
          retval = (d->tc.load_hdf5 (group_id, name) ? 1 : -1);
        }
      catch (const octave::execution_exception& ee)
        {
          retval = -1;
        }

      H5Tclose (type_id);
      H5Dclose (data_id);
    }

  if (! ident_valid)
    {
      // should we attempt to handle invalid identifiers by converting
      // bad characters to '_', say?
      warning ("load: skipping invalid identifier '%s' in hdf5 file",
               name);
    }

done:
  if (retval < 0)
    {
      // Must be warning.  A call to error aborts and leaves H5Giterate in
      // a mangled state that causes segfault on exit (bug #56149).
      warning ("load: error while reading hdf5 item '%s'", name);
    }

  if (retval > 0)
    {
      // get documentation string, if any:
      int comment_length = H5Gget_comment (group_id, name, 0, nullptr);

      if (comment_length > 1)
        {
          OCTAVE_LOCAL_BUFFER (char, tdoc, comment_length);
          H5Gget_comment (group_id, name, comment_length, tdoc);
          d->doc = tdoc;
        }
      else if (vname != name)
        {
          // the name was changed; store the original name
          // as the documentation string:
          d->doc = name;
        }

      // copy name (actually, vname):
      d->name = vname;
    }

  return retval;
}

#endif

octave_hdf5_err
hdf5_read_next_data (octave_hdf5_id group_id, const char *name, void *dv)
{
#if defined (HAVE_HDF5)

  hid_t new_id = check_hdf5_id_value (group_id, "hdf5_read_next_data");

  return hdf5_read_next_data_internal (new_id, name, dv);

#else
  err_disabled_feature ("hdf5_read_next_data", "HDF5");
#endif
}

octave_hdf5_err
hdf5_h5g_iterate (octave_hdf5_id loc_id, const char *name, int *idx,
                  void *operator_data)
{
#if defined (HAVE_HDF5)

  hid_t new_id = check_hdf5_id_value (loc_id, "hdf5_h5g_iterate");

  return H5Giterate (new_id, name, idx, hdf5_read_next_data_internal,
                     operator_data);

#else
  err_disabled_feature ("hdf5_h5g_iterate", "HDF5");
#endif
}

// Read the next Octave variable from the stream IS, which must really be an
// hdf5_ifstream.  Return the variable value in tc, its docstring in doc, and
// whether it is global in global.  The return value is the name of the
// variable, or NULL if none were found or there was an error.
std::string
read_hdf5_data (std::istream& is, const std::string& /* filename */,
                bool& global, octave_value& tc, std::string& doc,
                const string_vector& argv, int argv_idx, int argc)
{
#if defined (HAVE_HDF5)

  octave::check_hdf5_types ();

  std::string retval;

  doc.clear ();

  hdf5_ifstream& hs = dynamic_cast<hdf5_ifstream&> (is);
  hdf5_callback_data d;

  herr_t H5Giterate_retval = -1;

  hsize_t num_obj = 0;
#if defined (HAVE_HDF5_18)
  hid_t group_id = H5Gopen (hs.file_id, "/", octave_H5P_DEFAULT);
#else
  hid_t group_id = H5Gopen (hs.file_id, "/");
#endif
  H5Gget_num_objs (group_id, &num_obj);
  H5Gclose (group_id);

  // For large datasets and out-of-core functionality,
  // check if only parts of the data is requested
  bool load_named_vars = argv_idx < argc;
  while (load_named_vars && hs.current_item < static_cast<int> (num_obj))
    {
      std::vector<char> var_name;
      bool found = false;
      std::size_t len = 0;

      len = H5Gget_objname_by_idx (hs.file_id, hs.current_item, nullptr, 0);
      var_name.resize (len+1);
      H5Gget_objname_by_idx (hs.file_id, hs.current_item, &var_name[0], len+1);

      for (int i = argv_idx; i < argc; i++)
        {
          glob_match pattern (argv[i]);
          if (pattern.match (std::string (&var_name[0])))
            {
              found = true;
              break;
            }
        }

      if (found)
        break;

      hs.current_item++;
    }

  if (hs.current_item < static_cast<int> (num_obj))
    H5Giterate_retval = H5Giterate (hs.file_id, "/", &hs.current_item,
                                    hdf5_read_next_data_internal, &d);

  if (H5Giterate_retval > 0)
    {
      global = d.global;
      tc = d.tc;
      doc = d.doc;
    }
  else
    {
      // An error occurred (H5Giterate_retval < 0),
      // or there are no more datasets (H5Giterate_retval == 0).
      // hdf5_read_next_data_internal has already printed a warning msg.
    }

  if (! d.name.empty ())
    retval = d.name;

  return retval;

#else
  err_disabled_feature ("read_hdf5_data", "HDF5");
#endif
}

// Add an attribute named attr_name to loc_id (a simple scalar
// attribute with value 1).  Return value is >= 0 on success.
octave_hdf5_err
hdf5_add_attr (octave_hdf5_id loc_id, const char *attr_name)
{
#if defined (HAVE_HDF5)

  herr_t retval = 0;

  hid_t as_id = H5Screate (H5S_SCALAR);

  if (as_id >= 0)
    {
#if defined (HAVE_HDF5_18)
      hid_t a_id = H5Acreate (loc_id, attr_name, H5T_NATIVE_UCHAR,
                              as_id, octave_H5P_DEFAULT, octave_H5P_DEFAULT);
#else
      hid_t a_id = H5Acreate (loc_id, attr_name,
                              H5T_NATIVE_UCHAR, as_id, octave_H5P_DEFAULT);
#endif
      if (a_id >= 0)
        {
          unsigned char attr_val = 1;

          retval = H5Awrite (a_id, H5T_NATIVE_UCHAR, &attr_val);

          H5Aclose (a_id);
        }
      else
        retval = a_id;

      H5Sclose (as_id);
    }
  else
    retval = as_id;

  return retval;

#else
  err_disabled_feature ("hdf5_add_attr", "HDF5");
#endif
}

octave_hdf5_err
hdf5_add_scalar_attr (octave_hdf5_id loc_id, octave_hdf5_id type_id,
                      const char *attr_name, void *buf)
{
#if defined (HAVE_HDF5)

  herr_t retval = 0;

  hid_t as_id = H5Screate (H5S_SCALAR);

  if (as_id >= 0)
    {
#if defined (HAVE_HDF5_18)
      hid_t a_id = H5Acreate (loc_id, attr_name, type_id,
                              as_id, octave_H5P_DEFAULT, octave_H5P_DEFAULT);
#else
      hid_t a_id = H5Acreate (loc_id, attr_name,
                              type_id, as_id, octave_H5P_DEFAULT);
#endif
      if (a_id >= 0)
        {
          retval = H5Awrite (a_id, type_id, buf);

          H5Aclose (a_id);
        }
      else
        retval = a_id;

      H5Sclose (as_id);
    }
  else
    retval = as_id;

  return retval;

#else
  err_disabled_feature ("hdf5_add_scalar_attr", "HDF5");
#endif
}

// Save an empty matrix, if needed.  Returns
//    > 0  Saved empty matrix
//    = 0  Not an empty matrix; did nothing
//    < 0  Error condition
int
save_hdf5_empty (octave_hdf5_id loc_id, const char *name, const dim_vector& d)
{
#if defined (HAVE_HDF5)

  hsize_t sz = d.length ();
  OCTAVE_LOCAL_BUFFER (octave_idx_type, dims, sz);
  bool empty = false;
  hid_t space_hid = -1;
  hid_t data_hid = -1;
  int retval;
  for (hsize_t i = 0; i < sz; i++)
    {
      dims[i] = d(i);
      if (dims[i] < 1)
        empty = true;
    }

  if (! empty)
    return 0;

  space_hid = H5Screate_simple (1, &sz, nullptr);
  if (space_hid < 0) return space_hid;
#if defined (HAVE_HDF5_18)
  data_hid = H5Dcreate (loc_id, name, H5T_NATIVE_IDX, space_hid,
                        octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                        octave_H5P_DEFAULT);
#else
  data_hid = H5Dcreate (loc_id, name, H5T_NATIVE_IDX, space_hid,
                        octave_H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      return data_hid;
    }

  retval = H5Dwrite (data_hid, H5T_NATIVE_IDX, octave_H5S_ALL, octave_H5S_ALL,
                     octave_H5P_DEFAULT, dims) >= 0;

  H5Dclose (data_hid);
  H5Sclose (space_hid);

  if (retval)
    retval = hdf5_add_attr (loc_id, "OCTAVE_EMPTY_MATRIX");

  return (retval == 0 ? 1 : retval);

#else
  err_disabled_feature ("save_hdf5_empty", "HDF5");
#endif
}

// Load an empty matrix, if needed.  Returns
//    > 0  loaded empty matrix, dimensions returned
//    = 0  Not an empty matrix; did nothing
//    < 0  Error condition
int
load_hdf5_empty (octave_hdf5_id loc_id, const char *name, dim_vector& d)
{
#if defined (HAVE_HDF5)

  if (! hdf5_check_attr (loc_id, "OCTAVE_EMPTY_MATRIX"))
    return 0;

  hsize_t hdims, maxdims;
#if defined (HAVE_HDF5_18)
  hid_t data_hid = H5Dopen (loc_id, name, octave_H5P_DEFAULT);
#else
  hid_t data_hid = H5Dopen (loc_id, name);
#endif
  hid_t space_id = H5Dget_space (data_hid);
  H5Sget_simple_extent_dims (space_id, &hdims, &maxdims);
  int retval;

  OCTAVE_LOCAL_BUFFER (octave_idx_type, dims, hdims);

  retval = H5Dread (data_hid, H5T_NATIVE_IDX, octave_H5S_ALL, octave_H5S_ALL,
                    octave_H5P_DEFAULT, dims);
  if (retval >= 0)
    {
      d.resize (hdims);
      for (hsize_t i = 0; i < hdims; i++)
        d(i) = dims[i];
    }

  H5Sclose (space_id);
  H5Dclose (data_hid);

  return (retval == 0 ? hdims : retval);

#else
  err_disabled_feature ("load_hdf5_empty", "HDF5");
#endif
}

// save_type_to_hdf5 is not currently used, since hdf5 doesn't yet support
// automatic float<->integer conversions:

// return the HDF5 type id corresponding to the Octave save_type

octave_hdf5_id
save_type_to_hdf5 (save_type st)
{
#if defined (HAVE_HDF5)
#  if defined (HAVE_HDF5_INT2FLOAT_CONVERSIONS)

  switch (st)
    {
    case LS_U_CHAR:
      return H5T_NATIVE_UCHAR;

    case LS_U_SHORT:
      return H5T_NATIVE_USHORT;

    case LS_U_INT:
      return H5T_NATIVE_UINT;

    case LS_CHAR:
      return H5T_NATIVE_CHAR;

    case LS_SHORT:
      return H5T_NATIVE_SHORT;

    case LS_INT:
      return H5T_NATIVE_INT;

    case LS_FLOAT:
      return H5T_NATIVE_FLOAT;

    case LS_DOUBLE:
    default:
      return H5T_NATIVE_DOUBLE;
    }

#  else

  octave_unused_parameter (st);

  return -1;

#  endif

#else

  octave_unused_parameter (st);

  err_disabled_feature ("save_type_to_hdf5", "HDF5");

#endif
}

// Add the data from TC to the HDF5 location loc_id, which could
// be either a file or a group within a file.  Return true if
// successful.  This function calls itself recursively for lists
// (stored as HDF5 groups).

bool
add_hdf5_data (octave_hdf5_id loc_id, const octave_value& tc,
               const std::string& name, const std::string& doc,
               bool mark_global, bool save_as_floats)
{
#if defined (HAVE_HDF5)

  hsize_t dims[3];
  hid_t type_id, space_id, data_id, data_type_id;
  type_id = space_id = data_id = data_type_id = -1;

  bool retval = false;
  octave_value val = tc;
  // FIXME: diagonal & permutation matrices currently don't know how to save
  // themselves, so we convert them first to normal matrices using A = A(:,:).
  // This is a temporary hack.
  if (val.is_diag_matrix () || val.is_perm_matrix ()
      || val.type_id () == octave_lazy_index::static_type_id ())
    val = val.full_value ();

  std::string t = val.type_name ();
#if defined (HAVE_HDF5_18)
  data_id = H5Gcreate (loc_id, name.c_str (), octave_H5P_DEFAULT,
                       octave_H5P_DEFAULT, octave_H5P_DEFAULT);
#else
  data_id = H5Gcreate (loc_id, name.c_str (), 0);
#endif
  if (data_id < 0)
    goto error_cleanup;

  // attach the type of the variable
  type_id = H5Tcopy (H5T_C_S1); H5Tset_size (type_id, t.length () + 1);
  if (type_id < 0)
    goto error_cleanup;

  dims[0] = 0;
  space_id = H5Screate_simple (0, dims, nullptr);
  if (space_id < 0)
    goto error_cleanup;
#if defined (HAVE_HDF5_18)
  data_type_id = H5Dcreate (data_id, "type",  type_id, space_id,
                            octave_H5P_DEFAULT, octave_H5P_DEFAULT,
                            octave_H5P_DEFAULT);
#else
  data_type_id = H5Dcreate (data_id, "type",  type_id, space_id,
                            octave_H5P_DEFAULT);
#endif
  if (data_type_id < 0
      || H5Dwrite (data_type_id, type_id, octave_H5S_ALL, octave_H5S_ALL,
                   octave_H5P_DEFAULT, t.c_str ()) < 0)
    goto error_cleanup;

  // Now call the real function to save the variable
  retval = val.save_hdf5 (data_id, "value", save_as_floats);

  // attach doc string as comment:
  if (retval && doc.length () > 0
      && H5Gset_comment (loc_id, name.c_str (), doc.c_str ()) < 0)
    retval = false;

  // if it's global, add an attribute "OCTAVE_GLOBAL" with value 1
  if (retval && mark_global)
    retval = hdf5_add_attr (data_id, "OCTAVE_GLOBAL") >= 0;

  // We are saving in the new variable format, so mark it
  if (retval)
    retval = hdf5_add_attr (data_id, "OCTAVE_NEW_FORMAT") >= 0;

error_cleanup:

  if (data_type_id >= 0)
    H5Dclose (data_type_id);

  if (type_id >= 0)
    H5Tclose (type_id);

  if (space_id >= 0)
    H5Sclose (space_id);

  if (data_id >= 0)
    H5Gclose (data_id);

  if (! retval)
    error ("save: error while writing '%s' to hdf5 file", name.c_str ());

  return retval;

#else
  err_disabled_feature ("add_hdf5_data", "HDF5");
#endif
}

// Write data from TC in HDF5 (binary) format to the stream OS,
// which must be an hdf5_ofstream, returning true on success.

bool
save_hdf5_data (std::ostream& os, const octave_value& tc,
                const std::string& name, const std::string& doc,
                bool mark_global, bool save_as_floats)
{
#if defined (HAVE_HDF5)

  octave::check_hdf5_types ();

  hdf5_ofstream& hs = dynamic_cast<hdf5_ofstream&> (os);

  return add_hdf5_data (hs.file_id, tc, name, doc,
                        mark_global, save_as_floats);

#else
  err_disabled_feature ("save_hdf5_data", "HDF5");
#endif
}

#endif
