/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

// Author: Steven G. Johnson <stevenj@alum.mit.edu>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if defined (HAVE_HDF5)

#include <cfloat>
#include <cstring>
#include <cctype>

#include <fstream>
#include <iomanip>
#include <iostream>
#include <memory>
#include <string>

#include <hdf5.h>

#include "byte-swap.h"
#include "data-conv.h"
#include "file-ops.h"
#include "glob-match.h"
#include "lo-mappers.h"
#include "lo-sstream.h"
#include "mach-info.h"
#include "oct-env.h"
#include "oct-time.h"
#include "quit.h"
#include "str-vec.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "load-save.h"
#include "oct-obj.h"
#include "oct-map.h"
#include "ov-cell.h"
#include "pager.h"
#include "pt-exp.h"
#include "symtab.h"
#include "sysdep.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"
#include "version.h"
#include "dMatrix.h"

#include "ls-utils.h"
#include "ls-hdf5.h"

// this is only used for HDF5 import
// try to convert s into a valid identifier, replacing invalid chars with "_":

static std::string
make_valid_identifier (const std::string& nm)
{
  std::string retval;

  size_t nm_len = nm.length ();

  if (nm_len > 0)
    {
      if (! isalpha (nm[0]))
	retval += '_';

      for (size_t i = 0; i < nm_len; i++)
	{
	  char c = nm[i];
	  retval += (isalnum (c) || c == '_') ? c : '_';
	}
    }

  return retval;
}

static bool
ident_is_all_digits (const std::string& id)
{
  bool retval = true;

  size_t len = 0;

  for (size_t i = 0; i < len; i++)
    {
      if (! isdigit (id[i]))
	{
	  retval = false;
	  break;
	}
    }

  return retval;
}

// Define this to 1 if/when HDF5 supports automatic conversion between
// integer and floating-point binary data:
#define HAVE_HDF5_INT2FLOAT_CONVERSIONS 0

// Given two compound types t1 and t2, determine whether they 
// are compatible for reading/writing.  This function only
// works for non-nested types composed of simple elements (ints, floats...),
// which is all we need it for

bool
hdf5_types_compatible (hid_t t1, hid_t t2)
{
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
}

// Import a multidimensional (rank >= 3) dataset whose id is data_id, into tc.
// This works by calling itself recursively, building up lists of lists
//  of lists ... of 2d matrices.  rank and dims are the rank and dimensions
//  of the dataset.  type_id is the datatype to read into.  If it is
//  H5T_NATIVE_DOUBLE, we are reading a real matrix.  Otherwise, type_id
//  is assumed to be a complex type for reading a complex matrix.
//
//  Upon entry, we should have curdim = rank - 1, start = an array
//  of length rank = all zeros, and count = an array of length rank =
//  all ones except for the first two dimensions which equal the corresponding
//  entries in dims[]. 
//
//  Note that we process the dimensions in reverse order, reflecting
//  the fact that Octave is uses column-major (Fortran-order) data while
//  HDF5 is row-major.  This means that the HDF5 file is read
//  non-contiguously, but on the other hand means that for a 3d array
//  we get a list of xy-plane slices, which seems nice.  We could change
//  this behavior without much trouble; what is the best thing to do?
//
//  Returns a positive value upon success.

static herr_t
hdf5_import_multidim (hid_t data_id, hid_t space_id, hsize_t rank,
		      const hsize_t *dims, hsize_t curdim,
		      hssize_t *start, const hsize_t *count,
		      hid_t type_id, octave_value &tc)
{
  herr_t retval = 1;

  if (rank < 3 || curdim < 1 || curdim >= rank)
    return -1;

  if (curdim == 1)
    {
      // import 2d dataset for 1st 2 dims directly as a matrix
      int nr, nc;    // rows and columns
      nc = dims[0];  // octave uses column-major & HDF5 uses row-major
      nr = dims[1];

      hid_t mem_space_id = H5Screate_simple (2, dims, 0);

      if (mem_space_id < 0)
	return -1;

      if (H5Sselect_all (mem_space_id) < 0)
	return -1;
    
      if (H5Sselect_hyperslab (space_id, H5S_SELECT_SET,
			       start, 0, count, 0) < 0)
	{
	  H5Sclose (mem_space_id);
	  return -1;
	}
    
      if (type_id == H5T_NATIVE_DOUBLE)
	{
	  // real matrix
	  Matrix m (nr, nc);
	  double *re = m.fortran_vec ();
	  if (H5Dread (data_id, type_id, mem_space_id, space_id,
		       H5P_DEFAULT, (void *) re) < 0)
	    retval = -1;  // error
	  else
	    tc = m;
	}
      else
	{
	  // assume that we are using complex numbers
	  // complex matrix
	  ComplexMatrix m (nr, nc);
	  Complex *reim = m.fortran_vec ();
	  if (H5Dread (data_id, type_id, mem_space_id, space_id,
		       H5P_DEFAULT, (void *) X_CAST (double *, reim)) < 0)
	    retval = -1;  // error
	  else
	    tc = m;
	}
    
      H5Sclose (mem_space_id);

    }
  else
    {
      octave_value_list lst;

      for (hsize_t i = 0; i < dims[curdim]; ++i)
	{
	  octave_value slice;
	  start[curdim] = i;
	  retval = hdf5_import_multidim (data_id, space_id, rank,
					 dims, curdim-1, start, count,
					 type_id, slice);
	  if (retval < 0)
	    break;
	  lst.append (slice);
	}

      if (retval > 0)
	tc = octave_value (lst);
    }

  return retval;
}

// Return true if loc_id has the attribute named attr_name, and false
// otherwise.

bool
hdf5_check_attr (hid_t loc_id, const char *attr_name)
{
  bool retval = false;

  // we have to pull some shenanigans here to make sure
  // HDF5 doesn't print out all sorts of error messages if we
  // call H5Aopen for a non-existing attribute

  H5E_auto_t err_func;
  void *err_func_data;

  // turn off error reporting temporarily, but save the error
  // reporting function:

  H5Eget_auto (&err_func, &err_func_data);
  H5Eset_auto (0, 0);

  hid_t attr_id = H5Aopen_name (loc_id, attr_name);

  if (attr_id >= 0)
    {
      // successful
      retval = 1;
      H5Aclose (attr_id);
    }

  // restore error reporting:
  H5Eset_auto (err_func, err_func_data);

  return retval;
}

// The following two subroutines create HDF5 representations of the way
// we will store Octave complex and range types (pairs and triplets of
// floating-point numbers, respectively).  NUM_TYPE is the HDF5 numeric
// type to use for storage (e.g. H5T_NATIVE_DOUBLE to save as 'double').
// Note that any necessary conversions are handled automatically by HDF5.

static hid_t
hdf5_make_complex_type (hid_t num_type)
{
  hid_t type_id = H5Tcreate (H5T_COMPOUND, sizeof (double) * 2);

  H5Tinsert (type_id, "real", 0 * sizeof (double), num_type);
  H5Tinsert (type_id, "imag", 1 * sizeof (double), num_type);

  return type_id;
}

static hid_t
hdf5_make_range_type (hid_t num_type)
{
  hid_t type_id = H5Tcreate (H5T_COMPOUND, sizeof (double) * 3);

  H5Tinsert (type_id, "base", 0 * sizeof (double), num_type);
  H5Tinsert (type_id, "limit", 1 * sizeof (double), num_type);
  H5Tinsert (type_id, "increment", 2 * sizeof (double), num_type);

  return type_id;
}

// Callback data structure for passing data to hdf5_read_next_data, below.

struct
hdf5_callback_data
{
  hdf5_callback_data (void)
    : name (), global (false), tc (), doc (),
      complex_type (hdf5_make_complex_type (H5T_NATIVE_DOUBLE)),
      range_type (hdf5_make_range_type (H5T_NATIVE_DOUBLE)),
      import (false) { }

  // the following fields are set by hdf5_read_data on successful return:

  // the name of the variable
  std::string name;

  // whether it is global
  bool global;

  // the value of the variable, in Octave form
  octave_value tc;

  // a documentation string (NULL if none)
  std::string doc;

  // the following fields are input to hdf5_read_data:

  // HDF5 rep's of complex and range type
  hid_t complex_type, range_type;

  // whether to try extra hard to import "foreign" data
  bool import;
};

// This variable, set in read_hdf5_data(), tells whether we are using
// a version of HDF5 with a buggy H5Giterate (i.e. which neglects to
// increment the index parameter to the next unread item).
static bool have_h5giterate_bug = false;

// This function is designed to be passed to H5Giterate, which calls it
// on each data item in an HDF5 file.  For the item whose name is NAME in
// the group GROUP_ID, this function sets dv->tc to an Octave representation
// of that item.  (dv must be a pointer to hdf5_callback_data.)  (It also
// sets the other fields of dv).
//
// It returns 1 on success (in which case H5Giterate stops and returns),
// -1 on error, and 0 to tell H5Giterate to continue on to the next item
// (e.g. if NAME was a data type we don't recognize).

static herr_t
hdf5_read_next_data (hid_t group_id, const char *name, void *dv)
{
  hdf5_callback_data *d = static_cast <hdf5_callback_data *> (dv);

  H5G_stat_t info;
  herr_t retval = 0;
  bool ident_valid = valid_identifier (name);

  std::string vname = name;

  // Allow identifiers as all digits so we can load lists saved by
  // earlier versions of Octave.

  if (! ident_valid && (d->import || ident_is_all_digits (vname)))
    {
      // fix the identifier, replacing invalid chars with underscores
      vname = make_valid_identifier (vname);

      // check again (in case vname was null, empty, or some such thing):
      ident_valid = valid_identifier (vname); 
    }

  H5Gget_objinfo (group_id, name, 1, &info);

  if (info.type == H5G_DATASET && ident_valid)
    {
      retval = 1;

      hid_t data_id = H5Dopen (group_id, name);

      if (data_id < 0)
	{
	  retval = data_id;

	  goto done;
	}

      hid_t type_id = H5Dget_type (data_id);

      hid_t type_class_id = H5Tget_class (type_id);

#if HAVE_HDF5_INT2FLOAT_CONVERSIONS
      if (type_class_id == H5T_INTEGER || type_class_id == H5T_FLOAT)
	{
#else
      // hdf5 doesn't (yet) support automatic float/integer conversions
      if (type_class_id == H5T_FLOAT)
	{
#endif
	  // read real matrix or scalar variable

	  hid_t space_id = H5Dget_space (data_id);

	  hsize_t rank = H5Sget_simple_extent_ndims (space_id);

	  if (rank == 0)
	    {
	      // real scalar:
	      double dtmp;
	      if (H5Dread (data_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, 
			   H5P_DEFAULT, (void *) &dtmp) < 0)
		retval = -1;  // error
	      else
		d->tc = dtmp;
	    }
	  else if (rank > 0 && rank <= 2)
	    {
	      // real matrix
	      OCTAVE_LOCAL_BUFFER (hsize_t, dims, rank);
	      OCTAVE_LOCAL_BUFFER (hsize_t, maxdims, rank);

	      H5Sget_simple_extent_dims (space_id, dims, maxdims);

	      int nr, nc;  // rows and columns
	      // octave uses column-major & HDF5 uses row-major
	      nc = dims[0];
	      nr = rank > 1 ? dims[1] : 1;
	      Matrix m (nr, nc);
	      double *re = m.fortran_vec ();
	      if (H5Dread (data_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, 
			   H5P_DEFAULT, (void *) re) < 0)
		retval = -1;  // error
	      else
		d->tc = m;
	    }
	  else if (rank >= 3 && d->import)
	    {
	      OCTAVE_LOCAL_BUFFER (hsize_t, dims, rank);
	      OCTAVE_LOCAL_BUFFER (hsize_t, maxdims, rank);

	      H5Sget_simple_extent_dims (space_id, dims, maxdims);

	      OCTAVE_LOCAL_BUFFER (hssize_t, start, rank);
	      OCTAVE_LOCAL_BUFFER (hsize_t, count, rank);

	      for (hsize_t i = 0; i < rank; ++i)
		{
		  start[i] = 0;
		  count[i] = 1;
		}
	      count[0] = dims[0];
	      count[1] = dims[1];
	      retval = hdf5_import_multidim (data_id, space_id,
					     rank, dims, rank-1,
					     start, count,
					     H5T_NATIVE_DOUBLE, d->tc);
	    }
	  else
	    {
	      warning ("load: can't read %d-dim. hdf5 dataset %s",
		       rank, name);
	      retval = 0;  // skip; we can't read 3+ dimensional datasets
	    }

	  H5Sclose (space_id);
	}
      else if (type_class_id == H5T_STRING)
	{
	  // read string variable
	  hid_t space_id = H5Dget_space (data_id);
	  hsize_t rank = H5Sget_simple_extent_ndims (space_id);

	  if (rank == 0)
	    {
	      // a single string:
	      int slen = H5Tget_size (type_id);
	      if (slen < 0)
		retval = -1;  // error
	      else
		{
		  OCTAVE_LOCAL_BUFFER (char, s, slen);
		  // create datatype for (null-terminated) string
		  // to read into:
		  hid_t st_id = H5Tcopy (H5T_C_S1);
		  H5Tset_size (st_id, slen);
		  if (H5Dread (data_id, st_id, H5S_ALL, H5S_ALL, 
			       H5P_DEFAULT, (void *) s) < 0)
		    {
		      retval = -1;  // error
		    }
		  else
		    d->tc = s;

		  H5Tclose (st_id);
		}
	    }
	  else if (rank == 1)
	    {
	      // string vector
	      hsize_t elements, maxdim;
	      H5Sget_simple_extent_dims (space_id, &elements, &maxdim);
	      int slen = H5Tget_size (type_id);
	      if (slen < 0)
		retval = -1;  // error
	      else
		{
		  // hdf5 string arrays store strings of all the
		  // same physical length (I think), which is
		  // slightly wasteful, but oh well.

		  OCTAVE_LOCAL_BUFFER (char, s, elements * slen);

		  // create datatype for (null-terminated) string
		  // to read into:
		  hid_t st_id = H5Tcopy (H5T_C_S1);
		  H5Tset_size (st_id, slen);

		  if (H5Dread (data_id, st_id, H5S_ALL, H5S_ALL, 
			       H5P_DEFAULT, (void *) s) < 0)
		    retval = -1;  // error
		  else
		    {
		      charMatrix chm (elements, slen - 1);
		      for (hsize_t i = 0; i < elements; ++i)
			{
			  chm.insert (s + i*slen, i, 0);
			}
		      d->tc = octave_value (chm, true);
		    }

		  H5Tclose (st_id);
		}
	    }
	  else
	    {
	      warning ("load: can't read %d-dim. hdf5 string vector %s",
		       rank, name); 
	      // skip; we can't read higher-dimensional string vectors
	      retval = 0;
	    }
	}
      else if (type_class_id == H5T_COMPOUND)
	{
	  // check for complex or range data:

	  if (hdf5_types_compatible (type_id, d->complex_type))
	    {
	      // read complex matrix or scalar variable

	      hid_t space_id = H5Dget_space (data_id);
	      hsize_t rank = H5Sget_simple_extent_ndims (space_id);

	      if (rank == 0)
		{
		  // complex scalar:
		  Complex ctmp;
		  if (H5Dread (data_id, d->complex_type, H5S_ALL,
			       H5S_ALL, H5P_DEFAULT,
			       (void *) X_CAST (double *, &ctmp)) < 0)
		    retval = -1;  // error
		  else
		    d->tc = ctmp;
		}
	      else if (rank > 0 && rank <= 2)
		{
		  // complex matrix
		  OCTAVE_LOCAL_BUFFER (hsize_t, dims, rank);
		  OCTAVE_LOCAL_BUFFER (hsize_t, maxdims, rank);
		  H5Sget_simple_extent_dims (space_id, dims, maxdims);
		  int nr, nc;  // rows and columns
		  // octave uses column-major & HDF5 uses row-major
		  nc = dims[0];
		  nr = rank > 1 ? dims[1] : 1;
		  ComplexMatrix m (nr, nc);
		  Complex *reim = m.fortran_vec ();
		  if (H5Dread (data_id, d->complex_type, H5S_ALL,
			       H5S_ALL, H5P_DEFAULT,
			       (void *) X_CAST (double *, reim)) < 0)
		    retval = -1;  // error
		  else
		    d->tc = m;
		}
	      else if (rank >= 3 && d->import)
		{
		  OCTAVE_LOCAL_BUFFER (hsize_t, dims, rank);
		  OCTAVE_LOCAL_BUFFER (hsize_t, maxdims, rank);
		  H5Sget_simple_extent_dims (space_id, dims, maxdims);
		  OCTAVE_LOCAL_BUFFER (hssize_t, start, rank);
		  OCTAVE_LOCAL_BUFFER (hsize_t, count, rank);
		  for (hsize_t i = 0; i < rank; ++i)
		    {
		      start[i] = 0;
		      count[i] = 1;
		    }
		  count[0] = dims[0];
		  count[1] = dims[1];
		  retval = hdf5_import_multidim (data_id, space_id,
						 rank, dims, rank-1,
						 start, count,
						 d->complex_type,
						 d->tc);
		}
	      else
		{
		  warning ("load: can't read %d-dim. hdf5 dataset %s",
			   rank, name);
		  // skip; we can't read 3+ dimensional datasets
		  retval = 0;
		}
	      H5Sclose (space_id);
	    }
	  else if (hdf5_types_compatible (type_id, d->range_type))
	    {
	      // read range variable:
	      hid_t space_id = H5Dget_space (data_id);
	      hsize_t rank = H5Sget_simple_extent_ndims (space_id);

	      if (rank == 0)
		{
		  double rangevals[3];
		  if (H5Dread (data_id, d->range_type, H5S_ALL, H5S_ALL, 
			       H5P_DEFAULT, (void *) rangevals) < 0)
		    retval = -1;  // error
		  else
		    {
		      Range r (rangevals[0], rangevals[1], rangevals[2]);
		      d->tc = r;
		    }
		}
	      else
		{
		  warning ("load: can't read range array `%s' in hdf5 file",
			   name);
		  // skip; we can't read arrays of range variables
		  retval = 0;
		}

	      H5Sclose (space_id);
	    }
	  else
	    {
	      warning ("load: can't read `%s' (unknown compound datatype)",
		       name);
	      retval = 0; // unknown datatype; skip.
	    }
	}
      else
	{
	  warning ("load: can't read `%s' (unknown datatype)", name);
	  retval = 0; // unknown datatype; skip
	}

      H5Tclose (type_id);

      // check for OCTAVE_GLOBAL attribute:
      d->global = hdf5_check_attr (data_id, "OCTAVE_GLOBAL");

      H5Dclose (data_id);
    }
  else if (info.type == H5G_GROUP && ident_valid)
    {
      // read in group as a list or a structure
      retval = 1;

      hid_t subgroup_id = H5Gopen (group_id, name);

      if (subgroup_id < 0)
	{
	  retval = subgroup_id;
	  goto done;
	}

      // an HDF5 group is treated as an octave structure by
      // default (since that preserves name information), and an
      // octave list otherwise.

      bool is_list = hdf5_check_attr (subgroup_id, "OCTAVE_LIST");

      hdf5_callback_data dsub;

      dsub.complex_type = d->complex_type;
      dsub.range_type = d->range_type;
      dsub.import = d->import;

      herr_t retval2;
      octave_value_list lst;
      Octave_map m;
      int current_item = 0;
      while ((retval2 = H5Giterate (group_id, name, &current_item,
				    hdf5_read_next_data, &dsub)) > 0)
	{
	  if (is_list)
	    lst.append (dsub.tc);
	  else
	    {
	      octave_value ov = dsub.tc;

	      if (ov.is_list ())
		m [dsub.name] = ov.list_value ();
	      else
		m [dsub.name] = ov;
	    }

	  if (have_h5giterate_bug)
	    current_item++;  // H5Giterate returned the last index processed
	}

      if (retval2 < 0)
	retval = retval2;
      else
	{
	  d->global = hdf5_check_attr (group_id, "OCTAVE_GLOBAL");

	  if (is_list)
	    d->tc = octave_value (lst);
	  else
	    d->tc = m;
	}

      H5Gclose (subgroup_id);
    }
  else if (! ident_valid)
    {
      // should we attempt to handle invalid identifiers by converting
      // bad characters to '_', say?
      warning ("load: skipping invalid identifier `%s' in hdf5 file",
	       name);
    }

 done:

  if (retval < 0)
    error ("load: error while reading hdf5 item %s", name);

  if (retval > 0)
    {
      // get documentation string, if any:
      int comment_length = H5Gget_comment (group_id, name, 0, 0);

      if (comment_length > 1)
	{
	  OCTAVE_LOCAL_BUFFER (char, tdoc, comment_length);
	  H5Gget_comment (group_id, name, comment_length, tdoc);
	  d->doc = tdoc;
	}
      else if (vname != name)
	{
	  // the name was changed by import; store the original name
	  // as the documentation string:
	  d->doc = name;
	}

      // copy name (actually, vname):
      d->name = vname;
    }

  return retval;
}

// Read the next Octave variable from the stream IS, which must really be
// an hdf5_ifstream.  Return the variable value in tc, its doc string
// in doc, and whether it is global in global.  The return value is
// the name of the variable, or NULL if none were found or there was
// and error.  If import is true, we try extra hard to import "foreign"
// datasets (not created by Octave), although we usually do a reasonable
// job anyway.  (c.f. load -import documentation.)
std::string
read_hdf5_data (std::istream& is,
		const std::string& filename, bool& global,
		octave_value& tc, std::string& doc, bool import)
{
  std::string retval;

  doc.resize (0);

  hdf5_ifstream& hs = (hdf5_ifstream&) is;
  hdf5_callback_data d;

  d.import = import;

  // Versions of HDF5 prior to 1.2.2 had a bug in H5Giterate where it
  // would return the index of the last item processed instead of the
  // next item to be processed, forcing us to increment the index manually.

  unsigned int vers_major, vers_minor, vers_release;

  H5get_libversion (&vers_major, &vers_minor, &vers_release);

  // XXX FIXME XXX -- this test looks wrong.
  have_h5giterate_bug
    = (vers_major < 1
       || (vers_major == 1 && (vers_minor < 2
			       || (vers_minor == 2 && vers_release < 2))));

  herr_t H5Giterate_retval = H5Giterate (hs.file_id, "/", &hs.current_item,
					 hdf5_read_next_data, &d);

  if (have_h5giterate_bug)
    {
      // H5Giterate sets current_item to the last item processed; we want
      // the index of the next item (for the next call to read_hdf5_data)

      hs.current_item++;
    }

  if (H5Giterate_retval > 0)
    {
      global = d.global;
      tc = d.tc;
      doc = d.doc;
    }
  else
    {
      // an error occurred (H5Giterate_retval < 0) or there are no
      // more datasets print an error message if retval < 0?
      // hdf5_read_next_data already printed one, probably.
    }

  H5Tclose (d.complex_type);
  H5Tclose (d.range_type);

  if (! d.name.empty ())
    retval = d.name;

  return retval;
}

// Add an attribute named attr_name to loc_id (a simple scalar
// attribute with value 1).  Return value is >= 0 on success.
static herr_t
hdf5_add_attr (hid_t loc_id, const char *attr_name)
{
  herr_t retval = 0;

  hid_t as_id = H5Screate (H5S_SCALAR);

  if (as_id >= 0)
    {
      hid_t a_id = H5Acreate (loc_id, attr_name,
			      H5T_NATIVE_UCHAR, as_id, H5P_DEFAULT);

      if (a_id >= 0)
	{
	  unsigned char attr_val = 1;

	  retval = H5Awrite (a_id, H5T_NATIVE_UCHAR, (void*) &attr_val);

	  H5Aclose (a_id);
	}
      else
	retval = a_id;

      H5Sclose (as_id);
    }
  else
    retval = as_id;

  return retval;
}


// save_type_to_hdf5 is not currently used, since hdf5 doesn't yet support
// automatic float<->integer conversions:

#if HAVE_HDF5_INT2FLOAT_CONVERSIONS

// return the HDF5 type id corresponding to the Octave save_type

static hid_t
save_type_to_hdf5 (save_type st)
{
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
}
#endif /* HAVE_HDF5_INT2FLOAT_CONVERSIONS */

// Add the data from TC to the HDF5 location loc_id, which could
// be either a file or a group within a file.  Return true if
// successful.  This function calls itself recursively for lists
// (stored as HDF5 groups).

static bool
add_hdf5_data (hid_t loc_id, const octave_value& tc,
	       const std::string& name, const std::string& doc,
	       bool mark_as_global, bool save_as_floats)
{
  hsize_t dims[3];
  hid_t type_id = -1, space_id = -1, data_id = -1;
  bool data_is_group = 0;
  bool retval = 0;

  if (tc.is_string ())
    {
      int nr = tc.rows ();
      charMatrix chm = tc.char_matrix_value ();
      int nc = chm.cols ();

      // create datatype for (null-terminated) string to write from:
      type_id = H5Tcopy (H5T_C_S1); H5Tset_size (type_id, nc + 1);
      if (type_id < 0)
	goto error_cleanup;

      dims[0] = nr;
      space_id = H5Screate_simple (nr > 0 ? 1 : 0, dims, (hsize_t*) 0);
      if (space_id < 0)
	goto error_cleanup;

      data_id = H5Dcreate (loc_id, name.c_str (), 
			   type_id, space_id, H5P_DEFAULT);
      if (data_id < 0)
	goto error_cleanup;

      OCTAVE_LOCAL_BUFFER (char, s, nr * (nc + 1));

      for (int i = 0; i < nr; ++i)
	{
	  std::string tstr = chm.row_as_string (i);
	  strcpy (s + i * (nc+1), tstr.c_str ());
	}

      if (H5Dwrite (data_id, type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT,
		    (void*) s) < 0) {
	goto error_cleanup;
      }
    }
  else if (tc.is_range ())
    {
      space_id = H5Screate_simple (0, dims, (hsize_t*) 0);
      if (space_id < 0)
	goto error_cleanup;

      type_id = hdf5_make_range_type (H5T_NATIVE_DOUBLE);
      if (type_id < 0)
	goto error_cleanup;

      data_id = H5Dcreate (loc_id, name.c_str (), 
			   type_id, space_id, H5P_DEFAULT);
      if (data_id < 0)
	goto error_cleanup;
    
      Range r = tc.range_value ();
      double range_vals[3];
      range_vals[0] = r.base ();
      range_vals[1] = r.limit ();
      range_vals[2] = r.inc ();

      if (H5Dwrite (data_id, type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT,
		    (void*) range_vals) < 0)
	goto error_cleanup;
    }
  else if (tc.is_real_scalar ())
    {
      space_id = H5Screate_simple (0, dims, (hsize_t*) 0);
      if (space_id < 0) goto error_cleanup;

      data_id = H5Dcreate (loc_id, name.c_str (), 
			   H5T_NATIVE_DOUBLE, space_id, H5P_DEFAULT);
      if (data_id < 0)
	goto error_cleanup;

      double tmp = tc.double_value ();
      if (H5Dwrite (data_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,
		    H5P_DEFAULT, (void*) &tmp) < 0)
	goto error_cleanup;
    }
  else if (tc.is_real_matrix ())
    {
      Matrix m = tc.matrix_value ();
      dims[1] = m.rows ();    // Octave uses column-major, while
      dims[0] = m.columns (); // HDF5 uses row-major ordering

      space_id = H5Screate_simple (dims[1] > 1 ?2:1, dims, (hsize_t*) 0);
      if (space_id < 0)
	goto error_cleanup;

      hid_t save_type_id = H5T_NATIVE_DOUBLE;

      if (save_as_floats)
	{
	  if (m.too_large_for_float ())
	    {
	      warning ("save: some values too large to save as floats --");
	      warning ("save: saving as doubles instead");
	    }
	  else
	    save_type_id = H5T_NATIVE_FLOAT;
	}
#if HAVE_HDF5_INT2FLOAT_CONVERSIONS
      // hdf5 currently doesn't support float/integer conversions
      else
	{
	  double max_val, min_val;

	  if (m.all_integers (max_val, min_val))
	    save_type_id
	      = save_type_to_hdf5 (get_save_type (max_val, min_val));
	}
#endif /* HAVE_HDF5_INT2FLOAT_CONVERSIONS */

      data_id = H5Dcreate (loc_id, name.c_str (), 
			   save_type_id, space_id, H5P_DEFAULT);
      if (data_id < 0)
	goto error_cleanup;
    
      double *mtmp = m.fortran_vec ();
      if (H5Dwrite (data_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,
		    H5P_DEFAULT, (void*) mtmp) < 0)
	goto error_cleanup;
    }
  else if (tc.is_complex_scalar ())
    {
      space_id = H5Screate_simple (0, dims, (hsize_t*) 0);
      if (space_id < 0)
	goto error_cleanup;

      type_id = hdf5_make_complex_type (H5T_NATIVE_DOUBLE);
      if (type_id < 0)
	goto error_cleanup;

      data_id = H5Dcreate (loc_id, name.c_str (), 
			   type_id, space_id, H5P_DEFAULT);
      if (data_id < 0)
	goto error_cleanup;

      Complex tmp = tc.complex_value ();
      if (H5Dwrite (data_id, type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT,
		    (void*) X_CAST (double*, &tmp)) < 0)
	goto error_cleanup;
    }
  else if (tc.is_complex_matrix ())
    {
      ComplexMatrix m = tc.complex_matrix_value ();

      dims[1] = m.rows ();    // Octave uses column-major, while
      dims[0] = m.columns (); // HDF5 uses row-major ordering

      space_id = H5Screate_simple (dims[1] > 1 ?2:1, dims, (hsize_t*) 0);
      if (space_id < 0)
	goto error_cleanup;

      hid_t save_type_id = H5T_NATIVE_DOUBLE;

      if (save_as_floats)
	{
	  if (m.too_large_for_float ())
	    {
	      warning ("save: some values too large to save as floats --");
	      warning ("save: saving as doubles instead");
	    }
	  else
	    save_type_id = H5T_NATIVE_FLOAT;
	}
#if HAVE_HDF5_INT2FLOAT_CONVERSIONS
      // hdf5 currently doesn't support float/integer conversions
      else
	{
	  double max_val, min_val;

	  if (m.all_integers (max_val, min_val))
	    save_type_id
	      = save_type_to_hdf5 (get_save_type (max_val, min_val));
	}
#endif /* HAVE_HDF5_INT2FLOAT_CONVERSIONS */

      type_id = hdf5_make_complex_type (save_type_id);
      if (type_id < 0) goto error_cleanup;

      data_id = H5Dcreate (loc_id, name.c_str (), 
			   type_id, space_id, H5P_DEFAULT);
      if (data_id < 0)
	goto error_cleanup;
    
      hid_t complex_type_id = hdf5_make_complex_type (H5T_NATIVE_DOUBLE);
      if (complex_type_id < 0)
	goto error_cleanup;

      Complex *mtmp = m.fortran_vec ();
      if (H5Dwrite (data_id, complex_type_id, H5S_ALL, H5S_ALL, H5P_DEFAULT,
		    (void*) X_CAST (double *, mtmp)) < 0)
	{
	  H5Tclose (complex_type_id);
	  goto error_cleanup;
	}

      H5Tclose (complex_type_id);
    }
  else if (tc.is_list ())
    {
      data_id = H5Gcreate (loc_id, name.c_str (), 0);
      if (data_id < 0)
	goto error_cleanup;

      data_is_group = 1;

      // recursively add each element of the list to this group
      octave_value_list lst = tc.list_value ();

      for (int i = 0; i < lst.length (); ++i)
	{
	  // should we use lst.name_tags () to label the elements?
	  char s[20];
	  sprintf (s, "_%d", i);
	  bool retval2 = add_hdf5_data (data_id, lst (i), s, "",
					false, save_as_floats);
	  if (! retval2)
	    goto error_cleanup;
	}

      // mark with an attribute "OCTAVE_LIST" with value 1
      // to distinguish from structures (also stored as HDF5 groups):
      if (hdf5_add_attr (data_id, "OCTAVE_LIST") < 0)
	goto error_cleanup;
    }
  else if (tc.is_map ())
    {
      // an Octave structure
      data_id = H5Gcreate (loc_id, name.c_str (), 0);
      if (data_id < 0)
	goto error_cleanup;

      data_is_group = 1;

      // recursively add each element of the structure to this group
      Octave_map m = tc.map_value ();
      Octave_map::iterator i = m.begin ();
      while (i != m.end ())
	{
	  // XXX FIXME XXX -- if the length of the structure array is
	  // 1, should we really create a list object?
	  bool retval2 = add_hdf5_data (data_id, octave_value (m.contents (i)),
					m.key (i), "", false, save_as_floats);
	  if (! retval2)
	    goto error_cleanup;

	  i++;
	}
    }
  else
    {
      gripe_wrong_type_arg ("save", tc, false);
      goto error_cleanup;
    }

  // attach doc string as comment:
  if (doc.length () > 0
      && H5Gset_comment (loc_id, name.c_str (), doc.c_str ()) < 0)
    goto error_cleanup;

  retval = 1;

  // if it's global, add an attribute "OCTAVE_GLOBAL" with value 1
  if (mark_as_global)
    retval = hdf5_add_attr (data_id, "OCTAVE_GLOBAL") >= 0;

 error_cleanup:

  if (! retval)
    error ("save: error while writing `%s' to hdf5 file", name.c_str ());

  if (data_id >= 0)
    {
      if (data_is_group)
	H5Gclose (data_id);
      else
	H5Dclose (data_id);
    }

  if (space_id >= 0)
    H5Sclose (space_id);

  if (type_id >= 0)
    H5Tclose (type_id);

  return retval;
}

// Write data from TC in HDF5 (binary) format to the stream OS,
// which must be an hdf5_ofstream, returning true on success.

bool
save_hdf5_data (std::ostream& os, const octave_value& tc,
		const std::string& name, const std::string& doc,
		bool mark_as_global, bool save_as_floats)
{
  hdf5_ofstream& hs = (hdf5_ofstream&) os;

  return add_hdf5_data (hs.file_id, tc, name, doc,
			mark_as_global, save_as_floats);
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
