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

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "lo-ieee.h"
#include "mx-base.h"

#include "oct-obj.h"
#include "ops.h"
#include "ov-re-mat.h"
#include "ov-str-mat.h"
#include "gripes.h"
#include "pr-output.h"
#include "pt-mat.h"

#include "byte-swap.h"
#include "ls-oct-ascii.h"
#include "ls-hdf5.h"
#include "ls-utils.h"

DEFINE_OCTAVE_ALLOCATOR (octave_char_matrix_str);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_char_matrix_str, "string", "char");

static octave_value *
default_numeric_conversion_function (const octave_value& a)
{
  CAST_CONV_ARG (const octave_char_matrix_str&);

  NDArray nda = v.array_value (true);

  return error_state ? 0 : new octave_matrix (nda);
}

type_conv_fcn
octave_char_matrix_str::numeric_conversion_function (void) const
{
  return default_numeric_conversion_function;
}

octave_value
octave_char_matrix_str::do_index_op (const octave_value_list& idx,
				     int resize_ok)
{
  octave_value retval;

  int len = idx.length ();

  switch (len)
    {
    case 2:
      {
	idx_vector i = idx (0).index_vector ();
	idx_vector j = idx (1).index_vector ();

	retval = octave_value (charNDArray (matrix.index (i, j, resize_ok)),
			       true);
      }
      break;

    case 1:
      {
	idx_vector i = idx (0).index_vector ();

	retval = octave_value (charNDArray (matrix.index (i, resize_ok)),
			       true);
      }
      break;

    default:
      {
	Array<idx_vector> idx_vec (len);

	for (int i = 0; i < len; i++)
	  idx_vec(i) = idx(i).index_vector ();

	retval = octave_value (charNDArray (matrix.index (idx_vec, resize_ok)),
			       true);
      }
      break;
    }

  return retval;
}

void
octave_char_matrix_str::assign (const octave_value_list& idx,
				const charMatrix& rhs)
{
  int len = idx.length ();

  // XXX FIXME XXX
  charMatrix tmp = rhs;
  if (tmp.rows () == 1 && tmp.columns () == 0)
    tmp.resize (0, 0);    

  for (int i = 0; i < len; i++)
    matrix.set_index (idx(i).index_vector ());

  ::assign (matrix, tmp, Vstring_fill_char);
}

bool
octave_char_matrix_str::valid_as_scalar_index (void) const
{
  bool retval = false;
  error ("octave_char_matrix_str::valid_as_scalar_index(): not implemented");
  return retval;
}

#define CHAR_MATRIX_CONV(T, INIT, TNAME, FCN) \
  T retval INIT; \
 \
  if (! force_string_conv) \
    gripe_invalid_conversion ("string", TNAME); \
  else \
    { \
      if (Vwarn_str_to_num) \
	gripe_implicit_conversion ("string", TNAME); \
 \
      retval = octave_char_matrix::FCN (); \
    } \
 \
  return retval

double
octave_char_matrix_str::double_value (bool force_string_conv) const
{
  CHAR_MATRIX_CONV (double, = 0, "real scalar", double_value);
}

Complex
octave_char_matrix_str::complex_value (bool force_string_conv) const
{
  CHAR_MATRIX_CONV (Complex, = 0, "complex scalar", complex_value);
}

Matrix
octave_char_matrix_str::matrix_value (bool force_string_conv) const
{
  CHAR_MATRIX_CONV (Matrix, , "real matrix", matrix_value);
}

ComplexMatrix
octave_char_matrix_str::complex_matrix_value (bool force_string_conv) const
{
  CHAR_MATRIX_CONV (ComplexMatrix, , "complex matrix", complex_matrix_value);
}

NDArray
octave_char_matrix_str::array_value (bool force_string_conv) const
{
  CHAR_MATRIX_CONV (NDArray, , "real N-d array", array_value);
}

ComplexNDArray
octave_char_matrix_str::complex_array_value (bool force_string_conv) const
{
  CHAR_MATRIX_CONV (ComplexNDArray, , "complex N-d array",
		    complex_array_value);
}

string_vector
octave_char_matrix_str::all_strings (bool, bool) const
{
  string_vector retval;

  if (matrix.ndims () == 2)
    {
      charMatrix chm = matrix.matrix_value ();

      int n = chm.rows ();

      retval.resize (n);

      for (int i = 0; i < n; i++)
	retval[i] = chm.row_as_string (i, true);
    }
  else
    error ("invalid conversion of charNDArray to string_vector");

  return retval;
}

std::string
octave_char_matrix_str::string_value (bool) const
{
  std::string retval;

  if (matrix.ndims () == 2)
    {
      charMatrix chm = matrix.matrix_value ();

      retval = chm.row_as_string (0);  // XXX FIXME??? XXX
    }
  else
    error ("invalid conversion of charNDArray to string");

  return retval;
}

void
octave_char_matrix_str::print_raw (std::ostream& os, bool pr_as_read_syntax) const
{
  octave_print_internal (os, matrix, pr_as_read_syntax,
			 current_print_indent_level (), true);
}

bool 
octave_char_matrix_str::save_ascii (std::ostream& os,
				    bool& /* infnan_warned */, 
				    bool /* strip_nan_and_inf */)
{
  charMatrix chm = char_matrix_value ();
  int elements = chm.rows ();
  os << "# elements: " << elements << "\n";
  for (int i = 0; i < elements; i++)
    {
      unsigned len = chm.cols ();
      os << "# length: " << len << "\n";
      std::string tstr = chm.row_as_string (i, false, true);
      const char *tmp = tstr.data ();
      if (tstr.length () > len)
	panic_impossible ();
      os.write (X_CAST (char *, tmp), len);
      os << "\n";
    }

  return true;
}

bool 
octave_char_matrix_str::load_ascii (std::istream& is)
{
  int elements;
  bool success = true;
  std::streampos pos = is.tellg ();

  if (extract_keyword (is, "elements", elements, true))
    {

      if (elements >= 0)
	{
	  // XXX FIXME XXX -- need to be able to get max length
	  // before doing anything.

	  charMatrix chm (elements, 0);
	  int max_len = 0;
	  for (int i = 0; i < elements; i++)
	    {
	      int len;
	      if (extract_keyword (is, "length", len) && len >= 0)
		{
		  OCTAVE_LOCAL_BUFFER (char, tmp, len+1);
		  
		  if (len > 0 && ! is.read (X_CAST (char *, tmp), len))
		    {
		      error ("load: failed to load string constant");
		      success = false;
		      break;
		    }
		  else
		    {
		      tmp [len] = '\0';
		      if (len > max_len)
			{
			  max_len = len;
			  chm.resize (elements, max_len, 0);
			}
		      chm.insert (tmp, i, 0);
		    }
		}
	      else
		{
		  error ("load: failed to extract string length for element %d", 
			 i+1);
		  success = false;
		}
	    }
	  
	  if (! error_state)
	    matrix = chm;
	  
	}
      else
	{
	  error ("load: failed to extract number of string elements");
	  success = false;
	}
    }
  else
    {
      // re-read the same line again
      is.clear ();
      is.seekg (pos);

      int len;
      
      if (extract_keyword (is, "length", len) && len >= 0)
	{
	  // This is cruft for backward compatiability, but relatively harmless.

	  OCTAVE_LOCAL_BUFFER (char, tmp, len+1);

	  if (len > 0 && ! is.read (X_CAST (char *, tmp), len))
	    {
	      error ("load: failed to load string constant");
	    }
	  else
	    {
	      tmp [len] = '\0';

	      if (is)
		matrix = charMatrix (tmp);
	      else
		error ("load: failed to load string constant");
	    }
	}
    }

  return success;
}

bool 
octave_char_matrix_str::save_binary (std::ostream& os,
				     bool& /* save_as_floats */)
{
  FOUR_BYTE_INT nr = rows ();
  os.write (X_CAST (char *, &nr), 4);
  charMatrix chm = char_matrix_value ();
  for (int i = 0; i < nr; i++)
    {
      FOUR_BYTE_INT len = chm.cols ();
      os.write (X_CAST (char *, &len), 4);
      std::string tstr = chm.row_as_string (i);
      const char *btmp = tstr.data ();
      os.write (X_CAST (char *, btmp), len);
    }
  return true;
}

bool 
octave_char_matrix_str::load_binary (std::istream& is, bool swap,
				     oct_mach_info::float_format /* fmt */)
{
  FOUR_BYTE_INT elements;
  if (! is.read (X_CAST (char *, &elements), 4))
    return false;
  if (swap)
    swap_4_bytes (X_CAST (char *, &elements));
  charMatrix chm (elements, 0);
  int max_len = 0;
  for (int i = 0; i < elements; i++)
    {
      FOUR_BYTE_INT len;
      if (! is.read (X_CAST (char *, &len), 4))
	return false;
      if (swap)
	swap_4_bytes (X_CAST (char *, &len));
      OCTAVE_LOCAL_BUFFER (char, btmp, len+1);
      if (! is.read (X_CAST (char *, btmp), len))
	return false;
      if (len > max_len)
	{
	  max_len = len;
	  chm.resize (elements, max_len, 0);
	}
      btmp [len] = '\0';
      chm.insert (btmp, i, 0);
    }

  matrix = chm;
  return true;
}

#if defined (HAVE_HDF5)
bool
octave_char_matrix_str::save_hdf5 (hid_t loc_id, const char *name,
				   bool /* save_as_floats */)
{
  hsize_t dimens[3];
  hid_t space_hid = -1, type_hid = -1, data_hid = -1;
  bool retval = true;

  int nr = rows ();
  charMatrix chm = char_matrix_value ();
  int nc = chm.cols ();

  // create datatype for (null-terminated) string to write from:
  type_hid = H5Tcopy (H5T_C_S1); H5Tset_size (type_hid, nc + 1);
  if (type_hid < 0) return false;

  dimens[0] = nr;
  space_hid = H5Screate_simple (nr > 0 ? 1 : 0, dimens, (hsize_t*) 0);
  if (space_hid < 0)
    {
      H5Tclose (type_hid);
      return false;
    }

  data_hid = H5Dcreate (loc_id, name, type_hid, space_hid, H5P_DEFAULT);
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (char, s, nr * (nc + 1));

  for (int i = 0; i < nr; ++i)
    {
      std::string tstr = chm.row_as_string (i);
      strcpy (s + i * (nc+1), tstr.c_str ());
    }

  retval = H5Dwrite (data_hid, type_hid, H5S_ALL, H5S_ALL, H5P_DEFAULT,
		     (void*) s) >= 0;

  H5Dclose (data_hid);
  H5Tclose (type_hid);
  H5Sclose (space_hid);
  return retval;
}

bool 
octave_char_matrix_str::load_hdf5 (hid_t loc_id, const char *name,
				   bool /* have_h5giterate_bug */)
{
  hid_t data_hid = H5Dopen (loc_id, name);
  hid_t space_hid = H5Dget_space (data_hid);
  hsize_t rank = H5Sget_simple_extent_ndims (space_hid);
  hid_t type_hid = H5Dget_type (data_hid);

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
	  OCTAVE_LOCAL_BUFFER (char, s, slen);
	  // create datatype for (null-terminated) string
	  // to read into:
	  hid_t st_id = H5Tcopy (H5T_C_S1);
	  H5Tset_size (st_id, slen);
	  if (H5Dread (data_hid, st_id, H5S_ALL, H5S_ALL, 
		       H5P_DEFAULT, (void *) s) < 0)
	    {
	      H5Tclose (st_id);
	      H5Tclose (type_hid);
	      H5Sclose (space_hid);
	      H5Dclose (data_hid);
	      return false;
	    }

	  matrix = charMatrix (s);
	  
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
	  
	  OCTAVE_LOCAL_BUFFER (char, s, elements * slen);

	  // create datatype for (null-terminated) string
	  // to read into:
	  hid_t st_id = H5Tcopy (H5T_C_S1);
	  H5Tset_size (st_id, slen);

	  if (H5Dread (data_hid, st_id, H5S_ALL, H5S_ALL, 
		       H5P_DEFAULT, (void *) s) < 0)
	    {
	      H5Tclose (st_id);
	      H5Tclose (type_hid);
	      H5Sclose (space_hid);
	      H5Dclose (data_hid);
	      return false;
	    }

	  charMatrix chm (elements, slen - 1);
	  for (hsize_t i = 0; i < elements; ++i)
	    {
	      chm.insert (s + i*slen, i, 0);
	    }

	  matrix = chm;

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
#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
