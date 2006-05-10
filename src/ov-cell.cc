/*

Copyright (C) 1999 John W. Eaton

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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>
#include <sstream>
#include <vector>

#include "Array-util.h"
#include "byte-swap.h"
#include "lo-utils.h"
#include "quit.h"

#include "defun.h"
#include "error.h"
#include "ov-cell.h"
#include "oct-obj.h"
#include "unwind-prot.h"
#include "utils.h"
#include "ov-base-mat.h"
#include "ov-base-mat.cc"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "pr-output.h"
#include "ov-scalar.h"

#include "ls-oct-ascii.h"
#include "ls-oct-binary.h"
#include "ls-hdf5.h"
#include "ls-utils.h"

template class octave_base_matrix<Cell>;

DEFINE_OCTAVE_ALLOCATOR (octave_cell);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_cell, "cell", "cell");

octave_value_list
octave_cell::subsref (const std::string& type,
		      const std::list<octave_value_list>& idx, int nargout)
{
  octave_value_list retval;

  switch (type[0])
    {
    case '(':
      retval(0) = do_index_op (idx.front ());
      break;

    case '{':
      {
	octave_value tmp = do_index_op (idx.front ());

	if (! error_state)
	  {
	    Cell tcell = tmp.cell_value ();

	    if (tcell.length () == 1)
	      retval(0) = tcell(0,0);
	    else
	      {
		octave_idx_type n = tcell.numel ();

		octave_value_list lst (n, octave_value ());

		for (octave_idx_type i = 0; i < n; i++)
		  {
		    OCTAVE_QUIT;
		    lst(i) = tcell(i);
		  }

		retval(0) = octave_value (lst, true);
	      }
	  }
      }
      break;

    case '.':
      {
	std::string nm = type_name ();
	error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
      }
      break;

    default:
      panic_impossible ();
    }

  // FIXME -- perhaps there should be an
  // octave_value_list::next_subsref member function?  See also
  // octave_user_function::subsref.

  if (idx.size () > 1)
    retval = retval(0).next_subsref (nargout, type, idx);

  return retval;
}

octave_value
octave_cell::subsasgn (const std::string& type,
		       const std::list<octave_value_list>& idx,
		       const octave_value& rhs)
{
  octave_value retval;

  int n = type.length ();

  octave_value t_rhs = rhs;

  if (n > 1)
    {
      switch (type[0])
	{
	case '(':
	  {
	    octave_value tmp = do_index_op (idx.front (), true);

	    if (! tmp.is_defined ())
	      tmp = octave_value::empty_conv (type.substr (1), rhs);

	    if (! error_state)
	      {
		std::list<octave_value_list> next_idx (idx);

		next_idx.erase (next_idx.begin ());

		tmp.make_unique ();

		t_rhs = tmp.subsasgn (type.substr (1), next_idx, rhs);
	      }
	  }
	  break;

	case '{':
	  {
	    octave_value tmp = do_index_op (idx.front (), true);

	    if (! tmp.is_defined ())
	      tmp = octave_value::empty_conv (type.substr (1), rhs);

	    if (! error_state)
	      {
		Cell tcell = tmp.cell_value ();

		if (! error_state && tcell.length () == 1)
		  {
		    tmp = tcell(0,0);

		    std::list<octave_value_list> next_idx (idx);

		    next_idx.erase (next_idx.begin ());

		    tmp.make_unique ();

		    if (! tmp.is_defined () || tmp.is_empty ())
		      tmp = octave_value::empty_conv (type.substr (1), rhs);

		    if (! error_state)
		      t_rhs = tmp.subsasgn (type.substr (1), next_idx, rhs);
		  }
	      }
	  }
	  break;

	case '.':
	  {
	    std::string nm = type_name ();
	    error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
	  }
	  break;

	default:
	  panic_impossible ();
	}
    }

  if (! error_state)
    {
      switch (type[0])
	{
	case '(':
	  {
	    octave_value_list i = idx.front ();

	    if (t_rhs.is_cell ())
	      octave_base_matrix<Cell>::assign (i, t_rhs.cell_value ());
	    else
	      if (t_rhs.is_empty ())
		octave_base_matrix<Cell>::assign (i, Cell());
	      else
		octave_base_matrix<Cell>::assign (i, Cell (t_rhs));

	    count++;
	    retval = octave_value (this);
	  }
	  break;

	case '{':
	  {
	    octave_value_list i = idx.front ();

	    octave_base_matrix<Cell>::assign (i, Cell (t_rhs));

	    count++;
	    retval = octave_value (this);
	  }
	  break;

	case '.':
	  {
	    std::string nm = type_name ();
	    error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
	  }
	  break;

	default:
	  panic_impossible ();
	}
    }

  return retval;
}

void
octave_cell::assign (const octave_value_list& idx, const octave_value& rhs)
{
  if (rhs.is_cell ())
    octave_base_matrix<Cell>::assign (idx, rhs.cell_value ());
  else
    octave_base_matrix<Cell>::assign (idx, Cell (rhs));
}

size_t
octave_cell::byte_size (void) const
{
  size_t retval = 0;

  for (octave_idx_type i = 0; i < numel (); i++)
    retval += matrix(i).byte_size ();

  return retval;
}

octave_value_list
octave_cell::list_value (void) const
{
  octave_value_list retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = columns ();

  if (nr == 1 && nc > 0)
    {
      retval.resize (nc);

      for (octave_idx_type i = 0; i < nc; i++)
	retval(i) = matrix(0,i);
    }
  else if (nc == 1 && nr > 0)
    {
      retval.resize (nr);

      for (octave_idx_type i = 0; i < nr; i++)
	retval(i) = matrix(i,0);
    }
  else
    error ("invalid conversion from cell array to list");

  return retval;
}

string_vector
octave_cell::all_strings (bool pad) const
{
  string_vector retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = columns ();

  int n_elts = 0;

  octave_idx_type max_len = 0;

  for (octave_idx_type j = 0; j < nc; j++)
    {
      for (octave_idx_type i = 0; i < nr; i++)
	{
	  string_vector s = matrix(i,j).all_strings ();

	  if (error_state)
	    return retval;

	  n_elts += s.length ();

	  octave_idx_type s_max_len = s.max_length ();

	  if (s_max_len > max_len)
	    max_len = s_max_len;
	}
    }

  retval.resize (n_elts);

  octave_idx_type k = 0;

  for (octave_idx_type j = 0; j < nc; j++)
    {
      for (octave_idx_type i = 0; i < nr; i++)
	{
	  string_vector s = matrix(i,j).all_strings ();

	  int n = s.length ();

	  for (octave_idx_type ii = 0; ii < n; ii++)
	    {
	      std::string t = s[ii];
	      int t_len = t.length ();

	      if (pad && max_len > t_len)
		t += std::string (max_len - t_len, ' ');

	      retval[k++] = t;
	    }
	}
    }

  return retval;
}

bool
octave_cell::print_as_scalar (void) const
{
  return (ndims () > 2 || numel () == 0);
}

void
octave_cell::print (std::ostream& os, bool) const
{
  print_raw (os);
}

void
octave_cell::print_raw (std::ostream& os, bool) const
{
  int nd = matrix.ndims ();

  if (nd == 2)
    {
      octave_idx_type nr = rows ();
      octave_idx_type nc = columns ();

      if (nr > 0 && nc > 0)
	{
	  indent (os);
	  os << "{";
	  newline (os);

	  increment_indent_level ();

	  for (octave_idx_type j = 0; j < nc; j++)
	    {
	      for (octave_idx_type i = 0; i < nr; i++)
		{
		  OCTAVE_QUIT;

		  std::ostringstream buf;
		  buf << "[" << i+1 << "," << j+1 << "]";

		  octave_value val = matrix(i,j);

		  val.print_with_name (os, buf.str ());
		}
	    }

	  decrement_indent_level ();

	  indent (os);
	  os << "}";
	  newline (os);
	}
      else
	{
	  os << "{}";
	  if (Vprint_empty_dimensions)
	    os << "(" << nr << "x" << nc << ")";
	  os << "\n";
	}
    }
  else
    {
      indent (os);
      dim_vector dv = matrix.dims ();
      os << "{" << dv.str () << " Cell Array}";
      newline (os);
    }
}

#define CELL_ELT_TAG "<cell-element>"

bool 
octave_cell::save_ascii (std::ostream& os, bool& infnan_warned, 
			       bool strip_nan_and_inf)
{
  dim_vector d = dims ();
  if (d.length () > 2)
    {
      os << "# ndims: " << d.length () << "\n";
      
      for (int i = 0; i < d.length (); i++)
	os << " " << d (i);
      os << "\n";

      Cell tmp = cell_value ();
      
      for (octave_idx_type i = 0; i < d.numel (); i++)
	{
	  octave_value o_val = tmp.elem (i);

	  // Recurse to print sub-value.
	  bool b = save_ascii_data (os, o_val, CELL_ELT_TAG, infnan_warned, 
				    strip_nan_and_inf, 0, 0);
	      
	  if (! b)
	    return os;
	}
    }
  else
    {
      // Keep this case, rather than use generic code above for backward 
      // compatiability. Makes load_ascii much more complex!!
      os << "# rows: " << rows () << "\n"
	 << "# columns: " << columns () << "\n";

      Cell tmp = cell_value ();
      
      for (octave_idx_type j = 0; j < tmp.cols (); j++)
	{
	  for (octave_idx_type i = 0; i < tmp.rows (); i++)
	    {
	      octave_value o_val = tmp.elem (i, j);

	      // Recurse to print sub-value.
	      bool b = save_ascii_data (os, o_val, CELL_ELT_TAG, 
					infnan_warned, 
					strip_nan_and_inf, 0, 0);
	      
	      if (! b)
		return os;
	    }
	  
	  os << "\n";
	}
    }

  return true;
}

bool 
octave_cell::load_ascii (std::istream& is)
{
  bool success = true;

  string_vector keywords(2);

  keywords[0] = "ndims";
  keywords[1] = "rows";

  std::string kw;
  octave_idx_type val = 0;

  if (extract_keyword (is, keywords, kw, val, true))
    {
      if (kw == "ndims")
	{
	  int mdims = static_cast<int> (val);

	  if (mdims >= 0)
	    {
	      dim_vector dv;
	      dv.resize (mdims);

	      for (int i = 0; i < mdims; i++)
		is >> dv(i);

	      Cell tmp(dv);

	      for (octave_idx_type i = 0; i < dv.numel (); i++)
		{
		  octave_value t2;
		  bool dummy;

		  // recurse to read cell elements
		  std::string nm = read_ascii_data (is, std::string (), 
						    dummy, t2, i);

		  if (nm == CELL_ELT_TAG)
		    {
		      if (is)
			tmp.elem (i) = t2;
		    }
		  else
		    {
		      error ("load: cell array element had unexpected name");
		      success = false;
		      break;
		    }
		}

	      if (is)
		matrix = tmp;
	      else
		{
		  error ("load: failed to load matrix constant");
		  success = false;
		}
	    }
	  else
	    {
	      error ("load: failed to extract number of rows and columns");
	      success = false;
	    }
	}
      else if (kw == "rows")
	{
	  octave_idx_type nr = val;
	  octave_idx_type nc = 0;

	  if (nr >= 0 && extract_keyword (is, "columns", nc) && nc >= 0)
	    {
	      if (nr > 0 && nc > 0)
		{
		  Cell tmp (nr, nc);

		  for (octave_idx_type j = 0; j < nc; j++)
		    {
		      for (octave_idx_type i = 0; i < nr; i++)
			{
			  octave_value t2;
			  bool dummy;

			  // recurse to read cell elements
			  std::string nm = read_ascii_data (is, std::string (),
							    dummy, t2, i);

			  if (nm == CELL_ELT_TAG)
			    {
			      if (is)
				tmp.elem (i, j) = t2;
			    }
			  else
			    {
			      error ("load: cell array element had unexpected name");
			      success = false;
			      goto cell_read_error;
			    }
			}
		    }
	      
		cell_read_error:

		  if (is)
		    matrix = tmp;
		  else
		    {
		      error ("load: failed to load cell element");
		      success = false;
		    }
		}
	      else if (nr == 0 || nc == 0)
		matrix = Cell (nr, nc);
	      else
		panic_impossible ();
	    }
	  else
	    {
	      error ("load: failed to extract number of rows and columns for cell array");
	      success = false;
	    }
	}
      else
	panic_impossible ();
    }
  else
    {
      error ("load: failed to extract number of rows and columns");
      success = false;
    }

  return success;
}

bool 
octave_cell::save_binary (std::ostream& os, bool& save_as_floats)
{
  dim_vector d = dims ();
  if (d.length () < 1)
    return false;

  // Use negative value for ndims
  FOUR_BYTE_INT di = - d.length();
  os.write (reinterpret_cast<char *> (&di), 4);
  for (int i = 0; i < d.length (); i++)
    {
      di = d(i);
      os.write (reinterpret_cast<char *> (&di), 4);
    }
  
  Cell tmp = cell_value ();
      
  for (octave_idx_type i = 0; i < d.numel (); i++)
    {
      octave_value o_val = tmp.elem (i);

      // Recurse to print sub-value.
      bool b = save_binary_data (os, o_val, CELL_ELT_TAG, "", 0, 
				 save_as_floats);
	      
      if (! b)
	return false;
    }
  
  return true;
}

bool 
octave_cell::load_binary (std::istream& is, bool swap,
				 oct_mach_info::float_format fmt)
{
  bool success = true;
  FOUR_BYTE_INT mdims;
  if (! is.read (reinterpret_cast<char *> (&mdims), 4))
    return false;
  if (swap)
    swap_bytes<4> (&mdims);
  if (mdims >= 0)
    return false;

  mdims = -mdims;
  FOUR_BYTE_INT di;
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

  octave_idx_type nel = dv.numel ();
  Cell tmp(dv);

  for (octave_idx_type i = 0; i < nel; i++)
    {
      octave_value t2;
      bool dummy;
      std::string doc;

      // recurse to read cell elements
      std::string nm = read_binary_data (is, swap, fmt, std::string (), 
					 dummy, t2, doc);

      if (nm == CELL_ELT_TAG)
	{
	  if (is)
	    tmp.elem (i) = t2;
	}
      else
	{
	  error ("load: cell array element had unexpected name");
	  success = false;
	  break;
	}
    }

  if (is)
    matrix = tmp;
  else
    {
      error ("load: failed to load matrix constant");
      success = false;
    }

  return success;
}

#if defined (HAVE_HDF5)

bool
octave_cell::save_hdf5 (hid_t loc_id, const char *name, bool save_as_floats)
{
  dim_vector dv = dims ();
  int empty = save_hdf5_empty (loc_id, name, dv);
  if (empty)
    return (empty > 0);

  hsize_t rank = dv.length (); 
  hid_t space_hid = -1, data_hid = -1, size_hid = -1;

  data_hid = H5Gcreate (loc_id, name, 0);

  if (data_hid < 0)
    return false;

  // Have to save cell array shape, since can't have a 
  // dataset of groups....

  space_hid = H5Screate_simple (1, &rank, 0);

  if (space_hid < 0) 
    {
      H5Gclose (data_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (octave_idx_type, hdims, rank);

  // Octave uses column-major, while HDF5 uses row-major ordering
  for (hsize_t i = 0; i < rank; i++)
    hdims[i] = dv(rank-i-1);

  size_hid = H5Dcreate (data_hid, "dims", H5T_NATIVE_IDX, space_hid, 
			H5P_DEFAULT);
  if (size_hid < 0) 
    {
      H5Sclose (space_hid);
      H5Gclose (data_hid);
      return false;
    }

  if (! H5Dwrite (size_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL,
		  H5P_DEFAULT, hdims) < 0)
    {
      H5Dclose (size_hid);
      H5Sclose (space_hid);
      H5Gclose (data_hid);
      return false;
    }

  H5Dclose (size_hid);
  H5Sclose (space_hid);

  // Recursively add each element of the cell to this group.

  Cell tmp = cell_value ();
  
  for (octave_idx_type i = 0; i < dv.numel (); i++)
    {
      std::ostringstream buf;
      buf << "_" << i;
      std::string s = buf.str ();

      if (! add_hdf5_data(data_hid, tmp.elem (i), s.c_str (), "", false,
			  save_as_floats))
	{
	  H5Gclose (data_hid);
	  return false;
	}
    }

  H5Gclose (data_hid);

  return true;
}

bool
octave_cell::load_hdf5 (hid_t loc_id, const char *name,
			bool have_h5giterate_bug)
{
  bool retval = false;

  dim_vector dv;
  int empty = load_hdf5_empty (loc_id, name, dv);
  if (empty > 0)
    matrix.resize(dv);
  if (empty)
    return (empty > 0);

  hid_t group_id = H5Gopen (loc_id, name);

  if (group_id < 0)
    return false;

  hid_t data_hid = H5Dopen (group_id, "dims");
  hid_t space_hid = H5Dget_space (data_hid);
  hsize_t rank = H5Sget_simple_extent_ndims (space_hid);
  if (rank != 1) 
    {
      H5Dclose (data_hid);
      H5Gclose (group_id);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (hsize_t, hdims, rank);
  OCTAVE_LOCAL_BUFFER (hsize_t, maxdims, rank);

  H5Sget_simple_extent_dims (space_hid, hdims, maxdims);

  // Octave uses column-major, while HDF5 uses row-major ordering.

  dv.resize (hdims[0]);

  OCTAVE_LOCAL_BUFFER (octave_idx_type, tmp, hdims[0]);
  
  if (H5Dread (data_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL, 
	       H5P_DEFAULT, tmp) < 0)
    {
      H5Dclose (data_hid);
      H5Gclose (group_id);
      return false;
    }

  H5Dclose (data_hid);
  H5Gclose (group_id);

  for (hsize_t i = 0, j = hdims[0] - 1; i < hdims[0]; i++, j--)
    dv(j) = tmp[i];

  hdf5_callback_data dsub;

  herr_t retval2 = -1;

  Cell m (dv);

  int current_item = 0;

  if (have_h5giterate_bug)
    current_item = 1;   // Skip dims items in group.

#ifdef HAVE_H5GGET_NUM_OBJS
  hsize_t num_obj = 0;
  group_id = H5Gopen (loc_id, name); 
  H5Gget_num_objs (group_id, &num_obj);
  H5Gclose (group_id);
#endif

  for (octave_idx_type i = 0; i < dv.numel (); i++)
    {

#ifdef HAVE_H5GGET_NUM_OBJS
      if (current_item >= static_cast<int> (num_obj))
	retval2 = -1;
      else
#endif
	retval2 = H5Giterate (loc_id, name, &current_item,
			      hdf5_read_next_data, &dsub);
      
      if (retval2 <= 0)
	break;

      octave_value ov = dsub.tc;
      m.elem (i) = ov;

      if (have_h5giterate_bug)
	current_item++;  // H5Giterate returned the last index processed.

    }

  if (retval2 >= 0)
    {
      matrix = m;
      retval = true;
    }
  
  return retval;
}

#endif

DEFUN (iscell, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} iscell (@var{x})\n\
Return true if @var{x} is a cell array object.  Otherwise, return\n\
false.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_cell ();
  else
    print_usage ("iscell");

  return retval;
}

DEFUN (cell, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} cell (@var{x})\n\
@deftypefnx {Built-in Function} {} cell (@var{n}, @var{m})\n\
Create a new cell array object.  If invoked with a single scalar\n\
argument, @code{cell} returns a square cell array with the dimension\n\
specified.  If you supply two scalar arguments, @code{cell} takes\n\
them to be the number of rows and columns.  If given a vector with two\n\
elements, @code{cell} uses the values of the elements as the number of\n\
rows and columns, respectively.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  dim_vector dims;

  switch (nargin)
    {
    case 0:
      dims = dim_vector (0, 0);
      break;

    case 1:
      get_dimensions (args(0), "cell", dims);
      break;

    default:
      {
	dims.resize (nargin);

	for (int i = 0; i < nargin; i++)
	  {
	    dims(i) = args(i).is_empty () ? 0 : args(i).nint_value ();

	    if (error_state)
	      {
		error ("cell: expecting scalar arguments");
		break;
	      }
	  }
      }
      break;
    }

  if (! error_state)
    {
      int ndim = dims.length ();

      check_dimensions (dims, "cell");

      if (! error_state)
	{
	  switch (ndim)
	    {
	    case 1:
	      retval = Cell (dims(0), dims(0), Matrix ());
	      break;

	    default:
	      retval = Cell (dims, Matrix ());
	      break;
	    }
	}
    }

  return retval;
}

DEFUN (iscellstr, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} iscellstr (@var{cell})\n\
Return true if every element of the cell array @var{cell} is a\n\
character string\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      retval = true;

      octave_value arg = args (0);

      if (arg.is_cell ())
	{
	  Cell c = args(0).cell_value ();

	  if (! error_state)
	    {
	      for (int i = 0; i < c.length (); i++)
		{
		  if (! c(i).is_string ())
		    {
		      retval = false;
		      break;
		    }
		}
	    }
	  else
	    retval = false;
	}
      else
	retval = false;
    }
  else
    print_usage ("iscellstr");

  return retval;
}

// Note that since Fcellstr calls Fiscellstr, we need to have
// Fiscellstr defined first (to provide a declaration) and also we
// should keep it in the same file (so we don't have to provide a
// declaration) and so we don't have to use feval to call it.

DEFUN (cellstr, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} cellstr (@var{string})\n\
Create a new cell array object from the elements of the string\n\
array @var{string}.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      octave_value_list tmp = Fiscellstr (args, 1);

      if (tmp(0).is_true ())
	retval = args(0);
      else
	{
	  string_vector s = args(0).all_strings ();

	  if (! error_state)
	    retval = Cell (s, true);
	  else
	    error ("cellstr: expecting argument to be a 2-d character array");
	}
    }
  else
    print_usage ("cellstr");

  return retval;
}

DEFUN (struct2cell, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} struct2cell (@var{S})\n\
Create a new cell array from the objects stored in the struct object.\n\
If @var{f} is the number of fields in the structure, the resulting\n\
cell array will have a dimension vector corresponding to\n\
@code{[@var{F} size(@var{S})]}.\n\
@seealso{cell2struct, fieldnames}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      Octave_map m = args(0).map_value ();

      if (! error_state)
	{
	  dim_vector m_dv = m.dims ();

	  string_vector keys = m.keys ();

	  octave_idx_type fields_numel = keys.length ();

	  // The resulting dim_vector should have dimensions:
	  // [numel(fields) size(struct)]

	  dim_vector result_dv;
	  result_dv.resize (m_dv.length () + 1); // Add 1 for the fields.

	  result_dv(0) = fields_numel;

	  for (int i = 1; i < result_dv.length (); i++)
	    result_dv(i) = m_dv(i-1);

	  // Squeeze to be sure that a (3,1) vector doesn't get turned
	  // into a (3,3,1) vector.

	  result_dv = result_dv.squeeze ();

	  Cell c (result_dv);

	  // Use ra_idx both for counting and for assignments, so
	  // ra_idx(0) will both contain fields_numel for each call to
	  // increment_index and j for each assignment.

	  Array<octave_idx_type> ra_idx (result_dv.length (), 0);
	  ra_idx(0) = fields_numel;

	  for (octave_idx_type i = 0; i < m_dv.numel (); i++)
	    {
	      for (octave_idx_type j = 0; j < fields_numel; j++)
		{
		  ra_idx(0) = j;

		  Cell c_tmp = m.contents (keys(j));

		  if (c_tmp.length () > 1) // Is a cell.
		    c(ra_idx) = c_tmp;
		  else if (c_tmp.length () == 1) // Get octave_value.
		    c(ra_idx) = c_tmp(0);
		  else
		    c(ra_idx) = Cell ();

		  ra_idx(0) = fields_numel;
		}

	      increment_index (ra_idx, result_dv);
	    }

	  retval = c;
	}
      else
	error ("struct2cell: expecting argument to be a cell array");
    }
  else
    print_usage ("struct2cell");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
