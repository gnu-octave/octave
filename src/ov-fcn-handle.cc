/*

Copyright (C) 2003, 2004, 2005, 2006, 2007 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>
#include <sstream>
#include <vector>

#include "file-ops.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "input.h"
#include "oct-map.h"
#include "ov-base.h"
#include "ov-fcn-handle.h"
#include "ov-usr-fcn.h"
#include "pr-output.h"
#include "pt-pr-code.h"
#include "pt-misc.h"
#include "pt-stmt.h"
#include "pt-cmd.h"
#include "pt-exp.h"
#include "pt-assign.h"
#include "variables.h"
#include "parse.h"
#include "unwind-prot.h"
#include "defaults.h"
#include "file-stat.h"
#include "load-path.h"
#include "oct-env.h"

#include "byte-swap.h"
#include "ls-oct-ascii.h"
#include "ls-oct-binary.h"
#include "ls-hdf5.h"
#include "ls-utils.h"

DEFINE_OCTAVE_ALLOCATOR (octave_fcn_handle);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_fcn_handle,
				     "function handle",
				     "function_handle");

octave_value_list
octave_fcn_handle::subsref (const std::string& type,
			    const std::list<octave_value_list>& idx,
			    int nargout)
{
  octave_value_list retval;

  switch (type[0])
    {
    case '(':
      {
	out_of_date_check (fcn);

	if (fcn.is_defined ())
	  {
	    octave_function *f = function_value ();

	    if (f)
	      retval = f->subsref (type, idx, nargout);
	    else
	      error ("invalid function handle");
	  }
	else
	  error ("invalid function handle");
      }
      break;

    case '{':
    case '.':
      {
	std::string typ_nm = type_name ();
	error ("%s cannot be indexed with %c", typ_nm.c_str (), type[0]);
      }
      break;

    default:
      panic_impossible ();
    }

  // FIXME -- perhaps there should be an
  // octave_value_list::next_subsref member function?  See also
  // octave_builtin::subsref.

  if (idx.size () > 1)
    retval = retval(0).next_subsref (nargout, type, idx);

  return retval;
}

bool
octave_fcn_handle::set_fcn (const std::string &octaveroot, 
			    const std::string& fpath)
{
  bool success = true;

  if (octaveroot.length () != 0 && 
      fpath.length () >= octaveroot.length () &&
      fpath.substr (0, octaveroot.length ()) == octaveroot &&
      OCTAVE_EXEC_PREFIX != octaveroot)
    {
      // First check if just replacing matlabroot is enough
      std::string str = OCTAVE_EXEC_PREFIX + 
	fpath.substr (octaveroot.length ());		    
      file_stat fs (str);

      if (fs.exists ())
	{
	  size_t xpos = str.find_last_of (file_ops::dir_sep_chars);

	  std::string dir_name = str.substr (0, xpos);

	  octave_function *xfcn
	    = load_fcn_from_file (str, dir_name, "", nm);

	  if (xfcn)
	    {
	      octave_value tmp (xfcn);

	      fcn = octave_value (new octave_fcn_handle (tmp, nm));
	    }
	  else
	    {
	      error ("function handle points to non-existent function");
	      success = false;
	    }
	}
      else
	{
	  // Next just search for it anywhere in the system path
	  string_vector names(3);
	  names(0) = nm + ".oct";
	  names(1) = nm + ".mex";
	  names(2) = nm + ".m";

	  dir_path p (load_path::system_path ());

	  str = octave_env::make_absolute 
	    (p.find_first_of (names), octave_env::getcwd ());

	  size_t xpos = str.find_last_of (file_ops::dir_sep_chars);

	  std::string dir_name = str.substr (0, xpos);

	  octave_function *xfcn = load_fcn_from_file (str, dir_name, "", nm);

	  if (xfcn)
	    {
	      octave_value tmp (xfcn);

	      fcn = octave_value (new octave_fcn_handle (tmp, nm));
	    }
	  else
	    {
	      error ("function handle points to non-existent function");
	      success = false;
	    }
	}
    }
  else
    {
      if (fpath.length () > 0)
	{
	  size_t xpos = fpath.find_last_of (file_ops::dir_sep_chars);

	  std::string dir_name = fpath.substr (0, xpos);

	  octave_function *xfcn = load_fcn_from_file (fpath, dir_name, "", nm);

	  if (xfcn)
	    {
	      octave_value tmp (xfcn);

	      fcn = octave_value (new octave_fcn_handle (tmp, nm));
	    }
	  else
	    {
	      error ("function handle points to non-existent function");
	      success = false;
	    }
	}
      else
	{
	  fcn = symbol_table::find_function (nm);

	  if (! fcn.is_function ())
	    {
	      error ("function handle points to non-existent function");
	      success = false;
	    }
	}
    }

  return success;
}

bool
octave_fcn_handle::save_ascii (std::ostream& os)
{
  if (nm == "@<anonymous>")
    {
      os << nm << "\n";

      print_raw (os, true);
      os << "\n";

      if (fcn.is_undefined ())
	return false;

      octave_user_function *f = fcn.user_function_value ();

      std::list<symbol_table::symbol_record> vars
	= symbol_table::all_variables (f->scope ());

      size_t varlen = vars.size ();

      if (varlen > 0)
	{
	  os << "# length: " << varlen << "\n";

	  for (std::list<symbol_table::symbol_record>::const_iterator p = vars.begin ();
	       p != vars.end (); p++)
	    {
	      if (! save_ascii_data (os, p->varval (), p->name (), false, 0))
		return os;
	    }
	}
    }
  else
    {
      os << "# octaveroot: " << OCTAVE_EXEC_PREFIX << "\n";
      os << "# path: " << user_function_value ()-> fcn_file_name () << "\n";
      os << nm << "\n";
    }

  return true;
}

bool
octave_fcn_handle::load_ascii (std::istream& is)
{
  bool success = true;

  std::streampos pos = is.tellg ();
  std::string octaveroot = extract_keyword (is, "octaveroot", true);
  if (octaveroot.length() == 0)
    {
      is.seekg (pos);
      is.clear ();
    }
  pos = is.tellg ();
  std::string fpath = extract_keyword (is, "path", true);
  if (fpath.length() == 0)
    {
      is.seekg (pos);
      is.clear ();
    }

  is >> nm;

  if (nm == "@<anonymous>")
    {
      octave_idx_type len = 0;
      char c;
      std::ostringstream buf;

      // Skip preceeding newline(s).
      while (is.get (c) && c == '\n')
	/* do nothing */;

      if (is)
	{
	  buf << c;

	  // Get a line of text whitespace characters included, leaving
	  // newline in the stream.

	  while (is.peek () != '\n')
	    {
	      is.get (c);
	      if (! is)
		break;
	      buf << c;
	    }
	}

      pos = is.tellg ();

      symbol_table::scope_id local_scope = symbol_table::alloc_scope ();

      if (extract_keyword (is, "length", len, true) && len >= 0)
	{
	  if (len > 0)
	    {
	      for (octave_idx_type i = 0; i < len; i++)
		{
		  octave_value t2;
		  bool dummy;

		  std::string name
		    = read_ascii_data (is, std::string (), dummy, t2, i);

		  if (!is)
		    {
		      error ("load: failed to load anonymous function handle");
		      break;
		    }

		  symbol_table::varref (name, local_scope) = t2;
		}
	    }
	}
      else
	{
	  is.seekg (pos);
	  is.clear ();
	}

      if (is && success)
	{
	  unwind_protect::begin_frame ("anon_ascii_load");

	  symbol_table::push_scope (local_scope);

	  unwind_protect::add (symbol_table::pop_scope);

	  int parse_status;
	  octave_value anon_fcn_handle = 
	    eval_string (buf.str (), true, parse_status);

	  if (parse_status == 0)
	    {
	      octave_fcn_handle *fh = 
		anon_fcn_handle.fcn_handle_value ();
	      if (fh)
		fcn = fh->fcn;
	      else
		success = false;
	    }
	  else
	    success = false;

	  unwind_protect::run_frame ("anon_ascii_load");
	}
      else
	success = false;

      symbol_table::erase_scope (local_scope);
    }
  else
    success = set_fcn (octaveroot, fpath);

  return success;
}

/* 

%!test
%! a = 2;
%! f = @(x) a + x;
%! g = @(x) 2 * x;
%! h = @log2;
%! f2 = f;
%! g2 = g;
%! h2 = h;
%! nm = tmpnam();
%! unwind_protect
%!   save ("-text", nm, "f2", "g2", "h2");
%!   clear f2 g2 h2
%!   load (nm);
%!   assert (f(2),f2(2));
%!   assert (g(2),g2(2));
%!   assert (g(3),g2(3));
%!   unlink (nm);
%!   save ("-text", nm, "f2", "g2", "h2");
%! unwind_protect_cleanup
%!   unlink (nm);
%! end_unwind_protect

*/

bool
octave_fcn_handle::save_binary (std::ostream& os, bool& save_as_floats)
{
  if (nm == "@<anonymous>")
    {
      std::ostringstream nmbuf;

      if (fcn.is_undefined ())
	return false;

      octave_user_function *f = fcn.user_function_value ();

      std::list<symbol_table::symbol_record> vars
	= symbol_table::all_variables (f->scope ());

      size_t varlen = vars.size ();

      if (varlen > 0)
	nmbuf << nm << " " << varlen;
      else
	nmbuf << nm;

      std::string buf_str = nmbuf.str();
      int32_t tmp = buf_str.length ();
      os.write (reinterpret_cast<char *> (&tmp), 4);
      os.write (buf_str.c_str (), buf_str.length ());

      std::ostringstream buf;
      print_raw (buf, true);
      std::string stmp = buf.str ();
      tmp = stmp.length ();
      os.write (reinterpret_cast<char *> (&tmp), 4);
      os.write (stmp.c_str (), stmp.length ());

      if (varlen > 0)
	{
	  for (std::list<symbol_table::symbol_record>::const_iterator p = vars.begin ();
	       p != vars.end (); p++)
	    {
	      if (! save_binary_data (os, p->varval (), p->name (),
				      "", 0, save_as_floats))
		return os;
	    }
	}
    }
  else
    {
      std::ostringstream nmbuf;

      nmbuf << nm << "\n" << OCTAVE_EXEC_PREFIX << "\n" 
	    << user_function_value ()-> fcn_file_name () ;

      std::string buf_str = nmbuf.str ();
      int32_t tmp = buf_str.length ();
      os.write (reinterpret_cast<char *> (&tmp), 4);
      os.write (buf_str.c_str (), buf_str.length ());
    }

  return true;
}

bool
octave_fcn_handle::load_binary (std::istream& is, bool swap,
				oct_mach_info::float_format fmt)
{
  bool success = true;

  int32_t tmp;
  if (! is.read (reinterpret_cast<char *> (&tmp), 4))
    return false;
  if (swap)
    swap_bytes<4> (&tmp);

  OCTAVE_LOCAL_BUFFER (char, ctmp1, tmp+1);
  is.read (ctmp1, tmp);
  nm = std::string (ctmp1);

  if (! is)
    return false;

  if (nm.length() >= 12 && nm.substr (0, 12) == "@<anonymous>")
    {
      octave_idx_type len = 0;

      if (nm.length() > 12)
	{
	  std::istringstream nm_is (nm.substr(12));
	  nm_is >> len;
	  nm = nm.substr(0,12);
	}

      if (! is.read (reinterpret_cast<char *> (&tmp), 4))
	return false;
      if (swap)
	swap_bytes<4> (&tmp);

      OCTAVE_LOCAL_BUFFER (char, ctmp2, tmp+1);
      is.read (ctmp2, tmp);

      symbol_table::scope_id local_scope = symbol_table::alloc_scope ();
	      
      if (len > 0)
	{
	  for (octave_idx_type i = 0; i < len; i++)
	    {
	      octave_value t2;
	      bool dummy;
	      std::string doc;

	      std::string name = 
		read_binary_data (is, swap, fmt, std::string (), 
				  dummy, t2, doc);

	      if (!is)
		{
		  error ("load: failed to load anonymous function handle");
		  break;
		}

	      symbol_table::varref (name, local_scope) = t2;
	    }
	}

      if (is && success)
	{
	  unwind_protect::begin_frame ("anon_binary_load");

	  symbol_table::push_scope (local_scope);

	  unwind_protect::add (symbol_table::pop_scope);

	  int parse_status;
	  octave_value anon_fcn_handle = 
	    eval_string (ctmp2, true, parse_status);

	  if (parse_status == 0)
	    {
	      octave_fcn_handle *fh = anon_fcn_handle.fcn_handle_value ();
	      if (fh)
		fcn = fh->fcn;
	      else
		success = false;
	    }
	  else
	    success = false;

	  unwind_protect::run_frame ("anon_binary_load");
	}

      symbol_table::erase_scope (local_scope);
    }
  else
    {
      std::string octaveroot;
      std::string fpath;

      if (nm.find_first_of ("\n") != NPOS)
	{
	  size_t pos1 = nm.find_first_of ("\n");
	  size_t pos2 = nm.find_first_of ("\n", pos1 + 1);
	  octaveroot = nm.substr (pos1 + 1, pos2 - pos1 - 1);
	  fpath = nm.substr (pos2 + 1);
	  nm = nm.substr (0, pos1);
	}

      success = set_fcn (octaveroot, fpath);
     }
 
 return success;
}

/* 

%!test
%! a = 2;
%! f = @(x) a + x;
%! g = @(x) 2 * x;
%! h = @log2;
%! f2 = f;
%! g2 = g;
%! h2 = h;
%! nm = tmpnam();
%! unwind_protect
%!   save ("-binary", nm, "f2", "g2", "h2");
%!   clear f2 g2 h2
%!   load (nm);
%!   assert (f(2),f2(2));
%!   assert (g(2),g2(2));
%!   assert (g(3),g2(3));
%!   unlink (nm);
%!   save ("-binary", nm, "f2", "g2", "h2");
%! unwind_protect_cleanup
%!   unlink (nm);
%! end_unwind_protect

*/

#if defined (HAVE_HDF5)
bool
octave_fcn_handle::save_hdf5 (hid_t loc_id, const char *name,
			      bool save_as_floats)
{
  bool retval = true;

  hid_t group_hid = -1;
  group_hid = H5Gcreate (loc_id, name, 0);
  if (group_hid < 0)
    return false;

  hid_t space_hid = -1, data_hid = -1, type_hid = -1;;

  // attach the type of the variable
  type_hid = H5Tcopy (H5T_C_S1);
  H5Tset_size (type_hid, nm.length () + 1);
  if (type_hid < 0)
    {
      H5Gclose (group_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (hsize_t, hdims, 2);
  hdims[0] = 0;
  hdims[1] = 0;
  space_hid = H5Screate_simple (0 , hdims, 0);
  if (space_hid < 0)
    {
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return false;
    }

  data_hid = H5Dcreate (group_hid, "nm",  type_hid, space_hid, H5P_DEFAULT);
  if (data_hid < 0 || H5Dwrite (data_hid, type_hid, H5S_ALL, H5S_ALL,
				H5P_DEFAULT, nm.c_str ()) < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return false;
    }
  H5Dclose (data_hid);

  if (nm == "@<anonymous>")
    {
      std::ostringstream buf;
      print_raw (buf, true);
      std::string stmp = buf.str ();

      // attach the type of the variable
      H5Tset_size (type_hid, stmp.length () + 1);
      if (type_hid < 0)
	{
	  H5Sclose (space_hid);
	  H5Gclose (group_hid);
	  return false;
	}

      data_hid = H5Dcreate (group_hid, "fcn",  type_hid, space_hid,
			    H5P_DEFAULT);
      if (data_hid < 0 || H5Dwrite (data_hid, type_hid, H5S_ALL, H5S_ALL,
				    H5P_DEFAULT, stmp.c_str ()) < 0)
	{
	  H5Sclose (space_hid);
	  H5Tclose (type_hid);
	  H5Gclose (group_hid);
	  return false;
	}

      H5Dclose (data_hid);

      octave_user_function *f = fcn.user_function_value ();

      std::list<symbol_table::symbol_record> vars
	= symbol_table::all_variables (f->scope ());

      size_t varlen = vars.size ();

      if (varlen > 0)
	{
	  hid_t as_id = H5Screate (H5S_SCALAR);

	  if (as_id >= 0)
	    {
	      hid_t a_id = H5Acreate (group_hid, "SYMBOL_TABLE",
				      H5T_NATIVE_IDX, as_id, H5P_DEFAULT);

	      if (a_id >= 0)
		{
		  retval = (H5Awrite (a_id, H5T_NATIVE_IDX, &varlen) >= 0);

		  H5Aclose (a_id);
		}
	      else
		retval = false;

	      H5Sclose (as_id);
	    }
	  else
	    retval = false;

	  data_hid = H5Gcreate (group_hid, "symbol table", 0);
	  if (data_hid < 0) 
	    {
	      H5Sclose (space_hid);
	      H5Tclose (type_hid);
	      H5Gclose (group_hid);
	      return false;
	    }

	  for (std::list<symbol_table::symbol_record>::const_iterator p = vars.begin ();
	       p != vars.end (); p++)
	    {
	      if (! add_hdf5_data (data_hid, p->varval (), p->name (),
				   "", false, save_as_floats))
		break;
	    }
	  H5Gclose (data_hid);
	}
    }
  else
    {
      std::string octaveroot = OCTAVE_EXEC_PREFIX;
      std::string fpath = user_function_value ()-> fcn_file_name ();

      H5Sclose (space_hid);
      hdims[0] = 1;
      hdims[1] = octaveroot.length ();
      space_hid = H5Screate_simple (0 , hdims, 0);
      if (space_hid < 0)
	{
	  H5Tclose (type_hid);
	  H5Gclose (group_hid);
	  return false;
	}

      H5Tclose (type_hid);
      type_hid = H5Tcopy (H5T_C_S1);
      H5Tset_size (type_hid, octaveroot.length () + 1);

      hid_t a_id = H5Acreate (group_hid, "OCTAVEROOT",
			      type_hid, space_hid, H5P_DEFAULT);

      if (a_id >= 0)
	{
	  retval = (H5Awrite (a_id, type_hid, octaveroot.c_str ()) >= 0);

	  H5Aclose (a_id);
	}
      else
	{
	  H5Sclose (space_hid);
	  H5Tclose (type_hid);
	  H5Gclose (group_hid);
	  return false;
	}

      H5Sclose (space_hid);
      hdims[0] = 1;
      hdims[1] = fpath.length ();
      space_hid = H5Screate_simple (0 , hdims, 0);
      if (space_hid < 0)
	{
	  H5Tclose (type_hid);
	  H5Gclose (group_hid);
	  return false;
	}

      H5Tclose (type_hid);
      type_hid = H5Tcopy (H5T_C_S1);
      H5Tset_size (type_hid, fpath.length () + 1);

      a_id = H5Acreate (group_hid, "FILE", type_hid, space_hid, H5P_DEFAULT);

      if (a_id >= 0)
	{
	  retval = (H5Awrite (a_id, type_hid, fpath.c_str ()) >= 0);

	  H5Aclose (a_id);
	}
      else
	retval = false;
    }

  H5Sclose (space_hid);
  H5Tclose (type_hid);
  H5Gclose (group_hid);

  return retval;
}

bool
octave_fcn_handle::load_hdf5 (hid_t loc_id, const char *name,
			      bool have_h5giterate_bug)
{
  bool success = true;

  hid_t group_hid, data_hid, space_hid, type_hid, type_class_hid, st_id;
  hsize_t rank;
  int slen;

  group_hid = H5Gopen (loc_id, name);
  if (group_hid < 0)
    return false;

  data_hid = H5Dopen (group_hid, "nm");

  if (data_hid < 0)
    {
      H5Gclose (group_hid);
      return false;
    }

  type_hid = H5Dget_type (data_hid);
  type_class_hid = H5Tget_class (type_hid);

  if (type_class_hid != H5T_STRING)
    {
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  slen = H5Tget_size (type_hid);
  if (slen < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (char, nm_tmp, slen);

  // create datatype for (null-terminated) string to read into:
  st_id = H5Tcopy (H5T_C_S1);
  H5Tset_size (st_id, slen);

  if (H5Dread (data_hid, st_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, nm_tmp) < 0)
    {
      H5Tclose (st_id);
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }
  H5Tclose (st_id);
  H5Dclose (data_hid);
  nm = nm_tmp;

  if (nm == "@<anonymous>")
    {
      data_hid = H5Dopen (group_hid, "fcn");

      if (data_hid < 0)
	{
	  H5Sclose (space_hid);
	  H5Tclose (type_hid);
	  H5Gclose (group_hid);
	  return false;
	}

      H5Tclose (type_hid);
      type_hid = H5Dget_type (data_hid);
      type_class_hid = H5Tget_class (type_hid);

      if (type_class_hid != H5T_STRING)
	{
	  H5Sclose (space_hid);
	  H5Tclose (type_hid);
	  H5Dclose (data_hid);
	  H5Gclose (group_hid);
	  return false;
	}

      H5Sclose (space_hid);
      space_hid = H5Dget_space (data_hid);
      rank = H5Sget_simple_extent_ndims (space_hid);

      if (rank != 0)
	{
	  H5Sclose (space_hid);
	  H5Tclose (type_hid);
	  H5Dclose (data_hid);
	  H5Gclose (group_hid);
	  return false;
	}

      slen = H5Tget_size (type_hid);
      if (slen < 0)
	{
	  H5Sclose (space_hid);
	  H5Tclose (type_hid);
	  H5Dclose (data_hid);
	  H5Gclose (group_hid);
	  return false;
	}

      OCTAVE_LOCAL_BUFFER (char, fcn_tmp, slen);

      // create datatype for (null-terminated) string to read into:
      st_id = H5Tcopy (H5T_C_S1);
      H5Tset_size (st_id, slen);

      if (H5Dread (data_hid, st_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, fcn_tmp) < 0)
	{
	  H5Tclose (st_id);
	  H5Sclose (space_hid);
	  H5Tclose (type_hid);
	  H5Dclose (data_hid);
	  H5Gclose (group_hid);
	  return false;
	}
      H5Tclose (st_id);
      H5Dclose (data_hid);

      octave_idx_type len = 0;

      // we have to pull some shenanigans here to make sure
      // HDF5 doesn't print out all sorts of error messages if we
      // call H5Aopen for a non-existing attribute

      H5E_auto_t err_func;
      void *err_func_data;

      // turn off error reporting temporarily, but save the error
      // reporting function:
      H5Eget_auto (&err_func, &err_func_data);
      H5Eset_auto (0, 0);

      hid_t attr_id = H5Aopen_name (group_hid, "SYMBOL_TABLE");

      if (attr_id >= 0)
	{
	  if (H5Aread (attr_id, H5T_NATIVE_IDX, &len) < 0)
	    success = false;

	  H5Aclose (attr_id);
	}

      // restore error reporting:
      H5Eset_auto (err_func, err_func_data);

      symbol_table::scope_id local_scope = symbol_table::alloc_scope ();

      if (len > 0 && success)
	{
#ifdef HAVE_H5GGET_NUM_OBJS
	  hsize_t num_obj = 0;
	  data_hid = H5Gopen (group_hid, "symbol table"); 
	  H5Gget_num_objs (data_hid, &num_obj);
	  H5Gclose (data_hid);

	  if (num_obj != static_cast<hsize_t>(len))
	    {
	      error ("load: failed to load anonymous function handle");
	      success = false;
	    }
#endif

	  if (! error_state)
	    {
	      hdf5_callback_data dsub;
	      int current_item = 0;
	      for (octave_idx_type i = 0; i < len; i++)
		{
		  if (H5Giterate (group_hid, "symbol table", &current_item,
				  hdf5_read_next_data, &dsub) <= 0)
		    {
		      error ("load: failed to load anonymous function handle");
		      success = false;
		      break;
		    }

		  if (have_h5giterate_bug)
		    current_item++;  // H5Giterate returns last index processed

		  symbol_table::varref (dsub.name, local_scope) = dsub.tc;
		}
	    }
	}

      if (success)
	{
	  unwind_protect::begin_frame ("anon_hdf5_load");

	  symbol_table::push_scope (local_scope);

	  unwind_protect::add (symbol_table::pop_scope);

	  int parse_status;
	  octave_value anon_fcn_handle = 
	    eval_string (fcn_tmp, true, parse_status);

	  if (parse_status == 0)
	    {
	      octave_fcn_handle *fh = anon_fcn_handle.fcn_handle_value ();
	      if (fh)
		fcn = fh->fcn;
	      else
		success = false;
	    }
	  else
	    success = false;

	  unwind_protect::run_frame ("anon_hdf5_load");
	}

      symbol_table::erase_scope (local_scope);
    }
  else
    {
      std::string octaveroot;
      std::string fpath;

      // we have to pull some shenanigans here to make sure
      // HDF5 doesn't print out all sorts of error messages if we
      // call H5Aopen for a non-existing attribute

      H5E_auto_t err_func;
      void *err_func_data;

      // turn off error reporting temporarily, but save the error
      // reporting function:
      H5Eget_auto (&err_func, &err_func_data);
      H5Eset_auto (0, 0);

      hid_t attr_id = H5Aopen_name (group_hid, "OCTAVEROOT");
      if (attr_id >= 0)
	{
	  H5Tclose (type_hid);
	  type_hid = H5Aget_type (attr_id);
	  type_class_hid = H5Tget_class (type_hid);

	  if (type_class_hid != H5T_STRING)
	    success = false;
	  else
	    {
	      slen = H5Tget_size (type_hid);
	      st_id = H5Tcopy (H5T_C_S1);
	      H5Tset_size (st_id, slen);
	      OCTAVE_LOCAL_BUFFER (char, root_tmp, slen);

	      if (H5Aread (attr_id, st_id, root_tmp) < 0)
		success = false;
	      else
		octaveroot = root_tmp;

	      H5Tclose (st_id);
	    }

	  H5Aclose (attr_id);
	}

      if (success)
	{
	  attr_id = H5Aopen_name (group_hid, "FILE");
	  if (attr_id >= 0)
	    {
	      H5Tclose (type_hid);
	      type_hid = H5Aget_type (attr_id);
	      type_class_hid = H5Tget_class (type_hid);

	      if (type_class_hid != H5T_STRING)
		success = false;
	      else
		{
		  slen = H5Tget_size (type_hid);
		  st_id = H5Tcopy (H5T_C_S1);
		  H5Tset_size (st_id, slen);
		  OCTAVE_LOCAL_BUFFER (char, path_tmp, slen);

		  if (H5Aread (attr_id, st_id, path_tmp) < 0)
		    success = false;
		  else
		    fpath = path_tmp;

		  H5Tclose (st_id);
		}

	      H5Aclose (attr_id);
	    }
	}

      // restore error reporting:
      H5Eset_auto (err_func, err_func_data);

      success = (success ? set_fcn (octaveroot, fpath) : success);
    }

  H5Tclose (type_hid);
  H5Sclose (space_hid);
  H5Gclose (group_hid);

  return success;
}

/* 

%!test
%! if (!isempty(findstr(octave_config_info ("DEFS"),"HAVE_HDF5")))
%!   a = 2;
%!   f = @(x) a + x;
%!   g = @(x) 2 * x;
%!   h = @log2;
%!   f2 = f;
%!   g2 = g;
%!   h2 = h;
%!   nm = tmpnam();
%!   unwind_protect
%!     save ("-hdf5", nm, "f2", "g2", "h2");
%!     clear f2 g2 h2
%!     load (nm);
%!     assert (f(2),f2(2));
%!     assert (g(2),g2(2));
%!     assert (g(3),g2(3));
%!     unlink (nm);
%!     save ("-hdf5", nm, "f2", "g2", "h2");
%!   unwind_protect_cleanup
%!     unlink (nm);
%!   end_unwind_protect
%! endif

*/
#endif

void
octave_fcn_handle::print (std::ostream& os, bool pr_as_read_syntax) const
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

void
octave_fcn_handle::print_raw (std::ostream& os, bool pr_as_read_syntax) const
{
  bool printed = false;

  if (nm == "@<anonymous>")
    {
      tree_print_code tpc (os);

      // FCN is const because this member function is, so we can't
      // use it to call user_function_value, so we make a copy first.

      octave_value ftmp = fcn;

      octave_user_function *f = ftmp.user_function_value ();

      if (f)
	{
	  tree_parameter_list *p = f->parameter_list ();

	  os << "@(";

	  if (p)
	    p->accept (tpc);

	  os << ") ";

	  tree_statement_list *b = f->body ();

	  if (b)
	    {
	      assert (b->length () == 1);

	      tree_statement *s = b->front ();

	      if (s)
		{
		  if (s->is_expression ())
		    {
		      tree_expression *e = s->expression ();

		      if (e)
			e->accept (tpc);
		    }
		  else
		    {
		      tree_command *c = s->command ();

		      tpc.suspend_newline ();
		      c->accept (tpc);
		      tpc.resume_newline ();
		    }
		}
	    }

	  printed = true;
	}
    }

  if (! printed)
    octave_print_internal (os, nm, pr_as_read_syntax,
			   current_print_indent_level ());
}

octave_value
make_fcn_handle (const std::string& nm)
{
  octave_value retval;

  octave_value f = symbol_table::find_function (nm);

  if (f.is_defined ())
    retval = octave_value (new octave_fcn_handle (f, nm));
  else
    error ("error creating function handle \"@%s\"", nm.c_str ());

  return retval;
}

DEFUN (functions, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} functions (@var{fcn_handle})\n\
Return a struct containing information about the function handle\n\
@var{fcn_handle}.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      octave_fcn_handle *fh = args(0).fcn_handle_value ();

      if (! error_state)
	{
	  octave_function *fcn = fh ? fh->function_value (true) : 0;

	  if (fcn)
	    {
	      Octave_map m;

	      std::string fh_nm = fh->fcn_name ();

	      if (fh_nm == "@<anonymous>")
		{
		  std::ostringstream buf;
		  fh->print_raw (buf);
		  m.assign ("function", buf.str ());

		  m.assign ("type", "anonymous");
		}
	      else
		{
		  m.assign ("function", fh_nm);

		  if (fcn->is_nested_function ())
		    {
		      m.assign ("type", "subfunction");
		      Cell parentage (dim_vector (1, 2));
		      parentage.elem(0) = fh_nm;
		      parentage.elem(1) = fcn->parent_fcn_name ();
		      m.assign ("parentage", parentage); 
		    }
		  else
		    m.assign ("type", "simple");
		}

	      std::string nm = fcn->fcn_file_name ();

	      if (nm.empty ())
		{
		  if (fh_nm == "@<anonymous>")
		    {
		      m.assign ("file", "");

		      octave_user_function *fu = fh->user_function_value ();

		      std::list<symbol_table::symbol_record> vars
			= symbol_table::all_variables (fu->scope ());

		      size_t varlen = vars.size ();

		      if (varlen > 0)
			{
			  Octave_map ws;
			  for (std::list<symbol_table::symbol_record>::const_iterator p = vars.begin ();
			       p != vars.end (); p++)
			    {
			      ws.assign (p->name (), p->varval ());
			    }

			  m.assign ("workspace", ws);
			}
		    }
		  else if (fcn->is_user_function ())
		    {
		      octave_user_function *fu = fh->user_function_value ();
		      m.assign ("file", fu->fcn_file_name ());
		    }
		  else
		    m.assign ("file", "");
		}
	      else
		m.assign ("file", nm);

	      retval = m;
	    }
	  else
	    error ("functions: invalid function handle object");
	}
      else
	error ("functions: argument must be a function handle object");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (func2str, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} func2str (@var{fcn_handle})\n\
Return a string containing the name of the function referenced by\n\
the function handle @var{fcn_handle}.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      octave_fcn_handle *fh = args(0).fcn_handle_value ();

      if (! error_state && fh)
	{
	  std::string fh_nm = fh->fcn_name ();

	  if (fh_nm == "@<anonymous>")
	    {
	      std::ostringstream buf;

	      fh->print_raw (buf);

	      retval = buf.str ();
	    }
	  else
	    retval = fh_nm;
	}
      else
	error ("func2str: expecting valid function handle as first argument");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (str2func, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} str2func (@var{fcn_name})\n\
Return a function handle constructed from the string @var{fcn_name}.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      std::string nm = args(0).string_value ();

      if (! error_state)
	retval = make_fcn_handle (nm);
      else
	error ("str2func: expecting string as first argument");
    }
  else
    print_usage ();

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
