/*

Copyright (C) 2003 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "defun.h"
#include "error.h"
#include "gripes.h"
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

#include "byte-swap.h"
#include "ls-oct-ascii.h"
#include "ls-hdf5.h"
#include "ls-utils.h"

DEFINE_OCTAVE_ALLOCATOR (octave_fcn_handle);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_fcn_handle,
				     "function handle",
				     "function handle");

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
	octave_function *f = function_value ();
	retval = f->subsref (type, idx, nargout);
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

  // XXX FIXME XXX -- perhaps there should be an
  // octave_value_list::next_subsref member function?  See also
  // octave_builtin::subsref.

  if (idx.size () > 1)
    retval = retval(0).next_subsref (nargout, type, idx);

  return retval;
}

bool
octave_fcn_handle::save_ascii (std::ostream& os, bool&, bool)
{
  os << nm << "\n";

  if (nm == "@<anonymous>")
    {
      print_raw (os, true);
      os << "\n";
    }

  return true;
}

bool
octave_fcn_handle::load_ascii (std::istream& is)
{
  is >> nm;

  if (nm == "@<anonymous>")
    {
      char c;
      OSSTREAM buf;

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

      buf << OSSTREAM_ENDS;

      int parse_status;
      octave_value anon_fcn_handle = eval_string (OSSTREAM_STR (buf), 
						  true, parse_status);
      OSSTREAM_FREEZE (buf);

      if (parse_status == 0)
	{
	  octave_fcn_handle *fh = anon_fcn_handle.fcn_handle_value ();
	  if (fh)
	    fcn = fh->fcn;
	  else
	    return false;
	}
      else
	return false;
    }
  else
    {
      fcn = lookup_function (nm);
      if (! fcn.is_function ())
	return false;
    }

  return true;
}

bool
octave_fcn_handle::save_binary (std::ostream& os, bool&)
{
  FOUR_BYTE_INT tmp = nm.length ();
  os.write (X_CAST (char *, &tmp), 4);
  os.write (nm.c_str (), nm.length ());
  if (nm == "@<anonymous>")
    {
      OSSTREAM buf;
      print_raw (buf, true);
      std::string stmp = OSSTREAM_STR (buf);
      OSSTREAM_FREEZE (buf);
      tmp = stmp.length ();
      os.write (X_CAST (char *, &tmp), 4);
      os.write (stmp.c_str (), stmp.length ());
    }
  return true;
}

bool
octave_fcn_handle::load_binary (std::istream& is, bool swap,
				oct_mach_info::float_format)
{
  FOUR_BYTE_INT tmp;
  if (! is.read (X_CAST (char *, &tmp), 4))
    return false;
  if (swap)
    swap_bytes<4> (&tmp);

  OCTAVE_LOCAL_BUFFER (char, ctmp1, tmp+1);
  is.read (ctmp1, tmp);
  nm = std::string (ctmp1);

  if (! is)
    return false;

  if (nm == "@<anonymous>")
    {
      if (! is.read (X_CAST (char *, &tmp), 4))
	return false;
      if (swap)
	swap_bytes<4> (&tmp);

      OCTAVE_LOCAL_BUFFER (char, ctmp2, tmp+1);
      is.read (ctmp2, tmp);

      int parse_status;
      octave_value anon_fcn_handle = eval_string (ctmp2, true, parse_status);

      if (parse_status == 0)
	{
	  octave_fcn_handle *fh = anon_fcn_handle.fcn_handle_value ();
	  if (fh)
	    fcn = fh->fcn;
	  else
	    return false;
	}
      else
	return false;
    }
  else
    {
      fcn = lookup_function (nm);
      if (! fcn.is_function ())
	return false;
    }
  return true;
}

#if defined (HAVE_HDF5)
bool
octave_fcn_handle::save_hdf5 (hid_t loc_id, const char *name,
			      bool /* save_as_floats */)
{
  hid_t group_hid = -1;
  group_hid = H5Gcreate (loc_id, name, 0);
  if (group_hid < 0 ) return false;

  hid_t space_hid = -1, data_hid = -1, type_hid = -1;;
  bool retval = true;

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
  space_hid = H5Screate_simple (0 , hdims, (hsize_t*) 0);
  if (space_hid < 0)
    {
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return false;
    }

  data_hid = H5Dcreate (group_hid, "nm",  type_hid, space_hid, H5P_DEFAULT);
  if (data_hid < 0 || H5Dwrite (data_hid, type_hid, H5S_ALL, H5S_ALL,
				H5P_DEFAULT,
				X_CAST (void *, nm.c_str ())) < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return false;
    }
  H5Dclose (data_hid);

  if (nm == "@<anonymous>")
    {
      OSSTREAM buf;
      print_raw (buf, true);
      std::string stmp = OSSTREAM_STR (buf);
      OSSTREAM_FREEZE (buf);

      // attach the type of the variable
      H5Tset_size (type_hid, stmp.length () + 1);
      if (type_hid < 0)
	{
	  H5Gclose (group_hid);
	  return false;
	}

      data_hid = H5Dcreate (group_hid, "fcn",  type_hid, space_hid,
			    H5P_DEFAULT);
      if (data_hid < 0 || H5Dwrite (data_hid, type_hid, H5S_ALL, H5S_ALL,
				    H5P_DEFAULT,
				    X_CAST (void *, stmp.c_str ())) < 0)
	{
	  H5Sclose (space_hid);
	  H5Tclose (type_hid);
	  H5Gclose (group_hid);
	  return false;
	}

      H5Dclose (data_hid);
    }

  H5Sclose (space_hid);
  H5Tclose (type_hid);
  H5Gclose (group_hid);

  return retval;
}

bool
octave_fcn_handle::load_hdf5 (hid_t loc_id, const char *name,
			      bool /* have_h5giterate_bug */)
{
  hid_t group_hid, data_hid, space_hid, type_hid, type_class_hid, st_id;
  hsize_t rank;
  int slen;

  group_hid = H5Gopen (loc_id, name);
  if (group_hid < 0 ) return false;

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

  if (H5Dread (data_hid, st_id, H5S_ALL, H5S_ALL, H5P_DEFAULT,
	       X_CAST (void *, nm_tmp)) < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
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

      OCTAVE_LOCAL_BUFFER (char, fcn_tmp, slen);

      // create datatype for (null-terminated) string to read into:
      st_id = H5Tcopy (H5T_C_S1);
      H5Tset_size (st_id, slen);

      if (H5Dread (data_hid, st_id, H5S_ALL, H5S_ALL, H5P_DEFAULT,
		   X_CAST (void *, fcn_tmp)) < 0)
	{
	  H5Sclose (space_hid);
	  H5Tclose (type_hid);
	  H5Gclose (group_hid);
	  return false;
	}
      H5Dclose (data_hid);
      H5Tclose (st_id);

      int parse_status;
      octave_value anon_fcn_handle = eval_string (fcn_tmp, true, parse_status);

      if (parse_status == 0)
	{
	  octave_fcn_handle *fh = anon_fcn_handle.fcn_handle_value ();
	  if (fh)
	    fcn = fh->fcn;
	  else
	    return false;
	}
      else
	return false;
    }
  else
    {
      fcn = lookup_function (nm);
      if (! fcn.is_function ())
	return false;
    }

  return true;
}
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
			{
			  if (e->is_assignment_expression ())
			    {
			      // The parser builds an assignment to
			      // __retval__, and we don't want to
			      // display that part.

			      tree_simple_assignment *tsa
				= reinterpret_cast <tree_simple_assignment *> (e);
			      tree_expression *rhs = tsa->right_hand_side ();

			      if (rhs)
				rhs->accept (tpc);
			    }
			  else
			    e->accept (tpc);
			}
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

  octave_value f = lookup_function (nm);

  if (f.is_function ())
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

	      m.assign ("function", fh_nm);

	      if (fcn->is_nested_function ())
		m.assign ("type", "subfunction");
	      else
		m.assign ("type", "simple");

	      std::string nm = fcn->fcn_file_name ();

	      if (nm.empty ())
		{
		  if (fh_nm == "@<anonymous>")
		    m.assign ("file", "none");
		  else
		    m.assign ("file", "built-in function");
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
    print_usage ("functions");

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
	  retval = fh_nm;
	}
      else
	error ("func2str: expecting valid function handle as first argument");
    }
  else
    print_usage ("func2str");

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
    print_usage ("str2func");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
