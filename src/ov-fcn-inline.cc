/*

Copyright (C) 2004 David Bateman

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.

In addition to the terms of the GPL, you are permitted to link
this program with any Open Source program, as defined by the
Open Source Initiative (www.opensource.org)

*/

#if defined (__GNUG__) && defined (USE_PRAGMA_INTERFACE_IMPLEMENTATION)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <istream>
#include <iostream>

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-map.h"
#include "ov-base.h"
#include "ov-fcn-inline.h"
#include "pr-output.h"
#include "variables.h"
#include "parse.h"

#include "byte-swap.h"
#include "ls-oct-ascii.h"
#include "ls-hdf5.h"
#include "ls-utils.h"

DEFINE_OCTAVE_ALLOCATOR (octave_fcn_inline);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_fcn_inline,
				     "inline function",
				     "inline function");

octave_fcn_inline::octave_fcn_inline (const std::string& f,
				      const string_vector& a,
				      const std::string& n)
  : octave_fcn_handle (n), iftext (f), ifargs (a)
{
  // Form a string representing the function.

  OSSTREAM buf;

  buf << "@(";

  for (int i = 0; i < ifargs.length (); i++)
    {
      if (i > 0)
	buf << ", ";

      buf << ifargs(i);
    }

  buf << ") " << iftext << OSSTREAM_ENDS;

  int parse_status;
  octave_value anon_fcn_handle = eval_string (OSSTREAM_STR (buf), true,
					      parse_status);

  OSSTREAM_FREEZE (buf);

  if (parse_status == 0)
    {
      octave_fcn_handle *fh = anon_fcn_handle.fcn_handle_value ();

      if (fh)
	fcn = fh->fcn_val ();
    }

  if (fcn.is_undefined ())
    error ("inline: unable to define function");
}

bool
octave_fcn_inline::save_ascii (std::ostream& os, bool&, bool)
{
  os << "# nargs: " <<  ifargs.length () << "\n";
  for (int i = 0; i < ifargs.length (); i++)
    os << ifargs(i) << "\n";
  if (nm.length () < 1)
    // Write an invalid value to flag empty fcn handle name.
    os << "0\n";
  else
    os << nm << "\n";
  os << iftext << "\n";
  return true;
}

bool
octave_fcn_inline::load_ascii (std::istream& is)
{
  int nargs;
  if (extract_keyword (is, "nargs", nargs, true))
    {
      ifargs.resize (nargs);
      for (int i = 0; i < nargs; i++)
	is >> ifargs(i);
      is >> nm;
      if (nm == "0")
	nm = "";

      char c;
      OSSTREAM buf;

      // Skip preceeding newline(s)
      while (is.get (c) && c == '\n');

      if (is)
	{
	  buf << c;

	  // Get a line of text whitespace characters included, leaving
	  // newline in the stream
	  while (is.peek () != '\n')
	    {
	      is.get (c);
	      if (! is)
		break;
	      buf << c;
	    }
	}

      buf << OSSTREAM_ENDS;
      iftext = OSSTREAM_STR (buf);
      OSSTREAM_FREEZE (buf);

      octave_fcn_inline tmp (iftext, ifargs, nm);
      fcn = tmp.fcn;

      return true;
    }
  else
    return false;
}

bool
octave_fcn_inline::save_binary (std::ostream& os, bool&)
{
  FOUR_BYTE_INT tmp = ifargs.length ();
  os.write (X_CAST (char *, &tmp), 4);
  for (int i = 0; i < ifargs.length (); i++)
    {
      tmp = ifargs(i).length ();
      os.write (X_CAST (char *, &tmp), 4);
      os.write (ifargs(i).c_str (), ifargs(i).length ());
    }
  tmp = nm.length ();
  os.write (X_CAST (char *, &tmp), 4);
  os.write (nm.c_str (), nm.length ());
  tmp = iftext.length ();
  os.write (X_CAST (char *, &tmp), 4);
  os.write (iftext.c_str (), iftext.length ());
  return true;
}

bool
octave_fcn_inline::load_binary (std::istream& is, bool swap,
				oct_mach_info::float_format)
{
  FOUR_BYTE_INT nargs;
  if (! is.read (X_CAST (char *, &nargs), 4))
    return false;
  if (swap)
    swap_bytes<4> (&nargs);

  if (nargs < 1)
    return false;
  else
    {
      FOUR_BYTE_INT tmp;
      ifargs.resize (nargs);
      for (int i = 0; i < nargs; i++)
	{
	  if (! is.read (X_CAST (char *, &tmp), 4))
	    return false;
	  if (swap)
	    swap_bytes<4> (&tmp);

	  OCTAVE_LOCAL_BUFFER (char, ctmp, tmp+1);
	  is.read (ctmp, tmp);
	  ifargs(i) = std::string (ctmp);

	  if (! is)
	    return false;
	}

      if (! is.read (X_CAST (char *, &tmp), 4))
	return false;
      if (swap)
	swap_bytes<4> (&tmp);

      OCTAVE_LOCAL_BUFFER (char, ctmp1, tmp+1);
      is.read (ctmp1, tmp);
      nm = std::string (ctmp1);

      if (! is)
	return false;

      if (! is.read (X_CAST (char *, &tmp), 4))
	return false;
      if (swap)
	swap_bytes<4> (&tmp);

      OCTAVE_LOCAL_BUFFER (char, ctmp2, tmp+1);
      is.read (ctmp2, tmp);
      iftext = std::string (ctmp2);

      if (! is)
	return false;

      octave_fcn_inline ftmp (iftext, ifargs, nm);
      fcn = ftmp.fcn;
    }
  return true;
}

#if defined (HAVE_HDF5)
bool
octave_fcn_inline::save_hdf5 (hid_t loc_id, const char *name,
			      bool /* save_as_floats */)
{
  hid_t group_hid = -1;
  group_hid = H5Gcreate (loc_id, name, 0);
  if (group_hid < 0 ) return false;

  size_t len = 0;
  for (int i = 0; i < ifargs.length (); i++)
    if (len < ifargs(i).length ())
      len = ifargs(i).length ();

  hid_t space_hid = -1, data_hid = -1, type_hid = -1;;
  bool retval = true;

  // XXX FIXME XXX Is there a better way of saving string vectors, than a
  // null padded matrix?

  OCTAVE_LOCAL_BUFFER (hsize_t, hdims, 2);

  // Octave uses column-major, while HDF5 uses row-major ordering
  hdims[1] = ifargs.length ();
  hdims[0] = len + 1;

  space_hid = H5Screate_simple (2, hdims, 0);
  if (space_hid < 0)
    {
      H5Gclose (group_hid);
      return false;
    }

  data_hid = H5Dcreate (group_hid, "args", H5T_NATIVE_CHAR, space_hid,
			H5P_DEFAULT);
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (char, s, ifargs.length () * (len + 1));

  // Save the args as a null teminated list
  for (int i = 0; i < ifargs.length (); i++)
    {
      const char * cptr = ifargs(i).c_str ();
      for (size_t j = 0; j < ifargs(i).length (); j++)
	s[i*(len+1)+j] = *cptr++;
      s[ifargs(i).length ()] = '\0';
    }

  retval = H5Dwrite (data_hid, H5T_NATIVE_CHAR, H5S_ALL, H5S_ALL,
		     H5P_DEFAULT, s) >= 0;

  H5Dclose (data_hid);
  H5Sclose (space_hid);

  if (!retval)
    {
      H5Gclose (group_hid);
      return false;
    }

  // attach the type of the variable
  type_hid = H5Tcopy (H5T_C_S1);
  H5Tset_size (type_hid, nm.length () + 1);
  if (type_hid < 0)
    {
      H5Gclose (group_hid);
      return false;
    }

  hdims[0] = 0;
  space_hid = H5Screate_simple (0 , hdims, (hsize_t*) 0);
  if (space_hid < 0)
    {
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return false;
    }

  data_hid = H5Dcreate (group_hid, "nm",  type_hid, space_hid, H5P_DEFAULT);
  if (data_hid < 0 || H5Dwrite (data_hid, type_hid, H5S_ALL, H5S_ALL,
				H5P_DEFAULT, (void*) nm.c_str ()) < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return false;
    }
  H5Dclose (data_hid);

  // attach the type of the variable
  H5Tset_size (type_hid, iftext.length () + 1);
  if (type_hid < 0)
    {
      H5Gclose (group_hid);
      return false;
    }

  data_hid = H5Dcreate (group_hid, "iftext",  type_hid, space_hid,
			H5P_DEFAULT);
  if (data_hid < 0 || H5Dwrite (data_hid, type_hid, H5S_ALL, H5S_ALL,
				H5P_DEFAULT, (void*) iftext.c_str ()) < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Dclose (data_hid);
  H5Sclose (space_hid);
  H5Tclose (type_hid);
  H5Gclose (group_hid);

  return retval;
}

bool
octave_fcn_inline::load_hdf5 (hid_t loc_id, const char *name,
			      bool /* have_h5giterate_bug */)
{
  hid_t group_hid, data_hid, space_hid, type_hid, type_class_hid, st_id;
  hsize_t rank;
  int slen;

  group_hid = H5Gopen (loc_id, name);
  if (group_hid < 0 ) return false;

  data_hid = H5Dopen (group_hid, "args");
  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 2)
    {
      H5Dclose (data_hid);
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (hsize_t, hdims, rank);
  OCTAVE_LOCAL_BUFFER (hsize_t, maxdims, rank);

  H5Sget_simple_extent_dims (space_hid, hdims, maxdims);

  ifargs.resize (hdims[1]);

  OCTAVE_LOCAL_BUFFER (char, s1, hdims[0] * hdims[1]);

  if (H5Dread (data_hid, H5T_NATIVE_UCHAR, H5S_ALL, H5S_ALL,
	       H5P_DEFAULT, s1) < 0)
    {
      H5Dclose (data_hid);
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Dclose (data_hid);
  H5Sclose (space_hid);

  for (size_t i = 0; i < hdims[1]; i++)
    ifargs(i) = std::string (s1 + i*hdims[0]);

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

  data_hid = H5Dopen (group_hid, "iftext");

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

  OCTAVE_LOCAL_BUFFER (char, iftext_tmp, slen);

  // create datatype for (null-terminated) string to read into:
  st_id = H5Tcopy (H5T_C_S1);
  H5Tset_size (st_id, slen);

  if (H5Dread (data_hid, st_id, H5S_ALL, H5S_ALL, H5P_DEFAULT,
	       X_CAST (void *, iftext_tmp)) < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      H5Gclose (group_hid);
      return false;
    }
  H5Tclose (st_id);
  H5Dclose (data_hid);
  iftext = iftext_tmp;

  octave_fcn_inline ftmp (iftext, ifargs, nm);
  fcn = ftmp.fcn;

  return true;
}
#endif

void
octave_fcn_inline::print (std::ostream& os, bool pr_as_read_syntax) const
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

void
octave_fcn_inline::print_raw (std::ostream& os, bool pr_as_read_syntax) const
{
  OSSTREAM buf;

  if (nm.empty ())
    buf << "f(";
  else
    buf << nm << "(";

  for (int i = 0; i < ifargs.length (); i++)
    {
      if (i)
	buf << ", ";

      buf << ifargs(i);
    }

  buf << ") = " << iftext << OSSTREAM_ENDS;

  octave_print_internal (os, OSSTREAM_STR (buf), pr_as_read_syntax,
			 current_print_indent_level ());
  OSSTREAM_FREEZE (buf);
}

octave_value
octave_fcn_inline::convert_to_str_internal (bool, bool) const
{
  return octave_value (fcn_text ());
}

DEFUN (inline, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} inline (@var{str})\n\
@deftypefnx {Built-in Function} {} inline (@var{str}, @var{arg1}, ...)\n\
@deftypefnx {Built-in Function} {} inline (@var{str}, @var{n})\n\
Create an inline function from the character string @var{str}.\n\
If called with a single argument, the generated function is\n\
assumed to have a single argument and will be defined as the\n\
isolated lower case character, except i or j, that is closest\n\
to x. If more than argument is the same distance from x, the\n\
one later in the alphabet is chosen.\n\
\n\
If the second and subsequent arguments are character strings,\n\
they are the names of the arguments of the function.\n\
\n\
If the second argument is an integer @var{n}, the arguments are\n\
@code{\"x\"}, @code{\"P1\"}, @dots{}, @code{\"P@var{N}\"}.\n\
@end deftypefn\n\
@seealso{argnames, formula, vectorize}")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin > 0)
    {
      std::string fun = args(0).string_value ();

      if (! error_state)
	{
	  string_vector fargs;

	  if (nargin == 1)
	    {
	      int dist = -1;
	      char c = '\0';

	      fargs.resize (1);
	      fargs(0) = "x";

	      int fun_len = fun.length ();

	      for (int i = 0; i < fun_len; i++)
		{
		  if (islower (fun[i])
		      && (i == 0 || ! islower (fun[i-1]))
		      && (i == fun_len || ! islower (fun[i+1])))
		    {
		      char new_c = fun[i];

		      if (new_c == 'i' || new_c == 'j') 
			continue;

		      int new_dist = std::abs (new_c - 'x');

		      if (dist == -1 || new_dist < dist
			  || (new_dist == dist && c < new_c))
			{
			  fargs(0) = new_c;
			  dist = new_dist;
			  c = new_c;
			}
		    }
		}
	    }
	  else if (nargin == 2 && args(1).is_numeric_type ())
	    {
	      int n = args(1).int_value ();

	      if (! error_state)
		{
		  if (n >= 0)
		    {
		      fargs.resize (n+1);

		      fargs(0) = "x";

		      for (int i = 1; i < n+1; i++)
			{
			  OSSTREAM buf;
			  buf << "P" << i << OSSTREAM_ENDS;
			  fargs(i) = OSSTREAM_STR (buf);
			  OSSTREAM_FREEZE (buf);
			}
		    }
		  else
		    {
		      error ("inline: numeric argument must be nonnegative");
		      return retval;
		    }
		}
	      else
		{
		  error ("inline: expecting second argument to be an integer");
		  return retval;
		}
	    }
	  else
	    {
	      fargs.resize (nargin - 1);

	      for (int i = 1; i < nargin; i++)
		{
		  std::string s = args(i).string_value ();

		  if (! error_state)
		    fargs(i-1) = s;
		  else
		    {
		      error ("inline: expecting string arguments");
		      return retval;
		    }
		}
	    }

	  retval = octave_value (new octave_fcn_inline (fun, fargs));
	}
      else
	error ("inline: first argument must be a string");
    }
  else
    print_usage ("inline");

  return retval;
}

DEFUN (formula, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} formula (@var{fun})\n\
Return a character string representing the inline function @var{fun}.\n\
Note that @code{char (@var{fun})} is equivalent to\n\
@code{formula (@var{fun})}.\n\
@end deftypefn\n\
@seealso{argnames, inline, vectorize}")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_fcn_inline* fn = args(0).fcn_inline_value (true);

      if (fn)
	retval = octave_value (fn->fcn_text ());
      else
	error ("formula: must be an inline function");
    }
  else
    print_usage ("formula");

  return retval;
}

DEFUN (argnames, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} argnames (@var{fun})\n\
Return a cell array of character strings containing the names of\n\
the arguments of the inline function @var{fun}.\n\
@end deftypefn\n\
@seealso{argnames, inline, formula, vectorize}")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_fcn_inline *fn = args(0).fcn_inline_value (true);

      if (fn)
	{
	  string_vector t1 = fn->fcn_arg_names ();

	  Cell t2 (dim_vector (t1.length (), 1));

	  for (int i = 0; i < t1.length (); i++)
	    t2(i) = t1(i);

	  retval = t2;
	}
      else
	error ("argnames: argument must be an inline function");
    }
  else
    print_usage ("argnames");

  return retval;
}

DEFUN (vectorize, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} argnames (@var{fun})\n\
Create a vectorized version of the inline function @var{fun}\n\
by replacing all occurrences of @code{*}, @code{/}, etc., with\n\
@code{.*}, @code{./}, etc.\n\
@end deftypefn\n\
@seealso{argnames, inline, formula, vectorize}")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_fcn_inline* old = args(0).fcn_inline_value (true);

      if (old)
	{
	  std::string old_func = old->fcn_text ();
	  std::string new_func;

	  size_t i = 0;

	  while (i < old_func.length ())
	    {
	      std::string t1 = old_func.substr (i, 1);

	      if (t1 == "*" || t1 == "/" || t1 == "\\" || t1 == "^")
		{
		  if (i && old_func.substr (i-1, 1) != ".")
		    new_func.append (".");

		  // Special case for ** operator.
		  if (t1 == "*" && i < (old_func.length () - 1)
		      && old_func.substr (i+1, 1) == "*")
		    {
		      new_func.append ("*");
		      i++;
		    }
		}
	      new_func.append (t1);
	      i++;
	    }

	  retval = octave_value (new octave_fcn_inline (new_func, old->fcn_arg_names ()));
	}
      else
	error ("vectorize: must be an inline function");
    }
  else
    print_usage ("vectorize");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
