/*

Copyright (C) 2007 John W. Eaton

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

#include "Array-util.h"
#include "byte-swap.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "load-path.h"
#include "ls-hdf5.h"
#include "ls-oct-ascii.h"
#include "ls-oct-binary.h"
#include "ls-utils.h"
#include "oct-lvalue.h"
#include "ov-class.h"
#include "ov-fcn.h"
#include "pager.h"
#include "parse.h"
#include "pr-output.h"
#include "toplev.h"
#include "unwind-prot.h"
#include "variables.h"

DEFINE_OCTAVE_ALLOCATOR(octave_class);

int octave_class::t_id (-1);

const std::string octave_class::t_name ("class");

void
octave_class::register_type (void)
{
  t_id = octave_value_typeinfo::register_type
    (octave_class::t_name, "<unknown>", octave_value (new octave_class ()));
}

Cell
octave_class::dotref (const octave_value_list& idx)
{
  Cell retval;

  assert (idx.length () == 1);

  std::string nm = idx(0).string_value ();

  Octave_map::const_iterator p = map.seek (nm);

  if (p != map.end ())
    retval = map.contents (p);
  else
    error ("class has no member `%s'", nm.c_str ());

  return retval;
}

static void
gripe_invalid_index (void)
{
  error ("invalid index for class");
}

static void
gripe_invalid_index_for_assignment (void)
{
  error ("invalid index for class assignment");
}

static void
gripe_invalid_index_type (const std::string& nm, char t)
{
  error ("%s cannot be indexed with %c", nm.c_str (), t);
}

static void
gripe_failed_assignment (void)
{
  error ("assignment to class element failed");
}

static inline octave_value_list
sanitize (const octave_value_list& ovl)
{
  octave_value_list retval = ovl;

  for (octave_idx_type i = 0; i < ovl.length (); i++)
    {
      if (retval(i).is_magic_colon ())
	retval(i) = ":";
    }

  return retval;
}

static inline octave_value
make_idx_args (const std::string& type,
	       const std::list<octave_value_list>& idx,
	       const std::string& who)
{
  octave_value retval;

  size_t len = type.length ();

  if (len == idx.size ())
    {
      Cell type_field (len, 1);
      Cell subs_field (len, 1);

      std::list<octave_value_list>::const_iterator p = idx.begin ();

      for (size_t i = 0; i < len; i++)
	{
	  char t = type[i];

	  switch (t)
	    {
	    case '(':
	      type_field(i) = "()";
	      subs_field(i) = Cell (sanitize (*p++));
	      break;

	    case '{':
	      type_field(i) = "{}";
	      subs_field(i) = Cell (sanitize (*p++));
	      break;

	    case '.':
	      {
		type_field(i) = ".";

		octave_value_list vlist = *p++;

		if (vlist.length () == 1)
		  {
		    octave_value val = vlist(0);

		    if (val.is_string ())
		      subs_field(i) = val;
		    else
		      {
			error ("expecting character string argument for `.' index");
			return retval;
		      }
		  }
		else
		  {
		    error ("expecting single argument for `.' index");
		    return retval;
		  }
	      }
	      break;

	    default:
	      panic_impossible ();
	      break;
	    }
	}

      Octave_map m;

      m.assign ("type", type_field);
      m.assign ("subs", subs_field);

      retval = m;
    }
  else
    error ("invalid index for %s", who.c_str ());

  return retval;
}

octave_value_list
octave_class::subsref (const std::string& type,
		       const std::list<octave_value_list>& idx,
		       int nargout)
{
  octave_value_list retval;

  if (in_class_method ())
    {
      // FIXME -- this block of code is the same as the body of
      // octave_struct::subsref.  Maybe it could be shared instead of
      // duplicated.

      int skip = 1;

      switch (type[0])
	{
	case '(':
	  {
	    if (type.length () > 1 && type[1] == '.')
	      {
		std::list<octave_value_list>::const_iterator p = idx.begin ();
		octave_value_list key_idx = *++p;

		Cell tmp = dotref (key_idx);

		if (! error_state)
		  {
		    Cell t = tmp.index (idx.front ());

		    retval(0) = (t.length () == 1) ? t(0) : octave_value (t, true);

		    // We handled two index elements, so tell
		    // next_subsref to skip both of them.

		    skip++;
		  }
	      }
	    else
	      retval(0) = map.index (idx.front ());
	  }
	  break;

	case '.':
	  {
	    if (map.numel() > 0)
	      {
		Cell t = dotref (idx.front ());

		retval(0) = (t.length () == 1) ? t(0) : octave_value (t, true);
	      }
	  }
	  break;

	case '{':
	  gripe_invalid_index_type (type_name (), type[0]);
	  break;

	default:
	  panic_impossible ();
	}

      // FIXME -- perhaps there should be an
      // octave_value_list::next_subsref member function?  See also
      // octave_user_function::subsref.

      if (idx.size () > 1)
	retval = retval(0).next_subsref (nargout, type, idx, skip);
    }
  else
    {
      octave_value meth = symbol_table::find_method ("subsref", class_name ());

      if (meth.is_defined ())
	{
	  octave_value_list args;

	  args(1) = make_idx_args (type, idx, "subsref");

	  if (error_state)
	    return octave_value_list ();

	  count++;
	  args(0) = octave_value (this);

	  return feval (meth.function_value (), args, nargout);
	}
      else
	{
	  if (type.length () == 1 && type[0] == '(')
	    retval(0) = octave_value (map.index (idx.front ()), class_name ());
	  else
	    gripe_invalid_index ();
	}
    }

  return retval;
}

octave_value
octave_class::numeric_conv (const Cell& val, const std::string& type)
{
  octave_value retval;

  if (val.length () == 1)
    {
      retval = val(0);

      if (type.length () > 0 && type[0] == '.' && ! retval.is_map ())
	retval = Octave_map ();
    }
  else
    gripe_invalid_index_for_assignment ();

  return retval;
}

octave_value
octave_class::subsasgn (const std::string& type,
			const std::list<octave_value_list>& idx,
			const octave_value& rhs)
{
  octave_value retval;

  if (in_class_method ())
    {
      // FIXME -- this block of code is the same as the body of
      // octave_struct::subsasgn.  Maybe it could be shared instead of
      // duplicated.

      int n = type.length ();

      octave_value t_rhs = rhs;

      if (n > 1 && ! (type.length () == 2 && type[0] == '(' && type[1] == '.'))
	{
	  switch (type[0])
	    {
	    case '(':
	      {
		if (type.length () > 1 && type[1] == '.')
		  {
		    std::list<octave_value_list>::const_iterator p = idx.begin ();
		    octave_value_list t_idx = *p;

		    octave_value_list key_idx = *++p;

		    assert (key_idx.length () == 1);

		    std::string key = key_idx(0).string_value ();

		    octave_value u;

		    if (! map.contains (key))
		      u = octave_value::empty_conv (type.substr (2), rhs);
		    else
		      {
			Cell map_val = map.contents (key);

			Cell map_elt = map_val.index (idx.front (), true);

			u = numeric_conv (map_elt, type.substr (2));
		      }

		    if (! error_state)
		      {
			std::list<octave_value_list> next_idx (idx);

			// We handled two index elements, so subsasgn to
			// needs to skip both of them.

			next_idx.erase (next_idx.begin ());
			next_idx.erase (next_idx.begin ());

			u.make_unique ();

			t_rhs = u.subsasgn (type.substr (2), next_idx, rhs);
		      }
		  }
		else
		  gripe_invalid_index_for_assignment ();
	      }
	      break;

	    case '.':
	      {
		octave_value_list key_idx = idx.front ();

		assert (key_idx.length () == 1);

		std::string key = key_idx(0).string_value ();

		octave_value u;

		if (! map.contains (key))
		  u = octave_value::empty_conv (type.substr (1), rhs);
		else
		  {
		    Cell map_val = map.contents (key);

		    u = numeric_conv (map_val, type.substr (1));
		  }

		if (! error_state)
		  {
		    std::list<octave_value_list> next_idx (idx);

		    next_idx.erase (next_idx.begin ());

		    u.make_unique ();

		    t_rhs = u.subsasgn (type.substr (1), next_idx, rhs);
		  }
	      }
	      break;

	    case '{':
	      gripe_invalid_index_type (type_name (), type[0]);
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
		if (n > 1 && type[1] == '.')
		  {
		    std::list<octave_value_list>::const_iterator p = idx.begin ();
		    octave_value_list key_idx = *++p;

		    assert (key_idx.length () == 1);

		    std::string key = key_idx(0).string_value ();

		    if (! error_state)
		      {
			map.assign (idx.front (), key, t_rhs);

			if (! error_state)
			  {
			    count++;
			    retval = octave_value (this);
			  }
			else
			  gripe_failed_assignment ();
		      }
		    else
		      gripe_failed_assignment ();
		  }
		else
		  {
		    if (t_rhs.is_map())
		      {
			Octave_map rhs_map = t_rhs.map_value ();

			if (! error_state)
			  {
			    map.assign (idx.front (), rhs_map);

			    if (! error_state)
			      {
				count++;
				retval = octave_value (this);
			      }
			    else
			      gripe_failed_assignment ();
			  }
			else
			  error ("invalid class assignment");
		      }
		    else
		      {
			if (t_rhs.is_empty()) 
			  {
			    map.maybe_delete_elements (idx.front());

			    if (! error_state)
			      {
				count++;
				retval = octave_value (this);
			      }
			    else
			      gripe_failed_assignment ();
			  }
			else
			  error ("invalid class assignment");
		      }
		  }
	      }
	      break;

	    case '.':
	      {
		octave_value_list key_idx = idx.front ();

		assert (key_idx.length () == 1);

		std::string key = key_idx(0).string_value ();

		map.assign (key, t_rhs);

		if (! error_state)
		  {
		    count++;
		    retval = octave_value (this);
		  }
		else
		  gripe_failed_assignment ();
	      }
	      break;

	    case '{':
	      gripe_invalid_index_type (type_name (), type[0]);
	      break;

	    default:
	      panic_impossible ();
	    }
	}
      else
	gripe_failed_assignment ();
    }
  else
    {
      octave_value meth = symbol_table::find_method ("subsasgn", class_name ());

      if (meth.is_defined ())
	{
	  octave_value_list args;

	  args(2) = rhs;

	  args(1) = make_idx_args (type, idx, "subsasgn");

	  if (error_state)
	    return octave_value_list ();

	  count++;
	  args(0) = octave_value (this);

	  octave_value_list tmp = feval (meth.function_value (), args);

	  // FIXME -- should the subsasgn method be able to return
	  // more than one value?

	  if (tmp.length () > 1)
	    error ("expecting single return value from @%s/subsasgn",
		   class_name().c_str ());
	  else
	    retval = tmp(0);

	}
      else
	error ("no subsasgn method defined for class %s",
	       class_name().c_str ());
    }

  return retval;
}

size_t
octave_class::byte_size (void) const
{
  // Neglect the size of the fieldnames.

  size_t retval = 0;

  for (Octave_map::const_iterator p = map.begin (); p != map.end (); p++)
    {
      std::string key = map.key (p);

      octave_value val = octave_value (map.contents (p));

      retval += val.byte_size ();
    }

  return retval;
}

Octave_map
octave_class::map_value (void) const
{
  Octave_map retval;
  gripe_wrong_type_arg ("octave_class::map_value()", type_name ());
  return retval;
}

string_vector
octave_class::map_keys (void) const
{
  string_vector retval;
  gripe_wrong_type_arg ("octave_class::map_keys()", type_name ());
  return retval;
}

void
octave_class::print (std::ostream& os, bool) const
{
  print_raw (os);
}

void
octave_class::print_raw (std::ostream& os, bool) const
{
  unwind_protect::begin_frame ("octave_class_print");

  unwind_protect_int (Vstruct_levels_to_print);

  indent (os);
  os << "  <class " << class_name () << ">";
  newline (os);

  unwind_protect::run_frame ("octave_class_print");
}

bool
octave_class::print_name_tag (std::ostream& os, const std::string& name) const
{
  bool retval = false;

  indent (os);
  os << name << " =";
  newline (os);
  newline (os);

  return retval;
}

void
octave_class::print_with_name (std::ostream&, const std::string& name, 
			       bool) const
{
  if (! (evaluating_function_body && Vsilent_functions))
    {
      octave_value fcn = symbol_table::find_method ("display", class_name ());

      if (fcn.is_defined ())
	{
	  octave_value_list args;

	  args(0) = octave_value (clone (), 1);
      
	  string_vector arg_names (1);

	  arg_names[0] = name;

	  args.stash_name_tags (arg_names);

	  feval (fcn.function_value (), args);
	}
    }
}

bool 
octave_class::save_ascii (std::ostream&, bool&)
{
  gripe_wrong_type_arg ("octave_class::save_ascii()", type_name ());
  return false;
}

bool 
octave_class::load_ascii (std::istream&)
{
  gripe_wrong_type_arg ("octave_class::load_ascii()", type_name ());
  return false;
}

bool 
octave_class::save_binary (std::ostream&, bool&)
{
  gripe_wrong_type_arg ("octave_class::save_binary()", type_name ());
  return false;
}

bool 
octave_class::load_binary (std::istream&, bool,
				oct_mach_info::float_format)
{
  gripe_wrong_type_arg ("octave_class::load_binary()", type_name ());
  return false;
}

#if defined (HAVE_HDF5)

bool
octave_class::save_hdf5 (hid_t, const char *, bool)
{
  gripe_wrong_type_arg ("octave_class::save_binary()", type_name ());

  return false;
}

bool 
octave_class::load_hdf5 (hid_t, const char *, bool)
{
  gripe_wrong_type_arg ("octave_class::load_binary()", type_name ());

  return false;
}

#endif

#if 0
bool
octave_class::save_ascii (std::ostream& os, bool& infnan_warned)
{
  Octave_map m = map_value ();
  os << "# length: " << m.length () << "\n";

  Octave_map::iterator i = m.begin ();
  while (i != m.end ())
    {
      octave_value val = map.contents (i);

      bool b = save_ascii_data (os, val, m.key (i), infnan_warned, false, 0);
      
      if (! b)
	return os;

      i++;
    }

  return true;
}

bool 
octave_class::load_ascii (std::istream& is)
{
  octave_idx_type len = 0;
  bool success = true;

  if (extract_keyword (is, "length", len) && len >= 0)
    {
      if (len > 0)
	{
	  Octave_map m (map);

	  for (octave_idx_type j = 0; j < len; j++)
	    {
	      octave_value t2;
	      bool dummy;

	      // recurse to read cell elements
	      std::string nm
		= read_ascii_data (is, std::string (), dummy, t2, j);

	      if (!is)
		break;

	      Cell tcell = t2.is_cell () ? t2.cell_value () : Cell (t2);

	      if (error_state)
		{
		  error ("load: internal error loading class elements");
		  return false;
		}

	      m.assign (nm, tcell);
	    }

	  if (is) 
	    map = m;
	  else
	    {
	      error ("load: failed to load class");
	      success = false;
	    }
	}
      else if (len == 0 )
	map = Octave_map (dim_vector (1, 1));
      else
	panic_impossible ();
    }
  else {
    error ("load: failed to extract number of elements in class");
    success = false;
  }

  return success;
}

bool 
octave_class::save_binary (std::ostream& os, bool& save_as_floats)
{
  Octave_map m = map_value ();

  int32_t len = m.length();
  os.write (reinterpret_cast<char *> (&len), 4);
  
  Octave_map::iterator i = m.begin ();
  while (i != m.end ())
    {
      octave_value val = map.contents (i);

      bool b = save_binary_data (os, val, m.key (i), "", 0, save_as_floats);
      
      if (! b)
	return os;

      i++;
    }

  return true;
}

bool 
octave_class::load_binary (std::istream& is, bool swap,
			    oct_mach_info::float_format fmt)
{
  bool success = true;
  int32_t len;
  if (! is.read (reinterpret_cast<char *> (&len), 4))
    return false;
  if (swap)
    swap_bytes<4> (&len);

  if (len > 0)
    {
      Octave_map m (map);

      for (octave_idx_type j = 0; j < len; j++)
	{
	  octave_value t2;
	  bool dummy;
	  std::string doc;

	  // recurse to read cell elements
	  std::string nm = read_binary_data (is, swap, fmt, std::string (), 
					     dummy, t2, doc);

	  if (!is)
	    break;

	  Cell tcell = t2.is_cell () ? t2.cell_value () : Cell (t2);
 
	  if (error_state)
	    {
	      error ("load: internal error loading class elements");
	      return false;
	    }

	  m.assign (nm, tcell);
	}

      if (is) 
	map = m;
      else
	{
	  error ("load: failed to load class");
	  success = false;
	}
    }
  else if (len == 0 )
    map = Octave_map (dim_vector (1, 1));
  else
    panic_impossible ();

  return success;
}

#if defined (HAVE_HDF5)

bool
octave_class::save_hdf5 (hid_t loc_id, const char *name, bool save_as_floats)
{
  hid_t data_hid = -1;

  data_hid = H5Gcreate (loc_id, name, 0);
  if (data_hid < 0) return false;

  // recursively add each element of the class to this group
  Octave_map m = map_value ();
  Octave_map::iterator i = m.begin ();
  while (i != m.end ())
    {
      octave_value val = map.contents (i);

      bool retval2 = add_hdf5_data (data_hid, val, m.key (i), "", false, 
				    save_as_floats);

      if (! retval2)
	break;

      i++;
    }

  H5Gclose (data_hid);

  return true;
}

bool 
octave_class::load_hdf5 (hid_t loc_id, const char *name,
			  bool have_h5giterate_bug)
{
  bool retval = false;

  hdf5_callback_data dsub;

  herr_t retval2 = 0;
  Octave_map m (dim_vector (1, 1));
  int current_item = 0;
#ifdef HAVE_H5GGET_NUM_OBJS
  hsize_t num_obj = 0;
  hid_t group_id = H5Gopen (loc_id, name); 
  H5Gget_num_objs (group_id, &num_obj);
  H5Gclose (group_id);

  while (current_item < static_cast<int> (num_obj)
	 && (retval2 = H5Giterate (loc_id, name, &current_item,
				   hdf5_read_next_data, &dsub)) > 0)
#else
  while ((retval2 = H5Giterate (loc_id, name, &current_item,
				hdf5_read_next_data, &dsub)) > 0)
#endif
    {
      octave_value t2 = dsub.tc;

      Cell tcell = t2.is_cell () ? t2.cell_value () : Cell (t2);
 
      if (error_state)
	{
	  error ("load: internal error loading class elements");
	  return false;
	}

      m.assign (dsub.name, tcell);

      if (have_h5giterate_bug)
	current_item++;  // H5Giterate returned the last index processed
    }

  if (retval2 >= 0)
    {
      map = m;
      retval = true;
    }
  
  return retval;
}

#endif

#endif

mxArray *
octave_class::as_mxArray (void) const
{
  gripe_wrong_type_arg ("octave_class::as_mxArray ()", type_name ());

  return 0;
}

bool
octave_class::in_class_method (void) const
{
  octave_function *fcn = octave_call_stack::current ();

  return (fcn
	  && (fcn->is_class_method () || fcn->is_class_constructor ())
	  && fcn->dispatch_class () == class_name ());
}

DEFUN (class, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} class (@var{expr})\n\
@deftypefnx {Built-in Function} {} class (@var{s}, @var{id})\n\
\n\
Return the class of the expression @var{expr}, as a string or\n\
create a class object from the structure @var{s} with name @var{id}.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    retval = args(0).class_name ();
  else if (nargin == 2)
    {
      Octave_map m = args(0).map_value ();

      if (! error_state)
	{
	  std::string id = args(1).string_value ();

	  if (! error_state)
	    {
	      octave_function *fcn = octave_call_stack::caller ();

	      if (fcn && fcn->is_class_constructor ())
		retval = octave_value (new octave_class (m, id));
	      else
		error ("class: invalid call from outside class constructor");
	    }
	  else
	    error ("class: expecting character string as second argument");
	}
      else
	error ("class: expecting structure as first argument");
    }
  else
    print_usage ();

  return retval;
}


DEFUN (isobject, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isobject (@var{x})\n\
Return true if @var{x} is a class object.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_object ();
  else
    print_usage ();

  return retval;
}

DEFCMD (methods, args, nargout,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} methods (@var{x})\n\
@deftypefnx {Built-in Function} {} methods (\"classname\")\n\
Return a cell array containing the names of the methods for the\n\
object @var{x} or the named class.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      octave_value arg = args(0);

      std::string class_name;

      if (arg.is_object ())
	class_name = arg.class_name ();
      else if (arg.is_string ())
	class_name = arg.string_value ();
      else
	error ("methods: expecting object or class name as argument");

      if (! error_state)
	{
	  string_vector sv = load_path::methods (class_name);

	  if (nargout == 0)
	    {
	      octave_stdout << "Methods for class " << class_name << ":\n\n";

	      sv.list_in_columns (octave_stdout);

	      octave_stdout << std::endl;
	    }
	  else
	    retval = sv;
	}	  
    }
  else
    print_usage ();

  return retval;
}

static bool
is_built_in_class (const std::string& cn)
{
  static std::set<std::string> built_in_class_names;

  if (built_in_class_names.empty ())
    {
      built_in_class_names.insert ("double");
      built_in_class_names.insert ("single");
      built_in_class_names.insert ("cell");
      built_in_class_names.insert ("struct");
      built_in_class_names.insert ("logical");
      built_in_class_names.insert ("char");
      built_in_class_names.insert ("function handle");
      built_in_class_names.insert ("int8");
      built_in_class_names.insert ("uint8");
      built_in_class_names.insert ("int16");
      built_in_class_names.insert ("uint16");
      built_in_class_names.insert ("int32");
      built_in_class_names.insert ("uint32");
      built_in_class_names.insert ("int64");
      built_in_class_names.insert ("uint64");
    }

  return built_in_class_names.find (cn) != built_in_class_names.end ();
}

static void
set_class_relationship (const std::string& sup_class,
			const std::string& inf_class)
{
  // FIXME

  warning ("class precedence not implemented");
}

DEFUN (superiorto, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} superiorto (@var{class_name})\n\
When called from a class constructor, mark the object currently\n\
constructed as having a higher precedence than @var{class_name}.\n\
This function may only be called from a class constructor.\n\
@end deftypefn")
{
  octave_value retval;

  octave_function *fcn = octave_call_stack::caller ();

  if (fcn && fcn->is_class_constructor ())
    {
      if (args.length () == 1)
	{
	  std::string class_name = args(0).string_value ();

	  if (! error_state)
	    {
	      if (! is_built_in_class (class_name))
		{
		  std::string this_class_name = fcn->name ();

		  set_class_relationship (this_class_name, class_name);
		}
	      else
		{
		  // User defined classes always have higher precedence
		  // than built-in classes.
		}
	    }
	  else
	    error ("superiorto: expecting argument to be class name");
	}
      else
	print_usage ();
    }
  else
    error ("superiorto: invalid call from outside class constructor");

  return retval;
}

DEFUN (inferiorto, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} inferiorto (@var{class_name})\n\
When called from a class constructor, mark the object currently\n\
constructed as having a lower precedence than @var{class_name}.\n\
This function may only be called from a class constructor.\n\
@end deftypefn")
{
  octave_value retval;

  octave_function *fcn = octave_call_stack::caller ();

  if (fcn && fcn->is_class_constructor ())
    {
      if (args.length () == 1)
	{
	  std::string class_name = args(0).string_value ();

	  if (! error_state)
	    {
	      if (! is_built_in_class (class_name))
		{
		  std::string this_class_name = fcn->name ();

		  set_class_relationship (class_name, this_class_name);
		}
	      else
		error ("inferiorto: cannot give user-defined class lower precedence than built-in class");
	    }
	  else
	    error ("inferiorto: expecting argument to be class name");
	}
      else
	print_usage ();
    }
  else
    error ("inferiorto: invalid call from outside class constructor");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
