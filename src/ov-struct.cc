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

#include "Cell.h"
#include "error.h"
#include "oct-lvalue.h"
#include "ov-list.h"
#include "ov-struct.h"
#include "unwind-prot.h"
#include "variables.h"

DEFINE_OCTAVE_ALLOCATOR(octave_struct);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA(octave_struct, "struct");

octave_value_list
octave_struct::dotref (const octave_value_list& idx)
{
  octave_value_list retval;

  assert (idx.length () == 1);

  std::string nm = idx(0).string_value ();

  Octave_map::const_iterator p = map.seek (nm);

  if (p != map.end ())
    retval = map.contents (p);
  else
    error ("structure has no member `%s'", nm.c_str ());

  return retval;
}

static void
gripe_invalid_index (void)
{
  error ("invalid index for structure array");
}

static void
gripe_invalid_index_for_assignment (void)
{
  error ("invalid index for structure array assignment");
}

static void
gripe_invalid_index_type (const std::string& nm, char t)
{
  error ("%s cannot be indexed with %c", nm.c_str (), t);
}

static void
gripe_failed_assignment (void)
{
  error ("assignment to structure element failed");
}

octave_value
octave_struct::subsref (const std::string& type,
			const std::list<octave_value_list>& idx)
{
  octave_value retval;

  int skip = 1;

  switch (type[0])
    {
    case '(':
      {
	if (type.length () > 1 && type[1] == '.')
	  {
	    std::list<octave_value_list>::const_iterator p = idx.begin ();
	    octave_value_list key_idx = *++p;

	    octave_value_list tmp = dotref (key_idx);

	    if (! error_state)
	      {
		octave_value_list t_idx = idx.front ();

		if (t_idx.length () == 1)
		  {
		    idx_vector i = t_idx(0).index_vector ();
		    octave_value_list t = tmp.index (i);

		    retval = (t.length () == 1) ? t(0) : octave_value (t);

		    // We handled two index elements, so tell
		    // next_subsref to skip both of them.

		    skip++;
		  }
		else
		  gripe_invalid_index ();
	      }
	  }
	else
	  {
	    octave_value_list t_idx = idx.front ();

	    if (t_idx.length () == 1)
	      {
		idx_vector i = t_idx(0).index_vector ();
		retval = map.index (i);
	      }
	    else
	      gripe_invalid_index ();
	  }
      }
      break;

    case '.':
      {
	octave_value_list t = dotref (idx.front ());

	retval = (t.length () == 1) ? t(0) : octave_value (t);
      }
      break;

    case '{':
      gripe_invalid_index_type (type_name (), type[0]);
      break;

    default:
      panic_impossible ();
    }

  if (! error_state)
    retval = retval.next_subsref (type, idx, skip);

  return retval;
}

octave_value
octave_struct::numeric_conv (const octave_value_list& val,
			     const std::string& type)
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
octave_struct::subsasgn (const std::string& type,
			 const std::list<octave_value_list>& idx,
			 const octave_value& rhs)
{
  octave_value retval;

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

		if (t_idx.length () == 1)
		  {
		    octave_value_list key_idx = *++p;

		    assert (key_idx.length () == 1);

		    std::string key = key_idx(0).string_value ();

		    octave_value u;

		    if (! map.contains (key))
		      u = octave_value::empty_conv (type.substr (2), rhs);
		    else
		      {
			octave_value_list map_val = map[key];

			octave_value_list t_idx = idx.front ();

			idx_vector i = t_idx(0).index_vector ();

			octave_value_list map_elt = map_val.index (i, true);

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
		octave_value_list map_val = map[key];

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
		    octave_value_list t_idx = idx.front ();

		    if (t_idx.length () == 1)
		      {
			idx_vector i = t_idx(0).index_vector ();

			map.assign (i, key, t_rhs);

			if (! error_state)
			  retval = octave_value (this, count + 1);
			else
			  gripe_failed_assignment ();
		      }
		    else
		      gripe_invalid_index_for_assignment ();
		  }
		else
		  gripe_failed_assignment ();
	      }
	    else
	      {
		octave_value_list t_idx = idx.front ();

		if (t_idx.length () == 1)
		  {
		    idx_vector i = t_idx(0).index_vector ();

		    Octave_map rhs_map = t_rhs.map_value ();

		    if (! error_state)
		      {
			map.assign (i, rhs_map);

			if (! error_state)
			  retval = octave_value (this, count + 1);
			else
			  gripe_failed_assignment ();
		      }
		    else
		      error ("invalid structure assignment");
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
	      retval = octave_value (this, count + 1);
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

  return retval;
}

void
octave_struct::print (std::ostream& os, bool) const
{
  print_raw (os);
}

void
octave_struct::print_raw (std::ostream& os, bool) const
{
  // XXX FIXME XXX -- would be nice to print the output in some
  // standard order.  Maybe all substructures first, maybe
  // alphabetize entries, etc.

  unwind_protect::begin_frame ("octave_struct_print");

  unwind_protect_int (Vstruct_levels_to_print);

  if (Vstruct_levels_to_print >= 0)
    {
      bool print_keys_only = (Vstruct_levels_to_print == 0);

      Vstruct_levels_to_print--;

      indent (os);
      os << "{";
      newline (os);

      increment_indent_level ();

      int n = map.array_length ();

      for (Octave_map::const_iterator p = map.begin (); p != map.end (); p++)
	{
	  std::string key = map.key (p);
	  octave_value_list val = map.contents (p);

	  octave_value tmp = (n == 1) ? val(0) : octave_value (val);

	  if (print_keys_only)
	    {
	      indent (os);
	      os << key << ": " << tmp.type_name ();
	      newline (os);
	    }
	  else
	    tmp.print_with_name (os, key);
	}

      decrement_indent_level ();

      indent (os);
      os << "}";
      newline (os);
    }
  else
    {
      indent (os);
      os << "<structure>";
      newline (os);
    }

  unwind_protect::run_frame ("octave_struct_print");
}

bool
octave_struct::print_name_tag (std::ostream& os, const std::string& name) const
{
  bool retval = false;

  indent (os);

  if (Vstruct_levels_to_print < 0)
    os << name << " = ";
  else
    {
      os << name << " =";
      newline (os);
      retval = true;
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
