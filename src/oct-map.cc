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

#include "error.h"
#include "str-vec.h"

#include "oct-map.h"
#include "utils.h"

Cell
Octave_map::operator [] (const std::string& key) const
{
  const_iterator p = seek (key);

  return p != end () ? p->second : Cell ();
}

string_vector
Octave_map::keys (void) const
{
  int len = length ();

  string_vector names (len);

  int i = 0;
  for (const_iterator p = begin (); p != end (); p++)
    names[i++] = key (p);

  return names;
}

int
Octave_map::array_length (void) const
{
  if (array_len == 0 && length () != 0)
    {
      const_iterator p = begin ();
      array_len = contents(p).length ();
    }

  return array_len;
}

static string_vector
equiv_keys (const Octave_map& a, const Octave_map& b)
{
  string_vector retval;

  string_vector a_keys = a.keys().qsort ();
  string_vector b_keys = b.keys().qsort ();

  int a_len = a_keys.length ();
  int b_len = b_keys.length ();

  if (a_len == b_len)
    {
      for (int i = 0; i < a_len; i++)
	{
	  if (a_keys[i] != b_keys[i])
	    return retval;
	}

      retval = a_keys;
    }
  
  return retval;
}

Octave_map&
Octave_map::assign (const octave_value_list& idx, const Octave_map& rhs)
{
  string_vector t_keys = empty () ? rhs.keys () : equiv_keys (*this, rhs);

  if (! t_keys.empty ())
    {
      int len = t_keys.length ();

      for (int i = 0; i < len; i++)
	{
	  std::string key = t_keys[i];

	  Cell t_rhs = rhs[key];

	  assign (idx, key, t_rhs);

	  if (error_state)
	    break;
	}
    }
  else
    error ("field name mismatch in structure assignment");

  return *this;
}

Octave_map&
Octave_map::assign (const octave_value_list& idx, const std::string& key,
		    const Cell& rhs)
{
  Cell tmp = map[key];

  octave_value fill_value = Matrix ();

  tmp.assign (idx, rhs, fill_value);

  if (! error_state)
    {
      int rhs_len = tmp.length ();

      int len = array_length ();

      if (rhs_len < len)
	{
	  tmp.resize_and_fill (len, fill_value);
	}
      else if (rhs_len > len)
	{
	  for (iterator p = begin (); p != end (); p++)
	    contents(p).resize_and_fill (rhs_len, fill_value);

	  array_len = rhs_len;
	}

      map[key] = tmp;
    }

  return *this;
}

Octave_map&
Octave_map::assign (const std::string& key, const Cell& rhs)
{
  if (empty ())
    map[key] = rhs;
  else
    {
      Cell tmp = contents (begin ());

      if (tmp.length () == rhs.length ())
	map[key] = rhs;
      else
	error ("invalid structure assignment");
    }

  return *this;
}

Octave_map
Octave_map::index (const octave_value_list& idx)
{
  Octave_map retval;

  for (iterator p = begin (); p != end (); p++)
    {
      Cell tmp = contents(p).index (idx);

      if (error_state)
	break;

      retval[key(p)] = tmp;
    }

  return error_state ? Octave_map () : retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
