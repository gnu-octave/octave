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
Octave_map::contents (const std::string& k) const
{
  const_iterator p = seek (k);

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

Octave_map
Octave_map::reshape (const dim_vector& new_dims) const
{
  Octave_map retval;

  if (new_dims != dims ())
    {
      for (const_iterator p = begin (); p != end (); p++)
	retval.assign (key(p), contents(p).reshape (new_dims));

      retval.dimensions = new_dims;
    }
  else
    retval = *this;

  return retval;
}

int
Octave_map::numel (void) const
{
  int retval;

  if (empty ())
    retval = 0;
  else
    {
      Cell tmp = contents (begin ());
      retval = tmp.numel ();
    }

  return retval;
}

Octave_map 
Octave_map::resize (const dim_vector& dv) const
{
  Octave_map retval;

  if (dv != dims ())
    {
      for (const_iterator p = begin (); p != end (); p++)
	{
	  Cell tmp = contents(p);
	  tmp.resize(dv);
	  retval.assign (key(p), tmp);
	}
      
      retval.dimensions = dv;
    }
  else
    retval = *this;


  return retval;
}

Octave_map
Octave_map::concat (const Octave_map& rb, const Array<int>& ra_idx)
{
  Octave_map retval;

  if (length() == rb.length())
    {
      for (Octave_map::const_iterator pa = begin (); pa != end (); pa++)
	{
	  Octave_map::const_iterator pb = rb.seek (key(pa));

	  if (pb == rb.end ())
	    {
	      error ("field name mismatch in structure concatenation");
	      break;
	    }
	
	  retval.assign (key(pa),
			 contents(pa).insert (rb.contents(pb), ra_idx));
	}
    }
  else
    error ("field name mismatch in structure concatenation");

  return retval;
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
	  std::string k = t_keys[i];

	  Cell t_rhs = rhs.contents (k);

	  assign (idx, k, t_rhs);

	  if (error_state)
	    break;
	}
    }
  else
    error ("field name mismatch in structure assignment");

  return *this;
}

static dim_vector
common_size (const dim_vector& a, const dim_vector& b)
{
  dim_vector retval;

  int a_len = a.length ();
  int b_len = b.length ();

  int new_len = std::max (a_len, b_len);
  int min_len = std::min (a_len, b_len);

  retval.resize (new_len);

  for (int i = 0; i < min_len; i++)
    retval(i) = std::max (a(i), b(i));

  if (a_len < b_len)
    {
      for (int i = min_len; i < b_len; i++)
	retval(i) = b(i);
    }
  else if (a_len > b_len)
    {
      for (int i = min_len; i < a_len; i++)
	retval(i) = a(i);
    }

  return retval;
}

Octave_map&
Octave_map::assign (const octave_value_list& idx, const std::string& k,
		    const Cell& rhs)
{
  Cell tmp = map[k];

  octave_value fill_value = Matrix ();

  tmp.assign (idx, rhs, fill_value);

  if (! error_state)
    {
      dim_vector rhs_dims = tmp.dims ();

      dim_vector curr_dims = dims ();

      dim_vector new_dims = common_size (rhs_dims, curr_dims);

      if (new_dims != rhs_dims)
	{
	  tmp.resize (new_dims, fill_value);
	}
      else if (new_dims != curr_dims)
	{
	  for (iterator p = begin (); p != end (); p++)
	    contents(p).resize (rhs_dims, fill_value);
	}

      dimensions = new_dims;

      map[k] = tmp;
    }

  return *this;
}

Octave_map&
Octave_map::assign (const std::string& k, const octave_value& rhs)
{
  if (empty ())
    {
      map[k] = Cell (rhs);

      dimensions = dim_vector (1, 1);
    }
  else
    {
      dim_vector dv = dims ();

      if (dv.all_ones ())
	map[k] = Cell (rhs);
      else
	error ("invalid structure assignment");
    }

  return *this;
}

Octave_map&
Octave_map::assign (const std::string& k, const Cell& rhs)
{
  if (empty ())
    {
      map[k] = rhs;

      dimensions = rhs.dims ();
    }
  else
    {
      if (dims () == rhs.dims ())
	map[k] = rhs;
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

      retval.assign (key(p), tmp);
    }

  return error_state ? Octave_map () : retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
