/*

Copyright (C) 1995, 1996, 1997, 2002, 2003, 2004, 2005, 2006, 2007,
              2008, 2009 John W. Eaton

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

#include "error.h"
#include "str-vec.h"

#include "oct-map.h"
#include "utils.h"

Octave_map::Octave_map (const dim_vector& dv, const Cell& key_vals)
  : map (), key_list (), dimensions (dv)
{
  Cell c (dv);

  if (key_vals.is_cellstr ())
    {
      for (octave_idx_type i = 0; i < key_vals.numel (); i++)
	{
	  std::string k = key_vals(i).string_value ();
	  map[k] = c;
	  key_list.push_back (k);
	}
    }
  else
    error ("Octave_map: expecting keys to be cellstr");
}

Octave_map
Octave_map::squeeze (void) const
{
  Octave_map retval (dims ().squeeze ());

  for (const_iterator pa = begin (); pa != end (); pa++)
    {
      Cell tmp = contents (pa).squeeze ();

      if (error_state)
	break;

      retval.assign (key (pa), tmp);
    }

  // Preserve order of keys.
  retval.key_list = key_list;

  return retval;
}

/*
%!# test preservation of keys by squeeze
%!test
%!  x(1,1,1,1).d = 10; x(3,5,1,7).a = "b"; x(2,4,1,7).f = 27;
%!  assert (fieldnames (squeeze (x)), {"d"; "a"; "f"});
*/

Octave_map
Octave_map::permute (const Array<int>& vec, bool inv) const
{
  Octave_map retval (dims ());

  for (const_iterator pa = begin (); pa != end (); pa++)
    {
      Cell tmp = contents (pa).permute (vec, inv);

      if (error_state)
	break;

      retval.assign (key (pa), tmp);
    }

  // Preserve order of keys.
  retval.key_list = key_list;

  return retval;
}

/*
%!# test preservation of key order by permute
%!test
%!  x(1,1,1,1).d = 10; x(3,5,1,7).a = "b"; x(2,4,1,7).f = 27;
%!  assert (fieldnames (permute (x, [3, 4, 1, 2])), {"d"; "a"; "f"});
*/

Cell&
Octave_map::contents (const std::string& k)
{
  maybe_add_to_key_list (k);

  return map[k];
}

Cell
Octave_map::contents (const std::string& k) const
{
  const_iterator p = seek (k);

  return p != end () ? p->second : Cell ();
}

int
Octave_map::intfield (const std::string& k, int def_val) const
{
  int retval = def_val;

  Cell c = contents (k);

  if (! c.is_empty ())
    retval = c(0).int_value ();

  return retval;
}

std::string
Octave_map::stringfield (const std::string& k,
			 const std::string& def_val) const
{
  std::string retval = def_val;

  Cell c = contents (k);

  if (! c.is_empty ())
    retval = c(0).string_value ();

  return retval;
}

string_vector
Octave_map::keys (void) const
{
  assert (nfields () == key_list.size ());

  return string_vector (key_list);
}

Octave_map
Octave_map::transpose (void) const
{
  assert (ndims () == 2);

  dim_vector dv = dims ();

  octave_idx_type nr = dv(0);
  octave_idx_type nc = dv(1);

  dim_vector new_dims (nc, nr);

  Octave_map retval (new_dims);

  for (const_iterator p = begin (); p != end (); p++)
    retval.assign (key(p), Cell (contents(p).transpose ()));

  // Preserve order of keys.
  retval.key_list = key_list;

  return retval;
}

/*
%!# test preservation of key order by transpose
%!test
%!  x(1,1).d = 10; x(3,5).a = "b"; x(2,4).f = 27;
%!  assert (fieldnames (transpose (x)), {"d"; "a"; "f"});
%!  assert (fieldnames (x'), {"d"; "a"; "f"});
%!  assert (fieldnames (x.'), {"d"; "a"; "f"});
*/

Octave_map
Octave_map::reshape (const dim_vector& new_dims) const
{
  Octave_map retval;

  if (new_dims != dims ())
    {
      for (const_iterator p = begin (); p != end (); p++)
	retval.assign (key(p), contents(p).reshape (new_dims));

      retval.dimensions = new_dims;

      // Preserve order of keys.
      retval.key_list = key_list;
    }
  else
    retval = *this;

  return retval;
}

/*
%!# test preservation of key order by reshape
%!test
%!  x(1,1).d = 10; x(4,6).a = "b"; x(2,4).f = 27;
%!  assert (fieldnames (reshape (x, 3, 8)), {"d"; "a"; "f"});
*/

void
Octave_map::resize (const dim_vector& dv, bool fill)
{
  if (dv != dims ())
    {
      if (nfields () == 0)
	dimensions = dv;
      else
	{
	  for (const_iterator p = begin (); p != end (); p++)
	    {
	      Cell tmp = contents(p);

	      if (fill)
		tmp.resize (dv, Cell::resize_fill_value ());
	      else
		tmp.resize (dv);

	      dimensions = dv;

	      assign (key(p), tmp);
	    }
	}
    }
}

Octave_map
Octave_map::concat (const Octave_map& rb, const Array<octave_idx_type>& ra_idx)
{
  Octave_map retval;

  if (nfields () == rb.nfields ())
    {
      for (const_iterator pa = begin (); pa != end (); pa++)
	{
	  const_iterator pb = rb.seek (key(pa));

	  if (pb == rb.end ())
	    {
	      error ("field name mismatch in structure concatenation");
	      break;
	    }
	
	  retval.assign (key(pa),
			 contents(pa).insert (rb.contents(pb), ra_idx));
	}

      // Preserve order of keys.
      retval.key_list = key_list;
    }
  else
    {
      dim_vector dv = dims ();

      if (dv.all_zero ())
	retval = rb;
      else
	{
	  dv = rb.dims ();

	  if (dv.all_zero ())
	    retval = *this;
	  else
	    error ("invalid structure concatenation");
	}
    }

  return retval;
}

/*
%!# test preservation of key order by concatenation
%!test
%!  x(1, 1).d = 10; x(4, 6).a = "b"; x(2, 4).f = 27;
%!  y(1, 6).f = 11; y(1, 6).a = "c"; y(1, 6).d = 33;
%!  assert (fieldnames ([x; y]), {"d"; "a"; "f"});
*/

static bool
keys_ok (const Octave_map& a, const Octave_map& b, string_vector& keys)
{
  bool retval = false;

  keys = string_vector ();

  if (a.nfields () == 0)
    {
      keys = b.keys ();
      retval = true;
    }
  else
    {
      string_vector a_keys = a.keys().sort ();
      string_vector b_keys = b.keys().sort ();

      octave_idx_type a_len = a_keys.length ();
      octave_idx_type b_len = b_keys.length ();

      if (a_len == b_len)
	{
	  for (octave_idx_type i = 0; i < a_len; i++)
	    {
	      if (a_keys[i] != b_keys[i])
		goto done;
	    }

	  keys = a_keys;
	  retval = true;
	}
    }

 done:
  return retval;
}

Octave_map&
Octave_map::maybe_delete_elements (const octave_value_list& idx)
{
  string_vector t_keys = keys();
  octave_idx_type len = t_keys.length ();

  if (len > 0)
    {
      for (octave_idx_type i = 0; i < len; i++)
	{
	  std::string k = t_keys[i];

	  map[k] = contents(k).delete_elements (idx);

	  if (error_state)
	    break;
	}

      if (!error_state)
	dimensions = contents(t_keys[0]).dims();
    }

  return *this;
}

Octave_map&
Octave_map::assign (const octave_value_list& idx, const Octave_map& rhs)
{
  string_vector t_keys;

  if (keys_ok (*this, rhs, t_keys))
    {
      octave_idx_type len = t_keys.length ();

      if (len == 0)
	{
	  Cell tmp_lhs (dims ());
	  Cell tmp_rhs (rhs.dims ());

	  tmp_lhs.assign (idx, tmp_rhs, Matrix ());

	  if (! error_state)
	    resize (tmp_lhs.dims ());
	  else
	    error ("size mismatch in structure assignment");
	}
      else
	{
	  for (octave_idx_type i = 0; i < len; i++)
	    {
	      std::string k = t_keys[i];

	      Cell t_rhs = rhs.contents (k);

	      assign (idx, k, t_rhs);

	      if (error_state)
		break;
	    }
	}
    }
  else
    error ("field name mismatch in structure assignment");

  return *this;
}

Octave_map&
Octave_map::assign (const octave_value_list& idx, const std::string& k,
		    const Cell& rhs)
{
  Cell tmp;

  if (contains (k))
    tmp = map[k];
  else
    tmp = Cell (dimensions);

  tmp.assign (idx, rhs);

  if (! error_state)
    {
      dim_vector tmp_dims = tmp.dims ();

      if (tmp_dims != dimensions)
	{
	  for (iterator p = begin (); p != end (); p++)
	    contents(p).resize (tmp_dims, Cell::resize_fill_value ());

          dimensions = tmp_dims;
	}

      maybe_add_to_key_list (k);

      map[k] = tmp;
    }

  return *this;
}

Octave_map&
Octave_map::assign (const std::string& k, const octave_value& rhs)
{
  if (nfields () == 0)
    {
      maybe_add_to_key_list (k);

      map[k] = Cell (rhs);

      dimensions = dim_vector (1, 1);
    }
  else
    {
      dim_vector dv = dims ();

      if (dv.all_ones ())
	{
	  maybe_add_to_key_list (k);

	  map[k] = Cell (rhs);
	}
      else
	error ("invalid structure assignment");
    }

  return *this;
}

Octave_map&
Octave_map::assign (const std::string& k, const Cell& rhs)
{
  if (nfields () == 0)
    {
      maybe_add_to_key_list (k);

      map[k] = rhs;

      dimensions = rhs.dims ();
    }
  else
    {
      if (dims () == rhs.dims ())
	{
	  maybe_add_to_key_list (k);

	  map[k] = rhs;
	}
      else
	error ("invalid structure assignment");
    }

  return *this;
}

Octave_map
Octave_map::index (const octave_value_list& idx, bool resize_ok) const
{
  Octave_map retval;

  octave_idx_type n_idx = idx.length ();

  if (n_idx > 0)
    {
      Array<idx_vector> ra_idx (n_idx);

      for (octave_idx_type i = 0; i < n_idx; i++)
        {
          ra_idx(i) = idx(i).index_vector ();
          if (error_state)
            break;
        }

      if (! error_state)
        {
          for (const_iterator p = begin (); p != end (); p++)
            {
              Cell tmp = contents (p);

              tmp = tmp.Array<octave_value>::index (ra_idx, resize_ok);

              if (error_state)
                break;

              retval.assign (key(p), tmp);
            }

          // Preserve order of keys.
          retval.key_list = key_list;
        }
    }
  else
    retval = *this;

  return retval;
}

/*
%!# test preservation of key order by indexing
%!test
%!  x(1, 1).d = 10; x(4, 6).a = "b"; x(2, 4).f = 27;
%!  assert (fieldnames (x([1, 2], [2:5])), {"d"; "a"; "f"});
*/
