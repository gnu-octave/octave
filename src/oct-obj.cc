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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "error.h"
#include "oct-obj.h"

octave_allocator
octave_value_list::allocator (sizeof (octave_value_list));

bool
octave_value_list::valid_scalar_indices (void) const
{
  int n = length ();

  for (int i = 0; i < n; i++)
    if (! data[i].valid_as_scalar_index ())
      return false;

  return true;
}

void
octave_value_list::resize (int n, const octave_value& val)
{
  int len = length ();

  if (n > len)
    {
      data.resize (n);

      for (int i = len; i < n; i++)
	data[i] = val;
    }
  else if (n < len)
    data.resize (n);
}

octave_value_list&
octave_value_list::prepend (const octave_value& val)
{
  int n = length ();

  resize (n + 1);

  while (n > 0)
    {
      elem (n) = elem (n - 1);
      n--;
    }

  elem (0) = val;
  
  return *this;
}

octave_value_list&
octave_value_list::append (const octave_value& val)
{
  int n = length ();

  resize (n + 1);

  elem (n) = val;

  return *this;
}

octave_value_list&
octave_value_list::append (const octave_value_list& lst)
{
  int len = length ();
  int lst_len = lst.length ();

  resize (len + lst_len);

  for (int i = 0; i < lst_len; i++)
    elem (len + i) = lst (i);

  return *this;
}

octave_value_list&
octave_value_list::reverse (void)
{
  int n = length ();

  for (int i = 0; i < n / 2; i++)
    {
      octave_value tmp = elem (i);
      elem (i) = elem (n - i - 1);
      elem (n - i - 1) = tmp;
    }

  return *this;
}

octave_value_list
octave_value_list::splice (int offset, int rep_length,
			   const octave_value_list& lst) const
{ 
  octave_value_list retval;

  int len = length ();

  if (offset < 0 || offset >= len)
    {
      if (! (rep_length == 0 && offset == len))
	{
	  error ("octave_value_list::splice: invalid OFFSET");
	  return retval;
	}
    }

  if (rep_length < 0 || rep_length + offset > len)
    {
      error ("octave_value_list::splice: invalid LENGTH");
      return retval;
    }

  int lst_len = lst.length ();

  int new_len = len - rep_length + lst_len;

  retval.resize (new_len);

  int k = 0;

  for (int i = 0; i < offset; i++)
    retval(k++) = elem (i);

  for (int i = 0; i < lst_len; i++)
    retval(k++) = lst(i);

  for (int i = offset + rep_length; i < len; i++)
    retval(k++) = elem (i);

  return retval;
}

bool
octave_value_list::all_strings_p (void) const
{
  int n = length ();

  for (int i = 0; i < n; i++)
    if (! elem(i).is_string ())
      return 0;

  return 1;
}

string_vector
octave_value_list::make_argv (const std::string& fcn_name) const
{
  string_vector argv;

  if (all_strings_p ())
    {
      int len = length ();

      int total_nr = 0;

      for (int i = 0; i < len; i++)
	{
	  // An empty std::string ("") has zero columns and zero rows (a
	  // change that was made for Matlab contemptibility.

	  int n = elem(i).rows ();

	  total_nr += n ? n : 1;
	}

      argv.resize (total_nr+1);

      argv[0] = fcn_name;

      int k = 1;
      for (int i = 0; i < len; i++)
	{
	  int nr = elem(i).rows ();

	  if (nr < 2)
	    argv[k++] = elem(i).string_value ();
	  else
	    {
	      string_vector tmp = elem(i).all_strings ();

	      for (int j = 0; j < nr; j++)
		argv[k++] = tmp[j];
	    }
	}
    }
  else
    error ("%s: expecting all arguments to be strings", fcn_name.c_str ());

  return argv;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
