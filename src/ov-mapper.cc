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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "ov-mapper.h"
#include "ov.h"

octave_allocator
octave_mapper::allocator (sizeof (octave_mapper));

int
octave_mapper::t_id (-1);

const string
octave_mapper::t_name ("built-in mapper function");

static bool
any_element_less_than (const Matrix& a, double val)
{
  int nr = a.rows ();
  int nc = a.columns ();

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      if (a (i, j) < val)
	return true;

  return false;
}

static bool
any_element_greater_than (const Matrix& a, double val)
{
  int nr = a.rows ();
  int nc = a.columns ();

  for (int j = 0; j < nc; j++)
    for (int i = 0; i < nr; i++)
      if (a (i, j) > val)
	return true;

  return false;
}

octave_value
octave_mapper::apply (const octave_value& arg) const
{
  octave_value retval;

  if (ch_map_fcn)
    {
      // XXX FIXME XXX -- this could be done in a better way...

      octave_value tmp = arg.convert_to_str ();

      if (! error_state)
	{
	  charMatrix chm = tmp.char_matrix_value ();

	  if (! error_state)
	    {
	      int nr = chm.rows ();
	      int nc = chm.cols ();

	      switch (flag)
		{
		case 0:
		  {
		    Matrix result (nr, nc);

		    // islapha and friends can return any nonzero value
		    // to mean true, but we want to return 1 or 0 only.

		    for (int j = 0; j < nc; j++)
		      for (int i = 0; i < nr; i++)
			result (i, j) = ch_map_fcn (chm (i, j)) ? 1 : 0;

		    retval = result;
		  }
		  break;

		case 1:
		  {
		    Matrix result (nr, nc);

		    for (int j = 0; j < nc; j++)
		      for (int i = 0; i < nr; i++)
			result (i, j) = ch_map_fcn (chm (i, j));

		    retval = result;
		  }
		  break;

		case 2:
		  {
		    charMatrix result (nr, nc);

		    for (int j = 0; j < nc; j++)
		      for (int i = 0; i < nr; i++)
			result (i, j) = ch_map_fcn (chm (i, j));

		    retval = octave_value (result, true);
		  }
		  break;

		default:
		  panic_impossible ();
		  break;
		}
	    }
	}
    }
  else
    {
      if (arg.is_real_type ())
	{
	  if (arg.is_scalar_type ())
	    {
	      double d = arg.double_value ();

	      if (flag && (d < lower_limit || d > upper_limit))
		{
		  if (c_c_map_fcn)
		    retval = c_c_map_fcn (Complex (d));
		  else
		    error ("%s: unable to handle real arguments",
			   name().c_str ());
		}
	      else if (d_d_map_fcn)
		retval = d_d_map_fcn (d);
	      else
		error ("%s: unable to handle real arguments",
		       name().c_str ());
	    }
	  else
	    {
	      Matrix m = arg.matrix_value ();

	      if (error_state)
		return retval;

	      if (flag
		  && (any_element_less_than (m, lower_limit)
		      || any_element_greater_than (m, upper_limit)))
		{
		  if (c_c_map_fcn)
		    {
		      ComplexMatrix cm (m);
		      retval = cm.map (c_c_map_fcn);
		    }
		  else
		    error ("%s: unable to handle real arguments",
			   name().c_str ());
		}
	      else if (d_d_map_fcn)
		retval = m.map (d_d_map_fcn);
	      else
		error ("%s: unable to handle real arguments",
		       name().c_str ());
	    }
	}
      else if (arg.is_complex_type ())
	{
	  if (arg.is_scalar_type ())
	    {
	      Complex c = arg.complex_value ();

	      if (d_c_map_fcn)
		retval = d_c_map_fcn (c);
	      else if (c_c_map_fcn)
		retval = c_c_map_fcn (c);
	      else
		error ("%s: unable to handle complex arguments",
		       name().c_str ());
	    }
	  else
	    {
	      ComplexMatrix cm = arg.complex_matrix_value ();

	      if (error_state)
		return retval;

	      if (d_c_map_fcn)
		retval = cm.map (d_c_map_fcn);
	      else if (c_c_map_fcn)
		retval = cm.map (c_c_map_fcn);
	      else
		error ("%s: unable to handle complex arguments",
		       name().c_str ());
	    }
	}
      else
	gripe_wrong_type_arg ("mapper", arg);
    }

  return retval;
}

octave_value_list
octave_mapper::do_index_op (int, const octave_value_list& args)
{
  octave_value retval;

  if (error_state)
    return retval;

  int nargin = args.length ();

  if (nargin > 1)
    ::error ("%s: too many arguments", name().c_str ());
  else if (nargin < 1)
    ::error ("%s: too few arguments", name().c_str ());
  else
    {
      if (args(0).is_defined ())
	retval = apply (args(0));
      else
	::error ("%s: argument undefined", name().c_str ());
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
