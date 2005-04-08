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

#include "quit.h"

#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "ov-mapper.h"
#include "ov.h"
#include "toplev.h"
#include "unwind-prot.h"

DEFINE_OCTAVE_ALLOCATOR (octave_mapper);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_mapper,
				     "built-in mapper function",
				     "built-in mapper function");

static bool
any_element_less_than (const NDArray& a, double val)
{
  octave_idx_type len = a.length ();

  for (octave_idx_type i = 0; i < len; i++)
    {
      OCTAVE_QUIT;

      if (a(i) < val)
	return true;
    }

  return false;
}

static bool
any_element_greater_than (const NDArray& a, double val)
{
  octave_idx_type len = a.length ();

  for (octave_idx_type i = 0; i < len; i++)
    {
      OCTAVE_QUIT;

      if (a(i) > val)
	return true;
    }

  return false;
}

// In most cases, we could use the map member function from the NDArray
// classes, but as currently implemented, they don't allow us to
// detect errors and abort properly.  So use these macros to do the
// looping here instead.

#define MAPPER_LOOP_2(T, F, M, CONV, R) \
  do \
    { \
      octave_idx_type len = M.length (); \
 \
      T result (M.dims ()); \
 \
      for (octave_idx_type i = 0; i < len; i++) \
	{ \
          OCTAVE_QUIT; \
 \
	  result(i) = CONV (F (M(i))); \
 \
	  if (error_state) \
	    return retval; \
	} \
 \
      retval = R; \
    } \
  while (0)

#define MAPPER_LOOP_1(T, F, M, CONV) \
  MAPPER_LOOP_2 (T, F, M, CONV, result)

#define MAPPER_LOOP(T, F, M) \
  MAPPER_LOOP_1 (T, F, M, )

octave_value
octave_mapper::apply (const octave_value& arg) const
{
  octave_value retval;

  // XXX FIXME XXX -- is_real_type can return true.  Should it really
  // work that way?

  if (arg.is_real_type ()
      && (c_c_map_fcn || d_d_map_fcn || d_b_map_fcn)
      && ! (arg.is_string () && ch_map_fcn))
    {
      if (arg.is_scalar_type ())
	{
	  double d = arg.double_value ();

	  if (can_ret_cmplx_for_real && (d < lower_limit || d > upper_limit))
	    {
	      if (c_c_map_fcn)
		retval = c_c_map_fcn (Complex (d));
	      else
		error ("%s: unable to handle real arguments",
		       name().c_str ());
	    }
	  else if (d_d_map_fcn)
	    retval = d_d_map_fcn (d);
	  else if (d_b_map_fcn)
	    retval = d_b_map_fcn (d);
	  else
	    error ("%s: unable to handle real arguments",
		   name().c_str ());
	}
      else
	{
	  NDArray m = arg.array_value ();

	  if (error_state)
	    return retval;

	  if (can_ret_cmplx_for_real
	      && (any_element_less_than (m, lower_limit)
		  || any_element_greater_than (m, upper_limit)))
	    {
	      if (c_c_map_fcn)
		MAPPER_LOOP (ComplexNDArray, c_c_map_fcn, m);
	      else
		error ("%s: unable to handle real arguments",
		       name().c_str ());
	    }
	  else if (d_d_map_fcn)
	    MAPPER_LOOP (NDArray, d_d_map_fcn, m);
	  else if (d_b_map_fcn)
	    MAPPER_LOOP (boolNDArray, d_b_map_fcn, m);
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
	  else if (c_b_map_fcn)
	    retval = c_b_map_fcn (c);
	  else
	    error ("%s: unable to handle complex arguments",
		   name().c_str ());
	}
      else
	{
	  ComplexNDArray cm = arg.complex_array_value ();

	  if (error_state)
	    return retval;

	  if (d_c_map_fcn)
	    MAPPER_LOOP (NDArray, d_c_map_fcn, cm);
	  else if (c_c_map_fcn)
	    MAPPER_LOOP (ComplexNDArray, c_c_map_fcn, cm);
	  else if (c_b_map_fcn)
	    MAPPER_LOOP (boolNDArray, c_b_map_fcn, cm);
	  else
	    error ("%s: unable to handle complex arguments",
		   name().c_str ());
	}
    }
  else if (ch_map_fcn)
    {
      // XXX FIXME XXX -- this could be done in a better way...

      octave_value tmp = arg.convert_to_str ();

      if (! error_state)
	{
	  charNDArray chm = tmp.char_array_value ();

	  if (! error_state)
	    {
	      switch (ch_map_flag)
		{
		case 0:
		  MAPPER_LOOP_1 (boolNDArray, ch_map_fcn, chm, bool);
		  break;

		case 1:
		  MAPPER_LOOP (NDArray, ch_map_fcn, chm);
		  break;

		case 2:
		  MAPPER_LOOP_2 (charNDArray, ch_map_fcn, chm, ,
				 octave_value (result, true));
		  break;

		default:
		  panic_impossible ();
		  break;
		}
	    }
	}
    }
  else
    gripe_wrong_type_arg ("mapper", arg);

  return retval;
}

octave_value_list
octave_mapper::subsref (const std::string& type,
			const std::list<octave_value_list>& idx,
			int nargout)
{
  octave_value_list retval;

  switch (type[0])
    {
    case '(':
      {
	int tmp_nargout = (type.length () > 1 && nargout == 0) ? 1 : nargout;

	retval = do_multi_index_op (tmp_nargout, idx.front ());
      }
      break;

    case '{':
    case '.':
      {
	std::string nm = type_name ();
	error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
      }
      break;

    default:
      panic_impossible ();
    }

  // XXX FIXME XXX -- perhaps there should be an
  // octave_value_list::next_subsref member function?  See also
  // and octave_builtin::subsref.

  if (idx.size () > 1)
    retval = retval(0).next_subsref (nargout, type, idx);

  return retval;
}

octave_value_list
octave_mapper::do_multi_index_op (int, const octave_value_list& args)
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
	{
	  unwind_protect::begin_frame ("mapper_func_eval");

	  unwind_protect_ptr (curr_function);
	  unwind_protect_ptr (curr_caller_function);

	  curr_caller_function = curr_function;
	  curr_function = this;

	  retval = apply (args(0));

	  unwind_protect::run_frame ("mapper_func_eval");
	}
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
