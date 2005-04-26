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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <ctime>

#include <string>

#include "f77-fcn.h"
#include "lo-mappers.h"
#include "oct-rand.h"
#include "quit.h"

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "unwind-prot.h"
#include "utils.h"

static octave_value
do_rand (const octave_value_list& args, int nargin, const char *fcn)
{
  octave_value retval;

  dim_vector dims;

  switch (nargin)
    {
    case 0:
      {
	dims.resize (2);

	dims(0) = 1;
	dims(1) = 1;

	goto gen_matrix;
      }
      break;

    case 1:
      {
	octave_value tmp = args(0);

	if (tmp.is_string ())
	  {
	    std::string s_arg = tmp.string_value ();

	    if (s_arg == "dist")
	      {
		retval = octave_rand::distribution ();
	      }
	    else if (s_arg == "seed" || s_arg == "state")
	      {
		retval = octave_rand::seed ();
	      }
	    else if (s_arg == "uniform")
	      {
		octave_rand::uniform_distribution ();
	      }
	    else if (s_arg == "normal")
	      {
		octave_rand::normal_distribution ();
	      }
	    else
	      error ("%s: unrecognized string argument", fcn);
	  }
	else if (tmp.is_scalar_type ())
	  {
	    double dval = tmp.double_value ();

	    if (xisnan (dval))
	      {
		error ("%s: NaN is invalid a matrix dimension", fcn);
	      }
	    else
	      {
		dims.resize (2);

		dims(0) = NINTbig (tmp.double_value ());
		dims(1) = NINTbig (tmp.double_value ());

		if (! error_state)
		  goto gen_matrix;
	      }
	  }
	else if (tmp.is_range ())
	  {
	    Range r = tmp.range_value ();

	    if (r.all_elements_are_ints ())
	      {
		octave_idx_type n = r.nelem ();

		dims.resize (n);

		octave_idx_type base = NINTbig (r.base ());
		octave_idx_type incr = NINTbig (r.inc ());
		octave_idx_type lim = NINTbig (r.limit ());

		if (base < 0 || lim < 0)
		  error ("%s: all dimensions must be nonnegative", fcn);
		else
		  {
		    for (octave_idx_type i = 0; i < n; i++)
		      {
			dims(i) = base;
			base += incr;
		      }

		    goto gen_matrix;
		  }
	      }
	    else
	      error ("%s: expecting all elements of range to be integers",
		     fcn);
	  }
	else if (tmp.is_matrix_type ())
	  {
	    Array<int> iv = tmp.int_vector_value (true);

	    if (! error_state)
	      {
		octave_idx_type len = iv.length ();

		dims.resize (len);

		for (octave_idx_type i = 0; i < len; i++)
		  {
		    octave_idx_type elt = iv(i);

		    if (elt < 0)
		      {
			error ("%s: all dimensions must be nonnegative", fcn);
			goto done;
		      }

		    dims(i) = iv(i);
		  }

		goto gen_matrix;
	      }
	    else
	      error ("%s: expecting integer vector", fcn);
	  }
	else
	  {
	    gripe_wrong_type_arg ("rand", tmp);
	    return retval;
	  }
      }
      break;

    default:
      {
	octave_value tmp = args(0);

	if (nargin == 2 && tmp.is_string ())
	  {
	    std::string ts = tmp.string_value ();

	    if (ts == "seed" || ts == "state")
	      {
		double d = args(1).double_value ();

		if (! error_state)
		  octave_rand::seed (d);
	      }
	    else
	      error ("%s: unrecognized string argument", fcn);
	  }
	else
	  {
	    dims.resize (nargin);

	    for (int i = 0; i < nargin; i++)
	      {
		dims(i) = (octave_idx_type)args(i).int_value ();

		if (error_state)
		  {
		    error ("%s: expecting integer arguments", fcn);
		    goto done;
		  }
	      }

	    goto gen_matrix;
	  }
      }
      break;
    }

 done:

  return retval;

 gen_matrix:

  return octave_rand::nd_array (dims);
}

DEFUN_DLD (rand, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} rand (@var{x})\n\
@deftypefnx {Loadable Function} {} rand (@var{n}, @var{m})\n\
@deftypefnx {Loadable Function} {} rand (@code{\"seed\"}, @var{x})\n\
Return a matrix with random elements uniformly distributed on the\n\
interval (0, 1).  The arguments are handled the same as the arguments\n\
for @code{eye}.  In\n\
addition, you can set the seed for the random number generator using the\n\
form\n\
\n\
@example\n\
rand (\"seed\", @var{x})\n\
@end example\n\
\n\
@noindent\n\
where @var{x} is a scalar value.  If called as\n\
\n\
@example\n\
rand (\"seed\")\n\
@end example\n\
\n\
@noindent\n\
@code{rand} returns the current value of the seed.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  retval = do_rand (args, nargin, "rand");

  return retval;
}

static std::string current_distribution = octave_rand::distribution ();

static void
reset_rand_generator (void *)
{
  octave_rand::distribution (current_distribution);
}

DEFUN_DLD (randn, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} randn (@var{x})\n\
@deftypefnx {Loadable Function} {} randn (@var{n}, @var{m})\n\
@deftypefnx {Loadable Function} {} randn (@code{\"seed\"}, @var{x})\n\
Return a matrix with normally distributed random elements.  The\n\
arguments are handled the same as the arguments for @code{eye}.  In\n\
addition, you can set the seed for the random number generator using the\n\
form\n\
\n\
@example\n\
randn (\"seed\", @var{x})\n\
@end example\n\
\n\
@noindent\n\
where @var{x} is a scalar value.  If called as\n\
\n\
@example\n\
randn (\"seed\")\n\
@end example\n\
\n\
@noindent\n\
@code{randn} returns the current value of the seed.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  unwind_protect::begin_frame ("randn");

  // This relies on the fact that elements are popped from the unwind
  // stack in the reverse of the order they are pushed
  // (i.e. current_distribution will be reset before calling
  // reset_rand_generator()).

  unwind_protect::add (reset_rand_generator, 0);
  unwind_protect_str (current_distribution);

  current_distribution = "normal";

  octave_rand::distribution (current_distribution);

  retval = do_rand (args, nargin, "randn");

  unwind_protect::run_frame ("randn");

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
