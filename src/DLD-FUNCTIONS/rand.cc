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
do_rand (const octave_value_list& args, int nargin)
{
  octave_value retval;

  volatile int n = 0;
  volatile int m = 0;

  if (nargin == 0)
    {
      n = 1;
      m = 1;

      goto gen_matrix;
    }
  else if (nargin == 1)
    {
      octave_value tmp = args(0);

      if (tmp.is_string ())
	{
	  std::string s_arg = tmp.string_value ();

	  if (s_arg == "dist")
	    {
	      retval = octave_rand::distribution ();
	    }
	  else if (s_arg == "seed")
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
	    error ("rand: unrecognized string argument");
	}
      else if (tmp.is_scalar_type ())
	{
	  double dval = tmp.double_value ();

	  if (xisnan (dval))
	    {
	      error ("rand: NaN is invalid a matrix dimension");
	    }
	  else
	    {
	      m = n = NINT (tmp.double_value ());

	      if (! error_state)
		goto gen_matrix;
	    }
	}
      else if (tmp.is_range ())
	{
	  Range r = tmp.range_value ();
	  n = 1;
	  m = r.nelem ();
	  goto gen_matrix;
	}
      else if (tmp.is_matrix_type ())
	{
	  // XXX FIXME XXX -- this should probably use the function
	  // from data.cc.

	  Matrix a = args(0).matrix_value ();

	  if (error_state)
	    return retval;

	  n = a.rows ();
	  m = a.columns ();

	  if (n == 1 && m == 2)
	    {
	      n = NINT (a (0, 0));
	      m = NINT (a (0, 1));
	    }
	  else if (n == 2 && m == 1)
	    {
	      n = NINT (a (0, 0));
	      m = NINT (a (1, 0));
	    }
	  else
	    warning ("rand (A): use rand (size (A)) instead");

	  goto gen_matrix;
	}
      else
	{
	  gripe_wrong_type_arg ("rand", tmp);
	  return retval;
	}
    }
  else if (nargin == 2)
    {
      if (args(0).is_string ())
	{
	  if (args(0).string_value () == "seed")
	    {
	      double d = args(1).double_value ();

	      if (! error_state)
		octave_rand::seed (d);
	    }
	  else
	    error ("rand: unrecognized string argument");
	}
      else
	{
	  double dval = args(0).double_value ();

	  if (xisnan (dval))
	    {
	      error ("rand: NaN is invalid as a matrix dimension");
	    }
	  else
	    {
	      n = NINT (dval);

	      if (! error_state)
		{
		  m = NINT (args(1).double_value ());

		  if (! error_state)
		    goto gen_matrix;
		}
	    }
	}
    }

  return retval;

 gen_matrix:

  return octave_rand::matrix (n, m);
}

DEFUN_DLD (rand, args, nargout,
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

  if (nargin > 2 || nargout > 1)
    print_usage ("rand");
  else
    retval = do_rand (args, nargin);

  return retval;
}

static std::string current_distribution = octave_rand::distribution ();

static void
reset_rand_generator (void *)
{
  octave_rand::distribution (current_distribution);
}

DEFUN_DLD (randn, args, nargout,
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

  if (nargin > 2 || nargout > 1)
    print_usage ("randn");
  else
    {
      unwind_protect::begin_frame ("randn");

      // This relies on the fact that elements are popped from the
      // unwind stack in the reverse of the order they are pushed
      // (i.e. current_distribution will be reset before calling
      // reset_rand_generator()).

      unwind_protect::add (reset_rand_generator, 0);
      unwind_protect_str (current_distribution);

      current_distribution = "normal";

      octave_rand::distribution (current_distribution);

      retval = do_rand (args, nargin);

      unwind_protect::run_frame ("randn");
    }

  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
