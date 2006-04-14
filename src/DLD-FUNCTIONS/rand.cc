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
do_rand (const octave_value_list& args, int nargin, const char *fcn,
	 bool additional_arg = false)
{
  octave_value retval;
  NDArray a;
  int idx = 0;
  dim_vector dims;

  if (additional_arg)
    {
      if (nargin == 0)
	{
	  error ("%s: expecting at least one argument", fcn);
	  goto done;
	}
      else if (args(0).is_string())
	additional_arg = false;
      else
	{
	  a = args(0).array_value ();
	  if (error_state)
	    {
	      error ("%s: expecting scalar or matrix arguments", fcn);
	      goto done;
	    }
	  idx++;
	  nargin--;
	}
    }

  switch (nargin)
    {
    case 0:
      {
	if (additional_arg)
	  dims = a.dims ();
	else
	  {
	    dims.resize (2);

	    dims(0) = 1;
	    dims(1) = 1;
	  }
	goto gen_matrix;
      }
      break;

    case 1:
      {
	octave_value tmp = args(idx);

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
	    else if (s_arg == "state" || s_arg == "twister")
	      {
		retval = octave_rand::state ();
	      }
	    else if (s_arg == "uniform")
	      {
		octave_rand::uniform_distribution ();
	      }
	    else if (s_arg == "normal")
	      {
		octave_rand::normal_distribution ();
	      }
	    else if (s_arg == "exponential")
	      {
		octave_rand::exponential_distribution ();
	      }
	    else if (s_arg == "poisson")
	      {
		octave_rand::poisson_distribution ();
	      }
	    else if (s_arg == "gamma")
	      {
		octave_rand::gamma_distribution ();
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
	octave_value tmp = args(idx);

	if (nargin == 2 && tmp.is_string ())
	  {
	    std::string ts = tmp.string_value ();

	    if (ts == "seed")
	      {
		double d = args(idx+1).double_value ();

		if (! error_state)
		  octave_rand::seed (d);
	      }
	    else if (ts == "state" || ts == "twister")
	      {
		ColumnVector s = 
		  ColumnVector (args(idx+1).vector_value(false, true));

		if (! error_state)
		  octave_rand::state (s);
	      }
	    else
	      error ("%s: unrecognized string argument", fcn);
	  }
	else
	  {
	    dims.resize (nargin);

	    for (int i = 0; i < nargin; i++)
	      {
		dims(i) = args(idx+i).int_value ();

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

  dims.chop_trailing_singletons ();

  if (additional_arg)
    {
      if (a.length() == 1)
	return octave_rand::nd_array (dims, a(0));
      else
	{
	  if (a.dims() != dims)
	    {
	      error ("%s: mismatch in argument size", fcn);
	      return retval;
	    }
	  octave_idx_type len = a.length ();
	  NDArray m (dims);
	  double *v = m.fortran_vec ();
	  for (octave_idx_type i = 0; i < len; i++)
	    v[i] = octave_rand::scalar (a(i));
	  return m;
	}
    }
  else
    return octave_rand::nd_array (dims);
}

DEFUN_DLD (rand, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} rand (@var{x})\n\
@deftypefnx {Loadable Function} {} rand (@var{n}, @var{m})\n\
@deftypefnx {Loadable Function} {} rand (\"state\", @var{x})\n\
@deftypefnx {Loadable Function} {} rand (\"seed\", @var{x})\n\
Return a matrix with random elements uniformly distributed on the\n\
interval (0, 1).  The arguments are handled the same as the arguments\n\
for @code{eye}.\n\
\n\
You can query the state of the random number generator using the\n\
form\n\
\n\
@example\n\
v = rand (\"state\")\n\
@end example\n\
\n\
This returns a column vector @var{v} of length 625. Later, you can\n\
restore the random number generator to the state @var{v}\n\
using the form\n\
\n\
@example\n\
rand (\"state\", v)\n\
@end example\n\
\n\
@noindent\n\
You may also initialize the state vector from an arbitrary vector of\n\
length <= 625 for @var{v}.  This new state will be a hash based on the\n\
the value of @var{v}, not @var{v} itself.\n\
\n\
By default, the generator is initialized from @code{/dev/urandom} if it is\n\
available, otherwise from cpu time, wall clock time and the current\n\
fraction of a second.\n\
\n\
@code{rand} uses the Mersenne Twister with a period of 2^19937-1\n\
(See M. Matsumoto and T. Nishimura, ``Mersenne Twister: A 623-dimensionally\n\
equidistributed uniform pseudorandom number generator'', ACM Trans. on\n\
Modeling and Computer Simulation Vol. 8, No. 1, Januray pp.3-30 1998,\n\
@url{http://www.math.keio.ac.jp/~matumoto/emt.html}).\n\
Do NOT use for CRYPTOGRAPHY without securely hashing several returned\n\
values together, otherwise the generator state can be learned after\n\
reading 624 consecutive values.\n\
\n\
@code{rand} includes a second random number generator, that was the\n\
previous generator used in octave. The new generator is used by default\n\
as it is significantly faster than the old generator, and produces\n\
random numebrs with a significantly longer cycle time. However, in\n\
some circumstances it might be desireable to obtain the same random\n\
sequences as used by the old generators. To do this the keyword\n\
\"seed\" is used to specify that the old generators should be use,\n\
as in\n\
\n\
@example\n\
rand (\"seed\", val)\n\
@end example\n\
\n\
which sets the seed of the generator to @var{val}. The seed of the\n\
generator can be queried with\n\
\n\
@example\n\
s = rand (\"seed\")\n\
@end example\n\
\n\
However, it should be noted that querying the seed will not cause\n\
@code{rand} to use the old generators, only setting the seed will.\n\
To cause @code{rand} to once again use the new generators, the\n\
keyword \"state\" should be used to reset the state of the @code{rand}.\n\
@seealso{randn,rande,randg,randp}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  retval = do_rand (args, nargin, "rand");

  return retval;
}

/*
%!test # 'state' can be a scalar
%! rand('state',12); x = rand(1,4);
%! rand('state',12); y = rand(1,4);
%! assert(x,y);
%!test # 'state' can be a vector
%! rand('state',[12,13]); x=rand(1,4);
%! rand('state',[12;13]); y=rand(1,4);
%! assert(x,y);
%!test # querying 'state' doesn't disturb sequence
%! rand('state',12); rand(1,2); x=rand(1,2);
%! rand('state',12); rand(1,2);
%! s=rand('state'); y=rand(1,2);
%! assert(x,y);
%! rand('state',s); z=rand(1,2);
%! assert(x,z);
%!test # 'seed' must be a scalar
%! rand('seed',12); x = rand(1,4);
%! rand('seed',12); y = rand(1,4);
%! assert(x,y);
%!error(rand('seed',[12,13]))
%!test # querying 'seed' returns a value which can be used later
%! s=rand('seed'); x=rand(1,2);
%! rand('seed',s); y=rand(1,2);
%! assert(x,y);
%!test # querying 'seed' doesn't disturb sequence
%! rand('seed',12); rand(1,2); x=rand(1,2);
%! rand('seed',12); rand(1,2);
%! s=rand('seed'); y=rand(1,2);
%! assert(x,y);
%! rand('seed',s); z=rand(1,2);
%! assert(x,z);
*/

/*
%!test
%! % statistical tests may fail occasionally.
%! rand("state",12);
%! x = rand(100000,1);
%! assert(max(x)<1.); %*** Please report this!!! ***
%! assert(min(x)>0.); %*** Please report this!!! ***
%! assert(mean(x),0.5,0.0024);
%! assert(var(x),1/48,0.0632);
%! assert(skewness(x),0,0.012); 
%! assert(kurtosis(x),-6/5,0.0094);
%!test
%! % statistical tests may fail occasionally.
%! rand("seed",12);
%! x = rand(100000,1);
%! assert(max(x)<1.); %*** Please report this!!! ***
%! assert(min(x)>0.); %*** Please report this!!! ***
%! assert(mean(x),0.5,0.0024);
%! assert(var(x),1/48,0.0632);
%! assert(skewness(x),0,0.012); 
%! assert(kurtosis(x),-6/5,0.0094);
*/


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
@deftypefnx {Loadable Function} {} randn (\"state\", @var{x})\n\
@deftypefnx {Loadable Function} {} randn (\"seed\", @var{x})\n\
Return a matrix with normally distributed random elements. The\n\
arguments are handled the same as the arguments for @code{rand}.\n\
\n\
By default, @code{randn} uses a Marsaglia and Tsang Ziggurat technique to\n\
transform from a uniform to a normal distribution. (G. Marsaglia and\n\
W.W. Tsang, 'Ziggurat method for generating random variables',\n\
J. Statistical Software, vol 5, 2000,\n\
@url{http://www.jstatsoft.org/v05/i08/})\n\
\n\
@seealso{rand,rande,randg,randp}\n\
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
%!test
%! % statistical tests may fail occasionally.
%! rand("state",12);
%! x = randn(100000,1);
%! assert(mean(x),0,0.01);
%! assert(var(x),1,0.02);
%! assert(skewness(x),0,0.02);
%! assert(kurtosis(x),0,0.04);
%!test
%! % statistical tests may fail occasionally.
%! rand("seed",12);
%! x = randn(100000,1);
%! assert(mean(x),0,0.01);
%! assert(var(x),1,0.02);
%! assert(skewness(x),0,0.02);
%! assert(kurtosis(x),0,0.04);
*/

DEFUN_DLD (rande, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} rande (@var{x})\n\
@deftypefnx {Loadable Function} {} rande (@var{n}, @var{m})\n\
@deftypefnx {Loadable Function} {} rande (\"state\", @var{x})\n\
@deftypefnx {Loadable Function} {} rande (\"seed\", @var{x})\n\
Return a matrix with exponentially distributed random elements. The\n\
arguments are handled the same as the arguments for @code{rand}.\n\
\n\
By default, @code{randn} uses a Marsaglia and Tsang Ziggurat technique to\n\
transform from a uniform to a exponential distribution. (G. Marsaglia and\n\
W.W. Tsang, 'Ziggurat method for generating random variables',\n\
J. Statistical Software, vol 5, 2000,\n\
@url{http://www.jstatsoft.org/v05/i08/})\n\
@seealso{rand,randn,randg,randp}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  unwind_protect::begin_frame ("rande");

  // This relies on the fact that elements are popped from the unwind
  // stack in the reverse of the order they are pushed
  // (i.e. current_distribution will be reset before calling
  // reset_rand_generator()).

  unwind_protect::add (reset_rand_generator, 0);
  unwind_protect_str (current_distribution);

  current_distribution = "exponential";

  octave_rand::distribution (current_distribution);

  retval = do_rand (args, nargin, "rande");

  unwind_protect::run_frame ("rande");

  return retval;
}

/*
%!test
%! % statistical tests may fail occasionally
%! rand("state",12);
%! x = rande(100000,1);
%! assert(min(x)>0); % *** Please report this!!! ***
%! assert(mean(x),1,0.01);
%! assert(var(x),1,0.03);
%! assert(skewness(x),2,0.06);
%! assert(kurtosis(x),6,0.7);
%!test
%! % statistical tests may fail occasionally
%! rand("seed",12);
%! x = rande(100000,1);
%! assert(min(x)>0); % *** Please report this!!! ***
%! assert(mean(x),1,0.01);
%! assert(var(x),1,0.03);
%! assert(skewness(x),2,0.06);
%! assert(kurtosis(x),6,0.7);
*/

DEFUN_DLD (randg, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} randg (@var{a}, @var{x})\n\
@deftypefnx {Loadable Function} {} randg (@var{a}, @var{n}, @var{m})\n\
@deftypefnx {Loadable Function} {} randg (\"state\", @var{x})\n\
@deftypefnx {Loadable Function} {} randg (\"seed\", @var{x})\n\
Return a matrix with @code{gamma(@var{a},1)} distributed random elements.\n\
The arguments are handled the same as the arguments for @code{rand},\n\
except for the argument @var{a}.\n\
\n\
This can be used to generate many distributions:\n\
\n\
@table @asis\n\
@item @code{gamma (a,b)} for @code{a > -1}, @code{b > 0}\n\
@example\n\
r = b*randg(a)\n\
@end example\n\
@item @code{beta(a,b)} for @code{a > -1}, @code{b > -1}\n\
@example\n\
r1 = randg(a,1)\n\
r = r1 / (r1 + randg(b,1))\n\
@end example\n\
@item @code{Erlang(a, n)}\n\
@example\n\
r = a*randg(n)\n\
@end example\n\
@item @code{chisq(df)} for @code{df > 0}\n\
@example\n\
r = 2*randg(df/2)\n\
@end example\n\
@item @code{t(df)} for @code{0 < df < inf} (use randn if df is infinite)\n\
@example\n\
r = randn() / sqrt(2*randg(df/2)/df)\n\
@end example\n\
@item @code{F(n1,n2)} for @code{0 < n1}, @code{0 < n2}\n\
@example\n\
r1 = 2*randg(n1/2)/n1 or 1 if n1 is infinite\n\
r2 = 2*randg(n2/2)/n2 or 1 if n2 is infinite\n\
r = r1 / r2\n\n\
@end example\n\
@item negative @code{binomial (n, p)} for @code{n > 0}, @code{0 < p <= 1}\n\
@example\n\
r = randp((1-p)/p * randg(n))\n\
@end example\n\
@item non-central @code{chisq(df,L)}, for @code{df >= 0} and @code{L > 0}\n\
(use chisq if @code{L = 0})\n\
@example\n\
r = randp(L/2)\n\
r(r > 0) = 2*randg(r(r > 0))\n\
r(df > 0) += 2*randg(df(df > 0)/2)\n\
@end example\n\
@item @code{Dirichlet(a1,...,ak)}\n\
@example\n\
r = (randg(a1),...,randg(ak))\n\
r = r / sum(r)\n\
@end example\n\
@end table\n\
@seealso{rand,randn,rande,randp}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin < 1)
    error ("randg: insufficient arguments");
  else
    {
      unwind_protect::begin_frame ("randg");

      // This relies on the fact that elements are popped from the unwind
      // stack in the reverse of the order they are pushed
      // (i.e. current_distribution will be reset before calling
      // reset_rand_generator()).

      unwind_protect::add (reset_rand_generator, 0);
      unwind_protect_str (current_distribution);

      current_distribution = "gamma";

      octave_rand::distribution (current_distribution);

      retval = do_rand (args, nargin, "randg", true);

      unwind_protect::run_frame ("randg");
    }

  return retval;
}

/*
%!test
%! rand("state",12)
%!assert(randg([-inf,-1,0,inf,nan]),[nan,nan,nan,nan,nan]) % *** Please report
%!test
%! % statistical tests may fail occasionally.
%! a=0.1; x = randg(a,100000,1);
%! assert(mean(x),    a,         0.01);
%! assert(var(x),     a,         0.01);
%! assert(skewness(x),2/sqrt(a), 1.);
%! assert(kurtosis(x),6/a,       50.);
%!test
%! % statistical tests may fail occasionally.
%! a=0.95; x = randg(a,100000,1);
%! assert(mean(x),    a,         0.01);
%! assert(var(x),     a,         0.04);
%! assert(skewness(x),2/sqrt(a), 0.2);
%! assert(kurtosis(x),6/a,       2.);
%!test
%! % statistical tests may fail occasionally.
%! a=1; x = randg(a,100000,1);
%! assert(mean(x),a,             0.01);
%! assert(var(x),a,              0.04);
%! assert(skewness(x),2/sqrt(a), 0.2);
%! assert(kurtosis(x),6/a,       2.);
%!test
%! % statistical tests may fail occasionally.
%! a=10; x = randg(a,100000,1);
%! assert(mean(x),    a,         0.1);
%! assert(var(x),     a,         0.5);
%! assert(skewness(x),2/sqrt(a), 0.1);
%! assert(kurtosis(x),6/a,       0.5);
%!test
%! % statistical tests may fail occasionally.
%! a=100; x = randg(a,100000,1);
%! assert(mean(x),    a,         0.2);
%! assert(var(x),     a,         2.);
%! assert(skewness(x),2/sqrt(a), 0.05);
%! assert(kurtosis(x),6/a,       0.2);
%!test
%! rand("seed",12)
%!assert(randg([-inf,-1,0,inf,nan]),[nan,nan,nan,nan,nan]) % *** Please report
%!test
%! % statistical tests may fail occasionally.
%! a=0.1; x = randg(a,100000,1);
%! assert(mean(x),    a,         0.01);
%! assert(var(x),     a,         0.01);
%! assert(skewness(x),2/sqrt(a), 1.);
%! assert(kurtosis(x),6/a,       50.);
%!test
%! % statistical tests may fail occasionally.
%! a=0.95; x = randg(a,100000,1);
%! assert(mean(x),    a,         0.01);
%! assert(var(x),     a,         0.04);
%! assert(skewness(x),2/sqrt(a), 0.2);
%! assert(kurtosis(x),6/a,       2.);
%!test
%! % statistical tests may fail occasionally.
%! a=1; x = randg(a,100000,1);
%! assert(mean(x),a,             0.01);
%! assert(var(x),a,              0.04);
%! assert(skewness(x),2/sqrt(a), 0.2);
%! assert(kurtosis(x),6/a,       2.);
%!test
%! % statistical tests may fail occasionally.
%! a=10; x = randg(a,100000,1);
%! assert(mean(x),    a,         0.1);
%! assert(var(x),     a,         0.5);
%! assert(skewness(x),2/sqrt(a), 0.1);
%! assert(kurtosis(x),6/a,       0.5);
%!test
%! % statistical tests may fail occasionally.
%! a=100; x = randg(a,100000,1);
%! assert(mean(x),    a,         0.2);
%! assert(var(x),     a,         2.);
%! assert(skewness(x),2/sqrt(a), 0.05);
%! assert(kurtosis(x),6/a,       0.2);
*/


DEFUN_DLD (randp, args, ,
  "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} randp (@var{l}, @var{x})\n\
@deftypefnx {Loadable Function} {} randp (@var{l}, @var{n}, @var{m})\n\
@deftypefnx {Loadable Function} {} randp (\"state\", @var{x})\n\
@deftypefnx {Loadable Function} {} randp (\"seed\", @var{x})\n\
Return a matrix with Poisson distributed random elements. The arguments\n\
are handled the same as the arguments for @code{rand}, except for the\n\
argument @var{l}.\n\
\n\
Five different algorithms are used depending on the range of @var{l}\n\
and whether or not @var{l} is a scalar or a matrix.\n\
\n\
@table @asis\n\
@item For scalar @var{l} <= 12, use direct method.\n\
Press, et al., 'Numerical Recipes in C', Cambridge University Press, 1992.\n\
@item For scalar @var{l} > 12, use rejection method.[1]\n\
Press, et al., 'Numerical Recipes in C', Cambridge University Press, 1992.\n\
@item For matrix @var{l} <= 10, use inversion method.[2]\n\
Stadlober E., et al., WinRand source code, available via FTP.\n\
@item For matrix @var{l} > 10, use patchwork rejection method.\n\
Stadlober E., et al., WinRand source code, available via FTP, or\n\
H. Zechner, 'Efficient sampling from continuous and discrete\n\
unimodal distributions', Doctoral Dissertaion, 156pp., Technical\n\
University Graz, Austria, 1994.\n\
@item For @var{l} > 1e8, use normal approximation.\n\
L. Montanet, et al., 'Review of Particle Properties', Physical Review\n\
D 50 p1284, 1994\n\
@end table\n\
@seealso{rand,randn,rande,randg}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin < 1)
    error ("randp: insufficient arguments");
  else
    {
      unwind_protect::begin_frame ("randp");

      // This relies on the fact that elements are popped from the unwind
      // stack in the reverse of the order they are pushed
      // (i.e. current_distribution will be reset before calling
      // reset_rand_generator()).

      unwind_protect::add (reset_rand_generator, 0);
      unwind_protect_str (current_distribution);

      current_distribution = "poisson";

      octave_rand::distribution (current_distribution);

      retval = do_rand (args, nargin, "randp", true);

      unwind_protect::run_frame ("randp");
    }

  return retval;
}

/*
%!test
%! rand("state",12)
%!assert(randp([-inf,-1,0,inf,nan]),[nan,nan,0,nan,nan]); % *** Please report
%!test
%! % statistical tests may fail occasionally.
%! for a=[5 15]
%!   x = randp(a,100000,1);
%!   assert(min(x)>=0); % *** Please report this!!! ***
%!   assert(mean(x),a,0.03);
%!   assert(var(x),a,0.2);
%!   assert(skewness(x),1/sqrt(a),0.03);
%!   assert(kurtosis(x),1/a,0.08);
%! end
%!test
%! % statistical tests may fail occasionally.
%! for a=[5 15]
%!   x = randp(a*ones(100000,1),100000,1);
%!   assert(min(x)>=0); % *** Please report this!!! ***
%!   assert(mean(x),a,0.03);
%!   assert(var(x),a,0.2);
%!   assert(skewness(x),1/sqrt(a),0.03);
%!   assert(kurtosis(x),1/a,0.08);
%! end
%!test
%! rand("seed",12)
%!assert(randp([-inf,-1,0,inf,nan]),[nan,nan,0,nan,nan]); % *** Please report
%!test
%! % statistical tests may fail occasionally.
%! for a=[5 15]
%!   x = randp(a,100000,1);
%!   assert(min(x)>=0); % *** Please report this!!! ***
%!   assert(mean(x),a,0.03);
%!   assert(var(x),a,0.2);
%!   assert(skewness(x),1/sqrt(a),0.03);
%!   assert(kurtosis(x),1/a,0.08);
%! end
%!test
%! % statistical tests may fail occasionally.
%! for a=[5 15]
%!   x = randp(a*ones(100000,1),100000,1);
%!   assert(min(x)>=0); % *** Please report this!!! ***
%!   assert(mean(x),a,0.03);
%!   assert(var(x),a,0.2);
%!   assert(skewness(x),1/sqrt(a),0.03);
%!   assert(kurtosis(x),1/a,0.08);
%! end
*/

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
