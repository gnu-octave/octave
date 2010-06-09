/*

Copyright (C) 1996, 1997, 1998, 1999, 2000, 2002, 2003, 2005, 2006,
              2007, 2008, 2009 John W. Eaton
Copyright (C) 2009 VZLU Prague

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
#include "ov-re-mat.h"

/*
%!shared __random_statistical_tests__
%! % Flag whether the statistical tests should be run in "make check" or not
%! __random_statistical_tests__ = 0;
*/

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
                retval = octave_rand::state (fcn);
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
                if (args(idx+1).is_real_scalar ())
                  {
                    double d = args(idx+1).double_value ();

                    if (! error_state)
                      octave_rand::seed (d);
                  }
                else
                  error ("%s: seed must be a real scalar", fcn);
              }
            else if (ts == "state" || ts == "twister")
              {
                ColumnVector s = 
                  ColumnVector (args(idx+1).vector_value(false, true));

                if (! error_state)
                  octave_rand::state (s, fcn);
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
@deftypefn  {Loadable Function} {} rand (@var{x})\n\
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
This returns a column vector @var{v} of length 625.  Later, you can\n\
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
value of @var{v}, not @var{v} itself.\n\
\n\
By default, the generator is initialized from @code{/dev/urandom} if it is\n\
available, otherwise from cpu time, wall clock time and the current\n\
fraction of a second.\n\
\n\
To compute the pseudo-random sequence, @code{rand} uses the Mersenne\n\
Twister with a period of @math{2^{19937}-1} (See M. Matsumoto and T. Nishimura,\n\
@cite{Mersenne Twister: A 623-dimensionally equidistributed uniform pseudorandom number generator}, ACM Trans. on\n\
Modeling and Computer Simulation Vol. 8, No. 1, January pp.3-30 1998,\n\
@url{http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html}).\n\
Do @strong{not} use for cryptography without securely hashing\n\
several returned values together, otherwise the generator state\n\
can be learned after reading 624 consecutive values.\n\
\n\
Older versions of Octave used a different random number generator.\n\
The new generator is used by default\n\
as it is significantly faster than the old generator, and produces\n\
random numbers with a significantly longer cycle time.  However, in\n\
some circumstances it might be desirable to obtain the same random\n\
sequences as used by the old generators.  To do this the keyword\n\
\"seed\" is used to specify that the old generators should be use,\n\
as in\n\
\n\
@example\n\
rand (\"seed\", val)\n\
@end example\n\
\n\
which sets the seed of the generator to @var{val}.  The seed of the\n\
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
@seealso{randn, rande, randg, randp}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  retval = do_rand (args, nargin, "rand");

  return retval;
}

// FIXME -- The old generator (selected when "seed" is set) will not
// work properly if compiled to use 64-bit integers.

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
%! % Test fixed state
%! rand("state",1);
%! assert (rand(1,6), [0.1343642441124013 0.8474337369372327 0.763774618976614 0.2550690257394218 0.495435087091941 0.4494910647887382],1e-6);
%!test
%! % Test fixed seed
%! rand("seed",1);
%! assert (rand(1,6), [0.8668024251237512 0.9126510815694928 0.09366085007786751 0.1664607301354408 0.7408077004365623 0.7615650338120759],1e-6);
%!test
%! if (__random_statistical_tests__)
%!   % statistical tests may fail occasionally.
%!   rand("state",12);
%!   x = rand(100000,1);
%!   assert(max(x)<1.); %*** Please report this!!! ***
%!   assert(min(x)>0.); %*** Please report this!!! ***
%!   assert(mean(x),0.5,0.0024);
%!   assert(var(x),1/48,0.0632);
%!   assert(skewness(x),0,0.012); 
%!   assert(kurtosis(x),-6/5,0.0094);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   % statistical tests may fail occasionally.
%!   rand("seed",12);
%!   x = rand(100000,1);
%!   assert(max(x)<1.); %*** Please report this!!! ***
%!   assert(min(x)>0.); %*** Please report this!!! ***
%!   assert(mean(x),0.5,0.0024);
%!   assert(var(x),1/48,0.0632);
%!   assert(skewness(x),0,0.012); 
%!   assert(kurtosis(x),-6/5,0.0094);
%! endif
*/


static std::string current_distribution = octave_rand::distribution ();

static void
reset_rand_generator (void)
{
  octave_rand::distribution (current_distribution);
}

DEFUN_DLD (randn, args, ,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {} randn (@var{x})\n\
@deftypefnx {Loadable Function} {} randn (@var{n}, @var{m})\n\
@deftypefnx {Loadable Function} {} randn (\"state\", @var{x})\n\
@deftypefnx {Loadable Function} {} randn (\"seed\", @var{x})\n\
Return a matrix with normally distributed random\n\
elements having zero mean and variance one.  The arguments are\n\
handled the same as the arguments for @code{rand}.\n\
\n\
By default, @code{randn} uses the Marsaglia and Tsang ``Ziggurat technique'' to\n\
transform from a uniform to a normal distribution.  (G. Marsaglia and\n\
W.W. Tsang, @cite{Ziggurat method for generating random variables},\n\
J. Statistical Software, vol 5, 2000,\n\
@url{http://www.jstatsoft.org/v05/i08/})\n\
\n\
@seealso{rand, rande, randg, randp}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  unwind_protect frame;

  // This relies on the fact that elements are popped from the unwind
  // stack in the reverse of the order they are pushed
  // (i.e. current_distribution will be reset before calling
  // reset_rand_generator()).

  frame.add_fcn (reset_rand_generator);
  frame.protect_var (current_distribution);

  current_distribution = "normal";

  octave_rand::distribution (current_distribution);

  retval = do_rand (args, nargin, "randn");

  return retval;
}

/*
%!test
%! % Test fixed state
%! randn("state",1);
%! assert (randn(1,6), [-2.666521678978671 -0.7381719971724564 1.507903992673601 0.6019427189162239 -0.450661261143348 -0.7054431351574116],1e-6);
%!test
%! % Test fixed seed
%! randn("seed",1);
%! assert (randn(1,6), [-1.039402365684509 -1.25938892364502 0.1968704611063004 0.3874166905879974 -0.5976632833480835 -0.6615074276924133],1e-6);
%!test
%! if (__random_statistical_tests__)
%!   % statistical tests may fail occasionally.
%!   randn("state",12);
%!   x = randn(100000,1);
%!   assert(mean(x),0,0.01);
%!   assert(var(x),1,0.02);
%!   assert(skewness(x),0,0.02);
%!   assert(kurtosis(x),0,0.04);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   % statistical tests may fail occasionally.
%!   randn("seed",12);
%!   x = randn(100000,1);
%!   assert(mean(x),0,0.01);
%!   assert(var(x),1,0.02);
%!   assert(skewness(x),0,0.02);
%!   assert(kurtosis(x),0,0.04);
%! endif
*/

DEFUN_DLD (rande, args, ,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {} rande (@var{x})\n\
@deftypefnx {Loadable Function} {} rande (@var{n}, @var{m})\n\
@deftypefnx {Loadable Function} {} rande (\"state\", @var{x})\n\
@deftypefnx {Loadable Function} {} rande (\"seed\", @var{x})\n\
Return a matrix with exponentially distributed random elements.  The\n\
arguments are handled the same as the arguments for @code{rand}.\n\
\n\
By default, @code{randn} uses the Marsaglia and Tsang ``Ziggurat technique'' to\n\
transform from a uniform to a exponential distribution.  (G. Marsaglia and\n\
W.W. Tsang, @cite{Ziggurat method for generating random variables},\n\
J. Statistical Software, vol 5, 2000,\n\
@url{http://www.jstatsoft.org/v05/i08/})\n\
@seealso{rand, randn, randg, randp}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  unwind_protect frame;

  // This relies on the fact that elements are popped from the unwind
  // stack in the reverse of the order they are pushed
  // (i.e. current_distribution will be reset before calling
  // reset_rand_generator()).

  frame.add_fcn (reset_rand_generator);
  frame.protect_var (current_distribution);

  current_distribution = "exponential";

  octave_rand::distribution (current_distribution);

  retval = do_rand (args, nargin, "rande");

  return retval;
}

/*
%!test
%! % Test fixed state
%! rande("state",1);
%! assert (rande(1,6), [3.602973885835625 0.1386190677555021 0.6743112889616958 0.4512830847258422 0.7255744741233175 0.3415969205292291],1e-6);
%!test
%! % Test fixed seed
%! rande("seed",1);
%! assert (rande(1,6), [0.06492075175653866 1.717980206012726 0.4816154008731246 0.5231300676241517 0.103910739364359 1.668931916356087],1e-6);
%!test
%! if (__random_statistical_tests__)
%!   % statistical tests may fail occasionally
%!   rande("state",1);
%!   x = rande(100000,1);
%!   assert(min(x)>0); % *** Please report this!!! ***
%!   assert(mean(x),1,0.01);
%!   assert(var(x),1,0.03);
%!   assert(skewness(x),2,0.06);
%!   assert(kurtosis(x),6,0.7);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   % statistical tests may fail occasionally
%!   rande("seed",1);
%!   x = rande(100000,1);
%!   assert(min(x)>0); % *** Please report this!!! ***
%!   assert(mean(x),1,0.01);
%!   assert(var(x),1,0.03);
%!   assert(skewness(x),2,0.06);
%!   assert(kurtosis(x),6,0.7);
%! endif
*/

DEFUN_DLD (randg, args, ,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {} randg (@var{a}, @var{x})\n\
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
@item @code{gamma (a, b)} for @code{a > -1}, @code{b > 0}\n\
@example\n\
r = b * randg (a)\n\
@end example\n\
@item @code{beta (a, b)} for @code{a > -1}, @code{b > -1}\n\
@example\n\
@group\n\
r1 = randg (a, 1)\n\
r = r1 / (r1 + randg (b, 1))\n\
@end group\n\
@end example\n\
@item @code{Erlang (a, n)}\n\
@example\n\
r = a * randg (n)\n\
@end example\n\
@item @code{chisq (df)} for @code{df > 0}\n\
@example\n\
r = 2 * randg (df / 2)\n\
@end example\n\
@item @code{t(df)} for @code{0 < df < inf} (use randn if df is infinite)\n\
@example\n\
r = randn () / sqrt (2 * randg (df / 2) / df)\n\
@end example\n\
@item @code{F (n1, n2)} for @code{0 < n1}, @code{0 < n2}\n\
@example\n\
@group\n\
## r1 equals 1 if n1 is infinite\n\
r1 = 2 * randg (n1 / 2) / n1\n\
## r2 equals 1 if n2 is infinite\n\
r2 = 2 * randg (n2 / 2) / n2\n\
r = r1 / r2\n\n\
@end group\n\
@end example\n\
@item negative @code{binomial (n, p)} for @code{n > 0}, @code{0 < p <= 1}\n\
@example\n\
r = randp ((1 - p) / p * randg (n))\n\
@end example\n\
@item non-central @code{chisq (df, L)}, for @code{df >= 0} and @code{L > 0}\n\
(use chisq if @code{L = 0})\n\
@example\n\
@group\n\
r = randp (L / 2)\n\
r(r > 0) = 2 * randg (r(r > 0))\n\
r(df > 0) += 2 * randg (df(df > 0)/2)\n\
@end group\n\
@end example\n\
@item @code{Dirichlet (a1, @dots{} ak)}\n\
@example\n\
@group\n\
r = (randg (a1), @dots{}, randg (ak))\n\
r = r / sum (r)\n\
@end group\n\
@end example\n\
@end table\n\
@seealso{rand, randn, rande, randp}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin < 1)
    error ("randg: insufficient arguments");
  else
    {
      unwind_protect frame;

      // This relies on the fact that elements are popped from the unwind
      // stack in the reverse of the order they are pushed
      // (i.e. current_distribution will be reset before calling
      // reset_rand_generator()).

      frame.add_fcn (reset_rand_generator);
      frame.protect_var (current_distribution);

      current_distribution = "gamma";

      octave_rand::distribution (current_distribution);

      retval = do_rand (args, nargin, "randg", true);
    }

  return retval;
}

/*
%!test
%! randg("state",12)
%!assert(randg([-inf,-1,0,inf,nan]),[nan,nan,nan,nan,nan]) % *** Please report


%!test
%! % Test fixed state
%! randg("state",1);
%! assert (randg(0.1,1,6), [0.0103951513331241 8.335671459898252e-05 0.00138691397249762 0.000587308416993855 0.495590518784736 2.3921917414795e-12],1e-6);
%!test
%! % Test fixed state
%! randg("state",1);
%! assert (randg(0.95,1,6), [3.099382433255327 0.3974529788871218 0.644367450750855 1.143261091802246 1.964111762696822 0.04011915547957939],1e-6);
%!test
%! % Test fixed state
%! randg("state",1);
%! assert (randg(1,1,6), [0.2273389379645993 1.288822625058359 0.2406335209340746 1.218869553370733 1.024649860162554 0.09631230343599533],1e-6);
%!test
%! % Test fixed state
%! randg("state",1);
%! assert (randg(10,1,6), [3.520369644331133 15.15369864472106 8.332112081991205 8.406211067432674 11.81193475187611 10.88792728177059],1e-5);
%!test
%! % Test fixed state
%! randg("state",1);
%! assert (randg(100,1,6), [75.34570255262264 115.4911985594699 95.23493031356388 95.48926019250911 106.2397448229803 103.4813150404118],1e-4);
%!test
%! % Test fixed seed
%! randg("seed",1);
%! assert (randg(0.1,1,6), [0.07144210487604141 0.460641473531723 0.4749028384685516 0.06823389977216721 0.000293838675133884 1.802567535340305e-12],1e-6);
%!test
%! % Test fixed seed
%! randg("seed",1);
%! assert (randg(0.95,1,6), [1.664905071258545 1.879976987838745 1.905677795410156 0.9948706030845642 0.5606933236122131 0.0766092911362648],1e-6);
%!test
%! % Test fixed seed
%! randg("seed",1);
%! assert (randg(1,1,6), [0.03512085229158401 0.6488978862762451 0.8114678859710693 0.1666885763406754 1.60791552066803 1.90356981754303],1e-6);
%!test
%! % Test fixed seed
%! randg("seed",1);
%! assert (randg(10,1,6), [6.566435813903809 10.11648464202881 10.73162078857422 7.747178077697754 6.278522491455078 6.240195751190186],1e-5);
%!test
%! % Test fixed seed
%! randg("seed",1);
%! assert (randg(100,1,6), [89.40208435058594 101.4734725952148 103.4020004272461 93.62763214111328 88.33104705810547 88.1871337890625],1e-4);
%!test
%! if (__random_statistical_tests__)
%!   % statistical tests may fail occasionally.
%!   randg("state",12)
%!   a=0.1; x = randg(a,100000,1);
%!   assert(mean(x),    a,         0.01);
%!   assert(var(x),     a,         0.01);
%!   assert(skewness(x),2/sqrt(a), 1.);
%!   assert(kurtosis(x),6/a,       50.);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   % statistical tests may fail occasionally.
%!   randg("state",12)
%!   a=0.95; x = randg(a,100000,1);
%!   assert(mean(x),    a,         0.01);
%!   assert(var(x),     a,         0.04);
%!   assert(skewness(x),2/sqrt(a), 0.2);
%!   assert(kurtosis(x),6/a,       2.);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   % statistical tests may fail occasionally.
%!   randg("state",12)
%!   a=1; x = randg(a,100000,1);
%!   assert(mean(x),a,             0.01);
%!   assert(var(x),a,              0.04);
%!   assert(skewness(x),2/sqrt(a), 0.2);
%!   assert(kurtosis(x),6/a,       2.);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   % statistical tests may fail occasionally.
%!   randg("state",12)
%!   a=10; x = randg(a,100000,1);
%!   assert(mean(x),    a,         0.1);
%!   assert(var(x),     a,         0.5);
%!   assert(skewness(x),2/sqrt(a), 0.1);
%!   assert(kurtosis(x),6/a,       0.5);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   % statistical tests may fail occasionally.
%!   randg("state",12)
%!   a=100; x = randg(a,100000,1);
%!   assert(mean(x),    a,         0.2);
%!   assert(var(x),     a,         2.);
%!   assert(skewness(x),2/sqrt(a), 0.05);
%!   assert(kurtosis(x),6/a,       0.2);
%! endif
%!test
%! randg("seed",12)
%!assert(randg([-inf,-1,0,inf,nan]),[nan,nan,nan,nan,nan]) % *** Please report
%!test
%! if (__random_statistical_tests__)
%!   % statistical tests may fail occasionally.
%!   randg("seed",12)
%!   a=0.1; x = randg(a,100000,1);
%!   assert(mean(x),    a,         0.01);
%!   assert(var(x),     a,         0.01);
%!   assert(skewness(x),2/sqrt(a), 1.);
%!   assert(kurtosis(x),6/a,       50.);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   % statistical tests may fail occasionally.
%!   randg("seed",12)
%!   a=0.95; x = randg(a,100000,1);
%!   assert(mean(x),    a,         0.01);
%!   assert(var(x),     a,         0.04);
%!   assert(skewness(x),2/sqrt(a), 0.2);
%!   assert(kurtosis(x),6/a,       2.);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   % statistical tests may fail occasionally.
%!   randg("seed",12)
%!   a=1; x = randg(a,100000,1);
%!   assert(mean(x),a,             0.01);
%!   assert(var(x),a,              0.04);
%!   assert(skewness(x),2/sqrt(a), 0.2);
%!   assert(kurtosis(x),6/a,       2.);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   % statistical tests may fail occasionally.
%!   randg("seed",12)
%!   a=10; x = randg(a,100000,1);
%!   assert(mean(x),    a,         0.1);
%!   assert(var(x),     a,         0.5);
%!   assert(skewness(x),2/sqrt(a), 0.1);
%!   assert(kurtosis(x),6/a,       0.5);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   % statistical tests may fail occasionally.
%!   randg("seed",12)
%!   a=100; x = randg(a,100000,1);
%!   assert(mean(x),    a,         0.2);
%!   assert(var(x),     a,         2.);
%!   assert(skewness(x),2/sqrt(a), 0.05);
%!   assert(kurtosis(x),6/a,       0.2);
%! endif
*/


DEFUN_DLD (randp, args, ,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {} randp (@var{l}, @var{x})\n\
@deftypefnx {Loadable Function} {} randp (@var{l}, @var{n}, @var{m})\n\
@deftypefnx {Loadable Function} {} randp (\"state\", @var{x})\n\
@deftypefnx {Loadable Function} {} randp (\"seed\", @var{x})\n\
Return a matrix with Poisson distributed random elements with mean value\n\
parameter given by the first argument, @var{l}.  The arguments\n\
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
unimodal distributions', Doctoral Dissertation, 156pp., Technical\n\
University Graz, Austria, 1994.\n\
@item For @var{l} > 1e8, use normal approximation.\n\
L. Montanet, et al., 'Review of Particle Properties', Physical Review\n\
D 50 p1284, 1994\n\
@end table\n\
@seealso{rand, randn, rande, randg}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin < 1)
    error ("randp: insufficient arguments");
  else
    {
      unwind_protect frame;

      // This relies on the fact that elements are popped from the unwind
      // stack in the reverse of the order they are pushed
      // (i.e. current_distribution will be reset before calling
      // reset_rand_generator()).

      frame.add_fcn (reset_rand_generator);
      frame.protect_var (current_distribution);

      current_distribution = "poisson";

      octave_rand::distribution (current_distribution);

      retval = do_rand (args, nargin, "randp", true);
    }

  return retval;
}

/*
%!test
%! randp("state",12)
%!assert(randp([-inf,-1,0,inf,nan]),[nan,nan,0,nan,nan]); % *** Please report
%!test
%! % Test fixed state
%! randp("state",1);
%! assert(randp(5,1,6),[5 5 3 7 7 3])
%!test
%! % Test fixed state
%! randp("state",1);
%! assert(randp(15,1,6),[13 15 8 18 18 15])
%!test
%! % Test fixed state
%! randp("state",1);
%! assert(randp(1e9,1,6),[999915677 999976657 1000047684 1000019035 999985749 999977692],-1e-6)
%!test
%! % Test fixed state
%! randp("seed",1);
%! %%assert(randp(5,1,6),[8 2 3 6 6 8])
%! assert(randp(5,1,5),[8 2 3 6 6])
%!test
%! % Test fixed state
%! randp("seed",1);
%! assert(randp(15,1,6),[15 16 12 10 10 12])
%!test
%! % Test fixed state
%! randp("seed",1);
%! assert(randp(1e9,1,6),[1000006208 1000012224 999981120 999963520 999963072 999981440],-1e-6)
%!test
%! if (__random_statistical_tests__)
%!   % statistical tests may fail occasionally.
%!   randp("state",12)
%!   for a=[5, 15, 1e9; 0.03, 0.03, -5e-3; 0.03, 0.03, 0.03]
%!     x = randp(a(1),100000,1);
%!     assert(min(x)>=0); % *** Please report this!!! ***
%!     assert(mean(x),a(1),a(2));
%!     assert(var(x),a(1),0.02*a(1));
%!     assert(skewness(x),1/sqrt(a(1)),a(3));
%!     assert(kurtosis(x),1/a(1),3*a(3));
%!   endfor
%! endif
%!test
%! if (__random_statistical_tests__)
%!   % statistical tests may fail occasionally.
%!   randp("state",12)
%!   for a=[5, 15, 1e9; 0.03, 0.03, -5e-3; 0.03, 0.03, 0.03]
%!     x = randp(a(1)*ones(100000,1),100000,1);
%!     assert(min(x)>=0); % *** Please report this!!! ***
%!     assert(mean(x),a(1),a(2));
%!     assert(var(x),a(1),0.02*a(1));
%!     assert(skewness(x),1/sqrt(a(1)),a(3));
%!     assert(kurtosis(x),1/a(1),3*a(3));
%!   endfor
%! endif
%!test
%! randp("seed",12)
%!assert(randp([-inf,-1,0,inf,nan]),[nan,nan,0,nan,nan]); % *** Please report
%!test
%! if (__random_statistical_tests__)
%!   % statistical tests may fail occasionally.
%!   randp("seed",12)
%!   for a=[5, 15, 1e9; 0.03, 0.03, -5e-3; 0.03, 0.03, 0.03]
%!     x = randp(a(1),100000,1);
%!     assert(min(x)>=0); % *** Please report this!!! ***
%!     assert(mean(x),a(1),a(2));
%!     assert(var(x),a(1),0.02*a(1));
%!     assert(skewness(x),1/sqrt(a(1)),a(3));
%!     assert(kurtosis(x),1/a(1),3*a(3));
%!   endfor
%! endif
%!test
%! if (__random_statistical_tests__)
%!   % statistical tests may fail occasionally.
%!   randp("seed",12)
%!   for a=[5, 15, 1e9; 0.03, 0.03, -5e-3; 0.03, 0.03, 0.03]
%!     x = randp(a(1)*ones(100000,1),100000,1);
%!     assert(min(x)>=0); % *** Please report this!!! ***
%!     assert(mean(x),a(1),a(2));
%!     assert(var(x),a(1),0.02*a(1));
%!     assert(skewness(x),1/sqrt(a(1)),a(3));
%!     assert(kurtosis(x),1/a(1),3*a(3));
%!   endfor
%! endif
*/

DEFUN_DLD (randperm, args, ,
  "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {} randperm (@var{n})\n\
@deftypefnx {Loadable Function} {} randperm (@var{n}, @var{m})\n\
Return a row vector containing a random permutation of @code{1:@var{n}}.\n\
If @var{m} is supplied, return @var{m} permutations,\n\
one in each row of a NxM matrix.  The complexity is O(M*N) in both time and\n\
memory.  The randomization is performed using rand().\n\
All permutations are equally likely.\n\
@seealso{perms}\n\
@end deftypefn")
{
  int nargin = args.length ();
  octave_value retval;

  if (nargin == 1 || nargin == 2)
    {
      octave_idx_type n, m;
      
      if (nargin == 2)
        m = args(1).idx_type_value (true);
      else
        m = 1;

      n = args(0).idx_type_value (true);

      if (m < 0 || n < 0)
        error ("randperm: m and n must be non-negative");

      if (! error_state)
        {
          // Generate random numbers.
          NDArray r = octave_rand::nd_array (dim_vector (m, n));

          // Create transposed to allow faster access.
          Array<octave_idx_type> idx (dim_vector (n, m));

          double *rvec = r.fortran_vec ();

          octave_idx_type *ivec = idx.fortran_vec ();

          // Perform the Knuth shuffle.
          for (octave_idx_type j = 0; j < m; j++)
            {
              for (octave_idx_type i = 0; i < n; i++)
                ivec[i] = i;

              for (octave_idx_type i = 0; i < n; i++)
                {
                  octave_idx_type k = i + floor (rvec[i] * (n - i));
                  std::swap (ivec[i], ivec[k]);
                }

              ivec += n;
              rvec += n;
            }

          // Transpose.
          idx = idx.transpose ();

          // Re-fetch the pointers.
          ivec = idx.fortran_vec ();
          rvec = r.fortran_vec ();

          // Convert to doubles, reusing r.
          for (octave_idx_type i = 0, l = m*n; i < l; i++)
            rvec[i] = ivec[i] + 1;

          // Now create an array object with a cached idx_vector.
          retval = new octave_matrix (r, idx_vector (idx)); 
        }
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert(sort(randperm(20)),1:20)
%!assert(sort(randperm(20,50),2),repmat(1:20,50,1))
*/
