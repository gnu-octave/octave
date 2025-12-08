/////////////////////////////////////////////////////////////////////////*
//
// Copyright (C) 1996-2025 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cmath>
#include <complex>
#include <cstdint>
#include <functional>
#include <limits>
#include <string_view>
#include <unordered_map>
#include <string>

#include "boolNDArray.h"
#include "CNDArray.h"
#include "fNDArray.h"
#include "f77-fcn.h"
#include "mappers.h"
#include "oct-inttypes.h"
#include "oct-rand.h"
#include "quit.h"

#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "ovl.h"
#include "unwind-prot.h"
#include "utils.h"
#include "ov-re-mat.h"

OCTAVE_BEGIN_NAMESPACE(octave)

/*
%% Restore all rand* "seed" and "state" values in order, so that the
%% new "state" algorithm remains active after these tests complete.
%!function restore_rand_states (seed, state)
%!  rand ("seed", seed.rand);
%!  rande ("seed", seed.rande);
%!  randg ("seed", seed.randg);
%!  randn ("seed", seed.randn);
%!  randp ("seed", seed.randp);
%!  rand ("state", state.rand);
%!  rande ("state", state.rande);
%!  randg ("state", state.randg);
%!  randn ("state", state.randn);
%!  randp ("state", state.randp);
%!endfunction

%!shared __random_statistical_tests__, old_seed, old_state, restore_state
%! ## Flag whether the statistical tests should be run in "make check" or not
%! __random_statistical_tests__ = 0;
%! ## Save and restore the states of each of the random number generators
%! ## that are tested by the unit tests in this file.
%! old_seed.rand = rand ("seed");
%! old_seed.rande = rande ("seed");
%! old_seed.randg = randg ("seed");
%! old_seed.randn = randn ("seed");
%! old_seed.randp = randp ("seed");
%! old_state.rand = rand ("state");
%! old_state.rande = rande ("state");
%! old_state.randg = randg ("state");
%! old_state.randn = randn ("state");
%! old_state.randp = randp ("state");
%! restore_state = onCleanup (@() restore_rand_states (old_seed, old_state));
*/

static octave_value
do_rand (const octave_value_list& args, int nargin, const char *fcn,
         const std::string& distribution, bool additional_arg = false)
{
  NDArray a;
  int idx = 0;
  bool is_single = false;

  if (nargin > 0 && args(nargin-1).is_string ())
    {
      std::string s_arg = args(nargin-1).string_value ();

      if (s_arg == "single")
        {
          is_single = true;
          nargin--;
        }
      else if (s_arg == "double")
        nargin--;
    }

  if (additional_arg)
    {
      if (nargin == 0)
        error ("%s: at least one argument is required", fcn);
      else if (args(0).is_string ())
        additional_arg = false;
      else
        {
          a = args(0).xarray_value ("%s: dimension must be a scalar integer", fcn);

          idx++;
          nargin--;
        }
    }

  octave_value retval;
  dim_vector dims;

  // Restore current distribution on any exit.
  unwind_action restore_distribution
  ([] (const std::string& old_distribution)
  {
    rand::distribution (old_distribution);
  }, rand::distribution ());

  rand::distribution (distribution);

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
              retval = rand::distribution ();
            else if (s_arg == "seed")
              retval = rand::seed ();
            else if (s_arg == "state" || s_arg == "twister")
              retval = rand::state (fcn);
            else if (s_arg == "uniform")
              rand::uniform_distribution ();
            else if (s_arg == "normal")
              rand::normal_distribution ();
            else if (s_arg == "exponential")
              rand::exponential_distribution ();
            else if (s_arg == "poisson")
              rand::poisson_distribution ();
            else if (s_arg == "gamma")
              rand::gamma_distribution ();
            else
              error ("%s: unrecognized string argument", fcn);
          }
        else if (tmp.is_scalar_type ())
          {
            octave_idx_type n = tmp.idx_type_value (true);

            dims.resize (2);

            dims(0) = dims(1) = n;

            goto gen_matrix;
          }
        else if (tmp.is_range ())
          {
            range<double> r = tmp.range_value ();

            if (! r.all_elements_are_ints ())
              error ("%s: all elements of range must be integers", fcn);

            octave_idx_type n = r.numel ();

            dims.resize (n);

            octave_idx_type base = math::nint_big (r.base ());
            octave_idx_type incr = math::nint_big (r.increment ());

            for (octave_idx_type i = 0; i < n; i++)
              {
                // Negative dimensions treated as zero for Matlab compatibility
                dims(i) = (base >= 0 ? base : 0);
                base += incr;
              }

            goto gen_matrix;
          }
        else if (tmp.is_matrix_type ())
          {
            Array<octave_idx_type> iv;

            try
              {
                iv = tmp.octave_idx_type_vector_value (true);
              }
            catch (execution_exception& ee)
              {
                error (ee, "%s: dimensions must be a scalar or array of integers", fcn);
              }

            octave_idx_type len = iv.numel ();

            dims.resize (len);

            for (octave_idx_type i = 0; i < len; i++)
              {
                // Negative dimensions treated as zero for Matlab compatibility
                octave_idx_type elt = iv(i);
                dims(i) = (elt >=0 ? elt : 0);
              }

            goto gen_matrix;
          }
        else
          err_wrong_type_arg ("rand", tmp);
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

                    rand::seed (d);
                  }
                else if (args(idx+1).is_string ()
                         && args(idx+1).string_value () == "reset")
                  rand::reset ();
                else
                  error ("%s: seed must be a real scalar", fcn);
              }
            else if (ts == "state" || ts == "twister")
              {
                if (args(idx+1).is_string ()
                    && args(idx+1).string_value () == "reset")
                  rand::reset (fcn);
                else
                  {
                    ColumnVector s
                      = ColumnVector (args(idx+1).vector_value (false, true));

                    // Backwards compatibility with previous versions of
                    // Octave which mapped Inf to 0.
                    for (octave_idx_type i = 0; i < s.numel (); i++)
                      if (math::isinf (s.xelem (i)))
                        s.xelem (i) = 0.0;

                    rand::state (s, fcn);
                  }
              }
            else
              error ("%s: unrecognized string argument", fcn);
          }
        else
          {
            dims.resize (nargin);

            for (int i = 0; i < nargin; i++)
              {
                octave_idx_type elt = args(idx+i).idx_type_value (true);

                // Negative dimensions treated as zero for Matlab compatibility
                dims(i) = (elt >= 0 ? elt : 0);
              }

            goto gen_matrix;
          }
      }
      break;
    }

  // No "goto gen_matrix" in code path.  Must be done processing.
  return retval;

gen_matrix:

  dims.chop_trailing_singletons ();

  if (is_single)
    {
      if (additional_arg)
        {
          if (a.numel () == 1)
            return rand::float_nd_array (dims, a(0));
          else
            {
              if (a.dims () != dims)
                error ("%s: mismatch in argument size", fcn);

              octave_idx_type len = a.numel ();
              FloatNDArray m (dims);
              float *v = m.rwdata ();

              for (octave_idx_type i = 0; i < len; i++)
                v[i] = rand::float_scalar (a(i));

              return m;
            }
        }
      else
        return rand::float_nd_array (dims);
    }
  else
    {
      if (additional_arg)
        {
          if (a.numel () == 1)
            return rand::nd_array (dims, a(0));
          else
            {
              if (a.dims () != dims)
                error ("%s: mismatch in argument size", fcn);

              octave_idx_type len = a.numel ();
              NDArray m (dims);
              double *v = m.rwdata ();

              for (octave_idx_type i = 0; i < len; i++)
                v[i] = rand::scalar (a(i));

              return m;
            }
        }
      else
        return rand::nd_array (dims);
    }
}

DEFUN (rand, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{x} =} rand ()
@deftypefnx {} {@var{x} =} rand (@var{n})
@deftypefnx {} {@var{x} =} rand (@var{m}, @var{n}, @dots{})
@deftypefnx {} {@var{x} =} rand ([@var{m}, @var{n}, @dots{}])
@deftypefnx {} {@var{x} =} rand (@dots{}, @var{class})
@c FIXME: Should implement "like" option for rand function
@c @deftypefnx {} {@var{x} =} rand (@dots{}, "like", @var{var})
@deftypefnx {} {@var{v} =} rand ("state")
@deftypefnx {} {} rand ("state", @var{v})
@deftypefnx {} {} rand ("state", "reset")
@deftypefnx {} {@var{v} =} rand ("seed")
@deftypefnx {} {} rand ("seed", @var{v})
@deftypefnx {} {} rand ("seed", "reset")
Return a scalar, matrix, or N-dimensional array whose elements are random
numbers uniformly distributed on the interval (0, 1).

If called with no arguments, return a scalar random value.

If invoked with a single scalar integer argument @var{n}, return a square
@nospell{NxN} matrix.

If invoked with two or more scalar integer arguments, or a vector of integer
values, return an array with the given dimensions.

The optional argument @var{class} specifies the class of the return array.
The only valid options are @qcode{"double"} (default) or @qcode{"single"}.

Programming Note: Any negative dimensions are treated as zero, and any zero
dimensions will result in an empty matrix.  This odd behavior is for
@sc{matlab} compatibility.

The additional calling forms provide an interface to the underlying random
number generator.

You can query the state of the random number generator using the form

@example
v = rand ("state")
@end example

This returns a column vector @var{v} of length 625.  Later, you can restore
the random number generator to the state @var{v} using the form

@example
rand ("state", v)
@end example

@noindent
You may also initialize the state vector from an arbitrary vector of length
@leq{} 625 for @var{v}.  The new state will be a hash based on the value of
@var{v}, not @var{v} itself.

By default, the generator is initialized by contributing entropy from the
wall clock time, the CPU time, the current fraction of a second, the process
ID and---if available---up to 1024 bits from the C++ random number source
@code{random_device}, which might be non-deterministic (implementation
specific).  Note that this differs from @sc{matlab}, which always initializes
the random number generator to the same state at startup.  To obtain behavior
comparable to @sc{matlab}, initialize with a deterministic state vector in
Octave's startup files (@pxref{Startup Files}).

Programming Notes: To compute the pseudo-random sequence, @code{rand} uses the
Mersenne Twister with a period of @math{2^{19937}-1} (See
@nospell{M. Matsumoto and T. Nishimura}, @cite{Mersenne Twister: A
623-dimensionally equidistributed uniform pseudorandom number generator},
@nospell{ACM} Trans.@: on Modeling and Computer Simulation Vol.@tie{}8,
No.@tie{}1, pp.@tie{}3--30, January 1998,
@url{http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html}).
Do @strong{not} use for cryptography without securely hashing several returned
values together, otherwise the generator state can be learned after reading 624
consecutive values.

Older versions of Octave used a different random number generator.  The new
generator is used by default as it is significantly faster than the old
generator, and produces random numbers with a significantly longer cycle time.
However, in some circumstances it might be desirable to obtain the same random
sequences as produced by the old generators.  To do this the keyword
@qcode{"seed"} is used to specify that the old generators should be used, as in

@example
rand ("seed", val)
@end example

@noindent
which sets the seed of the generator to @var{val}.  The seed of the generator
can be queried with

@example
s = rand ("seed")
@end example

However, it should be noted that querying the seed will not cause @code{rand}
to use the old generators, only setting the seed will.  To cause @code{rand} to
once again use the new generators, the keyword @qcode{"state"} must be used
to reset @code{rand} back to using the default generator.

The state or seed of the generator can be reset to a new random value using
the @qcode{"reset"} keyword.

@seealso{randi, randn, rande, randg, randp}
@end deftypefn */)
{
  return do_rand (args, args.length (), "rand", "uniform");
}

// FIXME: The old generator (selected when "seed" is set) will not
// work properly if compiled to use 64-bit integers.

/*
%!test  # "state" can be a scalar
%! rand ("state", 12);  x = rand (1,4);
%! rand ("state", 12);  y = rand (1,4);
%! assert (x, y);
%!test  # "state" can be a vector
%! rand ("state", [12,13]);  x = rand (1,4);
%! rand ("state", [12;13]);  y = rand (1,4);
%! assert (x, y);
%!test  # querying "state" returns a value which can be used later
%! s = rand ("state");  x = rand (1,2);
%! rand ("state", s);   y = rand (1,2);
%! assert (x, y);
%!test  # querying "state" doesn't disturb sequence
%! rand ("state", 12);  rand (1,2);  x = rand (1,2);
%! rand ("state", 12);  rand (1,2);  s = rand ("state");  y = rand (1,2);
%! assert (x, y);
%! rand ("state", s);   z = rand (1,2);
%! assert (x, z);
%!test  # "seed" must be a scalar
%! rand ("seed", 12);  x = rand (1,4);
%! rand ("seed", 12);  y = rand (1,4);
%! assert (x, y);
%!error <seed must be a real scalar> rand ("seed", [12,13])
%!test  # querying "seed" returns a value which can be used later
%! s = rand ("seed");  x = rand (1,2);
%! rand ("seed", s);   y = rand (1,2);
%! assert (x, y);
%!test  # querying "seed" doesn't disturb sequence
%! rand ("seed", 12);  rand (1,2);  x = rand (1,2);
%! rand ("seed", 12);  rand (1,2);  s = rand ("seed");  y = rand (1,2);
%! assert (x, y);
%! rand ("seed", s);  z = rand (1,2);
%! assert (x, z);
*/

/*
%!test
%! ## Test a known fixed state
%! rand ("state", 1);
%! assert (rand (1,6), [0.1343642441124013 0.8474337369372327 0.763774618976614 0.2550690257394218 0.495435087091941 0.4494910647887382], eps);
%!test
%! ## Test a known fixed seed
%! rand ("seed", 1);
%! assert (rand (1,6), [0.8668024251237512 0.9126510815694928 0.09366085007786751 0.1664607301354408 0.7408077004365623 0.7615650338120759], 1e-6);
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   rand ("state", 12);
%!   x = rand (100_000, 1);
%!   assert (min (x) > 0);   #*** Please report this!!! ***
%!   assert (max (x) < 1);   #*** Please report this!!! ***
%!   assert (mean (x), 0.5, 0.0024);
%!   assert (var (x), 1/48, 0.0632);
%!   assert (skewness (x), 0, 0.012);
%!   assert (kurtosis (x), -6/5, 0.0094);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   rand ("seed", 12);
%!   x = rand (100_000, 1);
%!   assert (max (x) < 1);   #*** Please report this!!! ***
%!   assert (min (x) > 0);   #*** Please report this!!! ***
%!   assert (mean (x), 0.5, 0.0024);
%!   assert (var (x), 1/48, 0.0632);
%!   assert (skewness (x), 0, 0.012);
%!   assert (kurtosis (x), -6/5, 0.0094);
%! endif
*/

/*
## Test out-of-range values as rand() seeds.
%!function v = __rand_sample__ (initval)
%!  rand ("state", initval);
%!  v = rand (1, 6);
%!endfunction
%!
%!assert (__rand_sample__ (-1), __rand_sample__ (0))
%!assert (__rand_sample__ (-Inf), __rand_sample__ (0))
%!assert (__rand_sample__ (2^33), __rand_sample__ (intmax ("uint32")))
%!assert (__rand_sample__ (Inf), __rand_sample__ (0))
%!assert (__rand_sample__ (NaN), __rand_sample__ (0))
*/

/*
## Note: Matlab compatibility requires using 0 for negative dimensions.
%!assert (size (rand (1, -1, 2)), [1, 0, 2])

## Test input validation
%!error <conversion of 1.1 to.* failed> rand (1, 1.1)
%!error <dimensions must be .* array of integers> rand ([1, 1.1])
*/

static std::string current_distribution = rand::distribution ();

DEFUN (randn, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{x} =} randn ()
@deftypefnx {} {@var{x} =} randn (@var{n})
@deftypefnx {} {@var{x} =} randn (@var{m}, @var{n}, @dots{})
@deftypefnx {} {@var{x} =} randn ([@var{m}, @var{n}, @dots{}])
@deftypefnx {} {@var{x} =} randn (@dots{}, @var{class})
@c FIXME: Should implement "like" option for randn function
@c @deftypefnx {} {@var{x} =} randn (@dots{}, "like", @var{var})
@deftypefnx {} {@var{v} =} randn ("state")
@deftypefnx {} {} randn ("state", @var{v})
@deftypefnx {} {} randn ("state", "reset")
@deftypefnx {} {@var{v} =} randn ("seed")
@deftypefnx {} {} randn ("seed", @var{v})
@deftypefnx {} {} randn ("seed", "reset")
Return a scalar, matrix, or N-dimensional array whose elements are random
numbers from the stdnormal distribution having a mean of @code{0} and a
variance of @code{1}.

If called with no arguments, return a scalar random value.

If invoked with a single scalar integer argument @var{n}, return a square
@nospell{NxN} matrix.

If invoked with two or more scalar integer arguments, or a vector of integer
values, return an array with the given dimensions.

The optional argument @var{class} specifies the class of the return array.
The only valid options are @qcode{"double"} (default) or @qcode{"single"}.

Programming Note: Any negative dimensions are treated as zero, and any zero
dimensions will result in an empty matrix.  This odd behavior is for
@sc{matlab} compatibility.

The additional calling forms provide an interface to the underlying random
number generator.  See @ref{XREFrand,,@code{rand}} for documentation on
querying and controlling the random number generator.

Programming Note: By default, @code{randn} uses the
@nospell{Marsaglia and Tsang} ``Ziggurat technique'' to transform from a
uniform to a normal distribution.

Reference: @nospell{G. Marsaglia and W.W. Tsang},
@cite{Ziggurat Method for Generating Random Variables},
J.@tie{}Statistical Software, vol@tie{}5, 2000,
@url{https://www.jstatsoft.org/v05/i08/}

@seealso{rand, randi, rande, randg, randp}
@end deftypefn */)
{
  return do_rand (args, args.length (), "randn", "normal");
}

/*
%!test
%! ## Test a known fixed state
%! randn ("state", 1);
%! assert (randn (1, 6), [-2.666521678978671 -0.7381719971724564 1.507903992673601 0.6019427189162239 -0.450661261143348 -0.7054431351574116], 14*eps);
%!test
%! ## Test a known fixed seed
%! randn ("seed", 1);
%! assert (randn (1, 6), [-1.039402365684509 -1.25938892364502 0.1968704611063004 0.3874166905879974 -0.5976632833480835 -0.6615074276924133], 1e-6);
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randn ("state", 12);
%!   x = randn (100_000, 1);
%!   assert (mean (x), 0, 0.01);
%!   assert (var (x), 1, 0.02);
%!   assert (skewness (x), 0, 0.02);
%!   assert (kurtosis (x), 0, 0.04);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randn ("seed", 12);
%!   x = randn (100_000, 1);
%!   assert (mean (x), 0, 0.01);
%!   assert (var (x), 1, 0.02);
%!   assert (skewness (x), 0, 0.02);
%!   assert (kurtosis (x), 0, 0.04);
%! endif
*/

DEFUN (rande, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{x} =} rande ()
@deftypefnx {} {@var{x} =} rande (@var{n})
@deftypefnx {} {@var{x} =} rande (@var{m}, @var{n}, @dots{})
@deftypefnx {} {@var{x} =} rande ([@var{m}, @var{n}, @dots{}])
@deftypefnx {} {@var{x} =} rande (@dots{}, @var{class})
@c FIXME: Should implement "like" option for rande function
@c @deftypefnx {} {@var{x} =} rande (@dots{}, "like", @var{var})
@deftypefnx {} {@var{v} =} rande ("state")
@deftypefnx {} {} rande ("state", @var{v})
@deftypefnx {} {} rande ("state", "reset")
@deftypefnx {} {@var{v} =} rande ("seed")
@deftypefnx {} {} rande ("seed", @var{v})
@deftypefnx {} {} rande ("seed", "reset")
Return a scalar, matrix, or N-dimensional array whose elements are random
numbers from the exponential distribution with rate parameter @code{0}.

If called with no arguments, return a scalar random value.

If invoked with a single scalar integer argument @var{n}, return a square
@nospell{NxN} matrix.

If invoked with two or more scalar integer arguments, or a vector of integer
values, return an array with the given dimensions.

The optional argument @var{class} specifies the class of the return array.
The only valid options are @qcode{"double"} (default) or @qcode{"single"}.

Programming Note: Any negative dimensions are treated as zero, and any zero
dimensions will result in an empty matrix.  This odd behavior is for
@sc{matlab} compatibility.

The additional calling forms provide an interface to the underlying random
number generator.  See @ref{XREFrand,,@code{rand}} for documentation on
querying and controlling the random number generator.

Programming Note: By default, @code{rande} uses the
@nospell{Marsaglia and Tsang} ``Ziggurat technique'' to transform from a
uniform to an exponential distribution.

Reference: @nospell{G. Marsaglia and W.W. Tsang},
@cite{Ziggurat Method for Generating Random Variables},
J.@tie{}Statistical Software, vol@tie{}5, 2000,
@url{https://www.jstatsoft.org/v05/i08/}

@seealso{rand, randi, randn, randg, randp}
@end deftypefn */)
{
  return do_rand (args, args.length (), "rande", "exponential");
}

/*
%!test
%! ## Test a known fixed state
%! rande ("state", 1);
%! assert (rande (1, 6), [3.602973885835625 0.1386190677555021 0.6743112889616958 0.4512830847258422 0.7255744741233175 0.3415969205292291], 2*eps);
%!test
%! ## Test a known fixed seed
%! rande ("seed", 1);
%! assert (rande (1, 6), [0.06492075175653866 1.717980206012726 0.4816154008731246 0.5231300676241517 0.103910739364359 1.668931916356087], 1e-6);
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally
%!   rande ("state", 1);
%!   x = rande (100_000, 1);
%!   assert (min (x) > 0);   # *** Please report this!!! ***
%!   assert (mean (x), 1, 0.01);
%!   assert (var (x), 1, 0.03);
%!   assert (skewness (x), 2, 0.06);
%!   assert (kurtosis (x), 6, 0.7);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally
%!   rande ("seed", 1);
%!   x = rande (100_000, 1);
%!   assert (min (x)>0);   # *** Please report this!!! ***
%!   assert (mean (x), 1, 0.01);
%!   assert (var (x), 1, 0.03);
%!   assert (skewness (x), 2, 0.06);
%!   assert (kurtosis (x), 6, 0.7);
%! endif
*/

DEFUN (randg, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{x} =} randg (@var{a})
@deftypefnx {} {@var{x} =} randg (@var{a}, @var{n})
@deftypefnx {} {@var{x} =} randg (@var{a}, @var{m}, @var{n}, @dots{})
@deftypefnx {} {@var{x} =} randg (@var{a}, [@var{m}, @var{n}, @dots{}])
@deftypefnx {} {@var{x} =} randg (@dots{}, @var{class})
@c FIXME: Should implement "like" option for randg function
@c @deftypefnx {} {@var{x} =} randg (@dots{}, "like", @var{var})
@deftypefnx {} {@var{v} =} randg ("state")
@deftypefnx {} {} randg ("state", @var{v})
@deftypefnx {} {} randg ("state", "reset")
@deftypefnx {} {@var{v} =} randg ("seed")
@deftypefnx {} {} randg ("seed", @var{v})
@deftypefnx {} {} randg ("seed", "reset")
Return a scalar, matrix, or N-dimensional array whose elements are random
numbers from the gamma distribution @code{gamma (@var{a},1)}.

If called with no size arguments, return a scalar random value.

If invoked with a single scalar size argument @var{n}, return a square
@nospell{NxN} matrix.

If invoked with two or more scalar integer size arguments, or a vector of
integer values, return an array with the given dimensions.

The optional argument @var{class} specifies the class of the return array.
The only valid options are @qcode{"double"} (default) or @qcode{"single"}.

Programming Note: Any negative dimensions are treated as zero, and any zero
dimensions will result in an empty matrix.  This odd behavior is for
@sc{matlab} compatibility.

The additional calling forms provide an interface to the underlying random
number generator.  See @ref{XREFrand,,@code{rand}} for documentation on
querying and controlling the random number generator.

Programming Notes: The gamma distribution can be used to generate many
other distributions:

@table @asis
@item @code{gamma (a, b)} for @code{a > -1}, @code{b > 0}

@example
r = b * randg (a)
@end example

@item @code{beta (a, b)} for @code{a > -1}, @code{b > -1}

@example
@group
r1 = randg (a, 1)
r = r1 / (r1 + randg (b, 1))
@end group
@end example

@item @code{Erlang (a, n)}

@example
r = a * randg (n)
@end example

@item @code{chisq (df)} for @code{df > 0}

@example
r = 2 * randg (df / 2)
@end example

@item @code{t (df)} for @code{0 < df < inf} (use randn if df is infinite)

@example
r = randn () / sqrt (2 * randg (df / 2) / df)
@end example

@item @code{F (n1, n2)} for @code{0 < n1}, @code{0 < n2}

@example
@group
## r1 equals 1 if n1 is infinite
r1 = 2 * randg (n1 / 2) / n1
## r2 equals 1 if n2 is infinite
r2 = 2 * randg (n2 / 2) / n2
r = r1 / r2
@end group
@end example

@item negative @code{binomial (n, p)} for @code{n > 0}, @code{0 < p <= 1}

@example
r = randp ((1 - p) / p * randg (n))
@end example

@item non-central @code{chisq (df, L)}, for @code{df >= 0} and @code{L > 0}
(use chisq if @code{L = 0})

@example
@group
r = randp (L / 2)
r(r > 0) = 2 * randg (r(r > 0))
r(df > 0) += 2 * randg (df(df > 0)/2)
@end group
@end example

@item @code{Dirichlet (a1, @dots{} ak)}

@example
@group
r = (randg (a1), @dots{}, randg (ak))
r = r / sum (r)
@end group
@end example

@end table

@seealso{rand, randi, randn, rande, randp}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1)
    error ("randg: insufficient arguments");

  return do_rand (args, nargin, "randg", "gamma", true);
}

/*
%!test
%! randg ("state", 12);
%! assert (randg ([-inf, -1, 0, inf, NaN]), [NaN, NaN, NaN, NaN, NaN]);

%!test
%! ## Test a known fixed state
%! randg ("state", 1);
%! assert (randg (0.1, 1, 6), [0.0103951513331241 8.335671459898252e-05 0.00138691397249762 0.000587308416993855 0.495590518784736 2.3921917414795e-12], eps);
%!test
%! ## Test a known fixed state
%! randg ("state", 1);
%! assert (randg (0.95, 1, 6), [3.099382433255327 0.3974529788871218 0.644367450750855 1.143261091802246 1.964111762696822 0.04011915547957939], 12*eps);
%!test
%! ## Test a known fixed state
%! randg ("state", 1);
%! assert (randg (1, 1, 6), [0.2273389379645993 1.288822625058359 0.2406335209340746 1.218869553370733 1.024649860162554 0.09631230343599533], 40*eps);
%!test
%! ## Test a known fixed state
%! randg ("state", 1);
%! assert (randg (10, 1, 6), [3.520369644331133 15.15369864472106 8.332112081991205 8.406211067432674 11.81193475187611 10.88792728177059], 56*eps);
%!test
%! ## Test a known fixed state
%! randg ("state", 1);
%! assert (randg (100, 1, 6), [75.34570255262264 115.4911985594699 95.23493031356388 95.48926019250911 106.2397448229803 103.4813150404118], 256*eps);
%!test
%! ## Test a known fixed seed
%! randg ("seed", 1);
%! assert (randg (0.1, 1, 6), [0.07144210487604141 0.460641473531723 0.4749028384685516 0.06823389977216721 0.000293838675133884 1.802567535340305e-12], 1e-6);
%!test
%! ## Test a known fixed seed
%! randg ("seed", 1);
%! assert (randg (0.95, 1, 6), [1.664905071258545 1.879976987838745 1.905677795410156 0.9948706030845642 0.5606933236122131 0.0766092911362648], 1e-6);
%!test
%! ## Test a known fixed seed
%! randg ("seed", 1);
%! assert (randg (1, 1, 6), [0.03512085229158401 0.6488978862762451 0.8114678859710693 0.1666885763406754 1.60791552066803 1.90356981754303], 1e-6);
%!test
%! ## Test a known fixed seed
%! randg ("seed", 1);
%! assert (randg (10, 1, 6), [6.566435813903809 10.11648464202881 10.73162078857422 7.747178077697754 6.278522491455078 6.240195751190186], 1e-5);
%!test
%! ## Test a known fixed seed
%! randg ("seed", 1);
%! assert (randg (100, 1, 6), [89.40208435058594 101.4734725952148 103.4020004272461 93.62763214111328 88.33104705810547 88.1871337890625], 1e-4);
%!test
%! ## Test out-of-bounds values produce NaN w/old-style generators & floats
%! randg ("seed", 1);
%! result = randg ([-2 Inf], "single");
%! assert (result, single ([NaN NaN]));

%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randg ("state", 12);
%!   a = 0.1;
%!   x = randg (a, 100_000, 1);
%!   assert (mean (x),     a,          0.01);
%!   assert (var (x),      a,          0.01);
%!   assert (skewness (x), 2/sqrt (a), 1);
%!   assert (kurtosis (x), 6/a,        50);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randg ("state", 12);
%!   a = 0.95;
%!   x = randg (a, 100_000, 1);
%!   assert (mean (x),     a,          0.01);
%!   assert (var (x),      a,          0.04);
%!   assert (skewness (x), 2/sqrt (a), 0.2);
%!   assert (kurtosis (x), 6/a,        2);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randg ("state", 12);
%!   a = 1;
%!   x = randg (a, 100_000, 1);
%!   assert (mean (x),     a,          0.01);
%!   assert (var (x),      a,          0.04);
%!   assert (skewness (x), 2/sqrt (a), 0.2);
%!   assert (kurtosis (x), 6/a,        2);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randg ("state", 12);
%!   a = 10;
%!   x = randg (a, 100_000, 1);
%!   assert (mean (x),     a,          0.1);
%!   assert (var (x),      a,          0.5);
%!   assert (skewness (x), 2/sqrt (a), 0.1);
%!   assert (kurtosis (x), 6/a,        0.5);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randg ("state", 12);
%!   a = 100;
%!   x = randg (a, 100_000, 1);
%!   assert (mean (x),     a,          0.2);
%!   assert (var (x),      a,          2);
%!   assert (skewness (x), 2/sqrt (a), 0.05);
%!   assert (kurtosis (x), 6/a,        0.2);
%! endif
%!test
%! randg ("seed", 12);
%! assert (randg ([-inf, -1, 0, inf, NaN]), [NaN, NaN, NaN, NaN, NaN]);
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randg ("seed", 12);
%!   a = 0.1;
%!   x = randg (a, 100_000, 1);
%!   assert (mean (x),     a,          0.01);
%!   assert (var (x),      a,          0.01);
%!   assert (skewness (x), 2/sqrt (a), 1);
%!   assert (kurtosis (x), 6/a,        50);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randg ("seed", 12);
%!   a = 0.95;
%!   x = randg (a, 100_000, 1);
%!   assert (mean (x),     a,          0.01);
%!   assert (var (x),      a,          0.04);
%!   assert (skewness (x), 2/sqrt (a), 0.2);
%!   assert (kurtosis (x), 6/a,        2);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randg ("seed", 12);
%!   a = 1;
%!   x = randg (a, 100_000, 1);
%!   assert (mean (x),     a,          0.01);
%!   assert (var (x),      a,          0.04);
%!   assert (skewness (x), 2/sqrt (a), 0.2);
%!   assert (kurtosis (x), 6/a,        2);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randg ("seed", 12);
%!   a = 10;
%!   x = randg (a, 100_000, 1);
%!   assert (mean (x),     a,          0.1);
%!   assert (var (x),      a,          0.5);
%!   assert (skewness (x), 2/sqrt (a), 0.1);
%!   assert (kurtosis (x), 6/a,        0.5);
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randg ("seed", 12);
%!   a = 100;
%!   x = randg (a, 100_000, 1);
%!   assert (mean (x),     a,          0.2);
%!   assert (var (x),      a,          2);
%!   assert (skewness (x), 2/sqrt (a), 0.05);
%!   assert (kurtosis (x), 6/a,        0.2);
%! endif
*/

DEFUN (randp, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{x} =} randp (@var{l})
@deftypefnx {} {@var{x} =} randp (@var{l}, @var{n})
@deftypefnx {} {@var{x} =} randp (@var{l}, @var{m}, @var{n}, @dots{})
@deftypefnx {} {@var{x} =} randp (@var{l}, [@var{m}, @var{n}, @dots{}])
@deftypefnx {} {@var{x} =} randp (@dots{}, @var{class})
@c FIXME: Should implement "like" option for randp function
@c @deftypefnx {} {@var{x} =} randp (@dots{}, "like", @var{var})
@deftypefnx {} {@var{v} =} randp ("state")
@deftypefnx {} {} randp ("state", @var{v})
@deftypefnx {} {} randp ("state", "reset")
@deftypefnx {} {@var{v} =} randp ("seed")
@deftypefnx {} {} randp ("seed", @var{v})
@deftypefnx {} {} randp ("seed", "reset")
Return a scalar, matrix, or N-dimensional array whose elements are random
numbers from the Poisson distribution with mean value parameter @var{l}.

If called with no size arguments, return a scalar random value.

If invoked with a single scalar size argument @var{n}, return a square
@nospell{NxN} matrix.

If invoked with two or more scalar integer size arguments, or a vector of
integer values, return an array with the given dimensions.

The optional argument @var{class} specifies the class of the return array.
The only valid options are @qcode{"double"} (default) or @qcode{"single"}.

Programming Note: Any negative dimensions are treated as zero, and any zero
dimensions will result in an empty matrix.  This odd behavior is for
@sc{matlab} compatibility.

The additional calling forms provide an interface to the underlying random
number generator.  See @ref{XREFrand,,@code{rand}} for documentation on
querying and controlling the random number generator.

Programming Notes: Five different algorithms are used depending on the range of
@var{l} and whether @var{l} is a scalar or a matrix.

@table @asis
@item For scalar @var{l} @leq{} 12, use direct method.
W.H.@tie{}Press, et@tie{}al., @cite{Numerical Recipes in C},
Cambridge University Press, 1992.

@item For scalar @var{l} > 12, use rejection method.[1]
W.H.@tie{}Press, et@tie{}al., @cite{Numerical Recipes in C},
Cambridge University Press, 1992.

@item For matrix @var{l} @leq{} 10, use inversion method.[2]
@nospell{E.@tie{}Stadlober, et@tie{}al., WinRand source code}, available via
FTP.

@item For matrix @var{l} > 10, use patchwork rejection method.
@nospell{E.@tie{}Stadlober, et@tie{}al., WinRand source code}, available via
FTP, or @nospell{H.@tie{}Zechner}, @cite{Efficient sampling from continuous and
discrete unimodal distributions}, Doctoral Dissertation, 156pp., Technical
University @nospell{Graz}, Austria, 1994.

@item For @var{l} > 1e8, use normal approximation.
@nospell{L.@tie{}Montanet}, et@tie{}al., @cite{Review of Particle Properties},
Physical Review@tie{}D 50 p1284, 1994.
@end table

@seealso{rand, randi, randn, rande, randg}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1)
    error ("randp: insufficient arguments");

  return do_rand (args, nargin, "randp", "poisson", true);
}

/*
%!test
%! randp ("state", 12);
%! assert (randp ([-inf, -1, 0, inf, NaN]), [NaN, NaN, 0, NaN, NaN]);
%!test
%! ## Test a known fixed state
%! randp ("state", 1);
%! assert (randp (5, 1, 6), [5 5 3 7 7 3]);
%!test
%! ## Test a known fixed state
%! randp ("state", 1);
%! assert (randp (15, 1, 6), [13 15 8 18 18 15]);
%!test
%! ## Test a known fixed state
%! randp ("state", 1);
%! assert (randp (1e9, 1, 6),
%!         [999915677 999976657 1000047684 1000019035 999985749 999977692],
%!         -1e-6);
%!test
%! ## Test a known fixed seed
%! randp ("seed", 1);
%! %%assert (randp (5, 1, 6), [8 2 3 6 6 8])
%! assert (randp (5, 1, 5), [8 2 3 6 6]);
%!test
%! ## Test a known fixed seed
%! randp ("seed", 1);
%! assert (randp (15, 1, 6), [15 16 12 10 10 12]);
%!test
%! ## Test a known fixed seed
%! randp ("seed", 1);
%! assert (randp (1e9, 1, 6),
%!         [1000006208 1000012224 999981120 999963520 999963072 999981440],
%!         -1e-6);
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randp ("state", 12);
%!   for a = [5, 15, 1e9; 0.03, 0.03, -5e-3; 0.03, 0.03, 0.03]
%!     x = randp (a (1), 100_000, 1);
%!     assert (min (x) >= 0);   # *** Please report this!!! ***
%!     assert (mean (x), a(1), a(2));
%!     assert (var (x), a(1), 0.02*a(1));
%!     assert (skewness (x), 1/sqrt (a(1)), a(3));
%!     assert (kurtosis (x), 1/a(1), 3*a(3));
%!   endfor
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randp ("state", 12);
%!   for a = [5, 15, 1e9; 0.03, 0.03, -5e-3; 0.03, 0.03, 0.03]
%!     x = randp (a(1)* ones (100_000, 1), 100_000, 1);
%!     assert (min (x) >= 0);   # *** Please report this!!! ***
%!     assert (mean (x), a(1), a(2));
%!     assert (var (x), a(1), 0.02*a(1));
%!     assert (skewness (x), 1/sqrt (a(1)), a(3));
%!     assert (kurtosis (x), 1/a(1), 3*a(3));
%!   endfor
%! endif
%!test
%! randp ("seed", 12);
%! assert (randp ([-inf, -1, 0, inf, NaN]), [NaN, NaN, 0, NaN, NaN]);
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randp ("seed", 12);
%!   for a = [5, 15, 1e9; 0.03, 0.03, -5e-3; 0.03, 0.03, 0.03]
%!     x = randp (a(1), 100_000, 1);
%!     assert (min (x) >= 0);   # *** Please report this!!! ***
%!     assert (mean (x), a(1), a(2));
%!     assert (var (x), a(1), 0.02*a(1));
%!     assert (skewness (x), 1/sqrt (a(1)), a(3));
%!     assert (kurtosis (x), 1/a(1), 3*a(3));
%!   endfor
%! endif
%!test
%! if (__random_statistical_tests__)
%!   ## statistical tests may fail occasionally.
%!   randp ("seed", 12);
%!   for a = [5, 15, 1e9; 0.03, 0.03, -5e-3; 0.03, 0.03, 0.03]
%!     x = randp (a(1)* ones (100_000, 1), 100_000, 1);
%!     assert (min (x) >= 0);   # *** Please report this!!! ***
%!     assert (mean (x), a(1), a(2));
%!     assert (var (x), a(1), 0.02*a(1));
%!     assert (skewness (x), 1/sqrt (a(1)), a(3));
%!     assert (kurtosis (x), 1/a(1), 3*a(3));
%!   endfor
%! endif
*/

DEFUN (randperm, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{v} =} randperm (@var{n})
@deftypefnx {} {@var{v} =} randperm (@var{n}, @var{m})
Return a row vector containing a random permutation of @code{1:@var{n}}.

If @var{m} is supplied, return @var{m} unique entries, sampled without
replacement from @code{1:@var{n}}.

The complexity is O(@var{n}) in memory and O(@var{m}) in time, unless
@var{m} < @var{n}/5, in which case O(@var{m}) memory is used as well.  The
randomization is performed using rand().  All permutations are equally
likely.
@seealso{perms}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    print_usage ();

  octave_idx_type n = args(0).idx_type_value (true);
  octave_idx_type m = (nargin == 2) ? args(1).idx_type_value (true) : n;

  if (m < 0 || n < 0)
    error ("randperm: M and N must be non-negative");

  if (m > n)
    error ("randperm: M must be less than or equal to N");

  // Quick and dirty heuristic to decide if we allocate or not the
  // whole vector for tracking the truncated shuffle.
  bool short_shuffle = m < n/5;

  // Generate random numbers.
  NDArray r = rand::nd_array (dim_vector (1, m));
  double *rvec = r.rwdata ();

  octave_idx_type idx_len = (short_shuffle ? m : n);
  Array<octave_idx_type> idx;
  try
    {
      idx = Array<octave_idx_type> (dim_vector (1, idx_len));
    }
  catch (const std::bad_alloc&)
    {
      // Looks like n is too big and short_shuffle is false.
      // Let's try again, but this time with the alternative.
      idx_len = m;
      short_shuffle = true;
      idx = Array<octave_idx_type> (dim_vector (1, idx_len));
    }

  octave_idx_type *ivec = idx.rwdata ();

  for (octave_idx_type i = 0; i < idx_len; i++)
    ivec[i] = i;

  if (short_shuffle)
    {
      std::unordered_map<octave_idx_type, octave_idx_type> map (m);

      // Perform the Knuth shuffle only keeping track of moved
      // entries in the map
      for (octave_idx_type i = 0; i < m; i++)
        {
          // rand() gives u in [0,1). For non-negative values, truncation is
          // equivalent to floor, so static_cast<octave_idx_type> (u * span)
          // is safe and avoids the cost of std::floor.
          octave_idx_type k = i + static_cast<octave_idx_type> (rvec[i] * (n - i));

          // For shuffling first m entries, no need to use extra storage
          if (k < m)
            {
              std::swap (ivec[i], ivec[k]);
            }
          else
            {
              if (map.find (k) == map.end ())
                map[k] = k;

              std::swap (ivec[i], map[k]);
            }
        }
    }
  else
    {
      // Perform the Knuth shuffle of the first m entries
      for (octave_idx_type i = 0; i < m; i++)
        {
          // rand() gives u in [0,1). For non-negative values, truncation is
          // equivalent to floor, so static_cast<octave_idx_type> (u * span)
          // is safe and avoids the cost of std::floor.
          octave_idx_type k = i + static_cast<octave_idx_type> (rvec[i] * (n - i));
          std::swap (ivec[i], ivec[k]);
        }
    }

  // Convert to doubles, reusing r.
  for (octave_idx_type i = 0; i < m; i++)
    rvec[i] = ivec[i] + 1;

  if (m < n)
    idx.resize (dim_vector (1, m));

  // Now create an array object with a cached idx_vector.
  return ovl (new octave_matrix (r, idx_vector (idx)));
}

/*
%!assert (sort (randperm (20)), 1:20)
%!assert (length (randperm (20,10)), 10)

## Test biggish N
%!assert <*39378> (length (randperm (30_000^2, 100_000)), 100_000)

%!test
%! rand ("seed", 0);
%! for i = 1:100
%!   p = randperm (305, 30);
%!   assert (length (unique (p)), 30);
%! endfor
*/

// ================== port of randi.m ========================

// flintmax: largest consecutive integer representable in floating point
// For IEEE 754 double: 2^53, for float: 2^24
template <typename F>
inline constexpr F flintmax_v
  = static_cast<F> (std::uint64_t{1} << std::numeric_limits<F>::digits);

// Template function to fill array with random integers and cast to target type
// Uses rejection sampling to ensure unbiased results.
template <typename T>
static Array<T>
do_randi_array (const dim_vector& dims, double imin, double imax)
{
  constexpr double flintmax_val = flintmax_v<double>;

  const double rng = (imax - imin) + 1.0;
  const double K = std::floor (flintmax_val / rng);
  const double K_rng = K * rng;

  const octave_idx_type n = dims.numel ();

  NDArray rand_vals = rand::nd_array (dims);
  const double *rdata = rand_vals.data ();

  Array<T> result (dims);
  T *data = result.rwdata ();

  for (octave_idx_type i = 0; i < n; i++)
    {
      double r_prim = std::floor (rdata[i] * flintmax_val);

      // Rejection loop - rarely needed since K_rng is very close to flintmax
      while (r_prim >= K_rng)
        r_prim = std::floor (rand::scalar () * flintmax_val);

      data[i] = static_cast<T> (imin + std::floor (r_prim / K));
    }

  return result;
}

// Specialization for double: work in-place to avoid extra allocation
template <>
Array<double>
do_randi_array<double> (const dim_vector& dims, double imin, double imax)
{
  constexpr double flintmax_val = flintmax_v<double>;

  const double rng = (imax - imin) + 1.0;
  const double K = std::floor (flintmax_val / rng);
  const double K_rng = K * rng;

  const octave_idx_type n = dims.numel ();

  // Generate random values and transform in-place
  NDArray result = rand::nd_array (dims);
  double *data = result.rwdata ();

  for (octave_idx_type i = 0; i < n; i++)
    {
      double r_prim = std::floor (data[i] * flintmax_val);

      // Rejection loop - rarely needed since K_rng is very close to flintmax
      while (r_prim >= K_rng)
        r_prim = std::floor (rand::scalar () * flintmax_val);

      data[i] = imin + std::floor (r_prim / K);
    }

  return result;
}

// Specialization for float: work in-place to avoid extra allocation
template <>
Array<float>
do_randi_array<float> (const dim_vector& dims, double imin, double imax)
{
  constexpr double flintmax_val = flintmax_v<double>;

  const double rng = (imax - imin) + 1.0;
  const double K = std::floor (flintmax_val / rng);
  const double K_rng = K * rng;

  const octave_idx_type n = dims.numel ();

  // Generate random values in bulk, then transform
  // Use double precision for intermediate calculations to maintain accuracy
  NDArray rand_vals = rand::nd_array (dims);
  const double *rdata = rand_vals.data ();

  FloatNDArray result (dims);
  float *data = result.rwdata ();

  for (octave_idx_type i = 0; i < n; i++)
    {
      double r_prim = std::floor (rdata[i] * flintmax_val);

      // Rejection loop - rarely needed since K_rng is very close to flintmax
      while (r_prim >= K_rng)
        r_prim = std::floor (rand::scalar () * flintmax_val);

      data[i] = static_cast<float> (imin + std::floor (r_prim / K));
    }

  return result;
}

// Specialization for bool (logical): MATLAB uses value > 0, not value != 0
// so calls like randi ([-1, 0], 3, 3, 'logical') return all zeros.
template <>
Array<bool>
do_randi_array<bool> (const dim_vector& dims, double imin, double imax)
{
  constexpr double flintmax_val = flintmax_v<double>;

  const double rng = (imax - imin) + 1.0;
  const double K = std::floor (flintmax_val / rng);
  const double K_rng = K * rng;

  const octave_idx_type n = dims.numel ();

  NDArray rand_vals = rand::nd_array (dims);
  const double *rdata = rand_vals.data ();

  boolNDArray result (dims);
  bool *data = result.rwdata ();

  for (octave_idx_type i = 0; i < n; i++)
    {
      double r_prim = std::floor (rdata[i] * flintmax_val);

      while (r_prim >= K_rng)
        r_prim = std::floor (rand::scalar () * flintmax_val);

      // MATLAB logical: value > 0 gives true, otherwise false
      double value = imin + std::floor (r_prim / K);
      data[i] = value > 0.0;
    }

  return result;
}

DEFUN (randi, args, ,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{R} =} randi (@var{imax})
@deftypefnx {} {@var{R} =} randi (@var{imax}, @var{n})
@deftypefnx {} {@var{R} =} randi (@var{imax}, @var{m}, @var{n}, @dots{})
@deftypefnx {} {@var{R} =} randi (@var{imax}, [@var{m}, @var{n}, @dots{}])
@deftypefnx {} {@var{R} =} randi ([@var{imin}, @var{imax}], @dots{})
@deftypefnx {} {@var{R} =} randi (@dots{}, "@var{class}")
Return a scalar, matrix, or N-dimensional array whose elements are random
integers in the range @w{[1, @var{imax}]}.

If called with no size arguments, return a scalar random value.

If invoked with a single scalar size argument @var{n}, return a square
@nospell{NxN} matrix.

If invoked with two or more scalar integer size arguments, or a vector of
integer values, return an array with the given dimensions.

The integer range may optionally be described by a two-element matrix with a
lower and upper bound in which case the returned integers will be on the
interval @w{[@var{imin}, @var{imax}]}.

The optional argument @var{class} will return a matrix of the requested type.
The default is @qcode{"double"}.  Supported classes are: @qcode{"double"},
@qcode{"single"}, @qcode{"int8"}, @qcode{"int16"}, @qcode{"int32"},
@qcode{"uint8"}, @qcode{"uint16"}, @qcode{"uint32"}, and @qcode{"logical"}.

Programming Notes: @code{randi} relies internally on @code{rand} which uses
class @qcode{"double"} to represent numbers.  This limits the maximum integer
(@var{imax}) and range (@var{imax} - @var{imin}) to the value returned by the
@code{flintmax} function.  For IEEE@tie{}754 floating point numbers this value
is @w{@math{2^{53} - 1}}.

When the output class is @qcode{"logical"} the function constructs an array of
random integers as specified and then applies the test @w{@code{@var{x} > 0}}
to determine which elements will be true.  Because the one-input form of the
function uses @code{1} for @var{imin} @code{randi} will always return a matrix
of all ones.

Example: 150 integers in the range 1--10.

@example
ri = randi (10, 150, 1)
@end example

@seealso{rand, randn, rande, randg, randp}
@end deftypefn */)
{
  int nargin = args.length ();

  if (nargin < 1)
    print_usage ();

  constexpr double flintmax_dbl = flintmax_v<double>;

  // Parse bounds argument
  octave_value bounds_arg = args(0);

  if (! bounds_arg.isnumeric ())
    error ("randi: IMIN and IMAX must be integer bounds");

  if (bounds_arg.numel () < 1 || bounds_arg.numel () > 2)
    error ("randi: first argument must be scalar IMIN or 2-element vector [IMIN, IMAX]");

  NDArray bounds_array = bounds_arg.array_value ();

  // Check that all bounds are integers
  for (octave_idx_type i = 0; i < bounds_array.numel (); i++)
    {
      double val = bounds_array(i);
      if (val != std::trunc (val))
        error ("randi: IMIN and IMAX must be integer bounds");
    }

  double imin, imax;

  if (bounds_array.numel () == 1)
    {
      imin = 1.0;
      imax = bounds_array(0);
      if (imax < 1)
        error ("randi: IMAX must be >= 1");
    }
  else
    {
      imin = bounds_array(0);
      imax = bounds_array(1);
      if (imax < imin)
        error ("randi: IMIN must be <= IMAX");
    }

  // Check bounds against flintmax
  if (std::abs (imax) >= flintmax_dbl || std::abs (imin) >= flintmax_dbl)
    error ("randi: IMIN and IMAX must be smaller than flintmax");

  if ((imax - imin) >= (flintmax_dbl - 1.0))
    error ("randi: integer range must be smaller than flintmax-1");

  // Parse optional class argument (must be last if present)
  std::string rclass = "double";
  int size_args_end = nargin;

  if (nargin > 1 && args(nargin - 1).is_string ())
    {
      rclass = args(nargin - 1).string_value ();
      size_args_end = nargin - 1;
    }

  // Parse size arguments
  dim_vector dims;
  int size_args_start = 1;

  if (size_args_end == size_args_start)
    {
      // No size arguments - return scalar
      dims.resize (2);
      dims(0) = 1;
      dims(1) = 1;
    }
  else if (size_args_end == size_args_start + 1)
    {
      // Single size argument
      octave_value size_arg = args(size_args_start);

      if (size_arg.is_scalar_type ())
        {
          // FIXME: idx_type_value () produces a generic error
          //        "conversion of XX to int64_t value failed".
          // It might be nicer at some point to use try/catch block to map
          // this to a nicer error message about dimensions must be integers.
          octave_idx_type n = size_arg.idx_type_value (true);
          dims.resize (2);
          dims(0) = (n >= 0 ? n : 0);
          dims(1) = (n >= 0 ? n : 0);
        }
      else if (size_arg.is_matrix_type () || size_arg.is_range ())
        {
          Array<octave_idx_type> size_vec;
          try
            {
              size_vec = size_arg.octave_idx_type_vector_value (true);
            }
          catch (execution_exception& ee)
            {
              error (ee, "randi: dimensions must be a scalar or array of integers");
            }

          octave_idx_type len = size_vec.numel ();
          dims.resize (len);

          for (octave_idx_type i = 0; i < len; i++)
            {
              octave_idx_type d = size_vec(i);
              dims(i) = (d >= 0 ? d : 0);
            }
        }
      else
        error ("randi: dimensions must be a scalar or array of integers");
    }
  else
    {
      // Multiple size arguments
      int ndims = size_args_end - size_args_start;
      dims.resize (ndims);

      for (int i = 0; i < ndims; i++)
        {
          octave_idx_type d = args(size_args_start + i).idx_type_value (true);
          dims(i) = (d >= 0 ? d : 0);
        }
    }

  dims.chop_trailing_singletons ();

  // Validate class and check for range warnings using numeric_limits
  constexpr double flintmax_sgl = flintmax_v<float>;

  double maxval, minval;
  std::string_view rclass_sv = rclass;

  // Dispatch table using lambdas for type-specific generation
  std::function<octave_value (const dim_vector&, double, double)> generator;

  if (rclass_sv == "double")
    {
      maxval = flintmax_dbl;
      minval = -flintmax_dbl;
      generator = do_randi_array<double>;
    }
  else if (rclass_sv == "single")
    {
      maxval = flintmax_sgl;
      minval = -flintmax_sgl;
      generator = do_randi_array<float>;
    }
  else if (rclass_sv == "int8")
    {
      maxval = std::numeric_limits<int8_t>::max ();
      minval = std::numeric_limits<int8_t>::min ();
      generator = do_randi_array<octave_int8>;
    }
  else if (rclass_sv == "int16")
    {
      maxval = std::numeric_limits<int16_t>::max ();
      minval = std::numeric_limits<int16_t>::min ();
      generator = do_randi_array<octave_int16>;
    }
  else if (rclass_sv == "int32")
    {
      maxval = std::numeric_limits<int32_t>::max ();
      minval = std::numeric_limits<int32_t>::min ();
      generator = do_randi_array<octave_int32>;
    }
  else if (rclass_sv == "uint8")
    {
      maxval = std::numeric_limits<uint8_t>::max ();
      minval = std::numeric_limits<uint8_t>::min ();
      generator = do_randi_array<octave_uint8>;
    }
  else if (rclass_sv == "uint16")
    {
      maxval = std::numeric_limits<uint16_t>::max ();
      minval = std::numeric_limits<uint16_t>::min ();
      generator = do_randi_array<octave_uint16>;
    }
  else if (rclass_sv == "uint32")
    {
      maxval = std::numeric_limits<uint32_t>::max ();
      minval = std::numeric_limits<uint32_t>::min ();
      generator = do_randi_array<octave_uint32>;
    }
  else if (rclass_sv == "logical")
    {
      // No range warnings for logical.
      // Any value can be mapped to true (>0) or false (<=0)
      maxval = flintmax_dbl;
      minval = -flintmax_dbl;
      generator = do_randi_array<bool>;
    }
  else
    error ("randi: unknown requested output CLASS '%s'", rclass.c_str ());

  if (imax > maxval)
    warning ("randi: integer IMAX exceeds requested type.  "
             "Values might be truncated to requested type.");

  if (imin < minval)
    warning ("randi: integer IMIN exceeds requested type.  "
             "Values might be truncated to requested type.");

  return generator (dims, imin, imax);
}

/*
%!test
%! ri = randi (10, 1000, 1);
%! assert (ri, fix (ri));
%! assert (min (ri), 1);
%! assert (max (ri), 10);
%! assert (rows (ri), 1000);
%! assert (columns (ri), 1);
%! assert (class (ri), "double");

%!test
%! ri = randi (int64 (100), 1, 1000);
%! assert (ri, fix (ri));
%! assert (min (ri), 1);
%! assert (max (ri), 100);
%! assert (rows (ri), 1);
%! assert (columns (ri), 1000);
%! assert (class (ri), "double");

%!test
%! ri = randi ([-100, 100], 1000, 1);
%! assert (ri, fix (ri));
%! assert (min (ri) >= -100);
%! assert (max (ri) <= 100);
%! assert (class (ri), "double");

%!test
%! ri = randi ([-5, 10], 1000, 1, "int8");
%! assert (ri, fix (ri));
%! assert (min (ri), int8 (-5));
%! assert (max (ri), int8 (10));
%! assert (class (ri), "int8");

%!test
%! ri = randi ([-1000, 1000], 1000, 1, "int16");
%! assert (min (ri) >= int16 (-1000));
%! assert (max (ri) <= int16 (1000));
%! assert (class (ri), "int16");

%!test
%! ri = randi ([-1e6, 1e6], 1000, 1, "int32");
%! assert (min (ri) >= int32 (-1e6));
%! assert (max (ri) <= int32 (1e6));
%! assert (class (ri), "int32");

%!test
%! ri = randi ([0, 200], 1000, 1, "uint8");
%! assert (min (ri) >= uint8 (0));
%! assert (max (ri) <= uint8 (200));
%! assert (class (ri), "uint8");

%!test
%! ri = randi ([100, 1000], 1000, 1, "uint16");
%! assert (min (ri) >= uint16 (100));
%! assert (max (ri) <= uint16 (1000));
%! assert (class (ri), "uint16");

%!test
%! ri = randi ([1e5, 1e6], 1000, 1, "uint32");
%! assert (min (ri) >= uint32 (1e5));
%! assert (max (ri) <= uint32 (1e6));
%! assert (class (ri), "uint32");

%!test
%! ri = randi ([-5; 10], 1000, 1, "single");
%! assert (ri, fix (ri));
%! assert (min (ri), single (-5));
%! assert (max (ri), single (10));
%! assert (class (ri), "single");

## logical: MATLAB uses value > 0 for true, not value != 0
%!test
%! ri = randi ([0, 1], 1000, 1, "logical");
%! assert (islogical (ri));
%! assert (any (ri));        # at least one true (from 1s)
%! assert (! all (ri));      # at least one false (from 0s)

%!test
%! ## [-1, 0] should give all false since neither > 0
%! ri = randi ([-1, 0], 1000, 1, "logical");
%! assert (islogical (ri));
%! assert (! any (ri));      # all false

%!test
%! ## [-1, 1] gives ~1/3 true (only 1 > 0)
%! ri = randi ([-1, 1], 10000, 1, "logical");
%! assert (islogical (ri));
%! assert (any (ri));        # some true
%! assert (! all (ri));      # some false
%! assert (mean (ri), 1/3, 0.05);  # approximately 1/3 true

%!test
%! ## imax >= 1 always returns all true
%! ri = randi (10, 3, 3, "logical");
%! assert (islogical (ri));
%! assert (size (ri), [3, 3]);
%! assert (all (ri(:)));

%!assert (size (randi (10, 3, 1, 2)), [3, 1, 2])
%!assert (size (randi (10, [3, 1, 2])), [3, 1, 2])
%!assert (size (randi ([1, 10], [3, 4])), [3, 4])
%!assert (size (randi ([1, 10], [3, 4], "int8")), [3, 4])
%!assert (class (randi ([1, 10], [3, 4], "int16")), "int16")

%!shared max_int8, min_int8, max_uint8, min_uint8, max_single
%! max_int8 = double (intmax ("int8"));
%! min_int8 = double (intmin ("int8"));
%! max_uint8 = double (intmax ("uint8"));
%! min_uint8 = double (intmin ("uint8"));
%! max_single = double (flintmax ("single"));

## Test that no warning thrown if IMAX is exactly on the limits of the range
%!function test_no_warning (fcn, varargin)
%!  lastwarn ("");
%!  fcn (varargin{:});
%!  assert (lastwarn (), "");
%!endfunction

%!test test_no_warning (@randi, max_int8, "int8");
%!test test_no_warning (@randi, max_uint8, "uint8");
%!test test_no_warning (@randi, max_single, "single");
%!test test_no_warning (@randi, [min_int8, max_int8], "int8");
%!test test_no_warning (@randi, [min_uint8, max_uint8], "uint8");
%!test test_no_warning (@randi, [-max_single, max_single], "single");

## Test exceeding range
%!warning <exceeds requested type>
%! randi ([min_int8-1, max_int8], "int8");
%!warning <exceeds requested type>
%! randi ([min_uint8-1, max_uint8], "uint8");
%!warning <exceeds requested type>
%! randi ([min_int8, max_int8 + 1], "int8");
%!warning <exceeds requested type>
%! randi ([min_uint8, max_uint8 + 1], "uint8");
%!warning <exceeds requested type>
%! randi ([0, max_single + 1], "single");
%!warning <exceeds requested type>
%! ri = randi ([-5, 10], 1000, 1, "uint8");
%! assert (ri, fix (ri));
%! assert (min (ri), uint8 (-5));
%! assert (max (ri), uint8 (10));
%! assert (class (ri), "uint8");

## Test input validation
%!error <Invalid call> randi ()
%!error <must be integer bounds> randi ("test")
%!error <must be integer bounds> randi (struct ("a", 1))
%!error <first argument must be scalar IMIN> randi ([])
%!error <first argument must be .* 2-element vector> randi ([1,2,3])
%!error <must be integer bounds> randi (1.5)
%!error <must be integer bounds> randi ([1.5, 2])
%!error <must be integer bounds> randi ([1, 2.5])
%!error <must be integer bounds> randi ([1.5, 2.5])
%!error <IMAX must be .= 1> randi (0)
%!error <IMIN must be <= IMAX> randi ([10, 1])
%!error <IMIN and IMAX must be smaller than flintmax> randi (flintmax ())
%!error <range must be smaller than flintmax-1> randi ([-1, flintmax() - 1])
%!error randi (10, 1.5)
%!error <dimensions must be .* array of integers> randi (10, [1.5, 2])
%!error <dimensions must be .* array of integers> randi (10, 1.5:0.5:3)
%!error <dimensions must be a scalar or array of integers> randi (10, {1, 2})
%!error randi (10, 1.5, 2)
%!error <unknown requested output CLASS 'foo'> randi (10, "foo")
*/

OCTAVE_END_NAMESPACE(octave)
