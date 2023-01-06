########################################################################
##
## Copyright (C) 2020-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {} rng (@var{seed})
## @deftypefnx {} {} rng (@var{seed}, "@var{generator}")
## @deftypefnx {} {} rng ("shuffle")
## @deftypefnx {} {} rng ("shuffle", "@var{generator}")
## @deftypefnx {} {} rng ("default")
## @deftypefnx {} {@var{s} =} rng ()
## @deftypefnx {} {} rng (@var{s})
## @deftypefnx {} {@var{s} =} rng (@dots{})
## Set or query the seed of the random number generator used by @code{rand} and
## @code{randn}.
##
## The input @var{seed} is a scalar numeric value used to initialize the state
## vector of the random number generator.
##
## The optional string @var{generator} specifies the type of random number
## generator to be used.  Its value can be @qcode{"twister"},
## @qcode{"v5uniform"}, or @qcode{"v5normal"}.  The @qcode{"twister"} keyword
## is described below.  @qcode{"v5uniform"} and @qcode{"v5normal"} refer to
## older versions of Octave that used to use a different random number
## generator.
##
## The state or seed of the random number generator can be reset to a new
## random value using the @qcode{"shuffle"} keyword.
##
## The random number generator can be reset to default values using the
## @qcode{"default"} keyword.  The default values are to use the Mersenne
## Twister generator with a seed of 0.
##
## The optional return value @var{s} contains the state of the random number
## generator at the time the function is called (i.e., before it might be
## modified according to the input arguments).  It is encoded as a structure
## variable with three fields: @qcode{"Type"}, @qcode{"Seed"}, and
## @qcode{"State"}.  The random number generator can be restored to the state
## @var{s} using @code{rng (@var{s})}.  This is useful when the identical
## sequence of pseudo-random numbers is required for an algorithm.
##
## By default, and with the @qcode{"twister"} option, pseudo-random sequences
## are computed using the Mersenne Twister with a period of @math{2^{19937}-1}
## (See @nospell{M. Matsumoto and T. Nishimura},
## @cite{Mersenne Twister: A 623-dimensionally equidistributed uniform
## pseudorandom number generator},
## @nospell{ACM} Trans.@: on Modeling and Computer Simulation Vol.@: 8, No.@: 1,
## pp.@: 3--30, January 1998,
## @url{http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html}).
## Do @strong{not} use for cryptography without securely hashing several
## returned values together, otherwise the generator state can be learned after
## reading 624 consecutive values.
##
## @seealso{rand, randn}
## @end deftypefn

function s = rng (varargin)

  if (nargin > 2)
    print_usage ();
  endif

  ## Store current settings of random number generator
  ## FIXME: there doesn't seem to be a way to query the type of generator
  ##        currently used in Octave - assume "twister".
  ## FIXME: there doesn't seem to be a way to query the seed initialization
  ##        value - use "Not applicable".
  ## FIXME: rand and randn use different generators - storing both states.
  ## For older Matlab generators (v4, v5), the settings are stored like this:
  ##   struct ("Type","Legacy", "Seed", "Not applicable", "State",{[],[],...})

  ## Type is the generator name.
  ## Seed is the initial seed value.
  ## State is a structure describing internal state of the generator.
  srng = struct ("Type", "twister",
                 "Seed", "Not applicable",
                 "State", {{rand("state"), randn("state")}});

  if (nargin == 0)
    s = srng;
    return;
  endif

  arg1 = varargin{1};
  if (isscalar (arg1) && isnumeric (arg1) && isreal (arg1) && arg1 >= 0)
    s_rand = s_randn = arg1;
    generator = check_generator (varargin(2:end));

  elseif (ischar (arg1) && strcmpi (arg1, "shuffle"))
    ## Seed the random number generator based on the current time
    s_rand = s_randn = "reset";  # or sum (1000*clock)
    generator = check_generator (varargin(2:end));

  elseif (ischar (arg1) && strcmpi (arg1, "default") && nargin == 1)
    generator = "twister";
    s_rand = s_randn = 0;  # FIXME: In Matlab, seed 0 corresponds to 5489

  elseif (isstruct (arg1) && isscalar (arg1) && nargin == 1)
    if (numfields (arg1) != 3
        || ! all (isfield (arg1, {"Type", "Seed", "State"})))
      error ('rng: input structure requires "Type", "Seed", "State" fields"');
    endif
    ## Only the internal state "State" and generator type "Type" are needed
    generator = arg1.Type;
    if (iscell (arg1.State))
      [s_rand, s_randn] = deal (arg1.State{:});
    else
      s_rand = s_randn = arg1.State;
    endif

  else
    print_usage ();
  endif

  ## Set the type of random number generator and seed it
  if (isempty (generator))
    generator = srng.Type;
  endif
  switch (generator)
    case "twister"
      rand ("state", s_rand);
      randn ("state", s_randn);

    case "legacy"
      rand ("seed", s_rand);
      randn ("seed", s_randn);

    case "v5uniform"
      rand ("seed", s_rand);

    case "v5normal"
      randn ("seed", s_randn);

    otherwise
      error ('rng: invalid GENERATOR: "%s"', generator);

  endswitch

  if (nargout > 0)
    s = srng;
  endif

endfunction


function gen = check_generator (val)

  if (isempty (val))
    gen = "";
    return;
  elseif (! iscellstr (val))
    error ("rng: GENERATOR must be a string");
  endif

  gen = tolower (char (val));
  if (any (strcmp (gen, {"simdtwister", "combrecursive", "philox", "threefry", "multfibonacci", "v4"})))
    error ('rng: random number generator "%s" is not available in Octave', gen);
  elseif (! any (strcmp (gen, {"twister", "v5uniform", "v5normal"})))
    error ('rng: unknown random number generator "%s"', gen);
  endif

endfunction


%!test
%! state = rng ();
%! unwind_protect
%!   rng (42);
%!   ru1 = rand ();
%!   rn1 = randn ();
%!   rng (42);
%!   ru2 = rand ();
%!   rn2 = randn ();
%!   assert (ru2, ru1);
%!   assert (rn2, rn1);
%!   s1 = rng ();
%!   s2 = rng (42);
%!   assert (isequal (s1, s2));
%!   ru1 = rand ();
%!   rn1 = randn ();
%!   s3 = rng (42);
%!   ru2 = rand ();
%!   rn2 = randn ();
%!   assert (ru2, ru1);
%!   assert (rn2, rn1);
%! unwind_protect_cleanup
%!   rng (state);
%! end_unwind_protect

%!test
%! state = rng ();
%! unwind_protect
%!   rng (42, "twister");
%!   ru1 = rand ();
%!   rn1 = randn ();
%!   rng (42, "twister");
%!   ru2 = rand ();
%!   rn2 = randn ();
%!   assert (ru2, ru1);
%!   assert (rn2, rn1);
%!   s1 = rng ();
%!   s2 = rng (42, "twister");
%!   assert (isequal (s1, s2));
%!   ru1 = rand ();
%!   rn1 = randn ();
%!   s3 = rng (42, "twister");
%!   ru2 = rand ();
%!   rn2 = randn ();
%!   assert (ru2, ru1);
%!   assert (rn2, rn1);
%! unwind_protect_cleanup
%!   rng (state);
%! end_unwind_protect

%!test
%! state = rng ();
%! unwind_protect
%!   rng ("shuffle");
%!   rng ("shuffle", "twister");
%!   s = rng ("shuffle");
%!   assert (! isequal (s, rng ("shuffle")));
%!   s = rng ("shuffle", "twister");
%!   assert (! isequal (s, rng ("shuffle", "twister")));
%! unwind_protect_cleanup
%!   rng (state);
%! end_unwind_protect

%!test
%! state = rng ();
%! unwind_protect
%!   rng ("default");
%!   ru1 = rand ();
%!   rn1 = randn ();
%!   rng (0, "twister");
%!   ru2 = rand ();
%!   rn2 = randn ();
%!   assert (ru2, ru1);
%!   assert (rn2, rn1);
%!   rng (0, "twister");
%!   s = rng ("default");
%!   assert (isequal (s, rng ()));
%! unwind_protect_cleanup
%!   rng (state);
%! end_unwind_protect

%!test
%! state = rng ();
%! unwind_protect
%!   s = rng ();
%!   ru1 = rand ();
%!   rn1 = randn ();
%!   rng (s);
%!   ru2 = rand ();
%!   rn2 = randn ();
%!   assert (ru2, ru1);
%!   assert (rn2, rn1);
%!   rng (42);  rand (1,2);  x = rand (1,2);
%!   rng (42);  rand (1,2);  s = rng ();  y = rand (1,2);
%!   assert (x, y);
%!   rng (s);  z = rand (1,2);
%!   assert (x, z);
%!   s1 = rng ();
%!   s2 = rng (s1);
%!   assert (isequal (s1, s2));
%! unwind_protect_cleanup
%!   rng (state);
%! end_unwind_protect

## Test input validation
%!error <Invalid call> rng (1, 2, 3)
%!error <Invalid call> rng (eye (2))
%!error <Invalid call> rng ({})
%!error <Invalid call> rng (2i)
%!error <Invalid call> rng (-2)
%!error <Invalid call> rng ("foobar")
%!error <Invalid call> rng ("default", "twister")
%!error <Invalid call> rng (struct ("Seed", {1, 2}))
%!error <Invalid call> rng (struct ("Type",[],"State",[],"Seed",[]), "twister")
%!error <input structure requires "Type"> rng (struct ())
%!error <input structure requires "Type">
%! rng (struct ("Type1",[],"State",[],"Seed",[]));
%!error <GENERATOR must be a string> rng (0, struct ())
%!error <"philox" is not available in Octave> rng (0, "philox")
%!error <GENERATOR must be a string> rng ("shuffle", struct ())
%!error <unknown random number generator "foobar"> rng ("shuffle", "foobar")
