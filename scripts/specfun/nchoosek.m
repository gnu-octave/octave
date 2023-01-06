########################################################################
##
## Copyright (C) 2001-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{c} =} nchoosek (@var{n}, @var{k})
## @deftypefnx {} {@var{c} =} nchoosek (@var{set}, @var{k})
##
## Compute the binomial coefficient of @var{n} or list all possible
## combinations of a @var{set} of items.
##
## If @var{n} is a scalar then calculate the binomial coefficient
## of @var{n} and @var{k} which is defined as
## @tex
## $$
##  {n \choose k} = {n (n-1) (n-2) \cdots (n-k+1) \over k!}
##                = {n! \over k! (n-k)!}
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##  /   \
##  | n |    n (n-1) (n-2) @dots{} (n-k+1)       n!
##  |   |  = ------------------------- =  ---------
##  | k |               k!                k! (n-k)!
##  \   /
## @end group
## @end example
##
## @end ifnottex
## @noindent
## This is the number of combinations of @var{n} items taken in groups of
## size @var{k}.
##
## If the first argument is a vector, @var{set}, then generate all
## combinations of the elements of @var{set}, taken @var{k} at a time, with
## one row per combination.  The result @var{c} has @var{k} columns and
## @w{@code{nchoosek (length (@var{set}), @var{k})}} rows.
##
## For example:
##
## How many ways can three items be grouped into pairs?
##
## @example
## @group
## nchoosek (3, 2)
##    @result{} 3
## @end group
## @end example
##
## What are the possible pairs?
##
## @example
## @group
## nchoosek (1:3, 2)
##    @result{}  1   2
##        1   3
##        2   3
## @end group
## @end example
##
## Programming Note: When calculating the binomial coefficient @code{nchoosek}
## works only for non-negative, integer arguments.  Use @code{bincoeff} for
## non-integer and negative scalar arguments, or for computing many binomial
## coefficients at once with vector inputs for @var{n} or @var{k}.
##
## @seealso{bincoeff, perms}
## @end deftypefn

function C = nchoosek (v, k)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isvector (v))
    error ("nchoosek: first argument must be a scalar or a vector");
  endif
  if (! (isreal (k) && isscalar (k) && k >= 0 && k == fix (k)))
    error ("nchoosek: K must be an integer >= 0");
  endif
  if (isscalar (v))
    if (isnumeric (v) && (iscomplex (v) || v < k || v < 0 || v != fix (v)))
      error ("nchoosek: N must be a non-negative integer >= K");
    endif
  endif

  v = v(:).';  # convert to row vector
  n = numel (v);

  if (n == 1 && isnumeric (v))
    ## Improve precision over direct call to prod().
    ## Steps: 1) Make a list of integers for numerator and denominator,
    ## 2) filter out common factors, 3) multiply what remains.
    k = min (k, v-k);

    if (isinteger (v) || isinteger (k))
      numer = (v-k+1):v;
      denom = (1:k);
    else
      ## For a ~25% performance boost, multiply values pairwise so there
      ## are fewer elements in do/until loop which is the slow part.
      ## Since Odd*Even is guaranteed to be Even, also take out a factor
      ## of 2 from numerator and denominator.
      if (rem (k, 2))  # k is odd
        numer = [((v-k+1:v-(k+1)/2) .* (v-1:-1:v-(k-1)/2)) / 2, v];
        denom = [((1:(k-1)/2) .* (k-1:-1:(k+1)/2)) / 2, k];
      else             # k is even
        numer = ((v-k+1:v-k/2) .* (v:-1:v-k/2+1)) / 2;
        denom = ((1:k/2) .* (k:-1:k/2+1)) / 2;
      endif
    endif

    ## Remove common factors from numerator and denominator
    do
      for i = numel (denom):-1:1
        factors = gcd (denom(i), numer);
        [f, j] = max (factors);
        denom(i) /= f;
        numer(j) /= f;
      endfor
      denom = denom(denom > 1);
      numer = numer(numer > 1);
    until (isempty (denom))

    C = prod (numer, "native");
    if (isfloat (C) && C > flintmax (C))
      warning ("Octave:nchoosek:large-output-float", ...
               "nchoosek: possible loss of precision");
    elseif (isinteger (C) && C == intmax (C))
      warning ("Octave:nchoosek:large-output-integer", ...
               "nchoosek: result may have saturated at intmax");
    endif
  elseif (k == 0)
    C = v(zeros (1, 0));  # Return 1x0 object for Matlab compatibility
  elseif (k == 1)
    C = v(:);
  elseif (k == n)
    C = v;
  elseif (k > n)
    C = v(zeros (0, k));  # return 0xk object for Matlab compatibility
  elseif (k == 2)
    ## Can do it without transpose.
    x = repelem (v(1:n-1), [n-1:-1:1]).';
    y = cat (1, cellslices (v(:), 2:n, n*ones (1, n-1)){:});
    C = [x, y];
  elseif (k < n)
    C = v(k:n);
    l = 1:n-k+1;
    for j = 2:k
      c = columns (C);
      cA = cellslices (C, l, c*ones (1, n-k+1), 2);
      l = c-l+1;
      b = repelem (v(k-j+1:n-j+1), l);
      C = [b; cA{:}];
      l = cumsum (l);
      l = [1, 1 + l(1:n-k)];
    endfor
    C = C.';
  endif

endfunction


%!assert (nchoosek (80, 10), bincoeff (80, 10))
%!assert (nchoosek (1:5, 3),
%!        [1:3;1,2,4;1,2,5;1,3,4;1,3,5;1,4,5;2:4;2,3,5;2,4,5;3:5])

## Test basic behavior for various input types
%!assert (nchoosek ('a':'b', 2), 'ab')
%!assert (nchoosek ("a":"b", 2), "ab")
%!assert (nchoosek ({1,2}, 2), {1,2})
%!test
%! s(1).a = 1;
%! s(2).a = 2;
%! assert (nchoosek (s, 1), s(:));
%! assert (nchoosek (s, 2), s);

## Verify Matlab compatibility of return sizes & types
%!test
%! x = nchoosek (1:2, 0);
%! assert (size (x), [1, 0]);
%! assert (isa (x, "double"));
%! x = nchoosek (1:2, 3);
%! assert (size (x), [0, 3]);
%! assert (isa (x, "double"));

%!test
%! x = nchoosek (single (1:2), 0);
%! assert (size (x), [1, 0]);
%! assert (isa (x, "single"));
%! x = nchoosek (single (1:2), 3);
%! assert (size (x), [0, 3]);
%! assert (isa (x, "single"));

%!test
%! x = nchoosek ('a':'b', 0);
%! assert (size (x), [1, 0]);
%! assert (is_sq_string (x));
%! x = nchoosek ('a':'b', 3);
%! assert (size (x), [0, 3]);
%! assert (is_sq_string (x));

%!test
%! x = nchoosek ("a":"b", 0);
%! assert (size (x), [1, 0]);
%! assert (is_dq_string (x));
%! x = nchoosek ("a":"b", 3);
%! assert (size (x), [0, 3]);
%! assert (is_dq_string (x));

%!test
%! x = nchoosek (uint8(1):uint8(2), 0);
%! assert (size (x), [1, 0]);
%! assert (isa (x, "uint8"));
%! x = nchoosek (uint8(1):uint8(2), 3);
%! assert (size (x), [0, 3]);
%! assert (isa (x, "uint8"));

%!test
%! x = nchoosek ({1, 2}, 0);
%! assert (size (x), [1, 0]);
%! assert (iscell (x));
%! x = nchoosek ({1, 2}, 3);
%! assert (size (x), [0, 3]);
%! assert (iscell (x));

%!test
%! s.a = [1 2 3];
%! s.b = [4 5 6];
%! x = nchoosek (s, 0);
%! assert (size (x), [1, 0]);
%! assert (isstruct (x));
%! assert (fieldnames (x), {"a"; "b"});
%! x = nchoosek (s, 3);
%! assert (size (x), [0, 3]);
%! assert (isstruct (x));
%! assert (fieldnames (x), {"a"; "b"});

%!test
%! s.a = [1 2 3];
%! s.b = [4 5 6];
%! s(2).a = 1;  # make s a struct array rather than scalar struct
%! s(3).b = 2;  # make s at least three elements for k == 2 test below
%! x = nchoosek (s, 0);
%! assert (size (x), [1, 0]);
%! assert (isstruct (x));
%! assert (fieldnames (x), {"a"; "b"});
%! x = nchoosek (s, 2);
%! assert (size (x), [3, 2]);
%! assert (isstruct (x));
%! assert (fieldnames (x), {"a"; "b"});
%! x = nchoosek (s, 4);
%! assert (size (x), [0, 4]);
%! assert (isstruct (x));
%! assert (fieldnames (x), {"a"; "b"});

%!test <61565>
%! x = nchoosek (uint8 (10), uint8 (5));
%! assert (x, uint8 (252));
%! assert (class (x), "uint8");

%!test <*63538>
%! x = nchoosek ([1:3]', 2);
%! assert (x, [1 2; 1 3; 2 3]);

## Test input validation
%!error <Invalid call> nchoosek ()
%!error <Invalid call> nchoosek (1)
%!error <first argument must be a scalar or a vector> nchoosek (ones (3, 3), 1)
%!error <K must be an integer .= 0> nchoosek (100, 2i)
%!error <K must be an integer .= 0> nchoosek (100, [2 3])
%!error <K must be an integer .= 0> nchoosek (100, -45)
%!error <K must be an integer .= 0> nchoosek (100, 45.5)
%!error <N must be a non-negative integer .= K> nchoosek (100i, 2)
%!error <N must be a non-negative integer .= K> nchoosek (100, 145)
%!error <N must be a non-negative integer .= K> nchoosek (-100, 45)
%!error <N must be a non-negative integer .= K> nchoosek (100.5, 45)
%!warning <possible loss of precision> nchoosek (100, 45);
%!warning <result .* saturated> nchoosek (uint64 (80), uint64 (40));
%!warning <result .* saturated> nchoosek (uint32 (80), uint32 (40));
%!warning <result .* saturated> nchoosek (uint16 (80), uint16 (40));
%!warning <result .* saturated> nchoosek ( uint8 (80),  uint8 (40));
%!warning <result .* saturated> nchoosek ( int64 (80),  int64 (40));
%!warning <result .* saturated> nchoosek ( int32 (80),  int32 (40));
%!warning <result .* saturated> nchoosek ( int16 (80),  int16 (40));
%!warning <result .* saturated> nchoosek (  int8 (80),   int8 (40));
