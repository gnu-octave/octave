########################################################################
##
## Copyright (C) 2001-2024 The Octave Project Developers
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
## @deftypefn  {} {@var{s} =} rat (@var{x})
## @deftypefnx {} {@var{s} =} rat (@var{x}, @var{tol})
## @deftypefnx {} {[@var{n}, @var{d}] =} rat (@dots{})
##
## Find a rational approximation of @var{x} to within the tolerance defined by
## @var{tol}.
##
## If unspecified, the default tolerance is @code{1e-6 * norm (@var{x}(:), 1)}.
##
## When called with one output argument, return a string containing a
## continued fraction expansion (multiple terms).
##
## When called with two output arguments, return numeric matrices for the
## numerator and denominator of a fractional representation of @var{x} such
## that @code{@var{x} = @var{n} ./ @var{d}}.
##
## For example:
##
## @example
## @group
## s = rat (pi)
## @result{} s = 3 + 1/(7 + 1/16)
##
## [n, d] = rat (pi)
## @result{} n =  355
## @result{} d =  113
##
## n / d - pi
## @result{} 2.6676e-07
## @end group
## @end example
##
## Complex inputs are similar:
##
## @example
## @group
## s = rat (0.5 + i * pi)
## @result{} s = (1 + 1/(-2)) + (3 + 1/(7 + 1/16)) * i
##
## [n, d] = rat (0.5 + i * pi)
## @result{} n =  113 + 710i
## @result{} d =  226
##
## n / d - (0.5 + i * pi)
## @result{} 0 + 2.6676e-07i
## @end group
## @end example
##
## Programming Note: With one output @code{rat} produces a string which is a
## continued fraction expansion.  To produce a string which is a simple
## fraction (one numerator, one denominator) use @code{rats}.
##
## @seealso{rats, format}
## @end deftypefn

function [n, d] = rat (x, tol)

  if (nargin < 1)
    print_usage ();
  endif

  if (! isfloat (x))
    error ("rat: X must be a single or double array");
  endif

  y = x(:);

  ## Replace Inf with 0 while calculating ratios.
  inf_idx = isinf (x);
  y(inf_idx(:)) = 0;

  if (nargin == 1)
    ## default norm
    tol = 1e-6 * norm (y, 1);
  else
    if (! (isscalar (tol) && isnumeric (tol) && tol >= 0))
      error ("rat: TOL must be a numeric scalar >= 0");
    endif
  endif

  if (iscomplex (x))
    if (nargout == 2)  # return numerator and denominator
      [nr, dr] = rat (real (x), tol);
      [ni, di] = rat (imag (x), tol);
      d = lcm (dr, di);
      n = nr .* (d ./ dr) + ni .* (d ./ di) * i;
    elseif (nargout <= 1)  # string output
      realstr = rat (real (x), tol);
      imagstr = rat (imag (x), tol);
      n = [repmat("(", rows (realstr), 1), realstr, repmat(") + (", rows (realstr), 1), imagstr, repmat(") * i", rows (imagstr), 1)];
    endif
    return
  endif

  ## First step in the approximation is the integer portion

  ## First element in the continued fraction.
  n = round (y);
  d = ones (size (y));
  frac = y - n;
  lastn = ones (size (y));
  lastd = zeros (size (y));

  nsz = numel (y);
  steps = zeros ([nsz, 0]);

  ## Grab new factors until all continued fractions converge.
  while (1)
    ## Determine which fractions have not yet converged.
    idx = find (y != 0 & abs (y - n./d) >= tol);
    if (isempty (idx))
      if (isempty (steps))
        steps = NaN (nsz, 1);
      endif
      break;
    endif

    ## Grab the next step in the continued fraction.
    flip = 1 ./ frac(idx);
    ## Next element in the continued fraction.
    step = round (flip);

    if (nargout < 2)
      tsteps = NaN (nsz, 1);
      tsteps(idx) = step;
      steps = [steps, tsteps];
    endif

    frac(idx) = flip - step;

    ## Update the numerator/denominator.
    savedn = n;
    savedd = d;
    n(idx) = n(idx).*step + lastn(idx);
    d(idx) = d(idx).*step + lastd(idx);
    lastn = savedn;
    lastd = savedd;
  endwhile

  if (nargout <= 1)
    ## string output
    n = "";
    nsteps = columns (steps);
    ## Loop over all values in array
    for i = 1:nsz

      if (inf_idx(i))
        s = ifelse (x(i) > 0, "Inf", "-Inf");
      elseif (y(i) == 0)
        s = "0";
      else
        ## Create partial fraction expansion of one value
        s = [int2str(y(i)), " "];
        j = 1;

        while (true)
          step = steps(i, j++);
          if (isnan (step))
            break;
          endif
          if (j > nsteps || isnan (steps(i, j)))
            if (step < 0)
              s = [s(1:end-1), " + 1/(", int2str(step), ")"];
            else
              s = [s(1:end-1), " + 1/", int2str(step)];
            endif
            break;
          else
            s = [s(1:end-1), " + 1/(", int2str(step), ")"];
          endif
        endwhile
        s = [s, repmat(")", 1, j-2)];
      endif

      ## Append result to output
      n_nc = columns (n);
      s_nc = columns (s);
      if (n_nc > s_nc)
        s(:, s_nc+1:n_nc) = " ";
      elseif (s_nc > n_nc && n_nc != 0)
        n(:, n_nc+1:s_nc) = " ";
      endif
      n = cat (1, n, s);
    endfor
  else
    ## numerator, denominator output

    ## Move the minus sign to the numerator.
    n .*= sign (d);
    d = abs (d);

    ## Return the same shape as the input.
    n = reshape (n, size (x));
    d = reshape (d, size (x));

    ## Use 1/0 for Inf.
    n(inf_idx) = sign (x(inf_idx));
    d(inf_idx) = 0;
  endif

endfunction


%!assert (rat (pi), "3 + 1/(7 + 1/16)")
%!assert (rat (pi, 1e-2), "3 + 1/7")
## Test exceptional values
%!assert (rat (0), "0")
%!assert (rat (Inf), "Inf")
%!assert (rat (-Inf), "-Inf")

%!test
%! [n, d] = rat ([0.5, 0.3, 1/3]);
%! assert (n, [1, 3, 1]);
%! assert (d, [2, 10, 3]);
## Test exceptional values
%!test
%! [n, d] = rat ([Inf, 0, -Inf]);
%! assert (n, [1, 0, -1]);
%! assert (d, [0, 1, 0]);

## Test complex scalar input
%!test <*55198>
%! assert (rat (complex (0.5, pi)), "(1 + 1/(-2)) + (3 + 1/(7 + 1/16)) * i");
%! [n, d] = rat (complex (0.5, pi));
%! assert (n, 113 + 710*i);
%! assert (d, 226);

## Test complex vector input in all four quadrants
%!test <*55198>
%! theta = 72 * (1:4);
%! x = cosd (theta) + i * sind (theta);
%! [n, d] = rat (x);
%! assert (n, [274195+843885i, -39955+29029i, -39955-29029i, 274195-843885i]);
%! assert (d, [887313, 49387, 49387, 887313]);
%! assert (all (abs (n ./ d - x) <= 2e-6));
%! str = rat (x);
%! assert (str(1, :), "(0 + 1/(3 + 1/(4 + 1/(4 + 1/(4 + 1/4))))) + (1 + 1/(-20 + 1/(-2 + 1/(-3 + 1/(-6))))) * i");
%! assert (str(2, :), "(-1 + 1/(5 + 1/(4 + 1/(4 + 1/4)))       ) + (1 + 1/(-2 + 1/(-2 + 1/(-3 + 1/8)))    ) * i");
%! assert (str(3, :), "(-1 + 1/(5 + 1/(4 + 1/(4 + 1/4)))       ) + (-1 + 1/(2 + 1/(2 + 1/(3 + 1/(-8))))   ) * i");
%! assert (str(4, :), "(0 + 1/(3 + 1/(4 + 1/(4 + 1/(4 + 1/4))))) + (-1 + 1/(20 + 1/(2 + 1/(3 + 1/6)))     ) * i");

## Test complex exceptional inputs
%!test <*55198>
%! assert (rat (complex (inf, 0)), "(Inf) + (0) * i");
%! assert (rat (complex (0, inf)), "(0) + (Inf) * i");

## Test eval with complex inputs
%!test <*55198>
%! x = complex (0.5, pi);
%! assert (eval (rat (x)), x, 1e-6 * norm (x, 1))

%!assert <*43374> (eval (rat (0.75)), [0.75])

## Test input validation
%!error <Invalid call> rat ()
%!error <X must be a single or double array> rat (int8 (3))
%!error <TOL must be a numeric scalar> rat (1, "a")
%!error <TOL must be a numeric scalar> rat (1, [1 2])
%!error <TOL must be a numeric scalar .* 0> rat (1, -1)
