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
## @result{} s = complex (1 + 1/(-2), 3 + 1/(7 + 1/16))
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
## Programming Notes:
##
## 1. With one output @code{rat} produces a string which is a continued
## fraction expansion.  To produce a string which is a simple fraction
## (one numerator, one denominator) use @code{rats}.
##
## 2. The string output produced by @code{rat} can be passed to @code{eval}
## to get back the original input up to the tolerance used.
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

  if (iscomplex (x))
    if (nargout == 2)  # return numerator and denominator
      if (nargin == 2)
        [nr, dr] = rat (real (x), tol);
        [ni, di] = rat (imag (x), tol);
      else
        [nr, dr] = rat (real (x));
        [ni, di] = rat (imag (x));
      endif

      ## For inputs with inf, the output is set to 1/0 or -1/0.
      ## Override that to +inf/1 or -inf/1.
      ii = (dr == 0 & nr > 0); dr(ii) = 1; nr(ii) = +inf;
      ii = (dr == 0 & nr < 0); dr(ii) = 1; nr(ii) = -inf;
      ii = (di == 0 & ni > 0); di(ii) = 1; ni(ii) = +inf;
      ii = (di == 0 & ni < 0); di(ii) = 1; ni(ii) = -inf;

      d = lcm (dr, di);  # now this should always be nonzero
      n = complex (nr .* (d ./ dr), ni .* (d ./ di));
    elseif (nargout <= 1)  # string output
      if (nargin == 2)
        realstr = rat (real (x), tol);
        imagstr = rat (imag (x), tol);
      else
        realstr = rat (real (x));
        imagstr = rat (imag (x));
      endif

      nn = rows (realstr);
      start  = repmat ("complex (", nn, 1);
      mid    = repmat (", ",        nn, 1);
      finish = repmat (")",         nn, 1);
      n = [start, realstr, mid, imagstr, finish];
    endif
    return
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
%! assert (rat (complex (0.5, pi)), "complex (1 + 1/(-2), 3 + 1/(7 + 1/16))");
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
%! assert (str(1, :), "complex (0 + 1/(3 + 1/(4 + 1/(4 + 1/(4 + 1/4)))), 1 + 1/(-20 + 1/(-2 + 1/(-3 + 1/(-6)))))");
%! assert (str(2, :), "complex (-1 + 1/(5 + 1/(4 + 1/(4 + 1/4)))       , 1 + 1/(-2 + 1/(-2 + 1/(-3 + 1/8)))    )");
%! assert (str(3, :), "complex (-1 + 1/(5 + 1/(4 + 1/(4 + 1/4)))       , -1 + 1/(2 + 1/(2 + 1/(3 + 1/(-8))))   )");
%! assert (str(4, :), "complex (0 + 1/(3 + 1/(4 + 1/(4 + 1/(4 + 1/4)))), -1 + 1/(20 + 1/(2 + 1/(3 + 1/6)))     )");

## Test complex exceptional inputs
%!test <*55198>
%! assert (rat (complex (inf, 0)), "complex (Inf, 0)");
%! assert (rat (complex (0, inf)), "complex (0, Inf)");
%! assert (rat (complex (-inf, 0)), "complex (-Inf, 0)");
%! assert (rat (complex (0, -inf)), "complex (0, -Inf)");
%! assert (rat (complex (nan, 0)), "complex (NaN , 0)");
%! assert (rat (complex (0, nan)), "complex (0, NaN )");

%!test <*55198>
%! [n, d] = rat (complex (inf, 0));
%! assert (n, complex (inf, 0));
%! assert (d, 1);
%! [n, d] = rat (complex (0, inf));
%! assert (n, complex (0, inf));
%! assert (d, 1);
%! [n, d] = rat (complex (-inf, 0));
%! assert (n, complex (-inf, 0));
%! assert (d, 1);
%! [n, d] = rat (complex (0, -inf));
%! assert (n, complex (0, -inf));
%! assert (d, 1);
%! [n, d] = rat (complex (nan, 0));
%! assert (n, complex (nan, 0));
%! assert (d, 1);
%! [n, d] = rat (complex (0, nan));
%! assert (n, complex (0, nan));
%! assert (d, 1);

## Test eval with complex inputs
%!test <*55198>
%! x = complex (0.5, pi);
%! assert (eval (rat (x)), x, 1e-6 * norm (x, 1))

## Test eval with inf*i
%!test <*55198>
%! x = complex (0, inf);
%! assert (eval (rat (x)), x, 1e-6 * norm (x, 1))

%!assert <*43374> (eval (rat (0.75)), [0.75])

## Test input validation
%!error <Invalid call> rat ()
%!error <X must be a single or double array> rat (int8 (3))
%!error <TOL must be a numeric scalar> rat (1, "a")
%!error <TOL must be a numeric scalar> rat (1, [1 2])
%!error <TOL must be a numeric scalar .* 0> rat (1, -1)
