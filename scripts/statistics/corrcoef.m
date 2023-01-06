########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{r} =} corrcoef (@var{x})
## @deftypefnx {} {@var{r} =} corrcoef (@var{x}, @var{y})
## @deftypefnx {} {@var{r} =} corrcoef (@dots{}, @var{param}, @var{value}, @dots{})
## @deftypefnx {} {[@var{r}, @var{p}] =} corrcoef (@dots{})
## @deftypefnx {} {[@var{r}, @var{p}, @var{lci}, @var{hci}] =} corrcoef (@dots{})
## Compute a matrix of correlation coefficients.
##
## @var{x} is an array where each column contains a variable and each row is
## an observation.
##
## If a second input @var{y} (of the same size as @var{x}) is given then
## calculate the correlation coefficients between @var{x} and @var{y}.
##
## @var{param}, @var{value} are optional pairs of parameters and values which
## modify the calculation.  Valid options are:
##
## @table @asis
## @item @qcode{"alpha"}
## Confidence level used for the bounds of the confidence interval, @var{lci}
## and @var{hci}.  Default is 0.05, i.e., 95% confidence interval.
##
## @item @qcode{"rows"}
## Determine processing of NaN values.  Acceptable values are @qcode{"all"},
## @qcode{"complete"}, and @qcode{"pairwise"}.  Default is @qcode{"all"}.
## With @qcode{"complete"}, only the rows without NaN values are considered.
## With @qcode{"pairwise"}, the selection of NaN-free rows is made for each
## pair of variables.
## @end table
##
## Output @var{r} is a matrix of Pearson's product moment correlation
## coefficients for each pair of variables.
##
## Output @var{p} is a matrix of pair-wise p-values testing for the null
## hypothesis of a correlation coefficient of zero.
##
## Outputs @var{lci} and @var{hci} are matrices containing, respectively, the
## lower and higher bounds of the 95% confidence interval of each correlation
## coefficient.
## @seealso{corr, cov}
## @end deftypefn

## FIXME: It would be good to add a definition of the calculation method
## for a Pearson product moment correlation to the documentation.

function [r, p, lci, hci] = corrcoef (x, varargin)

  if (nargin == 0)
    print_usage ();
  endif

  alpha = 0.05;
  rows = "all";

  if (nargin > 1)

    ## Check for matrix argument y
    if (isnumeric (varargin{1}))
      y = varargin{1};
      nx = numel (x);
      ny = numel (y);
      if (nx > 0 && ny > 0 && nx != ny)
        error ("corrcoef: X and Y must be the same size");
      endif
      x = [x(:), y(:)];
      varargin(1) = [];
    endif

    ## Check for Parameter/Value arguments
    for i = 1:2:numel (varargin)

      if (! ischar (varargin{i}))
        error ("corrcoef: parameter %d must be a string", i);
      endif
      parameter = varargin{i};
      if (i+1 > numel (varargin))
        error ('corrcoef: parameter "%s" missing value', parameter);
      endif
      value = varargin{i+1};

      switch (tolower (parameter))
        case "alpha"
          if (isnumeric (value) && isscalar (value)
              && value >= 0 && value <= 1)
            alpha = value;
          else
            error ('corrcoef: "alpha" must be a scalar between 0 and 1');
          endif

        case "rows"
          if (! ischar (value))
            error ('corrcoef: "rows" value must be a string');
          endif
          value = tolower (value);
          switch (value)
            case {"all", "complete", "pairwise"}
              rows = value;
            otherwise
              error ('corrcoef: "rows" must be "all", "complete", or "pairwise"');
          endswitch

        otherwise
          error ('corrcoef: Unknown option "%s"', parameter);

      endswitch
    endfor
  endif

  if (strcmp (rows, "complete"))
    x(any (isnan (x), 2), :) = [];
  endif

  if (isempty (x) || isscalar (x))
    r = p = lci = hci = NaN;
    return;
  endif

  ## Flags for calculation
  pairwise = strcmp (rows, "pairwise");
  calc_pval = nargout > 1;

  if (isrow (x))
    x = x(:);
  endif
  [m, n] = size (x);
  r = eye (n);
  if (calc_pval)
    p = eye (n);
  endif
  if (strcmp (rows, "pairwise"))
    mpw = m * ones (n);
  endif
  for i = 1:n
    if (! pairwise && any (isnan (x(:,i))))
      r(i,i) = NaN;
      if (nargout > 1)
        p(i,i) = NaN;
      endif
    endif
    for j = i+1:n
      xi = x(:,i);
      xj = x(:,j);
      if (pairwise)
        idx = any (isnan ([xi xj]), 2);
        xi(idx) = xj(idx) = [];
        mpw(i,j) = mpw(j,i) = m - nnz (idx);
      endif
      r(i,j) = r(j,i) = corr (xi, xj);
      if (calc_pval)
        df = m - 2;
        stat = sqrt (df) * r(i,j) / sqrt (1 - r(i,j)^2);
        cdf = tcdf (stat, df);
        p(i,j) = p(j,i) = 2 * min (cdf, 1 - cdf);
      endif
    endfor
  endfor

  if (nargout > 2)
    if (pairwise)
      m = mpw;
    endif
    CI = sqrt (2) * erfinv (1-alpha) ./ sqrt (m-3);
    lci = tanh (atanh (r) - CI);
    hci = tanh (atanh (r) + CI);
  endif

endfunction


## Compute cumulative distribution function for T distribution.
function cdf = tcdf (x, n)

  if (iscomplex (x))
    error ("tcdf: X must not be complex");
  endif

  if (isa (x, "single"))
    cdf = zeros (size (x), "single");
  else
    cdf = zeros (size (x));
  endif

  k = ! isinf (x) & (n > 0);

  xx = x .^ 2;
  x_big_abs = (xx > n);

  ## deal with the case "abs(x) big"
  kk = k & x_big_abs;
  cdf(kk) = betainc (n ./ (n + xx(kk)), n/2, 1/2) / 2;

  ## deal with the case "abs(x) small"
  kk = k & ! x_big_abs;
  cdf(kk) = 0.5 * (1 - betainc (xx(kk) ./ (n + xx(kk)), 1/2, n/2));

  k &= (x > 0);
  if (any (k(:)))
    cdf(k) = 1 - cdf(k);
  endif

  k = isnan (x) | !(n > 0);
  cdf(k) = NaN;

  k = (x == Inf) & (n > 0);
  cdf(k) = 1;

endfunction


%!test
%! x = rand (5);
%! r = corrcoef (x);
%! assert (size (r) == [5, 5]);

%!test
%! x = [1, 2, 3];
%! r = corrcoef (x);
%! assert (size (r) == [1, 1]);

%!assert (isnan (corrcoef ([])))
%!assert (isnan (corrcoef (NaN)))
%!assert (isnan (corrcoef (1)))

%!test
%! x = [NaN, NaN];
%! r = corrcoef (x);
%! assert (size(r) == [1, 1] && isnan (r));

%!test
%! x = rand (5);
%! [r, p] = corrcoef (x);
%! assert (size (r) == [5, 5] && size (p) == [5 5]);
%! assert (diag (r), ones (5,1), eps);

%!test
%! x = rand (5,1);
%! y = rand (5,1);
%! R1 = corrcoef (x, y);
%! R2 = corrcoef ([x, y]);
%! assert (R1, R2);
%! R3 = corrcoef (x.', y.');
%! assert (R1, R3);

%!test
%! x = [1;2;3];
%! y = [1;2;3];
%! r = corrcoef (x, y);
%! assert (r, ones (2,2));

%!test
%! x = [1;2;3];
%! y = [3;2;1];
%! r = corrcoef (x, y);
%! assert (r, [1, -1; -1, 1]);

%!test
%! x = [1;2;3];
%! y = [1;1;1];
%! r = corrcoef (x, y);
%! assert (r, [1, NaN; NaN, 1]);

%!error <Invalid call> corrcoef ()
%!error <parameter 1 must be a string> corrcoef (1, 2, 3)
%!error <parameter "alpha" missing value> corrcoef (1, 2, "alpha")
%!error <"alpha" must be a scalar> corrcoef (1,2, "alpha", "1")
%!error <"alpha" must be a scalar> corrcoef (1,2, "alpha", ones (2,2))
%!error <"alpha" must be a scalar between 0 and 1> corrcoef (1,2, "alpha", -1)
%!error <"alpha" must be a scalar between 0 and 1> corrcoef (1,2, "alpha", 2)
%!error <"rows" must be "all"...> corrcoef (1,2, "rows", "foobar")
%!error <Unknown option "foobar"> corrcoef (1,2, "foobar", 1)
