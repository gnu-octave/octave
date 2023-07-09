########################################################################
##
## Copyright (C) 1995-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{c} =} cov (@var{x})
## @deftypefnx {} {@var{c} =} cov (@var{x}, @var{y})
## @deftypefnx {} {@var{c} =} cov (@dots{}, @var{opt})
## @deftypefnx {} {@var{c} =} cov (@dots{}, @var{nanflag})
## Compute the covariance matrix.
##
## The covariance between two variable vectors @var{A} and  @var{B} is
## calculated as:
## @tex
## $$
## \sigma_{ij} = {1 \over N-1} \sum_{i=1}^N (a_i - \bar{a})(b_i - \bar{b})
## $$
## where $\bar{a}$ and $\bar{b}$ are the mean values of $a$ and $b$ and $N$ is
## the length of the vectors $a$ and $b$.
## @end tex
## @ifnottex
##
## @example
## cov (@var{a},@var{b}) = 1/(N-1) * SUM_i (@var{a}(i) - mean (@var{a})) * (@var{b}(i) - mean (@var{b}))
## @end example
##
## @noindent
## where @math{N} is the length of the vectors @var{a} and @var{b}.
## @end ifnottex
##
## If called with one argument, compute @code{cov (@var{x}, @var{x})}.  If
## @var{x} is a vector, this is the scalar variance of @var{x}.  If @var{x} is
## a matrix, each row of @var{x} is treated as an observation, and each column
## as a variable, and the @w{(@var{i}, @var{j})-th} entry of
## @code{cov (@var{x})} is the covariance between the @var{i}-th and
## @var{j}-th columns in @var{x}.  If @var{x} has dimensions n x m, the output
## @var{c} will be a m x m square covariance matrix.
##
## If called with two arguments, compute @code{cov (@var{x}, @var{y})}, the
## covariance between two random variables @var{x} and @var{y}.  @var{x} and
## @var{y} must have the same number of elements, and will be treated as
## vectors with the covariance computed as
## @code{cov (@var{x}(:), @var{y}(:))}.  The output will be a 2 x 2
## covariance matrix.
##
## The optional argument @var{opt} determines the type of normalization to
## use.  Valid values are
##
## @table @asis
## @item 0 [default]:
##   Normalize with @math{N-1}.  This provides the best unbiased estimator of
## thecovariance
##
## @item 1:
##   Normalize with @math{N}.  This provides the second moment around the
## mean.  @var{opt} is set to 1 for N = 1.
## @end table
##
## The optional argument @var{nanflag} must appear last in the argument list
## and controls how NaN values are handled by @code{cov}.  The three valid
## values are:
##
## @table @asis
## @item includenan [default]:
##   Leave NaN values in @var{x} and @var{y}.  Output will follow the normal
## rules for handling NaN values in arithemtic operations.
##
## @item omitrows:
##   Rows containing NaN values are trimmed from both @var{x} and @var{y}
## prior to calculating the covariance.  (A NaN in one variable will remove
## that row from both @var{x} and @var{y}.)
##
## @item partialrows:
##   Rows containing NaN values are ignored from both @var{x} and @var{y}
##   independently for each @var{i}-th and @var{j}-th covariance
##   calculation.  This may result in a different number of observations,
##   @math{N}, being used to calculated each element of the covariance matrix.
## @end table
##
## Compatibility Note:  Previous versions of @code{cov} treated rows
## @var{x} and @var{y} as multivariate random variables.  This version
## attempts to maintain full compatibility with @sc{matlab} by treating
## @var{x} and @var{y} as two univariate distributions regardless of shape,
## resulting in a 2x2 output matrix.  Code relying on Octave's previous
## definition will need to be modified when running this newer version of
## @code{cov}.  The previous behavior can be obtained by using the
## NaN package's @code{covm} function as @code{covm (@var{x}, @var{y}, "D")}.
## @seealso{corr}
## @end deftypefn

function c = cov (x, varargin)

  if (nargin < 1 || nargin > 4)
    print_usage ();
  endif

  opt = 0;
  is_y = false;
  nanflag = "includenan";

  if (! (isnumeric (x) || islogical (x)))
      error ("cov: X must be a numeric vector or matrix");
  endif

  if (isrow (x))
    x = x.';
  endif

  nvarg = numel (varargin);

  if (nvarg > 0)
    switch (nvarg)
      case 3
        ## Only char input should be nanflag, must be last.
        if (ischar (varargin{1}) || ischar (varargin {2}))
          if (ischar (varargin{3}))
            error ("cov: only one NANFLAG parameter may be specified");
          else
            error ("cov: NANFLAG parameter must be the last input");
          endif
        endif

        y = varargin{1};
        opt = double (varargin{2}); # opt should not affect output class.
        nanflag = lower (varargin {3});
        is_y = true;

      case 2
        if (ischar (varargin{1}))
          error ("cov: NANFLAG parameter must be the last input");
        endif

        if (ischar (varargin{end}))
          nanflag = lower (varargin{end});

          if (isscalar (varargin{1}) && ...
            (varargin{1} == 0 || varargin{1} == 1))
            opt = double (varargin {1});

          else
            y = varargin{1};
            is_y = true;
          endif

        else
          y = varargin{1};
          opt = double (varargin{2});
          is_y = true;
        endif

      case 1
        if (ischar (varargin{end}))
          nanflag = lower (varargin{end});

        elseif (isscalar (varargin{1}) && ...
               (varargin{1} == 0 || varargin{1} == 1))
          opt = double (varargin {1});

        else
          y = varargin{1};
          is_y = true;
        endif
    endswitch

    if (is_y)
      if (! (isnumeric (y) || islogical (y)))
        error ("cov: Y must be a numeric vector or matrix");

      elseif (numel (x) != numel (y))
        error ("cov: X and Y must have the same number of observations");

      else
        ## Flatten to single array.  Process the same as two-column x.
        x = [x(:), y(:)];
      endif
    endif

    if (! any (strcmp (nanflag, {"includenan", "omitrows", "partialrows"})))
        error ("cov: unknown NANFLAG parameter '%s'", nanflag);
    endif

    if ((opt != 0 && opt != 1) || ! isscalar (opt))
      error ("cov: normalization paramter OPT must be 0 or 1");
    endif
  endif

  if (ndims (x) > 2)
    ## Note: Matlab requires 2D inputs even if providing a y input results in
    ##       reshaping for operation as cov (x(:), y(:)) (tested in 2022b).
    ##       Octave permits arbitrarily shaped inputs for the cov(x,y) case as
    ##       long as numel (x) == numel (y).  Only when no y is provided is X
    ##       restricted to 2D for consistent 2D columnwise behavior of cov.
    error ("cov: X must be a 2-D matrix or vector");
  endif

  ## Special case: empty inputs.  Output shape changes depends on number of
  ## columns.  Inputs already verified as limited to 2D.
  if (isempty (x))
    sx = size (x);

    if (sx == [0, 0])
      c = NaN;
    elseif (sx(1) > 0)
      c = [];
    else
      c = NaN (sx(2));
    endif

    if (isa (x, "single"))
      c = single (c);
    endif
    return;
  endif

  if (! strcmp (nanflag, "includenan") && all (nnx = ! isnan (x)))
    ## Avoid unnecessary slower nanflag processing.
    nanflag = "includenan";
  endif

  switch (nanflag)
    case {"includenan", "omitrows"}

      if (strcmp (nanflag, "omitrows"))
        ## Trim any rows in x containing a NaN.
        x = x(all (nnx, 2), :);
      endif

      n = rows (x);

      if (n < 2)
        ## Scalars and empties force opt = 1.
        opt = 1;
      endif

      x -= sum (x, 1) / n;
      c = x' * x / (n - 1 + opt);

    case "partialrows"
      ## Find all NaN locations for adjusted processing later.  NaN's will
      ## only be trimmed for the columns containing them when calculating
      ## covariance involving that row.  Each output element of c might have a
      ## unique n, opt, and mean for each of the two vectors used to calculate
      ## that element based on the paired column total number of non-NaN rows.
      ## Separate out simple vector case.  Project into dim3 for general case.

      if (iscolumn (x))
        x = x(nnx);
        n = numel (x);
        if (n < 2)
          opt = 1;
        endif
        x -= sum (x, 1) / n;

        ## Matrix multiplication preserves output size compatibliity if x is
        ## trimmed to empty.
        c = (x' * x) / (n - 1 + opt);
      else

        ## Number of elements in each column pairing.
        n = nnx' * nnx;

        ## opt for each column pairing
        opt = opt * ones (columns (x));
        opt (n < 2) = 1;

        ## Mean for each column pairing.
        ## Rotate x vectors into dim3, project into dim1 with non-nan array
        x = permute(x, [3, 2, 1]) .* permute (nnx, [2, 3, 1]);
        mu = x;
        mu (isnan (x)) = 0; # Exclude input NaNs from summation.
        mu = sum (mu, 3) ./ n; # Vectors trimmed to n=0 may create more NaNs.
        x -= mu; # Center x's.
        x(isnan (x)) = 0; # Exclude input and mean NaNs from output.

        ## Sum dim3 elements of x products to emulate x'*x.
        c = sum (permute (x, [2,1,3]) .* x, 3) ./ (n - 1 + opt);

      endif
  endswitch
endfunction

%!test
%! x = rand (10);
%! cx1 = cov (x);
%! cx2 = cov (x, x);
%! assert (size (cx1) == [10, 10]);
%! assert (size (cx2) == [2, 2]);

%!test
%! x = [1:3]';
%! y = [3:-1:1]';
%! assert (cov (x, x), [1 1; 1 1]);
%! assert (cov (x, x), cov ([x, x]));
%! assert (cov (x, y), [1 -1; -1 1]);
%! assert (cov (x, y), cov ([x, y]));

%!test
%! x = [1:3]';
%! y = [3:-1:1]';
%! assert (cov (single (x)), single (1));
%! assert (cov (single (x), x), single ([1 1; 1 1]));
%! assert (cov (x, single (x)), single ([1 1; 1 1]));
%! assert (cov (single (x), single (x)), single ([1 1; 1 1]));

%!test
%! x = [1:8];
%! c = cov (x);
%! assert (isscalar (c));
%! assert (c, 6);

%!test <*64395>
%! x = [1, 2, 3];
%! assert (cov (x), 1, eps);
%! assert (cov (x'), 1, eps);
%! assert (cov (x, x), ones (2), eps);
%! assert (cov (x', x), ones (2), eps);
%! assert (cov (x, x'), ones (2), eps);
%! assert (cov (x', x'), ones (2), eps);

%!test
%! x = [1 0; 1 0];
%! y = [1 2; 1 1];
%! z = [1/3 -1/6; -1/6 0.25];
%! assert (cov (x, y), z, eps);
%! assert (cov (x, y(:)), z, eps);
%! assert (cov (x, y(:)'), z, eps);
%! assert (cov (x', y(:)), z .* [1, -1; -1, 1], eps);
%! assert (cov (x(:), y), z, eps);
%! assert (cov (x(:)', y), z, eps);

## Test scalar inputs & class preservation
%!assert (cov (5), 0)
%!assert (cov (1, 0), 0)
%!assert (cov (1, 1), 0)
%!assert (cov (1, 0.1), [0, 0; 0, 0])
%!assert (cov (1, 1.1), [0, 0; 0, 0])
%!assert (cov (1, 3), [0, 0; 0, 0])
%!assert (cov (single (5)), single (0))
%!assert (cov (single (5), 0), single (0))
%!assert (cov (single (5), 1), single (0))
%!assert (cov (single (5), 1.1), single ([0, 0; 0, 0]))
%!assert (cov (5, single (0)), double (0))
%!assert (cov (5, single (1)), double (0))
%!assert (cov (5, single (1.1)), single ([0, 0; 0, 0]))
%!assert (cov (5, single (1.1), 0), single ([0, 0; 0, 0]))
%!assert (cov (5, single (1.1), 1), single ([0, 0; 0, 0]))
%!assert (cov (5, 1.1, single (0)), double([0, 0; 0, 0]))

## Test opt
%!test
%! x = [1:5];
%! c = cov (x, 0);
%! assert (c, 2.5);
%! c = cov (x, 1);
%! assert (c, 2);
%! c = cov (double (x), single (1));
%! assert (class (c), "double");
%!assert (cov (2, 4), zeros(2))
%!assert (cov (2, 4, 0), zeros(2))
%!assert (cov (2, 4, 1), zeros(2))
%!assert (cov (NaN, 4), [NaN, NaN; NaN, 0])
%!assert (cov (NaN, 4, 0), [NaN, NaN; NaN, 0])
%!assert (cov (NaN, 4, 1), [NaN, NaN; NaN, 0])

## Test logical inputs
%!assert (cov (logical(0), logical(0)), double(0))
%!assert (cov (0, logical(0)), double(0))
%!assert (cov (logical(0), 0), double(0))
%!assert (cov (logical([0 1; 1 0]), logical([0 1; 1 0])), double ([1 1;1 1]./3))

## Test empty and NaN handling (bug #50583)
%!assert <*50583> (cov ([]), NaN)
%!assert <*50583> (cov (single ([])), single (NaN))
%!assert <*50583> (cov ([], []), NaN (2, 2))
%!assert <*50583> (cov (single ([]), single([])),  single (NaN (2, 2)))
%!assert <*50583> (cov ([], single ([])), single (NaN (2, 2)))
%!assert <*50583> (cov (single ([]), []), single (NaN (2, 2)))
%!assert <*50583> (cov (ones(1, 0)), NaN)
%!assert <*50583> (cov (ones(2, 0)), [])
%!assert <*50583> (cov (ones(0, 1)), NaN)
%!assert <*50583> (cov (ones(0, 2)), NaN (2, 2))
%!assert <*50583> (cov (ones(0, 6)), NaN (6, 6))
%!assert <*50583> (cov (ones(2, 0), []), NaN (2, 2))
%!assert <*50583> (cov (ones(0,6), []), NaN (2, 2))
%!assert <*50583> (cov (NaN), NaN)
%!assert <*50583> (cov (NaN, NaN), NaN (2, 2))
%!assert <*50583> (cov (5, NaN), [0, NaN; NaN, NaN])
%!assert <*50583> (cov (NaN, 5), [NaN, NaN; NaN, 0])
%!assert <*50583> (cov (single (NaN)), single (NaN))
%!assert <*50583> (cov (NaN (2, 2)), NaN (2, 2))
%!assert <*50583> (cov (single (NaN (2, 2))), single (NaN (2, 2)))
%!assert <*50583> (cov (NaN(2, 9)), NaN(9, 9))
%!assert <*50583> (cov (NaN(9, 2)), NaN(2, 2))
%!assert <*50583> (cov ([NaN, 1, 2, NaN]), NaN)
%!assert <*50583> (cov ([1, NaN, 2, NaN]), NaN)

## Test NaN and nanflag option (bug #50571)
%!test <*50571>
%! x = magic(3);
%! y = magic(3);
%! x(3) = NaN;
%! assert (cov (y, "omitrows"), cov(y));
%! assert (cov (y, "partialrows"), cov(y));
%! assert (cov (x), [NaN, NaN, NaN; NaN, 16, -8; NaN, -8, 7]);
%! assert (cov (x, "omitrows"), ...
%!   [12.5, -10, -2.5; -10, 8, 2; -2.5, 2, 0.5], eps);
%! assert (cov (x, "partialrows"),
%!   [12.5, -10, -2.5; -10, 16, -8; -2.5, -8, 7], eps);
%! assert (cov (x, x), NaN(2,2));
%! assert (cov (x, y), [NaN, NaN; NaN, 7.5], eps);
%! assert (cov (y, x), [7.5, NaN; NaN, NaN], eps);
%! assert (cov (x, x, 'omitrows'), (471/56) * ones(2), eps);
%! assert (cov (x, x, 'partialrows'), (471/56) * ones(2), eps);
%! assert (cov (x, y, 'omitrows'), (471/56) * ones(2), eps);
%! assert (cov (x, y, 'partialrows'), (471/56)*[1,1;1,0] + [0,0;0,7.5], eps);
%! assert (cov (y, x, 'omitrows'), (471/56) * ones(2), eps);
%! assert (cov (y, x, 'partialrows'), (471/56)*[0,1;1,1] + [7.5,0;0,0], eps);
%! assert (cov (x, y, 0, 'omitrows'), (471/56) * ones(2), eps);
%! assert (cov (x, y, 0, 'partialrows'), (471/56)*[1,1;1,0] + [0,0;0,7.5], eps);
%!assert (cov ([NaN NaN NaN]), NaN)
%!assert <*50571> (cov ([NaN NaN NaN], "omitrows"), NaN)
%!assert <*50571> (cov ([NaN NaN NaN], "partialrows"), NaN)
%!assert (cov (NaN(3)), NaN(3))
%!assert <*50571> (cov (NaN(3), "omitrows"), NaN(3))
%!assert <*50571> (cov (NaN(3), "partialrows"), NaN(3))
%!assert (cov ([NaN NaN NaN],[1 2 3]), [NaN, NaN; NaN 1])
%!assert <*50571> (cov ([NaN NaN NaN],[1 2 3], "omitrows"), [NaN, NaN; NaN NaN])
%!assert <*50571> (cov ([NaN NaN NaN],[1 2 3], "partialrows"), [NaN, NaN; NaN 1])
%!test <*50571>
%! x = magic(3);
%! x(4:6) = NaN;
%! assert (cov (x), [7 NaN 1; NaN NaN NaN; 1 NaN 7]);
%! assert (cov (x, "omitrows"), NaN(3));
%! assert (cov (x, "partialrows"), [7 NaN 1; NaN NaN NaN; 1 NaN 7]);
%!assert <*50571> (cov (5, NaN, "omitrows"), NaN(2, 2))
%!assert <*50571> (cov (NaN, 5, "omitrows"), NaN(2, 2))
%!assert <*50571> (cov (5, NaN, "partialrows"), [0, NaN; NaN, NaN])
%!assert <*50571> (cov (NaN, 5, "partialrows"), [NaN, NaN; NaN, 0])
%!test <*50571>
%! x = [1:10]';
%! y = x;
%! x(2:2:10)=NaN;
%! y(1:2:9)=NaN;
%! assert (cov(x,y), NaN(2));
%! assert (cov(x,y,'omitrows'), NaN(2));
%! assert (cov(x,y,'partialrows'), [10 NaN; NaN, 10]);
%! x(10) = 5;
%! assert (cov(x,y), NaN(2));
%! assert (cov(x,y,'omitrows'), zeros(2));
%! assert (cov(x,y,'partialrows'), [8 0; 0, 10]);
#!assert (cov ([NaN, NaN, 3]), NaN)
#!assert <*50571>  (cov ([NaN, NaN, 3], 'omitrows'), 0)
#!assert <*50571> (cov ([NaN, NaN, 3], 'partialrows'), 0)
%!assert (cov ([NaN, NaN, NaN; NaN, 2, 3; NaN, NaN, 3]), NaN(3))
%!assert <*50571> (cov ([NaN, NaN, NaN; NaN, 2, 3; NaN, NaN, 3], 'omitrows'), NaN(3))
%!assert <*50571> (cov ([NaN, NaN, NaN; NaN, 2, 3; NaN, NaN, 3], 'partialrows'), [NaN, NaN, NaN; NaN(2,1), zeros(2)])
%!assert <*50571> (cov ([NaN, NaN, NaN; NaN, 2, 3; NaN, NaN, 3], 0, 'partialrows'), [NaN, NaN, NaN; NaN(2,1), zeros(2)])
%!assert <*50571> (cov ([NaN, NaN, NaN; NaN, 2, 3; NaN, NaN, 3], 1, 'partialrows'), [NaN, NaN, NaN; NaN(2,1), zeros(2)])
%!test <*50571>
%! x = magic(4);
%! x([4 5 7 10 14 16]) = NaN;
%! x1 = x(:,2);
%! x2 = x(:,3);
%! assert (cov(x1,x2),nan(2));
%! assert (cov(x1,x2,'omitrows'),zeros(2));
%! assert (cov(x1,x2, 'partialrows'), [4.5,0;0,39],eps);
%! assert (cov(x1,x2,0,'partialrows'), [4.5,0;0,39],eps);
%! assert (cov(x1,x2,1,'partialrows'), [2.25,0;0,26],eps);
%! assert (cov(x), nan(4));
%! assert (cov(x,'omitrows'), nan(4));
%! assert (cov(x,'partialrows'), [31 0 -10.5 3.5; 0 4.5 0 NaN; -10.5 0 39 -1.5; 3.5 NaN -1.5 0.5], eps);
%! assert (cov(x, 0, 'partialrows'), [31 0 -10.5 3.5; 0 4.5 0 NaN; -10.5 0 39 -1.5; 3.5 NaN -1.5 0.5], eps);
%! assert (cov(x, 1,'partialrows'), [62/3 0 -5.25 1.75; 0 2.25 0 NaN; -5.25 0 26 -0.75; 1.75 NaN -0.75 0.25], eps);

## Test input validation
%!error <Invalid call> cov ()
%!error <Invalid call> cov (1, 2, 3, 4, 5)
%!error <X must be a> cov ("foo")
%!error <X must be a> cov ({123})
%!error <X must be a> cov (struct())
%!error <X must be a> cov (ones (2, 2, 2))
%!error <X must be a> cov (ones (1, 0, 2))
%!error <only one NANFLAG> cov (1, "foo", 0, "includenan")
%!error <only one NANFLAG> cov (1, 1, "foo", "includenan")
%!error <normalization paramter OPT must be> cov (1, 2, [])
%!error <normalization paramter OPT must be> cov (1, 2, 1.1)
%!error <normalization paramter OPT must be> cov (1, 2, -1)
%!error <normalization paramter OPT must be> cov (1, 2, [0 1])
%!error <X and Y must have the same number> cov (5,[1 2])
%!error <X and Y must have the same number> cov (ones (2, 2), ones (2, 2, 2))
%!error <X and Y must have the same number> cov (ones (2, 2), ones (3, 2))
%!error <X and Y must have the same number> cov (1, [])
%!error <X and Y must have the same number> cov ([1, 2], ones(1, 0, 2))
%!error <unknown NANFLAG parameter 'foo'> cov (1, "foo")
%!error <unknown NANFLAG parameter 'foo'> cov (1, "foo")
%!error <unknown NANFLAG parameter 'foo'> cov (1, 2, "foo")
%!error <unknown NANFLAG parameter 'foo'> cov (1, 2, 0, "foo")
%!error <unknown NANFLAG parameter ''> cov (1, 2, 1, [])
%!error <NANFLAG parameter must be the last> cov (1, "includenan", 1)
%!error <NANFLAG parameter must be the last> cov (1, 1, "includenan", 1)
