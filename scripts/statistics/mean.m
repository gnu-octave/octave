## Copyright (C) 1995-2017 Kurt Hornik
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

## -*- texinfo -*-
## @deftypefn  {} {} mean (@var{x})
## @deftypefnx {} {} mean (@var{x}, @var{dim})
## @deftypefnx {} {} mean (@var{x}, @var{opt})
## @deftypefnx {} {} mean (@var{x}, @var{dim}, @var{opt})
## @deftypefnx {} {} mean (@dots{}, @var{outtype})
## Compute the mean of the elements of the vector @var{x}.
##
## The mean is defined as
##
## @tex
## $$ {\rm mean}(x) = \bar{x} = {1\over N} \sum_{i=1}^N x_i $$
## where $N$ is the number of elements of @var{x}.
##
## @end tex
## @ifnottex
##
## @example
## mean (@var{x}) = SUM_i @var{x}(i) / N
## @end example
##
## @noindent
## where @math{N} is the length of the @var{x} vector.
##
## @end ifnottex
## If @var{x} is a matrix, compute the mean for each column and return them
## in a row vector.
##
## If the optional argument @var{dim} is given, operate along this dimension.
##
## The optional argument @var{opt} selects the type of mean to compute.
## The following options are recognized:
##
## @table @asis
## @item @qcode{"a"}
## Compute the (ordinary) arithmetic mean.  [default]
##
## @item @qcode{"g"}
## Compute the geometric mean.
##
## @item @qcode{"h"}
## Compute the harmonic mean.
## @end table
##
## The optional argument @var{outtype} selects the data type of the
## output value.  The following options are recognized:
##
## @table @asis
## @item @qcode{"default"}
## Output will be of class double unless @var{x} is of class single,
## in which case the output will also be single.
##
## @item @qcode{"double"}
## Output will be of class double.
##
## @item @qcode{"native"}
## Output will be the same class as @var{x} unless @var{x} is of class
## logical in which case it returns of class double.
##
## @end table
##
## Both @var{dim} and @var{opt} are optional.  If both are supplied, either
## may appear first.
## @seealso{median, mode}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Compute arithmetic, geometric, and harmonic mean

function y = mean (x, varargin)

  if (nargin < 1 || nargin > 4)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("mean: X must be a numeric vector or matrix");
  endif
  nd = ndims (x);
  sz = size (x);

  ## We support too many options...

  ## If OUTTYPE is set, it must be the last option.  If DIM and
  ## MEAN_TYPE exist, they must be the first two options

  out_type = "default";
  if (numel (varargin))
    maybe_out_type = tolower (varargin{end});
    if (any (strcmpi (maybe_out_type, {"default", "double", "native"})))
      out_type = maybe_out_type;
      varargin(end) = [];
    endif
  endif

  scalars = cellfun (@isscalar, varargin);
  chars = cellfun (@ischar, varargin);
  numerics = cellfun (@isnumeric, varargin);

  dim_mask = numerics & scalars;
  mean_type_mask = chars & scalars;
  if (! all (dim_mask | mean_type_mask))
    print_usage ();
  endif

  switch (nnz (dim_mask))
    case 0 # Find the first non-singleton dimension
      (dim = find (sz > 1, 1)) || (dim = 1);
    case 1
      dim = varargin{dim_mask};
      if (dim != fix (dim) || dim < 1)
        error ("mean: DIM must be an integer and a valid dimension");
      endif
    otherwise
      print_usage ();
  endswitch

  switch (nnz (mean_type_mask))
    case 0
      mean_type = "a";
    case 1
      mean_type = varargin{mean_type_mask};
    otherwise
      print_usage ();
  endswitch

  ## The actual mean computation
  n = size (x, dim);
  switch (mean_type)
    case "a"
      y = sum (x, dim) / n;
    case "g"
      if (all (x(:) >= 0))
        y = exp (sum (log (x), dim) ./ n);
      else
        error ("mean: X must not contain any negative values");
      endif
    case "h"
      y = n ./ sum (1 ./ x, dim);
    otherwise
      error ("mean: mean type '%s' not recognized", mean_type);
  endswitch

  ## Convert output as requested
  switch (out_type)
    case "default"
      ## do nothing, the operators already do the right thing
    case "double"
      y = double (y);
    case "native"
      if (islogical (x))
        ## ignore it, return double anyway
      else
        y = cast (y, class (x));
      endif
    otherwise
      ## this should have been filtered out during input check, but...
      error ("mean: OUTTYPE '%s' not recognized", out_type);
  endswitch
endfunction


%!test
%! x = -10:10;
%! y = x';
%! z = [y, y+10];
%! assert (mean (x), 0);
%! assert (mean (y), 0);
%! assert (mean (z), [0, 10]);

## Test small numbers
%!assert (mean (repmat (0.1,1,1000), "g"), 0.1, 20*eps)

%!assert (mean (magic (3), 1), [5, 5, 5])
%!assert (mean (magic (3), 2), [5; 5; 5])
%!assert (mean ([2 8], "g"), 4)
%!assert (mean ([4 4 2], "h"), 3)
%!assert (mean (logical ([1 0 1 1])), 0.75)
%!assert (mean (single ([1 0 1 1])), single (0.75))
%!assert (mean ([1 2], 3), [1 2])

## Test input validation
%!error <Invalid call to mean.  Correct usage is> mean ()
%!error <Invalid call to mean.  Correct usage is> mean (1, 2, 3, 4)
%!error <X must be a numeric> mean ({1:5})
%!error <Invalid call to mean.  Correct usage is> mean (1, 2, 3)
%!error <Invalid call to mean.  Correct usage is> mean (1, ones (2,2))
%!error <DIM must be an integer> mean (1, 1.5)
%!error <DIM must be .* a valid dimension> mean (1, 0)
%!error <X must not contain any negative values> mean ([1 -1], "g")
%!error <mean type 'b' not recognized> mean (1, "b")
%!error <Invalid call to mean.  Correct usage is> mean (1, 1, "foo")

## Test outtype option
%!test
%! in = [1 2 3];
%! out = 2;
%! assert (mean (in, "default"), mean (in))
%! assert (mean (in, "default"), out)
%!
%! in = single ([1 2 3]);
%! out = 2;
%! assert (mean (in, "default"), mean (in))
%! assert (mean (in, "default"), single (out))
%! assert (mean (in, "double"), out)
%! assert (mean (in, "native"), single (out))
%!
%! in = uint8 ([1 2 3]);
%! out = 2;
%! assert (mean (in, "default"), mean (in))
%! assert (mean (in, "default"), out)
%! assert (mean (in, "double"), out)
%! assert (mean (in, "native"), uint8 (out))
%!
%! in = logical ([1 0 1]);
%! out = 2/3;
%! assert (mean (in, "default"), mean (in))
%! assert (mean (in, "default"), out)
%! assert (mean (in, "native"), out) # logical ignores native option
