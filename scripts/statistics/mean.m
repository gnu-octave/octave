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
## @deftypefn  {} {@var{y} =} mean (@var{x})
## @deftypefnx {} {@var{y} =} mean (@var{x}, 'all')
## @deftypefnx {} {@var{y} =} mean (@var{x}, @var{dim})
## @deftypefnx {} {@var{y} =} mean (@dots{}, '@var{outtype}')
## @deftypefnx {} {@var{y} =} mean (@dots{}, '@var{nanflag}')
## Compute the mean of the elements of @var{x}.
##
## @itemize
## @item
## If @var{x} is a vector, then @code{mean (@var{x})} returns the
## mean of the elements in @var{x} defined as
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
## where @math{N} is the number of elements in the @var{x} vector.
##
## @end ifnottex
##
## @item
## If @var{x} is a matrix, then @code{mean} returns a row vector with the mean
## of each column in @var{x}.
##
## @item
## If @var{x} is a multi-dimensional array, then @code{mean} operates along the
## first non-singleton dimension of @var{x}.
## @end itemize
##
## The optional input @var{dim} forces @code{mean} to operate over the
## specified dimension(s).  @var{dim} can either be a scalar dimension or a
## vector of non-repeating dimensions.  Dimensions must be positive integers,
## and the mean is calculated over the array slice defined by @var{dim}.
##
## Specifying dimension @qcode{"all"} will force @code{mean} to operate on all
## elements of @var{x}, and is equivalent to @code{mean (@var{x}(:))}.
##
## The optional input @var{outtype} specifies the data type that is returned.
## Valid values are:
##
## @table @asis
## @item @qcode{'default'} : Output is of type double, unless the input is
## single in which case the output is of type single.
##
## @item @qcode{'double'} : Output is of type double.
##
## @item @qcode{'native'} : Output is of the same type as the input
## (@code{class (@var{x})}), unless the input is logical in which case the
## output is of type double.
##
## @end table
##
## The optional input @var{nanflag} specifies whether to include/exclude NaN
## values from the calculation.  By default, NaN values are included in the
## calculation (@var{nanflag} has the value @qcode{'includenan'}).  To exclude
## NaN values, set the value of @var{nanflag} to @qcode{'omitnan'}.
##
## @seealso{median, mode}
## @end deftypefn

function y = mean (x, varargin)

  if (nargin < 1 || nargin > 4 || ! all (cellfun (@ischar, varargin(2:end))))
    print_usage ();
  endif

  ## Check all char arguments.
  all_flag = false;
  omitnan = false;
  outtype = "default";

  for i = 1:numel (varargin)
    if (ischar (varargin{i}))
      switch (varargin{i})
        case "all"
          all_flag = true;
        case "includenan"
          omitnan = false;
        case "omitnan"
          omitnan = true;
        case {"default", "double", "native"}
          outtype = varargin{i};
        otherwise
          print_usage ();
      endswitch
    endif
  endfor
  varargin(cellfun (@ischar, varargin)) = [];

  if (((numel (varargin) == 1) && ! (isnumeric (varargin{1}))) ...
      || (numel (varargin) > 1))
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("mean: X must be either a numeric or logical vector or matrix");
  endif

  if (numel (varargin) == 0)

    ## Single numeric input argument, no dimensions given.
    if (all_flag)
      n = numel (x(:));
      if (omitnan)
        idx = isnan (x);
        n -= sum (idx(:));
        x(idx) = 0;
      endif
      y = sum (x(:), 1, "double") ./ n;
    else
      sz = size (x);
      ## Find the first non-singleton dimension.
      (dim = find (sz != 1, 1)) || (dim = 1);
      n = size (x, dim);
      if (omitnan)
        idx = isnan (x);
        n = sum (! idx, dim);
        x(idx) = 0;
      endif
      y = sum (x, dim, "double") ./ n;
    endif

  else

    ## Two numeric input arguments, dimensions given.  Note scalar is vector!
    dim = varargin{1};
    if (! (isvector (dim) && all (dim > 0) && all (rem (dim, 1) == 0)))
      error ("mean: DIM must be a positive integer scalar or vector");
    endif

    if (isscalar (dim))

      n = size (x, dim);
      if (omitnan)
        idx = isnan (x);
        n = sum (! idx, dim);
        x(idx) = 0;
      endif
      y = sum (x, dim, "double") ./ n;

    else

      sz = size (x);
      ndims = numel (sz);
      misdim = [1:ndims];

      dim(dim > ndims) = [];  # weed out dimensions larger than array
      misdim(dim) = [];       # remove dims asked for leaving missing dims

      switch (numel (misdim))
        ## if all dimensions are given, compute x(:)
        case 0
          n = numel (x(:));
          if (omitnan)
            idx = isnan (x);
            n -= sum (idx(:));
            x(idx) = 0;
          endif
          y = sum (x(:), 1, "double") ./ n;

        ## for 1 dimension left, return column vector
        case 1
          x = permute (x, [misdim, dim]);
          y = zeros (size (x, 1), 1, "like", x);
          for i = 1:size (x, 1)
            x_vec = x(i,:)(:);
            if (omitnan)
              x_vec = x_vec(! isnan (x_vec));
            endif
            y(i) = sum (x_vec, 1, "double") ./ numel (x_vec);
          endfor
          y = ipermute (y, [misdim, dim]);

        ## for 2 dimensions left, return matrix
        case 2
          x = permute (x, [misdim, dim]);
          y = zeros (size (x, 1), size (x, 2), "like", x);
          for i = 1:size (x, 1)
            for j = 1:size (x, 2)
              x_vec = x(i,j,:)(:);
              if (omitnan)
                x_vec = x_vec(! isnan (x_vec));
              endif
              y(i,j) = sum (x_vec, 1, "double") ./ numel (x_vec);
            endfor
          endfor
          y = ipermute (y, [misdim, dim]);

        ## for more than 2 dimensions left, throw error
        otherwise
          error ("DIM must index at least N-2 dimensions of X");
      endswitch
    endif

  endif

  ## Convert output if requested
  switch (outtype)
    case "default"
      if isa(x, "single")
        y = single (y);
      endif
    case "double"
      y = double (y);
    case "native"
      if (! islogical (x))
        y = feval (class (x), y);
      endif
  endswitch

endfunction


%!test
%! x = -10:10;
%! y = x';
%! z = [y, y+10];
%! assert (mean (x), 0);
%! assert (mean (y), 0);
%! assert (mean (z), [0, 10]);

%!assert (mean (magic (3), 1), [5, 5, 5])
%!assert (mean (magic (3), 2), [5; 5; 5])
%!assert (mean (logical ([1 0 1 1])), 0.75)
%!assert (mean (single ([1 0 1 1])), single (0.75))
%!assert (mean ([1 2], 3), [1 2])

## Test outtype option
%!test
%! in = [1 2 3];
%! out = 2;
%! assert (mean (in, "default"), mean (in));
%! assert (mean (in, "default"), out);
%!
%! in = single ([1 2 3]);
%! out = 2;
%! assert (mean (in, "default"), mean (in));
%! assert (mean (in, "default"), single (out));
%! assert (mean (in, "double"), out);
%! assert (mean (in, "native"), single (out));
%!
%! in = uint8 ([1 2 3]);
%! out = 2;
%! assert (mean (in, "default"), mean (in));
%! assert (mean (in, "default"), out);
%! assert (mean (in, "double"), out);
%! assert (mean (in, "native"), uint8 (out));
%!
%! in = logical ([1 0 1]);
%! out = 2/3;
%! assert (mean (in, "default"), mean (in));
%! assert (mean (in, "default"), out);
%! assert (mean (in, "native"), out);  # logical ignores native option

## Test single input and optional arguments "all", DIM, "omitnan")
%!test
%! x = [-10:10];
%! y = [x;x+5;x-5];
%! assert (mean (x), 0);
%! assert (mean (y, 2), [0, 5, -5]');
%! assert (mean (y, "all"), 0);
%! y(2,4) = NaN;
%! assert (mean (y', "omitnan"), [0 5.35 -5]);
%! z = y + 20;
%! assert (mean (z, "all"), NaN);
%! m = [20 NaN 15];
%! assert (mean (z'), m);
%! assert (mean (z', "includenan"), m);
%! m = [20 25.35 15];
%! assert (mean (z', "omitnan"), m);
%! assert (mean (z, 2, "omitnan"), m');
%! assert (mean (z, 2, "native", "omitnan"), m');
%! assert (mean (z, 2, "omitnan", "native"), m');

## Test boolean input
%!test
%! assert (mean (true, "all"), 1);
%! assert (mean (false), 0);
%! assert (mean ([true false true]), 2/3, 4e-14);
%! assert (mean ([true false true], 1), [1 0 1]);
%! assert (mean ([true false NaN], 1), [1 0 NaN]);
%! assert (mean ([true false NaN], 2), NaN);
%! assert (mean ([true false NaN], 2, "omitnan"), 0.5);
%! assert (mean ([true false NaN], 2, "omitnan", "native"), 0.5);

## Test dimension indexing with vecdim in N-dimensional arrays
%!test
%! x = repmat ([1:20;6:25], [5 2 6 3]);
%! assert (size (mean (x, [3 2])), [10 1 1 3]);
%! assert (size (mean (x, [1 2])), [1 1 6 3]);
%! assert (size (mean (x, [1 2 4])), [1 1 6]);
%! assert (size (mean (x, [1 4 3])), [1 40]);
%! assert (size (mean (x, [1 2 3 4])), [1 1]);

## Test results with vecdim in N-dimensional arrays and "omitnan"
%!test
%! x = repmat ([1:20;6:25], [5 2 6 3]);
%! m = repmat ([10.5;15.5], [5 1 1 3]);
%! assert (mean (x, [3 2]), m, 4e-14);
%! x(2,5,6,3) = NaN;
%! m(2,3) = NaN;
%! assert (mean (x, [3 2]), m, 4e-14);
%! m(2,3) = 15.52301255230125;
%! assert (mean (x, [3 2], "omitnan"), m, 4e-14);

## Test limits of single precision summation limits on each code path
%!assert <*63848> (mean (ones (80e6, 1, "single")), 1, eps)
%!assert <*63848> (mean (ones (80e6, 1, "single"), "all"), 1, eps)
%!assert <*63848> (mean (ones (80e6, 1, "single"), 1), 1, eps)
%!assert <*63848> (mean (ones (80e6, 1, "single"), [1 2]), 1, eps)
%!assert <*63848> (mean (ones (80e6, 1, "single"), [1 3]), 1, eps)

## Test limits of double precision summation
%!assert <63848> (mean ([flintmax("double"), ones(1, 2^8-1, "double")]), ...
%!                               35184372088833-1/(2^8), eps(35184372088833))

## Test input validation
%!error <Invalid call> mean ()
%!error <Invalid call> mean (1, 2, 3)
%!error <Invalid call> mean (1, 2, 3, 4, 5)
%!error <Invalid call> mean (1, "all", 3)
%!error <Invalid call> mean (1, "b")
%!error <Invalid call> mean (1, 1, "foo")
%!error <X must be either a numeric or logical> mean ({1:5})
%!error <X must be either a numeric or logical> mean ("char")
%!error <DIM must be a positive integer> mean (1, ones (2,2))
%!error <DIM must be a positive integer> mean (1, 1.5)
%!error <DIM must be a positive integer> mean (1, -1)
%!error <DIM must be a positive integer> mean (1, -1.5)
%!error <DIM must be a positive integer> mean (1, 0)
%!error <DIM must be a positive integer> mean (1, NaN)
%!error <DIM must be a positive integer> mean (1, Inf)
%!error <DIM must index at least N-2 dimensions of X>
%!  mean (repmat ([1:20;6:25], [5 2 6 3 5]), [1 2])

