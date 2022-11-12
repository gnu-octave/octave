########################################################################
##
## Copyright (C) 1995-2022 The Octave Project Developers
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
## @deftypefnx {} {@var{y} =} mean (@var{x}, "all")
## @deftypefnx {} {@var{y} =} mean (@var{x}, @var{dim})
## @deftypefnx {} {@var{y} =} mean (@var{x}, @var{vecdim})
## @deftypefnx {} {@var{y} =} mean (@dots{}, @var{outtype})
## @deftypefnx {} {@var{y} =} mean (@dots{}, @var{nanflag})
## Compute the mean of the elements of @var{x}.
##
## @itemize
## @item
## If @var{x} is a vector, then @code{mean(@var{x})} returns the
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
## where @math{N} is the length of the @var{x} vector.
##
## @end ifnottex
##
## @item
## If @var{x} is a matrix, then @code{mean(@var{x})} returns a row vector
## with the mean of each columns in @var{x}.
##
## @item
## If @var{x} is a multidimensional array, then @code{mean(@var{x})}
## operates along the first nonsingleton dimension of @var{x}.
## @end itemize
##
## @code{mean(@var{x}, "all")} returns the mean of all the elements in @var{x}.
##
## @code{mean(@var{x}, @var{dim})} returns the mean along the
## operating dimension @var{dim} of @var{x}.
##
## @code{mean(@var{x}, @var{vecdim})} returns the mean over the
## dimensions specified in the vector @var{vecdim}.  For example, if @var{x}
## is a 2-by-3-by-4 array, then @code{mean(@var{x}, [1 2])} returns a 1-by-4
## array.  Each element of the output array is the mean of the elements on
## the corresponding page of @var{x}.  NOTE! @var{vecdim} MUST index at least
## N-2 dimensions of @var{x}, where @code{N = length (size (@var{x}))} and
## N < 8.  If @var{vecdim} indexes all dimensions of @var{x}, then it is
## equivalent to @code{mean(@var{x}, "all")}.
##
## @code{mean(@dots{}, @var{outtype})} returns the mean with a specified data
## type, using any of the input arguments in the previous syntaxes.
## @var{outtype} can be "default", "double", or "native".
##
## @code{mean(@dots{}, @var{nanflag})} specifies whether to exclude NaN values
## from the calculation, using any of the input argument combinations in
## previous syntaxes.  By default, NaN values are included in the calculation
## (@var{nanflag} has the value "includenan").  To exclude NaN values, set the
## value of @var{nanflag} to "omitnan".
##
## @seealso{median, mode}
## @end deftypefn

function y = mean (x, varargin)

  if (nargin < 1 || nargin > 4 || any (cellfun (@isnumeric, varargin(2:end))))
    print_usage ();
  endif

  ## Check all char arguments.
  all_flag = false;
  omitnan = false;
  outtype = "default";

  for i = 1:length (varargin)
    if (ischar (varargin{i}))
      switch (varargin{i})
        case "all"
          all_flag = true;
        case "omitnan"
          omitnan = true;
        case "includenan"
          omitnan = false;
        case {"default", "double", "native"}
          outtype = varargin{i};
        otherwise
          print_usage ();
      endswitch
    endif
  endfor
  varargin(cellfun (@ischar, varargin)) = [];

  if (((length (varargin) == 1) && ! (isnumeric (varargin{1}))) ...
      || (length (varargin) > 1))
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("mean: X must be either a numeric or boolean vector or matrix");
  endif

  if (length (varargin) == 0)

    ## Single numeric input argument, no dimensions given.
    if (all_flag)
      n = length (x(:));
      if (omitnan)
        n = length (x(! isnan (x)));
        x(isnan (x)) = 0;
      endif
      y = sum (x(:), 1) ./ n;
    else
      sz = size (x);
      dim = find (sz > 1, 1);
      if length (dim) == 0
        dim = 1;
      endif
      n = size (x, dim);
      if (omitnan)
        n = sum (! isnan (x), dim);
        x(isnan (x)) = 0;
      endif
      y = sum (x, dim) ./ n;
    endif

  else

    ## Two numeric input arguments, dimensions given.  Note scalar is vector!
    vecdim = varargin{1};
    if (! (isvector (vecdim) && all (vecdim)) || any (rem (vecdim, 1)))
      error ("mean: Dimension must be a positive integer scalar or vector");
    endif

    if (isscalar (vecdim))

      n = size (x, vecdim);
      if (omitnan)
        n = sum (! isnan (x), vecdim);
        x(isnan (x)) = 0;
      endif
      y = sum (x, vecdim) ./ n;

    else

      sz = size (x);
      ndims = length (sz);
      misdim = [1:ndims];

      ## keep remaining dimensions
      for i = 1:length (vecdim)
        misdim(misdim == vecdim(i)) = [];
      endfor

      switch (length (misdim))
        ## if all dimensions are given, compute x(:)
        case 0
          n = length (x(:));
          if (omitnan)
            n = length (x(! isnan (x)));
            x(isnan (x)) = 0;
          endif
          y = sum (x(:), 1) ./ n;

        ## for 1 dimension left, return column vector
        case 1
          x = permute (x, [misdim, vecdim]);
          for i = 1:size (x, 1)
            x_vec = x(i,:,:,:,:,:,:)(:);
            if (omitnan)
              x_vec = x_vec(! isnan (x_vec));
            endif
            y(i) = sum (x_vec, 1) ./ length (x_vec);
          endfor

        ## for 2 dimensions left, return matrix
        case 2
          x = permute (x, [misdim, vecdim]);
          for i = 1:size (x, 1)
            for j = 1:size (x, 2)
              x_vec = x(i,j,:,:,:,:,:)(:);
              if (omitnan)
                x_vec = x_vec(! isnan (x_vec));
              endif
              y(i,j) = sum (x_vec, 1) ./ length (x_vec);
            endfor
          endfor

        ## for more that 2 dimensions left, print usage
        otherwise
          error ("vecdim must index at least N-2 dimensions of X");
      endswitch
    endif

  endif

  ## Convert output as requested
  switch (outtype)
    case "default"
      ## do nothing, the operators already do the right thing
    case "double"
      y = double (y);
    case "native"
      if (! islogical (x))
        y = cast (y, class (x));
      endif
    otherwise
      error ("mean: OUTTYPE '%s' not recognized", outtype);
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

## Test input validation
%!error <Invalid call to mean.  Correct usage is> mean ()
%!error <Invalid call to mean.  Correct usage is> mean (1, 2, 3)
%!error <Invalid call to mean.  Correct usage is> mean (1, 2, 3, 4)
%!error <Invalid call to mean.  Correct usage is> mean (1, "all", 3)
%!error <Invalid call to mean.  Correct usage is> mean (1, "b")
%!error <Invalid call to mean.  Correct usage is> mean (1, 1, "foo")
%!error <X must be either a numeric or boolean> mean ({1:5})
%!error <X must be either a numeric or boolean> mean ("char")
%!error <Dimension must be a positive integer> mean (1, ones (2,2))
%!error <Dimension must be a positive integer> mean (1, 1.5)
%!error <Dimension must be a positive integer> mean (1, 0)
%!error <vecdim must index at least N-2 dimensions of X> ...
%!  mean (repmat ([1:20;6:25], [5 2 6 3 5]), [1 2])

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

## Test dimension indexing with vecdim in n-dimensional arrays
%!test
%! x = repmat ([1:20;6:25], [5 2 6 3]);
%! assert (size (mean (x, [3 2])), [10 3]);
%! assert (size (mean (x, [1 2])), [6 3]);
%! assert (size (mean (x, [1 2 4])), [1 6]);
%! assert (size (mean (x, [1 4 3])), [1 40]);
%! assert (size (mean (x, [1 2 3 4])), [1 1]);

## Test results with vecdim in n-dimensional arrays and "omitnan"
%!test
%! x = repmat ([1:20;6:25], [5 2 6 3]);
%! m = repmat ([10.5;15.5], [5,3]);
%! assert (mean (x, [3 2]), m, 4e-14);
%! x(2,5,6,3) = NaN;
%! m(2,3) = NaN;
%! assert (mean (x, [3 2]), m, 4e-14);
%! m(2,3) = 15.52301255230125;
%! assert (mean (x, [3 2], "omitnan"), m, 4e-14);
