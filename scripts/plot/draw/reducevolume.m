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
## @deftypefn  {} {[@var{nx}, @var{ny}, @var{nz}, @var{nv}] =} reducevolume (@var{v}, @var{r})
## @deftypefnx {} {[@var{nx}, @var{ny}, @var{nz}, @var{nv}] =} reducevolume (@var{x}, @var{y}, @var{z}, @var{v}, @var{r})
## @deftypefnx {} {@var{nv} =} reducevolume (@dots{})
##
## Reduce the volume of the dataset in @var{v} according to the values in
## @var{r}.
##
## @var{v} is a matrix that is non-singleton in the first 3 dimensions.
##
## @var{r} can either be a vector of 3 elements representing the reduction
## factors in the x-, y-, and z-directions or a scalar, in which case the same
## reduction factor is used in all three dimensions.
##
## @code{reducevolume} reduces the number of elements of @var{v} by taking
## only every @var{r}-th element in the respective dimension.
##
## Optionally, @var{x}, @var{y}, and @var{z} can be supplied to represent the
## set of coordinates of @var{v}.  They can either be matrices of the same size
## as @var{v} or vectors with sizes according to the dimensions of @var{v}, in
## which case they are expanded to matrices
## (@pxref{XREFmeshgrid,,@code{meshgrid}}).
##
## If @code{reducevolume} is called with two arguments then @var{x}, @var{y},
## and @var{z} are assumed to match the respective indices of @var{v}.
##
## The reduced matrix is returned in @var{nv}.
##
## Optionally, the reduced set of coordinates are returned in @var{nx},
## @var{ny}, and @var{nz}, respectively.
##
## Examples:
##
## @example
## @group
## @var{v} = reshape (1:6*8*4, [6 8 4]);
## @var{nv} = reducevolume (@var{v}, [4 3 2]);
## @end group
## @end example
##
## @example
## @group
## @var{v} = reshape (1:6*8*4, [6 8 4]);
## @var{x} = 1:3:24;  @var{y} = -14:5:11;  @var{z} = linspace (16, 18, 4);
## [@var{nx}, @var{ny}, @var{nz}, @var{nv}] = reducevolume (@var{x}, @var{y}, @var{z}, @var{v}, [4 3 2]);
## @end group
## @end example
##
## @seealso{isosurface, isonormals}
## @end deftypefn

function [nx, ny, nz, nv] = reducevolume (varargin)

  if (nargin < 2 || nargin > 5)
    print_usage ();
  endif

  [x, y, z, v, r] = __get_check_reducevolume_args__ (nargout, varargin{:});

  [nx, ny, nz, nv] = __reducevolume__ (x, y, z, v, r);

  if (nargout <= 1)
    nx = nv;
  endif

endfunction

function [x, y, z, v, r] = __get_check_reducevolume_args__ (naout, varargin)

  x = y = z = [];

  switch (nargin)
    case 3
      v = varargin{1};
      r = varargin{2};

    case 6
      if (naout == 4)
        x = varargin{1};
        y = varargin{2};
        z = varargin{3};
      endif
      v = varargin{4};
      r = varargin{5};

    otherwise
      error ("reducevolume: incorrect number of arguments");

  endswitch

  ## Check reduction values R
  if (isscalar (r))
    r = [r, r, r];
  elseif (numel (r) != 3)
    error (["reducevolume: reduction value R must be a scalar or " ...
            "a vector of length 3"]);
  endif

  if (any (r < 1 | r != fix (r)))
    error ("reducevolume: reduction values R must be positive integers");
  endif

  ## Check dimensions of data
  if (ndims (v) < 3)
    error ("reducevolume: data V must have at least 3 dimensions");
  endif

  v_sz = size (v);
  if (any (v_sz(1:3) < 2))
    error ("reducevolume: data must be a non-singleton 3-dimensional matrix");
  endif

  if (naout == 4)
    if (isempty (x))
      x = 1:columns (v);
    endif
    if (isempty (y))
      y = 1:rows (v);
    endif
    if (isempty (z))
      z = 1:size (v, 3);
    endif

    ## check x
    if (isvector (x) && length (x) == v_sz(2))
      x = repmat (x(:)', [v_sz(1) 1 v_sz(3)]);
    elseif (! size_equal (v, x))
      error ("reducevolume: X must match the size of data V");
    endif

    ## check y
    if (isvector (y) && length (y) == v_sz(1))
      y = repmat (y(:), [1 v_sz(2) v_sz(3)]);
    elseif (! size_equal (v, y))
      error ("reducevolume: Y must match the size of data V");
    endif

    ## check z
    if (isvector (z) && length (z) == v_sz(3))
      z = repmat (reshape (z(:), [1 1 length(z)]), ...
                  [v_sz(1) v_sz(2) 1]);
    elseif (! size_equal (v, z))
      error ("reducevolume: Z must match the size of data V");
    endif

  endif

endfunction

function [nx, ny, nz, nv] = __reducevolume__ (x, y, z, v, r)

  v_sz = size (v);
  nv = v(1:r(2):end, 1:r(1):end, 1:r(3):end, :);
  nv_sz = size (nv);
  if (length (nv_sz) < 3 || min (nv_sz) < 2)
    error ("reducevolume: reduction value R is too high");
  endif
  if (length (v_sz) > 3)
    nv = reshape (nv, [nv_sz(1:3) v_sz(4:end)]);
  endif

  if (isempty (x))
    nx = ny = nz = [];
  else
    nx = x(1:r(2):end, 1:r(1):end, 1:r(3):end);
    ny = y(1:r(2):end, 1:r(1):end, 1:r(3):end);
    nz = z(1:r(2):end, 1:r(1):end, 1:r(3):end);
  endif

endfunction


%!shared v, x, y, z, xx, yy, zz
%! v = reshape (1:6*8*4, [6 8 4]);
%! x = 1:3:22;  y = -14:5:11;  z = linspace (16, 18, 4);
%! [xx, yy, zz] = meshgrid (x, y, z);

## two inputs, one output
%!test
%! nv = reducevolume (v, [4 3 2]);
%! nv_expected = [1 25; 4 28];  nv_expected(:,:,2) = [97 121; 100 124];
%! assert (nv, nv_expected);

## two inputs, four outputs
%!test
%! [nx, ny, nz, nv] = reducevolume (v, [4 3 2]);
%! nx_expected(1:2,1,1:2) = 1;  nx_expected(:,2,:) = 5;
%! ny_expected(1,1:2,1:2) = 1;  ny_expected(2,:,:) = 4;
%! nz_expected(1:2,1:2,1) = 1;  nz_expected(:,:,2) = 3;
%! nv_expected = [1 25; 4 28];  nv_expected(:,:,2) = [97 121; 100 124];
%! assert (nx, nx_expected);
%! assert (ny, ny_expected);
%! assert (nz, nz_expected);
%! assert (nv, nv_expected);

## five inputs, one output
%!test
%! nv = reducevolume (x, y, z, v, [4 3 2]);
%! nv_expected = [1 25; 4 28];  nv_expected(:,:,2) = [97 121; 100 124];
%! assert (nv, nv_expected);

## five inputs, four outputs (coordinates are vectors)
%!test
%! [nx, ny, nz, nv] = reducevolume (x, y, z, v, [4 3 2]);
%! nx_expected(1:2,1,1:2) = x(1); nx_expected(:,2,:) = x(5);
%! ny_expected(1,1:2,1:2) = y(1); ny_expected(2,:,:) = y(4);
%! nz_expected(1:2,1:2,1) = z(1); nz_expected(:,:,2) = z(3);
%! nv_expected = [1 25; 4 28]; nv_expected(:,:,2) = [97 121; 100 124];
%! assert (nx, nx_expected);
%! assert (ny, ny_expected);
%! assert (nz, nz_expected);
%! assert (nv, nv_expected);

## five inputs, four outputs (coordinates are matrices)
%!test
%! [nx, ny, nz, nv] = reducevolume (xx, yy, zz, v, [4 3 2]);
%! nx_expected(1:2,1,1:2) = x(1); nx_expected(:,2,:) = x(5);
%! ny_expected(1,1:2,1:2) = y(1); ny_expected(2,:,:) = y(4);
%! nz_expected(1:2,1:2,1) = z(1); nz_expected(:,:,2) = z(3);
%! nv_expected = [1 25; 4 28]; nv_expected(:,:,2) = [97 121; 100 124];
%! assert (nx, nx_expected);
%! assert (ny, ny_expected);
%! assert (nz, nz_expected);
%! assert (nv, nv_expected);

## five inputs, four outputs (coordinates are matrices, R is scalar)
%!test
%! [nx, ny, nz, nv] = reducevolume (xx, yy, zz, v, 3);
%! nx_expected(1:2,1,1:2) = x(1); nx_expected(:,2,:) = x(4);
%! nx_expected(:,3,:) = x(7);
%! ny_expected(1,1:3,1:2) = y(1); ny_expected(2,:,:) = y(4);
%! nz_expected(1:2,1:3,1) = z(1); nz_expected(:,:,2) = z(4);
%! nv_expected = [1 19 37; 4 22 40];
%! nv_expected(:,:,2) = [145 163 181; 148 166 184];
%! assert (nx, nx_expected);
%! assert (ny, ny_expected);
%! assert (nz, nz_expected);
%! assert (nv, nv_expected);

## Test for each error
%!test
%!error <Invalid call> reducevolume ()
%!error <Invalid call> reducevolume (1)
%!error <Invalid call> reducevolume (1,2,3,4,5,6)
%!error <incorrect number of arguments> reducevolume (1, 2, 3)
%!error <R must be a scalar or a vector of length 3> reducevolume (v, [])
%!error <R must be a scalar or a vector of length 3> reducevolume (v, [1 2])
%!error <reduction values R must be positive integers> reducevolume (v, 0)
%!error <reduction values R must be positive integers> reducevolume (v, 1.5)
%!error <data V must have at least 3 dimensions>
%! v = reshape(1:6*8, [6 8]);
%! [nv] = reducevolume (v, [4 3 2]);
%!error <data must be a non-singleton 3-dimensional matrix>
%! v = reshape(1:6*8, [6 1 8]);
%! nv = reducevolume (v, [4 3 2]);
%!error <X must match the size of data V>
%! x = 1:2:24;
%! [nx, ny, nz, nv] = reducevolume (x, y, z, v, [4 3 2]);
%!error <Y must match the size of data V>
%! y = -14:6:11;
%! [nx, ny, nz, nv] = reducevolume (x, y, z, v, [4 3 2]);
%!error <Z must match the size of data V>
%! z = linspace (16, 18, 5);
%! [nx, ny, nz, nv] = reducevolume (x, y, z, v, [4 3 2]);
%!error <reduction value R is too high> [nv] = reducevolume (v, 5)
%!error <reduction value R is too high> [nv] = reducevolume (v, [4 7 2])
