## Copyright (C) 2018 Juan Pablo Carbajal
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

## Author: Juan Pablo Carbajal <ajuanpi+dev@gmail.com>
## Created: 2018-08-09

## -*- texinfo -*-
## @deftypefn  {} {@var{y} =} movfun (@var{fun}, @var{x}, @var{wlen})
## @deftypefnx {} {@var{y} =} movfun (@var{fun}, @var{x}, @var{[@var{nb}, @var{na}}])
## @deftypefnx {} {@var{y} =} movfun (@dots{}, @var{property}, @var{value})
## Apply function @var{fun} to a moving window of length @var{wlen} on data
## @var{x}.
##
## If @var{wlen} is a scalar, the function @var{fun} is applied to a moving
## window of length @var{wlen}.  In this case @var{wlen} must be an odd number.
## If @var{wlen} is an array with two elements @w{@code{[@var{nb}, @var{na}]}},
## the function is applied to a moving window @code{-@var{nb}:@var{na}}.  This
## window includes @var{nb} number of elements @strong{before} the current
## element and @var{na} number of elements @strong{after} the current element.
## The current element is always included.
##
## During calculations the data input @var{x} is reshaped into a 2-dimensional
## @var{wlen}-by-@var{N} matrix and @var{fun} is called on this new matrix.
## Therefore, @var{fun} must accept an array input argument and apply the
## computation on the columns of that array.
##
## When applied to a column vector of length @var{n}, the function @var{fun}
## must return a @strong{row} vector of length @var{n}.
## When applied to an array (possibly multi-dimensional) with @var{n} columns,
## @var{fun} may return a result in either of two formats: @w{Format 1)}
## an array of size 1-by-@var{n}-by-@var{dim3}-by-@dots{}-by-@var{dimN}.  This
## is the typical output format from Octave core functions.  Type
## @code{demo ("movfun", 5)} for an example of this use case.
## @w{Format 2)} a row vector of length
## @code{@var{n} * @var{numel_higher_dims}} where @var{numel_higher_dims} is
## @w{@code{prod (size (@var{x})(3:end))}}.  The output of @var{fun} for the
## i-th input column must be found in the output at indices
## @code{i:@var{n}:(@var{n}*@var{numel_higher_dims})}.
## This format is useful when concatenating functions into arrays, or when
## using @code{nthargout}.  Type @code{demo ("movfun", 6)} for an example of
## this case.
##
## The calculation can be controlled by specifying @var{property}/@var{value}
## pairs.  Valid properties are
##
## @table @asis
##
## @item @qcode{"dim"}
## Operate along the dimension specified, rather than the default of the first
## non-singleton dimension.
##
## @item @qcode{"Endpoints"}
##
## This property controls how results are calculated at the boundaries
## (@w{endpoints}) of the window.  Possible values are:
##
## @table @asis
## @item @qcode{"shrink"}  (default)
## The window is truncated at the beginning and end of the array to include
## only valid elements.  For example, with a window of length 3,
## @code{@var{y}(1) = @var{fun} (@var{x}(1:2))}, and
## @code{@var{y}(end) = @var{fun} (@var{x}(end-1:end))}.
##
## @item @qcode{"periodic"}
## The window is wrapped around so that
## @code{@var{y}(1) = @var{fun} ([@var{x}(end-@var{k}:end),
## @var{x}(1:@var{k})])}, where @var{k} is the radius of the window.  For
## example, with a window of length 3, 
## @code{@var{y}(1) = @var{fun} ([@var{x}(end-1:end), @var{x}(1)])},
##
## @item @qcode{"zero"}
## The array is pre-padded and post-padded with zeros to exactly contain the
## window.  For example, with a window of length 3,
## @code{@var{y}(1) = @var{fun} ([0, @var{x}(1:2)])}, and
## @code{@var{y}(end) = @var{fun} ([@var{x}(end-1:end), 0])}.
##
## @item @qcode{"same"}
## The resulting array @var{y} has the same values as @var{x} at the
## boundaries.
##
## @item @qcode{"fill"}
## The resulting array @var{y} has @code{NaN} at the boundaries.
##
## @end table
##
## Note that for some of these values, the window size at the boundaries is not
## the same as in the middle part, and @var{fun} must work with these cases.
##
## @item @qcode{"nancond"}
## Controls whether @code{NaN} or @code{NA} values should be excluded (value:
## @qcode{"omitnan"}), or included (value: @qcode{"includenan"}) in the
## arguments passed to @var{fun}.  The default is @qcode{"omitnan"}.
##
## @item @qcode{"outdim"}
## A row vector that selects which dimensions of the calculation will appear
## in the output @var{y}.  This is only useful when @var{fun} returns an
## N-dimensinoal array in @w{Format 1}.  The default is to return all output
## dimensions.
##
## @end table
##
## Programming Note: The property @qcode{"outdim"} can be used to save memory
## when the output of @var{fun} has many dimensions, or when a wrapper to the
## base function that selects the desired outputs is too costly.  When memory
## is not an issue, the easiest way to select output dimensions is to first
## calculate the complete result with @code{movfun} and then filter that result
## with indexing.  If code complexity is not an issue then a wrapper can be
## created using anonymous functions.  For example, if @code{basefun}
## is a function returning a @var{K}-dimensional row output, and only
## dimension @var{D} is desired, then the following wrapper could be used.
##
## @example
## @group
## @var{fun} = @@(x) basefun (x)(:,size(x,2) * (@var{D}-1) + (1:size(x,2)));
## @var{y} = movfun (@@fun, @dots{});
## @end group
## @end example
##
## @seealso{movslice, prepad, postpad, permute, reshape}
## @end deftypefn

## FIXME: variable names in function prototype should match documentation.
function Y = movfun (func, X, wlen, varargin)
  persistent dispatch;

  valid_bc = {'open', 'periodic', 'same', 'zero', 'discard'};
  if (isempty (dispatch))
    dispatch = struct ();
    for k = valid_bc
      cmd = sprintf ('dispatch.%s = @%s_bc;', k{1}, k{1});
      eval (cmd);
    endfor
  endif

  # Parse input arguments
  parser              = inputParser ();
  parser.FunctionName = 'movfun';
  parser.addParamValue ('Endpoints', 'open', ...
    @(x)any (ismember (tolower (x), valid_bc)));
  parser.addParamValue ('dim', [], ...
    @(x) isempty(x) || (isscalar (x) && x > 0 && x <= ndims(X)));
  parser.addParamValue ('nancond', 'omitnan', ...
    @(x) any (ismember (x, {'omitnan', 'includenan'})));
  parser.addParamValue ('outdim', [], ...
    @(x) isempty(x) || (isvector (x) && all (x > 0)));

  parser.parse(varargin{:});
  bc      = parser.Results.Endpoints;   % boundary condition
  dim     = parser.Results.dim;         % dimension to be used as input
  nancond = parser.Results.nancond;     % whether nan are ignored or not
  outdim  = parser.Results.outdim;      % selected output dimension of func
  clear parser
  #### End parse input arguments

  # If dim was not provided search the first non-singleton dimension
  szX  = size (X);
  if (isempty (dim))
    dim  = find (szX > 1, 1, 'first');
  endif

  # Window length is not a vector wlen = [wpre wpos]
  if (isscalar (wlen))
    # Check for proper window length
    # TODO: Matlab accepts even windows
    if (mod (wlen, 2) == 0)
      error ('Octave:invalid-input-arg', 'Window must be of odd length');
    endif
    if (wlen == 1)
      error ('Octave:invalid-input-arg', 'Window must be larger than 1');
    endif
  endif

  # Check that array is longer that wlen at dim. At least one full window must 
  # fit. Function max is used to include the case when wlen is an array.
  # TODO: consider bc to decide what to do here
  if (max (wlen) > szX(dim))
      error ('Octave:invalid-input-arg', ...
        'window length (%d) must be shorter than length along dim (%d=%d)', ...
        wlen, dim, szX(dim));
  endif

  # Move the desired dim to the 1st dimension
  dims  = length (szX);                  % number of dimensions
  dperm = [dim 1:(dim-1) (dim+1):dims];  % permutation of dimensions
  X     = permute (X, dperm);            % permute dim to first dimensions
  ncols = prod (szX(dperm(2:end)));      % rest dimensions as single column
  N     = szX(dperm(1));                 % length of dim
  X     = reshape (X, N, ncols);         % reshape input

  # obtain function for boundary conditions
  bcfunc = dispatch.(tolower (bc));

  # obtain slicer
  [slc, C, Cpre, Cpos, win] = movslice (N, wlen);

  # Check that outdim makes sense
  tmp     = func (zeros (length (win), 1));  % output for window
  noutdim = length (tmp);                    % number of output dimensions
  if (!isempty (outdim))
    if (max (outdim) > noutdim)
      error ('Octave:invalid-input-arg', ...
      'Selected output dimension (%d) is larger than output dimensions (%d)', ...
        max (outdim), noutdim);
    endif
  else
    outdim = 1:noutdim;
  endif
  soutdim = length (outdim); % length of selected output dimensions
  # If noutdim is not one then modify function to handle multiple outputs
  if (noutdim > 1)
    func_ = @(x) reshape (func (x), size (x, 2), noutdim)(:,outdim);
  else
    func_ = func;
  endif

  # apply processing to each column
  # Is it faster with cellfun? Do not think so
  # It could be parallel
  Y       = zeros (N, ncols, soutdim);
  for i = 1:ncols
    Y(:,i,:) = movfun_oncol (func_, X(:,i), wlen, bcfunc, ...
          slc, C, Cpre, Cpos, win, soutdim);
  endfor

  # Restore shape
  Y = reshape (Y, [szX(dperm), soutdim]);
  Y = permute (Y, [dperm, dims+1]);
  Y = squeeze (Y);

endfunction

function y = movfun_oncol (func, x, wlen, bcfunc, I, C, Cpre, Cpos, win, odim)
  N = length (x);
  y = NA (N, odim);
  # process center part
  y(C,:) = func (x(I));

  # process boundaries
  if !isempty (Cpre) % don't process zero length bkw-window
    y(Cpre,:) = bcfunc (func, x, Cpre, win, wlen, odim);
  endif
  if !isempty (Cpos) % don't process zero length fwd-window
    y(Cpos,:) = bcfunc (func, x, Cpos, win, wlen, odim);
  endif
endfunction

function y = open_bc (func, x, idxp, win, wlen, odim)
  N   = length (x);
  idx = idxp + win;
  tf  = !((idx < 1) | (idx > N)); % idx inside boundaries

  n   = length (idxp);
  y   = zeros (n, odim);
  for i = 1:n
    k      = idx(tf(:,i),i);
    y(i,:) = func (x(k));
  endfor
endfunction

function y = periodic_bc (func, x, idxp, win)
  N       = length (x);
  idx     = idxp + win;
  tf      = idx < 1;
  idx(tf) = N + idx(tf);
  tf      = idx > N;
  idx(tf) = idx(tf) - N;
  y       = func (x(idx));
endfunction

function y = same_bc (func, x, idxp, win)
  idx          = idxp + win;
  idx(idx < 1) = 1;
  N            = length (x);
  idx(idx > N) = N;
  y            = func (x(idx));
endfunction

function y = zero_bc (func, x, idxp, win, wlen)
  if isscalar (wlen)
    wlen = [wlen wlen];
  endif
  N = length (x);
  if (min (idxp) == 1)
    x = prepad (x, N + wlen(1));
    idx = idxp + win + wlen(1);
  elseif (max (idxp) == N)
    x   = postpad (x, N + wlen(2));
    idx = idxp + win;
  endif
  y = func (x(idx));
endfunction

function y = discard_bc (func, x, idxp, win, wlen, odim)
  y = nan (length (idxp), odim);
endfunction


%!demo
%! t  = 2 * pi * linspace (0,1,100).';
%! x  = sin (3 * t);
%! xn = x + 0.1 * randn (size (x));
%! x_o = movfun (@mean, xn, 5, 'Endpoints', 'open');
%! x_p = movfun (@mean, xn, 5, 'Endpoints', 'periodic');
%! x_f = movfun (@mean, xn, 5, 'Endpoints', 'same');
%! x_z = movfun (@mean, xn, 5, 'Endpoints', 'zero');
%! x_d = movfun (@mean, xn, 5, 'Endpoints', 'discard');
%!
%! figure (), clf
%!    h = plot (
%!              t, xn, 'o;noisy signal;',
%!              t, x, '-;true;',
%!              t, x_o, '-;open;',
%!              t, x_p, '-;periodic;',
%!              t, x_f, '-;same;',
%!              t, x_z, '-;zero;',
%!              t, x_d, '-;discard;');
%!    set (h(1), 'markerfacecolor', 'auto');
%!    set (h(2:end), 'linewidth', 3);
%!    axis tight
%!    xlabel ('time')
%!    ylabel ('signal')
%! #####
%! # Moving mean of noisy sinusoidal function with different boundary 
%! # conditions.

%!demo
%! t  = 2 * pi * linspace (0,1,100).';
%! x  = sin (3 * t);
%! xn = x + 0.1 * randn (size (x));
%! nw = 5;
%! x_ = zeros (size(x,1), nw);
%! w = 3 + (1:nw) * 4;
%! for i=1:nw
%!    x_(:,i) = movfun (@mean, xn, w(i), 'Endpoints', 'periodic');
%!  endfor
%!
%! figure (), clf
%!    h = plot (
%!              t, xn, 'o',
%!              t, x, '-',
%!              t, x_, '-');
%!    set (h(1), 'markerfacecolor', 'auto');
%!    set (h(2:end), 'linewidth', 3);
%!    axis tight
%!    xlabel ('time')
%!    ylabel ('signal')
%!    legend (h, {'noisy', 'true', strsplit(num2str(w)){:}});
%! #####
%! # Moving mean of noisy sinusoidal function with periodic boundary conditions
%! # using windows of different lengths.

%!demo
%! t  = linspace (0,1,100).';
%! X  = exp(-(t - [0.1:0.3:1]).^2/2/0.1^2);
%! Y  = movfun (@max, X, 15);
%! figure (), clf
%!    h = plot (
%!              t, X, '-',
%!              t, Y, '--');
%!    axis tight
%!    xlabel ('time')
%!    ylabel ('signal')
%! #####
%! # Moving max of different Gaussian functions. Illustrates the application
%! # on inputs with several columns.

%!demo
%! t  = linspace (0,1-1e-2,100).';
%! w  = 2 * pi * 3;
%! x  = sin (w * t);
%! y  = cos (w * t);
%! y_  = movfun (@diff, x, [1 0], 'Endpoints', 'periodic');
%! # Is the same as y_ = x(2:end) - x(1:end-1);
%! dt = t(2) - t(1);
%! y_  = y_ / w / dt;
%! figure (), clf
%!    h = plot (t, x, '-',
%!              t, y, '-', 
%!              t, y_, ':');
%!    set (h, 'linewidth', 3);
%!    axis tight
%!    xlabel ('time')
%!    ylabel ('signal')
%!    legend (h, {'sin', 'cos', 'bkw diff'});
%! #####
%! # Backward difference of sinusoidal function with periodic boundary 
%! # conditions. Illustrates the use of asymmetric windows.

%!demo
%! N    = 1e3;
%! wlen = 99;
%! x  = linspace (-1, 1, N).';
%! pp = [-2 0 1 0];
%! y  = polyval (pp, x);
%! yn = y + 0.1 * (abs (y) + 0.5) .* exp (randn (N, 1));
%!
%! st = movfun (@(y)statistics(y).', yn, wlen);
%! figure (), clf
%!    h = plot (x, y, '-',
%!              x, yn, '.',
%!              x, st(:,[3 6]), '-', 
%!              x, st(:,6) + [-1 1].*st(:,7), '-', 
%!              x, st(:,[1 2 4 5]), '-');
%!    set (h([1 3:4]), 'linewidth', 3); % mean
%!    set (h(5:end), 'color', 'k');
%!    axis tight
%!    xlabel ('x')
%!    ylabel ('y')
%!    legend (h, {'noiseless', 'noisy', 'mean', 'median'})
%! #####
%! # Moving window statistics. The plot highlights mean and median. black lines
%! # show minimum, first quartile, third quartile, and maximum.
%! # This illustrates the use of functions with multidimensional output.

%!demo
%! N    = 1e2;
%! wlen = 9;
%! x  = linspace (-1, 1, N).';
%! pp = [-2 0 1 0];
%! y  = polyval (pp, x);
%! y(:,2) = y + 0.1 * (abs (y) + 0.5) .* exp (randn (N, 1));
%! y(:,1) = -y(:,1) + 0.1 * randn (N, 1);
%!
%! func = @(y)[min(y) max(y)];
%! st = movfun (func, y, wlen);
%! figure (), clf
%!    h = plot (x, y, 'o',
%!              x, squeeze (st(:,1,:)), '-', 
%!              x, squeeze (st(:,2,:)), '-');
%!    axis tight
%!    set (h(3:4), 'color', get(h(1), 'color'));
%!    set (h(5:6), 'color', get(h(2), 'color'));
%!    xlabel ('x')
%!    ylabel ('y')
%!    legend (h(1:2), {'data1', 'data2'})
%! #####
%! # Moving window bounds.
%! # This illustrates the use of functions with flat multidimensional output.

%!error(movfun(@min, [0;0], 1)) % wlen == 1
%!error(movfun(@min, [0;0], 2)) % odd wlen
%!error(movfun(@min, [0;0], 3)) % wlen larger than data
%!error(movfun(@min, [0;0;0], [1 4])) % wlen larger than data
%!error(movfun(@min, [0;0;0], [4 1])) % wlen larger than data

%!test
%! X = (1:10).' + [-3, 0, 4];
%! ctrfun = @(x)x(2,:);
%! valid_bc = {'same', 'periodic', 'zero'};
%! for bc = valid_bc
%!   assert (movfun(ctrfun, X, 3, 'Endpoints', bc{1}), X);
%! endfor
%! X_ = X; X_([1 end],:) = nan;
%! assert (movfun(ctrfun, X, 3, 'Endpoints', 'discard'), X_);
%! X_ = X; X_([1 end],:) = X([2 end],:);
%! assert (movfun(ctrfun, X, 3, 'Endpoints', 'open'), X_);

%!test % dim == 2 same as transpose
%! X = randi (10, 3);
%! ctrfun = @(x)x(2,:);
%! valid_bc = {'same', 'periodic', 'zero'};
%! for bc = valid_bc
%!   assert (movfun(ctrfun, X.', 3, 'Endpoints', bc{1}, 'dim', 2), X.');
%! endfor
%! X_ = X; X_([1 end],:) = nan;
%! assert (movfun(ctrfun, X.', 3, 'Endpoints', 'discard', 'dim', 2), X_.');
%! X_ = X; X_([1 end],:) = X([2 end],:);
%! assert (movfun(ctrfun, X.', 3, 'Endpoints', 'open', 'dim', 2), X_.');

%!test
%! X = randi (10, 3, 10, 2);
%! Y = movfun (@(x)x(2,:), X, 3, 'Endpoints', 'same', 'dim', 2);
%! assert (X, Y);

%!test # bad zero_bc
%! X = ones (10, 1);
%! Y = X; Y(1:2) = Y([end end-1]) = [0.6;0.8];
%! assert (movfun (@mean, X, 5, 'Endpoints', 'zero'), Y);

## Asymmetric windows
%!shared X,wlen,wlen0b,wlen0f,ctrfun,Xd,UNO,UNOd0b,UNOd0f
%! X = (1:10).' + [-3, 0, 4];
%! wlen = [2 1];
%! wlen0b = [0 2];
%! wlen0f = [2 0];
%! ctrfun = @(x)x(wlen(1)+1,:);
%! Xd = X; Xd([1:2 end],:) = nan;
%! UNO = ones (7,1);
%! UNOd0b = UNOd0f = UNO; 
%! UNOd0b(end-1:end,:) = nan;
%! UNOd0f(1:2,:) = nan;

%!assert (movfun(ctrfun, X, wlen, 'Endpoints', 'same'), X);
%!assert (movfun(ctrfun, X, wlen, 'Endpoints', 'discard'), Xd);
%!assert (movfun(ctrfun, X, wlen, 'Endpoints', 'periodic'), X);
%!assert (movfun(ctrfun, X, wlen, 'Endpoints', 'zero'), X);
%!error movfun(ctrfun, X, wlen, 'Endpoints', 'open'); % for shorter x, indexing fails

%!assert (movfun(@min, UNO, wlen0b, 'Endpoints', 'same'), UNO);
%!assert (movfun(@min, UNO, wlen0f, 'Endpoints', 'same'), UNO);

%!assert (movfun(@min, UNO, wlen, 'Endpoints', 'open'), UNO);
%!assert (movfun(@min, UNO, wlen0b, 'Endpoints', 'open'), UNO);
%!assert (movfun(@min, UNO, wlen0f, 'Endpoints', 'open'), UNO);

%!assert (movfun(@min, UNO, wlen0b, 'Endpoints', 'discard'), UNOd0b);
%!assert (movfun(@min, UNO, wlen0f, 'Endpoints', 'discard'), UNOd0f);

%!assert (movfun(@min, UNO, wlen0b, 'Endpoints', 'periodic'), UNO);
%!assert (movfun(@min, UNO, wlen0f, 'Endpoints', 'periodic'), UNO);

%!assert (movfun(@max, UNO, wlen0b, 'Endpoints', 'zero'), UNO);
%!assert (movfun(@max, UNO, wlen0f, 'Endpoints', 'zero'), UNO);

## Multidimensional output
%!error(movfun(@(x)[min(x) max(x)], (1:10).', 3, 'Outdim', 3)) % outdim > dim
%!assert(size(movfun(@(x)[min(x) max(x)], (1:10).', 3)), [10 2])
%!assert(size(movfun(@(x)[min(x) max(x)], cumsum(ones(10,5),2), 3)), [10 5 2])
