########################################################################
##
## Copyright (C) 2018-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{y} =} movfun (@var{fcn}, @var{x}, @var{wlen})
## @deftypefnx {} {@var{y} =} movfun (@var{fcn}, @var{x}, @var{[@var{nb}, @var{na}}])
## @deftypefnx {} {@var{y} =} movfun (@dots{}, "@var{property}", @var{value})
##
## Apply function @var{fcn} to a moving window of length @var{wlen} on data
## @var{x}.
##
## If @var{wlen} is a scalar, the function @var{fcn} is applied to a moving
## window of length @var{wlen}.  When @var{wlen} is an odd number the window is
## symmetric and includes @w{@code{(@var{wlen} - 1) / 2}} elements on either
## side of the central element.  For example, when calculating the output at
## index 5 with a window length of 3, @code{movfun} uses data elements
## @w{@code{[4, 5, 6]}}.  If @var{wlen} is an even number, the window is
## asymmetric and has @w{@code{@var{wlen}/2}} elements to the left of the
## central element and @w{@code{@var{wlen}/2 - 1}} elements to the right of the
## central element.  For example, when calculating the output at index 5 with a
## window length of 4, @code{movfun} uses data elements
## @w{@code{[3, 4, 5, 6]}}.
##
## If @var{wlen} is an array with two elements @w{@code{[@var{nb}, @var{na}]}},
## the function is applied to a moving window @code{-@var{nb}:@var{na}}.  This
## window includes @var{nb} number of elements @emph{before} the current
## element and @var{na} number of elements @emph{after} the current element.
## The current element is always included.  For example, given
## @w{@code{@var{wlen} = [3, 0]}}, the data used to calculate index 5 is
## @w{@code{[2, 3, 4, 5]}}.
##
## During calculations the data input @var{x} is reshaped into a 2-dimensional
## @var{wlen}-by-@var{N} matrix and @var{fcn} is called on this new matrix.
## Therefore, @var{fcn} must accept an array input argument and apply the
## computation along dimension 1, i.e., down the columns of the array.
##
## When applied to an array (possibly multi-dimensional) with @var{n} columns,
## @var{fcn} may return a result in either of two formats: @w{Format 1)}
## an array of size 1-by-@var{n}-by-@var{dim3}-by-@dots{}-by-@var{dimN}.  This
## is the typical output format from Octave core functions.  Type
## @code{demo ("movfun", 5)} for an example of this use case.
## @w{Format 2)} a row vector of length
## @code{@var{n} * @var{numel_higher_dims}} where @var{numel_higher_dims} is
## @w{@code{prod (size (@var{x})(3:end))}}.  The output of @var{fcn} for the
## i-th input column must be found in the output at indices
## @w{@code{i:@var{n}:(@var{n}*@var{numel_higher_dims})}}.
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
## The window is truncated at the beginning and end of the array to exclude
## elements for which there is no source data.  For example, with a window of
## length 3, @code{@var{y}(1) = @var{fcn} (@var{x}(1:2))}, and
## @code{@var{y}(end) = @var{fcn} (@var{x}(end-1:end))}.
##
## @item @qcode{"discard"}
## Any @var{y} values that use a window extending beyond the original
## data array are deleted.  For example, with a 10-element data vector and a
## window of length 3, the output will contain only 8 elements.  The first
## element would require calculating the function over indices
## @w{@code{[0, 1, 2]}} and is therefore discarded.  The last element would
## require calculating the function over indices @w{@code{[9, 10, 11]}} and is
## therefore discarded.
##
## @item @qcode{"fill"}
## Any window elements outside the data array are replaced by @code{NaN}.  For
## example, with a window of length 3,
## @code{@var{y}(1) = @var{fcn} ([NaN, @var{x}(1:2)])}, and
## @code{@var{y}(end) = @var{fcn} ([@var{x}(end-1:end), NaN])}.
## This option usually results in @var{y} having @code{NaN} values at the
## boundaries, although it is influenced by how @var{fcn} handles @code{NaN},
## and also by the property @qcode{"nancond"}.
##
## @item @var{user_value}
## Any window elements outside the data array are replaced by the specified
## value @var{user_value} which must be a numeric scalar.  For example, with a
## window of length 3,
## @code{@var{y}(1) = @var{fcn} ([@var{user_value}, @var{x}(1:2)])}, and
## @code{@var{y}(end) = @var{fcn} ([@var{x}(end-1:end), @var{user_value}])}.
## A common choice for @var{user_value} is 0.
##
## @item @qcode{"same"}
## Any window elements outside the data array are replaced by the value of
## @var{x} at the boundary.  For example, with a window of length 3,
## @code{@var{y}(1) = @var{fcn} ([@var{x}(1), @var{x}(1:2)])}, and
## @code{@var{y}(end) = @var{fcn} ([@var{x}(end-1:end), @var{x}(end)])}.
##
## @item @qcode{"periodic"}
## The window is wrapped so that any missing data elements are taken from
## the other side of the data.  For example, with a window of length 3,
## @code{@var{y}(1) = @var{fcn} ([@var{x}(end), @var{x}(1:2)])}, and
## @code{@var{y}(end) = @var{fcn} ([@var{x}(end-1:end), @var{x}(1)])}.
##
## @end table
##
## Note that for some of these choices, the window size at the boundaries is
## not the same as for the central part, and @var{fcn} must work in these
## cases.
##
## @item @qcode{"nancond"}
## Controls whether @code{NaN} and @code{NA} values should be included (value:
## @qcode{"includenan"}), or excluded (value: @qcode{"omitnan"}), from the data
## passed to @var{fcn}.  The default is @qcode{"includenan"}.  Caution:
## The @qcode{"omitnan"} option is not yet implemented.
##
## @item @qcode{"outdim"}
## A row vector that selects which dimensions of the calculation will appear
## in the output @var{y}.  This is only useful when @var{fcn} returns an
## N-dimensional array in @w{Format 1}.  The default is to return all output
## dimensions.
##
## @end table
##
## Programming Note: The property @qcode{"outdim"} can be used to save memory
## when the output of @var{fcn} has many dimensions, or when a wrapper to the
## base function that selects the desired outputs is too costly.  When memory
## is not an issue, the easiest way to select output dimensions is to first
## calculate the complete result with @code{movfun} and then filter that result
## with indexing.  If code complexity is not an issue then a wrapper can be
## created using anonymous functions.  For example, if @code{basefcn}
## is a function returning a @var{K}-dimensional row output, and only
## dimension @var{D} is desired, then the following wrapper could be used.
##
## @example
## @group
## @var{fcn} = @@(x) basefcn (x)(:,columns(x) * (@var{D}-1) + (1:columns(x)));
## @var{y} = movfun (@@fcn, @dots{});
## @end group
## @end example
##
## @seealso{movslice, prepad, postpad, permute, reshape}
## @end deftypefn

function y = movfun (fcn, x, wlen, varargin)

  if (nargin < 3)
    print_usage ();
  endif

  valid_bc = {"shrink", "discard", "fill", "same", "periodic"};

  ## Parse input arguments
  parser = inputParser ();
  parser.FunctionName = "movfun";
  parser.addParamValue ("Endpoints", "shrink", ...
    @(x) any (strcmpi (x, valid_bc)) || (isnumeric (x) && isscalar (x)));
  parser.addParamValue ("dim", [], ...
    @(d) isempty (d) || (isscalar (d) && isindex (d, ndims (x))));
  parser.addParamValue ("nancond", "includenan", ...
    @(x) any (strcmpi (x, {"includenan", "omitnan"})));
  parser.addParamValue ("outdim", [], ...
    @(d) isempty (d) || (isvector (d) && isindex (d)));

  parser.parse (varargin{:});
  bc      = parser.Results.Endpoints;   # boundary condition
  dim     = parser.Results.dim;         # dimension to be used as input
  nancond = parser.Results.nancond;     # whether NaN are ignored or not
  outdim  = parser.Results.outdim;      # selected output dimension of fcn
  clear parser
  ## End parse input arguments

  ## If dim was not provided find the first non-singleton dimension.
  szx = size (x);
  if (isempty (dim))
    (dim = find (szx > 1, 1)) || (dim = 1);
  endif

  N = szx(dim);
  if (N == 0)
    ## Nothing to do.  Return immediately with empty output same shape as input.
    ## Technically, it would be best to return the correct class, rather than
    ## always "double", but this seems like a lot of work for little gain.
    y = zeros (szx);
    return;
  endif

  ## Calculate slicing indices.  This call also validates WLEN input.
  [slc, C, Cpre, Cpos, win] = movslice (N, wlen);

  ## Use [nb, na] format which makes replaceval_bc() simpler.
  if (isscalar (wlen))
    wlen = [wlen, wlen];
  endif

  omitnan = strcmpi (nancond, "omitnan");
  if (omitnan)
    warning ('movfun: "omitnan" is not yet implemented, using "includenan"');
  endif

  ## Move the desired dim to be the 1st dimension (rows)
  nd    = length (szx);                  # number of dimensions
  dperm = [dim, 1:(dim-1), (dim+1):nd];  # permutation of dimensions
  x     = permute (x, dperm);            # permute dims to first dimension
  ncols = prod (szx(dperm(2:end)));      # rest of dimensions as single column
  x     = reshape (x, N, ncols);         # reshape input

  ## Obtain function for boundary conditions
  if (isnumeric (bc))
    bcfcn = @replaceval_bc;
    bcfcn (true, bc);  # initialize replaceval function with value
  else
    switch (tolower (bc))
      case "shrink"
        bcfcn = @shrink_bc;

      case "discard"
        bcfcn = [];
        C -= length (Cpre);
        Cpre = Cpos = [];
        N = length (C);
        szx(dperm(1)) = N;

      case "fill"
        bcfcn = @replaceval_bc;
        bcfcn (true, NaN);

      case "same"
        bcfcn = @same_bc;

      case "periodic"
        bcfcn = @periodic_bc;

    endswitch
  endif

  ## FIXME: Validation doesn't seem to work correctly (noted 12/16/2018).
  ## Validate that outdim makes sense
  fout = fcn (zeros (length (win), 1, class (x)));  # output for window
  yclass = class (fout);                    # record class of fcn output
  noutdim = length (fout);                  # number of output dimensions
  if (! isempty (outdim))
    if (max (outdim) > noutdim)
      error ("Octave:invalid-input-arg", ...
             "movfun: output dimension OUTDIM (%d) is larger than largest available dimension (%d)", ...
             max (outdim), noutdim);
    endif
  else
    outdim = 1:noutdim;
  endif
  soutdim = length (outdim);  # length of selected output dimensions
  ## If noutdim is not one then modify function to handle multiple outputs
  if (noutdim > 1)
    fcn_ = @(x) reshape (fcn (x), columns (x), noutdim)(:, outdim);
  else
    fcn_ = fcn;
  endif

  ## Initialize output array of appropriate size and class.
  y = zeros (N, ncols, soutdim, yclass);
  ## Apply processing to each column
  ## FIXME: Is it faster with cellfun?  Don't think so, but needs testing.
  parfor i = 1:ncols
    y(:,i,:) = movfun_oncol (fcn_, yclass, x(:,i), wlen, bcfcn,
                             slc, C, Cpre, Cpos, win, soutdim);
  endparfor

  ## Restore shape
  y = reshape (y, [szx(dperm), soutdim]);
  y = permute (y, [dperm, nd+1]);
  y = squeeze (y);

endfunction

function y = movfun_oncol (fcn, yclass, x, wlen, bcfcn, slcidx, C, Cpre, Cpos, win, odim)

  N = length (Cpre) + length (C) + length (Cpos);
  y = zeros (N, odim, yclass);

  ## Process center of data
  y(C,:) = fcn (x(slcidx));

  ## Process boundaries
  if (! isempty (Cpre))
    y(Cpre,:) = bcfcn (fcn, x, Cpre, win, wlen, odim);
  endif
  if (! isempty (Cpos))
    y(Cpos,:) = bcfcn (fcn, x, Cpos, win, wlen, odim);
  endif

endfunction

## Apply "shrink" boundary conditions
## Function is not applied to any window elements outside the original data.
function y = shrink_bc (fcn, x, idxp, win, wlen, odim)

  N   = length (x);
  idx = idxp + win;
  tf  = (idx > 0) & (idx <= N);  # idx inside boundaries

  n   = length (idxp);
  y   = zeros (n, odim);
  ## FIXME: This nested for loop accounts for 70% of running time.
  ##        Given that "shrink" is the default Endpoint value this
  ##        code needs to be reworked.
  for i = 1:n
    k      = idx(tf(:,i),i);
    y(i,:) = fcn (x(k));
  endfor

endfunction

## Apply replacement value boundary conditions
## Window is padded at beginning and end with user-specified value.
function y = replaceval_bc (fcn, x, idxp, win, wlen, ~)

  persistent substitute;

  ## In-band method to initialize substitute value
  if (islogical (fcn))
    substitute = x;
    return;
  endif

  if (min (idxp) == 1)
    ## pre-pad window
    sz = size (x);
    sz(1) = wlen(1);
    x = [substitute(ones (sz)); x];
    idx = idxp + win + wlen(1);
  else
    ## post-pad window
    sz = size (x);
    sz(1) = wlen(2);
    x = [x; substitute(ones (sz))];
    idx = idxp + win;
  endif

  y = fcn (x(idx));

endfunction

## Apply "same" boundary conditions
## 'y' values outside window are replaced by value of 'x' at the window
## boundary.
function y = same_bc (fcn, x, idxp, win, ~, ~)

  idx          = idxp + win;
  idx(idx < 1) = 1;
  N            = length (x);
  idx(idx > N) = N;
  y            = fcn (x(idx));

endfunction

## Apply "periodic" boundary conditions
## Window wraps around.  Window values outside data array are replaced with
## data from the other end of the array.
function y = periodic_bc (fcn, x, idxp, win, ~, ~)

  N       = length (x);
  idx     = idxp + win;
  tf      = idx < 1;
  idx(tf) = N + idx(tf);
  tf      = idx > N;
  idx(tf) = idx(tf) - N;
  y       = fcn (x(idx));

endfunction


%!demo
%! clf;
%! t  = 2 * pi * linspace (0,1,100).';
%! x  = sin (3 * t);
%! xn = x + 0.1 * randn (size (x));
%! x_s = movfun (@mean, xn, 5, "Endpoints", "shrink");
%! x_p = movfun (@mean, xn, 5, "Endpoints", "periodic");
%! x_m = movfun (@mean, xn, 5, "Endpoints", "same");
%! x_z = movfun (@mean, xn, 5, "Endpoints", 0);
%! x_f = movfun (@mean, xn, 5, "Endpoints", "fill");
%!
%! h = plot (t, xn, "o;noisy signal;",
%!           t, x, "-;true;",
%!           t, x_s, "-;shrink;",
%!           t, x_p, "-;periodic;",
%!           t, x_m, "-;same;",
%!           t, x_z, "-;zero;",
%!           t, x_f, "-;fill;");
%! set (h(1), "markerfacecolor", "auto");
%! set (h(2:end), "linewidth", 3);
%! axis tight
%! xlabel ("time");
%! ylabel ("signal");
%! title ("moving mean with different boundary conditions");
%! #-----------------------------------------------------------------
%! # Moving mean of noisy sinusoidal function with different boundary
%! # conditions.

%!demo
%! clf;
%! t  = 2 * pi * linspace (0,1,100).';
%! x  = sin (3 * t);
%! xn = x + 0.1 * randn (size (x));
%! nwin = 5;
%! x_ = zeros (rows (x), nwin);
%! wlen = 3 + (1:nwin) * 4;
%! for i = 1:nwin
%!   x_(:,i) = movfun (@mean, xn, wlen(i), "Endpoints", "periodic");
%! endfor
%!
%! h = plot (t, xn, "o",
%!           t, x, "-",
%!           t, x_, "-");
%! set (h(1), "markerfacecolor", "auto");
%! set (h(2:end), "linewidth", 3);
%! axis tight
%! xlabel ("time");
%! ylabel ("signal");
%! title ({'moving mean with "periodic" boundary conditions',
%!         "and windows of different lengths"});
%! legend (h, {"noisy", "true", strsplit(num2str(wlen)){:}});
%! #-----------------------------------------------------------------
%! # Moving mean of noisy sinusoidal function with periodic boundary conditions
%! # using windows of different lengths.

%!demo
%! clf;
%! t  = linspace (0,1,100).';
%! x  = exp (-(t - [0.1:0.3:1]).^2/2/0.1^2);
%! y  = movfun (@max, x, 15);
%!
%! h = plot (t, x, "-",
%!           t, y, "--");
%! axis tight
%! xlabel ("time");
%! ylabel ("signal");
%! title ("moving max of several Gaussian functions");
%! #-----------------------------------------------------------------
%! # Moving max of different Gaussian functions.
%! # Illustrates the application of movfun() to inputs with several columns.

%!demo
%! clf;
%! t  = linspace (0,1-1e-2,100).';
%! w  = 2 * pi * 3;
%! x  = sin (w * t);
%! y  = cos (w * t);
%! y_  = movfun (@diff, x, [1 0], "Endpoints", "periodic");
%! ## Is the same as y_ = x(2:end) - x(1:end-1);
%! dt = t(2) - t(1);
%! y_  = y_ / w / dt;
%!
%! h = plot (t, x, "-",
%!           t, y, "-",
%!           t, y_, ":");
%! set (h, "linewidth", 3);
%! axis tight
%! xlabel ("time");
%! ylabel ("signal");
%! title ("movfun with periodic boundary conditions and asymmetric window");
%! legend (h, {"sin", "cos", "[nb, na]"});
%! #-----------------------------------------------------------------
%! # Backward diff() of sinusoidal function with periodic boundary conditions.
%! # Illustrates the use of asymmetric windows.

%!demo
%! clf;
%! N    = 1e3;
%! wlen = 99;
%! x  = linspace (-1, 1, N).';
%! pp = [-2 0 1 0];
%! y  = polyval (pp, x);
%! yn = y + 0.1 * (abs (y) + 0.5) .* exp (randn (N, 1));
%!
%! st = movfun (@(y) (statistics (y)).', yn, wlen);
%!
%! h = plot (x, y, "-",
%!           x, yn, ".",
%!           x, st(:,[3 6]), "-",
%!           x, st(:,6) + [-1 1].*st(:,7), "-",
%!           x, st(:,[1 2 4 5]), "-");
%! set (h([1 3:4]), "linewidth", 3);  # mean
%! set (h(5:end), "color", "k");
%! axis tight
%! xlabel ("x")
%! ylabel ("y")
%! title ("movfun() with Format 1 output data");
%! legend (h, {"noiseless", "noisy", "mean", "median"})
%! #-----------------------------------------------------------------
%! # Moving window statistics.  The plot highlights mean and median.
%! # Black lines how minimum, first quartile, third quartile, and maximum.
%! # Demo illustrates the use of functions with multidimensional output.

%!demo
%! clf;
%! N    = 1e2;
%! wlen = 9;
%! x  = linspace (-1, 1, N).';
%! pp = [-2 0 1 0];
%! y  = polyval (pp, x);
%! y(:,2) = y + 0.1 * (abs (y) + 0.5) .* exp (randn (N, 1));
%! y(:,1) = -y(:,1) + 0.1 * randn (N, 1);
%!
%! fcn = @(y) [min(y), max(y)];
%! st = movfun (fcn, y, wlen);
%!
%! h = plot (x, y, "o",
%!           x, squeeze (st(:,1,:)), "-",
%!           x, squeeze (st(:,2,:)), "-");
%! axis tight
%! set (h(3:4), "color", get (h(1), "color"));
%! set (h(5:6), "color", get (h(2), "color"));
%! xlabel ("x")
%! ylabel ("y")
%! title ("movfun() with Format 2 output data");
%! legend (h(1:2), {"data1", "data2"})
%! #-----------------------------------------------------------------
%! # Moving min() and max() on the same window.
%! # Demo illustrates the use of functions with flat multidimensional output.

%!test
%! x = (1:10).' + [-3, 0, 4];
%! ctrfun = @(x) x(2,:);
%! valid_bc = {"periodic", 0, "fill", "same"};
%! for bc = valid_bc
%!   assert (movfun (ctrfun, x, 3, "Endpoints", bc{1}), x);
%! endfor
%! x_ = x; x_([1 end],:) = x([2 end],:);
%! assert (movfun (ctrfun, x, 3, "Endpoints", "shrink"), x_);

%!test
%! ## dim == 2, same as transpose
%! x = randi (10, 3);
%! ctrfun = @(x) x(2,:);
%! valid_bc = {"periodic", 0, "fill", "same"};
%! for bc = valid_bc
%!   assert (movfun (ctrfun, x.', 3, "Endpoints", bc{1}, "dim", 2), x.');
%! endfor
%! x_ = x; x_([1 end],:) = x([2 end],:);
%! assert (movfun (ctrfun, x.', 3, "Endpoints", "shrink", "dim", 2), x_.');

%!test
%! x = randi (10, 3, 10, 2);
%! y = movfun (@(x) x(2,:), x, 3, "Endpoints", "same", "dim", 2);
%! assert (x, y);

%!test
%! ## bad zero_bc
%! x = ones (10, 1);
%! y = x; y(1:2) = y([end end-1]) = [0.6;0.8];
%! assert (movfun (@mean, x, 5, "Endpoints", 0), y);

## Asymmetric windows
%!shared x, wlen, wlen02, wlen20, ctrfun, UNO
%! x = (1:10).' + [-3, 0, 4];
%! wlen = [2, 1];
%! wlen02 = [0, 2];
%! wlen20 = [2, 0];
%! ctrfun = @(x) x(wlen(1)+1,:);
%! UNO = ones (7,1);

%!assert (movfun (ctrfun, x, wlen, "Endpoints", "periodic"), x)
%!assert (movfun (ctrfun, x, wlen, "Endpoints", 0), x)
%!assert (movfun (ctrfun, x, wlen, "Endpoints", "fill"), x)
%!assert (movfun (ctrfun, x, wlen, "Endpoints", "same"), x)
## for shorter x, indexing fails
%!error movfun (ctrfun, x, wlen, "Endpoints", "shrink")

%!assert (movfun (@min, UNO, wlen, "Endpoints", "shrink"), UNO)
%!assert (movfun (@min, UNO, wlen02, "Endpoints", "shrink"), UNO)
%!assert (movfun (@min, UNO, wlen20, "Endpoints", "shrink"), UNO)

%!assert (movfun (@min, UNO, wlen02, "Endpoints", "periodic"), UNO)
%!assert (movfun (@min, UNO, wlen20, "Endpoints", "periodic"), UNO)

%!assert (movfun (@max, UNO, wlen02, "Endpoints", 0), UNO)
%!assert (movfun (@max, UNO, wlen20, "Endpoints", 0), UNO)

%!assert (movfun (@min, UNO, wlen02, "Endpoints", "fill"), UNO)
%!assert (movfun (@min, UNO, wlen20, "Endpoints", "fill"), UNO)

%!assert (movfun (@min, UNO, wlen02, "Endpoints", "same"), UNO)
%!assert (movfun (@min, UNO, wlen20, "Endpoints", "same"), UNO)

## Multi-dimensional output
%!assert (size( movfun (@(x) [min(x), max(x)], (1:10).', 3)), [10 2])
%!assert (size( movfun (@(x) [min(x), max(x)], cumsum (ones (10,5),2), 3)),
%!        [10 5 2])
## outdim > dim
%!error movfun (@(x) [min(x), max(x)], (1:10).', 3, "Outdim", 3)

## Test for correct return class based on output of function. 
%!test <*63802>
%! x = single (1:10);
%! y = movfun (@mean, x, 3);
%! assert (class (y), 'single');
%! y = movfun (@mean, uint8 (x), 3);
%! assert (class (y), 'double');

## Test calculation along empty dimension
%!assert <*63802> (movfun (@mean, zeros (2,0,3, 'uint8'), 3, 'dim', 2),
%!                 zeros (2,0,3, 'double'))

## Test input validation
%!error <Invalid call> movfun ()
%!error <Invalid call> movfun (@min)
%!error <Invalid call> movfun (@min, 1)
%!error <WLEN must be .* array of integers> movfun (@min, 1, {1})
%!error <WLEN must be .* array of integers .= 0> movfun (@min, 1, -1)
%!error <WLEN must be .* array of integers> movfun (@min, 1, 1.5)
%!error <WLEN must be . 1> movfun (@min, 1, 1)
%!error <WLEN must be a scalar or 2-element array> movfun (@min, 1, [1, 2, 3])
%!error <WLEN \(3\) must be shorter than length along DIM \(1\)>
%! movfun (@min, 1, 3)
%!error <WLEN \(4\) must be shorter than length along DIM \(1\)>
%! movfun (@min, 1, [4, 1]);
%!error <WLEN \(5\) must be shorter than length along DIM \(1\)>
%! movfun (@min, 1, [1, 5]);
%!warning <"omitnan" is not yet implemented>
%! movfun (@min, 1:3, 3, "nancond", "omitnan");
## FIXME: This test is commented out until OUTDIM validation is clarified.
%!#error <OUTDIM \(5\) is larger than largest available dimension \(3\)>
%! movfun (@min, ones (6,3,4), 3, "outdim", 5);
