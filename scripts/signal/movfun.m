########################################################################
##
## Copyright (C) 2018-2024 The Octave Project Developers
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
## @deftypefnx {} {@var{y} =} movfun (@var{fcn}, @var{x}, [@var{nb}, @var{na}])
## @deftypefnx {} {@var{y} =} movfun (@dots{}, "@var{property}", @var{value})
##
## Apply function @var{fcn} to a moving window of length @var{wlen} on data
## @var{x}.
##
## The moving window length input @var{wlen} can either be a numeric scalar
## or a 2-element numeric array @w{@qcode{[@var{nb}, @var{na}]}}. The elements
## included in the moving window depend on the size and value of @var{wlen}
## as well as whether the @qcode{"SamplePoints"} option has been specified.
## For full details of element inclusion,
## @pxref{XREFmovslice,,@code{movslice}}.
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
## @item @qcode{"SamplePoints"}
##
## This property specifies a sorted, numeric vector of unique coordinate
## positions of the data points in @var{x}.  The default value is the vector
## @w{@qcode{[1 : @var{numel (x)}]}}.  When a non-default SamplePoints vector
## is specified, the moving window length @var{wlen} is measured
## against the SamplePoints positions to determine which points are
## included in each window slice.  SamplePoints need not be uniformly spaced.
## This can result in window slices containing different numbers of points.
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
## Note 1:  For non-uniform SamplePoint spacing, the only permitted value for
## @qcode{"EndPoints"} is @qcode{"shrink"}.
##
## Note 2:  For some @qcode{"Endpoints"} options, the window size at the
## boundaries may not be the same as for the central part, and @var{fcn} must
## work in these cases.
##
## @item @qcode{"nancond"}
## Controls whether @code{NaN} and @code{NA} values should be included (value:
## @qcode{"includenan"}), or excluded (value: @qcode{"omitnan"}), from the data
## passed to @var{fcn}.  The default is @qcode{"includenan"}.  The values
## @qcode{"includemissing"} and @qcode{"omitmissing"} may be used synonymously
## with @qcode{"includenan"} and @qcode{"omitnan"}. Note: The
## @qcode{"omitnan"} option is not yet implemented and will default to using
## @qcode{"includenan"} instead.
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
  valid_nancond = {"includenan", "includemissing", "omitnan", "omitmissing"};

  ## Parse required input arguments.
  ## Allow fcn to perform any x validation.
  ## Most wlen validation will be done by movslice.

  if (! is_function_handle (fcn))
    error ("movfun: FCN must be a valid function handle");
  endif

  if (! isnumeric (wlen))
    error ("movfun: WLEN must be numeric");
  endif

  ## Parse optional arguments
  dim = [];
  nancond = "includenan";
  outdim = [];
  sp.samplepoints = [];
  bc = "shrink";

  if nargin > 3
    if (mod (numel (varargin), 2))
      error ("movfun: Each PROPERTY must have a VALUE");
    endif

    vargidx = 1;
    while (vargidx < nargin - 3)
      prop = varargin{vargidx};
      if (! (ischar (prop) && isrow (prop)))
        error ("movfun: PROPERTY name must be a valid string");
      endif

      switch (lower (prop))
        case "dim"
          dim = varargin{++vargidx};
          if (! (isnumeric (dim) && isscalar (dim) && isindex (dim)))
            error ("movfun: DIM must be a positive integer-valued scalar");
          endif

        case "endpoints"
          bc = varargin{++vargidx};
          if (! ((isnumeric (bc) && isscalar (bc)) ||
                  (ischar (bc) && isrow (bc) &&
                   any (strcmpi (bc, valid_bc)))))
            error ("movfun: ENDPOINTS must be a numeric scalar or a valid Endpoint method");
          endif

        case "nancond"
          nancond = varargin{++vargidx};
          if (! (ischar (nancond) && isrow (nancond)
              && (any (strcmpi (nancond, valid_nancond)))))
            error ("movfun: NANCOND must be includenan, includemissing, omitnan, or omitmissing");
          endif

        case "outdim"
          outdim = varargin{++vargidx};
          if (! (isnumeric (outdim) && isvector (outdim)
                  && isindex (outdim)))
            error ("movfun: OUTDIM must be a numeric, positive, integer-valued scalar or vector");
          endif

        case "samplepoints"
          sp.samplepoints = varargin{++vargidx};
          if (! (isnumeric (sp.samplepoints) && isvector (sp.samplepoints)
                 && issorted (sp.samplepoints)
                 && all (diff (sp.samplepoints) != 0)))
            error ("movfun: SAMPLEPOINTS must be a sorted, non-repeating, numeric vector");
          endif
          sp.samplepoints = sp.samplepoints(:);
        otherwise
          error ("movfun: unknown PROPERTY '%s'", prop);
      endswitch

      vargidx++;

   endwhile
  endif

  if (isempty (x))
    ## Nothing to do.  Return immediately with empty output same shape and
    ## class as input.
    clsx = class (x);
    if (isnumeric (x) || islogical (x))
      y = zeros (size (x), clsx);
    else
      try
       y = cast (zeros (size (x)), clsx);
      catch err
        test_err_msg = ["cast: type conversion to '", clsx,"' is not supported"];
        if (strcmp (err.message, test_err_msg))
          y = zeros (size (x));
        else
          rethrow (err);
        endif
      end_try_catch
    endif
    return
  endif

  ## Finish optional parameter processing.

  ## If dim was not provided find the first non-singleton dimension.
  szx = size (x);
  nd = length (szx);
  if (isempty (dim))
    (dim = find (szx > 1, 1)) || (dim = 1);
  elseif (dim > nd)
    szx = [szx, ones(1, dim - nd)];
    nd = dim;
  endif
  N = szx(dim);

  ## Perform remaining samplepoints validation or set to default.
  sp.uniform = true;
  sp.standard = true;
  sp.spacing = 1;

  sp.apply = ! isempty (sp.samplepoints);

  if (sp.apply)
    if (numel (sp.samplepoints) != N)
      error("movfun: SamplePoints must be the same size as x in operating dimension");
    endif

    sp.spacing = diff (sp.samplepoints, 1, 1);
    sp.uniform = all (sp.spacing == sp.spacing(1));

    if (all (sp.spacing == 1))
      sp.standard = true;
      sp.apply = false;
    else
      sp.standard = false;
    endif

    if (sp.uniform)
      sp.spacing = sp.spacing(1);
    endif

    if (! sp.uniform && ! strcmpi (bc, "shrink"))
      error (["movfun: when SamplePoints are not uniformly spaced the ", ...
              "only valid EndPoints option is 'shrink'"]);
    endif
  endif

  ## Process NaN handling behavior.
  omitnan = any (strcmpi (nancond, {"omitnan", "omitmissing"}));
  if (omitnan)
    warning ('movfun: "omitnan" is not yet implemented, using "includenan"');
  endif

  ## Calculate slicing indices.  This call also validates WLEN input.
  ## wlen returned in [nb, na] form for subfunction processing.
  ## wlen integer-scalar state for inclusive/exclusive endpoint limit behavior
  ## returned by movslice, used by endpoints depending on win and not slcidx,
  ## which already includes the inclusive/exclusive behavior.
  ## slc and win forms will depend on whether samplpoints is default of 1:N.
  if (sp.standard)
    [slc, C, Cpre, Cpos, win, wlen] = movslice (N, wlen);
  else
    [slc, C, Cpre, Cpos, win, wlen, sp.scalar_wlen] = movslice (N, wlen, sp.samplepoints);
    ## Note that for non-standard sp, slc points already adjusted for
    ## scalar_wlen. win is always inclusive showing full window extents.
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
    switch (lower (bc))
      case "shrink"
        bcfcn = @shrink_bc;

      case "discard"
        ## Only called if samplepoints are uniform.  Convert slcidx back
        ## into standard form by trimming end cols and expanding center for
        ## all elements.
        bcfcn = [];
        C -= length (Cpre);

        if (sp.apply && sp.uniform)
          slc(:,[Cpre, Cpos]) = [];
          if (isempty(C))
            y = NaN (1, 0);
            return
          else
            slc = linspace (slc(1,C), slc(2, C),
                            diff (slc(:, C(1)), 1) + 1).';
          endif
          sp.apply = false;
        endif

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

  ## Validate that outdim makes sense.
  ## Check fcn ouptut for data sampled from x.  See bug #55984.
  if (! sp.apply)
    fout = fcn (x(1 : min (length (win), N))(:));  # output for window sized data
  else
    fout = fcn (x(1 : min (max (diff (slc, 1, 1)) + 1, N))(:));
  endif

  yclass = class (fout);              # record class of fcn output
  noutdim = length (fout);            # number of output columns
  if (! isempty (outdim))
    if (max (outdim) > noutdim)
      error ("Octave:invalid-input-arg", ...
             ["movfun: output dimension OUTDIM (%d) is larger than ", ...
              "largest available dimension (%d)"], ...
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
    y(:,i,:) = movfun_oncol (fcn_, yclass, x(:,i), wlen, bcfcn, ...
                             slc, C, Cpre, Cpos, win, soutdim, sp);
  endparfor

  ## Restore shape
  y = reshape (y, [szx(dperm), soutdim]);
  y = ipermute (y, [dperm, nd+1]);
  if (! isempty (y))
    y = squeeze (y);
  endif

endfunction

function y = movfun_oncol (fcn, yclass, x, wlen, bcfcn, slcidx, C, Cpre,
                        Cpos, win, odim, sp)


  ## Process center of data
  if (! isempty (C))
    N = length ([Cpre, C, Cpos]);
    y = zeros (N, odim, yclass);

    if (! sp.apply)
      if (isrow (slcidx))
        ## For wlen = 1 or [0, 0] slcidx will be a row vector.  Some fcn
        ## will process this as a single vector (one infinitely long window)
        ## rather than in a columnwise fashion.  Force elementwise
        ## processing with arrayfun.  This will be slower, but most
        ## functions should handle as trivial case or be low-computation
        ## overhead for N = 1.
        ## FIXME:  This could be sidestepped for most internal functions by
        ##         specifying the operating dimension.  E.g.,
        ##         sum (x(slcidx), 2).  That would require establishing
        ##         separate code paths for internal and general movfun
        ##         calls.
        y(C,:) = arrayfun (fcn, x(slcidx));

      else
        try
          y(C,:) = fcn (x(slcidx));
        catch err
          ## Operation failed, likely due to out-of-memory error for x(slcidx)
          if (! strcmp (err.identifier, "Octave:bad-alloc"))
            rethrow (err);
          endif

          ## Try divide and conquer approach with smaller slices of data.
          ## For loops are slow, so don't try too hard with this approach.
          N_SLICES = 8;  # configurable
          idx = fix (linspace (1, numel (C), N_SLICES));
          for ii = 1 : N_SLICES-1
            y(C(idx(ii):idx(ii+1)), :) = ...
                                    fcn (x(slcidx(:, idx(ii):idx(ii+1))));
          endfor
        end_try_catch
      endif

    else
      if (sp.uniform)
        try
          k = slcidx(1,C) + (0 : diff (slcidx(:, C(1)), 1, 1)).';
          y(C,:) = fcn (x(k));

        catch err
          ## Operation failed, likely due to out-of-memory error on slcidx
          ## expansion with linspace.
          if (! strcmp (err.identifier, "Octave:bad-alloc"))
            rethrow (err);
          endif

          ## Try divide and conquer approach with smaller slices of data.
          N_SLICES = 8;  # configurable
          idx = fix (linspace (1, numel (C), N_SLICES));
          num_elems = diff (slcidx(:, C(1)), 1);
          for ii = 1 : N_SLICES-1
            k = slcidx(1, C(idx(ii):idx(ii+1))) + (0:num_elems).';
            y(C(idx(ii):idx(ii+1)), :) = fcn (x(k));
          endfor

        end_try_catch
      else
        ## Non-uniform spacing creates windows with varying number of
        ## elements that cannot be simply vectorized.

        ## Simple loop over all columns:
        ##     for ii = C
        ##       k = slcidx(1,ii):slcidx(2,ii);
        ##       y(ii,:) = fcn (x(k));
        ##     endfor
        ##
        ## is faster than arrayfun:
        ##     y(C,:) = arrayfun (@(k) fcn (x(slcidx(1,k) : slcidx(2,k))), C);
        ##
        ## Precalculating slices with equal element count is faster when
        ## numel(C) is not small (e.g., n>10, 2 groups), and the penalty when
        ## numel(C) is small is low (< 5%).
        elem_cnt = diff (slcidx(:, C), 1, 1); # Elements in each slice
        elem_cnt_srt = sort (elem_cnt); # Sorted counts to build groups
        unq_cnt_idx = logical ([1, diff(elem_cnt_srt)]); # Location of first count
        unq_cnts = elem_cnt_srt(unq_cnt_idx); # List of unique element counts
        elem_cnt_grp_idx = elem_cnt == unq_cnts.'; # Logical group index by row

        for ii = 1 : numel (unq_cnts)
          grp_cols = C(elem_cnt_grp_idx(ii, :));

          try
            slc_grp = slcidx(1, grp_cols) + (0:unq_cnts(ii)).';
            y(grp_cols, :) = fcn (x(slc_grp));
          catch err
            ## Operation failed, likely due to out-of-memory error on slcidx
            ## expansion with linspace.
            if (! strcmp (err.identifier, "Octave:bad-alloc"))
              rethrow (err);
            endif

            ## Try divide and conquer approach with smaller slices of data.
            N_SLICES = 8;
            idx = fix (linspace (1, numel (grp_cols), N_SLICES));
            for jj = 1 : N_SLICES-1
              slc_grp = slcidx(1, grp_cols(idx(jj):idx(jj+1))) + ...
                           (0:unq_cnts(ii)).';
              y(grp_cols(idx(jj):idx(jj+1)), :) = fcn (x(slc_grp));
            endfor
          end_try_catch
        endfor
      endif
    endif

  else # empty C
    ## Large windows may create Cpre/Cpos overlap and empty C.
    N = length (unique_endpoints (Cpre, Cpos));
    y = zeros (N, odim, yclass);
  endif

  ## Process boundaries
  if (! (isempty (Cpre) && isempty (Cpos)))
    Cbc = unique_endpoints (Cpre, Cpos);
    y(Cbc, :) = bcfcn (fcn, x, Cpre, Cpos, win, wlen, odim, slcidx, sp);
  endif

endfunction


## Apply "shrink" boundary conditions
## Function is not applied to any window elements outside the original data.
function y = shrink_bc (fcn, x, Cpre, Cpos, win, ~, odim, idx, sp);

  Cp_unique = unique_endpoints (Cpre, Cpos);
  N = length (x);
  n = length (Cp_unique);

  if (! sp.apply)
    idx = Cp_unique + win;
    tf  = (idx > 0) & (idx <= N);  # find idx inside boundaries

    ## FIXME: This nested for loop accounts for 70% of running time.
    ##        Given that "shrink" is the default Endpoint value this
    ##        code needs to be reworked.
    ##
    ##        Some built-in functions like sum, min, max, etc., already use
    ##        specific fill values in place of "shrink" to allow vectorized
    ##        processing (e.g., replacing "shrink" with
    ##        "'endpoints', 0" for @sum or "'endpoints',-Inf" for @max), but
    ##        such assumptions can't be made for movfun with arbitrary fcn.
    ##
    ##        Note 20-Aug-2024: The number of true elements in each column of
    ##        tf is predictable based on Cpre and Cpos. Processing in
    ##        matched groups similar to how movfun_oncol blocks non-uniform
    ##        element count groups will only reduce number of loop interations
    ##        by up to 2x for scalar wlen or similar size [nb, na]. The effort
    ##        to find and extract indices for grouped processing looks likely
    ##        to take more computation time than the simple loop below, and
    ##        it is unlikely for the cost per iteration to be much less to
    ##        create a useful breakeven point.
    y   = zeros (n, odim);
    for ii = 1:n
      k = idx(tf(:,ii),ii);
      y(ii,:) = fcn (x(k));
    endfor

  else
    ## idx from movslice already contains "tf-trimmed" windows.
    y = zeros (n, odim);

    ## Loop over all columns slightly faster than:
    ##    y = arrayfun (@(k) fcn (x(idx(1, k):idx(2,k))), Cp_unique);
    ##
    ## Precalculating equal element-count groups is only slightly faster than
    ## and scales similarly as simple loop:
    ##    for ii = 1:n
    ##      k = idx(1, Cp_unique(ii)) : idx(2, Cp_unique(ii));
    ##      y(ii,:) = fcn (x(k));
    ##    endfor

    elem_cnt = diff (idx(:, Cp_unique)); # Elements in each slice
    elem_cnt_srt = sort (elem_cnt); # Sorted counts to build groups
    unq_cnt_idx = logical ([1, diff(elem_cnt_srt)]); # Location of first count
    unq_cnts = elem_cnt_srt(unq_cnt_idx); # List of unique element counts
    elem_cnt_grp_idx = elem_cnt == unq_cnts.'; # Logical group index by row

    for ii = 1 : numel (unq_cnts)
      col_grp_idx = elem_cnt_grp_idx(ii, :);
      k = idx(1, Cp_unique(col_grp_idx)) + (0:unq_cnts(ii)).';
      y(col_grp_idx,:) = fcn (x(k));
    endfor
  endif

endfunction

## Apply replacement value boundary conditions
## Window is padded at beginning and end with user-specified value.
function y = replaceval_bc (fcn, x, Cpre, Cpos, win, wlen, ~, idx, sp)

  persistent substitute;

  ## In-band method to initialize substitute value
  if (islogical (fcn))
    substitute = x;
    return;
  endif


  if (! sp.apply)

    idx = unique_endpoints (Cpre, Cpos) + win;

    if (! isempty (Cpre))
      ## pre-pad window, check if also post-pad
      sz = size (x);
      sz(1) = wlen(1);
      x = [substitute(ones (sz)); x];
      idx = idx + wlen(1);

      N = numel (x);
      if (idx(end) > N)
        sz(1) = idx(end) - N;
        x = [x; substitute(ones (sz))];
      endif

    else
      ## only post-pad window
      sz = size (x);
      sz(1) = wlen(2);
      x = [x; substitute(ones (sz))];
    endif

  else

    Cp_unique = unique_endpoints (Cpre, Cpos);
    sz = size (x);

    if (! isempty (Cpre))
      sz(1) = numel ([sp.samplepoints(1) : -sp.spacing : win(1)]) - 1;
      x =  [substitute(ones (sz)); x];
      idx += sz(1);
      idx(1, Cpre) -= sz(1) + 1 - Cpre;
      if (! isempty (Cpos))
        pts = [sp.samplepoints(end) : sp.spacing : win(end)];
        exclude_endpoint = pts(end) == sp.samplepoints(end) + wlen(2);
        sz(1) = numel (pts) - 1 - (sp.scalar_wlen && exclude_endpoint);
        idx(2, Cpos) += sz(1) + 1 - (numel(Cpos):-1:1);
        x =  [x; substitute(ones (sz))];
      endif
    else
      ## only post-pad window
      pts = [sp.samplepoints(end) : sp.spacing : win(end)];
      exclude_endpoint = pts(end) == sp.samplepoints(end) + wlen(2);
      sz(1) = numel (pts) - 1 - (sp.scalar_wlen && exclude_endpoint);
      idx(2, Cpos) += sz(1) + 1 - (numel(Cpos):-1:1);
      x =  [x; substitute(ones (sz))];
    endif

    elems = diff (idx(:, Cp_unique(1)), 1, 1);
    idx = idx(1, Cp_unique) + (0:elems).';
  endif

  y = fcn (x(idx));

endfunction

## Apply "same" boundary conditions
## 'y' values outside window are replaced by value of 'x' at the window
## boundary.
function y = same_bc (fcn, x, Cpre, Cpos, win, wlen, ~, idx, sp)

  N = length (x);

  if (! sp.apply)
    idx = unique_endpoints (Cpre, Cpos) + win;

  else
    sz_pre = numel ([sp.samplepoints(1) : -sp.spacing : win(1)]) - 1;

    pts_post = [sp.samplepoints(end) : sp.spacing : win(end)];
    exclude_endpoint = pts_post(end) == sp.samplepoints(end) + wlen(2);
    sz_post = numel (pts_post) - 1 - (sp.scalar_wlen && exclude_endpoint);

    idx(2, Cpos) += sz_post - (numel(Cpos)-1:-1:0);
    idx = double (idx(:, unique_endpoints (Cpre, Cpos))); ## Allow neg values
    idx(1, Cpre) -= sz_pre - (0 : numel (Cpre) - 1);

    elems = diff(idx(:, 1), 1, 1);
    idx = idx(1,:) + (0:elems).';
  endif

    idx(idx < 1) = 1;
    idx(idx > N) = N;
    y = fcn (x(idx));

endfunction

## Apply "periodic" boundary conditions
## Window wraps around.  Window values outside data array are replaced with
## data from the other end of the array.
function y = periodic_bc (fcn, x, Cpre, Cpos, win, wlen, ~, idx, sp)

  N   = length (x);

  if (! sp.apply)
    idx = unique_endpoints (Cpre, Cpos) + win;

  else
    sz_pre = numel ([sp.samplepoints(1) : -sp.spacing : win(1)]) - 1;

    pts_post = [sp.samplepoints(end) : sp.spacing : win(end)];
    exclude_endpoint = pts_post(end) == sp.samplepoints(end) + wlen(2);
    sz_post = numel (pts_post) - 1 - (sp.scalar_wlen && exclude_endpoint);

    idx(2, Cpos) += sz_post + 1 - (numel(Cpos):-1:1);
    idx = double (idx(:, unique_endpoints (Cpre, Cpos)));
    idx(1, Cpre) -= sz_pre + 1 - Cpre;

    elems = diff(idx(:, 1), 1, 1);
    idx = idx(1,:) + (0:elems).';
  endif

  tfpre =  idx < 1;
  tfpos =  idx > N;

  premult = ceil ((1 - idx(tfpre))/N);
  postmult = floor ((N - idx(tfpos))/N);

  idx(tfpre) += N*premult;
  idx(tfpos) += N*postmult;

  y = fcn (x(idx));

endfunction

## Faster unique routine to remove potential overlapping endpoint indices.
## Assumes a and b are vectors of valid, sorted linear indices. Either may be
## empty.  a will have the form [1:M], b will have the form [N:P].  M must be
## no larger than P, and N must be no smaller than 1.
function c = unique_endpoints (a, b)
  if (isempty (a))
    c = b;
  else
    notshared = cummax (b == a(end));
    if (any (notshared))
      c = [a(1:end-1), b(notshared)];
    else
      c = [a, b];
    endif
  endif
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

## Clear shared variables for remaining tests to reduce clutter on failure.
%!shared

## Test for correct return class based on output of function.
%!test <*63802>
%! x = single (1:10);
%! y = movfun (@mean, x, 3);
%! assert (class (y), 'single');
%! y = movfun (@mean, uint8 (x), 3);
%! assert (class (y), 'double');

## Test calculation along empty dimension
%!assert <*63802> (movfun (@mean, zeros (2, 0, 3, "single"), 3, 'dim', 2),
%!                 zeros (2,0,3, "single"))
%!assert <*66025> (movfun (@mean, zeros (2, 0, 3, "double"), 3, 'dim', 2),
%!                 zeros (2,0,3, "double"))
%!assert <*66025> (movfun (@mean, zeros (2, 0, 3, "uint8"), 3, 'dim', 2),
%!                 zeros (2,0,3, "uint8"))
%!assert <*66025> (movfun (@mean, zeros (2, 0, 3, "logical"), 3, 'dim', 2),
%!                 zeros (2,0,3, "logical"))
%!assert <*66025> (movfun (@mean, cell (2, 0, 3), 3, 'dim', 2),
%!                 zeros (2,0,3, "double"))
%!assert <*66025> (movfun (@mean, "", 3, 'dim', 2), "")
%!assert <*66025> (movfun (@mean, '', 3, 'dim', 2), '')

## Test for correct output shape for dim > 2 and ndims > 2
%!test <*65927>
%! a = reshape (1:30, 5, 3, 2);
%! b1 = cat (3, [1, 6, 11], [16, 21, 26]) + [0, 0.5, 1.5, 2.5, 3.5].';
%! b2 = cat (3, [1:5].', [16:20].') + [0, 2.5, 7.5];
%! b3 = cat (3, [1:5].', [8.5:1:12.5].') + [0, 5, 10];
%! assert (movfun (@mean, a, 2), b1, eps);
%! assert (movfun (@mean, a, 2, 'dim', 1), b1, eps);
%! assert (movfun (@mean, a, 2, 'dim', 2), b2, eps);
%! assert (movfun (@mean, a, 2, 'dim', 3), b3, eps);
%!
%! a2 = repmat (a, 1, 1, 1, 4);
%! assert (size (movfun (@mean, a2, 2)), [5, 3, 2, 4]);
%! assert (size (movfun (@mean, a2, 2, 'dim', 1)), [5, 3, 2, 4]);
%! assert (size (movfun (@mean, a2, 2, 'dim', 2)), [5, 3, 2, 4]);
%! assert (size (movfun (@mean, a2, 2, 'dim', 3)), [5, 3, 2, 4]);
%! assert (size (movfun (@mean, a2, 2, 'dim', 4)), [5, 3, 2, 4]);

## Test for wlen = 1 or window length > size (x, dim)
%!assert <*65928> (movfun (@sum, 1:10, 1), 1:10)
%!assert <*65928> (movfun (@sum, 1:10, [0, 0]), 1:10)
%!assert <*65928> (movfun (@sum, 1:10, 10), ...
%!                 [15, 21, 28, 36, 45, 55, 54, 52, 49, 45])
%!assert <*65928> (movfun (@sum, 1:10, 11),
%!                 [21, 28, 36, 45, 55, 55, 54, 52, 49, 45])
%!assert <*65928> (movfun (@sum, 1:10, 12),
%!                 [21, 28, 36, 45, 55, 55, 55, 54, 52, 49])
%!assert <*65928> (movfun (@sum, 1:10, 99), 55(ones (1, 10)))
%!assert <*65928> (movfun (@sum, 1:10, [9, 8]), [45, 55(ones (1, 9))])

## Test different values of dim
%!assert (movfun (@sum, 1:5, 3), [3, 6, 9, 12, 9])
%!assert (movfun (@sum, 1:5, 3, "dim", 2), [3, 6, 9, 12, 9])
%!assert <*65928> (movfun (@sum, 1:5, 3, "dim", 1), 1:5)
%!assert <*65928> (movfun (@sum, 1:5, 3, "dim", 3), 1:5)

%!assert (movfun (@sum, magic (3), 3), [11, 6, 13; 15, 15, 15; 7, 14, 9])
%!assert (movfun (@sum, magic (3), 3, "dim", 1), [11, 6, 13; 15, 15, 15; 7, 14, 9])
%!assert (movfun (@sum, magic (3), 3, "dim", 2), [9, 15, 7; 8, 15, 12; 13, 15, 11])
%!assert <*65928> (movfun (@sum, magic (3), 3, "dim", 3), magic (3))

## Test endpoint options with window lengths exceeding size (x, dim)
%!assert <*65928> (movfun (@sum, 1:5, 20, "endpoints", "shrink"), 15(ones (1, 5)))
%!assert <*65928> (movfun (@sum, 1:5, 3, "endpoints", "shrink", "dim", 1), 1:5)
%!assert <*65928> (movfun (@sum, 1:5, 3, "endpoints", "shrink", "dim", 3), 1:5)
%!assert <*65928> (movfun (@sum, 1:5, 20, "endpoints", "same"), 50:4:66)
%!assert <*65928> (movfun (@sum, 1:5, 3, "endpoints", "same", "dim", 1), 3:3:15)
%!assert <*65928> (movfun (@sum, 1:5, 3, "endpoints", "same", "dim", 3), 3:3:15)
%!assert <*65928> (movfun (@sum, 1:5, 20, "endpoints", "periodic"), 60(ones (1, 5)))
%!assert <*65928> (movfun (@sum, 1:5, 3, "endpoints", "periodic", "dim", 1), 3:3:15)
%!assert <*65928> (movfun (@sum, 1:5, 3, "endpoints", "periodic", "dim", 3), 3:3:15)
%!assert <*65928> (movfun (@sum, 1:5, 20, "endpoints", 1), 30(ones (1, 5)))
%!assert <*65928> (movfun (@sum, 1:5, 3, "endpoints", 1, "dim", 1), 3:7)
%!assert <*65928> (movfun (@sum, 1:5, 3, "endpoints", 1, "dim", 3), 3:7)
%!assert <*65928> (movfun (@sum, 1:5, 20, "endpoints", "discard"), NaN (1, 0))
%!assert <*65928> (movfun (@sum, 1:5, 3, "endpoints", "discard", "dim", 1), NaN (0, 5))
%!assert <*65928> (movfun (@sum, 1:5, 3, "endpoints", "discard", "dim", 3), NaN (1, 5, 0))
%!assert <*65928> (movfun (@sum, 1:5, 20, "endpoints", "fill"), NaN (1, 5))
%!assert <*65928> (movfun (@sum, 1:5, 3, "endpoints", "fill", "dim", 1), NaN (1, 5))
%!assert <*65928> (movfun (@sum, 1:5, 3, "endpoints", "fill", "dim", 3), NaN (1, 5))

## Samplepoint tests
## standard spacing
%!assert <*66025> (movfun (@sum, 1:10, 4, "samplepoints", 1:10), movfun (@sum, 1:10, 4))
%!assert <*66025> (movfun (@sum, 1:10, 5, "samplepoints", 1:10), movfun (@sum, 1:10, 5))
%!assert <*66025> (movfun (@sum, 1:10, 4.1, "samplepoints", 1:10), movfun (@sum, 1:10, 4.1))
%!assert <*66025> (movfun (@sum, 1:10, [2, 3], "samplepoints", 1:10), movfun (@sum, 1:10, [2, 3]))
%!assert <*66025> (movfun (@sum, 1:10, 60, "samplepoints", 1:10), movfun (@sum, 1:10, 60))
%!assert <*66025> (movfun (@sum, 1:10, [30, 30], "samplepoints", 1:10), movfun (@sum, 1:10, [30, 30]))

## nonstandard, uniform spacing
%!assert <*66025> (movfun (@sum, 1:10, 4, "samplepoints", 0.5:0.5:5), [10, 15, 21, 28, 36, 44, 52, 49, 45, 40])
%!assert <*66025> (movfun (@sum, 1:10, 5, "samplepoints", 0.5:0.5:5), [15, 21, 28, 36, 45, 55, 54, 52, 49, 45])
%!assert <*66025> (movfun (@sum, 1:10, 4.1, "samplepoints", 0.5:0.5:5), [15, 21, 28, 36, 45, 54, 52, 49, 45, 40])
%!assert <*66025> (movfun (@sum, 1:10, [2, 2], "samplepoints", 0.5:0.5:5), [15, 21, 28, 36, 45, 54, 52, 49, 45, 40])
%!assert <*66025> (movfun (@sum, 1:10, 2.5, "samplepoints", 0.25:0.25:2.5), [15, 21, 28, 36, 45, 55, 54, 52, 49, 45])
%!assert <*66025> (movfun (@sum, 1:10, [1.25, 1.25], "samplepoints", 0.25:0.25:2.5), [21, 28, 36, 45, 55, 55, 54, 52, 49, 45])
%!assert <*66025> (movfun (@sum, 1:10, 60, "samplepoints", 0.5:0.5:5), 55(ones(1, 10)))
%!assert <*66025> (movfun (@sum, 1:10, [30, 30], "samplepoints", 0.5:0.5:5), 55(ones(1, 10)))

## nonstandard, nonuniform spacing
%!assert <*66025> (movfun (@sum, 1:10, 4, "samplepoints", [1:9, 11]), [3, 6, 10, 14, 18, 22, 26, 30, 24, 19])
%!assert <*66025> (movfun (@sum, 1:10, 5, "samplepoints", [1:9, 11]), [6, 10, 15, 20, 25, 30, 35, 30, 34, 19])
%!assert <*66025> (movfun (@sum, 1:10, 4.1, "samplepoints", [1:9, 11]), [6, 10, 15, 20, 25, 30, 35, 30, 34, 19])
%!assert <*66025> (movfun (@sum, 1:10, [2, 2], "samplepoints", [1:9, 11]), [6, 10, 15, 20, 25, 30, 35, 30, 34, 19])
%!assert <*66025> (movfun (@sum, 1:10, 60, "samplepoints", [1:9, 11]), 55(ones(1, 10)))
%!assert <*66025> (movfun (@sum, 1:10, [30, 30], "samplepoints", [1:9, 11]), 55(ones(1, 10)))

## endpoints other than "shrink"
## constant
%!assert <*66025> (movfun (@sum, 10:10:100, 4, "samplepoints", 0.5:0.5:5, "endpoints", 1), [104, 153, 212, 281, 360, 440, 520, 491, 452, 403])
%!assert <*66025> (movfun (@sum, 10:10:100, 5, "samplepoints", 0.5:0.5:5, "endpoints", 1), [155, 214, 283, 362, 451, 550, 541, 522, 493, 454])
%!assert <*66025> (movfun (@sum, 10:10:100, 5, "samplepoints", 2:2:20, "endpoints", 1), [31, 60:30:270, 191])
%!assert <*66025> (movfun (@sum, 10:10:100, [2, 2], "samplepoints", 0.5:0.5:5, "endpoints", 1), [154, 213, 282, 361, 450, 540, 521, 492, 453, 404])
%!assert <*66025> (movfun (@sum, 10:10:100, [0, 2.5], "samplepoints", 2:2:20, "endpoints", 1), [30:20:190, 101])
%!assert <*66025> (movfun (@sum, 10:10:100, 40, "samplepoints", 0.5:0.5:5, "endpoints", 1), 620(ones (1, 10)))
%!assert <*66025> (movfun (@sum, 10:10:100, [20, 20], "samplepoints", 0.5:0.5:5, "endpoints", 1), 621(ones (1, 10)))

## fill
%!assert <*66025> (movfun (@sum, 1:10, 4, "samplepoints", 0.5:0.5:5, "endpoints", "fill"), [NaN(1, 4), 36, 44, 52, NaN(1, 3)])
%!assert <*66025> (movfun (@sum, 1:10, 5, "samplepoints", 0.5:0.5:5, "endpoints", "fill"), [NaN(1, 5), 55, NaN(1, 4)])
%!assert <*66025> (movfun (@sum, 1:10, 5, "samplepoints", 2:2:20, "endpoints", "fill"), [NaN, 6:3:27,NaN])
%!assert <*66025> (movfun (@sum, 1:10, [2, 2], "samplepoints", 0.5:0.5:5, "endpoints", "fill"), [NaN(1, 4), 45, 54, NaN(1, 4)])
%!assert <*66025> (movfun (@sum, 1:10, [0, 2.5], "samplepoints", 2:2:20, "endpoints", "fill"), [3:2:19, NaN])
%!assert <*66025> (movfun (@sum, 1:10, 40, "samplepoints", 0.5:0.5:5, "endpoints", "fill"), NaN(1, 10))
%!assert <*66025> (movfun (@sum, 1:10, [20, 20], "samplepoints", 0.5:0.5:5, "endpoints", "fill"), NaN(1, 10))

## discard
%!assert <*66025> (movfun (@sum, 1:10, 4,'samplepoints', 0.5:0.5:5, 'endpoints', 'discard'), [36, 44, 52])
%!assert <*66025> (movfun (@sum, 1:10, 5,'samplepoints', 0.5:0.5:5, 'endpoints', 'discard'), 55)
%!assert <*66025> (movfun (@sum, 1:10, [2, 2],'samplepoints', 0.5:0.5:5, 'endpoints', 'discard'), [45, 54])
%!assert <*66025> (movfun (@sum, 1:10, 40,'samplepoints', 0.5:0.5:5, 'endpoints', 'discard'), NaN(1, 0))
%!assert <*66025> (movfun (@sum, 1:10, [20, 20],'samplepoints', 0.5:0.5:5, 'endpoints', 'discard'), NaN(1, 0))

## same
%!assert <*66025> (movfun (@sum, 1:10, 4,'samplepoints', 0.5:0.5:5, 'endpoints', 'same'), [14, 18, 23, 29, 36, 44, 52, 59, 65, 70])
%!assert <*66025> (movfun (@sum, 1:10, 5,'samplepoints', 0.5:0.5:5, 'endpoints', 'same'), [20, 25, 31, 38, 46, 55, 64, 72, 79, 85])
%!assert <*66025> (movfun (@sum, 1:10, 5,'samplepoints', 2:2:20, 'endpoints', 'same'), [4, 6:3:27, 29])
%!assert <*66025> (movfun (@sum, 1:10, [2, 2],'samplepoints', 0.5:0.5:5, 'endpoints', 'same'), [19, 24, 30, 37, 45, 54, 62, 69, 75, 80])
%!assert <*66025> (movfun (@sum, 1:10, [0, 2.5],'samplepoints', 2:2:20, 'endpoints', 'same'), [3:2:19, 20])
%!assert <*66025> (movfun (@sum, 1:10, 40,'samplepoints', 0.5:0.5:5, 'endpoints', 'same'), [395, 404, 413, 422, 431, 440, 449, 458, 467, 476])
%!assert <*66025> (movfun (@sum, 1:10, [20, 20],'samplepoints', 0.5:0.5:5, 'endpoints', 'same'), [405, 414, 423, 432, 441, 450, 459, 468, 477, 486])

## periodic
%!assert <*66025> (movfun (@sum, 1:10, 4,'samplepoints', 0.5:0.5:5, 'endpoints', 'periodic'), [44, 42, 40, 38, 36, 44, 52, 50, 48, 46])
%!assert <*66025> (movfun (@sum, 1:10, 5,'samplepoints', 0.5:0.5:5, 'endpoints', 'periodic'), 55(ones (1, 10)))
%!assert <*66025> (movfun (@sum, 1:10, 5,'samplepoints', 2:2:20, 'endpoints', 'periodic'), [13, 6:3:27, 20])
%!assert <*66025> (movfun (@sum, 1:10, [2, 2],'samplepoints', 0.5:0.5:5, 'endpoints', 'periodic'), [49, 48, 47, 46, 45, 54, 53, 52, 51, 50])
%!assert <*66025> (movfun (@sum, 1:10, [0, 2.5],'samplepoints', 2:2:20, 'endpoints', 'periodic'), [3:2:19,11])
%!assert <*66025> (movfun (@sum, 1:10, 40,'samplepoints', 0.5:0.5:5, 'endpoints', 'periodic'), 440(ones (1, 10)))
%!assert <*66025> (movfun (@sum, 1:10, [20, 20],'samplepoints', 0.5:0.5:5, 'endpoints', 'periodic'), 441:450)

## endpoints with multicolumn fcn output
%!shared fcn
%! fcn = @(y) [sum(y), max(y)];

%!assert <*66025> (movfun (fcn, 1:10, 5), [6, 10:5:40, 34, 27; 3:10, 10, 10].')
%!assert <*66025> (movfun (fcn, 1:10, 5, "endpoints", "shrink"), [6, 10:5:40, 34, 27; 3:10, 10, 10].')
%!assert <*66025> (movfun (fcn, 1:10, 5, "endpoints", "discard"), [15:5:40; 5:10].')
%!assert <*66025> (movfun (fcn, 1:10, 5, "endpoints", "fill"), [NaN, NaN, 15:5:40, NaN, NaN; 3:10, 10, 10].')
%!assert <*66025> (movfun (fcn, 1:10, 5, "endpoints", 5), [16, 15, 15:5:40, 39, 37; 5, 5, 5:10, 10, 10].')
%!assert <*66025> (movfun (fcn, 1:10, 5, "endpoints", "same"), [8, 11, 15:5:40, 44, 47; 3:10, 10, 10].')
%!assert <*66025> (movfun (fcn, 1:10, 5, "endpoints", "periodic"), [25, 20, 15:5:40, 35, 30; 10, 10, 5:10, 10, 10].')

## uniform samplepoints
%!assert <*66025> (movfun (fcn, 1:10, 5, "endpoints", "shrink", "samplepoints", 2:2:20), [3:3:27, 19; 2:10, 10].')
%!assert <*66025> (movfun (fcn, 1:10, 5, "endpoints", "discard", "samplepoints", 2:2:20), [6:3:27; 3:10].')
%!assert <*66025> (movfun (fcn, 1:10, 5, "endpoints", "fill", "samplepoints", 2:2:20), [NaN, 6:3:27, NaN; 2:10, 10].')
%!assert <*66025> (movfun (fcn, 1:10, 5, "endpoints", 5, "samplepoints", 2:2:20), [8, 6:3:27, 24; 5, 3:10, 10].')
%!assert <*66025> (movfun (fcn, 1:10, 5, "endpoints", "same", "samplepoints", 2:2:20), [4, 6:3:27, 29; 2:10, 10].')
%!assert <*66025> (movfun (fcn, 1:10, 5, "endpoints", "periodic", "samplepoints", 2:2:20), [13, 6:3:27, 20; 10, 3:10, 10].')

## non-uniform samplepoints
%!assert <*66025> (movfun (fcn, 1:10, 5, "samplepoints", [1:9, 11]), [6, 10:5:35, 30, 34, 19; 3:9, 9, 10, 10].')
%!assert <*66025> (movfun (fcn, 1:10, 5, "endpoints", "shrink", "samplepoints", [1:9, 11]), [6, 10:5:35, 30, 34, 19; 3:9, 9, 10, 10].')

## Clear shared variables for remaining tests to reduce clutter on failure.
%!shared

## verify includenan and includemissing function
%!assert <*66025> (movfun (@sum, 1:10, 5, "nancond", "includenan"), movfun (@sum, 1:10, 5))
%!assert <*66025> (movfun (@sum, 1:10, 5, "nancond", "includemissing"), movfun (@sum, 1:10, 5))


## Test input validation
%!error <Invalid call> movfun ()
%!error <Invalid call> movfun (@min)
%!error <Invalid call> movfun (@min, 1:5)

%!error <FCN must be a valid function handle> movfun (1, 1:10, 3)
%!error <FCN must be a valid function handle> movfun (true, 1:10, 3)
%!error <FCN must be a valid function handle> movfun ({"foo"}, 1:10, 3)
%!error <FCN must be a valid function handle> movfun (struct("a", "b"), 1:10, 3)
%!error <WLEN must be numeric> movfun (@sum, 1:10, 'f')
%!error <WLEN must be numeric> movfun (@sum, 1:10, {1, 2})
%!error <Each PROPERTY must have a VALUE> movfun (@sum, 1:10, 3, "EndPoints")
%!error <Each PROPERTY must have a VALUE> movfun (@sum, 1:10, 3, "EndPoints", 3, "dim")
%!error <Each PROPERTY must have a VALUE> movfun (@sum, 1:10, 3, "EndPoints", "dim", 2)
%!error <PROPERTY name must be> movfun (@sum, 1:10, 3, 123, 3)
%!error <PROPERTY name must be> movfun (@sum, 1:10, 3, true, 3)
%!error <PROPERTY name must be> movfun (@sum, 1:10, 3, {"foo"}, 3)
%!error <PROPERTY name must be> movfun (@sum, 1:10, 3, struct (), 3)
%!error <PROPERTY name must be> movfun (@sum, 1:10, 3, ["foo"; "bar"], 3)
%!error <unknown PROPERTY 'foo'> movfun (@sum, 1:10, 3, "foo", 3)
%!error <DIM must be a > movfun (@sum, 1:10, 3, "dim", -1)
%!error <DIM must be a > movfun (@sum, 1:10, 3, "dim", 0)
%!error <DIM must be a > movfun (@sum, 1:10, 3, "dim", 1.5)
%!error <DIM must be a > movfun (@sum, 1:10, 3, "dim", {1})
%!error <DIM must be a > movfun (@sum, 1:10, 3, "dim", true)
%!error <DIM must be a > movfun (@sum, 1:10, 3, "dim", "c")
%!error <DIM must be a > movfun (@sum, 1:10, 3, "dim", [1, 2])
%!error <ENDPOINTS must be a > movfun (@sum, 1:10, 3, "endpoints", [1 2])
%!error <ENDPOINTS must be a > movfun (@sum, 1:10, 3, "endpoints", {1})
%!error <ENDPOINTS must be a > movfun (@sum, 1:10, 3, "endpoints", true)
%!error <ENDPOINTS must be a > movfun (@sum, 1:10, 3, "endpoints", "foo")
%!error <ENDPOINTS must be a > movfun (@sum, 1:10, 3, "endpoints", {"shrink"})
%!error <NANCOND must be> movfun (@sum, 1:10, 3, "nancond", 3)
%!error <NANCOND must be> movfun (@sum, 1:10, 3, "nancond", "foo")
%!error <NANCOND must be> movfun (@sum, 1:10, 3, "nancond", {"includenan"})
%!error <OUTDIM must be a> movfun (@sum, 1:10, 3, "outdim", -1)
%!error <OUTDIM must be a> movfun (@sum, 1:10, 3, "outdim", 0)
%!error <OUTDIM must be a> movfun (@sum, 1:10, 3, "outdim", [1 -1])
%!error <OUTDIM must be a> movfun (@sum, 1:10, 3, "outdim", 1.5)
%!error <OUTDIM must be a> movfun (@sum, 1:10, 3, "outdim", "a")
%!error <OUTDIM must be a> movfun (@sum, 1:10, 3, "outdim", {1})
%!error <OUTDIM must be a> movfun (@sum, 1:10, 3, "outdim", true)
%!error <SAMPLEPOINTS must be a> movfun (@sum, [1, 2, 3], 3, "samplepoints", "foo")
%!error <SAMPLEPOINTS must be a> movfun (@sum, [1, 2, 3], 3, "samplepoints", [1, 1, 3])
%!error <SAMPLEPOINTS must be a> movfun (@sum, [1, 2, 3], 3, "samplepoints", [1, 3, 2])
%!error <SAMPLEPOINTS must be a> movfun (@sum, [1, 2, 3], 3, "samplepoints", {1, 2, 3})
%!error <SAMPLEPOINTS must be a> movfun (@sum, [1, 2, 3, 4], 3, "samplepoints", [1 3; 2 4])
%!error <when SamplePoints are not uniformly spaced> movfun (@sum, 1:5, 3, "SamplePoints", [1:4, 4.5], "EndPoints", "fill")
%!error <when SamplePoints are not uniformly spaced> movfun (@sum, 1:5, 3, "SamplePoints", [1:4, 4.5], "EndPoints", 2)
%!error <SamplePoints must be the same size as x> movfun (@sum, 1:5, 3, "SamplePoints", 1:4)
%!error <SamplePoints must be the same size as x> movfun (@sum, magic (4), 3, "dim", 2, "SamplePoints", [1, 2])
%!warning <"omitnan" is not yet implemented>
%! movfun (@min, 1:3, 3, "nancond", "omitnan");
%!warning <"omitnan" is not yet implemented>
%! movfun (@min, 1:3, 3, "nancond", "omitmissing");
%!error movfun (@min, 1:3, "nancond", "omitnan");
## FIXME: This test is commented out until OUTDIM validation is clarified.
%!#error <OUTDIM \(5\) is larger than largest available dimension \(3\)>
%! movfun (@min, ones (6,3,4), 3, "outdim", 5);

