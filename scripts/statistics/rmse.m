########################################################################
##
## Copyright (C) 2025 The Octave Project Developers
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
## @deftypefn  {} {@var{E} =} rmse (@var{F}, @var{A})
## @deftypefnx {} {@var{E} =} rmse (@var{F}, @var{A}, @var{dim})
## @deftypefnx {} {@var{E} =} rmse (@var{F}, @var{A}, @var{vecdim})
## @deftypefnx {} {@var{E} =} rmse (@var{F}, @var{A}, "all")
## @deftypefnx {} {@var{E} =} rmse (@dots{}, @var{nanflag})
## @deftypefnx {} {@var{E} =} rmse (@dots{}, @qcode{'Weights'}, @var{W})
## Compute the root mean squared error between arrays.
##
## The root mean squared error is defined as
## @tex
## $$ {\rm rmse}(F, A) = {E} = \sqrt{{1\over N} \sum_{i=1}^N {\left| {A_i - F_i} \right|}^2} $$
## where $N$ is the number of elements in @var{F} and @var{A} after
## broadcasting is applied to the subtraction operation.
## @end tex
## @ifnottex
##
## @example
## rmse (@var{F}, @var{A}) = sqrt (SUM_i ((@var{A}(i) - @var{F}(i)) ^ 2) / N)
## @end example
##
## @noindent
## where @math{N} is the number of elements in @var{F} and @var{A} after
## broadcasting is applied to the subtraction operation.
##
## @end ifnottex
##
## The weighted root mean squared error is defined as
## @tex
## $$ {\rm rmse}(F, A) = {E_W} = \sqrt{\sum_{i=1}^N W_i{\left| {A_i - F_i} \right|}^2 \over \sum_{i=1}^N W_i} $$
## where $N$ is the number of elements in @var{F} and @var{A} after
## broadcasting is applied to the subtraction operation.
## @end tex
## @ifnottex
##
## @example
## weighted_rmse (@var{F}, @var{A}) = sqrt (SUM_i (@var{W}(i) * ((@var{A}(i) - @var{F}(i)) ^ 2)) / SUM_i (@var{W}(i)))
## @end example
##
## @noindent
## where @math{N} is the number of elements in @var{F} and @var{A} after
## broadcasting is applied to the subtraction operation.
##
## @end ifnottex
##
## @var{F} and @var{A} must either be the same size or have compatible sizes.
##
## If @var{F} and @var{A} are vectors of the same size, then
## @code{rmse (@var{F}, @var{A})} returns a scalar with the RMSE between the
## elements of @var{F} and @var{A}.
##
## If @code{@var{A} - @var{F}} is a matrix, then @code{rmse (@var{F}, @var{A})}
## returns a row vector with each element containing the RMSE between the
## corresponding columns of @code{@var{A} - @var{F}}.
##
## If @code{@var{A} - @var{F}} is an array, then @code{rmse (@var{F}, @var{A})}
## computes the RMSE along the first non-singleton dimension of the difference
## between the input arrays @var{F} and @var{A}.  The size of @var{E} along the
## operating dimension is 1, while all other dimensions are the same as in
## @code{@var{A} - @var{F}}.
##
## The optional input @var{dim} specifies the dimension to operate on and must
## be a positive integer.  Specifying any singleton dimension, including any
## dimension exceeding @code{ndims (@var{A} - @var{F})}, will return
## @code{abs ((@var{A} - @var{F}) ./ @var{A})}.
##
## Specifying the dimensions as @var{vecdim}, a vector of non-repeating
## dimensions, will return the rmse over the array slice defined by
## @var{vecdim}.  If @var{vecdim} indexes all dimensions in
## @code{@var{A} - @var{F}}, then it is equivalent to the option @qcode{"all"}.
## Any dimension in @var{vecdim} greater than @code{ndims (@var{A} - @var{F})}
## is ignored.
##
## Specifying the dimension as @qcode{"all"} will cause @code{rmse} to operate
## on all elements of @code{@var{A} - @var{F}}, and is equivalent to
## @code{rmse ((@var{A} - @var{F})(:))}.
##
## The optional variable @var{nanflag} specifies whether to include or exclude
## NaN values from the calculation using any of the previously specified input
## argument combinations.  The default value for @var{nanflag} is
## @qcode{"includenan"} which keeps NaN values in the calculation.  To exclude
## NaN values set the value of @var{nanflag} to @qcode{"omitnan"}.  The output
## will still contain NaN values if @code{@var{A} - @var{F}} consists of all NaN
## values in the operating dimension.
##
## The optional paired argument @code{@dots{}, "Weights", @var{W}} specifies a
## weighting scheme @var{W}, which is applied on the difference of the input
## arrays @var{F} and @var{A}, so that @code{rmse} computes the weighted RMSE.
## When operating along a single dimension, @var{W} must be a vector of the same
## length as the operating dimension or it must have the same size as @var{x}.
## When operating over an array slice defined by @var{vecdim}, @var{W} must have
## the same size as the operating array slice, i.e.
## @code{size (@var{A} - @var{F})(@var{vecdim})}, or the same size as
## @code{@var{A} - @var{F}}.
##
## @seealso{mape, meansq, rms}
## @end deftypefn

function E = rmse (F, A, varargin)

  if (nargin < 2 || nargin > 7)
    print_usage ();
  endif

  if (! any (isa (F, {'double', 'single'})))
    error ("rmse: F must be a floating point numeric array");
  endif

  if (! any (isa (A, {'double', 'single'})))
    error ("rmse: A must be a floating point numeric array");
  endif

  ## Determine output class
  if (isa (F, 'single') || isa (A, 'single'))
    outtype = "single";
    F = double (F);
    A = double (A);
  else
    outtype = "double";
  endif

  ## Check F and A for compatible sizes
  try
    AF = A - F;
  catch
    error ("rmse: F and A must have compatible sizes");
  end_try_catch

  ## Initialize flags
  all_flag = false;
  omitnan  = false;
  weighted = false;

  ## Process paired argument for Weights
  w_idx = find (cellfun (@(x) strcmpi (x, "weights"), varargin));
  if (! isempty (w_idx))
    if (numel (varargin) > w_idx)
      W = varargin{w_idx+1};
      if (! (isnumeric (W) && any (isa (W, {'double', 'single'}))))
        error ("rmse: WEIGHTS must be single or double");
      endif
      if (any (W(:) < 0))
        error ("rmse: WEIGHTS must be nonnegative");
      endif
    else
      error ("rmse: paired input argument for 'Weights' is missing");
    endif
    weighted = true;
    varargin([w_idx, w_idx+1]) = [];
  endif

  nvarg = numel (varargin);
  varg_chars = cellfun ("ischar", varargin);
  sz_AF = size (AF);
  nd_AF = ndims (AF);

  ## Square the difference here (no reduction to AF)
  ## sumsq will handle efficiently both real and complex numbers
  AF = sumsq (AF, nd_AF + 1);

  if (nvarg > 1 && ! varg_chars(2:end))
    ## Only first varargin can be numeric
    print_usage ();
  endif

  ## Process any other char arguments.
  if (any (varg_chars))
    for argin = varargin(varg_chars)
      switch (lower (argin{:}))
        case "all"
          all_flag = true;

        case "omitnan"
          omitnan = true;

        case "includenan"
          omitnan = false;

        otherwise
          print_usage ();

      endswitch
    endfor

    varargin(varg_chars) = [];
    nvarg = numel (varargin);
  endif

  if (nvarg == 1 && ! isnumeric (varargin{1}))
    ## After trimming char inputs only one arg can be left, must be numeric.
    print_usage ();
  endif

  ## Process special cases of input/output sizes.
  if (nvarg == 0)
    ## Single numeric input argument, no dimensions given.
    if (all_flag)
      AF = AF(:);
      n = numel (AF);

      ## Process weights
      if (weighted)
        if (isvector (W))
          if (numel (W) != n)
            error (strcat ("rmse: WEIGHTS vector must have the same", ...
                           " length as the operating dimension"));
          endif
        elseif (! isequal (size (W), sz_AF))
          error (strcat ("rmse: WEIGHTS array must have the same size as", ...
                         " the difference of the input arrays F and A"));
        endif
        W = W(:);
        AF = W .* AF;
        n = sum (W);
      endif

      ## Process omitnan
      if (omitnan)
        nanAF = isnan (AF);
        AF(nanAF) = [];
        if (weighted)
          W(nanAF) = [];
          n = sum (W);
        else
          n = numel (AF);
        endif
      endif

      E = sqrt (sum (AF, "extra") ./ n);

    else
      ## Handle 0x0 empty input, no dimensions given
      if (nd_AF == 2 && isempty (AF) && sz_AF == [0,0])
        if (isa (AF, "single"))
          E = NaN ("single");
        else
          E = NaN;
        endif
        return;
      endif

      ## Find the first non-singleton dimension.
      (dim = find (sz_AF != 1, 1)) || (dim = 1);
      n = sz_AF(dim);

      ## Process weights
      if (weighted)
        if (isequal (size (W), sz_AF))
          AF = W .* AF;
        elseif (isvector (W))
          if (numel (W) != n)
            error (strcat ("rmse: WEIGHTS vector must have the same", ...
                           " length as the operating dimension"));
          endif
          sz_W = ones (1, nd_AF);
          sz_W(dim) = n;
          W = reshape (W, sz_W);
          if (! isvector (AF))
            sz_W = sz_AF;
            sz_W(dim) = 1;
            W = repmat (W, sz_W);
          endif
          AF = W .* AF;
        else
          error (strcat ("rmse: WEIGHTS array must have the same size as", ...
                         " the difference of the input arrays F and A"));
        endif
        n = sum (W, dim);
      endif

      ## Process omitnan
      if (omitnan)
        nanAF = isnan (AF);
        AF(nanAF) = 0;
        if (weighted)
          W(nanAF) = 0;
          n = sum (W, dim);
        else
          n = sum (! nanAF, dim);
        endif
      endif

      E = sqrt (sum (AF, dim, "extra") ./ n);

    endif

  else
    ## Two numeric input arguments, dimensions given.  Note scalar is vector!
    vecdim = varargin{1};
    if (isempty (vecdim) || ! (isvector (vecdim) && isindex (vecdim)))
      error ("rmse: DIM must be a positive integer scalar or vector");
    endif

    if (isscalar (vecdim))
      dim = vecdim;  # alias for code readability

      if (dim > nd_AF)

        ## Process weights
        if (weighted)
          if (! isequal (size (W), sz_AF))
             error (strcat ("rmse: WEIGHTS array must have the same size", ...
                            " as 'A - F', when 'DIM > ndims (A - F)'"));
          endif
        endif

        E = sqrt (AF);

      else
        n = sz_AF(dim);

        ## Process weights
        if (weighted)
          if (isequal (size (W), sz_AF))
            AF = W .* AF;
          elseif (isvector (W))
            if (numel (W) != n)
              error (strcat ("rmse: WEIGHTS vector must have the same", ...
                             " length as the operating dimension"));
            endif
            sz_W = ones (1, nd_AF);
            sz_W(dim) = n;
            W = reshape (W, sz_W);
            if (! isvector (AF))
              sz_W = sz_AF;
              sz_W(dim) = 1;
              W = repmat (W, sz_W);
            endif
            AF = W .* AF;
          else
          error (strcat ("rmse: WEIGHTS array must have the same size as", ...
                         " the difference of the input arrays F and A"));
          endif
          n = sum (W, dim);
        endif

        ## Process omitnan
        if (omitnan)
          nanAF = isnan (AF);
          AF(nanAF) = 0;
          if (weighted)
            W(nanAF) = 0;
            n = sum (W, dim);
          else
            n = sum (! nanAF, dim);
          endif
        endif

        E = sqrt (sum (AF, dim, "extra") ./ n);

      endif

    else
      vecdim = sort (vecdim);
      if (! all (diff (vecdim)))
        error ("rmse: VECDIM must contain non-repeating positive integers");
      endif
      ## Ignore dimensions in VECDIM larger than actual array.
      vecdim(vecdim > nd_AF) = [];

      if (isempty (vecdim))

        ## Process weights
        if (weighted)
          if (! isequal (size (W), sz_AF))
             error (strcat ("rmse: WEIGHTS array must have the same size", ...
                            " as 'A - F', when 'all (VECDIM > ndims (A - F))'"));
          endif
        endif

        E = sqrt (AF);

      else

        ## Calculate permutation vector
        remdims = 1:nd_AF;       # All dimensions
        remdims(vecdim) = [];  # Delete dimensions specified by vecdim
        nremd = numel (remdims);

        ## If all dimensions are given, it is equivalent to 'all' flag
        if (nremd == 0)
          AF = AF(:);
          n = numel (AF);

          ## Process weights
          if (weighted)
            if (isvector (W))
              if (numel (W) != n)
                error (strcat ("rmse: WEIGHTS vector must have the same", ...
                               " length as the operating dimension"));
              endif
            elseif (! isequal (size (W), sz_AF))
              error (strcat ("rmse: WEIGHTS array must have the same size", ...
                             " as the difference of the input arrays F and A"));
            endif
            W = W(:);
            AF = W .* AF;
            n = sum (W);
          endif

          ## Process omitnan
          if (omitnan)
            nanAF = isnan (AF);
            AF(nanAF) = [];
            if (weighted)
              W(nanAF) = [];
              n = sum (W);
            else
              n = numel (AF);
            endif
          endif

          E = sqrt (sum (AF, "extra") ./ n);

        else
          ## Weights must either match vecdim page size or the size of F - A
          if (weighted)
            page_szw = size (W);
            if (isequal (size (W), sz_AF))
              AF = W .* AF;
            elseif (isequal (page_szw, sz_AF(vecdim)))
              ## Make W to be compatible with AF
              tmp = ones (1, nd_AF);
              tmp(vecdim) = page_szw;
              W = reshape (W, tmp);
              W = W .* ones (sz_AF);
              AF = W .* AF;
            else
              error (strcat ("rmse: WEIGHTS array must have the same size", ...
                             " as the operating page specified by VECDIM", ...
                             " or the difference of the input arrays F and A"));
            endif
          endif

          ## Permute to push vecdims to back
          perm = [remdims, vecdim];
          AF = permute (AF, perm);
          if (weighted)
            W = permute (W, perm);
          endif

          ## Reshape to squash all vecdims in final dimension
          sznew = [sz_AF(remdims), prod(sz_AF(vecdim))];
          AF = reshape (AF, sznew);
          if (weighted)
            W = reshape (W, sznew);
          endif

          ## Calculate rmse on final dimension
          dim = nremd + 1;

          ## Process omitnan
          if (omitnan)
            nanAF = isnan (AF);
            AF(nanAF) = 0;
            if (weighted)
              W(nanAF) = 0;
              n = sum (W, dim);
            else
              n = sum (! nanAF, dim);
            endif
          else
            if (weighted)
              n = sum (W, dim);
            else
              n = sznew(dim);
            endif
          endif

          E = sqrt (sum (AF, dim, "extra") ./ n);

          ## Inverse permute back to correct dimensions
          E = ipermute (E, perm);

        endif
      endif
    endif
  endif

  ## Convert output if necessary
  if (strcmp (outtype, 'single'))
    E = single (E);
  endif

endfunction

## Test inputs with NaN and zeros
%!test
%! F = [2, 11, 6];
%! A = [3, 10, 8];
%! assert (rmse (F, A), sqrt (2));
%! assert (rmse (F, A, 1), [1, 1, 2]);
%! assert (rmse (F, A, 2), sqrt (2));
%! assert (rmse (F, A, 3), [1, 1, 2]);
%! assert (rmse (F, A, 'all'), sqrt (2));
%!test
%! F = [3; 7; 4; NaN];
%! A = [4; 0; 6; 4];
%! assert (rmse (F, A), NaN);
%! assert (rmse (F, A, 'omitnan'), 4.2426, 1e-4);
%! assert (rmse (F, A, 1, 'omitnan'), 4.2426, 1e-4);
%! assert (rmse (F, A, 2, 'omitnan'), [1; 7; 2; NaN]);
%!test
%! F = [17, 21; 1, 5; 13, 17];
%! A = [18, 19; 5, 4; 15, 13];
%! assert (rmse (F, A), [2.6458, 2.6458], 1e-4);
%! assert (rmse (F, A, 1), [2.6458, 2.6458], 1e-4);
%! assert (rmse (F, A, 2), [1.5811; 2.9155; 3.1623], 1e-4);
%! assert (rmse (F, A, 'all'), 2.6458, 1e-4);
%! assert (rmse (F, A, [1, 2]), 2.6458, 1e-4);
%!test
%! F(:,:,1) = [3, 5; -1, 2];
%! F(:,:,2) = [4, 2; 7, -4];
%! A = [6, 5; NaN, 3];
%! E = rmse (F, A, [1, 2]);
%! assert (E(:,:,1), NaN);
%! assert (E(:,:,2), NaN);
%! E = rmse (F, A, [1, 2], 'omitnan');
%! assert (E(:,:,1), 1.8257, 1e-4);
%! assert (E(:,:,2), 4.5461, 1e-4);
%! assert (rmse (F, A, [1, 3]), [NaN, 3.8406], 1e-4);
%! assert (rmse (F, A, [1, 3], 'omitnan'), [2.5495, 3.8406], 1e-4);
%! assert (rmse (F, A, [2, 3]), [2.3452; NaN], 1e-4);
%! assert (rmse (F, A, [2, 3], 'omitnan'), [2.3452; 5], 1e-4);

## Test Weights
%!test
%! F = [2, 11, 6];
%! A = [3, 10, 8];
%! W = [1, 2, 1];
%! assert (rmse (F, A, 'Weights', W), 1.3229, 1e-4);
%! assert (rmse (F, A, 1, 'Weights', W), [1, 1, 2]);
%! assert (rmse (F, A, 2, 'Weights', W), 1.3229, 1e-4);
%! assert (rmse (F, A, 3, 'Weights', W), [1, 1, 2]);
%!test
%! F = [3; 7; 4; NaN];
%! A = [4; 0; 6; 4];
%! W = [1, 2, 1, 2];
%! assert (rmse (F, A, 'Weights', W), NaN);
%! assert (rmse (F, A, 'omitnan', 'Weights', W), 5.0744, 1e-4);
%! assert (rmse (F, A, 1, 'omitnan', 'Weights', W), 5.0744, 1e-4);
%!test
%! F = [3; 7; 4; NaN];
%! A = [4; 0; 6; 4];
%! W = [1; 2; 1; 2];
%! assert (rmse (F, A, 2, 'Weights', W), [1; 7; 2; NaN], 1e-4);
%! assert (rmse (F, A, 2, 'omitnan', 'Weights', W), [1; 7; 2; NaN], 1e-4);
%! assert (rmse (F, A, 3, 'Weights', W), [1; 7; 2; NaN], 1e-4);
%! assert (rmse (F, A, 3, 'omitnan', 'Weights', W), [1; 7; 2; NaN], 1e-4);
%!test
%! F = [3, 4; 7, 5; 4, 3; NaN, 8];
%! A = [4; 0; 6; 4];
%! W = [3; 1; 2; 2];
%! E = rmse (F, A, 1, 'omitnan', 'Weights', W);
%! assert (E, [3.1623, 3.0619], 1e-4);
%!test
%! F = [3, 4; 7, 5; 4, 3; NaN, 8];
%! A = [4; 0; 6; 4];
%! W = [3, 0.4; 1, 0.2; 2, 0.2; 2, 0.2];
%! assert (rmse (F, A, 1, 'Weights', W), [NaN, 3.1623], 1e-4);
%! assert (rmse (F, A, 1, 'omitnan', 'Weights', W), [3.1623, 3.1623], 1e-4);
%! assert (rmse (F, A, 2, 'Weights', W), [0.9393; 6.7082; 2.1106; NaN], 1e-4);
%! E = rmse (F, A, 2, 'omitnan', 'Weights', W);
%! assert (E, [0.9393; 6.7082; 2.1106; 4], 1e-4);

## Test Weights with VECDIM
%!test
%! F(:,:,1) = [3, 5; -1, 2];
%! F(:,:,2) = [4, NaN; 7, -4];
%! A = [6, 5; 2, 3];
%! W = [1, 1; 1, 1];
%! Ew = rmse (F, A, [1, 2], 'omitnan', 'Weights', W);
%! E = rmse (F, A, [1, 2], 'omitnan');
%! assert (Ew, E);
%! Ew = rmse (F, A, [1, 3], 'omitnan','Weights', W);
%! E = rmse (F, A, [1, 3], 'omitnan');
%! assert (Ew, E);
%! Ew = rmse (F, A, [2, 3], 'omitnan','Weights', W);
%! E = rmse (F, A, [2, 3], 'omitnan');
%! assert (Ew, E);
%!test
%! F(:,:,1) = [3, 5; -1, 2];
%! F(:,:,2) = [4, 2; 7, -4];
%! A = [6, 5; 0, 3];
%! W = [1, 2; 1, 1];
%! F1 = reshape (F, 1, 4, 2);
%! A1 = reshape (A, 1, 4);
%! W1 = reshape (W, 1, 4);
%! E1w = rmse (F1, A1, 2, 'Weights', W1);
%! assert (E1w(:,:,1), 1.4832, 1e-4);
%! assert (E1w(:,:,2), 4.8990, 1e-4);
%! E1 = rmse (F1, A1, 2);
%! assert (E1(:,:,1), 1.6583, 1e-4);
%! assert (E1(:,:,2), 5.2678, 1e-4);
%! assert (rmse (F, A, [1, 2], 'Weights', W), E1w);
%! assert (rmse (F, A, [1, 2]), E1);
%!test
%! F(:,:,1) = [3, 5; -1, 2];
%! F(:,:,2) = [4, 2; 7, -4];
%! A = [6, 5; 0, 3];
%! W = [0.5, 2; 1, 3];
%! F1 = reshape (F, 2, 4, 1);
%! A1 = reshape (repmat (A, 1, 1, 2), 2, 4, 1);
%! W1 = [0.5, 1, 2, 3; 0.5, 1, 2, 3];
%! E1w = rmse (F1, A1, 2, 'Weights', W1);
%! assert (E1w, [2.4651; 6.1582], 1e-4);
%! E1 = rmse (F1, A1, 2);
%! assert (E1, [2.3452; 5], 1e-4);
%! assert (rmse (F, A, [2, 3], 'Weights', W), E1w);
%! assert (rmse (F, A, [2, 3]), E1);


## Test input validation
%!error <Invalid call> rmse ()
%!error <Invalid call> rmse (1)
%!error <Invalid call> rmse (1, 2, 3, 4, 5, 6, 7, 8)
%!error <F must be a floating point numeric array> rmse ({1:5}, [1:5])
%!error <F must be a floating point numeric array> rmse (int32 ([1:5]), [1:5])
%!error <A must be a floating point numeric array> rmse ([1:5], {1:5})
%!error <A must be a floating point numeric array> rmse ([1:5], int32 ([1:5]))
%!error <WEIGHTS must be single or double> rmse ([1:5], [1:5], 'Weights', 'double')
%!error <WEIGHTS must be single or double> rmse ([1:5], [1:5], 'Weights', uint8 (1))
%!error <WEIGHTS must be nonnegative> rmse ([1:5], [1:5], 'Weights', -1)
%!error <paired input argument for 'Weights' is missing> rmse ([1:5], [1:5], 'Weights')
%!error <Invalid call> rmse ([1:5], [1:5], 2, 3)
%!error <Invalid call> rmse ([1:5], [1:5], "all", 3)
%!error <Invalid call> rmse ([1:5], [1:5], 'foobar')
%!error <WEIGHTS vector must have the same length as the operating dimension> ...
%! rmse ([1:5], [1:5], "all", 'Weights', [1, 2, 3])
%!error <WEIGHTS vector must have the same length as the operating dimension> ...
%! rmse ([1:5], 1, "all", 'Weights', [1, 2, 3])
%!error <WEIGHTS array must have the same size as the difference> ...
%! rmse (ones (5, 3), ones (5, 3), "all", 'Weights', ones (3, 5))
%!error <WEIGHTS array must have the same size as the difference> ...
%! rmse (ones (5, 3), ones (1, 3), "all", 'Weights', ones (3, 5))
%!error <WEIGHTS vector must have the same length as the operating dimension> ...
%! rmse ([1:5], [1:5], 'Weights', [1, 2, 3])
%!error <WEIGHTS vector must have the same length as the operating dimension> ...
%! rmse ([1:5], 1, 'Weights', [1, 2, 3])
%!error <WEIGHTS vector must have the same length as the operating dimension> ...
%! rmse (ones (5, 3), ones (5, 3), 'Weights', [1, 2, 3])
%!error <WEIGHTS vector must have the same length as the operating dimension> ...
%! rmse (ones (5, 3), ones (5, 1), 'Weights', [1, 2, 3])
%!error <WEIGHTS array must have the same size as the difference> ...
%! rmse (ones (5, 3), ones (5, 3), 'Weights', ones (3, 5))
%!error <WEIGHTS array must have the same size as the difference> ...
%! rmse (ones (5, 3), ones (1, 3), 'Weights', ones (3, 5))
%!error <DIM must be a positive integer> rmse ([1:5], [1:5], ones (2,2))
%!error <DIM must be a positive integer> rmse ([1:5], [1:5], 1.5)
%!error <DIM must be a positive integer> rmse ([1:5], [1:5], 0)
%!error <DIM must be a positive integer> rmse ([1:5], [1:5], [])
%!error <DIM must be a positive integer> rmse ([1:5], [1:5], -1)
%!error <DIM must be a positive integer> rmse ([1:5], [1:5], -1.5)
%!error <DIM must be a positive integer> rmse ([1:5], [1:5], NaN)
%!error <DIM must be a positive integer> rmse ([1:5], [1:5], Inf)
%!error <DIM must be a positive integer> rmse (repmat ([1:5;5:9], [5 2]), repmat ([1:5;5:9], [5 2]), -1)
%!error <DIM must be a positive integer> rmse (repmat ([1:5;5:9], [5 2]), repmat ([1:5;5:9], [5 2]), [1 -1])
%!error <DIM must be a positive integer> rmse ([1:5], [1:5], ones (1,0))
%!error <WEIGHTS array must have the same size as 'A - F', when 'DIM> ...
%! rmse (ones (5, 1), ones (5, 1), 3, 'Weights', ones (1, 5))
%!error <WEIGHTS vector must have the same length as the operating dimension> ...
%! rmse ([1:5], [1:5], 2, 'Weights', [1, 2, 3])
%!error <WEIGHTS array must have the same size as the difference> ...
%! rmse (ones (5, 3), ones (5, 3), 2, 'Weights', ones (3, 5))
%!error <VECDIM must contain non-repeating> rmse ([1:5], [1:5], [2 2])
%!error <WEIGHTS array must have the same size as 'A - F', when 'all> ...
%! rmse (ones (5, 1), ones (5, 1), [3, 4], 'Weights', ones (1, 5))
%!error <WEIGHTS vector must have the same length as the operating dimension> ...
%! rmse ([1:5], [1:5], [1, 2], 'Weights', [1, 2, 3])
%!error <WEIGHTS array must have the same size as the difference> ...
%! rmse (ones (5, 3), ones (5, 3), [1, 2], 'Weights', ones (3, 5))
%!error <WEIGHTS array must have the same size as the difference> ...
%! rmse (ones (5, 3), ones (1, 3), [1, 2], 'Weights', ones (3, 5))
%!error <WEIGHTS array must have the same size as the operating page> ...
%! rmse (ones (5, 3, 2), ones (5, 3, 2), [1, 2], 'Weights', ones (3, 5))
%!error <WEIGHTS array must have the same size as the operating page> ...
%! rmse (ones (5, 3, 2), ones (5, 1, 2), [1, 2], 'Weights', ones (3, 5))
