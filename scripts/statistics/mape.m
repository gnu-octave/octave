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
## @deftypefn  {} {@var{E} =} mape (@var{F}, @var{A})
## @deftypefnx {} {@var{E} =} mape (@var{F}, @var{A}, @var{dim})
## @deftypefnx {} {@var{E} =} mape (@var{F}, @var{A}, @var{vecdim})
## @deftypefnx {} {@var{E} =} mape (@var{F}, @var{A}, "all")
## @deftypefnx {} {@var{E} =} mape (@dots{}, @var{nanflag})
## @deftypefnx {} {@var{E} =} mape (@dots{}, @var{zeroflag})
## @deftypefnx {} {@var{E} =} mape (@dots{}, @qcode{'Weights'}, @var{W})
## Compute the mean absolute percentage error between arrays.
##
## The mean absolute percentage error is defined as
## @tex
## $$ {\rm mape}(F, A) = {E} = {1\over N} \sum_{i=1}^N \left| {A_i - F_i}\over{A_i} \right| \times 100 $$
## where $N$ is the number of elements in @var{F} and @var{A} after
## broadcasting is applied to the subtraction operation.
## @end tex
## @ifnottex
##
## @example
## mape (@var{F}, @var{A}) = SUM_i (abs ((@var{A}(i) - @var{F}(i)) / @var{A}(i))) * 100 / N
## @end example
##
## @noindent
## where @math{N} is the number of elements in @var{F} and @var{A} after
## broadcasting is applied to the subtraction operation.
##
## @end ifnottex
##
## The weighted mean absolute percentage error is defined as
## @tex
## $$ {\rm mape_w}(F, A) = {E_W} =  {{\sum_{i=1}^N W_i\left| {A_i - F_i}\over{A_i} \right| \times 100} \over {\sum_{i=1}^N W_i}} $$
## where $N$ is the number of elements in @var{F} and @var{A} after
## broadcasting is applied to the subtraction operation.
## @end tex
## @ifnottex
##
## @example
## weighted_mape (@var{F}, @var{A}) = SUM_i (@var{W}(i) * (abs ((@var{A}(i) - @var{F}(i)) / @var{A}(i)))) * 100 / SUM_i (@var{W}(i)
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
## @code{mape (@var{F}, @var{A})} returns a scalar with the MAPE between the
## elements of @var{F} and @var{A}.
##
## If @code{@var{A} - @var{F}} is a matrix, then @code{mape (@var{F}, @var{A})}
## returns a row vector with each element containing the MAPE between the
## corresponding columns of @code{@var{A} - @var{F}}.
##
## If @code{@var{A} - @var{F}} is an array, then @code{mape (@var{F}, @var{A})}
## computes the MAPE along the first non-singleton dimension of the difference
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
## dimensions, will return the mape over the array slice defined by
## @var{vecdim}.  If @var{vecdim} indexes all dimensions in
## @code{@var{A} - @var{F}}, then it is equivalent to the option @qcode{"all"}.
## Any dimension in @var{vecdim} greater than @code{ndims (@var{A} - @var{F})}
## is ignored.
##
## Specifying the dimension as @qcode{"all"} will cause @code{mape} to operate
## on all elements of @code{@var{A} - @var{F}}, and is equivalent to
## @code{mape ((@var{A} - @var{F})(:))}.
##
## The optional variable @var{nanflag} specifies whether to include or exclude
## NaN values from the calculation using any of the previously specified input
## argument combinations.  The default value for @var{nanflag} is
## @qcode{"includenan"} which keeps NaN values in the calculation.  To exclude
## NaN values set the value of @var{nanflag} to @qcode{"omitnan"}.  The output
## will still contain NaN values if @code{@var{A} - @var{F}} consists of all NaN
## values in the operating dimension.
##
## The optional variable @var{zeroflag} specifies whether to include or omit
## zero values from the calculation using any of the previously specified input
## argument combinations.  The default value for @var{zeroflag} is
## @qcode{"includezero"}, in which case the calculated MAPE is Inf, if @var{A}
## contains one or more zeros.  To ignore any zero values in @var{A}, set the
## value of @var{zeroflag} to @qcode{"omitzero"}.  If @var{A} consists of all
## zero values in the operating dimension, then MAPE is NaN.
##
## The optional paired argument @code{@dots{}, "Weights", @var{W}} specifies a
## weighting scheme @var{W}, which is applied on the difference of the input
## arrays @var{F} and @var{A}, so that @code{mape} computes the weighted MAPE.
## When operating along a single dimension, @var{W} must be a vector of the same
## length as the operating dimension or it must have the same size as @var{x}.
## When operating over an array slice defined by @var{vecdim}, @var{W} must have
## the same size as the operating array slice, i.e.
## @code{size (@var{A} - @var{F})(@var{vecdim})}, or the same size as
## @code{@var{A} - @var{F}}.
##
## @seealso{rmse, mean, abs}
## @end deftypefn

function E = mape (F, A, varargin)

  if (nargin < 2 || nargin > 7)
    print_usage ();
  endif

  if (! any (isa (F, {'double', 'single'})))
    error ("mape: F must be a floating point numeric array");
  endif

  if (! any (isa (A, {'double', 'single'})))
    error ("mape: A must be a floating point numeric array");
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
    error ("mape: F and A must have compatible sizes");
  end_try_catch

  ## Initialize flags
  all_flag = false;
  omitnan  = false;
  zeroflag = false;
  weighted = false;

  ## Process paired argument for Weights
  w_idx = find (cellfun (@(x) strcmpi (x, "weights"), varargin));
  if (! isempty (w_idx))
    if (numel (varargin) > w_idx)
      W = varargin{w_idx+1};
      if (! (isnumeric (W) && any (isa (W, {'double', 'single'}))))
        error ("mape: WEIGHTS must be single or double");
      endif
      if (any (W(:) < 0))
        error ("mape: WEIGHTS must be nonnegative");
      endif
    else
      error ("mape: paired input argument for 'Weights' is missing");
    endif
    weighted = true;
    varargin([w_idx, w_idx+1]) = [];
  endif

  nvarg = numel (varargin);
  varg_chars = cellfun ("ischar", varargin);
  sz_AF = size (AF);
  nd_AF = ndims (AF);

  ## Force A to the same size as AF
  if (! isequal (size (A), sz_AF))
    A = A .* ones (sz_AF);
  endif

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

        case "omitzero"
          zeroflag = true;

        case "includezero"
          zeroflag = false;

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
      A = A(:);
      n = numel (AF);

      ## Process weights
      if (weighted)
        if (isvector (W))
          if (numel (W) != n)
            error (strcat ("mape: WEIGHTS vector must have the same", ...
                           " length as the operating dimension"));
          endif
        elseif (! isequal (size (W), sz_AF))
          error (strcat ("mape: WEIGHTS array must have the same size as", ...
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

      ## Process zeroflag
      if (zeroflag)
        iszero = A == 0;
        if (any (iszero, "all"))
          A(iszero) = 1;
          AF(iszero) = 0;
          if (weighted)
            W(iszero) = 0;
            n = sum (W);
          else
            n = n - sum (iszero);
          endif
        endif
      endif

      E = sum (abs (AF ./ A), "extra") ./ n;

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
            error (strcat ("mape: WEIGHTS vector must have the same", ...
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
          error (strcat ("mape: WEIGHTS array must have the same size as", ...
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

      ## Process zeroflag
      if (zeroflag)
        iszero = A == 0;
        if (any (iszero, "all"))
          A(iszero) = 1;
          AF(iszero) = 0;
          if (weighted)
            W(iszero) = 0;
            n = sum (W, dim);
          else
            n = n - sum (iszero, dim);
          endif
        endif
      endif

      E = sum (abs (AF ./ A), dim, "extra") ./ n;

    endif

  else
    ## Two numeric input arguments, dimensions given.  Note scalar is vector!
    vecdim = varargin{1};
    if (isempty (vecdim) || ! (isvector (vecdim) && isindex (vecdim)))
      error ("mape: DIM must be a positive integer scalar or vector");
    endif

    if (isscalar (vecdim))
      dim = vecdim;  # alias for code readability

      if (dim > nd_AF)

        ## Process weights
        if (weighted)
          if (! isequal (size (W), sz_AF))
             error (strcat ("mape: WEIGHTS array must have the same size", ...
                            " as 'A - F', when 'DIM > ndims (A - F)'"));
          endif
        endif

        ## Process zeroflag
        if (zeroflag)
          iszero = A == 0;
          if (any (iszero, "all"))
            A(iszero) = 1;
            AF(iszero) = NaN;
          endif
        endif

        E = abs (AF ./ A);

      else
        n = sz_AF(dim);

        ## Process weights
        if (weighted)
          if (isequal (size (W), sz_AF))
            AF = W .* AF;
          elseif (isvector (W))
            if (numel (W) != n)
              error (strcat ("mape: WEIGHTS vector must have the same", ...
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
          error (strcat ("mape: WEIGHTS array must have the same size as", ...
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

        ## Process zeroflag
        if (zeroflag)
          iszero = A == 0;
          if (any (iszero, "all"))
            A(iszero) = 1;
            AF(iszero) = 0;
            if (weighted)
              W(iszero) = 0;
              n = sum (W, dim);
            else
              n = n - sum (iszero, dim);
            endif
          endif
        endif

        E = sum (abs (AF ./ A), dim, "extra") ./ n;

      endif

    else
      vecdim = sort (vecdim);
      if (! all (diff (vecdim)))
        error ("mape: VECDIM must contain non-repeating positive integers");
      endif
      ## Ignore dimensions in VECDIM larger than actual array.
      vecdim(vecdim > nd_AF) = [];

      if (isempty (vecdim))

        ## Process weights
        if (weighted)
          if (! isequal (size (W), sz_AF))
             error (strcat ("mape: WEIGHTS array must have the same size", ...
                            " as 'A - F', when 'all (VECDIM > ndims (A - F))'"));
          endif
        endif

        ## Process zeroflag
        if (zeroflag)
          iszero = A == 0;
          if (any (iszero, "all"))
            A(iszero) = 1;
            AF(iszero) = NaN;
          endif
        endif

        E = abs (AF ./ A);

      else

        ## Calculate permutation vector
        remdims = 1:nd_AF;       # All dimensions
        remdims(vecdim) = [];  # Delete dimensions specified by vecdim
        nremd = numel (remdims);

        ## If all dimensions are given, it is equivalent to 'all' flag
        if (nremd == 0)
          AF = AF(:);
          A = A(:);
          n = numel (AF);

          ## Process weights
          if (weighted)
            if (isvector (W))
              if (numel (W) != n)
                error (strcat ("mape: WEIGHTS vector must have the same", ...
                               " length as the operating dimension"));
              endif
            elseif (! isequal (size (W), sz_AF))
              error (strcat ("mape: WEIGHTS array must have the same size", ...
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

          ## Process zeroflag
          if (zeroflag)
            iszero = A == 0;
            if (any (iszero, "all"))
              A(iszero) = 1;
              AF(iszero) = 0;
              if (weighted)
                W(iszero) = 0;
                n = sum (W);
              else
                n = n - sum (iszero);
              endif
            endif
          endif

          E = sum (abs (AF ./ A), "extra") ./ n;

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
              error (strcat ("mape: WEIGHTS array must have the same size", ...
                             " as the operating page specified by VECDIM", ...
                             " or the difference of the input arrays F and A"));
            endif
          endif

          ## Permute to push vecdims to back
          perm = [remdims, vecdim];
          AF = permute (AF, perm);
          A = permute (A, perm);
          if (weighted)
            W = permute (W, perm);
          endif

          ## Reshape to squash all vecdims in final dimension
          sznew = [sz_AF(remdims), prod(sz_AF(vecdim))];
          AF = reshape (AF, sznew);
          A = reshape (A, sznew);
          if (weighted)
            W = reshape (W, sznew);
          endif

          ## Calculate mape on final dimension
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

          ## Process zeroflag
          if (zeroflag)
            iszero = A == 0;
            if (any (iszero, "all"))
              A(iszero) = 1;
              AF(iszero) = 0;
              if (weighted)
                W(iszero) = 0;
                n = sum (W, dim);
              else
                n = n - sum (iszero, dim);
              endif
            endif
          endif

          E = sum (abs (AF ./ A), dim, "extra") ./ n;

          ## Inverse permute back to correct dimensions
          E = ipermute (E, perm);

        endif
      endif
    endif
  endif

  ## Multiply by 100 to complete the equation
  E = E * 100;

  ## Convert output if necessary
  if (strcmp (outtype, 'single'))
    E = single (E);
  endif

endfunction

## Test inputs with NaN and zeros
%!test
%! F = [2, 11, 6];
%! A = [3, 10, 8];
%! assert (mape (F, A), 22.7778, 1e-4);
%! assert (mape (F, A, 1), [33.3333, 10.0000, 25.0000], 1e-4);
%! assert (mape (F, A, 2), 22.7778, 1e-4);
%! assert (mape (F, A, 3), [33.3333, 10.0000, 25.0000], 1e-4);
%! assert (mape (F, A, 'all'), 22.7778, 1e-4);
%!test
%! F = [3; 7; 4; NaN];
%! A = [4; 0; 6; 4];
%! assert (mape (F, A), NaN);
%! assert (mape (F, A, 'omitnan'), Inf);
%! assert (mape (F, A, 'omitzero'), NaN);
%! assert (mape (F, A, 'omitnan', 'omitzero'), 29.1667, 1e-4);
%! assert (mape (F, A, 1, 'omitnan', 'omitzero'), 29.1667, 1e-4);
%! E = mape (F, A, 2, 'omitnan', 'omitzero');
%! assert (E, [25; NaN; 33.3333; NaN], 1e-4);
%!test
%! F = [17, 21; 1, 5; 13, 17];
%! A = [18, 19; 5, 4; 15, 13];
%! assert (mape (F, A), [32.9630, 22.0985], 1e-4);
%! assert (mape (F, A, 1), [32.9630, 22.0985], 1e-4);
%! assert (mape (F, A, 2), [8.0409; 52.5000; 22.0513], 1e-4);
%! assert (mape (F, A, 'all'), 27.5307, 1e-4);
%! assert (mape (F, A, [1, 2]), 27.5307, 1e-4);
%!test
%! F(:,:,1) = [3, 5; -1, 2];
%! F(:,:,2) = [4, 2; 7, -4];
%! A = [6, 5; 0, 3];
%! E = mape (F, A, [1, 2]);
%! assert (E(:,:,1), Inf);
%! assert (E(:,:,2), Inf);
%! E = mape (F, A, [1, 2], 'omitzero');
%! assert (E(:,:,1), 27.7778, 1e-4);
%! assert (E(:,:,2), 108.8889, 1e-4);
%! assert (mape (F, A, [1, 3]), [Inf, 81.6667], 1e-4);
%! assert (mape (F, A, [1, 3], 'omitzero'), [41.6667, 81.6667], 1e-4);
%! assert (mape (F, A, [2, 3]), [35.8333; Inf], 1e-4);
%! assert (mape (F, A, [2, 3], 'omitzero'), [35.8333; 133.3333], 1e-4);
%!assert (mape ([1:4], zeros (1, 4)), Inf)
%!assert (mape ([1:4], zeros (1, 4), "omitzero"), NaN)

## Test Weights
%!test
%! F = [2, 11, 6];
%! A = [3, 10, 8];
%! W = [1, 2, 1];
%! assert (mape (F, A, 'Weights', W), 19.5833, 1e-4);
%! assert (mape (F, A, 1, 'Weights', W), [33.3333, 10, 25], 1e-4);
%! assert (mape (F, A, 2, 'Weights', W), 19.5833, 1e-4);
%! assert (mape (F, A, 3, 'Weights', W), [33.3333, 10, 25], 1e-4);
%!test
%! F = [3; 7; 4; NaN];
%! A = [4; 0; 6; 4];
%! W = [1, 2, 1, 2];
%! assert (mape (F, A, 'Weights', W), NaN);
%! assert (mape (F, A, 'omitnan', 'Weights', W), Inf);
%! assert (mape (F, A, 'omitzero', 'Weights', W), NaN);
%! assert (mape (F, A, 'omitnan', 'omitzero', 'Weights', W), 29.1667, 1e-4);
%! assert (mape (F, A, 1, 'omitnan', 'omitzero', 'Weights', W), 29.1667, 1e-4);
%!test
%! F = [3; 7; 4; NaN];
%! A = [4; 0; 6; 4];
%! W = [1; 2; 1; 2];
%! E = mape (F, A, 2, 'omitnan', 'omitzero', 'Weights', W);
%! assert (E, [25; NaN; 33.3333; NaN], 1e-4);
%! E = mape (F, A, 3, 'omitnan', 'omitzero', 'Weights', W);
%! assert (E, [25; NaN; 33.3333; NaN], 1e-4);
%!test
%! F = [3; 7; 4; NaN];
%! A = [4; 0; 6; 4];
%! W = [1; 1; 2; 2];
%! E = mape (F, A, 2, 'omitnan', 'omitzero', 'Weights', W);
%! assert (E, [25; NaN; 33.3333; NaN], 1e-4);
%! E = mape (F, A, 3, 'omitnan', 'omitzero', 'Weights', W);
%! assert (E, [25; NaN; 33.3333; NaN], 1e-4);
%! E = mape (F, A, 3, 'omitzero', 'Weights', W);
%! assert (E, [25; NaN; 33.3333; NaN], 1e-4);
%! E = mape (F, A, 3, 'omitnan', 'Weights', W);
%! assert (E, [25; Inf; 33.3333; NaN], 1e-4);
%! assert (mape (F, A, 3, 'Weights', W), [25; Inf; 33.3333; NaN], 1e-4);
%! assert (mape (F, A, 3), [25; Inf; 33.3333; NaN], 1e-4);
%!test
%! F = [3; 7; 4; NaN];
%! A = [4; 0; 6; 4];
%! W = [1; 1; 2; 2];
%! assert (mape (F, A, 1, 'omitnan', 'omitzero', 'Weights', W), 30.5556, 1e-4);
%! assert (mape (F, A, 1, 'omitzero', 'Weights', W), NaN);
%! assert (mape (F, A, 1, 'omitnan', 'Weights', W), Inf);
%! assert (mape (F, A, 1, 'Weights', W), NaN);
%! assert (mape (F, A, 1), NaN);
%!test
%! F = [3, 4; 7, 5; 4, 3; NaN, 8];
%! A = [4; 0; 6; 4];
%! W = [3; 1; 2; 2];
%! E = mape (F, A, 1, 'omitnan', 'omitzero', 'Weights', W);
%! assert (E, [28.3333, 42.8571], 1e-4);
%! E = mape (F, A, 1, 'omitzero', 'Weights', W);
%! assert (E, [NaN, 42.8571], 1e-4);
%! assert (mape (F, A, 1, 'omitnan', 'Weights', W), [Inf, Inf]);
%! assert (mape (F, A, 1, 'Weights', W), [NaN, Inf]);
%!test
%! F = [3, 4; 7, 5; 4, 3; NaN, 8];
%! A = [4; 0; 6; 4];
%! W = [3, 0.4; 1, 0.2; 2, 0.2; 2, 0.2];
%! E = mape (F, A, 1, 'omitnan', 'omitzero', 'Weights', W);
%! assert (E, [28.3333, 37.5000], 1e-4);
%! E = mape (F, A, 1, 'omitzero', 'Weights', W);
%! assert (E, [NaN, 37.5000], 1e-4);
%! assert (mape (F, A, 1, 'omitnan', 'Weights', W), [Inf, Inf]);
%! assert (mape (F, A, 1, 'Weights', W), [NaN, Inf]);

## Test Weights with VECDIM
%!test
%! F(:,:,1) = [3, 5; -1, 2];
%! F(:,:,2) = [4, 2; 7, -4];
%! A = [6, 5; 0, 3];
%! W = [1, 1; 1, 1];
%! Ew = mape (F, A, [1, 2], 'omitzero', 'Weights', W);
%! E = mape (F, A, [1, 2], 'omitzero');
%! assert (Ew, E);
%! Ew = mape (F, A, [1, 3], 'omitzero', 'Weights', W);
%! E = mape (F, A, [1, 3], 'omitzero');
%! assert (Ew, E);
%! Ew = mape (F, A, [2, 3], 'omitzero', 'Weights', W);
%! E = mape (F, A, [2, 3], 'omitzero');
%! assert (Ew, E);
%!test
%! F(:,:,1) = [3, 5; -1, 2];
%! F(:,:,2) = [4, NaN; 7, -4];
%! A = [6, 5; 2, 3];
%! W = [1, 1; 1, 1];
%! Ew = mape (F, A, [1, 2], 'omitnan', 'Weights', W);
%! E = mape (F, A, [1, 2], 'omitnan');
%! assert (Ew, E);
%! Ew = mape (F, A, [1, 3], 'omitnan','Weights', W);
%! E = mape (F, A, [1, 3], 'omitnan');
%! assert (Ew, E);
%! Ew = mape (F, A, [2, 3], 'omitnan','Weights', W);
%! E = mape (F, A, [2, 3], 'omitnan');
%! assert (Ew, E);
%!test
%! F(:,:,1) = [3, 5; -1, 2];
%! F(:,:,2) = [4, 2; 7, -4];
%! A = [6, 5; 0, 3];
%! W = [1, 2; 1, 1];
%! F1 = reshape (F, 1, 4, 2);
%! A1 = reshape (A, 1, 4);
%! W1 = reshape (W, 1, 4);
%! E1w = mape (F1, A1, 2, 'omitzero', 'Weights', W1);
%! assert (E1w(:,:,1), 20.8333, 1e-4);
%! assert (E1w(:,:,2), 96.6667, 1e-4);
%! E1 = mape (F1, A1, 2, 'omitzero');
%! assert (E1(:,:,1), 27.7778, 1e-4);
%! assert (E1(:,:,2), 108.8889, 1e-4);
%! assert (mape (F, A, [1, 2], 'omitzero', 'Weights', W), E1w);
%! assert (mape (F, A, [1, 2], 'omitzero'), E1);
%! E1w = mape (F1, A1, 2, 'Weights', W1);
%! assert (E1w(:,:,1), Inf);
%! assert (E1w(:,:,2), Inf);
%! E1 = mape (F1, A1, 2);
%! assert (E1(:,:,1), Inf);
%! assert (E1(:,:,2), Inf);
%! assert (mape (F, A, [1, 2], 'Weights', W), E1w);
%! assert (mape (F, A, [1, 2]), E1);
%!test
%! F(:,:,1) = [3, 5; -1, 2];
%! F(:,:,2) = [4, 2; 7, -4];
%! A = [6, 5; 0, 3];
%! W = [0.5, 2; 1, 3];
%! F1 = reshape (F, 2, 4, 1);
%! A1 = reshape (repmat (A, 1, 1, 2), 2, 4, 1);
%! W1 = [0.5, 1, 2, 3; 0.5, 1, 2, 3];
%! E1w = mape (F1, A1, 2, 'omitzero', 'Weights', W1);
%! assert (E1w, [41.7949; 183.3333], 1e-4);
%! E1 = mape (F1, A1, 2, 'omitzero');
%! assert (E1, [35.8333; 133.3333], 1e-4);
%! assert (mape (F, A, [2, 3], 'omitzero', 'Weights', W), E1w);
%! assert (mape (F, A, [2, 3], 'omitzero'), E1);
%! E1w = mape (F1, A1, 2, 'Weights', W1);
%! assert (E1w, [41.7949; Inf], 1e-4);
%! E1 = mape (F1, A1, 2);
%! assert (E1, [35.8333; Inf], 1e-4);
%! assert (mape (F, A, [2, 3], 'Weights', W), E1w);
%! assert (mape (F, A, [2, 3]), E1);


## Test input validation
%!error <Invalid call> mape ()
%!error <Invalid call> mape (1)
%!error <Invalid call> mape (1, 2, 3, 4, 5, 6, 7, 8)
%!error <F must be a floating point numeric array> mape ({1:5}, [1:5])
%!error <F must be a floating point numeric array> mape (int32 ([1:5]), [1:5])
%!error <A must be a floating point numeric array> mape ([1:5], {1:5})
%!error <A must be a floating point numeric array> mape ([1:5], int32 ([1:5]))
%!error <WEIGHTS must be single or double> mape ([1:5], [1:5], 'Weights', 'double')
%!error <WEIGHTS must be single or double> mape ([1:5], [1:5], 'Weights', uint8 (1))
%!error <WEIGHTS must be nonnegative> mape ([1:5], [1:5], 'Weights', -1)
%!error <paired input argument for 'Weights' is missing> mape ([1:5], [1:5], 'Weights')
%!error <Invalid call> mape ([1:5], [1:5], 2, 3)
%!error <Invalid call> mape ([1:5], [1:5], "all", 3)
%!error <Invalid call> mape ([1:5], [1:5], 'foobar')
%!error <WEIGHTS vector must have the same length as the operating dimension> ...
%! mape ([1:5], [1:5], "all", 'Weights', [1, 2, 3])
%!error <WEIGHTS vector must have the same length as the operating dimension> ...
%! mape ([1:5], 1, "all", 'Weights', [1, 2, 3])
%!error <WEIGHTS array must have the same size as the difference> ...
%! mape (ones (5, 3), ones (5, 3), "all", 'Weights', ones (3, 5))
%!error <WEIGHTS array must have the same size as the difference> ...
%! mape (ones (5, 3), ones (1, 3), "all", 'Weights', ones (3, 5))
%!error <WEIGHTS vector must have the same length as the operating dimension> ...
%! mape ([1:5], [1:5], 'Weights', [1, 2, 3])
%!error <WEIGHTS vector must have the same length as the operating dimension> ...
%! mape ([1:5], 1, 'Weights', [1, 2, 3])
%!error <WEIGHTS vector must have the same length as the operating dimension> ...
%! mape (ones (5, 3), ones (5, 3), 'Weights', [1, 2, 3])
%!error <WEIGHTS vector must have the same length as the operating dimension> ...
%! mape (ones (5, 3), ones (5, 1), 'Weights', [1, 2, 3])
%!error <WEIGHTS array must have the same size as the difference> ...
%! mape (ones (5, 3), ones (5, 3), 'Weights', ones (3, 5))
%!error <WEIGHTS array must have the same size as the difference> ...
%! mape (ones (5, 3), ones (1, 3), 'Weights', ones (3, 5))
%!error <DIM must be a positive integer> mape ([1:5], [1:5], ones (2,2))
%!error <DIM must be a positive integer> mape ([1:5], [1:5], 1.5)
%!error <DIM must be a positive integer> mape ([1:5], [1:5], 0)
%!error <DIM must be a positive integer> mape ([1:5], [1:5], [])
%!error <DIM must be a positive integer> mape ([1:5], [1:5], -1)
%!error <DIM must be a positive integer> mape ([1:5], [1:5], -1.5)
%!error <DIM must be a positive integer> mape ([1:5], [1:5], NaN)
%!error <DIM must be a positive integer> mape ([1:5], [1:5], Inf)
%!error <DIM must be a positive integer> mape (repmat ([1:5;5:9], [5 2]), repmat ([1:5;5:9], [5 2]), -1)
%!error <DIM must be a positive integer> mape (repmat ([1:5;5:9], [5 2]), repmat ([1:5;5:9], [5 2]), [1 -1])
%!error <DIM must be a positive integer> mape ([1:5], [1:5], ones (1,0))
%!error <WEIGHTS array must have the same size as 'A - F', when 'DIM> ...
%! mape (ones (5, 1), ones (5, 1), 3, 'Weights', ones (1, 5))
%!error <WEIGHTS vector must have the same length as the operating dimension> ...
%! mape ([1:5], [1:5], 2, 'Weights', [1, 2, 3])
%!error <WEIGHTS array must have the same size as the difference> ...
%! mape (ones (5, 3), ones (5, 3), 2, 'Weights', ones (3, 5))
%!error <VECDIM must contain non-repeating> mape ([1:5], [1:5], [2 2])
%!error <WEIGHTS array must have the same size as 'A - F', when 'all> ...
%! mape (ones (5, 1), ones (5, 1), [3, 4], 'Weights', ones (1, 5))
%!error <WEIGHTS vector must have the same length as the operating dimension> ...
%! mape ([1:5], [1:5], [1, 2], 'Weights', [1, 2, 3])
%!error <WEIGHTS array must have the same size as the difference> ...
%! mape (ones (5, 3), ones (5, 3), [1, 2], 'Weights', ones (3, 5))
%!error <WEIGHTS array must have the same size as the difference> ...
%! mape (ones (5, 3), ones (1, 3), [1, 2], 'Weights', ones (3, 5))
%!error <WEIGHTS array must have the same size as the operating page> ...
%! mape (ones (5, 3, 2), ones (5, 3, 2), [1, 2], 'Weights', ones (3, 5))
%!error <WEIGHTS array must have the same size as the operating page> ...
%! mape (ones (5, 3, 2), ones (5, 1, 2), [1, 2], 'Weights', ones (3, 5))
