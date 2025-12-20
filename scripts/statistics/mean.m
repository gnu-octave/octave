########################################################################
##
## Copyright (C) 1995-2025 The Octave Project Developers
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
## @deftypefn  {} {@var{m} =} mean (@var{x})
## @deftypefnx {} {@var{m} =} mean (@var{x}, @var{dim})
## @deftypefnx {} {@var{m} =} mean (@var{x}, @var{vecdim})
## @deftypefnx {} {@var{m} =} mean (@var{x}, "all")
## @deftypefnx {} {@var{m} =} mean (@dots{}, @var{nanflag})
## @deftypefnx {} {@var{m} =} mean (@dots{}, @var{outtype})
## @deftypefnx {} {@var{m} =} mean (@dots{}, @qcode{'Weights'}, @var{w})
## Compute the mean of the elements of @var{x}.
##
## The mean is defined as
## @tex
## $$ {\rm mean}(x) \equiv \bar{x} = {1\over N} \sum_{i=1}^N x_i $$
## where $N$ is the number of elements of @var{x}.
## @end tex
## @ifnottex
##
## @example
## mean (@var{x}) = SUM_i @var{x}(i) / N
## @end example
##
## @noindent
## where @math{N} is the number of elements in @var{x}.
##
## @end ifnottex
##
## The weighted mean is defined as
## @tex
## $$ {\rm mean_w}(x) \equiv \bar{x}_w = {\sum_{i=1}^N w_i x_i \over \sum_{i=1}^N w_i} $$
## where $N$ is the number of elements of @var{x}.
## @end tex
## @ifnottex
##
## @example
## weighted_mean (@var{x}) = SUM_i (@var{w}(i) * @var{x}(i)) / SUM_i (@var{w}(i)
## @end example
##
## @noindent
## where @math{N} is the number of elements in @var{x}.
##
## @end ifnottex
##
## If @var{x} is a vector, then @code{mean (@var{x})} returns the mean of the
## elements in @var{x}.
##
## If @var{x} is a matrix, then @code{mean (@var{x})} returns a row vector with
## each element containing the mean of the corresponding column in @var{x}.
##
## If @var{x} is an array, then @code{mean (@var{x})} computes the mean along
## the first non-singleton dimension of @var{x}.
##
## The optional input @var{dim} specifies the dimension to operate on and must
## be a positive integer.  Specifying any singleton dimension of @var{x},
## including any dimension exceeding @code{ndims (@var{x})}, will return
## @var{x}.
##
## Specifying multiple dimensions with input @var{vecdim}, a vector of
## non-repeating dimensions, will operate along the array slice defined by
## @var{vecdim}.  If @var{vecdim} indexes all dimensions of @var{x}, then it is
## equivalent to the option @qcode{"all"}.  Any dimension in @var{vecdim}
## greater than @code{ndims (@var{x})} is ignored.
##
## Specifying the dimension as @qcode{"all"} will cause @code{mean} to operate
## on all elements of @var{x}, and is equivalent to @code{mean (@var{x}(:))}.
##
## The optional input @var{outtype} specifies the data type that is returned.
## @var{outtype} can take the following values:
##
## @table @asis
## @item @qcode{'default'} : Output is of type double, unless the input is
## single in which case the output is of type single.
##
## @item @qcode{'double'} : Output is of type double.
##
## @item @qcode{'native'} : Output is of the same type as the input as reported
## by (@code{class (@var{x})}), unless the input is logical in which case the
## output is of type double.
## @end table
##
## The optional variable @var{nanflag} specifies whether to include or exclude
## NaN values from the calculation using any of the previously specified input
## argument combinations.  The default value for @var{nanflag} is
## @qcode{"includenan"} which keeps NaN values in the calculation.  To exclude
## NaN values set the value of @var{nanflag} to @qcode{"omitnan"}.  The output
## will still contain NaN values if @var{x} consists of all NaN values in the
## operating dimension.
##
## The optional argument pair @code{"Weights", @var{w}} specifies a weighting
## scheme @var{w}, which is applied to input @var{x}, so that @code{mean}
## computes the weighted mean.  When operating along a single dimension,
## @var{w} must be a vector of the same length as the operating dimension or it
## must have the same size as @var{x}.  When operating over an array slice
## defined by @var{vecdim}, @var{w} must have the same size as the operating
## array slice, i.e., @code{size (w) == size (x)(@var{vecdim})}, or the same
## size as @var{x}.
##
## @seealso{median, mode, movmean}
## @end deftypefn

function m = mean (x, varargin)

  if (nargin < 1 || nargin > 6)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("mean: X must be a numeric or logical array");
  endif

  ## Initialize flags
  all_flag = false;
  omitnan  = false;
  out_flag = false;
  weighted = false;

  ## Process paired argument for Weights
  w_idx = find (cellfun (@(x) strcmpi (x, "weights"), varargin));
  if (! isempty (w_idx))
    if (numel (varargin) > w_idx)
      w = varargin{w_idx+1};
      if (! (isnumeric (w) && any (isa (w, {'double', 'single'}))))
        error ("mean: WEIGHTS must be single or double");
      endif
      if (any (w(:) < 0))
        error ("mean: WEIGHTS must be nonnegative");
      endif
    else
      error ("mean: paired input argument for 'Weights' is missing");
    endif
    weighted = true;
    varargin([w_idx, w_idx+1]) = [];
  endif

  nvarg = numel (varargin);
  varg_chars = cellfun ("ischar", varargin);
  outtype = "default";
  szx = size (x);
  ndx = ndims (x);

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

        case "default"
          if (out_flag)
            error ("mean: only one OUTTYPE can be specified");
          endif
          if (isa (x, "single"))
            outtype = "single";
          else
            outtype = "double";
          endif
          out_flag = true;

        case "native"
          if (out_flag)
            error ("mean: only one OUTTYPE can be specified");
          endif
          outtype = class (x);
          if (strcmp (outtype, "logical"))
            outtype = "double";
          endif
          out_flag = true;

        case "double"
          if (out_flag)
            error ("mean: only one OUTTYPE can be specified");
          endif
          outtype = "double";
          out_flag = true;

        otherwise
          print_usage ();

      endswitch
    endfor

    varargin(varg_chars) = [];
    nvarg = numel (varargin);
  endif

  if (strcmp (outtype, "default"))
    if (isa (x, "single"))
      outtype = "single";
    else
      outtype = "double";
    endif
  endif

  if (nvarg == 1 && ! isnumeric (varargin{1}))
    ## After trimming char inputs only one arg can be left, must be numeric.
    print_usage ();
  endif

  ## Process special cases of input/output sizes.
  if (nvarg == 0)
    ## Single numeric input argument, no dimensions given.
    if (all_flag)
      x = x(:);
      n = numel (x);

      ## Process weights
      if (weighted)
        if (isvector (w))
          if (numel (w) != n)
            error (strcat ("mean: WEIGHTS vector must have the same", ...
                           " length as the operating dimension"));
          endif
        elseif (! isequal (size (w), szx))
          error ("mean: WEIGHTS array must have the same dimensions with X");
        endif
        w = w(:);
        x = w .* double (x);
        n = sum (w);
      endif

      if (omitnan)
        nanx = isnan (x);
        x(nanx) = [];
        if (weighted)
          w(nanx) = [];
          n = sum (w);
        else
          n = numel (x);
        endif
      endif

      if (any (isa (x, {"int64", "uint64"})))
        m = int64_mean (x, 1, n, outtype);
      else
        m = sum (x, "extra") ./ n;
      endif

    else
      ## Handle 0x0 empty input, no dimensions given
      if (ndx == 2 && isempty (x) && szx == [0,0])
        if (isa (x, "single"))
          m = NaN ("single");
        else
          m = NaN;
        endif
        return;
      endif

      ## Find the first non-singleton dimension.
      (dim = find (szx != 1, 1)) || (dim = 1);
      n = szx(dim);

      ## Process weights
      if (weighted)
        if (isvector (w))
          if (numel (w) != n)
            error (strcat ("mean: WEIGHTS vector must have the same", ...
                           " length as the operating dimension"));
          endif
          szw = ones (1, ndx);
          szw(dim) = n;
          w = reshape (w, szw);
          ## Force x to doubles to avoid integer path
          x = w .* double (x);
        elseif (! isequal (size (w), szx))
          error ("mean: WEIGHTS array must have the same dimensions with X");
        else
          x = w .* double (x);
        endif
        n = sum (w, dim);
      endif

      ## Process omitnan
      if (omitnan)
        nanx = isnan (x);
        x(nanx) = 0;
        if (weighted)
          w(nanx) = 0;
          n = sum (w, dim);
        else
          n = sum (! nanx, dim);
        endif
      endif

      if (any (isa (x, {"int64", "uint64"})))
        m = int64_mean (x, dim, n, outtype);
      else
        m = sum (x, dim, "extra") ./ n;
      endif

    endif

  else
    ## Two numeric input arguments, dimensions given.  Note scalar is vector!
    vecdim = varargin{1};
    if (isempty (vecdim) || ! (isvector (vecdim) && isindex (vecdim)))
      error ("mean: DIM must be a positive integer scalar or vector");
    endif

    if (isscalar (vecdim))
      dim = vecdim;  # alias for code readability
      if (dim > ndx)
        m = x;
      else
        n = szx(dim);

        ## Process weights
        if (weighted)
          if (isvector (w))
            if (numel (w) != n)
              error (strcat ("mean: WEIGHTS vector must have the same", ...
                             " length as the operating dimension"));
            endif
            szw = ones (1, ndx);
            szw(dim) = n;
            w = reshape (w, szw);
            ## Force x to doubles to avoid integer path
            x = w .* double (x);
          elseif (! isequal (size (w), szx))
            error ("mean: WEIGHTS array must have the same dimensions with X");
          else
            x = w .* double (x);
          endif
          n = sum (w, dim);
        endif

        ## Process omitnan
        if (omitnan)
          nanx = isnan (x);
          x(nanx) = 0;
          if (weighted)
            w(nanx) = 0;
            n = sum (w, dim);
          else
            n = sum (! nanx, dim);
          endif
        endif

        if (any (isa (x, {"int64", "uint64"})))
          m = int64_mean (x, dim, n, outtype);
        else
          m = sum (x, dim, "extra") ./ n;
        endif

      endif

    else
      vecdim = sort (vecdim);
      if (! all (diff (vecdim)))
         error ("mean: VECDIM must contain non-repeating positive integers");
      endif
      ## Ignore dimensions in VECDIM larger than actual array.
      vecdim(vecdim > ndx) = [];

      if (isempty (vecdim))
        m = x;
      else

        ## Calculate permutation vector
        remdims = 1:ndx;       # All dimensions
        remdims(vecdim) = [];  # Delete dimensions specified by vecdim
        nremd = numel (remdims);

        ## If all dimensions are given, it is equivalent to 'all' flag
        if (nremd == 0)
          x = x(:);
          n = numel (x);

          ## Process weights
          if (weighted)
            if (isvector (w))
              if (numel (w) != n)
                error (strcat ("mean: WEIGHTS vector must have the same", ...
                               " length as the operating dimension"));
              endif
            elseif (! isequal (size (w), szx))
              error ("mean: WEIGHTS array must have the same dimensions with X");
            endif
            w = w(:);
            x = w .* double (x);
            n = sum (w);
          endif

          if (omitnan)
            nanx = isnan (x);
            x(nanx) = [];
            if (weighted)
              w(nanx) = [];
              n = sum (w);
            else
              n = numel (x);
            endif
          endif

          if (any (isa (x, {"int64", "uint64"})))
            m = int64_mean (x, 1, n, outtype);
          else
            m = sum (x, "extra") ./ n;
          endif

        else
          ## Weights must either match vecdim page size or the size of input X
          if (weighted)
            page_szw = size (w);
            if (isequal (page_szw, szx(vecdim)))
              szw = ones (1, ndx);
              szw(vecdim) = page_szw;
              w = reshape (w, szw);
              ## Force x to doubles to avoid integer path
              x = w .* double (x);
            elseif (! isequal (size (w), szx))
              error (strcat ("mean: WEIGHTS array must have the same", ...
                             " dimensions as input X or its operating", ...
                             " page specified by VECDIM"));
            else
              x = w .* double (x);
            endif
          endif

          ## Permute to push vecdims to back
          perm = [remdims, vecdim];
          x = permute (x, perm);

          ## Reshape to squash all vecdims in final dimension
          sznew = [szx(remdims), prod(szx(vecdim))];
          x = reshape (x, sznew);
          if (weighted)
            sznew = [ones(1, numel (szx(remdims))), prod(szx(vecdim))];
            w = reshape (w, sznew);
          endif

          ## Calculate mean on final dimension
          dim = nremd + 1;
          if (omitnan)
            nanx = isnan (x);
            x(nanx) = 0;
            if (weighted)
              w(nanx) = 0;
              n = sum (w, dim);
            else
              n = sum (! nanx, dim);
            endif
          else
            if (weighted)
              n = sum (w, dim);
            else
              n = sznew(dim);
            endif
          endif

          if (any (isa (x, {"int64", "uint64"})))
            m = int64_mean (x, dim, n, outtype);
          else
            m = sum (x, dim, "extra") ./ n;
          endif

          ## Inverse permute back to correct dimensions
          m = ipermute (m, perm);

        endif
      endif
    endif
  endif

  ## Convert output if necessary
  if (! strcmp (class (m), outtype))
    if (! islogical (x))
      m = feval (outtype, m);
    endif
  endif

endfunction

function m = int64_mean (x, dim, n, outtype)
    ## Avoid int overflow in large ints.  Smaller ints processed as double
    ## avoids overflow but large int64 values have floating pt error as double.
    ## Use integer math and manual remainder correction to avoid this.
    if (any (abs (x(:)) >= flintmax / n))
      rmdr = double (rem (x, n)) / n;
      rmdr_hilo = logical (int8 (rmdr)); # Integer rounding direction indicator

      ## Do 'native' int summation to prevent double precision error,
      ## then add back in lost round-up/down remainders.

      m = sum (x/n, dim, "native");

      ## rmdr.*!rmdr_hilo = remainders that were rounded down in abs val
      ## signs retained, can be summed and added back.
      ## rmdr.*rmdr_hilo = remainders that were rounded up in abs val.
      ## need to add back difference between 1 and rmdr, retaining sign.

      rmdr = sum (rmdr .* !rmdr_hilo, dim) - ...
                sum ((1 - abs (rmdr)) .* rmdr_hilo .* sign (rmdr), dim);

      if (any (abs (m(:)) >= flintmax))
        ## Avoid float errors when combining for large m.
        ## FIXME: may also need to include checking rmdr for large numel (x),
        ##        as its value could be on the order of numel (x).
        if (any (strcmp (outtype, {"int64", "uint64"})))
          m += rmdr;
        else
          m = double (m) + rmdr;
        endif

      else
        m = double (m) + rmdr;
        switch (outtype)
          case "int64"
            m = int64 (m);
          case "uint64"
            m = uint64 (m);
        endswitch
      endif
    else
      m = double (sum (x, dim, "native")) ./ n;
    endif

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
%! assert (mean (in, "double"), out);
%! assert (mean (in, "native"), out);

%!test
%! in = single ([1 2 3]);
%! out = 2;
%! assert (mean (in, "default"), mean (in));
%! assert (mean (in, "default"), single (out));
%! assert (mean (in, "double"), out);
%! assert (mean (in, "native"), single (out));

%!test
%! in = logical ([1 0 1]);
%! out = 2/3;
%! assert (mean (in, "default"), mean (in), eps);
%! assert (mean (in, "default"), out, eps);
%! assert (mean (in, "double"), out, eps);
%! assert (mean (in, "native"), out, eps);

%!test
%! in = uint8 ([1 2 3]);
%! out = 2;
%! assert (mean (in, "default"), mean (in));
%! assert (mean (in, "default"), out);
%! assert (mean (in, "double"), out);
%! assert (mean (in, "native"), uint8 (out));

%!test
%! in = uint8 ([0 1 2 3]);
%! out = 1.5;
%! out_u8 = 2;
%! assert (mean (in, "default"), mean (in), eps);
%! assert (mean (in, "default"), out, eps);
%! assert (mean (in, "double"), out, eps);
%! assert (mean (in, "native"), uint8 (out_u8));
%! assert (class (mean (in, "native")), "uint8");

%!test # internal sum exceeding intmax
%! in = uint8 ([3 141 141 255]);
%! out = 135;
%! assert (mean (in, "default"), mean (in));
%! assert (mean (in, "default"), out);
%! assert (mean (in, "double"), out);
%! assert (mean (in, "native"), uint8 (out));
%! assert (class (mean (in, "native")), "uint8");

%!test # fractional answer with internal sum exceeding intmax
%! in = uint8 ([1 141 141 255]);
%! out = 134.5;
%! out_u8 = 135;
%! assert (mean (in, "default"), mean (in));
%! assert (mean (in, "default"), out);
%! assert (mean (in, "double"), out);
%! assert (mean (in, "native"), uint8 (out_u8));
%! assert (class (mean (in, "native")), "uint8");

%!test <54567> # large int64 sum exceeding intmax and double precision limit
%! in_same = uint64 ([intmax("uint64") intmax("uint64")-2]);
%! out_same = intmax ("uint64")-1;
%! in_opp = int64 ([intmin("int64"), intmax("int64")-1]);
%! out_opp = -1;
%! in_neg = int64 ([intmin("int64") intmin("int64")+2]);
%! out_neg = intmin ("int64")+1;
%!
%! ## both positive
%! assert (mean (in_same, "default"), mean (in_same));
%! assert (mean (in_same, "default"), double (out_same));
%! assert (mean (in_same, "double"), double (out_same));
%! assert (mean (in_same, "native"), uint64 (out_same));
%! assert (class (mean (in_same, "native")), "uint64");
%!
%! ## opposite signs
%! assert (mean (in_opp, "default"), mean (in_opp));
%! assert (mean (in_opp, "default"), double (out_opp));
%! assert (mean (in_opp, "double"), double (out_opp));
%! assert (mean (in_opp, "native"), int64 (out_opp));
%! assert (class (mean (in_opp, "native")), "int64");
%!
%! ## both negative
%! assert (mean (in_neg, "default"), mean (in_neg));
%! assert (mean (in_neg, "default"), double (out_neg));
%! assert (mean (in_neg, "double"), double (out_neg));
%! assert (mean (in_neg, "native"), int64 (out_neg));
%! assert (class (mean (in_neg, "native")), "int64");

## Additional tests int64 and double precision limits
%!test <54567>
%! in = [(intmin('int64')+5), (intmax('int64'))-5];
%! assert (mean (in, "native"), int64 (-1));
%! assert (class (mean (in, "native")), "int64");
%! assert (mean (double(in)), double (0) );
%! assert (mean (in), double (-0.5) );
%! assert (mean (in, "default"), double (-0.5) );
%! assert (mean (in, "double"), double (-0.5) );
%! assert (mean (in, "all", "native"), int64 (-1));
%! assert (mean (in, 2, "native"), int64 (-1));
%! assert (mean (in, [1 2], "native"), int64 (-1));
%! assert (mean (in, [2 3], "native"), int64 (-1));
%! assert (mean ([intmin("int64"), in, intmax("int64")]), double (-0.5));
%! assert (mean ([in; int64([1 3])], 2, "native"), int64 ([-1; 2]));

## Test input and optional arguments "all", DIM, "omitnan".
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
%! assert (mean (z, "all", "includenan"), NaN);
%! assert (mean (z, "all", "omitnan"), 20.03225806451613, 4e-14);
%! m = [20 NaN 15];
%! assert (mean (z'), m);
%! assert (mean (z', "includenan"), m);
%! m = [20 25.35 15];
%! assert (mean (z', "omitnan"), m);
%! assert (mean (z, 2, "omitnan"), m');
%! assert (mean (z, 2, "native", "omitnan"), m');
%! assert (mean (z, 2, "omitnan", "native"), m');

## Test logical input
%!test
%! assert (mean (true, "all"), 1);
%! assert (mean (false), 0);
%! assert (mean ([true false true]), 2/3, 4e-14);
%! assert (mean ([true false true], 1), [1 0 1]);
%! assert (mean ([true false NaN], 1), [1 0 NaN]);
%! assert (mean ([true false NaN], 2), NaN);
%! assert (mean ([true false NaN], 2, "omitnan"), 0.5);
%! assert (mean ([true false NaN], 2, "omitnan", "native"), 0.5);

## Test NaN inputs
%!test
%! x = magic (4);
%! x([2, 9:12]) = NaN;
%! assert (mean (x), [NaN 8.5, NaN, 8.5], eps);
%! assert (mean (x,1), [NaN 8.5, NaN, 8.5], eps);
%! assert (mean (x,2), NaN (4,1), eps);
%! assert (mean (x,3), x, eps);
%! assert (mean (x, 'omitnan'), [29/3, 8.5, NaN, 8.5], eps);
%! assert (mean (x, 1, 'omitnan'), [29/3, 8.5, NaN, 8.5], eps);
%! assert (mean (x, 2, 'omitnan'), [31/3; 9.5; 28/3; 19/3], eps);
%! assert (mean (x, 3, 'omitnan'), x, eps);

## Test empty inputs
%!assert (mean ([]), NaN (1, 1))
%!assert (mean (single ([])), NaN (1, 1, "single"))
%!assert (mean ([], 1), NaN (1, 0))
%!assert (mean ([], 2), NaN (0, 1))
%!assert (mean ([], 3), NaN (0, 0))
%!assert (mean (ones (1,0)), NaN (1,1))
%!assert (mean (ones (1,0), 1), NaN (1,0))
%!assert (mean (ones (1,0), 2), NaN (1,1))
%!assert (mean (ones (1,0), 3), NaN (1,0))
%!assert (mean (ones (0,1)), NaN (1,1))
%!assert (mean (ones (0,1), 1), NaN (1,1))
%!assert (mean (ones (0,1), 2), NaN (0,1))
%!assert (mean (ones (0,1), 3), NaN (0,1))
%!assert (mean (ones (0,1,0)), NaN (1,1,0))
%!assert (mean (ones (0,1,0), 1), NaN (1,1,0))
%!assert (mean (ones (0,1,0), 2), NaN (0,1,0))
%!assert (mean (ones (0,1,0), 3), NaN (0,1))
%!assert (mean (ones (0,0,1,0)), NaN (1,0,1,0))
%!assert (mean (ones (0,0,1,0), 1), NaN (1,0,1,0))
%!assert (mean (ones (0,0,1,0), 2), NaN (0,1,1,0))
%!assert (mean (ones (0,0,1,0), 3), NaN (0,0,1,0))

## Test dimension indexing with vecdim in N-dimensional arrays
%!test
%! x = repmat ([1:20;6:25], [5 2 6 3]);
%! assert (size (mean (x, [3 2])), [10 1 1 3]);
%! assert (size (mean (x, [1 2])), [1 1 6 3]);
%! assert (size (mean (x, [1 2 4])), [1 1 6]);
%! assert (size (mean (x, [1 4 3])), [1 40]);
%! assert (size (mean (x, [1 2 3 4])), [1 1]);

## Test exceeding dimensions
%!assert (mean (ones (2,2), 3), ones (2,2))
%!assert (mean (ones (2,2,2), 99), ones (2,2,2))
%!assert (mean (magic (3), 3), magic (3))
%!assert (mean (magic (3), [1 3]), [5, 5, 5])
%!assert (mean (magic (3), [1 99]), [5, 5, 5])

## Test results with vecdim in N-dimensional arrays and "omitnan"
%!test
%! x = repmat ([1:20;6:25], [5 2 6 3]);
%! m = repmat ([10.5;15.5], [5 1 1 3]);
%! assert (mean (x, [3 2]), m, 4e-14);
%! x(2,5,6,3) = NaN;
%! m(2,1,1,3) = NaN;
%! assert (mean (x, [3 2]), m, 4e-14);
%! m(2,1,1,3) = 15.52301255230125;
%! assert (mean (x, [3 2], "omitnan"), m, 4e-14);

## Test input case insensitivity
%!assert (mean ([1 2 3], "aLL"), 2)
%!assert (mean ([1 2 3], "OmitNan"), 2)
%!assert (mean ([1 2 3], "DOUBle"), 2)

## Test limits of single precision summation limits on each code path
%!assert <*63848> (mean (ones (80e6, 1, "single")), 1, eps)
%!assert <*63848> (mean (ones (80e6, 1, "single"), "all"), 1, eps)
%!assert <*63848> (mean (ones (80e6, 1, "single"), 1), 1, eps)
%!assert <*63848> (mean (ones (80e6, 1, "single"), [1 2]), 1, eps)
%!assert <*63848> (mean (ones (80e6, 1, "single"), [1 3]), 1, eps)

## Test 'Weights' with DIM, VECDIM, and "all" options
%!test
%! x = [1, 1; 7, 9; 1, 9; 1, 9; 6, 2];
%! w = [1; 2; 1; 2; 3];
%! assert (mean (x, 'Weights', w), [4, 5.7778], 1e-4);
%! assert (mean (x, 'Weights', w'), [4, 5.7778], 1e-4);
%! assert (mean (x, 1, 'Weights', w), [4, 5.7778], 1e-4);
%! assert (mean (x, 1, 'Weights', w'), [4, 5.7778], 1e-4);
%! assert (mean (x', 2, 'Weights', w), [4; 5.7778], 1e-4);
%! assert (mean (x', 2, 'Weights', w'), [4; 5.7778], 1e-4);
%!test
%! x = [1, 1; 7, 9; 1, 9; 1, 9; 6, 2];
%! x = reshape (x', [1, 2, 5]);
%! w = [1, 2, 1, 2, 3];
%! assert (size (x)([1, 3]), size (w));
%! assert (mean (x, [1, 3], 'Weights', w), [4, 5.7778], 1e-4);
%!test
%! x = [1, 1; 7, 9; 1, 9; 1, 9; 6, 2];
%! x = reshape (x', [1, 2, 5]);
%! w = ones (1, 2, 5);
%! assert (size (x), size (w));
%! assert (mean (x, 'all', 'Weights', w), mean (x, 'all'), 1e-4);
%! assert (mean (x, [1, 2, 3], 'Weights', w), mean (x, [1, 2, 3]), 1e-4);

## Test 'Weights' with 'omitnan' and outtype options
%!test
%! x = [1, 1; 7, 9; 1, 9; 1, 9; 6, 2];
%! w = [1; 2; 1; 2; 3];
%! assert (mean (x, 'omitnan', 'Weights', w), [4, 5.7778], 1e-4);
%! assert (mean (x, 1, 'omitnan', 'Weights', w), [4, 5.7778], 1e-4);
%! assert (mean (x', 2,'omitnan',  'Weights', w), [4; 5.7778], 1e-4);
%!test
%! x = [1, 1; 7, NaN; 1, 9; 1, 9; 6, 2];
%! x = reshape (x', [1, 2, 5]);
%! w = ones (1, 2, 5);
%! assert (size (x), size (w));
%! assert (mean (x, 'all', 'omitnan', 'Weights', w), mean (x, 'all', 'omitnan'), 1e-4);
%! assert (mean (x, [1, 2, 3], 'omitnan', 'Weights', w), mean (x, [1, 2, 3], 'omitnan'), 1e-4);
%!test
%! x = single ([1, 1; 7, 9; 1, 9; 1, 9; 6, 2]);
%! w = [1; 2; 1; 2; 3];
%! assert (class (mean (x, 'Weights', w)), "single");
%!test
%! x = int32 ([1, 1; 7, 9; 1, 9; 1, 9; 6, 2]);
%! w = [1; 2; 1; 2; 3];
%! assert (class (mean (x, 'native', 'Weights', w)), "int32");

## Test limits of double precision summation
%!assert <63848> (mean ([flintmax("double"), ones(1, 2^8-1, "double")]), ...
%!                35184372088833-1/(2^8), eps (35184372088833))
%!assert (mean (sparse ([flintmax("double"), ones(1, 2^8-1, "double")])), ...
%!        sparse (35184372088833-1/(2^8)), eps (35184372088833))
## Test limits of double precision summation with "all", DIM, and VECDIM options
%!assert (mean ([flintmax("double"), ones(1, 2^8-1, "double")], "all"), ...
%!                35184372088833-1/(2^8), eps (35184372088833))
%!assert (mean (sparse ([flintmax("double"), ones(1, 2^8-1, "double")]), "all"), ...
%!        sparse (35184372088833-1/(2^8)), eps (35184372088833))
%!assert (mean ([flintmax("double"), ones(1, 2^8-1, "double")], 2), ...
%!                35184372088833-1/(2^8), eps (35184372088833))
%!assert (mean (sparse ([flintmax("double"), ones(1, 2^8-1, "double")]), 2), ...
%!        sparse (35184372088833-1/(2^8)), eps (35184372088833))
%!assert (mean (reshape ([flintmax("double"), ones(1, 2^8-1, "double")], ...
%!                       1, 2, (2^8)/2), [2, 3]), ...
%!        35184372088833-1/(2^8), eps (35184372088833))

## Test sparse input
%!test
%! x = speye (3);
%! assert (mean (x(:)), sparse (1/3));
%! assert (mean (x, "all"), sparse (1/3));
%! assert (mean (x, [1, 2]), sparse (1/3));
%! assert (mean (x), sparse ([1/3, 1/3, 1/3]));
%! assert (mean (x, 2), sparse ([1/3; 1/3; 1/3]));
%! assert (mean (x, 3), x);

## Test input validation
%!error <Invalid call> mean ()
%!error <Invalid call> mean (1, 2, 3, 4, 5)
%!error <X must be a numeric or logical array> mean ({1:5})
%!error <WEIGHTS must be single or double> mean (1, 'Weights', 'double')
%!error <WEIGHTS must be single or double> mean (1, 'Weights', uint8 (1))
%!error <WEIGHTS must be nonnegative> mean (1, 'Weights', -1)
%!error <paired input argument for 'Weights' is missing> mean (1, 'Weights')
%!error <Invalid call> mean (1, 2, 3)
%!error <Invalid call> mean (1, "all", 3)
%!error <only one OUTTYPE> mean (1, 'native', 'default')
%!error <only one OUTTYPE> mean (1, 'default', 'native')
%!error <only one OUTTYPE> mean (1, 'default', 'double')
%!error <Invalid call> mean (1, 'foobar')
%!error <WEIGHTS vector must have the same length as the operating dimension> ...
%! mean ([1:5]', "all", 'Weights', [1, 2, 3])
%!error <WEIGHTS array must have the same dimensions with X> ...
%! mean (ones (5, 3), "all", 'Weights', ones (3, 5))
%!error <WEIGHTS vector must have the same length as the operating dimension> ...
%! mean ([1:5]', 'Weights', [1, 2, 3])
%!error <WEIGHTS vector must have the same length as the operating dimension> ...
%! mean (ones (5, 3), 'Weights', [1, 2, 3])
%!error <WEIGHTS array must have the same dimensions with X> ...
%! mean (ones (5, 3), 'Weights', ones (3, 5))
%!error <WEIGHTS vector must have the same length as the operating dimension> ...
%! mean ([1:5]', [1, 2], 'Weights', [1, 2, 3])
%!error <WEIGHTS array must have the same dimensions with X> ...
%! mean (ones (5, 3), [1, 2], 'Weights', ones (3, 5))
%!error <WEIGHTS array must have the same dimensions as input X or its> ...
%! mean (ones (5, 3, 2), [1, 2], 'Weights', ones (3, 5))
%!error <DIM must be a positive integer> mean (1, ones (2,2))
%!error <DIM must be a positive integer> mean (1, 1.5)
%!error <DIM must be a positive integer> mean (1, 0)
%!error <DIM must be a positive integer> mean (1, [])
%!error <DIM must be a positive integer> mean (1, -1)
%!error <DIM must be a positive integer> mean (1, -1.5)
%!error <DIM must be a positive integer> mean (1, NaN)
%!error <DIM must be a positive integer> mean (1, Inf)
%!error <DIM must be a positive integer> mean (repmat ([1:20;6:25], [5 2]), -1)
%!error <DIM must be a positive integer> mean (repmat ([1:5;5:9], [5 2]), [1 -1])
%!error <DIM must be a positive integer> mean (1, ones (1,0))
%!error <VECDIM must contain non-repeating> mean (1, [2 2])
