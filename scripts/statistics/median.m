########################################################################
##
## Copyright (C) 1996-2024 The Octave Project Developers
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
## @deftypefn  {} {@var{m} =} median (@var{x})
## @deftypefnx {} {@var{m} =} median (@var{x}, @var{dim})
## @deftypefnx {} {@var{m} =} median (@var{x}, @var{vecdim})
## @deftypefnx {} {@var{m} =} median (@var{x}, "all")
## @deftypefnx {} {@var{m} =} median (@dots{}, @var{nanflag})
## @deftypefnx {} {@var{m} =} median (@dots{}, @var{outtype})
## Compute the median value of the elements of @var{x}.
##
## When the elements of @var{x} are sorted, say
## @code{@var{s} = sort (@var{x})}, the median is defined as
## @tex
## $$
## {\rm median} (x) =
##   \cases{s(\lceil N/2\rceil), & $N$ odd;\cr
##           (s(N/2)+s(N/2+1))/2, & $N$ even.}
## $$
## where $N$ is the number of elements of @var{x}.
##
## @end tex
## @ifnottex
##
## @example
## @group
##              |  @var{s}(ceil (N/2))          N odd
## median (@var{x}) = |
##              | (@var{s}(N/2) + @var{s}(N/2+1))/2   N even
## @end group
## @end example
##
## @end ifnottex
##
## If @var{x} is an array, then @code{median (@var{x})} operates along the
## first non-singleton dimension of @var{x}.
##
## The optional variable @var{dim} forces @code{median} to operate over the
## specified dimension, which must be a positive integer-valued number.
## Specifying any singleton dimension in @var{x}, including any dimension
## exceeding @code{ndims (@var{x})}, will result in a median equal to @var{x}.
##
## Specifying the dimensions as  @var{vecdim}, a vector of non-repeating
## dimensions, will return the median over the array slice defined by
## @var{vecdim}.  If @var{vecdim} indexes all dimensions of @var{x}, then it is
## equivalent to the option @qcode{"all"}.  Any dimension in @var{vecdim}
## greater than @code{ndims (@var{x})} is ignored.
##
## Specifying the dimension as @qcode{"all"} will force @code{median} to
## operate on all elements of @var{x}, and is equivalent to
## @code{median (@var{x}(:))}.
##
## @code{median (@dots{}, @var{outtype})} returns the median with a specified
## data type, using any of the input arguments in the previous syntaxes.
## @var{outtype} can take the following values:
##
## @table @asis
## @item @qcode{"default"}
## Output is of type double, unless the input is single in which case the
## output is of type single.
##
## @item @qcode{"double"}
## Output is of type double.
##
## @item @qcode{"native"}.
## Output is of the same type as the input (@code{class (@var{x})}), unless the
## input is logical in which case the output is of type double.
## @end table
##
## The optional variable @var{nanflag} specifies whether to include or exclude
## NaN values from the calculation using any of the previously specified input
## argument combinations.  The default value for @var{nanflag} is
## @qcode{"includenan"} which keeps NaN values in the calculation.  To
## exclude NaN values set the value of @var{nanflag} to @qcode{"omitnan"}.
## The output will still contain NaN values if @var{x} consists of all NaN
## values in the operating dimension.
##
## @seealso{mean, mode, movmedian}
## @end deftypefn

function m = median (x, varargin)

  if (nargin < 1 || nargin > 4)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("median: X must be either numeric or logical");
  endif

  ## Set initial conditions
  all_flag    = false;
  omitnan     = false;
  perm_flag   = false;
  out_flag    = false;
  vecdim_flag = false;
  dim         = [];

  nvarg = numel (varargin);
  varg_chars = cellfun ("ischar", varargin);
  szx = sz_out = size (x);
  ndx = ndims (x);
  outtype = class (x);

  if (nvarg > 1 && ! varg_chars(2:end))
    ## Only first varargin can be numeric
    print_usage ();
  endif

  ## Process any other char arguments.
  if (any (varg_chars))
    for argin = varargin(varg_chars)
      switch (lower (argin{1}))
        case "all"
          all_flag = true;

        case "omitnan"
          omitnan = true;

        case "includenan"
          omitnan = false;

        case "native"
          if (out_flag)
            error ("median: only one OUTTYPE can be specified");
          endif
          if (strcmp (outtype, "logical"))
            outtype = "double";
          endif
          out_flag = true;

        case "default"
          if (out_flag)
            error ("median: only one OUTTYPE can be specified");
          endif
          if (! strcmp (outtype, "single"))
            outtype = "double";
          endif
          out_flag = true;

        case "double"
          if (out_flag)
            error ("median: only one OUTTYPE can be specified");
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

  if ((nvarg == 1 && ! isnumeric (varargin{1})) || nvarg > 1)
    ## After trimming char inputs should only be one numeric varargin left
    print_usage ();
  endif

  ## Process special cases for in/out size
  if (nvarg > 0)
    ## dim or vecdim provided
    if (all_flag)
      error ("median: 'all' cannot be used with DIM or VECDIM options");
    endif

    dim = varargin{1};
    vecdim_flag = ! isscalar (dim);

    if (! (isvector (dim) && dim > 0) || any (rem (dim, 1)))
      error ("median: DIM must be a positive integer scalar or vector");
    endif

    ## Adjust sz_out, account for possible dim > ndx by appending singletons
    sz_out(ndx + 1 : max (dim)) = 1;
    sz_out(dim(dim <= ndx)) = 1;
    szx(ndx + 1 : max (dim)) = 1;

    if (vecdim_flag)
      ## vecdim - try to simplify first
      dim = sort (dim);
      if (! all (diff (dim)))
         error ("median: VECDIM must contain non-repeating positive integers");
      endif

      ## dims > ndims(x) and dims only one element long don't affect median
      sing_dim_x = find (szx != 1);
      dim(dim > ndx | szx(dim) == 1) = [];

      if (isempty (dim))
        ## No dims left to process, return input as output
        if (! strcmp (class (x), outtype))
          m = feval (outtype, x);  # convert to outtype
        else
          m = x;
        endif
        return;
      elseif (numel (dim) == numel (sing_dim_x)
              && unique ([dim, sing_dim_x]) == dim)
        ## If DIMs cover all nonsingleton ndims(x) it's equivalent to "all"
        ##   (check lengths first to reduce unique overhead if not covered)
        all_flag = true;
      endif
    endif

  else
    ## Dim not provided.  Determine scalar dimension.
    if (all_flag)
      ## Special case 'all': Recast input as dim1 vector, process as normal.
      x = x(:);
      szx = [numel(x), 1];
      dim = 1;
      sz_out = [1 1];

    elseif (isrow (x))
      ## Special case row vector: Avoid setting dim to 1.
      dim = 2;
      sz_out = [1, 1];

    elseif (ndx == 2 && szx == [0, 0])
      ## Special case []: Do not apply sz_out(dim)=1 change.
      dim = 1;
      sz_out = [1, 1];

    else
      ## General case: Set dim to first non-singleton, contract sz_out along dim
      (dim = find (szx != 1, 1)) || (dim = 1);
      sz_out(dim) = 1;
    endif
  endif

  if (isempty (x))
    ## Empty input - output NaN or class equivalent in pre-determined size
    switch (outtype)
      case {"double", "single"}
        m = NaN (sz_out, outtype);
      case ("logical")
        m = false (sz_out);
      otherwise
        m = cast (NaN (sz_out), outtype);
    endswitch
    return;
  endif

  if (all (isnan (x(:))))
    ## all NaN input, output single or double NaNs in pre-determined size
    m = NaN (sz_out, outtype);
    return;
  endif

  if (szx(dim) == 1)
    ## Operation along singleton dimension - nothing to do
    if (! strcmp (class (x), outtype))
      m = feval (outtype, x);  # convert to outtype
    else
      m = x;
    endif
    return;
  endif

  ## Permute dim to simplify all operations along dim1.  At func. end ipermute.
  if (numel (dim) > 1 || (dim != 1 && ! isvector (x)))
    perm = 1 : ndx;

    if (! vecdim_flag)
      ## Move dim to dim 1
      perm([1, dim]) = [dim, 1];
      x = permute (x, perm);
      szx([1, dim]) = szx([dim, 1]);
      dim = 1;

    else
      ## Move vecdims to front
      perm(dim) = [];
      perm = [dim, perm];
      x = permute (x, perm);

      ## Reshape all vecdims into dim1
      num_dim = prod (szx(dim));
      szx(dim) = [];
      szx = [num_dim, ones(1, numel(dim)-1), szx];
      x = reshape (x, szx);
      dim = 1;
    endif

    perm_flag = true;
  endif

  ## Find column locations of NaNs
  nanfree = ! any (isnan (x), dim);

  if (omitnan && nanfree(:))
    ## Don't use omitnan path if no NaNs are present.  Prevents any data types
    ## without a defined NaN from following slower omitnan codepath.
    omitnan = false;
  endif

  x = sort (x, dim); # Note: pushes any NaN's to end for omitnan compatibility

  if (omitnan)
    ## Ignore any NaN's in data.  Each operating vector might have a
    ## different number of non-NaN data points.

    if (isvector (x))
      ## Checks above ensure either dim1 or dim2 vector
      x = x(! isnan (x));
      n = numel (x);
      k = floor ((n + 1) / 2);
      if (mod (n, 2))
        ## odd
        m = x(k);
      else
        ## even
        m = (x(k) + x(k + 1)) / 2;
      endif

    else
      ## Each column may have a different n and k.  Force index column vector
      ## for consistent orientation for 2D and nD inputs, then use sub2ind to
      ## get correct element(s) for each column.

      n = sum (! isnan (x), 1)(:);
      k = floor ((n + 1) / 2);
      m_idx_odd = mod (n, 2) & n;
      m_idx_even = (! m_idx_odd) & n;

      m = NaN ([1, szx(2 : end)]);

      if (ndims (x) > 2)
        szx = [szx(1), prod(szx(2 : end))];
      endif

      ## Grab kth value, k possibly different for each column
      if (any (m_idx_odd))
        x_idx_odd = sub2ind (szx, k(m_idx_odd), find (m_idx_odd));
        m(m_idx_odd) = x(x_idx_odd);
      endif
      if (any (m_idx_even))
        k_even = k(m_idx_even);
        x_idx_even = sub2ind (szx, [k_even, k_even + 1], ...
                                (find (m_idx_even))(:, [1, 1]));
        m(m_idx_even) = sum (x(x_idx_even), 2) / 2;
      endif
    endif

  else
    ## No "omitnan".  All 'vectors' uniform length.
    ## All types without a NaN value will use this path.
    if (all (! nanfree))
      m = NaN (sz_out);

    else
      if (isvector (x))
        n = numel (x);
        k = floor ((n + 1) / 2);

        m = x(k);
        if (! mod (n, 2))
          ## Even
          if (any (isa (x, "integer")))
            ## avoid int overflow issues
            m2 = x(k + 1);
            if (sign (m) != sign (m2))
              m += m2;
              m /= 2;
            else
              m += (m2 - m) / 2;
            endif
          else
            m += (x(k + 1) - m) / 2;
          endif
        endif

      else
        ## Nonvector, all operations were permuted to be along dim 1
        n = szx(1);
        k = floor ((n + 1) / 2);

        if (isfloat (x))
          m = NaN ([1, szx(2 : end)]);
        else
          m = zeros ([1, szx(2 : end)], outtype);
        endif

        if (! mod (n, 2))
          ## Even
          if (any (isa (x, "integer")))
            ## avoid int overflow issues

            ## Use flattened index to simplify N-D operations
            m(1, :) = x(k, :);
            m2 = x(k + 1, :);

            samesign = prod (sign ([m(1, :); m2]), 1) == 1;
            m(1, :) = samesign .* m(1, :) + ...
                       (m2 + !samesign .* m(1, :) - samesign .* m(1, :)) / 2;

          else
            m(nanfree) = (x(k, nanfree) + x(k + 1, nanfree)) / 2;
          endif
        else
          ## Odd.  Use flattened index to simplify N-D operations
          m(nanfree) = x(k, nanfree);
        endif
      endif
    endif
  endif

  if (perm_flag)
    ## Inverse permute back to correct dimensions
    m = ipermute (m, perm);
  endif

  ## Convert output type as requested
  if (! strcmp (class (m), outtype))
    m = feval (outtype, m);
  endif

endfunction


%!assert (median (1), 1)
%!assert (median ([1, 2, 3]), 2)
%!assert (median ([1, 2, 3]'), 2)
%!assert (median (cat (3, 3, 1, 2)), 2)
%!assert (median ([3, 1, 2]), 2)
%!assert (median ([2, 4, 6, 8]), 5)
%!assert (median ([8, 2, 6, 4]), 5)
%!assert (median (single ([1, 2, 3])), single (2))
%!assert (median ([1, 2], 3), [1, 2])

%!test
%! x = [1, 2, 3, 4, 5, 6];
%! x2 = x';
%! y = [1, 2, 3, 4, 5, 6, 7];
%! y2 = y';
%!
%! assert (median (x) == median (x2) && median (x) == 3.5);
%! assert (median (y) == median (y2) && median (y) == 4);
%! assert (median ([x2, 2 * x2]), [3.5, 7]);
%! assert (median ([y2, 3 * y2]), [4, 12]);

## Test outtype option
%!test
%! in = [1, 2, 3];
%! out = 2;
%! assert (median (in, "default"), median (in));
%! assert (median (in, "default"), out);
%!test
%! in = single ([1, 2, 3]);
%! out = 2;
%! assert (median (in, "default"), single (median (in)));
%! assert (median (in, "default"), single (out));
%! assert (median (in, "double"), double (out));
%! assert (median (in, "native"), single (out));
%!test
%! in = uint8 ([1, 2, 3]);
%! out = 2;
%! assert (median (in, "default"), double (median (in)));
%! assert (median (in, "default"), double (out));
%! assert (median (in, "double"), out);
%! assert (median (in, "native"), uint8 (out));
%!test
%! in = logical ([1, 0, 1]);
%! out = 1;
%! assert (median (in, "default"), double (median (in)));
%! assert (median (in, "default"), double (out));
%! assert (median (in, "double"), double (out));
%! assert (median (in, "native"), double (out));

## Test single input and optional arguments "all", DIM, "omitnan"
%!test
%! x = repmat ([2 2.1 2.2 2 NaN; 3 1 2 NaN 5; 1 1.1 1.4 5 3], [1, 1, 4]);
%! y = repmat ([2 1.1 2 NaN NaN], [1, 1, 4]);
%! assert (median (x), y);
%! assert (median (x, 1), y);
%! y = repmat ([2, 1.1, 2, 3.5, 4], [1, 1, 4]);
%! assert (median (x, "omitnan"), y);
%! assert (median (x, 1, "omitnan"), y);
%! y = repmat ([2.05; 2.5; 1.4], [1, 1, 4]);
%! assert (median (x, 2, "omitnan"), y);
%! y = repmat ([NaN; NaN; 1.4], [1, 1, 4]);
%! assert (median (x, 2), y);
%! assert (median (x, "all"), NaN);
%! assert (median (x, "all", "omitnan"), 2);
%!assert (median (cat (3, 3, 1, NaN, 2), "omitnan"), 2)
%!assert (median (cat (3, 3, 1, NaN, 2), 3, "omitnan"), 2)

## Test boolean input
%!test
%! assert (median (true, "all"), logical (1));
%! assert (median (false), logical (0));
%! assert (median ([true, false, true]), true);
%! assert (median ([true, false, true], 2), true);
%! assert (median ([true, false, true], 1), logical ([1 0 1]));
%! assert (median ([true, false, NaN], 1), [1, 0, NaN]);
%! assert (median ([true, false, NaN], 2), NaN);
%! assert (median ([true, false, NaN], 2, "omitnan"), 0.5);
%! assert (median ([true, false, NaN], 2, "omitnan", "native"), double (0.5));

## Test dimension indexing with vecdim in n-dimensional arrays
%!test
%! x = repmat ([1:20; 6:25], [5, 2, 6, 3]);
%! assert (size (median (x, [3, 2])), [10, 1, 1, 3]);
%! assert (size (median (x, [1, 2])), [1, 1, 6, 3]);
%! assert (size (median (x, [1, 2, 4])), [1, 1, 6]);
%! assert (size (median (x, [1, 4, 3])), [1, 40]);
%! assert (size (median (x, [1, 2, 3, 4])), [1, 1]);

## Test exceeding dimensions
%!assert (median (ones (2, 2), 3), ones (2, 2))
%!assert (median (ones (2, 2, 2), 99), ones (2, 2, 2))
%!assert (median (magic (3), 3), magic (3))
%!assert (median (magic (3), [1, 3]), [4, 5, 6])
%!assert (median (magic (3), [1, 99]), [4, 5, 6])

## Test results with vecdim in N-dimensional arrays and "omitnan"
%!test
%! x = repmat ([2 2.1 2.2 2 NaN; 3 1 2 NaN 5; 1 1.1 1.4 5 3], [1, 1, 4]);
%! assert (median (x, [3, 2]), [NaN, NaN, 1.4]');
%! assert (median (x, [3, 2], "omitnan"), [2.05, 2.5, 1.4]');
%! assert (median (x, [1, 3]), [2, 1.1, 2, NaN, NaN]);
%! assert (median (x, [1, 3], "omitnan"), [2, 1.1, 2, 3.5, 4]);

## Test empty, NaN, Inf inputs
%!assert (median (NaN), NaN)
%!assert (median (NaN, "omitnan"), NaN)
%!assert (median (NaN (2)), [NaN, NaN])
%!assert (median (NaN (2), "omitnan"), [NaN, NaN])
%!assert (median ([1, NaN, 3]), NaN)
%!assert (median ([1, NaN, 3], 1), [1, NaN, 3])
%!assert (median ([1, NaN, 3], 2), NaN)
%!assert (median ([1, NaN, 3]'), NaN)
%!assert (median ([1, NaN, 3]', 1), NaN)
%!assert (median ([1, NaN, 3]', 2), [1; NaN; 3])
%!assert (median ([1, NaN, 3], "omitnan"), 2)
%!assert (median ([1, NaN, 3]', "omitnan"), 2)
%!assert (median ([1, NaN, 3], 1, "omitnan"), [1, NaN, 3])
%!assert (median ([1, NaN, 3], 2, "omitnan"), 2)
%!assert (median ([1, NaN, 3]', 1, "omitnan"), 2)
%!assert (median ([1, NaN, 3]', 2, "omitnan"), [1; NaN; 3])
%!assert (median ([1, 2, NaN, 3]), NaN)
%!assert (median ([1, 2, NaN, 3], "omitnan"), 2)
%!assert (median ([1,2,NaN;4,5,6;NaN,8,9]), [NaN, 5, NaN])
%!assert <*64011> (median ([1,2,NaN;4,5,6;NaN,8,9], "omitnan"), [2.5, 5, 7.5], eps)
%!assert (median ([1, 2 ; NaN, 4]), [NaN, 3])
%!assert (median ([1, 2 ; NaN, 4], "omitnan"), [1, 3])
%!assert (median ([1, 2 ; NaN, 4], 1, "omitnan"), [1, 3])
%!assert (median ([1, 2 ; NaN, 4], 2, "omitnan"), [1.5; 4], eps)
%!assert (median ([1, 2 ; NaN, 4], 3, "omitnan"), [1, 2 ; NaN, 4])
%!assert (median ([NaN, 2 ; NaN, 4]), [NaN, 3])
%!assert (median ([NaN, 2 ; NaN, 4], "omitnan"), [NaN, 3])
%!assert (median (ones (1, 0, 3)), NaN (1, 1, 3))

## Test all NaN vectors and arrays
%!assert <*65405> (median ([NaN, NaN], 1, "omitnan"), [NaN, NaN])
%!assert <*65405> (median ([NaN, NaN], 2, "omitnan"), NaN)
%!assert <*65405> (median ([NaN, NaN]', 1, "omitnan"), NaN)
%!assert <*65405> (median ([NaN, NaN]', 2, "omitnan"), [NaN; NaN])
%!assert <*65405> (median ([NaN, NaN], "omitnan"), NaN)
%!assert <*65405> (median ([NaN, NaN]', "omitnan"), NaN)
%!assert <*65405> (median (NaN (1, 9), 1, "omitnan"), NaN (1, 9))
%!assert <*65405> (median (NaN (1, 9), 2, "omitnan"), NaN)
%!assert <*65405> (median (NaN (1, 9), 3, "omitnan"), NaN (1, 9))
%!assert <*65405> (median (NaN (9, 1), 1, "omitnan"), NaN)
%!assert <*65405> (median (NaN (9, 1), 2, "omitnan"), NaN (9, 1))
%!assert <*65405> (median (NaN (9, 1), 3, "omitnan"), NaN (9, 1))
%!assert <*65405> (median (NaN (9, 2), 1, "omitnan"), NaN (1, 2))
%!assert <*65405> (median (NaN (9, 2), 2, "omitnan"), NaN (9, 1))
%!assert <*65405> (median (NaN (9, 2), "omitnan"), NaN (1, 2))

## Test single inputs
%!assert (median (NaN ("single")), NaN ("single"))
%!assert (median (NaN ("single"), "omitnan"), NaN ("single"))
%!assert (median (NaN ("single"), "double"), NaN ("double"))
%!assert (median (single ([1, 2 ; NaN, 4])), single ([NaN, 3]))
%!assert (median (single ([1, 2 ; NaN, 4]), "double"), double ([NaN, 3]))
%!assert (median (single ([1, 2 ; NaN, 4]), "omitnan"), single ([1, 3]))
%!assert (median (single ([1, 2 ; NaN, 4]), "omitnan", "double"), double ([1, 3]))
%!assert (median (single ([NaN, 2 ; NaN, 4]), "double"), double ([NaN 3]))
%!assert (median (single ([NaN, 2 ; NaN, 4]), "omitnan"), single ([NaN 3]))
%!assert (median (single ([NaN, 2 ; NaN, 4]), "omitnan", "double"), double ([NaN 3]))

## Test omitnan with 2D & 3D inputs to confirm correct sub2ind orientation
%!test <*64011>
%! x = [magic(3), magic(3)];
%! x([3, 7, 11, 12, 16, 17]) = NaN;
%! ynan = [NaN, 5, NaN, NaN, 5, NaN];
%! yomitnan = [5.5, 5, 4.5, 8, 5, 2];
%! assert (median (x), ynan);
%! assert (median (x, "omitnan"), yomitnan, eps);
%! assert (median (cat (3, x, x)), cat (3, ynan, ynan));
%! assert (median (cat (3, x, x), "omitnan"), cat (3, yomitnan, yomitnan), eps);

%!assert (median (Inf), Inf)
%!assert (median (-Inf), -Inf)
%!assert (median ([-Inf, Inf]), NaN)
%!assert (median ([3, Inf]), Inf)
%!assert (median ([3, 4, Inf]), 4)
%!assert (median ([Inf, 3, 4]), 4)
%!assert (median ([Inf, 3, Inf]), Inf)

%!assert (median ([]), NaN)
%!assert (median (ones (1, 0)), NaN) 
%!assert (median (ones (0, 1)), NaN)
%!assert (median ([], 1), NaN (1, 0))
%!assert (median ([], 2), NaN (0, 1))
%!assert (median ([], 3), NaN (0, 0))
%!assert (median (ones (1, 0), 1), NaN (1, 0))
%!assert (median (ones (1, 0), 2), NaN (1, 1))
%!assert (median (ones (1, 0), 3), NaN (1, 0))
%!assert (median (ones (0, 1), 1), NaN (1, 1))
%!assert (median (ones (0, 1), 2), NaN (0, 1))
%!assert (median (ones (0, 1), 3), NaN (0, 1))
%!assert (median (ones (0, 1, 0, 1), 1), NaN (1, 1, 0))
%!assert (median (ones (0, 1, 0, 1), 2), NaN (0, 1, 0))
%!assert (median (ones (0, 1, 0, 1), 3), NaN (0, 1, 1))
%!assert (median (ones (0, 1, 0, 1), 4), NaN (0, 1, 0))

## Test complex inputs (should sort by abs(a))
%!assert (median ([1, 3, 3i, 2, 1i]), 2)
%!assert (median ([1, 2, 4i; 3, 2i, 4]), [2, 1+1i, 2+2i])

## Test multidimensional arrays
%!shared a, b, x, y
%! old_state = rand ("state");
%! restore_state = onCleanup (@() rand ("state", old_state));
%! rand ("state", 2);
%! a = rand (2, 3, 4, 5);
%! b = rand (3, 4, 6, 5);
%! x = sort (a, 4);
%! y = sort (b, 3);
%!assert <*35679> (median (a, 4), x(:, :, :, 3))
%!assert <*35679> (median (b, 3), (y(:, :, 3, :) + y(:, :, 4, :))/2)
%!shared   # Clear shared to prevent variable echo for any later test failures

## Test N-dimensional arrays with odd non-NaN data points
%!test
%! x = ones (15, 1, 4);
%! x([13,15], 1, :) = NaN;
%! assert (median (x, 1, "omitnan"), ones (1, 1, 4))

## Test non-floating point types
%!assert (median ([true, false]), true)
%!assert (median (logical ([])), false)
%!assert (median (uint8 ([1, 3])), uint8 (2))
%!assert (median (uint8 ([])), uint8 (NaN))
%!assert (median (uint8 ([NaN 10])), uint8 (5))
%!assert (median (int8 ([1, 3, 4])), int8 (3))
%!assert (median (int8 ([])), int8 (NaN))
%!assert (median (single ([1, 3, 4])), single (3))
%!assert (median (single ([1, 3, NaN])), single (NaN))

## Test same sign int overflow when getting mean of even number of values
%!assert <*54567> (median (uint8 ([253, 255])), uint8 (254))
%!assert <*54567> (median (uint8 ([253, 254])), uint8 (254))
%!assert <*54567> (median (int8 ([127, 126, 125, 124; 1 3 5 9])), ...
%!                 int8 ([64 65 65 67]))
%!assert <*54567> (median (int8 ([127, 126, 125, 124; 1 3 5 9]), 2), ...
%!                 int8 ([126; 4]))
%!assert <*54567> (median (int64 ([intmax("int64"), intmax("int64")-2])), ...
%!                 intmax ("int64") - 1)
%!assert <*54567> (median ( ...
%!                 int64 ([intmax("int64"), intmax("int64")-2; 1 2]), 2), ...
%!                 int64([intmax("int64") - 1; 2]))
%!assert <*54567> (median (uint64 ([intmax("uint64"), intmax("uint64")-2])), ...
%!                 intmax ("uint64") - 1)
%!assert <*54567> (median ( ...
%!                 uint64 ([intmax("uint64"), intmax("uint64")-2; 1 2]), 2), ...
%!                 uint64([intmax("uint64") - 1; 2]))

## Test opposite sign int overflow when getting mean of even number of values
%!assert <*54567> (median (...
%! [intmin('int8'), intmin('int8')+5, intmax('int8')-5, intmax('int8')]), ...
%! int8 (-1))
%!assert <*54567> (median ([int8([1 2 3 4]); ...
%! intmin('int8'), intmin('int8')+5, intmax('int8')-5, intmax('int8')], 2), ...
%! int8 ([3;-1]))
%!assert <*54567> (median (...
%! [intmin('int64'), intmin('int64')+5, intmax('int64')-5, intmax('int64')]), ...
%! int64 (-1))
%!assert <*54567> (median ([int64([1, 2, 3, 4]); ...
%! intmin('int64'), intmin('int64')+5, intmax('int64')-5, intmax('int64')], 2), ...
%! int64 ([3;-1]))

## Test int accuracy loss doing mean of close int64/uint64 values as double
%!assert <*54567> (median ([intmax("uint64"), intmax("uint64")-2]), ...
%!                 intmax ("uint64")-1)
%!assert <*54567> (median ([intmax("uint64"), intmax("uint64")-2], "default"), ...
%!                 double (intmax ("uint64")-1))
%!assert <*54567> (median ([intmax("uint64"), intmax("uint64")-2], "double"), ...
%!                 double (intmax ("uint64")-1))
%!assert <*54567> (median ([intmax("uint64"), intmax("uint64")-2], "native"), ...
%!                 intmax ("uint64")-1)

## Test input case insensitivity
%!assert (median ([1 2 3], "aLL"), 2)
%!assert (median ([1 2 3], "OmitNan"), 2)
%!assert (median ([1 2 3], "DOUBle"), 2)

## Test input validation
%!error <Invalid call> median ()
%!error <Invalid call> median (1, 2, 3)
%!error <Invalid call> median (1, 2, 3, 4)
%!error <Invalid call> median (1, "all", 3)
%!error <Invalid call> median (1, "b")
%!error <Invalid call> median (1, 1, "foo")
%!error <'all' cannot be used with> median (1, 3, "all")
%!error <'all' cannot be used with> median (1, [2 3], "all")
%!error <X must be either numeric or logical> median ({1:5})
%!error <X must be either numeric or logical> median ("char")
%!error <only one OUTTYPE can be specified> median(1, "double", "native")
%!error <DIM must be a positive integer> median (1, ones (2,2))
%!error <DIM must be a positive integer> median (1, 1.5)
%!error <DIM must be a positive integer> median (1, 0)
%!error <DIM must be a positive integer> median ([1 2 3], [-1 1])
%!error <VECDIM must contain non-repeating> median(1, [1 2 2])
