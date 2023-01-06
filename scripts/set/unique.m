########################################################################
##
## Copyright (C) 2000-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{y} =} unique (@var{x})
## @deftypefnx {} {@var{y} =} unique (@var{x}, "rows")
## @deftypefnx {} {@var{y} =} unique (@dots{}, "sorted")
## @deftypefnx {} {@var{y} =} unique (@dots{}, "stable")
## @deftypefnx {} {[@var{y}, @var{i}, @var{j}] =} unique (@dots{})
## @deftypefnx {} {[@var{y}, @var{i}, @var{j}] =} unique (@dots{}, "first")
## @deftypefnx {} {[@var{y}, @var{i}, @var{j}] =} unique (@dots{}, "last")
## @deftypefnx {} {[@var{y}, @var{i}, @var{j}] =} unique (@dots{}, "legacy")
## Return the unique elements of @var{x}.
##
## If the input @var{x} is a column vector then return a column vector;
## Otherwise, return a row vector.  @var{x} may also be a cell array of
## strings.
##
## If the optional argument @qcode{"rows"} is given then return the unique
## rows of @var{x}.  The input must be a 2-D numeric matrix to use this option.
##
## The optional argument @qcode{"sorted"}/@qcode{"stable"} controls the order
## in which unique values appear in the output.  The default is
## @qcode{"sorted"} and values in the output are placed in ascending order.
## The alternative @qcode{"stable"} preserves the order found in the input
## @var{x}.
##
## If requested, return column index vectors @var{i} and @var{j} such that
## @code{@var{y} = @var{x}(@var{i})} and @code{@var{x} = @var{y}(@var{j})}.
##
## Additionally, if @var{i} is a requested output then one of the flags
## @qcode{"first"} or @qcode{"last"} may be given.  If @qcode{"last"} is
## specified, return the highest possible indices in @var{i}, otherwise, if
## @qcode{"first"} is specified, return the lowest.  The default is
## @qcode{"first"}.
##
## Example 1 : sort order
##
## @example
## @group
## unique ([3, 1, 1, 2])
## @result{} [1, 2, 3]
## unique ([3, 1, 1, 2], "stable")
## @result{} [3, 1, 2]
## @end group
## @end example
##
## Example 2 : index selection
##
## @example
## @group
## [~, @var{i}] = unique ([3, 1, 1, 2], "first")
## @result{} @var{i} = [2; 4; 1]
## [~, @var{i}] = unique ([3, 1, 1, 2], "last")
## @result{} @var{i} = [3; 4; 1]
## @end group
## @end example
##
## Programming Notes: The input flag @qcode{"legacy"} changes the algorithm
## to be compatible with @sc{matlab} releases prior to R2012b.  Specifically,
## The index ordering flag is changed to @qcode{"last"}, and the shape of the
## outputs @var{i}, @var{j} will follow the shape of the input @var{x} rather
## than always being column vectors.
##
## The third output, @var{j}, has not been implemented yet when the sort
## order is @qcode{"stable"}.
##
## @seealso{union, intersect, setdiff, setxor, ismember}
## @end deftypefn

function [y, i, j] = unique (x, varargin)

  if (nargin < 1)
    print_usage ();
  elseif (! (isnumeric (x) || islogical (x) || ischar (x) || iscellstr (x)))
    error ("unique: X must be an array or cell array of strings");
  endif

  if (nargin > 1)
    ## parse options
    if (! iscellstr (varargin))
      error ("unique: options must be strings");
    endif

    optrows   = any (strcmp ("rows", varargin));
    optfirst  = any (strcmp ("first", varargin));
    optlast   = any (strcmp ("last", varargin));
    optsorted = any (strcmp ("sorted", varargin));
    optstable = any (strcmp ("stable", varargin));
    optlegacy = any (strcmp ("legacy", varargin));
    if (optfirst && optlast)
      error ('unique: cannot specify both "first" and "last"');
    elseif (optsorted && optstable)
      error ('unique: cannot specify both "sorted" and "stable"');
    elseif ((optfirst || optlast) && (optsorted || optstable))
      error ('unique: cannot specify "first"/"last" with "sorted"/"stable"');
    elseif (optlegacy && (optsorted || optstable))
      error ('unique: cannot specify "sorted" or "stable" with "legacy"');
    elseif (optrows + optfirst + optlast + optsorted + optstable + optlegacy
            != nargin-1)
      error ("unique: invalid option");
    endif

    ## Set defaults if not set earlier.
    if (! optfirst && ! optlast)
      optfirst = true;
    endif
    if (! optsorted && ! optstable)
      optsorted = true;
    endif

    if (optrows && iscellstr (x))
      warning ('unique: "rows" is ignored for cell arrays');
      optrows = false;
    endif
  else
    optrows = false;
    optfirst = true;
    optsorted = true;
    optlegacy = false;
  endif

  ## FIXME: The operations
  ##
  ##   match = (y(1:n-1) == y(2:n));
  ##   y(idx) = [];
  ##
  ## are very slow on sparse matrices.  Until they are fixed to be as
  ## fast as for full matrices, operate on the nonzero elements of the
  ## sparse array as long as we are not operating on rows.
  if (issparse (x) && ! optrows && nargout <= 1)
    if (nnz (x) < numel (x))
      y = unique ([0; nonzeros(x)], varargin{:});
    else
      ## Corner case where sparse matrix is actually full
      y = unique (full (x), varargin{:});
    endif
    return;
  endif

  if (optrows)
    n = rows (x);
    isrowvec = false;
  else
    n = numel (x);
    isrowvec = isrow (x);
  endif

  ## Special cases 0 and 1
  if (n == 0)
    y = x;
    if (! optrows && any (size (x)))
      if (iscellstr (x))
        y = cell (0, 1);
      else
        y = zeros (0, 1, class (x));
      endif
    endif
    i = j = [];
    return;
  elseif (n == 1)
    y = x;
    i = j = 1;
    return;
  endif

  ## Calculate y output
  if (optrows)
    if (nargout > 1 || ! optsorted)
      [y, i] = sortrows (x);
      i = i(:);
    else
      y = sortrows (x);
    endif
    match = all (y(1:n-1,:) == y(2:n,:), 2);
    if (optsorted)
      y(match,:) = [];
    else
      y = x;
      y(i([false; match]), :) = [];
    endif
  else
    if (isvector (x))
      y = x;
    else
      y = x(:);
    endif
    if (nargout > 1 || ! optsorted)
      [y, i] = sort (y);
      i = i(:);
    else
      y = sort (y);
    endif
    if (iscellstr (y))
      match = strcmp (y(1:n-1), y(2:n));
    else
      match = (y(1:n-1) == y(2:n));
    endif
    if (optsorted)
      y(match) = [];
    else
      if (isvector (x))
        y = x;
      else
        y = x(:);
      endif
      y(i([false; match(:)])) = [];
    endif
  endif

  ## Calculate j output (3rd output)
  if (isargout (3))
    j = i;  # cheap way to copy dimensions
    j(i) = cumsum ([1; ! match(:)]);
    if (! optsorted)
      warning ("unique: third output J is not yet implemented");
      j = [];
    endif

    if (optlegacy && isrowvec)
      j = j.';
    endif
  endif

  ## Calculate i output (2nd output)
  if (isargout (2))
    if (optsorted)
      idx = find (match);
      if (! optlegacy && optfirst)
        idx += 1;   # in-place is faster than other forms of increment
      endif
      i(idx) = [];
    else
      i([false; match(:)]) = [];
      ## FIXME: Is there a way to avoid a call to sort?
      i = sort (i);
    endif

    if (optlegacy && isrowvec)
      i = i.';
    endif
  endif

endfunction


%!assert (unique ([1 1 2; 1 2 1; 1 1 2]), [1;2])
%!assert (unique ([1 1 2; 1 0 1; 1 1 2],"rows"), [1 0 1; 1 1 2])
%!assert (unique ([]), [])
%!assert (unique ([1]), [1])
%!assert (unique ([1 2]), [1 2])
%!assert (unique ([1;2]), [1;2])
%!assert (unique ([1,NaN,Inf,NaN,Inf]), [1,Inf,NaN,NaN])
%!assert (unique ([1,NaN,Inf,NaN,Inf], "stable"), [1,NaN,Inf,NaN])
%!assert (unique ({"Foo","Bar","Foo"}), {"Bar","Foo"})
%!assert (unique ({"Foo","Bar","Foo"}, "stable"), {"Foo", "Bar"})
%!assert (unique ({"Foo","Bar","FooBar"}'), {"Bar","Foo","FooBar"}')
%!assert (unique (zeros (1,0)), zeros (0,1))
%!assert (unique (zeros (1,0), "rows"), zeros (1,0))
%!assert (unique (cell (1,0)), cell (0,1))
%!assert (unique ({}), {})
%!assert (unique ([1,2,2,3,2,4], "rows"), [1,2,2,3,2,4])
%!assert (unique ([1,2,2,3,2,4]), [1,2,3,4])
%!assert (unique ([1,2,2,3,2,4]', "rows"), [1;2;3;4])
%!assert (unique (sparse ([2,0;2,0])), [0;2])
%!assert (unique (sparse ([1,2;2,3])), [1;2;3])
%!assert (unique ([1,2,2,3,2,4]', "rows"), [1;2;3;4])
%!assert (unique (single ([1,2,2,3,2,4]), "rows"), single ([1,2,2,3,2,4]))
%!assert (unique (single ([1,2,2,3,2,4])), single ([1,2,3,4]))
%!assert (unique (single ([1,2,2,3,2,4]'), "rows"), single ([1;2;3;4]))
%!assert (unique (uint8 ([1,2,2,3,2,4]), "rows"), uint8 ([1,2,2,3,2,4]))
%!assert (unique (uint8 ([1,2,2,3,2,4])), uint8 ([1,2,3,4]))
%!assert (unique (uint8 ([1,2,2,3,2,4]'), "rows"), uint8 ([1;2;3;4]))

## Test options with numeric inputs
%!test
%! [y,i,j] = unique ([1,1,2,3,3,3,4], "sorted");
%! assert (y, [1,2,3,4]);
%! assert (i, [1;3;4;7]);
%! assert (j, [1;1;2;3;3;3;4]);

%!test
%! [y,i,~] = unique ([4,4,2,2,2,3,1], "stable");
%! assert (y, [4,2,3,1]);
%! assert (i, [1;3;6;7]);
%! ## FIXME: 'j' input not calculated with stable
%! ##assert (j, []);

%!test
%! [y,i,j] = unique ([1,1,2,3,3,3,4]', "last");
%! assert (y, [1,2,3,4]');
%! assert (i, [2;3;6;7]);
%! assert (j, [1;1;2;3;3;3;4]);

## Test options with cellstr inputs
%!test
%! [y,i,j] = unique ({"z"; "z"; "z"});
%! assert (y, {"z"});
%! assert (i, [1]);
%! assert (j, [1;1;1]);

%!test
%! [y,i,~] = unique ({"B"; "A"; "B"}, "stable");
%! assert (y, {"B"; "A"});
%! assert (i, [1; 2]);

%!test
%! A = [1,2,3; 1,2,3];
%! [y,i,j] = unique (A, "rows");
%! assert (y, [1,2,3]);
%! assert (A(i,:), y);
%! assert (y(j,:), A);

%!test
%! A = [4,5,6; 1,2,3; 4,5,6];
%! [y,i,~] = unique (A, "rows", "stable");
%! assert (y, [4,5,6; 1,2,3]);
%! assert (A(i,:), y);
%! ## FIXME: 'j' output not calculated correctly with "stable"
%! ##assert (y(j,:), A);

## Test "legacy" option
%!test
%! [y,i,j] = unique ([1,1,2,3,3,3,4], "legacy");
%! assert (y, [1,2,3,4]);
%! assert (i, [2,3,6,7]);
%! assert (j, [1,1,2,3,3,3,4]);

%!test
%! A = [7 9 7; 0 0 0; 7 9 7; 5 5 5; 1 4 5];
%! [y,i,j] = unique (A, "rows", "legacy");
%! assert (y, [0 0 0; 1 4 5; 5 5 5; 7 9 7]);
%! assert (i, [2; 5; 4; 3]);
%! assert (j, [4; 1; 4; 3; 2]);

## Test input validation
%!error <Invalid call> unique ()
%!error <X must be an array or cell array of strings> unique ({1})
%!error <options must be strings> unique (1, 2)
%!error <cannot specify both "first" and "last"> unique (1, "first", "last")
%!error <cannot specify both "sorted" and "stable">
%! unique (1, "sorted", "stable");
%!error <cannot specify "first"/"last" with "sorted"/"stable">
%! unique (1, "first", "sorted");
%!error <cannot specify "first"/"last" with "sorted"/"stable">
%! unique (1, "last", "stable");
%!error <cannot specify "sorted" or "stable" with "legacy">
%! unique (1, "sorted", "legacy");
%!error <cannot specify "sorted" or "stable" with "legacy">
%! unique (1, "stable", "legacy");
%!error <invalid option> unique (1, "middle")
%!error <invalid option> unique ({"a", "b", "c"}, "UnknownOption")
%!error <invalid option> unique ({"a", "b", "c"}, "UnknownOption1", "UnknownOption2")
%!error <invalid option> unique ({"a", "b", "c"}, "rows", "UnknownOption2")
%!error <invalid option> unique ({"a", "b", "c"}, "UnknownOption1", "last")
%!warning <"rows" is ignored for cell arrays> unique ({"1"}, "rows");
%!warning <third output J is not yet implemented>
%! [y,i,j] = unique ([2,1], "stable");
%! assert (j, []);
