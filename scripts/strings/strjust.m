## Copyright (C) 2000-2011 Paul Kienzle
## Copyright (C) 2009 Jaroslav Hajek
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {} strjust (@var{s}, @var{pos})
## Return the text, @var{s}, justified according to @var{pos}, which may
## be @samp{"left"}, @samp{"center"}, or @samp{"right"}.  If @var{pos}
## is omitted, @samp{"right"} is assumed. 
##
## Null characters are replaced by spaces.  All other character
## data are treated as non-white space.
##
## Example:
##
## @example
## @group
## strjust (["a"; "ab"; "abc"; "abcd"])
##      @result{} ans =
##            a
##           ab
##          abc
##         abcd
## @end group
## @end example
## @seealso{deblank, strrep, strtrim, untabify}
## @end deftypefn

function y = strjust (s, pos)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (nargin == 1)
    pos = "right";
  else
    pos = tolower (pos);
  endif

  if (ndims (s) != 2)
    error ("strjust: input must be a string or character matrix");
  endif

  if (isempty (s))
    y = s;
  else
    ## Apparently, Matlab considers nulls to be blanks as well; however, does
    ## not preserve the nulls, but rather converts them to blanks.  That's a
    ## bit unexpected, but it allows simpler processing, because we can move
    ## just the nonblank characters. So we'll do the same here.

    [nr, nc] = size (s);
    ## Find the indices of all nonblanks.
    nonbl = s != " " & s != "\0";
    [idx, jdx] = find (nonbl);

    if (strcmp (pos, "right"))
      ## We wish to find the maximum column index for each row. Because jdx is
      ## sorted, we can take advantage of the fact that assignment is processed
      ## sequentially and for duplicate indices the last value will remain.
      maxs = nc * ones (nr, 1);
      maxs(idx) = jdx;
      shift = nc - maxs;
    elseif (strcmp (pos, "left"))
      ## See above for explanation.
      mins = ones (nr, 1);
      mins(flipud (idx(:))) = flipud (jdx(:));
      shift = 1 - mins;
    else
      ## Use both of the above.
      mins = ones (nr, 1);
      mins(flipud (idx(:))) = flipud (jdx(:));
      maxs = nc * ones (nr, 1);
      maxs(idx) = jdx;
      shift = floor ((nc + 1 - maxs - mins) / 2); 
    endif

    ## Adjust the column indices.
    jdx += shift (idx);

    ## Create a blank matrix and position the nonblank characters.
    y = " "(ones (1, nr), ones (1, nc));
    y(sub2ind ([nr, nc], idx, jdx)) = s(nonbl);
  endif

endfunction

%!error <Invalid call to strjust> strjust();
%!error <Invalid call to strjust> strjust(["a";"ab"], "center", 1);
%!assert (strjust (["a"; "ab"; "abc"; "abcd"]),
%!        ["   a";"  ab"; " abc"; "abcd"]);
%!assert (strjust (["a"; "ab"; "abc"; "abcd"], "center"),
%!        [" a  "; " ab"; "abc "; "abcd"]);
