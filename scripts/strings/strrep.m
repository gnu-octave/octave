## Copyright (C) 1995, 1996, 1998, 1999, 2000, 2005, 2006, 2007
##               Kurt Hornik
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
## @deftypefn {Function File} {} strrep (@var{s}, @var{x}, @var{y})
## Replaces all occurrences of the substring @var{x} of the string @var{s}
## with the string @var{y} and returns the result.  For example,
##
## @example
## strrep ("This is a test string", "is", "&%$")
##      @result{} "Th&%$ &%$ a test string"
## @end example
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>
## Created: 11 November 1994
## Adapted-By: jwe

function t = strrep (s, x, y)

  if (nargin != 3)
    print_usage ();
  endif

  if (! (ischar (s) && ischar (x) && ischar (y)))
    error ("strrep: all arguments must be strings");
  endif

  if (length (x) > length (s) || isempty (x))
    t = s;
    return;
  endif

  ind = findstr (s, x, 0);

  if (length(ind) == 0)
    t = s;
  elseif (length(y) > 0)      # replacement
    ## Copy the parts of s that aren't being replaced.  This is done
    ## with an index vector, with jumps where each search string
    ## is found.  For a jump of 0 (target length == replacement length)
    ## the index is just cumsum ( ones (length (s))).  For non-zero
    ## jumps, add the jump size to the ones vector at each found position.
    jump = length(y) - length(x);
    if (jump > 0)     # s expands
      di = ones(size(s));
      di(ind) = 1 + jump * ones (length (ind), 1);
      t(cumsum (di)) = s;
    elseif (jump < 0) # s contracts
      di = ones (jump * length (ind) + length (s), 1);
      di (ind + jump * [0:length(ind)-1]) = 1 - jump * ones(length(ind), 1);
      t = s (cumsum (di));
    else              # s stays the same length
      t = s;
    endif
    ## Now, substitute a copy of the replacement string whereever the
    ## search string was found.  Note that we must first update the
    ## target positions to account for any expansion or contraction
    ## of s that may have occurred.
    ind = ind + jump * [0:length(ind)-1];
    repeat = [1:length(y)]' * ones (1, length (ind));
    dest = ones (length (y), 1) * ind + repeat - 1;
    t(dest) = y(repeat);
  else                        # deletion
    ## Build an index vector of all locations where the target was found
    ## in the search string, and zap them. 
    t = toascii (s);
    repeat = [1:length(x)]' * ones (1, length (ind));
    delete = ones (length (x), 1) * ind + repeat - 1;
    t(delete) = [];
    t = char (t);
  endif

endfunction

%!assert(strcmp (strrep ("This is a test string", "is", "&%$"),
%! "Th&%$ &%$ a test string"));

%!error strrep ();

%!error strrep ("foo", "bar", 3, 4);
