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
## @deftypefn {} {[x, y] =} validsetargs (@var{caller}, @var{x}, @var{y}, @var{varargin})
## Internal function to validate arguments passed to functions operating on
## sets.
## @seealso{intersect, ismember, setdiff, setxor, union}
## @end deftypefn

function [x, y] = validsetargs (caller, x, y, varargin)

  isallowedarraytype = @(x) isnumeric (x) || ischar (x) || islogical (x);

  if (nargin == 3)
    icx = iscellstr (x);
    icy = iscellstr (y);
    if (icx || icy)
      if (icx && ischar (y))
        y = cellstr (y);
      elseif (icy && ischar (x))
        x = cellstr (x);
      elseif (icy && isempty (x))
        x = {};
      elseif (icx && isempty (y))
        y = {};
      elseif (! (icx && icy))
        error ("%s: cell array of strings cannot be combined with a nonstring value", caller);
      endif
    elseif (! (isallowedarraytype (x) && isallowedarraytype (y)))
      error ("%s: A and B must be arrays or cell arrays of strings", caller);
    endif
  else
    optlegacy = false;
    optsorted = false;
    optstable = false;

    for arg = varargin
      switch (arg{1})
        case "legacy"
          optlegacy = true;

        case "rows"
          if (iscell (x) || iscell (y))
            error ('%s: cells not supported with "rows" flag', caller);
          elseif (! (isallowedarraytype (x) && isallowedarraytype (y)))
            error ("%s: A and B must be arrays or cell arrays of strings", caller);
          else
            if (ndims (x) > 2 || ndims (y) > 2)
              error ('%s: A and B must be 2-dimensional matrices with "rows" flag', caller);
            elseif (columns (x) != columns (y) && ! (isempty (x) || isempty (y)))
              error ("%s: number of columns in A and B must match", caller);
            endif
          endif

        case "sorted"
          optsorted = true;

        case "stable"
          optstable = true;

        otherwise
          error ("%s: invalid option: %s", caller, arg{1});

      endswitch
    endfor

    if (optsorted + optstable + optlegacy > 1)
      error ('%s: only one of "sorted", "stable", or "legacy" may be specified',
             caller);
    endif

  endif

endfunction


## BIST tests for function are in union.m
