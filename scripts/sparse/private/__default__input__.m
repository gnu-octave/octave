########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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
## @deftypefn {} {@var{varargout} =} __default__input__ (@var{def_val}, @var{varargin})
## Check if the input arguments of a function are empty or missing.
## If they are, use default values.
##
## The input arguments are:
##
## @itemize @minus
## @item @var{def_val} is a cell array that contains the values to use
## as default.
##
## @item @var{varargin} are the input arguments.
## @end itemize
##
## The output arguments are:
##
## @itemize @minus
## @item @var{varargout} are the input arguments where any empty or omitted
## parameters have been replaced with default values.
##
## @end itemize
##
## @end deftypefn

function varargout = __default__input__ (def_val, varargin)

  m = numel (def_val);
  n = numel (varargin);
  count = min (m, n);

  ## Check for missing values in input and replace with default value.
  for i = 1:count
    if (isempty (varargin{i}))
      varargout{i} = def_val{i};
    else
      varargout{i} = varargin{i};
    endif
  endfor

  ## Copy any remaining items to output
  if (n < m)
    varargout(n+1:m) = def_val(n+1:m);
  elseif (m < n)
    varargout(m+1:n) = varargin(m+1:n);
  endif

endfunction
