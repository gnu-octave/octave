## Copyright (C) 2008 Jaroslav Hajek
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
## @deftypefn {Function File} {@var{idx} =} strchr (@var{str}, @var{chars})
## @deftypefnx {Function File} {@var{idx} =} strchr (@var{str}, @var{chars}, @var{n})
## @deftypefnx {Function File} {@var{idx} =} strchr (@var{str}, @var{chars}, @var{n}, @var{direction})
## Search for the string @var{str} for occurences of characters from the set @var{chars}.
## The return value, as well as the @var{n} and @var{direction} arguments behave
## identically as in @code{find}.
##
## This will be faster than using regexp in most cases.
##
## @seealso{find}
## @end deftypefn

function varargout = strchr (str, chars, varargin)
  if (nargin < 2 || ! ischar (str) || ! ischar (chars))
    print_usage ();
  endif
  f = false (1, 256);
  f(chars + 1) = true;
  varargout = cell (1, nargout);
  varargout{1} = [];
  [varargout{:}] = find (reshape (f(str + 1), size (str)), varargin{:});
endfunction 

%!assert(strchr("Octave is the best software","best"),[3, 6, 9, 11, 13, 15, 16, 17, 18, 20, 23, 27])
