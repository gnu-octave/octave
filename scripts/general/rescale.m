## Copyright (C) 2019 Christian Himpe
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

## -*- texinfo -*-
## @deftypefn  {} {@var{B} =} rescale (@var{A})
## @deftypefnx {} {@var{B} =} rescale (@var{A}, @var{l}, @var{u})
## @deftypefnx {} {@var{B} =} rescale (@dots{}, "inputmin", @var{inmin})
## @deftypefnx {} {@var{B} =} rescale (@dots{}, "inputmax", @var{inmax})
## Rescale range of array values.
##
## The values of the input array elements are rescaled to the interval
## [@var{l}, @var{u}].  Optionally before rescaling, the elements of @var{A} are
## truncated to the interval [@var{inmin}, @var{inmax}].  In this case, these
## values are scaled to @var{l} and @var{u}, respectively.  The default value
## for the lower bound @var{l} of the output interval is 0. The default upper
## bound @var{u} is 1.
##
## The formula used for rescaling is:
## @tex
## $$B = l + {A - inmin \over inmax - inmin} \cdot (u - l).$$
## @end tex
##
## @seealso{min, max}
## @end deftypefn

## Author: Christian Himpe <christian.himpe@wwu.de>
## Created: November 2019

function B = rescale (A, varargin)

  ## Check if 1st argument is a matrix
  if (! isnumeric(A) || ! any (nargin == [1,3,5,7]))
    print_usage ();
  endif

  l = 0;
  u = 1;

  ## If 2nd and 3rd argument are numeric, set non-default interval.
  if (nargin > 1 && isnumeric (varargin{1}))
    if (! isnumeric (varargin{2}))
      print_usage ();
    endif
    l = varargin{1}; 
    u = varargin{2};
    if (l > u)
      error ("rescale: L must not be larger than U.");
    endif
  endif

  ## Check for named argument "inputmin".
  inminidx = find (strcmpi (varargin, "inputmin"), 1, "last");
  if (! isempty (inminidx))
    if (! isnumeric (varargin{inminidx + 1}))
      error ("rescale: INMIN must be numeric.");
    endif
    inmin = varargin{inminidx + 1};
  else
    inmin = min (A(:));
  endif

  ## Check for named argument "inputmax".
  inmaxidx = find (strcmpi (varargin, "inputmax"), 1, "last");
  if (! isempty (inmaxidx))
    if (! isnumeric (varargin{inmaxidx + 1}))
      error ("rescale: INMAX must be numeric.");
    endif
    inmax = varargin{inmaxidx + 1};
  else
    inmax = max (A(:));
  endif

  ## Truncate values in A
  A(A < inmin) = inmin;
  A(A > inmax) = inmax;

  ## Rescale A to interval [l,u] in range interval [inmin,inmax].
  B = l + ( (A - inmin) ./ (inmax - inmin) ) .* (u - l);

endfunction

%!assert (rescale (0:5), (0:5)/5)
%!assert (rescale (0:6, "inputmin", 1, "inputmax", 5), [0,0,0.25,0.5,0.75,1,1]);
%!assert (rescale (1:4, 3, 9), [3,5,7,9]);
