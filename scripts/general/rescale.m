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
## @deftypefn  {} {} rescale (@var{A})
## @deftypefnx {} {} rescale (@var{A}, @var{l}, @var{u})
## @deftypefnx {} {} rescale (@var{A}, 'inputmin', @var{inmin})
## @deftypefnx {} {} rescale (@var{A}, 'inputmax', @var{inmax})
## @deftypefnx {} {} rescale (@var{A}, 'inputmin', @var{inmin}, 'inputmax', @var{inmax})
## @deftypefnx {} {} rescale (@var{A}, @var{l}, @var{u}, 'inputmin', @var{inmin})
## @deftypefnx {} {} rescale (@var{A}, @var{l}, @var{u}, 'inputmax', @var{inmax})
## @deftypefnx {} {} rescale (@var{A}, @var{l}, @var{u}, 'inputmin', @var{inmin}, 'inputmax', @var{inmax})
## Rescale matrix.
##
## Rescales a matrix to the interval [l,u]. The name-value pairs 'inputmin' and
## 'inputmax' set all elements of A to min(A,inmin) and max(A,inmax)
## respectively. The applied formula is:
## @tex
## B = l + \frac{A - inmin}{inmax - inmin} * (u - l).
## @end tex
##
## The default value for the lower bound @var{l} is 0, and for the upper bound
## @var{u} is 1. If the name-value pairs 'inputmin' or 'inputmax' are not set,
## the minimum and maximum input range are set to @var{inmin} = min(A(:)) and
## @var{inmax) = max(A(:)) respectively.
##
## @seealso{min, max}
## @end deftypefn

## Author: Christian Himpe <christian.himpe@wwu.de>
## Created: November 2019

function B = rescale(A,varargin)

    % Check if 1st argument is a matrix
    if not(isnumeric(A)) || not(any(nargin == [1,3,5,7]))

        print_usage();
    endif

    l = 0;
    u = 1.0;

    % Check if 2nd and 3rd argument are numeric, then set non-default interval.
    if nargin > 1 && isnumeric(varargin{1})

        l = varargin{1}; 

        if isnumeric(varargin{2})

            u = varargin{2};
        else

            print_usage();
        endif
    endif

    % Check for named argument 'inputmin'.
    if not(isempty(varargin))

        inminidx = find(strcmp(lower(varargin),'inputmin'));
    else

        inminidx = [];
    endif
        
    if not(isempty(inminidx)) && isnumeric(varargin(inminidx(end) + 1))

        inmin = varargin(inminidx(end) + 1);
    else

        inmin = min(A(:));
    endif

    % Check for named argument 'inputmax'.
    if not(isempty(varargin))

        inmaxidx = find(strcmp(lower(varargin),'inputmax'));
    else

        inmaxidx = [];
    endif
        
    if not(isempty(inmaxidx)) && isnumeric(varargin(inmaxidx(end) + 1));

        inmax = varargin(inmaxidx(end) + 1);
    else

        inmax = max(A(:));
    endif  

    % Rescale A to interval [l,u] in range interval [inmin,inmax].
    B = l + ( (A - inmin) ./ (inmax - inmin) ) .* (u - l);

endfunction
