## Copyright (C) 2006  Bill Denney  <denney@seas.upenn.edu>
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Command} {@var{a} =} arrayfun (@var{name}, @var{c})
## @deftypefnx {Command} {@var{a} =} arrayfun (@var{func}, @var{c})
## @deftypefnx {Command} {@var{a} =} arrayfun (@var{func}, @var{c}, @var{d})
## @deftypefnx {Command} {@var{a} =} arrayfun (@var{func}, @var{c}, @var{options})
## @deftypefnx {Command} {[@var{a}, @var{b}, @dots{}] =} arrayfun (@var{func}, @var{c}, @dots{})
## Execute a function on each element of an array.  This is useful for
## functions that do not accept array arguments.  If the function does
## accept array arguments it is better to call the function directly.
##
## See @code{cellfun} for complete usage instructions.
## @seealso{cellfun}
## @end deftypefn

function varargout = arrayfun (func, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  ## Convert everything to cells and call cellfun (let cellfun error
  ## check the options in case more options come available).
  sizetomatch = size (varargin{1});
  m2cargs{1} = ones (size (varargin{1}, 1), 1);
  m2cargs{2} = ones (size (varargin{1}, 2), 1);
  cfarg{1} = mat2cell (varargin{1}, m2cargs{:});
  stillmatches = true;
  idx = 1;
  while (stillmatches && idx < numel (varargin))
    idx++;
    thissize = size (varargin{idx});
    if (numel (thissize) == numel (sizetomatch)
	&& all (thissize == sizetomatch))
      if (ischar (varargin{idx})
	  && (strcmpi (varargin{idx}, "UniformOutput")
	      || strcmpi (varargin{idx}, "ErrorHandler")))
	## Catch these strings just in case they happen to be the same
	## size as the other input.
	stillmatches = false;
      else
	cfarg{idx} = mat2cell (varargin{idx}, m2cargs{:});
      endif
    else
      stillmatches = false;
    endif
  endwhile

  varargout = cell (max ([nargout, 1]), 1);
  [varargout{:}] = cellfun (func, cfarg{:}, varargin{idx+1:numel(varargin)});
endfunction

%!test
%! fun = @(x,y) 2*x+y;
%! A = [1,2;3,4];
%! assert(arrayfun(fun,A,A),fun(A,A))
