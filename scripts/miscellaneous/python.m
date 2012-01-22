## Copyright (C) 2008-2012 Julian Schnidder
## Copyright (C) 2012 Carnë Draug
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
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {[@var{output}, @var{status}] =} python (@var{scriptfile})
## @deftypefnx {Function File} {[@var{output}, @var{status}] =} python (@var{scriptfile}, @var{argument1}, @var{argument2}, @dots{})
## Invoke python script @var{scriptfile} with possibly a list of
## command line arguments.
## Returns output in @var{output} and status
## in @var{status}.
## @seealso{system}
## @end deftypefn

## Author: Carnë Draug <carandraug+dev@gmail.com>

function [output, status] = python (scriptfile = "-c ''", varargin)

  ## VARARGIN is intialized to {}(1x0) if no additional arguments are
  ## supplied, so there is no need to check for it, or provide an
  ## initial value in the argument list of the function definition.

  if (ischar (scriptfile)
      && ((nargin != 1 && iscellstr (varargin))
          || (nargin == 1 && ! isempty (scriptfile))))
    [status, output] = system (cstrcat ("python ", scriptfile,
                                        sprintf (" %s", varargin{:})));
  else
    error ("python: invalid arguments");
  endif

endfunction
