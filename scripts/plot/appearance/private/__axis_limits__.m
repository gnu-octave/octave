########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{limits} =} __axis_limits__ (@var{fcn})
## @deftypefnx {} {@var{xmode} =} __axis_limits__ (@var{fcn}, "mode")
## @deftypefnx {} {} __axis_limits__ (@var{fcn}, [@var{x_lo} @var{x_hi}])
## @deftypefnx {} {} __axis_limits__ (@var{fcn}, "auto")
## @deftypefnx {} {} __axis_limits__ (@var{fcn}, "manual")
## @deftypefnx {} {} __axis_limits__ (@var{fcn}, @var{hax}, @dots{})
## @deftypefn {} {@var{old_limits} =} __axis_limits__ (@var{fcn}, @dots{})
## Internal function that implements common code to query or set the axis
## limits for a plot.
## @seealso{xlim, ylim, zlim, title}
## @end deftypefn

function retval = __axis_limits__ (fcn, varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ (fcn, varargin{:});

  if (isempty (hax))
    hax = gca ();
  endif

  if (nargin == 0)
    retval = get (hax, fcn);
  else
    retval = [];
    fcnmode = [fcn "mode"];
    arg = varargin{1};
    if (ischar (arg))
      if (strcmpi (arg, "mode"))
        retval = get (hax, fcnmode);
      elseif (any (strcmpi (arg, {"auto", "manual"})))
        set (hax, fcnmode, arg);
      endif
    else
      if (! isnumeric (arg) || any (size (arg(:)) != [2, 1]))
        error ("%s: LIMITS must be a 2-element vector", fcn);
      elseif (arg(1) >= arg(2))
        error ("%s: axis limits must be increasing", fcn);
      endif
      set (hax, fcn, arg(:));
    endif
  endif

endfunction
