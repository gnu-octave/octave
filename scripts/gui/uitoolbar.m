########################################################################
##
## Copyright (C) 2012-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{hui} =} uitoolbar ()
## @deftypefnx {} {@var{hui} =} uitoolbar (@var{property}, @var{value}, @dots{})
## @deftypefnx {} {@var{hui} =} uitoolbar (@var{parent})
## @deftypefnx {} {@var{hui} =} uitoolbar (@var{parent}, @var{property}, @var{value}, @dots{})
##
## Create a uitoolbar object.  A uitoolbar displays uitoggletool and uipushtool
## buttons.
##
## If @var{parent} is omitted then a uitoolbar for the current figure is
## created.  If no figure is available, a new figure is created first.
##
## If @var{parent} is given then a uitoolbar relative to @var{parent} is
## created.
##
## Any provided property value pairs will override the default values of the
## created uitoolbar object.
##
## The full list of properties is documented at @ref{Uitoolbar Properties}.
##
## The optional return value @var{hui} is a graphics handle to the created
## uitoolbar object.
##
## Examples:
##
## @example
## @group
## % create figure without a default toolbar
## f = figure ("toolbar", "none");
## % create empty toolbar
## t = uitoolbar (f);
## @end group
## @end example
## @seealso{figure, uitoggletool, uipushtool}
## @end deftypefn

function hui = uitoolbar (varargin)

  [h, args] = __uiobject_split_args__ ("uitoolbar", varargin, {"figure"});

  htmp = __go_uitoolbar__ (h, args{:});

  if (nargout > 0)
    hui = htmp;
  endif

endfunction
