## Copyright (C) 2016-2018 Andrew Thornton
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
## @deftypefn  {} {@var{hui} =} uibuttongroup (@var{property}, @var{value}, @dots{})
## @deftypefnx {} {@var{hui} =} uibuttongroup (@var{parent}, @var{property}, @var{value}, @dots{})
## @deftypefnx {} {} uibuttongroup (@var{h})
##
## Create a uibuttongroup object and return a handle to it.
##
## A uibuttongroup is used to group uicontrol objects.
##
## If @var{parent} is omitted then a uibuttongroup for the current figure is
## created.  If no figure is available, a new figure is created first.
##
## If @var{parent} is given then a uibuttongroup relative to @var{parent} is
## created.
##
## Any provided property value pairs will override the default values of the
## created uibuttongroup object.
##
## Properties of uibuttongroup objects are documented at
## @ref{Uibuttongroup Properties}.
##
## Examples:
##
## @example
## @group
## ## Create figure and panel on it
## f = figure;
## ## Create a button group
## gp = uibuttongroup (f, "Position", [ 0 0.5 1 1])
## ## Create a buttons in the group
## b1 = uicontrol (gp, "style", "radiobutton", ...
##                 "string", "Choice 1", ...
##                 "Position", [ 10 150 100 50 ]);
## b2 = uicontrol (gp, "style", "radiobutton", ...
##                 "string", "Choice 2", ...
##                 "Position", [ 10 50 100 30 ]);
## ## Create a button not in the group
## b3 = uicontrol (f, "style", "radiobutton", ...
##                 "string", "Not in the group", ...
##                 "Position", [ 10 50 100 50 ]);
## @end group
## @end example
##
## When called with a single argument @var{h} which is a handle to an existing
## uibuttongroup object, switch the focus to the specified uibuttongroup.  This
## functionality is not currently implemented.
## @seealso{figure, uipanel}
## @end deftypefn

## Author: zeripath

function hui = uibuttongroup (varargin)

  if (nargin == 1 && isgraphics (varargin{1}, "uibuttongroup"))
    warning ("uibuttongroup: focusing not implemented yet");
    return;
  endif

  [h, args] = __uiobject_split_args__ ("uibuttongroup", varargin,
                                       {"figure", "uipanel", "uibuttongroup"});
  hui = __go_uibuttongroup__ (h, args{:});

endfunction


%!demo
%! f = figure;
%! gp = uibuttongroup (f, "Position", [ 0 0.5 1 1], ...
%!                     "selectionchangedfcn", ...
%!                     @(x, y) display (['Selection Changed: ' get(y.NewValue, 'String')]));
%! b1 = uicontrol (gp, "style", "radiobutton", ...
%!                 "string", "Choice 1", ...
%!                 "Position", [ 10 150 100 50 ]);
%! b2 = uicontrol (gp, "style", "radiobutton", ...
%!                 "string", "Choice 2", ...
%!                 "Position", [ 10 50 100 30 ]);
%! b3 = uicontrol (f, "style", "radiobutton", ...
%!                 "string", "Not in the group", ...
%!                 "Position", [ 10 50 100 50 ]);
%! disp (['Current selected: ' get(get(gp, 'selectedobject'), 'String')]);
%! pause (0.5);
%! disp (['Select None']);
%! set (gp, 'selectedobject', []);
%! pause (0.1);
%! disp (['Current selected: ' get(get(gp, 'selectedobject'), 'String')]);
%! pause (0.5);
%! disp (['Select b1']);
%! set (gp, 'selectedobject', b1);
%! disp (['Current selected: ' get(get(gp, 'selectedobject'), 'String')]);

## Uncertain if tests can be performed
%!assert (1)
