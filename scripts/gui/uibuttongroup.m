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
## @deftypefn  {} {@var{hui} =} uibuttongroup ()
## @deftypefnx {} {@var{hui} =} uibuttongroup (@var{property}, @var{value}, @dots{})
## @deftypefnx {} {@var{hui} =} uibuttongroup (@var{parent})
## @deftypefnx {} {@var{hui} =} uibuttongroup (@var{parent}, @var{property}, @var{value}, @dots{})
## @c FIXME: 3rd form is not documented by Matlab nor implemented in Octave.
## @c        Should it be removed?  (1/9/2022).
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
## The full list of properties is documented at @ref{Uibuttongroup Properties}.
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
%! f = clf ();
%! gp = uibuttongroup (f, "position", [0 0.5 1 0.5], ...
%!                     "selectionchangedfcn", ...
%!                     @(h, e) fprintf ("Selection changed: %s\n", get (e.NewValue, "string")));
%! b1 = uicontrol (gp, "style", "radiobutton", ...
%!                     "string", "Choice 1", ...
%!                     "units", "normalized", ...
%!                     "position", [0.01 0.5 0.98 0.5]);
%! b2 = uicontrol (gp, "style", "radiobutton", ...
%!                     "string", "Choice 2", ...
%!                     "units", "normalized", ...
%!                     "position", [0.01 0 0.98 0.5]);
%! b3 = uicontrol (f, "style", "radiobutton", ...
%!                    "string", "Not in the group", ...
%!                    "units", "normalized", ...
%!                    "position", [ 0.01 0 0.98 0.5 ]);
%! fprintf ("Current selected: %s\n", get (get (gp, "selectedobject"), "string"));
%! pause (0.5);
%! disp ("Select b2");
%! set (gp, "selectedobject", b2);
%! fprintf ("Current selected: %s\n", get (get (gp, "selectedobject"), "string"));
%! pause (0.5);
%! disp ("Select None");
%! set (gp, "selectedobject", []);
%! pause (0.1);
%! fprintf ("Current selected: %s\n", get (get (gp, "selectedobject"), "string"));

## Test mutual selection logic for radiobuttons
## FIXME: commented out until a test can be found that doesn't rely on
##        long values for pause() which still can occasionally fail.
%!#test <*55230>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   bg = uibuttongroup (hf);
%!   b1 = uicontrol (bg, "style", "radiobutton", "units", "normalized", ...
%!                       "position", [0, 0, 1, 0.5]);
%!   b2 = uicontrol (bg, "style", "radiobutton", "units", "normalized", ...
%!                       "position", [0, 0.5, 1, 0.5]);
%!   assert (get (bg, "selectedobject"), b1);
%!   assert (get (b1, "value"), 1);
%!   assert (get (b2, "value"), 0);
%!   ## select radiobutton 2
%!   set (bg, "selectedobject", b2);
%!   pause (0.5);
%!   assert (get (b1, "value"), 0);
%!   assert (get (b2, "value"), 1);
%!   ## set radiobutton 1
%!   set (b1, "value", 1);
%!   pause (0.5);
%!   assert (get (bg, "selectedobject"), b1);
%!   assert (get (b1, "value"), 1);
%!   assert (get (b2, "value"), 0);
%!   ## unset all radiobuttons
%!   set (bg, "selectedobject", []);
%!   pause (0.5);
%!   assert (get (b1, "value"), 0);
%!   assert (get (b2, "value"), 0);
%!   ## change style of selected button
%!   set (b1, "value", 1);
%!   pause (0.5);
%!   assert (get (bg, "selectedobject"), b1);
%!   set (b1, "style", "pushbutton");
%!   pause (0.5);
%!   assert (get (bg, "selectedobject"), []);
%!   ## add new button
%!   b3 = uicontrol (bg, "style", "togglebutton");
%!   pause (0.5);
%!   assert (get (bg, "selectedobject"), b3);
%!   assert (get (b2, "value"), 0);
%!   assert (get (b3, "value"), 1);
%!   ## remove selected button
%!   delete (b3);
%!   pause (0.5);
%!   assert (get (bg, "selectedobject"), []);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test mutual selection logic for togglebuttons
## FIXME: commented out until a test can be found that doesn't rely on
##        long values for pause() which still can occasionally fail.
%!#test <*55230>
%! hf = figure ("visible", "off");
%! unwind_protect
%!   bg = uibuttongroup (hf);
%!   b1 = uicontrol (bg, "style", "togglebutton", "units", "normalized", ...
%!                       "position", [0, 0, 1, 0.5]);
%!   b2 = uicontrol (bg, "style", "togglebutton", "units", "normalized", ...
%!                       "position", [0, 0.5, 1, 0.5]);
%!   assert (get (bg, "selectedobject"), b1);
%!   assert (get (b1, "value"), 1);
%!   assert (get (b2, "value"), 0);
%!   ## select togglebutton 2
%!   set (bg, "selectedobject", b2);
%!   pause (0.5);
%!   assert (get (b1, "value"), 0);
%!   assert (get (b2, "value"), 1);
%!   ## set togglebutton 1
%!   set (b1, "value", 1);
%!   pause (0.5);
%!   assert (get (bg, "selectedobject"), b1);
%!   assert (get (b1, "value"), 1);
%!   assert (get (b2, "value"), 0);
%!   ## unset all togglebuttons
%!   set (bg, "selectedobject", []);
%!   pause (0.5);
%!   assert (get (b1, "value"), 0);
%!   assert (get (b2, "value"), 0);
%!   ## change style of selected button
%!   set (b1, "value", 1);
%!   pause (0.5);
%!   assert (get (bg, "selectedobject"), b1);
%!   set (b1, "style", "pushbutton");
%!   assert (get (bg, "selectedobject"), []);
%!   ## add new button
%!   b3 = uicontrol (bg, "style", "togglebutton");
%!   pause (0.5);
%!   assert (get (bg, "selectedobject"), b3);
%!   assert (get (b2, "value"), 0);
%!   assert (get (b3, "value"), 1);
%!   ## remove selected button
%!   delete (b3);
%!   pause (0.5);
%!   assert (get (bg, "selectedobject"), []);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
