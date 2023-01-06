########################################################################
##
## Copyright (C) 2010-2023 The Octave Project Developers
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
## @deftypefn {} {[@var{sel}, @var{ok}] =} listdlg (@var{key}, @var{value}, @dots{})
## Return user inputs from a list dialog box in a vector of selection indices
## (@var{sel}) and a flag indicating how the user closed the dialog box
## (@var{ok}).
##
## The indices in @var{sel} are 1-based.
##
## The value of @var{ok} is 1 if the user closed the box with the OK button,
## otherwise it is 0 and @var{sel} is empty.
##
## Input arguments are specified in form of @var{key}, @var{value} pairs.
## The @qcode{"ListString"} argument pair @strong{must} be specified.
##
## Valid @var{key} and @var{value} pairs are:
##
## @table @asis
## @item @qcode{"ListString"}
## a cell array of strings specifying the items to list in the dialog.
##
## @item @qcode{"SelectionMode"}
## can be either @qcode{"Single"} (only one item may be selected at a time) or
## @qcode{"Multiple"} (default).
##
## @item @qcode{"ListSize"}
## a two-element vector @code{[@var{width}, @var{height}]} specifying the size
## of the list field in pixels.  The default is [160, 300].
##
## @item @qcode{"InitialValue"}
## a vector containing 1-based indices of elements which will be pre-selected
## when the list dialog is first displayed.
## The default is 1 (first item).
##
## @item @qcode{"Name"}
## a string to be used as the dialog caption.  Default is "".
##
## @item @qcode{"PromptString"}
## a cell array of strings to be displayed above the list of items.
## Default is @{@}.
##
## @item @qcode{"OKString"}
## a string used to label the OK button.  Default is @qcode{"OK"}.
##
## @item @qcode{"CancelString"}
## a string used to label the Cancel button.  Default is @qcode{"Cancel"}.
## @end table
##
## Example:
##
## @example
## @group
## my_options = @{"An item", "another", "yet another"@};
## [sel, ok] = listdlg ("ListString", my_options,
##                      "SelectionMode", "Multiple");
## if (ok == 1)
##   disp ("You selected:");
##   for i = 1:numel (sel)
##     disp (sprintf ("\t%s", my_options@{sel(i)@}));
##   endfor
## else
##   disp ("You cancelled.");
## endif
## @end group
## @end example
##
## @seealso{menu, errordlg, helpdlg, inputdlg, msgbox, questdlg, warndlg}
## @end deftypefn

function [sel, ok] = listdlg (varargin)

  if (nargin < 2)
    print_usage ();
  endif

  if (mod (nargin, 2) != 0)
    error ("listdlg: KEY/VALUE inputs must occur in pairs");
  endif

  listcell = {""};
  selmode = "multiple";
  listsize = [160, 300];
  initialvalue = 1;
  name = "";
  prompt = {};
  okstring = "OK";
  cancelstring = "Cancel";

  ## handle key, value pairs
  for i = 1:2:nargin-1
    if (strcmpi (varargin{i}, "ListString"))
      listcell = varargin{i+1};
    elseif (strcmpi (varargin{i}, "SelectionMode"))
      selmode = tolower (varargin{i+1});
    elseif (strcmpi (varargin{i}, "ListSize"))
      listsize = varargin{i+1};
    elseif (strcmpi (varargin{i}, "InitialValue"))
      initialvalue = varargin{i+1};
    elseif (strcmpi (varargin{i}, "Name"))
      name = varargin{i+1};
    elseif (strcmpi (varargin{i}, "PromptString"))
      prompt = varargin{i+1};
    elseif (strcmpi (varargin{i}, "OKString"))
      okstring = varargin{i+1};
    elseif (strcmpi (varargin{i}, "CancelString"))
      cancelstring = varargin{i+1};
    else
      error ("listdlg: invalid KEY <%s>", varargin{i});
    endif
  endfor

  if (isempty (listcell))
    error ('listdlg: "ListString" property must not be empty');
  endif

  ## make sure listcell strings are a cell array
  if (! iscell (listcell))
    listcell = {listcell};
  elseif (iscellstr (listcell{1}))
    listcell = listcell{1};
  endif

  ## make sure prompt strings are a cell array
  if (! iscell (prompt))
    prompt = {prompt};
  endif

  ## validate selection mode
  if (! strcmp (selmode, "multiple") && ! strcmp (selmode, "single"))
    error ('listdlg: "SelectionMode" must be "single" or "multiple"');
  endif

  if (! __event_manager_have_dialogs__ ())
    error ("listdlg is not available in this version of Octave");
  endif

  [sel, ok] = __event_manager_list_dialog__ (listcell, selmode, listsize,
                                             initialvalue, name, prompt,
                                             okstring, cancelstring);

endfunction


%!demo
%! disp ('- test listdlg with "SelectionMode"="single".  No caption, no prompt.');
%! itemlist = {"An item", "another", "yet another"};
%! s = listdlg ("ListString", itemlist, "SelectionMode", "Single");
%! imax = numel (s);
%! for i=1:1:imax
%!   disp (["Selection #", num2str(i), ": ", itemlist{s(i)}]);
%! endfor

%!demo
%! disp ('- test listdlg with "SelectionMode"="multiple" and pre-selection.  Has caption and two prompt lines.');
%! itemlist = {"An item", "another", "yet another"};
%! s = listdlg ("ListString", itemlist, ...
%!              "SelectionMode", "Multiple", ...
%!              "Name", "Selection Dialog", ...
%!              "InitialValue", [1,2,3],
%!              "PromptString", {"Select <b>an</b> item...",
%!                               "...or <b>multiple</b> items"});
%! imax = numel (s);
%! for i=1:1:imax
%!   disp (["Selection #", num2str(i), ": ", itemlist{s(i)}]);
%! endfor

%!demo
%! disp ("- test listdlg with ListSize.");
%! itemlist = {"Neutron", "Electron", "Quark", "Proton", "Neutrino"};
%! s = listdlg ("ListString", itemlist,
%!              "Name", "Bits and Pieces",
%!              "ListSize", [200 75]);
%! imax = numel (s);
%! for i=1:1:imax
%!   disp (["Selection #", num2str(i), ": ", itemlist{s(i)}]);
%! endfor

## Test input validation
%!error <Invalid call> listdlg ()
%!error <Invalid call> listdlg (1)
%!error listdlg ("SelectionMode")
%!error <must occur in pairs> listdlg ("SelectionMode", "multiple", "Name")
%!error <invalid KEY .FooBar.> listdlg ("FooBar", 1)
%!error <"ListString" property must not be empty> listdlg ("ListString", {})
%!error <"SelectionMode" must be "single" or "multiple">
%! listdlg ("ListString", {"A"}, "SelectionMode", "foobar");
