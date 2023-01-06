########################################################################
##
## Copyright (C) 2019-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This filename is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the filename COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

## -*- texinfo -*-
## @deftypefn {} {[@var{@dots{}}] =} gui_mainfcn (@var{gui_state}, @dots{})
## Compatibility function for figures created with @sc{matlab}'s Guide.
##
## This function is undocumented and users should not use it in new code.
##
## @seealso{openfig, hgload, struct2hdl}
## @end deftypefn

function varargout = gui_mainfcn (gui_state, varargin)

  if (nargin == 1 || isempty (gui_state.gui_Callback))
    ## Open figure
    copies = ifelse (gui_state.gui_Singleton, "reuse", "new");
    if (isempty (gui_state.gui_LayoutFcn))
      filename = file_in_loadpath ([gui_state.gui_Name ".fig"]);
      H = openfig (filename, copies, "invisible");
    else
      H = feval (gui_LayoutFcn, copies);
    endif

    ## Set figure properties from input
    for i = 1:2:numel (varargin)
      try
        set (H, varargin{i}, varargin{i+1});
      catch
        break;
      end_try_catch
    endfor

    ## Store graphics handles in guidata
    handles = guihandles (H);
    guidata (H, handles);

    ## Execute opening function
    ## FIXME: According to comments in auto-generated examples, the opening
    ## function may block (with "uiwait").  But also the opening function should
    ## execute just before the figure is made visible.  How is this supposed to
    ## work?
    feval (gui_state.gui_OpeningFcn, H, [], handles, varargin{:});
    set (H, "visible", "on");
    handles = guidata (H);

    ## Execute output function
    varargout{1} = feval (gui_state.gui_OutputFcn, H, [], handles);
  else
    [varargout{1:nargout}] = feval (gui_state.gui_Callback, varargin{2:end});
  endif

endfunction
