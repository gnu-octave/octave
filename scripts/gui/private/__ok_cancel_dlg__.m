########################################################################
##
## Copyright (C) 2019-2023 The Octave Project Developers
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
## @deftypefn {} {[@var{hf}, @var{hok}, @var{hcancel}] =} __ok_cancel_dlg__ (@var{dlg_title})
## Undocumented internal function.
## @seealso{}
## @end deftypefn

function [hf, hok, hcancel, hpanel] = __ok_cancel_dlg__ (dlg_title, varargin)

  hf = dialog ("name", dlg_title, varargin{:});
  setappdata (hf, "__ok_cancel_btn__", "cancel");

  hpanel = uipanel (hf, "units", "pixels", "bordertype", "none");

  hok = uicontrol (hf, "style", "pushbutton", "string", "Ok");

  hcancel = uicontrol (hf, "style", "pushbutton", "string", "Cancel");

  cb_fix_button_position (hf, [], hcancel, hok, hpanel);
  set (hf, "sizechangedfcn", {@cb_fix_button_position, hcancel, hok, hpanel});

endfunction

function  cb_fix_button_position (hf, evt, hcancel, hok, hpanel)

  persistent margin = 20;
  persistent hgt = 30;
  persistent wd = 70;

  units = get (hf, "units");
  unwind_protect
    set (hf, "units", "pixels");
    pos = get (hf, "position");
    set (hok, "position",
         [(pos(3) - 2 * margin - 2 * wd), margin, wd, hgt]);
    set (hcancel, "position",
         [(pos(3) - margin - wd), margin, wd, hgt]);
    set (hpanel, "position", [0 60 pos(3) pos(4)-60]);
  unwind_protect_cleanup
    set (hf, "units", units);
  end_unwind_protect

endfunction
