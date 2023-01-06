########################################################################
##
## Copyright (C) 2008-2023 The Octave Project Developers
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
## @deftypefn  {} {@var{h} =} findall ()
## @deftypefnx {} {@var{h} =} findall (@var{prop_name}, @var{prop_value}, @dots{})
## @deftypefnx {} {@var{h} =} findall (@var{prop_name}, @var{prop_value}, "-@var{logical_op}", @var{prop_name}, @var{prop_value})
## @deftypefnx {} {@var{h} =} findall ("-property", @var{prop_name})
## @deftypefnx {} {@var{h} =} findall ("-regexp", @var{prop_name}, @var{pattern})
## @deftypefnx {} {@var{h} =} findall (@var{hlist}, @dots{})
## @deftypefnx {} {@var{h} =} findall (@var{hlist}, "flat", @dots{})
## @deftypefnx {} {@var{h} =} findall (@var{hlist}, "-depth", @var{d}, @dots{})
## Find graphics object, including hidden ones, with specified properties.
##
## The return value @var{h} is a list of handles to the found graphic objects.
##
## @code{findall} performs the same search as @code{findobj}, but it
## includes hidden objects (HandleVisibility = @qcode{"off"}).  For full
## documentation, @pxref{XREFfindobj,,@code{findobj}}.
## @seealso{findobj, allchild, get, set}
## @end deftypefn

function h = findall (varargin)

  unwind_protect
    shh = get (0, "showhiddenhandles");
    set (0, "showhiddenhandles", "on");
    h = findobj (varargin{:});
  unwind_protect_cleanup
    set (0, "showhiddenhandles", shh);
  end_unwind_protect

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = findall (hf);
%!   types = {"uitoolbar"};
%!   htb = uitoolbar (hf);
%!   types = [types {"uimenu", "uimenu", "uimenu", ...
%!                   "uimenu", "uimenu", "uimenu"}];
%!   hm1 = uimenu (hf, "label", "menu1", "handlevisibility", "off");
%!   uimenu (hm1, "label", "menu1");
%!   hm2 = uimenu (hf, "label", "menu2", "handlevisibility", "off");
%!   uimenu (hm2, "label", "menu2");
%!   hm3 = uimenu (hf, "label", "menu3");
%!   uimenu (hm3, "label", "menu3");
%!   types = [types {"uipushtool", "uitoggletool", "uipushtool"}];
%!   uipushtool (htb, "handlevisibility", "off");
%!   uitoggletool (htb);
%!   uipushtool (htb);
%!   h = setxor (findall (hf), h);
%!   assert (get (h, "type"), types(:));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect
