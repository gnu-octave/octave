## Copyright (C) 2010-2013 Shai Ayal
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
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{x}, @var{y}, @var{buttons}] =} __fltk_ginput__ (@var{n})
## Undocumented internal function.
## @end deftypefn

## This is ginput.m implementation for fltk.

function [x, y, button] = __fltk_ginput__ (n = -1)

  if (isempty (gca))
    error ("ginput: must have at least one axes");
  endif

  x = y = button = [];
  ginput_accumulator (0, 0, 0, 0);  # initialize accumulator

  unwind_protect

    orig_buttondownfcn = get (gca, "buttondownfcn");
    set (gca, "buttondownfcn", @ginput_buttondownfcn);

    orig_ginput_keypressfcn = get (gcf, "keypressfcn");
    set (gcf, "keypressfcn", @ginput_keypressfcn);

    do
      __fltk_check__ ();

      ## Release CPU.
      sleep (0.01);

      [x, y, n0, button] = ginput_accumulator (-1, 0, 0, 0);
    until (n0 == n || n0 < 0)

  unwind_protect_cleanup
    set (gca, "buttondownfcn", orig_buttondownfcn);
    set (gcf, "keypressfcn", orig_ginput_keypressfcn);
  end_unwind_protect

endfunction

function [x, y, n, button] = ginput_accumulator (mode, xn, yn, btn)
  persistent x y n button;

  if (mode == 0)
    ## Initialize.
    x = y = button = [];
    n = 0;
  elseif (mode == 1)
    ## Append mouse button or key press.
    x = [x; xn];
    y = [y; yn];
    button = [button; btn];
    n += 1;
  elseif (mode == 2)
    ## The end due to Enter.
    n = -1;
 endif

endfunction

function ginput_buttondownfcn (src, button)
  point = get (src, "currentpoint");
  ginput_accumulator (1, point(1,1), point(2,1), button);
endfunction

function ginput_keypressfcn (src, evt)
  point = get (gca, "currentpoint");
  key = evt.Key;
  if (key == "return")
    ## Enter key stops ginput.
    ginput_accumulator (2, NaN, NaN, NaN);
  else
    ginput_accumulator (1, point(1,1), point(2,1), uint8 (key(1)));
  endif
endfunction

