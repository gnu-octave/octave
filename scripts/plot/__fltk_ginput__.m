## Copyright (C) 2010-2011 Shai Ayal
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
## @deftypefn {Function File} {[@var{x}, @var{y}, @var{buttons}] =} __fltk_ginput__ (@var{f}, @var{n})
## Undocumented internal function.
## @end deftypefn

## This is ginput.m implementation for fltk.

function [x, y, button] = __fltk_ginput__ (f, n = -1)

  if (isempty (get (f, "currentaxes")))
    error ("ginput: must have at least one axes");
  endif

  x = y = button = [];
  ginput_aggregator (0, 0, 0);

  unwind_protect

    orig_windowbuttondownfcn = get (f, "windowbuttondownfcn");
    set (f, "windowbuttondownfcn", @ginput_windowbuttondownfcn);

    orig_ginput_keypressfcn = get (f, "keypressfcn");
    set (f, "keypressfcn", @ginput_keypressfcn);
    
    while (true)
      __fltk_redraw__ ();
      
      ## release CPU
      sleep (0.01);

      [x, y, n0] = ginput_aggregator (-1, 0, 0);
      if (n0 == n || n0 < 0)
        break;
      endif
    endwhile
    
    ## FIXME -- got to get the buttons somehow
    button = ones (size (x));
  unwind_protect_cleanup
    set (f, "windowbuttondownfcn", orig_windowbuttondownfcn);
    set (f, "keypressfcn", orig_ginput_keypressfcn);
  end_unwind_protect

endfunction

function [x, y, n] = ginput_aggregator (mode , xn, yn)
  persistent x y n;

  if (mode == 0)
    x = [];
    y = [];
    n = 0;
  elseif (mode == 1)
    x = [x; xn];
    y = [y; yn];
    n += 1;
  elseif (mode == 2)
    n = -1;
  endif
endfunction

function ginput_windowbuttondownfcn (src, data)
  point = get (get (src,"currentaxes"), "currentpoint");
  ginput_aggregator (1, point(1,1), point(2,1));
endfunction

function ginput_keypressfcn (src, evt)
  if (evt.Key == 10)    # linefeed character
    ginput_aggregator (2, 0, 0);
  endif
endfunction

