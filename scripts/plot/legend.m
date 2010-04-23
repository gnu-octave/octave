## Copyright (C) 2001, 2006, 2007, 2008, 2009 Laurent Mazet
## Copyright (C) 2006 John W. Eaton
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
## @deftypefn {Function File} {} legend (@var{st1}, @var{st2}, @dots{})
## @deftypefnx {Function File} {} legend (@var{st1}, @var{st2}, @dots{}, "location", @var{pos})
## @deftypefnx {Function File} {} legend (@var{matstr})
## @deftypefnx {Function File} {} legend (@var{matstr}, "location", @var{pos})
## @deftypefnx {Function File} {} legend (@var{cell})
## @deftypefnx {Function File} {} legend (@var{cell}, "location", @var{pos})
## @deftypefnx {Function File} {} legend ('@var{func}')
##
## Display a legend for the current axes using the specified strings
## as labels.  Legend entries may be specified as individual character
## string arguments, a character array, or a cell array of character
## strings.  Legend works on line graphs, bar graphs, etc.  A plot must
## exist before legend is called.
##
## The optional parameter @var{pos} specifies the location of the legend
## as follows:
##
## @multitable @columnfractions 0.06 0.14 0.80
## @item @tab north @tab
##   center top
## @item @tab south @tab
##   center bottom
## @item @tab east @tab
##   right center
## @item @tab west @tab
##   left center
## @item @tab northeast @tab
##   right top (default)
## @item @tab northwest @tab
##   left top
## @item @tab southeast @tab
##   right bottom
## @item @tab southwest @tab
##   left bottom
## @item 
## @item @tab outside @tab
##   can be appended to any location string
## @end multitable
##
## Some specific functions are directly available using @var{func}:
##
## @table @asis
## @item "show"
##   Show legends from the plot
## @item "hide"
## @itemx "off"
##   Hide legends from the plot
## @item "boxon"
##   Draw a box around legends
## @item "boxoff"
##   Withdraw the box around legends
## @item "left"
##   Text is to the left of the keys
## @item "right"
##   Text is to the right of the keys
## @end table
## @end deftypefn

function legend (varargin)

  [ca, varargin, nargin] = __plt_get_axis_arg__ ("legend", varargin{:});
  nargs = nargin;

  if (nargs > 0)
    pos = varargin{nargs};
    if (isnumeric (pos) && isscalar (pos) && round (pos) == pos)
      if (pos >= -1 && pos <= 4)
        set (ca, "keypos", pos);
        nargs--;
      else
        error ("legend: invalid position specified");
      endif
    endif
  endif
  
  if (nargs > 1)
    pos = varargin{nargs-1};
    str = varargin{nargs};
    if (strcmpi (pos, "location")  && ischar (str))
      set (ca, "keypos", str);
      nargs -= 2;
    endif
  endif

  kids = get (ca, "children");
  nkids = numel (kids);
  k = 1;
  turn_on_legend = false;

  if (nargs == 1)
    arg = varargin{1};
    if (ischar (arg))
      if (rows (arg) == 1)
        str = tolower (deblank (arg));
        switch (str)
          case {"off", "hide"}
            set (ca, "key", "off");
            nargs--;
          case "show"
            set (ca, "key", "on");
            nargs--;
          case "toggle"
            val = get (ca, "key");
            if (strcmpi (val, "on"))
              set (ca, "key", "off");
            else
              set (ca, "key", "on");
            endif
            nargs--;
          case "boxon"
            set (ca, "key", "on", "keybox", "on");
            nargs--;
          case "boxoff"
            set (ca, "keybox", "off");
            nargs--;
          case "left"
            set (ca, "keyreverse", "off")
            nargs--;
          case "right"
            set (ca, "keyreverse", "on")
            nargs--;
          otherwise
        endswitch
      else
        varargin = cellstr (arg);
        nargs = numel (varargin);
      endif
    elseif (iscellstr (arg))
      varargin = arg;
      nargs = numel (varargin);
    else
      error ("legend: expecting argument to be a character string");
    endif
  endif

  if (nargs > 0)
    have_data = false;
    for k = 1:nkids
      typ = get (kids(k), "type");
      if (strcmp (typ, "line") || strcmp (typ, "surface")
          || strcmp (typ, "patch") || strcmp (typ, "hggroup"))
        have_data = true;
        break;
      endif
    endfor
    if (! have_data)
      warning ("legend: plot data is empty; setting key labels has no effect");
    endif
  endif

  warned = false;
  k = nkids;
  for i = 1:nargs
    arg = varargin{i};
    if (ischar (arg))
      typ = get (kids(k), "type");
      while (k > 1
             && ! (strcmp (typ, "line") || strcmp (typ, "surface")
                   || strcmp (typ, "patch") || strcmp (typ, "hggroup")))
        typ = get (kids(--k), "type");
      endwhile
      if (k > 0)
        if (strcmp (get (kids(k), "type"), "hggroup"))
          hgkids = get (kids(k), "children");
          for j = 1 : length (hgkids)
            hgobj = get (hgkids (j));
            if (isfield (hgobj, "keylabel"))
              set (hgkids(j), "keylabel", arg);
              break;
            endif
          endfor
        else
          set (kids(k), "keylabel", arg);
        endif
        turn_on_legend = true;
        if (--k == 0)
          break;
        endif
      elseif (! warned)
        warned = true;
        warning ("legend: ignoring extra labels");
      endif
    else
      error ("legend: expecting argument to be a character string");
    endif
  endfor

  if (turn_on_legend)
    set (ca, "key", "on");
  endif

endfunction

%!demo
%! clf
%! plot(1:10, 1:10, 1:10, fliplr(1:10));
%! title("incline is blue and decline is green");
%! legend({"I'm blue", "I'm green"}, "location", "east")

%!demo
%! clf
%! plot(1:10, 1:10, 1:10, fliplr(1:10));
%! title("incline is blue and decline is green");
%! legend("I'm blue", "I'm green", "location", "east")

%!demo
%! clf
%! plot(1:10, 1:10);
%! title("a very long label can sometimes cause problems");
%! legend({"hello world"}, "location", "northeastoutside")

%!demo
%! clf
%! plot(1:10, 1:10);
%! title("a very long label can sometimes cause problems");
%! legend("hello world", "location", "northeastoutside")

%!demo
%! clf
%! labels = {};
%! for i = 1:5
%!     h = plot(1:100, i + rand(100,1)); hold on;
%!     set (h, "color", get (gca, "colororder")(i,:))
%!     labels = {labels{:}, cstrcat("Signal ", num2str(i))};
%! endfor; hold off;
%! title("Signals with random offset and uniform noise")
%! xlabel("Sample Nr [k]"); ylabel("Amplitude [V]");
%! legend(labels, "location", "southoutside")
%! legend("boxon")

%!demo
%! clf
%! labels = {};
%! for i = 1:5
%!     h = plot(1:100, i + rand(100,1)); hold on;
%!     set (h, "color", get (gca, "colororder")(i,:))
%!     labels = {labels{:}, cstrcat("Signal ", num2str(i))};
%! endfor; hold off;
%! title("Signals with random offset and uniform noise")
%! xlabel("Sample Nr [k]"); ylabel("Amplitude [V]");
%! legend(labels{:}, "location", "southoutside")
%! legend("boxon")

%!demo
%! clf
%! x = linspace (0, 10);
%! plot (x, x);
%! hold ("on");
%! stem (x, x.^2, 'g')
%! legend ("linear");
%! hold ("off");

%!demo
%! clf
%! x = linspace (0, 10);
%! plot (x, x, x, x.^2);
%! legend ("linear");

%!demo
%! clf
%! x = linspace (0, 10);
%! plot (x, x, x, x.^2);
%! legend ("linear", "quadratic");

%!demo
%! clf
%! bar (rand (2, 3))
%! ylim ([0 1.2])
%! legend ({"1st Bar", "2nd Bar", "3rd Bar"})

%!demo
%! clf
%! bar (rand (2, 3))
%! ylim ([0 1.2])
%! legend ("1st Bar", "2nd Bar", "3rd Bar")
