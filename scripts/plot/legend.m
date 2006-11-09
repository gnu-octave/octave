## Copyright (C) 2001 Laurent Mazet
## Copyright (C) 2006 John W. Eaton
##
## This program is free software; it is distributed in the hope that it
## will be useful, but WITHOUT ANY WARRANTY; without even the implied
## warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See
## the GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this file; see the file COPYING.  If not, write to the
## Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {} legend (@var{st1}, @var{st2}, @var{st3}, @var{...})
## @deftypefnx {Function File} {} legend (@var{st1}, @var{st2}, @var{st3}, @var{...}, @var{pos})
## @deftypefnx {Function File} {} legend (@var{matstr})
## @deftypefnx {Function File} {} legend (@var{matstr}, @var{pos})
## @deftypefnx {Function File} {} legend (@var{cell})
## @deftypefnx {Function File} {} legend (@var{cell}, @var{pos})
## @deftypefnx {Function File} {} legend ('@var{func}')
##
## Legend puts a legend on the current plot using the specified strings
## as labels. Use independant strings (@var{st1}, @var{st2}, @var{st3}...), a
## matrix of strings (@var{matstr}), or a cell array of strings (@var{cell}) to
## specify legends. Legend works on line graphs, bar graphs, etc...
## Be sure to call plot before calling legend.
##
## @var{pos} optionally  places the legend in the specified location:
##
## @multitable @columnfractions 0.1 0.1 0.8
## @item @tab 0 @tab
##   Don't move the legend box (default)
## @item @tab 1 @tab
##   Upper right-hand corner
## @item @tab 2 @tab
##   Upper left-hand corner
## @item @tab 3 @tab
##   Lower left-hand corner
## @item @tab 4 @tab
##   Lower right-hand corner
## @item @tab -1 @tab
##   To the top right of the plot
## @item @tab -2 @tab
##   To the bottom right of the plot
## @item @tab -3 @tab
##   To the bottom of the plot
## @item @tab [@var{x}, @var{y}] @tab
##   To the arbitrary postion in plot [@var{x}, @var{y}]
## @end multitable
##
## Some specific functions are directely avaliable using @var{func}:
##
## @table @code
## @item show
##   Show legends from the plot
## @item hide
## @itemx off
##   Hide legends from the plot
## @item boxon
##   Draw a box around legends
## @item boxoff
##   Withdraw the box around legends
## @item left
##   Text is to the left of the keys
## @item right
##   Text is to the right of the keys
## @end table
## @end deftypefn

## PKG_ADD mark_as_command legend

function legend (varargin)

  __plot_globals__;

  cf = __current_figure__;
  mxi = __multiplot_xi__;
  myi = __multiplot_yi__;

  nargs = nargin;

  if (nargs > 0)
    pos = varargin{nargs};
    if (isnumeric (pos) && isscalar (pos) && round (pos) == pos)
      if (pos >= -3 && pos <= 4)
	__plot_key_properties__{cf}{mxi,myi}.position = pos;
	nargs--;
      else
	error ("legend: invalid position specified");
      endif
    endif
  endif

  if (nargs == 1)
    arg = varargin{1};
    if (ischar (arg))
      if (rows (arg) == 1)
	str = tolower (deblank (arg));
	switch (str)
	  case {"off", "hide"}
	    __plot_key_properties__{cf}{mxi,myi}.visible = false;
	  case "show"
	    __plot_key_properties__{cf}{mxi,myi}.visible = true;
	  case "toggle"
	    __plot_key_properties__{cf}{mxi,myi}.visible ...
	      = ! __plot_key_properties__{cf}{mxi,myi}.visible;
	  case "boxon"
	    __plot_key_properties__{cf}{mxi,myi}.visible = true;
	    __plot_key_properties__{cf}{mxi,myi}.box = true;
	  case "boxoff"
	    __plot_key_properties__{cf}{mxi,myi}.box = false;
	  otherwise
	    __plot_key_labels__{cf}{mxi,myi}{1} = arg;
	endswitch
	nargs--;
      else
	varargin = cellstr (arg);
	nargs = numel (vargin);
      endif
    elseif (iscellstr (arg))
      varargin = arg;
      nargs = numel (varargin);
    else
      error ("legend: expecting argument to be a character string");
    endif
  endif

  for i = 1:nargs
    arg = varargin{i};
    if (ischar (arg))
      __plot_key_labels__{cf}{mxi,myi}{i} = arg;
    else
      error ("legend: expecting argument to be a character string");
    endif
  endfor

  if (automatic_replot)
    replot ();
  endif

endfunction

%!demo
%! close all;
%! plot(1:10, 1:10);
%! title("a very long label can sometimes cause problems");
%! legend({"hello world"}, -1)

%!demo
%! close all;
%! labels = {};
%! for i = 1:5
%!     plot(1:100, i + rand(100,1)); hold on;
%!     labels = {labels{:}, strcat("Signal ", num2str(i))};
%! endfor; hold off;
%! title("Signals with random offset and uniform noise")
%! xlabel("Sample Nr [k]"); ylabel("Amplitude [V]");
%! legend(labels, -1)
%! legend("boxon")
