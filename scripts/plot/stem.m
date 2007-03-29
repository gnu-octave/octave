## Copyright (C) 2006 Michel D. Schmid
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{h} =} stem (@var{x}, @var{y}, @var{linespec})
## Plot a stem graph and return the handles of hte line and marker
## objects used to draw the stems.  The default color is @code{"r"}
## (red). The default line style is @code{"-"} and the default marker is
## @code{"o"}.
##
## For example,
## @example
## x = 1:10;
## stem (x);
## @end example
## @noindent
## plots 10 stems with hights from 1 to 10;
##
## @example
## x = 1:10;
## y = ones (1, length (x))*2.*x;
## stem (x, y);
## @end example
## @noindent
## plots 10 stems with hights from 2 to 20;
## 
## @example
## x = 1:10;
## y = ones (size (x))*2.*x;
## h = stem (x, y, "b");
## @end example
## @noindent
## plots 10 bars with hights from 2 to 20
## (the color is blue, and @var{h} is a 2-by-10 array of handles in
## which the first row holds the line handles and the 
## the second row holds the marker handles);
##
## @example
## x = 1:10;
## y = ones (size (x))*2.*x;
## h = stem (x, y, "-.k");
## @end example
## @noindent
## plots 10 stems with hights from 2 to 20
## (the color is black, line style is @code{"-."}, and @var{h} is a 2-by-10
## array of handles in which the first row holds the line handles and
## the second rows holds the marker handles);
##
## @example
## x = 1:10;
## y = ones (size (x))*2.*x;
## h = stem (x, y, "-.k.");
## @end example
## @noindent
## plots 10 stems with hights from 2 to 20
## (the color is black, line style is @code{"-."} and the marker style
## is @code{"."}, and @var{h} is a 2-by-10 array of handles in which the
## first row holds the line handles and the second row holds the marker
## handles);
##
## @example
## x = 1:10;
## y = ones (size (x))*2.*x;
## h = stem (x, y, "fill");
## @end example
## @noindent
## plots 10 stems with hights from 2 to 20
## (the color is rgb-triple defined, the line style is @code{"-"},
## the marker style is @code{"o"}, and @var{h} is a 2-by-10 array of
## handles in which the first row holds the line handles and the second
## row holds the marker handles).
##
## Color definitions with rgb-triples are not valid!
##
## @seealso{bar, barh, plot}
## @end deftypefn

## Author: Michel D. Schmid <michaelschmid@users.sourceforge.net>
## Adapted-by: jwe

function h = stem (varargin)

  if (nargin < 1 || nargin > 4)
    print_usage ();
  endif

  [x, y, dofill, lc, ls, mc, ms] = check_stem_arg (varargin{:});

  newplot ();

  ## first, plot the lines.. without marker
  ## Use a loop and calls to line here because setting properties this
  ## way doesn't work with plot yet.
  idxhh = 0;
  for i = 1:numel(x)
    hh(++idxhh) = line ([x(i); x(i)], [0; y(i)], "color", lc, "linestyle", ls);
  endfor

  ## second, plot the markers..
  hhh = [];
  hhhh = [];

  ## Use a loop and calls to line here because setting properties this
  ## way doesn't work with plot yet.
  idxhhh = 0;
  for i = 1:numel(x)
    hhh(++idxhhh) = line ([x(i); x(i)], [y(i); y(i)]);
  endfor

  if (find (y < 0))
    x_axis_range = get (gca, "xlim");
    hhhh = line (x_axis_range, [0, 0], "color", "k");
  endif

  if (dofill)
    set (hhh, "markerfacecolor", mc);
  endif

  if (nargout > 0)
    if (! isempty (hhhh))
      hhhh = hhhh*(ones (length (hh), 1))';
    endif
    h = [hh; hhh; hhhh];
  endif

endfunction

function [x, y, dofill, lc, ls, mc, ms] = check_stem_arg (varargin)

  ## set specifiers to default values
  [lc, ls, mc, ms] = set_default_values ();
  dofill = 0;
  fill_2 = 0;
  linespec_2 = 0;

  ## check input arguments
  if (nargin == 1)
    y = varargin{1};
    if (isvector (y))
      x = 1:length(y);
    elseif (ismatrix (y))
      x = 1:rows(y);
    else 
      error ("stem: Y must be a matrix");
    endif # in each case, x & y will be defined

  elseif (nargin == 2)
    ## several possibilities
    ## 1. the real y data
    ## 2. 'fill'
    ## 3. line spec
    if (ischar (varargin{2}))
      ## only 2. or 3. possible
      if (strcmp ("fill", varargin{2}))
	dofill = 1;
      else
	## parse the linespec
	[lc, ls, mc, ms] = stem_line_spec (varargin{2});
      endif
      y = varargin{1};
      if (isvector (y))
	x = 1:length(y);
      elseif (ismatrix (y))
	x = 1:rows(y);
      else
	error ("stem: Y must be a matrix");
      endif # in each case, x & y will be defined
    else
      ## must be the real y data
      x = varargin{1};
      y = varargin{2};
      if (! (ismatrix (x) && ismatrix (y)))
	error ("stem: X and Y must be matrices");
      endif
    endif
  elseif (nargin == 3)
    ## again several possibilities
    ## arg2 1. real y
    ## arg2 2. 'fill' or linespec
    ## arg3 1. 'fill' or linespec
    if (ischar (varargin{2}))
      ## only arg2 2. / arg3 1. & arg3 3. are possible
      if (strcmp ("fill", varargin{2}))
	dofill = 1;
	fill_2 = 1; # be sure, no second "fill" is in the arguments
      else
	## must be a linespec
	[lc, ls, mc, ms] = stem_line_spec (varargin{2});
	linespec_2 = 1;
      endif
      y = varargin{1};
      if (isvector (y))
	x = 1:length(y);
      elseif (ismatrix (y))
	x = 1:size(y,1);
      else
	error ("stem: Y must be a matrix");
      endif # in each case, x & y will be defined
    else
      ## must be the real y data
      x = varargin{1};
      y = varargin{2};
      if (! (ismatrix (x) && ismatrix (y)))
	error ("stem: X and Y must be matrices");
      endif
    endif # if ischar(varargin{2})
    ## varargin{3} must be char...
    ## check for "fill" ..
    if (strcmp ("fill", varargin{3}) & fill_2)
      error ("stem:check_stem_arg: duplicate fill argument");
    elseif (strcmp("fill", varargin{3}) & linespec_2)
      # must be "fill"
      dofill = 1;
      fill_2 = 1;
    elseif (strcmp ("fill", varargin{3}) & ! linespec_2)
      ## must be "fill"
      dofill = 1;
      fill_2 = 1;
    elseif (! linespec_2)
      ## must be linespec
      [lc, ls, mc, ms] = stem_line_spec (varargin{3});
      linespec_2 = 1;
    endif
  elseif (nargin == 4)
    x = varargin{1};
    y = varargin{2};
    if (! (ismatrix (x) && ismatrix (y)))
      error ("X and Y must be matrices");
    endif

    if (strcmp ("fill", varargin{3}))
      dofill = 1;
      fill_2 = 1; # be sure, no second "fill" is in the arguments
    else
      ## must be a linespec
      [lc, ls, mc, ms] = stem_line_spec (varargin{3});
      linespec_2 = 1;
    endif

    ## check for "fill" ..
    if (strcmp ("fill", varargin{4}) & fill_2)
      error ("stem:check_stem_arg: duplicate fill argument");
    elseif (strcmp ("fill", varargin{4}) & linespec_2)
      ## must be "fill"
      dofill = 1;
      fill_2 = 1;
    elseif (! strcmp ("fill", varargin{4}) & ! linespec_2)
      ## must be linespec
      [lc, ls, mc, ms] = stem_line_spec (varargin{4});
      linespec_2 = 1;
    endif
  endif

endfunction

function [lc, ls, mc, ms] = stem_line_spec (str)
  if (! ischar (str))
    error ("stem:stem_line_spec: wrong argument type, must be \"fill\" or a string of specifiers");
  endif
  [lc, ls, mc, ms] = set_default_values ();
  ## Parse the line specifier string.
  cur_props = __pltopt__ ("stem", str);
  for i = 1:length(cur_props)
    if (isfield (cur_props(i), "markeredgecolor"))
      mc = cur_props(i).markeredgecolor;
    elseif (isfield (cur_props(i), "color")); # means line color
      lc = cur_props(i).color;
    elseif (isfield (cur_props(i), "linestyle"))
      ls = cur_props(i).linestyle;
    elseif (isfield (cur_props(i), "marker"))
      ms = cur_props(i).marker;
    endif
  endfor
endfunction

function [lc, ls, mc, ms] = set_default_values ()
  ## set default values
  mc = [1, 0, 0];
  lc = [1, 0, 0];
  ls = "-";
  ms = "o";
endfunction
