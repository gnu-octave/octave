## Copyright (C) 2006, 2007 Michel D. Schmid
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

## Undocumented internal function

## Author: Michel D. Schmid <michaelschmid@users.sourceforge.net>
## Adapted-by: jwe

function h = __stem__ (have_z, varargin)

  [ax, varargin, nargin] = __plt_get_axis_arg__ ("stem", varargin{:});

  if (nargin < 1 || nargin > 4)
    print_usage ();
  endif

  [x, y, z, dofill, lc, ls, mc, ms] = check_stem_arg (have_z, varargin{:});

  if (dofill)
    fc = mc;
  else
    fc = "none";
  endif

  newplot ();
  nx = numel (x);
  xt = x(:)';
  xt = [xt; xt; NaN(1, nx)](:);
  if (have_z)
    yt = y(:)';
    yt = [yt; yt; NaN(1, nx)](:);
    zt = z(:)';
    zt = [zeros(1, nx); zt; NaN(1, nx)](:);
  else
    yt = y(:)';
    yt = [zeros(1, nx); yt; NaN(1, nx)](:);
  endif

  oldax = gca ();
  unwind_protect
    axes (ax);

    if (have_z)
      h_stems = plot3 (xt, yt, zt, "color", lc, "linestyle", ls,
		      x, y, z, "color", mc, "marker", ms, "linestyle", "",
		      "markerfacecolor", fc);

      h_baseline = [];
    else
      h_stems = plot (xt, yt, "color", lc, "linestyle", ls,
		      x, y, "color", mc, "marker", ms, "linestyle", "",
		      "markerfacecolor", fc);

      ## Must draw the plot first to get proper x limits.
      drawnow();
      x_axis_range = get (gca, "xlim");
      h_baseline = line (x_axis_range, [0, 0], "color", [0, 0, 0]);
    endif
  unwind_protect_cleanup
    axes (oldax);
  end_unwind_protect

  h = [h_stems; h_baseline];

endfunction

function [x, y, z, dofill, lc, ls, mc, ms] = check_stem_arg (have_z, varargin)

  ## set specifiers to default values
  [lc, ls, mc, ms] = set_default_values ();
  dofill = 0;
  fill_2 = 0;
  linespec_2 = 0;
  z = [];

  ## check input arguments
  if (nargin == 2)
    if (have_z)
      z = varargin{1};
      x = 1 : rows (z);
      y = 1 : columns (z);
    else
      y = varargin{1};
      if (isvector (y))
	x = 1:length(y);
      elseif (ismatrix (y))
	x = 1:rows(y);
      else 
	error ("stem: Y must be a matrix");
      endif # in each case, x & y will be defined
    endif
  elseif (nargin == 3)
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
      if (have_z)
	z = varargin{1};
	x = 1 : rows (z);
	y = 1 : columns (z);
      else
	y = varargin{1};
	if (isvector (y))
	  x = 1:length(y);
	elseif (ismatrix (y))
	  x = 1:rows(y);
	else
	  error ("stem: Y must be a matrix");
	endif # in each case, x & y will be defined
      endif
    else
      if (have_z)
	error ("stem3: must define X, Y and Z");
      else
	## must be the real y data
	x = varargin{1};
	y = varargin{2};
	if (! (ismatrix (x) && ismatrix (y)))
	  error ("stem: X and Y must be matrices");
	endif
      endif
    endif
  elseif (nargin == 4)
    ## again several possibilities
    ## arg2 1. real y
    ## arg2 2. 'fill' or linespec
    ## arg3 1. real z
    ## arg3 2. 'fill' or linespec
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
      if (have_z)
	z = varargin{1};
	x = 1 : rows (z);
	y = 1 : columns (z);
      else
	y = varargin{1};
	if (isvector (y))
	  x = 1:length(y);
	elseif (ismatrix (y))
	  x = 1:size(y,1);
	else
	  error ("stem: Y must be a matrix");
	endif # in each case, x & y will be defined
      endif
    else
      if (have_z)
	x = varargin{1};
	y = varargin{2};
	z = varargin{3};
	if (! (ismatrix (x) && ismatrix (y) && ismatrix (z)))
	  error ("stem3: X, Y and Z must be matrices");
	endif
      else
	## must be the real y data
	x = varargin{1};
	y = varargin{2};
	if (! (ismatrix (x) && ismatrix (y)))
	  error ("stem: X and Y must be matrices");
	endif
      endif
    endif # if ischar(varargin{2})
    if (! have_z)
      ## varargin{3} must be char...
      ## check for "fill" ..
      if (strcmp ("fill", varargin{3}) & fill_2)
	error ("stem:check_stem_arg: duplicate fill argument");
      elseif (strcmp("fill", varargin{3}) & linespec_2)
	## must be "fill"
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
    endif
  elseif (nargin == 5)
    if (have_z)
      x = varargin{1};
      y = varargin{2};
      z = varargin{3};
      if (! (ismatrix (x) && ismatrix (y) && ismatrix (z)))
	error ("stem3: X, Y and Z must be matrices");
      endif
    else
      x = varargin{1};
      y = varargin{2};
      if (! (ismatrix (x) && ismatrix (y)))
	error ("X and Y must be matrices");
      endif
    endif

    if (! have_z)
      if (strcmp ("fill", varargin{3}))
	dofill = 1;
	fill_2 = 1; # be sure, no second "fill" is in the arguments
      else
	## must be a linespec
	[lc, ls, mc, ms] = stem_line_spec (varargin{3});
	linespec_2 = 1;
      endif
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
  elseif (nargin == 6 && have_z)
    x = varargin{1};
    y = varargin{2};
    z = varargin{3};
    if (! (ismatrix (x) && ismatrix (y) && ismatrix (z)))
      error ("stem3: X, Y and Z must be matrices");
    endif

    if (strcmp ("fill", varargin{4}))
      dofill = 1;
      fill_2 = 1; # be sure, no second "fill" is in the arguments
    else
      ## must be a linespec
      [lc, ls, mc, ms] = stem_line_spec (varargin{4});
      linespec_2 = 1;
    endif

    ## check for "fill" ..
    if (strcmp ("fill", varargin{5}) & fill_2)
      error ("stem3:check_stem_arg: duplicate fill argument");
    elseif (strcmp ("fill", varargin{5}) & linespec_2)
      ## must be "fill"
      dofill = 1;
      fill_2 = 1;
    elseif (! strcmp ("fill", varargin{5}) & ! linespec_2)
      ## must be linespec
      [lc, ls, mc, ms] = stem_line_spec (varargin{5});
      linespec_2 = 1;
    endif
  elseif (have_z)
    error ("stem3: incorrect number of arguments");
  else
    error ("stem: incorrect number of arguments");
  endif

endfunction

function [lc, ls, mc, ms] = stem_line_spec (str)
  if (! ischar (str))
    error ("stem:stem_line_spec: wrong argument type, must be \"fill\" or a string of specifiers");
  endif
  [lc, ls, mc, ms] = set_default_values ();
  ## Parse the line specifier string.
  cur_props = __pltopt__ ("stem", str, false);
  for i = 1:length(cur_props)
    if (isfield (cur_props(i), "color") && ! isempty (cur_props(i).color)); # means line color
      mc = lc = cur_props(i).color;
    elseif (isfield (cur_props(i), "linestyle"))
      ls = cur_props(i).linestyle;
    elseif (isfield (cur_props(i), "marker") && ! strcmp (cur_props(i).marker, "none"))
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
