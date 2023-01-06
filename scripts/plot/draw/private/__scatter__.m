########################################################################
##
## Copyright (C) 2007-2023 The Octave Project Developers
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
## @deftypefn {} {@var{hs} =} __scatter__ (@dots{})
## Undocumented internal function.
## @end deftypefn

function hs = __scatter__ (varargin)

  hax = varargin{1};
  nd  = varargin{2};
  fcn = varargin{3};
  x   = varargin{4}(:);
  y   = varargin{5}(:);

  if (nd == 2)
    istart = 6;
  else
    z = varargin{6}(:);
    istart = 7;
  endif

  ## Force mixtures of int and float data to be float (Bug #4116).
  if (xor (isfloat (x), isfloat (y)))
    if (isfloat (x))
      y = cast (y, class (x));
    else
      x = cast (x, class (y));
    endif
  endif
  if (nd != 2)
    if (xor (isfloat (x), isfloat (z)))
      if (isfloat (x))
        z = cast (z, class (x));
      else
        x = cast (x, class (z));
        y = cast (y, class (z));
      endif
    endif
  endif

  if (istart <= nargin)
    s = varargin{istart}(:);
    if (isempty (s) || ischar (s))
      s = 36;
    endif
    if (! ischar (varargin{istart}))
      istart += 1;
    endif
  else
    s = 36;
  endif

  firstnonnumeric = find (! cellfun ("isnumeric", varargin(istart:nargin)), 1);
  if (isempty (firstnonnumeric))
    firstnonnumeric = Inf;
  else
    firstnonnumeric += istart - 1;
  endif

  if (istart <= nargin && firstnonnumeric > istart)
    c = varargin{istart};
    if (isvector (c) && columns (c) != 3)
      c = c(:);
    endif
  elseif (firstnonnumeric == istart && ischar (varargin{istart})
          && any (tolower (varargin{istart}(1)) == "ymcrgbwk"))
    [linespec, valid] = __pltopt__ (fcn, varargin{istart}, false);
    if (valid)
      c = varargin{istart};
      firstnonnumeric += 1;
    else
      c = [];
    endif
  else
    c = [];
  endif

  ## Remove NaNs
  idx = isnan (x) | isnan (y) | isnan (s);
  if (nd == 3)
    idx |= isnan (z);
    z(idx) = [];
  endif
  x(idx) = [];
  y(idx) = [];
  if (nd == 2)
    z = zeros (numel (x), 0);
  endif
  if (numel (s) > 1)
    s(idx) = [];
  endif
  if (rows (c) > 1)
    c(idx,:) = [];
  endif

  ## Validate inputs
  if (nd == 2 && ! size_equal (x, y))
    error ([fcn ": X and Y must have the same size"]);
  elseif (nd == 3 && ! size_equal (x, y, z))
    error ([fcn ": X, Y, and Z must have the same size"]);
  endif

  if (! isscalar (s) && ! size_equal (x, s))
    error ([fcn ": size of S must match X, Y, and Z"]);
  endif

  if (rows (c) > 1 && rows (c) != rows (x))
    error ([fcn ": number of colors in C must match number of points in X"]);
  endif

  newargs = {};
  filled = false;
  have_marker = false;
  marker = "o";
  iarg = firstnonnumeric;
  while (iarg <= nargin)
    arg = varargin{iarg++};
    if (ischar (arg) && (strcmpi (arg, "filled") || strcmpi (arg, "fill")))
      filled = true;
    elseif ((ischar (arg) || iscellstr (arg)) && ! have_marker)
      [linespec, valid] = __pltopt__ (fcn, arg, false);
      if (valid)
        ## Valid linestyle, but possibly not valid marker
        have_marker = true;
        marker = linespec.marker;
        if (strcmp (marker, "none"))
          marker = "o";
        elseif (isempty (marker))
          have_marker = false;
          marker = "o";
        endif
      else
        ## Prop/Val pair
        newargs{end+1} = arg;
        if (iarg <= nargin)
          newargs{end+1} = varargin{iarg++};
        endif
      endif
    else
      ## Prop/Val pair
      newargs{end+1} = arg;
      if (iarg <= nargin)
        newargs{end+1} = varargin{iarg++};
      endif
    endif
  endwhile

  if (strcmp ("gnuplot", graphics_toolkit ()))
    ## Legacy code using patch for gnuplot toolkit
    hs = __gnuplot_scatter__ (hax, fcn, x, y, z, c, s, marker, filled, newargs);

  else
    ## Use OpenGL rendering for "qt" and "fltk" graphics toolkits
    if (isempty (x))
      c = x;
    endif
    if (ischar (c))
      c = str2rgb (c);
    endif
    if (isempty (c))
      cdata_args = {};
    else
      cdata_args = {"cdata", c};
    endif
    if (filled)
      filled_args = {"markeredgecolor", "none", "markerfacecolor", "flat"};
    else
      filled_args = {};
    endif

    hs = __go_scatter__ (hax, "xdata", x(:), "ydata", y(:), "zdata", z(:),
                         cdata_args{:}, "sizedata", s(:), "marker", marker,
                         filled_args{:}, newargs{:});
  endif

endfunction

## Convert a color code to the corresponding RGB values
function rgb = str2rgb (str)

  rgb = [];

  switch (str)
    case 'b'
      rgb = [0, 0, 1];
    case 'k'
      rgb = [0, 0, 0];
    case 'r'
      rgb = [1, 0, 0];
    case 'g'
      rgb = [0, 1, 0];
    case 'y'
      rgb = [1, 1, 0];
    case 'm'
      rgb = [1, 0, 1];
    case 'c'
      rgb = [0, 1, 1];
    case 'w'
      rgb = [1, 1, 1];
  endswitch

endfunction
