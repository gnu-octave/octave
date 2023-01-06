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
## @deftypefn {} {[@var{h}, @var{fail}] =} __patch__ (@var{p}, @dots{})
## Undocumented internal function.
## @end deftypefn

## __patch__ (p, x, y, c)
## Create patch object from x and y with color c and parent p.
## Return handle to patch object.

function h = __patch__ (p, varargin)

  h = NaN;
  nargin = nargin - 1;

  is_numeric_arg = cellfun (@isnumeric, varargin);

  if (isempty (varargin))
    args = {};
  elseif (is_numeric_arg(1))
    if (nargin < 3 || ! is_numeric_arg(2))
      print_usage ("patch");
    endif

    if (nargin >= 4 && all (is_numeric_arg(1:4)))
      x = varargin{1};
      y = varargin{2};
      z = varargin{3};
      c = varargin{4};
      iarg = 5;
    elseif (nargin >= 3 && all (is_numeric_arg(1:3)))
      x = varargin{1};
      y = varargin{2};
      if (nargin > 3 && iscolorspec (varargin{4}))
        z = varargin{3};
        c = varargin{4};
        iarg = 5;
      else
        z = [];
        c = varargin{3};
        iarg = 4;
      endif
    elseif (nargin >= 3 && all (is_numeric_arg(1:2)))
      x = varargin{1};
      y = varargin{2};
      z = [];
      if (iscolorspec (varargin{3}))
        c = varargin{3};
        iarg = 4;
      elseif (nargin == 3)
        error ("patch: invalid color specification C");
      else
        c = [];
        iarg = 3;
      endif
    endif

    if (isvector (x))
      x = x(:);
      y = y(:);
      z = z(:);
      if (isnumeric (c))
        if (isvector (c))
          if (isequal (size (c), [1, 3]))
            ## Do nothing, this is a single RGB color specification
          elseif (numel (c) == numel (x))
            c = c(:);
          endif
        elseif (rows (c) != numel (x) && columns (c) == numel (x))
          c = c.';
        endif
      endif
    endif
    args{1} = "xdata";
    args{2} = x;
    args{3} = "ydata";
    args{4} = y;
    args{5} = "zdata";
    args{6} = z;

    if (isnumeric (c) && ! isempty (c))

      if (ndims (c) == 3 && columns (c) == 1)
        c = permute (c, [1, 3, 2]);
      endif

      if (isvector (c) && numel (c) == columns (x))
        ## One color per face
        if (isnan (c))
          args{7} = "facecolor";
          args{8} = [1, 1, 1];
        else
          args{7} = "facecolor";
          args{8} = "flat";
        endif
        args{9} = "cdata";
        args{10} = c;
      elseif (isrow (c) && numel (c) == 3)
        ## One RGB color
        args{7} = "facecolor";
        args{8} = c;
        args{9} = "cdata";
        args{10} = [];
      elseif (ndims (c) == 3 && size (c, 3) == 3)
        ## CDATA is specified as 3-D RGB data
        if ((rows (c) == 1 && columns (c) == 1) ...
            || (rows (c) == 1 && columns (c) == columns (x)))
          ## Single patch color or per-face color
          args{7} = "facecolor";
          args{8} = "flat";
          args{9} = "cdata";
          args{10} = c;
        elseif (rows (c) == rows (x) && columns (c) == columns (x))
          ## Per-vertex color
          args{7} = "facecolor";
          args{8} = "interp";
          args{9} = "cdata";
          args{10} = c;
        else
          error ("patch: invalid TrueColor data C");
        endif
      else
        ## Color vectors
        if (isempty (c))
          args{7} = "facecolor";
          args{8} = "interp";
          args{9} = "cdata";
          args{10} = [];
        elseif (size_equal (c, x) && size_equal (c, y))
          args{7} = "facecolor";
          args{8} = "interp";
          args{9} = "cdata";
          args{10} = c;
        else
          error ("patch: size of X, Y, and C must be equal");
        endif
      endif
    elseif (iscolorspec (c))
      ## Color specification is a string
      args{7} = "facecolor";
      args{8} = tolower (c);
      args{9} = "cdata";
      args{10} = [];
    elseif (! isempty (c))
      error ("patch: invalid color specification C");
    endif

    args = [args, varargin(iarg:end)];

  else
    ## "Property"/Value pair input
    args = varargin;
  endif

  if (isempty (p))
    p = gca ();
  endif
  h = __go_patch__ (p, args{:});

endfunction

function retval = iscolorspec (arg)

  retval = false;
  if (ischar (arg))
    persistent colors = {"y", "yellow", "r", "red", "m", "magenta", ...
                         "c", "cyan", "g", "green", "b", "blue", ...
                         "w", "white", "k", "black"};
    if (any (strcmpi (arg, colors)))
      retval = true;
    endif
  endif

endfunction
