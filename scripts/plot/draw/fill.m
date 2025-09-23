########################################################################
##
## Copyright (C) 2007-2025 The Octave Project Developers
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
## @deftypefn  {} {} fill (@var{x}, @var{y}, @var{c})
## @deftypefnx {} {} fill (@var{x1}, @var{y1}, @var{c1}, @var{x2}, @var{y2}, @var{c2})
## @deftypefnx {} {} fill (@dots{}, @var{prop}, @var{val})
## @deftypefnx {} {} fill (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} fill (@dots{})
## Create one or more filled 2-D polygons.
##
## The inputs @var{x} and @var{y} are the coordinates of the polygon vertices.
## If the inputs are matrices then the rows represent different vertices and
## each column produces a different polygon.  @code{fill} will close any open
## polygons before plotting.
##
## The input @var{c} determines the color of the polygon.  The simplest form
## is a single color specification such as a @code{plot} format or an
## RGB-triple.  In this case the polygon(s) will have one unique color.  If
## @var{c} is a vector or matrix then the color data is first scaled using
## @code{clim} and then indexed into the current colormap.  A vector will color
## each polygon (a column from matrices @var{x} and @var{y}) with a single
## computed color.  A matrix @var{c} of the same size as @var{x} and @var{y}
## will compute the color of each vertex and then interpolate the face color
## between the vertices.
##
## Multiple property/value pairs for the underlying patch object may be
## specified, but they must appear in pairs.  The full list of properties is
## documented at @ref{Patch Properties}.
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a vector of graphics handles to the
## created patch objects.
##
## Example: red square
##
## @example
## @group
## vertices = [0 0
##             1 0
##             1 1
##             0 1];
## fill (vertices(:,1), vertices(:,2), "r");
## axis ([-0.5 1.5, -0.5 1.5])
## axis equal
## @end group
## @end example
##
## @seealso{patch, fill3, clim, colormap}
## @end deftypefn

function h = fill (varargin)

  [hax, varargin] = __plt_get_axis_arg__ ("fill", varargin{:});

  hlist = [];
  iargs = __find_patches__ (varargin{:});

  opts = {};
  if (numel (varargin) > iargs(end) + 2)
    opts = varargin(iargs(end)+3 : end);
  endif

  if (! all (cellfun (@(x) iscolorspec (x), varargin(iargs + 2))))
    print_usage ();
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);
    old_nxtplt = get (hax, "nextplot");
    if (! ishold ())
      set (hax, "box", "on");
    endif
    unwind_protect
      set (hax, "nextplot", "add");

      for i = 1 : numel (iargs)
        x = varargin{iargs(i)};
        y = varargin{iargs(i) + 1};
        cdata = varargin{iargs(i) + 2};

        ## FIXME: Probably should validate that x, y, cdata are 2-D.

        if (isrow (x))
          x = x(:);
        endif
        if (isrow (y))
          y = y(:);
        endif

        if (! size_equal (x, y))
          if (iscolumn (x))
            rx = rows (x);
            [ry, cy] = size (y);
            if (rx == ry)
              x = repmat (x, [1, cy]);
            elseif (rx == cy)
              y = y.';
              x = repmat (x, [1, ry]);
            else
              error ("fill: vector X and matrix Y must have a length which matches along one dimension");
            endif
          elseif (iscolumn (y))
            ry = rows (y);
            [rx, cx] = size (x);
            if (ry == rx)
              y = repmat (y, [1, cx]);
            elseif (ry == cx)
              x = x.';
              y = repmat (y, [1, rx]);
            else
              error ("fill: matrix X and vector Y must have a length which matches along one dimension");
            endif
          else
            error ("fill: matrices X and Y must be the same size");
          endif
        endif

        ## Test for color specification as text ('r') or RGB triple.
        if (ischar (cdata) ||
            (all (size (cdata) == [1, 3]) && all (cdata >= 0 & cdata <= 1)))
          one_color = true;
        else
          one_color = false;
        endif

        ## Manage cdata to ensure for loop below works
        if (! one_color && isvector (cdata))
          if (numel (cdata) == columns (x))
            ## One color per polygon
            cdata = cdata(:).';
          elseif (numel (cdata) == rows (x))
            ## Vertex colors.  Replicate cdata to match size of data.
            cdata = repmat (cdata(:), [1, columns(x)]);
          else
            error ("fill: invalid format for color data C");
          endif
        endif

        ## For Matlab compatibility, return 1 patch object for each column
        for j = 1 : columns (x)
          if (one_color)
            htmp = __patch__ (hax, x(:,j), y(:,j), cdata, opts{:});
          else
            htmp = __patch__ (hax, x(:,j), y(:,j), cdata(:,j), opts{:});
          endif
          hlist(end+1, 1) = htmp;
        endfor

      endfor

    unwind_protect_cleanup
      if (strcmp (old_nxtplt, "replace"))
        set (hax, "nextplot", old_nxtplt);
      endif
    end_unwind_protect

  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

  if (nargout > 0)
    h = hlist;
  endif

endfunction

function iargs = __find_patches__ (varargin)
  iargs = 1:3:nargin;
  optidx = find (! cellfun ('isnumeric', varargin(iargs)), 1);
  iargs(optidx:end) = [];
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
  elseif (isnumeric (arg))
    ## Assume any numeric argument is correctly formatted cdata.
    ## Let patch worry about the multiple different input formats.
    retval = true;
  endif

endfunction


%!demo
%! clf;
%! t1 = (1/16:1/8:1) * 2*pi;
%! t2 = ((1/16:1/8:1) + 1/32) * 2*pi;
%! x1 = sin (t1) - 0.8;
%! y1 = cos (t1);
%! x2 = sin (t2) + 0.8;
%! y2 = cos (t2);
%! h = fill (x1,y1,"r", x2,y2,"g");
%! title ({"fill() function"; "cdata specified with string"});

%!demo
%! clf;
%! t1 = (1/16:1/8:1) * 2*pi;
%! t2 = ((1/16:1/8:1) + 1/32) * 2*pi;
%! x1 = sin (t1) - 0.8;
%! y1 = cos (t1);
%! x2 = sin (t2) + 0.8;
%! y2 = cos (t2);
%! h = fill (x1,y1,1, x2,y2,2);
%! title ({"fill() function"; 'cdata = row vector produces FaceColor = "flat"'});

%!demo
%! clf;
%! x = [0 0
%!      1 0.5
%!      1 0.5
%!      0 0];
%! y = [0 0
%!      0 0
%!      1 0.5
%!      1 0.5];
%! c = [1 2 3 4]';
%! fill (x, y, [c c]);
%! title ({"fill() function"; 'cdata = column vector produces FaceColor = "interp"'});
