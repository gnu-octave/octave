## Copyright (C) 2007-2012 David Bateman
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
## @deftypefn  {Function File} {} fill (@var{x}, @var{y}, @var{c})
## @deftypefnx {Function File} {} fill (@var{x1}, @var{y1}, @var{c1}, @var{x2}, @var{y2}, @var{c2})
## @deftypefnx {Function File} {} fill (@dots{}, @var{prop}, @var{val})
## @deftypefnx {Function File} {} fill (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} fill (@dots{})
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
## @code{caxis} and then indexed into the current colormap.  A row vector will
## color each polygon (a column from matrices @var{x} and @var{y}) with a
## single computed color.  A matrix @var{c} of the same size as @var{x} and
## @var{y} will compute the color of each vertex and then interpolate the face
## color between the vertices.
##
## Multiple property/value pairs for the underlying patch object may be
## specified, but they must appear in pairs.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a vector of graphics handles to
## the created patch objects.
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
## @seealso{patch, caxis, colormap}
## @end deftypefn

function h = fill (varargin)

  [hax, varargin] = __plt_get_axis_arg__ ("fill", varargin{:});

  hlist = [];
  iargs = __find_patches__ (varargin{:});

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);
    old_nxtplt = get (hax, "nextplot");
    set (hax, "nextplot", "add");

    for i = 1 : length (iargs)
      if (i == length (iargs))
        args = varargin(iargs(i):end);
      else
        args = varargin(iargs(i):iargs(i+1)-1);
      endif
      [htmp, fail] = __patch__ (hax, args{:});
      if (fail)
        print_usage ();
      endif
      hlist(end + 1, 1) = htmp;
    endfor

    if (strcmp (old_nxtplt, "replace"))
      set (hax, "nextplot", old_nxtplt);
    endif

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
  iargs = [];
  i = 1;
  while (i < nargin)
    iargs(end + 1) = i;
    if (ischar (varargin{i})
        && (strcmpi (varargin{i}, "faces")
            || strcmpi (varargin{i}, "vertices")))
      i += 4;
    elseif (isnumeric (varargin{i}))
      i += 2;
    endif

    if (i <= nargin)
      while (true);
        if (ischar (varargin{i})
            && (strcmpi (varargin{i}, "faces")
                || strcmpi (varargin{i}, "vertices")))
          break;
        elseif (isnumeric (varargin{i}))
          ## Assume its the colorspec
          i++;
          break;
        elseif (ischar (varargin{i}))
          colspec = tolower (varargin{i});
          collen = length (colspec);
          if (any (strncmp (colspec, 
                            {"blue", "black", "k", "red", "green", ...
                             "yellow", "magenta", "cyan", "white"},
                            collen)))
            i++;
            break;
          endif
        else
          i += 2;
        endif
      endwhile
    endif
  endwhile

endfunction


%!demo
%! clf;
%! t1 = (1/16:1/8:1) * 2*pi;
%! t2 = ((1/16:1/8:1) + 1/32) * 2*pi;
%! x1 = sin (t1) - 0.8;
%! y1 = cos (t1);
%! x2 = sin (t2) + 0.8;
%! y2 = cos (t2);
%! h = fill (x1,y1,'r', x2,y2,'g');

