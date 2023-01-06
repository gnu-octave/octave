########################################################################
##
## Copyright (C) 2005-2023 The Octave Project Developers
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
## @deftypefn  {} {} patch ()
## @deftypefnx {} {} patch (@var{x}, @var{y}, @var{c})
## @deftypefnx {} {} patch (@var{x}, @var{y}, @var{z}, @var{c})
## @deftypefnx {} {} patch ("Faces", @var{faces}, "Vertices", @var{verts}, @dots{})
## @deftypefnx {} {} patch (@dots{}, "@var{prop}", @var{val}, @dots{})
## @deftypefnx {} {} patch (@dots{}, @var{propstruct}, @dots{})
## @deftypefnx {} {} patch (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} patch (@dots{})
## Create patch object in the current axes with vertices at locations
## (@var{x}, @var{y}) and of color @var{c}.
##
## If the vertices are matrices of size @nospell{MxN} then each polygon patch
## has M vertices and a total of N polygons will be created.  If some polygons
## do not have M vertices use NaN to represent "no vertex".  If the @var{z}
## input is present then 3-D patches will be created.
##
## The color argument @var{c} can take many forms.  To create polygons
## which all share a single color use a string value (e.g., @qcode{"r"} for
## red), a scalar value which is scaled by @code{caxis} and indexed into the
## current colormap, or a 3-element RGB vector with the precise TrueColor.
##
## If @var{c} is a vector of length N then the ith polygon will have a color
## determined by scaling entry @var{c}(i) according to @code{caxis} and then
## indexing into the current colormap.  More complicated coloring situations
## require directly manipulating patch property/value pairs.
##
## Instead of specifying polygons by matrices @var{x} and @var{y}, it is
## possible to present a unique list of vertices and then a list of polygon
## faces created from those vertices.  In this case the
## @qcode{"Vertices"} matrix will be an @nospell{Nx2} (2-D patch) or
## @nospell{Nx3} (3-D patch).  The @nospell{MxN} @qcode{"Faces"} matrix
## describes M polygons having N vertices---each row describes a
## single polygon and each column entry is an index into the
## @qcode{"Vertices"} matrix to identify a vertex.  The patch object
## can be created by directly passing the property/value pairs
## @qcode{"Vertices"}/@var{verts}, @qcode{"Faces"}/@var{faces} as
## inputs.
##
## Instead of using property/value pairs, any property can be set by passing a
## structure @var{propstruct} with the respective field names.
##
## If the first argument @var{hax} is an axes handle, then plot into this axes,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created patch
## object.
##
## Programming Note: The full list of properties is documented at
## @ref{Patch Properties}.  Useful patch properties include:
## @qcode{"cdata"}, @qcode{"edgecolor"}, @qcode{"facecolor"}, @qcode{"faces"},
## and @qcode{"facevertexcdata"}.
## @seealso{fill, get, set}
## @end deftypefn

function h = patch (varargin)

  [hax, varargin] = __plt_get_axis_arg__ ("patch", varargin{:});

  if (! isempty (hax))
    hax = hax(1);
  endif

  htmp = __patch__ (hax, varargin{:});

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! t1 = (1/16:1/8:1)' * 2*pi;
%! t2 = ((1/16:1/8:1)' + 1/32) * 2*pi;
%! x1 = sin (t1) - 0.8;
%! y1 = cos (t1);
%! x2 = sin (t2) + 0.8;
%! y2 = cos (t2);
%! patch ([x1,x2], [y1,y2], "r");
%! title ("patches with same number of vertices");

%!demo
%! clf;
%! t1 = (1/16:1/8:1)' * 2*pi;
%! t2 = ((1/16:1/16:1)' + 1/32) * 2*pi;
%! x1 = sin (t1) - 0.8;
%! y1 = cos (t1);
%! x2 = sin (t2) + 0.8;
%! y2 = cos (t2);
%! patch ([[x1;NaN(8,1)],x2], [[y1;NaN(8,1)],y2], "r");
%! title ("Unclosed patch by using NaN");

%!demo
%! clf;
%! t1 = (1/16:1/8:1)' * 2*pi;
%! t2 = ((1/16:1/16:1)' + 1/32) * 2*pi;
%! x1 = sin (t1) - 0.8;
%! y1 = cos (t1);
%! x2 = sin (t2) + 0.8;
%! y2 = cos (t2);
%! vert = [x1, y1; x2, y2];
%! fac = [1:8,NaN(1,8);9:24];
%! patch ("Faces",fac, "Vertices",vert, "FaceColor","r");
%! title ("patch() with separate specification of Faces and Vertices");

%!demo
%! clf;
%! t1 = (1/16:1/8:1)' * 2*pi;
%! t2 = ((1/16:1/16:1)' + 1/32) * 2*pi;
%! x1 = sin (t1) - 0.8;
%! y1 = cos (t1);
%! x2 = sin (t2) + 0.8;
%! y2 = cos (t2);
%! vert = [x1, y1; x2, y2];
%! p.Faces = [1:8,NaN(1,8);9:24];
%! p.FaceColor = "flat";
%! patch (p, 'Vertices', vert, 'FaceVertexCData', [0, 1, 0; 0, 0, 1]);
%! title ("patch() with specification of color for each vertex");

%!demo
%! ## Property change on multiple patches
%! clf;
%! t1 = (1/16:1/8:1)' * 2*pi;
%! t2 = ((1/16:1/8:1)' + 1/32) * 2*pi;
%! x1 = sin (t1) - 0.8;
%! y1 = cos (t1);
%! x2 = sin (t2) + 0.8;
%! y2 = cos (t2);
%! h = patch ([x1,x2], [y1,y2], cat (3, [0,0],[1,0],[0,1]));
%! pause (1);
%! set (h, "FaceColor", "r");
%! title ("change color on multiple patch() objects");

%!demo
%! clf;
%! vertices = [0, 0, 0;
%!             1, 0, 0;
%!             1, 1, 0;
%!             0, 1, 0;
%!             0.5, 0.5, 1];
%! faces = [1, 2, 5;
%!          2, 3, 5;
%!          3, 4, 5;
%!          4, 1, 5];
%! patch ("Vertices", vertices, "Faces", faces, ...
%!        "FaceVertexCData", jet (4), "FaceColor", "flat");
%! view (-37.5, 30);
%! box off;
%! title ('"FaceColor" = "flat"');

%!demo
%! clf;
%! vertices = [0, 0, 0;
%!             1, 0, 0;
%!             1, 1, 0;
%!             0, 1, 0;
%!             0.5, 0.5, 1];
%! faces = [1, 2, 5;
%!          2, 3, 5;
%!          3, 4, 5;
%!          4, 1, 5];
%! patch  ("Vertices", vertices, "Faces", faces, ...
%!        "FaceVertexCData", jet (5), "FaceColor", "interp");
%! view (-37.5, 30);
%! box off;
%! title ('"FaceColor" = "interp"');

%!demo
%! clf;
%! colormap (jet (64));
%! x = [0 1 1 0];
%! y = [0 0 1 1];
%! subplot (2,1,1);
%!  title ("Blue, Light Green, and Red Horizontal Bars");
%!  patch (x, y + 0, 1);
%!  patch (x, y + 1, 2);
%!  patch (x, y + 2, 3);
%! subplot (2,1,2);
%!  title ("Blue, Light Green, and Red Vertical Bars");
%!  patch (x + 0, y, 1 * ones (size (x)));
%!  patch (x + 1, y, 2 * ones (size (x)));
%!  patch (x + 2, y, 3 * ones (size (x)));

%!demo
%! clf;
%! colormap (jet (64));
%! x = [0 1 1 0];
%! y = [0 0 1 1];
%! subplot (2,1,1);
%!  title ("Blue horizontal bars: Dark to Light");
%!  patch (x, y + 0, 1, "cdatamapping", "direct");
%!  patch (x, y + 1, 9, "cdatamapping", "direct");
%!  patch (x, y + 2, 17, "cdatamapping", "direct");
%! subplot (2,1,2);
%!  title ("Blue vertical bars: Dark to Light");
%!  patch (x + 0, y, 1 * ones (size (x)), "cdatamapping", "direct");
%!  patch (x + 1, y, 9 * ones (size (x)), "cdatamapping", "direct");
%!  patch (x + 2, y, 17 * ones (size (x)), "cdatamapping", "direct");

%!demo
%! clf;
%! colormap (jet (64));
%! x = [ 0 0; 1 1; 1 0 ];
%! y = [ 0 0; 0 1; 1 1 ];
%! p = patch (x, y, "b");
%! set (p, "cdatamapping", "direct", "facecolor", "flat", "cdata", [1 32]);
%! title ("Direct mapping of colors: Light-Green UL and Blue LR triangles");

%!demo
%! clf;
%! colormap (jet (64));
%! x = [ 0 0; 1 1; 1 0 ];
%! y = [ 0 0; 0 1; 1 1 ];
%! p = patch (x, y, [1 32]);
%! title ("Autoscaling of colors: Red UL and Blue LR triangles");

%!demo
%! clf;
%! vertices = [0 0 0; 0.5 -0.5 0; 1 0 0; 1 1 0; 0 1 1; 1 0 1; 0 -1 0] + 3;
%! faces = [1 2 3 4 5 6 7];
%! ha = axes ();
%! hp = patch ("Vertices", vertices, "Faces", faces, "FaceColor", "g");
%! xlabel ("x"), ylabel ("y"), zlabel ("z");
%! view (3);
%! set (ha, "XTick", [], "YTick", [], "ZTick", []);
%! text (vertices(1,1), vertices(1,2), vertices(1,3), "1");
%! text (vertices(2,1), vertices(2,2), vertices(2,3), "2");
%! text (vertices(3,1), vertices(3,2), vertices(3,3), "3");
%! text (vertices(4,1), vertices(4,2), vertices(4,3), "4");
%! text (vertices(5,1), vertices(5,2), vertices(5,3), "5");
%! text (vertices(6,1), vertices(6,2), vertices(6,3), "6");
%! text (vertices(7,1), vertices(7,2), vertices(7,3), "7");
%! title ("Non-coplanar patch");


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = patch ();
%!   assert (findobj (hf, "type", "patch"), h);
%!   assert (get (h, "xdata"), [0; 1; 0], eps);
%!   assert (get (h, "ydata"), [1; 1; 0], eps);
%!   assert (isempty (get (h, "zdata")));
%!   assert (isempty (get (h, "cdata")));
%!   assert (get (h, "faces"), [1, 2, 3], eps);
%!   assert (get (h, "vertices"), [0 1; 1 1; 0 0], eps);
%!   assert (get (h, "facenormalsmode"), "auto");
%!   assert (get (h, "facenormals"), get (0, "defaultpatchfacenormals"));
%!   assert (get (h, "vertexnormalsmode"), "auto");
%!   assert (get (h, "vertexnormals"), get (0, "defaultpatchvertexnormals"));
%!   assert (get (h, "type"), "patch");
%!   assert (get (h, "facecolor"), [0 0 0]);
%!   assert (get (h, "linestyle"), get (0, "defaultpatchlinestyle"));
%!   assert (get (h, "linewidth"), get (0, "defaultpatchlinewidth"), eps);
%!   assert (get (h, "marker"), get (0, "defaultpatchmarker"));
%!   assert (get (h, "markersize"), get (0, "defaultpatchmarkersize"));
%!   hl = light ();
%!   assert (get (h, "facelighting"), "flat");
%!   assert (get (h, "facenormals"), [0 0 1], eps);
%!   assert (get (h, "vertexnormals"), []);
%!   set (h, "facelighting", "gouraud")
%!   assert (get (h, "vertexnormals"), [0 0 1; 0 0 1; 0 0 1], eps);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! c = 0.9;
%! unwind_protect
%!   h = patch ([0 1 0], [0 1 1], c);
%!   assert (get (gca, "clim"), [c - 1, c + 1]);
%!   h = patch ([0 1 0], [0 1 1], 2 * c);
%!   assert (get (gca, "clim"), [c, 2 * c]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error <invalid color specification C> patch (1, 1, 'x')
%!error <invalid TrueColor data C> patch (1, 1, rand (1,2,3))
%!error <size of X, Y, and C must be equal> patch (1, 1, [1, 2])
%!error <invalid color specification C> patch (1, 1, {1})
