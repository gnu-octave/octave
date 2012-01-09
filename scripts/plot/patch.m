## Copyright (C) 2005-2012 John W. Eaton
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
## @deftypefn  {Function File} {} patch ()
## @deftypefnx {Function File} {} patch (@var{x}, @var{y}, @var{c})
## @deftypefnx {Function File} {} patch (@var{x}, @var{y}, @var{z}, @var{c})
## @deftypefnx {Function File} {} patch (@var{fv})
## @deftypefnx {Function File} {} patch ('Faces', @var{f}, 'Vertices', @var{v}, @dots{})
## @deftypefnx {Function File} {} patch (@dots{}, @var{prop}, @var{val})
## @deftypefnx {Function File} {} patch (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} patch (@dots{})
## Create patch object from @var{x} and @var{y} with color @var{c} and
## insert in the current axes object.  Return handle to patch object.
##
## For a uniform colored patch, @var{c} can be given as an RGB vector,
## scalar value referring to the current colormap, or string value (for
## example, "r" or "red").
##
## If passed a structure @var{fv} contain the fields "vertices", "faces"
## and optionally "facevertexcdata", create the patch based on these
## properties.
##
## The optional return value @var{h} is a graphics handle to the created patch
## object.
## @seealso{fill}
## @end deftypefn

## Author: jwe

function retval = patch (varargin)

  [h, varargin] = __plt_get_axis_arg__ ("patch", varargin{:});

  [tmp, failed] = __patch__ (h, varargin{:});

  if (failed)
    print_usage ();
  endif

  if (nargout > 0)
    retval = tmp;
  endif

endfunction

%!demo
%! ## Patches with same number of vertices
%! clf
%! t1 = (1/16:1/8:1)'*2*pi;
%! t2 = ((1/16:1/8:1)' + 1/32)*2*pi;
%! x1 = sin (t1) - 0.8;
%! y1 = cos (t1);
%! x2 = sin (t2) + 0.8;
%! y2 = cos (t2);
%! patch([x1,x2],[y1,y2],'r');

%!demo
%! ## Unclosed patch
%! clf
%! t1 = (1/16:1/8:1)'*2*pi;
%! t2 = ((1/16:1/16:1)' + 1/32)*2*pi;
%! x1 = sin (t1) - 0.8;
%! y1 = cos (t1);
%! x2 = sin (t2) + 0.8;
%! y2 = cos (t2);
%! patch([[x1;NaN(8,1)],x2],[[y1;NaN(8,1)],y2],'r');

%!demo
%! ## Specify vertices and faces separately
%! clf
%! t1 = (1/16:1/8:1)'*2*pi;
%! t2 = ((1/16:1/16:1)' + 1/32)*2*pi;
%! x1 = sin (t1) - 0.8;
%! y1 = cos (t1);
%! x2 = sin (t2) + 0.8;
%! y2 = cos (t2);
%! vert = [x1, y1; x2, y2];
%! fac = [1:8,NaN(1,8);9:24];
%! patch('Faces',fac,'Vertices',vert,'FaceColor','r');

%!demo
%! ## Specify vertices and faces separately
%! clf
%! t1 = (1/16:1/8:1)'*2*pi;
%! t2 = ((1/16:1/16:1)' + 1/32)*2*pi;
%! x1 = sin (t1) - 0.8;
%! y1 = cos (t1);
%! x2 = sin (t2) + 0.8;
%! y2 = cos (t2);
%! vert = [x1, y1; x2, y2];
%! fac = [1:8,NaN(1,8);9:24];
%! patch('Faces',fac,'Vertices',vert,'FaceVertexCData', [0, 1, 0; 0, 0, 1]);

%!demo
%! ## Property change on multiple patches
%! clf
%! t1 = (1/16:1/8:1)'*2*pi;
%! t2 = ((1/16:1/8:1)' + 1/32)*2*pi;
%! x1 = sin (t1) - 0.8;
%! y1 = cos (t1);
%! x2 = sin (t2) + 0.8;
%! y2 = cos (t2);
%! h = patch([x1,x2],[y1,y2],cat (3,[0,0],[1,0],[0,1]));
%! pause (1);
%! set (h, 'FaceColor', 'r');

%!demo
%! clf
%! vertices = [0, 0, 0;
%!             1, 0, 0;
%!             1, 1, 0;
%!             0, 1, 0;
%!             0.5, 0.5, 1];
%! faces = [1, 2, 5;
%!          2, 3, 5;
%!          3, 4, 5;
%!          4, 1, 5];
%! patch ('Vertices', vertices, 'Faces', faces, ...
%!        'FaceVertexCData', jet(4), 'FaceColor', 'flat');
%! view (-37.5, 30);

%!demo
%! clf
%! vertices = [0, 0, 0;
%!             1, 0, 0;
%!             1, 1, 0;
%!             0, 1, 0;
%!             0.5, 0.5, 1];
%! faces = [1, 2, 5;
%!          2, 3, 5;
%!          3, 4, 5;
%!          4, 1, 5];
%! patch ('Vertices', vertices, 'Faces', faces, ...
%!        'FaceVertexCData', jet(5), 'FaceColor', 'interp');
%! view (-37.5, 30);

%!demo
%! clf
%! colormap (jet);
%! x = [0 1 1 0];
%! y = [0 0 1 1];
%! subplot (2, 1, 1);
%! title ("Blue, Light-Green, and Red Horizontal Bars");
%! patch (x, y + 0, 1);
%! patch (x, y + 1, 2);
%! patch (x, y + 2, 3);
%! subplot (2, 1, 2);
%! title ("Blue, Light-Green, and Red Vertical Bars");
%! patch (x + 0, y, 1 * ones (size (x)));
%! patch (x + 1, y, 2 * ones (size (x)));
%! patch (x + 2, y, 3 * ones (size (x)));

%!demo
%! clf
%! colormap (jet);
%! x = [0 1 1 0];
%! y = [0 0 1 1];
%! subplot (2, 1, 1);
%! title ("Blue horizontal bars: Dark to Light");
%! patch (x, y + 0, 1, "cdatamapping", "direct");
%! patch (x, y + 1, 9, "cdatamapping", "direct");
%! patch (x, y + 2, 17, "cdatamapping", "direct");
%! subplot (2, 1, 2);
%! title ("Blue vertical bars: Dark to Light")
%! patch (x + 0, y, 1 * ones (size (x)), "cdatamapping", "direct");
%! patch (x + 1, y, 9 * ones (size (x)), "cdatamapping", "direct");
%! patch (x + 2, y, 17 * ones (size (x)), "cdatamapping", "direct");

%!demo
%! clf;
%! colormap (jet);
%! x = [ 0 0; 1 1; 1 0 ];
%! y = [ 0 0; 0 1; 1 1 ];
%! p = patch (x, y, "facecolor", "b");
%! title ("Two blue triangles");
%! set (p, "cdatamapping", "direct", "facecolor", "flat", "cdata", [1 32]);
%! title ("Direct mapping of colors: Light-Green UL and Blue LR triangles");

%!demo
%! clf;
%! colormap (jet);
%! x = [ 0 0; 1 1; 1 0 ];
%! y = [ 0 0; 0 1; 1 1 ];
%! p = patch (x, y, [1 32]);
%! title ("Autoscaling of colors: Red UL and Blue LR triangles");

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = patch;
%!   assert (findobj (hf, "type", "patch"), h);
%!   assert (get (h, "xdata"), [0; 1; 0], eps);
%!   assert (get (h, "ydata"), [1; 1; 0], eps);
%!   assert (isempty (get (h, "zdata")));
%!   assert (isempty (get (h, "cdata")));
%!   assert (get (h, "faces"), [1, 2, 3], eps);
%!   assert (get (h, "vertices"), [0 1; 1 1; 0 0], eps);
%!   assert (get (h, "type"), "patch");
%!   assert (get (h, "facecolor"), [0 0 0]);
%!   assert (get (h, "linestyle"), get (0, "defaultpatchlinestyle"));
%!   assert (get (h, "linewidth"), get (0, "defaultpatchlinewidth"), eps);
%!   assert (get (h, "marker"), get (0, "defaultpatchmarker"));
%!   assert (get (h, "markersize"), get (0, "defaultpatchmarkersize"));
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

