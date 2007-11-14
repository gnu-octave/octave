## Copyright (C) 2005, 2007 John W. Eaton
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
## @deftypefn {Function File} {} patch ()
## @deftypefnx {Function File} {} patch (@var{x}, @var{y}, @var{c})
## @deftypefnx {Function File} {} patch (@var{x}, @var{y}, @var{c}, @var{opts})
## @deftypefnx {Function File} {} patch ('Faces', @var{f}, 'Vertices', @var{v}, @dots{})
## @deftypefnx {Function File} {} patch (@dots{}, @var{prop}, @var{val})
## @deftypefnx {Function File} {} patch (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} = } patch (@dots{})
## Create patch object from @var{x} and @var{y} with color @var{c} and
## insert in the current axes object.  Return handle to patch object.
##
## For a uniform colored patch, @var{c} can be given as an RGB vector,
## scalar value referring to the current colormap, or string value (for
## example, "r" or "red"). 
## @end deftypefn

## Author: jwe

function h = patch (varargin)

  if (isscalar (varargin{1}) && ishandle (varargin{1}))
    h = varargin {1};
    if (! strcmp (get (h, "type"), "axes") && ! strcmp (get (h, "type"), "hggroup"))
      error ("patch: expecting first argument to be an axes or hggroup object");
    endif
    oldh = gca ();
    unwind_protect
      axes (ancestor (h, "axes"));
      [tmp, fail] = __patch__ (h, varargin{2:end});
    unwind_protect_cleanup
      axes (oldh);
    end_unwind_protect
  else
    [tmp, fail] = __patch__ (gca (), varargin{:});
  endif

  if (fail)
    print_usage ();
  endif

  if (nargout > 0)
    h = tmp;
  endif

endfunction

%!demo
%! ## Patches with same number of vertices
%! close all;
%! t1 = (1/16:1/8:1)'*2*pi;
%! t2 = ((1/16:1/8:1)' + 1/32)*2*pi;
%! x1 = sin(t1) - 0.8;
%! y1 = cos(t1);
%! x2 = sin(t2) + 0.8;
%! y2 = cos(t2);
%! patch([x1,x2],[y1,y2],'r');

%!demo
%! ## Unclosed patch
%! close all;
%! t1 = (1/16:1/8:1)'*2*pi;
%! t2 = ((1/16:1/16:1)' + 1/32)*2*pi;
%! x1 = sin(t1) - 0.8;
%! y1 = cos(t1);
%! x2 = sin(t2) + 0.8;
%! y2 = cos(t2);
%! patch([[x1;NaN(8,1)],x2],[[y1;NaN(8,1)],y2],'r');

%!demo
%! ## Specify vertices and faces separately
%! close all;
%! t1 = (1/16:1/8:1)'*2*pi;
%! t2 = ((1/16:1/16:1)' + 1/32)*2*pi;
%! x1 = sin(t1) - 0.8;
%! y1 = cos(t1);
%! x2 = sin(t2) + 0.8;
%! y2 = cos(t2);
%! vert = [x1, y1; x2, y2];
%! fac = [1:8,NaN(1,8);9:24];
%! patch('Faces',fac,'Vertices',vert,'FaceColor','r');

%!demo
%! ## Property change on multiple patches
%! close all;
%! t1 = (1/16:1/8:1)'*2*pi;
%! t2 = ((1/16:1/8:1)' + 1/32)*2*pi;
%! x1 = sin(t1) - 0.8;
%! y1 = cos(t1);
%! x2 = sin(t2) + 0.8;
%! y2 = cos(t2);
%! h = patch([x1,x2],[y1,y2],cat (3,[0,0],[1,0],[0,1]));
%! pause (1);
%! set (h, 'FaceColor', 'r');
