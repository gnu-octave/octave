## Copyright (C) 2007-2012 John W. Eaton
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
## @deftypefn  {Function File} {} text (@var{x}, @var{y}, @var{label})
## @deftypefnx {Function File} {} text (@var{x}, @var{y}, @var{z}, @var{label})
## @deftypefnx {Function File} {} text (@var{x}, @var{y}, @var{label}, @var{p1}, @var{v1}, @dots{})
## @deftypefnx {Function File} {} text (@var{x}, @var{y}, @var{z}, @var{label}, @var{p1}, @var{v1}, @dots{})
## @deftypefnx {Function File} {@var{h} =} text (@dots{})
## Create a text object with text @var{label} at position @var{x},
## @var{y}, @var{z} on the current axes.  Property-value pairs following
## @var{label} may be used to specify the appearance of the text.
##
## The optional return value @var{h} is a graphics handle to the created text
## object.
## @end deftypefn

## Author: jwe

function h = text (varargin)

  nargs = nargin;
  offset = 0;

  if (nargs > 2 && isnumeric (varargin{1}) && isnumeric (varargin{2}))
    x = varargin{1};
    y = varargin{2};
    offset = 3;

    if (nargin > 3 && isnumeric (varargin{3}))
      z = varargin{3};
      offset = 4;
    else
      z = zeros (size (x));
      offset = 3;
    endif

    label = varargin{offset};
    varargin(1:offset) = [];

    nx = numel (x);
    ny = numel (y);
    nz = numel (z);
    if (ischar (label) || isnumeric (label))
      nt = size (label, 1);
      if (nx > 1 && nt == 1)
        ## Mutiple text objects with same string
        label = repmat ({label}, [nx, 1]);
        nt = nx;
      elseif (nx > 1 && nt == nx)
        ## Mutiple text objects with different strings
        label = cellstr (label);
      elseif (ischar (label))
        ## Single text object with one or more lines
        label = {label};
      endif
    elseif (iscell (label))
      nt = numel (label);
      if (nx > 1 && nt == 1)
        label = repmat ({label}, [nx, 1]);
        nt = nx;
      elseif (! (nx > 1 && nt == nx))
        label = {label};
        nt = 1;
      endif
    else
      error ("text: expecting LABEL to be a character string or cell array of character strings");
    endif
  else
    x = y = z = 0;
    nx = ny = nz = 1;
    label = {""};
    nt = 1;
  endif

  if (rem (numel (varargin), 2) == 0)

    if (nx == ny && nx == nz && (nt == nx || nt == 1 || nx == 1))
      pos = [x(:), y(:), z(:)];
      ca = gca ();
      tmp = zeros (nt, 1);
      if (nx == 1)
        ## TODO - Modify __go_text__() to accept cell-strings
        tmp = __go_text__ (ca, "string", "foobar",
                           varargin{:},
                           "position", pos);
        set (tmp, "string", label{1});
      elseif (nt == nx)
        for n = 1:nt
          tmp(n) = __go_text__ (ca, "string", label{n},
                                varargin{:},
                                "position", pos(n,:));
        endfor
        __request_drawnow__ ();
      else
        error ("text: dimension mismatch for coordinates and LABEL");
      endif
    elseif (nt == nx || nt == 1 || nx == 1)
      error ("text: dimension mismatch for coordinates");
    else
      error ("text: mismatch betwween coordinates and strings");
    endif

    if (nargout > 0)
      h = tmp;
    endif

  else
    print_usage ();
  endif

endfunction

%!demo
%! clf
%! ha = {"left", "center", "right"};
%! va = {"bottom", "middle", "top"};
%! x = [0.25 0.5 0.75];
%! y = [0.25 0.5 0.75];
%! for t = 0:30:359;
%!   for nh = 1:numel(ha)
%!     for nv = 1:numel(va)
%!       text (x(nh), y(nv), "Hello World", ...
%!             "rotation", t, ...
%!             "horizontalalignment", ha{nh}, ...
%!             "verticalalignment", va{nv});
%!     endfor
%!   endfor
%! endfor
%! set (gca, "xtick", [0.25, 0.5, 0.75], ...
%!           "xticklabel", ha, ...
%!           "ytick", [0.25, 0.5, 0.75], ...
%!           "yticklabel", va);
%! axis ([0 1 0 1]);
%! xlabel ("horizontal alignment");
%! ylabel ("vertical alignment");
%! title ("text alignment and rotation (0:30:360 degrees)")

%!demo
%! clf
%! h = mesh (peaks, "edgecolor", 0.7 * [1 1 1], ...
%!                  "facecolor", "none", ...
%!                  "facealpha", 0);
%! for t = 0:45:359;
%!   text (25, 25, 0, "Vertical Alignment = Bottom", ...
%!                    "rotation", t, ...
%!                    "horizontalalignment", "left", ...
%!                    "verticalalignment", "bottom");
%! endfor
%! caxis ([-100 100]);
%! title ("Vertically Aligned at Bottom");

%!demo
%! clf
%! axis ([0 8 0 8]);
%! title (["1st title";"2nd title"]);
%! xlabel (["1st xlabel";"2nd xlabel"]);
%! ylabel (["1st ylabel";"2nd ylabel"]);
%! text (4, 4, {"Hello", "World"}, ...
%!       "horizontalalignment", "center", ...
%!       "verticalalignment", "middle");
%! grid on

%!demo
%! clf
%! h = mesh (peaks, "edgecolor", 0.7 * [1 1 1], ...
%!                  "facecolor", "none", ...
%!                  "facealpha", 0);
%! title (["1st title";"2nd title"]);
%! xlabel (["1st xlabel";"2nd xlabel"]);
%! ylabel (["1st ylabel";"2nd ylabel"]);
%! zlabel (["1st zlabel";"2nd zlabel"]);
%! text (0, 0, 5, {"Hello", "World"}, ...
%!       "horizontalalignment", "center", ...
%!       "verticalalignment", "middle");
%! hold on;
%! plot3 (0, 0, 5, "+k");

%!demo
%! clf
%! h = text (0.5, 0.3, "char");
%! assert ("char", class (get (h, "string")));
%! h = text (0.5, 0.4, ["char row 1"; "char row 2"]);
%! assert ("char", class (get (h, "string")));
%! h = text (0.5, 0.6, {"cell2str (1,1)", "cell2str (1,2)"; "cell2str (2,1)", "cell2str (2,2)"});
%! assert ("cell", class (get (h, "string")));
%! h = text (0.5, 0.8, "foobar");
%! set (h, "string", 1:3);
%! h = text ([0.1, 0.1], [0.3, 0.4], "one string & two objects");
%! assert ("char", class (get (h(1), "string")));
%! assert ("char", class (get (h(2), "string")));
%! h = text ([0.1, 0.1], [0.5, 0.6], {"one cellstr & two objects"});
%! assert ("cell", class (get (h(1), "string")));
%! assert ("cell", class (get (h(2), "string")));
%! h = text ([0.1, 0.1], [0.7, 0.8], {"cellstr 1 object 1", "cellstr 2 object 2"});
%! assert ("char", class (get (h(1), "string")));
%! assert ("char", class (get (h(2), "string")));
%! h = text ([0.1, 0.1], [0.1, 0.2], ["1st string & 1st object"; "2nd string & 2nd object"]);
%! assert ("char", class (get (h(1), "string")));
%! assert ("char", class (get (h(2), "string")));
%! h = text (0.7, 0.6, "single string");
%! assert ("char", class (get (h, "string")));
%! h = text (0.7, 0.5, {"single cell-string"});
%! assert ("cell", class (get (h, "string")));
%! xlabel (1:2);
%! ylabel (1:2);
%! title (1:2);

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   h = text (0.5, 0.3, "char");
%!   assert ("char", class (get (h, "string")));
%!   h = text (0.5, 0.4, ["char row 1"; "char row 2"]);
%!   assert ("char", class (get (h, "string")));
%!   h = text (0.5, 0.6, {"cell2str (1,1)", "cell2str (1,2)"; "cell2str (2,1)", "cell2str (2,2)"});
%!   assert ("cell", class (get (h, "string")));
%!   h = text (0.5, 0.8, "foobar");
%!   set (h, "string", 1:3);
%!   h = text ([0.1, 0.1], [0.3, 0.4], "one string & two objects");
%!   assert ("char", class (get (h(1), "string")));
%!   assert ("char", class (get (h(2), "string")));
%!   h = text ([0.1, 0.1], [0.5, 0.6], {"one cellstr & two objects"});
%!   assert ("cell", class (get (h(1), "string")));
%!   assert ("cell", class (get (h(2), "string")));
%!   h = text ([0.1, 0.1], [0.7, 0.8], {"cellstr 1 object 1", "cellstr 2 object 2"});
%!   assert ("char", class (get (h(1), "string")));
%!   assert ("char", class (get (h(2), "string")));
%!   h = text ([0.1, 0.1], [0.1, 0.2], ["1st string & 1st object"; "2nd string & 2nd object"]);
%!   assert ("char", class (get (h(1), "string")));
%!   assert ("char", class (get (h(2), "string")));
%!   h = text (0.7, 0.6, "single string");
%!   assert ("char", class (get (h, "string")));
%!   h = text (0.7, 0.5, {"single cell-string"});
%!   assert ("cell", class (get (h, "string")));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

