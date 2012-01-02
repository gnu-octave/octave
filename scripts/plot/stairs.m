## Copyright (C) 1993-2012 John W. Eaton
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
## @deftypefn  {Function File} {} stairs (@var{y})
## @deftypefnx {Function File} {} stairs (@var{x}, @var{y})
## @deftypefnx {Function File} {} stairs (@dots{}, @var{style})
## @deftypefnx {Function File} {} stairs (@dots{}, @var{prop}, @var{val})
## @deftypefnx {Function File} {} stairs (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} stairs (@dots{})
## @deftypefnx {Function File} {[@var{xstep}, @var{ystep}] =} stairs (@dots{})
## Produce a stairstep plot.  The arguments may be vectors or matrices.
##
## If only one argument is given, it is taken as a vector of y-values
## and the x coordinates are taken to be the indices of the elements.
##
## If one output argument is requested, return a graphics handle to the plot.
## If two output arguments are specified, the data are generated but
## not plotted.  For example,
##
## @example
## stairs (x, y);
## @end example
##
## @noindent
## and
##
## @example
## @group
## [xs, ys] = stairs (x, y);
## plot (xs, ys);
## @end group
## @end example
##
## @noindent
## are equivalent.
## @seealso{plot, semilogx, semilogy, loglog, polar, mesh, contour,
## bar, xlabel, ylabel, title}
## @end deftypefn

## Author: jwe

function [xs, ys] = stairs (varargin)

  [ax, varargin, nargin] = __plt_get_axis_arg__ ("stairs", varargin{:});

  if (nargin < 1)
    print_usage ();
  else
    if (nargout > 1)
      [h, xs, ys] = __stairs__ (false, varargin{:});
    else
      oldax = gca ();
      unwind_protect
        axes (ax);
        newplot ();
        [h, xxs, yys] = __stairs__ (true, varargin{:});
      unwind_protect_cleanup
        axes (oldax);
      end_unwind_protect
    endif
    if (nargout == 1)
      xs = h;
    endif
  endif
endfunction

function [h, xs, ys] = __stairs__ (doplot, varargin)

  if (nargin == 2 || ischar (varargin{2}))
    y = varargin {1};
    varargin(1) = [];
    if (ismatrix (y))
      if (isvector (y))
        y = y(:);
      endif
      x = 1:rows (y);
    endif
  else
    x = varargin{1};
    y = varargin{2};
    varargin(1:2) = [];
  endif

  if (ndims (x) > 2 || ndims (y) > 2)
    error ("stairs: expecting 2-d arguments");
  endif

  vec_x = isvector (x);

  if (vec_x)
    x = x(:);
  endif

  if (isvector (y))
    y = y(:);
  endif

  if (ismatrix (y))
    [nr, nc] = size (y);
    if (vec_x)
      x = repmat (x, [1, nc]);
    else
      [x_nr, x_nc] = size (x);
      if (x_nr != nr || x_nc != nc)
        error ("stairs: argument size mismatch");
      endif
    endif
  endif

  len = 2*nr - 1;

  xs = ys = zeros (len, nc);

  xs(1,:) = x(1,:);
  ys(1,:) = y(1,:);

  xtmp = x(2:nr,:);
  ridx = 2:2:len-1;
  xs(ridx,:) = xtmp;
  ys(ridx,:) = y(1:nr-1,:);

  ridx = 3:2:len;
  xs(ridx,:) = xtmp;
  ys(ridx,:) = y(2:nr,:);

  have_line_spec = false;
  for i = 1 : length (varargin)
    arg = varargin {i};
    if ((ischar (arg) || iscell (arg)) && ! have_line_spec)
      [linespec, valid] = __pltopt__ ("stairs", arg, false);
      if (valid)
        have_line_spec = true;
        varargin(i) = [];
        break;
      endif
    endif
  endfor

  if (doplot)
    h = [];
    unwind_protect
      hold_state = get (gca (), "nextplot");
      for i = 1 : size(y, 2)
        hg = hggroup ();
        h = [h; hg];
        args = __add_datasource__ ("stairs", hg, {"x", "y"}, varargin{:});

        addproperty ("xdata", hg, "data", x(:,i).');
        addproperty ("ydata", hg, "data", y(:,i).');

        addlistener (hg, "xdata", @update_data);
        addlistener (hg, "ydata", @update_data);

        if (have_line_spec)
          tmp = line (xs(:,i).', ys(:,i).', "color", linespec.color,
                      "parent", hg);
        else
          tmp = line (xs(:,i).', ys(:,i).', "color", __next_line_color__ (),
                      "parent", hg);
        endif

        addproperty ("color", hg, "linecolor", get (tmp, "color"));
        addproperty ("linewidth", hg, "linelinewidth", get (tmp, "linewidth"));
        addproperty ("linestyle", hg, "linelinestyle", get (tmp, "linestyle"));

        addproperty ("marker", hg, "linemarker", get (tmp, "marker"));
        addproperty ("markerfacecolor", hg, "linemarkerfacecolor",
                     get (tmp, "markerfacecolor"));
        addproperty ("markeredgecolor", hg, "linemarkeredgecolor",
                     get (tmp, "markeredgecolor"));
        addproperty ("markersize", hg, "linemarkersize",
                     get (tmp, "markersize"));

        addlistener (hg, "color", @update_props);
        addlistener (hg, "linewidth", @update_props);
        addlistener (hg, "linestyle", @update_props);
        addlistener (hg, "marker", @update_props);
        addlistener (hg, "markerfacecolor", @update_props);
        addlistener (hg, "markeredgecolor", @update_props);
        addlistener (hg, "markersize", @update_props);

        if (! isempty (args))
          set (hg, args{:});
        endif
      endfor
    unwind_protect_cleanup
      set (gca (), "nextplot", hold_state);
    end_unwind_protect
  else
    h = 0;
  endif

endfunction


%!demo
%! clf
%! x = 1:10;
%! rand_1x10_data1 = [0.073, 0.455, 0.837, 0.124, 0.426, 0.781, 0.004, 0.024, 0.519, 0.698];
%! y = rand_1x10_data1;
%! stairs (x, y);

%!demo
%! clf
%! x = 1:10;
%! rand_1x10_data2 = [0.014, 0.460, 0.622, 0.394, 0.531, 0.378, 0.466, 0.788, 0.342, 0.893];
%! y = rand_1x10_data2;
%! [xs, ys] = stairs (x, y);
%! plot (xs, ys);

%!demo
%! clf
%! stairs (1:9);

%!demo
%! clf
%! [xs, ys] = stairs (9:-1:1);
%! plot (xs, ys);


function update_props (h, d)
  set (get (h, "children"), "color", get (h, "color"),
       "linewidth", get (h, "linewidth"),
       "linestyle", get (h, "linestyle"),
       "marker", get (h, "marker"),
       "markerfacecolor", get (h, "markerfacecolor"),
       "markeredgecolor", get (h, "markeredgecolor"),
       "markersize", get (h, "markersize"));
endfunction

function update_data (h, d)
  x = get (h, "xdata");
  y = get (h, "ydata");

  nr = length (x);
  len = 2 * nr - 1;
  xs = ys = zeros (1, len);

  xs(1) = x(1);
  ys(1) = y(1);

  xtmp = x(2:nr);
  ridx = 2:2:len-1;
  xs(ridx) = xtmp;
  ys(ridx) = y(1:nr-1);

  ridx = 3:2:len;
  xs(ridx) = xtmp;
  ys(ridx) = y(2:nr);

  set (get (h, "children"), "xdata", xs, "ydata", ys);
endfunction
