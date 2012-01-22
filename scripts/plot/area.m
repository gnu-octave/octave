## Copyright (C) 2007-2012 Michael Goffioul
## Copyright (C) 2007-2009 David Bateman
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
## @deftypefn  {Function File} {} area (@var{x}, @var{y})
## @deftypefnx {Function File} {} area (@var{x}, @var{y}, @var{lvl})
## @deftypefnx {Function File} {} area (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {Function File} {} area (@var{y}, @dots{})
## @deftypefnx {Function File} {} area (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} area (@dots{})
## Area plot of cumulative sum of the columns of @var{y}.  This shows the
## contributions of a value to a sum, and is functionally similar to
## @code{plot (@var{x}, cumsum (@var{y}, 2))}, except that the area under
## the curve is shaded.
##
## If the @var{x} argument is omitted it is assumed to be given by
## @code{1 : rows (@var{y})}.  A value @var{lvl} can be defined that determines
## where the base level of the shading under the curve should be defined.
##
## Additional arguments to the @code{area} function are passed to
## @code{patch}.
##
## The optional return value @var{h} is a graphics handle to the hggroup
## object representing the area patch objects.
## @seealso{plot, patch}
## @end deftypefn

function h = area (varargin)

  [ax, varargin, nargin] = __plt_get_axis_arg__ ("area", varargin{:});

  if (nargin > 0)
    idx = 1;
    x = y = [];
    bv = 0;
    args = {};
    ## Check for (X) or (X,Y) arguments and possible base value.
    if (nargin >= idx && ismatrix (varargin{idx}))
      y = varargin{idx};
      idx++;
      if (nargin >= idx)
        if (isscalar (varargin{idx}))
          bv = varargin{idx};
          idx++;
        elseif (ismatrix (varargin{idx}))
          x = y;
          y = varargin{idx};
          idx++;
          if (nargin >= idx && isscalar (varargin{idx}))
            bv = varargin{idx};
            idx++;
          endif
        endif
      endif
    else
      print_usage ();
    endif
    ## Check for additional args.
    if (nargin >= idx)
      args = {varargin{idx:end}};
    endif
    newplot ();
    if (isvector (y))
      y = y(:);
    endif
    if (isempty (x))
      x = repmat ([1:size(y, 1)]', 1, size (y, 2));
    elseif (isvector (x))
      x = repmat (x(:),  1, size (y, 2));
    endif

    oldax = gca ();
    unwind_protect
      axes (ax);
      tmp = __area__ (ax, x, y, bv, args{:});
    unwind_protect_cleanup
      axes (oldax);
    end_unwind_protect

    if (nargout > 0)
      h = tmp;
    endif
  else
    print_usage ();
  endif

endfunction

function retval = __area__ (ax, x, y, bv, varargin)

  y0 = bv * ones (1, rows (y));
  y0 = zeros (1, rows (y));
  retval = [];
  for i = 1: size (y, 2);
    hg = hggroup ();
    retval = [retval; hg];
    args = __add_datasource__ ("area", hg, {"x", "y"}, varargin{:});

    x1 = x(:, 1).';
    y1 = y (:, i).';
    addproperty ("xdata", hg, "data", x1);
    addproperty ("ydata", hg, "data", y1);

    addlistener (hg, "xdata", @update_data);
    addlistener (hg, "ydata", @update_data);

    if (i == 1)
      h = patch (ax, [x1(1), x1, fliplr(x1)], [bv, y1, bv*ones(1, length(y1))],
                 __next_line_color__ (), "parent", hg);
    else
      y1 = y0 + y1;
      h = patch (ax, [x1(1), x1, fliplr(x1)], [y0(1), y1, fliplr(y0)],
                 __next_line_color__ (), "parent", hg);
    endif

    y0 = y1;

    addproperty ("basevalue", hg, "data", bv);
    addlistener (hg, "basevalue", @move_baseline);

    addproperty ("edgecolor", hg, "patchedgecolor", get (h, "edgecolor"));
    addproperty ("linewidth", hg, "patchlinewidth", get (h, "linewidth"));
    addproperty ("linestyle", hg, "patchlinestyle", get (h, "linestyle"));
    addproperty ("facecolor", hg, "patchfacecolor", get (h, "facecolor"));

    addlistener (hg, "edgecolor", @update_props);
    addlistener (hg, "linewidth", @update_props);
    addlistener (hg, "linestyle", @update_props);
    addlistener (hg, "facecolor", @update_props);

    addproperty ("areagroup", hg, "data");
    set (retval, "areagroup", retval);

    if (! isempty (args))
      set (hg, args{:});
    endif
  endfor

endfunction

function update_props (h, d)
  kids = get (h, "children");
  set (kids, "edgecolor", get (h, "edgecolor"),
       "linewidth", get (h, "linewidth"),
       "linestyle", get (h, "linestyle"),
       "facecolor", get (h, "facecolor"));
endfunction

function move_baseline (h, d)
  persistent recursion = false;

  ## Don't allow recursion
  if (! recursion)
    unwind_protect
      recursion = true;
      hlist = get (h, "areagroup");
      b0 = get (h, "basevalue");

      for hh = hlist(:)'
        if (hh != h)
          b1 = get (hh, "basevalue");
          if (b1 != b0)
            set (hh, "basevalue", b0);
          endif
        endif
      endfor
      update_data (h, d);
    unwind_protect_cleanup
      recursion = false;
    end_unwind_protect
  endif
endfunction

function update_data (h, d)
  hlist = get (h, "areagroup");
  bv = get (h, "basevalue");
  for i = 1 : length (hlist)
    hh = hlist(i);
    x1 = get (hh, "xdata")(:);
    y1 = get (hh, "ydata")(:);

    set (get (hh, "children"), "xdata", [x1(1); x1; flipud(x1)]);
    if (i == 1)
      set (get (hh, "children"), "ydata", [bv; y1; bv*ones(length(y1), 1)]);
    else
      y1 = y0 + y1;
      set (get (hh, "children"), "ydata", [y0(1); y1; flipud(y0)]);
    endif

    y0 = y1;
  endfor
endfunction
