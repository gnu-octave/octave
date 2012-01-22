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
## @deftypefn {Function File} {} __plt__ (@var{caller}, @var{h}, @var{varargin})
## Undocumented internal function.
## @end deftypefn

## Author: jwe

function retval = __plt__ (caller, h, varargin)

  nargs = nargin - 2;

  if (nargs > 0)

    k = 1;

    x_set = false;
    y_set = false;
    property_set = false;
    properties = {};

    hlegend = [];
    fkids = get (gcf(), "children");
    for i = 1 : numel(fkids)
      if (ishandle (fkids (i)) && strcmp (get (fkids (i), "type"), "axes")
          && (strcmp (get (fkids (i), "tag"), "legend")))
        udata = get (fkids (i), "userdata");
        if (! isempty (intersect (udata.handle, gca ())))
          hlegend = fkids (i);
          break;
        endif
      endif
    endfor

    setlgnd = false;
    if (isempty (hlegend))
      hlgnd = [];
      tlgnd = {};
    else
      [hlgnd, tlgnd] = __getlegenddata__ (hlegend);
    endif

    ## Gather arguments, decode format, gather plot strings, and plot lines.

    retval = [];

    while (nargs > 0 || x_set)

      if (nargs == 0)
        ## Force the last plot when input variables run out.
        next_cell = {};
        next_arg = {""};
      else
        next_cell = varargin(k);
        next_arg = varargin{k++};
      endif

      nargs--;

      if (ischar (next_arg) || iscellstr (next_arg))
        if (x_set)
          [options, valid] = __pltopt__ (caller, next_arg, false);
          if (! valid)
            if (nargs == 0)
              error ("%s: properties must appear followed by a value", caller);
            endif
            properties = [properties, [next_cell, varargin(k++)]];
            nargs--;
            continue;
          else
            while (nargs > 0 && ischar (varargin{k}))
              if (nargs < 2)
                error ("%s: properties must appear followed by a value",
                       caller);
              endif
              properties = [properties, varargin(k:k+1)];
              k += 2;
              nargs -= 2;
            endwhile
          endif
          if (y_set)
            tmp = __plt2__ (h, x, y, options, properties);
            [hlgnd, tlgnd, setlgnd] = __plt_key__ (tmp, options, hlgnd, tlgnd, setlgnd);
            properties = {};
            retval = [retval; tmp];
          else
            tmp = __plt1__ (h, x, options, properties);
            [hlgnd, tlgnd, setlgnd] = __plt_key__ (tmp, options, hlgnd, tlgnd, setlgnd);
            properties = {};
            retval = [retval; tmp];
          endif
          x_set = false;
          y_set = false;
        else
          error ("plot: no data to plot");
        endif
      elseif (x_set)
        if (y_set)
          options = __pltopt__ (caller, {""});
          tmp = __plt2__ (h, x, y, options, properties);
          [hlgnd, tlgnd, setlgnd] = __plt_key__ (tmp, options, hlgnd, tlgnd, setlgnd);
          retval = [retval; tmp];
          x = next_arg;
          y_set = false;
          properties = {};
        else
          y = next_arg;
          y_set = true;
        endif
      else
        x = next_arg;
        x_set = true;
      endif

    endwhile

    if (setlgnd)
      legend (gca(), hlgnd, tlgnd);
    endif
  else
    error ("__plt__: invalid number of arguments");
  endif

endfunction

function [hlgnd, tlgnd, setlgnd] = __plt_key__ (h, options, hlgnd, tlgnd, setlgnd)
  n = numel (h);
  if (numel (options) == 1)
    options = repmat (options(:), n, 1);
  endif

  for i = 1 : n
    key = options.key;
    if (! isempty (key))
      hlgnd = [hlgnd(:); h(i)];
      tlgnd = {tlgnd{:}, key};
      setlgnd = true;
    endif
  endfor
endfunction

function retval = __plt1__ (h, x1, options, properties)

  if (nargin < 2 || nargin > 4)
    print_usage ();
  endif

  if (nargin < 3 || isempty (options))
    options = __default_plot_options__ ();
  endif

  if (nargin < 4)
    properties = {};
  endif

  if (! isstruct (options))
    error ("__plt1__: options must be a struct array");
  endif

  [nr, nc] = size (x1);
  if (nr == 1)
    x1 = x1.';
    tmp = nr;
    nr = nc;
    nc = tmp;
  endif
  x1_i = imag (x1);
  if (any (any (x1_i)))
    x2 = x1_i;
    x1 = real (x1);
  else
    x2 = x1;
    x1 = (1:nr)';
  endif

  retval = __plt2__ (h, x1, x2, options, properties);

endfunction

function retval = __plt2__ (h, x1, x2, options, properties)

  if (nargin < 3 || nargin > 5)
    print_usage ();
  endif

  if (nargin < 4 || isempty (options))
    options = __default_plot_options__ ();
  endif

  if (nargin < 5)
    properties = {};
  endif

  if (! isstruct (options))
    error ("__plt1__: options must be a struct array");
  endif

  if (any (any (imag (x1))))
    x1 = real (x1);
  endif

  if (any (any (imag (x2))))
    x2 = real (x2);
  endif

  h_set = false;
  if (isempty (x1) && isempty (x2))
    retval = zeros (0, 1);
  elseif (isscalar (x1))
    if (isscalar (x2))
      retval = __plt2ss__ (h, x1, x2, options, properties);
    elseif (isvector (x2))
      retval = __plt2sv__ (h, x1, x2, options, properties);
    else
      error ("__plt2__: invalid data for plotting");
    endif
  elseif (isvector (x1))
    if (isscalar (x2))
      retval = __plt2vs__ (h, x1, x2, options, properties);
    elseif (isvector (x2))
      retval = __plt2vv__ (h, x1, x2, options, properties);
    elseif (ismatrix (x2))
      retval = __plt2vm__ (h, x1, x2, options, properties);
    else
      error ("__plt2__: invalid data for plotting");
    endif
  elseif (ismatrix (x1))
    if (isvector (x2))
      retval = __plt2mv__ (h, x1, x2, options, properties);
    elseif (ismatrix (x2))
      retval = __plt2mm__ (h, x1, x2, options, properties);
    else
      error ("__plt2__: invalid data for plotting");
    endif
  else
    error ("__plt2__: invalid data for plotting");
  endif

endfunction

function retval = __plt2mm__ (h, x, y, options, properties)

  if (nargin < 3 || nargin > 5)
    print_usage ();
  endif

  if (nargin < 4 || isempty (options))
    options = __default_plot_options__ ();
  endif

  if (nargin < 5)
    properties = {};
  endif

  [x_nr, x_nc] = size (x);
  [y_nr, y_nc] = size (y);

  k = 1;
  if (x_nr == y_nr && x_nc == y_nc)
    if (x_nc > 0)
      if (numel (options) == 1)
        options = repmat (options(:), x_nc, 1);
      endif
      retval = zeros (x_nc, 1);
      for i = 1:x_nc
        linestyle = options(i).linestyle;
        marker = options(i).marker;
        if (isempty (marker) && isempty (linestyle))
           [linestyle, marker] = __next_line_style__ ();
        endif
        color = options(i).color;
        if (isempty (color))
          color = __next_line_color__ ();
        endif

        retval(i) = line (x(:,i), y(:,i), "color", color,
                          "linestyle", linestyle,
                          "marker", marker, properties{:});
      endfor
    else
      error ("__plt2mm__: arguments must be a matrices");
    endif
  else
    error ("__plt2mm__: matrix dimensions must match");
  endif

endfunction

function retval = __plt2mv__ (h, x, y, options, properties)

  if (nargin < 3 || nargin > 5)
    print_usage ();
  endif

  if (nargin < 4 || isempty (options))
    options = __default_plot_options__ ();
  endif

  if (nargin < 5)
    properties = {};
  endif

  [x_nr, x_nc] = size (x);
  [y_nr, y_nc] = size (y);

  if (y_nr == 1)
    y = y';
    tmp = y_nr;
    y_nr = y_nc;
    y_nc = tmp;
  endif

  if (x_nr == y_nr)
    1;
  elseif (x_nc == y_nr)
    x = x';
    tmp = x_nr;
    x_nr = x_nc;
    x_nc = tmp;
  else
    error ("__plt2mv__: matrix dimensions must match");
  endif

  if (x_nc > 0)
    if (numel (options) == 1)
      options = repmat (options(:), x_nc, 1);
    endif
    retval = zeros (x_nc, 1);
    for i = 1:x_nc
      linestyle = options(i).linestyle;
      marker = options(i).marker;
      if (isempty (marker) && isempty (linestyle))
        [linestyle, marker] = __next_line_style__ ();
      endif
      color = options(i).color;
      if (isempty (color))
        color = __next_line_color__ ();
      endif

      retval(i) = line (x(:,i), y, "color", color,
                        "linestyle", linestyle,
                        "marker", marker, properties{:});
    endfor
  else
    error ("__plt2mv__: arguments must be a matrices");
  endif

endfunction

function retval = __plt2ss__ (h, x, y, options, properties)

  if (nargin < 3 || nargin > 5)
    print_usage ();
  endif

  if (nargin < 4 || isempty (options))
    options = __default_plot_options__ ();
  endif

  if (nargin < 5)
    properties = {};
  endif

  if (numel (options) > 1)
    options = options(1);
  endif

  [x_nr, x_nc] = size (x);
  [y_nr, y_nc] = size (y);

  if (x_nr == 1 && x_nr == y_nr && x_nc == 1 && x_nc == y_nc)
    linestyle = options.linestyle;
    marker = options.marker;
    if (isempty (marker) && isempty (linestyle))
      [linestyle, marker] = __next_line_style__ ();
    endif
    color = options.color;
    if (isempty (color))
      color = __next_line_color__ ();
    endif

    retval = line (x, y, "color", color,
                   "linestyle", linestyle,
                   "marker", marker, properties{:});
  else
    error ("__plt2ss__: arguments must be scalars");
  endif

endfunction

function retval = __plt2sv__ (h, x, y, options, properties)

  if (nargin < 3 || nargin > 5)
    print_usage ();
  endif

  if (nargin < 4 || isempty (options))
    options = __default_plot_options__ ();
  endif

  if (nargin < 5)
    properties = {};
  endif

  if (isscalar (x) && isvector (y))
    len = numel (y);
    if (numel (options) == 1)
      options = repmat (options(:), len, 1);
    endif
    retval = zeros (len, 1);
    for i = 1:len
      linestyle = options(i).linestyle;
      marker = options(i).marker;
      if (isempty (marker) && isempty (linestyle))
        [linestyle, marker] = __next_line_style__ ();
      endif
      color = options(i).color;
      if (isempty (color))
        color = __next_line_color__ ();
      endif

      retval(i) = line (x, y(i), "color", color,
                        "linestyle", linestyle,
                        "marker", marker, properties{:});
    endfor
  else
    error ("__plt2sv__: first arg must be scalar, second arg must be vector");
  endif

endfunction

function retval = __plt2vm__ (h, x, y, options, properties)

  if (nargin < 3 || nargin > 5)
    print_usage ();
  endif

  if (nargin < 4 || isempty (options))
    options = __default_plot_options__ ();
  endif

  if (nargin < 5)
    properties = {};
  endif

  [x_nr, x_nc] = size (x);
  [y_nr, y_nc] = size (y);

  if (x_nr == 1)
    x = x';
    tmp = x_nr;
    x_nr = x_nc;
    x_nc = tmp;
  endif

  if (x_nr == y_nr)
    1;
  elseif (x_nr == y_nc)
    y = y';
    tmp = y_nr;
    y_nr = y_nc;
    y_nc = tmp;
  else
    error ("__plt2vm__: matrix dimensions must match");
  endif

  if (y_nc > 0)
    if (numel (options) == 1)
      options = repmat (options(:), y_nc, 1);
    endif
    retval = zeros (y_nc, 1);
    for i = 1:y_nc
      linestyle = options(i).linestyle;
      marker = options(i).marker;
      if (isempty (marker) && isempty (linestyle))
        [linestyle, marker] = __next_line_style__ ();
      endif
      color = options(i).color;
      if (isempty (color))
        color = __next_line_color__ ();
      endif

      retval(i) = line (x, y(:,i), "color", color,
                        "linestyle", linestyle,
                        "marker", marker, properties{:});
    endfor
  else
    error ("__plt2vm__: arguments must be a matrices");
  endif

endfunction

function retval = __plt2vs__ (h, x, y, options, properties)

  if (nargin < 3 || nargin > 5)
    print_usage ();
  endif

  if (nargin < 4 || isempty (options))
    options = __default_plot_options__ ();
  endif

  if (nargin < 5)
    properties = {};
  endif

  if (isvector (x) && isscalar (y))
    len = numel (x);
    if (numel (options) == 1)
      options = repmat (options(:), len, 1);
    endif
    retval = zeros (len, 1);
    for i = 1:len
      linestyle = options(i).linestyle;
      marker = options(i).marker;
      if (isempty (marker) && isempty (linestyle))
        [linestyle, marker] = __next_line_style__ ();
      endif
      color = options(i).color;
      if (isempty (color))
        color = __next_line_color__ ();
      endif

      retval(i) = line (x(i), y, "color", color,
                        "linestyle", linestyle,
                        "marker", marker, properties{:});
    endfor
  else
    error ("__plt2vs__: first arg must be vector, second arg must be scalar");
  endif

endfunction

function retval = __plt2vv__ (h, x, y, options, properties)

  if (nargin < 3 || nargin > 5)
    print_usage ();
  endif

  if (nargin < 4 || isempty (options))
    options = __default_plot_options__ ();
  endif

  if (nargin < 5)
    properties = {};
  endif

  if (numel (options) > 1)
    options = options(1);
  endif

  [x_nr, x_nc] = size (x);
  [y_nr, y_nc] = size (y);

  if (x_nr == 1)
    x = x';
    tmp = x_nr;
    x_nr = x_nc;
    x_nc = tmp;
  endif

  if (y_nr == 1)
    y = y';
    tmp = y_nr;
    y_nr = y_nc;
    y_nc = tmp;
  endif

  if (x_nr == y_nr)
    linestyle = options.linestyle;
    marker = options.marker;
    if (isempty (marker) && isempty (linestyle))
      [linestyle, marker] = __next_line_style__ ();
    endif
    color = options.color;
    if (isempty (color))
      color = __next_line_color__ ();
    endif

    retval = line (x, y, "color", color,
              "linestyle", linestyle,
              "marker", marker, properties{:});
  else
    error ("__plt2vv__: vector lengths must match");
  endif

endfunction
