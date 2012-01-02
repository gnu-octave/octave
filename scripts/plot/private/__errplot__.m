## Copyright (C) 2000-2012 Teemu Ikonen
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
## @deftypefn {Function File} {@var{h} =} __errplot__ (@var{fstr}, @var{p}, @dots{})
## Undocumented internal function.
## @end deftypefn

## Created: 18.7.2000
## Author: Teemu Ikonen <tpikonen@pcu.helsinki.fi>
## Keywords: errorbar, plotting

function h = __errplot__ (fstr, p, varargin)

  if (nargin < 4 || nargin > 8) # at least two data arguments needed
    print_usage ();
  endif

  [fmt, valid] = __pltopt__ ("__errplot__", fstr);

  [len, nplots] = size (varargin{1});
  h = [];

  for i = 1:nplots
    ## Set the plot type based on linestyle.

    if (strcmp (fmt.errorstyle, "~"))
      ifmt = "yerr";
    elseif (strcmp (fmt.errorstyle, ">"))
      ifmt = "xerr";
    elseif (strcmp (fmt.errorstyle, "~>"))
      ifmt = "xyerr";
    elseif (strcmp (fmt.errorstyle, "#"))
      ifmt = "box";
    elseif (strcmp (fmt.errorstyle, "#~"))
      ifmt = "boxy";
    elseif (strcmp (fmt.errorstyle, "#~>"))
      ifmt = "boxxy";
    else
      ifmt = "yerr";
    endif

    hg = hggroup ("parent", p);
    h = [h; hg];
    args = __add_datasource__ ("__errplot__", hg,
                               {"x", "y", "l", "u", "xl", "xu"});

    if (isempty (fmt.color))
      fmt.color = __next_line_color__ ();
    endif
    if (isempty (fmt.marker) && isempty (fmt.linestyle))
      [fmt.linestyle, fmt.marker] = __next_line_style__ ();
    endif
    hl = [(__line__ (hg, "linestyle", fmt.linestyle, "marker", fmt.marker,
                   "color", fmt.color)),
          (__line__ (hg, "linestyle", "-", "marker", "none",
                   "color", fmt.color))];

    switch (numel(varargin))
      case 2
        ydata = varargin{1}(:,i);
        xdata = 1:numel (ydata);
        if (strcmp (ifmt, "xerr") || strcmp (ifmt, "box"))
          xldata = varargin{2}(:,i);
          xudata = ldata;
          ldata = [];
          udata = [];
        elseif (strcmp (ifmt, "yerr") || strcmp (ifmt, "boxy"))
          ldata = varargin{2}(:,i);
          udata = ldata;
          xldata = [];
          xudata = [];
        else
          error ("errorbar: 2 column errorplot is only valid for xerr or yerr");
        endif
      case 3
        if (strcmp (ifmt, "boxxy") || strcmp (ifmt, "xyerr"))
          ydata = varargin{1}(:,i);
          xdata = 1:numel (ydata);
          xldata = varargin{2}(:,i);
          xudata = xldata;
          ldata = varargin{3}(:,i);
          udata = ldata;
        elseif (strcmp (ifmt, "xerr") || strcmp (ifmt, "box"))
          xdata = varargin{1}(:,i);
          ydata = varargin{2}(:,i);
          xldata = varargin{3}(:,i);
          xudata = xldata;
          ldata = [];
          udata = [];
        else # yerr or boxy
          xdata = varargin{1}(:,i);
          ydata = varargin{2}(:,i);
          ldata = varargin{3}(:,i);
          udata = ldata;
          xldata = [];
          xudata = [];
        endif
      case 4
        if (strcmp (ifmt, "boxxy") || strcmp (ifmt, "xyerr"))
          xdata = varargin{1}(:,i);
          ydata = varargin{2}(:,i);
          xldata = varargin{3}(:,i);
          xudata = xldata;
          ldata = varargin{4}(:,i);
          udata = ldata;
        elseif (strcmp (ifmt, "xerr") || strcmp (ifmt, "box"))
          xdata = varargin{1}(:,i);
          ydata = varargin{2}(:,i);
          xldata = varargin{3}(:,i);
          xudata = varargin{4}(:,i);
          ldata = [];
          udata = [];
        else # yerr or boxy
          xdata = varargin{1}(:,i);
          ydata = varargin{2}(:,i);
          ldata = varargin{3}(:,i);
          udata = varargin{4}(:,i);
          xldata = [];
          xudata = [];
        endif
      case 6 # boxxy, xyerr
        if (strcmp (ifmt, "boxxy") || strcmp (ifmt, "xyerr"))
          xdata = varargin{1}(:,i);
          ydata = varargin{2}(:,i);
          xldata = varargin{3}(:,i);
          xudata = varargin{4}(:,i);
          ldata = varargin{5}(:,i);
          udata = varargin{6}(:,i);
        else
          error ("errorbar: error plot with 6 columns only valid for boxxy and xyerr");
        endif
      otherwise
        error ("errorbar: error plot requires 2, 3, 4, or 6 arguments");
    endswitch

    addproperty ("xdata", hg, "data", xdata(:));
    addproperty ("ydata", hg, "data", ydata(:));
    addproperty ("ldata", hg, "data", ldata(:));
    addproperty ("udata", hg, "data", udata(:));
    addproperty ("xldata", hg, "data", xldata(:));
    addproperty ("xudata", hg, "data", xudata(:));
    addproperty ("format", hg, "string", ifmt);

    addproperty ("color", hg, "linecolor", get (hl(1), "color"));
    addproperty ("linewidth", hg, "linelinewidth", get (hl(1), "linewidth"));
    addproperty ("linestyle", hg, "linelinestyle", get (hl(1), "linestyle"));
    addproperty ("marker", hg, "linemarker", get (hl(1), "marker"));
    addproperty ("markerfacecolor", hg, "linemarkerfacecolor",
                 get (hl(1), "markerfacecolor"));
    addproperty ("markeredgecolor", hg, "linemarkerfacecolor",
                 get (hl(1), "markeredgecolor"));
    addproperty ("markersize", hg, "linemarkersize",
                 get (hl(1), "markersize"));

    fcn = {@update_props, hl};
    addlistener (hg, "color", fcn);
    addlistener (hg, "linewidth", fcn);
    addlistener (hg, "linestyle", fcn);
    addlistener (hg, "marker", fcn);
    addlistener (hg, "markerfacecolor", fcn);
    addlistener (hg, "markersize", fcn);

    fcn = {@update_data, hl};
    addlistener (hg, "xdata", fcn);
    addlistener (hg, "ydata", fcn);
    addlistener (hg, "ldata", fcn);
    addlistener (hg, "udata", fcn);
    addlistener (hg, "xldata", fcn);
    addlistener (hg, "xudata", fcn);
    addlistener (hg, "format", fcn);

    hax = ancestor (hg, "axes");
    addlistener (hax, "xscale", fcn);
    addlistener (hax, "yscale", fcn);

    update_data (hg, [], hl);

  endfor

  ## Process legend key
  if (! isempty (fmt.key))    
    hlegend = [];
    fkids = get (gcf(), "children");
    for i = 1 : numel (fkids)
      if (ishandle (fkids(i)) && strcmp (get (fkids(i), "type"), "axes")
          && (strcmp (get (fkids(i), "tag"), "legend")))
        udata = get (fkids(i), "userdata");
        if (! isempty (intersect (udata.handle, gca ())))
          hlegend = fkids (i);
          break;
        endif
      endif
    endfor

    if (isempty (hlegend))
      hlgnd = [];
      tlgnd = {};
    else
      [hlgnd, tlgnd] = __getlegenddata__ (hlegend);
    endif
 
    hlgnd(end+1) = hg;
    tlgnd(end+1) = fmt.key;

    legend (gca(), hlgnd, tlgnd);
  end 

endfunction

function [xdata, ydata] = errorbar_data (xdata, ydata, ldata, udata,
                                         xldata, xudata, ifmt,
                                         xscale, yscale)
  if (strcmp (xscale, "linear"))
    dx = 0.01 * (max (xdata(:)) - min (xdata(:)));
    xlo = xdata - dx;
    xhi = xdata + dx;
  else
    n = xdata > 0;
    rx = exp(0.01 * (max (log(xdata(n))) - min (log(xdata(n)))));
    xlo = xdata/rx;
    xhi = xdata*rx;
  endif
  if (strcmp (yscale, "linear"))
    dy = 0.01 * (max (ydata(:)) - min (ydata(:)));
    ylo = ydata - dy;
    yhi = ydata + dy;
  else
    n = ydata > 0;
    ry = exp(0.01 * (max (log(ydata(n))) - min (log(ydata(n)))));
    ylo = ydata/ry;
    yhi = ydata*ry;
  endif
  nans = NaN + xdata(:);
  if (strcmp (ifmt, "yerr"))
    xdata = [xdata, xdata, nans, ...
             xlo, xhi, nans, ...
             xlo, xhi, nans];
    ydata = [ydata-ldata, ydata+udata, nans, ...
             ydata+udata, ydata+udata, nans, ...
             ydata-ldata, ydata-ldata, nans];
  elseif (strcmp (ifmt, "xerr"))
    xdata = [xdata-xldata, xdata+xudata, nans, ...
             xdata+xudata, xdata+xudata, nans, ...
             xdata-xldata, xdata-xldata, nans];
    ydata = [ydata, ydata, nans, ...
             ylo, yhi, nans, ...
             ylo, yhi, nans];
  elseif (strcmp (ifmt, "boxy"))
    dx = 0.01 * (max (xdata(:)) - min (xdata(:)));
    xdata = [xlo, xhi, xhi, xlo, xlo, nans];
    ydata = [ydata-ldata, ydata-ldata, ydata+udata, ydata+udata, ...
             ydata-ldata, nans];
  elseif (strcmp (ifmt, "box"))
    dy = 0.01 * (max (ydata(:)) - min (ydata(:)));
    xdata = [xdata-xldata, xdata+xudata, xdata+xudata, xdata-xldata, ...
             xdata-xldata, nans];
    ydata = [ylo, ylo, yhi, yhi, ylo, nans];
  elseif (strcmp (ifmt, "boxxy"))
    xdata = [xdata-xldata, xdata+xudata, xdata+xudata, xdata-xldata, ...
             xdata-xldata, nans];
    ydata = [ydata-ldata, ydata-ldata, ydata+udata, ydata+udata, ...
             ydata-ldata, nans];
  elseif (strcmp (ifmt, "xyerr"))
    [x1, y1] = errorbar_data (xdata, ydata, ldata, udata,
                              xldata, xudata, "xerr", xscale, yscale);
    [x2, y2] = errorbar_data (xdata, ydata, ldata, udata,
                              xldata, xudata, "yerr", xscale, yscale);
    xdata = [x1; x2];
    ydata = [y1; y2];
    return
  else
    error ("errorbar: valid error bar types are xerr, yerr, boxxy, and xyerr");
  endif

  xdata = xdata.'(:);
  ydata = ydata.'(:);

endfunction

function update_props (hg, dummy, hl)
  set (hl, "color", get (hg, "color"),
           "linewidth", get (hg, "linewidth"));,
  set (hl(1), "linestyle", get (hg, "linestyle"),
              "marker", get (hg, "marker"),
              "markersize", get (hg, "markersize"),
              "markerfacecolor", get (hg, "markerfacecolor"),
              "markeredgecolor", get (hg, "markeredgecolor"));
endfunction

function update_data (hg, dummy, hl)

  if (strcmp (get (hg, "type"), "axes"))
    hax = hg;
    hg = ancestor (hl(1), "hggroup");
  else
    hax = ancestor (hg, "axes");
  endif
  xscale = get (hax, "xscale");
  yscale = get (hax, "yscale");

  xdata = get (hg, "xdata");
  ydata = get (hg, "ydata");
  ldata = get (hg, "ldata");
  udata = get (hg, "udata");
  xldata = get (hg, "xldata");
  xudata = get (hg, "xudata");
  ifmt = get (hg, "format");

  set (hl(1), "xdata", xdata);
  set (hl(1), "ydata", ydata);

  [errorbar_xdata, errorbar_ydata] = ...
          errorbar_data (xdata, ydata, ldata, udata, xldata, xudata, ...
                         ifmt, xscale, yscale);

  set (hl(2), "xdata", errorbar_xdata);
  set (hl(2), "ydata", errorbar_ydata);

endfunction

