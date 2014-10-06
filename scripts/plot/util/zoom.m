## Copyright (C) 2014 John W. Eaton
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
## @deftypefn  {Command} {} zoom (@var{factor})
## @deftypefnx {Command} {} zoom out
## @deftypefnx {Command} {} zoom reset
## Zoom the current axes object.
##
## Given a numeric argument greater than zero, zoom by the given factor.
## If the zoom factor is greater than one, zoom in on the plot.  If the
## factor is less than one, zoom out.  If the zoom factor is a two- or
## three-element vector, then the elements specify the zoom factors for
## the x, y, and z axes respectively.
##
## Given the option @qcode{"out"}, zoom to the initial zoom setting.
##
## Given the option @qcode{"reset"}, set the initial zoom setting to the
## current axes limits.
##
## @seealso{pan, rotate3d}
## @end deftypefn

## Eventually we need to also support these features:
## @deftypefn {Command} {} zoom
## @deftypefnx {Command} {} zoom on
## @deftypefnx {Command} {} zoom off
## @deftypefnx {Command} {} zoom xon
## @deftypefnx {Command} {} zoom yon
## @deftypefnx {Command} {} zoom (@var{hfig}, @var{option})
## @deftypefnx {Command} {zoom_object_handle =} zoom (@var{hfig})

function zoom (varargin)

  hfig = NaN;

  nargs = nargin;

  if (nargs > 2)
    print_usage ();
  endif

  if (nargin == 1 && nargout > 0 && isfigure (varargin{1}))
    error ("zoom_object_handle = zoom (hfig): not implemented");
  endif

  if (nargs == 2)
    hfig = varargin{1};
    if (isfigure (hfig))
      varargin(1) = [];
      nargs--;
    else
      error ("zoom: expecting figure handle as first argument");
    endif
  endif

  if (isnan (hfig))
    hfig = gcf ();
  endif

  if (nargs == 0)
    error ("zoom: toggling zoom mode is not implemented");
  elseif (nargs == 1)
    arg = varargin{1};
    if (isnumeric (arg))
      factor = arg;
      switch (numel (factor))
        case 3
          xfactor = factor(1);
          yfactor = factor(2);
          zfactor = factor(3);
        case 2
          xfactor = factor(1);
          yfactor = factor(2);
          zfactor = 1;
        case 1
          xfactor = yfactor = zfactor = factor;
        otherwise
          error ("zoom: invalid factor");
      endswitch
      if (xfactor < 0 || yfactor < 0 || zfactor < 0)
        error ("zoom: factor must be greater than 1");
      elseif (xfactor == 1 && yfactor == 1 && zfactor == 1)
        return;
      endif
      cax = get (hfig, "currentaxes");
      if (! isempty (cax))
        limits = axis ();
        initial_zoom = getappdata (cax, "initial_zoom");
        if (isempty (initial_zoom))
          setappdata (cax, "__initial_zoom__", limits);
        endif
        limits(1:2) /= xfactor;
        limits(3:4) /= yfactor;
        if (numel (limits) > 4)
          limits(5:6) /= zfactor;
        endif
        axis (cax, limits);
      endif
    elseif (ischar (arg))
      switch (arg)
        case {"on", "off", "xon", "yon"}
          error ("zoom %s: not implemented", arg);

        case "out"
          cax = get (hfig, "currentaxes");
          if (! isempty (cax))
            initial_zoom = getappdata (cax, "__initial_zoom__");
            if (! isempty (initial_zoom))
              axis (cax, initial_zoom);
            endif
          endif

        case "reset"
          cax = get (hfig, "currentaxes");
          if (! isempty (cax))
            setappdata (cax, "__initial_zoom__", axis ());
          endif

        otherwise
          error ("zoom: unrecognized option '%s'", arg);
      endswitch
    else
      error ("zoom: wrong type argument '%s'", class (arg));
    endif
  endif

endfunction
