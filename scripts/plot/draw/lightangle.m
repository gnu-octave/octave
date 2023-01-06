########################################################################
##
## Copyright (C) 2019-2023 The Octave Project Developers
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
## @deftypefn  {} {} lightangle (@var{az}, @var{el})
## @deftypefnx {} {} lightangle (@var{hax}, @var{az}, @var{el})
## @deftypefnx {} {} lightangle (@var{hl}, @var{az}, @var{el})
## @deftypefnx {} {@var{hl} =} lightangle (@dots{})
## @deftypefnx {} {[@var{az}, @var{el}] =} lightangle (@var{hl})
## Add a light object to the current axes using spherical coordinates.
##
## The light position is specified by an azimuthal rotation @var{az} and an
## elevation angle @var{el}, both in degrees.
##
## If the first argument @var{hax} is an axes handle, then create a new light
## object in this axes, rather than the current axes returned by @code{gca}.
##
## If the first argument @var{hl} is a handle to a light object, then act on
## this light object rather than creating a new object.
##
## The optional return value @var{hl} is a graphics handle to the light object.
##
## Example:
##
## Add a light object to a plot
##
## @example
## @group
## @c doctest: +SKIP
## clf;
## sphere (36);
## lightangle (45, 30);
## @end group
## @end example
##
## @seealso{light, view, camlight}
## @end deftypefn

function varargout = lightangle (varargin)

  if (nargin == 0 || nargin > 3 || nargout > 2 || (nargin > 1 && nargout > 1))
    print_usage ();
  endif

  hl = hax = az = el = [];

  if (nargin == 1)
    hl = varargin{1};
    if (! isscalar (hl) || ! isgraphics (hl, "light"))
      error ("lightangle: HL must be a handle to a light object");
    endif
  elseif (nargin == 2)
    az = varargin{1};
    el = varargin{2};
  elseif (nargin == 3)
    h = varargin{1};
    if (isscalar (h) && isaxes (h))
      hax = h;
    elseif (isscalar (h) && isgraphics (h, "light"))
      hl = h;
    else
      error ("lightangle: H must be a handle to an axes or light object");
    endif
    az = varargin{2};
    el = varargin{3};
  endif

  if (nargin == 1)
    pos = get (hl, "Position");
    [az, el] = cart2sph (pos(1), pos(2), pos(3));
    az = rad2deg (az) + 90;  # see view.m
    el = rad2deg (el);
    varargout = { az, el };
    return;
  endif

  if (! isscalar (az) || ! isnumeric (az)
      || ! isscalar (el) || ! isnumeric (el))
    error ("lightangle: AZ and EL must be numeric scalars");
  endif

  if (! isempty (hl))
    hax = ancestor (hl, "axes");
  endif

  if (isempty (hax))
    hax = gca ();
  endif

  if (isempty (hl))
    hl = light (hax);
  endif

  pos = get (hl, "Position");

  az = deg2rad (az - 90);
  el = deg2rad (el);

  if (strcmp (get (hl, "Style"), "local"))
    pos -= get (hax, "CameraTarget");
  endif

  [pos(1), pos(2), pos(3)] = sph2cart (az, el, norm (pos));

  if (strcmp (get (hl, "Style"), "local"))
    pos += get (hax, "CameraTarget");
  endif

  set (hl, "Position", pos);

  if (nargout == 1)
    varargout = { hl };
  endif

endfunction


%!demo
%! clf;
%! sphere (36);
%! lightangle (45, 30);
%! title ("lightangle() demo #1");

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   sphere (24);
%!   hl = lightangle (45, 20);
%!   assert (isgraphics (hl, "light"));
%!   [az, el] = lightangle (hl);
%!   assert ([45, 20], [az, el], -20*eps);
%!   lightangle (hl, 90, 45);
%!   [az, el] = lightangle (hl);
%!   assert ([90, 45], [az, el], -20*eps);
%!   pos = get (hl, "Position");
%!   assert ([1, 0, 1], pos, -20*eps);
%!   hl = lightangle (gca (), 45, 20);
%!   assert (isgraphics (hl, "light"));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error <Invalid call> lightangle ()
%!error <Invalid call> lightangle (1, 2, 3, 4)
%!error <Invalid call> [a, b, c] = lightangle (45, 30)
%!error <Invalid call> [a, b] = lightangle (45, 30)
%!error <HL must be a handle to a light object> lightangle (0)
%!error <H must be a handle to an axes or light object> lightangle (0, 90, 45)
%!error <AZ and EL must be numeric scalars> lightangle ([1 2], 0)
%!error <AZ and EL must be numeric scalars> lightangle ({1}, 0)
%!error <AZ and EL must be numeric scalars> lightangle (0, [1 2])
%!error <AZ and EL must be numeric scalars> lightangle (0, {1})
