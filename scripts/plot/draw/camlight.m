########################################################################
##
## Copyright (C) 2016-2023 The Octave Project Developers
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
## @deftypefn  {} {} camlight {}
## @deftypefnx {} {} camlight right
## @deftypefnx {} {} camlight left
## @deftypefnx {} {} camlight headlight
## @deftypefnx {} {} camlight (@var{az}, @var{el})
## @deftypefnx {} {} camlight (@dots{}, @var{style})
## @deftypefnx {} {} camlight (@var{hl}, @dots{})
## @deftypefnx {} {} camlight (@var{hax}, @dots{})
## @deftypefnx {} {@var{h} =} camlight (@dots{})
## Add a light object to a figure using a simple interface.
##
## When called with no arguments, a light object is added to the current plot
## and is placed slightly above and to the right of the camera's current
## position: this is equivalent to @code{camlight right}.  The commands
## @code{camlight left} and @code{camlight headlight} behave similarly with
## the placement being either left of the camera position or centered on the
## camera position.
##
## For more control, the light position can be specified by an azimuthal
## rotation @var{az} and an elevation angle @var{el}, both in degrees,
## relative to the current properties of the camera.
##
## The optional string @var{style} specifies whether the light is a local point
## source (@qcode{"local"}, the default) or placed at infinite distance
## (@qcode{"infinite"}).
##
## If the first argument @var{hl} is a handle to a light object, then act on
## this light object rather than creating a new object.
##
## If the first argument @var{hax} is an axes handle, then create a new light
## object in this axes, rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the light object.
## This can be used to move or further change properties of the light object.
##
## Examples:
##
## Add a light object to a plot
##
## @example
## @group
## @c doctest: +SKIP
## sphere (36);
## camlight
## @end group
## @end example
##
## Position the light source exactly
##
## @example
## @group
## @c doctest: +SKIP
## camlight (45, 30);
## @end group
## @end example
##
## Here the light is first pitched upwards (@pxref{XREFcamup,,@code{camup}})
## from the camera position (@pxref{XREFcampos,,@code{campos}}) by 30
## degrees.  It is then yawed by 45 degrees to the right.  Both rotations
## are centered around the camera target
## (@pxref{XREFcamtarget,,@code{camtarget}}).
##
## Return a handle to further manipulate the light object
##
## @example
## @group
## @c doctest: +SKIP
## clf
## sphere (36);
## hl = camlight ("left");
## set (hl, "color", "r");
## @end group
## @end example
##
## @seealso{light}
## @end deftypefn

function h = camlight (varargin)

  if (nargin > 4)
    print_usage ();
  endif

  ## Note: There is a very small chance of a collision between a numeric double
  ## specifying azimuth and a light handle object (also a numeric double).
  ## We don't worry about that.
  if (numel (varargin) > 0 && numel (varargin{1}) == 1
      && ishghandle (varargin{1}))
    typ = get (varargin{1}, "type");
    if (strcmp (typ, "light"))
      hl  = varargin{1};
      hax = get (hl, "parent");
    elseif (strcmp (typ, "axes"))
      hax = varargin{1};
      hl  = [];
    else
      error ("camlight: HL must be a handle to an axes or light object");
    endif
    varargin(1) = [];
    nargin = nargin - 1;
  else
    hl  = [];
    hax = [];
  endif

  style = "local";
  where = "";

  if (nargin == 0)
    where = "right";

  elseif (nargin == 1 && ischar (varargin{1}))
    arg1 = varargin{1};
    if (strcmpi (arg1, "local") || strcmpi (arg1, "infinite"))
      style = arg1;
      where = "right";
    else
      where = arg1;
    endif

  elseif (nargin == 2)
    arg1 = varargin{1};
    arg2 = varargin{2};
    if (isnumeric (arg1) && isscalar (arg1)
        && isnumeric (arg2) && isscalar (arg2))
      az = arg1;
      el = arg2;
    elseif (ischar (arg1) && ischar (arg2))
      where = arg1;
      style = arg2;
    else
      print_usage ();
    endif

  elseif (nargin == 3 && ischar (varargin{3})
          && isnumeric (varargin{1}) && isscalar (varargin{1})
          && isnumeric (varargin{2}) && isscalar (varargin{2}))
    [az, el, style] = varargin{1:3};

  else
    print_usage ();
  endif

  if (! isempty (where))
    switch (tolower (where))
      case "left"
        az = -30;
        el = 30;

      case "right"
        az = 30;
        el = 30;

      case "headlight"
        az = 0;
        el = 0;

      otherwise
        error ("camlight: invalid light position '%s'", where);

    endswitch
  endif

  if (isempty (hax))
    hax = gca ();
  endif
  cam_up = get (hax, "cameraupvector");
  cam_pos = get (hax, "cameraposition");
  cam_target = get (hax, "cameratarget");

  view_ax = cam_target - cam_pos;
  view_ax /= norm (view_ax);
  ## Orthogonalize the camup vector
  yaw_ax = cam_up - view_ax*dot (cam_up, view_ax);
  pitch_ax = cross (cam_up, view_ax);

  ## First pitch up by 'el', then yaw by 'az'
  ## (order matters, this matches experiments with Matlab).
  pos = num2cell (cam_pos);
  [pos{:}] = __rotate_around_axis__ (pos{:}, el, pitch_ax, cam_target);
  [pos{:}] = __rotate_around_axis__ (pos{:}, az, yaw_ax, cam_target);
  pos = [pos{:}];

  if (isempty (hl))
    hl = light (hax, "Position", pos, "style", style);
  else
    set (hl, "Position", pos, "style", style);
  endif

  if (nargout > 0)
    h = hl;
  endif

endfunction


%!demo
%! clf;
%! ## Adding lights to a scene
%! sphere (64);
%! camlight
%!
%! ## Add a second light
%! camlight left
%!
%! title ({"camlight()", "lights are left and right"});

%!demo
%! clf;
%! sphere (48);
%! title ({"camlight()", "light in fixed position ignores camera change"});
%! axis equal;
%! shading flat;
%! view (30, 30);
%!
%! camlight
%!
%! for a = 30:2:390
%!   view (a, 30);
%!   drawnow ();
%!   pause (0.01);
%! endfor

%!demo
%! clf;
%! sphere (48);
%! title ({"camlight()", "update light position with camera change"});
%! axis equal;  shading flat
%! view (30, 30);
%!
%! hl = camlight ();          # keep a handle to the light
%!
%! for a = 30:2:390
%!   view (a, 30);
%!   camlight (hl, "right");  # update light position
%!   drawnow ();
%!   pause (0.01);
%! endfor

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   sphere (24);
%!   set (gca (), "cameraposition", [8 12 7.3]);
%!   set (gca (), "cameraupvector", [0 2 2]);
%!   set (gca (), "cameratarget", [0.5 -0.3 -0.3]);
%!   h = camlight (45, 20);
%!   A = get (h, "position");
%!   ## From  Matlab R2016a for Windows:
%!   B = [-3.3012070881570281 15.474861455795915 1.1158286348951763];
%!   assert (A, B, -20*eps);
%!
%!   h = camlight (300, -190);
%!   A = get (h, "position");
%!   B = [-11.054849015640565 2.9313301431006460 -11.315623892092516];
%!   assert (A, B, -20*eps);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Move a light
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   sphere ();
%!   h1 = camlight ("left");
%!   h2 = camlight ();
%!   p2 = get (h2, "position");
%!   camlight (h1, "right")
%!   p1 = get (h1, "position");
%!   assert (p1, p2);
%!   camlight (h1);
%!   p1 = get (h1, "position");
%!   assert (p1, p2);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Updating style also moves light
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   sphere ();
%!   h1 = camlight ("right");
%!   h  = camlight ("headlight");
%!   p1 = get (h1, "position");
%!   h2 = camlight (h, "local");
%!   p2 = get (h2, "position");
%!   assert (h, h2);
%!   assert (p1, p2);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test an axes handle as first argument
%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   hax1 = subplot (1, 2, 1);
%!   hax2 = subplot (1, 2, 2);
%!   hl1  = camlight ();
%!   hl2  = camlight (hax1);
%!   assert (get (hl1, "Parent"), hax2);
%!   assert (get (hl2, "Parent"), hax1);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

## Test input validation
%!error camlight (1,2,3,4,5)
%!error <HL must be a handle to an axes or light object> camlight (0, "left")
%!error <Invalid call> camlight ({1}, {2})
%!error <Invalid call> camlight (rand (), 1, 2, 3)
%!error <invalid light position 'foobar'> camlight ("foobar")
%!error <invalid light position 'foobar'> camlight ("foobar", "local")
