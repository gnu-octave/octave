## Copyright (C) 2006 Søren Hauberg
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {[@var{fcn}, @var{default_zoom}] =} image_viewer (@var{fcn}, @var{default_zoom})
## Change the program or function used for viewing images and return the
## previous values.
##
## When the @code{image} or @code{imshow} function is called it will
## launch an external program to display the image.  The default behaviour
## is to use gnuplot if the installed version supports image viewing,
## and otherwise try the programs @code{display}, @code{xv}, and
## @code{xloadimage}.  Using this function it is possible to change that
## behaviour.
##
## When called with one input argument images will be displayed by saving
## the image to a file and the system command @var{command} will called
## to view the image.  The @var{command} must be a string containing
## @code{%s} and possibly @code{%f}. The @code{%s} will be replaced by
## the filename of the image, and the @code{%f} will (if present) be
## replaced by the zoom factor given to the @code{image} function.
## For example,
## @example
## image_viewer ("eog %s");
## @end example
## changes the image viewer to the @code{eog} program.
##
## With two input arguments, images will be displayed by calling
## the function @var{function_handle}.  For example,
## @example
## image_viewer (data, @@my_image_viewer);
## @end example
## sets the image viewer function to @code{my_image_viewer}.  The image
## viewer function is called with
## @example
## my_image_viewer (@var{x}, @var{y}, @var{im}, @var{zoom}, @var{data})
## @end example
## where @var{x} and @var{y} are the axis of the image, @var{im} is the image
## variable, and @var{data} is extra user-supplied data to be passed to
## the viewer function.
##
## With three input arguments it is possible to change the zooming.
## Some programs (like @code{xloadimage}) require the zoom factor to be
## between 0 and 100, and not 0 and 1 like Octave assumes. This is
## solved by setting the third argument to 100.
##
## @seealso{image, imshow}
## @end deftypefn

function [ocmd, ofcn, ozoom] = image_viewer (cmd, fcn, zoom)

  persistent view_cmd;
  persistent view_fcn;
  persistent view_zoom = 1;

  if (isempty (view_fcn))
    if (isempty (view_cmd)
	&& compare_versions (__gnuplot_version__ (), "4.0", ">"))
      view_fcn = "gnuplot_internal";
    else
      view_fcn = @__img_via_file__;
    endif
  endif

  if (nargin > 3)
    print_usage ();
  endif

  if (nargout > 0)
    ocmd = view_cmd;
    ofcn = view_fcn;
    ozoom = view_zoom;
  endif

  if (nargin > 0)

    if (nargin < 3)
      zoom = 1;
      if (nargin < 2)
	fcn = [];
      endif
    endif

    view_cmd = cmd;
    view_fcn = fcn;
    view_zoom = zoom;

    if (nargin > 1)
      if (isa (fcn, "function_handle"))
	view_fcn = fcn;
      else
	error ("image_viewer: expecting second argument to be a function handle");
      endif
    endif

    if (nargin > 2)
      if (isnumeric (zoom) && isscalar (zoom) && isreal (zoom))
	view_zoom = zoom;
      else
	error ("image_viewer: expecting third argument to be a real scalar");
      endif
    endif

  endif

endfunction
