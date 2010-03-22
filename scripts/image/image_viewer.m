## Copyright (C) 2006, 2007, 2008, 2009 S�ren Hauberg
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
## @deftypefn {Function File} {[@var{fcn}, @var{default_zoom}] =} image_viewer (@var{fcn}, @var{default_zoom})
## Change the program or function used for viewing images and return the
## previous values.
##
## When the @code{image} or @code{imshow} function is called it will
## launch an external program to display the image.  The default behavior
## is to use gnuplot if the installed version supports image viewing,
## and otherwise try the programs @code{display}, @code{xv}, and
## @code{xloadimage}.  Using this function it is possible to change that
## behavior.
##
## When called with one input argument images will be displayed by saving
## the image to a file and the system command @var{command} will be called
## to view the image.  The @var{command} must be a string containing
## @code{%s} and possibly @code{%f}.  The @code{%s} will be replaced by
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
## between 0 and 100, and not 0 and 1 like Octave assumes.  This is
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

## Display an image by saving it to a file in PPM format and launching
## @var{command}.
##
## The @var{command} must be a format string containing @code{%s} and
## possibly @code{%f}.  The @code{%s} will be replaced by the filename
## of the image, and the @code{%f} will be replaced by @var{zoom}. The
## @var{x} and @var{y} arguments are ignored.

function __img_via_file__ (x, y, im, zoom, command)

  ppm_name = tmpnam ();
  saveimage (ppm_name, im, "ppm");

  rm = sprintf ("rm -f \"%s\"", ppm_name);

  if (isempty (command))
    ## Different image viewer commands.
    xv = sprintf ("xv -raw -expand %f \"%s\"", zoom, ppm_name);
    xloadimage = sprintf ("xloadimage -zoom %f \"%s\"", zoom*100, ppm_name);
    im_display = sprintf ("display -resize %f%% \"%s\"", zoom*100, ppm_name);
  
    ## Need to let the shell clean up the tmp file because we are putting
    ## the viewer in the background.
    status = system (sprintf ("( %s || %s || %s && %s ) > /dev/null 2>&1 &",
                              im_display, xv, xloadimage, rm));
  else
    ## Does the command support zooming?
    if (findstr (command, "%f"))
      command = sprintf (command, zoom, ppm_name);
    else
      command = sprintf (command, ppm_name);
    endif
    status = system (sprintf ("( %s && %s) > /dev/null 2>&1 &", command, rm));
  endif
  
  ## Did the system call fail?
  if (status != 0)
    error ("the image viewing command failed");
  endif

endfunction
