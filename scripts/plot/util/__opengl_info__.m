## Copyright (C) 2016-2017 John Donoghue <john.donoghue@ieee.org>
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {} __opengl_info__
## @deftypefnx {} {@var{retval} =} __opengl_info__ ()
##
## Get OpenGL driver information.
##
## If no output values are requested, display information about the
## OpenGL subsystem.  If an output is requested, return the information
## in a structure.
##
## Fields in the structure are:
##
## @table @asis
## @item version
## OpenGL Driver version string
##
## @item vendor
## OpenGL Driver vendor string
##
## @item renderer
## OpenGL renderer string
##
## @item extensions
## List of enabled extensions for the OpenGL driver.
## @end table
##
## @example
## glinfo = __opengl_info__ ();
## @end example
##
## @end deftypefn

function retval = __opengl_info__ ()

  ## currently we only handle a single argument
  if (nargin != 0)
    print_usage ();
  endif

  [info, msg] = gl_info ();

  if (! isempty (msg))
    warning (msg);
  else
    if (nargout == 0)
      printf ("   version: %s\n", info.version);
      printf ("    vendor: %s\n", info.vendor);
      printf ("  renderer: %s\n", info.renderer);
      printf ("extensions:\n");
      printf ("  %s\n", info.extensions{:});
    else
      retval = info;
    endif
  endif

endfunction

function info = fig_gl_info (h)
  info = [];

  if (ishandle (h) && strcmp (get (h, "renderer"), "opengl"))
    vers = get (h, "__gl_version__");
    vend = get (h, "__gl_vendor__");
    rend = get (h, "__gl_renderer__");
    exts = get (h, "__gl_extensions__");
    if (! isempty (vend))
      info.version = vers;
      info.vendor = vend;
      info.renderer = rend;
      info.extensions = strsplit (strtrim (exts));
    endif
  endif
endfunction

function [info, msg] = gl_info ()
  info = [];
  msg = "";

  ## If we have any open figures, take a look for any OpenGL info.
  figs = findall (0, "type", "figure");

  for hf = figs.'
    info = fig_gl_info (hf);
    if (! isempty (info))
      break;
    endif
  endfor

  ## If no info yet, try open a figure to get the info.
  if (isempty (info))
    ## Need to create a figure, place an OpenGL object, and force drawing.
    h = figure ("position", [0,0,1,1], "toolbar", "none", "menubar", "none");
    hax = axes ();
    ## Hmm, drawnow did not seem to be working as intended here.
    pause (0.2);
    info = fig_gl_info (h);
    close (h);
  endif

  if (isempty (info))
    msg = "__opengl_info__: can not obtain OpenGL information";
  endif

endfunction


## Duplicate the test since there is currently no way to write
## "HAVE_OPENGL && (HAVE_FLTK || HAVE_QT)"

%!testif HAVE_OPENGL, HAVE_FLTK; have_window_system
%! a = __opengl_info__ ();
%! assert (! isempty (a))
%! assert (isfield (a, "version"))

%!testif HAVE_OPENGL, HAVE_QT; have_window_system
%! a = __opengl_info__ ();
%! assert (! isempty (a))
%! assert (isfield (a, "version"))

%!error __opengl_info ("foobar")
