## Copyright (C) 2008-2012 David Bateman
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
## @deftypefn  {Function File} {} clabel (@var{c}, @var{h})
## @deftypefnx {Function File} {} clabel (@var{c}, @var{h}, @var{v})
## @deftypefnx {Function File} {} clabel (@var{c}, @var{h}, "manual")
## @deftypefnx {Function File} {} clabel (@var{c})
## @deftypefnx {Function File} {} clabel (@var{c}, @var{h})
## @deftypefnx {Function File} {} clabel (@dots{}, @var{prop}, @var{val}, @dots{})
## @deftypefnx {Function File} {@var{h} =} clabel (@dots{})
## Add labels to the contours of a contour plot.  The contour plot is specified
## by the contour matrix @var{c} and optionally the contourgroup object @var{h}
## that are returned by @code{contour}, @code{contourf} and @code{contour3}.
## The contour labels are rotated and placed in the contour itself.
##
## By default, all contours are labeled.  However, the contours to label can be
## specified by the vector @var{v}.  If the "manual" argument is given then
## the contours to label can be selected with the mouse.
##
## Additional property/value pairs that are valid properties of text objects
## can be given and are passed to the underlying text objects.  Additionally,
## the property "LabelSpacing" is available allowing the spacing between labels
## on a contour (in points) to be specified.  The default is 144 points, or 2
## inches.
##
## The optional return value @var{h} is a vector of graphics handles to
## the text objects representing each label.  
## The "userdata" property of the text objects contains the numerical value of
## the contour label.
##
## An example of the use of @code{clabel} is
##
## @example
## @group
## [c, h] = contour (peaks (), -4 : 6);
## clabel (c, h, -4:2:6, "fontsize", 12);
## @end group
## @end example
##
## @seealso{contour, contourf, contour3, meshc, surfc, text}
## @end deftypefn

function retval = clabel (c, varargin)
  label_spacing = 2 * 72;
  have_hg = false;
  have_labelspacing = false;

  if (nargin < 1)
    print_usage ();
  elseif (nargin == 1)
    hparent = gca ();
  else
    arg = varargin{1};
    if (isscalar (arg) && ishandle(arg)
        && strcmp (get (arg, "type"), "hggroup"))
      obj = get (arg);
      if (! isfield (obj, "contourmatrix"))
        error ("clabel: expecting the handle to be a contour group");
      endif
      hg = arg;
      have_hg = true;
      varargin(1) = [];
    else
      hparent = gca ();
    endif
  endif

  if (length(varargin) > 0 && isnumeric (varargin{1}))
    v = varargin{1}(:);
    varargin(1) = [];
  else
    v = [];
  endif

  for i = 1 : length (varargin) - 1
    arg = varargin{i};
    if (strcmpi (arg, "labelspacing"))
      label_spacing = varargin{i+1};
      have_labelspacing = true;
      varargin(i:i+1) = [];
      break;
    endif
  endfor

  for i = 1 : length (varargin)
    arg = varargin{i};
    if (strcmpi (arg, "manual"))
      error ("clabel: manual contouring mode not supported");
    endif
  endfor

  if (have_hg)
    if (! isempty (v))
      if (have_labelspacing)
        set (hg, "textlistmode", "manual", "textlist", v,
             "labelspacing", label_spacing, "showtext", "on");
      else
        set (hg, "textlistmode", "manual", "textlist", v, "showtext", "on");
      endif
    else
      if (have_labelspacing)
        set (hg,"showtext", "on", "labelspacing", label_spacing);
      else
        set (hg,"showtext", "on");
      endif
    endif
    retval = findobj (hg, "type", "text");
    if (! isempty (varargin))
      set (retval, varargin {:});
    endif
  else
    retval =  __clabel__ (c, v, hparent, label_spacing, [], varargin{:});
  endif
endfunction


%!demo
%! clf
%! [c, h] = contour (peaks(), -4:6);
%! clabel (c, h, -4:2:6, "fontsize", 12);

%!demo
%! clf
%! [c, h] = contourf (peaks(), -7:6);
%! clabel (c, h, -6:2:6, "fontsize", 12);

