## Copyright (C) 2007-2012 David Bateman
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
## @deftypefn  {Function File} {} feather (@var{u}, @var{v})
## @deftypefnx {Function File} {} feather (@var{z})
## @deftypefnx {Function File} {} feather (@dots{}, @var{style})
## @deftypefnx {Function File} {} feather (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} feather (@dots{})
##
## Plot the @code{(@var{u}, @var{v})} components of a vector field emanating
## from equidistant points on the x-axis.  If a single complex argument
## @var{z} is given, then @code{@var{u} = real (@var{z})} and
## @code{@var{v} = imag (@var{z})}.
##
## The style to use for the plot can be defined with a line style @var{style}
## in a similar manner to the line styles used with the @code{plot} command.
##
## The optional return value @var{h} is a vector of graphics handles to the
## line objects representing the drawn vectors.
##
## @example
## @group
## phi = [0 : 15 : 360] * pi/180;
## feather (sin (phi), cos (phi));
## @end group
## @end example
##
## @seealso{plot, quiver, compass}
## @end deftypefn

function retval = feather (varargin)

  [h, varargin, nargin] = __plt_get_axis_arg__ ("feather", varargin{:});

  arrowsize = 0.25;

  if (nargin == 0)
    print_usage ();
  elseif (nargin == 1 || (nargin == 2 && ! isnumeric (varargin{2})))
    ioff = 2;
    z = varargin{1}(:).';
    u = real (z);
    v = imag (z);
  elseif (nargin > 1 && isnumeric (varargin{2}))
    ioff = 3;
    u = varargin{1}(:).';
    v = varargin{2}(:).';
  endif

  line_spec = "b-";
  have_line_spec = false;
  while (ioff <= nargin)
    arg = varargin{ioff++};
    if ((ischar (arg) || iscell (arg)) && ! have_line_spec)
      [linespec, valid] = __pltopt__ ("feather", arg, false);
      if (valid)
        line_spec = arg;
        have_line_spec = false;
        break;
      else
        error ("feather: invalid linespec");
      endif
    else
      error ("feather: unrecognized argument");
    endif
  endwhile

  ## Matlab draws feather plots, with the arrow head as one continous
  ## line, and each arrow separately. This is completely different than
  ## quiver and quite ugly.
  n = length (u);
  xend = [1 : n] + u;
  xtmp = [1 : n] + u .* (1 - arrowsize);
  yend = v;
  ytmp = v .* (1 - arrowsize);
  x = [[1 : n]; xend; xtmp  - v * arrowsize; xend; ...
       xtmp + v * arrowsize];
  y = [zeros(1, n); yend; ytmp  + u * arrowsize / 3; yend; ...
       ytmp - u * arrowsize / 3];

  oldh = gca ();
  unwind_protect
    axes (h);
    newplot ();
    hlist = plot (h, x, y, line_spec, [1, n], [0, 0], line_spec);
  unwind_protect_cleanup
    axes (oldh);
  end_unwind_protect

  if (nargout > 0)
    retval = hlist;
  endif

endfunction


%!demo
%! clf
%! phi = [0 : 15 : 360] * pi / 180;
%! feather (sin (phi), cos (phi))

