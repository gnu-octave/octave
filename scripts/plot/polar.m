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
## @deftypefn  {Function File} {} polar (@var{theta}, @var{rho})
## @deftypefnx {Function File} {} polar (@var{theta}, @var{rho}, @var{fmt})
## @deftypefnx {Function File} {} polar (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} polar (@dots{})
## Create a two-dimensional plot from polar coordinates @var{theta} and
## @var{rho}.
##
## The optional argument @var{fmt} specifies the line format.
##
## The optional return value @var{h} is a graphics handle to the created plot.
##
## @seealso{plot}
## @end deftypefn

## Author: jwe

function retval = polar (varargin)

  [h, varargin, nargs] = __plt_get_axis_arg__ ("polar", varargin{:});

  if (nargs < 1)
    print_usage();
  endif

  oldh = gca ();
  unwind_protect
    axes (h);
    newplot ();

    if (nargs == 3)
      if (! ischar (varargin{3}))
        error ("polar: third argument must be a string");
      endif
      tmp = __plr2__ (h, varargin{:});
      maxr = max (varargin {2} (:));
    elseif (nargs == 2)
      if (ischar (varargin{2}))
        tmp = __plr1__ (h, varargin{:});
        if (iscomplex(varargin{1}))
          maxr = max (imag(varargin{1})(:));
        else
          maxr = max (varargin{1}(:));
        endif
      else
        fmt = "";
        tmp = __plr2__ (h, varargin{:}, fmt);
        maxr = max (varargin {2} (:));
      endif
    elseif (nargs == 1)
      fmt = "";
      tmp = __plr1__ (h, varargin{:}, fmt);
      if (iscomplex(varargin{1}))
        maxr = max (imag(varargin{1})(:));
      else
        maxr = max (varargin{1}(:));
      endif
    else
      print_usage ();
    endif

    set (h, "xlim", [-maxr, maxr], "ylim", [-maxr, maxr],
         "xaxislocation", "zero", "yaxislocation", "zero",
         "plotboxaspectratio", [1, 1, 1]);

    if (nargout > 0)
      retval = tmp;
    endif
  unwind_protect_cleanup
    axes (oldh);
  end_unwind_protect

endfunction

function retval = __plr1__ (h, theta, fmt)

  if (nargin != 3)
    print_usage ();
  endif

  [nr, nc] = size (theta);
  if (nr == 1)
    theta = theta';
    tmp = nr;
    nr = nc;
    nc = tmp;
  endif
  theta_i = imag (theta);
  if (any (theta_i))
    rho = theta_i;
    theta = real (theta);
  else
    rho = theta;
    theta = (1:nr)';
  endif

  retval = __plr2__ (h, theta, rho, fmt);

endfunction

function retval = __plr2__ (h, theta, rho, fmt)

  if (nargin != 4)
    print_usage ();
  endif

  if (any (imag (theta)))
    theta = real (theta);
  endif

  if (any (imag (rho)))
    rho = real (rho);
  endif

  if (isscalar (theta))
    if (isscalar (rho))
      x = rho * cos (theta);
      y = rho * sin (theta);
      retval = __plt__ ("polar", h, x, y, fmt);
    else
      error ("__plr2__: invalid data for plotting");
    endif
  elseif (isvector (theta))
    if (isvector (rho))
      if (length (theta) != length (rho))
        error ("__plr2__: vector lengths must match");
      endif
      if (rows (rho) == 1)
        rho = rho';
      endif
      if (rows (theta) == 1)
        theta = theta';
      endif
      x = rho .* cos (theta);
      y = rho .* sin (theta);
      retval = __plt__ ("polar", h, x, y, fmt);
    elseif (ismatrix (rho))
      [t_nr, t_nc] = size (theta);
      if (t_nr == 1)
        theta = theta';
        tmp = t_nr;
        t_nr = t_nc;
        t_nc = tmp;
      endif
      [r_nr, r_nc] = size (rho);
      if (t_nr != r_nr)
        rho = rho';
        tmp = r_nr;
        r_nr = r_nc;
        r_nc = tmp;
      endif
      if (t_nr != r_nr)
        error ("__plr2__: vector and matrix sizes must match");
      endif
      x = diag (cos (theta)) * rho;
      y = diag (sin (theta)) * rho;
      retval = __plt__ ("polar", h, x, y, fmt);
    else
      error ("__plr2__: invalid data for plotting");
    endif
  elseif (ismatrix (theta))
    if (isvector (rho))
      [r_nr, r_nc] = size (rho);
      if (r_nr == 1)
        rho = rho';
        tmp = r_nr;
        r_nr = r_nc;
        r_nc = tmp;
      endif
      [t_nr, t_nc] = size (theta);
      if (r_nr != t_nr)
        theta = theta';
        tmp = t_nr;
        t_nr = t_nc;
        t_nc = tmp;
      endif
      if (r_nr != t_nr)
        error ("__plr2__: vector and matrix sizes must match");
      endif
      diag_r = diag (rho);
      x = diag_r * cos (theta);
      y = diag_r * sin (theta);
      retval = __plt__ ("polar", h, x, y, fmt);
    elseif (ismatrix (rho))
      if (! size_equal (rho, theta))
        error ("__plr2__: matrix dimensions must match");
      endif
      x = rho .* cos (theta);
      y = rho .* sin (theta);
      retval = __plt__ ("polar", h, x, y, fmt);
    else
      error ("__plr2__: invalid data for plotting");
    endif
  else
    error ("__plr2__: invalid data for plotting");
  endif

endfunction


%!demo
%! clf
%! theta = linspace (0, 2*pi, 1000);
%! rho = sin (7*theta);
%! polar (theta, rho);

%!demo
%! clf
%! theta = linspace (0, 10*pi, 1000);
%! rho = sin (5/4*theta);
%! polar (theta, rho);

