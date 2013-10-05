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
## @deftypefnx {Function File} {} polar (@var{cplx})
## @deftypefnx {Function File} {} polar (@var{cplx}, @var{fmt})
## @deftypefnx {Function File} {} polar (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} polar (@dots{})
## Create a 2-D plot from polar coordinates @var{theta} and @var{rho}.
##
## If a single complex input @var{cplx} is given then the real part is used
## for @var{theta} and the imaginary part is used for @var{rho}.
##
## The optional argument @var{fmt} specifies the line format in the same way
## as @code{plot}.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created plot.
##
## @seealso{rose, compass, plot}
## @end deftypefn

## Author: jwe

function h = polar (varargin)

  [hax, varargin, nargs] = __plt_get_axis_arg__ ("polar", varargin{:});

  if (nargs < 1)
    print_usage ();
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);

    if (nargs == 3)
      if (! ischar (varargin{3}))
        error ("polar: FMT argument must be a string");
      endif
      htmp = __plr2__ (hax, varargin{:});
      maxr = max (varargin{2}(:));
    elseif (nargs == 2)
      if (ischar (varargin{2}))
        htmp = __plr1__ (hax, varargin{:});
        if (iscomplex (varargin{1}))
          maxr = max (imag (varargin{1})(:));
        else
          maxr = max (varargin{1}(:));
        endif
      else
        fmt = "";
        htmp = __plr2__ (hax, varargin{:}, fmt);
        maxr = max (varargin{2}(:));
      endif
    elseif (nargs == 1)
      fmt = "";
      htmp = __plr1__ (hax, varargin{:}, fmt);
      if (iscomplex (varargin{1}))
        maxr = max (imag (varargin{1})(:));
      else
        maxr = max (varargin{1}(:));
      endif
    else
      print_usage ();
    endif

    set (hax, "xlim", [-maxr, maxr], "ylim", [-maxr, maxr],
              "xaxislocation", "zero", "yaxislocation", "zero",
              "plotboxaspectratio", [1, 1, 1]);

  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

  if (nargout > 0)
    h = htmp;
  endif

endfunction

function retval = __plr1__ (h, theta, fmt)

  theta = theta(:);
  if (iscomplex (theta))
    rho = imag (theta);
    theta = real (theta);
  else
    rho = theta;
    theta = (1:rows (rho))';
  endif

  retval = __plr2__ (h, theta, rho, fmt);

endfunction

function retval = __plr2__ (h, theta, rho, fmt)

  if (ndims (theta) > 2 || ndims (rho) > 2)
    error ("polar: THETA and RHO must be 2-D objects");
  endif
  theta = real (theta);
  rho = real (rho);

  if (isscalar (theta))
    if (isscalar (rho))
      x = rho * cos (theta);
      y = rho * sin (theta);
      retval = __plt__ ("polar", h, x, y, fmt);
    else
      error ("polar: Can't plot constant THETA with varying RHO");
    endif
  elseif (isvector (theta))
    if (isvector (rho))
      if (length (theta) != length (rho))
        error ("polar: THETA and RHO vector lengths must match");
      endif
      rho = rho(:);
      theta = theta(:);
      x = rho .* cos (theta);
      y = rho .* sin (theta);
      retval = __plt__ ("polar", h, x, y, fmt);
    elseif (ismatrix (rho))
      theta = theta(:);
      t_nr = rows (theta);
      [r_nr, r_nc] = size (rho);
      if (t_nr != r_nr)
        rho = rho';
        r_nr = r_nc;
      endif
      if (t_nr != r_nr)
        error ("polar: THETA vector and RHO matrix sizes must match");
      endif
      x = diag (cos (theta)) * rho;
      y = diag (sin (theta)) * rho;
      retval = __plt__ ("polar", h, x, y, fmt);
    else
      error ("polar: invalid data for plotting");
    endif
  elseif (ismatrix (theta))
    if (isvector (rho))
      rho = rho(:);
      r_nr = rows (rho);
      [t_nr, t_nc] = size (theta);
      if (r_nr != t_nr)
        theta = theta';
        t_nr = t_nc;
      endif
      if (r_nr != t_nr)
        error ("polar: THETA matrix and RHO vector sizes must match");
      endif
      diag_r = diag (rho);
      x = diag_r * cos (theta);
      y = diag_r * sin (theta);
      retval = __plt__ ("polar", h, x, y, fmt);
    elseif (ismatrix (rho))
      if (! size_equal (rho, theta))
        error ("polar: THETA and RHO matrix dimensions must match");
      endif
      x = rho .* cos (theta);
      y = rho .* sin (theta);
      retval = __plt__ ("polar", h, x, y, fmt);
    else
      error ("polar: invalid data for plotting");
    endif
  else
    error ("polar: invalid data for plotting");
  endif

endfunction


%!demo
%! clf;
%! theta = linspace (0,2*pi,1000);
%! rho = sin (7*theta);
%! polar (theta, rho);
%! title ('polar() plot');

%!demo
%! clf;
%! theta = linspace (0,2*pi,1000);
%! cplx = theta + i*sin (7*theta);
%! polar (cplx, 'g');
%! title ('polar() plot of complex data');

%!demo
%! clf;
%! theta = linspace (0,8*pi,1000);
%! rho = sin (5/4*theta);
%! polar (theta, rho);
%! title ('polar() plot');

