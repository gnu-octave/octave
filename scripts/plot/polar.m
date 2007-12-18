## Copyright (C) 1993, 1994, 1995, 1996, 1997, 1999, 2000, 2004, 2005,
##               2006, 2007 John W. Eaton
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
## @deftypefn {Function File} {} polar (@var{theta}, @var{rho}, @var{fmt})
## Make a two-dimensional plot given the polar coordinates @var{theta} and
## @var{rho}.
##
## The optional third argument specifies the line type.
## @seealso{plot}
## @end deftypefn

## Author: jwe

function retval = polar (varargin)

  [h, varargin, nargs] = __plt_get_axis_arg__ ("polar", varargin{:});

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
	 "dataaspectratio", [1, 1, 1]); 

    if (nargout > 0)
      retval = tmp;
    endif
  unwind_protect_cleanup
    axes (oldh);
  end_unwind_protect

endfunction
